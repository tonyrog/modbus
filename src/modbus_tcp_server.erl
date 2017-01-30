%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2015, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Example modbus server using exo_socket_server
%%% @end
%%% Created : 18 Oct 2015 by Tony Rogvall <tony@rogvall.se>

-module(modbus_tcp_server).

-behaviour(exo_socket_server).
-export([init/2, data/3, close/2, error/3, control/4]).

-export([start_link/1]).
-export([stop/1]).

-include("../include/modbus.hrl").

-record(state,
	{
	  proto_id = 0,
	  unit_ids = [255],
	  buf = <<>>,
	  callback
	}).

-define(DEFAULT_TCP_PORT, 502).

start_link(Opts0) ->
    Opts = Opts0 ++ application:get_all_env(modbus),
    lager:debug("start options ~p", [Opts]),
    Port = proplists:get_value(port, Opts, ?DEFAULT_TCP_PORT),
    Callback = proplists:get_value(callback, Opts),
    UnitIDs = case proplists:get_value(unit_ids, Opts) of
		  List when is_list(List) -> List;
		  I when is_integer(I) -> [I];
		  undefined ->
		      %% old syntax
		      case proplists:get_value(unit_id, Opts, 255) of
			  List when is_list(List) -> List;
			  I when is_integer(I) -> [I]
		      end
	      end,
    exo_app:start(),
    exo_socket_server:start_link(Port, [tcp],
				 [{active,once},{mode,binary},
				  {reuseaddr,true},{nodelay,true}],
				 ?MODULE, [Callback,UnitIDs]).

stop(Pid) when is_pid(Pid) ->
    exo_socket_server:stop(Pid).

%% init(Socket::socket(), Args::[term()] 
%%   -> {ok,state()} | {stop,reason(),state()}
init(_Socket, [Callback,UnitIDs]) ->
    lager:debug("unit ids ~p", [UnitIDs]),
    {ok, 
     #state{ 
	callback = Callback,
	unit_ids = UnitIDs
       }}.

%% data(Socket::socket(), Data::io_list(), State::state()) 
%%   -> {ok,state()}|{close,state()}|{stop,reason(),state()}
data(Socket, Data, State) ->
    Buf = <<(State#state.buf)/binary, Data/binary>>,
    case Buf of
	<<TransID:16,_ProtoID:16,Length:16,Data1:Length/binary, Buf1/binary>> ->
	    case Data1 of
		<<UnitID, Func, Params/binary>> ->
		    lager:debug("unit_id ~p received, check list ~p",
				[UnitID, State#state.unit_ids]),
		    case lists:member(UnitID,State#state.unit_ids) of
			true ->
			    try handle_pdu(Socket,UnitID,TransID,Func,Params,
					   State#state { buf = Buf1 }) of
				State1 ->
				    {ok, State1}
			    catch
				error:_Reason ->
				    send(Socket,TransID,State#state.proto_id,
					 UnitID, 
					 16#80 + (Func band 16#7f),
					 <<?SLAVE_DEVICE_FAILURE>>),
				    {ok, State}
			    end;
			false ->
			    lager:warning("pdu not for us", []),
			    {ok, State#state { buf = Buf1 }}
			end;
		_ ->
		    lager:warning("pdu too short", []),
		    {ok, State#state { buf = Buf1 }}
	    end;
	_ ->
	    %% FIXME: throw if too big
	    {ok, State#state { buf = Buf }}
    end.

%% close(Socket::socket(), State::state())
%%   -> {ok,state()}
close(_Socket, State) ->
    {ok, State}.

%% error(Socket::socket(),Error::error(), State:state())
%%   -> {ok,state()} | {stop,reason(),state()}

error(_Socket, Error,State) ->
    {stop, Error, State}.

%% control(Socket::socket(), Request::term(), 
%%         From::term(), State:state())
%%   -> {reply, Reply::term(),state() [,Timeout]} | 
%%      {noreply,state() [,Timeout]} |
%%      {ignore,state()[,Timeout]} | 
%%      {send, Bin::binary(),state()[,Timeout]} |
%%      {data, Data::term()[,Timeout]} |
%%      {stop,reason(), Reply::term(),state()]}

control(_Socket, _Request, _From, State) ->
    {reply, {error, no_control}, State}.

%%
%% Handle modbus command
%%
handle_pdu(Socket, UnitID, TransID, ?READ_DISCRETE_INPUTS,
	   <<Addr:16,N:16>>, State) ->
    Coils = apply(State#state.callback,read_discrete_inputs,[Addr,N]),
    Bin = modbus:coils_to_bin(Coils),
    Len = byte_size(Bin),
    send(Socket, TransID, State#state.proto_id, UnitID,
	 ?READ_DISCRETE_INPUTS, <<Len, Bin/binary>>),
    State;
handle_pdu(Socket, UnitID, TransID, ?READ_COILS,
	   <<Addr:16,N:16>>, State) ->
    Coils = apply(State#state.callback,read_coils,[Addr,N]),
    Bin = modbus:coils_to_bin(Coils),
    Len = byte_size(Bin),
    send(Socket, TransID, State#state.proto_id, UnitID,
	 ?READ_COILS, <<Len, Bin/binary>>),
    State;
handle_pdu(Socket, UnitID, TransID, ?WRITE_SINGLE_COIL,
	   <<Addr:16,Value:16>>, State) ->
    Value1 = apply(State#state.callback, write_single_coil, [Addr,Value]),
    send(Socket, TransID, State#state.proto_id, UnitID,
	 ?WRITE_SINGLE_COIL, <<Addr:16, Value1:16>>),
    State;
handle_pdu(Socket, UnitID, TransID, ?WRITE_MULTIPLE_COILS, 
	   <<Addr:16, N:16, _M, Data/binary>>, State) ->
    Coils = modbus:bits_to_coils(N, Data),
    N1 = apply(State#state.callback, write_multiple_coils, [Addr,Coils]),
    send(Socket, TransID, State#state.proto_id, UnitID, 
	 ?WRITE_MULTIPLE_COILS, <<Addr:16, N1:16>>),
    State;
handle_pdu(Socket, UnitID, TransID, ?READ_INPUT_REGISTERS,
	   <<Addr:16,N:16>>, State) ->
    Regs = apply(State#state.callback, read_input_registers, [Addr,N]),
    RegData = << <<Reg:16>> || Reg <- Regs >>,
    Len = byte_size(RegData),
    send(Socket, TransID, State#state.proto_id,UnitID,
	 ?READ_INPUT_REGISTERS,<<Len, RegData/binary>>), 
    State;
handle_pdu(Socket, UnitID, TransID, ?READ_HOLDING_REGISTERS,
	   <<Addr:16,N:16>>, State) ->
    Regs = apply(State#state.callback, read_holding_registers, [Addr,N]),
    RegData = << <<Reg:16>> || Reg <- Regs >>,
    Len = byte_size(RegData),
    send(Socket, TransID, State#state.proto_id,UnitID,
	 ?READ_HOLDING_REGISTERS,<<Len, RegData/binary>>), 
    State;
handle_pdu(Socket, UnitID, TransID, ?WRITE_SINGLE_HOLDING_REGISTER,
	   <<Addr:16,Value:16>>, State) ->
    Value1 = apply(State#state.callback, write_single_holding_register, 
		   [Addr,Value]),
    send(Socket, TransID, State#state.proto_id,  UnitID,
	 ?WRITE_SINGLE_HOLDING_REGISTER, <<Addr:16, Value1:16>>),
    State;
handle_pdu(Socket, UnitID, TransID, ?WRITE_MULTIPLE_HOLDING_REGISTERS,
	   <<Addr:16,_N:16,_M,Data/binary>>, State) ->
    Values = [ V ||  <<V:16>> <= Data ], %% check M? and check N!
    N1 = apply(State#state.callback, write_multiple_holding_registers,
	       [Addr,Values]),
    send(Socket,TransID, State#state.proto_id,UnitID,
	 ?WRITE_MULTIPLE_HOLDING_REGISTERS,<<Addr:16, N1:16>>),
    State;
handle_pdu(Socket, UnitID, TransID, Func,<<_/binary>>, State) ->
    send(Socket,TransID,State#state.proto_id, UnitID,
	 16#80 + (Func band 16#7f), <<?ILLEGAL_FUNCTION>>).

send(Socket,TransID,ProtoID,UnitID,Func,Bin) ->
    Length  = byte_size(Bin) + 2,
    Data    = <<TransID:16, ProtoID:16, Length:16, UnitID, Func, Bin/binary>>,
    exo_socket:send(Socket, Data).
