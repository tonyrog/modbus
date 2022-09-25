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
%%%    Example modbus server using gen_tcp
%%% @end
%%% Created : 18 Oct 2015 by Tony Rogvall <tony@rogvall.se>

-module(modbus_tcp_server).

-behaviour(gen_server).

-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-export([start_link/1]).
-export([stop/1]).

-include("../include/modbus.hrl").

-define(debug(F,A), ok).
%%-define(debug(F,A), io:format((F)++"\r\n", (A))).

%%-define(warning(F,A), ok).
-define(warning(F,A), io:format((F)++"\r\n", (A))).

-define(DEFAULT_TCP_PORT, 502).

-record(serv,
	{
	 listen :: gen_tcp:socket(),
	 accept :: pid(),
	 mon :: reference(),
	 port = ?DEFAULT_TCP_PORT,
	 proto_id = 0,
	 unit_ids = [255],
	 callback :: fun()
	}).

-record(state,
	{
	 socket :: gen_tcp:socket(),
	 proto_id = 0,
	 unit_ids = [255],
	 buf = <<>>,
	 callback
	}).

start_link(Opts0) ->
    Opts = Opts0 ++ application:get_all_env(modbus),
    ?debug("start options ~p", [Opts]),
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
    Args = [Port,Callback,UnitIDs,
	    {active,once},{mode,binary},{reuseaddr,true},{nodelay,true}],
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, stop).

%% init(Socket::socket(), Args::[term()] 
%%   -> {ok,state()} | {stop,reason(),state()}
init([Port,Callback,UnitIDs|ListenOpts]) ->
    ?debug("unit ids ~p", [UnitIDs]),
    case gen_tcp:listen(Port, ListenOpts) of
	{ok, L} ->
	    S0 = #serv { listen = L, 
			 port = Port,
			 callback = Callback,
			 unit_ids = UnitIDs },
	    {ok, accept(S0)};
	Error ->
	    Error
    end.

handle_call(_Call,_From, S) ->
    {reply, {error, bad_call}, S}.

handle_cast(_Cast, S) ->
    {noreply, S}.

handle_info({accept,_Res,Pid},  S) when Pid =:= S#serv.accept ->
    erlang:demonitor(S#serv.mon, [flush]),
    {noreply, accept(S#serv { mon = undefined })};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, S) ->
    if Ref =:= S#serv.mon ->  %% acceptor crashed in gen_tcp:accept???
	    {noreply, accept(S#serv { mon = undefined })};
       true -> %% old or not for us
	    {noreply, S}
    end;
handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


accept(S) ->
    Self = self(),
    {Accept,Mon} = spawn_monitor(fun() ->
					 acceptor(Self,
						  S#serv.listen,
						  S#serv.callback,
						  S#serv.unit_ids)
				 end),
    S#serv { accept = Accept, mon = Mon }.
    

acceptor(Parent, L, Callback, UnitIDs) ->
    case gen_tcp:accept(L) of
	{ok, Socket} ->
	    Parent ! {accept, ok, self()},
	    loop(#state { socket = Socket,
			  unit_ids = UnitIDs,
			  callback = Callback });
	Err={error, _} ->
	    Parent ! {accept, Err, self()}
    end.

loop(State=#state { socket = Socket }) ->
    receive
	{tcp, Socket, Data} ->
	    inet:setopts(Socket, [{active, once}]),
	    Buf = <<(State#state.buf)/binary, Data/binary>>,
	    case Buf of
		<<TransID:16,_ProtoID:16,Length:16,Data1:Length/binary, Buf1/binary>> ->
		    case Data1 of
			<<UnitID, Func, Params/binary>> ->
			    ?debug("unit_id ~p received, check list ~p",
				   [UnitID, State#state.unit_ids]),
			    case lists:member(UnitID,State#state.unit_ids) of
				true ->
				    try handle_pdu(Socket,UnitID,TransID,Func,Params, State#state { buf = Buf1 }) of
					State1 ->
					    loop(State1)
				    catch
					error:_Reason ->
					    gen_tcp:send(Socket,TransID,State#state.proto_id,
							 UnitID, 
						 16#80 + (Func band 16#7f),
						 <<?SLAVE_DEVICE_FAILURE>>),
					    loop(State)
				    end;
				false ->
				    ?warning("pdu not for us", []),
				    loop(State#state { buf = Buf1 })
			    end;
			_ ->
			    ?warning("pdu too short", []),
			    loop(State#state { buf = Buf1 })
		    end;
		_ ->
		    %% FIXME: throw if too big
		    loop(State#state { buf = Buf })
	    end;
	{tcp_closed, Socket} ->
	    {ok, State};
	{tcp_error, Socket, Error} ->
	    {error, Error}
    end.

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
    gen_tcp:send(Socket, Data).
