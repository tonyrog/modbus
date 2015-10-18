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
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Modbus TCP client
%%% @end
%%% Created : 18 Oct 2015 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(modbus_tcp_client).

-behaviour(gen_server).

-include_lib("lager/include/log.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(DEFAULT_TCP_PORT, 502).

-record(state, 
	{
	  socket,
	  host = "localhost",
	  port = ?DEFAULT_TCP_PORT,
	  protocol = [tcp],
	  tags  = {tcp,tcp_closed,tcp_error},
	  proto_id = 0,
	  trans_id = 1,
	  unit_id  = 255,
	  requests = [],
	  buf = <<>>
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Opts0) ->
    Opts = Opts0 ++ application:get_all_env(modbus),
    Host = proplists:get_value(host, Opts, "localhost"),
    Port = proplists:get_value(port, Opts, ?DEFAULT_TCP_PORT),
    Protocol = proplists:get_value(protocol, Opts, [tcp]),
    UnitID = proplists:get_value(unit_id, Opts, 255),
    SocketOptions = [{mode,binary},{active,once},{nodelay,true}],
    case exo_socket:connect(Host, Port, Protocol, SocketOptions, 5000) of
	{ok,Socket} ->
	    Tags = exo_socket:tags(Socket),
	    {ok, #state{ socket=Socket, 
			 host=Host,
			 port=Port,
			 protocol=Protocol,
			 tags=Tags,
			 unit_id=UnitID
		       }};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({pdu,Function,Params}, From, State) when is_binary(Params) ->
    TransID = State#state.trans_id,
    ProtoID = State#state.proto_id,
    Length  = byte_size(Params)+1,
    Data    = <<TransID:16,ProtoID:16,Length:16,Function,Params/binary>>,
    exo_socket:send(State#state.socket, Data),
    Req = {TransID, Function, From },
    {noreply, State#state { trans_id = TransID+1,
			    requests = [Req | State#state.requests]}};
handle_call(_Request, _From, State) ->
    {reply, {error,badarg}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Tag,_Socket,Data}, State = #state { tags={Tag,_,_} }) ->
    exo_socket:setopts(State#state.socket, [{active, once}]),
    Buf = <<(State#state.buf)/binary, Data/binary>>,
    case Buf of
	<<TransID:16, _ProtoID:16, Length:16, Pdu:Length/binary,
	  Buf1/binary>> ->
	    State1 = handle_pdu(TransID, Pdu, State#state { buf = Buf1 }),
	    {noreply, State1};
	_ ->
	    {noreply, State#state { buf = Buf }}
    end;
handle_info({Tag,_Socket},State = #state { tags={_,Tag,_} }) ->
    %% FIXME: reconnect
    {stop, closed, State};
handle_info({Tag,_Socket,Error},State = #state { tags={_,_,Tag} }) ->
    %% FIXME: reconnect
    {stop, Error, State};
handle_info(_Info, State) ->
    lager:warning("got info ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_pdu(TransID, Pdu, State) ->
    case lists:keytake(TransID, 1, State#state.requests) of
	false ->
	    ?warning("transaction ~p not found", [TransID]),
	    State;
	{value,{_,Function,From},Reqs} ->
	    ?debug("response pdu=~p\n", [Pdu]),
	    case Pdu of
		<<1:1,Function:7,ErrorCode>> ->
		    gen_server:reply(From, {error, ErrorCode}),
		    State#state { requests = Reqs };
		<<Function,Data/binary>> ->
		    %% FIXME: some more cases here
		    gen_server:reply(From, {ok,Data}),
		    State#state { requests = Reqs }
	    end
    end.
