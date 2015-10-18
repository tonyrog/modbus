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
%%%    Demo server side callback module
%%% @end
%%% Created : 18 Oct 2015 by Tony Rogvall <tony@rogvall.se>

-module(modbus_demo_callback).

-export([read_discrete_inputs/2,
	 read_coils/2,
	 write_single_coil/2,
	 write_multiple_coils/2,
	 read_input_registers/2,
	 read_holding_registers/2,
	 write_single_holding_register/2,
	 write_multiple_holding_registers/2]).

read_discrete_inputs(Addr,N) ->
    io:format("demo_callback: read_discrete_inputs: addr=~w, n=~w\n", [Addr,N]),
    [ (random:uniform(2)-1) || _ <- lists:seq(1, N)].
			       
read_coils(Addr,N) ->
    io:format("demo_callback: read_coils: addr=~w, n=~w\n", [Addr,N]),
    [ (random:uniform(2)-1) || _ <- lists:seq(1, N)].

write_single_coil(Addr, Value) ->
    io:format("demo_callback: write_single_coil: addr=~w, value=~w\n", 
	      [Addr,Value]),
    Value.

write_multiple_coils(Addr,BitList) ->
    io:format("demo_callback: write_multiple_coils: addr=~w, values=~w\n", 
	      [Addr,BitList]),
    length(BitList).

read_input_registers(Addr, N) ->
    io:format("demo_callback: read_input_registers: addr=~w, n=~w\n", [Addr,N]),
    [ (random:uniform(16#10000)-1) || _ <- lists:seq(1, N)].

read_holding_registers(Addr, N) ->
    io:format("demo_callback: read_holding_registers: addr=~w, n=~w\n", 
	      [Addr,N]),
    [ (random:uniform(16#10000)-1) || _ <- lists:seq(1, N)].

write_single_holding_register(Addr, Value) ->
    io:format("demo_callback: write_single_holding_register: addr=~w, value=~w\n", [Addr,Value]),
    Value.

write_multiple_holding_registers(Addr, Values) ->
    io:format("demo_callback: write_multiple_holding_registers: addr=~w, values=~w\n", [Addr,Values]),
    length(Values).



