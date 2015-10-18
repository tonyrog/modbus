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
%%%    Client API for modbus
%%% @end
%%% Created : 18 Oct 2015 by Tony Rogvall <tony@rogvall.se>

-module(modbus).

-export([read_discrete_inputs/3,
	 read_coils/3,
	 write_single_coil/3,
	 write_multiple_coils/3,
	 read_input_registers/3,
	 read_holding_registers/3,
	 write_single_holding_register/3,
	 write_multiple_holding_registers/3]).

-export([bits_to_coils/2]).
-export([coils_to_bin/1]).

-include("../include/modbus.hrl").
	 	 
read_discrete_inputs(Pid,Addr,N) ->
    case gen_server:call(Pid, {pdu,?READ_DISCRETE_INPUTS,
			       <<Addr:16, N:16>>}) of
	{ok, <<Len,Bin:Len/binary>>} when N =< Len*8 ->
	    {ok, bits_to_coils(N, Bin)};
	Error ->
	    Error
    end.

read_coils(Pid,Addr,N) ->
    case gen_server:call(Pid, {pdu,?READ_COILS,
			       <<Addr:16, N:16>>}) of
	{ok, <<Len,Bin:Len/binary>>} when N =< Len*8 ->
	    {ok, bits_to_coils(N, Bin)};
	Error ->
	    Error
    end.

write_single_coil(Pid, Addr, Value) when is_integer(Addr), Addr >= 0,
					 is_integer(Value), Value >= 0 ->
    Value1 = if Value =/= 0 -> 16#FF00; true -> 0 end,
    case gen_server:call(Pid, {pdu,?WRITE_SINGLE_COIL,
			       <<Addr:16, Value1:16>>}) of
	{ok, <<Addr:16, Value1:16>>} ->
	    ok;
	Error -> Error
    end.

write_multiple_coils(Pid,Addr,BitList) -> 
    N = length(BitList),
    Data = coils_to_bin(BitList),
    M = byte_size(Data),
    case gen_server:call(Pid, {pdu, ?WRITE_MULTIPLE_COILS,
			       <<Addr:16, N:16, M, Data/binary>>}) of
	{ok, <<Addr:16, N:16>>} ->
	    ok;
	Error -> Error
    end.

read_input_registers(Pid,Addr,N) ->
    case gen_server:call(Pid, {pdu,?READ_INPUT_REGISTERS, 
			       <<Addr:16, N:16>>}) of
	{ok, <<Len, RegData:Len/binary>>} ->
	    {ok, [ Reg || <<Reg:16>> <= RegData ]};
	Error -> Error
    end.

read_holding_registers(Pid, Addr, N) ->
    case gen_server:call(Pid, {pdu, ?READ_HOLDING_REGISTERS,
			       <<Addr:16, N:16>>}) of
	{ok, <<Len, RegData:Len/binary>>} ->
	    {ok, [ Reg || <<Reg:16>> <= RegData ]};
	Error -> Error
    end.

write_single_holding_register(Pid, Addr, Value) ->
    case gen_server:call(Pid, {pdu,?WRITE_SINGLE_HOLDING_REGISTER,
			       <<Addr:16, Value:16>>}) of
	{ok, <<Addr:16, Value:16>>} ->
	    ok;
	Error -> Error
    end.

write_multiple_holding_registers(Pid, Addr, Values) ->
    N = length(Values),
    Data = << <<V:16>> || V <- Values >>,
    M = byte_size(Data),
    case gen_server:call(Pid, {pdu,?WRITE_MULTIPLE_HOLDING_REGISTERS,
			       <<Addr:16,N:16,M,Data/binary>>}) of
	{ok, <<Addr:16, N:16>>} ->
	    ok;
	Error -> Error
    end.

%% utils
%% bits are stored lsb 
bits_to_coils(N, Bin) when is_integer(N), N >= 0,
			   is_binary(Bin) ->
    Bits = lists:append([ lists:reverse([B||<<B:1>> <= <<Byte>> ]) ||
			    <<Byte>> <= Bin]),
    {BitList,_} = lists:split(N, Bits),
    BitList.
    
coils_to_bin(Bits) ->
    coils_to_bin(Bits, <<>>).

coils_to_bin([B0,B1,B2,B3,B4,B5,B6,B7|Bits], Acc) ->
    coils_to_bin(Bits, <<Acc/binary, B7:1,B6:1,B5:1,B4:1,
			  B3:1,B2:1,B1:1,B0:1>>);
coils_to_bin([], Acc) ->
    Acc;
coils_to_bin(Bits, Acc) ->
    M = length(Bits),
    [B0,B1,B2,B3,B4,B5,B6,B7] = Bits++lists:duplicate(8-M, 0),
    <<Acc/binary, B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1>>.
