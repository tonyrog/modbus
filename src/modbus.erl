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

%% preset unit id
-export([read_discrete_inputs/3,
	 read_coils/3,
	 write_single_coil/3,
	 write_multiple_coils/3,
	 read_input_registers/3,
	 read_holding_registers/3,
	 write_single_holding_register/3,
	 write_multiple_holding_registers/3]).

%%unit id as parameter
-export([read_discrete_inputs/4,
	 read_coils/4,
	 write_single_coil/4,
	 write_multiple_coils/4,
	 read_input_registers/4,
	 read_holding_registers/4,
	 write_single_holding_register/4,
	 write_multiple_holding_registers/4]).

-export([bits_to_coils/2]).
-export([coils_to_bin/1]).

-include("../include/modbus.hrl").
	 	 
read_discrete_inputs(Pid, Addr, N)
  when is_integer(Addr), Addr >= 0 ->
    send_read_pdu(Pid, {pdu, ?READ_DISCRETE_INPUTS,
			<<Addr:16, N:16>>}, N).
read_discrete_inputs(Pid, UnitId, Addr, N)
  when is_integer(UnitId), is_integer(Addr), Addr >= 0 ->
    send_read_pdu(Pid, {pdu, UnitId, ?READ_DISCRETE_INPUTS,
			<<Addr:16, N:16>>}, N).

read_coils(Pid, Addr, N)
  when is_integer(Addr), Addr >= 0 ->
    send_read_pdu(Pid, {pdu, ?READ_COILS, <<Addr:16, N:16>>}, N).
read_coils(Pid, UnitId, Addr, N)
  when is_integer(UnitId), is_integer(Addr), Addr >= 0 ->
    send_read_pdu(Pid, {pdu, UnitId, ?READ_COILS, <<Addr:16, N:16>>}, N).

write_single_coil(Pid, Addr, Value)
  when is_integer(Addr), Addr >= 0,
       is_integer(Value), Value >= 0 ->
    Value1 = if Value =/= 0 -> 16#FF00; true -> 0 end,
    send_write_pdu(Pid, {pdu, ?WRITE_SINGLE_COIL, 
			 <<Addr:16, Value1:16>>}, 
		   Addr, Value1).
write_single_coil(Pid, UnitId, Addr, Value)
  when is_integer(UnitId), 
       is_integer(Addr), Addr >= 0,
       is_integer(Value), Value >= 0 ->
    Value1 = if Value =/= 0 -> 16#FF00; true -> 0 end,
    send_write_pdu(Pid, {pdu, UnitId, ?WRITE_SINGLE_COIL,
			 <<Addr:16, Value1:16>>}, 
		   Addr, Value1).

write_multiple_coils(Pid, Addr, BitList)
  when is_integer(Addr), Addr >= 0 -> 
    {N, Data, M} = bitlist_to_params(BitList),
    send_write_pdu(Pid, {pdu, ?WRITE_MULTIPLE_COILS, 
			 <<Addr:16, N:16, M, Data/binary>>},
		   Addr, N).
write_multiple_coils(Pid, UnitId, Addr, BitList)
  when is_integer(Addr), Addr >= 0 -> 
    {N, Data, M} = bitlist_to_params(BitList),
    send_write_pdu(Pid, {pdu, UnitId, ?WRITE_MULTIPLE_COILS, 
			 <<Addr:16, N:16, M, Data/binary>>},
		   Addr, N).

read_input_registers(Pid, Addr, N)
  when is_integer(Addr), Addr >= 0 ->
    send_read_reg_pdu(Pid, {pdu, ?READ_INPUT_REGISTERS, <<Addr:16, N:16>>}).
read_input_registers(Pid, UnitId, Addr, N)
  when is_integer(UnitId), is_integer(Addr), Addr >= 0 ->
    send_read_reg_pdu(Pid, {pdu, UnitId, ?READ_INPUT_REGISTERS, 
			    <<Addr:16, N:16>>}).

read_holding_registers(Pid, Addr, N)
  when is_integer(Addr), Addr >= 0 ->
    send_read_reg_pdu(Pid, {pdu, ?READ_HOLDING_REGISTERS, <<Addr:16, N:16>>}).
read_holding_registers(Pid, UnitId, Addr, N)
  when is_integer(UnitId), is_integer(Addr), Addr >= 0 ->
    send_read_reg_pdu(Pid, {pdu, UnitId, ?READ_HOLDING_REGISTERS, 
			    <<Addr:16, N:16>>}).

write_single_holding_register(Pid, Addr, Value)
  when is_integer(Addr), Addr >= 0 ->
    send_write_pdu(Pid, {pdu, ?WRITE_SINGLE_HOLDING_REGISTER,
			 <<Addr:16, Value:16>>}, 
		   Addr, Value).
write_single_holding_register(Pid, UnitId, Addr, Value)
  when is_integer(UnitId), is_integer(Addr), Addr >= 0 ->
    send_write_pdu(Pid, {pdu, UnitId, ?WRITE_SINGLE_HOLDING_REGISTER,
			 <<Addr:16, Value:16>>}, 
		   Addr, Value).

write_multiple_holding_registers(Pid, Addr, Values)
  when is_integer(Addr), Addr >= 0 ->
    {N, Data, M} = valuelist_to_params(Values),
    send_write_pdu(Pid, {pdu, ?WRITE_MULTIPLE_HOLDING_REGISTERS,
			 <<Addr:16,N:16,M,Data/binary>>}, 
		   Addr, N).
write_multiple_holding_registers(Pid, UnitId, Addr, Values)
  when is_integer(UnitId), is_integer(Addr), Addr >= 0 ->
    {N, Data, M} = valuelist_to_params(Values),
    send_write_pdu(Pid, {pdu, UnitId, ?WRITE_MULTIPLE_HOLDING_REGISTERS,
			 <<Addr:16,N:16,M,Data/binary>>}, 
		   Addr, N).

send_read_pdu(Pid, Msg, N) ->
    case gen_server:call(Pid, Msg) of
	{ok, <<Len,Bin:Len/binary>>} when N =< Len*8 ->
	    {ok, bits_to_coils(N, Bin)};
	Error ->
	    Error
    end.

send_write_pdu(Pid, Msg, Address, Value) ->
    case gen_server:call(Pid, Msg) of
	{ok, <<Address:16, Value:16>>} ->
	    ok;
	Error -> Error
    end.

send_read_reg_pdu(Pid, Msg) ->
    case gen_server:call(Pid, Msg) of
	{ok, <<Len, RegData:Len/binary>>} ->
	    {ok, [ Reg || <<Reg:16>> <= RegData ]};
	Error ->
	    Error
    end.

bitlist_to_params(BitList) ->
    N = length(BitList),
    Data = coils_to_bin(BitList),
    M = byte_size(Data),
    {N, Data, M}.

valuelist_to_params(Values) ->
    N = length(Values),
    Data = << <<V:16>> || V <- Values >>,
    M = byte_size(Data),
    {N, Data, M}.


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
