%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2026, Tony Rogvall
%%% @doc
%%%    Solinteg Modbus access
%%%
%%%    Register/status definitions translated from
%%%    doc/solinteg_modbus_protocol_mht-25-50.pdf
%%%      Table 3.1  RO registers
%%%      Table 3.2  Device model info
%%%      Table 3.3  Troubleshooting (fault flags)
%%% @end
%%% Created :  24 Jun 2026 by Tony Rogvall <tony@rogvall.se>

-module(modbus_solinteg).

-compile(export_all).

-record(item,
	{
	 label   :: atom(),
	 unit_id :: integer(),
	 address :: integer(),
	 type :: uint16|uint32|le_uint32|int16|int32|le_int32|
		 float|le_float|{string,integer()}|
		 {enum,atom(),list()}|{bits,atom(),list()},
	 scale :: undefined | number() | {using,Address::integer()},
	 unit  :: undefined | string(),
	 description
	}).

-define(BASE, 10000).
-define(ADDR(I), (?BASE+(I))).

open(Host) ->
    modbus_tcp_client:start_link([{port,502},{host,Host}]).

close(Pid) ->
    modbus_tcp_client:stop(Pid).

info_items() ->
    [
     #item { label = device_sn,
	     address = ?ADDR(0),
	     type    = {string,16}
	   },
     #item { label = inverter_model,
	     address = ?ADDR(8),
	     type = uint16
	   },
     #item { label = firmware_version,  %% <maj>.<min>.<rel>.<patch>
	     address = ?ADDR(11),
	     type    = uint32
	   },
     #item { label = date_yymm,  %% 0xYYMM
	     address = ?ADDR(100),
	     type    = uint16
	   },
     #item { label = date_ddhh,  %% 0xDDHH
	     address = ?ADDR(101),
	     type    = uint16
	   },
     #item { label = date_mmss, %% 0xMMSS
	     address = ?ADDR(102),
	     type    = uint16
	   },
     #item { label = safety_code,
	     address = ?ADDR(104),
	     type    = uint16
	   }
    ].

%% Table 3.1 (10105 - 10121): Inverter working status and fault flags.
status_items() ->
    [
     #item { label = inverter_working_status,
	     description = "Inverter Working Status",
	     address = ?ADDR(105),
	     type    = {enum,uint16,
			[{wait_grid,       0, "Wait for grid connection"},
			 {self_checking,   1, "Self-checking"},
			 {on_grid,         2, "On-grid generating"},
			 {device_fault,    3, "Device fault"},
			 {firmware_upgrade,4, "Firmware upgrade"},
			 {off_grid,        5, "Off-grid generating"}
			]},
	     unit = ""  %% must have a unit to be displayed
	   },
     #item { label = fault_flag1,
	     description = "Fault FLAG1",
	     address = ?ADDR(112),
	     type = {bits,uint32,fault_flag1_bits()}
	   },
     #item { label = fault_flag2,
	     description = "Fault FLAG2",
	     address = ?ADDR(114),
	     type = {bits,uint32,fault_flag2_bits()}
	   },
     #item { label = fault_flag3,
	     description = "Fault FLAG3",
	     address = ?ADDR(120),
	     type = {bits,uint32,fault_flag3_bits()}
	   }
    ].

%% Table 3.1 (18000): ARM fault flag (separate register block)
arm_fault_items() ->
    [
     #item { label = arm_fault_flag1,
	     description = "ARM Fault FLAG1",
	     address = ?ADDR(8000),
	     type = {bits,uint32,arm_fault_flag1_bits()}
	   }
    ].

%% Table 3.3 Troubleshooting, Fault FLAG1 @10112
fault_flag1_bits() ->
    [{16#00000001, "Mains Lost"},
     {16#00000002, "Grid Voltage Fault"},
     {16#00000004, "Grid Frequency Fault"},
     {16#00000008, "DCI Fault"},
     {16#00000010, "ISO Over Limitation"},
     {16#00000020, "GFCI Fault"},
     {16#00000040, "PV Over Voltage"},
     {16#00000080, "Bus Voltage Fault"},
     {16#00000100, "Inverter Over Temperature"}].

%% Table 3.3 Troubleshooting, Fault FLAG2 @10114
fault_flag2_bits() ->
    [{16#00000002, "SPI Fault"},
     {16#00000004, "E2 Fault"},
     {16#00000008, "GFCI Device Fault"},
     {16#00000010, "AC Transducer Fault"},
     {16#00000020, "Relay Check Fail"},
     {16#00000040, "Internal Fan Fault"},
     {16#00000080, "External Fan Fault"}].

%% Table 3.3 Troubleshooting, Fault FLAG3 @10120
fault_flag3_bits() ->
    [{16#00000001, "Bus Hardware Fault"},
     {16#00000002, "PV Power Low"},
     {16#00000004, "Battery Voltage Fault"},
     {16#00000008, "BAK Voltage Fault"},
     {16#00000010, "Bus Voltage Lower"},
     {16#00000020, "Sys Hardware Fault"},
     {16#00000040, "BAK Over Power"},
     {16#00000080, "Inverter Over Voltage"},
     {16#00000100, "Inverter Over Freq"},
     {16#00000200, "Inverter Over Current"},
     {16#00000400, "Phase Order Err"}].

%% Table 3.3 Troubleshooting, ARM Fault FLAG1 @18000
arm_fault_flag1_bits() ->
    [{16#00000001, "SCI Fault"},
     {16#00000002, "FLASH Fault"},
     {16#00000004, "Meter Comm Fault"}].

%% Table 3.1 (10994 - 11065): Meter / grid / PV measurements.
%% Read as one contiguous block (72 registers).
measurement_items() ->
    [
     #item { label = pmeter_a, description = "Pmeter on phase A",
	     address = ?ADDR(994), type = int32, scale = 0.001, unit = "kW" },
     #item { label = pmeter_b, description = "Pmeter on phase B",
	     address = ?ADDR(996), type = int32, scale = 0.001, unit = "kW" },
     #item { label = pmeter_c, description = "Pmeter on phase C",
	     address = ?ADDR(998), type = int32, scale = 0.001, unit = "kW" },
     #item { label = pmeter_total, description = "Pmeter of three phases",
	     address = ?ADDR(1000), type = int32, scale = 0.001, unit = "kW" },
     #item { label = grid_injection_total,
	     description = "Total Grid-Injection Energy on Meter",
	     address = ?ADDR(1002), type = uint32, scale = 0.01, unit = "kWh" },
     #item { label = grid_purchase_total,
	     description = "Total Purchasing Energy from Grid on Meter",
	     address = ?ADDR(1004), type = uint32, scale = 0.01, unit = "kWh" },
     #item { label = voltage_ab, description = "AB line voltage",
	     address = ?ADDR(1006), type = uint16, scale = 0.1, unit = "V" },
     #item { label = voltage_bc, description = "BC line voltage",
	     address = ?ADDR(1007), type = uint16, scale = 0.1, unit = "V" },
     #item { label = voltage_ca, description = "CA line voltage",
	     address = ?ADDR(1008), type = uint16, scale = 0.1, unit = "V" },
     #item { label = voltage_a, description = "Phase A Voltage",
	     address = ?ADDR(1009), type = uint16, scale = 0.1, unit = "V" },
     #item { label = current_a, description = "Phase A Current",
	     address = ?ADDR(1010), type = uint16, scale = 0.1, unit = "A" },
     #item { label = voltage_b, description = "Phase B Voltage",
	     address = ?ADDR(1011), type = uint16, scale = 0.1, unit = "V" },
     #item { label = current_b, description = "Phase B Current",
	     address = ?ADDR(1012), type = uint16, scale = 0.1, unit = "A" },
     #item { label = voltage_c, description = "Phase C Voltage",
	     address = ?ADDR(1013), type = uint16, scale = 0.1, unit = "V" },
     #item { label = current_c, description = "Phase C Current",
	     address = ?ADDR(1014), type = uint16, scale = 0.1, unit = "A" },
     #item { label = grid_frequency, description = "Grid Frequency",
	     address = ?ADDR(1015), type = uint16, scale = 0.01, unit = "Hz" },
     #item { label = p_ac, description = "P_AC",
	     address = ?ADDR(1016), type = int32, scale = 0.001, unit = "kW" },
     #item { label = energy_today, description = "Energy-today",
	     address = ?ADDR(1018), type = uint32, scale = 0.1, unit = "kWh" },
     #item { label = energy_total, description = "Energy-total",
	     address = ?ADDR(1020), type = uint32, scale = 0.1, unit = "kWh" },
     #item { label = generation_hours, description = "Total Generation Hours",
	     address = ?ADDR(1022), type = uint32, scale = 1, unit = "h" },
     #item { label = pv_input_power, description = "Total PV Input Power",
	     address = ?ADDR(1028), type = uint32, scale = 0.001, unit = "kW" },
     #item { label = temp1, description = "Temp.1",
	     address = ?ADDR(1032), type = int16, scale = 0.1, unit = "C" },
     #item { label = temp2, description = "Temp.2",
	     address = ?ADDR(1033), type = int16, scale = 0.1, unit = "C" },
     #item { label = temp3, description = "Temp.3",
	     address = ?ADDR(1034), type = int16, scale = 0.1, unit = "C" },
     #item { label = temp4, description = "Temp.4",
	     address = ?ADDR(1035), type = int16, scale = 0.1, unit = "C" },
     #item { label = pv1_voltage, description = "PV1 Voltage",
	     address = ?ADDR(1038), type = uint16, scale = 0.1, unit = "V" },
     #item { label = pv1_current, description = "PV1 Current",
	     address = ?ADDR(1039), type = uint16, scale = 0.1, unit = "A" },
     #item { label = pv2_voltage, description = "PV2 Voltage",
	     address = ?ADDR(1040), type = uint16, scale = 0.1, unit = "V" },
     #item { label = pv2_current, description = "PV2 Current",
	     address = ?ADDR(1041), type = uint16, scale = 0.1, unit = "A" },
     #item { label = pv1_input_power, description = "PV1 Input Power",
	     address = ?ADDR(1062), type = uint32, scale = 0.001, unit = "kW" },
     #item { label = pv2_input_power, description = "PV2 Input Power",
	     address = ?ADDR(1064), type = uint32, scale = 0.001, unit = "kW" }
    ].

%% Table 3.1 (30254 - 30259): Battery measurements.
battery_items() ->
    [
     #item { label = battery_voltage, description = "Battery Voltage",
	     address = ?ADDR(20254), type = uint16, scale = 0.1, unit = "V" },
     #item { label = battery_current, description = "Battery Current",
	     address = ?ADDR(20255), type = int16, scale = 0.1, unit = "A" },
     #item { label = battery_mode, description = "Battery Mode",
	     address = ?ADDR(20256),
	     type = {enum,uint16,
		     [{discharge, 0, "Discharge"},
		      {charge,    1, "Charge"}]},
	     unit = "" },
     #item { label = battery_power, description = "Battery Power",
	     address = ?ADDR(20258), type = int32, scale = 0.001, unit = "kW" }
    ].

%% Table 3.1 (33000 - 33019): BMS status.
bms_items() ->
    [
     #item { label = soc, description = "SOC",
	     address = ?ADDR(23000), type = uint16, scale = 0.01, unit = "%" },
     #item { label = soh, description = "SOH",
	     address = ?ADDR(23001), type = uint16, scale = 0.01, unit = "%" },
     #item { label = bms_status, description = "BMS Status",
	     address = ?ADDR(23002), type = uint16, unit = "" },
     #item { label = bms_pack_temp, description = "BMS Pack Temperature",
	     address = ?ADDR(23003), type = uint16, scale = 0.1, unit = "C" },
     #item { label = bms_error_code, description = "BMS ERROR CODE",
	     address = ?ADDR(23016), type = uint32, unit = "" },
     #item { label = bms_warn_code, description = "BMS WARN CODE",
	     address = ?ADDR(23018), type = uint32, unit = "" }
    ].

%% Table 3.2 Device Model Info  (10008: high byte = type, low byte = index)
-define(MC(H,L), (((H) bsl 8) bor (L))).

decode_model(?MC(30,00)) -> "MHT-4K-25";
decode_model(?MC(31,00)) -> "MHS-3K-30D";
decode_model(?MC(32,00)) -> "MHT-25K-100";
decode_model(?MC(30,01)) -> "MHT-5K-25";
decode_model(?MC(31,01)) -> "MHS-3.6K-30D";
decode_model(?MC(32,01)) -> "MHT-30K-100";
decode_model(?MC(30,02)) -> "MHT-6K-25";
decode_model(?MC(31,02)) -> "MHS-4.2K-30D";
decode_model(?MC(32,02)) -> "MHT-36K-100";
decode_model(?MC(30,03)) -> "MHT-8K-25";
decode_model(?MC(31,03)) -> "MHS-4.6K-30D";
decode_model(?MC(32,03)) -> "MHT-40K-100";
decode_model(?MC(30,04)) -> "MHT-10K-25";
decode_model(?MC(31,04)) -> "MHS-5K-30D";
decode_model(?MC(32,04)) -> "MHT-50K-100";
decode_model(?MC(30,05)) -> "MHT-12K-25";
decode_model(?MC(31,05)) -> "MHS-6K-30D";
decode_model(?MC(30,06)) -> "MHT-10K-40";
decode_model(?MC(31,06)) -> "MHS-7K-30D";
decode_model(?MC(30,07)) -> "MHT-12K-40";
decode_model(?MC(31,07)) -> "MHS-8K-30D";
decode_model(?MC(30,08)) -> "MHT-15K-40";
decode_model(?MC(31,08)) -> "MHS-3K-30S";
decode_model(?MC(30,09)) -> "MHT-20K-40";
decode_model(?MC(31,09)) -> "MHS-3.6K-30S";
decode_model(?MC(40,00)) -> "MRT-4K-25";
decode_model(?MC(41,00)) -> "MRS-3K-30";
decode_model(?MC(42,00)) -> "MRT-25K-100";
decode_model(?MC(40,01)) -> "MRT-5K-25";
decode_model(?MC(41,01)) -> "MRS-3.6K-30";
decode_model(?MC(42,01)) -> "MRT-30K-100";
decode_model(?MC(40,02)) -> "MRT-6K-25";
decode_model(?MC(41,02)) -> "MRS-4.2K-30";
decode_model(?MC(42,02)) -> "MRT-36K-100";
decode_model(?MC(40,03)) -> "MRT-8K-25";
decode_model(?MC(41,03)) -> "MRS-4.6K-30";
decode_model(?MC(42,03)) -> "MRT-40K-100";
decode_model(?MC(40,04)) -> "MRT-10K-25";
decode_model(?MC(41,04)) -> "MRS-5K-30";
decode_model(?MC(42,04)) -> "MRT-50K-100";
decode_model(?MC(40,05)) -> "MRT-12K-25";
decode_model(?MC(41,05)) -> "MRS-6K-30";
decode_model(?MC(40,06)) -> "MRT-10K-40";
decode_model(?MC(41,06)) -> "MRS-7K-30";
decode_model(?MC(40,07)) -> "MRT-12K-40";
decode_model(?MC(41,07)) -> "MRS-8K-30";
decode_model(?MC(40,08)) -> "MRT-15K-40";
decode_model(?MC(40,09)) -> "MRT-20K-40";
decode_model(_) -> "N/A".


read_info(Pid, UnitId) ->
    read_items(Pid, UnitId, info_items()).

show_info(Pid, UnitId) ->
    Items = read_items(Pid, UnitId, info_items()),
    show_info_(Items).

show_info_([{device_sn,_,Sn}|Info]) ->
    io:format("Device SN: ~p\n", [Sn]),
    show_info_(Info);
show_info_([{inverter_model,_,Model}|Info]) ->
    io:format("Model: ~p\n", [decode_model(Model)]),
    show_info_(Info);
show_info_([{firmware_version,_,Version}|Info]) ->
    <<Maj,Min,Rel,Pat>> = <<Version:32>>,
    io:format("Version: ~w.~w.~w.~w\n", [Maj, Min, Rel, Pat]),
    show_info_(Info);
show_info_([{date_yymm,_,YYMM},{date_ddhh,_,DDHH},{date_mmss,_,MMSS}|Info]) ->
    {YYYY0, _, _} = date(),
    YYYY = ((YYYY0 div 100)*100) + (YYMM bsr 8),
    io:format("Date: ~4..0w-~2..0w-~2..0w\n", [YYYY, (YYMM band 16#ff),
					     (DDHH bsr 8)]),
    io:format("Time: ~2..0w:~2..0w:~2..0w\n", [DDHH band 16#ff,
					     MMSS bsr 8,
					     MMSS band 16#ff]),
    show_info_(Info);
show_info_([{safety_code,_,Code}|Info]) ->
    io:format("Code: ~w\n", [Code]),
    show_info_(Info);
show_info_([{Item,_,Value}|Info]) ->
    io:format("~s: ~p\n", [Item, Value]),
    show_info_(Info);
show_info_([]) ->
    ok.

read_status(Pid, UnitId) ->
    read_items(Pid, UnitId, status_items()).

show_status(Pid, UnitId) ->
    show_items(Pid, UnitId, status_items()).

read_arm_fault(Pid, UnitId) ->
    read_items(Pid, UnitId, arm_fault_items()).

show_arm_fault(Pid, UnitId) ->
    show_items(Pid, UnitId, arm_fault_items()).

read_measurements(Pid, UnitId) ->
    read_items(Pid, UnitId, measurement_items()).

show_measurements(Pid, UnitId) ->
    show_items(Pid, UnitId, measurement_items()).

read_battery(Pid, UnitId) ->
    read_items(Pid, UnitId, battery_items()).

show_battery(Pid, UnitId) ->
    show_items(Pid, UnitId, battery_items()).

read_bms(Pid, UnitId) ->
    read_items(Pid, UnitId, bms_items()).

show_bms(Pid, UnitId) ->
    show_items(Pid, UnitId, bms_items()).

%% Read/show everything (each block is a separate modbus read)
show_all(Pid, UnitId) ->
    show_info(Pid, UnitId),
    show_status(Pid, UnitId),
    show_arm_fault(Pid, UnitId),
    show_measurements(Pid, UnitId),
    show_battery(Pid, UnitId),
    show_bms(Pid, UnitId).

read_items(Pid, UnitId, Items) ->
    ItemAddrLen = [{I#item.address,item_size(I#item.type)} ||
		      I <- Items],
    StartAddr = lists:min([Addr || {Addr,_Len} <- ItemAddrLen]),
    StopAddr = lists:max([Addr+Len-1 || {Addr,Len} <- ItemAddrLen]),
    N = (StopAddr-StartAddr+1),  %% in number of 16 bit registers

    io:format("Read ~w items from address ~w unit=~w\n",
	      [ N, StartAddr, UnitId]),
    {ok,Regs} = modbus:read_holding_registers(Pid, UnitId, StartAddr, N),
    Bin = << <<X:16>> || X <- Regs >>,
    decode_items(Items, StartAddr, Bin).

show_items(Pid, UnitId, Items) ->
    Values = read_items(Pid, UnitId, Items),
    show_items_list(Items, Values).

show_items_list(Items, Values) ->
    Map = lists:foldl(
      fun({_Label,Addr,Value}, Mi) ->
	      Mi# { Addr => Value }
      end, #{}, Values),
    lists:foreach(
      fun(Item) ->
	      case maps:get(Item#item.address, Map, undefined) of
		  {faults, Faults} ->
		      show_faults(Item#item.description, Faults);
		  Value when Item#item.unit =/= undefined ->
		      show_value(Item, Value, Map);
		  _ ->
		      ok
	      end
      end, Items),
    ok.

show_value(Item, Value, Map) ->
    Scale = case Item#item.scale of
		{using,SAddr} ->
		    Pow10 = maps:get(SAddr, Map),
		    math:pow(10, Pow10);
		Sc when is_number(Sc) ->
		    Sc;
		undefined ->
		    1
	    end,
    if is_float(Value); is_float(Scale) ->
	    io:format("~s = ~.2f~s\n",
		      [Item#item.description, Value*Scale, Item#item.unit]);
       is_integer(Value), is_integer(Scale) ->
	    io:format("~s = ~w~s\n",
		      [Item#item.description, Value*Scale, Item#item.unit]);
       is_atom(Value); is_list(Value), Scale =:= 1 ->
	    io:format("~s = ~s~s\n",
		      [Item#item.description, Value, Item#item.unit]);
       true ->
	    ignore
    end.

show_faults(Desc, []) ->
    io:format("~s = OK\n", [Desc]);
show_faults(Desc, Faults) ->
    io:format("~s = ~s\n", [Desc, string:join(Faults, ", ")]).

decode_items([#item { label = L, address = Addr, type = Type } | Items],
	     StartAddr, Bin) ->
    Offset = Addr - StartAddr,
    Value = decode_item(Type,Offset,Bin),
    [{L, Addr, Value} | decode_items(Items, StartAddr, Bin)];
decode_items([], _StartAddress, _Bin) ->
    [].

decode_item(Type,Offset,Bin) ->
    case Type of
	{enum,BaseType,Enums} ->
	    Value = decode_item(BaseType,Offset,Bin),
	    case lists:keyfind(Value,2,Enums) of
		false -> Value;
		{Enum,_,_Description} -> Enum
	    end;
	{bits,BaseType,Bits} ->
	    Value = decode_item(BaseType,Offset,Bin),
	    {faults, [Desc || {Bit,Desc} <- Bits, (Value band Bit) =/= 0]};
	uint16 ->
	    <<_:Offset/unit:16, Value:16, _/binary>> = Bin,
	    Value;
	uint32 ->
	    <<_:Offset/unit:16, Value:32, _/binary>> = Bin,
	    Value;
	le_uint32 -> %% the uint32 is little endian 16 bit swapped!!! wow
	    <<_:Offset/unit:16, A:2/binary,B:2/binary, _/binary>> = Bin,
	    <<Value:32>> = <<B:2/binary,A:2/binary>>,
	    Value;
	acc32 -> %% le_uint32
	    <<_:Offset/unit:16, A:2/binary,B:2/binary, _/binary>> = Bin,
	    <<Value:32>> = <<B:2/binary,A:2/binary>>,
	    Value;
	int16 ->
	    <<_:Offset/unit:16, Value:16/signed, _/binary>> = Bin,
	    Value;
	int32 ->
	    <<_:Offset/unit:16, Value:32/signed, _/binary>> = Bin,
	    Value;
	le_int32 ->
	    <<_:Offset/unit:16, A:2/binary,B:2/binary, _/binary>> = Bin,
	    <<Value:32/signed>> = <<B:2/binary,A:2/binary>>,
	    Value;
	float32 ->
	    <<_:Offset/unit:16, Value:32/float, _/binary>> = Bin,
	    Value;
	le_float32 ->
	    <<_:Offset/unit:16, A:2/binary,B:2/binary, _/binary>> = Bin,
	    <<Value:32/float>> = <<B:2/binary,A:2/binary>>,
	    Value;
	{string,N} ->
	    <<_:Offset/unit:16, Value:N/binary, _/binary>> = Bin,
	    decode_string(Value)
    end.

decode_string(<<0,_/binary>>) -> [];
decode_string(<<C,Cs/binary>>) -> [C|decode_string(Cs)];
decode_string(<<>>) -> [].

scale_value(Value, Pow10) ->
    Value * math:pow(10, Pow10).

%% return item size in number of uint16!
item_size(uint16) -> 1;
item_size(uint32) -> 2;
item_size(le_uint32) -> 2;
item_size(int16) ->  1;
item_size(int32) ->  2;
item_size(le_int32) ->  2;
item_size(acc32) ->  2;  %% increasing uint32...
item_size({string,N}) -> (N+1) div 2;
item_size(float32) -> 2;
item_size(le_float32) -> 2;
item_size({enum,BaseType,_Description}) -> item_size(BaseType);
item_size({bits,BaseType,_Bits}) -> item_size(BaseType).
