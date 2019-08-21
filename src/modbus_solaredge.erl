%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    SolarEdge Modbus access
%%% @end
%%% Created :  5 Aug 2019 by Tony Rogvall <tony@rogvall.se>

-module(modbus_solaredge).

-compile(export_all).

-record(item,
	{
	 label   :: atom(),
	 unit_id :: integer(),
	 address :: integer(),
	 type :: uint16|uint32|le_uint32|int16|int32|le_int32|
		 float|le_float|{string,integer()},
	 scale :: undefined | number() | {using,Address::integer()},
	 unit  :: undefined | string(),
	 description
	}).

-define(BASE, 40000-1).
-define(ADDR(I), (?BASE+(I))).

open() ->
    open("192.168.2.89").

open(Host) ->
    modbus_tcp_client:start_link([{port,1502},{host,Host}]).

info_items() ->
    [
     #item { label = c_sun_spec_id,
	     address = ?ADDR(1),
	     type    = le_uint32         %% size = 2 (2xuint16)
	   },
     #item { label = c_sun_spec_did,
	     address = ?ADDR(3),
	     type    = uint16
	   },
     #item { label   = c_sun_spec_length,
	     address = ?ADDR(4),
	     type    = uint16
	   },
     #item { label   = c_manufacturer,
	     address = ?ADDR(5),
	     type    = {string,32}
	   },
     #item { label   = c_model,
	     address = ?ADDR(21),
	     type    = {string,32}
	   },
     #item { label   = c_version,
	     address = ?ADDR(45),
	     type    = {string,16}
	   },
     #item { label   = c_serial_number,
	     address = ?ADDR(53),
	     type    = {string,32}
	   },
     #item { label   = c_device_address,
	     address = ?ADDR(69),
	     type    = uint16,
	     description = "MODBUS Unit ID"
	   }
    ].

status_items() ->
    [
     #item { label = c_sun_spec_did,
	     address = ?ADDR(70),
	     type    = uint16,
	     description = "101=single, 102=split, 103=three phase"
	   },
     #item { label = c_sun_spec_length,
	     address = ?ADDR(71),
	     type    = uint16,
	     description = "Length of model block = 50"
	   },
     #item { label = i_ac_current,
	     description = "AC Total Current",
	     address = ?ADDR(72),
	     type    = uint16,
	     scale   = {using, ?ADDR(76)},
	     unit    = "A"
	   },
     #item { label = i_ac_current_a,
	     description = "AC Phase A Current",
	     address = ?ADDR(73),
	     type    = uint16,
	     scale   = {using, ?ADDR(76)},
	     unit    = "A"
	   },
     #item { label = i_ac_current_b,
	     description = "AC Phase B Current value",
	     address = ?ADDR(74),
	     type    = uint16,
	     scale   = {using, ?ADDR(76)},
	     unit    = "A"
	   },
     #item { label = i_ac_current_c,
	     description = "AC Phase C Current",
	     address = ?ADDR(75),
	     type    = uint16,
	     scale   = {using, ?ADDR(76)},
	     unit    = "A"
	   },

     #item { label = i_ac_current_sf,
	     description = "AC Current scale factor",
	     address = ?ADDR(76),
	     type    = int16
	   },
     
     #item { label = i_ac_voltage_ab,
	     description = "AC Voltage Phase A-B",
	     address = ?ADDR(77),
	     type    = uint16,
	     scale   = {using,?ADDR(83)},
	     unit    = "V"
	   },
     #item { label = i_ac_voltage_bc,
	     description = "AC Voltage Phase B-C",
	     address = ?ADDR(78),
	     type    = uint16,
	     scale   = {using,?ADDR(83)},
	     unit    = "V"
	   },
     #item { label = i_ac_voltage_ca,
	     description = "AC Voltage Phase C-A",
	     address = ?ADDR(79),
	     type    = uint16,
	     scale   = {using,?ADDR(83)},
	     unit    = "V"
	   },
     #item { label = i_ac_voltage_an,
	     description = "AC Voltage Phase A-N",
	     address = ?ADDR(80),
	     type    = uint16,
	     scale   = {using,?ADDR(83)},
	     unit    = "V"
	   },
     #item { label = i_ac_voltage_bn,
	     description = "AC Voltage Phase B-N",
	     address = ?ADDR(81),
	     type    = uint16,
	     scale   = {using,?ADDR(83)},
	     unit    = "V"
	   },
     #item { label = i_ac_voltage_cn,
	     description = "AC Voltage Phase C-N",
	     address = ?ADDR(82),
	     type    = uint16,
	     scale   = {using,?ADDR(83)},
	     unit    = "V"
	   },
     #item { label = i_ac_voltage_sf,
	     description = "AC Voltage scale factor",
	     address = ?ADDR(83),
	     type    = int16
	   },

     #item { label = i_ac_power,
	     description = "AC Power",
	     address = ?ADDR(84),
	     type    = int16,
	     scale   = {using,?ADDR(85)},
	     unit    = "W"
	   },

     #item { label = i_ac_power_sf,
	     description = "AC Power scale factor",
	     address = ?ADDR(85),
	     type    = int16
	   },

     #item { label = i_ac_frequency,
	     description = "AC Frequency",
	     address = ?ADDR(86),
	     type    = int16,
	     scale   = {using,?ADDR(87)},
	     unit    = "Hz"
	   },

     #item { label = i_ac_frequency_sf,
	     description = "AC Frequency scale factor",
	     address = ?ADDR(87),
	     type    = int16
	   },

     #item { label = i_ac_va,
	     description = "Apparent Power",
	     address = ?ADDR(88),
	     type    = int16,
	     scale   = {using,?ADDR(89)},
	     unit    = "VA"
	   },

     #item { label = i_ac_va_sf,
	     description = "Apparent Power Scale factor",
	     address = ?ADDR(89),
	     type    = int16
	   },

     #item { label = i_ac_var,
	     description = "Reactive Power",
	     address = ?ADDR(90),
	     type    = int16,
	     scale   = {using,?ADDR(91)},
	     unit    = "VAR"
	   },

     #item { label = i_ac_var_sf,
	     description = "Reactive Power Scale factor",
	     address = ?ADDR(91),
	     type    = int16
	   },

     #item { label = i_ac_pf,
	     description = "Power Factor",
	     address = ?ADDR(92),
	     type    = int16,
	     scale   = {using,?ADDR(93)},
	     unit    = "%"
	   },

     #item { label = i_ac_pf_sf,
	     description = "Power Factor Scale factor",
	     address = ?ADDR(93),
	     type    = int16
	   },

     #item { label = i_ac_energy_wh,
	     description = "AC Lifetime Energy production",
	     address = ?ADDR(94),
	     type    = acc32,
	     scale   = {using,?ADDR(96)},
	     unit    = "Wh"
	   },

     #item { label = i_ac_energy_wh_sf,
	     description = "AC Lifetime Energy production scale factor",
	     address = ?ADDR(96),
	     type    = uint16
	   },
     
     #item { label = i_dc_current,
	     description = "DC Current",
	     address = ?ADDR(97),
	     type    = uint16,
	     scale   = {using,?ADDR(98)},
	     unit    = "A"
	   },

     #item { label = i_dc_current_sf,
	     description = "DC Current Scale factor",
	     address = ?ADDR(98),
	     type    = int16
	   },

     #item { label = i_dc_voltage,
	     description = "DC Voltage",
	     address = ?ADDR(99),
	     type    = uint16,
	     scale   = {using,?ADDR(100)},
	     unit    = "V"
	   },

     #item { label = i_dc_voltage_sf,
	     description = "DC Voltage value Scale factor",
	     address = ?ADDR(100),
	     type    = int16
	   },

     #item { label = i_dc_power,
	     description = "DC Power value",
	     address = ?ADDR(101),
	     type    = int16,
	     scale   = {using,?ADDR(102)},
	     unit    = "W"
	   },

     #item { label = i_dc_power_sf,
	     description = "DC Power value scale factor",
	     address = ?ADDR(102),
	     type    = int16
	   },
     %% 103?
     #item { label = i_temp_sink,
	     description = "Head Sink Temperature",
	     address = ?ADDR(104),
	     type    = int16,
	     scale   = {using,?ADDR(107)},
	     unit    = "C"
	   },
     %% 105, 106?
     #item { label = i_temp_sf,
	     description = "Temperature scale factor",
	     address = ?ADDR(107),
	     type    = int16
	   },

     #item { label = i_status,
	     description = "Operating State",
	     address = ?ADDR(108),
	     type    = {enum,uint16,
			[{off,1,"Off"},
			 {sleeping,2,"Sleeping - night mode"},
			 {starting,3,"Grid Monitoring/wake-up"},
			 {mppt,4,"Inverter is ON and producing power"},
			 {throttled,5,"Production (curtailed)"},
			 {shutting_down,6,"Shutting down"},
			 {fault,7,"Fault"},
			 {standby,8,"Mainenance/setup"}]},
	     unit = "" %% must have a unit to be displayed
	   },
     #item { label = i_status_vendor,
	     description = "Vendor specific Operating State",
	     address = ?ADDR(109),
	     type    = uint16,
	     unit = "" %% must have a unit to be displayed
	   }
    ].


read_info(Pid, UnitId) ->
    read_items(Pid, UnitId, info_items()).

read_status(Pid, UnitId) ->
    read_items(Pid, UnitId, status_items()).

show_status(Pid, UnitId) ->
    show_items(Pid, UnitId, status_items()).

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
    Map = lists:foldl(
      fun({_Label,Addr,Value}, Mi) ->
	      Mi# { Addr => Value }
      end, #{}, read_items(Pid, UnitId, Items)),
    lists:foreach(
      fun(Item) when Item#item.unit =/= undefined ->
	      Scale = case Item#item.scale of
			  {using,SAddr} ->
			      Pow10 = maps:get(SAddr, Map),
			      math:pow(10, Pow10);
			  Sc when is_number(Sc) ->
			      Sc;
			  undefined ->
			      1
		      end,
	      Value = maps:get(Item#item.address, Map),
	      if is_float(Value); is_float(Scale) ->
		      io:format("~s = ~.2f~s\n",
				[Item#item.description, 
				 Value*Scale, Item#item.unit]);
		 is_integer(Value), is_integer(Scale) ->
		      io:format("~s = ~w~s\n",
				[Item#item.description, 
				 Value*Scale, Item#item.unit]);
		 is_atom(Value); is_list(Value), Scale =:= 1 ->
		      io:format("~s = ~s~s\n",
				[Item#item.description, 
				 Value, Item#item.unit]);
		 true ->
		      ignore
	      end;
	 (_Item) ->
	      ok
      end, Items),
    ok.

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
	uint16 ->
	    <<_:Offset/unit:16, Value:16, _/binary>> = Bin,
	    Value;
	uint32 ->
	    <<_:Offset/unit:16, Value:32, _/binary>> = Bin,
	    Value;
	le_uint32 -> %% the uint32 is little endia 16 bit swapped!!! wow
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
item_size({enum,BaseType,_Description}) -> item_size(BaseType).

