%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2023 by Tony Rogvall <tony@rogvall.se>

-module(modbus_nibe).

-type int_type()    :: uint16|uint32|le_uint32|int16|int32|le_int32.
-type float_type()  :: float|le_float.
-type scalar_type() :: int_type() | float_type() | {string,integer()} |
		       {enum,int_type(),[{integer(),string()}]}.

-record(item,
	{
	 label   :: atom(),
	 title   :: string(),
	 unit_id :: integer(),
	 address :: integer(),
	 type :: scalar_type(),
	 scale :: undefined | number() | {using,Address::integer()},
	 min   :: number(),
	 max   :: number(),
	 default :: number(),
	 write :: boolean(),
	 unit  :: undefined | string(),
	 description :: string()
	}).

info_items() ->
    [

    ].

status_items() ->
    [

    ].

items() ->
    [
     #item {
	address = 40004,
	title = "BT1 Outdoor Temperature",
	description = "Current outdoor temperature",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt1-outdoor-temperature-40004"
       },
     #item {
	address = 40005,
	title = "EP23-BT2 Supply temp S4",
	description = "Supply temperature for system 4",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep23-bt2-supply-temp-s4-40005"
       },
     #item {
	address = 40006,
	title = "EP22-BT2 Supply temp S3",
	description = "Supply temperature for system 3",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep22-bt2-supply-temp-s3-40006"
       },
     #item {
	address = 40007,
	title = "EP21-BT2 Supply temp S2",
	description = "Supply temperature for system 2",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep21-bt2-supply-temp-s2-40007"
       },
     #item {
	address = 40008,
	title = "BT2 Supply temp S1",
	description = "Supply temperature for system 1",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt2-supply-temp-s1-40008"
       },
     #item {
	address = 40012,
	title = "EB100-EP14-BT3 Return temp",
	description = "Return temperature",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "eb100-ep14-bt3-return-temp-40012"
       },
     #item {
	address = 40013,
	title = "BT7 HW Top",
	description = "Hot water top temperature, BT7",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt7-hw-top-40013"
       },
     #item {
	address = 40014,
	title = "BT6 HW Load",
	description = "Hot water load temperature, BT6",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt6-hw-load-40014"
       },
     #item {
	address = 40020,
	title = "EB100-BT16 Evaporator temp",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "eb100-bt16-evaporator-temp-40020"
       },
     #item {
	address = 40022,
	title = "EB100-EP14-BT17 Suction",
	description = "Suction temperature, BT17",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "eb100-ep14-bt17-suction-40022"
       },
     #item {
	address = 40023,
	title = "EB100-BT18 Compressor temp.",
	description = "Valid only for F3/470",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "eb100-bt18-compressor-temp-40023"
       },
     #item {
	address = 40024,
	title = "EB100-BT19 Addition temp.",
	description = "Valid only for F3/470",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "eb100-bt19-addition-temp-40024"
       },
     #item {
	address = 40025,
	title = "BT20 Exhaust air temp. 1",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt20-exhaust-air-temp-1-40025"
       },
     #item {
	address = 40026,
	title = "BT21 Vented air temp. 1",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt21-vented-air-temp-1-40026"
       },
     #item {
	address = 40030,
	title = "EP23-BT50 Room Temp S4",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep23-bt50-room-temp-s4-40030"
       },
     #item {
	address = 40031,
	title = "EP22-BT50 Room Temp S3",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep22-bt50-room-temp-s3-40031"
       },
     #item {
	address = 40032,
	title = "EP21-BT50 Room Temp S2",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep21-bt50-room-temp-s2-40032"
       },
     #item {
	address = 40033,
	title = "BT50 Room Temp S1",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt50-room-temp-s1-40033"
       },
     #item {
	address = 40043,
	title = "BT53 Solar Panel Temp",
	description = "Used in Solar and AHPS Docking accessories",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt53-solar-panel-temp-40043"
       },
     #item {
	address = 40044,
	title = "BT54 Solar Load Temp",
	description = "Used in Solar and AHPS Docking accessories",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt54-solar-load-temp-40044"
       },
     #item {
	address = 40054,
	title = "EB100-FD1 Temperature limiter",
	type = int16,
	scale = 1,
	label = "eb100-fd1-temperature-limiter-40054"
       },
     #item {
	address = 40067,
	title = "BT1 Average",
	description = "EB100-BT1 Outdoor temperature average",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt1-average-40067"
       },
     #item {
	address = 40071,
	title = "BT25 Ext. Supply",
	description = "External supply temperature, BT25",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt25-ext-supply-40071"
       },
     #item {
	address = 40074,
	title = "EB100-FR1 Anode Status",
	type = int16,
	scale = 1,
	label = "eb100-fr1-anode-status-40074"
       },
     #item {
	address = 40075,
	title = "BT22 Supply air temp.",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt22-supply-air-temp-40075"
       },
     #item {
	address = 40076,
	title = "EP30-BT55 Solar Tank Top Temp",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep30-bt55-solar-tank-top-temp-40076"
       },
     #item {
	address = 40079,
	title = "EB100-BE3 Current",
	unit = "A",
	type = uint32,
	scale = 10,
	label = "eb100-be3-current-40079"
       },
     #item {
	address = 40081,
	title = "EB100-BE2 Current",
	unit = "A",
	type = uint32,
	scale = 10,
	label = "eb100-be2-current-40081"
       },
     #item {
	address = 40083,
	title = "EB100-BE1 Current",
	unit = "A",
	type = uint32,
	scale = 10,
	label = "eb100-be1-current-40083"
       },
     #item {
	address = 40122,
	title = "BT52 external water heater load temp.",
	description = "Used in DEH and AHPS Docking accessories",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt52-external-water-heater-load-temp-40122"
       },
     #item {
	address = 40127,
	title = "EP23-BT3 Return temp S4",
	description = "Return temperature for system 4",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep23-bt3-return-temp-s4-40127"
       },
     #item {
	address = 40128,
	title = "EP22-BT3 Return temp S3",
	description = "Return temperature for system 3",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep22-bt3-return-temp-s3-40128"
       },
     #item {
	address = 40129,
	title = "EP21-BT3 Return temp S2",
	description = "Return temperature for system 2",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep21-bt3-return-temp-s2-40129"
       },
     #item {
	address = 40159,
	title = "EP47-BT2 Supply temp S8",
	description = "Supply temperature for system 8",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep47-bt2-supply-temp-s8-40159"
       },
     #item {
	address = 40160,
	title = "EP46-BT2 Supply temp S7",
	description = "Supply temperature for system 7",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep46-bt2-supply-temp-s7-40160"
       },
     #item {
	address = 40161,
	title = "EP45-BT2 Supply temp S6",
	description = "Supply temperature for system 6",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep45-bt2-supply-temp-s6-40161"
       },
     #item {
	address = 40162,
	title = "EP44-BT2 Supply temp S5",
	description = "Supply temperature for system 5",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep44-bt2-supply-temp-s5-40162"
       },
     #item {
	address = 40163,
	title = "EP47-BT3 Return temp S8",
	description = "Return temperature for system 8",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep47-bt3-return-temp-s8-40163"
       },
     #item {
	address = 40164,
	title = "EP46-BT3 Return temp S7",
	description = "Return temperature for system 7",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep46-bt3-return-temp-s7-40164"
       },
     #item {
	address = 40165,
	title = "EP45-BT3 Return temp S6",
	description = "Return temperature for system 6",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep45-bt3-return-temp-s6-40165"
       },
     #item {
	address = 40166,
	title = "EP44-BT3 Return temp S5",
	description = "Return temperature for system 5",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep44-bt3-return-temp-s5-40166"
       },
     #item {
	address = 40167,
	title = "EP47-BT50 Room Temp S8",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep47-bt50-room-temp-s8-40167"
       },
     #item {
	address = 40168,
	title = "EP46-BT50 Room Temp S7",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep46-bt50-room-temp-s7-40168"
       },
     #item {
	address = 40169,
	title = "EP45-BT50 Room Temp S6",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep45-bt50-room-temp-s6-40169"
       },
     #item {
	address = 40170,
	title = "EP44-BT50 Room Temp S5",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep44-bt50-room-temp-s5-40170"
       },
     #item {
	address = 40185,
	title = "BT1 Average, 1h",
	description = "EB100-BT1 Outdoor temperature average, 1h",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt1-average-1h-40185"
       },
     #item {
	address = 40188,
	title = "EP47-BT50 Room Temp S8 Average",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep47-bt50-room-temp-s8-average-40188"
       },
     #item {
	address = 40189,
	title = "EP46-BT50 Room Temp S7 Average",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep46-bt50-room-temp-s7-average-40189"
       },
     #item {
	address = 40190,
	title = "EP45-BT50 Room Temp S6 Average",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep45-bt50-room-temp-s6-average-40190"
       },
     #item {
	address = 40191,
	title = "EP44-BT50 Room Temp S5 Average",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep44-bt50-room-temp-s5-average-40191"
       },
     #item {
	address = 40192,
	title = "EP23-BT50 Room Temp S4 Average",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep23-bt50-room-temp-s4-average-40192"
       },
     #item {
	address = 40193,
	title = "EP22-BT50 Room Temp S3 Average",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep22-bt50-room-temp-s3-average-40193"
       },
     #item {
	address = 40194,
	title = "EP21-BT50 Room Temp S2 Average",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "ep21-bt50-room-temp-s2-average-40194"
       },
     #item {
	address = 40195,
	title = "BT50 Room Temp S1 Average",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt50-room-temp-s1-average-40195"
       },
     #item {
	address = 40212,
	title = "BT74 Average",
	description = "BT74 heating cooling sensor average",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt74-average-40212"
       },
     #item {
	address = 40217,
	title = "Calc. Supply S8",
	description = "Calculated supply temperature for the climate system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "calc-supply-s8-40217"
       },
     #item {
	address = 40218,
	title = "Calc. Supply S7",
	description = "Calculated supply temperature for the climate system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "calc-supply-s7-40218"
       },
     #item {
	address = 40219,
	title = "Calc. Supply S6",
	description = "Calculated supply temperature for the climate system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "calc-supply-s6-40219"
       },
     #item {
	address = 40220,
	title = "Calculated Supply S5",
	description = "Calculated supply temperature for the climate system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "calculated-supply-s5-40220"
       },
     #item {
	address = 40305,
	title = "Mixing Valve State S8",
	description = "State of the mixing valve for the climate system",
	type = uint8,
	scale = 1,
	label = "mixing-valve-state-s8-40305"
       },
     #item {
	address = 40306,
	title = "Mixing Valve State S7",
	description = "State of the mixing valve for the climate system",
	type = uint8,
	scale = 1,
	label = "mixing-valve-state-s7-40306"
       },
     #item {
	address = 40307,
	title = "Mixing Valve State S6",
	description = "State of the mixing valve for the climate system",
	type = uint8,
	scale = 1,
	label = "mixing-valve-state-s6-40307"
       },
     #item {
	address = 40308,
	title = "Mixing Valve State S5",
	description = "State of the mixing valve for the climate system",
	type = uint8,
	scale = 1,
	label = "mixing-valve-state-s5-40308"
       },
     #item {
	address = 40339,
	title = "External adjustment activated via input S8",
	type = uint8,
	scale = 1,
	label = "external-adjustment-activated-via-input-s8-40339"
       },
     #item {
	address = 40340,
	title = "External adjustment activated via input S7",
	type = uint8,
	scale = 1,
	label = "external-adjustment-activated-via-input-s7-40340"
       },
     #item {
	address = 40341,
	title = "External adjustment activated via input S6",
	type = uint8,
	scale = 1,
	label = "external-adjustment-activated-via-input-s6-40341"
       },
     #item {
	address = 40342,
	title = "External adjustment activated via input S5",
	type = uint8,
	scale = 1,
	label = "external-adjustment-activated-via-input-s5-40342"
       },
     #item {
	address = 40365,
	title = "Extra heating system pump S8",
	type = uint8,
	scale = 1,
	label = "extra-heating-system-pump-s8-40365"
       },
     #item {
	address = 40366,
	title = "Extra heating system pump S7",
	type = uint8,
	scale = 1,
	label = "extra-heating-system-pump-s7-40366"
       },
     #item {
	address = 40367,
	title = "Extra heating system pump S6",
	type = uint8,
	scale = 1,
	label = "extra-heating-system-pump-s6-40367"
       },
     #item {
	address = 40368,
	title = "Extra heating system pump S5",
	type = uint8,
	scale = 1,
	label = "extra-heating-system-pump-s5-40368"
       },
     #item {
	address = 40755,
	title = "Tot. ext. HW add op.time",
	description = "Total external hw-electric additive operation time",
	unit = "h",
	type = int32,
	scale = 10,
	min = 0.0,
	max = 9999999.0,
	default = 0.0,
	label = "tot-ext-hw-add-op-time-40755"
       },
     #item {
	address = 40868,
	title = "+Adjust AUX Port",
	unit = "1",
	type = uint8,
	scale = 1,
	label = "adjust-aux-port-40868"
       },
     #item {
	address = 40870,
	title = "+Adjust OP mode",
	description = "Tells if plusadjust system calls for heat or cool",
	type = uint8,
	scale = 1,
	label = "adjust-op-mode-40870"
       },
     #item {
	address = 40871,
	title = "+Adjust Comfort mode",
	description = "Which comfort mode",
	type = uint8,
	scale = 1,
	label = "adjust-comfort-mode-40871"
       },
     #item {
	address = 40872,
	title = "+Adjust Parallell adjustment",
	description = "requested adjustment",
	type = int8,
	scale = 1,
	label = "adjust-parallell-adjustment-40872"
       },
     #item {
	address = 40873,
	title = "+Adjust Humidity",
	description = "Humidity",
	unit = "%RH",
	type = int16,
	scale = 10,
	label = "adjust-humidity-40873"
       },
     #item {
	address = 40874,
	title = "+Adjust Temp indoor",
	description = "Average of all room sensors",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "adjust-temp-indoor-40874"
       },
     #item {
	address = 40875,
	title = "+Adjust Temp outdoor",
	description = "Outdoor temp",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "adjust-temp-outdoor-40875"
       },
     #item {
	address = 40876,
	title = "+Adjust Version",
	description = "PCA input version",
	type = uint16,
	scale = 1,
	label = "adjust-version-40876"
       },
     #item {
	address = 40877,
	title = "+Adjust Activated",
	description = "If plusadjust accessory is activated",
	type = uint8,
	scale = 1,
	label = "adjust-activated-40877"
       },
     #item {
	address = 40878,
	title = "+Adjust Need",
	description = "If plusadjust system calls for heat",
	type = uint8,
	scale = 1,
	label = "adjust-need-40878"
       },
     #item {
	address = 40889,
	title = "BT64 Average",
	description = "BT64 Cooling supply average",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt64-average-40889"
       },
     #item {
	address = 41027,
	title = "Humidity average",
	description = "Humidity average",
	unit = "%",
	type = int16,
	scale = 10,
	label = "humidity-average-41027"
       },
     #item {
	address = 41189,
	title = "AA20-BE5 EME10 Current",
	unit = "A",
	type = int16,
	scale = 10,
	label = "aa20-be5-eme10-current-41189"
       },
     #item {
	address = 41190,
	title = "AA20-BE5 EME10 Average Current",
	unit = "A",
	type = int16,
	scale = 10,
	label = "aa20-be5-eme10-average-current-41190"
       },
     #item {
	address = 41191,
	title = "PV Panel State",
	type = uint8,
	scale = 1,
	label = "pv-panel-state-41191"
       },
     #item {
	address = 41256,
	title = "Fan speed current",
	description = "The current fan speed after scheduling and blocks are considered",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "fan-speed-current-41256"
       },
     #item {
	address = 41257,
	title = "Fan speed current",
	description = "The current fan speed after scheduling and blocks are considered",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "fan-speed-current-41257"
       },
     #item {
	address = 41258,
	title = "Fan speed current",
	description = "The current fan speed after scheduling and blocks are considered",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "fan-speed-current-41258"
       },
     #item {
	address = 41265,
	title = "Smart Home Mode",
	description = "Current smart home mode, 0=Default,1=Away from home,2=Vacation",
	scale = 1,
	label = "smart-home-mode-41265",
	type = {enum,uint8,[
			    {0, "Default"},
			    {1, "Away from home"},
			    {2, "Vacation"}]}
       },
     #item {
	address = 41266,
	title = "Offset to smart home system",
	description = "Offset to smart home system",
	unit = "\x{00b0C}",
	type = int8,
	scale = 10,
	label = "offset-to-smart-home-system-41266"
       },
     #item {
	address = 41267,
	title = "Smart Home ctrl syst 8",
	description = "Smart Home is controlling the system",
	type = uint8,
	scale = 1,
	label = "smart-home-ctrl-syst-8-41267"
       },
     #item {
	address = 41268,
	title = "Smart Home ctrl syst 7",
	description = "Smart Home is controlling the system",
	type = uint8,
	scale = 1,
	label = "smart-home-ctrl-syst-7-41268"
       },
     #item {
	address = 41269,
	title = "Smart Home ctrl syst 6",
	description = "Smart Home is controlling the system",
	type = uint8,
	scale = 1,
	label = "smart-home-ctrl-syst-6-41269"
       },
     #item {
	address = 41270,
	title = "Smart Home ctrl syst 5",
	description = "Smart Home is controlling the system",
	type = uint8,
	scale = 1,
	label = "smart-home-ctrl-syst-5-41270"
       },
     #item {
	address = 41271,
	title = "Smart Home ctrl syst 4",
	description = "Smart Home is controlling the system",
	type = uint8,
	scale = 1,
	label = "smart-home-ctrl-syst-4-41271"
       },
     #item {
	address = 41272,
	title = "Smart Home ctrl syst 3",
	description = "Smart Home is controlling the system",
	type = uint8,
	scale = 1,
	label = "smart-home-ctrl-syst-3-41272"
       },
     #item {
	address = 41273,
	title = "Smart Home ctrl syst 2",
	description = "Smart Home is controlling the system",
	type = uint8,
	scale = 1,
	label = "smart-home-ctrl-syst-2-41273"
       },
     #item {
	address = 41274,
	title = "Smart Home ctrl syst 1",
	description = "Smart Home is controlling the system",
	type = uint8,
	scale = 1,
	label = "smart-home-ctrl-syst-1-41274"
       },
     #item {
	address = 41928,
	title = "Smart Price Adaption Price",
	description = "The current electric price",
	type = "u16",
	scale = 100,
	label = "smart-price-adaption-price-41928"
       },
     #item {
	address = 41929,
	title = "Smart Price Adaption Price Level",
	description = "Whether the current price is unknown (0), low (1), medium (2), high (3)",
	type = uint8,
	scale = 1,
	label = "smart-price-adaption-price-level-41929"
       },
     #item {
	address = 41930,
	title = "AA23-BE5 Power 10",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-power-10-41930"
       },
     #item {
	address = 41931,
	title = "AA23-BE5 Power 9",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-power-9-41931"
       },
     #item {
	address = 41932,
	title = "AA23-BE5 Power 8",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-power-8-41932"
       },
     #item {
	address = 41933,
	title = "AA23-BE5 Power 7",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-power-7-41933"
       },
     #item {
	address = 41934,
	title = "AA23-BE5 Power 6",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-power-6-41934"
       },
     #item {
	address = 41935,
	title = "AA23-BE5 Power 5",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-power-5-41935"
       },
     #item {
	address = 41936,
	title = "AA23-BE5 Power 4",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-power-4-41936"
       },
     #item {
	address = 41937,
	title = "AA23-BE5 Power 3",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-power-3-41937"
       },
     #item {
	address = 41938,
	title = "AA23-BE5 Power 2",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-power-2-41938"
       },
     #item {
	address = 41939,
	title = "AA23-BE5 Power 1",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-power-1-41939"
       },
     #item {
	address = 41940,
	title = "AA23-BE5 Error High 10",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-high-10-41940"
       },
     #item {
	address = 41941,
	title = "AA23-BE5 Error High 9",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-high-9-41941"
       },
     #item {
	address = 41942,
	title = "AA23-BE5 Error High 8",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-high-8-41942"
       },
     #item {
	address = 41943,
	title = "AA23-BE5 Error High 7",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-high-7-41943"
       },
     #item {
	address = 41944,
	title = "AA23-BE5 Error High 6",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-high-6-41944"
       },
     #item {
	address = 41945,
	title = "AA23-BE5 Error High 5",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-high-5-41945"
       },
     #item {
	address = 41946,
	title = "AA23-BE5 Error High 4",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-high-4-41946"
       },
     #item {
	address = 41947,
	title = "AA23-BE5 Error High 3",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-high-3-41947"
       },
     #item {
	address = 41948,
	title = "AA23-BE5 Error High 2",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-high-2-41948"
       },
     #item {
	address = 41949,
	title = "AA23-BE5 Error High 1",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-high-1-41949"
       },
     #item {
	address = 41950,
	title = "AA23-BE5 Error Low 10",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-low-10-41950"
       },
     #item {
	address = 41951,
	title = "AA23-BE5 Error Low 9",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-low-9-41951"
       },
     #item {
	address = 41952,
	title = "AA23-BE5 Error Low 8",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-low-8-41952"
       },
     #item {
	address = 41953,
	title = "AA23-BE5 Error Low 7",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-low-7-41953"
       },
     #item {
	address = 41954,
	title = "AA23-BE5 Error Low 6",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-low-6-41954"
       },
     #item {
	address = 41955,
	title = "AA23-BE5 Error Low 5",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-low-5-41955"
       },
     #item {
	address = 41956,
	title = "AA23-BE5 Error Low 4",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-low-4-41956"
       },
     #item {
	address = 41957,
	title = "AA23-BE5 Error Low 3",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-low-3-41957"
       },
     #item {
	address = 41958,
	title = "AA23-BE5 Error Low 2",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-low-2-41958"
       },
     #item {
	address = 41959,
	title = "AA23-BE5 Error Low 1",
	unit = "W",
	type = "u16",
	scale = 1,
	label = "aa23-be5-error-low-1-41959"
       },
     #item {
	address = 41960,
	title = "AA23-BE5 Com Percentage 10",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "aa23-be5-com-percentage-10-41960"
       },
     #item {
	address = 41961,
	title = "AA23-BE5 Com Percentage 9",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "aa23-be5-com-percentage-9-41961"
       },
     #item {
	address = 41962,
	title = "AA23-BE5 Com Percentage 8",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "aa23-be5-com-percentage-8-41962"
       },
     #item {
	address = 41963,
	title = "AA23-BE5 Com Percentage 7",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "aa23-be5-com-percentage-7-41963"
       },
     #item {
	address = 41964,
	title = "AA23-BE5 Com Percentage 6",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "aa23-be5-com-percentage-6-41964"
       },
     #item {
	address = 41965,
	title = "AA23-BE5 Com Percentage 5",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "aa23-be5-com-percentage-5-41965"
       },
     #item {
	address = 41966,
	title = "AA23-BE5 Com Percentage 4",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "aa23-be5-com-percentage-4-41966"
       },
     #item {
	address = 41967,
	title = "AA23-BE5 Com Percentage 3",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "aa23-be5-com-percentage-3-41967"
       },
     #item {
	address = 41968,
	title = "AA23-BE5 Com Percentage 2",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "aa23-be5-com-percentage-2-41968"
       },
     #item {
	address = 41969,
	title = "AA23-BE5 Com Percentage 1",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "aa23-be5-com-percentage-1-41969"
       },
     #item {
	address = 41980,
	title = "AA23-BE5 Voltage1 10",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage1-10-41980"
       },
     #item {
	address = 41981,
	title = "AA23-BE5 Voltage1 9",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage1-9-41981"
       },
     #item {
	address = 41982,
	title = "AA23-BE5 Voltage1 8",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage1-8-41982"
       },
     #item {
	address = 41983,
	title = "AA23-BE5 Voltage1 7",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage1-7-41983"
       },
     #item {
	address = 41984,
	title = "AA23-BE5 Voltage1 6",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage1-6-41984"
       },
     #item {
	address = 41985,
	title = "AA23-BE5 Voltage1 5",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage1-5-41985"
       },
     #item {
	address = 41986,
	title = "AA23-BE5 Voltage1 4",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage1-4-41986"
       },
     #item {
	address = 41987,
	title = "AA23-BE5 Voltage1 3",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage1-3-41987"
       },
     #item {
	address = 41988,
	title = "AA23-BE5 Voltage1 2",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage1-2-41988"
       },
     #item {
	address = 41989,
	title = "AA23-BE5 Voltage1 1",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage1-1-41989"
       },
     #item {
	address = 41990,
	title = "AA23-BE5 Voltage2 10",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage2-10-41990"
       },
     #item {
	address = 41991,
	title = "AA23-BE5 Voltage2 9",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage2-9-41991"
       },
     #item {
	address = 41992,
	title = "AA23-BE5 Voltage2 8",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage2-8-41992"
       },
     #item {
	address = 41993,
	title = "AA23-BE5 Voltage2 7",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage2-7-41993"
       },
     #item {
	address = 41994,
	title = "AA23-BE5 Voltage2 6",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage2-6-41994"
       },
     #item {
	address = 41995,
	title = "AA23-BE5 Voltage2 5",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage2-5-41995"
       },
     #item {
	address = 41996,
	title = "AA23-BE5 Voltage2 4",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage2-4-41996"
       },
     #item {
	address = 41997,
	title = "AA23-BE5 Voltage2 3",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage2-3-41997"
       },
     #item {
	address = 41998,
	title = "AA23-BE5 Voltage2 2",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage2-2-41998"
       },
     #item {
	address = 41999,
	title = "AA23-BE5 Voltage2 1",
	unit = "V",
	type = "u16",
	scale = 10,
	label = "aa23-be5-voltage2-1-41999"
       },
     #item {
	address = 42000,
	title = "AA23-BE5 Temperature 10",
	unit = "\x{00b0C}",
	type = "u16",
	scale = 10,
	label = "aa23-be5-temperature-10-42000"
       },
     #item {
	address = 42001,
	title = "AA23-BE5 Temperature 9",
	unit = "\x{00b0C}",
	type = "u16",
	scale = 10,
	label = "aa23-be5-temperature-9-42001"
       },
     #item {
	address = 42002,
	title = "AA23-BE5 Temperature 8",
	unit = "\x{00b0C}",
	type = "u16",
	scale = 10,
	label = "aa23-be5-temperature-8-42002"
       },
     #item {
	address = 42003,
	title = "AA23-BE5 Temperature 7",
	unit = "\x{00b0C}",
	type = "u16",
	scale = 10,
	label = "aa23-be5-temperature-7-42003"
       },
     #item {
	address = 42004,
	title = "AA23-BE5 Temperature 6",
	unit = "\x{00b0C}",
	type = "u16",
	scale = 10,
	label = "aa23-be5-temperature-6-42004"
       },
     #item {
	address = 42005,
	title = "AA23-BE5 Temperature 5",
	unit = "\x{00b0C}",
	type = "u16",
	scale = 10,
	label = "aa23-be5-temperature-5-42005"
       },
     #item {
	address = 42006,
	title = "AA23-BE5 Temperature 4",
	unit = "\x{00b0C}",
	type = "u16",
	scale = 10,
	label = "aa23-be5-temperature-4-42006"
       },
     #item {
	address = 42007,
	title = "AA23-BE5 Temperature 3",
	unit = "\x{00b0C}",
	type = "u16",
	scale = 10,
	label = "aa23-be5-temperature-3-42007"
       },
     #item {
	address = 42008,
	title = "AA23-BE5 Temperature 2",
	unit = "\x{00b0C}",
	type = "u16",
	scale = 10,
	label = "aa23-be5-temperature-2-42008"
       },
     #item {
	address = 42009,
	title = "AA23-BE5 Temperature 1",
	unit = "\x{00b0C}",
	type = "u16",
	scale = 10,
	label = "aa23-be5-temperature-1-42009"
       },
     #item {
	address = 42010,
	title = "AA23-BE5 Energy 10",
	unit = "kWh",
	type = uint32,
	scale = 10,
	label = "aa23-be5-energy-10-42010"
       },
     #item {
	address = 42012,
	title = "AA23-BE5 Energy 9",
	unit = "kWh",
	type = uint32,
	scale = 10,
	label = "aa23-be5-energy-9-42012"
       },
     #item {
	address = 42014,
	title = "AA23-BE5 Energy 8",
	unit = "kWh",
	type = uint32,
	scale = 10,
	label = "aa23-be5-energy-8-42014"
       },
     #item {
	address = 42016,
	title = "AA23-BE5 Energy 7",
	unit = "kWh",
	type = uint32,
	scale = 10,
	label = "aa23-be5-energy-7-42016"
       },
     #item {
	address = 42018,
	title = "AA23-BE5 Energy 6",
	unit = "kWh",
	type = uint32,
	scale = 10,
	label = "aa23-be5-energy-6-42018"
       },
     #item {
	address = 42020,
	title = "AA23-BE5 Energy 5",
	unit = "kWh",
	type = uint32,
	scale = 10,
	label = "aa23-be5-energy-5-42020"
       },
     #item {
	address = 42022,
	title = "AA23-BE5 Energy 4",
	unit = "kWh",
	type = uint32,
	scale = 10,
	label = "aa23-be5-energy-4-42022"
       },
     #item {
	address = 42024,
	title = "AA23-BE5 Energy 3",
	unit = "kWh",
	type = uint32,
	scale = 10,
	label = "aa23-be5-energy-3-42024"
       },
     #item {
	address = 42026,
	title = "AA23-BE5 Energy 2",
	unit = "kWh",
	type = uint32,
	scale = 10,
	label = "aa23-be5-energy-2-42026"
       },
     #item {
	address = 42028,
	title = "AA23-BE5 Energy 1",
	unit = "kWh",
	type = uint32,
	scale = 10,
	label = "aa23-be5-energy-1-42028"
       },
     #item {
	address = 42030,
	title = "AA23-BE5 EME20 Version",
	type = "u16",
	scale = 1,
	label = "aa23-be5-eme20-version-42030"
       },
     #item {
	address = 42033,
	title = "PV Panel Heat Offset",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 10,
	label = "pv-panel-heat-offset-42033"
       },
     #item {
	address = 42034,
	title = "PV Panel Pool Offset",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 10,
	label = "pv-panel-pool-offset-42034"
       },
     #item {
	address = 42035,
	title = "AA23-BE5 EME20 Total Power",
	unit = "W",
	type = uint32,
	scale = 1,
	label = "aa23-be5-eme20-total-power-42035"
       },
     #item {
	address = 42037,
	title = "AA23-BE5 EME20 Total Average Power",
	unit = "W",
	type = uint32,
	scale = 1,
	label = "aa23-be5-eme20-total-average-power-42037"
       },
     #item {
	address = 42075,
	title = "AA23-BE5 EME20 Total Energy",
	unit = "kWh",
	type = uint32,
	scale = 10,
	label = "aa23-be5-eme20-total-energy-42075"
       },
     #item {
	address = 42080,
	title = "AA23-BE5 Alarm 504",
	type = uint8,
	scale = 1,
	label = "aa23-be5-alarm-504-42080"
       },
     #item {
	address = 42081,
	title = "AA23-BE5 Alarm 505",
	type = uint8,
	scale = 1,
	label = "aa23-be5-alarm-505-42081"
       },
     #item {
	address = 42082,
	title = "AA23-BE5 Alarm 506",
	type = uint8,
	scale = 1,
	label = "aa23-be5-alarm-506-42082"
       },
     #item {
	address = 42083,
	title = "AA23-BE5 Alarm 507",
	type = uint8,
	scale = 1,
	label = "aa23-be5-alarm-507-42083"
       },
     #item {
	address = 42084,
	title = "AA23-BE5 Alarm 508",
	type = uint8,
	scale = 1,
	label = "aa23-be5-alarm-508-42084"
       },
     #item {
	address = 42085,
	title = "AA23-BE5 Alarm 509",
	type = uint8,
	scale = 1,
	label = "aa23-be5-alarm-509-42085"
       },
     #item {
	address = 42086,
	title = "AA23-BE5 Alarm 510",
	type = uint8,
	scale = 1,
	label = "aa23-be5-alarm-510-42086"
       },
     #item {
	address = 42087,
	title = "AA23-BE5 Alarm 511",
	type = uint8,
	scale = 1,
	label = "aa23-be5-alarm-511-42087"
       },
     #item {
	address = 42100,
	title = "BT1 Average, 24h",
	description = "EB100-BT1 Outdoor temperature average, 24h",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt1-average-24h-42100"
       },
     #item {
	address = 42101,
	title = "Used heating power average, 24h",
	description = "Used heating power average, 24h",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "used-heating-power-average-24h-42101"
       },
     #item {
	address = 42136,
	title = "BT22 Supply air temp.",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt22-supply-air-temp-42136"
       },
     #item {
	address = 42137,
	title = "BT22 Supply air temp.",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt22-supply-air-temp-42137"
       },
     #item {
	address = 42138,
	title = "BT22 Supply air temp.",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "bt22-supply-air-temp-42138"
       },
     #item {
	address = 43001,
	title = "Software version",
	type = "u16",
	scale = 1,
	label = "software-version-43001"
       },
     #item {
	address = 43006,
	title = "Calc. Supply S4",
	description = "Calculated supply temperature for the climate system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "calc-supply-s4-43006"
       },
     #item {
	address = 43007,
	title = "Calc. Supply S3",
	description = "Calculated supply temperature for the climate system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "calc-supply-s3-43007"
       },
     #item {
	address = 43008,
	title = "Calc. Supply S2",
	description = "Calculated supply temperature for the climate system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "calc-supply-s2-43008"
       },
     #item {
	address = 43009,
	title = "Calc. Supply S1",
	description = "Calculated supply temperature for the climate system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	label = "calc-supply-s1-43009"
       },
     #item {
	address = 43013,
	title = "Freeze Protection Status",
	description = "1 = Freeze protection active",
	type = {enum,uint8,[{1, "Freeze protection active"}]},
	scale = 1,
	label = "freeze-protection-status-43013"
       },
     #item {
	address = 43081,
	title = "Tot. op.time add.",
	description = "Total electric additive operation time",
	unit = "h",
	type = int32,
	scale = 10,
	min = 0.0,
	max = 1000000.0,
	default = 0.0,
	label = "tot-op-time-add-43081"
       },
     #item {
	address = 43084,
	title = "Int. el.add. Power",
	description = "Current power from the internal electrical addition",
	unit = "kW",
	type = int16,
	scale = 100,
	label = "int-el-add-power-43084"
       },
     #item {
	address = 43086,
	title = "Prio",
	description = "Indicates what heating action (HW/heat/pool) currently prioritised 10=Off 20=Hot Water 30=Heat 40=Pool 41=Pool 2 50=Transfer 60=Cooling",
	scale = 1,
	label = "prio-43086",
	type = {enum,int8,
		[
		 {10, "Off"},
		 {20, "Hot Water"},
		 {30, "Heat"},
		 {40, "Pool"},
		 {41, "Pool 2"},
		 {50, "Transfer"},
		 {60, "Cooling"}]}
       }
     #item {
	    address = 43091,
	    title = "Int. el.add. State",
	    description = "Number of steps active for internal step-controlled addition",
	    type = uint8,
	    scale = 1,
	    label = "int-el-add-state-43091"
	   },
     #item {
	address = 43093,
	title = "Mixing Valve State S4",
	description = "State of the mixing valve for the climate system",
	type = uint8,
	scale = 1,
	label = "mixing-valve-state-s4-43093"
       },
     #item {
	address = 43094,
	title = "Mixing Valve State S3",
	description = "State of the mixing valve for the climate system",
	type = uint8,
	scale = 1,
	label = "mixing-valve-state-s3-43094"
       },
     #item {
	address = 43095,
	title = "Mixing Valve State S2",
	description = "State of the mixing valve for the climate system",
	type = uint8,
	scale = 1,
	label = "mixing-valve-state-s2-43095"
       },
     #item {
	address = 43096,
	title = "Mixing Valve State S1",
	description = "State of the mixing valve for the climate system",
	type = uint8,
	scale = 1,
	label = "mixing-valve-state-s1-43096"
       },
     #item {
	address = 43105,
	title = "Status FJVM",
	description = "The state of the FJVM accessory",
	type = uint8,
	scale = 1,
	label = "status-fjvm-43105"
       },
     #item {
	address = 43108,
	title = "Fan speed current",
	description = "The current fan speed after scheduling and blocks are considered",
	unit = "%",
	type = uint8,
	scale = 1,
	label = "fan-speed-current-43108"
       },
     #item {
	address = 43158,
	title = "External adjustment activated via input S4",
	type = uint8,
	scale = 1,
	label = "external-adjustment-activated-via-input-s4-43158"
       },
     #item {
	address = 43159,
	title = "External adjustment activated via input S3",
	type = uint8,
	scale = 1,
	label = "external-adjustment-activated-via-input-s3-43159"
       },
     #item {
	address = 43160,
	title = "External adjustment activated via input S2",
	type = uint8,
	scale = 1,
	label = "external-adjustment-activated-via-input-s2-43160"
       },
     #item {
	address = 43161,
	title = "External adjustment activated via input S1",
	type = uint8,
	scale = 1,
	label = "external-adjustment-activated-via-input-s1-43161"
       },
     #item {
	address = 43180,
	title = "HWC Pump Status GP11",
	description = "Hot water circulation pump status. 1=on, 0=off",
	type = {enum,uint8,[
			    {0, "off"},
			    {1, "on"}]
	       },
	scale = 1,
	label = "hwc-pump-status-gp11-43180"    
       },
     #item {
	address = 43239,
	title = "Tot. HW op.time add.",
	description = "Total electric additive operation time in hot water mode",
	unit = "h",
	type = int32,
	scale = 10,
	min = 0.0,
	max = 9999999.0,
	default = 0.0,
	label = "tot-hw-op-time-add-43239"
       },
     #item {
	address = 43383,
	title = "FJVM Relays",
	description = "Indicates the active relays on the FJVM accessory. The information is binary encoded",
	type = uint8,
	scale = 1,
	label = "fjvm-relays-43383"
       },
     #item {
	address = 43416,
	title = "Compressor starts EB100-EP14",
	description = "Number of compressorer starts",
	type = int32,
	scale = 1,
	min = 0.0,
	max = 9999999.0,
	default = 0.0,
	label = "compressor-starts-eb100-ep14-43416"
       },
     #item {
	address = 43420,
	title = "Tot. op.time compr. EB100-EP14",
	description = "Total compressorer operation time",
	unit = "h",
	type = int32,
	scale = 1,
	min = 0.0,
	max = 9999999.0,
	default = 0.0,
	label = "tot-op-time-compr-eb100-ep14-43420"
       },
     #item {
	address = 43424,
	title = "Tot. HW op.time compr. EB100-EP14",
	description = "Total compressorer operation time in hot water mode",
	unit = "h",
	type = int32,
	scale = 1,
	min = 0.0,
	max = 9999999.0,
	default = 0.0,
	label = "tot-hw-op-time-compr-eb100-ep14-43424"
       },
     #item {
	address = 43427,
	title = "Compressor State EP14",
	description = "20 = Stopped, 40 = Starting, 60 = Running, 100 = Stopping",
	type = {enum,uint8, [
			     {20, "Stopped"},
			     {40, "Starting"},
			     {60, "Running"},
			     {100, "Stopping"}]
	       },
	scale = 1,
	label = "compressor-state-ep14-43427"

       },
     #item {
	address = 43431,
	title = "Supply Pump State EP14",
	description = "10=off,15=starting,20=on,40=10-day mode,80=calibration",
	type = {enum,uint8, [
			     {10, "off"},
			     {15, "starting"},
			     {20, "on"},
			     {40, "10-day mode"},
			     {80, "calibration"}]
	       },
	scale = 1,
	label = "supply-pump-state-ep14-43431"    
       },
     #item {
	address = 43435,
	title = "Cpr Status EP14",
	description = "Status of the compressor. 1=on,0=off 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	label = "cpr-status-ep14-43435"    
       },
     #item {
	address = 43460,
	title = "State DEH",
	description = "The state of the DEH accessory",
	type = uint8,
	scale = 1,
	label = "state-deh-43460"
       },
     #item {
	address = 43514,
	title = "EB100-EP14 PCA Base Relays",
	description = "Indicates active relays on the PCA Base card. Please refer to the wiring diagram for relay description. Binary encoded. 1=on, 0=off. Bit0=K4,Bit1=K3,Bit2=K2,Bit3=K1",
	type = uint8,
	scale = 1,
	label = "eb100-ep14-pca-base-relays-43514"
       },
     #item {
	address = 43516,
	title = "PCA-Power Relays EP14",
	description = "Indicates the active relays on the PCA-Power card. The information is binary encoded",
	type = uint8,
	scale = 1,
	label = "pca-power-relays-ep14-43516"
       },
     #item {
	address = 44331,
	title = "Software release",
	type = uint8,
	scale = 1,
	label = "software-release-44331"
       },
     #item {
	address = 44744,
	title = "Extra heating system pump S4",
	type = uint8,
	scale = 1,
	label = "extra-heating-system-pump-s4-44744"
       },
     #item {
	address = 44745,
	title = "Extra heating system pump S3",
	type = uint8,
	scale = 1,
	label = "extra-heating-system-pump-s3-44745"
       },
     #item {
	address = 44746,
	title = "Extra heating system pump S2",
	type = uint8,
	scale = 1,
	label = "extra-heating-system-pump-s2-44746"
       },
     #item {
	address = 44750,
	title = "FJVM pump",
	type = uint8,
	scale = 1,
	label = "fjvm-pump-44750"
       },
     #item {
	address = 44874,
	title = "State SG Ready",
	type = uint8,
	scale = 1,
	label = "state-sg-ready-44874"
       },
     #item {
	address = 44878,
	title = "SG Ready input A",
	type = uint8,
	scale = 1,
	label = "sg-ready-input-a-44878"
       },
     #item {
	address = 44879,
	title = "SG Ready input B",
	type = uint8,
	scale = 1,
	label = "sg-ready-input-b-44879"
       },
     #item {
	address = 44896,
	title = "Smart Price Adaption Heating Offset",
	type = int8,
	scale = 10,
	label = "smart-price-adaption-heating-offset-44896"
       },
     #item {
	address = 44897,
	title = "Smart Price Adaption HW Comfort Mode",
	description = "0=Eco,1=Normal,2=Luxury,10=Normal+,20=Mini",
	type = {enum,int8, [
			    {0, "Eco"},
			    {1, "Normal"},
			    {2, "Luxury"},
			    {10, "Normal+"},
			    {20, "Mini"}]
	       },
	scale = 1,
	label = "smart-price-adaption-hw-comfort-mode-44897"    
       },
     #item {
	address = 44898,
	title = "Smart Price Adaption Pool Offset",
	type = int8,
	scale = 1,
	label = "smart-price-adaption-pool-offset-44898"
       },
     #item {
	address = 44899,
	title = "Smart Price Adaption Cool Offset",
	type = int8,
	scale = 1,
	label = "smart-price-adaption-cool-offset-44899"
       },
     #item {
	address = 44908,
	title = "State smart price adaption",
	type = uint8,
	scale = 1,
	label = "state-smart-price-adaption-44908"
       },
     #item {
	address = 45001,
	title = "Alarm",
	description = "Indicates the alarm number of the most severe current alarm",
	type = int16,
	scale = 1,
	label = "alarm-45001"
       },
     #item {
	address = 47291,
	title = "Floor drying timer",
	unit = "hrs",
	type = "u16",
	scale = 1,
	min = 0.0,
	max = 10000.0,
	default = 0.0,
	label = "floor-drying-timer-47291"
       },
     #item {
	address = 40879,
	title = "+Adjust Parallell factor",
	unit = "1",
	type = int8,
	scale = 1,
	min = 1.0,
	max = 10.0,
	default = 5.0,
	label = "adjust-parallell-factor-40879",
	write = true
       },
     #item {
	address = 40880,
	title = "+Adjust Max change",
	description = "Largest allowed change",
	type = int8,
	scale = 1,
	label = "adjust-max-change-40880",
	write = true
       },
     #item {
	address = 40881,
	title = "+Adjust Affect system8",
	description = "System affected by paralell change",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "adjust-affect-system8-40881",
	write = true
       },
     #item {
	address = 40882,
	title = "+Adjust Affect system7",
	description = "System affected by paralell change",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "adjust-affect-system7-40882",
	write = true
       },
     #item {
	address = 40883,
	title = "+Adjust Affect system6",
	description = "System affected by paralell change",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "adjust-affect-system6-40883",
	write = true
       },
     #item {
	address = 40884,
	title = "+Adjust Affect system5",
	description = "System affected by paralell change",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "adjust-affect-system5-40884",
	write = true
       },
     #item {
	address = 40885,
	title = "+Adjust Affect system4",
	description = "System affected by paralell change",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "adjust-affect-system4-40885",
	write = true
       },
     #item {
	address = 40886,
	title = "+Adjust Affect system3",
	description = "System affected by paralell change",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "adjust-affect-system3-40886",
	write = true
       },
     #item {
	address = 40887,
	title = "+Adjust Affect system2",
	description = "System affected by paralell change",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "adjust-affect-system2-40887",
	write = true
       },
     #item {
	address = 40888,
	title = "+Adjust Affect system1",
	description = "System affected by paralell change",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "adjust-affect-system1-40888",
	write = true
       },
     #item {
	address = 45171,
	title = "Alarm Reset",
	description = "Reset alarm by setting value 1",
	type = uint8,
	scale = 1,
	label = "alarm-reset-45171",
	write = true
       },
     #item {
	address = 47004,
	title = "Heat Curve S4",
	description = "Heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 15.0,
	default = 9.0,
	label = "heat-curve-s4-47004",
	write = true
       },
     #item {
	address = 47005,
	title = "Heat Curve S3",
	description = "Heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 15.0,
	default = 9.0,
	label = "heat-curve-s3-47005",
	write = true
       },
     #item {
	address = 47006,
	title = "Heat Curve S2",
	description = "Heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 15.0,
	default = 9.0,
	label = "heat-curve-s2-47006",
	write = true
       },
     #item {
	address = 47007,
	title = "Heat Curve S1",
	description = "Heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 15.0,
	default = 9.0,
	label = "heat-curve-s1-47007",
	write = true
       },
     #item {
	address = 47008,
	title = "Heat Offset S4",
	description = "Offset of the heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "heat-offset-s4-47008",
	write = true
       },
     #item {
	address = 47009,
	title = "Heat Offset S3",
	description = "Offset of the heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "heat-offset-s3-47009",
	write = true
       },
     #item {
	address = 47010,
	title = "Heat Offset S2",
	description = "Offset of the heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "heat-offset-s2-47010",
	write = true
       },
     #item {
	address = 47011,
	title = "Heat Offset S1",
	description = "Offset of the heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "heat-offset-s1-47011",
	write = true
       },
     #item {
	address = 47012,
	title = "Min Supply System 4",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 200.0,
	label = "min-supply-system-4-47012",
	write = true
       },
     #item {
	address = 47013,
	title = "Min Supply System 3",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 200.0,
	label = "min-supply-system-3-47013",
	write = true
       },
     #item {
	address = 47014,
	title = "Min Supply System 2",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 200.0,
	label = "min-supply-system-2-47014",
	write = true
       },
     #item {
	address = 47015,
	title = "Min Supply System 1",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 200.0,
	label = "min-supply-system-1-47015",
	write = true
       },
     #item {
	address = 47016,
	title = "Max Supply System 4",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 800.0,
	default = 600.0,
	label = "max-supply-system-4-47016",
	write = true
       },
     #item {
	address = 47017,
	title = "Max Supply System 3",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 800.0,
	default = 600.0,
	label = "max-supply-system-3-47017",
	write = true
       },
     #item {
	address = 47018,
	title = "Max Supply System 2",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 800.0,
	default = 600.0,
	label = "max-supply-system-2-47018",
	write = true
       },
     #item {
	address = 47019,
	title = "Max Supply System 1",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 800.0,
	default = 600.0,
	label = "max-supply-system-1-47019",
	write = true
       },
     #item {
	address = 47020,
	title = "Own Heating Curve P7",
	description = "User defined heating curve point",
	unit = "\x{00b0C}",
	type = int8,
	scale = 1,
	min = 5.0,
	max = 80.0,
	default = 15.0,
	label = "own-heating-curve-p7-47020",
	write = true
       },
     #item {
	address = 47021,
	title = "Own Heating Curve P6",
	description = "User defined heating curve point",
	unit = "\x{00b0C}",
	type = int8,
	scale = 1,
	min = 5.0,
	max = 80.0,
	default = 15.0,
	label = "own-heating-curve-p6-47021",
	write = true
       },
     #item {
	address = 47022,
	title = "Own Heating Curve P5",
	description = "User defined heating curve point",
	unit = "\x{00b0C}",
	type = int8,
	scale = 1,
	min = 5.0,
	max = 80.0,
	default = 26.0,
	label = "own-heating-curve-p5-47022",
	write = true
       },
     #item {
	address = 47023,
	title = "Own Heating Curve P4",
	description = "User defined heating curve point",
	unit = "\x{00b0C}",
	type = int8,
	scale = 1,
	min = 5.0,
	max = 80.0,
	default = 32.0,
	label = "own-heating-curve-p4-47023",
	write = true
       },
     #item {
	address = 47024,
	title = "Own Heating Curve P3",
	description = "User defined heating curve point",
	unit = "\x{00b0C}",
	type = int8,
	scale = 1,
	min = 5.0,
	max = 80.0,
	default = 35.0,
	label = "own-heating-curve-p3-47024",
	write = true
       },
     #item {
	address = 47025,
	title = "Own Heating Curve P2",
	description = "User defined heating curve point",
	unit = "\x{00b0C}",
	type = int8,
	scale = 1,
	min = 5.0,
	max = 80.0,
	default = 40.0,
	label = "own-heating-curve-p2-47025",
	write = true
       },
     #item {
	address = 47026,
	title = "Own Heating Curve P1",
	description = "User defined heating curve point",
	unit = "\x{00b0C}",
	type = int8,
	scale = 1,
	min = 5.0,
	max = 80.0,
	default = 45.0,
	label = "own-heating-curve-p1-47026",
	write = true
       },
     #item {
	address = 47027,
	title = "Point offset outdoor temp.",
	description = "Outdoor temperature point where the heat curve is offset",
	unit = "\x{00b0C}",
	type = int8,
	scale = 1,
	min = -40.0,
	max = 30.0,
	default = 0.0,
	label = "point-offset-outdoor-temp-47027",
	write = true
       },
     #item {
	address = 47028,
	title = "Point offset",
	description = "Amount of offset at the point offset temperature",
	unit = "\x{00b0C}",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "point-offset-47028",
	write = true
       },
     #item {
	address = 47029,
	title = "External adjustment S4",
	description = "Change of the offset of the heat curve when closing the external adjustment input",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "external-adjustment-s4-47029",
	write = true
       },
     #item {
	address = 47030,
	title = "External adjustment S3",
	description = "Change of the offset of the heat curve when closing the external adjustment input",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "external-adjustment-s3-47030",
	write = true
       },
     #item {
	address = 47031,
	title = "External adjustment S2",
	description = "Change of the offset of the heat curve when closing the external adjustment input",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "external-adjustment-s2-47031",
	write = true
       },
     #item {
	address = 47032,
	title = "External adjustment S1",
	description = "Change of the offset of the heat curve when closing the external adjustment input",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "external-adjustment-s1-47032",
	write = true
       },
     #item {
	address = 47033,
	title = "External adjustment with room sensor S4",
	description = "Room temperature setting when closing the external adjustment input",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "external-adjustment-with-room-sensor-s4-47033",
	write = true
       },
     #item {
	address = 47034,
	title = "External adjustment with room sensor S3",
	description = "Room temperature setting when closing the external adjustment input",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "external-adjustment-with-room-sensor-s3-47034",
	write = true
       },
     #item {
	address = 47035,
	title = "External adjustment with room sensor S2",
	description = "Room temperature setting when closing the external adjustment input",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "external-adjustment-with-room-sensor-s2-47035",
	write = true
       },
     #item {
	address = 47036,
	title = "External adjustment with room sensor S1",
	description = "Room temperature setting when closing the external adjustment input",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "external-adjustment-with-room-sensor-s1-47036",
	write = true
       },
     #item {
	address = 47041,
	title = "Hot water comfort mode",
	description = "Setting in menu 2.2. 0=Economy,1=Normal,2=Luxury,4=Smart Control 0=Economy 1=Normal 2=Luxury",
	type = {enum,int8, [
			    {0, "Economy"},
			    {1, "Normal"},
			    {2, "Luxury"},
			    {4, "Smart Control"}]
	       },
	scale = 1,
	min = 0.0,
	max = 4.0,
	default = 1.0,
	label = "hot-water-comfort-mode-47041",
	write = true    
       },
     #item {
	address = 47043,
	title = "Start temperature HW Luxury",
	description = "Start temperature for heating water",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 520.0,
	label = "start-temperature-hw-luxury-47043",
	write = true
       },
     #item {
	address = 47044,
	title = "Start temperature HW Normal",
	description = "Start temperature for heating water",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 490.0,
	label = "start-temperature-hw-normal-47044",
	write = true
       },
     #item {
	address = 47045,
	title = "Start temperature HW Economy",
	description = "Start temperature for heating water",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 450.0,
	label = "start-temperature-hw-economy-47045",
	write = true
       },
     #item {
	address = 47046,
	title = "Stop temperature Periodic HW",
	description = "Temperature where hot water generation will stop",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 550.0,
	max = 700.0,
	default = 550.0,
	label = "stop-temperature-periodic-hw-47046",
	write = true
       },
     #item {
	address = 47047,
	title = "Stop temperature HW Luxury",
	description = "Temperature where hot water generation will stop",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 580.0,
	label = "stop-temperature-hw-luxury-47047",
	write = true
       },
     #item {
	address = 47048,
	title = "Stop temperature HW Normal",
	description = "Temperature where hot water generation will stop",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 550.0,
	label = "stop-temperature-hw-normal-47048",
	write = true
       },
     #item {
	address = 47049,
	title = "Stop temperature HW Economy",
	description = "Temperature where hot water generation will stop",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 510.0,
	label = "stop-temperature-hw-economy-47049",
	write = true
       },
     #item {
	address = 47050,
	title = "Periodic HW",
	description = "Activates the periodic hot water generation",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 1.0,
	label = "periodic-hw-47050",
	write = true
       },
     #item {
	address = 47051,
	title = "Periodic HW Interval",
	description = "Interval between Periodic hot water sessions",
	unit = "days",
	type = int8,
	scale = 1,
	min = 1.0,
	max = 90.0,
	default = 14.0,
	label = "periodic-hw-interval-47051",
	write = true
       },
     #item {
	address = 47054,
	title = "Run time HWC",
	description = "Run time for the hot water circulation system",
	unit = "min",
	type = int8,
	scale = 1,
	min = 1.0,
	max = 60.0,
	default = 60.0,
	label = "run-time-hwc-47054",
	write = true
       },
     #item {
	address = 47055,
	title = "Still time HWC",
	description = "Still time for the hot water circulation system",
	unit = "min",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 60.0,
	default = 0.0,
	label = "still-time-hwc-47055",
	write = true
       },
     #item {
	address = 47131,
	title = "Language",
	description = "Display language in the heat pump 0=English 1=Svenska 2=Deutsch 3=Francais 4=Espanol 5=Suomi 6=Lietuviu 7=Cesky 8=Polski 9=Nederlands 10=Norsk 11=Dansk 12=Eesti 13=Latviesu 16=Magyar",
	type = {enum,int8, [
			    {0, "English"},
			    {1, "Svenska"},
			    {2, "Deutsch"},
			    {3, "Francais"},
			    {4, "Espanol"},
			    {5, "Suomi"},
			    {6, "Lietuviu"},
			    {7, "Cesky"},
			    {8, "Polski"},
			    {9, "Nederlands"},
			    {10, "Norsk"},
			    {11, "Dansk"},
			    {12, "Eesti"},
			    {13, "Latviesu"},
			    {16, "Magyar"}]
	       },
	scale = 1,
	min = 0.0,
	max = 21.0,
	default = 0.0,
	label = "language-47131",
	write = true

       },
     #item {
	address = 47137,
	title = "Operational mode",
	description = "The operational mode of the heat pump 0=Auto 1=Manual 2=Add. heat only",
	type = {enum,uint8, [
			     {0, "Auto"},
			     {1, "Manual"},
			     {2, "Add. heat only"}]
	       },
	scale = 1,
	min = 0.0,
	max = 2.0,
	default = 0.0,
	label = "operational-mode-47137",
	write = true

       },
     #item {
	address = 47138,
	title = "Operational mode heat medium pump",
	description = " 10=Intermittent 20=Continous 30=Economy 40=Auto",
	type = {enum,uint8, [
			     {10, "Intermittent"},
			     {20, "Continous"},
			     {30, "Economy"},
			     {40, "Auto"}]
	       },
	scale = 1,
	min = 10.0,
	max = 40.0,
	default = 40.0,
	label = "operational-mode-heat-medium-pump-47138",
	write = true

       },
     #item {
	address = 47212,
	title = "Max int add. power",
	unit = "kW",
	type = int16,
	scale = 100,
	min = 0.0,
	max = 4500.0,
	default = 600.0,
	label = "max-int-add-power-47212",
	write = true
       },
     #item {
	address = 47214,
	title = "Fuse",
	description = "Size of the fuse that the HP is connected to",
	unit = "A",
	type = "u16",
	scale = 1,
	min = 1.0,
	max = 400.0,
	default = 16.0,
	label = "fuse-47214",
	write = true
       },
     #item {
	address = 47261,
	title = "Exhaust Fan speed 4",
	unit = "%",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 100.0,
	default = 100.0,
	label = "exhaust-fan-speed-4-47261",
	write = true
       },
     #item {
	address = 47262,
	title = "Exhaust Fan speed 3",
	unit = "%",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 100.0,
	default = 80.0,
	label = "exhaust-fan-speed-3-47262",
	write = true
       },
     #item {
	address = 47263,
	title = "Exhaust Fan speed 2",
	unit = "%",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 100.0,
	default = 30.0,
	label = "exhaust-fan-speed-2-47263",
	write = true
       },
     #item {
	address = 47264,
	title = "Exhaust Fan speed 1",
	unit = "%",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 100.0,
	default = 0.0,
	label = "exhaust-fan-speed-1-47264",
	write = true
       },
     #item {
	address = 47265,
	title = "Exhaust Fan speed normal",
	unit = "%",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 100.0,
	default = 65.0,
	label = "exhaust-fan-speed-normal-47265",
	write = true
       },
     #item {
	address = 47266,
	title = "Supply Fan speed 4",
	unit = "%",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 100.0,
	default = 90.0,
	label = "supply-fan-speed-4-47266",
	write = true
       },
     #item {
	address = 47267,
	title = "Supply Fan speed 3",
	unit = "%",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 100.0,
	default = 70.0,
	label = "supply-fan-speed-3-47267",
	write = true
       },
     #item {
	address = 47268,
	title = "Supply Fan speed 2",
	unit = "%",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 100.0,
	default = 25.0,
	label = "supply-fan-speed-2-47268",
	write = true
       },
     #item {
	address = 47269,
	title = "Supply Fan speed 1",
	unit = "%",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 100.0,
	default = 0.0,
	label = "supply-fan-speed-1-47269",
	write = true
       },
     #item {
	address = 47270,
	title = "Supply Fan speed normal",
	unit = "%",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 100.0,
	default = 60.0,
	label = "supply-fan-speed-normal-47270",
	write = true
       },
     #item {
	address = 47271,
	title = "Fan return time 4",
	description = "Time from a changed fan speed until it returns to normal speed",
	unit = "h",
	type = uint8,
	scale = 1,
	min = 1.0,
	max = 99.0,
	default = 4.0,
	label = "fan-return-time-4-47271",
	write = true
       },
     #item {
	address = 47272,
	title = "Fan return time 3",
	description = "Time from a changed fan speed until it returns to normal speed",
	unit = "h",
	type = uint8,
	scale = 1,
	min = 1.0,
	max = 99.0,
	default = 4.0,
	label = "fan-return-time-3-47272",
	write = true
       },
     #item {
	address = 47273,
	title = "Fan return time 2",
	description = "Time from a changed fan speed until it returns to normal speed",
	unit = "h",
	type = uint8,
	scale = 1,
	min = 1.0,
	max = 99.0,
	default = 4.0,
	label = "fan-return-time-2-47273",
	write = true
       },
     #item {
	address = 47274,
	title = "Fan return time 1",
	description = "Time from a changed fan speed until it returns to normal speed",
	unit = "h",
	type = uint8,
	scale = 1,
	min = 1.0,
	max = 99.0,
	default = 4.0,
	label = "fan-return-time-1-47274",
	write = true
       },
     #item {
	address = 47275,
	title = "Filter Reminder period",
	description = "Time between the reminder of filter replacement/cleaning.",
	unit = "Months",
	type = uint8,
	scale = 1,
	min = 1.0,
	max = 24.0,
	default = 3.0,
	label = "filter-reminder-period-47275",
	write = true
       },
     #item {
	address = 47276,
	title = "Floor drying",
	description = " 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "floor-drying-47276",
	write = true

       },
     #item {
	address = 47277,
	title = "Floor drying period 7",
	description = "Days each period is active",
	unit = "days",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 30.0,
	default = 2.0,
	label = "floor-drying-period-7-47277",
	write = true
       },
     #item {
	address = 47278,
	title = "Floor drying period 6",
	description = "Days each period is active",
	unit = "days",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 30.0,
	default = 2.0,
	label = "floor-drying-period-6-47278",
	write = true
       },
     #item {
	address = 47279,
	title = "Floor drying period 5",
	description = "Days each period is active",
	unit = "days",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 30.0,
	default = 2.0,
	label = "floor-drying-period-5-47279",
	write = true
       },
     #item {
	address = 47280,
	title = "Floor drying period 4",
	description = "Days each period is active",
	unit = "days",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 30.0,
	default = 3.0,
	label = "floor-drying-period-4-47280",
	write = true
       },
     #item {
	address = 47281,
	title = "Floor drying period 3",
	description = "Days each period is active",
	unit = "days",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 30.0,
	default = 2.0,
	label = "floor-drying-period-3-47281",
	write = true
       },
     #item {
	address = 47282,
	title = "Floor drying period 2",
	description = "Days each period is active",
	unit = "days",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 30.0,
	default = 2.0,
	label = "floor-drying-period-2-47282",
	write = true
       },
     #item {
	address = 47283,
	title = "Floor drying period 1",
	description = "Days each period is active",
	unit = "days",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 30.0,
	default = 2.0,
	label = "floor-drying-period-1-47283",
	write = true
       },
     #item {
	address = 47284,
	title = "Floor drying temp. 7",
	description = "Supply temperature each period",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 15.0,
	max = 70.0,
	default = 20.0,
	label = "floor-drying-temp-7-47284",
	write = true
       },
     #item {
	address = 47285,
	title = "Floor drying temp. 6",
	description = "Supply temperature each period",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 15.0,
	max = 70.0,
	default = 30.0,
	label = "floor-drying-temp-6-47285",
	write = true
       },
     #item {
	address = 47286,
	title = "Floor drying temp. 5",
	description = "Supply temperature each period",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 15.0,
	max = 70.0,
	default = 40.0,
	label = "floor-drying-temp-5-47286",
	write = true
       },
     #item {
	address = 47287,
	title = "Floor drying temp. 4",
	description = "Supply temperature each period",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 15.0,
	max = 70.0,
	default = 45.0,
	label = "floor-drying-temp-4-47287",
	write = true
       },
     #item {
	address = 47288,
	title = "Floor drying temp. 3",
	description = "Supply temperature each period",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 15.0,
	max = 70.0,
	default = 40.0,
	label = "floor-drying-temp-3-47288",
	write = true
       },
     #item {
	address = 47289,
	title = "Floor drying temp. 2",
	description = "Supply temperature each period",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 15.0,
	max = 70.0,
	default = 30.0,
	label = "floor-drying-temp-2-47289",
	write = true
       },
     #item {
	address = 47290,
	title = "Floor drying temp. 1",
	description = "Supply temperature each period",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 15.0,
	max = 70.0,
	default = 20.0,
	label = "floor-drying-temp-1-47290",
	write = true
       },
     #item {
	address = 47292,
	title = "Trend temperature",
	description = "Above the set outdoor temperature the addition activation time is limited to give the compressor more time to raise the hot water temperature.",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 0.0,
	max = 200.0,
	default = 70.0,
	label = "trend-temperature-47292",
	write = true
       },
     #item {
	address = 47293,
	title = "Transfer time HW-Heat",
	description = "Time between hot water and heating operating mode",
	unit = "mins",
	type = int8,
	scale = 1,
	min = 1.0,
	max = 60.0,
	default = 45.0,
	label = "transfer-time-hw-heat-47293",
	write = true
       },
     #item {
	address = 47300,
	title = "DOT",
	description = "Dimensioning outdoor temperature",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = -400.0,
	max = 200.0,
	default = -180.0,
	label = "dot-47300",
	write = true
       },
     #item {
	address = 47301,
	title = "delta T at DOT",
	description = "Delta T (BT12-BT3)at dimensioning outdoor temperature",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 10.0,
	max = 250.0,
	default = 100.0,
	label = "delta-t-at-dot-47301",
	write = true
       },
     #item {
	address = 47302,
	title = "Climate system 2 accessory",
	description = "Activates the climate system 2 accessory 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "climate-system-2-accessory-47302",
	write = true

       },
     #item {
	address = 47303,
	title = "Climate system 3 accessory",
	description = "Activates the climate system 3 accessory 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "climate-system-3-accessory-47303",
	write = true

       },
     #item {
	address = 47304,
	title = "Climate system 4 accessory",
	description = "Activates the climate system 4 accessory 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "climate-system-4-accessory-47304",
	write = true

       },
     #item {
	address = 47305,
	title = "Climate system 4 mixing valve amp.",
	description = "Mixing valve amplification for extra climate systems",
	type = int8,
	scale = 10,
	min = 1.0,
	max = 100.0,
	default = 10.0,
	label = "climate-system-4-mixing-valve-amp-47305",
	write = true
       },
     #item {
	address = 47306,
	title = "Climate system 3 mixing valve amp.",
	description = "Mixing valve amplification for extra climate systems",
	type = int8,
	scale = 10,
	min = 1.0,
	max = 100.0,
	default = 10.0,
	label = "climate-system-3-mixing-valve-amp-47306",
	write = true
       },
     #item {
	address = 47307,
	title = "Climate system 2 mixing valve amp.",
	description = "Mixing valve amplification for extra climate systems",
	type = int8,
	scale = 10,
	min = 1.0,
	max = 100.0,
	default = 10.0,
	label = "climate-system-2-mixing-valve-amp-47307",
	write = true
       },
     #item {
	address = 47308,
	title = "Climate system 4 shunt wait",
	description = "Wait time between changes of the shunt in extra climate systems",
	unit = "secs",
	type = int16,
	scale = 10,
	min = 10.0,
	max = 300.0,
	default = 30.0,
	label = "climate-system-4-shunt-wait-47308",
	write = true
       },
     #item {
	address = 47309,
	title = "Climate system 3 shunt wait",
	description = "Wait time between changes of the shunt in extra climate systems",
	unit = "secs",
	type = int16,
	scale = 10,
	min = 10.0,
	max = 300.0,
	default = 30.0,
	label = "climate-system-3-shunt-wait-47309",
	write = true
       },
     #item {
	address = 47310,
	title = "Climate system 2 shunt wait",
	description = "Wait time between changes of the shunt in extra climate systems",
	unit = "secs",
	type = int16,
	scale = 10,
	min = 10.0,
	max = 300.0,
	default = 30.0,
	label = "climate-system-2-shunt-wait-47310",
	write = true
       },
     #item {
	address = 47351,
	title = "FJVM accessory",
	description = "Activates the FJVM accessory 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "fjvm-accessory-47351",
	write = true

       },
     #item {
	address = 47352,
	title = "SMS40 accessory",
	description = "Activates the SMS40 accessory",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "sms40-accessory-47352",
	write = true
       },
     #item {
	address = 47365,
	title = "RMU System 1",
	description = "Activates the RMU accessory for system 1",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "rmu-system-1-47365",
	write = true
       },
     #item {
	address = 47366,
	title = "RMU System 2",
	description = "Activates the RMU accessory for system 2",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "rmu-system-2-47366",
	write = true
       },
     #item {
	address = 47367,
	title = "RMU System 3",
	description = "Activates the RMU accessory for system 3",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "rmu-system-3-47367",
	write = true
       },
     #item {
	address = 47368,
	title = "RMU System 4",
	description = "Activates the RMU accessory for system 4",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "rmu-system-4-47368",
	write = true
       },
     #item {
	address = 47370,
	title = "Allow Additive Heating",
	description = "Whether to allow additive heating (only valid for operational mode Manual)",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 1.0,
	label = "allow-additive-heating-47370",
	write = true
       },
     #item {
	address = 47371,
	title = "Allow Heating",
	description = "Whether to allow heating (only valid for operational mode Manual or Add. heat only)",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 1.0,
	label = "allow-heating-47371",
	write = true
       },
     #item {
	address = 47372,
	title = "Allow Cooling",
	description = "Whether to allow cooling (only valid for operational mode Manual or Add. heat only)",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 1.0,
	label = "allow-cooling-47372",
	write = true
       },
     #item {
	address = 47374,
	title = "Start Temperature Cooling",
	description = "Start temperature for cooling, as set in menu 4.9.2 0=Off 1=On",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = -200.0,
	max = 400.0,
	default = 250.0,
	label = "start-temperature-cooling-47374",
	write = true
       },
     #item {
	address = 47375,
	title = "Stop Temperature Heating",
	description = "Stop temperature for heating, as set in menu 4.9.2",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = -200.0,
	max = 400.0,
	default = 170.0,
	label = "stop-temperature-heating-47375",
	write = true
       },
     #item {
	address = 47376,
	title = "Stop Temperature Additive",
	description = "Stop temperature for additive, as set in menu 4.9.2",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = -250.0,
	max = 400.0,
	default = 150.0,
	label = "stop-temperature-additive-47376",
	write = true
       },
     #item {
	address = 47377,
	title = "Outdoor Filter Time",
	description = " 12=12 Hours 24=24 Hours",
	unit = "h",
	type = {enum,uint8, [
			     {12, "12 Hours"},
			     {24, "24 Hours"}]
	       },
	scale = 1,
	min = 0.0,
	max = 48.0,
	default = 24.0,
	label = "outdoor-filter-time-47377",
	write = true

       },
     #item {
	address = 47378,
	title = "Max diff. comp.",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 10.0,
	max = 250.0,
	default = 100.0,
	label = "max-diff-comp-47378",
	write = true
       },
     #item {
	address = 47379,
	title = "Max diff. add.",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 10.0,
	max = 240.0,
	default = 70.0,
	label = "max-diff-add-47379",
	write = true
       },
     #item {
	address = 47384,
	title = "Date format",
	description = " 1=DD-MM-YY 2=YY-MM-DD",
	type = {enum,uint8, [
			     {1, "DD-MM-YY"},
			     {2, "YY-MM-DD"}]
	       },
	scale = 1,
	min = 1.0,
	max = 2.0,
	default = 1.0,
	label = "date-format-47384",
	write = true

       },
     #item {
	address = 47385,
	title = "Time format",
	description = " 12=12 hours 24=24 Hours",
	type = {enum,uint8, [
			     {12, "12 hours"},
			     {24, "24 hours"}]
	       },
	scale = 1,
	min = 12.0,
	max = 24.0,
	default = 24.0,
	label = "time-format-47385",
	write = true

       },
     #item {
	address = 47387,
	title = "HW production",
	description = "Activates hot water production where applicable 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "hw-production-47387",
	write = true

       },
     #item {
	address = 47388,
	title = "Alarm lower room temp.",
	description = "Lowers the room temperature during red light alarms to notify the occupants of the building that something is the matter 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "alarm-lower-room-temp-47388",
	write = true

       },
     #item {
	address = 47389,
	title = "Alarm lower HW temp.",
	description = "Lowers the hot water temperature during red light alarms to notify the occupants of the building that something is the matter 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 1.0,
	label = "alarm-lower-hw-temp-47389",
	write = true

       },
     #item {
	address = 47391,
	title = "Use room sensor S4",
	description = "When activated the system uses the room sensor 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "use-room-sensor-s4-47391",
	write = true

       },
     #item {
	address = 47392,
	title = "Use room sensor S3",
	description = "When activated the system uses the room sensor 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "use-room-sensor-s3-47392",
	write = true

       },
     #item {
	address = 47393,
	title = "Use room sensor S2",
	description = "When activated the system uses the room sensor 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "use-room-sensor-s2-47393",
	write = true

       },
     #item {
	address = 47394,
	title = "Use room sensor S1",
	description = "When activated the system uses the room sensor 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "use-room-sensor-s1-47394",
	write = true

       },
     #item {
	address = 47395,
	title = "Room sensor setpoint S4",
	description = "Sets the room temperature setpoint for the system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "room-sensor-setpoint-s4-47395",
	write = true
       },
     #item {
	address = 47396,
	title = "Room sensor setpoint S3",
	description = "Sets the room temperature setpoint for the system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "room-sensor-setpoint-s3-47396",
	write = true
       },
     #item {
	address = 47397,
	title = "Room sensor setpoint S2",
	description = "Sets the room temperature setpoint for the system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "room-sensor-setpoint-s2-47397",
	write = true
       },
     #item {
	address = 47398,
	title = "Room sensor setpoint S1",
	description = "Sets the room temperature setpoint for the system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "room-sensor-setpoint-s1-47398",
	write = true
       },
     #item {
	address = 47399,
	title = "Room sensor factor S4",
	description = "Setting of how much the difference between set and actual room temperature should affect the supply temperature.",
	type = uint8,
	scale = 10,
	min = 0.0,
	max = 60.0,
	default = 10.0,
	label = "room-sensor-factor-s4-47399",
	write = true
       },
     #item {
	address = 47400,
	title = "Room sensor factor S3",
	description = "Setting of how much the difference between set and actual room temperature should affect the supply temperature.",
	type = uint8,
	scale = 10,
	min = 0.0,
	max = 60.0,
	default = 10.0,
	label = "room-sensor-factor-s3-47400",
	write = true
       },
     #item {
	address = 47401,
	title = "Room sensor factor S2",
	description = "Setting of how much the difference between set and actual room temperature should affect the supply temperature.",
	type = uint8,
	scale = 10,
	min = 0.0,
	max = 60.0,
	default = 10.0,
	label = "room-sensor-factor-s2-47401",
	write = true
       },
     #item {
	address = 47402,
	title = "Room sensor factor S1",
	description = "Setting of how much the difference between set and actual room temperature should affect the supply temperature.",
	type = uint8,
	scale = 10,
	min = 0.0,
	max = 60.0,
	default = 10.0,
	label = "room-sensor-factor-s1-47402",
	write = true
       },
     #item {
	address = 47442,
	title = "preset flow clim. sys.",
	description = "Preset flow setting for climate system. 0 = manual setting, 1 = radiator, 2 = floor heating, 3 = radiator + floor heating.",
	type = {enum,uint8, [
			     {0, "manual setting"},
			     {1, "radiator"},
			     {2, "floor heating"},
			     {3, "radiator + floor heating"}]
	       },
	scale = 1,
	min = 0.0,
	max = 3.0,
	default = 1.0,
	label = "preset-flow-clim-sys-47442",
	write = true

       },
     #item {
	address = 47525,
	title = "Heat Curve S8",
	description = "Heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 15.0,
	default = 9.0,
	label = "heat-curve-s8-47525",
	write = true
       },
     #item {
	address = 47536,
	title = "Fan synch mode",
	description = "If the fan should have a lower speed when the compressor is not running 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "fan-synch-mode-47536",
	write = true

       },
     #item {
	address = 47537,
	title = "Night cooling",
	description = "If the fan should have a higher speed when there is a high room temp and a low outdoor temp. 0=Off 1=On",
	type = {enum,uint8, [
			     {0, "Off"},
			     {1, "On"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "night-cooling-47537",
	write = true

       },
     #item {
	address = 47538,
	title = "Start room temp. night cooling",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 20.0,
	max = 30.0,
	default = 25.0,
	label = "start-room-temp-night-cooling-47538",
	write = true
       },
     #item {
	address = 47539,
	title = "Night Cooling Min. diff.",
	description = "Minimum difference between room temp and outdoor temp to start night cooling",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 3.0,
	max = 10.0,
	default = 6.0,
	label = "night-cooling-min-diff-47539",
	write = true
       },
     #item {
	address = 47556,
	title = "DEH accessory",
	description = "Activates the DEH accessory",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "deh-accessory-47556",
	write = true
       },
     #item {
	address = 47564,
	title = "Allow Heating Sys1",
	description = "Whether to allow heating for system 1 (only valid for operational mode Manual or Add. heat only)",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 1.0,
	label = "allow-heating-sys1-47564",
	write = true
       },
     #item {
	address = 47565,
	title = "Allow Heating Sys2",
	description = "Whether to allow heating for system 2 (only valid for operational mode Manual or Add. heat only)",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 1.0,
	label = "allow-heating-sys2-47565",
	write = true
       },
     #item {
	address = 47567,
	title = "Heat Curve S7",
	description = "Heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 15.0,
	default = 9.0,
	label = "heat-curve-s7-47567",
	write = true
       },
     #item {
	address = 48043,
	title = "Holiday - Activated",
	description = "0=inactive, 10=active",
	type = {enum,uint8, [
			     {0, "inactive"},
			     {10, "active"}]
	       },
	scale = 1,
	min = 0.0,
	max = 10.0,
	default = 0.0,
	label = "holiday-activated-48043",
	write = true

       },
     #item {
	address = 48132,
	title = "Temporary Lux",
	description = "0=Off, 1=3h, 2=6h, 3=12h, 4=One time increase",
	type = {enum,int8, [
			    {0, "Off"},
			    {1, "3h"},
			    {2, "6h"},
			    {3, "12h"},
			    {4, "One time increase"}]
	       },
	scale = 1,
	label = "temporary-lux-48132",
	write = true

       },
     #item {
	address = 48282,
	title = "SG Ready heating",
	description = "Sets whether or not SG Ready should affect heating",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 1.0,
	label = "sg-ready-heating-48282",
	write = true
       },
     #item {
	address = 48283,
	title = "SG Ready cooling",
	description = "Sets whether or not SG Ready should affect cooling",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 1.0,
	label = "sg-ready-cooling-48283",
	write = true
       },
     #item {
	address = 48284,
	title = "SG Ready hot water",
	description = "Sets whether or not SG Ready should affect hot water",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 1.0,
	label = "sg-ready-hot-water-48284",
	write = true
       },
     #item {
	address = 48285,
	title = "SG Ready pool",
	description = "Sets whether or not SG Ready should affect pool",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 1.0,
	label = "sg-ready-pool-48285",
	write = true
       },
     #item {
	address = 48488,
	title = "Heat Curve S6",
	description = "Heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 15.0,
	default = 9.0,
	label = "heat-curve-s6-48488",
	write = true
       },
     #item {
	address = 48489,
	title = "Heat Curve S5",
	description = "Heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 15.0,
	default = 9.0,
	label = "heat-curve-s5-48489",
	write = true
       },
     #item {
	address = 48491,
	title = "Heat Offset S8",
	description = "Offset of the heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "heat-offset-s8-48491",
	write = true
       },
     #item {
	address = 48492,
	title = "Heat Offset S7",
	description = "Offset of the heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "heat-offset-s7-48492",
	write = true
       },
     #item {
	address = 48493,
	title = "Heat Offset S6",
	description = "Offset of the heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "heat-offset-s6-48493",
	write = true
       },
     #item {
	address = 48494,
	title = "Heat Offset S5",
	description = "Offset of the heat curve, see manual for more information",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "heat-offset-s5-48494",
	write = true
       },
     #item {
	address = 48495,
	title = "Min Supply System 8",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 200.0,
	label = "min-supply-system-8-48495",
	write = true
       },
     #item {
	address = 48496,
	title = "Min Supply System 7",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 200.0,
	label = "min-supply-system-7-48496",
	write = true
       },
     #item {
	address = 48497,
	title = "Min Supply System 6",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 200.0,
	label = "min-supply-system-6-48497",
	write = true
       },
     #item {
	address = 48498,
	title = "Min Supply System 5",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 700.0,
	default = 200.0,
	label = "min-supply-system-5-48498",
	write = true
       },
     #item {
	address = 48499,
	title = "Max Supply System 8",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 800.0,
	default = 600.0,
	label = "max-supply-system-8-48499",
	write = true
       },
     #item {
	address = 48500,
	title = "Max Supply System 7",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 800.0,
	default = 600.0,
	label = "max-supply-system-7-48500",
	write = true
       },
     #item {
	address = 48501,
	title = "Max Supply System 6",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 800.0,
	default = 600.0,
	label = "max-supply-system-6-48501",
	write = true
       },
     #item {
	address = 48502,
	title = "Max Supply System 5",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 800.0,
	default = 600.0,
	label = "max-supply-system-5-48502",
	write = true
       },
     #item {
	address = 48503,
	title = "External adjustment S8",
	description = "Change of the offset of the heat curve when closing the external adjustment input",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "external-adjustment-s8-48503",
	write = true
       },
     #item {
	address = 48504,
	title = "External adjustment S7",
	description = "Change of the offset of the heat curve when closing the external adjustment input",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "external-adjustment-s7-48504",
	write = true
       },
     #item {
	address = 48505,
	title = "External adjustment S6",
	description = "Change of the offset of the heat curve when closing the external adjustment input",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "external-adjustment-s6-48505",
	write = true
       },
     #item {
	address = 48506,
	title = "External adjustment S5",
	description = "Change of the offset of the heat curve when closing the external adjustment input",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "external-adjustment-s5-48506",
	write = true
       },
     #item {
	address = 48507,
	title = "External adjustment with room sensor S8",
	description = "Room temperature setting when closing the external adjustment input",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "external-adjustment-with-room-sensor-s8-48507",
	write = true
       },
     #item {
	address = 48508,
	title = "External adjustment with room sensor S7",
	description = "Room temperature setting when closing the external adjustment input",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "external-adjustment-with-room-sensor-s7-48508",
	write = true
       },
     #item {
	address = 48509,
	title = "External adjustment with room sensor S6",
	description = "Room temperature setting when closing the external adjustment input",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "external-adjustment-with-room-sensor-s6-48509",
	write = true
       },
     #item {
	address = 48510,
	title = "External adjustment with room sensor S5",
	description = "Room temperature setting when closing the external adjustment input",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "external-adjustment-with-room-sensor-s5-48510",
	write = true
       },
     #item {
	address = 48569,
	title = "Climate system 5 accessory",
	description = "Activates the climate system 5 accessory",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "climate-system-5-accessory-48569",
	write = true
       },
     #item {
	address = 48570,
	title = "Climate system 6 accessory",
	description = "Activates the climate system 6 accessory",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "climate-system-6-accessory-48570",
	write = true
       },
     #item {
	address = 48571,
	title = "Climate system 7 accessory",
	description = "Activates the climate system 7 accessory",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "climate-system-7-accessory-48571",
	write = true
       },
     #item {
	address = 48572,
	title = "Climate system 8 accessory",
	description = "Activates the climate system 8 accessory",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "climate-system-8-accessory-48572",
	write = true
       },
     #item {
	address = 48573,
	title = "Climate system 8 mixing valve amp.",
	description = "Mixing valve amplification for extra climate systems",
	type = int8,
	scale = 10,
	min = 1.0,
	max = 100.0,
	default = 10.0,
	label = "climate-system-8-mixing-valve-amp-48573",
	write = true
       },
     #item {
	address = 48574,
	title = "Climate system 7 mixing valve amp.",
	description = "Mixing valve amplification for extra climate systems",
	type = int8,
	scale = 10,
	min = 1.0,
	max = 100.0,
	default = 10.0,
	label = "climate-system-7-mixing-valve-amp-48574",
	write = true
       },
     #item {
	address = 48575,
	title = "Climate system 6 mixing valve amp.",
	description = "Mixing valve amplification for extra climate systems",
	type = int8,
	scale = 10,
	min = 1.0,
	max = 100.0,
	default = 10.0,
	label = "climate-system-6-mixing-valve-amp-48575",
	write = true
       },
     #item {
	address = 48576,
	title = "Climate system 5 mixing valve amp.",
	description = "Mixing valve amplification for extra climate systems",
	type = int8,
	scale = 10,
	min = 1.0,
	max = 100.0,
	default = 10.0,
	label = "climate-system-5-mixing-valve-amp-48576",
	write = true
       },
     #item {
	address = 48577,
	title = "Climate system 8 shunt wait",
	description = "Wait time between changes of the shunt in extra climate systems",
	unit = "secs",
	type = int16,
	scale = 10,
	min = 10.0,
	max = 300.0,
	default = 30.0,
	label = "climate-system-8-shunt-wait-48577",
	write = true
       },
     #item {
	address = 48578,
	title = "Climate system 7 shunt wait",
	description = "Wait time between changes of the shunt in extra climate systems",
	unit = "secs",
	type = int16,
	scale = 10,
	min = 10.0,
	max = 300.0,
	default = 30.0,
	label = "climate-system-7-shunt-wait-48578",
	write = true
       },
     #item {
	address = 48579,
	title = "Climate system 6 shunt wait",
	description = "Wait time between changes of the shunt in extra climate systems",
	unit = "secs",
	type = int16,
	scale = 10,
	min = 10.0,
	max = 300.0,
	default = 30.0,
	label = "climate-system-6-shunt-wait-48579",
	write = true
       },
     #item {
	address = 48580,
	title = "Climate system 5 shunt wait",
	description = "Wait time between changes of the shunt in extra climate systems",
	unit = "secs",
	type = int16,
	scale = 10,
	min = 10.0,
	max = 300.0,
	default = 30.0,
	label = "climate-system-5-shunt-wait-48580",
	write = true
       },
     #item {
	address = 48675,
	title = "Use room sensor S8",
	description = "When activated the system uses the room sensor",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "use-room-sensor-s8-48675",
	write = true
       },
     #item {
	address = 48676,
	title = "Use room sensor S7",
	description = "When activated the system uses the room sensor",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "use-room-sensor-s7-48676",
	write = true
       },
     #item {
	address = 48677,
	title = "Use room sensor S6",
	description = "When activated the system uses the room sensor",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "use-room-sensor-s6-48677",
	write = true
       },
     #item {
	address = 48678,
	title = "Use room sensor S5",
	description = "When activated the system uses the room sensor",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "use-room-sensor-s5-48678",
	write = true
       },
     #item {
	address = 48680,
	title = "Room sensor setpoint S8",
	description = "Sets the room temperature setpoint for the system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "room-sensor-setpoint-s8-48680",
	write = true
       },
     #item {
	address = 48681,
	title = "Room sensor setpoint S7",
	description = "Sets the room temperature setpoint for the system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "room-sensor-setpoint-s7-48681",
	write = true
       },
     #item {
	address = 48682,
	title = "Room sensor setpoint S6",
	description = "Sets the room temperature setpoint for the system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "room-sensor-setpoint-s6-48682",
	write = true
       },
     #item {
	address = 48683,
	title = "Room sensor setpoint S5",
	description = "Sets the room temperature setpoint for the system",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 300.0,
	default = 200.0,
	label = "room-sensor-setpoint-s5-48683",
	write = true
       },
     #item {
	address = 48685,
	title = "Room sensor factor S8",
	description = "Setting of how much the difference between set and actual room temperature should affect the supply temperature.",
	type = uint8,
	scale = 10,
	min = 0.0,
	max = 60.0,
	default = 10.0,
	label = "room-sensor-factor-s8-48685",
	write = true
       },
     #item {
	address = 48686,
	title = "Room sensor factor S7",
	description = "Setting of how much the difference between set and actual room temperature should affect the supply temperature.",
	type = uint8,
	scale = 10,
	min = 0.0,
	max = 60.0,
	default = 10.0,
	label = "room-sensor-factor-s7-48686",
	write = true
       },
     #item {
	address = 48687,
	title = "Room sensor factor S6",
	description = "Setting of how much the difference between set and actual room temperature should affect the supply temperature.",
	type = uint8,
	scale = 10,
	min = 0.0,
	max = 60.0,
	default = 10.0,
	label = "room-sensor-factor-s6-48687",
	write = true
       },
     #item {
	address = 48688,
	title = "Room sensor factor S5",
	description = "Setting of how much the difference between set and actual room temperature should affect the supply temperature.",
	type = uint8,
	scale = 10,
	min = 0.0,
	max = 60.0,
	default = 10.0,
	label = "room-sensor-factor-s5-48688",
	write = true
       },
     #item {
	address = 48732,
	title = "Cool offset S8",
	description = "Offset of the cool curve, see manual for more information ",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "cool-offset-s8-48732",
	write = true
       },
     #item {
	address = 48733,
	title = "Cool offset S7",
	description = "Offset of the cool curve, see manual for more information ",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "cool-offset-s7-48733",
	write = true
       },
     #item {
	address = 48734,
	title = "Cool offset S6",
	description = "Offset of the cool curve, see manual for more information ",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "cool-offset-s6-48734",
	write = true
       },
     #item {
	address = 48735,
	title = "Cool offset S5",
	description = "Offset of the cool curve, see manual for more information ",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "cool-offset-s5-48735",
	write = true
       },
     #item {
	address = 48736,
	title = "Cool offset S4",
	description = "Offset of the cool curve, see manual for more information ",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "cool-offset-s4-48736",
	write = true
       },
     #item {
	address = 48737,
	title = "Cool offset S3",
	description = "Offset of the cool curve, see manual for more information ",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "cool-offset-s3-48737",
	write = true
       },
     #item {
	address = 48738,
	title = "Cool offset S2",
	description = "Offset of the cool curve, see manual for more information ",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "cool-offset-s2-48738",
	write = true
       },
     #item {
	address = 48739,
	title = "Cool offset S1",
	description = "Offset of the cool curve, see manual for more information ",
	type = int8,
	scale = 1,
	min = -10.0,
	max = 10.0,
	default = 0.0,
	label = "cool-offset-s1-48739",
	write = true
       },
     #item {
	address = 48755,
	title = "Transformer ratio",
	description = "Ratio of the current measurement transformers",
	type = "u16",
	scale = 1,
	min = 300.0,
	max = 2500.0,
	default = 300.0,
	label = "transformer-ratio-48755",
	write = true
       },
     #item {
	address = 48794,
	title = "RH set value",
	description = "RH set value",
	unit = "%",
	type = int8,
	scale = 1,
	min = 30.0,
	max = 90.0,
	default = 60.0,
	label = "rh-set-value-48794",
	write = true
       },
     #item {
	address = 48810,
	title = "Prevent humidity S8 ",
	description = " ",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "prevent-humidity-s8-48810",
	write = true
       },
     #item {
	address = 48811,
	title = "Prevent humidity S7",
	description = " ",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "prevent-humidity-s7-48811",
	write = true
       },
     #item {
	address = 48812,
	title = "Prevent humidity S6",
	description = " ",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "prevent-humidity-s6-48812",
	write = true
       },
     #item {
	address = 48813,
	title = "Prevent humidity S5",
	description = " ",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "prevent-humidity-s5-48813",
	write = true
       },
     #item {
	address = 48814,
	title = "Prevent humidity S4",
	description = " ",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "prevent-humidity-s4-48814",
	write = true
       },
     #item {
	address = 48815,
	title = "Prevent humidity S3",
	description = " ",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "prevent-humidity-s3-48815",
	write = true
       },
     #item {
	address = 48816,
	title = "Prevent humidity S2",
	description = " ",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "prevent-humidity-s2-48816",
	write = true
       },
     #item {
	address = 48817,
	title = "Prevent humidity S1",
	description = " ",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "prevent-humidity-s1-48817",
	write = true
       },
     #item {
	address = 48852,
	title = "Modbus40 Word Swap",
	description = "If set; swapping the words in 32-bit variables when value requested via ''read holding register'' commando.",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 1.0,
	label = "modbus40-word-swap-48852",
	write = true
       },
     #item {
	address = 48889,
	title = "MODBUS40 Disable LOG.SET",
	description = "If set, the system will ignore the existing LOG.SET on the USB stick.1=ignore LOG.SET,0=use LOG.SET",
	type = {enum,uint8, [
			     {0, "use LOG.SET"},
			     {1, "ignore LOG.SET"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "modbus40-disable-log-set-48889",
	write = true

       },
     #item {
	address = 48926,
	title = "Humidity factor",
	description = "Setting of how much the difference between set and actual room humidity should affect the supply temperature.",
	type = uint8,
	scale = 10,
	min = 5.0,
	max = 100.0,
	default = 10.0,
	label = "humidity-factor-48926",
	write = true
       },
     #item {
	address = 48927,
	title = "Humidity cool factor",
	description = "Setting of how much the difference between set and actual room humidity should affect the supply temperature in cooling mode.",
	type = uint8,
	scale = 10,
	label = "humidity-cool-factor-48927",
	write = true
       },
     #item {
	address = 48930,
	title = "EME10 Activated",
	description = "0=not activated, 1=activated",
	type = {enum,uint8, [
			     {0, "not activated"},
			     {1, "activated"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "eme10-activated-48930",
	write = true

       },
     #item {
	address = 48931,
	title = "EME PV Panel Affect Heating",
	description = "0=not affecting, 1=affecting",
	type = {enum,uint8, [
			     {0, "not affecting"},
			     {1, "affecting"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "eme-pv-panel-affect-heating-48931",
	write = true

       },
     #item {
	address = 48932,
	title = "EME PV Panel Affect Hot Water",
	description = "0=not affecting, 1=affecting",
	type = {enum,uint8, [
			     {0, "not affecting"},
			     {1, "affecting"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "eme-pv-panel-affect-hot-water-48932",
	write = true

       },
     #item {
	address = 48968,
	title = "FLM 1 set point, cooling",
	description = "Set point, cooling, when using FLM cooling",
	unit = "\x{00b0C}",
	type = int16,
	scale = 10,
	min = 50.0,
	max = 400.0,
	default = 210.0,
	label = "flm-1-set-point-cooling-48968",
	write = true
       },
     #item {
	address = 48970,
	title = "Outdoor Air Mixing function",
	description = "Activates the Outdoor Air Mixing function.",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "outdoor-air-mixing-function-48970",
	write = true
       },
     #item {
	address = 48976,
	title = "Smart home room control",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "smart-home-room-control-48976",
	write = true
       },
     #item {
	address = 49289,
	title = "Ground water pump speed control ",
	description = " ",
	type = int8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "ground-water-pump-speed-control-49289",
	write = true
       },
     #item {
	address = 49297,
	title = "EME20 Activated",
	description = "0=not activated, 1=activated",
	type = {enum,uint8, [
			     {0, "not activated"},
			     {1, "activated"}]
	       },
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "eme20-activated-49297",
	write = true

       },
     #item {
	address = 49298,
	title = "EME PV Panel Affect Pool",
	description = "0=not affecting, 1=affecting",
	type = {enum,uint8, [
			     {0, "not affecting"},
			     {1, "affecting"}]
	       },
	scale = 1,
	label = "eme-pv-panel-affect-pool-49298",
	write = true

       },
     #item {
	address = 49358,
	title = "Night cooling",
	description = "If the fan should have a higher speed when there is a high room temp and a low outdoor temp.",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "night-cooling-49358",
	write = true
       },
     #item {
	address = 49359,
	title = "Night cooling",
	description = "If the fan should have a higher speed when there is a high room temp and a low outdoor temp.",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "night-cooling-49359",
	write = true
       },
     #item {
	address = 49360,
	title = "Night cooling",
	description = "If the fan should have a higher speed when there is a high room temp and a low outdoor temp.",
	type = uint8,
	scale = 1,
	min = 0.0,
	max = 1.0,
	default = 0.0,
	label = "night-cooling-49360",
	write = true
       },
     #item {
	address = 49361,
	title = "Start room temp. night cooling",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 20.0,
	max = 30.0,
	default = 25.0,
	label = "start-room-temp-night-cooling-49361",
	write = true
       },
     #item {
	address = 49362,
	title = "Start room temp. night cooling",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 20.0,
	max = 30.0,
	default = 25.0,
	label = "start-room-temp-night-cooling-49362",
	write = true
       },
     #item {
	address = 49363,
	title = "Start room temp. night cooling",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 20.0,
	max = 30.0,
	default = 25.0,
	label = "start-room-temp-night-cooling-49363",
	write = true
       },
     #item {
	address = 49364,
	title = "Night Cooling Min. diff.",
	description = "Minimum difference between room temp and outdoor temp to start night cooling",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 3.0,
	max = 10.0,
	default = 6.0,
	label = "night-cooling-min-diff-49364",
	write = true
       },
     #item {
	address = 49365,
	title = "Night Cooling Min. diff.",
	description = "Minimum difference between room temp and outdoor temp to start night cooling",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 3.0,
	max = 10.0,
	default = 6.0,
	label = "night-cooling-min-diff-49365",
	write = true
       },
     #item {
	address = 49366,
	title = "Night Cooling Min. diff.",
	description = "Minimum difference between room temp and outdoor temp to start night cooling",
	unit = "\x{00b0C}",
	type = uint8,
	scale = 1,
	min = 3.0,
	max = 10.0,
	default = 6.0,
	label = "night-cooling-min-diff-49366",
	write = true
       },
     #item {
	address = 49431,
	title = "Stop Temperature Supply Air Heating",
	description = "Stop temperature for supply air heating",
	unit = "\x{00b0C}",
	type = int16,
	scale = 1,
	min = 100.0,
	max = 400.0,
	default = 170.0,
	label = "stop-temperature-supply-air-heating-49431",
	write = true
       },
     #item {
	address = 47260,
	title = "Fan Mode",
	description = "0=Normal 1=Fan speed 1, 2=Fan speed 2, 3=Fan speed 3, 4=Fan speed 4",
	type = {enum,uint8, [
			     {0, "Normal"},
			     {1, "Fan mode 1"},
			     {2, "Fan mode 2"},
			     {3, "Fan mode 3"},
			     {4, "Fan mode 4"}]
	       },
	scale = 1,
	min = 0,
	max = 4,
	default = 0,
	label = "fan-mode-47260",
	write = true

       }
    ].

