# modbus

To test modbus (tcp) you can start a demo server

    modbus_tcp_server:start_link([{port, 8899}, {callback, modbus_demo_callback}]).

Then in an other termial window

    {ok,Pid} = modbus_tcp_client:start_link([{port,8899}, {host,"localhost"}]).

And now run som modbus commands

    > modbus:read_holding_registers(Pid, 0, 5).
    {ok,[29070,47385,61985,32865,20403]}

    > modbus:read_coils(Pid, 7, 9).                  
    {ok,[1,1,1,0,1,0,0,1,0]}

    > modbus:write_multiple_coils(Pid, 7, [1,1,1,0,1,0,0,1,1]).
    ok

And so on
