-module(ackermann_gs).
-export([test/0]).

test() ->
    {ok, Master} = ackermann_gs_master:start_link(),
    ok = ackermann_gs_master:start_slave(Master, 3, 10),
    ok = ackermann_gs_master:start_slave(Master, 3, 11),
    2 = ackermann_gs_master:started_slaves(Master).
