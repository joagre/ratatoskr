-module(ackermann).
-export([test/0]).

test() ->
    Master = spawn(fun() -> master_server(0) end),
    Master ! {start_slave, 3, 10},
    Master ! {start_slave, 3, 11},
    Ref = make_ref(),
    Master ! {started_slaves, {self(), Ref}},
    receive
        {2, Ref} ->
            ok;
        BadMessage ->
            throw({bad_message, BadMessage})
    end.

master_server(StartedSlaves) ->
    receive
        {start_slave, M, N} ->
            Master = self(),
            Slave = spawn(fun() -> slave_server(Master) end),
            Slave ! {start, M, N},
            master_server(StartedSlaves + 1);
        {slave_running, M, N} ->
            io:format("Slave ~p:~p started~n", [M, N]),
            master_server(StartedSlaves);
        {slave_done, M, N, Result} ->
            io:format("Slave ~p:~p calculated ~p~n", [M, N, Result]),
            master_server(StartedSlaves);
        {started_slaves, {From, Ref}} ->
            From ! {StartedSlaves, Ref},
            master_server(StartedSlaves)
    end.

slave_server(Master) ->
    receive
        {start, M, N} ->
            Master ! {slave_running, M, N},
            Result = ackermann(M, N),
            Master ! {slave_done, M, N, Result},
            slave_server(Master)
    end.

ackermann(0, N) ->
    N + 1;
ackermann(M, 0) when M > 0 ->
    ackermann(M - 1, 1);
ackermann(M, N) ->
    ackermann(M - 1, ackermann(M, N - 1)).
