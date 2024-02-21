-module(ackermann_launch).
-compile(export_all).

test() ->
    Master = spawn(fun() -> master_server() end),
    Master ! {start_slaves, 3, 10}.

master_server() ->
    receive
        {start_slaves, M, N} ->
            start_slaves(M, N),
            master_server();
        {slave_started, M, N} ->
            io:format("Slave ~p:~p started~n", [M, N]),
            master_server();
        {slave_done, M, N, Result} ->
            io:format("Slave ~p:~p calculated ~p~n", [M, N, Result]),
            master_server()
    end.

start_slaves(_M, 0) ->
    [];
start_slaves(M, N) ->
    Master = self(),
    Slave = spawn(fun() -> slave_server(Master) end),
    Slave ! {start, M, N},
    [Slave|start_slaves(M, N - 1)].

slave_server(Master) ->
    receive
        {start, M, N} ->
            Master ! {slave_started, M, N},
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
