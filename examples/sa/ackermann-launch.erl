-module('ackermann-launch').
-compile(export_all).

main(_) ->
    Master = spawn(fun() -> master_server([]) end),
    Master ! {start_slaves, 3, 10}.

master_server(Slaves) ->
    receive
        {start_slaves, M, N} ->
            Slaves = start_slaves(M, N),
            master_server(Slaves);
        {slave_started, Slave, M, N} ->
            io:format("Slave ~p:~p started~n", [M, N]),
            master_server([Slave|Slaves]);
        {slave_done, Slave, M, N, Result} ->
            io:format("Slave ~p:~p calculated ~p~n", [M, N, Result]),
            master_server(Slaves -- [Slave])
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
            Master ! {slave_started, self(), M, N},
            Result = ackermann_function(M, N),
            Master ! {slave_done, self(), M, N, Result},
            slave_server(Master)
    end.

ackermann_function(M, N) when M == 0 ->
    N + 1;
ackermann_function(M, N) when N /= 0 ->
    ackermann_function(M - 1, 1);
ackermann_function(M, N) ->
    ackermann_function(M - 1, ackermann_function(M, N - 1)).
