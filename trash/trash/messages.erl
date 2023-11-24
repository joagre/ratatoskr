-module(messages).
-compile(export_all).

start(N) ->
    spawn_all(self(), N),
    wait_for_all(N).

spawn_all(_Self, 0) ->
    io:format("All jobs have been started\n");
spawn_all(Self, N) ->
    spawn(fun() -> Self ! ackermann(3, N) end),
    spawn_all(Self, N - 1).

wait_for_all(0) ->
    io:format("All jobs have returned a result\n");
wait_for_all(N) ->
    receive
        Result ->
            io:format("~w: ~w\n",  [N, Result]),
            wait_for_all(N - 1)
    end.

ackermann(0, N) ->
    N + 1;
ackermann(M, 0) when M > 0 ->
    ackermann(M - 1, 1);
ackermann(M, N) when M > 0, N > 0 ->
    ackermann(M - 1, ackermann(M, N - 1)).
