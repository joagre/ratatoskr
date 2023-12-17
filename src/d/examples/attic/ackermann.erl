-module(ackermann).
-compile(export_all).

start_4_1() ->
    ackermann(4, 1),
    erlang:halt().

ackermann(0, N) ->
    N + 1;
ackermann(M, 0) when M > 0 ->
    ackermann(M - 1, 1);
ackermann(M, N) when M > 0, N > 0 ->
    ackermann(M - 1, ackermann(M, N - 1)).
