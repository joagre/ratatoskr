-module(ackermann).
-export([ackermann/0, ackermann/2]).

ackermann() ->
    ackermann(4, 1).

ackermann(0, N) ->
    N + 1;
ackermann(M, 0) ->
    ackermann(M - 1, 1);
ackermann(M, N) ->
    ackermann(M - 1, ackermann(M, N - 1)).
