-module(ackermann_gs_slave).
-behaviour(gen_server).

-export([start_link/1, start/3]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {master}).

start_link(Master) ->
    gen_server:start_link(?MODULE, [Master], []).

start(Slave, M, N) ->
    gen_server:cast(Slave, {start, M, N}).

init([Master]) ->
    {ok, #state{master = Master}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start, M, N}, #state{master = Master} = State) ->
    ackermann_gs_master:slave_running(Master, M, N),
    Result = ackermann(M, N),
    ackermann_gs_master:slave_done(Master, M, N, Result),
    {noreply, State}.

ackermann(0, N) ->
    N + 1;
ackermann(M, 0) when M > 0 ->
    ackermann(M - 1, 1);
ackermann(M, N) ->
    ackermann(M - 1, ackermann(M, N - 1)).
