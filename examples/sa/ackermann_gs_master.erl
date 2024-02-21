-module(ackermann_gs_master).
-behaviour(gen_server).

-export([start_link/0, start_slave/3, slave_running/3, slave_done/4,
         started_slaves/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {started_slaves = 0}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_slave(Master, M, N) ->
    gen_server:cast(Master, {start_slave, M, N}).

slave_running(Master, M, N) ->
    gen_server:cast(Master, {slave_running, M, N}).

slave_done(Master, M, N, Result) ->
    gen_server:cast(Master, {slave_done, M, N, Result}).

started_slaves(Master) ->
    gen_server:call(Master, started_slaves).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(started_slaves, _From, State) ->
    {reply, State#state.started_slaves, State}.

handle_cast({start_slave, M, N}, State) ->
    {ok, Slave} = ackermann_gs_slave:start_link(self()),
    ackermann_gs_slave:start(Slave, M, N),
    {noreply, State#state{started_slaves = State#state.started_slaves + 1}};
handle_cast({slave_running, M, N}, State) ->
    io:format("Slave ~p:~p started~n", [M, N]),
    {noreply, State};
handle_cast({slave_done, M, N, Result}, State) ->
    io:format("Slave ~p:~p calculated ~p~n", [M, N, Result]),
    {noreply, State}.
