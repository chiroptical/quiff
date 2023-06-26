-module(task_manager).
-behaviour(gen_server).

-export([
    start_link/0,

    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2
]).

-record(state, {pids = maps:new() :: maps:iterator(string(), pid())}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

terminate(_Reason, _State) ->
    ok.

handle_call({new_worker, Name}, _From, State = #state{pids = Pids}) ->
    case maps:find(Name, Pids) of
        error ->
            {reply, make_new_worker, State};
        {ok, Pid} ->
            case is_process_alive(Pid) of
                false ->
                    {reply, make_new_worker, State};
                true ->
                    %% Worker already exists, send it a message to ensure it
                    %% lives for another 30 minutes.
                    {reply, {worker_exists, Pid}, State}
            end
    end;
handle_call({worker_info, Name}, _From, State = #state{pids = Pids}) ->
    case maps:find(Name, Pids) of
        error ->
            {reply, no_worker_exists, State};
        {ok, Pid} ->
            case is_process_alive(Pid) of
                false ->
                    {reply, worker_is_dead, State};
                true ->
                    %% Worker is running send it a message to ensure it
                    %% lives for another 30 minutes.
                    {reply, {worker_is_running, Pid}, State}
            end
    end;
handle_call(Msg, _From, State) ->
    io:format("Unrecognized message: ~p~n", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    io:format("Unrecognized message: ~p~n", [Msg]),
    {noreply, State}.
