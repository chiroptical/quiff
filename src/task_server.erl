-module(task_server).
-behaviour(gen_server).
%% Public API
-export([
    start_link/0,
    next_task/1,
    add_task/2,
    complete_task/2,

    %% gen_server
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% If the gen_server doesn't recieve a new message in 30 minutes, we'll kill
%% the server.
%% TODO: Add this to {reply, Response, NextState, NO_MESSAGES_TIMEOUT}
%% Need to read about system messages that may be sent to the server
%% TODO: Switch this to 10 or 30 minutes?
-define(NO_MESSAGES_TIMEOUT, 60_000).

-record(state, {current_tasks = queue:new() :: queue:queue(nonempty_string())}).

%% TODO: May want to start a gen_server per user
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}, ?NO_MESSAGES_TIMEOUT}.

terminate(_Reason, _State) ->
    ok.

next_task(Pid) ->
    gen_server:call(Pid, next_task).

add_task(Pid, Task) ->
    gen_server:call(Pid, {add_task, Task}).

complete_task(Pid, Task) ->
    gen_server:call(Pid, {complete_task, Task}).

reply(Msg, State) ->
    {reply, Msg, State, ?NO_MESSAGES_TIMEOUT}.

noreply(State) ->
    {noreply, State, ?NO_MESSAGES_TIMEOUT}.

handle_call(next_task, _From, State = #state{current_tasks = CurrentTasks}) ->
    case queue:out(CurrentTasks) of
        %% The queue is empty, nothing to yield
        {empty, _Next} ->
            reply(tasks_empty, State);
        {{value, Top}, _Next} ->
            reply({next_task, Top}, State)
    end;
handle_call({add_task, Task}, _From, State = #state{current_tasks = CurrentTasks}) ->
    case string:is_empty(Task) of
        true ->
            reply(task_empty, State);
        false ->
            reply({task_added, Task}, State#state{current_tasks = queue:in(Task, CurrentTasks)})
    end;
handle_call({complete_task, Task}, _From, State = #state{current_tasks = CurrentTasks}) ->
    case string:is_empty(Task) of
        true ->
            reply(task_empty, State);
        false ->
            case queue:out(CurrentTasks) of
                %% The queue is empty, nothing to complete
                {empty, _Next} ->
                    reply(tasks_empty, State);
                %% The queue is not empty, and Top task matches the completed one.
                {{value, Top}, Next} when Top == Task ->
                    reply({task_completed, Task}, State#state{current_tasks = Next});
                %% The queue is not empty, but Top task doesn't match completed task.
                %% Could indicate the task was already marked as completed, so we need
                %% a new set of tasks.
                {{value, _}, Next} ->
                    reply({task_gone, Task}, State#state{current_tasks = Next})
            end
    end;
handle_call(Msg, _From, State) ->
    io:format("Unrecognized message: ~p~n", [Msg]),
    noreply(State).

handle_cast(Msg, State) ->
    io:format("Unrecognized message: ~p~n", [Msg]),
    noreply(State).

%% If we don't recieve a message for awhile, we'll just terminate the gen_server
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(Msg, State) ->
    io:format("Unrecognized message: ~p~n", [Msg]),
    reply(unknown_message, State).
