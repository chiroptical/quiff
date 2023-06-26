-module(task_supervisor).
-behaviour(supervisor).

-export([
    start_link/1,
    init/1
]).

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one},
    ChildSpec =
        {task_server, {task_server, start_link, []}, transient, 2000, worker, [task_server]},
    {ok, {SupFlags, [ChildSpec]}}.
