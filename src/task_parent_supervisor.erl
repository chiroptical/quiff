-module(task_parent_supervisor).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1,
    shutdown/0
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupervisorFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [],
    {ok, {SupervisorFlags, ChildSpecs}}.

shutdown() ->
    exit(whereis(?MODULE), shutdown).
