-module(poke_main_supervisor).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1,
    start_pokee/2,
    poke/1
]).

-define(DEFAULT_TIMEOUT, 60000).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpec =
        {poke_supervisor, {poke_supervisor, start_link, []}, transient, 5000, worker, [
            poke_supervisor
        ]},
    {ok, {SupFlags, [ChildSpec]}}.

start_pokee(Name, Timeout) ->
    SupServer = poke_supervisor:server_name(Name),
    supervisor:start_child(?MODULE, [SupServer, Name, Timeout]).

poke(Name) ->
    ChildServer = poke_server:server_name(Name),
    case whereis(ChildServer) of
        undefined ->
            start_pokee(Name, ?DEFAULT_TIMEOUT),
            gen_server:call(ChildServer, poke);
        _Pid ->
            gen_server:call(ChildServer, poke)
    end.
