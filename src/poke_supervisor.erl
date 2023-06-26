-module(poke_supervisor).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1,

    start_pokee/2,
    poke/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpec =
        {poke_server, {poke_server, start_link, []}, transient, 5000, worker, [poke_server]},
    {ok, {SupFlags, [ChildSpec]}}.

start_pokee(Name, Timeout) ->
    Server = poke_server:server_name(Name),
    supervisor:start_child(?MODULE, [Server, Name, Timeout]).

poke(Name) ->
    Server = poke_server:server_name(Name),
    gen_server:call(Server, poke).
