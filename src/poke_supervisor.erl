-module(poke_supervisor).
-behavior(supervisor).

-export([
    start_link/3,
    init/1,
    server_name/1
]).

server_name(Name) ->
    ModuleBinary = atom_to_binary(?MODULE, utf8),
    NameBinary = atom_to_binary(Name, utf8),
    binary_to_atom(<<ModuleBinary/binary, <<"$">>/binary, NameBinary/binary>>, utf8).

start_link(SupServer, Name, Timeout) ->
    ChildServer = poke_server:server_name(Name),
    supervisor:start_link({local, SupServer}, ?MODULE, [ChildServer, Name, Timeout]).

init([ChildServer, Name, Timeout]) ->
    SupFlags = #{
        strategy => one_for_one,
        auto_shutdown => all_significant
    },
    ChildSpec =
        #{
            id => poke_server,
            start => {poke_server, start_link, [ChildServer, Name, Timeout]},
            restart => transient,
            shutdown => 5000,
            type => worker,
            modules => [poke_server],
            significant => true
        },
    {ok, {SupFlags, [ChildSpec]}}.
