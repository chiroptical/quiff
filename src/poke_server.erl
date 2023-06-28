-module(poke_server).
-behaviour(gen_server).

-export([
    start_link/3,
    init/1,
    server_name/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {name, timeout = infinity}).

server_name(Name) ->
    ModuleBinary = atom_to_binary(?MODULE, utf8),
    NameBinary = atom_to_binary(Name, utf8),
    binary_to_atom(<<ModuleBinary/binary, <<"$">>/binary, NameBinary/binary>>, utf8).

start_link(Server, Name, Timeout) ->
    gen_server:start_link({local, Server}, ?MODULE, [Name, Timeout], []).

init([Name, Timeout]) ->
    process_flag(trap_exit, true),
    {ok, #state{name = Name, timeout = Timeout}, Timeout}.

terminate(_Reason, _State) ->
    ok.

reply(Msg, State = #state{timeout = Timeout}) ->
    {reply, Msg, State, Timeout}.

noreply(State = #state{timeout = Timeout}) ->
    {noreply, State, Timeout}.

handle_call(poke, _From, State = #state{name = Name}) ->
    reply({poke, Name}, State);
handle_call(Msg, _From, State) ->
    io:format("Unrecognized message: ~p~n", [Msg]),
    noreply(State).

handle_cast(Msg, State) ->
    io:format("Unrecognized message: ~p~n", [Msg]),
    noreply(State).

handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(Msg, State) ->
    io:format("Unrecognized message: ~p~n", [Msg]),
    noreply(State).
