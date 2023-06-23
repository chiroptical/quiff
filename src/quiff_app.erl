%%%-------------------------------------------------------------------
%% @doc quiff public API
%% @end
%%%-------------------------------------------------------------------

-module(quiff_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    quiff_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
