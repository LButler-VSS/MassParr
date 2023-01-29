%%%-------------------------------------------------------------------
%% @doc w04 public API
%% @end
%%%-------------------------------------------------------------------

-module(w04_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    w04_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
