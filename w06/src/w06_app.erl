%%%-------------------------------------------------------------------
%% @doc w06 public API
%% @end
%%%-------------------------------------------------------------------

-module(w06_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    w06_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
