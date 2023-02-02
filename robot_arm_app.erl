%%%-------------------------------------------------------------------
%% @doc robot_arm public API
%% @end
%%%-------------------------------------------------------------------

-module(robot_arm_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    robot_arm_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
