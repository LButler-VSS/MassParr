

%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright Â© 2022, Lee S. Barney
%%% @reference Licensed under the 
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% documentation goes here
%%% @end

%%% Created : 24 June 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(packing_arm_controller).
-behaviour(gen_statem).


%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/2,start_link/2,stop/1]).

%% Supervisor Callbacks
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
%% State Callbacks
-export([handle_event/4]).


%%%===================================================================
%%% Public API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),term()) -> {ok, atom()}.
start(Statem_name,Initial_state) ->
    gen_statem:start({local,Statem_name}, ?MODULE, Initial_state, []).

%%--------------------------------------------------------------------
%% @doc
%% 
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------


-spec start_link(atom(),term()) -> {ok, atom()}.
start_link(Statem_name,Initial_state) ->
    gen_statem:start_link({local,Statem_name},?MODULE,Initial_state,[]).


%%--------------------------------------------------------------------
%% @doc
%% This function gracefully shuts down the state machine.
%%
%% The parameter of stop is an atom that is a registered name 
%% 
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom()) -> ok.
stop(Statem_name) ->
    gen_statem:stop(Statem_name).

%% Mandatory callback functions
%% @private
terminate(_Reason, _State, _Data) ->
    void.
%% @private
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

%% @private
init({Height, Plane, Arm}) ->
    {ok,{Height, Plane, Arm},[]};
init (_) ->
    {ok, {up, left, open}, []}.

%% @private
callback_mode() -> handle_event_function.


%internal functions

%%%%Robot execute calls%%%
height_down() -> ok.
height_up() -> ok.
plane_left() -> ok.
plane_right() -> ok.
close_arm() -> ok.
open_arm() -> ok.


%query state
handle_event({call,From}, rq_state, Current_state, _State_data) ->
    {keep_state_and_data, [{reply, From, Current_state}]};

%%%PACKING LOOP STATES %%%
handle_event({call,From}, next, {up, left, open}, _State_data) ->
    height_down(),
    {next_state, {down,left,open}, _State_data, [{reply, From, ok}]};
handle_event({call, From}, next, {down, left, open}, _State_data) ->
    close_arm(),
    {next_state, {down,left,closed}, _State_data, [{reply, From, ok}]};
handle_event({call, From}, next, {down,left,closed}, _State_data) ->
    height_up(),
    {next_state, {up,left,closed}, _State_data, [{reply,From,ok}]};
handle_event({call, From}, next, {up,left,closed}, _State_data) ->
    plane_right(),
    {next_state, {up,right,closed}, _State_data, [{reply, From, ok}]};
handle_event({call, From}, next, {up,right,closed}, _State_data) ->
    height_down(),
    {next_state, {down, right, closed}, _State_data, [{reply, From, ok}]};
handle_event({call, From}, next, {down,right,closed}, _State_data) ->
    open_arm(),
    {next_state, {down,right,open}, _State_data, [{reply, From, ok}]};
handle_event({call, From}, next, {down,right,open}, _State_data) ->
    height_up(),
    {next_state, {up,right,open}, _State_data, [{reply, From, ok}]};
handle_event({call, From}, next, {up,right,open}, _State_data) ->
    plane_left(),
    {next_state, {up,left,open}, _State_data, [{reply,From, ok}]}.


%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
-endif.
