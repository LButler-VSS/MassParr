%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright © 2022, Lee S. Barney
%%% @reference Licensed under the 
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This is a round robin balancer. Given a set of module-id pairs, this balancer
%%% will distribute work in a  
%%% <a href="https://www.techtarget.com/whatis/definition/round-robin">
%%% round-robin</a> fashion.
%%%
%%% To use this round robin balancer, the balanced worker item must have a
%%% locally or globally registered name. The registered name is used 
%%% to add the item to a balancer.
%%%
%%%
%%%
%%% Be aware that a worker item can, via its ID, be added to more than 
%%% one rr_balancer. This is by design, not by accident. 
%%% @end

%%% Created : 24 June 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(app_cont).
-behaviour(gen_statem).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/1,start_link/1,stop/1,call/0]).

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
-spec start(term()) -> {ok, atom()}.
start(Initial_state) ->
    gen_statem:start({local,?MODULE}, ?MODULE, Initial_state, []).

%%--------------------------------------------------------------------
%% @doc
%% 
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(term()) -> {ok, atom()}.
start_link(Initial_state) ->
    gen_statem:start_link({local,?MODULE},?MODULE,Initial_state,Initial_state).


%%--------------------------------------------------------------------
%% @doc
%% This function gracefully shuts down the balancer.
%%
%% The parameter of stop is an atom that
%% is a registered name of a round robin balancer.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom()) -> ok.
stop(Statem_name) ->
    gen_statem:stop(Statem_name).

make_dict() ->
    CommList = [{add,fun(X) -> X + 1 end},{minus, fun(X) -> X - 1 end},{multiply, fun(X) -> X * 2 end},{rekt, fun() -> 'lol get rekt' end}],
    dict:from_list(CommList).

%% Mandatory callback functions
%% @private
terminate(_Reason, _State, _Data) ->
    void.
%% @private
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
%% @private
init(Worker_ids) ->
    %% Set the initial state to be the list of available Worker_ids
    %% and types.
    {ok,ready,make_dict()}.
%% @private
callback_mode() -> handle_event_function.

%%% state callback(s)
call() ->
    gen_statem:call(distributor,next).

%%
%% Used to select which registered worker is to be used next in 
%% a round robin fashion.
%% @private
handle_event({call,From}, {command, Command, Args}, ready, Dict) ->
    %Modify the state data and replace State_data below with the modified state data.
    {Ok_Error, Value} = dict:find(Command, Dict),
    case Ok_Error of
         ok -> Result = apply(Value, Args),
         {next_state, ready,Dict,[{reply,From,{ok, Result}}]};
         error -> 
            {next_state, ready,Dict,[{reply,From,{error, nil}}]}
    end.

%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
start_test() ->
    {setup,
        fun() -> 2 end,
        fun() -> gen_statem:stop(?MODULE) end,
        [?_assertEqual(ok, start([]))]}.

stop_test() ->
    {setup,
        fun() -> gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []) end,
        fun() -> ok end,
        [fun() -> io:fwrite("Heck\n") end,
         ?_assertEqual(hi, stop(?MODULE))]}.

handle_events_test() ->
    {setup,
        fun() -> gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []) end,
        fun() -> gen_statem:stop(?MODULE) end,
        [?_assertMatch({ok, 5}, gen_statem:call(?MODULE, {command, add, [4]})),
         ?_assertMatch({ok, 3}, gen_statem:call(?MODULE, {command, minus, [4]})),
         ?_assertMatch({ok, 8}, gen_statem:call(?MODULE, {command, multiply, [4]})),
         ?_assertMatch({ok, 'lol get rekt'}, gen_statem:call(?MODULE, {command, rekt, []})),
         ?_assertMatch({error, nil}, gen_statem:call(?MODULE, {command, divide, [4]})),
         ?_assertMatch({ok, 5}, gen_statem:call(?MODULE, {command, add, []}))]
    }.


-endif.