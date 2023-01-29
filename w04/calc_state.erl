-module(calc_state).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SERVER, ?MODULE).

%% API
-export([start/0,start/2,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for 
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

%% Any other API functions go here.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
        {ok,[]}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), State::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.
handle_call({add, List}, _From, _State) ->
    New_State = add(List),
    {reply,
        New_State,
        New_State};

handle_call({sub, {Num1, Num2}}, _From, _State) ->
    New_State = sub(Num1, Num2),
    {reply,
        New_State,
        New_State};

handle_call({mult, List}, _From, _State) ->
    New_State = mult(List),
    {reply,
        New_State,
        New_State};

handle_call({divide, {Dividend, Divider}}, _From, _State) ->
    New_State = divide(Dividend, Divider),
    {reply,
        New_State,
        New_State};

handle_call(show_last, _From, []) ->
    {reply,
        'There has not been any activity.',
        []};

handle_call(show_last, _From, State) ->
    {reply,
        State,
        State};

handle_call(stop, _From, _State) ->
    {stop,normal,
            replace_stopped,
        down}; %% setting the server's internal state to down

handle_call(_, _From, State) ->
    {reply,
        {error, "get rekt lol."},
        State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term()}.
handle_cast(_Msg, State) ->
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
                                   {noreply, term(), integer()} |
                                   {stop, term(), term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), term()) -> term().
terminate(_Reason, _State) ->
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================

add([]) -> nil;
add(List) -> lists:foldl(fun(N,M) -> N + M end, 0, List).

sub(Num1,Num2) -> Num1 - Num2.

mult([]) -> nil;
mult(List) -> lists:foldl(fun(N,M) -> N * M end, 1, List).

divide(Dividend, Divisor) -> Dividend div Divisor.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
    {setup,
        fun() -> 2 end,
        fun() -> gen_server:stop(?SERVER), gen_server:stop(testing_name) end,
        [?_assertEqual(ok, start()), ?_assertEqual(ok,start(local,testing_name))]}.

stop_test() ->
    {setup,
        fun() -> gen_server:start_link({local, add}, ?MODULE, [], []) end,
        fun() -> ok end,
        [?_assertEqual(ok, stop())]}.

add_test_() ->
    [?_assertMatch(10, add([2,3,4,1])),
    ?_assertMatch(7, add([7])),
    ?_assertMatch(7, add(7)),
    ?_assertMatch(nil, add([]))].

sub_test_() ->
    [?_assertMatch(-7, sub(7, 14)),
    ?_assertMatch(7, sub(7,0))].

multi_test_() ->
    [?_assertMatch(24, mult([2,3,4,1])),
    ?_assertMatch(7, mult([7])),
    ?_assertMatch(7, mult(7)),
    ?_assertMatch(nil, mult([]))].

divide_test_() ->
    [?_assertMatch(42, divide(420,10)),
    ?_assertMatch(6, divide(30,5))].

handle_call_test() ->
    {setup,
        fun() -> gen_server:start_link({local, handle_test}, ?MODULE, [], []) end,
        fun() -> gen_server:stop(handle_test) end,
        [?_assertMatch(10, gen_server:call(handle_test, {add, [1,2,3,4]})),
        ?_assertMatch(3, gen_server:call(handle_test, {sub, {5,2}})),
        ?_assertMatch(24, gen_server:call(handle_test, {mult, [1,2,3,4]})),
        ?_assertMatch(5, gen_server:call(handle_test, {divide, {10,2}})),
        ?_assertMatch({ok, normal}, gen_server:call(handle_test, {stop, []}))]}.

-endif.