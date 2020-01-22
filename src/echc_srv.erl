-module(echc_srv).

-behaviour(gen_server).

%% API
-export([start_link/2, get/3, set/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  getter,
  cache = #{},
  queue = #{}
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Name :: atom(), Getter :: fun((term()) -> term())) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Getter) ->
  gen_server:start_link({local, Name}, ?MODULE, [Getter], []).

get(Server, Key, TTL) ->
  gen_server:call(Server, {get, Key, TTL}).

set(Server, Key, Value, TTL) ->
  gen_server:cast(Server, {set, Key, Value, TTL}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Getter]) ->
  {ok, #state{getter = Getter}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({get, Key, TTL}, From,
    State = #state{cache = Cache, queue = Queue, getter = Getter}) ->
  case cache_get(Key, Cache) of
    {ok, Value} -> {reply, Value, State};
    not_found ->
      case maps:get(Key, Queue, []) of
        [] ->
          Server = self(),
          spawn_link(fun() -> gen_server:cast(Server, {set, Key, Getter(Key), TTL}) end),
          {noreply, State#state{queue = Queue#{Key => [From]}}};
        Targets -> {noreply, State#state{queue = Queue#{Key => [From | Targets]}}}
      end
  end;
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

cache_get(Key, Cache) ->
  Now = erlang:system_time(millisecond),
  case maps:find(Key, Cache) of
    {ok, {infinity, Value}} -> {ok, Value};
    {ok, {TTL, Value}} when TTL >= Now -> {ok, Value};
    _ -> not_found
  end.

cache_set(Key, Value, Cache, infinity) ->
  Cache#{Key => {infinity, Value}};
cache_set(Key, Value, Cache, TTL) ->
  Cache#{Key => {erlang:system_time(millisecond) + TTL, Value}}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({set, Key, Value, TTL}, State = #state{cache = Cache, queue = Queue}) ->
  [gen_server:reply(Target, Value) || Target <- maps:get(Key, Queue, [])],
  {noreply, State#state{cache = cache_set(Key, Value, Cache, TTL), queue = Queue#{Key => []}}};
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
