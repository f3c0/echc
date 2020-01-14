-module(echc_SUITE).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([set_get/1, rand_get/1, parallel_get/1, counter_get/1]).

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(echc),
  Config.

end_per_suite(Config) -> Config.

all() ->
  [{group, g}].

groups() -> [
  {g, [parallel], [set_get, rand_get, parallel_get, counter_get]}
].

set_get(_Config) ->
  Cache = set_get,
  {ok, Pid} = echc:start(Cache, fun getter_one/1),
  one = echc:get(Cache, 1),
  one = echc:get(Cache, 2),
  one = echc:get(Cache, 3),
  echc:set(Cache, 2, two),
  one = echc:get(Cache, 1),
  two = echc:get(Cache, 2),
  one = echc:get(Cache, 3),
  exit(Pid, for_restart_test),
  ct:sleep(100),
  one = echc:get(Cache, 1),
  one = echc:get(Cache, 2),
  one = echc:get(Cache, 3),
  ok.

rand_get(_Config) ->
  Cache = rand_get,
  {ok, _} = echc:start(Cache, fun getter_rand/1),
  R1 = echc:get(Cache, a),
  R1 = echc:get(Cache, a),
  R2 = echc:get(Cache, b),
  R2 = echc:get(Cache, b),
  ok.

parallel_get(_Config) ->
  Cache = rand_pget,
  {ok, _} = echc:start(Cache, fun getter_rand/1),
  R1 = pmap(fun(Key) -> echc:get(Cache, Key div 10) end, lists:seq(1, 100)),
  R2 = pmap(fun(Key) -> echc:get(Cache, Key div 10) end, lists:seq(1, 100)),
  true = lists:sort(R1) =:= lists:sort(R2).

counter_get(_Config) ->
  Cache = counter_get,
  {ok, _} = echc:start(Cache, getter_counter_fun()),
  R = pmap(fun(Key) -> echc:get(Cache, Key) end, [a,b,c,a,b,a,c,a,b,d,c,a]),
  [1,2,3,4] = lists:usort(R).

getter_one(_Key) -> one.

getter_rand(Id) ->
  timer:sleep(round(100 * rand:uniform() * 3)),
  #{id => Id, val => rand:uniform()}.

getter_counter_fun() ->
  P = spawn(fun() -> get_counter_loop(1) end),
  fun(_Key) ->
    P ! self(),
    receive C -> C end
  end.

get_counter_loop(Current) ->
  receive
    From ->
      From ! Current,
      get_counter_loop(Current + 1)
  end.

pmap(F, L) ->
  S = self(),
  [spawn(fun() -> S ! F(E) end) || E <- L],
  [receive R -> R end || _E <- L].
