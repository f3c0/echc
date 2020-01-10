-module(echc).

-export([start/2, get/2, set/3]).
-export([getter_rand/1]).

start(Name, Getter) ->
  {ok, Pid} = echc_sup:start_child(Name, Getter),
  {Name, Pid}.

get(Name, Key) ->
  echc_srv:get(Name, Key).

set(Name, Key, Value) ->
  echc_srv:set(Name, Key, Value).

getter_rand(Id) ->
  io:format("get: ~p ...~n", [Id]),
  timer:sleep(timer:seconds(round(rand:uniform() * 3))),
  Val = rand:uniform(),
  io:format("get: ~p -> ~p~n", [Id, Val]),
  #{id => Id, val => Val}.
