-module(echc).

-export([start/2, get/2, set/3, get/3, set/4]).

start(Name, Getter) ->
  echc_sup:start_child(Name, Getter).

get(Name, Key) ->
  get(Name, Key, infinity).

get(Name, Key, TTL) ->
  echc_srv:get(Name, Key, TTL).

set(Name, Key, Value) ->
  set(Name, Key, Value, infinity).

set(Name, Key, Value, TTL) ->
  echc_srv:set(Name, Key, Value, TTL).
