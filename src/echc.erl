-module(echc).

-export([start/2, get/2, set/3]).

start(Name, Getter) ->
  echc_sup:start_child(Name, Getter).

get(Name, Key) ->
  echc_srv:get(Name, Key).

set(Name, Key, Value) ->
  echc_srv:set(Name, Key, Value).
