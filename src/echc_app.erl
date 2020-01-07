%%%-------------------------------------------------------------------
%% @doc echc public API
%% @end
%%%-------------------------------------------------------------------

-module(echc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    echc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
