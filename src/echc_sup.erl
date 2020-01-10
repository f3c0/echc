-module(echc_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Name, Getter) ->
  supervisor:start_child(?SERVER, #{
    id => Name,
    start => {echc_srv, start_link, [Name, Getter]},
    restart => permanent,
    shutdown => timer:seconds(1),
    type => worker
  }).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 3,
    period => 1
  },
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
