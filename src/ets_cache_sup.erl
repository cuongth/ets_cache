-module(ets_cache_sup).
-author('bronzeboyvn@gmail').

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

init(Opts) ->
    MaxSize = proplists:get_value(ets_maxsize, Opts, 32 * 1024 * 1024),
    Threshold = proplists:get_value(ets_threshold, Opts, 0.85),
    Weight = proplists:get_value(ets_weight, Opts, 30),
    ValOpts = [{ets_maxsize, MaxSize}, {ets_threshold, Threshold}],
    ChkOpt = [{ets_weight, Weight}],
    ValServ = {cache_server, {cache_server, start_link, [ValOpts]},
              permanent, 2000, worker, [cache_server]},
    ChkServ = {check_server, {check_server, start_link, [ChkOpt]},
              permanent, 2000, worker, [check_server]},
    Children = [ValServ, ChkServ],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
