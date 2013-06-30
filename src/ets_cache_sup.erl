-module(ets_cache_sup).
-author('bronzeboyvn@gmail').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    Weight = 30,
    Threshold = 0.85,
    MaxSize = 32 * 1024 * 1024,
    Opts = [{weight, Weight},
        {threshold, Threshold},
        {maxsize, MaxSize}],
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).

init([Opts]) ->
    Prefix = proplists:get_value(prefix, Opts, "bronzeboyvn_"),
    ValOpt = [{prefix, Prefix}],
    ValServ = {cache_server, {cache_server, start_link, [ValOpt]},
              permanent, 2000, worker, [cache_server]},
    ChkServ = {check_server, {check_server, start_link, [Opts]},
              permanent, 2000, worker, [check_server]},
    Children = [ValServ, ChkServ],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
