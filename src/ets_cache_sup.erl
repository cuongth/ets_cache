-module(ets_cache_sup).
-author('bronzeboyvn@gmail').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ValServ = {cache_server, {cache_server, start_link, []},
              permanent, 2000, worker, [cache_server]},
    ChkServ = {check_server, {check_server, start_link, []},
              permanent, 2000, worker, [check_server]},
    Children = [ValServ, ChkServ],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
