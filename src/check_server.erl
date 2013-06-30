-module(check_server).
-author('bronzeboyvn@gmail.com').

-behaviour(gen_server).

%% API
-export([
         start_link/0, start_link/1,
	 stop/0
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

stop() ->
    gen_server:cast(?SERVER, stop).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    io:fwrite("check_server init!~n", []),
    {ok, ok, 0};
init([Opts]) ->
    io:fwrite("check_server ~p\n", [Opts]),
        {ok, ok, 0}.

handle_call(_Msg, _From, State) ->
    {reply, {ok, ok}, State}.

handle_cast(stop, State) ->
    io:fwrite("check_server stop!~n", []),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, {ok, ok}, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:fwrite("check_server terminate!~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


