-module(check_server).
-author('bronzeboyvn@gmail.com').

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

-record(checkstate, {weight, checkets}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(Arg) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arg, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    Weight = proplists:get_value(ets_weight, Opts, 30),
    CheckEts = ets:new(bronzeboyvn_check_server, [private]),
    {ok, #checkstate{
      weight = Weight, checkets = CheckEts}
    }.

handle_call({lookup, Key}, _From, #checkstate{
        checkets = CheckEts} = State) ->
    io:fwrite("[LOOKUP] check ~p", [self()]),
    Reply = case ets:lookup(CheckEts, Key) of
        [] ->
            notfound;
        [Head | _Tail] ->
            {_, {Accesses, StartTime, TTL}} = Head,
	    Now = calendar:local_time(),
            CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
            NewTTL = TTL + StartTime - CurrentTime,
            if
                NewTTL < 0 ->
                    ets:delete(CheckEts, Key),
                    expired;
                true ->
                    ets:insert(CheckEts, {Key, {Accesses+1, CurrentTime, NewTTL}}),
                    ok
            end
    end,
    io:fwrite(", reply ~p", [Reply]),
    {reply, Reply, State};
handle_call({set, Key, TTL}, _From, #checkstate{
        checkets = CheckEts} = State) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    ets:insert(CheckEts, {Key, {0, StartTime, TTL}}),
    {reply, ok, State};
handle_call(sortkey, _From, #checkstate{
        weight = Weight,
        checkets = CheckEts} = State) ->
    Wgts = ets:foldl(
        fun({K, {Accesses, Start, TTL}}, Acc) ->
            [{K, Accesses*Weight + Start + TTL} | Acc]
        end, [], CheckEts),
    CmpFun = fun(A, B) ->
        {_, Time1} = A,
        {_, Time2} = B,
        if
            Time1 < Time2 -> true;
            true -> false
        end
    end,
    SortedKeys = lists:sort(CmpFun, Wgts),
    {reply, SortedKeys, State};
handle_call({shrink, CurrentTime}, _From, #checkstate{
        checkets = CheckEts} = State) ->
    io:fwrite("[SET] check ~p", [self()]),
    ExpireCond = [{{'$1',{'_','$2','$3'}},
        [{'=<',{'+','$2','$3'},{const,CurrentTime}}],
        ['$1']}],
    ExpiredKeys = ets:select(CheckEts, ExpireCond),
    lists:foreach(fun(K) ->
        ets:delete(CheckEts, K)
    end, ExpiredKeys),
    {reply, ExpiredKeys, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({delete, Key}, #checkstate{checkets = CheckEts} = State) ->
    ets:delete(CheckEts, Key),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #checkstate{checkets = CheckEts}) ->
    io:fwrite("check terminates", []),
    ets:delete(CheckEts),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
