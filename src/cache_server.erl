-module(cache_server).
-author('bronzeboyvn@gmail.com').

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, stop/0,
    get/1, set/3, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

-record(cachestate, {threshold, maxsize, cacheets}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(Arg) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arg, []).

stop() ->
    gen_server:cast(?MODULE, stop).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

set(Key, Value, TTL) ->
    gen_server:cast(?MODULE, {set, Key, Value, TTL}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    MaxSize = proplists:get_value(maxsize, Opts, 32 * 1024 * 1024),
    Threshold = proplists:get_value(threshold, Opts, 0.85),
    ValueEts = ets:new(bronzeboyvn_cache_server, [private]),
    {ok, #cachestate{maxsize = MaxSize,
        threshold = Threshold,
        cacheets = ValueEts}}.

handle_call({get, Key}, _From, #cachestate{cacheets = ValueEts} = State) ->
    Reply = case check_server:sync_lookup(Key) of
        ok ->
            case ets:lookup(ValueEts, Key) of
                [] ->
                    <<>>;
                [H | _] ->
                    {_, Value} = H,
                    Value
            end;
	exprired ->
            ets:delete(ValueEts, Key),
            <<>>;
        _ ->
            <<>>
    end,
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({set, Key, Value, TTL}, #cachestate{
        maxsize = MaxSize, 
        threshold = Threshold,
        cacheets = ValueEts} = State) ->
    ets:insert(ValueEts, {Key, Value}),
    check_server:sync_set(Key, TTL),
    case check_memsize(ValueEts, MaxSize) of
        over ->
            shrink_table(ValueEts),
            delete_light_items(ValueEts, Threshold*MaxSize);
        _ ->
            ok
    end,
    {noreply, State};
handle_cast({delete, Key}, #cachestate{
        cacheets = ValueEts} = State) ->
    ets:delete(ValueEts, Key),
    check_server:async_delete(Key),
    {noreply, State};
handle_cast(stop, #cachestate{
        maxsize = MaxSize,
        threshold = Threshold,
        cacheets = ValueEts}) ->
    ets:delete(ValueEts),
    NewState = #cachestate{maxsize = MaxSize,
        threshold = Threshold},
    {stop, normal, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #cachestate{cacheets = ValueEts}) ->
    ets:delete(ValueEts),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_memsize(ValueEts, Size) ->
    F = fun (X) ->
        case X of 
            {memory, _} -> true;
            _ -> false
        end
    end,
    [{_, H}|_] = lists:filter(F, ets:info(ValueEts)),
    ValueSize = H * erlang:system_info(wordsize),
    if
        ValueSize > Size -> over;
        true -> ok
    end.

shrink_table(ValueEts) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    ExpiredKeys = check_sever:sync_shrink(CurrentTime),
    lists:foreach(fun(K) ->
        ets:delete(ValueEts, K)
    end, ExpiredKeys).

delete_light_items(ValueEts, ThresholdSize) ->
    case check_memsize(ValueEts, ThresholdSize) of
        over ->
            SortedKeys = check_server:sync_sort_key(),
            delete_items(ValueEts, ThresholdSize, SortedKeys);
        _ ->
            ok
    end.

delete_items(_ValueEts, _ThresholdSize, []) ->
    ok;
delete_items(ValueEts, ThresholdSize, [Head|Tail]) ->
    ets:delete(ValueEts, Head),
    check_est:async_delete(Head),
    case check_memsize(ValueEts, ThresholdSize) of
        over ->
            delete_items(ValueEts, ThresholdSize, [Tail]);
        _ ->
            ok
    end.
