-module(cache_server).
-author('bronzeboyvn@gmail.com').

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
    stop/1, get/2, set/4, delete/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

-record(cachestate, {threshold, maxsize, cacheets, checkpid}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(Arg) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arg, []).

stop(NameOrPid) ->
    gen_server:cast(NameOrPid, stop).

get(NameOrPid, Key) ->
    io:fwrite("[GET] boss ~p, cache_server ~p~n", [self(), NameOrPid]),
    gen_server:call(NameOrPid, {get, Key}).

set(NameOrPid, Key, Value, TTL) ->
    io:fwrite("[SET] boss ~p, cache_server ~p~n", [self(), NameOrPid]),
    gen_server:cast(NameOrPid, {set, Key, Value, TTL}).

delete(NameOrPid, Key) ->
    gen_server:cast(NameOrPid, {delete, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    MaxSize = proplists:get_value(ets_maxsize, Opts, 8 * 1024 * 1024),
    Threshold = proplists:get_value(ets_threshold, Opts, 0.85),
    CheckPid = proplists:get_value(checkpid, Opts),
    ValueEts = ets:new(bronzeboyvn_cache_server, [private]),
    {ok, #cachestate{maxsize = MaxSize,
        threshold = Threshold,
        cacheets = ValueEts,
        checkpid = CheckPid}}.

handle_call({get, Key}, _From, #cachestate{ 
    cacheets = ValueEts,
    checkpid = CheckPid} = State) ->
    io:fwrite("[GET] cache_server ~p~n", [self()]),
    Reply = case gen_server:call(CheckPid, {lookup, Key}) of
        ok ->
            case ets:lookup(ValueEts, Key) of
                [] ->
                    <<>>;
                [H | _] ->
                    {_, Value} = H,
                    Value
            end;
	    expired ->
            io:fwrite("[GET] key ~p: EXPIRED", [Key]),
            ets:delete(ValueEts, Key),
            <<>>;
        _ ->
            io:fwrite("[GET] key ~p: EMPTY", [Key]),
            <<>>
    end,
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({set, Key, Value, TTL}, #cachestate{
        maxsize = MaxSize, 
        threshold = Threshold,
        cacheets = ValueEts,
        checkpid = CheckPid} = State) ->
    ets:insert(ValueEts, {Key, Value}),
    io:fwrite("[SET] cache ~p~n", [self()]),
    gen_server:call(CheckPid, {set, Key, TTL}),
    case check_memsize(ValueEts, MaxSize) of
        over ->
            io:fwrite("shrink_table~n", []),
            shrink_table(ValueEts, CheckPid),
            delete_light_items(ValueEts, Threshold*MaxSize, CheckPid);
        _ ->
	    io:fwrite("increase table~n", []),
            ok
    end,
    {noreply, State};
handle_cast({delete, Key}, #cachestate{
        cacheets = ValueEts,
        checkpid = CheckPid} = State) ->
    ets:delete(ValueEts, Key),
    gen_server:cast(CheckPid, {delete, Key}),
    {noreply, State};
handle_cast(stop, #cachestate{
        cacheets = _ValueEts,
        checkpid = CheckPid} = State) ->
    io:fwrite("clean check ~p", [CheckPid]),
    gen_server:cast(CheckPid, stop),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #cachestate{cacheets = ValueEts}) ->
    io:fwrite("cache terminates", []),
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
        true ->
	    io:fwrite("ValueSize ~p <= Size ~p~n", [ValueSize, Size]),
	    ok
    end.

shrink_table(ValueEts, CheckPid) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    ExpiredKeys = gen_server:call(CheckPid, {shrink, CurrentTime}),
    io:fwrite("ExpiredKeys ~p~n", [ExpiredKeys]),
    lists:foreach(fun(K) ->
        ets:delete(ValueEts, K)
    end, ExpiredKeys).

delete_light_items(ValueEts, ThresholdSize, CheckPid) ->
    case check_memsize(ValueEts, ThresholdSize) of
        over ->
            SortedKeys = gen_server:call(CheckPid, sortkey),
            io:fwrite("sorted ~p~n", [SortedKeys]),
            delete_items(CheckPid, ValueEts, ThresholdSize, SortedKeys);
        _ ->
            ok
    end.

delete_items(_CheckPid, _ValueEts, _ThresholdSize, []) ->
    ok;
delete_items(CheckPid, ValueEts, ThresholdSize, [{HeadKey, _}|Tail]) ->
    io:fwrite("cache ~p delete HEAD ~p~n", [self(), HeadKey]),
    ets:delete(ValueEts, HeadKey),
    gen_server:cast(CheckPid, {delete, HeadKey}),
    case check_memsize(ValueEts, ThresholdSize) of
        over ->
            delete_items(CheckPid, ValueEts, ThresholdSize, Tail);
        _ ->
            ok
    end.
