ets_cache
=========

ETS cache with time to live (TTL)

To use:
Compile project by make.

Start by calling one of following function:

	ets_cache_sup:start_link().
	application:start(ets_cache).
	ets_cache_sup:start_link(Options).

Functions set, get, delete:

	cache_server:set(Key, Value, TTLInSeconds) -> ok
	cache_server:get(Key) -> Value or <<>>
	cache_server:delete(Key) -> ok

By default,
Options = [{ets_maxsize, 16*1024*1024}, {ets_threshold, 0.85}, {ets_weight, 30}].

This means all {Key, Value} elements are kept in cache until table size exceeds maxsize.
When it happends, we delete exprired elements. After shrinking, if table size still exceeds maxsize*threshold, we sort element by weight (element has more accesses, it's heavier). We repeat to delete the lightest while table size still exceeds maxsize*threshold.
