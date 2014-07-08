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

By default

	Options = [{ets_maxsize, 8*1024*1024}, {ets_threshold, 0.85}, {ets_weight, 30}].

This means all {Key, Value} elements are kept in cache until table size exceeds maxsize.
When it happends, we delete exprired elements. After shrinking, if table size still exceeds maxsize*threshold, we sort element by weight (element has more accesses, it's heavier). We repeat to delete the lightest while table size still exceeds maxsize*threshold.

The MIT License
===============

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
