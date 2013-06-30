{application, ets_cache,
 [{description, "ETS cache with TTL"},
  {vsn, "0.1.0"},
  {modules, [ets_cache_app,
             ets_cache_sup,
             cache_server,
             check_server]},
  {registered, [ets_cache_sup, cache_server, check_server]},
  {applications, [kernel, stdlib]},
  {mod, {ets_cache_app, []}}
]}.
