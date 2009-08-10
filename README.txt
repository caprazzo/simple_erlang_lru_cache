Simple Erlang LRU Term Cache
============================

This module prvides a simple LRU cache for erlang terms.

- 

sample session:

1> Cache = simple_cache:new([{max_age, 1000},{max_count, 400}]).
{simple_cache_obj,380941,1000000,400}
2> Cache:put(key, "value").
ok
3> Cache:get(key).
{hit, "value").
4> Cache:get(not_key).
miss
5> timer:sleep(1000).
ok
6> Cache:get(key).
miss



  

