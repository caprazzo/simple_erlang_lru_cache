Simple Erlang LRU Term Cache
============================

This module provides a simple LRU cache for Erlang terms.

When an object is inserted in the cache its insertion time
is recorded and later used to calculate the object age.

Each time an object is successfully retrieved from the cache (a HIT),
a LastHit time is recorded for the object. This time
is then used to remove least recently used objects.

Objects older then max_age are never returned (MISS) and
are removed either during the get or during a purge.

A purge is triggered each time an insert brings the object count over
max_count. During a purge action:

- all expired entries are removed
- if object count is still above max_count objects,
- max_count/4 least recently used objects are removed from the cache.

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



  

