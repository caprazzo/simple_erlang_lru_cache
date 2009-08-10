Simple Erlang LRU Term Cache
============================

Usage:

1> Cache = simple_cache:new([{max_age, 1000},{max_count, 100}]).

When object count reaches max_count, the cache is pruned:

- objects older than max_age milliseconds are removed from the cache.

  

