-module(simple_cache).

%% @doc LRU cache for erlang terms
%% Usage:

%% Cache = simple_cache:new([{max_age, 10000}, {max_count, 500}]).
%% Cache:put(my_key, {any, term_ here}),
%% {hit, Value} = Cache:get(my_key),
%% miss = Cache:get(other_key).

-export([new/1]).

%% @type simple_cache_config() = [{max_age, integer()}, {max_count, integer()}].
%% @spec new(simple_cache_config()) -> simple_cache_obj().

new(Config) ->
	MaxAge = proplists:get_value(max_age, Config),
	MaxCount = proplists:get_value(max_count, Config),
	Tab = ets:new(simple_cache, [private, set]),
	simple_cache_obj:new(Tab, MaxAge*1000, MaxCount).



