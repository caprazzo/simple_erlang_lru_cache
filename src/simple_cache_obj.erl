-module(simple_cache_obj, [Tab, MaxAge, MaxCount]).

-export([put/2, get/1, dump/0]).

%% @spec put(term(), term()) -> true
put(Key, Value) ->
	InsertTime = LastHit = erlang:now(),
	ets:insert(Tab, {Key, Value, InsertTime, LastHit}),
	case ets:info(Tab, size) > MaxCount of
		true ->
			purge(Tab, MaxAge, MaxCount);
		false ->
			ok
	end,
	ok.

%% @spec get(term()) -> {hit, term()}|miss
get(Key) ->
	case ets:lookup(Tab, Key) of
		[] -> miss;
		[{Key, Value, InsertTime, _Lasthit}] ->
			case timer:now_diff(erlang:now(), InsertTime) > MaxAge of
				true ->
					ets:delete(Tab, Key),
					miss;
				false ->
					ets:update_element(Tab, Key, {4,erlang:now()}),
					{hit, Value}
			end
	end.

dump() ->
	ets:tab2list(Tab).

purge(Tab, MaxAge, MaxCount) ->
	case ets:info(Tab, size) > MaxCount of
		true -> drop_expired_items(Tab, MaxAge);
		false -> ok
	end,			  
	Size = ets:info(Tab, size), 
	case Size > MaxCount of
		true ->
			CutCount = Size - MaxCount div 4 * 3,
			drop_least_used_items(Tab, CutCount);	
		false ->
			ok
	end.

drop_expired_items(Tab, MaxAge) ->
	Now = erlang:now(),
	Fun = fun({Key, _, InsertTime, _}, _) ->
		case timer:now_diff(Now, InsertTime) > MaxAge of
			true -> ets:delete(Tab, Key);
			false -> nil
		end
	end,
	ets:foldl(Fun, nil, Tab).

drop_least_used_items(Tab, CutCount) ->
	%% order the entries by LasyHit time (smaller time means used longer ago)	
	UsageTree = ets:foldl(fun ({Key, _, _, LastHit}, UsageTree) ->
		gb_trees:insert(LastHit, Key, UsageTree)
	end, gb_trees:empty(), Tab),
	%% remove CutCount least used elements from cache table
	chop(gb_trees:iterator(UsageTree), CutCount, Tab).
	
chop(_Iter, 0, _Tab) ->
	ok;
chop(Iter, N, Tab) ->
	{_Key, Val, Iter1} = gb_trees:next(Iter),
	ets:delete(Tab, Val),
	chop(Iter1, N-1, Tab).





