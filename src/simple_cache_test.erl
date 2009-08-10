-module(simple_cache_test).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

simple_hit_miss_test() ->
	C = simple_cache:new([{max_age, 1000000}, {max_count,5}]),
	ok = C:put(a,"A"),
	{hit,"A"} = C:get(a),
	miss = C:get(b),
	ok = C:put(a,"AA"),
	{hit, "AA"} = C:get(a).
	
max_count_behaviour_test() ->
	C = simple_cache:new([{max_age, 1000000}, {max_count,5}]),
	[] = C:dump(),
	C:put(a,"A"),
	[{a,"A",_,_}] = C:dump(),
	C:put(b,"B"),
	[{b,"B",_,_},{a,"A",_,_}] = C:dump(),
	C:put(c,"C"),
	[{c,"C",_,_},{b,"B",_,_},{a,"A",_,_}] = C:dump(),
	C:put(d,"D"),
	[{d,"D",_,_},{c,"C",_,_},{b,"B",_,_},{a,"A",_,_}] = C:dump(),
	C:put(e,"E"),
	[{e,"E",_,_},{d,"D",_,_},{c,"C",_,_},{b,"B",_,_},{a,"A",_,_}] = C:dump(),
	C:put(f,"F"),
	%% cut to 3/4
	[{f,"F",_,_},{e,"E",_,_},{d,"D",_,_}] = C:dump(),
	C:put(g,"G"),
	[{g,"G",_,_},{f,"F",_,_},{e,"E",_,_},{d,"D",_,_}] =  C:dump(),
	C:put(h,"H"),
	[{h,"H",_,_},{g,"G",_,_},{f,"F",_,_},{e,"E",_,_},{d,"D",_,_}] =  C:dump().

expire_behaviour_test() ->
	C = simple_cache:new([{max_age, 100}, {max_count,5}]),
	C:put(a,"A"),
	C:put(b,"B"),
	timer:sleep(120),
	[{b,"B",_,_},{a,"A",_,_}] = C:dump(),
	miss = C:get(a),
	[{b,"B",_,_}] = C:dump(),
	miss = C:get(b),
	[] = C:dump().

lru_behviour_test() ->
	C = simple_cache:new([{max_age, 1000000}, {max_count,5}]),
	[] = C:dump(),
	C:put(a,"A"),
	[{a,"A",_,_}] = C:dump(),
	C:put(b,"B"),
	[{b,"B",_,_},{a,"A",_,_}] = C:dump(),
	C:put(c,"C"),
	[{c,"C",_,_},{b,"B",_,_},{a,"A",_,_}] = C:dump(),
	C:put(d,"D"),
	[{d,"D",_,_},{c,"C",_,_},{b,"B",_,_},{a,"A",_,_}] = C:dump(),
	C:put(e,"E"),
	[{e,"E",_,_},{d,"D",_,_},{c,"C",_,_},{b,"B",_,_},{a,"A",_,_}] = C:dump(),
	%% hit a and b then overflow (b and c shuld die as least used)
	C:get(a),
	C:get(b),
	C:put(f,"F"),
	%% cut to 3/4
	[{f,"F",_,_},{b,"B",_,_},{a,"A",_,_}] = C:dump(),
	C:put(g,"G"),
	[{g,"G",_,_},{f,"F",_,_},{b,"B",_,_},{a,"A",_,_}] =  C:dump(),
	C:put(h,"H"),
	[{h,"H",_,_},{g,"G",_,_},{f,"F",_,_},{b,"B",_,_},{a,"A",_,_}] =  C:dump().

expire_and_lru_behaviour_test() ->
	C = simple_cache:new([{max_age, 100}, {max_count,5}]),
	C:put(a,"A"), 
	C:put(b,"B"),
	C:put(c,"C"),
	C:put(d,"D"),
	timer:sleep(50),
	C:get(a),
	C:get(b),
	C:put(e,"E"),
	C:put(f,"F"),
	[{f,"F",_,_},{e,"E",_,_},{b,"B",_,_}] = C:dump(),
	timer:sleep(75),
	miss = C:get(a),
	miss = C:get(b),
	{hit,"F"} = C:get(f),
	{hit,"E"} = C:get(e),
	timer:sleep(50),
	miss = C:get(f),
	miss = C:get(e).
	
	

