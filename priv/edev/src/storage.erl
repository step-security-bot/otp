-module(storage).
-export([dict/2, ets_set/2, gb_trees/2, lists/2, hash/2, hash2/2, procd/2]).
-export([benchmark_arguments/0, benchmark_unit/0]).

-export([fill/3, inspect/3, map/3, foreach/3, fold/3]).

benchmark_arguments() ->
    [{Store, [Type, N]}|| Store <- [dict, ets_set, gb_trees, hash2, procd], Type <- [fill, inspect, map, fold, foreach], N <- [10000,100000,1000000]].

benchmark_unit() -> "ms".

dict(Type, N)     -> test(store_dict, Type, N).
ets_set(Type, N)  -> test(store_ets_set, Type, N).
gb_trees(Type, N) -> test(store_gb_trees, Type, N).
hash(Type, N)     -> test(store_hash, Type, N).
hash2(Type, N)    -> test(store_hash2, Type, N).
procd(Type, N)    -> test(store_procd, Type, N).
lists(Type, N)    -> test(store_lists, Type, N).


test(Store, fill, N) ->
    Keys = lists:seq(1, N),
    S0   = Store:new(),
    T0   = now(),
    S1   = fill(Store, Keys, S0),
    T1   = now(),
    Store:delete(S1),
    timer:now_diff(T1, T0)/1000;
test(Store, Type, N) ->
    Keys = lists:seq(1, N),
    S0   = Store:new(),
    S1   = fill(Store, Keys, S0),
    T0   = now(),
    S2   = erlang:apply(?MODULE, Type, [Store, Keys, S1]),
    T1   = now(),
    Store:delete(S2),
    timer:now_diff(T1, T0)/1000.

fill(Store, Keys, S0) ->
    lists:foldl(
	fun(Key, S) ->
	    Store:add(Key, random:uniform(2000), S)
	end, S0, Keys).

inspect(Store, Keys, S0) ->
    lists:foreach(fun
	(Key) ->
	    _V = Store:get(Key, S0)
	end, Keys),
    S0.

map(Store, _Keys, S0) ->
    Store:map(fun
	(_Key, Value) -> Value*2
    end, S0).

foreach(Store, _Keys, S0) ->
    Store:foreach(fun
	(_Key, _Value) -> ok
    end, S0).

fold(Store, _Keys, S0) ->
    _O = Store:fold(fun
	(_Key, Value, Out) -> Out + Value
    end, 0, S0),
    S0.
