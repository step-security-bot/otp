%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    type_conversion.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-03-07
%%

-module(type_conversion).

-export([
	list_to_uint32/1, list_to_uint32/2,
	binary_to_uint32/1, binary_to_uint32/2,
	benchmark_arguments/0,
	benchmark_unit/0
    ]).

-define(iterations, (200)).

benchmark_unit() ->
    "ms".
 
benchmark_arguments() ->
    [{F, [N]} || F <- [list_to_uint32, binary_to_uint32], N <- [65535, 524287]].

list_to_uint32(N) -> list_to_uint32(N, ?iterations).
list_to_uint32(N, I) ->
    Vs = lists:seq(1, N),
    Ls = make_lists(Vs),
    T0 = now(),
    test_lists(Ls, I),
    T1 = now(),
    (timer:now_diff(T1, T0) div I) / 1000.


binary_to_uint32(N) -> binary_to_uint32(N, ?iterations).
binary_to_uint32(N, I) ->
    Vs = lists:seq(1, N),
    Ls = make_binaries(Vs),
    T0 = now(),
    test_binaries(Ls, I),
    T1 = now(),
    (timer:now_diff(T1, T0) div I) / 1000.


make_binaries([]) -> [];
make_binaries([V|Vs]) ->
    [{V, <<V:32>>}|make_binaries(Vs)].

make_lists([]) -> [];
make_lists([V|Vs]) ->
    <<V0,V1,V2,V3>> = <<V:32>>,
    [{V, [V0,V1,V2,V3]}|make_lists(Vs)].

test_binaries(Bs, I) ->
    test_binaries(Bs, Bs, I).
test_binaries(Bs, [{V, Vb}|Values], I) ->
    V = uint32b(Vb),
    test_binaries(Bs, Values, I);
test_binaries(Bs, [], I) when I > 0 ->
    test_binaries(Bs, Bs, I - 1);
test_binaries(_, [], 0) ->
    ok.

test_lists(Ls, I) ->
    test_lists(Ls, Ls, I).
test_lists(Ls, [{V, Vl}|Values], I) ->
    V = uint32l(Vl),
    test_lists(Ls, Values, I);
test_lists(Ls, [], I) when I > 0 ->
    test_lists(Ls, Ls, I - 1);
test_lists(_, [], 0) ->
    ok.


uint32l([V0,V1,V2,V3]) ->
    (V0 bsl 24) bor (V1 bsl 16) bor (V2 bsl 8) bor V3.


uint32b(<<_:8, V:24>>) -> V.



