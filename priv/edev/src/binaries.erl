-module(binaries).
-export([grow_shrink/2]).

-export([benchmark_arguments/0]).
-export([benchmark_unit/0]).

benchmark_arguments() ->
    [{grow_shrink, [80, 3]}].

benchmark_unit() -> "ms".

grow_shrink(N,M) ->
    B0 = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    B1 = [B0,B0,B0,B0,B0,B0,B0,B0],
    B2 = [B1,B1,B1,B1,B1,B1,B1,B1],
    B3 = [B2,B2,B2,B2,B2,B2,B2,B2],
    Bl = [B3,B3,B3,B3,B3,B3,B3,B3],
    B  = list_to_binary([Bl,Bl,Bl,Bl,Bl,Bl,Bl,Bl]),
    T0 = now(),
    grow(B, B, 0, N, M),
    T1 = now(),
    timer:now_diff(T1, T0)/1000.

grow(B0, B, I, In, N) when I > In -> 
    shrink(B0, B, 0, In, N);
grow(B0, B, I, In, N) -> 
    B1 = list_to_binary([B0,B]),
    grow(B0, B1, I + 1, In, N).

shrink(_, _, _, _, 0) -> ok;
shrink(B0, B, I, In, N) when I > In -> 
    grow(B0, B, 0, In, N - 1);
shrink(B0, B, I, In, N) ->
    {SB1, _} = erlang:split_binary(B, size(B) - size(B0)),
    B1 = list_to_binary([SB1]),
    shrink(B0, B1, I + 1, In, N).

