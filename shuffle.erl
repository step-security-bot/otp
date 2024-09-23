-module(shuffle).
-export([run/0, run/1]).

run() ->
    run(erlang:system_info(schedulers_online)).

run(N) ->
    [spawn(fun shuffle/0) || lists:seq(1, N)].

shuffle() ->
    shuffle(0, 1, 2, 3).

shuffle(A, B, C, D) ->
    shuffle(D, A, B + C, C - B).
