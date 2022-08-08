-module(test).

-export([go/0]).

go() ->
    spawn_monitor(
      fun() ->
              Tid = ets:new(a,[]),
              exit(loop(Tid, 10, []))
      end),
    receive {'DOWN', _, _, _, M} -> io:format(user,"~p~n",[M]) end,
    ok.

loop(Tid, 0, N) ->
    erlang:garbage_collect(),
    CC1 = erlang:process_info(self(), cons_count),
    P = spawn_link(fun() -> receive ok -> ok end end),
    P ! N,
    P ! lists:seq(1,100),
    erlang:garbage_collect(),
    CC2 = erlang:process_info(self(), cons_count),
    ets:insert(Tid, {1, N}),
    ets:insert(Tid, {1, lists:seq(1, 100)}),
    CC3 = erlang:process_info(self(), cons_count),
    erlang:binary_to_term(<<131,107,0,1,1>>),
    erlang:binary_to_term(<<131,108,0,0,0,1,100,0,1,97,106>>),
    CC4 = erlang:process_info(self(), cons_count),
    erlang:display(length(lists:flatten(N))),
    [CC1,CC2,CC3,CC4];
loop(Tid, N, Acc) ->
    [erlang:process_info(self(),cons_count) | loop(Tid, N-1, [lists:seq(1,100) | Acc])].
