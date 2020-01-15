-module(containers).
-export([go/0]).
-export([benchmark_arguments/0, benchmark_unit/0]).

go() ->
    Res = lists:map(fun({F, Actions}) ->
                            {F, [go(F, Action, N) || {Action, N} <- Actions]}
                    end, benchmark_arguments()),
    [{_, Asm},{_, Interp},{_,Hipe}] = Res,
    io:format("%%%%   Asm vs Interp~n"),
    lists:map(fun({{_,A,N,AsmRes},{_,A,N,InterpRes}}) ->
                      io:format("~p(~p) Asm: ~p Int: ~p Ratio: ~p~n",
                                [A,N,AsmRes,InterpRes,AsmRes/InterpRes]),
                      #{action=>A,
                        iter=>N,
                        asm=>AsmRes,
                        interp=>InterpRes,
                        ratio =>AsmRes / InterpRes}
              end, lists:zip(Asm,Interp)),
    io:format("%%%%   Hipe vs Interp~n"),
    lists:map(fun({{_,A,N,AsmRes},{_,A,N,InterpRes}}) ->
                      io:format("~p(~p) Hipe: ~p Int: ~p Ratio: ~p~n",
                                [A,N,AsmRes,InterpRes,AsmRes/InterpRes]),
                      #{action=>A,
                        iter=>N,
                        asm=>AsmRes,
                        interp=>InterpRes,
                        ratio =>AsmRes / InterpRes}
              end, lists:zip(Hipe,Interp)),
    io:format("%%%%   Hipe vs Asm~n"),
    lists:map(fun({{_,A,N,AsmRes},{_,A,N,InterpRes}}) ->
                      io:format("~p(~p) Hipe: ~p Asm: ~p Ratio: ~p~n",
                                [A,N,AsmRes,InterpRes,AsmRes/InterpRes]),
                      #{action=>A,
                        iter=>N,
                        asm=>AsmRes,
                        interp=>InterpRes,
                        ratio =>AsmRes / InterpRes}
              end, lists:zip(Hipe,Asm)).

go(F, Action, N) ->
    F:module_info(),
    spawn_monitor(
      fun() ->
              erlang:exit(gb_trees(F, Action, N))
      end),
    receive
        {_,_,_,_,M} ->
            {F, Action, N, M}
    end.

benchmark_arguments() ->
    c:c(gb_trees1),
    c:c(gb_trees2),
    c:c(gb_trees3,[native]),
    [{F, [{Action,N} || Action <- [add, lookup],
                        N <- [10000, 1000000]]}
     || F <- [gb_trees1, gb_trees2, gb_trees3]].

benchmark_unit() -> "ms".

gb_trees(F, add, N) ->
    T0 = erlang:monotonic_time(),
    F:badd(N),
    T1 = erlang:monotonic_time(),
    trunc((T1 - T0) / 1000) / 1000;
gb_trees(F, lookup, N) ->
    T = F:badd(N),
    T0 = erlang:monotonic_time(),
    F:blookup(N, T),
    T1 = erlang:monotonic_time(),
    trunc((T1 - T0) / 1000) / 1000;
gb_trees(_, _, _) -> undefined.
