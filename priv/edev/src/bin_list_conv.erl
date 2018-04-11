%%%-------------------------------------------------------------------
%%% @author Rickard Green <rickard@erlang.org>
%%% @copyright (C) 2014, Rickard Green
%%% @doc
%%%
%%% @end
%%% Created :  5 May 2014 by Rickard Green <rickard@erlang.org>
%%%-------------------------------------------------------------------
-module(bin_list_conv).

-export([list_to_binary/2, binary_to_list/2]).

-export([benchmark_arguments/0, benchmark_unit/0]).

benchmark_arguments() ->
    [{list_to_binary, [100000000,1]},
     {list_to_binary, [10000000,10]},
     {list_to_binary, [1000000,100]},
     {list_to_binary, [100000,1000]},
     {list_to_binary, [100,1000000]},
     {list_to_binary, [10,10000000]},

     {binary_to_list, [100000000,1]},
     {binary_to_list, [10000000,10]},
     {binary_to_list, [1000000,100]},
     {binary_to_list, [100000,1000]},
     {binary_to_list, [100,1000000]},
     {binary_to_list, [10,10000000]}].
     
benchmark_unit() -> "ns".

-define(LARGE_BIN, (512*1024+10)).
-define(LARGE_BIN_LIM, (1024*1024)).


mk_list(0, Acc) ->
    Acc;
mk_list(Sz, Acc) ->
    mk_list(Sz-1, [$A+(Sz band 63) | Acc]).

mk_list(Sz) when Sz >= ?LARGE_BIN_LIM ->
    SzLeft = Sz - ?LARGE_BIN,
    SzHd = SzLeft div 2,
    SzTl = SzLeft - SzHd,
    [mk_list(SzHd, []), erlang:list_to_binary(mk_list(?LARGE_BIN, [])), mk_list(SzTl, [])];
mk_list(Sz) ->
    mk_list(Sz, []).

list2iolist(List) ->
    list2iolist(List, []).

list2iolist([], Acc) ->
    Acc;
list2iolist([X0, X1, X2, X3, X4, X5 | Xs], Acc) when is_integer(X0), 0 =< X0, X0 < 256,
						     is_integer(X1), 0 =< X1, X1 < 256,
						     is_integer(X2), 0 =< X2, X2 < 256,
						     is_integer(X3), 0 =< X3, X3 < 256,
						     is_integer(X4), 0 =< X4, X4 < 256,
						     is_integer(X5), 0 =< X5, X5 < 256 ->
    NewAcc = case (X0+X1+X2+X3+X4+X5) band 3 of
		 0 ->
		     [Acc, [[[[[[[[[[[[X0,[],<<"">>,X1]]]]]]]]],[X2,X3]],[],[],[],[],X4],X5]];
		 1 ->
		     [Acc, [], erlang:list_to_binary([X0, X1, X2, X3, X4, X5])];
		 2 ->
		     [Acc, [[[[X0|erlang:list_to_binary([X1])],[X2|erlang:list_to_binary([X3])],[X4|erlang:list_to_binary([X5])]]]|<<"">>]];
		 3 ->
		     [Acc, X0, X1, X2, <<"">>, [], X3, X4 | erlang:list_to_binary([X5])]
	     end,
    list2iolist(Xs, NewAcc);
list2iolist([X | Xs], Acc) ->
    list2iolist(Xs, [Acc,X]).

list_to_binary(Loops, ListSize) ->
    Top = self(),
    List = mk_list(ListSize),
    IoList = list2iolist(List),

    {Test, TestMon} = spawn_monitor(fun () ->
					    S = now(),
					    l2b_loop(Loops, IoList),
					    E = now(),
					    Top ! {self(), S, E}
				    end),
    Res = receive
	      {'DOWN', TestMon, process, Test, Reason} ->
		  exit(Reason);
	      {Test, Start, End} ->
		  timer:now_diff(End, Start)/(Loops/1000)
	  end,
    receive
	{'DOWN', TestMon, process, Test, _} ->
	    ok
    end,
    Res.

l2b_loop(0, _IoList)  ->
    ok;
l2b_loop(N, IoList) ->
    _ = erlang:list_to_binary(IoList),
    l2b_loop(N-1, IoList).

binary_to_list(Loops, BinSz) ->
    Top = self(),
    Bin = erlang:list_to_binary(mk_list(BinSz)),

    {Test, TestMon} = spawn_monitor(fun () ->
					    S = now(),
					    b2l_loop(Loops, Bin),
					    E = now(),
					    Top ! {self(), S, E}
				    end),
    Res = receive
	      {'DOWN', TestMon, process, Test, Reason} ->
		  exit(Reason);
	      {Test, Start, End} ->
		  timer:now_diff(End, Start)/(Loops/1000)
	  end,
    receive
	{'DOWN', TestMon, process, Test, _} ->
	    ok
    end,
    Res.

b2l_loop(0, _Bin)  ->
    ok;
b2l_loop(N, Bin) ->
    _ = binary_to_list(Bin),
    b2l_loop(N-1, Bin).
