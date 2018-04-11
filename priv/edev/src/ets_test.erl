%%
%% %CopyrightBegin%
%%                                                                                                                                   
%% Copyright Ericsson AB 2009. All Rights Reserved.                                                                             
%% 
%% The contents of this file are subject to the Erlang Public License,                                                               
%% Version 1.1, (the "License"); you may not use this file except in                                                                 
%% compliance with the License. You should have received a copy of the                                                               
%% Erlang Public License along with this software. If not, it can be                                                                 
%% retrieved online at http://www.erlang.org/.                                                                                       
%% 
%% Software distributed under the License is distributed on an "AS IS"                                                               
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See                                                               
%% the License for the specific language governing rights and limitations                                                            
%% under the License.                                                                                                                
%% 
%% %CopyrightEnd%

%%%-------------------------------------------------------------------
%%% File    : ets_test.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : 
%%%
%%% Created : 27 Mar 2008 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(ets_test).

-define(TSIZE, 1000000).

-export([run/1, multi1/3, multi2/4]).
-export([benchmark_arguments/0]).

benchmark_arguments() ->
    [{multi1, [N,W,R]} || N <- [10000], W <- [0,1,10], R <- [100, 500]].



run(N) ->
    Parent = self(),
    Ps = lists:map(fun (_) -> spawn_link(fun () -> proc(Parent) end) end,
		   lists:seq(1, N)),
    Start = now(),
    lists:foreach(fun (P) -> P ! {go, Parent} end, Ps),
    lists:foreach(fun (P) -> receive {done, P} -> ok end end, Ps),
    Stop = now(),
    lists:foreach(fun (P) -> unlink(P), exit(P, bye) end, Ps),
    timer:now_diff(Stop, Start)/1000.

proc(P) ->
    T = ets:new(x, []),
    receive {go, P} -> ok end,
    lists:foreach(fun (X) -> i(T, 0, X) end, lists:seq(1,20)),
    P ! {done, self()}.
    

i(T, N, M) when N < ?TSIZE ->
    ets:insert(T, {N, M}),
    i(T, N+1, M+1);
i(_, _, _) ->
    ok.

r(_T, 0) ->
    ok;
r(T, N) ->
    [{N, _}] = ets:lookup(T, N),
    r(T, N-1).
w(_T, 0, _V) ->
    ok;
w(T, N, V) ->
    true = ets:insert(T, {N, V}),
    w(T, N-1, V).


multi1(N,W,R) ->
    Parent = self(),
    T=ets:new(x, [public]),
    w(T, N, init),
    Ws = lists:map(fun (_) ->
			   spawn_link(fun () ->
					      receive go -> ok end,
					      w(T, N, self()),
					      w(T, N, self()),
					      Parent ! {done, self()},
					      receive after infinity -> ok end
				      end)
		   end,
		   lists:seq(1, W)),
    Rs = lists:map(fun (_) ->
			   spawn_link(fun () ->
					      receive go -> ok end,
					      r(T, N),
					      r(T, N),
					      Parent ! {done, self()},
					      receive after infinity -> ok end
				      end)
		   end,
		   lists:seq(1, R)),
%    io:format("Writers=~p~nReaders=~p~n", [Ws,Rs]),
    Start = now(),
    lists:foreach(fun (P) -> P ! go end, Ws),
    lists:foreach(fun (P) -> P ! go end, Rs),
    lists:foreach(fun (P) -> receive {done, P} -> ok end end, Ws),
    lists:foreach(fun (P) -> receive {done, P} -> ok end end, Rs),
    Stop = now(),
    lists:foreach(fun (P) -> unlink(P), exit(P, bye) end, Ws),
    lists:foreach(fun (P) -> unlink(P), exit(P, bye) end, Rs),
    timer:now_diff(Stop, Start)/1000.

rloop(_T, I) when I < 0 ->
    ok;
rloop(T, I) ->
    [{I, _}] = ets:lookup(T, I),
    rloop(T, I-1).

repeat(_F, I) when I < 0 ->
    ok;
repeat(F, I) ->
    F(I),
    repeat(F, I-1).

count(0) ->
    ok;
count(N) ->
    count(N-1).

rw(_T, _N, _R, I) when I < 0 ->
    ok;
rw(T, N, R, I) ->
    repeat(fun (_) ->
		   case I rem 10 of
		       false -> ets:match(T, '$');
		       _ -> true = ets:insert(T, {I, get_data()})
		   end,
		   count(10),
		   ok = rloop(T, N)
	   end,
	   R),
    rw(T, N, R, I-1).

mk_data() ->
    put(?MODULE, {make_ref(), an_atom,"here are some  data", <<"some more data">>}).
%    put(?MODULE, self()).

get_data() ->
    get(?MODULE).

multi2(E, N, R, P) when is_integer(E), is_integer(N), is_integer(R), is_integer(P) ->
    mk_data(),
    Parent = self(),
    EPs = lists:map(fun (_) ->
			    T=ets:new(x, [public]),
			    repeat(fun (I) ->
					   true = ets:insert(T, {I, get_data()})
				   end,
				   N),
			    {T, lists:map(fun (_) ->
						  spawn_link(fun () ->
								     mk_data(),
								     receive go -> ok end,
								     rw(T, N, R, N),
								     Parent ! {done, self()},
								     receive after infinity -> ok end
							     end)
					  end,
					  lists:seq(1, P))}
		    end,
		    lists:seq(1, E)),
%    erlang:display(self()),
%    lists:foreach(fun ({_T, Ps}) ->
%			  lists:foreach(fun (Proc) -> erlang:display(Proc) end, Ps)
%		  end,
%		  EPs),
    Start = now(),
    lists:foreach(fun ({_T, Ps}) ->
			  lists:foreach(fun (Proc) -> Proc ! go end, Ps)
		  end,
		  EPs),
    lists:foreach(fun ({_T, Ps}) ->
			  lists:foreach(fun (Proc) ->
						receive {done, Proc} -> ok end end, Ps)
		  end,
		  EPs),
    Stop = now(),
    lists:foreach(fun ({T, _Ps}) ->
			  ets:delete(T)
		  end,
		  EPs),
    lists:foreach(fun ({_T, Ps}) ->
			  lists:foreach(fun (Proc) -> unlink(Proc), exit(Proc, bye) end, Ps)
		  end,
		  EPs),
    timer:now_diff(Stop, Start)/1000000.
    
    
