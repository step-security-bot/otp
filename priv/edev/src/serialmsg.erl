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

%% Author  : Björn-Egil Dahlberg
%% Created : 17 Dec 2009 by Björn-Egil Dahlberg

-module(serialmsg).
-export([push/3, push/2, push/1, push/0]).
-export([go/0, go/2, go/3]).
-export([benchmark_arguments/0, benchmark_unit/0]).


benchmark_arguments() ->
    [{push,[P, N, L]} || P <- [10,100], N <- [10,100,1000], L <- [1000,2000]].
 
benchmark_unit() -> "ms".

push() -> push(10).
push(P) -> push(P, 10).
push(P, N) -> push(P, N, 1000).

push(P, N, L) ->
    Recvs = setup_receivers(P),
    Disp  = setup_dispatcher(),
    Gens  = setup_generators(Recvs, Disp, N, L),
    T1    = now(),
    [Pid ! {self(), do} || Pid <- Gens],
    [receive {Pid, done} -> ok end || Pid <- Recvs],
    T2    = now(),
    Disp ! {self(), done},
    timer:now_diff(T2, T1)/1000.

%% setups

setup_receivers(P) -> setup_receivers(P, self(), []).
setup_receivers(0, _, Out) -> Out;
setup_receivers(P, Pid, Out) -> 
    setup_receivers(P - 1, Pid, [spawn_link(fun() -> receiver(Pid) end)|Out]).

setup_dispatcher() ->
    Me = self(),
    spawn_link(fun() -> dispatcher(Me) end).

setup_generators(Recvs, Disp, N, L) -> setup_generators(Recvs, Disp, self(), N, L, []).
setup_generators([],_,  _, _, _, Out) -> Out;
setup_generators([Recv|Recvs], Disp, Pid, N, L, Out) ->
    setup_generators(Recvs, Disp, Pid, N, L, [spawn_link(fun() -> generator(Recv, Disp, Pid, N, L) end) | Out]).

%% processes

receiver(Master) ->
    receive
    	{_, done} ->
	    Master ! {self(), done};
    	{_, _} ->
            receiver(Master)
    end.

dispatcher(Master) ->
    receive
  	{Master, done} -> 
	    ok;
    	{Pid, To, Data} -> 
	    To ! {Pid, Data},
	    dispatcher(Master)
    end.

generator(Recv, Disp, Master, N, L) ->
    Data = lists:seq(1, L),
    receive
   	{Master, do} -> 
	    generator_push_loop(Recv, Disp, N, Data);
   	{Master, do, NewN} -> 
	    generator_push_loop(Recv, Disp, NewN, Data)
    end.

generator_push_loop(Recv, Disp, 0, _) ->
    Disp ! {self(), Recv, done};
generator_push_loop(Recv, Disp, N, Data) ->
    Disp ! {self(), Recv, Data},
    generator_push_loop(Recv, Disp, N - 1, Data).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
go() -> go(3000, 3000).
go(P, N) -> go(P, N, 100).
go(P, N, L) -> go(100, P, 100, N, L).

go(Pi, P, Ni, N, L) when Pi =< P ->
    Ms = push(Pi, Ni, L),
    io:format("~8w\t~8w\t~.4f~n", [Pi, Ni, Ms]),
    %io:format(" P:~7w, N:~7w, L:~7w -> ~.4f ms~n", [Pi, Ni, L, Ms]),
    go(Pi + 100, P, Ni, N, L);

go(_, P, Ni, N, L) when Ni =< N -> go(100, P, Ni + 100, N, L);
go(_, _, _, _, _) -> ok.

