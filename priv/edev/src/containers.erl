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


-module(containers).
-export([gb_trees/2, array/2]).
-export([benchmark_arguments/0, benchmark_unit/0]).

benchmark_arguments() ->
    [{F, [Action, N]} || F <- [gb_trees, array], Action <- [add, lookup], N <- [10000, 1000000]].

benchmark_unit() -> "ms".

gb_trees(add, N) ->
    T0 = now(),
    add(gb_trees, N),
    T1 = now(),
    timer:now_diff(T1,T0)/1000;
gb_trees(lookup, N) ->
    T = add(gb_trees, N),
    T0 = now(),
    lookup(gb_trees, N, T),
    T1 = now(),
    timer:now_diff(T1,T0)/1000;
gb_trees(_, _) -> undefined.

array(add, N) -> 
    A = array:new([{size, N + 1}, {default, 0}, {fixed, true}]),
    T0 = now(),
    add(array, N, A),
    T1 = now(),
    timer:now_diff(T1,T0)/1000;
array(lookup, N) -> 
    A0 = array:new([{size, N + 1}, {default, 0}, {fixed, true}]),
    A1 = add(array, N, A0),
    T0 = now(),
    lookup(array, N, A1),
    T1 = now(),
    timer:now_diff(T1,T0)/1000;
array(_, _) -> undefined.





add(gb_trees, N) -> add(gb_trees, N, gb_trees:empty()).
add(_, 0, T) -> T;
add(gb_trees, N, T) -> add(gb_trees, N - 1, gb_trees:enter(N, N, T));
add(array, N, A) -> add(array, N - 1, array:set(N, N, A));
add(_, _, T) -> T.


lookup(_, 0, T) -> T;
lookup(gb_trees, N, T) -> gb_trees:get(N, T), lookup(gb_trees, N - 1, T);
lookup(array, N, A)    -> array:get(N, A), lookup(array, N - 1, A);
lookup(_, _, T) -> T.
