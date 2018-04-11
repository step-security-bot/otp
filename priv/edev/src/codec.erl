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

-module(codec).

-export([transform/0, compare/0]).
-export([transform/1, compare/1]).
-export([transform/2, compare/2]).
-export([atoms/1, numbers/1, strings/1, terms/1]).
-export([benchmark_arguments/0, benchmark_unit/0]).


benchmark_arguments() ->
    [{transform, [N]} || N <- [1000, 2000]] ++
    [{compare  , [N]} || N <- [1000, 5000]].

benchmark_unit() -> "ms".


transform(N) -> go(transform, N).
compare(N)   -> go(compare, N).

transform() -> go(transform, 2000).
compare()   -> go(compare, 50000).

go(Type, N) ->
    Term = [atoms(), numbers(), list_to_tuple(strings()), strings(), list_to_tuple(atoms())],
    
    T0 = now(),
    erlang:apply(?MODULE, Type, [Term, N]),
    T1 = now(),
    timer:now_diff(T1, T0)/1000.

%% compare

compare(Term, N) -> compare(Term, decode(encode(Term)), N).
compare(_, _, 0) -> ok;
compare(T1, T2, N) ->
    true = T1 == T2,
    compare(T1, T2, N - 1).

%% transform

transform(_, 0) -> ok;
transform(Term, N) ->
    transform(decode(encode(Term)), N - 1).

encode(Term) -> term_to_binary(Term).
decode(Bin)  -> binary_to_term(Bin).
    
%% aux

terms(N) -> terms(N, random:uniform(3), []).
terms(0, _, Out) -> Out;
terms(N, 1, Out) -> terms(N - 1,random:uniform(3), [{strings(3)}|Out]);
terms(N, 2, Out) -> terms(N - 1,random:uniform(3), [numbers(1)|Out]);
terms(N, 3, Out) -> terms(N - 1,random:uniform(3), [strings(1)|Out]).



numbers() -> numbers(2000).
numbers(N) -> numbers(N, 100).
numbers(N,L) -> [ [ random:uniform(20000) || _ <- lists:seq(1,L)] || _ <- lists:seq(1,N)].

atoms() -> atoms(2000).
atoms(N) -> atoms(N, 100).
atoms(N,L) -> atoms_do(strings(N,L),[]).
atoms_do([], Out) -> Out;
atoms_do([String|Strings], Out) -> atoms_do(Strings, [list_to_atom(String)|Out]).

strings() -> strings(2000).
strings(N) -> strings(N, 100).
strings(N, L) -> strings(N, L, []).
strings(0, _, Out) -> Out;
strings(N, L, Out) -> strings(N - 1, L, [string(L)|Out]).
string(L) -> [random:uniform(25) + 96 || _ <- lists:seq(1,L)].
