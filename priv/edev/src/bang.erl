%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%

%%%-------------------------------------------------------------------
%%% File    : bang.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : 
%%%
%%% Created :  3 Dec 2008 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(bang).
-export([bang/2]).

-export([benchmark_arguments/0]).

benchmark_arguments() ->
    #{ bang =>
           #{ before_scenario => fun before/1,
              run => fun bang/1
            },
       inputs => [{S,M} || S <- [1000, 10], M <- [10, 1000]]
     }.

before({S,M}) ->
    Bang   = {make_ref(),make_ref(),make_ref(),make_ref(),make_ref()},
    Rec    = spawn_link(fun() -> rec(Bang, S*M) end),
    monitor(process, Rec),
    {lists:seq(1, S), M, Rec, Bang}.

bang({S, M, Rec, Bang}) ->
    lists:foreach(fun(_) ->
                          spawn_link(fun() -> send(Rec, Bang, M) end)
                  end, S),
    receive _Down -> ok end.

send(_T, _M, 0) -> ok;
send(T, M, N)   -> T ! M, send(T, M, N-1).

rec(_M, 0) -> ok;
rec(M, N)  -> receive M -> rec(M, N-1) end.
