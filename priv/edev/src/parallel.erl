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
%% Created : 16 April 2010

-module(parallel).
-export([benchmark_arguments/0, benchmark_unit/0]).
-export([now_time/2]).

benchmark_arguments() ->
    [{now_time, [N, M]} || N <- [5000,15000,30000], M <- [64, 128, 256]].

benchmark_unit() -> "ms".

now_time(N, M) ->
    Me   = self(),
    Base = [ ok || _ <- lists:seq(1,M) ],
    T0   = now(),
    Pids = [spawn_link(fun() -> loop(Me, N, []) end) || _ <- lists:seq(1, M)],
    Res  = [receive {Pid, What} -> What end || Pid <- Pids],
    T1   = now(),
    Base = Res,
    timer:now_diff(T1,T0)/1000.


loop(Pid, 0, Out) -> Pid ! {self(), check_now(Out)};
loop(Pid, N, Out) -> loop(Pid, N - 1, [now()|Out]).

check_now([_,_]) -> ok;
check_now([T1,T0|_]) when T1 < T0 -> error;
check_now([_|Ts]) -> check_now(Ts).

