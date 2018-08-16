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
%%% File    : big.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : A simple message passing benchmark
%%%
%%% Created : 30 Dec 2005 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(big).

-export([bang/1]).

-export([benchmark_arguments/0]).

benchmark_arguments() ->
    #{ bang =>
           #{ before_scenario => fun spawn_procs/1,
              run => fun(Procs) ->
                             send_procs(Procs, {procs, Procs, self()}),
                             receive_msgs(Procs),
                             Procs
                     end
            },
       inputs => [250,500,750]
     }.

pinger([], [], true) ->
    receive
	{procs, Procs, ReportTo} ->
	    pinger(Procs, [], ReportTo)
    end;
pinger([], [], false) ->
    receive {ping, From} -> From ! {pong, self()} end,
    pinger([],[],false);
pinger([], [], ReportTo) ->
    ReportTo ! self(),
    pinger([],[],false);
pinger([],[Po|Pos] = Pongers, ReportTo) ->
    receive
	{ping, From} ->
	    From ! {pong, self()},
	    pinger([], Pongers, ReportTo);
	{pong, Po} ->
	    pinger([], Pos, ReportTo)
    end;
pinger([Pi|Pis], Pongers, ReportTo) ->
    receive {ping, From} -> From ! {pong, self()}
    after 0 -> ok
    end,
    Pi ! {ping, self()},
    pinger(Pis, [Pi|Pongers], ReportTo).

spawn_procs(N) when N =< 0 ->
    [];
spawn_procs(N) ->
    [spawn_link(fun () -> pinger([],[],true) end) | spawn_procs(N-1)].

send_procs([], Msg) ->
    Msg;
send_procs([P|Ps], Msg) ->
    P ! Msg,
    send_procs(Ps, Msg).

receive_msgs([]) ->
    ok;
receive_msgs([P|Ps]) ->
    receive
	P ->
	    receive_msgs(Ps)
    end.


bang(N) when is_integer(N) ->
    
