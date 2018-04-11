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

-module(procparser).
-export([run/2]).
-export([benchmark_arguments/0]).

benchmark_arguments() ->
    [{run, [normal, O]} || O <- [[],[{fullsweep_after, 0}]]].

run(Chain, SpawnOpts) -> 
    Me  = self(), 
    T0  = now(),
    spawn_opt(fun() -> read_ahead_init(Me, Chain, SpawnOpts) end, [link] ++ SpawnOpts),
    generator(),
    T1  = now(),
    timer:now_diff(T1,T0)/1000.

generator() -> generator(10).
generator(0) ->
    receive 
    	{_Pid, done} -> ok;
    	{Pid, get} ->
	    Pid ! {self(), eod},
	    generator(0)
    end;
generator(N) ->
    receive
    	{Pid, get} ->
	    B = list_to_binary([random:uniform(2) - 1, lists:duplicate(4096*9000-1,2)]),
	    Pid ! {self(), data, B},
	    generator(N - 1)
    end.

read_ahead_init(Generator, chain, SpawnOpts) ->
    Ahead = self(),
    Parse = spawn_opt(fun() -> parse(Ahead) end, [link] ++ SpawnOpts),
    Chain = spawn_opt(fun() -> chain(Generator,Parse) end, [link] ++ SpawnOpts),
    read_ahead(Chain, Parse, <<>>);

read_ahead_init(Generator, normal, SpawnOpts) ->
    Ahead = self(),
    Parse = spawn_opt(fun() -> parse(Ahead) end, [link] ++ SpawnOpts),
    read_ahead(Generator, Parse, <<>>).

read_ahead(Parent, Pid, Bin) ->
    receive
    	{_, done} -> 
	    Parent ! {self(), done};
    	{Req, get, N} ->
	    case Bin of
	    	<<B0:N/binary, Rest/binary>> ->
		    Req ! {self(), B0},
		    read_ahead(Parent, Pid, Rest);
		_ ->
		    Parent ! {self(), get},
		    receive 
		    	{Parent, data, B} ->
			    Bin0 = list_to_binary([Bin, B]),
			    self() ! {Req, get, N},
			    read_ahead(Parent, Pid, Bin0);
			{Parent, eod} ->
		    	    Req ! {self(), Bin},
		    	    Req ! {self(), eod},
			    read_ahead(Parent, Pid, <<>>)
		    end
	    end
    end.

chain(From, To) ->
    receive
        {To, get} ->
	    io:format("get~n"),
	    From ! {self(), get},
	    chain(From, To);
	{From, data, B} ->
	    io:format("data~n"),
	    To ! {self(), data, B},
	    chain(From, To);
	{From, eod} ->
	    io:format("eod~n"),
	    To ! {self(), eod},
	    chain(From, To);
	{To, done} ->
	    io:format("done~n"),
	    From ! {self(), done};
	Other ->
	    io:format("Other: ~p~n", [Other]),
	    chain(From, To)
    end.


parse(Pid) -> parse(Pid, get_data(Pid, 1024)).

parse(Pid,  eod) -> Pid ! {self(), done};
parse(Pid, <<>>) -> Pid ! {self(), done};
parse(Pid, <<A:8, _/binary>>) when A  > 0 -> parse(Pid, get_data(Pid));
parse(Pid, <<A:8, _/binary>>) when A == 0 -> parse(Pid, get_data(Pid)).

get_data(Pid) -> get_data(Pid, 1024).
get_data(Pid, N) -> Pid ! {self(), get, N}, receive {Pid, B} -> B end.
