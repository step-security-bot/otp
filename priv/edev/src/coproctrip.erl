%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    procroundtrip.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-02-27
%%
%% Originally from kevin smith: https://gist.github.com/kevsmith/5048687

-module(coproctrip).
 
-export([
	run/1,
	benchmark_arguments/0,
	benchmark_unit/0
    ]).

benchmark_unit() ->
    "ms".
 
benchmark_arguments() ->
    [{run, [N]} || N <- [5000000]].


run(Count) ->
    Me = self(),
    T0 = now(),
    F  = spawn(fun() -> worker(Me, Count) end),
    spawn(fun() -> worker(Me, F, Count) end),
    wait_for_done(2),
    T1 = now(),
    timer:now_diff(T1,T0)/1000.
 
wait_for_done(0) ->
    ok;
wait_for_done(Count) ->
    receive
        done ->
            wait_for_done(Count - 1)
    end.
 
worker(Owner, 0) ->
    Owner ! done;
worker(Owner, Count) ->
    receive
        {From, ping} ->
            From ! pong,
            worker(Owner, Count - 1)
    end.
 
worker(Owner, _Target, 0) ->
    Owner ! done;
worker(Owner, Target, Count) ->
    Target ! {self(), ping},
    receive
        pong ->
            worker(Owner, Target, Count - 1)
    end.
