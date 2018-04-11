%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    test.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-03-28
%%

-module(capacity).

-export([
	load/2,
	sample/1,
	benchmark_arguments/0,
	benchmark_unit/0]).


-record(state, {
	type,
	target,
	sample_n,
	util_tot,
	ramp_i,
	ticker_sample
    }).



benchmark_arguments() ->
    [{load, [Type, N]} || 
	Type <- [scheduler, cpu],
	N <- [50,75]
    ].

benchmark_unit() -> "#processes".


load(Type, Target) ->
    Me  = self(),
    case start_util(Type) of
	ok ->
	    mload:start(),
	    loop(#state{
		    type = Type,
		    target = Target,
		    sample_n = 0,
		    util_tot = 0,
		    ticker_sample = start_ticker(Me, 1800, sample)
		});
	error ->
	    0
    end.

loop(#state{ sample_n = N, util_tot = Tot, target = Target } = S0) when N > 2 ->
    S1 = S0#state{ sample_n = 0, util_tot = 0 },
    Loaded = mload:loaded(),
    %io:format("sample_from_target = ~.2f, load = ~w~n", [Target - Tot/N, Loaded]),
    case {change_load(Tot/N - Target), Loaded} of
	{done,_} ->
	    Res = mload:loaded(),
	    %io:format("target found = ~.2f -> load = ~w~n", [Target - Tot/N, Res]),
	    mload:stop(),
	    stop_util(S1#state.type),
	    Res;
	{{dec, _V}, 0} ->
	    %io:format("target cannot be found -> load = ~w~n", [0]),
	    mload:stop(),
	    stop_util(S1#state.type),
	    0;
	{{Change, V}, _} ->
	    %io:format("~w load with ~w~n", [Change, V]),
	    mload:Change(V),
	    loop(S1)
    end;
	
loop(#state{ ticker_sample = Sample, type = Type, util_tot = Tot, sample_n = N } = S ) ->
    receive
	{Sample, sample} ->
	    Util = sample(Type),
	    % io:format("sample ~w ~.2f~n", [Type, Util]),
	    loop(S#state{ util_tot = Tot + Util, sample_n = N + 1 })
    end.

sample(scheduler) ->
    Utils = sched_util:util(),
    lists:sum([V*100 || {_, V} <- Utils]) / length(Utils);
sample(cpu) ->
    Utils = cpu_sup:util([per_cpu]),
    lists:sum([V || {_, V,_,_} <- Utils]) / length(Utils).

start_ticker(Pid, T, Msg) ->
    spawn_link(fun() -> ticker_loop(Pid, T, Msg) end).

ticker_loop(Pid, T, Msg) ->
    receive after T ->
	    Pid ! {self(), Msg}, 
	    ticker_loop(Pid, T, Msg)
    end.


%% aux
change_load(Off) when Off < 0.3, Off > -0.3 -> done;
change_load(Off) ->
    V = Off/100,
    Weight = 3000,

    Change = trunc(Weight*V),

    if 
	Change < 0 -> {inc, trunc(-Change) + 1};
	true -> {dec, trunc(Change) + 1}
    end.

start_cpu_util() ->
    try
	application:start(sasl),
	application:start(os_mon),
	cpu_sup:util([per_cpu]),
	ok
    catch
	_:_ ->
	    error
    end.


stop_cpu_util() ->
    application:stop(os_mon),
    application:stop(sasl),
    ok.

start_util(cpu) -> start_cpu_util();
start_util(scheduler) -> try sched_util:start(), ok catch _:_ -> error end.

stop_util(cpu) -> stop_cpu_util();
stop_util(scheduler) -> sched_util:stop().
