-module(test).

-export([test/2, test2/2, test3/2]).

test(P,N) ->
    trace_file_nif:start(ok),
    L = lists:seq(1,P),
    [spawn_monitor(fun() -> loop(Pn, N, self(), self()) end) || Pn <- L],
    [receive _ -> ok end || _ <- L],
    trace_file_nif:stop(ok),
    receive ok -> ok end.

loop(_P, 0, _S, _C) ->
    ok;
loop(P, N, S, C) ->
    case trace_file_nif:enabled_call(call, S, C) of
        trace ->
            trace_file_nif:trace_call(call, S, C, {a, b, N}, #{});
        _ ->
            ok
    end,
    loop(P, N-1, S, C).

test2(P,N) ->
    Prt = ((dbg:trace_port(file, "/tmp/dmp"))()),
    L = lists:seq(1,P),
    [spawn_monitor(fun() -> loop2(Pn, N, Prt, self()) end) || Pn <- L],
    [receive _M -> ok end || _ <- L],
    erlang:port_control(Prt, $f, ""),
    port_close(Prt).

loop2(_P, 0, _S, _C) ->
    ok;
loop2(P, N, S, C) ->
    case erl_tracer:enabled(call, S, C) of
        trace ->
            erl_tracer:trace(call, S, C, {a, b, N}, #{});
        _ ->
            ok
    end,
    loop2(P, N-1, S, C).

test3(P,N) ->
    Pid = spawn(fun flush/0),
    L = lists:seq(1,P),
    [spawn_monitor(fun() -> loop3(Pn, N, Pid, self()) end) || Pn <- L],
    [receive _M -> ok end || _ <- L],
    Pid ! {sync, self()},
    receive ok -> ok end.

loop3(_P, 0, _S, _C) ->
    ok;
loop3(P, N, S, C) ->
    case erl_tracer:enabled(call, S, C) of
        trace ->
            erl_tracer:trace(call, S, C, {a, b, N}, #{});
        _ ->
            ok
    end,
    loop3(P, N-1, S, C).

flush() ->
    receive
        {sync,Pid} ->
            Pid ! ok;
        _ ->
            flush()
    end.
