-module(trace_file_nif).

%%% @doc A low overhead nif to trace to file
%%%

-export([start/1, start/2, stop/1, parse/1]).

-export([trace/5,
         trace_call/5,
         trace_garbage_collection/5,
         trace_return_to/5,
         trace_procs/5,
         trace_running_procs/5
        ]).

-export([enabled/3,
         enabled_call/3,
         enabled_garbage_collection/3,
         enabled_return_to/3,
         enabled_procs/3,
         enabled_running_procs/3
        ]).


-on_load(on_load/0).

on_load() ->
    PrivDir = code:priv_dir(runtime_tools),
    LibName = "trace_file_nif",
    Lib = filename:join([PrivDir, "lib", LibName]),
    Status = case erlang:load_nif(Lib, 0) of
                 ok -> ok;
                 {error, {load_failed, _}}=Error1 ->
                     ArchLibDir = 
                         filename:join([PrivDir, "lib", 
                                        erlang:system_info(system_architecture)]),
                     Candidates =
                         filelib:wildcard(filename:join([ArchLibDir,LibName ++ "*" ])),
                     case Candidates of
                         [] -> Error1;
                         [Candidate|_] ->
                             erlang:load_nif(filename:rootname(Candidate), 0)
                     end;
                 Error1 -> Error1
             end,
    case Status of
        ok -> ok;
        {error, {E, Str}} ->
            error_logger:error_msg(
              "Unable to load trace_file_nif library."
              "Failed with error:~n\"~p, ~s\"~n",
              [E,Str]),
            Status
    end.

start(Filename) ->
    start(Filename, #{ filter => "gzip",
                       filter_options => [{"compression-level","1"}]}).
start(Filename, Opts) ->
    case open_archive(add_zero(Filename), add_zero(Opts)) of
        {error, Reason} ->
            {error, Reason};
        TracerRef ->
            TracerRef = start_tracer(TracerRef),
            receive
                {started, TracerRef} ->
                    TracerRef
            end
    end.

add_zero(Map) when is_map(Map) ->
    maps:map(fun(_Key, Val) -> add_zero(Val) end, Map);
add_zero([{Key, Val} | T]) ->
    [{add_zero(Key), add_zero(Val)} | add_zero(T)];
add_zero([]) -> [];
add_zero(String) when is_list(String) ->
    [String,0];
add_zero(Atom) when is_atom(Atom) ->
    Atom.

open_archive(_Filename, _Options) ->
    erlang:nif_error(nif_not_loaded).

start_tracer(_Ref) ->
    erlang:nif_error(nif_not_loaded).

stop(TracerRef) ->
    TracerRef = stop_tracer(TracerRef),
    receive
        {stopped, TracerRef} ->
            close_archive(TracerRef)
    end.

stop_tracer(_TracerRef) ->
    erlang:nif_error(nif_not_loaded).

close_archive(Archive) ->
    Endian = endianess(),
    Atoms = erlang:system_info(atoms),
    close_archive(Archive, Endian, Atoms).

endianess() ->
    case <<32:32/native>> of
        <<32:32/big>> ->
            big;
        <<32:32/little>> ->
            little
    end.

close_archive(_Archive, _Endian, _Atoms) ->
    erlang:nif_error(nif_not_loaded).

-define(TRACE_CALL,1).
-define(TRACE_RETURN_TO,2).
-define(TRACE_SPAWNED,3).
-define(TRACE_EXIT,4).
-define(TRACE_IN,5).
-define(TRACE_OUT,6).
-define(TRACE_GC_MINOR_START,7).
-define(TRACE_GC_MINOR_END,8).
-define(TRACE_GC_MAJOR_START,9).
-define(TRACE_GC_MAJOR_END,10).

parse(Filename) ->
    {ok, Files} = erl_tar:extract(Filename, [compressed, memory]),
    Atoms = parse_atoms(proplists:get_value("atoms",Files)),
    Events = lists:flatmap(
               fun({"atoms", _Data}) -> [];
                  ({_, Data}) -> parse_file(Data, Atoms)
               end,
               Files),
    SEvents = lists:sort(fun(E1,E2) ->
                                 element(size(E1), E1) < element(size(E2), E2)
                         end, Events),
    lists:map(fun fix_ts/1, SEvents).

fix_ts(E) ->
    {Mega, S, Micro, _Nano} = element(size(E), E),
    setelement(size(E), E, {Mega, S, Micro}).

parse_file(<<?TRACE_CALL:64/native, Pid:64/native, TS:64/native,
             M:64/native, F:64/native, A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), call, make_mfa(M,F,A,Atoms), {cp, undefined}, make_ts(TS)} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_RETURN_TO:64/native, Pid:64/native, TS:64/native,
             M:64/native, F:64/native, A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), return_to, make_mfa(M,F,A,Atoms), make_ts(TS)} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_SPAWNED:64/native, Pid:64/native, TS:64/native,
             M:64/native, F:64/native, A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), spawned, undefined, make_mfa(M,F,A,Atoms), make_ts(TS)} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_EXIT:64/native, Pid:64/native, TS:64/native,
             _M:64/native, _F:64/native, _A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), exit, undefined, make_ts(TS)} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_IN:64/native, Pid:64/native, TS:64/native,
             M:64/native, F:64/native, A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), in, make_mfa(M,F,A,Atoms), make_ts(TS)} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_OUT:64/native, Pid:64/native, TS:64/native,
             M:64/native, F:64/native, A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), out, make_mfa(M,F,A,Atoms), make_ts(TS)} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_GC_MINOR_START:64/native, Pid:64/native, TS:64/native,
             _M:64/native, _F:64/native, _A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), gc_minor_start, undefined, make_ts(TS)} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_GC_MINOR_END:64/native, Pid:64/native, TS:64/native,
             _M:64/native, _F:64/native, _A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), gc_minor_end, undefined, make_ts(TS)} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_GC_MAJOR_START:64/native, Pid:64/native, TS:64/native,
             _M:64/native, _F:64/native, _A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), gc_major_start, undefined, make_ts(TS)} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_GC_MAJOR_END:64/native, Pid:64/native, TS:64/native,
             _M:64/native, _F:64/native, _A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), gc_major_end, undefined, make_ts(TS)} | parse_file(Rest,Atoms)];
parse_file(<<>>, _Atoms) ->
    [];
parse_file(<<0:64/native,_/binary>>, _Atoms) ->
    [].

parse_atoms(<<0:64/native,_/binary>>) ->
    [];
parse_atoms(<<>>) ->
    [];
parse_atoms(<<Tag:64/native, Sz:64/native, Str:Sz/binary, Rest/binary>>) ->
    [{Tag, list_to_atom(binary_to_list(Str))} | parse_atoms(Rest)].

make_mfa(M,F,A,Atoms) ->
    {proplists:get_value(M,Atoms),proplists:get_value(F,Atoms),A bsr 4}.

make_pid(Pid) ->
    3 = Pid band 3,
    PidData = (Pid bsr 35) band 16#0fffffff,
    PidList = io_lib:format("<0.~p.~p>",[PidData band 16#7fff,
                                         (PidData bsr 15) band 16#7fff]),
    list_to_pid(lists:flatten(PidList)).

make_ts(TS) ->
    ErlangSystemTime = TS div 1000, % Convert from ns to us
    MegaSecs = ErlangSystemTime div 1000000000000,
    Secs = ErlangSystemTime div 1000000 - MegaSecs*1000000,
    MicroSecs = ErlangSystemTime rem 1000000,
    {MegaSecs, Secs, MicroSecs, TS rem 1000}.

trace(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_call(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_garbage_collection(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_return_to(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_procs(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_running_procs(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

enabled(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_call(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_garbage_collection(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_return_to(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_procs(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_running_procs(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).
