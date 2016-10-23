-module(trace_file_nif).

%%% @doc A low overhead nif to trace to file
%%%

-export([start/1, stop/1, parse/1]).

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
                     Candidate =
                         filelib:wildcard(filename:join([ArchLibDir,LibName ++ "*" ])),
                     case Candidate of
                         [] -> Error1;
                         _ ->
                             ArchLib = filename:join([ArchLibDir, LibName]),
                             erlang:load_nif(ArchLib, 0)
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

start(_) ->
    erlang:nif_error(nif_not_loaded).

stop(_) ->
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
    lists:flatmap(
      fun({"atoms", _Data}) -> [];
         ({_, Data}) -> parse_file(Data, Atoms)
      end,
      Files).

parse_file(<<?TRACE_CALL:64/native, Pid:64/native, TS:64/native,
             M:64/native, F:64/native, A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), call, make_mfa(M,F,A,Atoms), TS} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_RETURN_TO:64/native, Pid:64/native, TS:64/native,
             M:64/native, F:64/native, A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), return_to, make_mfa(M,F,A,Atoms), TS} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_SPAWNED:64/native, Pid:64/native, TS:64/native,
             M:64/native, F:64/native, A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), spawned, undefined, make_mfa(M,F,A,Atoms), TS} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_EXIT:64/native, Pid:64/native, TS:64/native,
             _M:64/native, _F:64/native, _A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), exit, undefined, TS} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_IN:64/native, Pid:64/native, TS:64/native,
             M:64/native, F:64/native, A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), in, make_mfa(M,F,A,Atoms), TS} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_OUT:64/native, Pid:64/native, TS:64/native,
             M:64/native, F:64/native, A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), out, make_mfa(M,F,A,Atoms), TS} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_GC_MINOR_START:64/native, Pid:64/native, TS:64/native,
             _M:64/native, _F:64/native, _A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), gc_minor_start, undefined, TS} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_GC_MINOR_END:64/native, Pid:64/native, TS:64/native,
             _M:64/native, _F:64/native, _A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), gc_minor_end, undefined, TS} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_GC_MAJOR_START:64/native, Pid:64/native, TS:64/native,
             _M:64/native, _F:64/native, _A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), gc_major_start, undefined, TS} | parse_file(Rest,Atoms)];
parse_file(<<?TRACE_GC_MAJOR_END:64/native, Pid:64/native, TS:64/native,
             _M:64/native, _F:64/native, _A:64/native, Rest/binary>>, Atoms) ->
    [{trace_ts, make_pid(Pid), gc_major_end, undefined, TS} | parse_file(Rest,Atoms)];
parse_file(<<>>, _Atoms) ->
    [];
parse_file(<<0:64/native,_/binary>>, _Atoms) ->
    [].

parse_atoms(<<0:64/native,_/binary>>) ->
    [];
parse_atoms(<<>>) ->
    [];
parse_atoms(<<Tag:64/native, Sz:16/native, Str:Sz/binary, Rest/binary>>) ->
    [{Tag, list_to_atom(binary_to_list(Str))} | parse_atoms(Rest)].

make_mfa(M,F,A,Atoms) ->
    {proplists:get_value(M,Atoms),proplists:get_value(F,Atoms),A bsr 4}.

make_pid(Pid) ->
    3 = Pid band 3,
    PidData = (Pid bsr 35) band 16#0fffffff,
    {0,PidData band 16#7fff,(PidData bsr 15) band 16#7fff}.

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
