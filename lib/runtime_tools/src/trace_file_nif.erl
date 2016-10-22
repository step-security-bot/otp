-module(trace_file_nif).

%%% @doc A low overhead nif to trace to file
%%%

-export([start/1, stop/1]).

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
