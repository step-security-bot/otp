-module(ebench_run).

%% API exports
-export([main/1]).

-import(ebench, [abort/1, abort/2]).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    {ok, Opts, Rest} = ebench:parse_and_check(Args, options(), fun usage/1),
    run_commands(maps:merge(Opts, parse_cmd_rest(Rest))).

%%====================================================================
%% Internal functions
%%====================================================================

options() ->
    [{iterations, $i, "iter", {integer, 3}, "Number of iterations"} |
     ebench:benchmark_options()].

usage(Opts) ->
    getopt:usage(Opts, "ebench run", "Title=Command..."),
    io:format("Example:~n"
              "  ./ebench run -c small BASE=erl \"BOUND=erl +sbtdb\"~n").

parse_cmd_rest(Args) ->
    #{ commands =>
           lists:map(
             fun(Arg) ->
                     [Title | Cmd] = string:lexemes(Arg, "="),
                     [Erl | Opts] = string:lexemes(Cmd, " "),
                     {Title, Erl, lists:join(" ", Opts)}
             end, Args) }.

run_commands(Opts = #{ commands := Cmds }) ->
    TSOpts = Opts#{ ts => calendar:local_time() },
    Classes = ebench:benchmarks(Opts),
    [run_command(Title, Erl, CmdOpts, Classes, TSOpts) || {Title, Erl, CmdOpts} <- Cmds].

run_command(Title, Erl, CmdOpts, Classes, Opts = #{ ts := Ts }) ->
    io:format("Benchmarking: ~s ~s~n",[Erl, CmdOpts]),
    Latest = filename:join([".", "results", Title, "latest"]),
    TSDir = filename:join([".", "results", Title, ts2str(Ts)]),
    ebench:mkdir(TSDir),
    case file:read_link_info(Latest) of
        {ok, _} ->
            ok = file:delete(Latest);
        _E ->
            ok
    end,
    ok = file:make_symlink(lists:flatten(ts2str(Ts)), Latest),
    [run_class(Title, Erl, CmdOpts, Class, BMs, Opts) || {Class, BMs} <- Classes].

run_class(Title, Erl, CmdOpts, Class, BMs, Opts = #{ ts := Ts }) ->
    ERL_PATH = filename:dirname(os:find_executable(Erl)),
    ebench:compile_class(Class, ERL_PATH, Opts),
    io:format("  Class: ~s~n", [Class]),
    file:write_file(filename:join([".", "results", Title, ts2str(Ts), "METADATA"]),
                    io_lib:format("~p.~n~p.",
                                  [erlang:system_info(system_version),
                                   lists:join(" ",erlang:system_info(emu_args))])),
    Fd = ebench:open(filename:join([".", "results", Title, ts2str(Ts), Class])),
    [run_benchmark(Title, Erl, CmdOpts, Class, BM, Fd, Opts) || BM <- BMs].

run_benchmark(Title, Erl, CmdOpts, Class, BM, Fd, Opts) ->
    Rebar3Path = os:cmd("rebar3 path"),
    io:format("    Running: ~s...", [BM]),
    Name = [Title,"-",Class,"-",BM],
    Cmd = lists:concat(
            [Erl, " ", CmdOpts, " -noshell"
             " -pz ", Rebar3Path,
             " -pa ", ebench:class_ebin_dir(Class, Opts), " ", ebench:class_priv_dir(Class, Opts),
             " -s ebench_runner main ",Name," ", maps:get(iterations, Opts) ," ",
             BM, " -s init stop"]),
    io:format(Fd, "{~p,~p}.~n",[BM, parse(Cmd, os:cmd(Cmd))]),
    io:format("done~n").

ts2str({{YY,MM,DD},{HH,Mi,SS}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",[YY,MM,DD,HH,Mi,SS]).

parse(Cmd, String) ->
    try
        {ok, Tokens, _} = erl_scan:string(String),
        {ok, Term} = erl_parse:parse_term(Tokens),
        Term
    catch _:_ ->
            abort("Failed to parse: ~p~nCmd: ~s~n",[String, Cmd])
    end.
