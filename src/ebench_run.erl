-module(ebench_run).

%% API exports
-export([main/1, slogan/0]).

-include_lib("eministat/src/eministat.hrl").

-import(ebench, [abort/1, abort/2]).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    {ok, Opts, Rest} = ebench:parse_and_check(Args, options(), fun usage/1),
    run_command(Opts, Rest).

slogan() ->
    "Run the selected benchmarks".

%%====================================================================
%% Internal functions
%%====================================================================

options() ->
    [{iterations, $i, "iter", {integer, 3}, "Number of iterations."},
     {init, undefined, "init", {string, "ok."}, "Expression to run before each benchmark."},
     {stop, undefined, "stop", {string, "ok."}, "Expression to run after each benchmark."},
     {latest, $l, "latest", {string, "${TITLE}.term"}, "Name of latest symlink. "
      "Set to empty string to not create any latest link."},
     {title, $t, "title", {string, "BASE"}, "Title of the benchmark."},
     {output, $o, "output", {string,"${TITLE}-${TS}.term"},
      "File to place output into."},
     {command, undefined, undefined, undefined,
      "Command with arguments to start emulator. [default: erl]"}
     | ebench:benchmark_options()].

usage(Opts) ->
    getopt:usage(Opts, "ebench run", "-- [<command>]"),
    io:format("Run one or more benchmarks. The benchmarks to run are selected~n"
              "using the -c and -b options. See 'ebench list -h' for more details.~n"
              "The results of the benchmarks are put in the file set by -o. These~n"
              "files can later be passed to 'ebench plot' or 'ebench eministat' for~n"
              "further analysis.~n"
              "~n"
              "A symlink called ${TITLE}.term will be updated each time a~n"
              "benchmark run is completed to point to the latest run.~n"
              "~n"
              "The --init and --stop options can take execute erlang expression.~n"
              "If the expression returns a fun/3, that fun will be executed as~n"
              "F(Title, Class, Benchmark).~n"
              "~n"
              "Example:~n"
              "  ./ebench run -c small -t BASE -- erl~n"
              "  ./ebench run -c small -t BOUND -- erl +sbtdb~n"
             ).

run_command(Opts, []) ->
    run_command(Opts, ["erl"]);
run_command(Opts = #{ title := Title }, [Cmd | CmdOpts]) ->
    TsOpts = Opts#{ ts => calendar:local_time() },
    Output = make_output(TsOpts),
    D = ebench:open(Output),

    OutputOpts = TsOpts#{ output_file => Output },

    io:format(D, "~p.~n",[{metadata,
                           OutputOpts#{
                              system_version => erlang:system_info(system_version),
                              emu_args => lists:join(" ",erlang:system_info(emu_args))}}]),

    Classes = ebench:benchmarks(OutputOpts),
    io:format("Benchmarking using ~s ~s~n",[Cmd, lists:join(" ", CmdOpts)]),
    make_latest(OutputOpts),
    [run_class(D, Title, Cmd, CmdOpts, Class, BMs, OutputOpts) || {Class, BMs} <- Classes],
    io:format("Results saved into ~s~n",[Output]),
    file:close(D).

run_class(D, Title, Cmd, CmdOpts, Class, BMs, Opts) ->
    ERL_PATH = filename:dirname(os:find_executable(Cmd)),
    ebench:compile_class(Class, ERL_PATH, Opts),
    io:format(" Class: ~s~n", [Class]),
    [run_benchmark(D, Title, Cmd, CmdOpts, Class, BM, Opts) || BM <- BMs].

run_benchmark(D, Title, Cmd, CmdOpts, Class, BM, Opts) ->
    Rebar3Path = string:split(os:cmd("rebar3 path")," ", all),
    io:format("  ~s...~*.s", [BM,10 - length(BM), ""]),
    Name = [Title,"-",Class,"-",BM],
    CmdLine =
        ["-noshell", "-pz"] ++ Rebar3Path ++
        ["-pa", ebench:class_ebin_dir(Class, Opts),
         ebench:class_priv_dir(Class, Opts),
         "-s","ebench_runner", "init", Title, Class, BM, maps:get(init, Opts),
         "-s","ebench_runner", "main", Name, tostr(maps:get(iterations, Opts)) , BM,
         "-s","ebench_runner", "stop", Title, Class, BM, maps:get(stop, Opts)],
    BMData = parse(CmdLine, spawn_emulator(Cmd, CmdOpts ++ CmdLine)),
    io:format(D, "{~p,~p,~p}.~n",[Class, BM, BMData]),
    {Factor, Unit} = case eministat_ds:mean(BMData) of
                         Mean when Mean > 1000 * 1000 ->
                             {1000 * 1000, "s"};
                         Mean when Mean > 1000 ->
                             {1000, "ms"};
                         _Mean ->
                             {1, "µs"}
                     end,
    io:format(" ~13g ± ~.4f ~s",
              [eministat_ds:mean(BMData) / Factor,
               eministat_ds:std_dev(BMData) / Factor,
               Unit]),

    %% We use eministat to calculate the useability of the data set
    %% python perf uses: > 10% stddev, > 50% min/max.

    {_, Severity} = eministat_analysis:outlier_variance(
                      eministat_ds:mean(BMData),
                      eministat_ds:std_dev(BMData),
                      BMData#dataset.n),

    case Severity of
        severe -> io:format(" WARNING: Severe outlier variance, the data set is probably unusable.");
        _ -> io:format(", ~p outlier variance.", [Severity])
    end,
    io:format("~n").

make_output(#{ output := Output, title := Title, ts := TS }) ->
    keyword_replace(Output,
                    [{"TITLE", Title}, {"TS", tostr(TS)}]).

make_latest(#{ latest := "" }) ->
    ok;
make_latest(#{ latest := LatestTemplate, output_file := Output, title := Title }) ->
    Latest = keyword_replace(LatestTemplate, [{"TITLE", Title}]),
    case file:read_link_info(Latest) of
        {ok, _} ->
            ok = file:delete(Latest);
        _E ->
            ok
    end,
    ok = file:make_symlink(Output, Latest).

%% Replace ${KEY} with value from proplist
keyword_replace(String, Keywords) ->
    keyword_replace(String, Keywords, undefined).

keyword_replace("${" ++ T, Keywords, undefined) ->
    keyword_replace(T, Keywords, "");
keyword_replace([H|T], Keywords, undefined) ->
    [H | keyword_replace(T, Keywords, undefined)];
keyword_replace("}" ++ T, Keywords, Keyword) ->
    [proplists:get_value(lists:reverse(Keyword), Keywords)
     | keyword_replace(T, Keywords, undefined)];
keyword_replace([H|T], Keywords, Keyword) ->
    keyword_replace(T, Keywords, [H|Keyword]);
keyword_replace([], _, undefined) ->
    [];
keyword_replace([], _, Keyword) ->
    Keyword.

tostr({{YY,MM,DD},{HH,Mi,SS}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B-~2..0B-~2..0B",[YY,MM,DD,HH,Mi,SS]);
tostr(Int) when is_integer(Int) ->
    integer_to_list(Int).

parse(Cmd, String) ->
    try
        {ok, Tokens, _} = erl_scan:string(String),
        {ok, Term} = erl_parse:parse_term(Tokens),
        Term
    catch _:_ ->
            abort("Failed to parse: ~p~nCmd: ~s~n",[String, Cmd])
    end.

spawn_emulator(Cmd, Args) ->
    SpawnArgs = [stderr_to_stdout, stream, in, hide, {args, Args}],
    io:format("Args ~p~n", [SpawnArgs]),
    Port = open_port({spawn_executable, os:find_executable(Cmd)}, SpawnArgs),
    MonRef = erlang:monitor(port, Port),
    Bytes = get_data(Port, MonRef, []),
    demonitor(MonRef, [flush]),
    Bytes.

get_data(Port, MonRef, Sofar) ->
    receive
        {Port, {data, Bytes}} -> get_data(Port, MonRef, [Sofar, Bytes]);
        {'DOWN', MonRef, _, _, _} -> lists:flatten(Sofar)
    end.
