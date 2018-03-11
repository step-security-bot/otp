-module(ebench_plot).

%% API exports
-export([main/1, slogan/0]).

-import(ebench, [abort/1, abort/2]).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    {ok, Opts, Rest} = ebench:parse_and_check(Args, options(), fun usage/1),
    plot(Opts, Rest).

slogan() ->
    "Plot the results of benchmarks using gnuplot".

%%====================================================================
%% Internal functions
%%====================================================================

options() ->
    [{type, undefined, "type", {string, "histogram"},
      "What type of plot should be done, histogram or timeseries."},
     {output, $o, "output", {string,"benchmark.svg"},
      "The file to output the analysis to"},
     {base, undefined, undefined, undefined,
      "The base benchmark file to compare with."}
     | ebench:benchmark_options()].

usage_options(Opts) ->
    ebench:sort_options(
      Opts ++ [{compare, undefined, undefined, undefined,
                "The benchmark to compare with."}]).

usage(Opts) ->
    getopt:usage(usage_options(Opts), "ebench plot", "<base> <compare...>"),
    io:format("Compare two or more benchmark runs using gnuplot.~n"
              "~n"
              "Example:~n"
              "  ./ebench plot BASE.term BOUND.term~n"
              "  ./ebench plot --type timeseries BASE-*.term BOUND-*.term~n"
             ).

plot(_, []) ->
    ebench:format_error(
      {error, {missing_required_option, compare}},
      fun usage/1, usage_options(options())),
    erlang:halt(1);
plot(Opts = #{ base := Base, type := Type }, Rest) ->
    [abort("Could not find gnuplot program") || os:find_executable("gnuplot") =:= false],
    plot(Type, [Base | Rest], Opts);
plot(_, _) ->
    ebench:format_error(
      {error, {missing_required_option, base}},
      fun usage/1, options()),
    erlang:halt(1).

plot("histogram", Tags, Opts) ->

    TagData = ebench:read_tags(Tags, ebench:benchmarks(Opts)),

    {ok, D, TmpName} = ebench:opentmp(),

    io:format(D, "Benchmarks", []),
    [io:format(D, " ~s ~s-min ~s-max", [Tag, Tag, Tag]) || Tag <- Tags],
    io:format(D, "~n", []),

    ebench:tdforeach(
      fun(Class, BM, Data) ->
              io:format(D, "~s/~s",[Class, BM]),
              [io:format(D, " ~f ~f ~f",[eministat_ds:median(DS),
                                         eministat_ds:min(DS),
                                         eministat_ds:max(DS)])
               || {_, DS} <- Data],
              io:format(D, "~n", [])
      end, TagData, Tags),

    file:close(D),
    Cmd = ["gnuplot -e \""
           "data='", TmpName, "';"
           "out='", maps:get(output, Opts), "';"
           "tags=",integer_to_list(length(Tags)),
           "\" gnuplot_scripts/multitag_histo.gnuplot"],
    case os:cmd(Cmd) of
        [] ->
            io:format("plotted ~s~n", [maps:get(output, Opts)]);
        Output ->
            abort("~s~n~s~n",[Cmd, Output])
    end,
    file:delete(TmpName);
plot("timeseries", Files, Opts) ->

    ClassBenchmarks = ebench:benchmarks(Opts),
    TagData = ebench:read_tags(Files, ClassBenchmarks),

    %% Sort on the timestamp
    TsTagData = lists:sort([{maps:get(ts, MD), Tag, BMs, MD} || {Tag, BMs, MD} <- TagData]),

    %% Get all the tags in all the files
    Tags = lists:usort([Tag || {Tag, _BMs, _MD} <- TagData]),

    lists:foreach(
      fun({Class, Benchmarks}) ->
              lists:foreach(
                fun(Benchmark) ->
                        %% Create one datafile per benchmark type
                        {ok, D, TmpName} = ebench:opentmp(),

                        %% Output the title of the tags used in the given files
                        io:format(D, "Benchmarks", []),
                        [io:format(D, " ~s ~s-min ~s-max", [Tag, Tag, Tag]) || Tag <- Tags],
                        io:format(D, "~n", []),

                        [{_, NormalizeDS}] = getdsdata(TsTagData, Class, Benchmark),

                        N = eministat_ds:median(NormalizeDS),

                        %% Loop over TagData sorted on timestamp
                        lists:foreach(
                           fun({TS, Tag, _CBMs, _MD} = TD) ->

                                   %% Get the DS for the specific benchmark we are interested in
                                   case getdsdata(TD, Class, Benchmark) of
                                       [{Benchmark, DS}] ->
                                           io:format(D, "~s", [ts2str(TS)]),
                                           %% For each tag, output either the data or
                                           %% the data missing tag.
                                           lists:foreach(
                                             fun(T) when T =:= Tag ->
                                                     io:format(D, " ~f ~f ~f",
                                                               [eministat_ds:median(DS) / N,
                                                                eministat_ds:min(DS) / N,
                                                                eministat_ds:max(DS) / N]);
                                                (_) ->
                                                     io:format(D, " - - -", [])
                                             end, Tags),
                                           io:format(D, "~n", []);
                                       _ ->
                                           ok
                                   end
                           end, TsTagData),

                        %% Run the gnuplot command that plots the graph
                        Cmd = ["gnuplot -e \""
                               "data='", TmpName, "';"
                               "out='", Benchmark ++ ".svg", "';"
                               "tags=",integer_to_list(length(Tags)),
                               "\" gnuplot_scripts/timeseries.gnuplot"],
                        case os:cmd(Cmd) of
                            [] ->
                                io:format("plotted ~s~n", [Benchmark ++ ".svg"]);
                            Output ->
                                abort("~s~n~s~n",[Cmd, Output])
                        end,
                        file:delete(TmpName),
                        ok
                end, Benchmarks)
      end, ClassBenchmarks),
    ok.

getdsdata([TD|T], Class, Benchmark) ->
    case getdsdata(TD, Class, Benchmark) of
        [] ->
            getdsdata(T, Class, Benchmark);
        Else ->
            Else
    end;
getdsdata({_TS, _Tag, CBMs, _MD}, Class, Benchmark) ->
    [lists:keyfind(Benchmark, 1, BMs) || {C, BMs} <- CBMs, C =:= Class].

ts2str({{YY,MM,DD},{HH,Mi,SS}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B-~2..0B-~2..0B",[YY,MM,DD,HH,Mi,SS]).
