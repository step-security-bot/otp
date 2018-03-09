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
    [{type, undefined, "type", {string,"histogram"},
      "What type of plot should be done"},
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
    io:format("Compare two benchmark runs using gnuplot.~n"
              "~n"
              "Example:~n"
              "  ./ebench plot BASE BOUND~n").

plot(_, []) ->
    ebench:format_error(
      {error, {missing_required_option, compare}},
      fun usage/1, usage_options(options())),
    erlang:halt(1);
plot(Opts = #{ base := Base, type := Type }, Rest) ->
    TagData = ebench:read_tags([Base | Rest], ebench:benchmarks(Opts)),
    plot(Type, TagData, [Base | Rest], Opts);
plot(_, _) ->
    ebench:format_error(
      {error, {missing_required_option, base}},
      fun usage/1, options()),
    erlang:halt(1).

plot("histogram", TagData, Titles, Opts) ->
    [abort("Could not find gnuplot program") || os:find_executable("gnuplot") =:= false],

    DataFile = string:trim(os:cmd("mktemp")),
    {ok, D} = file:open(DataFile, [write]),

    io:format(D, "Benchmarks", []),
    [io:format(D, " ~s ~s-min ~s-max", [Title, Title, Title]) || Title <- Titles],
    io:format(D, "~n", []),

    ebench:tdforeach(
      fun(Class, BM, Data) ->
              io:format(D, "~s/~s",[Class, BM]),
              [io:format(D, " ~f ~f ~f",[eministat_ds:median(DS),
                                         eministat_ds:min(DS),
                                         eministat_ds:max(DS)])
               || {_, DS} <- Data],
              io:format(D, "~n", [])
      end, TagData, Titles),

    file:close(D),
    Cmd = ["gnuplot -e \""
           "data='", DataFile, "';"
           "out='", maps:get(output, Opts), "';"
           "tags=",integer_to_list(length(Titles)),
           "\" gnuplot_scripts/multitag_histo.gnuplot"],
    case os:cmd(Cmd) of
        [] ->
            io:format("plotted ~s~n", [maps:get(output, Opts)]);
        Output ->
            abort("~s~n~s~n",[Cmd, Output])
    end,
    file:delete(DataFile).
