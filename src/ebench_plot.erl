-module(ebench_plot).

%% API exports
-export([main/1]).

-import(ebench, [abort/1, abort/2]).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    {ok, Opts, Rest} = ebench:parse_and_check(Args, options(), fun usage/1),
    plot(maps:merge(Opts, ebench:parse_tag_rest(Rest))).

%%====================================================================
%% Internal functions
%%====================================================================

options() ->
    [{type, undefined, "type", {string,"histogram"},
      "What type of plot should be done"},
     {output, $o, "output", {string,"benchmark.svg"},
      "The file to output the analysis to"} |
     ebench:benchmark_options()].

usage(Opts) ->
    getopt:usage(Opts, "ebench plot", "[Title[=Tag]...]"),
    io:format("Example:~n"
              "  ./ebench plot -c small BASE BOUND=latest~n").

plot(Opts = #{ tags := Tags, type := Type }) ->
    TagData = ebench:read_tags(Tags, ebench:benchmarks(Opts)),
    plot(Type, TagData, Opts).

plot("histogram", TagData, Opts) ->
    [abort("Could not find gnuplot program") || os:find_executable("gnuplot") =:= false],

    DataFile = string:trim(os:cmd("mktemp")),
    {ok, D} = file:open(DataFile, [write]),

    io:format(D, "Benchmarks", []),
    [io:format(D, " ~s ~s-min ~s-max", [Title, Title, Title]) || {Title, _} <- maps:get(tags,Opts)],
    io:format(D, "~n", []),

    ebench:tdforeach(
      fun(Class, BM, Data) ->
              io:format(D, "~s/~s",[Class, BM]),
              [io:format(D, " ~f ~f ~f",[eministat_ds:median(DS),
                                         eministat_ds:min(DS),
                                         eministat_ds:max(DS)])
               || {_, DS} <- Data],
              io:format(D, "~n", [])
      end, TagData, Opts),

    file:close(D),
    Cmd = ["gnuplot -e \""
           "data='", DataFile, "';"
           "out='", maps:get(output, Opts), "';"
           "tags=",integer_to_list(length(maps:get(tags,Opts))),
           "\" gnuplot_scripts/multitag_histo.gnuplot"],
    case os:cmd(Cmd) of
        [] ->
            ok;
        Output ->
            abort("~s~n~s~n",[Cmd, Output])
    end,
    file:delete(DataFile).
