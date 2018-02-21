-module(ebench).

%% API exports
-export([main/1]).

-mode(compile).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    case getopt:parse(options(), Args) of
        {ok, {ArgOpts, Rest}} ->
            Opts = maps:merge(opts_from_list(ArgOpts), parse_rest(Rest)),
            run_benchmarks(Opts);
        Error ->
            io:format(standard_error, "~s~n",[getopt:format_error(options(), Error)]),
            getopt:usage(options(), ?MODULE_STRING, "Title=Command..."),
            io:format("Example:~n"
                      "  ./" ?MODULE_STRING " -c small BASE=erl \"BOUND=erl +sbtdb\"~n")
    end.

%%====================================================================
%% Internal functions
%%====================================================================

options() ->
    [
     {iterations, $i, "iter", {integer, 1}, "Number of iterations"},
     {class, $c, "class", {string, "all"}, "Which class of benchmarks to run"},
     {benchmark, $b, "benchmark", undefined, "Which benchmark to run"}
     {list, $l, "list", undefined, "List all benchmarks"}
    ].

opts_from_list(OptList) ->
    lists:foldl(fun({class = Key, Val}, M) ->
                        Classes = maps:get(Key, M, []),
                        M#{ Key => [Val | Classes]};
                   ({Key, Val}, M) ->
                        M#{ Key => Val }
                end, #{}, OptList).


parse_rest(Args) ->
    Commands = lists:map(
                 fun(Arg) ->
                         [Title | Cmd] = string:lexemes(Arg, "="),
                         {Title, Cmd}
                 end, Args),
    #{ commands => Commands }.

print(Term) ->
    io:format("~p~n",[Term]).

run_benchmarks(Opts) ->
    print(Opts),
    erlang:halt(0).
