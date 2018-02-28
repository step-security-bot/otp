-module(ebench_eministat).

%% API exports
-export([main/1, slogan/0]).

-import(ebench, [abort/1, abort/2]).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    {ok, Opts, Rest} = ebench:parse_and_check(Args, options(), fun usage/1),
    eministat(Opts, Rest).

slogan() ->
    "Run eministat:x/3 to compare benchmark runs".

%%====================================================================
%% Internal functions
%%====================================================================

options() ->
    [{confidence,$l,"confidence_level",{float, 95.0},
      "The confidence level to use"},
     {base, undefined, undefined, undefined,
      "The base benchmark file to compare with."}
     | ebench:benchmark_options()].

usage(Opts) ->
    getopt:usage(Opts, "ebench eministat", "<base> <compare...>"),
    io:format("Compare two benchmark runs using eministat:x/3. For more details~n"
              "on what the output from eministat:x/3 means see ~n"
              "https://github.com/jlouis/eministat#description-of-the-output~n"
              "~n"
              "Example:~n"
              "  ./ebench eministat BASE BOUND.term~n"
              "  ./ebench eministat -c small BASE BOUND-2063-04-05T11-05-34~n").

eministat(Opts = #{ base := Base, confidence := Conf }, Rest) ->
    ebench:tdforeach(
      fun(_Class, _BM, Data) ->
              [BaseDS | RestDS] = [DS || {_, DS} <- Data],
              eministat:x(Conf, BaseDS, RestDS)
      end, ebench:read_tags([Base | Rest], ebench:benchmarks(Opts)), [Base | Rest]).
