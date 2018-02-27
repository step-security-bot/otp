-module(ebench_eministat).

%% API exports
-export([main/1]).

-import(ebench, [abort/1, abort/2]).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    {ok, Opts, Rest} = ebench:parse_and_check(Args, options(), fun usage/1),
    eministat(maps:merge(Opts, ebench:parse_tag_rest(Rest))).

%%====================================================================
%% Internal functions
%%====================================================================

options() ->
    ebench:benchmark_options().

usage(Opts) ->
    getopt:usage(Opts, "ebench eministat", "[Title[=Tag]...]"),
    io:format("Example:~n"
              "  ./ebench eministat -c small BASE BOUND=latest~n").

eministat(Opts = #{ tags := Tags }) ->
    ebench:tdforeach(
      fun(_Class, _BM, Data) ->
              [BaseDS | RestDS] = [DS || {_, DS} <- Data],
              eministat:x(95.0, BaseDS, RestDS)
      end, ebench:read_tags(Tags, ebench:benchmarks(Opts)), Opts).
