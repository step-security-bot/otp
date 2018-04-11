-module(ebench_list).

%% API exports
-export([main/1, slogan/0]).

-import(ebench, [abort/1, abort/2]).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    {ok, Opts, _Rest} = ebench:parse_and_check(Args, options(), fun usage/1),
    [begin
         io:format("Class: ~s~n",[Class]),
         [io:format("  ~s~n",[ebench:benchmark2str(BM)]) || BM <- BMs]
     end || {Class, BMs} <- ebench:benchmarks(Opts), BMs =/= []].

slogan() ->
    "List all available benchmarks".

%%====================================================================
%% Internal functions
%%====================================================================

options() ->
    ebench:benchmark_options().

usage(Opts) ->
    getopt:usage(Opts, "ebench list", ""),
    io:format("List classes and their benchmarks according to the filters.~n"
              "The -c and -b options can be used to filter which benchmarks~n"
              "should be selected. Multiple -c and -b options are allowed to~n"
              "to be passed to ebench at the same time.~n"
              "The -c filter works as a match on the exact same of the class.~n"
              "The -b filter is a regexp matching the full name of the benchmark.~n"
              "~n"
              "Example:~n"
              "  ./ebench list -c dons~n"
              "  Class: dons~n"
              "    mean~n"
              "    sum~n"
              "    zip~n"
              "    zip3v~n"
              "~n"
              "  ./ebench list -c dons -b mean~n"
              "  Class: dons~n"
              "    mean~n"
              "~n"
              "  ./ebench list -b mean -b sum~n"
              "  Class: dons~n"
              "    mean~n"
              "    sum~n"
              "~n"
             ).
