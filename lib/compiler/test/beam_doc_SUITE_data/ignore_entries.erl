-module(ignore_entries).

-doc "
IncludeLib shoud be ignored
".
-include_lib("common_test/include/ct.hrl").

-doc #{authors => "Feature Should be ignored"}.
-feature(maybe_expr, enable).


-export([main/0]).

-doc "
Import Should be ignored
".
-import(list, []).

-doc "
Behaviour Should be ignored
".
-behaviour(gen_server).


main() ->
    ok.
