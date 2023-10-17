-module(slogan).

-export([main/0, bar/0, no_slogan/1]).

-doc "
main()

Returns ok.
".
main() ->
    ok.

-doc "
foo()
Not a slogan since foo =/= bar
".
bar() ->
    ok.

-doc "
Not a slogan

Tests slogans in multi-clause
".
no_slogan(X) when is_atom(X) ->
    X;
no_slogan(_X) ->
    ok.
