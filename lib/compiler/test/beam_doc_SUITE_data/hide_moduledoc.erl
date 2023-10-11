-module(hide_moduledoc).

-export([main/0]).

-moduledoc false.

-doc "
Doc test module
".
main() ->
    ok.
