-module(singleton_docformat).

-export([main/0]).

-moduledoc #{format => "text/asciidoc",
             since => "1.0",
             deprecated => "Use something else"}.
-moduledoc "
Moduledoc test module
".


-doc "
Doc test module
".
main() ->
    ok.
