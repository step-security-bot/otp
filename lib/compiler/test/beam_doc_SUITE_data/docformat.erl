-module(docformat).

-export([main/0]).

-moduledoc #{format => "text/asciidoc"}.
-moduledoc #{since => "1.0"}.
-moduledoc #{deprecated => "Use something else"}.
-moduledoc "
Moduledoc test module
".


-doc "
Doc test module
".
main() ->
    ok.
