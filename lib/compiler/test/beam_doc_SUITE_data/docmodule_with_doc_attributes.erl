-module(docmodule_with_doc_attributes).

-export([main/0]).

-moduledoc "
Moduledoc test module
".


-doc "
Doc test module
".
main() ->
    ok().


-doc #{authors => "Someone"}.
ok() ->
     no_docs().

no_docs() ->
    ok.
