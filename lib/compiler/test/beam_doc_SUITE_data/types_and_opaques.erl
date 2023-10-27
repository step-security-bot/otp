-module(types_and_opaques).

-export_type([name/1,unnamed/0, mmaybe/1]).
-export_type([natural_number/0, param/1]).
-export_type([hidden_false/0, hidden/0]).

-doc "
name(_)

Represents the name of a person.
".
-type name(_Ignored) :: string().



-doc #{since => "1.0"}.
-doc #{equiv => non_neg_integer/0}.
-type natural_number() :: non_neg_integer().

-doc "
Tests generics
".
-doc #{equiv => madeup()}.
-type param(X) :: {X, integer(), Y :: string()}.

-doc #{equiv => non_neg_integer()}.
-doc "
unnamed()

Represents the name of a person that cannot be named.
".
-opaque unnamed() :: name(integer()).

-doc #{ authors => "Someone else" }.
-doc "
mmaybe(X) ::= nothing | X.

Represents a maybe type.
".
-opaque mmaybe(X) :: nothing | X.

-opaque non_exported() :: atom().

-type not_exported_either() :: atom().

-doc hidden.
-doc #{ authors => "Someone else" }.
-type hidden_false() :: atom().

-doc false.
-doc "
Here is ok.
".
-type hidden() :: hidden_false().
