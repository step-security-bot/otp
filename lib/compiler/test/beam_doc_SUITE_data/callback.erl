-module(callback).

%% -doc "
%% This should be ignored
%% ".
%% -behaviour(gen_server).

-export([all_ok/0, main/0]).

-doc "
Callback fn that always returns ok.
".
-callback all_ok() -> ok.

-doc "
Test changing order
".
-doc #{equiv => ok()}.
-callback change_order() -> ok.


-doc #{equiv => ok/0}.
-doc "
This is a test
".
all_ok() ->
    all_ok().

-doc #{equiv => main()}.
-spec main() -> ok.
-doc "
all_ok()

Calls all_ok/0
".
main() ->
    all_ok().

