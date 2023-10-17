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


-doc #{equiv => ok/0}.
-doc "
This is a test
".
all_ok() ->
    ok.

-spec main() -> ok.
-doc "
all_ok()

Calls all_ok/0
".
main() ->
    all_ok().


