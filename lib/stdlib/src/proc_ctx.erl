-module(proc_ctx).

-export([put/1, put/2, get/0, get/1, clear/0, clear/1]).
-compile({no_auto_import,[get/0]}).
-define(PDTAG,'$proc_ctx').

put(Ctx) when is_map(Ctx) ->
    erlang:put(?PDTAG, Ctx).
put(Key, Value) ->
    Old = case erlang:get(?PDTAG) of
              undefined ->
                  #{};
              Else ->
                  Else
          end,
    erlang:put(?PDTAG, Old#{ Key => Value }),
    ok.

get() ->
    erlang:get(?PDTAG).
get(Key) ->
    maps:get(Key, get()).

clear() ->
    erlang:erase(?PDTAG).
clear(Key) ->
    maps:remove(Key, erlang:get(?PDTAG)).
