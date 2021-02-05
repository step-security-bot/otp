-module(proc_ctx).

-export([put/1, put/2, put/3, update/2, get/0, get/1,
         clear/0, clear/1, process/1]).
-compile({no_auto_import,[get/0, get/1]}).
-define(PDTAG,'$proc_ctx').

put(Ctx) when is_map(Ctx) ->
    erlang:put(?PDTAG, Ctx).
put(Key, Value) ->
    put(Key, Value, fun(_Key, Val, _What) -> Val end).
put(Key, Value, Fun) ->
    Old = case erlang:get(?PDTAG) of
              undefined ->
                  #{};
              Else ->
                  Else
          end,
    erlang:put(?PDTAG, Old#{ Key => {Value, Fun} }).

update(Key, Value) ->
    {_, Fun} = get(Key),
    put(Key, Value, Fun).

get() ->
    erlang:get(?PDTAG).
get(Key) ->
    maps:get(Key, get()).

clear() ->
    erlang:erase(?PDTAG).
clear(Key) ->
    maps:remove(Key, erlang:get(?PDTAG)).

process(What) ->
    maps:map(
      fun(Key, {Value, Fun}) ->
              {Fun(Key, Value, What), Fun}
      end, get()).
