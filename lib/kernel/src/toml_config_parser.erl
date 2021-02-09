%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2020. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(toml_config_parser).

%% External exports
-export([parse/2]).

-type anno_config_key() :: {atom(),erl_anno:anno()}.
-type anno_config_value() :: {term(),erl_anno:anno()}.
-type anno_config() :: #{ anno_config_key() => anno_config_value() | anno_config() } |
                         [{ anno_config_key(), anno_config_value() | anno_config() }].
-type anno_app_config() :: {AppName :: anno_config_key(),
                            Config :: anno_config()}.

-spec parse(Data :: unicode:chardata(), Anno :: erl_anno:anno()) ->
          {ok, AppConfig :: [anno_app_config()], MoreFiles :: [file:name()]} |
          {error, erl_anno:anno(), Reason :: unicode:chardata()}.
parse(Data, FName) ->
    case tomerl:parse(Data) of
        {ok, Config} ->
            {ok, munge_config(Config, erl_anno:set_file(FName, erl_anno:new(0)))};
        {error,{parse,Line}} ->
            {error,{Line, FName, "Could not parse toml config file"}}
    end.

munge_config(Config, Anno) ->
    maps:fold(
      fun(App, Keys, Acc) ->
              [{{binary_to_atom(App), Anno}, munge_application(App, Keys, Anno)} | Acc]
      end, [], Config).

munge_application(_App, KV, Anno) ->
    maps:fold(
      fun(Key, Val, Acc1) ->
              [{{binary_to_atom(Key), Anno}, munge_values(Val, Anno)} | Acc1]
      end, [], KV).

munge_values(Bin, Anno) when is_binary(Bin) ->
    {case unicode:characters_to_list(Bin, utf8) of
         [$@|Lst] ->
             case erl_scan:string(Lst ++ ".") of
                 {ok, Tokens, _} ->
                     {ok, Term} = erl_parse:parse_term(Tokens),
                     Term
             end;
         Lst ->
             case io_lib:quote_atom(dummy, Lst) of
                 true ->
                     Bin;
                 false ->
                     list_to_atom(Lst)
             end
     end, Anno};
munge_values(V, Anno) when is_map(V) ->
    maps:from_list(
      maps:fold(
        fun(Key, Val, Acc) ->
                [{{binary_to_atom(Key), Anno}, munge_values(Val, Anno)} | Acc]
        end, [], V));
munge_values(Vs, Anno) when is_list(Vs) ->
    [munge_values(V, Anno) || V <- Vs];
munge_values(V, Anno) ->
    {V, Anno}.
