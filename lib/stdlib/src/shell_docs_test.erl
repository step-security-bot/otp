%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-module(shell_docs_test).
-moduledoc false.

-include_lib("kernel/include/eep48.hrl").

-export([module/1]).

module(#docs_v1{ docs = Docs }) ->
    case lists:flatten(
           [[kind(KFA, Lang, D) || Lang := D <- EntryDocs]
            || {KFA, _Anno, _Sig, EntryDocs, _Meta} <- Docs, is_map(EntryDocs)]) of
        [] ->
            ok;
        Else ->
            Else
    end.

kind({function, _append, _1} = KFA, Lang, Docs) ->
    io:format("Testing: ~p~n",[KFA]),
    case test(inspect(shell_docs_markdown:parse_md(Docs))) of
        [] -> [];
        Else ->
            {KFA, lists:flatten(Else)}
    end;
kind(KFA, _Lang, _Docs) ->
    io:format("Skipping: ~p~n",[KFA]),
    [].
    
test({pre,[],[{code,[{class,~"language-erlang"}],[<<">",_/binary>> = Code]}]}) ->
    run_test(Code);
test({_Tag,_Attr, Content}) ->
    test(Content);
test([H | T]) ->
    [test(H) | test(T)];
test(Text) when is_binary(Text); Text =:= [] ->
    [].

run_test(Code) ->
    Lines = string:split(Code, "\n", all),
    Tests = inspect(parse_tests(Lines, [])),
    lists:foldl(fun(Test, Bindings) ->
                        run_tests(Test, Bindings)
                end, erl_eval:new_bindings(), Tests).

parse_tests([], []) ->
    [];
parse_tests([<<>>], []) ->
    [];
parse_tests([<<"%", _Skip>> | T], Cmd) ->
    parse_tests(T, Cmd);
parse_tests([<<"> ", NewCmd/binary>> | T], _) ->
    parse_tests(T, [NewCmd]);
parse_tests([<<"  ", More/binary>> | T], Acc) ->
    parse_tests(T, [More | Acc]);
parse_tests([NewMatch | T], Cmd) ->
    {Match, Rest} = parse_match(T, [NewMatch]),
    [{test, lists:join($\n, lists:reverse(Cmd)),
      lists:join($\n, lists:reverse(Match))} | parse_tests(Rest, [])].

parse_match([<<"%", _Skip>> | T], Acc) ->
    parse_match(T, Acc);
parse_match([<<" ", More>> | T], Acc) ->
    parse_match(T, [More | Acc]);
parse_match(Rest, Acc) ->
    {Acc, Rest}.

run_tests({test, Test, Match}, Bindings) ->
    maybe
        Cmd = lists:flatten([unicode:characters_to_list(Match),
                             "=",
                             unicode:characters_to_list(Test)]),
        {ok, T, _} ?= erl_scan:string(Cmd),
        {ok, Ast} ?= erl_parse:parse_exprs(T),
        {value, _Res, NewBindings} ?= erl_eval:exprs(Ast, Bindings),
        NewBindings
    else
        E -> throw(E)
    end.

inspect(Term) ->
    io:format("~tp~n",[Term]),
    Term.
