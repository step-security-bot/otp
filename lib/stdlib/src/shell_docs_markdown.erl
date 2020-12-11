%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(shell_docs_markdown).

-export([render_docs/2]).

-include_lib("kernel/include/eep48.hrl").
-include("shell_docs.hrl").

%%% General rendering functions
render_docs(DocContents, #config{} = D) ->
    {Doc,_} = trimnl(render_docs(DocContents, [], 0, 0, D)),
    Doc.

render_docs(Elems,State,Pos,Ind,D) when is_list(Elems) ->
    lists:mapfoldl(fun(Elem,P) ->
%%%                           io:format("Elem: ~p (~p) (~p,~p)~n",[Elem,State,P,Ind]),
                           render_docs(Elem,State,P,Ind,D)
                   end,Pos,Elems);
render_docs(Elem,State,Pos,Ind,D) ->
    render_element(Elem,State,Pos,Ind,D).

%%% The function is the main element rendering function
%%%
%%% Elem: The current element to process
%%% Stack: A stack of element names to see where we are in the dom
%%% Pos: The current print position on the current line
%%% Ind: How much the text should be indented after a newline
%%% Config: The renderer's configuration
%%%
%%% Each element is responsible for putting new lines AFTER itself
%%% The indents are done when a new element is to be rendered and Pos < Ind.
%%%
%%% Any block elements (i.e. p, ul, li etc) are responsible for trimming
%%% extra new lines. eg. <ul><li><p>content</p></li></ul> should only
%%% have two newlines at the end.
-spec render_element(Elem :: shell_docs:chunk_element(),
                     Stack :: [shell_docs:chunk_element_type()],
                     Pos :: non_neg_integer(),
                     Indent :: non_neg_integer(),
                     Config :: #config{}) ->
          {unicode:chardata(), Pos :: non_neg_integer()}.

%% Catch h1, h2 and h3 before the padding is done as they reset padding
render_element({h1,_,Content},State,0 = Pos,_Ind,D) ->
    {Docs, NewPos} = render_docs(Content,State,Pos,0,D),
    trimnlnl({["# ", Docs], NewPos});
render_element({hsig,_,Content},State,0 = Pos,_Ind,D) ->
    {Docs, NewPos} = render_docs(Content,State,Pos,0,D),
    trimnlnl({["## ", Docs], NewPos});
render_element({h2,_,Content},State,0 = Pos,_Ind,D) ->
    {Docs, NewPos} = render_docs(Content,State,Pos,0,D),
    trimnlnl({["## ", Docs], NewPos});
render_element({h3,_,Content},State,Pos,_Ind,D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content,State,Pos,0,D),
    trimnlnl({["### ", Docs], NewPos});

render_element({Elem,_Attr,_Content} = E,State,Pos,Ind,D) when Pos > Ind, ?IS_BLOCK(Elem) ->
    {Docs,NewPos} = render_element(E,State,0,Ind,D),
    {["\n",Docs],NewPos};
render_element({'div',[{class,What}],Content},State,Pos,Ind,D) ->
    Title = unicode:characters_to_binary([string:titlecase(What),":"]),
    {Header, 0} = render_element({h3,[],[Title]},State,Pos,Ind,D),
    {Docs, 0} = render_element({'div',[],Content},['div'|State], 0, Ind+2, D),
    {[Header,Docs],0};
render_element({Tag,_,Content},State,Pos,Ind,D) when Tag =:= p; Tag =:= 'div' ->
    trimnlnl(render_docs(Content, [Tag|State], Pos, Ind, D));

render_element(Elem,State,Pos,Ind,D) when Pos < Ind ->
    {Docs,NewPos} = render_element(Elem,State,Ind,Ind,D),
    {[pad(Ind - Pos), Docs],NewPos};

render_element({a,Attr,Content}, State, Pos, Ind,D) ->
    {Docs, NewPos} = render_docs(Content, State, Pos, Ind, D),
    case proplists:get_value(rel,Attr) of
        <<"https://erlang.org/doc/link/seemfa">> ->
            Href = proplists:get_value(href, Attr),
            [_App, MFA] = string:split(Href,":"),
            [Mod, FA] = string:split(MFA,"#"),
            [Func, Arity] = string:split(FA,"/"),
            {["[",Docs,"](https://erlang.org/doc/man/",Mod,".html#",Func,"-",Arity,")"],NewPos};
        <<"https://erlang.org/doc/link/seetype">> ->
            Href = proplists:get_value(href, Attr),
            case string:lexemes(Href,":#/") of
                [_App, Mod, Type, Arity] ->
                    {["[",Docs,"](https://erlang.org/doc/man/",Mod,".html#","type-",Type,"-",Arity,")"],NewPos};
                [_App, Mod, Type] ->
                    {["[",Docs,"](https://erlang.org/doc/man/",Mod,".html#","type-",Type,")"],NewPos}
            end;
        <<"https://erlang.org/doc/link/seeerl">> ->
            Href = proplists:get_value(href, Attr),
            [_App, Mod|Anchor] = string:lexemes(Href,":#"),
            {["[",Docs,"](https://erlang.org/doc/man/",Mod,".html#",Anchor,")"],NewPos};
        _ ->
            {Docs,NewPos}
    end;

render_element({code,_,Content},[pre|_]  = State,Pos,Ind,D) ->
    %% When code is within a pre we don't emit any underline
    render_docs(Content, [code|State], Pos, Ind,D);
render_element({code,_,Content},State,Pos,Ind,D) ->
    {Docs, NewPos} = render_docs(Content, [code|State], Pos, Ind,D),
    {["`",Docs,"`"], NewPos};

render_element({i,_,Content},State,Pos,Ind,D) ->
    {Docs, NewPos} = render_docs(Content, [i|State], Pos, Ind,D),
    case lists:member(pre,State) of
        true ->
            {[Docs], NewPos};
        false ->
            {["*",Docs,"*"], NewPos}
    end;

render_element({br,[],[]},_State,Pos,_Ind,_D) ->
    {"",Pos};

render_element({em,_,Content},State,Pos,Ind,D) ->
    {Docs, NewPos} = render_docs(Content, State, Pos, Ind,D),
    case lists:member(pre,State) of
        true ->
            {[Docs], NewPos};
        false ->
            {["**",Docs,"**"], NewPos}
    end;

render_element({pre,_,Content},State,Pos,Ind,D) ->
    %% For pre we make sure to respect the newlines in pre
    {Docs, _} = trimnl(render_docs(Content, [pre|State], Pos, Ind, D)),
    {["```erlang\n",Docs,"```\n\n"], 0};

render_element({ul,[{class,<<"types">>}],Content},State,_Pos,Ind,D) ->
    {Docs, _} = render_docs(Content, [types|State], 0, Ind, D),
    trimnlnl(["```erlang\n", Docs,"```"]);
render_element({li,Attr,Content},[types|_] = State,Pos,Ind,C) ->
    Doc =
        case {proplists:get_value(name, Attr),proplists:get_value(class, Attr)} of
            {undefined,Class} when Class =:= undefined; Class =:= <<"type">> ->
                %% Inline html for types
                render_docs(Content,[type|State],Pos,Ind,C);
            {_,<<"description">>} ->
                %% Inline html for type descriptions
                render_docs(Content,[type|State],Pos,Ind+2,C);
            {Name,_} ->
                %% Try to render from type metadata
                case render_type_signature(binary_to_atom(Name),C) of
                    undefined when Content =:= [] ->
                        %% Failed and no content, emit place-holder
                        {["-type ",Name,"() :: term()."],0};
                    undefined ->
                        %% Failed with metadata, render the content
                        render_docs(Content,[type|State],Pos,Ind,C);
                    Type ->
                        %% Emit the erl_pp typespec
                        {Type,0}
                end
        end,
    trimnl(Doc);
render_element({ul,[],Content},State,Pos,Ind,D) ->
    render_docs(Content, [ul|State], Pos, Ind,D);
render_element({ol,[],Content},State,Pos,Ind,D) ->
    render_docs(Content, [ol|State], Pos, Ind,D);
render_element({li,[],Content},[ul | _] = State, Pos, Ind,D) ->
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos + 2,Ind + 2, D),
    trimnl(["* ",Docs]);
render_element({li,[],Content},[ol | _] = State, Pos, Ind,D) ->
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos + 2,Ind + 2, D),
    trimnl(["1. ", Docs]);

render_element({dl,_,Content},State,Pos,Ind,D) ->
    trimnlnl(render_docs(Content, [dl|State], Pos, Ind,D));
render_element({dt,_,Content},[dl | _] = State,Pos,Ind,D) ->
    {Docs, NewPos} = trimnl(render_docs([{em,[],Content}], [li | State], Pos + 2, Ind + 2, D)),
    {["* ", Docs, ""], NewPos};
render_element({dd,_,Content},[dl | _] = State,Pos,Ind,D) ->
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos+2, Ind + 2, D),
    trimnl([pad(2 + Ind - Pos), Docs]);

render_element(B, _State, Pos, Ind, #config{ }) when is_binary(B) ->
    Pre = string:replace(B,"\n",[nlpad(Ind)],all),
    {Pre, Pos + lastline(Pre)};

render_element({Tag,Attr,Content}, State, Pos, Ind,D) ->
    case lists:member(Tag,?ALL_ELEMENTS) of
        true ->
            throw({unhandled_element,Tag,Attr,Content});
        false ->
            %% We ignore tags that we do not care about
            ok
    end,
    render_docs(Content, State, Pos, Ind,D).

render_type_signature(Name, #config{ docs = #docs_v1{ metadata = #{ types := AllTypes }}}) ->
    case [Type || Type = {TName,_} <- maps:keys(AllTypes), TName =:= Name] of
        [] ->
            undefined;
        Types ->
            [erl_pp:attribute(maps:get(Type, AllTypes)) || Type <- Types]
    end.

%% Pad N spaces (and possibly pre-prend newline), disabling any ansi formatting while doing so.
pad(N) ->
    pad(N,"").
nlpad(N) ->
    %% It is important that we disable the ansi code before the new-line as otherwise the
    %% ansi decoration may be enabled when c:paged_output tries to ask if more content
    %% should be displayed.
    pad(N,"\n").
pad(N, Extra) ->
    [Extra, lists:duplicate(N," ")].

%% Look for the length of the last line of a string
lastline(Str) ->
    LastStr = case string:find(Str,"\n",trailing) of
                  nomatch ->
                      Str;
                  Match ->
                      tl(string:next_codepoint(Match))
              end,
    string:length(LastStr).

%% These functions make sure that we trim extra newlines added
%% by the renderer. For example if we do <li><p></p></li>
%% that would add 4 \n at after the last </li>. This is trimmed
%% here to only be 2 \n
trimnlnl({Chars, _Pos}) ->
    trimnlnl(Chars);
trimnlnl(Chars) ->
    nl(nl(string:trim(Chars, trailing, "\n"))).
trimnl({Chars, _Pos}) ->
    trimnl(Chars);
trimnl(Chars) ->
    nl(string:trim(Chars, trailing, "\n")).
nl({Chars, _Pos}) ->
    nl(Chars);
nl(Chars) ->
    {[Chars,"\n"],0}.
