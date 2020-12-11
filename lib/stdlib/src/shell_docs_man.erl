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
-module(shell_docs_man).

-export([render_docs/2]).

-include_lib("kernel/include/eep48.hrl").
-include("shell_docs.hrl").

%%% General rendering functions
render_docs(DocContents, #config{} = Config) ->
    render_docs(DocContents, 0, Config).
render_docs(DocContents, Ind, D = #config{}) when is_integer(Ind) ->
    init_ansi(D),
    try
        {Doc,_} = trimnl(render_docs(DocContents, [], 0, Ind, D)),
        Doc
    after
        clean_ansi()
    end.

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
%%% The indents are done either by render_words when a newline happens
%%% or when a new element is to be rendered and Pos < Ind.
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

render_element({IgnoreMe,_,Content}, State, Pos, Ind,D)
  when IgnoreMe =:= a ->
    render_docs(Content, State, Pos, Ind,D);

%% Catch h1, h2 and h3 before the padding is done as they reset padding
render_element({h1,_,Content},State,0 = Pos,_Ind,D) ->
    trimnlnl(render_element({code,[],[{em,[],Content}]}, State, Pos, 0, D));
render_element({h2,_,Content},State,0 = Pos,_Ind,D) ->
    trimnlnl(render_element({em,[],Content}, State, Pos, 0, D));
render_element({h3,_,Content},State,Pos,_Ind,D) when Pos =< 2 ->
    trimnlnl(render_element({code,[],Content}, State, Pos, 2, D));

render_element({pre,_Attr,_Content} = E,State,Pos,Ind,D) when Pos > Ind ->
    %% We pad `pre` with two newlines if the previous section did not indent the region.
    {Docs,NewPos} = render_element(E,State,0,Ind,D),
    {["\n\n",Docs],NewPos};
render_element({Elem,_Attr,_Content} = E,State,Pos,Ind,D) when Pos > Ind, ?IS_BLOCK(Elem) ->
    {Docs,NewPos} = render_element(E,State,0,Ind,D),
    {["\n",Docs],NewPos};
render_element({'div',[{class,What}],Content},State,Pos,Ind,D) ->
    {Docs,_} = render_docs(Content, ['div'|State], 0, Ind+2, D),
    trimnlnl([pad(Ind - Pos),string:titlecase(What),":\n",Docs]);
render_element({Tag,_,Content},State,Pos,Ind,D) when Tag =:= p; Tag =:= 'div' ->
    trimnlnl(render_docs(Content, [Tag|State], Pos, Ind, D));

render_element(Elem,State,Pos,Ind,D) when Pos < Ind ->
    {Docs,NewPos} = render_element(Elem,State,Ind,Ind,D),
    {[pad(Ind - Pos), Docs],NewPos};

render_element({code,_,Content},[pre|_]  = State,Pos,Ind,D) ->
    %% When code is within a pre we don't emit any underline
    render_docs(Content, [code|State], Pos, Ind,D);
render_element({code,_,Content},State,Pos,Ind,D) ->
    Underline = sansi(underline),
    {Docs, NewPos} = render_docs(Content, [code|State], Pos, Ind,D),
    {[Underline,Docs,ransi(underline)], NewPos};

render_element({i,_,Content},State,Pos,Ind,D) ->
    %% Just ignore i as ansi does not have cursive style
    render_docs(Content, State, Pos, Ind,D);

render_element({br,[],[]},_State,Pos,_Ind,_D) ->
    {"",Pos};

render_element({em,_,Content},State,Pos,Ind,D) ->
    Bold = sansi(bold),
    {Docs, NewPos} = render_docs(Content, State, Pos, Ind,D),
    {[Bold,Docs,ransi(bold)], NewPos};

render_element({pre,_,Content},State,Pos,Ind,D) ->
    %% For pre we make sure to respect the newlines in pre
    trimnlnl(render_docs(Content, [pre|State], Pos, Ind+2, D));

render_element({ul,[{class,<<"types">>}],Content},State,_Pos,Ind,D) ->
    {Docs, _} = render_docs(Content, [types|State], 0, Ind+2, D),
    trimnlnl(["Types:\n", Docs]);
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
    render_docs(Content, [l|State], Pos, Ind,D);
render_element({ol,[],Content},State,Pos,Ind,D) ->
    %% For now ul and ol does the same thing
    render_docs(Content, [l|State], Pos, Ind,D);
render_element({li,[],Content},[l | _] = State, Pos, Ind,D) ->
    Bullet = get_bullet(State, D#config.encoding),
    BulletLen = string:length(Bullet),
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos + BulletLen,Ind + BulletLen, D),
    trimnlnl([Bullet,Docs]);

render_element({dl,_,Content},State,Pos,Ind,D) ->
    render_docs(Content, [dl|State], Pos, Ind,D);
render_element({dt,_,Content},[dl | _] = State,Pos,Ind,D) ->
    Underline = sansi(underline),
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos, Ind, D),
    {[Underline,Docs,ransi(underline),":","\n"], 0};
render_element({dd,_,Content},[dl | _] = State,Pos,Ind,D) ->
    trimnlnl(render_docs(Content, [li | State], Pos, Ind + 2, D));

render_element(B, State, Pos, Ind,#config{ columns = Cols }) when is_binary(B) ->
    case lists:member(pre,State) of
        true ->
            Pre = string:replace(B,"\n",[nlpad(Ind)],all),
            {Pre, Pos + lastline(Pre)};
        _ ->
            render_words(split_to_words(B),State,Pos,Ind,[[]],Cols)
    end;

render_element({Tag,Attr,Content}, State, Pos, Ind,D) ->
    case lists:member(Tag,?ALL_ELEMENTS) of
        true ->
            throw({unhandled_element,Tag,Attr,Content});
        false ->
            %% We ignore tags that we do not care about
            ok
    end,
    render_docs(Content, State, Pos, Ind,D).

render_words(Words,[_,types|State],Pos,Ind,Acc,Cols) ->
    %% When we render words and are in the types->type state we indent
    %% the extra lines two additional spaces to make it look nice
    render_words(Words,State,Pos,Ind+2,Acc,Cols);
render_words([Word|T],State,Pos,Ind,Acc,Cols) when is_binary(Word) ->
    WordLength = string:length(Word),
    NewPos = WordLength + Pos,
    %% We do not want to add a newline if this word is only a punctuation
    IsPunct = is_tuple(re:run(Word,"^\\W$",[unicode])),
    if
        NewPos > (Cols - 10 - Ind), Word =/= <<>>, not IsPunct ->
            %% Word does not fit, time to add a newline and also pad to Indent level
            render_words(T,State,WordLength+Ind+1,Ind,[[[nlpad(Ind), Word]]|Acc],Cols);
        true ->
            %% Word does fit on line
            [Line | LineAcc] = Acc,
            %% Add + 1 to length for space
            NewPosSpc = NewPos+1,
            render_words(T,State,NewPosSpc,Ind,[[Word|Line]|LineAcc],Cols)
    end;
render_words([],_State,Pos,_Ind,Acc,_Cols) ->
    Lines = lists:map(fun(RevLine) ->
                            Line = lists:reverse(RevLine),
                            lists:join($ ,Line)
                      end,lists:reverse(Acc)),
    {iolist_to_binary(Lines), Pos}.

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
    Pad = lists:duplicate(N," "),
    case ansi() of
        undefined ->
            [Extra, Pad];
        Ansi ->
            ["\033[0m",Extra,Pad,Ansi]
    end.

get_bullet(_State,latin1) ->
    <<" * ">>;
get_bullet(State,unicode) ->
    %% Fancy bullet point logic!
    case length([l || l <- State]) of
        Level when Level > 4 ->
            get_bullet(State, latin1);
        Level ->
            lists:nth(Level,
                      [<<" • "/utf8>>,<<" ￮ "/utf8>>,
                       <<" ◼ "/utf8>>,<<" ◻ "/utf8>>])
    end.

%% Look for the length of the last line of a string
lastline(Str) ->
    LastStr = case string:find(Str,"\n",trailing) of
                  nomatch ->
                      Str;
                  Match ->
                      tl(string:next_codepoint(Match))
              end,
    string:length(LastStr).

split_to_words(B) ->
    binary:split(B,[<<" ">>],[global]).

%% These functions make sure that we trim extra newlines added
%% by the renderer. For example if we do <li><p></p></li>
%% that would add 4 \n at after the last </li>. This is trimmed
%% here to only be 2 \n
trimnlnl({Chars, _Pos}) ->
    nl(nl(string:trim(Chars, trailing, "\n")));
trimnlnl(Chars) ->
    nl(nl(string:trim(Chars, trailing, "\n"))).
trimnl({Chars, _Pos}) ->
    nl(string:trim(Chars, trailing, "\n")).
nl({Chars, _Pos}) ->
    nl(Chars);
nl(Chars) ->
    {[Chars,"\n"],0}.

%% We keep the current ansi state in the pdict so that we know
%% what to disable and enable when doing padding
init_ansi(#config{ ansi = undefined, io_opts = Opts }) ->
    %% We use this as our heuristic to see if we should print ansi or not
    case {application:get_env(kernel, shell_docs_ansi),
          proplists:is_defined(echo, Opts) andalso
          proplists:is_defined(expand_fun, Opts),
          os:type()} of
        {{ok,false}, _, _} ->
            put(ansi, noansi);
        {{ok,true}, _, _} ->
            put(ansi, []);
        {_, _, {win32,_}} ->
            put(ansi, noansi);
        {_, true,_} ->
            put(ansi, []);
        {_, false,_} ->
            put(ansi, noansi)
    end;
init_ansi(#config{ ansi = true }) ->
    put(ansi, []);
init_ansi(#config{ ansi = false }) ->
    put(ansi, noansi).



clean_ansi() ->
    case get(ansi) of
        [] -> erase(ansi);
        noansi -> erase(ansi)
    end,
    ok.

%% Set ansi
sansi(Type) -> sansi(Type, get(ansi)).
sansi(_Type, noansi) ->
    [];
sansi(Type, Curr) ->
    put(ansi,[Type | Curr]),
    ansi(get(ansi)).

%% Clear ansi
ransi(Type) -> ransi(Type, get(ansi)).
ransi(_Type, noansi) ->
    [];
ransi(Type, Curr) ->
    put(ansi,proplists:delete(Type,Curr)),
    case ansi(get(ansi)) of
        undefined ->
            "\033[0m";
        Ansi ->
            Ansi
    end.

ansi() -> ansi(get(ansi)).
ansi(noansi) -> undefined;
ansi(Curr) ->
    case lists:usort(Curr) of
        [] ->
            undefined;
        [bold] ->
            "\033[;1m";
        [underline] ->
            "\033[;;4m";
        [bold,underline] ->
            "\033[;1;4m"
    end.
