%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023-2028. All Rights Reserved.
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
%% Purpose : Generate documentation as per EEP-48
%% Tool to convert an Erlang program in its abstract form to EEP-48 format.
%%
%% Example:
%%
%% 1> compile:file(test, [beam_docs]).
%%

-module(beam_doc).

-feature(maybe_expr, enable).

-export([main/2]).

-import(lists, [foldl/3]).

-include_lib("kernel/include/eep48.hrl").

-moduledoc false.

-doc "
Internal record used when transforming Erlang abstract syntax into EEP-48
documentation format.

- exported_functions are a filter to document only the exported functions
- exported_types are a filter to document only the exported types
- callbacks are always implicitly exported
".
-record(docs, {cwd                 :: unicode:chardata(),             % Cwd
               module_name = undefined  :: unicode:chardata() | undefined,
               exported_functions = sets:new() :: sets:set({atom(), non_neg_integer()}),
               exported_types     = sets:new() :: sets:set({atom(), non_neg_integer()}),
               export_all         = false :: boolean(),
               doc    = none  :: atom() | unicode:chardata(), % Function/type/callback local doc
               meta   = #{exported => false} :: map()}).      % Function/type/callback local meta

-type internal_docs() :: #docs{}.

-define(DEFAULT_MODULE_DOC_LOC, 1).
-define(DEFAULT_FORMAT, <<"text/markdown">>).

-doc "
Transforms an Erlang abstract syntax form into EEP-48 documentation format.
".
-spec main(term(), term()) -> #docs_v1{} | badarg.
main(Dirname, AST) ->
    io:format("~p~n~n", [AST]),
    try
        {ModuleDocAnno, ModuleDoc} = extract_moduledoc(AST),
        DocFormat = extract_docformat(AST),
        Docs = extract_documentation(AST, new_state(Dirname)),
        DocV1 = #docs_v1{},
        Meta = extract_meta(AST, DocV1#docs_v1.metadata),
        DocV1#docs_v1{ format = DocFormat,
                       anno = ModuleDocAnno,
                       metadata = Meta,
                       module_doc = create_module_doc(ModuleDoc),
                       docs = Docs }
    catch E:R:ST ->
            erlang:raise(E, R, ST)
    end.

extract_meta(AST, MetaData) ->
    foldl(fun ({attribute, _ModuleDocAnno, moduledoc, MetaDoc}, Meta) when is_map(MetaDoc) ->
                  maps:merge(Meta, MetaDoc);
              (_, Meta) -> Meta
          end, MetaData, AST).

-spec extract_moduledoc(AST :: [tuple()]) -> ModuleDoc :: {erl_anno:anno(), binary() | none | hidden}.
extract_moduledoc([]) ->
    {?DEFAULT_MODULE_DOC_LOC, none};
extract_moduledoc([{attribute, ModuleDocAnno, moduledoc, false}| _AST]) ->
    extract_moduledoc([{attribute, ModuleDocAnno, moduledoc, hidden}| _AST]);
extract_moduledoc([{attribute, ModuleDocAnno, moduledoc, hidden}| _AST]) ->
    {ModuleDocAnno, hidden};
extract_moduledoc([{attribute, ModuleDocAnno, moduledoc, ModuleDoc}| _AST]) when is_list(ModuleDoc) ->
    {ModuleDocAnno, unicode:characters_to_binary(string:trim(ModuleDoc))};
extract_moduledoc([_| AST]) ->
    extract_moduledoc(AST).

extract_docformat([]) ->
    ?DEFAULT_FORMAT;
extract_docformat([{attribute, _ModuleDocAnno, moduledoc, MetaFormat} | Ls]) when is_map(MetaFormat) ->
    case maps:get(format, MetaFormat, not_found) of
        not_found -> extract_docformat(Ls);
        Format when is_list(Format) -> unicode:characters_to_binary(Format);
        Format when is_binary(Format) -> Format
    end;
extract_docformat([_ | Ls]) ->
    extract_docformat(Ls).

-spec create_module_doc(ModuleDoc :: binary()) -> map().
create_module_doc(ModuleDoc) when is_atom(ModuleDoc) ->
    ModuleDoc;
create_module_doc(ModuleDoc) when not is_atom(ModuleDoc) ->
    create_module_doc(<<"en">>, ModuleDoc).

-spec create_module_doc(Lang :: binary(), ModuleDoc :: binary()) -> map().
create_module_doc(Lang, ModuleDoc) ->
    #{Lang => ModuleDoc}.

-spec new_state(Dirname :: unicode:chardata()) -> internal_docs().
new_state(Dirname) ->
    reset_state(#docs{cwd = Dirname}).

-spec reset_state(State :: internal_docs()) -> internal_docs().
reset_state(State) ->
    State#docs{doc = none, meta = #{exported => false}}.

-spec update_meta(State :: internal_docs(), Meta :: map()) -> internal_docs().
update_meta(#docs{meta = Meta0}=State, Meta1) ->
    State#docs{meta = maps:merge(Meta0, Meta1)}.

-spec update_doc(State :: internal_docs(), Doc :: unicode:chardata() | atom()) -> internal_docs().
update_doc(State, Doc) ->
    Meta = State#docs.meta,
    State#docs{doc = string:trim(Doc),
               meta = Meta#{exported := true}}.

-spec update_module(State :: internal_docs(), ModuleName :: unicode:chardata()) -> internal_docs().
update_module(#docs{}=State, ModuleName) ->
    State#docs{module_name = ModuleName}.

-spec update_export_funs(State :: internal_docs(), proplists:proplist()) -> internal_docs().
update_export_funs(State, ExportedFuns) ->
    ExportedFuns1 = sets:union(State#docs.exported_functions, sets:from_list(ExportedFuns)),
    State#docs{exported_functions = ExportedFuns1}.

-spec update_export_types(State :: internal_docs(), proplists:proplist()) -> internal_docs().
update_export_types(State, ExportedTypes) ->
    ExportedTypes1 = sets:union(State#docs.exported_types, sets:from_list(ExportedTypes)),
    State#docs{exported_types = ExportedTypes1}.

update_export_all(State, ExportAll) ->
    State#docs{ export_all = ExportAll }.

remove_exported_type_info(State) ->
    Meta = State#docs.meta,
    State#docs{meta = maps:remove(exported, Meta)}.

extract_documentation([{attribute,_ANNO,compile, export_all} | T]=_AST, State) ->
    extract_documentation(T, update_export_all(State, true));
extract_documentation([{attribute,_ANNO,export,ExportedFuns} | T]=_AST, State) ->
    extract_documentation(T, update_export_funs(State, ExportedFuns));
extract_documentation([{attribute,_ANNO,export_type,ExportedTypes} | T]=_AST, State) ->
    extract_documentation(T, update_export_types(State, ExportedTypes));
extract_documentation([{attribute, _Anno, file, {ModuleName, _A}} | T], State) ->
    extract_documentation(T, update_module(State, ModuleName));
extract_documentation([{attribute, _Anno, doc, Meta0}=_AST | T], State) when is_map(Meta0) ->
    extract_documentation(T, update_meta(State, Meta0));
extract_documentation([{attribute, _Anno, doc, Doc}=_AST | T], State) when Doc =:= hidden orelse Doc =:= false ->
    extract_documentation(T, State);
extract_documentation([{attribute, _Anno, doc, Doc}=_AST | T], State) when is_list(Doc) ->
    extract_documentation(T, update_doc(State, Doc));
extract_documentation([AST0 | _T]=AST,
                      #docs{meta = #{ equiv := {call,_,{atom,_,EquivF},Args}} = Meta}=State)
    when is_tuple(AST0) andalso (tuple_size(AST0) > 2 orelse tuple_size(AST0) < 6) ->
    Meta1 = Meta#{ equiv := {EquivF, length(Args)}},
    extract_documentation(AST, update_meta(State, Meta1));
extract_documentation([{function, Anno, F, A, [{clause, _, ClauseArgs, _, _}]}=_AST | T],
                      #docs{exported_functions = ExpFuns}=State) ->
    case sets:is_element({F, A}, ExpFuns) orelse State#docs.export_all of
        true ->
            State1 = remove_exported_type_info(State),
            FunDoc = template_gen_doc({function, Anno, F, A, ClauseArgs}, State1),
            [FunDoc | extract_documentation(T, reset_state(State))];
        false ->
            extract_documentation(T, reset_state(State))
    end;
extract_documentation([{function, Anno, F, A, _Body}=_AST | T],
                      #docs{doc = Doc, exported_functions=ExpFuns}=State) when Doc =/= none ->
    maybe
        true ?= sets:is_element({F, A}, ExpFuns) orelse State#docs.export_all,
        {Slogan, DocsWithoutSlogan} =
            %% First we check if there is a doc prototype
            case extract_slogan(Doc, F, A) of
                undefined -> {io_lib:format("~p/~p",[F,A]), Doc};
                SloganDocs -> SloganDocs
            end,
        AttrBody = {function, F, A},
        State1 = remove_exported_type_info(State),
        FunDoc = gen_doc(Anno, AttrBody, Slogan, DocsWithoutSlogan, State1),
        [FunDoc | extract_documentation(T, reset_state(State))]
    else
        _ -> extract_documentation(T, reset_state(State))
    end;
extract_documentation([{attribute, Anno, TypeOrOpaque, {Type, _, TypeArgs}}=_AST | T],
                      #docs{exported_types=ExpTypes}=State)
  when TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque ->
    Args = fun_to_varargs(TypeArgs),
    case sets:is_element({Type, length(Args)}, ExpTypes) of
        true ->
            State1 = State#docs{ meta = #{export => true}},
            FunDoc = template_gen_doc({type, Anno, Type, length(Args), Args}, State1),
            [FunDoc | extract_documentation(T, reset_state(State1))];
        false ->
            extract_documentation(T, reset_state(State))
    end;
extract_documentation([{attribute, Anno, callback, {{CB, A}, [Fun]}}=_AST | T], State) ->
    Args = fun_to_varargs(Fun),
    State1 = remove_exported_type_info(State),
    FunDoc = template_gen_doc({callback, Anno, CB, A, Args}, State1),
    [FunDoc | extract_documentation(T, reset_state(State))];
extract_documentation([_H|T], State) ->
    extract_documentation(T, State);
extract_documentation([], #docs{doc = none}) ->
    [].


-spec gen_doc(Anno, AttrBody, Slogan, Docs, State) -> Response when
      Anno      :: erl_anno:anno(),
      AttrBody  :: {function | type | callback, term(), integer()},
      Slogan    :: unicode:chardata(),
      Docs      :: none | hidden | unicode:chardata(),
      State     :: internal_docs(),
      Signature :: [binary()],
      D         :: map() | none | hidden,
      Meta      :: map(),
      Response  :: {AttrBody, Anno, Signature, D, Meta}.
gen_doc(Anno0, AttrBody, Slogan, Doc, #docs{meta = Meta, module_name = Module})
    when Doc =:= none orelse Doc =:= hidden ->
    Anno1 = erl_anno:set_file(Module, Anno0),
    {AttrBody, Anno1, [unicode:characters_to_binary(Slogan)], Doc, Meta};
gen_doc(Anno0, AttrBody, Slogan, Docs, #docs{meta = Meta, module_name = Module}) ->
    Anno1 = erl_anno:set_file(Module, Anno0),
    {AttrBody, Anno1, [unicode:characters_to_binary(Slogan)],
      #{ <<"en">> => unicode:characters_to_binary(string:trim(Docs)) }, Meta}.

template_gen_doc({Attr, Anno, F, A, _Args}, #docs{doc = Doc}=State) when Doc =:= none orelse Doc =:= hidden ->
    {Slogan, DocsWithoutSlogan} = {io_lib:format("~p/~p",[F,A]), Doc},
    AttrBody = {Attr, F, A},
    gen_doc(Anno, AttrBody, Slogan, DocsWithoutSlogan, State);
template_gen_doc({Attr, Anno, F, A, Args}, #docs{doc = Doc}=State) ->
    {Slogan, DocsWithoutSlogan} =
        case extract_slogan(Doc, F, A) of
            undefined ->
                maybe
                    true ?= lists:all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, Args),
                    {extract_slogan_from_args(F, Args), Doc}
                else
                    _E2 ->
                        {io_lib:format("~p/~p",[F,A]), Doc}
                end;
            SloganDocs ->
                SloganDocs
        end,
    AttrBody = {Attr, F, A},
    gen_doc(Anno, AttrBody, Slogan, DocsWithoutSlogan, State).

-spec fun_to_varargs(tuple() | term()) -> list(term()).
fun_to_varargs({type, _, bounded_fun, [T|_]}) ->
    fun_to_varargs(T);
fun_to_varargs({type, _, 'fun', [{type,_,product,Args}|_] }) ->
    lists:map(fun fun_to_varargs/1, Args);
fun_to_varargs({ann_type, _, [Name|_]}) ->
    Name;
fun_to_varargs({var,_,_} = Name) ->
    Name;
fun_to_varargs(Else) ->
    Else.

extract_slogan(none, _F, _A) -> undefined;
extract_slogan(Doc, F, A) ->
    maybe
        [MaybeSlogan | Rest] = string:split(Doc, "\n"),
        {ok, Toks, _} ?= erl_scan:string(unicode:characters_to_list([MaybeSlogan,"."])),
        {ok, [{call,_,{atom,_,F},Args}]} ?= erl_parse:parse_exprs(Toks),
        A ?= length(Args),
        {MaybeSlogan, Rest}
    else
        _ -> undefined
    end.

extract_slogan_from_args(F, Args) ->
    io_lib:format("~p(~ts)",[F, lists:join(", ",[string:trim(atom_to_list(Arg),leading,"_") || {var, _, Arg} <- Args])]).
