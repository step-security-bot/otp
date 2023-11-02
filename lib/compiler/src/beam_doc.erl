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

               %% tracks exported functions from multiple `-export([...])`
               exported_functions = sets:new() :: sets:set({atom(), non_neg_integer()}),

               %% tracks exported type from multiple `-export_type([...])`
               exported_types     = sets:new() :: sets:set({atom(), non_neg_integer()}),

               % keeps track of `-compile(export_all)`
               export_all         = false :: boolean(),

               % Function/type/callback local doc. either none of some string was added
               doc    = none  :: none | unicode:chardata(),

               %% track if the doc was never added (none), marked hidden (-doc hidden)
               %% or entered (-doc "..."). If entered, doc_status = set, and doc = "...".
               doc_status = none :: none  | hidden | set,

               % Function/type/callback local meta.
               %% exported => boolean(), keeps track of types that are private but used in public functions
               %% thus, they must be considered as exported for documentation purposes.
               meta   = #{exported => false} :: map(),

               %% Result AST
               ast_fns = [] :: list(),
               ast_types = [] :: list(),
               ast_callbacks = [] :: list()}).

-type internal_docs() :: #docs{}.

-define(DEFAULT_MODULE_DOC_LOC, 1).
-define(DEFAULT_FORMAT, <<"text/markdown">>).

-doc "
Transforms an Erlang abstract syntax form into EEP-48 documentation format.
".
-spec main(term(), term()) -> #docs_v1{} | badarg.
main(Dirname, AST) ->
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
                       docs = process_docs(Docs) }
    catch E:R:ST ->
            erlang:raise(E, R, ST)
    end.

process_docs(#docs{ast_callbacks = AstCallbacks, ast_fns = AstFns, ast_types = AstTypes}) ->
    AstTypes ++ AstCallbacks ++ AstFns.

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
    State#docs{doc = none, doc_status = none, meta = #{exported => false}}.

update_docstatus(State, V) ->
    State#docs{doc_status = V}.

update_ast(function, #docs{ast_fns=AST}=State, Fn) ->
    State#docs{ast_fns = [Fn | AST]};
update_ast(Type,#docs{ast_types=AST}=State, Fn) when Type =:= type orelse Type =:= opaque->
    State#docs{ast_types = [Fn | AST]};
update_ast(callback, #docs{ast_callbacks = AST}=State, Fn) ->
    State#docs{ast_callbacks = [Fn | AST]}.

-spec update_meta(State :: internal_docs(), Meta :: map()) -> internal_docs().
update_meta(#docs{meta = Meta0}=State, Meta1) ->
    State#docs{meta = maps:merge(Meta0, Meta1)}.

-spec update_doc(State :: internal_docs(), Doc :: unicode:chardata() | atom()) -> internal_docs().
update_doc(#docs{doc_status = DocStatus}=State, Doc) ->
    %% The exported := true only applies to types and should be ignored for functions.
    %% This is because we need to export private types that are used on public
    %% functions, or the documentation will create dead links.
    Status = case DocStatus of
                 none -> set;
                 Other -> Other
             end,
    State1 = update_docstatus(State, Status),
    State2 = update_meta(State1, #{exported => true}),
    State2#docs{doc = string:trim(Doc)}.

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
extract_documentation([{attribute, _Anno, doc, _Meta0}| _]=AST, State) ->
    extract_documentation_from_doc(AST, State);
extract_documentation([AST0 | _T]=AST,
                      #docs{meta = #{ equiv := {call,_,Equiv,Args}} = Meta}=State)
    when is_tuple(AST0) andalso (tuple_size(AST0) > 2 orelse tuple_size(AST0) < 6) ->
    Meta1 = Meta#{ equiv := {erl_parse:normalise(Equiv), [erl_parse:normalise(A) || A <- Args]}},
    extract_documentation(AST, update_meta(State, Meta1));
extract_documentation([{function, _Anno, _F, _A, _Body} | _]=AST, State) ->
    State1 = remove_exported_type_info(State),
    extract_documentation_from_funs(AST, State1);
extract_documentation([{attribute, _Anno, TypeOrOpaque, _} | _]=AST,State)
  when TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque ->
    extract_documentation_from_type(AST, State);
extract_documentation([{attribute, _Anno, callback, _} | _]=AST, State) ->
    State1 = remove_exported_type_info(State),
    extract_documentation_from_cb(AST, State1);
extract_documentation([_H|T], State) ->
    extract_documentation(T, State);
extract_documentation([], #docs{doc_status = none}=State) ->
    State.


extract_documentation_from_type([{attribute, Anno, TypeOrOpaque, {Type, _, TypeArgs}}=_AST | T],
                      #docs{exported_types=ExpTypes, meta=Meta}=State)
  when TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque ->
    Args = fun_to_varargs(TypeArgs),
    case sets:is_element({Type, length(Args)}, ExpTypes) of
        true ->
            State1 = State#docs{ meta = Meta#{exported := true}},
            State2 = template_gen_doc({type, Anno, Type, length(Args), Args}, State1),
            extract_documentation(T, State2);
        false ->
            %% TODO: add all types, given that function specifications may used them before they are defined.
            %% from the specs we need to keep track of which types must be exported.
            %% because of this, if we return (as we do now) a list of ASTs, we need to do one more
            %% pass that catches the private types that should be exported.
            extract_documentation(T, reset_state(State))
    end.


extract_documentation_from_doc([{attribute, _Anno, doc, Meta0}=_AST | T], State) when is_map(Meta0) ->
    extract_documentation(T, update_meta(State, Meta0));
extract_documentation_from_doc([{attribute, _Anno, doc, DocStatus}=_AST | T], State) when DocStatus =:= hidden orelse DocStatus =:= false ->
    State1 = update_docstatus(State, hidden),
    extract_documentation(T, State1);
extract_documentation_from_doc([{attribute, _Anno, doc, Doc}=_AST | T], State) when is_list(Doc) ->
    extract_documentation(T, update_doc(State, Doc)).


extract_documentation_from_funs([{function, Anno, F, A, [{clause, _, ClauseArgs, _, _}]}=_AST | T],
                      #docs{exported_functions = ExpFuns}=State) ->
    case sets:is_element({F, A}, ExpFuns) orelse State#docs.export_all of
        true ->
            State1 = template_gen_doc({function, Anno, F, A, ClauseArgs}, State),
            extract_documentation(T, State1);
        false ->
            extract_documentation(T, reset_state(State))
    end;
extract_documentation_from_funs([{function, Anno, F, A, _Body}=_AST | T],
                      #docs{doc = Doc, exported_functions=ExpFuns}=State) when Doc =/= none ->
    case sets:is_element({F, A}, ExpFuns) orelse State#docs.export_all of
        true ->
            {Slogan, DocsWithoutSlogan} =
                %% First we check if there is a doc prototype
                case extract_slogan(Doc, F, A) of
                    undefined -> {io_lib:format("~p/~p",[F,A]), Doc};
                    SloganDocs -> SloganDocs
                end,
            AttrBody = {function, F, A},
            State1 = remove_exported_type_info(State),
            Result = gen_doc(Anno, AttrBody, Slogan, DocsWithoutSlogan, State1),
            State2 = update_ast(function, State1, Result),
            extract_documentation(T, reset_state(State2));
        false ->
            extract_documentation(T, reset_state(State))
    end.

extract_documentation_from_cb([{attribute, Anno, callback, {{CB, A}, [Fun]}} | T], State) ->
    Args = fun_to_varargs(Fun),
    State1 = remove_exported_type_info(State),
    State2 = template_gen_doc({callback, Anno, CB, A, Args}, State1),
    extract_documentation(T, State2).


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
gen_doc(Anno0, AttrBody, Slogan, DocStatus, #docs{meta = Meta, module_name = Module})
    when DocStatus =:= none orelse DocStatus =:= hidden ->
    Anno1 = erl_anno:set_file(Module, Anno0),
    {AttrBody, Anno1, [unicode:characters_to_binary(Slogan)], DocStatus, Meta};
gen_doc(Anno0, AttrBody, Slogan, Docs, #docs{meta = Meta, module_name = Module}) ->
    Anno1 = erl_anno:set_file(Module, Anno0),
    {AttrBody, Anno1, [unicode:characters_to_binary(Slogan)],
      #{ <<"en">> => unicode:characters_to_binary(string:trim(Docs)) }, Meta}.

template_gen_doc({Attr, Anno, F, A, _Args}, #docs{doc_status = DocStatus}=State)
  when DocStatus =:= none orelse DocStatus =:= hidden ->
    {Slogan, DocsWithoutSlogan} = {io_lib:format("~p/~p",[F,A]), DocStatus},
    AttrBody = {Attr, F, A},
    Result = gen_doc(Anno, AttrBody, Slogan, DocsWithoutSlogan, State),
    reset_state(update_ast(Attr, State, Result));
template_gen_doc({Attr, Anno, F, A, Args}, #docs{doc = Doc, doc_status = DocStatus}=State)
  when DocStatus =:= set andalso is_list(Doc) ->
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
    Result = gen_doc(Anno, AttrBody, Slogan, DocsWithoutSlogan, State),
    reset_state(update_ast(Attr, State, Result)).

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

extract_slogan(Doc, _F, _A) when Doc =:= none orelse Doc =:= hidden ->
    undefined;
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
