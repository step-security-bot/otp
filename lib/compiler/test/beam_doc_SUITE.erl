
-module(beam_doc_SUITE).
-export([all/0, singleton_moduledoc/1, singleton_doc/1,
         docmodule_with_doc_attributes/1, hide_moduledoc/1, docformat/1,
         singleton_docformat/1, singleton_meta/1, slogan/1, types_and_opaques/1, callback/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [singleton_moduledoc,
     singleton_doc,
     docmodule_with_doc_attributes,
     hide_moduledoc,
     docformat,
     singleton_docformat,
     singleton_meta,
     slogan,
     types_and_opaques,
     callback].

-define(get_name(), atom_to_list(?FUNCTION_NAME)).

singleton_moduledoc(Conf) ->
    ModuleName = "singletonmoduledoc",
    {ok, ModName} = compile_file(Conf, ModuleName),

    Mime = <<"text/markdown">>,
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    {ok, {docs_v1, _,_, Mime,ModuleDoc, _,_}} = code:get_doc(ModName),
    ok.

singleton_doc(Conf) ->
    ModuleName = "singletondoc",
    {ok, ModName} = compile_file(Conf, ModuleName),
    Mime = <<"text/markdown">>,
    Doc = #{<<"en">> => <<"Doc test module">>},
    FooDoc = #{<<"en">> => <<"Tests multi-clauses">>},
    {ok, {docs_v1, 1,_, Mime, none, _,
          [{{function, main,0},_, [<<"main()">>], Doc, _},
           {{function, foo,1},_, [<<"foo(ok)">>], FooDoc, _}
          ]}} = code:get_doc(ModName),
    ok.

docmodule_with_doc_attributes(Conf) ->
    ModuleName = "docmodule_with_doc_attributes",
    {ok, ModName} = compile_file(Conf, ModuleName),
    Mime = <<"text/markdown">>,
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    Doc = #{<<"en">> => <<"Doc test module">>},
    {ok, {docs_v1, _,_, Mime, ModuleDoc, _,
          [{{function, main,_},_, _, Doc, _},
           {{function,ok,0}, _, [<<"ok/0">>],none,#{authors := "Someone"}},
           {{function,no_docs,0},_, [<<"no_docs/0">>],none,#{}}]}} = code:get_doc(ModName),
    ok.

hide_moduledoc(Conf) ->
    {ok, ModName} = compile_file(Conf, "hide_moduledoc"),
    {ok, {docs_v1, _,_, _Mime, hidden, _, []}} = code:get_doc(ModName),
    ok.

docformat(Conf) ->
    {ok, ModName} = compile_file(Conf, "docformat"),
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    Meta = #{format => "text/asciidoc",
             deprecated => "Use something else",
             otp_doc_vsn => {1,0,0},
             since => "1.0"},
    Doc = #{<<"en">> => <<"Doc test module">>},
    {ok, {docs_v1, _,_, <<"text/asciidoc">>, ModuleDoc, Meta,
          [{{function, main,_},_, _, Doc, _}]}} = code:get_doc(ModName),
    ok.

singleton_docformat(Conf) ->
    {ok, ModName} = compile_file(Conf, "singleton_docformat"),
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    Meta = #{format => <<"text/asciidoc">>,
             deprecated => "Use something else",
             otp_doc_vsn => {1,0,0},
             since => "1.0"},
    Doc = #{<<"en">> => <<"Doc test module\n\nMore info here\nEquivalent to `main/3`">>},
    FunMeta = #{ authors => [<<"Beep Bop">>] },
    {ok, {docs_v1, _,_, <<"text/asciidoc">>, ModuleDoc, Meta,
          [{{function, main,_},_, _, Doc, FunMeta}]}} = code:get_doc(ModName),
    ok.

singleton_meta(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Meta = #{ authors => [<<"Beep Bop">>]},
    DocMain = #{<<"en">> => <<"Equivalent to `main/3`">>},
    DocMain1 = #{<<"en">> => <<"Returns always ok.\nEquivalent to `main/1`">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{function, main,0},_, [<<"main()">>], DocMain, Meta},
           {{function, main1,0},_, [<<"main1()">>], DocMain1, #{}}]}} = code:get_doc(ModName),
    ok.

slogan(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Doc = #{<<"en">> => <<"Returns ok.">>},
    Slogan = [<<"main()">>],
    BarDoc = #{ <<"en">> => <<"foo()\nNot a slogan since foo =/= bar">> },
    NoSlogan = [<<"no_slogan/1">>],
    NoSloganDoc = #{ <<"en">> => <<"Not a slogan\n\nTests slogans in multi-clause">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{function, main,0},_,Slogan, Doc, #{}},
           {{function, bar,0},_,[<<"bar()">>], BarDoc, #{}},
           {{function, no_slogan,1},_,NoSlogan, NoSloganDoc, #{}}]}} = code:get_doc(ModName),
    ok.

types_and_opaques(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    TypeDoc = #{<<"en">> => <<"Represents the name of a person.">>},
    TypeWithMetaDoc = #{<<"en">> => <<"Equivalent to `t:non_neg_integer/0`">>},
    OpaqueDoc = #{<<"en">> =>
                      <<"Represents the name of a person that cannot be named.\nEquivalent to `non_neg_integer/0`">>},
    MaybeOpaqueDoc = #{<<"en">> => <<"mmaybe(X) ::= nothing | X.\n\nRepresents a maybe type.">>},
    MaybeMeta = #{ authors => "Someone else" },
    NaturalNumberMeta = #{since => "1.0"},
    {ok, {docs_v1, _,_, _, none, _,
          [{{type, name,1},_,[<<"name(_)">>], TypeDoc, #{}},
           {{type, natural_number,0},_,[<<"natural_number()">>], TypeWithMetaDoc, NaturalNumberMeta},
           {{type, unnamed,0},_,[<<"unnamed()">>], OpaqueDoc, #{}},
           {{type, mmaybe,1},_,[<<"mmaybe(X)">>], MaybeOpaqueDoc, MaybeMeta}
          ]}} = code:get_doc(ModName),
    ok.

callback(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Doc = #{<<"en">> => <<"Callback fn that always returns ok.">>},
    ImpCallback = #{<<"en">> => <<"This is a test\nEquivalent to `ok/0`">>},
    FunctionDoc = #{<<"en">> => <<"all_ok()\n\nCalls all_ok/0">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{callback, all_ok,0},_,[<<"all_ok()">>], Doc, #{}},
           {{function, all_ok,0},_, [<<"all_ok()">>],ImpCallback, #{}},
           {{function, main,0},_,[<<"main()">>], FunctionDoc, #{}}
          ]}} = code:get_doc(ModName),
    ok.


compile_file(Conf, ModuleName) ->
    ErlModName = ModuleName ++ ".erl",
    Filename = filename:join(proplists:get_value(data_dir, Conf), ErlModName),
    compile:file(Filename, [beam_docs]).
