
-module(beam_doc_SUITE).
-export([all/0, singleton_moduledoc/1, singleton_doc/1,
         docmodule_with_doc_attributes/1, hide_moduledoc/1, docformat/1,
         singleton_docformat/1, singleton_meta/1, slogan/1,
         types_and_opaques/1, callback/1, hide_moduledoc2/1,
         private_types/1, export_all/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [singleton_moduledoc,
     singleton_doc,
     docmodule_with_doc_attributes,
     hide_moduledoc,
     hide_moduledoc2,
     docformat,
     singleton_docformat,
     singleton_meta,
     slogan,
     types_and_opaques,
     callback,
     private_types,
     export_all].

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
          [{{function, foo,1},_, [<<"foo(ok)">>], FooDoc, _},
           {{function, main,0},_, [<<"main()">>], Doc, _}]}} = code:get_doc(ModName),
    ok.

docmodule_with_doc_attributes(Conf) ->
    ModuleName = "docmodule_with_doc_attributes",
    {ok, ModName} = compile_file(Conf, ModuleName),
    Mime = <<"text/markdown">>,
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    Doc = #{<<"en">> => <<"Doc test module">>},
    {ok, {docs_v1, _,_, Mime, ModuleDoc, _,
          [{{function,no_docs,0},_, [<<"no_docs/0">>],none,#{}},
           {{function,ok,0}, _, [<<"ok/0">>],none,#{authors := "Someone"}},
           {{function, main,_},_, _, Doc, _}]}} = code:get_doc(ModName),
    ok.

hide_moduledoc(Conf) ->
    {ok, ModName} = compile_file(Conf, "hide_moduledoc"),
    {ok, {docs_v1, _,_, _Mime, hidden, _,
          [{{function, main, 0}, _, [<<"main()">>],
            #{ <<"en">> := <<"Doc test module">> }, #{}}]}} = code:get_doc(ModName),
    ok.

%% TODO: crashes
hide_moduledoc2(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    {ok, {docs_v1, _,_, _Mime, hidden, _,
          [{{function, main, 0}, _, [<<"main/0">>],hidden, #{}}]}} = code:get_doc(ModName),
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
    Doc = #{<<"en">> => <<"Doc test module\n\nMore info here">>},
    FunMeta = #{ authors => [<<"Beep Bop">>], equiv => {main, 3} },
    {ok, {docs_v1, _,_, <<"text/asciidoc">>, ModuleDoc, Meta,
          [{{function, main,_},_, _, Doc, FunMeta}]}} = code:get_doc(ModName),
    ok.

singleton_meta(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Meta = #{ authors => [<<"Beep Bop">>], equiv => {main,3}},
    DocMain1 = #{<<"en">> => <<"Returns always ok.">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{function, main1,0},_, [<<"main1()">>], DocMain1, #{equiv := {main,1}}},
           {{function, main,0},_, [<<"main/0">>], none, Meta}]}}
        = code:get_doc(ModName),
    ok.

slogan(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Doc = #{<<"en">> => <<"Returns ok.">>},
    Slogan = [<<"main(Foo)">>],
    BarDoc = #{ <<"en">> => <<"foo()\nNot a slogan since foo =/= bar">> },
    NoSlogan = [<<"no_slogan/1">>],
    NoSloganDoc = #{ <<"en">> => <<"Not a slogan\n\nTests slogans in multi-clause">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{function, spec_no_doc_slogan, 1}, _, [<<"spec_no_doc_slogan(Y)">>], _, #{}},
           {{function, no_doc_slogan, 1}, _, [<<"no_doc_slogan(X)">>], _, #{}},
           {{function, spec_slogan, 1}, _, [<<"spec_slogan(Y)">>], _, #{}},
           {{function, no_slogan,1},_,NoSlogan, NoSloganDoc, #{}},
           {{function, bar,0},_,[<<"bar()">>], BarDoc, #{}},
           {{function, main,0},_,Slogan, Doc, #{}}]}} = code:get_doc(ModName),
    ok.

types_and_opaques(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    TypeDoc = #{<<"en">> => <<"Represents the name of a person.">>},
    GenericsDoc = #{<<"en">> => <<"Tests generics">>},
    OpaqueDoc = #{<<"en">> =>
                      <<"Represents the name of a person that cannot be named.">>},
    MaybeOpaqueDoc = #{<<"en">> => <<"mmaybe(X) ::= nothing | X.\n\nRepresents a maybe type.">>},
    MaybeMeta = #{ authors => "Someone else", exported => true },
    NaturalNumberMeta = #{since => "1.0", equiv => {non_neg_integer,0}, exported => true},
    {ok, {docs_v1, _,_, _, none, _,
          [{{type,hidden,0},_,[<<"hidden/0">>],hidden,#{exported := true}},
           {{type,hidden_false,0},_,[<<"hidden_false/0">>],hidden,#{exported := true,authors := "Someone else"}},
           {{type, mmaybe,1},_,[<<"mmaybe(X)">>], MaybeOpaqueDoc, MaybeMeta},
           {{type, unnamed,0},_,[<<"unnamed()">>], OpaqueDoc, #{equiv := {non_neg_integer,0}, exported := true}},
           {{type, param,1},_,[<<"param(X)">>], GenericsDoc, #{equiv := {madeup, 0}, exported := true}},
           {{type, natural_number,0},_,[<<"natural_number/0">>], none, NaturalNumberMeta},
           {{type, name,1},_,[<<"name(_)">>], TypeDoc, #{exported := true}}
          ]}} = code:get_doc(ModName),
    ok.

callback(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Doc = #{<<"en">> => <<"Callback fn that always returns ok.">>},
    ImpCallback = #{<<"en">> => <<"This is a test">>},
    FunctionDoc = #{<<"en">> => <<"all_ok()\n\nCalls all_ok/0">>},
    ChangeOrder = #{<<"en">> => <<"Test changing order">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{callback,ann,1},_,[<<"ann/1">>],none,#{}},
           {{callback,param,1},_,[<<"param/1">>],none,#{}},
           {{callback, change_order,0},_,[<<"change_order()">>], ChangeOrder, #{equiv := {ok, 0}}},
           {{callback, all_ok,0},_,[<<"all_ok()">>], Doc, #{}},
           {{function, main2,0},_,[<<"main2()">>], #{<<"en">> := <<"Second main">>}, #{equiv := {main,0}}},
           {{function, main,0},_,[<<"main()">>], FunctionDoc, #{}},
           {{function, all_ok,0},_, [<<"all_ok()">>],ImpCallback, #{equiv := {ok, 0}}}
          ]}} = code:get_doc(ModName),
    ok.

private_types(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Code = code:get_doc(ModName),
    {ok, {docs_v1, _,_, _, none, _,
          [
           {{type,hidden_export_t,0},_,[<<"hidden_export_t/0">>],hidden,#{exported := true}},
           {{type,public_t,0},_, [<<"public_t/0">>], none,#{ exported := true}},

           %% TODO: fix line below
           %% {{type,private_t,0},_, [<<"private_t/0">>], none,#{ exported := false}},
           {{function,hidden,0},_,[<<"hidden/0">>],hidden,#{}},
           {{function,bar,0},_,[<<"bar/0">>],none,#{}}
           ]}} = Code,
    ok.


export_all(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    ImpCallback = #{<<"en">> => <<"This is a test">>},
    FunctionDoc = #{<<"en">> => <<"all_ok()\n\nCalls all_ok/0">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{function, main2,0},_,[<<"main2()">>], #{<<"en">> := <<"Second main">>}, #{equiv := {main,0}}},
           {{function, main,0},_,[<<"main()">>], FunctionDoc, #{}},
           {{function, all_ok,0},_, [<<"all_ok()">>],ImpCallback, #{equiv := {ok, 0}}}
          ]}} = code:get_doc(ModName),
    ok.


compile_file(Conf, ModuleName) ->
    ErlModName = ModuleName ++ ".erl",
    Filename = filename:join(proplists:get_value(data_dir, Conf), ErlModName),
    compile:file(Filename, [beam_docs]).
