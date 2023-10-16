
-module(beam_doc_SUITE).
-export([all/0, singleton_moduledoc/1, singleton_doc/1,
         docmodule_with_doc_attributes/1, hide_moduledoc/1, docformat/1,
         singleton_docformat/1, singleton_meta/1, slogan/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [singleton_moduledoc,
     singleton_doc,
     docmodule_with_doc_attributes,
     hide_moduledoc,
     docformat,
     singleton_docformat,
     singleton_meta,
     slogan].

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
    {ok, {docs_v1, 1,_, Mime, none, _,
          [{{function, main,_},_, _, Doc, _}]}} = code:get_doc(ModName),
    ok.

docmodule_with_doc_attributes(Conf) ->
    ModuleName = "docmodule_with_doc_attributes",
    {ok, ModName} = compile_file(Conf, ModuleName),
    Mime = <<"text/markdown">>,
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    Doc = #{<<"en">> => <<"Doc test module">>},
    {ok, {docs_v1, _,_, Mime, ModuleDoc, _,
          [{{function, main,_},_, _, Doc, _}]}} = code:get_doc(ModName),
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
    Doc = #{<<"en">> => <<"Doc test module\n\nMore info here">>},
    {ok, {docs_v1, _,_, <<"text/asciidoc">>, ModuleDoc, Meta,
          [{{function, main,_},_, _, Doc, _}]}} = code:get_doc(ModName),
    ok.

singleton_meta(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Meta = #{ authors => [<<"Beep Bop">>], equiv => {main,3} },
    Meta1 = #{equiv => {main,1}},
    DocMain = #{<<"en">> => <<"Equivalent to `main/3`">>},
    DocMain1 = #{<<"en">> => <<"Returns always ok.\nEquivalent to `main(_)`">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{function, main,0},_, [<<"main()">>], DocMain, Meta},
           {{function, main1,0},_, [<<"main1()">>], DocMain1, Meta1}]}} = code:get_doc(ModName),
    ok.

slogan(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Doc = #{<<"en">> => <<"Returns ok.">>},
    Slogan = [<<"main()">>],
    {ok, {docs_v1, _,_, _, none, _,
          [{{function, main,0},_,Slogan, Doc, #{}}]}} = code:get_doc(ModName),
    ok.


compile_file(Conf, ModuleName) ->
    ErlModName = ModuleName ++ ".erl",
    Filename = filename:join(proplists:get_value(data_dir, Conf), ErlModName),
    compile:file(Filename, [beam_docs]).
