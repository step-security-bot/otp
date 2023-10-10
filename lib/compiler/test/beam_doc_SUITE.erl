-module(beam_doc_SUITE).

-export([all/0, simple_moduledoc/1]).
-include_lib("common_test/include/ct.hrl").

all() ->
    [simple_moduledoc].



simple_moduledoc(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),

    ModuleContent = <<"-module(moduledoc).

                       -export([]).

                       -moduledoc \"
                       Moduledoc test module
                       \".
                       ">>,
    {ModName, Filename} = create_file(PrivDir, ModuleContent),
    %% positive test: checks that all works as expected
    %% FileName = CreateFile("module_attr", ErlModName, ModuleFileContent),
    {ok, ModName} = compile:file(Filename, [beam_docs]),
    %% ct:pal("~p~n~p~n~p~n", [Result, ModuleAtom, Result =:= ModuleAtom]),


    Mime = <<"text/markdown">>,
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    {ok, {docs_v1, _,_, Mime,ModuleDoc, _,_}} = code:get_doc(ModName),
    ok.

create_dir(Dir) ->
  ok = file:make_dir(Dir).

create_file(PrivDir, ModuleContent) ->
    Dir = "module_attr",
    ModuleName = "moduledoc",
    ModuleAtom = list_to_atom(ModuleName),
    ErlModName = ModuleName ++ ".erl",

    Dirname = filename:join(PrivDir, Dir),
    Filename = filename:join(Dirname, ErlModName),
    ok = filelib:ensure_dir(Filename),
    ok = file:write_file(Filename, ModuleContent),

    true = code:add_path(Dirname),
    {ModuleAtom, Filename}.
