-module(ebench).

%% API exports
-export([main/1, benchmarks/1, parse_and_check/3, benchmark_options/0]).

%% Utility function export
-export([mkdir/1, open/1, compile_class/3, class_ebin_dir/2, class_priv_dir/2,
        read_tags/2, tdforeach/3, abort/1, abort/2, parse_tag_rest/1]).

%% Exported to silence warning
-export([print/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:setopts([{encoding, unicode}]),
    case parse_action(Args) of
        {"run", RunArgs} ->
            ebench_run:main(RunArgs);
        {"plot", PlotArgs} ->
            ebench_plot:main(PlotArgs);
        {"eministat", StatArgs} ->
            ebench_eministat:main(StatArgs);
        {"list", ListArgs} ->
            ebench_list(ListArgs);
        {"help", _HelpArgs} ->
            ok;
        {Unknown, _} ->
            format_error({error,{invalid_option,Unknown}}, fun usage/1, global_options()),
            erlang:halt(1)
    end,
    init:stop().

parse_and_check(Args, Opts, UsageFun) ->
    AllOpts = Opts ++ global_options(),
    case opts_from_list(getopt:parse_and_check(AllOpts, Args)) of
        {ok, {#{ help := true }, _Rest}} ->
            UsageFun(AllOpts),
            erlang:halt(0);
        {ok, {ArgOpts, Rest}} ->
            {ok, ArgOpts, Rest};
        Error ->
            format_error(Error, UsageFun, AllOpts),
            erlang:halt(1)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

parse_action([]) ->
    parse_action(["help"]);
parse_action(["-h" | T]) ->
    parse_action(["help" | T]);
parse_action([Action | T]) ->
    {Action, T}.

benchmark_options() ->
    [{class, $c, "class", {string, "all"}, "Which class of benchmarks to run"},
     {class_directory, undefined, "class_directory", {string,"priv"},
      "Directory to look for benchmark classes in."},
     {benchmark, $b, "benchmark", {string,"all"}, "Which benchmark to run"}].

global_options() ->
    [{help, $h, "help", {boolean, false}, "Show help for the given action"}].

usage(Opts) ->
    getopt:usage(Opts, "ebench <action>").

format_error(Error, UsageFun, Opts) ->
    io:format(standard_error, "~s~n",[getopt:format_error(Opts, Error)]),
    UsageFun(Opts).

opts_from_list({ok, {OptList, Rest}}) ->
    OptMap = lists:foldl(
               fun({Key, Val}, M) when Key =:= class; Key =:= benchmark ->
                       M#{ Key => [Val | maps:get(Key, M, [])]};
                  ({Key, Val}, M) ->
                       M#{ Key => Val };
                  (Key, M) ->
                       M#{ Key => true }
               end, #{}, OptList),
    {ok, {OptMap, Rest}};
opts_from_list(Error) ->
    Error.


ebench_list(Args) ->
    {ok, Opts, _Rest} = ebench:parse_and_check(Args, benchmark_options(), fun usage/1),
    [begin
         io:format("Class: ~s~n",[Class]),
         [io:format("  ~s~n", [BM]) || BM <- BMs]
     end || {Class, BMs} <- benchmarks(Opts), BMs =/= []].

classes(#{ class_directory := CD, class := Class }) ->
    [filename:basename(D) || D <- filelib:wildcard(filename:join(CD,"*")),
                             Class =:= ["all"] orelse lists:member(filename:basename(D), Class)].

benchmarks(Opts) ->
    benchmarks(classes(Opts), Opts).

benchmarks([Class|T], Opts = #{ benchmark := BM }) ->
    compile_class(Class, Opts),
    Modules = filelib:wildcard(filename:join(class_ebin_dir(Class, Opts),"*.beam")),
    BMs = lists:flatmap(
            fun(M) ->
                    RootName = filename:rootname(M),
                    BaseRootName = filename:basename(RootName),
                    [BaseRootName || is_benchmark(RootName),
                                     BM =:= ["all"] orelse lists:member(BaseRootName, BM)]
            end, Modules),
    case BMs of
        [] ->
            benchmarks(T, Opts);
        _ ->
            [{Class, BMs} | benchmarks(T, Opts)]
    end;
benchmarks([], _) ->
    [].

is_benchmark(ModulePath) ->
    case code:load_abs(ModulePath) of
        {module, M} ->
            lists:member({main,1}, M:module_info(exports));
        _E ->
            false
    end.

print(Term) ->
    io:format("~p~n",[Term]).

class_dir(Class, #{ class_directory := CD }) ->
    filename:join(CD, Class).

class_lib_dir(Class, Opts) ->
    filename:join([class_dir(Class, Opts), "_build", "default", "lib", Class]).

class_priv_dir(Class, Opts) ->
    filename:join(class_lib_dir(Class, Opts), "priv").

class_ebin_dir(Class, Opts) ->
    filename:join(class_lib_dir(Class, Opts), "ebin").

compile_class(Class, Opts) ->
    compile_class(Class, "", Opts).
compile_class(Class, Path, Opts) ->
    os:cmd("cd " ++ class_dir(Class, Opts) ++ " && rebar3 clean && "
           "PATH=$PATH:" ++ Path ++ " rebar3 compile").

parse_tag_rest(Args) ->
    #{ tags =>
           lists:map(
             fun(Arg) ->
                     case string:lexemes(Arg, "=") of
                         [Title] ->
                             {Title, "latest"};
                         [Title, Tag] ->
                             {Title, Tag}
                     end
             end, Args) }.

read_tags(Tags, Classes) ->
    [read_tag(Tag, Classes) || Tag <- Tags].
read_tag({Title,Tag}, Classes) ->
    TagDir = filename:join(["results", Title, Tag]),
    ClassData = lists:flatmap(fun({Class,BMs}) ->
                           read_tag_class(TagDir, Class, BMs)
                   end, Classes),
    {Title, Tag, ClassData}.

read_tag_class(TagDir, Class, BMs) ->
    case file:consult(filename:join(TagDir, Class)) of
        {ok, BMData} ->
            [{Class, [{BM, DS} || {BM, DS} <- BMData, lists:member(BM, BMs)]}];
        {error, enoent} ->
            []
    end.

tdforeach(Fun, TagData, Opts) ->
    maps:map(
      fun(Class, BMs) ->
              maps:map(
                fun(BM, DSs) ->
                        DS = lists:map(
                               fun(Tag) ->
                                       lists:keyfind(Tag, 1, DSs)
                               end, maps:get(tags, Opts)),
                        Fun(Class, BM, DS)
                end, BMs)
      end, tdinvert(TagData)).

tdinvert(TagData) ->
    tdinvert(TagData, #{}).

tdinvert([{Title, Tag, Classes} | T], Acc) ->
    tdinvert(T, tdinvert({Title,Tag}, Classes, Acc));
tdinvert([], Acc) ->
    Acc.
tdinvert(TT, [{Class, BMs}|T], Acc) when is_list(BMs) ->
    BMAcc = maps:get(Class, Acc, #{}),
    tdinvert(TT, T, Acc#{ Class => tdinvert(TT, BMs, BMAcc) });
tdinvert(TT, [{BM, DS}|T], Acc) ->
    tdinvert(TT, T, Acc#{ BM => [{TT, DS} | maps:get(BM, Acc, [])]});
tdinvert(_TT, [], Acc) ->
    Acc.


open(Filename) ->
    mkdir(filename:dirname(Filename)),
    {ok, Fd} = file:open(Filename, [write]),
    Fd.

mkdir(Dir) ->
    mkdir(string:lexemes(Dir,"/"), []).
mkdir(["."|T], []) ->
    mkdir(T, ["."]);
mkdir([D|T], Pre) ->
    DirName = filename:join(Pre, D),
    case file:read_file_info(DirName) of
        {error, _} ->
            ok = file:make_dir(DirName);
        _ ->
            ok
    end,
    mkdir(T, DirName);
mkdir([], _) ->
    ok.

abort(String) ->
    abort("~s~n",[String]).
abort(Format, Args) ->
    io:format(standard_error, Format, Args),
    erlang:halt(1).
