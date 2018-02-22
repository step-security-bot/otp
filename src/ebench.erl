-module(ebench).

%% API exports
-export([main/1]).

%% Exported to silence warning
-export([print/1]).

-mode(compile).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:setopts([{encoding, unicode}]),
    case getopt:parse(options(), Args) of
        {ok, {ArgOpts, Rest}} ->
            case opts_from_list(ArgOpts) of
                #{ list := true } = Opts ->
                    [begin
                         io:format("Class: ~s~n",[Class]),
                         [io:format("  ~s~n", [BM]) || BM <- BMs]
                     end || {Class, BMs} <- benchmarks(Opts), BMs =/= []];
                #{ analyze := _ } = Opts->
                    analyze(maps:merge(Opts, parse_analyze_rest(Rest)));
                Opts ->
                    run_commands(maps:merge(Opts, parse_cmd_rest(Rest)))
            end,
            erlang:halt(0);
        Error ->
            io:format(standard_error, "~s~n",[getopt:format_error(options(), Error)]),
            getopt:usage(options(), ?MODULE_STRING, "Title=Command..."),
            io:format("Example:~n"
                      "  ./" ?MODULE_STRING " -c small BASE=erl \"BOUND=erl +sbtdb\"~n")
    end.

%%====================================================================
%% Internal functions
%%====================================================================

options() ->
    [
     {iterations, $i, "iter", {integer, 3}, "Number of iterations"},
     {class, $c, "class", {string, "all"}, "Which class of benchmarks to run"},
     {class_directory, undefined, "class_directory", {string,"priv"},
      "Directory to look for benchmark classes in."},
     {benchmark, $b, "benchmark", {string,"all"}, "Which benchmark to run"},
     {list, $l, "list", undefined, "List all benchmarks"},
     {analyze, $a, "analyze", undefined, "Analyze a benchmark run"},
     {type, undefined, "type", {string,"eministat"},
      "Which tool that should be used to do the analysis"}
    ].

opts_from_list(OptList) ->
    lists:foldl(fun({Key, Val}, M) when Key =:= class; Key =:= benchmark ->
                        M#{ Key => [Val | maps:get(Key, M, [])]};
                   ({Key, Val}, M) ->
                        M#{ Key => Val };
                   (Key, M) ->
                        M#{ Key => true }
                end, #{}, OptList).

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
    [{Class, BMs} | benchmarks(T, Opts)];
benchmarks([], _) ->
    [].

is_benchmark(ModulePath) ->
    case code:load_abs(ModulePath) of
        {module, M} ->
            lists:member({main,1}, M:module_info(exports));
        _E ->
            false
    end.

parse_cmd_rest(Args) ->
    #{ commands =>
           lists:map(
             fun(Arg) ->
                     [Title | Cmd] = string:lexemes(Arg, "="),
                     [Erl | Opts] = string:lexemes(Cmd, " "),
                     {Title, Erl, lists:join(" ", Opts)}
             end, Args) }.

run_commands(Opts = #{ commands := Cmds }) ->
    TSOpts = Opts#{ ts => calendar:local_time() },
    Classes = benchmarks(Opts),
    [run_command(Title, Erl, CmdOpts, Classes, TSOpts) || {Title, Erl, CmdOpts} <- Cmds].

run_command(Title, Erl, CmdOpts, Classes, Opts = #{ ts := Ts }) ->
    io:format("Benchmarking: ~s ~s~n",[Erl, CmdOpts]),
    Latest = filename:join([".", "results", Title, "latest"]),
    TSDir = filename:join([".", "results", Title, ts2str(Ts)]),
    mkdir(TSDir),
    case file:read_link_info(Latest) of
        {ok, _} ->
            ok = file:delete(Latest);
        _E ->
            ok
    end,
    ok = file:make_symlink(lists:flatten(ts2str(Ts)), Latest),
    [run_class(Title, Erl, CmdOpts, Class, BMs, Opts) || {Class, BMs} <- Classes].

run_class(Title, Erl, CmdOpts, Class, BMs, Opts = #{ ts := Ts }) ->
    ERL_PATH = filename:dirname(os:find_executable(Erl)),
    compile_class(Class, ERL_PATH, Opts),
    io:format("  Class: ~s~n", [Class]),
    file:write_file(filename:join([".", "results", Title, ts2str(Ts), "METADATA"]),
                    io_lib:format("~p.~n~p.",
                                  [erlang:system_info(system_version),
                                   lists:join(" ",erlang:system_info(emu_args))])),
    Fd = open(filename:join([".", "results", Title, ts2str(Ts), Class])),
    [run_benchmark(Title, Erl, CmdOpts, Class, BM, Fd, Opts) || BM <- BMs].

run_benchmark(Title, Erl, CmdOpts, Class, BM, Fd, Opts) ->
    Rebar3Path = os:cmd("rebar3 path"),
    io:format("    Running: ~s...", [BM]),
    Name = [Title,"-",Class,"-",BM],
    Cmd = lists:concat(
            [Erl, " ", CmdOpts, " -noshell"
             " -pz ", Rebar3Path,
             " -pa ", class_ebin_dir(Class, Opts), " ", class_priv_dir(Class, Opts),
             " -s ebench_runner main ",Name," ", maps:get(iterations, Opts) ," ",
             BM, " -s init stop"]),
    io:format(Fd, "{~p,~p}.~n",[BM, parse(os:cmd(Cmd))]),
    io:format("done~n").

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

ts2str({{YY,MM,DD},{HH,Mi,SS}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",[YY,MM,DD,HH,Mi,SS]).

parse(String) ->
    try
        {ok, Tokens, _} = erl_scan:string(String),
        {ok, Term} = erl_parse:parse_term(Tokens),
        Term
    catch _:_ ->
            io:format("Failed to parse: ~p~n",[String]),
            erlang:halt(1)
    end.


parse_analyze_rest(Args) ->
    #{ tags =>
           lists:map(
             fun(Arg) ->
                     case string:lexemes(Arg, "=") of
                         [Title] ->
                             {Title,"latest"};
                         [Title, Tag] ->
                             {Title, Tag}
                     end
             end, Args) }.

analyze(Opts = #{ tags := Tags, type := Type }) ->
    Classes = benchmarks(Opts),
    TagData = [read_tag(Tag, Classes) || Tag <- Tags],
    analyze(Type, TagData, Opts).

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


analyze("eministat", TagData, Opts) ->
    tdforeach(fun(_Class, _BM, Data) ->
                      [BaseDS | RestDS] = [DS || {_, DS} <- Data],
                      eministat:x(95.0, BaseDS, RestDS)
              end, TagData, Opts).

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
