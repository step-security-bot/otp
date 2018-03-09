-module(ebench).

%% API exports
-export([main/1, benchmarks/1, parse_and_check/3, benchmark_options/0]).

%% Utility function export
-export([mkdir/1, open/1, compile_class/3, class_ebin_dir/2, class_priv_dir/2,
         read_tags/2, tdforeach/3, abort/1, abort/2, parse_tag_rest/1,
         format_error/3, sort_options/1]).

%% Exported to silence warning
-export([print/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:setopts([{encoding, unicode}]),
    case parse_task(Args) of
        {"help", _HelpArgs} ->
            usage(global_options());
        {Task, TaskArgs} ->
            case lists:keyfind(Task, 1, tasks()) of
                {Task, Callback} ->
                    Callback:main(TaskArgs);
                false ->
                    case Task of
                        "-" ++ _ ->
                            format_error({error, {invalid_option, Task}},
                                         fun usage/1, global_options());
                        Task ->
                            format_error({error, {"invalid task", Task}},
                                         fun usage/1, global_options())
                    end,
                    erlang:halt(1)
            end
    end,
    init:stop().

%%====================================================================
%% Argument handling functions
%%====================================================================

parse_task([]) ->
    parse_task(["help"]);
parse_task(["-h" | T]) ->
    parse_task(["help" | T]);
parse_task([Task | T]) ->
    {Task, T}.

tasks() ->
    [{"run", ebench_run},
     {"plot", ebench_plot},
     {"eministat", ebench_eministat},
     {"list", ebench_list}].

parse_and_check(Args, Opts, UsageFun) ->
    AllOpts = sort_options(Opts ++ global_options()),
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

benchmark_options() ->
    [{class, $c, "class", {string, "all"}, "Which class of benchmarks to run. "
      "This option can be given multiple times."},
     {class_directory, undefined, "class_directory", {string,"priv"},
      "Directory to look for benchmark classes in."},
     {benchmark, $b, "benchmark", {string,"all"}, "Which benchmark to run. "
      "This option can be given multiple times."}].

global_options() ->
    [{help, $h, "help", {boolean, false}, "Print help for the given task"}].

%% This sort function makes it so that all options with a short opt
%% is first ordered, the long opt and then no opt.
sort_options(Options) ->
    lists:usort(fun({NameA, OptA, LongA, _, _},
                    {NameB, OptB, LongB, _, _}) ->
                        if
                            OptA =/= OptB ->
                                OptA =< OptB;
                            LongA =/= LongB and is_list(LongA) and is_list(LongB) ->
                                LongA =< LongB;
                            LongA =/= LongB ->
                                LongA >= LongB;
                            true ->
                                NameA =< NameB
                        end
                end, Options).

usage(Opts) ->
    getopt:usage([{task, undefined, undefined, string, "Task to run"}|Opts],
                 "ebench"),
    io:format("ebench is a tool for benchmarking Erlang performance.~n~n"
              "Several tasks are available:~n~n"),
    [io:format("~.12s~s~n",[Task,CB:slogan()]) || {Task, CB} <- tasks()],
    io:format("~n"
              "Run 'ebench <task> -h' for details about each task.~n").

format_error(Error, UsageFun, Opts) ->
    io:format(standard_error, "~s~n",[getopt:format_error(Opts, Error)]),
    UsageFun(sort_options(Opts)).

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

%%====================================================================
%% Functions to figure out which classes and benchmarks are available
%%====================================================================
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

%%====================================================================
%% Functions to work with benchmark result data
%%====================================================================
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
read_tag(Filename, Classes) ->
    print(Filename),
    {ok, Data} =
        case file:consult(Filename) of
            {error, enoent} ->
                case file:consult(Filename ++ ".term") of
                    {error, Reason} ->
                        abort("Could not open ~s or ~s.term, reason ~p~n",
                              [Filename, Filename, Reason]);
                    D ->
                        D
                end;
            D ->
                D
        end,
    {metadata, Metadata} = lists:keyfind(metadata, 1, Data),
    ClassDataMap = lists:foldl(
                     fun({Class, BM, BMData}, ClassMap) ->
                             BMs = maps:get(Class, ClassMap, []),
                             ClassMap#{ Class => [{BM, BMData} | BMs] };
                        (_, ClassMap) ->
                             ClassMap
                     end, #{}, Data),
    ClassData = lists:flatmap(fun({Class, BMs}) ->
                                      case maps:find(Class, ClassDataMap) of
                                          error ->
                                              [];
                                          {ok, BMData} ->
                                              [{Class, [{BM, DS} || {BM, DS} <- BMData,
                                                                    lists:member(BM, BMs)]}]
                                      end
                              end, Classes),
    {maps:get(title, Metadata), ClassData}.


tdforeach(Fun, TagData, Titles) ->
    maps:map(
      fun(Class, BMs) ->
              maps:map(
                fun(BM, DSs) ->
                        DS = lists:map(
                               fun(Tag) ->
                                       lists:keyfind(Tag, 1, DSs)
                               end, Titles),
                        Fun(Class, BM, DS)
                end, BMs)
      end, tdinvert(TagData)).

tdinvert(TagData) ->
    tdinvert(TagData, #{}).

tdinvert([{Title, Classes} | T], Acc) ->
    tdinvert(T, tdinvert(Title, Classes, Acc));
tdinvert([], Acc) ->
    Acc.
tdinvert(TT, [{Class, BMs}|T], Acc) when is_list(BMs) ->
    BMAcc = maps:get(Class, Acc, #{}),
    tdinvert(TT, T, Acc#{ Class => tdinvert(TT, BMs, BMAcc) });
tdinvert(TT, [{BM, DS}|T], Acc) ->
    tdinvert(TT, T, Acc#{ BM => [{TT, DS} | maps:get(BM, Acc, [])]});
tdinvert(_TT, [], Acc) ->
    Acc.

%%====================================================================
%% Small convenience functions
%%====================================================================
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

print(Term) ->
    io:format("~p~n",[Term]).
