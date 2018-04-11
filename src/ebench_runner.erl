-module(ebench_runner).

%% API exports
-export([main/1, init/1, stop/1]).

%%====================================================================
%% API functions
%%====================================================================

%% Entry point
main([Name, Iters, M, F, ArgsAtom]) ->
    {value, Args, _} = eval(ArgsAtom),
    run(atom_to_list(Name), list_to_integer(atom_to_list(Iters)), M, F, Args).

init([Title, Class, BM, Fun]) ->
    exec_fun(Title, Class, BM, Fun).

stop([Title, Class, BM, Fun]) ->
    exec_fun(Title, Class, BM, Fun),
    init:stop().

%%====================================================================
%% Internal functions
%%====================================================================

exec_fun(Title, Class, BM, FunAtom) ->
    Bindings = erl_eval:add_binding(
                 'Benchmark', BM,
                 erl_eval:add_binding(
                   'Class', Class,
                   erl_eval:add_binding(
                     'Title', Title,
                     erl_eval:new_bindings()))),
    {value, Fun, _} = eval(FunAtom, Bindings),
    if
        is_function(Fun, 3) ->
            Fun(atom_to_list(Title), Class, BM);
        true ->
            ok
    end.

eval(Atom) ->
    eval(Atom, erl_eval:new_bindings()).
eval(Atom, Bindings) ->
    {ok, Tokens, _} = erl_scan:string(atom_to_list(Atom) ++ "."),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    erl_eval:exprs(Parsed, Bindings).

run(Name, Iters, M, F, A) ->
    Self = self(),
    Pid = spawn_opt(
      fun() ->
              %% Supress IO
              {ok, Fd} = file:open("/dev/null", [write]),
              group_leader(Fd, self()),
              Runner = fun() -> try apply(M, F, A)
                                catch exit:ok -> ok;
                                      E:R ->
                                        io:format(user, "~p:~p ~p~n",[E, R, erlang:get_stacktrace()]),
                                        erlang:halt(1)
                                end
                       end,
              Self ! eministat:s(Name, Runner, Iters)
      end, []),
    erlang:monitor(process, Pid),
    receive
        Res ->
            io:format("~p.",[Res])
    end.
