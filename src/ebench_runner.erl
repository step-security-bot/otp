-module(ebench_runner).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% Entry point
main([Name, Iters, BM]) ->
    Args = get_args(BM),
    run(atom_to_list(Name), list_to_integer(atom_to_list(Iters)), BM, Args).
%%====================================================================
%% Internal functions
%%====================================================================

get_args(BM) ->
    try BM:medium() of
        Args -> Args
    catch _:_ ->
            []
    end.

run(Name, Iters, BM, Args) ->
    Self = self(),
    Pid = spawn_opt(
      fun() ->
              %% Supress IO
              {ok, F} = file:open("/dev/null", [write]),
              group_leader(F, self()),
              Runner = fun() -> try BM:main(Args)
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
