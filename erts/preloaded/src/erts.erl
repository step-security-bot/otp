%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2019. All Rights Reserved.
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
-module(erts).

-behaviour(application).

-export([start/2, stop/1, check_env/1]).

-compile({parse_transform,application}).
-env([async/0, break/0, time/0, coredump/0,
      scalability/0, unicode/0, ets/0,
      processes/0, ports/0, atoms/0,
      io/0, dist/0, memory/0,
      schedulers/0]).
-env_function(check).

-type async() ::
        #{
          %% +A
          size => 1..1024,
          %% +a
          stacksize => 16..8192
         }.

%% +B
-type break() :: disable | ignore.
-type time() ::
        #{
          %% +c
          correction => boolean(),
          %% +C
          warp_mode => no_time_warp | single_time_warp | multi_time_warp,
          %% +T
          timing => 0..9
         }.
%% +d
-type coredump() :: boolean().
%% +rg
-type scalability() :: #{ reader_groups => 1..256 }.
-type unicode() ::
        #{
          %% +fnl/u/a
          filename =>
              #{ encoding => latin1 | unicode | automatic,
                 error => warn | ignore | error
               },
          %% +pc
          printable_characters => #{ encoding => latin1 | unicode }
         }.
-type ets() ::
        #{
          %% +e
          limit => integer(),
          %% +ec
          compressed => boolean(),
          %% +r
          always_realloc => boolean()
         }.

-type processes() ::
         #{
           %% +P
           limit => 1024..134217727,
           heap =>
               #{ term =>
                      #{
                        %% +hms
                        min => integer(),
                        max =>
                            #{
                              %% +hmax
                              size => integer(),
                              %% +hmaxel
                              error_logger => boolean(),
                              %% +hmaxk
                              kill => boolean
                             }
                       },
                  %% +hmbs
                  binary => #{ min => integer() },
                  %% +hmqd
                  message_queue => on_heap | off_heap
                },
           %% +hpds
           dictionary => #{ size => integer() }
          }.

%% +Q
-type ports() :: #{ limit => integer() }.

%% +t
-type atoms() :: #{ limit  => integer() }.

-type io() ::
         #{
           %% +IOp
           pollsets => 1..1024,
           %% +IOt
           pollthreads => 1..1024
          }.

-type dist() ::
         #{
           %% +R
           compatability => 22..24,
           %% +zdbbl
           buffer_busy_limit => 1..2097151,
           %% +zdntgc
           delayed_node_table_gc => 0..100000000
          }.

-type util_alloc_opts() ::
        #{
          %% +Mue
          enabled => boolean(),

          migration =>
              #{ abandon_carrier =>
                     #{
                       %% +Muacul
                       utilization_limit => 0..100,
                       %% +Mucfml
                       free_block_min_limit => integer(),
                       %% +Muacnl
                       limit => integer()
                      }
               },
          %% +Muas, we can probably make this more intuitive...
          allocation_strategy => bf | aobf | aoff | aoffcbf | aoffcaobf |
          ageffcaoff | ageffcbf | ageffcaobf |
          gf | af,

          goodfit => #{ max_block_search_depth => integer() },

          singleblock =>
              #{
                absolute_carrier_shrink_threshold => integer(),
                %% +Mummsbc
                max_mseg_carriers => integer(),
                %% +Mursbcmt
                relative_carrier_move_threshold => integer(),
                %% +Mursbcst
                relative_carrier_shrink_threshold => integer(),
                %% +Musbct
                carrier_threshold => integer()
               },
          multiblock =>
              #{
                %% +Mummbcs
                main_carrier_size => integer(),
                %% +Musmbc
                min_carrier_size => integer(),
                %% +Mulmbc
                max_carrier_size => integer(),
                %% +Mumbcgs
                growth_stages => integer(),
                %% +Mummmbc
                max_mseg_carriers => integer(),
                %% +Murmbcmt
                relative_carrier_move_threshold => integer()
               },
          %% +Muramv
          realloc_always_moves => boolean,
          %% +Mut
          global => boolean
         }.

-type memory() ::
         #{
           util => util_alloc_opts(),
           %% +MB
           binary_alloc => util_alloc_opts(),
           %% +MD
           std_alloc => util_alloc_opts(),
           %% +ME
           ets_alloc => util_alloc_opts(),
           %% +MF
           fix_alloc => util_alloc_opts(),
           %% +MH
           eheap_alloc => util_alloc_opts(),
           %% +ML
           ll_alloc => util_alloc_opts(),
           %% +MR
           driver_alloc => util_alloc_opts(),
           %% +MS
           sl_alloc => util_alloc_opts(),
           %% +MT
           temp_alloc => util_alloc_opts(),
           cache =>
               #{
                 %% +MMamcbf
                 absolute_max_bad_fit => integer(),
                 %% +MMrmcbf
                 relative_max_bad_fit => integer(),
                 %% +MMmcs
                 max_segments => 0..30
                },
           super_carrier =>
               #{
                 %% +MMsco
                 only => boolean(),
                 %% +MMscrfsd
                 reserved_free_segment_desciptors => integer(),
                 %% +MMscrpm
                 reserve_physical_memory => boolean(),
                 %% +MMscs
                 size => integer()
                },
           sys_alloc =>
               #{
                 %% +MYe
                 enabled => boolean(),
                 %% +MYm
                 malloc_library => libc,
                 %% +MYtt
                 trim_threshold_size => integer(),
                 %% +MYtp
                 top_pad_size => integer()
                },
           literal_alloc =>
               #{
                 %% +MIscs
                 super_carrier_size => integer()
                },
           config => min | max | r9c | r10b | r11b | config,
           alloc_util =>
               #{
                 %% +Muycs
                 sys_alloc_carrier_size => integer(),
                 %% +Mummc
                 max_mseg_alloc_carriers => integer(),
                 %% +Musac
                 use_sys_alloc => boolean()
                }
          }.

-type scheduler() ::
        #{
          online => 1..1024,
          available => 1..1024,
          busy_wait_threashold => none|very_short|short|medium|long|very_long,
          stacksize => 20..8192,
          wakeup_threshold => very_low|low|medium|high|very_high
         }.

-type schedulers() ::
        #{
          normal => scheduler(),
          dirty => #{ cpu => scheduler(),
                      io => scheduler() },
          %% +sbt
          bind_type =>
              default_bind | no_node_processor_spread |
          no_node_thread_spread | no_spread |
          processor_spread | spread | thread_spread |
          thread_no_node_processor_spread | unbound,
          load =>
              #{
                %% +scl
                compact => boolean(),
                %% +sub
                utilization => boolean()
               },
           %% +sct
           topology => string(),
           %% +sfwi
           forced_wakeup_interval => integer(),
           %% +spp
           port_parallelism => boolean(),
           %% +swct
           wake_cleanup_threashold => very_eager | eager | medium | lazy | very_lazy,
           %% +sws
           wakeup_strategy => default | legacy
         }.

start(_, []) ->
    Envs = application:get_all_env(),
    CmdLine =
        traverse(
          fun(V, {Fun, Format}, _Path, Acc) ->
                  case Fun() of
                      V ->
                          Acc;
                      _NotV ->
                          if is_function(Format) ->
                                  Format(V) ++ Acc;
                              true ->
                                  erlang:display({Format,V}),
                                  [io_lib:format(Format,[V]) | Acc]
                           end
                  end
          end, maps:from_list(Envs)),
    case CmdLine of
        [] ->
            {ok, self()};
        CmdLine ->
            {EmuArgs, ErlArgs} = lists:splitwith(
                                   fun(E) -> E =/= "--" end,
                                   erlang:system_info(emu_args)),
            {_, NewErlArgs} = lists:foldl(
                                fun(_, {true, Acc}) ->
                                        {false, Acc};
                                   ("-fdconfig",{false, Acc}) ->
                                        {true, Acc};
                                   ("-config",{false, Acc}) ->
                                        {true, Acc};
                                   (Arg, {false, Acc}) ->
                                        {false, [Arg | Acc]}
                                end, {false, []}, ErlArgs),
            AllEnv = [{App, application:get_all_env(App)} ||
                         {App,_,_} <- application:loaded_applications(),
                         App =/= erts],
            erts_internal:exec(EmuArgs ++ CmdLine ++ lists:reverse(NewErlArgs) ++ ["-fdconfig","3"],
                               3, list_to_binary(io_lib:format("~w.",[AllEnv]))),
            {ok, self()}
    end.

stop(_State) ->
    ok.

check_env(Env) ->
    check(Env, fun(Value, Path) -> check_env(Env, Value, Path) end).

check_env(Env, Online, [schedulers, dirty, cpu, online] = Path) ->
    Available = get_path([schedulers, dirty, cpu, available],
                         Env, erlang:system_info(dirty_cpu_schedulers)),
    if Online > Available ->
            [{error, invalid_value,
              fun() ->
                      io_lib:format(
                        "invalid value: ~p.~n  "
                        "Number of online schedulers (~p) must be less than or equal to available (~p)",
                        [Online, Online, Available])
              end, Path}];
       true ->
            []
    end;
check_env(_, _, _) ->
    [].

get_path([Key|S],Env,Default) ->
    case maps:find(Key, Env) of
        error  ->
            Default;
        {ok, Value} ->
            get_path(S, Value, Default)
    end;
get_path([], Val, _Default) ->
    Val.

%% check_env(Key, Value) ->
%%     case check_env(Key, Value) of
%%         ok ->
%%             ok
%%     end.

%% envmappings() ->
%%     #{ schedulers =>
%%            #{ normal => {integer, {1, 1024}},
%%               dirty =>
%%                   #{ io => {integer, {1, 1024}},
%%                      cpu => {integer, {1, 1024}}
%%                    },
%%               bind_type =>
%%                   {atom,
%%                    [default_bind, no_node_processor_spread,
%%                     no_node_thread_spread, no_spread,
%%                     processor_spread, spread, thread_spread,
%%                     thread_no_node_processor_spread, unbound]}
%%             }
%%        }.

mappings() ->
    #{ schedulers =>
           #{ normal =>
                  #{ online =>
                         {fun() -> erlang:system_info(schedulers_online) end,
                          fun(V) -> ["-S",integer_to_list(V)] end}
                   },
              dirty =>
                  #{ io =>
                         #{ online =>
                                {fun() -> erlang:system_info(dirty_io_schedulers) end,
                                 fun(V) -> ["-SDio",integer_to_list(V)] end}
                          },
                     cpu =>
                         #{ online =>
                                {fun() -> erlang:system_info(dirty_cpu_schedulers_online) end,
                                 fun(V) -> ["-SDcpu",integer_to_list(V)] end}
                          }
                   },
              bind_type => {fun() -> erlang:system_info(scheduler_bind_type) end,
                            fun(V) -> ["-sbt",sbt(V)] end}
            }
     }.

sbt(default_bind) ->
    sbt(thread_no_node_processor_spread);
sbt(no_node_processor_spread) ->
    "nnps";
sbt(no_node_thread_spread) ->
    "nnts";
sbt(no_spread) ->
    "ns";
sbt(processor_spread) ->
    "ps";
sbt(spread) ->
    "s";
sbt(thread_spread) ->
    "ts";
sbt(thread_no_node_processor_spread) ->
    "tnnps";
sbt(unbound) ->
    "ub".

traverse(Fun, KV) ->
    traverse(Fun, KV, mappings(), [], []).
traverse(Fun, KV, Mappings, Path, Acc) when is_map(Mappings), is_map(KV) ->
    maps:fold(
      fun(Key, Value, FoldAcc) ->
              case maps:find(Key, Mappings) of
                  {ok, MapValue} ->
                      traverse(Fun, Value, MapValue, [Key | Path], FoldAcc);
                  false ->
                      throw({error, {invalid_key,Key}})
              end
      end, Acc, KV);
traverse(Fun, LeftValue, RightValue, Path, Acc) ->
    Fun(LeftValue, RightValue, Path, Acc).
