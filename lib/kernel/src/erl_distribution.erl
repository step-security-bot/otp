%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(erl_distribution).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0,start_link/2,init/1,start/1,stop/0]).

-export([init_config/0,

         dist_proto/0, dist_auto_connect/0,
         dist_name/0, dist_hidden/0,
         dist_ticktime/0, dist_res/0,
         dist_listen/0,

         epmd_start/0, epmd_port/0,

         erl_epmd_port/0]).

%% Called during system start-up.

start_link() ->
    case dist_name() of
        {Name,ShortOrLong} ->
            start_link([Name,ShortOrLong,net_kernel:get_net_ticktime()],true);
        undefined ->
            ignore
    end.

%% Called from net_kernel:start/1 to start distribution after the
%% system has already started.

start(Args) ->
    C = {net_sup_dynamic, {?MODULE,start_link,[Args,false]}, permanent,
	 1000, supervisor, [erl_distribution]},
    supervisor:start_child(kernel_sup, C).

%% Stop distribution.

stop() ->
    case supervisor:terminate_child(kernel_sup, net_sup_dynamic) of
	ok ->
	    supervisor:delete_child(kernel_sup, net_sup_dynamic);
	Error ->
	    case whereis(net_sup) of
		Pid when is_pid(Pid) ->
		    %% Dist. started through -sname | -name flags
		    {error, not_allowed};
		_ ->
		    Error
	    end
    end.

%%%
%%% Internal helper functions.
%%%

%% Helper start function.

start_link(Args, CleanHalt) ->
    supervisor:start_link({local,net_sup}, ?MODULE, [Args,CleanHalt]).

init(NetArgs) ->
    {DistResMod,DistResArg} = dist_res(),

    [erlang:open_port({spawn_executable,os:find_executable("epmd")},
                      [hide,{args,["-daemon"]}])
     || erl_distribution:epmd_start() ],

    DistResSpecs = DistResMod:childspecs(DistResArg),

    Auth = {auth,{auth,start_link,[]},permanent,2000,worker,[auth]},
    Kernel = {net_kernel,{net_kernel,start_link,NetArgs},
	      permanent,2000,worker,[net_kernel]},
    EarlySpecs = net_kernel:protocol_childspecs(),
    {ok,{{one_for_all,0,1}, EarlySpecs ++ DistResSpecs ++ [Auth,Kernel]}}.


%%
%% Move all backwards compatibility options into application parameters
%%
init_config() ->

    case init:get_argument(proto_dist) of
        {ok, [Protos]} ->
            application:set_env(kernel,dist_proto,Protos);
        _ ->
            ok
    end,
        
    FindDistName =
        fun(Arg,Flag) ->
                case init:get_argument(Arg) of
                    {ok,[[SName]|Rest]} ->
                        [?LOG_WARNING("Multiple -~p given to erl, using the first, ~p",
                                      [Arg, SName]) || Rest =/= []],
                        application:set_env(kernel,dist_name,{SName,Flag});
                    _ ->
                        ok
                end
        end,

    %% We check longnames first so that any -sname takes priority
    FindDistName(name,longnames),
    FindDistName(sname,shortnames),

    case init:get_argument(hidden) of
        {ok, [[]]} ->
            application:set_env(kernel,dist_hidden,true);
        _ ->
            ok
    end,

    case application:get_env(kernel,net_ticktime) of
        {ok, NetTicktime} ->
            application:set_env(kernel,dist_ticktime,NetTicktime);
        _ ->
            ok
    end,

    case init:get_argument(epmd_module) of
        {ok, [[Mod]]} ->
            application:set_env(kernel,dist_res,{inet_dist_res,[list_to_atom(Mod)]});
        _ ->
            ok
    end,

    case dist_name() of
        {_,_} -> application:set_env(kernel,epmd_start,true);
        _ -> ok
    end,

    case init:get_argument(start_epmd) of
        {ok, [StartEpmd]} ->
            application:set_env(kernel,epmd_start,list_to_atom(hd(lists:reverse(StartEpmd))));
        _ ->
            ok
    end,

    ok.

%% Convenience functions to get kernel dist application parameters
dist_proto() ->
    get_env(?FUNCTION_NAME).
dist_auto_connect() ->
    get_env(?FUNCTION_NAME).
dist_name() ->
    get_env(?FUNCTION_NAME).
dist_hidden() ->
    get_env(?FUNCTION_NAME).
dist_ticktime() ->
    get_env(?FUNCTION_NAME).
dist_res() ->
    get_env(?FUNCTION_NAME).
dist_listen() ->
    get_env(?FUNCTION_NAME).
epmd_start() ->
    get_env(?FUNCTION_NAME).
epmd_port() ->
    get_env(?FUNCTION_NAME).
erl_epmd_port() ->
    get_env(?FUNCTION_NAME).

get_env(Var) ->
    erlang:display({Var,application:get_all_env()}),
    {ok, Val} = application:get_env(kernel, Var),
    Val.
