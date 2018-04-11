%%
%% %CopyrightBegin%
%%                                                                                                                                   
%% Copyright Ericsson AB 2009. All Rights Reserved.                                                                             
%% 
%% The contents of this file are subject to the Erlang Public License,                                                               
%% Version 1.1, (the "License"); you may not use this file except in                                                                 
%% compliance with the License. You should have received a copy of the                                                               
%% Erlang Public License along with this software. If not, it can be                                                                 
%% retrieved online at http://www.erlang.org/.                                                                                       
%% 
%% Software distributed under the License is distributed on an "AS IS"                                                               
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See                                                               
%% the License for the specific language governing rights and limitations                                                            
%% under the License.                                                                                                                
%% 
%% %CopyrightEnd%

%%%-------------------------------------------------------------------
%%% File    : msgq.erl
%%% Author  : Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% Description : Testing bad behavoir of msg_q
%%%
%%% Created :  1 Oct 2008 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------
-module(msgq).

-export([test/0, test/2]).
-export([benchmark_arguments/0, benchmark_unit/0]).

benchmark_arguments() ->
    [{test, [5000, 10]}].

benchmark_unit() -> "ms".

test() ->
    test(5000, 10).

test(Msgs, Sleep) ->
    
    Me = self(),
    Pid1 = spawn_link(fun() -> dispatcher(Me) end),
    register(dispatcher, Pid1),
    Pid2 = spawn_link(fun() -> worker(Sleep) end),
    register(worker, Pid2),
    bang_messages(Msgs),
    dispatcher ! {self(), continue},
    T0 = now(),
    receive {Pid1, done} -> ok end,
    T1 = now(),
    timer:now_diff(T1, T0)/1000.



%%%%%%%%%%%%%%%%%%% Dispatcher %%%%%%%%%%%%%%%%%%%%%

dispatcher(Parent) ->
    receive 
	{Parent, continue} ->
	    dispatch()
    end,
    Parent ! {self(), done}.

dispatch() ->
    receive 
	Msg -> 
	    call_worker(Msg),
	    dispatch()
    after 0 ->
	    ok
    end.
	       
%%%%%%%%%%%%%%%%%%  Worker %%%%%%%%%%%%%%%%%%%%%%%%%


call_worker(R) ->
    Proc = whereis(worker),
    Ref = erlang:monitor(process, Proc),
    worker ! {self(), R},
    receive
	{'DOWN', Ref, process, Proc, _Info} ->
            badarg;
	{Proc, Reply} ->
	    erlang:demonitor(Ref),
	    receive  
		{'DOWN', Ref, process, Proc, _Reason} ->
                    Reply
	    after 0 ->
                    Reply
	    end
    end.
		        
worker(Sleep) ->
    receive 
	{Pid, _Msg} ->
	    timer:sleep(Sleep),
	    Pid ! {self(), ok}
    end,
    worker(Sleep).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


bang_messages(Count) when Count > 0 ->
    dispatcher ! {do_some_work, Count},
    bang_messages(Count-1);
bang_messages(_) -> ok.
