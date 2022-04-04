%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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
-module(interactive_shell_props).
-export([prop_crash/0]).

%%% This will include the .hrl file for the installed testing tool:
-include_lib("common_test/include/ct_property_test.hrl").
-import(interactive_shell_SUITE,
        [start_tmux/1,stop_tmux/1,send_tmux/3,check_location/2]).

-ifndef(FORALL).
-define(FORALL(A,B,C),ok).
-endif.

prop_crash() ->
    numtests(100,
             ?FORALL(Actions,actions(),
                     begin
                         Term = start_tmux([]),
                         try
                             io:format("Actions: ~p~n",[Actions]),
                             [send_tmux(Term,Action,Value) || {Action,Value} <- Actions],
                             send_tmux(Term,key,"Linefeed"),
                             send_tmux(Term,key,"Linefeed"),
                             check_location(Term,{0,0}),
                             stop_tmux(Term),
                             true
                         catch _:R ->
                                 io:format("Testing ~p error: ~n~p~n",[Actions, R]),
                                 false
                         after
                             ok
                         end
                     end)).

actions() ->
    ?LET(A,
         [as(),as(),as(),as(),as(),as(),as(),as(),as(),
          as(),as(),as(),as(),as(),as(),as(),as(),as()],
         lists:concat(A)).

as() ->
    list(
      {key,
       oneof(
         ["a","b",[16#1f600],[16#1F91A,16#1F3FC],
          %% "한","Z̤͔ͧ̑̓","ä͖̭̈̇","lͮ̒ͫ","ǧ̗͚̚","o̙̔ͮ̇͐̇",
          %% "{","}","[","]",":",[integer($\s,1114111)],
          "Ctrl+T", "Left", "Ctrl+R","Linefeed" %%, "Right","Home","End",
          %% "BackSpace","Delete"
         ])}).
