%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2021. All Rights Reserved.
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

%%
%% Description: a gen_server implementing a simple
%% terminal (using the group module) for a CLI
%% over SSH

-module(ssh_server_channel).
-moduledoc """
\-behaviour(ssh_server_channel). (Replaces ssh_daemon_channel)

> #### Note {: class=info }
> This module replaces ssh_daemon_channel.
>
> The old module is still available for compatibility, but should not be used for new programs. The old module will not be maintained except for some error corrections

SSH services (clients and servers) are implemented as channels that are multiplexed over an SSH connection and communicates over the [SSH Connection Protocol](http://www.ietf.org/rfc/rfc4254.txt). This module provides a callback API that takes care of generic channel aspects for daemons, such as flow control and close messages. It lets the callback functions take care of the service (application) specific parts. This behavior also ensures that the channel process honors the principal of an OTP-process so that it can be part of a supervisor tree. This is a requirement of channel processes implementing a subsystem that will be added to the `ssh` applications supervisor tree.

> #### Note {: class=info }
> When implementing a client subsystem handler, use [\-behaviour(ssh_client_channel)](`m:ssh_client_channel`) instead.
""".

%% API to server side channel that can be plugged into the erlang ssh daemeon
-doc """
Args = term()  
  Last argument to `start_link/4`.  
State = term()  
Reason = term()  

Makes necessary initializations and returns the initial channel state if the initializations succeed.

The time-out values that can be returned have the same semantics as in a `m:gen_server`. If the time-out occurs, `c:handle_msg/2` is called as `c:handle_msg(timeout, State)`.
""".
-doc(#{title => <<"Callback Functions">>,since => <<"OTP 21.0">>}).
-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.

-doc """
Reason = term()  
State = term()  

This function is called by a channel process when it is about to terminate. Before this function is called, [ssh_connection:close/2 ](`ssh_connection:close/2`)is called, if it has not been called earlier. This function does any necessary cleaning up. When it returns, the channel process terminates with reason `Reason`. The return value is ignored.
""".
-doc(#{title => <<"Callback Functions">>,since => <<"OTP 21.0">>}).
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().

-doc """
Msg = timeout | term()  
ChannelId = [ssh:channel_id()](`t:ssh:channel_id/0`)  
State = term()  

Handles other messages than SSH Connection Protocol, call, or cast messages sent to the channel.

Possible Erlang 'EXIT' messages is to be handled by this function and all channels are to handle the following message.

* __`{ssh_channel_up, `[ssh:channel_id()](`t:ssh:channel_id/0`)`, `[ssh:connection_ref()](`t:ssh:connection_ref/0`)`}`__ - This is the first message that the channel receives. This is especially useful if the server wants to send a message to the client without first receiving a message from it. If the message is not useful for your particular scenario, ignore it by immediately returning `{ok, State}`.
""".
-doc(#{title => <<"Callback Functions">>,since => <<"OTP 21.0">>}).
-callback handle_msg(Msg ::term(), State :: term()) ->
    {ok, State::term()} | {stop, ChannelId::ssh:channel_id(), State::term()}. 
-doc """
Msg = [ssh_connection:event()](`t:ssh_connection:event/0`)  
ChannelId = [ssh:channel_id()](`t:ssh:channel_id/0`)  
State = term()  

Handles SSH Connection Protocol messages that may need service-specific attention. For details, see [ssh_connection:event()](`t:ssh_connection:event/0`).

The following message is taken care of by the `ssh_server_channel` behavior.

* __`{closed, `[ssh:channel_id()](`t:ssh:channel_id/0`)`}`__ - The channel behavior sends a close message to the other side, if such a message has not already been sent. Then it terminates the channel with reason `normal`.
""".
-doc(#{title => <<"Callback Functions">>,since => <<"OTP 21.0">>}).
-callback handle_ssh_msg(ssh_connection:event(),
			 State::term()) -> {ok, State::term()} |
					   {stop, ChannelId::ssh:channel_id(),
					    State::term()}.

%%% Internal API
-export([start_link/5,
         get_print_info/1, get_print_info/2
        ]).

start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec) ->
    ssh_client_channel:start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec).


get_print_info(Pid) ->
    ssh_client_channel:get_print_info(Pid).

get_print_info(Pid, Arg) ->
    ssh_client_channel:get_print_info(Pid, Arg).

