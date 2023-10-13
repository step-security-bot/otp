%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2019. All Rights Reserved.
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

-module(snmpm_network_interface).
-moduledoc """
Behaviour module for the SNMP manager network interface.

This module defines the behaviour of the manager network interface. A `snmpm_network_interface` compliant module must export the following functions:

* [start_link/2](`m:snmpm_network_interface#start_link`)
* [stop/1](`m:snmpm_network_interface#stop`)
* [send_pdu/7](`m:snmpm_network_interface#send_pdu`)
* [inform_response/4](`m:snmpm_network_interface#inform_response`)
* [note_store/2](`m:snmpm_network_interface#note_store`)
* [info/1](`m:snmpm_network_interface#info`)
* [get_log_type/1](`m:snmpm_network_interface#get_log_type`)
* [set_log_type/2](`m:snmpm_network_interface#set_log_type`)
* [verbosity/2](`m:snmpm_network_interface#verbosity`)

The semantics of them and their exact signatures are explained below.

Legacy API function `c:send_pdu/7` that has got separate `IpAddr` and `PortNumber` arguments still works as before for backwards compatibility reasons.

[](){: id=start_link }
""".

-doc """
Server = pid()  
NoteStore = pid()  

Start-link the network interface process.

`Server` is the pid of the managing process.

`NoteStore` is the pid of the note-store process.

[](){: id=stop }
""".
-callback start_link(Server, NoteStore) ->
    {ok, Pid} | {error, Reason} when
      Server    :: pid(),
      NoteStore :: pid(),
      Pid       :: pid(),
      Reason    :: term().

-doc """
Pid = pid()  

Stop the network interface process.

[](){: id=send_pdu }
""".
-callback stop(Pid) ->
    snmp:void() when
      Pid :: pid().

-doc """
Pid = pid()  
Pdu = pdu()  
Vsn = 'version-1' | 'version-2' | 'version-3'  
MsgData = term()  
Domain = transportDomainUdpIpv4 | transportDomainUdpIpv6  
Addr = \{[inet:ip_address(), inet:port_number()](`t:inet:ip_address/0`)\}  
ExtraInfo = term()  

Request the network interface process (`Pid`) to send this pdu (`Pdu`).

`ExtraInfo` is some opaque data that is passed to the net-if process. It originates from the `ExtraInfo` parameter in the calls to the [synchronous get-request](`m:snmpm#sync_get2`), [asynchronous get-request](`m:snmpm#async_get2`), [synchronous get-next-request](`m:snmpm#sync_get_next2`), [asynchronous get-next-request](`m:snmpm#async_get_next2`), [synchronous set-request](`m:snmpm#sync_set2`) and [asynchronous set-request](`m:snmpm#async_set2`) functions. Whether the net-if process chooses to use this is implementation dependent. The net-if process included in this application ignores it.

[](){: id=inform_response }
""".
-callback send_pdu(Pid, Pdu, Vsn, MsgData, Domain, Addr, ExtraInfo) ->
    snmp:void() when
      Pid       :: pid(),
      Pdu       :: snmp:pdu(),
      Vsn       :: 'version-1' | 'version-2' | 'version-3',
      MsgData   :: term(),
      Domain    :: snmp:tdomain(),
      Addr      :: {inet:ip_address(), inet:port_number()},
      ExtraInfo :: term().

-doc """
Pid = pid()  
Ref = term()  
Addr = address()  
Port = integer()  

Instruct the network interface process to send the response (acknowledgment) to an inform-request.

`Ref` is something that can be used to identify the inform-request, e.g. request-id of the inform-request.

`Addr` and `Port` identifies the agent, from which the inform-request originated.

[](){: id=note_store }
""".
-callback inform_response(Pid, Ref, Addr, Port) ->
    snmp:void() when
      Pid  :: pid(),
      Ref  :: term(),
      Addr :: inet:ip_address(),
      Port :: inet:port_number().

-doc """
Pid = pid()  
NoteStore = pid()  

Change the pid of the note-store process. This is used when the server re-starts the note_store (e.g. after a crach).

[](){: id=info }
""".
-callback note_store(Pid, NoteStore) ->
    snmp:void() when
      Pid       :: pid(),
      NoteStore :: pid().

-doc """
Pid = pid()  

The info returned is basically up to the implementer to decide. The implementation provided by this application provides info about memory allocation and various socket information.

The info returned by this function is returned together with other info collected by the manager when the [info](`m:snmpm#info`) function is called (tagged with the key `net_if`).

[](){: id=verbosity }
""".
-callback info(Pid) ->
    Info when
      Pid   :: pid(),
      Info  :: [{Key, Value}],
      Key   :: term(),
      Value :: term().

-doc """
Pid = pid()  
Verbosity = verbosity()  

Change the verbosity of the network interface process.

[](){: id=get_log_type }
""".
-callback verbosity(Pid, Verbosity) ->
    snmp:void() when
      Pid       :: pid(),
      Verbosity :: snmp:verbosity().

-doc """
Pid = pid()  
LogType = atl_type()  
Reason = term()  

The Audit Trail Log is managed by the network interface process. So, it is this process that has to return the actual log-type.

[](){: id=set_log_type }
""".
-callback get_log_type(Pid) ->
    {ok, LogType} | {error, Reason} when
      Pid     :: pid(),
      LogType :: snmp:atl_type(),
      Reason  :: term().

-doc """
Pid = pid()  
NewType = OldType = atl_type()  
Reason = term()  

The Audit Trail Log is managed by the network interface process. So, it is this process that has to do the actual changing of the type.

See [set_log_type](`m:snmpm#set_log_type`) for more info.
""".
-callback set_log_type(Pid, NewType) ->
    {ok, OldType} | {error, Reason} when
      Pid     :: pid(),
      NewType :: snmp:atl_type(),
      OldType :: snmp:atl_type(),
      Reason  :: term().


