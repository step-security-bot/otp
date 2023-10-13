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
-module(snmpa_mib_data).
-moduledoc """
Behaviour module for the SNMP agent mib-server data module.

This module defines the behaviour of the SNMP agent mib-server data module. A `snmpa_mib_data` compliant module must export the following functions:

* [new/1](`m:snmpa_mib_data#new`)
* [close/1](`m:snmpa_mib_data#close`)
* [sync/1](`m:snmpa_mib_data#sync`)
* [load_mib/4](`m:snmpa_mib_data#load_mib`)
* [unload_mib/4](`m:snmpa_mib_data#unload_mib`)
* [lookup/2](`m:snmpa_mib_data#lookup`)
* [next/3](`m:snmpa_mib_data#next`)
* [register_subagent/3](`m:snmpa_mib_data#register_subagent`)
* [unregister_subagent/2](`m:snmpa_mib_data#unregister_subagent`)
* [which_mib/2](`m:snmpa_mib_data#which_mib`)
* [which_mibs/1](`m:snmpa_mib_data#which_mibs`)
* [whereis_mib/2](`m:snmpa_mib_data#whereis_mib`)
* [dump/2](`m:snmpa_mib_data#dump`)
* [info/1](`m:snmpa_mib_data#info`)
* [backup/2](`m:snmpa_mib_data#backup`)
* [code_change/4](`m:snmpa_mib_data#code_change`)

The semantics of them and their exact signatures are explained below.

Note that the data extracted from the imported (loaded) mibs are stored partly by the mib-server and partly by the symbolic-store server. See the default mib-server data module, `snmpa_mib_data_tttn` for details.
""".

-include_lib("snmp/include/snmp_types.hrl").

%%%-----------------------------------------------------------------
%%% This is the behaviour for the MIB server backend internal 
%%% data storage. 
%%%-----------------------------------------------------------------

%% These types should really be defined elsewhere...
-export_type([
	      mib_view/0, 
	      mib_view_elem/0, 
	      mib_view_mask/0, 
	      mib_view_inclusion/0
	     ]).

-type mib_view()           :: [mib_view_elem()].
-type mib_view_elem()      :: {SubTree :: snmp:oid(), 
			       Mask :: [non_neg_integer()], 
			       Inclusion :: mib_view_inclusion()}.
-type mib_view_mask()      :: [non_neg_integer()]. 
-type mib_view_inclusion() :: 1 | 2. % 1 = included, 2 = excluded

-type filename() :: file:filename().


-doc """
Storage = mib_storage()  
State = term()  

Create a new mib-server data instance.

[](){: id=close }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback new(MibStorage :: snmpa:mib_storage()) -> State :: term().

-doc """
State = term()  

Close the mib-storage.

[](){: id=sync }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback close(State :: term()) -> ok.

-doc """
State = term()  

Synchronize (write to disc, if possible) the mib-server data. This depends on the `mib_storage` option, and will only have an effect if the mib-storage option has an actual disc component (such as dets, or ets with a file).

[](){: id=load_mib }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback sync(State :: term()) -> ok.

-doc """
State = NewState = term()  
Filename = filename()  
MeOverride = boolean()  
TeOverride = boolean()  
Reason = already_loaded | term()  

Load the mib specified by the `Filename` argument into the mib-server. The `MeOverride` and `TeOverride` arguments specifies how the mib-server shall handle duplicate mib- and trap- entries.

[](){: id=unload_mib }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback load_mib(State :: term(), FileName :: filename(),
		   MeOverride :: boolean(), 
		   TeOverride :: boolean()) -> 
    {ok, NewState :: term()} | {error, Reason :: already_loaded | term()}.

-doc """
State = NewState = term()  
Filename = filename()  
MeOverride = boolean()  
TeOverride = boolean()  
Reason = not_loaded | term()  

Unload the mib specified by the `Filename` argument from the mib-server. The `MeOverride` and `TeOverride` arguments specifies how the mib-server shall handle duplicate mib- and trap- entries.

[](){: id=lookup }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback unload_mib(State :: term(), FileName :: filename(),
		   MeOverride :: boolean(), 
		   TeOverride :: boolean()) -> 
    {ok, NewState :: term()} | {error, Reason :: not_loaded | term()}.

-doc """
State = term()  
Reply = \{variable, ME\} | \{table_column, ME, TEOid\} | \{subagent, SAPid, SAOid\} | \{false, Reason\}  
Oid = TEOid = SAOid = oid()  
SAPid = pid()  
ME = me()  
Reason = term()  

Find the mib-entry corresponding to the `Oid`. If it is a variable, the `Oid` must be <Oid for var>.0 and if it is a table, `Oid` must be <table>.<entry>.<col>.<any>.

[](){: id=next }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback lookup(State :: term(), Oid :: snmp:oid()) -> 
    {false, Reason :: term()} | 
    {variable, MibEntry :: snmpa:me()} |
    {table_column, MibEntry :: snmpa:me(), TableEntryOid :: snmp:oid()} |
    {subagent, SubAgentPid :: pid(), SAOid :: snmp:oid()}.

-doc """
State = term()  
Reply = false | endOfTable | \{subagent, SAPid, SAOid\} | \{variable, ME, VarOid\} | \{table, TableOid, TableRestOid, ME\}  
Oid = SAOid = VarOid = TableOid = TableRestOid = oid()  
SAPid = pid()  
ME = me()  

Finds the lexicographically next oid.

[](){: id=register_subagent }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback next(State :: term(), Oid :: snmp:oid(), MibView :: mib_view()) -> 
    endOfView | false | 
    {subagent, SubAgentPid :: pid(), SAOid :: snmp:oid()} |
    {variable, MibEntry :: snmpa:me(), VarOid :: snmp:oid()} |
    {table, TableOid :: snmp:oid(), TableRestOid :: snmp:oid(), MibEntry :: snmpa:me()}.

-doc """
State = NewState = term()  
Reply = \{ok, NewState\} | \{error, Reason\}  
Oid = oid()  
Pid = pid()  
Reason = term()  

Register the subagent, process, handling part of the mib-tree.

[](){: id=unregister_subagent }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback register_subagent(State :: term(), 
			    Oid   :: snmp:oid(), 
			    Pid   :: pid()) -> 
    {ok, NewState :: term()} | {error, Reason :: term()}.

-doc """
State = NewState = term()  
Reply = \{ok, NewState\} | \{ok, NewState, Pid\} | \{error, Reason\}  
PidOrOid = pid() | oid()  
Pid = pid()  
Reason = term()  

Unregister the subagent, handling part of the mib-tree, as specified by the `oid()` or `t:pid()` (`PidOrOid`).

When unregister the subagent using an `oid()`, the `t:pid()` of the process handling the sub-tree is also returned.

[](){: id=dump }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback unregister_subagent(State :: term(), 
			      PidOrOid :: pid() | snmp:oid()) -> 
    {ok, NewState :: term()}               | % When second arg was a pid()
    {ok, NewState :: term(), Pid :: pid()} | % When second arg was a oid()
    {error, Reason :: term()}.

-doc """
State = term()  
Reply = ok | \{error, Reason\}  
Destination = io | filename()  
Pid = pid()  
Reason = term()  

Dump the mib-server data to `stdio` (Destination = `io`) or the specified file.

[](){: id=which_mib }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback dump(State :: term(), Destination :: io | filename()) -> 
    ok | {error, Reason :: term()}.

-doc """
State = term()  
Reply = \{ok, MibFile\} | \{error, Reason\}  
Oid = oid()  
MibFile = string()  
Reason = term()  

Retrieve the mib-file to which an given `oid()` belongs.

[](){: id=which_mibs }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback which_mib(State :: term(), Oid :: snmp:oid()) -> 
    {ok, Mib :: string()} | {error, Reason :: term()}.

-doc """
State = term()  
Reply = \[\{MibName, Filename\}]  
MibName = atom()  
Filename = string()  

Retrieve all loaded mib-files.

[](){: id=whereis_mib }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback which_mibs(State :: term()) -> 
    [{MibName :: atom(), Filename :: filename()}].

-doc """
State = term()  
MibName = atom()  
Reply = \{ok, Filename\} | \{error, Reason\}  
Filename = string()  
Reason = term()  

Retrieve the mib file for the mib.

[](){: id=info }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback whereis_mib(State :: term(), MibName :: atom()) -> 
    {ok, Filename :: filename()} | {error, Reason :: term()}.

-doc """
State = term()  
Reply = \{ok, Filename\} | \{error, Reason\}  
Filename = string()  
Reason = term()  

Retrieve misc info for the mib data.

This is a utility function used to inspect, for instance, memory usage, in a simple way.

[](){: id=backup }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback info(State :: term()) -> list().

-doc """
State = term()  
Reply = ok | \{error, Reason\}  
BackupDir = string()  
Reason = term()  

Perform a backup of the mib-server data.

Note that its implementation dependent (and also dependent on mib-storage is used) if a backup is possible.

[](){: id=code_change }
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback backup(State :: term(), BackupDir :: string()) -> 
    ok |  {error, Reason :: term()}.

-doc """
Destination = up | down  
Vsn = term()  
Extra = term()  
State = NewState = term()  

Perform a code-change (upgrade or downgrade).

See `m:gen_server` for more info regarding the `Vsn` and `Extra` arguments.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback code_change(Direction :: up | down, 
		      Vsn :: term(), 
		      Extra :: term(), 
		      State :: term()) -> 
    NewState :: term().

%% Backwards-compatibility callback
-callback unload_mib(State, Filename) -> {ok, NewState} | {error, Reason} when
      State :: term(),
      Filename :: filename(),
      NewState :: term(),
      Reason :: not_loaded | term().

-optional_callbacks([unload_mib/2]).


