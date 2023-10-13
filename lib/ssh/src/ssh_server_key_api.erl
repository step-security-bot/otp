%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2021. All Rights Reserved.
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

-module(ssh_server_key_api).
-moduledoc """
\-behaviour(ssh_server_key_api).

Behaviour describing the API for public key handling of an SSH server. By implementing the callbacks defined in this behavior, the public key handling of an SSH server can be customized. By default the SSH application implements this behavior with help of the standard OpenSSH files, see the [ssh(6)](ssh_app.md) application manual.
""".

-include_lib("public_key/include/public_key.hrl").
-include("ssh.hrl").

-export_type([daemon_key_cb_options/1]).

%%%****************************************************************
%%% The option key_cb_private is to pass options needed by other
%%% callback modules than the default ssh_file.erl
%%%
%%% If ssh:daemon(n, [ {key_cb_private, {hi,{there}}} ]
%%% is called, the term() will be {hi,{there}}

-doc """
Options provided to [ssh:daemon/2,3](`ssh:daemon/2`).

The option list given in the [`key_cb`](`t:ssh:key_cb_common_option/0`) option is available with the key `key_cb_private`.
""".
-type daemon_key_cb_options(T) :: [{key_cb_private,[T]} | ssh:daemon_option()].


%%%****************************************************************
%%% Fetch the host's private key that is of type Algorithm.

-doc """
Algorithm = [ssh:pubkey_alg()](`t:ssh:pubkey_alg/0`)  
  Host key algorithm.  
DaemonOptions = [daemon_key_cb_options()](`t:daemon_key_cb_options/1`)  
PrivateKey = [public_key:private_key()](`t:public_key:private_key/0`) | [crypto:engine_key_ref()](`t:crypto:engine_key_ref/0`)  
  Private key of the host matching the `Algorithm`. It may be a reference to a 'ssh-rsa', rsa-sha2-* or 'ssh-dss' (NOT ecdsa) key stored in a loaded Engine.  
Reason = term()  

Fetches the private key of the host.
""".
-doc(#{since => <<"OTP R16B">>}).
-callback host_key(Algorithm :: ssh:pubkey_alg(),
		   DaemonOptions :: daemon_key_cb_options(any())
                  ) ->
    {ok, PrivateKey :: public_key:private_key()} | {error, term()}.

%%%****************************************************************
%%% Check that PublicKey is known to be a public key for the User

-doc """
PublicUserKey = [public_key:public_key()](`t:public_key:public_key/0`)  
  Normally an RSA, DSA or ECDSA public key, but handling of other public keys can be added  
User = string()  
  User owning the public key.  
DaemonOptions = [daemon_key_cb_options()](`t:daemon_key_cb_options/1`)  
Result = boolean()  

Checks if the user key is authorized.
""".
-doc(#{since => <<"OTP R16B">>}).
-callback is_auth_key(PublicKey :: public_key:public_key(),
		      User :: string(),
		      DaemonOptions :: daemon_key_cb_options(any())
                     ) ->
    boolean().

