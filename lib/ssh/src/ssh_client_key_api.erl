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

-module(ssh_client_key_api).
-moduledoc """
\-behaviour(ssh_client_key_api).

Behavior describing the API for public key handling of an SSH client. By implementing the callbacks defined in this behavior, the public key handling of an SSH client can be customized. By default the `ssh` application implements this behavior with help of the standard OpenSSH files, see the [ssh(6)](ssh_app.md) application manual.
""".

-include_lib("public_key/include/public_key.hrl").
-include("ssh.hrl").

-export_type([client_key_cb_options/1]).

%%%****************************************************************
%%% The option key_cb_private is to pass options needed by other
%%% callback modules than the default ssh_file.erl
%%%
%%% If ssh:connect(x, n, [ {key_cb_private, {hi,{there}}} ]
%%% is called, the term() will be {hi,{there}}

-doc """
Options provided to [ssh:connect/\[3,4]](`ssh:connect/3`).

The option list given in the [`key_cb`](`t:ssh:key_cb_common_option/0`) option is available with the key `key_cb_private`.
""".
-type client_key_cb_options(T) :: [{key_cb_private,[T]} | ssh:client_option()].


%%%****************************************************************
%%% Checks if the public key Key is a host key for (any of) the
%%% host(s) in the argument Host with the port Port.
%%%
%%% Due to compatibility reasons, the OTP/SSH application first
%%% checks is_host_key/4 and then the old is_host_key/3

-doc """
Key = [public_key:public_key()](`t:public_key:public_key/0`)  
  Normally an RSA, DSA or ECDSA public key, but handling of other public keys can be added.  
Host = [inet:ip_address()](`t:inet:ip_address/0`) | [inet:hostname()](`t:inet:hostname/0`) | [ [inet:ip_address()](`t:inet:ip_address/0`) | [inet:hostname()](`t:inet:hostname/0`) ]  
  Description of the host with one or more IP addresses or hostnames.  
Port = [inet:port_number()](`t:inet:port_number/0`)  
  The Port number of the host.  
Algorithm = [ssh:pubkey_alg()](`t:ssh:pubkey_alg/0`)  
  Host key algorithm.  
ConnectOptions = [client_key_cb_options()](`t:client_key_cb_options/1`)  
Result = boolean() | \{error, Error::term()\}  
  The exact error message depends on the actual callback module. The Error message makes the connection to fail, and is returned from e.g ssh:connect/3.  

Checks if a host key is trusted.

This function is preferred to the old [`Module:is_host_key/4`](`c:is_host_key/4`) since it also uses the peer host port number and may return an error message.

The OTP/SSH application first calls this function in the callback module, and then the old [`Module:is_host_key/4`](`c:is_host_key/4`) for compatibility.
""".
-doc(#{since => <<"OTP 23.0">>}).
-callback is_host_key(Key :: public_key:public_key(),
                      Host :: inet:ip_address() | inet:hostname()
                            | [inet:ip_address() | inet:hostname()],
                      Port :: inet:port_number(),
		      Algorithm :: ssh:pubkey_alg(),
                      Options :: client_key_cb_options(any())
                     ) ->
    boolean() | {error, term()} .

%%% is_host_key/4 is an old variant which is kept for compatibility.
%%% Use is_host_key/5 in new programs.

-doc """
Key = [public_key:public_key()](`t:public_key:public_key/0`)  
  Normally an RSA, DSA or ECDSA public key, but handling of other public keys can be added.  
Host = string()  
  Description of the host.  
Algorithm = [ssh:pubkey_alg()](`t:ssh:pubkey_alg/0`)  
  Host key algorithm.  
ConnectOptions = [client_key_cb_options()](`t:client_key_cb_options/1`)  
Result = boolean()  

This function is retired in favour for [`Module:is_host_key/5`](`c:is_host_key/5`) which is the preferred API function. The calling SSH application will still try the `c:is_host_key/4` if the call to `c:is_host_key/5` failed.

Checks if a host key is trusted.
""".
-doc(#{since => <<"OTP R16B">>}).
-callback is_host_key(Key :: public_key:public_key(),
                      Host :: string(),
                      Algorithm :: ssh:pubkey_alg(),
                      Options :: client_key_cb_options(any())
                     ) ->
    boolean().

-optional_callbacks(
   [is_host_key/4, is_host_key/5     % One in the pair must be defined
   ]).


%%%****************************************************************
%%% Fetch the user's private key that is of type Algorithm.

-doc """
Algorithm = [ssh:pubkey_alg()](`t:ssh:pubkey_alg/0`)  
  Host key algorithm.  
ConnectOptions = [client_key_cb_options()](`t:client_key_cb_options/1`)  
Result = \{ok, [public_key:private_key()](`t:public_key:private_key/0`)\} | \{ok, \{ssh2_pubkey, PubKeyBlob :: binary()\}\} | \{error, term()\}  

Fetches the users *public key* matching the `Algorithm`. Some key callback modules may return `{ssh2_pubkey, PubKeyBlob :: binary()}`.

> #### Note {: class=info }
> The private key contains the public key.
""".
-doc(#{since => <<"OTP R16B">>}).
-callback user_key(Algorithm :: ssh:pubkey_alg(),
                   Options :: client_key_cb_options(any())
                  ) ->
    {ok, public_key:private_key()} |
    {ok, {ssh2_pubkey, PubKeyBlob :: binary()}} |
    {error, string()}.


%%%****************************************************************
%%% Remembers that the the public key Key is a key for the host(s)
%%% in the argument Host with the port Port.
%%%
%%% Due to compatibility reasons, the OTP/SSH application first
%%% tries add_host_key/4 and then the old add_host_key/3

-doc """
Host = [inet:ip_address()](`t:inet:ip_address/0`) | [inet:hostname()](`t:inet:hostname/0`) | [ [inet:ip_address()](`t:inet:ip_address/0`) | [inet:hostname()](`t:inet:hostname/0`) ]  
  The host that owns the `PublicHostKey`. One or more IP addresses or hostnames.  
Port = [inet:port_number()](`t:inet:port_number/0`)  
  The Port number of the Host.  
PublicHostKey = [public_key:public_key()](`t:public_key:public_key/0`)  
  Of ECDSA keys, only the Normally an RSA, DSA or ECDSA public key, but handling of other public keys can be added.  
ConnectOptions = [client_key_cb_options()](`t:client_key_cb_options/1`)  

Adds a host key to the set of trusted host keys.

This function is preferred to the old [`Module:add_host_key/3`](`c:add_host_key/3`) since it also uses the peer host port number and may return an error message.

The OTP/SSH application first calls this function in the callback module, and then the old [`Module:add_host_key/3`](`c:add_host_key/3`) for compatibility.
""".
-doc(#{since => <<"OTP R16B">>}).
-callback add_host_key(Host :: inet:ip_address() | inet:hostname()
                             | [inet:ip_address() | inet:hostname()],
                       Port :: inet:port_number(),
                       PublicKey :: public_key:public_key(),
                       Options :: client_key_cb_options(any())
                      ) ->
    ok | {error, term()}.

%%% is_host_key/3 is an old variant which is kept for compatibility.
%%% Use is_host_key/4 in new constructions.

-doc """
HostNames = string()  
  Description of the host that owns the `PublicHostKey`.  
PublicHostKey = [public_key:public_key()](`t:public_key:public_key/0`)  
  Of ECDSA keys, only the Normally an RSA, DSA or ECDSA public key, but handling of other public keys can be added.  
ConnectOptions = [client_key_cb_options()](`t:client_key_cb_options/1`)  

This function is retired in favour for [`Module:add_host_key/4`](`c:add_host_key/4`) which is the preferred API function. The calling SSH application will still try the `c:add_host_key/3` if the call to `c:add_host_key/4` failed.

Adds a host key to the set of trusted host keys.
""".
-doc(#{since => <<"OTP R16B">>}).
-callback add_host_key(Host :: string(),
                       PublicKey :: public_key:public_key(),
                       Options :: client_key_cb_options(any())
                      ) ->
    ok | {error, term()}.

-optional_callbacks(
   [add_host_key/3, add_host_key/4       % One in the pair be defined
   ]).


%%%****************************************************************
%%% Sign the SigData with the *private* key corresponding to PubKeyBlob
%%%

-callback sign(PubKeyBlob :: binary(),
               SigData :: binary(),
               Options :: client_key_cb_options(any())) ->
    Blob :: binary().

-optional_callbacks([sign/3]).


