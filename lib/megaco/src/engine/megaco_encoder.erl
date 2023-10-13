%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2019. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Megaco encoder behaviour module
%%----------------------------------------------------------------------

-module(megaco_encoder).
-moduledoc """
Megaco encoder behaviour.

The following functions should be exported from a `megaco_encoder` callback module:

## DATA TYPES

> #### Note {: class=info }
> Note that the actual definition of (some of) these records depend on the megaco protocol version used. For instance, the `'TransactionReply'` record has two more fields in version 3, so a simple erlang type definition cannot be made here.

```text
protocol_version() = integer()
segment_no()       = integer()
megaco_message() = #'MegacoMessage{}'
transaction() = {transactionRequest,     transaction_request()}      |
                {transactionPending,     transaction_reply()}        |
                {transactionReply,       transaction_pending()}      |
                {transactionResponseAck, transaction_response_ack()} |
                {segmentReply,           segment_reply()}
transaction_request() = #'TransactionRequest'{}
transaction_pending() = #'TransactionPending'{}
transaction_reply() = #'TransactionReply'{}
transaction_response_ack() = [transaction_ack()]
transaction_ack() = #'TransactionAck'{}
segment_reply() = #'SegmentReply'{}
action_request() = #'ActionRequest'{}
action_reply() = #'ActionReply'{}
command_request() = #'CommandRequest'{}
error_desc()   = #'ErrorDescriptor'{}
```

[](){: id=encode_message }
""".

-export_type([
              protocol_version/0,
              segment_no/0,
              megaco_message/0,
              transaction/0,
              transaction_request/0,
              transaction_pending/0,
              transaction_reply/0,
              transaction_response_ack/0,
              transaction_ack/0,
              segment_reply/0,
              action_request/0,
              action_reply/0,
              command_request/0,
              error_desc/0
             ]).


-include("megaco_message_internal.hrl").

-type protocol_version() :: integer().
-type segment_no()       :: integer().
-type megaco_message() :: #'MegacoMessage'{}.
-type transaction()    :: {transactionRequest,     transaction_request()}      |
                          {transactionPending,     transaction_reply()}        |
                          {transactionReply,       transaction_pending()}      |
                          {transactionResponseAck, transaction_response_ack()} |
                          {segmentReply,           segment_reply()}.
-type transaction_request()      :: #'TransactionRequest'{}.
-type transaction_pending()      :: #'TransactionPending'{}.
%% The problem with TransactionReply is that its definition depend
%% on which version of the protocol we are using. As of version 3,
%% it has two more fields.
%% -type transaction_reply()        :: #'TransactionReply'{}.
-type transaction_reply()        :: {'TransactionReply', _, _} |
                                    {'TransactionReply', _, _, _, _}.
-type transaction_response_ack() :: [transaction_ack()].
-type transaction_ack()          :: #'TransactionAck'{}.
-type segment_reply()            :: #'SegmentReply'{}.
%% -type action_request()           :: #'ActionRequest'{}.
-type action_request()           :: {'ActionRequest', _, _, _, _}.
%% -type action_reply()             :: #'ActionReply'{}.
-type action_reply()             :: {'ActionReply', _, _, _}.
%% -type command_request()           :: #'CommandRequest'{}.
-type command_request()          :: {'CommandRequest', _, _, _}.
-type error_desc()               :: #'ErrorDescriptor'{}.

-doc """
EncodingConfig = list()  
Version = integer()  
Message = megaco_message()  
Bin = binary()  
Error = term()  

Encode a megaco message.

[](){: id=decode_message }
""".
-callback encode_message(EncodingConfig,
                         Version,
                         Message) -> {ok, Bin} | Error when
      EncodingConfig :: list(),
      Version        :: protocol_version(),
      Message        :: megaco_message(),
      Bin            :: binary(),
      Error          :: term().

-doc """
EncodingConfig = list()  
Version = integer() | dynamic  
Message = megaco_message()  
Bin = binary()  
Error = term()  

Decode a megaco message.

Note that if the Version argument is `dynamic`, the decoder should try to figure out the actual version from the message itself and then use the proper decoder, e.g. version 1.  
If on the other hand the Version argument is an integer, it means that this is the expected version of the message and the decoder for that version should be used.

[](){: id=decode_mini_message }
""".
-callback decode_message(EncodingConfig,
                         Version,
                         Bin) -> {ok, Message} | Error when
      EncodingConfig :: list(),
      Version        :: protocol_version() | dynamic,
      Bin            :: binary(),
      Message        :: megaco_message(),
      Error          :: term().

-doc """
EncodingConfig = list()  
Version = integer() | dynamic  
Message = megaco_message()  
Bin = binary()  
Error = term()  

Perform a minimal decode of a megaco message.

The purpose of this function is to do a minimal decode of Megaco message. A successfull result is a `'MegacoMessage'` in which only version and mid has been initiated. This function is used by the megaco_messenger module when the `c:decode_message/3` function fails to figure out the mid (the actual sender) of the message.

Note again that a successfull decode only returns a partially initiated message.

[](){: id=encode_transaction }
""".
-callback decode_mini_message(EncodingConfig,
                              Version,
                              Bin) -> {ok, Message} | Error when
      EncodingConfig :: list(),
      Version        :: protocol_version() | dynamic,
      Bin            :: binary(),
      Message        :: megaco_message(),
      Error          :: term().

-doc """
EncodingConfig = list()  
Version = integer()  
Transaction = transaction()  
OK = \{ok, Bin\}  
Bin = binary()  
Error = \{error, Reason\}  
Reason = not_implemented | OtherReason  
OtherReason = term()  

Encode a megaco transaction. If this, for whatever reason, is not supported, the function should return the error reason `not_implemented`.

This functionality is used both when the transaction sender is used and for segmentation. So, for either of those to work, this function *must* be fully supported\!

[](){: id=encode_action_requests }
""".
-callback encode_transaction(EncodingConfig,
                             Version,
                             Transaction) -> {ok, Bin} | {error, Reason} when
      EncodingConfig :: list(),
      Version        :: protocol_version(),
      Transaction    :: transaction(),
      Bin            :: binary(),
      Reason         :: not_implemented | term().

-doc """
EncodingConfig = list()  
Version = integer()  
ARs = action_requests()  
action_requests() = \[action_request()]  
OK = \{ok, Bin\}  
Bin = binary()  
Error = \{error, Reason\}  
Reason = not_implemented | OtherReason  
OtherReason = term()  

Encode megaco action requests. This function is called when the user calls the function [encode_actions/3](`m:megaco#encode_actions`). If that function is never used or if the codec cannot support this (the encoding of individual actions), then return with error reason `not_implemented`.

[](){: id=encode_action_reply }
""".
-callback encode_action_requests(EncodingConfig,
                                 Version,
                                 ARs) -> {ok, Bin} | {error, Reason} when
      EncodingConfig :: list(),
      Version        :: protocol_version(),
      ARs            :: [action_request()],
      Bin            :: binary(),
      Reason         :: not_implemented | term().

-doc """
EncodingConfig = list()  
Version = integer()  
AR = action_reply()  
OK = \{ok, Bin\}  
Bin = binary()  
Error = \{error, Reason\}  
Reason = not_implemented | OtherReason  
OtherReason = term()  

Encode a megaco action reply. If this, for whatever reason, is not supported, the function should return the error reason `not_implemented`.

This function is used when segmentation has been configured. So, for this to work, this function *must* be fully supported\!
""".
-callback encode_action_reply(EncodingConfig,
                              Version,
                              AR) -> {ok, Bin} | {error, Reason} when
      EncodingConfig :: list(),
      Version        :: protocol_version(),
      AR             :: action_reply(),
      Bin            :: binary(),
      Reason         :: not_implemented | term().

-optional_callbacks(
   [
    encode_action_reply/3 % Only used if segementation is used
   ]).

