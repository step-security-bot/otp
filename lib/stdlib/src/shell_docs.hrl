%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

-record(config, { docs,
                  type = man,
                  encoding,
                  ansi,
                  io_opts = io:getopts(),
                  columns
                }).

-define(ALL_ELEMENTS,[a,p,'div',br,h1,h2,h3,i,em,pre,code,ul,ol,li,dl,dt,dd]).
%% inline elements are:
-define(INLINE,[i,em,code,a]).
-define(IS_INLINE(ELEM),(((ELEM) =:= a) orelse ((ELEM) =:= code)
                         orelse ((ELEM) =:= i) orelse ((ELEM) =:= em))).
%% non-inline elements are:
-define(BLOCK,[p,'div',pre,br,ul,ol,li,dl,dt,dd,h1,h2,h3]).
-define(IS_BLOCK(ELEM),not ?IS_INLINE(ELEM)).
-define(IS_PRE(ELEM),(((ELEM) =:= pre))).
