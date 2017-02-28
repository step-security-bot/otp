%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-module(code_opt).

-include_lib("compiler/src/beam_opcodes.hrl").
-include_lib("compiler/src/beam_disasm.hrl").

-export([load/1,opt/1]).


%% (<0.80.0>) call beam_asm:module({test,[{module_info,0},{module_info,1},{test,1}],
%%       [],
%%       [{function,test,1,2,
%%                  [{label,1},
%%                   {line,[{location,"test.erl",5}]},
%%                   {func_info,{atom,test},{atom,test},1},
%%                   {label,2},
%%                   {allocate,1,1},
%%                   {move,{x,0},{y,0}},
%%                   {make_fun2,{f,8},0,0,0},
%%                   {move,{y,0},{x,2}},
%%                   {move,{integer,0},{x,1}},
%%                   {line,[{location,"test.erl",6}]},
%%                   {call_ext_last,3,{extfunc,lists,foldl,3},1}]},
%%        {function,module_info,0,4,
%%                  [{label,3},
%%                   {line,[]},
%%                   {func_info,{atom,test},{atom,module_info},0},
%%                   {label,4},
%%                   {move,{atom,test},{x,0}},
%%                   {line,[]},
%%                   {call_ext_only,1,{extfunc,erlang,get_module_info,1}}]},
%%        {function,module_info,1,6,
%%                  [{label,5},
%%                   {line,[]},
%%                   {func_info,{atom,test},{atom,module_info},1},
%%                   {label,6},
%%                   {move,{x,0},{x,1}},
%%                   {move,{atom,test},{x,0}},
%%                   {line,[]},
%%                   {call_ext_only,2,{extfunc,erlang,get_module_info,2}}]},
%%        {function,'-test/1-fun-0-',2,8,
%%                  [{label,7},
%%                   {line,[{location,"test.erl",6}]},
%%                   {func_info,{atom,test},{atom,'-test/1-fun-0-'},2},
%%                   {label,8},
%%                   {line,[{location,"test.erl",7}]},
%%                   {gc_bif,'+',{f,0},2,[{x,0},{x,1}],{x,0}},
%%                   return]}],
%%       9},[{<<"Abst">>,[]}],"/home/eluklar/git/otp/test.erl",[{outdir,"."}],[report_errors,report_warnings,{outdir,"."}])

load(File) ->
    {ok, Module, Asm} = opt(File),
    case erlang:load_module(Module, Asm) of
        {error, not_purged} ->
            erlang:check_process_code(self(), Module),
            true = erlang:purge_module(Module),
            erlang:load_module(Module, Asm);
        Success ->
            Success
    end.

opt(File) ->
    Module = beam_disasm:file(File),
    {ok, BeamAsm} =
        beam_asm:module({Module#beam_file.module,
                         Module#beam_file.labeled_exports,
                         Module#beam_file.attributes,
                         fixup_code(Module#beam_file.code),
                         num_labels(Module) + 1},
                        [{<<"Abst">>,[]}],
                        proplists:get_value(source, Module#beam_file.compile_info),
                        proplists:get_value(options, Module#beam_file.compile_info),
                        proplists:get_value(options, Module#beam_file.compile_info)),
    {ok, Module#beam_file.module, BeamAsm}.

num_labels(#beam_file{ code = Module } ) ->
    lists:foldl(fun(#function{ code = Code }, Max) ->
                        lists:foldl(fun({label,N}, M) when N > M ->
                                            N;
                                       (_, M) ->
                                            M
                                    end, Max, Code)
                end, 0, Module).


fixup_code(Functions) ->
    fixup_code(Functions, Functions).
fixup_code([#function { code = Code } = F | T], Functions) ->
    [F#function{ code = fixup_make_fun2(Code, Functions)} | fixup_code(T, Functions)];
fixup_code([], _Functions) ->
    [].

fixup_make_fun2([{make_fun2,{M,F,A},OldIndex,OldUniq,NumFree}|R], Functions) ->
    Function = find_function({M,F,A}, Functions),
    [{make_fun2, {f, Function#function.entry}, OldIndex, OldUniq,NumFree}
     | fixup_make_fun2(R, Functions)];
fixup_make_fun2([H|T], Functions) ->
    [H|fixup_make_fun2(T,Functions)];
fixup_make_fun2([], _Functions) ->
    [].

find_function({_M,F,A}, Functions) ->
    [Func] = lists:filter(fun(#function{ name = Name, arity = Arity }) ->
                                  Name =:= F andalso Arity =:= A
                          end, Functions),
    Func.
