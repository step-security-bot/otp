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


%% Maybe it would be a better idea to do this all on core erlang?
%% That way we can use the inliner written there, we just have
%% to introduce some special call instructions that can
%% call local functions in other modules.
-module(code_opt).

-include_lib("compiler/src/beam_opcodes.hrl").
-include_lib("compiler/src/beam_disasm.hrl").

-export([load/1,opt/1,test/1]).

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

test(File) ->
    Module = beam_disasm:file(File),
    Code = fixup_code(Module#beam_file.code),
    NewCode = optimize(Code),
    {Code, NewCode}.

opt(File) ->
    Module = beam_disasm:file(File),
    Code = fixup_code(Module#beam_file.code),

    BeamInfo = {Module#beam_file.module,
                [{F,A} || {F,A,_Lbl} <- Module#beam_file.labeled_exports],
                Module#beam_file.attributes,
                Code, num_labels(Code) + 1},
    {ok, AfterA} = beam_a:module(BeamInfo, []),

    NewCode = optimize(Code),
    {ok, {_, _, _, Is, _} = AfterBlocks} = beam_block:module(AfterA, []),
    io:format("~p",[beam_utils:live_opt((hd(Is))#function.code)]),
    beam_validator:module(BeamInfo, []),
    {ok, BeamAsm} =
        beam_asm:module(BeamInfo, [{<<"Abst">>,[]}],
                        proplists:get_value(source, Module#beam_file.compile_info),
                        proplists:get_value(options, Module#beam_file.compile_info),
                        proplists:get_value(options, Module#beam_file.compile_info)),
    {ok, Module#beam_file.module, BeamAsm}.

optimize(Functions) ->
    [optimize_function(F, Functions) || F <- Functions].

optimize_function(#function{ code = Is } = F, Functions) ->
    Is0 = lists:flatten(inline(Is, Functions)),
    IsN = if
              Is0 =/= Is ->
                  %% Only check liveness if something was changed
                  Is1 = live_opt(Is0),
                  dce(Is1);
              true ->
                  Is0
          end,
    F#function{ code = IsN }.

%% Optimize the code by inlining small functions
%% Current heuristic only allows local calls to be
%% inlined where the inlined function does not contain
%% any labels or function calls.
inline([{CallOp, _N, Lbl} = Op|T], Functions) when CallOp =:= call; CallOp =:= call_only ->
    case may_inline(Lbl, Functions) of
        no ->
            [Op | inline(T, Functions)];
        Code ->
            [Code | inline(T, Functions)]
    end;
inline([Op|T], Functions) ->
    [Op | inline(T, Functions)];
inline([], _Functions) ->
    [].

may_inline({f, Lbl}, [#function{ entry = Lbl, code = Is }|_T]) ->
    InlineIs = beam_utils:code_at(Lbl, beam_utils:index_labels(Is)),
    may_inline_Is(InlineIs, []);
may_inline(Lbl, [_F | T]) ->
    may_inline(Lbl, T);
may_inline(_, []) ->
    no.


may_inline_Is([I | _Is], _Acc)
  when element(1,I) =:= call;
       element(1,I) =:= call_only;
       element(1,I) =:= call_last;
       element(1,I) =:= label ->
    no;
may_inline_Is([return | _Is], Acc) ->
    lists:reverse(Acc);
may_inline_Is([I | Is], Acc) ->
    may_inline_Is(Is, [I | Acc]);
may_inline_Is(_, _Acc) ->
    no.


%% Go through the instructions from the back and reallocate registers.

live_opt(Is) ->
    io:format("~p~n",[Is]),
    Is.

%%     live_opt(lists:reverse(Is), sets:new(), [], #{}).

%% live_opt([return | Is], LiveSet, NewIs, LiveInfo) ->
%%     true = sets:size(LiveNow) == 0,
%%     live_opt(Is, sets:add_element({x,0}, LiveSet), [return | NewIs], LiveInfo);
%% live_opt([{deallocate,N} = I| Is], LiveSet, NewIs, LiveInfo) ->
%%     live_opt(Is, sets:union(LiveNow, sets:from_list([{y,I} || I <- lists:seq(1,N)])),
%%              [I | NewIs], LiveInfo);
%% live_opt() ->

%% Go through the instruction and remove any dead instructions
dce(Is) ->
    Is.


%% Get the number of the highest label
num_labels(Functions) ->
    lists:foldl(fun(#function{ code = Code }, Max) ->
                        lists:foldl(fun({label,N}, M) when N > M ->
                                            N;
                                       (_, M) ->
                                            M
                                    end, Max, Code)
                end, 0, Functions).


%% Fix the code given by beam_disasm to look like what beam_asm expects.
fixup_code(Functions) ->
    fixup_code(Functions, Functions).
fixup_code([#function { code = Code } = F | T], Functions) ->
    [F#function{ code = fixup_make_fun2(Code, Functions)} | fixup_code(T, Functions)];
fixup_code([], _Functions) ->
    [].

fixup_make_fun2([{line,_}|R], Functions)  ->
    fixup_make_fun2(R, Functions);
fixup_make_fun2([{Op,Live,{M,F,A}}|R], Functions) when Op == call; Op == call_only ->
    Function = find_function({M,F,A}, Functions),
    [{call, Live, {f, Function#function.entry}} | fixup_make_fun2(R, Functions)];
fixup_make_fun2([{call_last,Live,{M,F,A},D}|R], Functions) ->
    Function = find_function({M,F,A}, Functions),
    [{call_last, Live, {f, Function#function.entry}, D} | fixup_make_fun2(R, Functions)];
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
