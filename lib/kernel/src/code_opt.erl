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

-export([ssa/1]).

ssa(File) ->
    Module = beam_disasm:file(File),
    Code = fixup_code(Module#beam_file.code),

    BeamInfo = {Module#beam_file.module,
                [{F,A} || {F,A,_Lbl} <- Module#beam_file.labeled_exports],
                Module#beam_file.attributes,
                Code, num_labels(Code) + 1},
    {ok, AfterA} = beam_a:module(BeamInfo, []),
    {ok, AfterBlocks} = beam_block:module(AfterA, []),
    module(AfterBlocks, []).

module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0} = F)
  when Name =/= test ->
    F;
function({function,Name,Arity,CLabel,Is0}) ->
    try
        io:format("~p~n",[Is0]),
	Is1 = to_ssa(Is0, CLabel, Arity),
        io:format("~p~n",[Is1]),

	{function,Name,Arity,CLabel,Is1}
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

to_ssa([{label, N} | Is], CLabel, Arity) ->
    Args = maps:from_list([{{x,I},{var,{block,0},I}} || I <- lists:seq(0,Arity-1)]),
    Labels = maps:from_list([{{f,N+I},{block,I}} || I <- lists:seq(0,CLabel+1)]),
    to_ssa(Is, {block, 0}, Arity, [], maps:merge(Args, Labels),
           #{ bid => maps:size(Labels),
              {block, 0} => #{ regs => [], in => #{ {block, entry} => Args }} }).

to_ssa([{label, N} | IsT], CurrBid, VarId, Is, Maps, Blocks) ->

    NextBid = maps:get({f,N}, Maps),

    case Is =/= [] andalso (is_test(hd(Is)) orelse is_return(hd(Is))) of
        true ->
            Is1 = Is;
        false ->
            Is1 = [{test, jump, [], NextBid} | Is]
    end,

    case Is =/= [] andalso (is_return(hd(Is))) of
        true ->
            NextBids = [];
        false ->
            NextBids = [NextBid]
    end,

    NewBlocks = close_block(Blocks, CurrBid, Is1, Maps, NextBids),
    {Phis, {NextMaps, NewVarId}} = init_block(NextBid, VarId, Maps, NewBlocks),

    to_ssa(IsT, NextBid, NewVarId, Phis, NextMaps, NewBlocks);
to_ssa([{func_info, _M, _F, _A} | IsT], Bid, VarId, Is, Maps, Blocks) ->
    to_ssa(IsT, Bid, VarId, Is, Maps, Blocks);
to_ssa([{block, BIs} | IsT], Bid, VarId, Is, Maps, Blocks) ->
    to_ssa(BIs ++ IsT, Bid, VarId, Is, Maps, Blocks);
to_ssa([{deallocate, _N} | IsT], Bid, VarId, Is, Maps, Blocks) ->
    to_ssa(IsT, Bid, VarId, Is, Maps, Blocks);
to_ssa([{set, Write, Read, {alloc,_,_}} | IsT], Bid, VarId, Is, Maps, Blocks) ->
    to_ssa(IsT, Bid, VarId, Is, Maps, Blocks);


to_ssa([{set, Write, Read, {get_tuple_element,N}} | IsT], Bid, VarId, Is, Maps, Blocks) ->
    to_ssa([{set, Write, Read ++ [N], get_tuple_element} | IsT],
            Bid, VarId, Is, Maps, Blocks);
to_ssa([{set, Write, [], {put_tuple,N}} | IsT], Bid, VarId, Is, Maps, Blocks) ->
    {Puts, T} = lists:split(N, IsT),
    MergedRead = [Reg || {set, [], [Reg], put} <- Puts],
    true = length(Puts) == length(MergedRead),
    to_ssa([{set, Write, [N] ++ MergedRead, put_tuple} | T],
            Bid, VarId, Is, Maps, Blocks);
to_ssa([{set, Write, Read, move} | IsT], Bid, VarId, Is, Maps, Blocks) ->
    NumNewVars = length(Write),
    SSAMap = maps:merge(Maps,
                        maps:from_list(
                          lists:zip(Write, map_vars(Read, Maps)))),
    to_ssa(IsT, Bid, VarId + NumNewVars, Is, SSAMap, Blocks);
to_ssa([{set, Write, Read, Op} | IsT], Bid, VarId, Is, Maps, Blocks) ->
    NumNewVars = length(Write),
    SSAMap = maps:merge(Maps,
                        maps:from_list(
                          lists:zip(Write, gen_vars(Bid, VarId, NumNewVars)))),
    NewI = {set, map_vars(Write, SSAMap), map_vars(Read, Maps), Op},
    to_ssa(IsT, Bid, VarId + NumNewVars, [NewI|Is], SSAMap, Blocks);

to_ssa([{test, Op, Fail, Args} | IsT], CurrBid, VarId, Is, Maps, Blocks) ->
    NewBid = {block, maps:get(bid, Blocks)},
    NextBids = [NewBid] ++ map_vars([Fail], Maps),
    NewI = {test, Op, map_vars(Args, Maps), NextBids},

    NewBlocks = close_block(Blocks, CurrBid, [NewI|Is], Maps, NextBids),

    {Phis, {NextMaps, NewVarId}} = init_block(NewBid, VarId, Maps, NewBlocks),

    to_ssa(IsT, NewBid, NewVarId, Phis, NextMaps,
           NewBlocks#{ bid => maps:get(bid, Blocks) + 1 } );
to_ssa([{jump, To}, {label, N} | IsT], CurrBid, VarId, Is, Maps, Blocks) ->
    NextBid = map_value({f,N}, Maps),
    NextBids = map_vars([To], Maps),
    NewI = {test, jump, [], NextBids},

    NewBlocks = close_block(Blocks, CurrBid, [NewI | Is], Maps, NextBids),
    {Phis, {NextMaps, NewVarId}} = init_block(NextBid, VarId, Maps, NewBlocks),

    to_ssa(IsT, NextBid, NewVarId, Phis, NextMaps, NewBlocks);
to_ssa([{select, select_val, Val, Fail, Dests}, {label, N} | IsT],
       CurrBid, VarId, Is, Maps, Blocks) ->
    NextBid = map_value({f,N}, Maps),

    {val, Vs, Ds} = lists:foldl(fun(V,{val,Vs,Ds}) ->
                                         {dest,[V|Vs],Ds};
                                    (D,{dest,Vs,Ds}) ->
                                         {val,Vs,[D|Ds]}
                                 end,{val,[],[]}, Dests),

    NextBids = map_vars([Fail | Ds], Maps),

    NewI = {test, select_val, map_vars([Val|Vs], Maps), NextBids},

    NewBlocks = close_block(Blocks, CurrBid, [NewI | Is], Maps, NextBids),
    {Phis, {NextMaps, NewVarId}} = init_block(NextBid, VarId, Maps, NewBlocks),

    to_ssa(IsT, NextBid, NewVarId, Phis, NextMaps, NewBlocks);

to_ssa([{call, Arity, To} | IsT], Bid, VarId, Is, Maps, Blocks) ->
    NewI = {set, [{x,0}], [{x,I} || I <- lists:seq(0,Arity-1)], {call, To}},
    to_ssa([NewI | IsT], Bid, VarId, Is, Maps, Blocks);
to_ssa([{make_fun2, Lbl, _OldIndex, _OldUniq, _NumFree} | IsT], Bid, VarId, Is, Maps, Blocks) ->
    NewI = {set, [{x,0}], [], {make_fun2, Lbl}},
    to_ssa([NewI | IsT], Bid, VarId, Is, Maps, Blocks);
to_ssa([return | IsT], Bid, VarId, Is, Maps, Blocks) ->
    NewI = {set, [], [{x,0}], return},
    to_ssa([NewI | IsT], Bid, VarId, Is, Maps, Blocks);

to_ssa([I | IsT], Bid, VarId, Is, Maps, Blocks) ->
    to_ssa(IsT, Bid, VarId, [I | Is], Maps, Blocks);
to_ssa([], CurrBid, VarId, Is, Maps, Blocks) ->
    close_block(Blocks, CurrBid, Is, Maps, []).

close_block(Blocks, CurrBid, Is, Maps, NextBids) ->
    Block = maps:get(CurrBid, Blocks),
    NewBlocks = update_succs(CurrBid, Blocks, Maps, NextBids),
    NewBlocks#{ CurrBid := Block#{ regs := lists:usort(lists:flatten(maps:get(regs, Block))),
                                   is => lists:reverse(Is),
                                   out => Maps,
                                   succs => NextBids} }.

update_succs(PredBid, Blocks, Maps, [Bid | T]) ->
    Block = #{ regs := Regs, in := In } = maps:get(Bid, Blocks, #{ regs => [], in => #{ } }),
    NewBlock = Block#{ regs := [get_regs(Maps)|Regs], in := In#{ PredBid => Maps } },
    update_succs(PredBid, Blocks#{ Bid => NewBlock }, Maps, T);
update_succs(_, Blocks, _, []) ->
    Blocks.

init_block(Bid, VarId, Maps, Blocks) ->
    #{ regs := Regs, in := In } = maps:get(Bid, Blocks),
    lists:mapfoldl(
      fun(Reg, {Map, VId}) ->
              {PhiId, Phi} = create_phi(Reg, In, Bid, VId),
              {Phi, {Map#{ Reg => PhiId }, VId + 1}}
      end,{labels(Maps), VarId}, lists:usort(lists:flatten(Regs))).

create_phi(Reg, In, Bid, VId) ->
    PhiId = {var, Bid, VId},
    {PhiId, {phi, [PhiId], create_incoming(Reg, In)}}.

create_incoming(Reg, In) ->
    maps:map(fun(Bid, Map) ->
                     map_value(Reg, Map)
             end, In).

gen_vars(Bid, Offset, Num) ->
    [{var,Bid, Offset + I} || I <- lists:seq(0, Num - 1)].
map_vars(Values, Map) ->
    [map_value(Value, Map) || Value <- Values].
map_value(Value, Map) ->
    maps:get(Value, Map, Value).

get_regs(Maps) ->
    [R || {Tag,_} = R <- maps:keys(Maps),
          Tag =:= x orelse Tag =:= y].
get_labels(Maps) ->
    [R || {Tag,_} = R <- maps:keys(Maps), Tag =:= f].
labels(Maps) ->
    maps:from_list([{K, maps:get(K, Maps)} || {f, _} = K <- maps:keys(Maps)]).

is_test({test, _, _, _}) ->
    true;
is_test(_) ->
    false.

is_return({set, _, _, return}) ->
    true;
is_return(_) ->
    false.


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
