-module(jit_tests_cases).

-export([is_nil/0, literal/0, idiv/0, get_list/0, get_tl/0, get_hd/0,
         'is_atom'/0, 'is_boolean'/0, 'is_binary'/0, is_bitstring/0,
         'is_float'/0, 'is_function'/0, is_function2/0, is_integer/0,
         is_list/0, is_map/0, is_number/0, is_port/0, is_pid/0,
         is_reference/0, is_tagged_tuple/0, is_tuple/0, is_tuple_of_arity/0]).

-import(jit_tests,[id/1]).

% -export([l/0]).

% l() ->
%     receive a -> a after 0 -> ok end.

%% Tests that the is_nil check at the end of the [A,B,C,D] is
%% not eliminated by any optimizations.
is_nil() ->
    ok = is_nil(id([])),
    nok = is_nil(id([1])),
    ok.
is_nil([]) -> ok;
is_nil(_) -> nok.

literal() ->
    ok = literal(id([1,2,3,4])),
    nok = literal(id([1,2,3,4, 5])),
    ok.
literal([1,2,3,4]) -> ok;
literal(_) -> nok.

idiv() ->
    2 = idiv(id(4), id(2)),
    {'EXIT',{badarith,[{erlang,'div',[a,2],_}|_]}} = (catch idiv(id(a), id(2))),
    {'EXIT',{badarith,[{erlang,'div',[2,0],_}|_]}} = (catch idiv(id(2), id(0))),

    %% Test the threshold when a div becomes a bignum
    576460752303423488 = (catch idiv(id((-1 bsl 59)), id(-1))),

    %% Test bignum division
    9007199254740992 = idiv(id(1 bsl 64), id(2 bsl 10)),
    ok.
idiv(A,B) ->
    A div B.

get_list() ->
    10 = get_list(id([1,2,3,4])),
    ok.
get_list([A|T]) ->
    A + get_list(T);
get_list([]) ->
    0.

get_tl() ->
    4 = get_tl(id([1,2,3,4])),
    ok.
get_tl([_|T]) ->
    1 + get_tl(T);
get_tl([]) ->
    0.

get_hd() ->
    1 = get_hd(id([1,2,3,4])),
    ok.
get_hd([A|_]) ->
    A;
get_hd([]) ->
    0.

'is_atom'() ->
    true = my_is_atom(id(a)),
    false = my_is_atom(id(1)),
    false = my_is_atom(id(<<>>)),
    ok.
my_is_atom(A) when is_atom(A) ->
    true;
my_is_atom(_) ->
    false.

is_boolean() ->
    true = my_is_boolean(id(true)),
    true = my_is_boolean(id(false)),
    false = my_is_boolean(id(a)),
    false = my_is_boolean(id(<<>>)),
    ok.

my_is_boolean(B) when is_boolean(B) ->
    true;
my_is_boolean(_) ->
    false.

'is_binary'() ->
    RefcBin = <<0:(65 * 8)>>,
    SubBin = binary_part(RefcBin, {1,4}),
    true = my_is_binary(id(<<>>)),
    true = my_is_binary(id(RefcBin)),
    true = my_is_binary(id(SubBin)),
    false = my_is_binary(id(<<0:1>>)),
    false = my_is_binary(id(a)),
    ok.

my_is_binary(B) when is_binary(B) ->
    true;
my_is_binary(_) ->
    false.

'is_bitstring'() ->
    RefcBin = <<0:(65 * 8)>>,
    SubBin = binary_part(RefcBin, {1,4}),
    true = is_bitstring(id(<<>>)),
    true = is_bitstring(id(RefcBin)),
    true = is_bitstring(id(SubBin)),
    true = is_bitstring(id(<<0:1>>)),
    false = is_bitstring(id(a)),
    ok.

'is_float'() ->
    true = my_is_float(id(0.0)),
    false = my_is_float(id(0)),
    ok.

my_is_float(F) when is_float(F) ->
    true;
my_is_float(_) ->
    false.

'is_function'() ->
    F = fun() -> ok end,
    E = fun 'is_function'/0,
    true = my_is_function(id(F)),
    true = my_is_function(id(E)),
    false = my_is_function(id(0)),
    false = my_is_function(id(0.0)),
    
    ok.

my_is_function(F) when is_function(F) ->
    true;
my_is_function(_) ->
    false.

'is_function2'() ->
    F0 = fun() -> ok end,
    F1 = fun(A) -> A end,
    E0 = fun 'is_function'/0,
    E1 = fun 'my_is_function'/1,
    true = my_is_function2(id(F0)),
    true = my_is_function2(id(E0)),
    false = my_is_function2(id(0)),
    false = my_is_function2(id(0.0)),
    false = my_is_function2(id(F1)),
    false = my_is_function2(id(E1)),

    true = my_is_function2(id(F0),id(0)),
    true = my_is_function2(id(E0),id(0)),
    false = my_is_function2(id(0),id(0)),
    false = my_is_function2(id(0.0),id(0)),
    false = my_is_function2(id(F1),id(0)),
    false = my_is_function2(id(E1),id(0)),

    ok.
my_is_function2(F) when is_function(F, 0) ->
    true;
my_is_function2(_) ->
    false.

my_is_function2(F,A) when is_function(F, A) ->
    true;
my_is_function2(_,_) ->
    false.

'is_integer'() ->
    true = my_is_integer(id(0)),
    true = my_is_integer(id(1 bsl 64)),
    false = my_is_integer(id(0.0)),
    false = my_is_integer(id(a)),
    ok.
my_is_integer(I) when is_integer(I) ->
    true;
my_is_integer(_I) ->
    false.

is_list() ->
    true = my_is_list(id([])),
    true = my_is_list(id([1])),
    true = my_is_list(id([1|2])),
    false = my_is_list(id(a)),
    ok.
my_is_list(L) when is_list(L) ->
    true;
my_is_list(_L) ->
    false.

is_map() ->
    HAMT = maps:from_list([{I,I} || I <- lists:seq(1,100)]),
    true = my_is_map(id(#{})),
    true = my_is_map(id(HAMT)),
    false = my_is_map(id(<<>>)),
    false = my_is_map(id(a)),
    ok.
my_is_map(M) when is_map(M) ->
    true;
my_is_map(_M) ->
    false.

is_number() ->
    true = my_is_number(id(0)),
    true = my_is_number(id(1 bsl 64)),
    true = my_is_number(id(0.0)),
    false = my_is_number(id(a)),
    ok.
my_is_number(I) when is_number(I) ->
    true;
my_is_number(_I) ->
    false.

is_pid() ->
    ExternalPid = binary_to_term(<<131,103,100,0,14,116,50,64,101,108,120,100,51,50,57,49,118,48,107,0,0,0,75,0,0,0,0,0>>),
    true = my_is_pid(id(self())),
    true = my_is_pid(id(ExternalPid)),
    false = my_is_pid(id(0.0)),
    false = my_is_pid(id(a)),
    ok.
my_is_pid(P) when is_pid(P) ->
    true;
my_is_pid(_P) ->
    false.

is_port() ->
    ExternalPort = binary_to_term(<<131,102,100,0,14,116,50,64,101,108,120,100,51,50,57,49,118,48,107,0,0,0,0,0>>),
    true = my_is_port(id(hd(erlang:ports()))),
    true = my_is_port(id(ExternalPort)),
    false = my_is_port(id(0.0)),
    false = my_is_port(id(a)),
    ok.
my_is_port(P) when is_port(P) ->
    true;
my_is_port(_P) ->
    false.

is_reference() ->
    ExternalRef = binary_to_term(<<131,90,0,3,100,0,14,116,50,64,101,108,120,100,51,50,57,49,118,48,107,0,0,0,1,0,1,53,224,188,124,0,1,230,114,44,190>>),
    true = my_is_ref(id(make_ref())),
    true = my_is_ref(id(ExternalRef)),
    false = my_is_ref(id(0.0)),
    false = my_is_ref(id(a)),
    ok.

my_is_ref(R) when is_reference(R) ->
    true;
my_is_ref(_R) ->
    false.

is_tagged_tuple() ->
    true = is_tagged_tuple(id({a,b})),
    false = is_tagged_tuple(id({b,b})),
    false = is_tagged_tuple(id({a,b,c})),
    false = is_tagged_tuple(id({a})),
    false = is_tagged_tuple(id(0.0)),
    false = is_tagged_tuple(id(0)),

    true = is_tagged_tuple_ff(id({a,b})),
    true2 = is_tagged_tuple_ff(id({b,b,b})),
    false = is_tagged_tuple_ff(id(a)),
    ok.
is_tagged_tuple({a,_}) ->
    true;
is_tagged_tuple(_) ->
    false.
is_tagged_tuple_ff(T) when is_tuple(T) ->
    case T of
        {a,_} -> true;
        _ -> true2
    end;
is_tagged_tuple_ff(_) ->
    false.

is_tuple() ->
    true = my_is_tuple(id({})),
    false = my_is_tuple(id([])),
    false = my_is_tuple(id(0.0)),
    ok.
my_is_tuple(T) when is_tuple(T) ->
    true;
my_is_tuple(_T) ->
    false.

is_tuple_of_arity() ->
    true = is_tuple_of_arity(id({a})),
    false = is_tuple_of_arity(id({})),
    false = is_tuple_of_arity(id([])),
    false = is_tuple_of_arity(id(0.0)),
    ok.
is_tuple_of_arity({_}) ->
    true;
is_tuple_of_arity(_) ->
    false.

%% 'bnot'() ->
%%     'bnot'(
%%     F = fun(E) -> bnot E end,

%%     %% Warmup
%%     map(F, seq(1, 200)),
    

%%     [bnot 1,bnot 2,bnot 3,bnot 4] = map(F, Valid),

%%     ok.

%% export() ->

%%     Valid = [1,2,3,4],

%%     %% Warmup
%%     map(fun ?MODULE:export_fun/1, [Valid || _N <- seq(1, 200)]),
    

%%     [[4,3,2,1],[4,3,2,1]] = map(fun ?MODULE:export_fun/1, [Valid, Valid]),

%%     ok.

%% export_fun(E) ->
%%     lists:reverse(E).

%% environment() ->

%%     Valid = id([1,2,3,4]),

%%     F = fun(E) when E =:= Valid -> true;
%%            (_E) -> false
%%         end,

%%     %% Warmup
%%     map(F, [Valid || _N <- seq(1, 200)]),
    

%%     [true, true] = map(F, [Valid,Valid]),

%%     ok.

%% environment_guard() ->

%%     List = [1,2,3,4],

%%     MakeFun = fun(Valid) ->
%%                       fun(E) when E =:= Valid -> true;
%%                          (_E) -> false
%%                       end
%%               end,

%%     FList = MakeFun(List),
%%     FAtom = MakeFun(atom),

%%     %% Warmup
%%     map(fun(E) -> E end, [List || _N <- seq(1, 200)]),
    
%%     map(FList, [List || _N <- seq(1, 200)]),
    
%%     map(FAtom, [List || _N <- seq(1, 200)]),
    

%%     [true, false] = map(FList, [List,atom]),
%%     [false, true] = map(FAtom, [List,atom]),

%%     ok.

%% make_fun() ->

%%     Valid = [1,2,3,4],
%%     Invalid = [1,2,3,5],

%%     F = fun(E) -> fun() -> E == Valid end end,

%%     %% Warmup
%%     map(F, [Valid || _N <- seq(1, 200)]),
    

%%     [F1, F2] = map(F, [Valid,Invalid]),
%%     [true, false] = [F1(), F2()],

%%     ok.

%% is_function() ->

%%     ZeroLocal = fun() -> ok end,
%%     ZeroExport = fun ?MODULE:is_function/0,
%%     OneLocal = fun(_) -> ok end,
%%     TwoExport = fun map/2,
%%     Ref = make_ref(),
%%     TupleFun = {?MODULE, is_function},

%%     ConstF = fun(F) when is_function(F, 0) -> true; (_) -> false end,

%%     %% Warmup
%%     map(ConstF, [ZeroLocal || _N <- seq(1, 200)]),
    
%%     map(ConstF, [Ref || _N <- seq(1, 200)]),
    
%%     map(ConstF, [TwoExport || _N <- seq(1, 200)]),
    

%%     [true, true, false, false, false, false]
%%         = map(ConstF, [ZeroLocal, ZeroExport,
%%                              OneLocal, TwoExport,
%%                              Ref, TupleFun]),

    

%%     DynF = fun({F,A}) when is_function(F, A) -> true; (_) -> false end,

%%     %% Warmup
%%     map(DynF, [{ZeroLocal,1} || _N <- seq(1, 200)]),
    
%%     map(DynF, [{ZeroLocal,0} || _N <- seq(1, 200)]),
    
%%     map(DynF, [{fun map/2,2} || _N <- seq(1, 200)]),
    
%%     map(DynF, [{fun map/2,20} || _N <- seq(1, 200)]),
    

%%     [true, false, true, false, true, false, true, false, false, false]
%%         = map(DynF, [{ZeroLocal,0},{ZeroLocal,1000},
%%                            {ZeroExport,0},{ZeroExport,make_ref()},
%%                            {OneLocal,1},{OneLocal,-1},
%%                            {TwoExport,2},{TwoExport,2.0},
%%                            Ref, TupleFun]),

%%     ok.

%% length() ->

%%     Short = [1,2,3,4],
%%     Long = lists:duplicate(10000, 10000),
%%     Longer = lists:duplicate(100000, 100000),
%%     Invalid = [1,2,3|4],

%%     F = fun(E) -> length(E) end,

%%     %% Warmup
%%     map(F, [Short || _N <- seq(1, 200)]),
    

%%     [4, 10000] = map(F, [Short,Long]),


%%     %% We do a little dance here in order for length
%%     %% to have to take the its argument in x(1) instead
%%     %% of x(0) which tests that error handling is done
%%     %% correctly.
%%     F2 = fun(E,{_L,A}) ->
%%                  {length(A),A ++ E}
%%          end,

%%     Acc = {0,[]},

%%     %% Warmup
%%     foldl(F2, Acc, [Short || _N <- seq(1, 200)]),
    

%%     {10004, _} = foldl(F2, Acc, [Short,Long,Longer]),

%%     try foldl(F2, Acc, [Short, Invalid, Short]) of
%%         %% Should never return
%%         error ->
%%             error
%%     catch error:badarg:ST ->
%%             SI = Short ++ Invalid,
%%             [{erlang,length,[SI],_}|_] = ST,
%%             ok
%%     end.

%% length_guards() ->

%%     Short = [1,2,3,4],
%%     Long = lists:duplicate(10000, 10000),
%%     Longer = lists:duplicate(100000, 100000),
%%     Invalid = [1,2,3|4],

%%     F = fun(E) when is_number(length(E)) ->
%%                 true;
%%            (_) ->
%%                 false
%%         end,

%%     %% Warmup
%%     map(F, [Short || _N <- seq(1, 200)]),
    

%%     [true, true, true, false] = map(F, [Short,Long,Longer,Invalid]),

%%     ok.

%% 'catch'() ->

%%     F = fun(E) -> catch integer_to_list(E) end,

%%     %% Warmup
%%     map(F, [atom || _N <- seq(1, 200)]),
    

%%     [_, "1", "2", _] = map(F, [atom, 1, 2, atom]),

%%     ok.

%% try_catch() ->

%%     F = fun(E) -> try integer_to_list(E) catch _:_ -> error end end,

%%     %% Warmup
%%     map(F, [atom || _N <- seq(1, 200)]),
    

%%     [error, "1", "2", error] = map(F, [atom, 1, 2, atom]),

%%     ok.

%% pdict() ->

%%     put(test, 1),

%%     F = fun(E) -> get(test) + E end,

%%     %% Warmup
%%     map(F, seq(1, 200)),
    

%%     [2, 3, 4, 5] = map(F, [1,2,3,4]),

    

%%     [put(I, I) || I <- seq(1,200)],

%%     F2 = fun(E) -> get(E) - E end,

%%     %% Warmup
%%     map(F2, seq(1, 200)),
    

%%     [0, 0, 0, 0] = map(F2, [1,2,3,4]),

%%     ok.

%% bif_call() ->

%%     Valid = [1,2,3,4],
%%     Invalid = [1,2,3|4],

%%     F = fun(E) -> A = erlang:adler32(E), id(A) end,

%%     %% Warmup
%%     map(F, [Valid || _N <- seq(1, 200)]),
    

%%     [1572875, 1572875] = map(F, [Valid,Valid]),

%%     try map(F, [Valid,Invalid]) of
%%         %% Should never return
%%         error ->
%%             error
%%     catch error:badarg ->
%%             ok
%%     end.

%% bif_call_only() ->

%%     Valid = [1,2,3,4],
%%     Invalid = [1,2,3|4],

%%     F = fun(E) -> erlang:adler32(E) end,

%%     %% Warmup
%%     map(F, [Valid || _N <- seq(1, 200)]),
    

%%     [1572875,1572875] = map(F, [Valid,Valid]),

%%     try map(F, [Valid,Invalid]) of
%%         %% Should never return
%%         error ->
%%             error
%%     catch error:badarg ->
%%             ok
%%     end.


%% %% We test what happens with a call_bif_only when the return is in the trace.
%% bif_call_only_return() ->

%%     erts_debug:set_internal_state(available_internal_state, true),

%%     %% Warmup
%%     #{} = bif_call_only_return_foldr(#{}, lists:duplicate(200, 0)),
    

%%     #{} = bif_call_only_return_foldr(#{}, lists:duplicate(200, 1)),
    

%%     [#{} = bif_call_only_return_foldr(#{}, [1]) || _ <- seq(1,200)],
    

%%     erts_debug:set_internal_state(available_internal_state, false),

%%     ok.

%% bif_call_only_return_foldr(M, []) ->
%%     M;
%% bif_call_only_return_foldr(M, [H|T]) ->
%%     NewM = bif_call_only_return_foldr(M, T),
%%     case H of
%%         I when (I band 1) == 0 ->
%%             NewM;
%%         I when (I band 1) == 1 ->
%%             erts_debug:get_internal_state({trap,NewM})
%%     end.

%% id(A) ->
%%     A.

%% bif_trap() ->

%%     erts_debug:set_internal_state(available_internal_state, true),

%%     %% erts_debug:get_internal_state({trap, Term}) is a special function used
%%     %% to tests trapping of bifs. Each time it is called it will send
%%     %% a message to the calling process with Term.
%%     %% If Term is a tuple, it will cause the bif to trap to erlang:element(1, Term).
%%     %% If Term is 'error', it will fail with badarg.
%%     F = fun(E) -> R = erts_debug:get_internal_state({trap, E}), id(R) end,

%%     %% Warmup
%%     Refs = map(F, [make_ref() || _N <- seq(1, 200)]),
    
%%     200 = length([receive R -> R after 500 -> exit({timeout, R}) end || R <- Refs]),
%%     [] = flush(),

%%     %% Test that when calling F, it traps and sends only one message
%%     Ref = make_ref(),
%%     [Ref] = map(F, [{Ref}]),
%%     [{Ref}] = flush(),

%%     %% Test that when calling F, it fails and sends only one message
%%     try map(F, [error]) of
%%         ok -> ok
%%     catch error:badarg:ST ->
%%             [{erts_debug, get_internal_state, [{trap, error}],[]}|_] = ST,
%%             [error] = flush()
%%     end,
%%     erts_debug:set_internal_state(available_internal_state, false),
%%     ok.

%% bif_only_trap() ->

%%     erts_debug:set_internal_state(available_internal_state, true),

%%     %% erts_debug:get_internal_state({trap, Term}) is a special function used
%%     %% to tests trapping of bifs. Each time it is called it will send
%%     %% a message to the calling process with Term.
%%     %% If Term is a tuple, it will cause the bif to trap to erlang:element(1, Term).
%%     %% If Term is 'error', it will fail with badarg.
%%     F = fun(E) -> erts_debug:get_internal_state({trap, E}) end,

%%     %% Warmup
%%     Refs = map(F, [make_ref() || _N <- seq(1, 200)]),
    
%%     200 = length([receive R -> R after 500 -> exit({timeout, R}) end || R <- Refs]),
%%     [] = flush(),

%%     %% Test that when calling F, it traps and sends only one message
%%     Ref = make_ref(),
%%     [Ref] = map(F, [{Ref}]),
%%     [{Ref}] = flush(),

%%     %% Test that when calling F, it fails and sends only one message
%%     try map(F, [error]) of
%%         ok -> ok
%%     catch error:badarg:ST ->
%%             [{erts_debug, get_internal_state, [{trap, error}],[]}|_] = ST,
%%             [error] = flush()
%%     end,
%%     erts_debug:set_internal_state(available_internal_state, false),
%%     ok.

%% flush() ->
%%     flush([]).
%% flush(Acc) ->
%%     receive M -> flush([M|Acc])
%%     after 500 -> Acc
%%     end.

%% ubif_call_succ() ->

%%     Valid = [1,2,3,4],
%%     Invalid = [1,2,3|4],

%%     ValidSeq = [Valid || _N <- seq(1, 200)],
%%     InvalidSeq = [Invalid || _N <- seq(1, 200)],

%%     F = fun(E) when is_integer(length(E)) ->
%%                 E;
%%            (_E) ->
%%                 invalid
%%         end,

    

%%     %% Warmup
%%     map(F, ValidSeq),
    

%%     [Valid,Valid] = map(F, [Valid, Valid]),
%%     [Valid,invalid] = map(F, [Valid, Invalid]),

%%     %% Warmup side trace
%%     map(F, InvalidSeq),
    

%%     [Valid,Valid] = map(F, [Valid, Valid]),
%%     [Valid,invalid] = map(F, [Valid, Invalid]),

%%     ok.

%% ubif_call_fail() ->

%%     Valid = [1,2,3,4],
%%     Invalid = [1,2,3|4],

%%     ValidSeq = [Valid || _N <- seq(1, 200)],
%%     InvalidSeq = [Invalid || _N <- seq(1, 200)],

%%     F = fun(E) when is_integer(length(E)) ->
%%                 E;
%%            (_E) ->
%%                 invalid
%%         end,

    

%%     %% Warmup
%%     map(F, InvalidSeq),
    

%%     [Valid,Valid] = map(F, [Valid,Valid]),
%%     [Valid,invalid] = map(F, [Valid,Invalid]),

%%     %% Warmup side trace
%%     map(F, ValidSeq),
    

%%     [Valid,Valid] = map(F, [Valid,Valid]),
%%     [Valid,invalid] = map(F, [Valid,Invalid]),

%%     ok.

%% big_increment_fail() ->

%%     F = fun(E) -> E + 1 end,

%%     %% Warmup
%%     map(F, seq(1,200)),
    

%%     [576460752303423488] = map(F, [576460752303423487]),

%%     ok.

%% %% Untested implemented binary instructions: i_bs_skip_bits_all2 (1,8)

%% get_binary_utf8() ->

%%     %% This is almost the length_b function taken from string
%%     F = fun(Bin) -> length_b(Bin) end,

%%     %% Warmup
%%     map(F, [<<"abcdefghij">> || _I <- seq(1,200)]),
    
%%     map(F, [<<"abcdΩfghijk"/utf8>> || _I <- seq(1,200)]),
    

%%     11 = F(<<"abcdefghijk">>),
%%     10 = F(<<"abcdefghij">>),
%%     9 = F(<<"abcdefghi">>),
%%     8 = F(<<"abcdefgh">>),

%%     11 = F(<<"abcdÈfghijk"/utf8>>),
%%     11 = F(<<"abcdΩfghijk"/utf8>>),

%%     ok.

%% -define(ASCII_LIST(CP1,CP2), CP1 < 256, CP2 < 256, CP1 =/= $\r).

%% length_b(<<CP1/utf8, Bin/binary>>) ->
%%     length_b(Bin, CP1, 0).

%% length_b(<<CP2/utf8, Rest/binary>>, CP1, N)
%%   when ?ASCII_LIST(CP1,CP2) ->
%%     length_b(Rest, CP2, N+1);
%% length_b(<<>>, _CP1, N) ->
%%     N+1;
%% length_b(Bin0, CP1, N) ->
%%     [_|Bin1] = unicode_util:gc([CP1|Bin0]),
%%     case unicode_util:cp(Bin1) of
%%         [] -> N+1;
%%         [CP3|Bin] -> length_b(Bin, CP3, N+1)
%%     end.

%% get_binary_int8() ->

%%     Bin = << <<I:8>> || I <- seq(0,255)>>,

%%     F = fun F(<<C:8, R/binary>>) when C < 127 ->
%%                 F(R);
%%             F(<<C:8, R/binary>>) ->
%%                 [C, R]
%%         end,

%%     %% Warmup
%%     map(F, [Bin || _I <- seq(1,200)]),
    
%%     map(F, [Bin || _I <- seq(1,200)]),
    

%%     [127, <<128:8,_/binary>>] = F(Bin),

%%     ok.

%% get_binary_int16() ->

%%     Bin = << <<I:16>> || I <- seq(0,255)>>,

%%     F = fun F(<<C:16, R/binary>>) when C < 127 ->
%%                 F(R);
%%             F(<<C:16, R/binary>>) ->
%%                 [C, R]
%%         end,

%%     %% Warmup
%%     map(F, [Bin || _I <- seq(1,200)]),
    
%%     map(F, [Bin || _I <- seq(1,200)]),
    

%%     [127, <<128:16,_/binary>>] = F(Bin),

%%     ok.

%% get_binary_int32() ->

%%     Bin = << <<I:32>> || I <- seq(0,255)>>,

%%     F = fun F(<<C:32, R/binary>>) when C < 127 ->
%%                 F(R);
%%             F(<<C:32, R/binary>>) ->
%%                 [C, R]
%%         end,

%%     %% Warmup
%%     map(F, [Bin || _I <- seq(1,200)]),
    
%%     map(F, [Bin || _I <- seq(1,200)]),
    

%%     [127, <<128:32,_/binary>>] = F(Bin),

%%     ok.

%% get_binary_imm() ->

%%     Bin = << <<I:8>> || I <- seq(0,255)>>,

%%     F = fun F(<<C:8, _B:1/binary, R/binary>>) when C < 127 ->
%%                 F(R);
%%             F(<<C:8, B:1/binary, R/binary>>) ->
%%                 [C, B, R]
%%         end,

%%     %% Warmup
%%     map(F, [Bin || _I <- seq(1,200)]),
    
%%     map(F, [Bin || _I <- seq(1,200)]),
    

%%     [128, <<129>>, <<130:8,_/binary>>] = F(Bin),

%%     ok.

%% get_binary_imm_bits() ->

%%     Bin = << <<I:8>> || I <- seq(0,255)>>,
%%     PaddedBin = <<0:1, Bin/binary>>,
%%     <<_:1, UnalignedBin/bits>> = PaddedBin,


%%     ok = get_binary_imm_bits(
%%            fun get_binary_imm_bits_fun_4/1,
%%            Bin, UnalignedBin,
%%            fun([10, <<11>>, <<12:8,_/binary>>]) -> ok end),

%%     ok = get_binary_imm_bits(
%%            fun get_binary_imm_bits_fun_4s/1,
%%            Bin, UnalignedBin,
%%            fun([-6, <<11>>, <<12:8,_/binary>>]) -> ok end),

%%     ok = get_binary_imm_bits(
%%            fun get_binary_imm_bits_fun_12/1,
%%            Bin, UnalignedBin,
%%            fun([515, <<4>>, <<5:8,_/binary>>]) -> ok end),

%%     ok = get_binary_imm_bits(
%%            fun get_binary_imm_bits_fun_59/1,
%%            Bin, UnalignedBin,
%%            fun([8865217259568, <<56>>, <<8:5,_/binary>>]) -> ok end),

%%     ok = get_binary_imm_bits(
%%            fun get_binary_imm_bits_fun_59s/1,
%%            Bin, UnalignedBin,
%%            fun([-240752705118094144,<<196>>,<<50:6,_/binary>>]) -> ok end),

%%     ok = get_binary_imm_bits(
%%            fun get_binary_imm_bits_fun_64/1,
%%            Bin, UnalignedBin,
%%            fun([7161960797921896810,<<"k">>,<<108:8,_/binary>>]) -> ok end),

%%     ok.

%% get_binary_imm_bits(Fun, Bin, UnalignedBin, Res) ->

    

%%     %% Warmup
%%     map(Fun, [Bin || _I <- seq(1,200)]),
    
%%     map(Fun, [UnalignedBin || _I <- seq(1,200)]),
    

%%     ok = Res(Fun(Bin)),
%%     ok = Res(Fun(UnalignedBin)),
%%     ok.

%% get_binary_imm_bits_fun_4(<<C:4, _B:1/binary, R/bits>>) when C < 8 ->
%%     get_binary_imm_bits_fun_4(R);
%% get_binary_imm_bits_fun_4(<<C:4, B:1/binary, R/bits>>) ->
%%     [C, B, R].

%% get_binary_imm_bits_fun_4s(<<C:4/signed, _B:1/binary, R/bits>>) when C >= 0 ->
%%     get_binary_imm_bits_fun_4s(R);
%% get_binary_imm_bits_fun_4s(<<C:4/signed, B:1/binary, R/bits>>) ->
%%     [C, B, R].

%% get_binary_imm_bits_fun_12(<<C:12, _B:1/binary, R/bits>>) when C < 8 ->
%%     get_binary_imm_bits_fun_12(R);
%% get_binary_imm_bits_fun_12(<<C:12, B:1/binary, R/bits>>) ->
%%     [C, B, R].

%% get_binary_imm_bits_fun_59(<<C:59, _B:1/binary, R/bits>>) when C < 8 ->
%%     get_binary_imm_bits_fun_59(R);
%% get_binary_imm_bits_fun_59(<<C:59, B:1/binary, R/bits>>) ->
%%     [C, B, R].

%% get_binary_imm_bits_fun_59s(<<C:59/signed, _B:1/binary, R/bits>>) when C >= 0 ->
%%     get_binary_imm_bits_fun_59s(R);
%% get_binary_imm_bits_fun_59s(<<C:59/signed, B:1/binary, R/bits>>) ->
%%     [C, B, R].

%% get_binary_imm_bits_fun_64(<<C:64, _B:1/binary, R/bits>>) when C < 6513452424949962400 ->
%%     get_binary_imm_bits_fun_64(R);
%% get_binary_imm_bits_fun_64(<<C:64, B:1/binary, R/bits>>) ->
%%     [C, B, R].

%% get_binary_all() ->

%%     Bin = << <<I:8>> || I <- seq(0,255)>>,

%%     F = fun(<<C:8, R/binary>>) when C == 0 ->
%%                 {C, R};
%%            (_) ->
%%                 nok
%%         end,

%%     %% Warmup
%%     map(F, [Bin || _I <- seq(1,200)]),
    

%%     {0, <<1:8,_/binary>>} = F(Bin),

%%     ok.

%% get_binary_skip_imm() ->

%%     Bin = << <<I:8>> || I <- seq(0,255)>>,

%%     F = fun F(<<C:8, _B:1/binary, R/binary>>) when C < 127 ->
%%                 F(R);
%%             F(<<C:8, _B:1/binary, R/binary>>) ->
%%                 [C, R]
%%         end,

%%     %% Warmup
%%     map(F, [Bin || _I <- seq(1,200)]),
    
%%     map(F, [Bin || _I <- seq(1,200)]),
    

%%     [128, <<130:8,_/binary>>] = F(Bin),

%%     ok.

%% binary_restore() ->

%%     Bin = << <<I:8>> || I <- seq(0,255)>>,

%%     F = fun F(<<1:1, B:7, R/binary>>) ->
%%                 [1, B, R];
%%             F(<<_C:8, R/binary>>) ->
%%                 F(R)
%%         end,

%%     %% Warmup
%%     map(F, [Bin || _I <- seq(1,200)]),
    
%%     map(F, [Bin || _I <- seq(1,200)]),
    

%%     [1, 0, <<129:8,_/binary>>] = F(Bin),

%%     ok.

%% test_binary_tail_imm() ->

%%     Bin = << <<I:8>> || I <- seq(0,255)>>,

%%     F = fun F(<<_:1/binary>>) ->
%%                 ok;
%%             F(B) ->
%%                 %% This also tests context to binary
%%                 <<_:8,R/binary>> = B,
%%                 F(R)
%%         end,

%%     %% Warmup
%%     map(F, [Bin || _I <- seq(1,200)]),
    
%%     map(F, [Bin || _I <- seq(1,200)]),
    

%%     [ok] = map(F, [<<0,1,2,3,4>>]),

%%     ok.

%% %% Test that match contexts are re-used across
%% %% function boundaries correctly.
%% %% This function should only create 1 match-context
%% %% but at the moment it creates 3.
%% test_binary_subfunction() ->

%%     Bin = << <<I:8>> || I <- seq(0,255)>>,

%%     F0 = fun(<<I:8,Rest/binary>>) ->
%%                  {I,Rest}
%%          end,

%%     F = fun(<<I:8,Rest/binary>>) ->
%%                 {J,<<K:8,Rest2/binary>>} = F0(Rest),
%%                 {I,J,K, Rest2}
%%         end,

%%     %% Warmup
%%     map(F, [Bin || _I <- seq(1,200)]),
    
%%     map(F, [Bin || _I <- seq(1,200)]),
    

%%     ok.

%% new_map() ->

%%     F = fun(E) ->
%%                 #{ E => E }
%%         end,

%%     %% Warmup
%%     map(F, seq(1,200)),
    

%%     [#{ a := a }, #{ b := b }] = map(F, [a, b]),
%%     ok.

%% new_map_update() ->

%%     F = fun(E) ->
%%                 #{ E => E, [E] => [E], {E} => {E} }
%%         end,

%%     %% Warmup
%%     map(F, seq(1,200)),
    

%%     [#{ a := a, {a} := {a}, [a] := [a] },
%%      #{ b := b, {b} := {b}, [b] := [b] }] = map(F, [a, b]),

%%     ok.

%% new_map_lit() ->

%%     F = fun(E) ->
%%                 #{ 1 => E, 2 => [E], 3 => {E}}
%%         end,

%%     %% Warmup
%%     map(F, seq(1,200)),
    

%%     [#{ 1 := a, 2 := [a], 3 := {a} },
%%      #{ 1 := b, 2 := [b], 3 := {b} }] = map(F, [a, b]),

%%     ok.

%% new_hashmap() ->

%%     %% Test 
%%     F = fun(E) ->
%%                 #{
%%                   11 => E, 21 => E, 31 => E,
%%                   12 => E, 22 => E, 32 => E,
%%                   13 => E, 23 => E, 33 => E,
%%                   14 => E, 24 => E, 34 => E,
%%                   15 => E, 25 => E, 35 => E,
%%                   16 => E, 26 => E, 36 => E,
%%                   17 => E, 27 => E, 37 => E,
%%                   18 => E, 28 => E, 38 => E,
%%                   19 => E, 29 => E, 39 => E,
%%                   40 => E, 50 => E, 60 => E,
%%                   41 => E, 51 => E, 61 => E
%%                  }
%%         end,

%%     %% Warmup
%%     map(F, seq(1,200)),
    

%%     [#{ 11 := a, 12 := a, 13 := a, 61 := a },
%%      #{ 11 := b, 12 := b, 13 := b, 61 := b }] = map(F, [a, b]),

%%     ok.

%% get_map_element() ->
%%     M = id(#{ 1 => a, 2 => b, 3 => c}),

%%     F = fun(K) ->
%%                  case M of
%%                      #{ K := V } -> V;
%%                      _ -> error
%%                  end
%%         end,

%%     %% Warmup
%%     map(F, [1 || _ <- seq(1,200)]),
    
%%     map(F, [u || _ <- seq(1,200)]),
    

%%     [a, b, c, error] = map(F, [1, 2, 3, 4]),

%%     ok.

%% get_map_elements() ->
%%     M = id(#{ 1 => a, 2 => b, 3 => c}),
%%     Maps = [M || _ <- seq(1,200)],

%%     F = fun(#{ 1 := V1, 2 := V2, 3 := V3 }) ->
%%                 {V1, V2, V3};
%%            (_) ->
%%                 error
%%         end,

%%     %% Warmup
%%     map(F, Maps),
    
%%     map(F, [#{} || _ <- seq(1,200)]),
    
    
%%     [{a,b,c}, {b,a,z}, error, error] = map(F, [#{ 1 => a, 2 => b, 3 => c },
%%                                                #{ 1 => b, 2 => a, 3 => z },
%%                                                #{ }, #{ 2 => d, 3 => b }]),

%%     ok.

%% get_map_element_hash() ->

%%     M = id(#{ 1 => a, 2 => b, 3 => c}),
%%     Maps = [M || _ <- seq(1,200)],

%%     F = fun(#{ 1 := V1, 2 := V2 }) ->
%%                 {V1, V2};
%%            (#{ 1 := V }) ->
%%                 V;
%%            (_) ->
%%                 error
%%         end,

%%     %% Warmup
%%     map(F, Maps),
    
%%     map(F, [#{} || _ <- seq(1,200)]),
    
    
%%     [a, {b, a}, error, d] = map(F, [#{ 1 => a }, #{ 1 => b, 2 => a }, #{ }, #{ 1 => d }]),

%%     ok.

%% update_map_assoc() ->

%%     F = fun({K, V}, M) ->
%%                 M#{ K => V };
%%            (V, M) ->
%%                 M#{ a => V }
%%         end,

%%     %% Warmup
%%     foldl(F, #{}, [{I, I*2} || I <- seq(1,200)]),
    
%%     foldl(F, #{}, [I + 2000 || I <- seq(1,200)]),
    

%%     #{ 1 := 2, 2 := 4, 8 := 16, a := 10} = foldl(F, #{}, [{1, 2}, {2, 4}, {8, 16}, 3, 10]),

%%     ok.

%% update_map_exact() ->

%%     OrigMap = #{ 1 => 0 },

%%     F = fun({K, V}, M) ->
%%                 M#{ K := V };
%%            (V, M) ->
%%                 M#{ 1 := V }
%%         end,

%%     %% Warmup
%%     foldl(F, OrigMap, [{1, I} || I <- seq(1,200)]),
    
%%     foldl(F, OrigMap, seq(1,200)),
    

%%     #{ 1 := 8 } = foldl(F, #{1 => 0}, [{1, 2}, {1, 4}, 8]),

%%     ok.


%% update_map_chain() ->

%%     F = fun({K, V}, M) ->
%%                 M1 = M#{ K => V },
%%                 #{ K := V } = M1
%%         end,

%%     %% Warmup
%%     foldl(F, #{}, [{I, I*2} || I <- seq(1,200)]),
    

%%     #{ 1 := 2, 3 := 4 } = foldl(F, #{}, [{1,2},{3,4}]),

%%     ok.

%% jump_on_val_zero() ->

%%     F = fun(0) ->
%%                 a;
%%            (1) ->
%%                 b;
%%            (2) ->
%%                 c;
%%            (A) ->
%%                 A
%%         end,

%%     %% Warmup
%%     map(F, [0 || _I <- seq(1,200)]),
    
%%     map(F, [1 || _I <- seq(1,200)]),
    
%%     map(F, [z || _I <- seq(1,200)]),
    
%%     map(F, [3 || _I <- seq(1,200)]),
    
%%     map(F, [-1 || _I <- seq(1,200)]),
    

%%     [a,b,a,b,a,c,3,z,-1] = map(F,[0,1,0,1,0,2,3,z,-1]),
%%     ok.

%% jump_on_val() ->

%%     F = fun(10) ->
%%                 a;
%%            (11) ->
%%                 b;
%%            (12) ->
%%                 c;
%%            (A) ->
%%                 A
%%         end,

%%     %% Warmup
%%     map(F, [10 || _I <- seq(1,200)]),
    
%%     map(F, [11 || _I <- seq(1,200)]),
    
%%     map(F, [z || _I <- seq(1,200)]),
    
%%     map(F, [13 || _I <- seq(1,200)]),
    

%%     [a,b,a,b,a,c,13,z] = map(F,[10,11,10,11,10,12,13,z]),

%%     ok.

%% select_arity() ->

%%     %% Create a select arity val
%%     F = fun({add,N,M}) ->
%%                 N + M;
%%            ({sub,N,M}) ->
%%                 N - M;
%%            ({mul,N,M}) ->
%%                 N * M;
%%            ({neg,N}) ->
%%                 -N;
%%            (N) ->
%%                 N
%%         end,

%%     %% Warmup
%%     map(F, [{add,1,2} || X <- seq(1, 200)]),
    
%%     map(F, [{sub,1,2} || X <- seq(1, 200)]),
    
%%     map(F, [{neg,2} || X <- seq(1, 200)]),
    
%%     map(F, [m || X <- seq(1, 200)]),
    

%%     [3,5,0,100,m,-100] = map(F, [{add,1,2},{add,3,2},{sub,5,5},{mul,10,10},m,{neg,100}]),

    
    

%%     %% Test that the fail label can be hot
%%     %% Warmup
%%     map(F, [m || X <- seq(1, 200)]),
    
%%     map(F, [{sub,1,2} || X <- seq(1, 200)]),
    

%%     [3,5,0,100,m,-100] = map(F, [{add,1,2},{add,3,2},{sub,5,5},{mul,10,10},m,{neg,100}]),

%%     ok.

%% select_bins() ->

%%     Strings = ["åäö","ÅÄÖ", "ÅäÖ"],

%%     F = fun(E) -> unicode_util:lowercase(E) end,

%%     %% Warmup
%%     map(F, [integer_to_list(X) || X <- seq(1, 200)]),
    

%%     ["åäö","åÄÖ","åäÖ"] = map(F, Strings),

%%     ok.

%% select_bins_small() ->

%%     Strings = ["åäö","ÅÄÖ", "ÅäÖ"],

%%     F = fun select_bins_small/1,

%%     %% Warmup
%%     map(F, [integer_to_list(X) || X <- seq(1, 200)]),
    

%%     ["åäö","åÄÖ","åäÖ"] = map(F, Strings),

%%     ok.

%% %% 11 clauses needed for loader to make it into a binary search
%% select_bins_small([$A|T]) ->
%%     [$a|T];
%% select_bins_small([$B|T]) ->
%%     [$b|T];
%% select_bins_small([$C|T]) ->
%%     [$c|T];
%% select_bins_small([$H|T]) ->
%%     [$h|T];
%% select_bins_small([$J|T]) ->
%%     [$j|T];
%% select_bins_small([$Å|T]) ->
%%     [$å|T];
%% select_bins_small([$Z|T]) ->
%%     [$z|T];
%% select_bins_small([$Ä|T]) ->
%%     [$ä|T];
%% select_bins_small([$Ö|T]) ->
%%     [$ö|T];
%% select_bins_small([$T|T]) ->
%%     [$t|T];
%% select_bins_small([$S|T]) ->
%%     [$s|T];
%% select_bins_small(E) ->
%%     E.

%% nested_calls() ->

%%     F1 = fun(I) when I rem 3 == 2 ->  I + 1;
%%             (I) -> I + I end,
%%     F2 = fun(I) ->
%%                  {I,F1(I)}
%%          end,

%%     %% Warmup
%%     map(F2, seq(1, 200)),
    
%%     map(F2, seq(1, 200)),
    

%%     [{1,2},{2,3},{3,6},{4,8},{5,6},{6,12}] = map(F2, [1, 2, 3, 4, 5, 6]),

%%     ok.

%% fold() ->

%%     F1 = fun(I) when I rem 3 == 2 ->  I + 1;
%%             (I) -> I + I end,
%%     F2 = fun(I) when I rem 3 == 2 ->  I + 1;
%%             (I) ->  I * I end,
%%     F3 = fun(I) when I rem 3 == 2 ->  I + 1;
%%             (I) ->  I - I end,
%%     F4 = fun(I) when I rem 3 == 2 ->  I + 1;
%%             (I) ->  I div I end,

%%     %% Warmup
%%     map(F1, seq(1, 200)),
    
%%     map(F2, seq(1, 200)),
    
%%     map(F3, seq(1, 200)),
    
%%     map(F4, seq(1, 200)),
    

%%     map(F1, seq(1, 200)),
    
%%     map(F2, seq(1, 200)),
    
%%     map(F3, seq(1, 200)),
    
%%     map(F4, seq(1, 200)),
    

%% %    [16,32,48,64,80] = map(F, Valid),

%%     ok.

%% seqmap4() ->
%%     F0 = fun(X) -> X + 1 end,
%%     F1 = fun(X) -> X + 2 end,
%%     F2 = fun(X) -> X + 3 end,
%%     F3 = fun(X) -> X + 4 end,

%%     Data = seq(1, 1000),

%%     F = fun(_E) -> map(F3, map(F2, map(F1, map(F0, Data)))) end,

%%     %% Warmup
%%     map(F, seq(1, 400)),
    
%%     map(F, seq(1, 200)),
%%     ok.

%% mean() ->
%%     %% Warmup
%%     F = fun(_) ->
%%                 mean(25000)
%%         end,
%%     map(F, seq(1, 200)),
    
%%     map(F, seq(1, 200)),
%%     ok.

%% mean(N) ->
%%   sum(duplicate(N, round(math:pi()))) div N.

%% duplicate(N, X) ->
%%   duplicate(N, X, []).

%% duplicate(0, _, Acc) -> Acc;
%% duplicate(N, X, Acc) -> duplicate(N-1, X, [X|Acc]).

%% sum(X) -> sum(X, 0).
%% sum([], Acc) -> Acc;
%% sum([X|Xs], Acc) -> sum(Xs, X+Acc).

%% map(F, [H|T]) ->
%%     [F(H)|map(F, T)];
%% map(F, []) when is_function(F, 1) -> [].

%% foldl(F, A, []) when is_function(F, 2) ->
%%     A;
%% foldl(F, A, [H|T]) ->
%%     foldl(F, F(H,A), T).

%% seq(N,M) when N > M ->
%%     [];
%% seq(N,M) ->
%%     [N|seq(N+1,M)].
