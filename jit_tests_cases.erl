-module(jit_tests_cases).

-export([is_nil/0, literal/0, idiv/0, get_list/0, get_tl/0, get_hd/0,
         'is_atom'/0, 'is_boolean'/0, 'is_binary'/0, is_bitstring/0,
         'is_float'/0, 'is_function'/0, is_function2/0, is_integer/0,
         is_list/0, is_map/0, is_number/0, is_port/0, is_pid/0,
         is_reference/0, is_tagged_tuple/0, is_tuple/0, is_tuple_of_arity/0,
         guard_bif/0, length/0, call_light_bif/0, call_light_bif_only/0]).

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

guard_bif() ->
    % body arity 1
    5 = byte_size(id(<<0,1,2,3,4>>)),
    {'EXIT',{badarg,[{erlang,byte_size,[a],[]}|_]}} = (catch byte_size(id(a))),
    % body arity 2
    <<1,2>> = binary_part(id(<<0,1,2,3,4>>), {1, 2}),
    {'EXIT',{badarg,[{erlang,binary_part,[a,{1,2}],[]}|_]}} = (catch binary_part(id(a), {1, 2})),
    % body arity 3
    <<1,2>> = binary_part(id(<<0,1,2,3,4>>), 1, 2),
    {'EXIT',{badarg,[{erlang,binary_part,[a,1,2],[]}|_]}} = (catch binary_part(id(a), 1, 2)),

    true = my_byte_size(id(<<0,1,2,3,4>>)),
    false = my_byte_size(id(a)),

    true = my_binary_part(id(<<0,1,2,3,4>>), {1,2}),
    false = my_binary_part(id(a), {1,2}),

    true = my_binary_part(id(<<0,1,2,3,4>>), 1,2),
    false = my_binary_part(id(a), 1,2),

    ok.
my_byte_size(B) when is_integer(byte_size(B)) ->
    true;
my_byte_size(_B) ->
    false.
my_binary_part(B, Pat) when is_binary(binary_part(B,Pat)) ->
    true;
my_binary_part(_B, _Pat) ->
    false.
my_binary_part(B, S, L) when is_binary(binary_part(B,S,L)) ->
    true;
my_binary_part(_B, _S, _L) ->
    false.

length() ->
    1 = length(id([1])),
    2000 = length(id(lists:duplicate(2000,2))), %% Yield
    {'EXIT',{badarg,[{erlang,length,[a],[]}|_]}} = (catch length(id(a))),
    ok.

call_light_bif() ->
    <<131,100,0,1,97>> = erlang:term_to_binary(id(a)),
    %% Yield
    B = erlang:term_to_binary(id(lists:duplicate(2000,1))),
    <<131,107,7,208,1,1>> = binary_part(B, 0, 6),
    erlang:garbage_collect(),
    %% GC
    [131,107,7,208,1|_] = erlang:binary_to_list(id(B)),

    {'EXIT',{badarg,[{erlang,binary_to_list,[a],[]}|_]}} = (catch erlang:binary_to_list(id(a))),

    ok.

call_light_bif_only() ->
    <<131,100,0,1,97>> = t2b(id(a)),
    %% Yield
    B = t2b(id(lists:duplicate(2000,1))),
    <<131,107,7,208,1,1>> = binary_part(B, 0, 6),
    erlang:garbage_collect(),
    %% GC
    [131,107,7,208,1|_] = b2l(id(B)),

    {'EXIT',{badarg,[{erlang,binary_to_list,[a],[]}|_]}} = (catch b2l(id(a))),

    ok.

t2b(Arg) ->
    erlang:term_to_binary(Arg).
b2l(Arg) ->
    erlang:binary_to_list(Arg).