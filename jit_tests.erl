-module(jit_tests).

-export([test/0, test/1, test/2, id/1]).

id(N) -> N.

tests() ->
    Fns = jit_tests_cases:module_info(exports),
    [{F, erlang:make_fun(jit_tests_cases, F, N)} ||
        {F, N} <- Fns, F /= module_info].
    %% [
    %%  {is_nil, fun jit_tests_cases:is_nil/0},
    %%  {literal, fun jit_tests_cases:literal/0}
     %% {idiv, fun jit_tests_cases:idiv/0},
     %% {'bnot', fun jit_tests_cases:'bnot'/0},
     %% {environment, fun jit_tests_cases:environment/0},
     %% {environment_guard, fun jit_tests_cases:environment_guard/0},
     %% {export, fun jit_tests_cases:export/0},
     %% {make_fun, fun jit_tests_cases:make_fun/0},
     %% {big_increment_fail, fun jit_tests_cases:big_increment_fail/0},
     %% {'catch', fun jit_tests_cases:'catch'/0},
     %% {try_catch, fun jit_tests_cases:'try_catch'/0},

     %% %% Bifs
     %% {is_function, fun jit_tests_cases:is_function/0},
     %% {length, fun jit_tests_cases:length/0},
     %% {length_guards, fun jit_tests_cases:length_guards/0},
     %% {pdict, fun jit_tests_cases:pdict/0},
     %% {bif_call, fun jit_tests_cases:bif_call/0},
     %% {bif_call_only, fun jit_tests_cases:bif_call_only/0},
     %% {bif_call_only_return, fun jit_tests_cases:bif_call_only_return/0},
     %% {bif_trap, fun jit_tests_cases:bif_trap/0},
     %% {bif_only_trap, fun jit_tests_cases:bif_only_trap/0},
     %% {ubif_call_succ, fun jit_tests_cases:ubif_call_succ/0},
     %% {ubif_call_fail, fun jit_tests_cases:ubif_call_fail/0},

     %% %% Get Binary
     %% {get_binary_utf8, fun jit_tests_cases:get_binary_utf8/0},
     %% {get_binary_int8, fun jit_tests_cases:get_binary_int8/0},
     %% {get_binary_int16, fun jit_tests_cases:get_binary_int16/0},
     %% {get_binary_int32, fun jit_tests_cases:get_binary_int32/0},
     %% {get_binary_imm, fun jit_tests_cases:get_binary_imm/0},
     %% {get_binary_imm_bits, fun jit_tests_cases:get_binary_imm_bits/0},
     %% {get_binary_all, fun jit_tests_cases:get_binary_all/0},
     %% {get_binary_skip_imm, fun jit_tests_cases:get_binary_skip_imm/0},
     %% {binary_restore, fun jit_tests_cases:binary_restore/0},
     %% {test_binary_tail_imm, fun jit_tests_cases:test_binary_tail_imm/0},
     %% {test_binary_subfunction, fun jit_tests_cases:test_binary_subfunction/0},

     %% %% Maps
     %% {new_map, fun jit_tests_cases:new_map/0},
     %% {new_map_update, fun jit_tests_cases:new_map_update/0},
     %% {new_map_lit, fun jit_tests_cases:new_map_lit/0},
     %% {new_hashmap, fun jit_tests_cases:new_hashmap/0},
     %% {get_map_element, fun jit_tests_cases:get_map_element/0},
     %% {get_map_elements, fun jit_tests_cases:get_map_elements/0},
     %% {get_map_element_hash, fun jit_tests_cases:get_map_element_hash/0},
     %% {update_map_assoc, fun jit_tests_cases:update_map_assoc/0},
     %% {update_map_exact, fun jit_tests_cases:update_map_exact/0},
     %% {update_map_chain, fun jit_tests_cases:update_map_chain/0},

     %% %% Control flow
     %% {jump_on_val_zero, fun jit_tests_cases:jump_on_val_zero/0},
     %% {jump_on_val, fun jit_tests_cases:jump_on_val/0},
     %% {fold, fun jit_tests_cases:fold/0},
     %% {select_arity, fun jit_tests_cases:select_arity/0},
     %% {select_bins, fun jit_tests_cases:select_bins/0},
     %% {select_bins_small, fun jit_tests_cases:select_bins_small/0},
     %% {nested_calls, fun jit_tests_cases:nested_calls/0},
     %% {seqmap4, fun jit_tests_cases:seqmap4/0},
     %% {mean, fun jit_tests_cases:mean/0}
    %% ].

test(Test) when is_atom(Test) ->
    test([Test]);
test(Tests) when is_list(Tests) ->
    [test(Test, proplists:get_value(Test, tests())) || Test <- Tests].
test(Name, F) ->
    catch jit_compiler:save_traces(),
    erlang:display({testcase,Name}),
    ok = F().

test() ->
    [test(Name, F) || {Name, F} <- tests()],
    io:format("All tests ok~n").
