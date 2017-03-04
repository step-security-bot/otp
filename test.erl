-module(test).

%-compile(inline).
%-compile({inline_size,100000}).
-compile(inline_list_funcs).
-export([test/1,test/2]).

test(L) ->
    lists:foldl(fun(A, Acc) ->
                        A + Acc
                end, 0, L).


test(A, B) ->
    C = case A of
            {ok, B, D} ->
                D;
            {meep, E, L} ->
                {E,L}
        end,
    test(C).

orig() ->
    {function,'-test/1-lists^foldl/2-0-',3,8,
     [{label,7},
      {func_info,{atom,test},{atom,'-test/1-lists^foldl/2-0-'},3},
      {label,8},
      {test,is_nonempty_list,{f,9},[{x,0}]},
      {block,[{set,[],[],{alloc,3,{nozero,2,0,[]}}},
              {set,[{x,0},{y,1}],[{x,0}],get_list},
              {set,[{y,0}],[{x,2}],move}]},
      {call_fun,2},
      {block,[{set,[{x,1}],[{x,0}],move},
              {set,[{x,2}],[{y,0}],move},
              {set,[{x,0}],[{y,1}],move}]},
      {call,3,{f,8}},
      {deallocate,2},
      return,
      {label,9},
      {test,is_nil,{f,10},[{x,0}]},
      {test,is_function2,{f,10},[{x,2},{integer,2}]},
      {block,[{set,[{x,0}],[{x,1}],move}]},
      return,
      {label,10},
      {block,[{set,[],[],{alloc,3,{nozero,nostack,6,[]}}},
              {set,[{x,0}],[{x,0},nil],put_list},
              {set,[{x,0}],[{x,1},{x,0}],put_list},
              {set,[{x,1}],[{x,2},{x,0}],put_list},
              {set,[{x,0}],[{atom,function_clause}],move}]},
      {call_ext,2,{extfunc,erlang,error,2}}]}.

ssa() ->
    {function,'-test/1-lists^foldl/2-0-',3,[a,b,c],
     [{block, ba, [{test,is_nonempty_list,[a],bb,bc}]},
      {block, bb, [{set,[bb_a,bb_b],[a],           get_list},
                   {set,[bb_c],     [bb_a,b,c],    call_fun},
                   {set,[bb_d],     [bb_b,bb_c,c], {call,{block,ba}}},
                   {set,[],         [bb_d],        return}]},
      {block, bc, [{test,is_nil,[a],bd,be}]},
      {block, bd, [{test,is_function2,[c],bf,be}]},
      {block, be, [{set,[be_a],[a, {const, []}], put_list},
                   {set,[be_b],[b, bb_a], put_list},
                   {set,[be_c],[c, bb_b], put_list},
                   {set,[be_d],[{const, function_clause}, bb_c],
                    {call_ext,{extfunc,erlang,error,2}}}]},
      {block, bf, [{set,[],[b],return}]}]}.

orig2() ->
    [{label,3},
     {func_info,{atom,test},{atom,test},2},
     {label,4},
     {block,[{set,[],[],{alloc,2,{zero,0,0,[]}}}]},
     {test,is_tuple,{f,8},[{x,0}]},
     {test,test_arity,{f,8},[{x,0},3]},
     {block,
      [{set,[{x,2}],[{x,0}],{get_tuple_element,0}},
       {set,[{x,3}],[{x,0}],{get_tuple_element,1}},
       {set,[{x,4}],[{x,0}],{get_tuple_element,2}}]},
     {test,is_atom,{f,8},[{x,2}]},
     {select,select_val,
      {x,2},
      {f,8},
      [{atom,ok},{f,5},{atom,meep},{f,6}]},
     {label,5},
     {test,is_eq_exact,{f,8},[{x,3},{x,1}]},
     {block,[{set,[{x,0}],[{x,4}],move}]},
     {jump,{f,7}},
     {label,6},
     {block,
      [{set,[],[],{alloc,5,{nozero,nostack,3,[]}}},
       {set,[{x,0}],[],{put_tuple,2}},
       {set,[],[{x,3}],put},
       {set,[],[{x,4}],put}]},
     {label,7},
     {call,1,{f,2}},
     {deallocate,0},
     return,
     {label,8},
     {case_end,{x,0}}].

ssa2() ->
    {function,test,2,[a,b],
     [{block, ba, [{test,is_tuple,[a],bb,bc}]},
      {block, bb, [{test,test_arity,[a],bd,bc}]},
      {block, bc, [{set,[],[a],case_end}]}
      {block, bd, [{set,[bd_a],[a, {const, 0}],get_tuple_element},
                   {set,[bd_b],[a, {const, 1}],get_tuple_element},
                   {set,[bd_c],[a, {const, 2}],get_tuple_element},
                   {test,is_atom,[bd_a],be,bc}]},
      {block, be, [{test,select_val,[bd_a],bc,
                    [{{const, ok}, bf}, {{const, meep}, bg}]}]},
      {block, bf, [{test,is_eq_exact,[db_b,b],bh,bc}]},
      {block, bg, [{set,[bg_a],[bd_b,bd_c],put_tuple},
                   {jump, bh}]},
      {block, bh, [{phi,[bh_a],[{bd_c, bf}, {bg_a, bg}]},
                   {set,[bh_b],[bh_a],{call, 1, test}},
                   {set,[],[bh_b],return}]}]}.
