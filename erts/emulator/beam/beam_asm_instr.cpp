/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2020. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */
#include <algorithm>
#include "beam_asm.hpp"

extern "C" {
    #include "erl_bif_table.h"
}

extern "C" {
#include "beam_catches.h"
  BeamInstr *apply(Process* p, Eterm* reg, BeamInstr *I, Uint offs);
  BeamInstr *fixed_apply(Process* p, Eterm* reg, BeamInstr *I, Uint offs);
  void get_trace_from_exc(void);
  void add_stacktrace(void);
  Eterm *get_freason_ptr_from_exc(Eterm);
  Eterm call_fun(void);
  Eterm apply_fun(void);
  Eterm new_fun(void);

  BeamInstr* handle_error(Process* c_p, BeamInstr* pc, Eterm* reg, ErtsCodeMFA *bif_mfa);

  extern erts_atomic32_t the_active_code_index;
}

using namespace asmjit;

/* Helpers */

void BeamModuleAssembler::emit_gc_test(ArgVal Ns, ArgVal Nh, ArgVal Live) {
  Uint need = Ns.getValue() + Nh.getValue();
  Label after_gc_check = a.newLabel();

  // Build the GC Test
  a.mov(TMP1, E);
  a.sub(TMP1, HTOP);
  a.cmp(TMP1, need * sizeof(Eterm));
  a.ja(after_gc_check);

  a.mov(ARG2,need);
  a.mov(ARG4,Live.getValue());
  call((uint64_t)ga->get_garbage_collect());
  a.bind(after_gc_check);
}

void BeamModuleAssembler::emit_dispatch(x86::Gp where, enum beamasm_ret how)
{
  Label yield = a.newLabel();
  // Test if we have to do a yield here
  /* Do the call */
  a.sub(FCALLS,1);
  a.jl(yield);
  a.jmp(where);
  a.bind(yield);
  // we store the address to jump to in TMP3
  if (where != TMP3)
    a.mov(TMP3, where);
  a.mov(RET,how);
  farjmp(ga->get_return());
}

void BeamModuleAssembler::emit_dispatch_rel(ArgVal CallDest) {
  a.lea(TMP3,x86::qword_ptr(labels[CallDest.getValue()]));

//  auto label = std::find(functions.begin(), functions.end(),
//                         CallDest.getValue()-1);
//    emit_dbg_call((ErtsCodeMFA*)getCode(*label));

  emit_dispatch(TMP3);
}

void BeamModuleAssembler::emit_dispatch_return(x86::Gp dest) {
  Label yield = a.newLabel();

  // TMP1 holds where to jump to, set in $RETURN

  a.cmp(FCALLS,0);
  a.jle(yield);

  comment("Do the return");
  a.sub(FCALLS,1);
  a.jmp(dest);

  a.bind(yield);
  comment("Yield to interpreter");
  a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), 0);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, arity)), 1);
  a.mov(TMP3, dest);
  a.mov(RET,RET_context_switch3);
  farjmp(ga->get_return());
}

void BeamModuleAssembler::emit_setup_return(x86::Gp dest) {
  mov(dest,CP);
  mov(CP,ArgVal(ArgVal::TYPE::i,NIL));
}

#ifdef DEBUG
static void validate_term(Eterm term) {
    if (is_boxed(term)) {
        Eterm header = *boxed_val(term);

        if (header_is_bin_matchstate(header)) {
            return;
        }
    }

    size_object_x(term, nullptr);
}
#endif

void BeamModuleAssembler::emit_validate(ArgVal arity) {
#ifdef DEBUG
    Label next = a.newLabel(), crash = a.newLabel();

#ifdef HARD_DEBUG
    emit_heavy_swapout();
#endif

    /* Crash if the native stack is not 16-byte-aligned */
    a.test(x86::rsp, 15);
    a.jne(crash);

    /* Crash if return address is not a valid CP. */
    mov(TMP1, CP);
    a.test(TMP1, _CPMASK);
    a.jne(crash);

    /* Crash if the Erlang heap is not word-aligned */
    a.test(HTOP, sizeof(Eterm) - 1);
    a.jne(crash);

    /* Crash if HTOP > E */
    a.cmp(HTOP, E);
    a.ja(crash);

    a.jmp(next);
    a.bind(crash);
    a.hlt();
    a.bind(next);

#ifdef HARD_DEBUG
    a.push(x86::rbx);
    a.push(x86::rbp);
    a.push(x86::r12);
    a.push(x86::r13);
    a.push(x86::r14);
    a.push(x86::r15);

    for(unsigned i = 0; i < arity.getValue(); i++) {
        a.mov(ARG1, x86::qword_ptr(x_reg, i * sizeof(Eterm)));
        call((uint64_t)validate_term);
    }

    a.pop(x86::r15);
    a.pop(x86::r14);
    a.pop(x86::r13);
    a.pop(x86::r12);
    a.pop(x86::rbp);
    a.pop(x86::rbx);
#endif
#endif
}

/* Instrs */

void BeamModuleAssembler::emit_i_validate(ArgVal Arity, Instruction *Inst) {
    emit_validate(Arity);
}

void BeamModuleAssembler::emit_allocate_heap(ArgVal NeedStack, ArgVal NeedHeap, ArgVal Live, Instruction *Inst) {
  ArgVal needed = NeedStack + 1;
  emit_gc_test(needed, NeedHeap, Live);
  alloc(needed * sizeof(Eterm));
  mov(CP,ArgVal(ArgVal::TYPE::i,NIL));
}

void BeamModuleAssembler::emit_allocate(ArgVal NeedStack, ArgVal Live, Instruction *Inst) {
  emit_allocate_heap(NeedStack, ArgVal(ArgVal::TYPE::u, 0), Live);
}

void BeamModuleAssembler::emit_allocate_heap_zero(ArgVal NeedStack, ArgVal NeedHeap, ArgVal Live, Instruction *Inst) {
  emit_allocate_heap(NeedStack, NeedHeap, Live);
  for (unsigned i = 0; i < NeedStack.getValue(); i++) {
    a.mov(x86::qword_ptr(E, 1 + i * sizeof(Eterm)), NIL);
  }
}

void BeamModuleAssembler::emit_allocate_zero(ArgVal NeedStack, ArgVal Live, Instruction *Inst) {
  emit_allocate_heap_zero(NeedStack, ArgVal(ArgVal::TYPE::u, 0), Live);
}

void BeamModuleAssembler::emit_test_heap(ArgVal Nh, ArgVal Live, Instruction *Inst) {
  emit_gc_test(ArgVal(ArgVal::u, 1), Nh, Live);
}

void BeamModuleAssembler::emit_return(Instruction *Inst) {
  /* Validate return address and {x,0} */
  emit_validate(ArgVal(ArgVal::u, 1));

  emit_setup_return(TMP4);
  emit_dispatch_return(TMP4);
}

void BeamModuleAssembler::emit_deallocate(ArgVal Deallocate, Instruction *Inst) {
  dealloc(Deallocate);
}

void BeamModuleAssembler::emit_i_call(ArgVal CallDest, Instruction *Inst) {
  Label ret = a.newLabel();

  /* Save the return CP on the stack */
  a.lea(TMP1, x86::qword_ptr(ret));
  mov(CP,TMP1);

  emit_dispatch_rel(CallDest);

  // Need to align this label in order for it to be recognized as is_CP
  a.align(kAlignCode, 8);
  a.bind(ret);
}

void BeamModuleAssembler::emit_i_call_last(ArgVal CallDest, ArgVal Deallocate, Instruction *Inst) {
  emit_deallocate(Deallocate);
  emit_dispatch_rel(CallDest);
}

void BeamModuleAssembler::emit_i_call_only(ArgVal CallDest, Instruction *Inst) {
  emit_dispatch_rel(CallDest);
}

void BeamModuleAssembler::emit_dispatch_export(ArgVal Exp) {
  Label yield = a.newLabel(), dispatch = a.newLabel();

  a.mov(ARG1, (uint64_t)(&the_active_code_index));
  a.mov(x86::edi, x86::dword_ptr(ARG1));

  /* Load export pointer / addressv */
  ERTS_CT_ASSERT(offsetof(Export, addressv) == 0);
  make_move_patch(ARG2, imports[Exp.getValue()].patches);
  a.mov(TMP3, x86::qword_ptr(ARG2, x86::edi, 3));

  /* Do the call */
  a.sub(FCALLS,1);
  a.jl(yield);
  a.bind(dispatch);
  a.jmp(TMP3);

  a.bind(yield);
  /* Yield address is in TMP3 */
  a.mov(RET, RET_context_switch);
  farjmp(ga->get_return());
}

void BeamModuleAssembler::emit_i_call_ext(ArgVal Exp, Instruction *Inst) {
  Label next = a.newLabel();

  /* Save the return CP on the stack */
  a.lea(TMP1, x86::qword_ptr(next));
  mov(CP,TMP1);

  emit_dispatch_export(Exp);

  // Need to align this label in order for it to be recognized as is_CP
  a.align(kAlignCode, 8);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_call_ext_only(ArgVal Exp, Instruction *Inst) {
  emit_dispatch_export(Exp);
}

void BeamModuleAssembler::emit_i_call_ext_last(ArgVal Exp, ArgVal Deallocate, Instruction *Inst) {
  emit_deallocate(Deallocate);
  emit_dispatch_export(Exp);
}

void BeamModuleAssembler::emit_normal_exit(Instruction *Inst) {
  emit_heavy_swapout();
  emit_proc_lc_unrequire();

  a.mov(x86::qword_ptr(c_p,offsetof(Process,freason)), EXC_NORMAL);
  a.mov(x86::qword_ptr(c_p,offsetof(Process,arity)), 0);
  a.mov(ARG1,c_p);
  a.mov(ARG2,imm(am_normal));
  call((uint64_t)erts_do_exit_process);

  emit_proc_lc_require();
  emit_heavy_swapin();

  a.mov(TMP3, x86::qword_ptr(c_p,offsetof(Process,i)));
  a.mov(RET,RET_do_schedule);
  farjmp(ga->get_return());
}

void BeamModuleAssembler::emit_continue_exit(Instruction *Inst) {
  emit_heavy_swapout();
  emit_proc_lc_unrequire();

  a.mov(ARG1,c_p);
  call((uint64_t)erts_continue_exit_process);

  emit_proc_lc_require();
  emit_heavy_swapin();

  a.mov(TMP3, x86::qword_ptr(c_p,offsetof(Process,i)));
  a.mov(RET,RET_do_schedule);
  farjmp(ga->get_return());
}

// this is an alias for handle_error
void BeamModuleAssembler::emit_error_action_code(Instruction *Inst) {
  farjmp(ga->get_error_action_code());
}

x86::Gp BeamModuleAssembler::emit_apply(uint64_t deallocate) {
  Label dispatch = a.newLabel(), entry = a.newLabel();
  a.bind(entry);
  emit_heavy_swapout();
  a.mov(ARG1, c_p);
  a.mov(ARG2, x_reg);
  a.mov(ARG3, 0);
  a.mov(ARG4, deallocate);
  call((uint64_t)apply);
  emit_heavy_swapin();
  a.cmp(RET, 0);
  a.jne(dispatch);
  emit_handle_error(entry, &bif_trap_export[BIF_apply_3]->info.mfa);
  a.bind(dispatch);
  return RET;
}

void BeamModuleAssembler::emit_i_apply(Instruction *Inst) {
  Label next = a.newLabel();
  x86::Gp disp = emit_apply(0);

  /* Save the return CP on the stack */
  a.lea(TMP1, x86::qword_ptr(next));
  mov(CP,TMP1);

  emit_dispatch(disp);

  // Need to align this label in order for it to be recognized as is_CP
  a.align(kAlignCode, 8);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_apply_last(ArgVal Deallocate, Instruction *Inst) {
  x86::Gp disp = emit_apply(Deallocate.getValue());
  emit_deallocate(Deallocate);
  emit_dispatch(disp);
}

void BeamModuleAssembler::emit_i_apply_only(Instruction *Inst) {
  emit_dispatch(emit_apply(0));
}

x86::Gp BeamModuleAssembler::emit_apply(ArgVal Arity, uint64_t deallocate) {
  Label dispatch = a.newLabel(), entry = a.newLabel();
  a.bind(entry);
  emit_heavy_swapout();
  a.mov(ARG1, c_p);
  a.mov(ARG2, x_reg);
  mov(ARG3, Arity);
  if (deallocate) {
    mov(ARG4, Arity);
  } else {
    a.mov(ARG4, 0);
  }
  a.mov(ARG5, deallocate);
  call((uint64_t)fixed_apply);
  emit_heavy_swapin();
  a.cmp(RET, 0);
  a.jne(dispatch);
  emit_handle_error(entry, &bif_trap_export[BIF_apply_3]->info.mfa);
  a.bind(dispatch);
  return RET;
}

void BeamModuleAssembler::emit_apply(ArgVal Arity, Instruction *I) {
  Label next = a.newLabel();
  x86::Gp disp = emit_apply(Arity, (uint64_t)0);

  /* Save the return CP on the stack */
  a.lea(TMP1, x86::qword_ptr(next));
  mov(CP,TMP1);

  emit_dispatch(disp);

  // Need to align this label in order for it to be recognized as is_CP
  a.align(kAlignCode, 8);
  a.bind(next);
}

void BeamModuleAssembler::emit_apply_last(ArgVal Arity, ArgVal Deallocate, Instruction *I) {
  x86::Gp disp = emit_apply(Arity, Deallocate.getValue());
  emit_deallocate(Deallocate);
  emit_dispatch(disp);
}

x86::Gp BeamModuleAssembler::emit_call_fun(ArgVal Fun) {
  Label entry = a.newLabel(), dispatch = a.newLabel();
  a.bind(entry);
  emit_heavy_swapout();
  a.mov(ARG1, c_p);
  mov(ARG2, Fun);
  a.mov(ARG3, x_reg);
  a.mov(ARG4, THE_NON_VALUE);
  call((uint64_t)call_fun);
  emit_heavy_swapin();
  a.cmp(RET, 0);
  a.jne(dispatch);
  emit_handle_error(entry, nullptr);
  a.bind(dispatch);
  return RET;
}

void BeamModuleAssembler::emit_i_call_fun(ArgVal Fun, Instruction *Inst) {
  Label next = a.newLabel();
  x86::Gp disp = emit_call_fun(Fun);

  /* Save the return CP on the stack */
  a.lea(TMP1, x86::qword_ptr(next));
  mov(CP,TMP1);

  emit_dispatch(disp, RET_context_switch_fun);

  // Need to align this label in order for it to be recognized as is_CP
  a.align(kAlignCode, 8);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_call_fun_last(ArgVal Fun, ArgVal Deallocate, Instruction *Inst) {
  x86::Gp disp = emit_call_fun(Fun);
  emit_deallocate(Deallocate);
  emit_dispatch(disp, RET_context_switch_fun);
}

x86::Gp BeamModuleAssembler::emit_apply_fun() {
  Label dispatch = a.newLabel(), entry = a.newLabel();
  a.bind(entry);
  emit_heavy_swapout();
  a.mov(ARG1, c_p);
  mov(ARG2, x0);
  mov(ARG3, x1);
  a.mov(ARG4, x_reg);
  call((uint64_t)apply_fun);
  emit_heavy_swapin();
  a.cmp(RET, 0);
  a.jne(dispatch);
  emit_handle_error(entry, nullptr);
  a.bind(dispatch);
  return RET;
}

void BeamModuleAssembler::emit_i_apply_fun(Instruction *Inst) {
  Label next = a.newLabel();
  x86::Gp disp = emit_apply_fun();

  /* Save the return CP on the stack */
  a.lea(TMP1, x86::qword_ptr(next));
  mov(CP,TMP1);

  emit_dispatch(disp, RET_context_switch_fun);

  // Need to align this label in order for it to be recognized as is_CP
  a.align(kAlignCode, 8);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_apply_fun_last(ArgVal Deallocate, Instruction *Inst) {
  x86::Gp disp = emit_apply_fun();
  emit_deallocate(Deallocate);
  emit_dispatch(disp, RET_context_switch_fun);
}

void BeamModuleAssembler::emit_i_apply_fun_only(Instruction *Inst) {
  emit_dispatch(emit_apply_fun(), RET_context_switch_fun);
}

void BeamModuleAssembler::emit_i_make_fun(ArgVal FunP, ArgVal NumFree, Instruction *Inst) {
  emit_heavy_swapout();
  a.mov(ARG1, c_p);
  a.mov(ARG2, x_reg);
  mov(ARG3, FunP);
  mov(ARG4, NumFree);
  call((uint64_t)new_fun);
  emit_heavy_swapin();
  mov(x0, RET);
}

x86::Mem BeamModuleAssembler::emit_list_val(x86::Gp Src) {
  return x86::qword_ptr(Src, -TAG_PRIMARY_LIST);
}

x86::Mem BeamModuleAssembler::emit_car(x86::Mem Src) {
  return Src;
}

x86::Mem BeamModuleAssembler::emit_cdr(x86::Mem Src) {
  return incr(Src);
}

void BeamModuleAssembler::emit_get_list(ArgVal Src, ArgVal Hd, ArgVal Tl, Instruction *Inst) {
  mov(TMP1, Src);
  x86::Mem lst = emit_list_val(TMP1);
  a.mov(TMP2, emit_car(lst)); // get car
  a.mov(TMP3, emit_cdr(lst)); // get cdr
  mov(Hd,TMP2);
  mov(Tl,TMP3);
}

void BeamModuleAssembler::emit_get_hd(ArgVal Src, ArgVal Hd, Instruction *Inst) {
  mov(TMP1, Src);
  x86::Mem lst = emit_list_val(TMP1);
  a.mov(TMP2, emit_car(lst)); // get car
  mov(Hd,TMP2);
}

void BeamModuleAssembler::emit_get_tl(ArgVal Src, ArgVal Tl, Instruction *Inst) {
  mov(TMP1, Src);
  x86::Mem lst = emit_list_val(TMP1);
  a.mov(TMP2, emit_cdr(lst)); // get cdr
  mov(Tl,TMP2);
}

void BeamModuleAssembler::emit_i_get(ArgVal Src, ArgVal Dst, Instruction *Inst) {
  a.mov(ARG1, c_p);
  mov(ARG2, Src);
  call((uint64_t)erts_pd_hash_get);
  mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_get_hash(ArgVal Src, ArgVal Hash, ArgVal Dst, Instruction *Inst) {
  a.mov(ARG1, c_p);
  mov(ARG2, Hash);
  mov(ARG3, Src);
  call((uint64_t)erts_pd_hash_get_with_hx);
  mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_get_tuple_element(ArgVal Src, ArgVal Element, ArgVal Dst, Instruction *Inst) {
  mov(TMP1, Src);
  a.mov(TMP1, emit_boxed_val(TMP1, Element.getValue()));
  mov(Dst, TMP1);
}

void BeamModuleAssembler::emit_init(ArgVal Y, Instruction *Inst) {
  mov(Y, NIL);
}

void BeamModuleAssembler::emit_i_trim(ArgVal Words, Instruction *Inst) {
  dealloc(Words * sizeof(Eterm));
  mov(CP, NIL);
}

void BeamModuleAssembler::emit_move(ArgVal Src, ArgVal Dst,  Instruction *Inst) {
  mov(Dst, Src);
}

void BeamModuleAssembler::emit_swap(ArgVal R1, ArgVal R2, Instruction *Inst) {
  mov(TMP1, R1);
  mov(TMP2, R2);
  mov(R2, TMP1);
  mov(R1, TMP2);
}

void BeamModuleAssembler::emit_node(ArgVal Dst, Instruction *Inst) {
  a.mov(TMP1, imm(&erts_this_node));
  a.mov(TMP1, x86::qword_ptr(TMP1));
  a.mov(TMP1, x86::qword_ptr(TMP1, offsetof(ErlNode, sysname)));
  mov(Dst, TMP1);
}

void BeamModuleAssembler::emit_put_list(ArgVal Hd, ArgVal Tl, ArgVal Dst, Instruction *Inst) {
  mov(x86::qword_ptr(HTOP,0), Hd);
  mov(x86::qword_ptr(HTOP,1 * sizeof(Eterm)), Tl);
  lea(Dst, x86::qword_ptr(HTOP,TAG_PRIMARY_LIST));
  a.lea(HTOP, x86::qword_ptr(HTOP,2 * sizeof(Eterm)));
}

void BeamModuleAssembler::emit_update_list(ArgVal Hd, ArgVal Dst, Instruction *Inst) {
  emit_put_list(Hd, Dst, Dst);
}

void BeamModuleAssembler::emit_put_tuple2(ArgVal Dst, ArgVal Arity, Instruction *Inst) {
  comment("Move arity word");
  a.mov(x86::qword_ptr(HTOP, 0), make_arityval(Arity.getValue()));
  comment("Move tuple data");
  for (unsigned i = 2; i < Inst->args.size(); i++) {
    mov(x86::qword_ptr(HTOP, (i - 1) * sizeof(Eterm)), Inst->args[i]);
  }
  comment("Create boxed ptr");
  a.lea(TMP1, x86::qword_ptr(HTOP, TAG_PRIMARY_BOXED));
  mov(Dst, TMP1);
  a.lea(HTOP, x86::qword_ptr(HTOP, (Arity.getValue() + 1) * sizeof(Eterm)));
}

void BeamModuleAssembler::emit_self(ArgVal Dst, Instruction *Inst) {
  a.mov(TMP1, x86::qword_ptr(c_p, offsetof(Process, common.id)));
  mov(Dst, TMP1);
}

void BeamModuleAssembler::emit_set_tuple_element(ArgVal Element, ArgVal Tuple, ArgVal Offset, Instruction *Inst) {
  mov(TMP2, Tuple);
  mov(emit_boxed_val(TMP2, Offset.getValue()), Element); // May use TMP1
}

void BeamModuleAssembler::emit_is_list(Label Fail, x86::Gp Src) {
  a.and_(Src, _TAG_PRIMARY_MASK-TAG_PRIMARY_LIST);
  a.cmp(Src, 0);
  a.jne(Fail);
}

void BeamModuleAssembler::emit_is_nonempty_list(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  mov(TMP1, Src);
  emit_is_list(labels[Fail.getValue()], TMP1);
}

void BeamModuleAssembler::emit_jump(ArgVal Fail, Instruction *Inst) {
  a.jmp(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_atom(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  mov(TMP1, Src);
  a.and_(TMP1, _TAG_IMMED2_MASK);
  a.cmp(TMP1, _TAG_IMMED2_ATOM);
  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_boolean(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  Label next = a.newLabel();

  /* Since am_true and am_false differ by a single bit, we can simplify the
   * check by clearing said bit and comparing against the lesser one. */
  ERTS_CT_ASSERT(am_false == make_atom(0));
  ERTS_CT_ASSERT(am_true == make_atom(1));

  mov(TMP1, Src);
  a.and_(TMP1, ~(am_true & ~_TAG_IMMED1_MASK));
  a.cmp(TMP1, am_false);
  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_boxed(Label Fail, x86::Gp Src) {
  a.test(Src,_TAG_PRIMARY_MASK-TAG_PRIMARY_BOXED);
  a.jne(Fail);
}

x86::Mem BeamModuleAssembler::emit_boxed_val(x86::Gp Src, uint64_t bytes) {
  ASSERT(bytes % sizeof(Eterm) == 0);
  return x86::qword_ptr(Src, -TAG_PRIMARY_BOXED + bytes);
}

void BeamModuleAssembler::emit_is_binary(Label Fail, x86::Gp Src, Label next, Label subbin) {
  ASSERT(Src != TMP2);
  emit_is_boxed(Fail, Src);
  a.mov(TMP2, emit_boxed_val(Src));

  // TODO: These checks can be optimized, check gcc gen for details
  a.and_(TMP2, _TAG_HEADER_MASK);
  a.cmp(TMP2, _TAG_HEADER_REFC_BIN);
  a.je(next);
  a.cmp(TMP2, _TAG_HEADER_HEAP_BIN);
  a.je(next);
  a.cmp(TMP2, _TAG_HEADER_SUB_BIN);
  a.je(subbin);
  a.jmp(Fail);
}

void BeamModuleAssembler::emit_is_binary(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  Label next = a.newLabel(), subbin = a.newLabel();

  mov(TMP1, Src);
  emit_is_binary(labels[Fail.getValue()], TMP1, next, subbin);

  a.bind(subbin);
  a.cmp(x86::byte_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlSubBin, bitsize)), 0);
  a.jne(labels[Fail.getValue()]);

  a.bind(next);
}

void BeamModuleAssembler::emit_is_bitstring(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  Label next = a.newLabel();
  mov(TMP1, Src);
  emit_is_binary(labels[Fail.getValue()], TMP1, next, next);
  a.bind(next);
}

void BeamModuleAssembler::emit_is_float(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  mov(TMP1, Src);
  emit_is_boxed(labels[Fail.getValue()], TMP1);
    
  a.cmp(emit_boxed_val(TMP1), HEADER_FLONUM);
  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_function(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  Label next = a.newLabel();
  mov(TMP1, Src);
  emit_is_boxed(labels[Fail.getValue()], TMP1);
  a.mov(TMP2, emit_boxed_val(TMP1));
  a.cmp(TMP2, HEADER_FUN);
  a.je(next);
  a.cmp(TMP2, HEADER_EXPORT);
  a.jne(labels[Fail.getValue()]);

  a.bind(next);
}

void BeamModuleAssembler::emit_cold_is_function2(ArgVal Fail, ArgVal Src, ArgVal Arity, Instruction *Inst) {
  a.mov(ARG1, c_p);
  mov(ARG2, Src);
  mov(ARG3, Arity);
  call((uint64_t)erl_is_function);
  a.cmp(RET, am_true);
  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_hot_is_function2(ArgVal Fail, ArgVal Src, ArgVal Arity, Instruction *Inst) {
  Label next = a.newLabel(), fun = a.newLabel();
  mov(TMP1, Src);
  emit_is_boxed(labels[Fail.getValue()], TMP1);
  a.mov(TMP2, emit_boxed_val(TMP1));
  a.cmp(TMP2, HEADER_FUN);
  a.je(fun);
  a.cmp(TMP2, HEADER_EXPORT);
  a.jne(labels[Fail.getValue()]);

  comment("Check arity of export fun");
  a.mov(TMP2, emit_boxed_val(TMP1, sizeof(Eterm)));
  a.cmp(x86::qword_ptr(TMP2, offsetof(Export, info.mfa.arity)), Arity.getValue());
  a.jne(labels[Fail.getValue()]);
  a.jmp(next);

  comment("Check arity of fun");
  a.bind(fun);
  a.cmp(emit_boxed_val(TMP1, offsetof(ErlFunThing, arity)), Arity.getValue());
  a.jne(labels[Fail.getValue()]);

  a.bind(next);
}

void BeamModuleAssembler::emit_is_integer(Label Fail, Label next, Label BigFail, x86::Gp Src) {
  ASSERT(Src != TMP2);
  a.mov(TMP2, Src);
  a.and_(TMP2, _TAG_IMMED1_MASK);
  a.cmp(TMP2, _TAG_IMMED1_SMALL);
  a.je(next);

  // Reuse TMP2 as the important bits are still available
  emit_is_boxed(Fail, TMP2);
    
  a.mov(Src, emit_boxed_val(Src));
  // Important that we leave the boxed word in Src, See emit_is_number for details
  a.mov(TMP3, Src);
  a.and_(TMP3, _TAG_HEADER_MASK-_BIG_SIGN_BIT);
  a.cmp(TMP3, _TAG_HEADER_POS_BIG);
  a.jne(BigFail);
  a.jmp(next);
}

void BeamModuleAssembler::emit_is_integer(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  Label next = a.newLabel();
  mov(TMP1, Src);
  emit_is_integer(labels[Fail.getValue()], next, labels[Fail.getValue()], TMP1);
  a.bind(next);
}

void BeamModuleAssembler::emit_is_list(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  Label next = a.newLabel();
  mov(TMP1, Src);
  a.mov(TMP2, NIL);
  a.cmp(TMP1, TMP2);
  a.je(next);
  emit_is_list(labels[Fail.getValue()], TMP1);
  a.bind(next);
}

void BeamModuleAssembler::emit_is_map(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  mov(TMP1, Src);
  emit_is_boxed(labels[Fail.getValue()], TMP1);
    
  a.mov(TMP1, emit_boxed_val(TMP1));
  a.and_(TMP1, _TAG_HEADER_MASK);
  a.cmp(TMP1, _TAG_HEADER_MAP);
  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_nil(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  a.cmp(getRef(Src), NIL);
  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_number(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  Label next = a.newLabel(), is_float = a.newLabel();
  mov(TMP1, Src);
    
  emit_is_integer(labels[Fail.getValue()], next, is_float, TMP1);

  a.bind(is_float);
  // emit_is_integer leaves the boxed header in the TMP1 so we can just do this test
  a.cmp(TMP1, HEADER_FLONUM);
  a.jne(labels[Fail.getValue()]);

  a.bind(next);
}

void BeamModuleAssembler::emit_is_pid(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  Label next = a.newLabel();
  mov(TMP1, Src);
  a.mov(TMP2, TMP1);
  a.and_(TMP2, _TAG_IMMED1_MASK);
  a.cmp(TMP2, _TAG_IMMED1_PID);
  a.je(next);

  // Reuse TMP2 as the important bits are still available
  emit_is_boxed(labels[Fail.getValue()], TMP2);
  a.mov(TMP1, emit_boxed_val(TMP1));
  a.and_(TMP1, _TAG_HEADER_MASK);
  a.cmp(TMP1, _TAG_HEADER_EXTERNAL_PID);
  a.jne(labels[Fail.getValue()]);
  a.bind(next);
}

void BeamModuleAssembler::emit_is_port(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  Label next = a.newLabel();
  mov(TMP1, Src);
  a.mov(TMP2, TMP1);
  a.and_(TMP2, _TAG_IMMED1_MASK);
  a.cmp(TMP2, _TAG_IMMED1_PORT);
  a.je(next);

  // Reuse TMP2 as the important bits are still available
  emit_is_boxed(labels[Fail.getValue()], TMP2);
  a.mov(TMP1, emit_boxed_val(TMP1));
  a.and_(TMP1, _TAG_HEADER_MASK);
  a.cmp(TMP1, _TAG_HEADER_EXTERNAL_PORT);
  a.jne(labels[Fail.getValue()]);
  a.bind(next);
}

void BeamModuleAssembler::emit_is_reference(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  Label next = a.newLabel();
  mov(TMP1, Src);

  emit_is_boxed(labels[Fail.getValue()], TMP1);
  a.mov(TMP1, emit_boxed_val(TMP1));
  a.cmp(TMP1, ERTS_REF_THING_HEADER);
  a.je(next);
  a.and_(TMP1, _TAG_HEADER_MASK);
  a.cmp(TMP1, _TAG_HEADER_EXTERNAL_REF);
  a.jne(labels[Fail.getValue()]);

  a.bind(next);
}

void BeamModuleAssembler::emit_is_tagged_tuple(ArgVal Fail, ArgVal Src, ArgVal Arity, ArgVal Tag, Instruction *Inst) {

  mov(TMP1, Src);
  emit_is_boxed(labels[Fail.getValue()], TMP1);
  a.cmp(emit_boxed_val(TMP1), Arity.getValue());
  a.jne(labels[Fail.getValue()]);
  a.cmp(emit_boxed_val(TMP1, sizeof(Eterm)), Tag.getValue());
  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_tagged_tuple_ff(ArgVal NotTuple, ArgVal NotRecord, ArgVal Src, ArgVal Arity, ArgVal Tag, Instruction *Inst) {

  mov(TMP1, Src);
  emit_is_boxed(labels[NotTuple.getValue()], TMP1);
  a.mov(TMP2, emit_boxed_val(TMP1));
  a.mov(TMP3, TMP2);
  a.and_(TMP2, _TAG_HEADER_MASK);
  a.cmp(TMP2, _TAG_HEADER_ARITYVAL);
  a.jne(labels[NotTuple.getValue()]);
  a.cmp(TMP3, Arity.getValue());
  a.jne(labels[NotRecord.getValue()]);
  a.cmp(emit_boxed_val(TMP1, sizeof(Eterm)), Tag.getValue());
  a.jne(labels[NotRecord.getValue()]);
}

void BeamModuleAssembler::emit_is_tuple(ArgVal Fail, ArgVal Src, Instruction *Inst) {
  mov(TMP1, Src);
  emit_is_boxed(labels[Fail.getValue()], TMP1);
  a.mov(TMP1, emit_boxed_val(TMP1));
  a.and_(TMP1, _TAG_HEADER_MASK);
  a.cmp(TMP1, _TAG_HEADER_ARITYVAL);
  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_tuple_of_arity(ArgVal Fail, ArgVal Src, ArgVal Arity, Instruction *Inst) {
  mov(TMP1, Src);

  emit_is_boxed(labels[Fail.getValue()], TMP1);

  a.mov(TMP2, emit_boxed_val(TMP1));
  a.cmp(TMP2, Arity.getValue());
  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_test_arity(ArgVal Fail, ArgVal Src, ArgVal Arity, Instruction *Inst) {
  mov(TMP1, Src);
  a.cmp(emit_boxed_val(TMP1), Arity.getValue());
  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_i_is_eq_exact_immed(ArgVal Fail, ArgVal X, ArgVal Y, Instruction *Inst) {
  cmp(getRef(X),Y.getValue());
  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_i_is_ne_exact_immed(ArgVal Fail, ArgVal X, ArgVal Y, Instruction *Inst) {
  cmp(getRef(X),Y.getValue());
  a.je(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_eq_exact(ArgVal Fail, ArgVal X, ArgVal Y, Instruction *Inst) {
  Label next = a.newLabel();

  mov(ARG2, Y); /* May clobber ARG1 */
  mov(ARG1, X);
  a.cmp(ARG1, ARG2);
  a.je(next);

  /* Fancy way of checking if both are immed */
  a.mov(TMP3, ARG1);
  a.and_(TMP3, _TAG_PRIMARY_MASK);
  a.and_(TMP3, ARG2);
  a.cmp(TMP3, TAG_PRIMARY_IMMED1);

  a.je(labels[Fail.getValue()]);
  call((uint64_t)eq);
  a.test(RET, RET);
  a.jz(labels[Fail.getValue()]);

  a.bind(next);
}

void BeamModuleAssembler::emit_i_is_eq_exact_literal(ArgVal Fail, ArgVal Src, ArgVal Literal, Instruction *Inst) {
  mov(ARG2, Literal); /* May clobber ARG1 */
  mov(ARG1, Src);

  a.mov(TMP3, ARG1);
  a.and_(TMP3, _TAG_IMMED1_MASK);
  a.cmp(TMP3, TAG_PRIMARY_IMMED1);
  a.je(labels[Fail.getValue()]);

  call((uint64_t)eq);
  a.test(RET, RET);
  a.jz(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_ne_exact(ArgVal Fail, ArgVal X, ArgVal Y, Instruction *Inst) {
  Label next = a.newLabel();

  mov(ARG2, Y); /* May clobber ARG1 */
  mov(ARG1, X);

  a.cmp(ARG1, ARG2);
  a.je(labels[Fail.getValue()]);

  /* Fancy way of checking if both are immed */
  a.mov(TMP3, ARG1);
  a.and_(TMP3, _TAG_PRIMARY_MASK);
  a.and_(TMP3, ARG2);
  a.cmp(TMP3, TAG_PRIMARY_IMMED1);
  a.je(next);

  call((uint64_t)eq);
  a.test(RET, RET);
  a.jnz(labels[Fail.getValue()]);

  a.bind(next);
}

void BeamModuleAssembler::emit_i_is_ne_exact_literal(ArgVal Fail, ArgVal Src, ArgVal Literal, Instruction *Inst) {
  Label next = a.newLabel();

  mov(ARG2, Literal); /* May clobber ARG1 */
  mov(ARG1, Src);

  a.mov(TMP3, ARG1);
  a.and_(TMP3, _TAG_IMMED1_MASK);
  a.cmp(TMP3, TAG_PRIMARY_IMMED1);
  a.je(next);

  call((uint64_t)eq);
  a.test(RET, RET);
  a.jnz(labels[Fail.getValue()]);

  a.bind(next);
}

void BeamModuleAssembler::emit_cmp_spec(x86::Inst::Id jmpOp, Label Fail, Label next, Operand y, Operand x, unsigned EqOnly) {
  Label generic = a.newLabel(),
    small_check = a.newLabel(),
    small_cmp = a.newLabel(),
    float_check = a.newLabel(),
    atom_cmp = a.newLabel(),
    float_cmp = a.newLabel();

  /* TODO: We should also allow X and Y to be imm values */
  ASSERT(y == ARG1 && x == ARG2);

  x86::Gp X = x.as<x86::Gp>();
  x86::Gp Y = y.as<x86::Gp>();

  /* Place the small test first as lt is most likely used on small integers */
  comment("is_both_small(X, Y)");
  a.bind(small_check);
  a.mov(TMP3,X);
  a.and_(TMP3,Y);
  a.and_(TMP3,_TAG_IMMED1_MASK);
  a.cmp(TMP3,_TAG_IMMED1_SMALL);
  a.je(small_cmp);

  comment("is_atom(X) && is_atom(Y)");
  a.mov(TMP3,X);
  a.and_(TMP3,_TAG_IMMED2_MASK);
  a.cmp(TMP3,_TAG_IMMED2_ATOM);
  a.jne(float_check);
  a.mov(TMP3,Y);
  a.and_(TMP3,_TAG_IMMED2_MASK);
  a.cmp(TMP3,_TAG_IMMED2_ATOM);
  a.je(atom_cmp);

  comment("is_float(X) && is_float(Y)");
  a.bind(float_check);
  emit_is_boxed(generic, X);
  a.mov(TMP3, emit_boxed_val(X));
  a.cmp(TMP3, HEADER_FLONUM);
  a.jne(generic);
  emit_is_boxed(generic, Y);
  a.mov(TMP3, emit_boxed_val(Y));
  a.cmp(TMP3, HEADER_FLONUM);
  a.je(float_cmp);

  comment("erts_cmp_compound(X,Y,0,0)");
  a.bind(generic);
  a.xor_(ARG3,ARG3);
  a.mov(ARG4,EqOnly);
  call((uint64_t)erts_cmp_compound);
  a.cmp(RET,0);
  a.emit(jmpOp, Fail);
  a.jmp(next);

  a.bind(atom_cmp);
  a.jmp(generic);

  a.bind(small_cmp);
  comment("signed_val(X) $Op signed_val(Y)");
  a.cmp(ARG1, ARG2);
  a.emit(jmpOp, Fail);
  a.jmp(next);

  a.bind(float_cmp);
  a.jmp(generic);

}

void BeamModuleAssembler::emit_is_eq(ArgVal Fail, ArgVal A, ArgVal B, Instruction *Inst) {
  Label next = a.newLabel();
  mov(ARG2, B); /* May clobber ARG1 */
  mov(ARG1, A);
  comment("X == Y -> next");
  a.cmp(ARG1, ARG2);
  a.je(next);
  emit_cmp_spec(x86::Inst::kIdJne, labels[Fail.getValue()], next, ARG1, ARG2, 1);
  a.bind(next);
}

void BeamModuleAssembler::emit_is_ne(ArgVal Fail, ArgVal A, ArgVal B, Instruction *Inst) {
  Label next = a.newLabel();
  mov(ARG2, B); /* May clobber ARG1 */
  mov(ARG1, A);
  comment("X == Y -> fail");
  a.cmp(ARG1, ARG2);
  a.je(labels[Fail.getValue()]);
  emit_cmp_spec(x86::Inst::kIdJe, labels[Fail.getValue()], next, ARG1, ARG2, 1);
  a.bind(next);
}

void BeamModuleAssembler::emit_is_lt(ArgVal Fail, ArgVal A, ArgVal B, Instruction *Inst) {
  Label next = a.newLabel();
  mov(ARG2, B); /* May clobber ARG1 */
  mov(ARG1, A);
  comment("X == Y -> fail");
  a.cmp(ARG1, ARG2);
  a.je(labels[Fail.getValue()]);
  emit_cmp_spec(x86::Inst::kIdJge, labels[Fail.getValue()], next, ARG1, ARG2, 0);
  a.bind(next);
}

void BeamModuleAssembler::emit_is_ge(ArgVal Fail, ArgVal A, ArgVal B, Instruction *Inst) {
  Label next = a.newLabel();
  mov(ARG2, B); /* May clobber ARG1 */
  mov(ARG1, A);
  comment("X == Y -> next");
  a.cmp(ARG1, ARG2);
  a.je(next);
  emit_cmp_spec(x86::Inst::kIdJl, labels[Fail.getValue()], next, ARG1, ARG2, 0);
  a.bind(next);
}

void BeamModuleAssembler::emit_badmatch(ArgVal Src, Instruction *Inst) {
  Label entry = a.newLabel();
  a.bind(entry);
  mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), Src);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), BADMATCH);
  emit_handle_error(entry);
}

void BeamModuleAssembler::emit_case_end(ArgVal Src, Instruction *Inst) {
  Label entry = a.newLabel();
  a.bind(entry);
  mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), Src);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), EXC_CASE_CLAUSE);
  emit_handle_error(entry);
}

void BeamModuleAssembler::emit_system_limit(ArgVal Fail, Instruction *Inst) {
  Label entry = a.newLabel();
  a.bind(entry);
  if (Fail.getValue() == 0) {
    a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), SYSTEM_LIMIT);
    emit_handle_error(entry);
  } else {
    a.jmp(labels[Fail.getValue()]);
  }
}

void BeamModuleAssembler::emit_if_end(Instruction *Inst) {
  Label entry = a.newLabel();
  a.bind(entry);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), EXC_IF_CLAUSE);
  emit_handle_error(entry);
}

void BeamModuleAssembler::emit_catch(ArgVal Y, ArgVal Fail, Instruction *Inst) {
  a.inc(x86::qword_ptr(c_p, offsetof(Process, catches)));

  Label patch_addr = a.newLabel();

  a.bind(patch_addr);
  a.mov(TMP1, imm(LLONG_MAX));
  mov(Y, TMP1);

  /* Offset of 0x2 = movabs */
  catches.push_back({{patch_addr, 0x2, 0}, labels[Fail.getValue()]});
}

void BeamModuleAssembler::emit_catch_end(ArgVal Y, Instruction *Inst) {
  Label next = a.newLabel(), not_throw = a.newLabel(), not_error = a.newLabel(),
    build_exit_tuple = a.newLabel();
  emit_try_end(Y);

  // TODO: Should generate a global snippet for this shenanigans like gc
  a.cmp(getRef(x0), THE_NON_VALUE);
  a.jne(next);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), NIL);
  a.cmp(getRef(x1), am_throw);
  a.jne(not_throw);
  mov(x0, x2);
  a.jmp(next);

  a.bind(not_throw);
  a.cmp(getRef(x1), am_error);
  a.jne(not_error);
  emit_swapout();
  a.mov(ARG1, c_p);
  mov(ARG2, x2);
  mov(ARG3, x3);
  call((uint64_t)add_stacktrace);
  emit_swapin();
  mov(x2, RET);

  a.bind(not_error);

  a.mov(TMP1, E);
  a.sub(TMP1, HTOP);
  a.cmp(TMP1, 3 * sizeof(Eterm));
  a.ja(build_exit_tuple);

  mov(x0, x2);
  a.mov(ARG2, 3);
  a.mov(ARG4, 1);
  call((uint64_t)ga->get_garbage_collect());
  mov(x2, x0);

  a.bind(build_exit_tuple);
  Instruction i = {op_put_tuple2_xI, {x0, ArgVal(ArgVal::u, 2), ArgVal(ArgVal::i, am_EXIT), x2}, Inst->I};
  emit_put_tuple2(i.args[0], i.args[1], &i);

  a.bind(next);
}

void BeamModuleAssembler::emit_try_end(ArgVal Y, Instruction *Inst) {
  a.dec(x86::qword_ptr(c_p, offsetof(Process, catches)));
  emit_init(Y);
}

void BeamModuleAssembler::emit_try_case(ArgVal Y, Instruction *Inst) {
  emit_try_end(Y);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), NIL);
  mov(x0, x1);
  mov(x1, x2);
  mov(x2, x3);
}

void BeamModuleAssembler::emit_try_case_end(ArgVal Src, Instruction *Inst) {
  Label entry = a.newLabel();
  a.bind(entry);
  mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), Src);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), EXC_TRY_CLAUSE);
  emit_handle_error(entry);
}

void BeamModuleAssembler::emit_i_raise(Instruction *Inst) {
  Label entry = a.newLabel(), next = a.newLabel(), primary_exception = a.newLabel();
  mov(ARG1, x2);
  mov(TMP2, x1);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), TMP2);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, ftrace)), ARG1);
  call((uint64_t)get_trace_from_exc);
  a.cmp(RET, 0);
  a.jne(primary_exception);
  a.mov(TMP1, EXC_ERROR);
  a.jmp(next);

  a.bind(primary_exception);
  a.mov(TMP1, x86::qword_ptr(c_p, offsetof(Process, freason)));
  a.and_(TMP1, (EXF_PRIMARY | EXC_CLASSBITS));

  a.bind(next);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), TMP1);
  emit_handle_error(entry);
}

void BeamModuleAssembler::emit_build_stacktrace(Instruction *Inst) {
  emit_swapout();

  a.mov(ARG1, c_p);
  mov(ARG2, x0);
  call((uint64_t)build_stacktrace);
  emit_swapin();

  mov(x0, RET);
}

static bool raw_raise(Eterm stacktrace, Eterm exc_class, Eterm value, Process *c_p) {
  Eterm* freason_ptr;

  /*
   * Note that the i_raise instruction will override c_p->freason
   * with the freason field stored inside the StackTrace struct in
   * ftrace. Therefore, we must take care to store the class both
   * inside the StackTrace struct and in c_p->freason (important if
   * the class is different from the class of the original
   * exception).
   */
  freason_ptr = get_freason_ptr_from_exc(stacktrace);

  if (exc_class == am_error) {
    *freason_ptr = c_p->freason = EXC_ERROR & ~EXF_SAVETRACE;
    c_p->fvalue = value;
    c_p->ftrace = stacktrace;
    return false;
  } else if (exc_class == am_exit) {
    *freason_ptr = c_p->freason = EXC_EXIT & ~EXF_SAVETRACE;
    c_p->fvalue = value;
    c_p->ftrace = stacktrace;
    return false;
  } else if (exc_class == am_throw) {
    *freason_ptr = c_p->freason = EXC_THROWN & ~EXF_SAVETRACE;
    c_p->fvalue = value;
    c_p->ftrace = stacktrace;
    return false;
  } else {
    return true;
  }
}

void BeamModuleAssembler::emit_raw_raise(Instruction *Inst) {
  Label entry = a.newLabel(),next = a.newLabel();
  a.bind(entry);
  mov(ARG1, x2);
  mov(ARG2, x0);
  mov(ARG3, x1);
  a.mov(ARG4, c_p);
  call((uint64_t)raw_raise);
  a.cmp(RET, 0);
  a.jne(next);
  emit_handle_error(entry);
  a.bind(next);
  mov(x0, am_badarg);
}

void BeamModuleAssembler::emit_i_yield(Instruction *Inst) {
  // Ignore for now... not important
  mov(ArgVal(ArgVal::x, 0), am_true);
};
