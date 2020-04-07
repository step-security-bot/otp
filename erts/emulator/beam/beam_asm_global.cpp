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

#include "beam_asm.hpp"

using namespace asmjit;

extern "C" {
  BeamInstr beam_exit[1];
  void call_error_handler(void);
  void handle_error(void);
}

BeamGlobalAssembler::BeamGlobalAssembler(JitRuntime *rt) : BeamAssembler(rt) {
    Error err;

#define EMIT_FUNC(NAME)                                                 \
    /* setLogger(#NAME ".asm"); */                                      \
      emit_##NAME();                                                    \
      err = rt->add(&NAME##_code,&code);                                \
      ERTS_ASSERT(!err && "Failed to create " #NAME " function");       \
      reset();

    BEAM_GLOBAL_FUNCS(EMIT_FUNC)
}

void BeamGlobalAssembler::emit_garbage_collect() {
    /* This is the common stub used for calling garbage_collect. This functiuon is called
        with a custom calling convention where ARG2 and ARG4 are set, but the swapout and
        all other arguments have to be moved.
    */
    emit_function_preamble();
    // TODO: Should set c_p->i to I here....
    emit_swapout();
    a.mov(ARG1, c_p);
    a.mov(ARG3, x_reg);
    a.mov(ARG5, FCALLS);
    call((uint64_t)erts_garbage_collect_nobump);
    a.sub(FCALLS, RET);
    emit_swapin();
    emit_function_postamble();
}

void BeamGlobalAssembler::emit_gc_after_bif() {
    /* This is a common stub called after a bif call to see if a gc is needed.
       RET = bif return
       FCALLS = c_p->mbuf ptr value before bif
       ARG5 = arity
     */
    Label after_gc = a.newLabel(), do_gc = a.newLabel();
    emit_function_preamble();
    comment("check if gc is needed");
    a.mov(TMP1, x86::qword_ptr(c_p, offsetof(Process,stop)));
    a.sub(TMP1, x86::qword_ptr(c_p, offsetof(Process,htop)));
    a.sar(TMP1, 3);
    a.cmp(TMP1, x86::qword_ptr(c_p, offsetof(Process,mbuf_sz)));
    a.jb(do_gc);
    // This asm code is taken from what GCC does
    a.mov(TMP1, x86::qword_ptr(c_p, offsetof(Process,bin_vheap_sz)));
    a.cmp(x86::qword_ptr(c_p, offsetof(Process,off_heap.overhead)), TMP1);
    a.mov(x86::edx, x86::dword_ptr(c_p, offsetof(Process,flags)));
    a.seta(x86::cl);
    a.shr(x86::edx, 10);
    a.and_(x86::edx, 1);
    a.or_(x86::cl, x86::dl);
    a.jne(do_gc);
    emit_function_postamble();

    comment("do a gc_after_bif_call");
    a.bind(do_gc);
    a.mov(ARG1, c_p);
    a.mov(ARG2, FCALLS);
    a.mov(ARG3, RET);
    a.mov(ARG4, x_reg);
    call((uint64_t)erts_gc_after_bif_call_lhf);

    emit_function_postamble();
}

#define STACK_SLOTS 12

void BeamGlobalAssembler::emit_call() {
    emit_function_preamble(STACK_SLOTS);

    // Push all callee save
    unsigned slot = STACK_SLOTS;
    a.mov(x86::qword_ptr(x86::rsp, --slot * 8), c_p);
    a.mov(x86::qword_ptr(x86::rsp, --slot * 8), x_reg);
    a.mov(x86::qword_ptr(x86::rsp, --slot * 8), HTOP);
    a.mov(x86::qword_ptr(x86::rsp, --slot * 8), E);
    a.mov(x86::qword_ptr(x86::rsp, --slot * 8), FCALLS);
    a.mov(x86::qword_ptr(x86::rsp, --slot * 8), f_reg);

    a.mov(x86::qword_ptr(x86::rsp, --slot * 8), ARG1); // ctx

    a.mov(x86::qword_ptr(x86::rsp, --slot * 8), ARG5);
    ASSERT(x86::qword_ptr(x86::rsp, slot * 8) == EBS);

    a.mov(x86::qword_ptr(x86::rsp, --slot * 8), ARG6); // neg_o_reds

    // We need three extra slots of guard bif calls and other things
    ASSERT(slot > 2);

    // Move the arguments to the correct registers, ignoring c_p, FCALLS and f_reg for now...
    // typedef void (*BeamAsmFunc)(BeamInstr **,Process *,Eterm *, Eterm **, Eterm **, Sint *, FloatDef *);
    a.mov(c_p, ARG2);
    a.mov(x_reg, ARG3);
    a.mov(FCALLS, x86::qword_ptr(ARG1, offsetof(BeamAsmContext, FCALLS)));
    a.mov(f_reg, ARG4);
    emit_swapin();

    // Check if we are just returning from a dirty nif/bif call and if so we
    // need to do a bit of cleaning up before continuing.
    a.mov(RET, x86::qword_ptr(c_p, offsetof(Process, i)));
    a.cmp(x86::qword_ptr(RET), op_call_nif_WWW);

    Label next = a.newLabel();
    a.jne(next);
    farjmp(get_dispatch_nif());
    a.bind(next);
    a.jmp(RET);
}

void BeamGlobalAssembler::emit_return() {
    // Update the process state
    emit_heavy_swapout();

    // Put the BeamAsmContext in TMP1
    a.mov(TMP1, x86::qword_ptr(x86::rsp, (STACK_SLOTS - 7) * 8));

    Label next = a.newLabel();
    // Set I to be the return address, the address is stored in TMP3
    //
    // FIXME: This must not be set when handle_error returns NULL. Handle this
    // in post_error_handling instead.
    a.cmp(TMP3, 0);
    a.je(next);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, i)), TMP3);
    a.bind(next);

    // Restore the rest of the emulator state
    a.mov(x86::qword_ptr(TMP1, offsetof(BeamAsmContext, FCALLS)), FCALLS);

    // Pop all callee save
    unsigned slot = STACK_SLOTS;
    a.mov(c_p,x86::qword_ptr(x86::rsp, --slot * 8));
    a.mov(x_reg, x86::qword_ptr(x86::rsp, --slot * 8));
    a.mov(HTOP, x86::qword_ptr(x86::rsp, --slot * 8));
    a.mov(E,x86::qword_ptr(x86::rsp, --slot * 8));
    a.mov(FCALLS,x86::qword_ptr(x86::rsp, --slot * 8));
    a.mov(f_reg,x86::qword_ptr(x86::rsp, --slot * 8));

    emit_function_postamble(STACK_SLOTS);
}

void BeamGlobalAssembler::emit_call_error_handler() {
  emit_heavy_swapout();
  a.mov(ARG1,c_p);
  // ARG2 is set in module assembler
  a.mov(ARG3,x_reg);
  a.mov(ARG4,imm(am_undefined_function));
  call((uint64_t)call_error_handler);
  emit_heavy_swapin();
  a.cmp(RET,0);
  a.je(get_handle_error());
  a.jmp(RET);
}

void BeamModuleAssembler::emit_call_error_handler(Instruction *I) {
  /* We're ALWAYS in an Export entry, just after an ErtsCodeMFA; prepare our
   * `I` for use in erts_code_to_codemfa. */
  a.lea(ARG2, x86::qword_ptr(x86::rip, -7));
  farjmp((uint64_t)ga->get_call_error_handler());
}

void BeamModuleAssembler::emit_handle_error(Label I, ErtsCodeMFA *mfa) {
    /* TODO: We can change this to only set ARG2 and ARG4 and then jump to global code... */
    emit_swapout();
    a.mov(ARG1, c_p);
    /* FIXME: MOV -> LEA */
    a.lea(ARG2, x86::qword_ptr(I));
    a.mov(ARG3, x_reg);
    a.mov(ARG4, imm(mfa));
    call((uint64_t)handle_error);
    farjmp(ga->get_post_error_handling());
}

void BeamModuleAssembler::emit_handle_error(Label I, ArgVal exp) {
    /* TODO: We can change this to only set ARG2 and ARG4 and then jump to global code... */
    emit_swapout();
    a.mov(ARG1, c_p);
    /* FIXME: MOV -> LEA */
    a.lea(ARG2, x86::qword_ptr(I));
    a.mov(ARG3, x_reg);
    make_move_patch(ARG4, imports[exp.getValue()].patches, offsetof(Export, info.mfa));
    call((uint64_t)handle_error);
    farjmp(ga->get_post_error_handling());
}

// this is an alias for handle_error
void BeamGlobalAssembler::emit_error_action_code() {
  emit_swapout();
  a.mov(ARG1, c_p);
  a.mov(ARG2, 0);
  a.mov(ARG3, x_reg);
  a.mov(ARG4, 0);
  call((uint64_t)handle_error);
  farjmp(this->get_post_error_handling());
}

void BeamGlobalAssembler::emit_post_error_handling() {
  Label dispatch = a.newLabel();
  a.mov(TMP3, RET);
  a.cmp(TMP3, 0);
  a.jne(dispatch);
  a.mov(RET, RET_do_schedule);
  farjmp(this->get_return());
  a.bind(dispatch);
  emit_swapin();
  a.jmp(RET);
}

void BeamGlobalAssembler::emit_i_func_info() {
  // I when the error happened is stored in TMP1.
  a.lea(TMP1, x86::qword_ptr(TMP1, offsetof(ErtsCodeInfo,mfa)));

  a.mov(x86::qword_ptr(c_p,offsetof(Process,freason)), EXC_FUNCTION_CLAUSE);
  a.mov(x86::qword_ptr(c_p,offsetof(Process,current)), TMP1);
  farjmp(this->get_handle_error());
}

void BeamGlobalAssembler::emit_dbg() {
//   a.push(RET);
//   a.push(ARG1);
//   a.push(ARG2);
//   a.push(ARG3);
//   a.push(ARG4);
//   a.push(ARG5);
//   a.push(ARG6);
//   emit_swapout();
//   a.mov(ARG1, x86::qword_ptr(x86::rsp, (7 + 3) * sizeof(void*)));
//   a.mov(ARG2, c_p);
//   a.mov(ARG3, x_reg);
//   /* We read the current code location from the stack and then align it */
//   a.mov(ARG4, x86::qword_ptr(x86::rsp, (7) * sizeof(void*)));
//   a.and_(ARG4, Imm(~3ull));
//   call((uint64_t)&BeamModuleAssembler::dbg);
//   a.pop(ARG6);
//   a.pop(ARG5);
//   a.pop(ARG4);
//   a.pop(ARG3);
//   a.pop(ARG2);
//   a.pop(ARG1);
//   a.pop(RET);
  a.ret();
}

void BeamModuleAssembler::emit_proc_lc_unrequire(void) {
#ifdef ERTS_ENABLE_LOCK_CHECK
    a.mov(ARG1, c_p);
    a.mov(ARG2, ERTS_PROC_LOCK_MAIN);
    a.mov(qTMP1_MEM, RET);
    call((uint64_t)erts_proc_lc_unrequire_lock);
    a.mov(RET, qTMP1_MEM);
#endif
}

void BeamModuleAssembler::emit_proc_lc_require(void) {
#ifdef ERTS_ENABLE_LOCK_CHECK
    a.mov(ARG1, c_p);
    a.mov(ARG2, ERTS_PROC_LOCK_MAIN);
    a.mov(qTMP1_MEM, RET);
    call((uint64_t)erts_proc_lc_require_lock);
    a.mov(RET, qTMP1_MEM);
#endif
}
