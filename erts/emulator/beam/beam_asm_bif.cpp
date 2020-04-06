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

extern "C" {
#include "erl_bif_table.h"
#include "bif.h"
  ErtsCodeMFA *ubif2mfa(void* uf);
  void handle_error(void);
}

template<typename T>
void BeamModuleAssembler::emit_yield_error_test(Label entry, T exp, bool only) {
  Label next = a.newLabel(), cont, error = a.newLabel();
  comment("check if error or yield");
  a.cmp(RET, THE_NON_VALUE);
  a.jne(next);
  a.cmp(x86::qword_ptr(c_p, offsetof(Process,freason)), TRAP);
  a.jne(error);
  comment("yield");
  if (!only) {
    cont = a.newLabel();
    a.lea(RET, x86::qword_ptr(cont));
    mov(CP, RET);
  }
  a.mov(TMP3, x86::qword_ptr(c_p, offsetof(Process, i)));
  a.mov(RET,RET_context_switch);
  farjmp(ga->get_return());
  a.bind(error);
  emit_handle_error(entry, exp);
  a.bind(next);
  mov(ArgVal(ArgVal::x,0), RET);
  if (only) {
    emit_return();
  } else {
    a.align(kAlignCode, 8);
    a.bind(cont);
  }
}

void BeamModuleAssembler::emit_call_light_bif_only(ArgVal Bif, ArgVal Exp, Instruction *I) {
  Label entry = a.newLabel();
  a.align(kAlignCode, 8);
  a.bind(entry);

  // TODO: Add fcalls check here

  emit_swapout();
  emit_proc_lc_unrequire();

  a.add(FCALLS, -1);
  a.mov(x86::qword_ptr(c_p, offsetof(Process,fcalls)), FCALLS);
  a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process,mbuf))); // Save the previous mbuf for GC call
  a.mov(ARG1, c_p);
  a.mov(ARG2, x_reg);
  a.lea(ARG3, x86::qword_ptr(entry));
  call(Bif);

  make_move_patch(ARG5, imports[Exp.getValue()].patches, offsetof(Export, info.mfa.arity));
  a.mov(ARG5, x86::qword_ptr(ARG5));

  a.mov(TMP1, ga->get_gc_after_bif());
  a.call(TMP1);

  emit_proc_lc_require();
  emit_swapin();

  a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process,fcalls)));
  emit_yield_error_test(entry, Exp, true);
}

void BeamModuleAssembler::emit_call_light_bif(ArgVal Bif, ArgVal Exp, Instruction *I) {
  Export *exp = (Export*)Exp.getValue();
  Label entry = a.newLabel();
  a.align(kAlignCode, 8);
  a.bind(entry);

  emit_swapout();
  emit_proc_lc_unrequire();

  a.add(FCALLS, -1);
  a.mov(x86::qword_ptr(c_p, offsetof(Process,fcalls)), FCALLS);
  a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process,mbuf))); // Save the previous mbuf for GC call
  a.mov(ARG1, c_p);
  a.mov(ARG2, x_reg);
  a.lea(ARG3, x86::qword_ptr(entry));
  call(Bif);

  make_move_patch(ARG5, imports[Exp.getValue()].patches, offsetof(Export, info.mfa.arity));
  a.mov(ARG5, x86::qword_ptr(ARG5));

  a.mov(TMP1, ga->get_gc_after_bif());
  a.call(TMP1);

  emit_proc_lc_require();
  emit_swapin();

  a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process,fcalls)));
  emit_yield_error_test(entry, Exp, false);
}

// WARNING: Note that args here HAVE to be given in reverse order as that is the way
// that the bifs expect them to be in....
void BeamModuleAssembler::emit_call_guard_bif(std::vector<ArgVal> args, ArgVal bif, ArgVal Dst, Label entry, Label next) {
  Label fail = a.newLabel();
  unsigned stackAdj = (args.size()+1) / 2 * sizeof(Eterm); // Allocate aligned stack
  a.sub(x86::rsp, stackAdj);
  // Store bif arguments on stack
  for (unsigned i = 0; i < args.size(); i++)
    mov(x86::qword_ptr(x86::rsp, i * sizeof(Eterm)), args[i]);

  a.mov(x86::qword_ptr(c_p, offsetof(Process, fcalls)), FCALLS);

  a.mov(ARG1, c_p);
  a.mov(ARG2, x86::rsp);
  a.mov(ARG3, x86::qword_ptr(entry));
  call(bif);

  a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process, fcalls)));

  // Adjust stack back
  a.add(x86::rsp, stackAdj);
  a.cmp(RET, THE_NON_VALUE);
  a.je(fail);
  mov(Dst, RET);
  a.jmp(next);
  a.bind(fail);

}

void BeamModuleAssembler::emit_i_bif1_body(ArgVal Src1, ArgVal Bif, ArgVal Dst, Instruction *I) {
  Label next = a.newLabel(), entry = a.newLabel();
  a.bind(entry);
  emit_call_guard_bif({Src1}, Bif, Dst, entry, next);
  emit_bif_arg_error({Src1}, entry, ubif2mfa((void*)Bif.getValue()));
  a.bind(next);
}

void BeamModuleAssembler::emit_i_bif2_body(ArgVal Src1, ArgVal Src2, ArgVal Bif, ArgVal Dst, Instruction *I) {
  Label next = a.newLabel(), entry = a.newLabel();
  a.bind(entry);
  emit_call_guard_bif({Src2, Src1}, Bif, Dst, entry, next);
  emit_bif_arg_error({Src2, Src1}, entry, ubif2mfa((void*)Bif.getValue()));
  a.bind(next);
}

void BeamModuleAssembler::emit_i_bif3_body(ArgVal Src1, ArgVal Src2, ArgVal Src3, ArgVal Bif, ArgVal Dst, Instruction *I) {
  Label next = a.newLabel(), entry = a.newLabel();
  a.bind(entry);;
  emit_call_guard_bif({Src3, Src2, Src1}, Bif, Dst, entry, next);
  emit_bif_arg_error({Src3, Src2, Src1}, entry, ubif2mfa((void*)Bif.getValue()));
  a.bind(next);
}

void BeamModuleAssembler::emit_i_bif1(ArgVal Src1, ArgVal Fail, ArgVal Bif, ArgVal Dst, Instruction *I) {
  Label next = a.newLabel(), entry = a.newLabel();
  a.bind(entry);
  emit_call_guard_bif({Src1}, Bif, Dst, entry, next);
  a.jmp(labels[Fail.getValue()]);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_bif2(ArgVal Src1, ArgVal Src2, ArgVal Fail, ArgVal Bif, ArgVal Dst, Instruction *I) {
  Label next = a.newLabel(), entry = a.newLabel();
  a.bind(entry);
  emit_call_guard_bif({Src2, Src1}, Bif, Dst, entry, next);
  a.jmp(labels[Fail.getValue()]);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_bif3(ArgVal Src1, ArgVal Src2, ArgVal Src3, ArgVal Fail, ArgVal Bif, ArgVal Dst, Instruction *I) {
  Label next = a.newLabel(), entry = a.newLabel();
  a.bind(entry);
  emit_call_guard_bif({Src3, Src2, Src1}, Bif, Dst, entry, next);
  a.jmp(labels[Fail.getValue()]);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_length_setup(ArgVal Live, ArgVal Src, Instruction *I) {
  ArgVal slot(ArgVal::TYPE::x, Live.getValue());

  mov(slot + 0, Src);
  mov(slot + 1, make_small(0));
  mov(slot + 2, Src);
}

void BeamModuleAssembler::emit_i_length(ArgVal Fail, ArgVal Live, ArgVal Dst, Instruction *I) {
  Label entry = a.newLabel(), next = a.newLabel(),
    trap = a.newLabel(), error = a.newLabel();

  a.align(kAlignCode, 8);
  a.bind(entry),
    a.mov(x86::qword_ptr(c_p, offsetof(Process, fcalls)), FCALLS);
  a.mov(ARG1, c_p);
  a.lea(ARG2, x86::qword_ptr(x_reg, Live.getValue()*sizeof(Eterm)));
  call((uint64_t)erts_trapping_length_1);
  a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process, fcalls)));
  a.cmp(RET, THE_NON_VALUE);
  a.je(trap);
  mov(Dst, RET);
  a.jmp(next);

  comment("test trap");
  a.bind(trap);
  a.mov(TMP1, x86::qword_ptr(c_p, offsetof(Process, freason)));
  a.cmp(TMP1, TRAP);
  a.jne(error);
  comment("do trap");
  a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), 0);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, arity)), Live.getValue() + 3);
  a.lea(TMP3, x86::qword_ptr(entry));
  a.mov(RET,RET_context_switch3);
  farjmp(ga->get_return());

  a.bind(error);
  emit_bif_arg_error({ArgVal(ArgVal::x, Live.getValue() + 2)}, entry, &bif_trap_export[BIF_length_1].info.mfa);

  a.bind(next);
}

void BeamModuleAssembler::emit_send(Instruction *I) {
  Label next = a.newLabel(), error = a.newLabel(), cont = a.newLabel(), entry = a.newLabel();
  a.bind(entry);

  emit_proc_lc_unrequire();

  a.dec(FCALLS);
  emit_heavy_swapout();
  a.mov(ARG1, c_p);
  mov(ARG2, ArgVal(ArgVal::x, 0));
  mov(ARG3, ArgVal(ArgVal::x, 1));
  call((uint64_t)erl_send);
  emit_proc_lc_require();
  emit_swapin();
  a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process, fcalls)));
  emit_yield_error_test(entry, nullptr, false);
}

#ifdef ERTS_ENABLE_LOCK_CHECK
#    define PROCESS_MAIN_CHK_LOCKS(P)                   \
  do {                                                  \
    if ((P))                                            \
      erts_proc_lc_chk_only_proc_main((P));             \
    ERTS_LC_ASSERT(!erts_thr_progress_is_blocking());   \
  } while (0)
#    define ERTS_REQ_PROC_MAIN_LOCK(P)				\
  do {                                                          \
    if ((P))                                                    \
      erts_proc_lc_require_lock((P), ERTS_PROC_LOCK_MAIN,       \
                                __FILE__, __LINE__);            \
  } while (0)
#    define ERTS_UNREQ_PROC_MAIN_LOCK(P)                        \
  do {                                                          \
    if ((P))                                                    \
      erts_proc_lc_unrequire_lock((P), ERTS_PROC_LOCK_MAIN);    \
  } while (0)
#else
#  define PROCESS_MAIN_CHK_LOCKS(P)
#  define ERTS_REQ_PROC_MAIN_LOCK(P)
#  define ERTS_UNREQ_PROC_MAIN_LOCK(P)
#endif

static Eterm
call_bif(Process *c_p, Eterm *reg, BeamInstr *I, ErtsBifFunc vbf) {
    ErlHeapFragment *live_hf_end;
    ErtsCodeMFA *codemfa;
    Eterm result;

    codemfa = erts_code_to_codemfa(I);

    /* In case we apply process_info/1,2 or load_nif/1 */
    c_p->current = codemfa;
    
    /* In case we apply check_process_code/2. */
    c_p->i = I;

    /* To allow garbage collection on ourselves
     * (check_process_code/2, put/2, etc). */
    c_p->arity = 0;

    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    {
        live_hf_end = c_p->mbuf;

        ERTS_CHK_MBUF_SZ(c_p);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p));
        result = vbf(c_p, reg, I);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
        ERTS_CHK_MBUF_SZ(c_p);

        ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
        ERTS_HOLE_CHECK(c_p);

    }
    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);

    if (ERTS_IS_GC_DESIRED(c_p)) {
        result = erts_gc_after_bif_call_lhf(c_p, live_hf_end, result, reg,
                                            codemfa->arity);
    }

    return result;
}

enum nif_ret {
  RET_NIF_next,
  RET_NIF_handle_error,
  RET_NIF_do_schedule = RET_do_schedule
};

typedef Eterm NifF(struct enif_environment_t*, int argc, Eterm argv[]);

static Eterm
call_nif(Process *c_p, BeamInstr *I, Eterm *reg, NifF *fp, struct erl_module_nif *NifMod) {
    Eterm nif_bif_result;
    Eterm bif_nif_arity;
    BifFunction vbf = (void*)fp;
    ErlHeapFragment *live_hf_end;
    ErtsCodeMFA *codemfa;

    codemfa = erts_code_to_codemfa(I);

    c_p->current = codemfa; /* current and vbf set to please handle_error */

    bif_nif_arity = codemfa->arity;
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);

    {
        typedef Eterm NifF(struct enif_environment_t*, int argc, Eterm argv[]);
        struct enif_environment_t env;
        ASSERT(c_p->scheduler_data);
        live_hf_end = c_p->mbuf;
        ERTS_CHK_MBUF_SZ(c_p);
        erts_pre_nif(&env, c_p, NifMod, NULL);

        ASSERT((c_p->scheduler_data)->current_nif == NULL);
        (c_p->scheduler_data)->current_nif = &env;

        nif_bif_result = (*fp)(&env, bif_nif_arity, reg);
        if (env.exception_thrown)
            nif_bif_result = THE_NON_VALUE;

        ASSERT((c_p->scheduler_data)->current_nif == &env);
        (c_p->scheduler_data)->current_nif = NULL;

        erts_post_nif(&env);
        ERTS_CHK_MBUF_SZ(c_p);

        PROCESS_MAIN_CHK_LOCKS(c_p);
        ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
        ERTS_MSACC_SET_STATE_CACHED_M_X(ERTS_MSACC_STATE_EMULATOR);
        ASSERT(!env.exiting);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    }
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
    ERTS_HOLE_CHECK(c_p);

    if (ERTS_IS_GC_DESIRED(c_p)) {
        nif_bif_result = erts_gc_after_bif_call_lhf(c_p, live_hf_end,
                                                    nif_bif_result,
                                                    reg, bif_nif_arity);
    }

    return nif_bif_result;
}

void BeamGlobalAssembler::emit_bif_nif_epilogue(void) {
  Label check_trap = a.newLabel(), error = a.newLabel();

  a.cmp(RET, THE_NON_VALUE);
  a.je(check_trap);
  comment("Do return and dispatch to it");
  a.mov(getRef(x0), RET);
  a.mov(RET,getRef(CP));
  a.mov(TMP1,NIL);
  a.mov(getRef(CP),TMP1);
  a.jmp(RET);

  a.bind(check_trap);
  a.cmp(x86::qword_ptr(c_p, offsetof(Process,freason)), TRAP);
  a.jne(error);
  comment("yield");
  a.mov(TMP3, x86::qword_ptr(c_p, offsetof(Process, i)));
  a.mov(RET,RET_context_switch);
  a.mov(TMP1, get_return());
  a.jmp(TMP1);
  a.bind(error);
  a.mov(ARG1, c_p);
  a.mov(ARG2, qTMP1_MEM);
  a.mov(ARG3, x_reg);
  a.lea(ARG4, x86::qword_ptr(ARG2, -24));
  call((uint64_t)handle_error);
  farjmp(get_post_error_handling());
}

void BeamGlobalAssembler::emit_call_bif(void)
{
  a.dec(FCALLS);

  emit_heavy_swapout();

  /* These arguments have already been provided:
   *
   *    ARG3 = I (rip)
   *    ARG4 = function to be called
   */

  /* Save current I for the epilogue. */
  a.mov(qTMP1_MEM, ARG3);

  a.mov(ARG1, c_p);
  a.mov(ARG2, x_reg);
  a.mov(RET, (uint64_t)call_bif);
  a.call(RET);

  emit_heavy_swapin();
  emit_bif_nif_epilogue();
}

void BeamModuleAssembler::emit_call_bif(ArgVal Func, Instruction *I)
{
  /* This must be the first instruction.
   *
   * -7 is a filthy hack to make it refer to the current instruction. */
  a.lea(ARG3, x86::qword_ptr(x86::rip, -7));
  mov(ARG4, Func);
  a.mov(RET,ga->get_call_bif());
  a.jmp(RET);
}

void BeamGlobalAssembler::emit_call_nif(void)
{
  Label check_trap = a.newLabel(), error = a.newLabel();

  a.dec(FCALLS);
  emit_heavy_swapout();

  a.mov(ARG1, c_p);
  a.mov(qTMP1_MEM, ARG2);
  // a.mov(ARG2, ARG2); THIS IS SUPPLIED AS ARGUMENT
  a.mov(ARG3, x_reg);
  a.mov(ARG4, x86::qword_ptr(ARG2, 8));
  a.mov(ARG5, x86::qword_ptr(ARG2, 16));
  a.mov(ARG6, x86::qword_ptr(ARG2, 24));
  call((uint64_t)call_nif);

  emit_heavy_swapin();
  emit_bif_nif_epilogue();
}

void BeamGlobalAssembler::emit_dispatch_nif(void) {
  a.mov(RET,get_call_nif());
  a.mov(ARG2, x86::qword_ptr(c_p, offsetof(Process,i)));
  a.jmp(RET);
}

void BeamModuleAssembler::emit_call_nif(ArgVal Func, ArgVal NifMod, ArgVal DirtyFunc, Instruction *I)
{
  uint64_t val;
  Label entry = a.newLabel();

  // The start of this function has to mimic the layout of ErtsNativeFunc...
  a.jmp(entry); // call_op
  a.align(kAlignCode, 8);
  val = Func.getValue();
  a.embed(&val,sizeof(val)); // dfunc
  val = NifMod.getValue();
  a.embed(&val,sizeof(val)); // m
  val = DirtyFunc.getValue();
  a.embed(&val,sizeof(val)); // func

  // The real code starts here
  a.bind(entry);
  a.mov(RET,ga->get_call_nif());
  a.lea(ARG2, x86::qword_ptr(currLabel));
  a.jmp(RET);
}

static enum nif_ret
load_nif(Process *c_p, BeamInstr *I, Eterm *reg) {
  if (erts_try_seize_code_write_permission(c_p)) {
    Eterm result;

    PROCESS_MAIN_CHK_LOCKS((c_p));
    ERTS_UNREQ_PROC_MAIN_LOCK((c_p));
    result = erts_load_nif(c_p, I, reg[0], reg[1]);
    erts_release_code_write_permission();
    ERTS_REQ_PROC_MAIN_LOCK(c_p);

    if (ERTS_LIKELY(is_value(result))) {
      reg[0] = result;
      return RET_NIF_next;
    } else {
      c_p->freason = BADARG;
      return RET_NIF_handle_error;
    }
  } else {
    /* Yield and try again */
    c_p->stop[0] = make_cp(I);
    c_p->current = NULL;
    c_p->arity = 2;
    return RET_NIF_do_schedule;
  }
}

void BeamModuleAssembler::emit_i_load_nif(Instruction *I) {
  Label next = a.newLabel(), schedule = a.newLabel();
  emit_swapout();
  a.mov(ARG1, c_p);
  a.lea(ARG2, x86::qword_ptr(currLabel));
  a.mov(ARG3, x_reg);
  call((uint64_t)load_nif);
  emit_swapin();
  a.cmp(RET, RET_NIF_do_schedule);
  a.je(schedule);
  a.cmp(RET, RET_NIF_next);
  a.je(next);
  static ErtsCodeMFA mfa = {am_erlang, am_load_nif, 2};
  emit_handle_error(currLabel, &mfa);
  a.bind(schedule);
  farjmp(ga->get_return());
  a.bind(next);
}


