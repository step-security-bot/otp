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
}

extern "C" BeamInstr *handle_error(void);

void BeamModuleAssembler::emit_bif_arg_error(std::vector<ArgVal> args, Label entry, ErtsCodeMFA *mfa) {
    comment("handle_error");
    for (unsigned i = 0; i < args.size(); i++)
        mov(ArgVal(ArgVal::x, i), args[i]);
    emit_handle_error(entry, mfa);
}

void BeamModuleAssembler::emit_is_both_small(Label fail, x86::Gp A, x86::Gp B) {
    ASSERT(TMP1 != A && TMP1 != B);

    comment("is_both_small(X, Y)");
    a.mov(TMP1, A);
    a.and_(TMP1, B);
    a.and_(TMP1,_TAG_IMMED1_MASK);
    a.cmp(TMP1,_TAG_IMMED1_SMALL);
    a.jne(fail);
}

void BeamModuleAssembler::emit_i_increment(ArgVal Src, ArgVal Val, ArgVal Dst, Instruction *Inst) {
    Label entry = a.newLabel(), mixed = a.newLabel(), next = a.newLabel();

    a.bind(entry);

    /* Place the values in ARG2 and ARG3 to prepare for the mixed call. Note
     * that ARG3 is untagged at this point */
    mov(ARG2, Src);
    a.mov(ARG3, Val.getValue() << _TAG_IMMED1_SIZE);
    a.mov(TMP4, ARG2);
    a.and_(TMP4, _TAG_IMMED1_MASK);
    a.cmp(TMP4, _TAG_IMMED1_SMALL);
    a.jne(mixed);
    a.mov(RET, ARG2);
    a.add(RET, ARG3);
    a.jno(next);

    /* Call mixed addition */
    a.bind(mixed);
    a.mov(ARG1, c_p);
    a.or_(ARG3, _TAG_IMMED1_SMALL);
    call((uint64_t)erts_mixed_plus);

    /* Check if addition worked */
    a.cmp(RET,THE_NON_VALUE);
    a.jne(next);

    emit_badarith(entry);

    /* all went well, store result in dst */
    a.bind(next);
    mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_plus(ArgVal LHS, ArgVal RHS,
                                      ArgVal Fail, ArgVal Dst,
                                      Instruction *I) {
    Label entry = a.newLabel(), next = a.newLabel(), generic = a.newLabel();

    a.bind(entry);

    mov(ARG2,LHS); // Used by erts_mixed_plus in this slot
    mov(ARG3,RHS); // Used by erts_mixed_plus in this slot
    emit_is_both_small(generic, ARG2, ARG3);

    comment("add with overflow check");
    a.mov(RET, ARG2);
    a.mov(TMP4, ARG3);
    a.and_(TMP4, ~_TAG_IMMED1_MASK);
    a.add(RET, TMP4);
    a.jno(next);

    a.bind(generic);
    a.mov(ARG1, c_p);
    // ARG2 and ARG3 set above
    call((uint64_t)erts_mixed_plus);
    a.cmp(RET,THE_NON_VALUE);

    if (Fail.getValue() != 0) {
        a.je(labels[Fail.getValue()]);
    } else {
        a.jne(next);
        emit_bif_arg_error({LHS, RHS}, entry, &bif_trap_export[BIF_splus_2]->info.mfa);
    }

    a.bind(next);
    mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_minus(ArgVal LHS, ArgVal RHS,
                                       ArgVal Fail, ArgVal Dst,
                                       Instruction *I) {
    Label entry = a.newLabel(), next = a.newLabel(), generic = a.newLabel();

    a.bind(entry);

    comment("is_both_small(X, Y)");
    mov(ARG2,LHS); // Used by erts_mixed_plus in this slot
    mov(ARG3,RHS); // Used by erts_mixed_plus in this slot
    emit_is_both_small(generic, ARG2, ARG3);

    comment("sub with overflow check");
    a.mov(RET, ARG2);
    a.mov(TMP4, ARG3);
    a.and_(TMP4, ~_TAG_IMMED1_MASK);
    a.sub(RET, TMP4);
    a.jno(next);

    a.bind(generic);
    a.mov(ARG1, c_p);
    // ARG2 and ARG3 set above
    call((uint64_t)erts_mixed_minus);
    a.cmp(RET,THE_NON_VALUE);

    if (Fail.getValue() != 0) {
        a.je(labels[Fail.getValue()]);
    } else {
        a.jne(next);
        emit_bif_arg_error({LHS, RHS}, entry,
                           &bif_trap_export[BIF_sminus_2]->info.mfa);
    }

    a.bind(next);
    mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_int_div(ArgVal Fail, ArgVal LHS, ArgVal RHS,
                                         ArgVal Dst, Instruction *Inst) {
    Label next, generic, fail, entry;

    next = a.newLabel();
    generic = a.newLabel();
    fail = a.newLabel();
    entry = a.newLabel();

    a.bind(entry);
    mov(ARG5, LHS);
    mov(ARG6, RHS);

    comment("Test div by zero");
    a.cmp(ARG6, SMALL_ZERO);
    if (Fail.getValue() != 0) {
        a.je(labels[Fail.getValue()]);
    } else {
        a.mov(x86::qword_ptr(c_p, offsetof(Process,freason)), BADARITH);
        a.je(fail);
    }

    emit_is_both_small(generic, ARG5, ARG6);

    a.mov(RET, ARG5);
    a.mov(TMP1, ARG6);
    a.sar(RET, _TAG_IMMED1_SIZE);
    a.sar(TMP1, _TAG_IMMED1_SIZE);
    a.cqo(); // This clobbers ARG3
    a.idiv(TMP1); // Result in rax aka RET

    comment("IS_SSMALL");
    a.mov(TMP1, RET);
    a.sar(TMP1, SMALL_BITS-1);
    a.inc(TMP1);
    a.cmp(TMP1, 2);
    a.jge(generic);

    comment("tag small");
    a.sal(RET, _TAG_IMMED1_SIZE);
    a.or_(RET, _TAG_IMMED1_SMALL);
    a.jmp(next);

    a.bind(generic);
    a.mov(ARG1, c_p);
    a.mov(ARG2, ARG5);
    a.mov(ARG3, ARG6);
    call((uint64_t)erts_int_div);
    a.cmp(RET,THE_NON_VALUE);

    if (Fail.getValue() != 0) {
        a.je(labels[Fail.getValue()]);
    } else {
        a.jne(next);
        a.bind(fail);
        emit_bif_arg_error({LHS, RHS}, entry,
                           &bif_trap_export[BIF_intdiv_2]->info.mfa);
    }

    a.bind(next);
    mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_m_div(ArgVal Fail, ArgVal LHS, ArgVal RHS,
                                       ArgVal Dst, Instruction *Inst) {
    Label next = a.newLabel(), entry = a.newLabel();

    a.bind(entry);
    mov(ARG2, LHS);
    mov(ARG3, RHS);
    /* Must be set last since mov() may clobber TMP1 */
    a.mov(ARG1, c_p);
    call((uint64_t)erts_mixed_div);
    a.cmp(RET,THE_NON_VALUE);

    if (Fail.getValue() != 0) {
        a.je(labels[Fail.getValue()]);
    } else {
        a.jne(next);
        emit_bif_arg_error({LHS, RHS}, entry,
                           &bif_trap_export[BIF_div_2]->info.mfa);
    }

    a.bind(next);
    mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_times(ArgVal Fail, ArgVal LHS, ArgVal RHS,
                                       ArgVal Dst, Instruction *Inst) {
    Label entry = a.newLabel(), next = a.newLabel(), generic = a.newLabel();

    a.bind(entry);

    mov(ARG5, LHS);
    mov(ARG6, RHS);
    emit_is_both_small(generic, ARG5, ARG6);

    comment("mul overflow");
    a.mov(RET, ARG5);
    a.mov(TMP3, ARG6);
    a.and_(RET, ~_TAG_IMMED1_MASK);
    a.sar(TMP3, _TAG_IMMED1_SIZE);
    a.imul(RET, TMP3); /* Clobbers TMP3 */
    a.jo(generic);

    comment("tag small");
    a.or_(RET, _TAG_IMMED1_SMALL);
    a.jmp(next);

    a.bind(generic);
    a.mov(ARG1, c_p);
    a.mov(ARG2, ARG5);
    a.mov(ARG3, ARG6);
    call((uint64_t)erts_mixed_times);
    a.cmp(RET,THE_NON_VALUE);

    if (Fail.getValue() != 0) {
        a.je(labels[Fail.getValue()]);
    } else {
        a.jne(next);
        emit_bif_arg_error({LHS, RHS}, entry,
                           &bif_trap_export[BIF_stimes_2]->info.mfa);
    }

    a.bind(next);
    mov(Dst, RET);
}
