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
    #include "big.h"
}

static x86::Mem getFRef(x86::Gp Reg, ArgVal Dst) {
    return x86::qword_ptr(Reg, Dst.getValue() * sizeof(double));
}

// x64.fload(Src, Dst);
void BeamModuleAssembler::emit_fload(ArgVal Src, ArgVal Dst, Instruction *Inst) {
    ASSERT(Dst.getType() == ArgVal::TYPE::l);

    /* {thing_word,double} */
    mov(TMP1, Src);
    a.movsd(x86::xmm0, emit_boxed_val(TMP1, sizeof(Eterm)));
    a.movsd(getFRef(f_reg, Dst), x86::xmm0);
}

// x64.fstore(Src, Dst);
void BeamModuleAssembler::emit_fstore(ArgVal Src, ArgVal Dst, Instruction *Inst) {
    ASSERT(Src.getType() == ArgVal::TYPE::l);

    /* {thing_word,double} */
    a.movsd(x86::xmm0, getFRef(f_reg, Src));
    a.mov(x86::qword_ptr(HTOP), HEADER_FLONUM);
    a.movsd(x86::qword_ptr(HTOP, sizeof(Eterm)), x86::xmm0);

    a.mov(TMP1, HTOP);
    a.or_(TMP1, make_float(0));
    mov(Dst, TMP1);

    a.add(HTOP, FLOAT_SIZE_OBJECT);
}

static int handle_fconv(Eterm src, double *dst) {
    if (is_small(src)) {
        *dst = (double)signed_val(src);
    } if (is_float(src)) {
        GET_DOUBLE(src, *(FloatDef *)dst);
    } else if (is_big(src)) {
        if (big_to_double(src, dst) < 0) {
            return 1;
        }
    } else {
        return 1;
    }

    return 0;
}

// x64.fconv(Src, Dst);
void BeamModuleAssembler::emit_fconv(ArgVal Src, ArgVal Dst, Instruction *Inst) {
    ASSERT(Dst.getType() == ArgVal::TYPE::l);

    Label next = a.newLabel();

    mov(TMP1, Src);
    a.lea(ARG2, getFRef(f_reg, Dst));
    call((uint64_t)handle_fconv);
    a.test(RET, RET);
    a.je(next);

    /* BADARITH */
    emit_nyi();

    a.bind(next);
}

// x64.i_fadd(Src1, Src2, Dst);
void BeamModuleAssembler::emit_i_fadd(ArgVal LHS, ArgVal RHS, ArgVal Dst, Instruction *Inst) {
    ASSERT(LHS.getType() == ArgVal::TYPE::l);
    ASSERT(RHS.getType() == ArgVal::TYPE::l);
    ASSERT(Dst.getType() == ArgVal::TYPE::l);

    Label next = a.newLabel();

    a.movsd(x86::xmm0, getFRef(f_reg, LHS));
    a.movsd(x86::xmm1, getFRef(f_reg, RHS));
    a.addpd(x86::xmm0, x86::xmm1);
    a.vucomisd(x86::xmm0, x86::xmm0);
    a.je(next);

    /* BADARITH */
    emit_nyi();

    a.bind(next);
    a.movsd(x86::qword_ptr(f_reg, Dst.getValue() * sizeof(double)), x86::xmm0);
}

// x64.i_fsub(Src1, Src2, Dst);
void BeamModuleAssembler::emit_i_fsub(ArgVal LHS, ArgVal RHS, ArgVal Dst, Instruction *Inst) {
    ASSERT(LHS.getType() == ArgVal::TYPE::l);
    ASSERT(RHS.getType() == ArgVal::TYPE::l);
    ASSERT(Dst.getType() == ArgVal::TYPE::l);

    Label next = a.newLabel();

    a.movsd(x86::xmm0, getFRef(f_reg, LHS));
    a.movsd(x86::xmm1, getFRef(f_reg, LHS));
    a.subpd(x86::xmm0, x86::xmm1);
    a.vucomisd(x86::xmm0, x86::xmm0);
    a.je(next);

    /* BADARITH */
    emit_nyi();

    a.bind(next);
    a.movsd(x86::qword_ptr(f_reg, Dst.getValue() * sizeof(double)), x86::xmm0);
}

// x64.i_fmul(Src1, Src2, Dst);
void BeamModuleAssembler::emit_i_fmul(ArgVal LHS, ArgVal RHS, ArgVal Dst, Instruction *Inst) {
    ASSERT(LHS.getType() == ArgVal::TYPE::l);
    ASSERT(RHS.getType() == ArgVal::TYPE::l);
    ASSERT(Dst.getType() == ArgVal::TYPE::l);

    Label next = a.newLabel();

    a.movsd(x86::xmm0, getFRef(f_reg, LHS));
    a.movsd(x86::xmm1, getFRef(f_reg, LHS));
    a.mulpd(x86::xmm0, x86::xmm1);
    a.vucomisd(x86::xmm0, x86::xmm0);
    a.je(next);

    /* BADARITH */
    emit_nyi();

    a.bind(next);
    a.movsd(x86::qword_ptr(f_reg, Dst.getValue() * sizeof(double)), x86::xmm0);
}

// x64.i_fdiv(Src1, Src2, Dst);
void BeamModuleAssembler::emit_i_fdiv(ArgVal LHS, ArgVal RHS, ArgVal Dst, Instruction *Inst) {
    ASSERT(LHS.getType() == ArgVal::TYPE::l);
    ASSERT(RHS.getType() == ArgVal::TYPE::l);
    ASSERT(Dst.getType() == ArgVal::TYPE::l);

    Label next = a.newLabel();

    a.movsd(x86::xmm0, getFRef(f_reg, LHS));
    a.movsd(x86::xmm1, getFRef(f_reg, RHS));
    a.divpd(x86::xmm0, x86::xmm1);
    a.vucomisd(x86::xmm0, x86::xmm0);
    a.je(next);

    /* BADARITH */
    emit_nyi();

    a.bind(next);
    a.movsd(x86::qword_ptr(f_reg, Dst.getValue() * sizeof(double)), x86::xmm0);
}

// x64.i_fnegate(Src, Dst);
void BeamModuleAssembler::emit_i_fnegate(ArgVal Src, ArgVal Dst, Instruction *Inst) {
    ASSERT(Src.getType() == ArgVal::TYPE::l);
    ASSERT(Dst.getType() == ArgVal::TYPE::l);

    Label next = a.newLabel();

    /* xmm0 = 0.0 */
    a.psubd(x86::xmm0, x86::xmm0);
    a.movsd(x86::xmm1, getFRef(f_reg, Src));
    a.subpd(x86::xmm0, x86::xmm1);
    a.vucomisd(x86::xmm0, x86::xmm0);
    a.je(next);

    /* BADARITH */
    emit_nyi();

    a.bind(next);
    a.movsd(getFRef(f_reg, Dst), x86::xmm0);
}

#ifndef NO_FPE_SIGNALS

void BeamModuleAssembler::emit_fclearerror(Instruction *Inst) {
    a.hlt();
}

void BeamModuleAssembler::emit_i_fcheckerror(Instruction *Inst) {
    a.hlt();
}

#endif
