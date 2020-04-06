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
    Label dispatch = a.newLabel();
    comment("handle_error");
    for (unsigned i = 0; i < args.size(); i++)
        mov(ArgVal(ArgVal::x, i), args[i]);
    emit_handle_error(entry, mfa);
}

void BeamModuleAssembler::emit_i_increment(ArgVal Src, ArgVal Val, ArgVal Dst, Instruction *Inst) {
    Label mixed = a.newLabel(), next = a.newLabel();

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

    // TODO: Have to store I in a register here so that we know
    //       what happened
    a.je(labels[1]); // Jump to badarith code

    /* all went well, store result in dst */
    a.bind(next);
    mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_int_div(ArgVal Fail, ArgVal Op1, ArgVal Op2, ArgVal Dst, Instruction *Inst) {
    Label next = a.newLabel(), generic = a.newLabel(), fail = a.newLabel(), entry = a.newLabel();

    a.bind(entry);
    mov(ARG3, Op2);
    comment("Test div by zero");
    a.cmp(ARG3, SMALL_ZERO);
    a.mov(x86::qword_ptr(c_p, offsetof(Process,freason)), BADARITH);
    a.je(fail);

    mov(ARG2, Op1);

    comment("is_both_small");
    mov(TMP1,Op1);
    a.and_(TMP1,ARG2);
    a.and_(TMP1,_TAG_IMMED1_MASK);
    a.cmp(TMP1,_TAG_IMMED1_SMALL);
    a.jne(generic);

    a.mov(RET, ARG2);
    a.mov(TMP1, ARG3);
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
    // ARG2 set above
    mov(ARG3, Op2); // ARG3 may have been clobbered by cqto so re-read
    call((uint64_t)erts_int_div);
    a.cmp(RET,THE_NON_VALUE);
    if (Fail.getValue() != 0) {
        a.je(labels[Fail.getValue()]);
        a.jmp(next);
    } else {
        a.jne(next);
    }
    a.bind(fail);
    emit_bif_arg_error({Op1, Op2}, entry, &bif_trap_export[BIF_intdiv_2].info.mfa);

    a.bind(next);
    mov(Dst, RET);

}
