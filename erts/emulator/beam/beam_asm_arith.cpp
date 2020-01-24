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

#include "beam_asm.h"

extern "C" {
    #include "erl_bif_table.h"
}

extern "C" BeamInstr *handle_error(void);

void BeamModuleAssembler::emit_bif_arg_error(std::vector<ArgVal> args, Instruction *inst, ErtsCodeMFA *mfa) {
    Label dispatch = a.newLabel();
    for (unsigned i = 0; i < args.size(); i++)
        mov(ArgVal(ArgVal::x, i), args[i]);
    emit_swapout();
    a.mov(ARG1, c_p);
    a.mov(ARG2, (uint64_t)inst->I);
    a.mov(ARG3, x_reg);
    a.mov(ARG4, (uint64_t)mfa);
    a.call((uint64_t)handle_error);
    emit_swapin();
    a.mov(TMP3, RET);
    a.cmp(TMP3, 0);
    a.jne(dispatch);
    a.mov(RET, RET_do_schedule);
    a.jmp(ga->getSwapout());
    a.bind(dispatch);
    a.mov(RET, RET_dispatch);
    a.jmp(ga->getSwapout());
}

void BeamModuleAssembler::emit_i_int_div(ArgVal Fail, ArgVal Op1, ArgVal Op2, ArgVal Dst, Instruction *Inst) {
    Label next = a.newLabel(), generic = a.newLabel(), fail = a.newLabel();

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
    a.call((uint64_t)erts_int_div);
    a.cmp(RET,THE_NON_VALUE);
    if (Fail.getValue() != 0) {
        a.je(labels[Fail.getValue()]);
        a.jmp(next);
    } else {
        a.jne(next);
    }
    a.bind(fail);
    emit_bif_arg_error({Op1, Op2}, inst, &bif_trap_export[BIF_intdiv_2].info.mfa);

    a.bind(next);
    mov(Dst, RET);

}