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

extern "C" ErtsCodeMFA *ubif2mfa(void* uf);

void BeamModuleAssembler::emit_call_light_bif_only(ArgVal Bif, ArgVal Exp, Instruction *I) {
    Label next = a.newLabel();
    emit_swapout();
    a.add(FCALLS, -1);
    a.mov(x86::qword_ptr(c_p, offsetof(Process,fcalls)), FCALLS);
    a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process,mbuf))); // Save the previous mbuf for GC call
    a.mov(ARG1, c_p);
    a.mov(ARG2, x_reg);
    a.mov(ARG3, 0); // Should be I
    a.call(Bif.getValue());
    // TODO: Should emit a gc check here
    emit_swapin();
    a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process,fcalls)));
    a.cmp(RET, THE_NON_VALUE);
    a.jne(next);
    //emit_nyi();
    a.bind(next);
    mov(ArgVal(ArgVal::x,0), RET);
    emit_return();
}

// WARNING: Note that args here HAVE to be given in reverse order as that is the way
// that the bifs expect them to be in....
void BeamModuleAssembler::emit_call_guard_bif(std::vector<ArgVal> args, ArgVal bif, ArgVal Dst, Instruction *I, Label next) {
    Label fail = a.newLabel();
    unsigned stackAdj = (args.size()+1) / 2 * sizeof(Eterm); // Allocate aligned stack
    a.sub(x86::rsp, stackAdj);
    // Store bif arguments on stack
    for (unsigned i = 0; i < args.size(); i++)
        mov(x86::qword_ptr(x86::rsp, i * sizeof(Eterm)), args[i]);

    a.mov(x86::qword_ptr(c_p, offsetof(Process, fcalls)), FCALLS);

    a.mov(ARG1, c_p);
    a.mov(ARG2, x86::rsp);
    a.mov(ARG3, (uint64_t)I->I);
    a.call(bif.getValue());

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
    Label next = a.newLabel();
    emit_call_guard_bif({Src1}, Bif, Dst, I, next);
    emit_bif_arg_error({Src1}, I, ubif2mfa((void*)Bif.getValue()));
    a.bind(next);
}

void BeamModuleAssembler::emit_i_bif2_body(ArgVal Src1, ArgVal Src2, ArgVal Bif, ArgVal Dst, Instruction *I) {
    Label next = a.newLabel();
    emit_call_guard_bif({Src2, Src1}, Bif, Dst, I, next);
    emit_bif_arg_error({Src2, Src1}, I, ubif2mfa((void*)Bif.getValue()));
    a.bind(next);
}

void BeamModuleAssembler::emit_i_bif3_body(ArgVal Src1, ArgVal Src2, ArgVal Src3, ArgVal Bif, ArgVal Dst, Instruction *I) {
    Label next = a.newLabel();
    emit_call_guard_bif({Src3, Src2, Src1}, Bif, Dst, I, next);
    emit_bif_arg_error({Src3, Src2, Src1}, I, ubif2mfa((void*)Bif.getValue()));
    a.bind(next);
}

void BeamModuleAssembler::emit_i_bif1(ArgVal Src1, ArgVal Fail, ArgVal Bif, ArgVal Dst, Instruction *I) {
    Label next = a.newLabel();
    emit_call_guard_bif({Src1}, Bif, Dst, I, next);
    a.jmp(Fail.getValue());
    a.bind(next);
}

void BeamModuleAssembler::emit_i_bif2(ArgVal Src1, ArgVal Src2, ArgVal Fail, ArgVal Bif, ArgVal Dst, Instruction *I) {
    Label next = a.newLabel();
    emit_call_guard_bif({Src2, Src1}, Bif, Dst, I, next);
    a.jmp(Fail.getValue());
    a.bind(next);
}

void BeamModuleAssembler::emit_i_bif3(ArgVal Src1, ArgVal Src2, ArgVal Src3, ArgVal Fail, ArgVal Bif, ArgVal Dst, Instruction *I) {
    Label next = a.newLabel();
    emit_call_guard_bif({Src3, Src2, Src1}, Bif, Dst, I, next);
    a.jmp(Fail.getValue());
    a.bind(next);
}