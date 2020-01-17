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

using namespace asmjit;

ArgVal ArgVal::x0(ArgVal::x, 0);
ArgVal ArgVal::x1(ArgVal::x, 1);
ArgVal ArgVal::x2(ArgVal::x, 2);
ArgVal ArgVal::x3(ArgVal::x, 3);

BeamGlobalAssembler::BeamGlobalAssembler(JitRuntime *rt) : BeamAssembler(rt) {
    Error err;

    // setLogger("garbage_collect.asm");
    emit_garbage_collect();
    err = rt->add(&garbage_collect,&code);
    ERTS_ASSERT(!err && "Failed to create garbage collection function");
    reset();

    // setLogger("swapin.asm");
    emit_asm_swapin();
    err = rt->add(&swapin,&code);
    ERTS_ASSERT(!err && "Failed to create asm swampin function");
    reset();

    // setLogger("swapout.asm");
    emit_asm_swapout();
    err = rt->add(&swapout,&code);
    ERTS_ASSERT(!err && "Failed to create asm swampout function");
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
    a.call((uint64_t)erts_garbage_collect_nobump);
    a.sub(FCALLS, RET);
    emit_swapin();
    emit_function_postamble();
}

void BeamGlobalAssembler::emit_asm_swapin() {
    emit_function_preamble();

    // Push all callee save
    a.push(c_p);
    a.push(x_reg);
    a.push(HTOP);
    a.push(E);
    a.push(FCALLS);
    a.push(f_reg);

    // Have to make sure the stack is 16 byte aligned here
    a.push(ARG1); // *I
    a.push(ARG4); // *HTOP
    a.push(ARG5); // *E
    a.push(ARG6); // *FCALLS

    // Move the arguments to the correct registers, ignoring c_p, FCALLS and f_reg for now...
    // typedef void (*BeamAsmFunc)(BeamInstr **,Process *,Eterm *, Eterm **, Eterm **, Sint *, FloatDef *);
    a.mov(c_p, ARG2);
    a.mov(x_reg, ARG3);
    a.mov(HTOP, x86::qword_ptr(ARG4));
    a.mov(E, x86::qword_ptr(ARG5));
    a.mov(FCALLS, x86::qword_ptr(ARG6));
    a.mov(f_reg, ARG7);
    a.jmp(ARG8);
}

void BeamGlobalAssembler::emit_asm_swapout() {

    // Below here we simulate a return instruction to get the proper return into
    // the interpreter again

    // Set I to be the return address, the address is stored in TMP3
    a.mov(TMP2, x86::qword_ptr(x86::rsp, 24));
    a.mov(x86::qword_ptr(TMP2, 0), TMP3);

    // Restore the rest of the emulator state
    a.mov(TMP1, x86::qword_ptr(x86::rsp, 16));
    a.mov(x86::qword_ptr(TMP1, 0), HTOP);
    a.mov(TMP1, x86::qword_ptr(x86::rsp, 8));
    a.mov(x86::qword_ptr(TMP1, 0), E);
    a.mov(TMP1, x86::qword_ptr(x86::rsp, 0));
    a.mov(x86::qword_ptr(TMP1, 0), FCALLS);

    // Adjust stack
    a.lea(x86::rsp, x86::qword_ptr(x86::rsp, 4 * 8));

    // Pop all callee save
    a.pop(f_reg);
    a.pop(FCALLS);
    a.pop(E);
    a.pop(HTOP);
    a.pop(x_reg);
    a.pop(c_p);

    emit_function_postamble();
}