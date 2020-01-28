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
    ERTS_ASSERT(!err && "Failed to create asm swapin function");
    reset();

    // setLogger("swapout.asm");
    emit_asm_swapout();
    err = rt->add(&swapout,&code);
    ERTS_ASSERT(!err && "Failed to create asm swapout function");
    reset();

    // setLogger("gc_after_bif.asm");
    emit_gc_efter_bif();
    err = rt->add(&gc_after_bif,&code);
    ERTS_ASSERT(!err && "Failed to create asm gc_after_bif function");
    reset();
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

void BeamGlobalAssembler::emit_gc_efter_bif() {
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
    a.cmp(TMP1, x86::qword_ptr(c_p, offsetof(Process,off_heap.overhead)));
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
    a.call((uint64_t)erts_gc_after_bif_call_lhf);

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
    a.push(ARG1); // ctx
    a.push(ARG5); // *EBS

    // Move the arguments to the correct registers, ignoring c_p, FCALLS and f_reg for now...
    // typedef void (*BeamAsmFunc)(BeamInstr **,Process *,Eterm *, Eterm **, Eterm **, Sint *, FloatDef *);
    a.mov(c_p, ARG2);
    a.mov(x_reg, ARG3);
    a.mov(HTOP, x86::qword_ptr(ARG1, offsetof(BeamAsmContext, HTOP)));
    a.mov(E, x86::qword_ptr(ARG1, offsetof(BeamAsmContext, E)));
    a.mov(FCALLS, x86::qword_ptr(ARG1, offsetof(BeamAsmContext, FCALLS)));
    a.mov(f_reg, ARG4);
    a.jmp(ARG6);
}

void BeamGlobalAssembler::emit_asm_swapout() {

    // Below here we simulate a return instruction to get the proper return into
    // the interpreter again

    // Put the BeamAsmContext in TMP1
    a.mov(TMP1, x86::qword_ptr(x86::rsp, 8));

    // Set I to be the return address, the address is stored in TMP3
    a.mov(x86::qword_ptr(TMP1, offsetof(BeamAsmContext, I)), TMP3);

    // Restore the rest of the emulator state
    a.mov(x86::qword_ptr(TMP1, offsetof(BeamAsmContext, HTOP)), HTOP);
    a.mov(x86::qword_ptr(TMP1, offsetof(BeamAsmContext, E)), E);
    a.mov(x86::qword_ptr(TMP1, offsetof(BeamAsmContext, FCALLS)), FCALLS);

    // Adjust stack
    a.lea(x86::rsp, x86::qword_ptr(x86::rsp, 2 * 8));

    // Pop all callee save
    a.pop(f_reg);
    a.pop(FCALLS);
    a.pop(E);
    a.pop(HTOP);
    a.pop(x_reg);
    a.pop(c_p);

    emit_function_postamble();
}