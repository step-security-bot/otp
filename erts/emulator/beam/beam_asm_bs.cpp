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
    #include "erl_binary.h"
    #include "erl_bits.h"
}

void BeamModuleAssembler::emit_fail_head_or_body(ArgVal Fail) {
    if (Fail.getValue()) {
        a.jmp(labels[Fail.getValue()]);
    } else {
        a.mov(RET, RET_find_func_info);
        a.jmp(ga->getSwapout());
    }
}

void BeamModuleAssembler::emit_badarg(ArgVal Fail) {
    a.mov(x86::qword_ptr(c_p, offsetof(Process,freason)), BADARG);
    emit_fail_head_or_body(Fail);
}

void TEST_BIN_VHEAP(Process *c_p, Eterm *reg, Uint VNh, Uint Nh, Uint Live) {
    int need = Nh;
    if (c_p->stop - c_p->htop < need || MSO(c_p).overhead + VNh >= BIN_VHEAP_SZ(c_p)) {
        c_p->fcalls -= erts_garbage_collect_nobump(c_p, need, reg, Live, c_p->fcalls);
    }
}

void GC_TEST(Process *c_p, Eterm *reg, Uint Ns, Uint Nh, Uint Live) {
    int need = Nh + Ns;
    if (ERTS_UNLIKELY(c_p->stop - c_p->htop < need)) {
       c_p->fcalls -= erts_garbage_collect_nobump(c_p, need, reg, Live, c_p->fcalls);
    }
}

Eterm i_bs_init(Process *c_p, Eterm *reg, ERL_BITS_DECLARE_STATEP, Eterm BsOp1, Eterm BsOp2, unsigned Live) {
    if (BsOp1 <= ERL_ONHEAP_BIN_LIMIT) {
        ErlHeapBin* hb;
        Uint bin_need;

        bin_need = heap_bin_size(BsOp1);
        erts_bin_offset = 0;
        erts_writable_bin = 0;
        GC_TEST(c_p, reg, 0, bin_need+BsOp2+ERL_SUB_BIN_SIZE, Live);
        hb = (ErlHeapBin *) c_p->htop;
        c_p->htop += bin_need;
        hb->thing_word = header_heap_bin(BsOp1);
        hb->size = BsOp1;
        erts_current_bin = (byte *) hb->data;
        return make_binary(hb);
    } else {
        Binary* bptr;
        ProcBin* pb;

        erts_bin_offset = 0;
        erts_writable_bin = 0;
        TEST_BIN_VHEAP(c_p, reg, BsOp1 / sizeof(Eterm),
                        BsOp2 + PROC_BIN_SIZE + ERL_SUB_BIN_SIZE, Live);

        /*
         * Allocate the binary struct itself.
         */
        bptr = erts_bin_nrml_alloc(BsOp1);
        erts_current_bin = (byte *) bptr->orig_bytes;

        /*
         * Now allocate the ProcBin on the heap.
         */
        pb = (ProcBin *) c_p->htop;
        c_p->htop += PROC_BIN_SIZE;
        pb->thing_word = HEADER_PROC_BIN;
        pb->size = BsOp1;
        pb->next = MSO(c_p).first;
        MSO(c_p).first = (struct erl_off_heap_header*) pb;
        pb->val = bptr;
        pb->bytes = (byte*) bptr->orig_bytes;
        pb->flags = 0;

        OH_OVERHEAD(&(MSO(c_p)), BsOp1 / sizeof(Eterm));

        return make_binary(pb);
    }
}

void BeamModuleAssembler::emit_i_bs_init(ArgVal Size, ArgVal Live, ArgVal Dst, Instruction *I) {
    emit_swapout();
    a.mov(x86::qword_ptr(c_p, offsetof(Process, fcalls)), FCALLS);
    a.mov(ARG1, c_p);
    a.mov(ARG2, x_reg);
    a.mov(ARG3, EBS);
    mov(ARG4, Size);
    a.mov(ARG5, 0);
    mov(ARG6, Live);
    a.call((uint64_t)i_bs_init);
    a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process, fcalls)));
    emit_swapin();
    mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_new_bs_put_integer_imm(ArgVal Src, ArgVal Fail, ArgVal Sz, ArgVal Flags, Instruction *I) {
    Label next = a.newLabel();
    a.mov(ARG1, EBS);
    mov(ARG2, Src);
    mov(ARG3, Sz);
    mov(ARG4, Flags);
    a.call((uint64_t)erts_new_bs_put_integer);
    a.cmp(RET, 0);
    a.jne(next);
    emit_badarg(Fail);
    a.bind(next);
}