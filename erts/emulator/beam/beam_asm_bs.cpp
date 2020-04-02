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
#include "erl_binary.h"
#include "erl_bits.h"
}

void BeamModuleAssembler::emit_fail_head_or_body(Label entry, ArgVal Fail) {
  if (Fail.getValue()) {
    a.jmp(labels[Fail.getValue()]);
  } else {
    emit_handle_error(entry);
  }
}

void BeamModuleAssembler::emit_badarg(Label entry, ArgVal Fail) {
  a.mov(x86::qword_ptr(c_p, offsetof(Process,freason)), BADARG);
  emit_fail_head_or_body(entry, Fail);
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
  call((uint64_t)i_bs_init);
  a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process, fcalls)));
  emit_swapin();
  mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_new_bs_put_integer_imm(ArgVal Src, ArgVal Fail, ArgVal Sz, ArgVal Flags, Instruction *I) {
  Label next = a.newLabel(), entry = a.newLabel();
  a.bind(entry);
  a.mov(ARG1, EBS);
  mov(ARG2, Src);
  mov(ARG3, Sz);
  mov(ARG4, Flags);
  call((uint64_t)erts_new_bs_put_integer);
  a.cmp(RET, 0);
  a.jne(next);
  emit_badarg(entry, Fail);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_bs_start_match3(ArgVal Src, ArgVal Live, ArgVal Fail, ArgVal Dst, Instruction *I) {
  Label is_binary = a.newLabel(), next = a.newLabel();
  mov(TMP1, Src);
  emit_is_boxed(labels[Fail.getValue()], TMP1);
  a.mov(TMP2, emit_boxed_val(TMP1));
  a.mov(TMP3, TMP2);
  a.and_(TMP3, _HEADER_SUBTAG_MASK);
  a.cmp(TMP3, BIN_MATCHSTATE_SUBTAG);
  a.je(next);
  a.cmp(TMP3, _TAG_HEADER_REFC_BIN);
  a.je(is_binary);
  a.cmp(TMP3, _TAG_HEADER_HEAP_BIN);
  a.je(is_binary);
  a.cmp(TMP3, _TAG_HEADER_SUB_BIN);
  a.jne(labels[Fail.getValue()]);
  a.bind(is_binary);
  comment("is_binary");
  mov(ArgVal(ArgVal::x, Live.getValue()), Src);
  emit_gc_test(ArgVal(ArgVal::i,0), ArgVal(ArgVal::i,ERL_BIN_MATCHSTATE_SIZE(0)), Live + 1);
  emit_swapout();
  a.mov(ARG1, c_p);
  mov(ARG2, ArgVal(ArgVal::x, Live.getValue()));
  call((uint64_t)erts_bs_start_match_3);
  emit_swapin();
  a.lea(TMP1, x86::qword_ptr(RET, TAG_PRIMARY_BOXED));
  a.bind(next);
  mov(Dst, TMP1);
}

void BeamModuleAssembler::emit_i_bs_start_match3_gp(ArgVal Src, ArgVal Live, ArgVal Fail, ArgVal Dst, ArgVal Pos, Instruction *I) {
  emit_nyi();
}

static bool i_bs_match_string(Eterm Ctx, Uint bits, byte *bytes) {
  ErlBinMatchBuffer* mb;
  Uint offs;

  mb = ms_matchbuffer(Ctx);
  if (mb->size - mb->offset < bits) {
    return false;
  }
  offs = mb->offset & 7;
  if (offs == 0 && (bits & 7) == 0) {
    if (sys_memcmp(bytes, mb->base+(mb->offset>>3), bits>>3)) {
      return false;
    }
  } else if (erts_cmp_bits(bytes, 0, mb->base+(mb->offset>>3), mb->offset & 7, bits)) {
    return false;
  }
  mb->offset += bits;
  return true;
}

void BeamModuleAssembler::emit_i_bs_match_string(ArgVal Ctx, ArgVal Fail, ArgVal Bits, ArgVal Ptr, Instruction *I) {
  mov(ARG1, Ctx);
  a.mov(ARG2, Bits.getValue());
  make_move_patch(ARG3, strings[Ptr.getValue()]);
  call((uint64_t)i_bs_match_string);
  a.cmp(RET, 0);
  a.je(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_i_bs_get_position(ArgVal Ctx, ArgVal Dst, Instruction *I) {
  mov(TMP1, Ctx);
  a.mov(TMP1, x86::qword_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb.offset)));
  a.sal(TMP1, _TAG_IMMED1_SIZE);
  a.or_(TMP1, _TAG_IMMED1_SMALL);
  mov(Dst, TMP1);
}

static Eterm i_bs_get_integer_8(Process *c_p, Eterm context) {
  Eterm _result;
  ErlBinMatchBuffer* _mb = ms_matchbuffer(context);

  if (_mb->size - _mb->offset < 8) {
    return THE_NON_VALUE;
  }
  if (BIT_OFFSET(_mb->offset) != 0) {
    _result = erts_bs_get_integer_2(c_p, 8, 0, _mb);
  } else {
    _result = make_small(_mb->base[BYTE_OFFSET(_mb->offset)]);
    _mb->offset += 8;
  }
  return _result;
}

void BeamModuleAssembler::emit_i_bs_get_integer_8(ArgVal Ctx, ArgVal Fail, ArgVal Dst, Instruction *I) {
  a.mov(ARG1, c_p);
  mov(ARG2, Ctx);
  call((uint64_t)i_bs_get_integer_8);
  a.cmp(RET, THE_NON_VALUE);
  a.je(labels[Fail.getValue()]);
  mov(Dst, RET);
}

static Eterm i_bs_get_integer_16(Process *c_p, Eterm context) {
  Eterm _result;
  ErlBinMatchBuffer* _mb = ms_matchbuffer(context);

  if (_mb->size - _mb->offset < 16) {
    return THE_NON_VALUE;
  }
  if (BIT_OFFSET(_mb->offset) != 0) {
    _result = erts_bs_get_integer_2(c_p, 16, 0, _mb);
  } else {
    _result = make_small(_mb->base[BYTE_OFFSET(_mb->offset)]);
    _mb->offset += 16;
  }
  return _result;
}

void BeamModuleAssembler::emit_i_bs_get_integer_16(ArgVal Ctx, ArgVal Fail, ArgVal Dst, Instruction *I) {
  a.mov(ARG1, c_p);
  mov(ARG2, Ctx);
  call((uint64_t)i_bs_get_integer_16);
  a.cmp(RET, THE_NON_VALUE);
  a.je(labels[Fail.getValue()]);
  mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_get_integer_imm(
  ArgVal Ctx, ArgVal Size, ArgVal Live, ArgVal Fail, ArgVal Flags, ArgVal Dst, Instruction *I) {
  mov(ArgVal(ArgVal::x, Live.getValue()), Ctx);
  emit_gc_test(ArgVal(ArgVal::i,0), ArgVal(ArgVal::i,1+WSIZE(NBYTES(Size.getValue()))), Live + 1);
  emit_i_bs_get_integer_small_imm(ArgVal(ArgVal::x, Live.getValue()), Size, Fail, Flags, Dst, I);
}

void BeamModuleAssembler::emit_i_bs_get_integer_small_imm(
  ArgVal Ctx, ArgVal Size, ArgVal Fail, ArgVal Flags, ArgVal Dst, Instruction *I) {
  mov(ARG4, Ctx);
  a.mov(ARG1, c_p);
  mov(ARG2, Size);
  mov(ARG3, Flags);
  a.lea(ARG4, x86::qword_ptr(ARG4, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb)));
  emit_light_swapout();
  call((uint64_t)erts_bs_get_integer_2);
  emit_light_swapin();
  a.cmp(RET, THE_NON_VALUE);
  a.je(labels[Fail.getValue()]);
  mov(Dst, RET);
}

void BeamModuleAssembler::emit_bs_test_zero_tail2(ArgVal Fail, ArgVal Ctx, Instruction *I) {
  mov(TMP1, Ctx);
  a.mov(TMP2, x86::qword_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb.offset)));
  a.mov(TMP1, x86::qword_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb.size)));
  a.cmp(TMP1, TMP2);
  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_bs_set_position(ArgVal Ctx, ArgVal Pos, Instruction *I) {
  mov(TMP1, Ctx);
  mov(TMP2, Pos);
  a.sar(TMP2, _TAG_IMMED1_SIZE);
  a.mov(x86::qword_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb.offset)), TMP2);
}

void BeamModuleAssembler::emit_i_bs_get_binary_all2(
  ArgVal Ctx, ArgVal Fail, ArgVal Live, ArgVal Unit, ArgVal Dst, Instruction *I) {
  mov(ArgVal(ArgVal::x, Live.getValue()), Ctx);
  emit_gc_test(ArgVal(ArgVal::i,0), ArgVal(ArgVal::i,EXTRACT_SUB_BIN_HEAP_NEED), Live + 1);
  mov(TMP1, ArgVal(ArgVal::x, Live.getValue()));
  a.mov(RET, x86::qword_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb.size)));
  a.sub(RET, x86::qword_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb.offset)));
  mov(TMP4, Unit);
  a.xor_(x86::rdx, x86::rdx);
  a.div(TMP4); // TODO: Should change to bit check if Unit is power of 2
  a.test(TMP3, TMP3); // The remainder is put in TMP3
  a.jne(labels[Fail.getValue()]);
  emit_light_swapout();
  a.lea(ARG2, x86::qword_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb)));
  a.mov(ARG1, c_p);
  call((uint64_t)erts_bs_get_binary_all_2);
  emit_light_swapin();
  mov(Dst, RET);
}
