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

// Clobbers RET + TMP3
x86::Gp BeamModuleAssembler::emit_bs_get_unchecked_field_size(ArgVal Size, int unit, Label fail) {

  mov(RET, Size);
  a.mov(TMP3, RET);
  a.and_(TMP3, _TAG_IMMED1_MASK);
  a.cmp(TMP3, _TAG_IMMED1_SMALL);
  a.jne(fail);
  a.sar(RET, _TAG_IMMED1_SIZE);
  a.cmp(RET, 0);
  a.jl(fail);

  /* Size = (Size) * (Unit) */
  a.mov(TMP3, unit);
  a.mul(TMP3); /* CLOBBERS ARG3! */
  return RET;
}

// Clobbers RET + TMP3
x86::Gp BeamModuleAssembler::emit_bs_get_field_size(ArgVal Size, int unit, Label fail) {
  x86::Gp ret = emit_bs_get_unchecked_field_size(Size, unit, fail);
  a.jo(fail);
  return ret;
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

Eterm i_bs_init(Process *c_p, Eterm *reg, ERL_BITS_DECLARE_STATEP, Eterm BsOp1,
                Eterm BsOp2, unsigned Live) {
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

void BeamModuleAssembler::emit_i_bs_init_heap(ArgVal Size, ArgVal Heap, ArgVal Live, ArgVal Dst, Instruction *I) {
  emit_heavy_swapout();
  a.mov(ARG1, c_p);
  a.mov(ARG2, x_reg);
  a.mov(ARG3, EBS);
  mov(ARG4, Size);
  mov(ARG5, Heap);
  mov(ARG6, Live);
  call((uint64_t)i_bs_init);
  emit_heavy_swapin();
  mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_init_fail_heap(ArgVal Size, ArgVal Heap,
                                                   ArgVal Fail, ArgVal Live,
                                                   ArgVal Dst, Instruction *I) {
  Label entry = a.newLabel(), next = a.newLabel(), fail = a.newLabel();
  a.bind(entry);
  // Clobbers TMP3
  x86::Gp ret = emit_bs_get_unchecked_field_size(Size, 1, fail);
  ASSERT(ret == RET);
  emit_heavy_swapout();
  a.mov(ARG1, c_p);
  a.mov(ARG2, x_reg);
  a.mov(ARG3, EBS);
  a.mov(ARG4, ret);
  mov(ARG5, Heap);
  mov(ARG6, Live);
  call((uint64_t)i_bs_init);
  emit_heavy_swapin();
  mov(Dst, RET);
  a.jmp(next);
  a.bind(fail);
  emit_badarg(entry, Fail);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_bs_init(ArgVal Size, ArgVal Live, ArgVal Dst, Instruction *I) {
  ArgVal Heap(ArgVal::TYPE::u, 0);
  emit_i_bs_init_heap(Size, Heap, Live, Dst, I);
}

void BeamModuleAssembler::emit_i_bs_init_fail(ArgVal Size, ArgVal Fail,
                                              ArgVal Live, ArgVal Dst,
                                              Instruction *I) {
  ArgVal Heap(ArgVal::TYPE::u, 0);
  emit_i_bs_init_fail_heap(Size, Heap, Fail, Live, Dst, I);
}


static Eterm i_bs_init_bits(Process *c_p, Eterm *reg, ERL_BITS_DECLARE_STATEP,
                            Uint num_bits, Uint alloc, unsigned Live) {
  Eterm new_binary;
  Uint num_bytes = ((Uint64)num_bits+(Uint64)7) >> 3;

  if (num_bits & 7) {
    alloc += ERL_SUB_BIN_SIZE;
  }
  if (num_bytes <= ERL_ONHEAP_BIN_LIMIT) {
    alloc += heap_bin_size(num_bytes);
  } else {
    alloc += PROC_BIN_SIZE;
  }
  GC_TEST(c_p, reg, 0, alloc, Live);

  /* num_bits = Number of bits to build
   * num_bytes = Number of bytes to allocate in the binary
   * alloc = Total number of words to allocate on heap
   * Operands: NotUsed NotUsed Dst
   */
  if (num_bytes <= ERL_ONHEAP_BIN_LIMIT) {
    ErlHeapBin* hb;

    erts_bin_offset = 0;
    erts_writable_bin = 0;
    hb = (ErlHeapBin *) c_p->htop;
    c_p->htop += heap_bin_size(num_bytes);
    hb->thing_word = header_heap_bin(num_bytes);
    hb->size = num_bytes;
    erts_current_bin = (byte *) hb->data;
    new_binary = make_binary(hb);

  do_bits_sub_bin:
    if (num_bits & 7) {
      ErlSubBin* sb;

      sb = (ErlSubBin *) c_p->htop;
      c_p->htop += ERL_SUB_BIN_SIZE;
      sb->thing_word = HEADER_SUB_BIN;
      sb->size = num_bytes - 1;
      sb->bitsize = num_bits & 7;
      sb->offs = 0;
      sb->bitoffs = 0;
      sb->is_writable = 0;
      sb->orig = new_binary;
      new_binary = make_binary(sb);
    }
//    HEAP_SPACE_VERIFIED(0);
    return new_binary;
  } else {
    Binary* bptr;
    ProcBin* pb;

    erts_bin_offset = 0;
    erts_writable_bin = 0;

    /*
     * Allocate the binary struct itself.
     */
    bptr = erts_bin_nrml_alloc(num_bytes);
    erts_current_bin = (byte *) bptr->orig_bytes;

    /*
     * Now allocate the ProcBin on the heap.
     */
    pb = (ProcBin *) c_p->htop;
    c_p->htop += PROC_BIN_SIZE;
    pb->thing_word = HEADER_PROC_BIN;
    pb->size = num_bytes;
    pb->next = MSO(c_p).first;
    MSO(c_p).first = (struct erl_off_heap_header*) pb;
    pb->val = bptr;
    pb->bytes = (byte*) bptr->orig_bytes;
    pb->flags = 0;
    OH_OVERHEAD(&(MSO(c_p)), pb->size / sizeof(Eterm));
    new_binary = make_binary(pb);
    goto do_bits_sub_bin;
  }
}

// i_bs_init_bits(NumBits, Live, Dst);
void BeamModuleAssembler::emit_i_bs_init_bits(ArgVal NumBits, ArgVal Live, ArgVal Dst, Instruction *I) {
  ArgVal heap(ArgVal::TYPE::u, 0);
  emit_i_bs_init_bits_heap(NumBits, heap, Live, Dst, I);
}
// i_bs_init_bits_heap(NumBits, Alloc, Live, Dst);
void BeamModuleAssembler::emit_i_bs_init_bits_heap(ArgVal NumBits, ArgVal Alloc, ArgVal Live, ArgVal Dst, Instruction *I) {
  emit_heavy_swapout();
  a.mov(ARG1, c_p);
  a.mov(ARG2, x_reg);
  a.mov(ARG3, EBS);
  mov(ARG4, NumBits);
  mov(ARG5, Alloc);
  mov(ARG6, Live);
  call((uint64_t)i_bs_init_bits);
  emit_heavy_swapin();
  mov(Dst, RET);
}
// i_bs_init_bits_fail(NumBits, Fail, Live, Dst);
void BeamModuleAssembler::emit_i_bs_init_bits_fail(ArgVal NumBits, ArgVal Fail,
                                                   ArgVal Live, ArgVal Dst, Instruction *I) {
  ArgVal Heap(ArgVal::TYPE::u, 0);
  emit_i_bs_init_bits_fail_heap(NumBits, Heap, Fail, Live, Dst, I);
}
// i_bs_init_bits_fail_heap(NumBits, Alloc, Fail, Live, Dst);
void BeamModuleAssembler::emit_i_bs_init_bits_fail_heap(ArgVal NumBits, ArgVal Alloc,
                                                        ArgVal Fail, ArgVal Live, ArgVal Dst, Instruction *I) {
  Label entry = a.newLabel(), next = a.newLabel(), fail = a.newLabel();
  a.bind(entry);
  // Clobbers TMP3
  x86::Gp ret = emit_bs_get_unchecked_field_size(NumBits, 1, fail);
  ASSERT(ret == RET);
  emit_heavy_swapout();
  a.mov(ARG1, c_p);
  a.mov(ARG2, x_reg);
  a.mov(ARG3, EBS);
  a.mov(ARG4, ret);
  mov(ARG5, Alloc);
  mov(ARG6, Live);
  call((uint64_t)i_bs_init_bits);
  emit_heavy_swapin();
  mov(Dst, RET);
  a.jmp(next);
  a.bind(fail);
  emit_badarg(entry, Fail);
  a.bind(next);
}

void BeamModuleAssembler::emit_bs_put_string(ArgVal Size, ArgVal Ptr, Instruction *I) {
  a.mov(ARG1, EBS);
  make_move_patch(ARG2, strings, Ptr.getValue());
  mov(ARG3, Size);
  call((uint64_t)erts_new_bs_put_string);
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

void BeamModuleAssembler::emit_i_new_bs_put_integer(ArgVal Fail, ArgVal Sz, ArgVal Flags,
                                                    ArgVal Src, Instruction *I) {
  Label next = a.newLabel(), entry = a.newLabel(), fail = a.newLabel();
  a.bind(entry);
  // Clobbers TMP3
  x86::Gp ret = emit_bs_get_unchecked_field_size(Sz, Flags.getValue() >> 3, fail);
  ASSERT(ret == RET);
  a.mov(ARG1, EBS);
  mov(ARG2, Src);
  a.mov(ARG3, ret);
  mov(ARG4, Flags);
  call((uint64_t)erts_new_bs_put_integer);
  a.cmp(RET, 0);
  a.jne(next);
  a.bind(fail);
  emit_badarg(entry, Fail);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_all(ArgVal Src, ArgVal Fail,
                                                       ArgVal Unit, Instruction *I) {
  Label next = a.newLabel(), entry = a.newLabel();
  a.bind(entry);
  a.mov(ARG1, EBS);
  mov(ARG2, Src);
  mov(ARG3, Unit);
  call((uint64_t)erts_new_bs_put_binary_all);
  a.cmp(RET, 0);
  a.jne(next);
  emit_badarg(entry, Fail);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_imm(ArgVal Fail, ArgVal Sz,
                                                       ArgVal Src, Instruction *I) {
  Label next = a.newLabel(), entry = a.newLabel();
  a.bind(entry);
  a.mov(ARG1, EBS);
  mov(ARG2, Src);
  mov(ARG3, Sz);
  call((uint64_t)erts_new_bs_put_binary);
  a.cmp(RET, 0);
  a.jne(next);
  emit_badarg(entry, Fail);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_new_bs_put_float(ArgVal Fail, ArgVal Sz, ArgVal Flags,
                                                  ArgVal Src, Instruction *I) {
  Label next = a.newLabel(), entry = a.newLabel(), fail = a.newLabel();
  a.bind(entry);
  // Clobbers TMP3
  x86::Gp ret = emit_bs_get_unchecked_field_size(Sz, Flags.getValue() >> 3, fail);
  ASSERT(ret == RET);
  a.mov(ARG1, c_p);
  mov(ARG2, Src);
  a.mov(ARG3, ret);
  mov(ARG4, Flags);
  call((uint64_t)erts_new_bs_put_float);
  a.cmp(RET, 0);
  a.jne(next);
  a.bind(fail);
  emit_badarg(entry, Fail);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_new_bs_put_float_imm(ArgVal Fail, ArgVal Sz, ArgVal Flags,
                                                      ArgVal Src, Instruction *I) {
  Label next = a.newLabel(), entry = a.newLabel();
  a.bind(entry);
  a.mov(ARG1, c_p);
  mov(ARG2, Src);
  mov(ARG3, Sz);
  mov(ARG4, Flags);
  call((uint64_t)erts_new_bs_put_float);
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
  emit_i_bs_start_match3(Src, Live, Fail, Dst);
  emit_i_bs_get_position(Dst, Pos);
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
  make_move_patch(ARG3, strings, Ptr.getValue());
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

static Eterm i_bs_get_integer_32(Process *c_p, Eterm context) {
  ErlBinMatchBuffer* _mb = ms_matchbuffer(context);
  Uint32 _result;

  if (_mb->size - _mb->offset < 32) {
    return THE_NON_VALUE;
  }

  if (BIT_OFFSET(_mb->offset) != 0) {
    _result = erts_bs_get_unaligned_uint32(_mb);
  } else {
    _result = get_int32(_mb->base + _mb->offset / 8);
  }

  _mb->offset += 32;
  return make_small(_result);
}

void BeamModuleAssembler::emit_i_bs_get_integer_32(ArgVal Ctx, ArgVal Fail, ArgVal Dst, Instruction *I) {
  a.mov(ARG1, c_p);
  mov(ARG2, Ctx);
  call((uint64_t)i_bs_get_integer_32);
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

static Eterm i_bs_get_integer(Process *c_p, Eterm *reg, Eterm context,
                              Uint flags, Uint size, Uint Live) {
  ErlBinMatchBuffer* mb;

  if (size >= SMALL_BITS) {
    Uint wordsneeded;
    /* Check bits size before potential gc.
     * We do not want a gc and then realize we don't need
     * the allocated space (i.e. if the op fails).
     *
     * Remember to re-acquire the matchbuffer after gc.
     */

    mb = ms_matchbuffer(context);
    if (mb->size - mb->offset < size) {
      return THE_NON_VALUE;
    }
    wordsneeded = 1+WSIZE(NBYTES((Uint) size));
    reg[Live] = context;
    GC_TEST(c_p, reg, 0, wordsneeded, Live+1);
    context = reg[Live];
  }
  mb = ms_matchbuffer(context);
  return erts_bs_get_integer_2(c_p, size, flags, mb);
}

void BeamModuleAssembler::emit_i_bs_get_integer(ArgVal Ctx, ArgVal Fail, ArgVal Live,
                                                ArgVal FlagsAndUnit, ArgVal Sz, ArgVal Dst,
                                                Instruction *I) {
  Label fail = a.newLabel(), next = a.newLabel();
  // Clobbers TMP3
  x86::Gp ret = emit_bs_get_field_size(Sz, FlagsAndUnit.getValue() >> 3, fail);
  ASSERT(ret == RET);
  emit_heavy_swapout();
  a.mov(ARG1, c_p);
  a.mov(ARG2, x_reg);
  mov(ARG3, Ctx);
  mov(ARG4, FlagsAndUnit);
  a.mov(ARG5, ret);
  mov(ARG6, Live);
  call((uint64_t)i_bs_get_integer);
  emit_heavy_swapin();
  a.cmp(RET, THE_NON_VALUE);
  a.jne(next);
  a.bind(fail);
  a.jmp(labels[Fail.getValue()]);
  a.bind(next);
  mov(Dst, RET);
}

// x64.bs_test_tail2(Fail, Ctx, Offset);
void BeamModuleAssembler::emit_bs_test_tail2(ArgVal Fail, ArgVal Ctx,
                                             ArgVal Offset, Instruction *I) {
  ASSERT(Offset.getType() == ArgVal::TYPE::u);

  mov(TMP1, Ctx);
  a.mov(TMP2, x86::qword_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb.size)));
  a.sub(TMP2, x86::qword_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb.offset)));

  if (Offset.getValue() != 0) {
    a.cmp(TMP2, Offset.getValue());
  }

  a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_bs_set_position(ArgVal Ctx, ArgVal Pos, Instruction *I) {
  mov(TMP1, Ctx);
  mov(TMP2, Pos);
  a.sar(TMP2, _TAG_IMMED1_SIZE);
  a.mov(x86::qword_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb.offset)), TMP2);
}

void BeamModuleAssembler::emit_i_bs_get_binary_all2(ArgVal Ctx, ArgVal Fail,
                                                    ArgVal Live, ArgVal Unit,
                                                    ArgVal Dst, Instruction *I) {
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

static Eterm bs_get_tail(ErlBinMatchBuffer* mb, ErlSubBin *sb) {
    Uint size, offs;

    offs = mb->offset;
    size = mb->size - offs;

    sb->thing_word = HEADER_SUB_BIN;
    sb->size = BYTE_OFFSET(size);
    sb->bitsize = BIT_OFFSET(size);
    sb->offs = BYTE_OFFSET(offs);
    sb->bitoffs = BIT_OFFSET(offs);
    sb->is_writable = 0;
    sb->orig = mb->orig;

    return make_binary(sb);
}

void BeamModuleAssembler::emit_bs_get_tail(ArgVal Ctx, ArgVal Dst, ArgVal Live, Instruction *I) {
  mov(ArgVal(ArgVal::x, Live.getValue()), Ctx);
  emit_gc_test(ArgVal(ArgVal::i,0), ArgVal(ArgVal::i,ERL_SUB_BIN_SIZE), Live + 1);
  mov(TMP1, ArgVal(ArgVal::x, Live.getValue()));

  a.lea(ARG1, x86::qword_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb)));
  a.mov(ARG2, HTOP);
  call((uint64_t)bs_get_tail);
  a.add(HTOP, ERL_SUB_BIN_SIZE * sizeof(Eterm));
  mov(Dst, RET);
}

/* Bits to skip are passed in RET */
void BeamModuleAssembler::emit_bs_skip_bits(ArgVal Fail, ArgVal Ctx) {
  mov(TMP1, Ctx);
  a.lea(TMP1, x86::qword_ptr(TMP1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb)));
  a.add(RET, x86::qword_ptr(TMP1, offsetof(ErlBinMatchBuffer, offset)));
  a.cmp(RET, x86::qword_ptr(TMP1, offsetof(ErlBinMatchBuffer, size)));
  a.jg(labels[Fail.getValue()]);
  a.mov(x86::qword_ptr(TMP1, offsetof(ErlBinMatchBuffer, offset)), RET);
}

//x64.i_bs_skip_bits2(Ctx, Bits, Fail, Unit);
void BeamModuleAssembler::emit_i_bs_skip_bits2(ArgVal Ctx, ArgVal Bits, ArgVal Fail, ArgVal Unit, Instruction *I) {
  x86::Gp ret = emit_bs_get_field_size(Bits, Unit.getValue(), labels[Fail.getValue()]);
  ASSERT(ret == RET);
  emit_bs_skip_bits(Fail, Ctx);
}

// x64.i_bs_skip_bits_imm2(Fail, Ms, Bits);
void BeamModuleAssembler::emit_i_bs_skip_bits_imm2(ArgVal Fail, ArgVal Ctx, ArgVal Bits, Instruction *I) {
  mov(RET, Bits);
  emit_bs_skip_bits(Fail, Ctx);
}

//x64.i_bs_get_binary2(Ctx, Fail, Live, Sz, Flags, Dst);
void BeamModuleAssembler::emit_i_bs_get_binary2(ArgVal Ctx, ArgVal Fail, ArgVal Live,
                                                ArgVal Size, ArgVal Flags, ArgVal Dst,
                                                Instruction *I) {
  Label fail = a.newLabel(), next = a.newLabel();


  mov(ArgVal(ArgVal::x, Live.getValue()), Ctx);
  emit_gc_test(ArgVal(ArgVal::i,0), ArgVal(ArgVal::i,EXTRACT_SUB_BIN_HEAP_NEED), Live + 1);
  mov(TMP4, ArgVal(ArgVal::x, Live.getValue()));

  // Clobbers RET + TMP3
  x86::Gp ret = emit_bs_get_field_size(Size, Flags.getValue() >> 3, fail);
  ASSERT(ret == RET);

  emit_swapout();
  a.mov(ARG1, c_p);
  a.mov(ARG2, ret);
  a.mov(ARG3, Flags.getValue());
  a.lea(ARG4, x86::qword_ptr(TMP4, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb)));
  call((uint64_t)erts_bs_get_binary_2);
  emit_swapin();

  a.cmp(RET, THE_NON_VALUE);
  a.jne(next);
  a.bind(fail);
  a.jmp(labels[Fail.getValue()]);
  a.bind(next);
  mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_utf8_size(ArgVal Src, ArgVal Dst, Instruction *I) {
  Label next = a.newLabel();
  mov(TMP1, Src);
  a.cmp(TMP1, make_small(0x80UL));
  a.mov(RET, make_small(1));
  a.jl(next);
  a.cmp(TMP1, make_small(0x800UL));
  a.mov(RET, make_small(2));
  a.jl(next);
  a.cmp(TMP1, make_small(0x10000UL));
  a.mov(RET, make_small(3));
  a.jl(next);
  a.mov(RET, make_small(4));
  a.bind(next);
  mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_put_utf8(ArgVal Fail, ArgVal Src, Instruction *I) {
  Label entry = a.newLabel(), next = a.newLabel();
  a.bind(entry);
  a.mov(ARG1, EBS);
  mov(ARG2, Src);
  call((uint64_t)erts_bs_put_utf8);
  a.cmp(RET, 0);
  a.jne(next);
  emit_badarg(entry, Fail);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_bs_get_utf8(ArgVal Ctx, ArgVal Fail, ArgVal Dst, Instruction *I) {
  mov(ARG1, Ctx);
  a.lea(ARG1, x86::qword_ptr(ARG1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb)));
  call((uint64_t)erts_bs_get_utf8);
  a.cmp(RET, THE_NON_VALUE);
  a.je(labels[Fail.getValue()]);
  mov(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_utf16_size(ArgVal Src, ArgVal Dst, Instruction *I) {
  Label next = a.newLabel();
  mov(TMP1, Src);
  a.mov(RET, make_small(2));
  a.cmp(TMP1, make_small(0x10000UL));
  a.jb(next);
  a.mov(RET, make_small(4));
  a.bind(next);
  mov(Dst, RET);
}

void BeamModuleAssembler::emit_bs_put_utf16(ArgVal Fail, ArgVal Flags, ArgVal Src, Instruction *I) {
  Label entry = a.newLabel(), next = a.newLabel();
  a.bind(entry);
  /* mov may clobber TMP1 */
  mov(ARG3, Flags);
  mov(ARG2, Src);
  a.mov(ARG1, EBS);
  call((uint64_t)erts_bs_put_utf16);
  a.cmp(RET, 0);
  a.jne(next);
  emit_badarg(entry, Fail);
  a.bind(next);
}

void BeamModuleAssembler::emit_i_bs_get_utf16(ArgVal Ctx, ArgVal Fail, ArgVal Flags, ArgVal Dst, Instruction *I) {
  mov(ARG1, Ctx);
  a.lea(ARG1, x86::qword_ptr(ARG1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb)));
  a.mov(ARG2, Flags.getValue());
  call((uint64_t)erts_bs_get_utf16);
  a.cmp(RET, THE_NON_VALUE);
  a.je(labels[Fail.getValue()]);
  mov(Dst, RET);
}

//x64.bs_test_unit(Fail, Ctx, Unit);
void BeamModuleAssembler::emit_bs_test_unit(ArgVal Fail, ArgVal Ctx, ArgVal Unit, Instruction *I) {
  unsigned int unit = Unit.getValue();
  mov(ARG1, Ctx);

  a.lea(ARG1, x86::qword_ptr(ARG1, -TAG_PRIMARY_BOXED + offsetof(ErlBinMatchState, mb)));
  a.mov(RET, x86::qword_ptr(ARG1, offsetof(ErlBinMatchBuffer, size)));
  a.sub(RET, x86::qword_ptr(ARG1, offsetof(ErlBinMatchBuffer, offset)));

  if ((unit & (unit - 1))) {
    a.mov(TMP1, unit);
    a.div(TMP1);
    a.test(x86::rdx, x86::rdx);
  } else {
    a.test(TMP1, unit - 1);
  }

  a.jnz(labels[Fail.getValue()]);
}

// x64.bs_add(Fail, Src1, Src2, Unit, Dst);
void BeamModuleAssembler::emit_bs_add(ArgVal Fail, ArgVal Src1, ArgVal Src2,
                                      ArgVal Unit, ArgVal Dst, Instruction *I) {
  Label entry = a.newLabel(), next = a.newLabel();
  a.bind(entry);

  mov(ARG1, Src1);
  mov(ARG2, Src2);

  /* Both arguments must be immediates on x64. */
  a.mov(TMP3, ARG1);
  a.and_(TMP3, _TAG_PRIMARY_MASK);
  a.and_(TMP3, ARG2);
  a.cmp(TMP3, TAG_PRIMARY_IMMED1);
  a.jne(labels[Fail.getValue()]);

  /* signed_val(ARG1) >= 0 && signed_val(ARG2) >= 0 */
  a.sar(ARG1, _TAG_IMMED1_SIZE);
  a.js(labels[Fail.getValue()]);
  a.sar(ARG2, _TAG_IMMED1_SIZE);
  a.js(labels[Fail.getValue()]);

  a.mov(RET, Unit.getValue());
  a.mul(ARG2); /* CLOBBERS RDX = ARG3! */
  if (Fail.getValue() != 0) {
    a.jo(labels[Fail.getValue()]);
  } else {
    a.jno(next);
    emit_system_limit(Fail, I);
    a.bind(next);
  }
  a.add(RET, ARG1);

  a.shl(RET, _TAG_IMMED1_SIZE);
  a.or_(RET, _TAG_IMMED1_SMALL);
  mov(Dst, RET);
}

// x64.i_bs_append(Fail, ExtraHeap, Live, Unit, Size, Dst);
void BeamModuleAssembler::emit_i_bs_append(ArgVal Fail, ArgVal ExtraHeap,
                                           ArgVal Live, ArgVal Unit,
                                           ArgVal Size, ArgVal Dst,
                                           Instruction *I) {
    Label entry = a.newLabel(), next = a.newLabel();
    a.bind(entry);

    emit_heavy_swapout();

    /* reg[live] = x(SCRATCH_X_REG); */
    mov(TMP1, ArgVal(ArgVal::TYPE::x, 1023));
    mov(ArgVal(ArgVal::TYPE::x, Live.getValue()), TMP1);

    a.mov(ARG2, x_reg);
    mov(ARG3, Live);
    mov(ARG4, Size);
    mov(ARG5, ExtraHeap);
    mov(ARG6, Unit);
    /* Must be last since mov() of immediates clobbers ARG1 */
    a.mov(ARG1, c_p);
    call((uint64_t)erts_bs_append);
    emit_heavy_swapin();

    a.cmp(RET, THE_NON_VALUE);
    a.jne(next);
    emit_fail_head_or_body(entry, Fail);
    a.bind(next);
    mov(Dst, RET);
}

// x64.i_bs_private_append(Fail, Unit, Size, Src, Dst);
void BeamModuleAssembler::emit_i_bs_private_append(ArgVal Fail, ArgVal Unit,
                                                   ArgVal Size, ArgVal Src,
                                                   ArgVal Dst, Instruction *I) {
    Label entry = a.newLabel(), next = a.newLabel();

    a.bind(entry);
    mov(ARG2, Src);
    mov(ARG3, Size);
    mov(ARG4, Unit);
    /* Must be last since mov() clobbers ARG1 */
    a.mov(ARG1, c_p);
    call((uint64_t)erts_bs_private_append);

    a.cmp(RET, THE_NON_VALUE);
    a.jne(next);
    emit_fail_head_or_body(entry, Fail);
    a.bind(next);
    mov(Dst, RET);
}


// x64.bs_init_writable();
void BeamModuleAssembler::emit_bs_init_writable(Instruction *I) {
    emit_heavy_swapout();
    a.mov(ARG1, c_p);
    mov(ARG2, x0);
    call((uint64_t)erts_bs_init_writable);
    mov(x0, RET);
    emit_heavy_swapin();
}
