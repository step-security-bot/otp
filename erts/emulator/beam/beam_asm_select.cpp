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
#include <algorithm>
#include "beam_asm.hpp"


using namespace asmjit;

void BeamModuleAssembler::emit_linear_search(x86::Gp val, Label fail,
                                             int offset, int count,
                                             const std::vector<ArgVal> &args) {
  for (int i = 0; i < count; i++) {
    ArgVal value = args[offset + i];
    ArgVal label = args[offset + i + count];
    cmp(val, value.getValue());
    a.je(labels[label.getValue()]);
  }
  a.jmp(fail);
}

void BeamModuleAssembler::emit_i_select_tuple_arity(ArgVal Src, ArgVal Fail, ArgVal N, Instruction* I) {
  mov(TMP1, Src);
  emit_is_boxed(labels[Fail.getValue()], TMP1);
  a.mov(TMP1, emit_boxed_val(TMP1));
  a.mov(TMP2, TMP1);
  a.and_(TMP1, _TAG_HEADER_MASK);
  a.cmp(TMP1, _TAG_HEADER_ARITYVAL);
  a.jne(labels[Fail.getValue()]);

  emit_linear_search(TMP2, labels[Fail.getValue()], 3, N.getValue(), I->args);
}

void BeamModuleAssembler::emit_i_select_val_lins(ArgVal Src, ArgVal Fail, ArgVal N, Instruction* I) {
  mov(TMP2, Src);
  emit_linear_search(TMP2, labels[Fail.getValue()], 3, N.getValue(), I->args);
}

static const BeamInstr select_val_bins(const Eterm select_val,
                                       const uint64_t count,
                                       const BeamInstr fail,
                                       const BeamInstr *data) {
    Eterm *low;
    Eterm *high;
    Eterm *mid;
    Sint bdiff;

    low = (Eterm*)data;
    high = low + count;

    while ((bdiff = (int)((char*)high - (char*)low)) > 0) {
        unsigned int boffset;
        Eterm mid_val;

        boffset = ((unsigned int)bdiff >> 1) & ~(sizeof(Eterm) - 1);
        mid = (Eterm*)((char*)low + boffset);
        mid_val = *mid;

        if (select_val < mid_val) {
            high = mid;
        } else if (select_val > mid_val) {
            low = mid + 1;
        } else {
            const BeamInstr *jump_tab = &data[count];
            return jump_tab[mid - data];
        }
    }

    return fail;
}

void BeamModuleAssembler::emit_i_select_val_bins(ArgVal Src, ArgVal Fail, ArgVal N, Instruction* I) {
  /* FIXME: Emit this as a jump tree if it's relatively small. */
  Label data = embed_instr_rodata(I, 3, N.getValue() * 2);

  mov(ARG1, Src);
  a.mov(ARG2, N.getValue());
  a.lea(ARG3, x86::qword_ptr(labels[Fail.getValue()]));
  a.lea(ARG4, x86::qword_ptr(data));
  call((uint64_t)select_val_bins);
  a.jmp(RET);
}

void BeamModuleAssembler::emit_i_jump_on_val(ArgVal Src, ArgVal Fail, ArgVal N, ArgVal Base, Instruction* I) {
  Label data = embed_instr_rodata(I, 4, N.getValue());

  mov(TMP1, Src);
  a.mov(TMP2, TMP1);
  a.and_(TMP2, _TAG_IMMED1_MASK);
  a.cmp(TMP2, _TAG_IMMED1_SMALL);
  a.jne(labels[Fail.getValue()]);
  a.sar(TMP1, _TAG_IMMED1_SIZE);

  if (Base.getValue() != 0) {
    a.mov(TMP2, Base.getValue());
    a.sub(TMP1, TMP2);
  }

  a.cmp(TMP1, N.getValue());
  a.jae(labels[Fail.getValue()]);

  a.lea(RET, x86::qword_ptr(data));
  a.jmp(x86::qword_ptr(RET, TMP1, 3));
}
