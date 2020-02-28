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

void BeamModuleAssembler::emit_i_select_tuple_arity2(ArgVal Src, ArgVal Fail, ArgVal T1, ArgVal T2, Instruction* I) {
  mov(TMP1, Src);
  emit_is_boxed(labels[Fail.getValue()], TMP1);
  a.mov(TMP1, emit_boxed_val(TMP1));
  a.mov(TMP2, TMP1);
  a.and_(TMP1, _TAG_HEADER_MASK);
  a.cmp(TMP1, _TAG_HEADER_ARITYVAL);
  a.jne(labels[Fail.getValue()]);
  cmp(TMP2, T1.getValue());
  a.je(labels[I->args[4].getValue()]);
  cmp(TMP2, T2.getValue());
  a.je(labels[I->args[5].getValue()]);
  a.jmp(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_i_select_tuple_arity(ArgVal Src, ArgVal Fail, ArgVal N, Instruction* I) {
  mov(TMP1, Src);
  emit_is_boxed(labels[Fail.getValue()], TMP1);
  a.mov(TMP1, emit_boxed_val(TMP1));
  a.mov(TMP2, TMP1);
  a.and_(TMP1, _TAG_HEADER_MASK);
  a.cmp(TMP1, _TAG_HEADER_ARITYVAL);
  a.jne(labels[Fail.getValue()]);
  for (unsigned i = 0; i < N.getValue(); i++) {
    ArgVal value = I->args[3 + i];
    ArgVal label = I->args[3 + N.getValue() + i];
    cmp(TMP2, value.getValue());
    a.je(labels[label.getValue()]);
  }
  a.jmp(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_i_select_val2(ArgVal Src, ArgVal Fail, ArgVal T1, ArgVal T2, Instruction* I) {
  cmp(getRef(Src), T1.getValue());
  a.je(labels[I->args[4].getValue()]);
  cmp(getRef(Src), T2.getValue());
  a.je(labels[I->args[5].getValue()]);
  a.jmp(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_i_select_val_lins(ArgVal Src, ArgVal Fail, ArgVal N, Instruction* I) {
  mov(TMP2, Src);
  for (unsigned i = 0; i < N.getValue(); i++) {
    ArgVal value = I->args[3 + i];
    ArgVal label = I->args[3 + N.getValue() + i];
    cmp(TMP2, value.getValue());
    a.je(labels[label.getValue()]);
  }
  a.jmp(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_i_jump_on_val_zero(ArgVal Src, ArgVal Fail, ArgVal N, Instruction* I) {
  Label fail = a.newLabel();
  mov(TMP1, Src);
  a.mov(TMP2, TMP1);
  a.and_(TMP2, _TAG_IMMED1_MASK);
  a.cmp(TMP2, _TAG_IMMED1_SMALL);
  a.jne(fail);
  a.sar(TMP1, _TAG_IMMED1_SIZE);
  a.cmp(TMP1, N.getValue());
  a.jge(fail);
  for (unsigned i = 0; i < N.getValue(); i++) {
    a.cmp(TMP1, i);
    a.je(labels[inst.args[i+3].getValue()]);
  }
  a.bind(fail);
  if (Fail.getValue()) {
    a.jmp(labels[Fail.getValue()]);
  } else {
    emit_nyi();
  }
}

void BeamModuleAssembler::emit_i_jump_on_val(ArgVal Src, ArgVal Fail, ArgVal N, ArgVal Base, Instruction* I) {
  Label fail = a.newLabel();
  mov(TMP1, Src);
  a.mov(TMP2, TMP1);
  a.and_(TMP2, _TAG_IMMED1_MASK);
  a.cmp(TMP2, _TAG_IMMED1_SMALL);
  a.jne(fail);
  a.sar(TMP1, _TAG_IMMED1_SIZE);
  a.sub(TMP1, Base.getValue());
  a.cmp(TMP1, N.getValue());
  a.jge(fail);
  for (unsigned i = 0; i < N.getValue(); i++) {
    a.cmp(TMP1, i);
    a.je(labels[inst.args[i+4].getValue()]);
  }
  a.bind(fail);
  if (Fail.getValue()) {
    a.jmp(labels[Fail.getValue()]);
  } else {
    emit_nyi();
  }
}
