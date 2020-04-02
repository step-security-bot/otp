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

extern "C" {
  Eterm erts_gc_update_map_assoc(Process* p, Eterm* reg, Uint live,
                                 Uint n, BeamInstr* new_p) ERTS_NOINLINE;
  Eterm erts_gc_update_map_exact(Process* p, Eterm* reg, Uint live,
                                 Uint n, Eterm* new_p) ERTS_NOINLINE;
  Eterm get_map_element(Eterm map, Eterm key);
  Eterm get_map_element_hash(Eterm map, Eterm key, Uint32 hx);
  Eterm erts_gc_new_map(Process* p, Eterm* reg, Uint live,
                        Uint n, BeamInstr* ptr) ERTS_NOINLINE;
  Eterm erts_gc_new_small_map_lit(Process* p, Eterm* reg, Eterm keys_literal,
                        Uint live, BeamInstr* ptr) ERTS_NOINLINE;
}

// x64.new_map(Dst, Live, N);
void BeamModuleAssembler::emit_new_map(ArgVal Dst, ArgVal Live, ArgVal N, Instruction *Inst) {
    Label data = embed_instr_rodata(Inst, 3, N.getValue());

    emit_heavy_swapout();

    a.mov(ARG1, c_p);
    a.mov(ARG2, x_reg);
    a.mov(ARG3, Live.getValue());
    a.mov(ARG4, N.getValue());
    a.lea(ARG5, x86::qword_ptr(data));
    call((uint64_t)erts_gc_new_map);
    mov(Dst, RET);

    emit_heavy_swapin();
}

// x64.i_new_small_map_lit(Dst, Live, N);
void BeamModuleAssembler::emit_i_new_small_map_lit(ArgVal Dst, ArgVal Live, ArgVal Keys, Instruction *Inst) {
    Eterm literal_keys;
    Uint map_size;
    Label data;

    literal_keys = Keys.getValue();
    map_size = arityval(*tuple_val(literal_keys));

    data = embed_instr_rodata(Inst, 3, map_size);

    emit_heavy_swapout();

    a.mov(ARG1, c_p);
    a.mov(ARG2, x_reg);
    mov(ARG3, Keys);
    a.mov(ARG4, Live.getValue());
    a.lea(ARG5, x86::qword_ptr(data));
    call((uint64_t)erts_gc_new_small_map_lit);
    mov(Dst, RET);

    emit_heavy_swapin();
}

// x64.i_get_map_element(Fail, Src, Key, Dst);
void BeamModuleAssembler::emit_i_get_map_element(ArgVal Fail, ArgVal Src, ArgVal Key, ArgVal Dst, Instruction *Inst) {
    mov(ARG1, Src);
    mov(ARG2, Key);
    call((uint64_t)get_map_element);
    a.cmp(RET, THE_NON_VALUE);
    a.je(labels[Fail.getValue()]);
    mov(Dst, RET);
}

// x64.i_get_map_element_hash(Fail, Src, Key, Hx, Dst);
void BeamModuleAssembler::emit_i_get_map_element_hash(ArgVal Fail, ArgVal Src, ArgVal Key, ArgVal Hx, ArgVal Dst, Instruction *Inst) {
    mov(ARG1, Src);
    mov(ARG2, Key);
    mov(ARG3, Hx);
    call((uint64_t)get_map_element_hash);
    a.cmp(RET, THE_NON_VALUE);
    a.je(labels[Fail.getValue()]);
    mov(Dst, RET);
}

// x64.update_map_assoc(Src, Dst, Live, N);
void BeamModuleAssembler::emit_update_map_assoc(ArgVal Src, ArgVal Dst, ArgVal Live, ArgVal N, Instruction *Inst) {
    Label data = embed_instr_rodata(Inst, 4, N.getValue());

    emit_heavy_swapout();
    mov(x86::qword_ptr(x_reg, Live.getValue() * sizeof(Eterm)), Src);

    a.mov(ARG1, c_p);
    a.mov(ARG2, x_reg);
    a.mov(ARG3, Live.getValue());
    a.mov(ARG4, N.getValue());
    a.lea(ARG5, x86::qword_ptr(data));
    call((uint64_t)erts_gc_update_map_assoc);
    mov(Dst, RET);

    emit_heavy_swapin();
}

// x64.update_map_exact(Src, Fail, Dst, Live, N);
void BeamModuleAssembler::emit_update_map_exact(ArgVal Src, ArgVal Fail, ArgVal Dst, ArgVal Live, ArgVal N, Instruction *Inst) {
    /* We _KNOW_ Src is a map */
    Label data, error, next;

    data = embed_instr_rodata(Inst, 5, N.getValue());

    error = a.newLabel();
    next = a.newLabel();

    emit_heavy_swapout();
    mov(x86::qword_ptr(x_reg, Live.getValue() * sizeof(Eterm)), Src);

    a.mov(ARG1, c_p);
    a.mov(ARG2, x_reg);
    a.mov(ARG3, Live.getValue());
    a.mov(ARG4, N.getValue());
    a.lea(ARG5, x86::qword_ptr(data));
    call((uint64_t)erts_gc_update_map_exact);

    emit_heavy_swapin();
    a.cmp(RET, THE_NON_VALUE);
    a.jne(next);

    comment("HANDLE_UPDATE_MAP_EXACT_ERROR");
    emit_nyi();

    a.bind(next);
    mov(Dst, RET);
}
