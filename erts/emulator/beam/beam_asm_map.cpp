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
  #include "erl_map.h"

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

    emit_heavy_swapin();
    mov(Dst, RET);
}

// x64.i_new_small_map_lit(Dst, Live, Keys);
void BeamModuleAssembler::emit_i_new_small_map_lit(ArgVal Dst, ArgVal Live, ArgVal Keys, Instruction *Inst) {
    Label data = embed_instr_rodata(Inst, 3, (Inst->args.size() - 3));

    emit_heavy_swapout();

    ASSERT(Keys.isLiteral());
    mov(ARG3, Keys); /* Might clobber ARG1 */
    a.mov(ARG1, c_p);
    a.mov(ARG2, x_reg);
    a.mov(ARG4, Live.getValue());
    a.lea(ARG5, x86::qword_ptr(data));
    call((uint64_t)erts_gc_new_small_map_lit);

    emit_heavy_swapin();
    mov(Dst, RET);
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


#define PUT_TERM_REG(term, desc)		\
do {						\
    switch (loader_tag(desc)) {			\
    case LOADER_X_REG:				\
	reg[loader_x_reg_index(desc)] = (term);	\
	break;					\
    case LOADER_Y_REG:				\
	E[loader_y_reg_index(desc)] = (term);	\
	break;					\
    default:					\
	ASSERT(0);				\
	break;					\
    }						\
} while(0)


static Uint i_get_map_elements(Eterm map, Eterm *reg, Eterm *E, Uint n, BeamInstr *fs) {
    Uint sz;

    /* This instruction assumes Arg1 is a map,
     * i.e. that it follows a test is_map if needed.
     */

    if (is_flatmap(map)) {
	flatmap_t *mp;
	Eterm *ks;
	Eterm *vs;

	mp = (flatmap_t *)flatmap_val(map);
	sz = flatmap_get_size(mp);

	if (sz == 0) {
        return 0;
	}

	ks = flatmap_get_keys(mp);
	vs = flatmap_get_values(mp);

	while(sz) {
	    if (EQ((Eterm) fs[0], *ks)) {
		PUT_TERM_REG(*vs, fs[1]);
		n--;
		fs += 3;
		/* no more values to fetch, we are done */
		if (n == 0) {
                    return 1;
		}
	    }
	    ks++, sz--, vs++;
	}
        return 0;
    } else {
	const Eterm *v;
	Uint32 hx;
	ASSERT(is_hashmap(map));
	while(n--) {
	    hx = fs[2];
	    ASSERT(hx == hashmap_make_hash((Eterm)fs[0]));
	    if ((v = erts_hashmap_get(hx, (Eterm)fs[0], map)) == NULL) {
		    return 0;
	    }
	    PUT_TERM_REG(*v, fs[1]);
	    fs += 3;
	}
        return 1;
    }
}

#undef PUT_TERM_REG

// x64.i_get_map_elements(Fail, Src, Key, Dst);
void BeamModuleAssembler::emit_i_get_map_elements(ArgVal Fail, ArgVal Src, ArgVal N, Instruction *Inst) {
    Label data = embed_instr_rodata(Inst, 3, N.getValue());

    mov(ARG1, Src);
    a.mov(ARG2, x_reg);
    a.mov(ARG3, E);
    a.mov(ARG4, (N.getValue() / 3));
    a.lea(ARG5, x86::qword_ptr(data));
    call((uint64_t)i_get_map_elements);
    a.test(RET,RET);
    a.je(labels[Fail.getValue()]);
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

    emit_heavy_swapin();
    mov(Dst, RET);
}

// x64.update_map_exact(Src, Fail, Dst, Live, N);
void BeamModuleAssembler::emit_update_map_exact(ArgVal Src, ArgVal Fail, ArgVal Dst, ArgVal Live, ArgVal N, Instruction *Inst) {
    /* We _KNOW_ Src is a map */
    Label data, entry, error, next;

    data = embed_instr_rodata(Inst, 5, N.getValue());

    entry = a.newLabel();
    error = a.newLabel();
    next = a.newLabel();

    a.bind(entry);

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

    emit_fail_head_or_body(entry, Fail);

    a.bind(next);
    mov(Dst, RET);
}
