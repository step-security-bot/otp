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

#include <asmjit/asmjit.h>
#include <assert.h>

#include "beam_asm.h"

using namespace asmjit;

void BeamAssembler::emit_swapin() {
    a.mov(E,x86::qword_ptr(c_p, offsetof(Process, stop)));
    a.mov(HTOP,x86::qword_ptr(c_p, offsetof(Process, htop)));
}

void BeamAssembler::emit_swapout() {
    a.mov(x86::qword_ptr(c_p, offsetof(Process, stop)), E);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, htop)), HTOP);
}

#define MAX_OPARGS 8

/*
 * Type for an operand for a generic instruction.
 */

typedef struct {
    unsigned type;		/* Type of operand. */
    BeamInstr val;		/* Value of operand. */
} GenOpArg;

/*
 * A generic operation.
 */

typedef struct genop {
    unsigned int op;		/* Opcode. */
    int arity;			/* Number of arguments. */
    GenOpArg def_args[MAX_OPARGS]; /* Default buffer for arguments. */
    GenOpArg* a;		/* The arguments. */
    struct genop* next;		/* Next genop. */
} GenOp;

static JitRuntime *rt;
static BeamGlobalAssembler *bga;
extern "C" uint64_t beamasm_call;

extern "C" void beamasm_init() {
    rt = new JitRuntime();
    bga = new BeamGlobalAssembler(rt);
    beamasm_call = bga->getSwapin();
}

extern "C" void *beamasm_new_module(Eterm mod, int num_labels) {
    return new BeamModuleAssembler(rt, bga, mod, num_labels);
}

extern "C" int beamasm_emit(void *instance, unsigned specific_op, GenOp *op, BeamInstr *I) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    std::vector<ArgVal> args;

    for (int i = 0; i < op->arity; i++) {
        args.push_back(ArgVal(op->a[i].type,op->a[i].val));
    }
    
    return ba->emit(specific_op, args, I);
}

extern "C" void *beamasm_get_module(void *instance, void **labels, unsigned *catch_no,
                                    Literal *literals, int num_literals,
                                    BeamInstr *imports, int num_imports) {
    std::vector<Literal> lits(literals, literals+num_literals);
    std::vector<BeamInstr> imps(imports, imports+num_imports);
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    void *module = ba->codegen(lits, imps, catch_no, labels);
    delete ba;
    return module;
}

extern "C" void beamasm_delete_module(void *instance) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    delete ba;
}
