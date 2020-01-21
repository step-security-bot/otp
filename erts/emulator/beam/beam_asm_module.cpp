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

#include "beam_asm.h"

using namespace asmjit;

std::string getAtom(Eterm atom) {
    // Ugly atom hack to get around that we cannot do global inline in cpp files at the moment...
    Atom *ap = (Atom *) erts_atom_table.seg_table[atom_val(atom)>>INDEX_PAGE_SHIFT][atom_val(atom)&INDEX_PAGE_MASK];
    return std::string((const char*)ap->name, ap->len);
}

BeamModuleAssembler::BeamModuleAssembler(JitRuntime *rt, BeamGlobalAssembler *ga, Eterm mod, unsigned num_labels) : BeamAssembler(rt, getAtom(mod) + ".asm") {

    this->ga = ga;

    for (unsigned i = 0; i < num_labels; i++) {
        std::string lblName = "label_" + std::to_string(i);
        labels[i] = a.newNamedLabel(lblName.data());
    }

    emit_function_preamble();
    for (unsigned i = 0; i < num_labels-1; i++) {
        a.lea(TMP2, x86::qword_ptr(labels[i+1]));
        a.mov(x86::qword_ptr(ARG1, i * sizeof(BeamInstr)), TMP2);
    }
    emit_function_postamble();

    emit_postamble();
}

bool BeamModuleAssembler::emit(unsigned specific_op, std::vector<ArgVal> args, BeamInstr *I) {
    Instruction i = {specific_op, args, I};
    if (specific_op == op_i_func_info_IaaI) {
        currFunction = args;
        prev_op = specific_op;
    } else if (specific_op == op_label_L && prev_op == op_i_func_info_IaaI) {
        ErtsCodeMFA mfa = { currFunction[1].getValue(),
                            currFunction[2].getValue(),
                            currFunction[3].getValue() };
        labelToMFA[args[0].getValue()] = mfa;
        prev_op = specific_op;
    }

    instrs.push_back(i);
    return true;
}

int BeamModuleAssembler::codegen(std::vector<Literal> literals, std::vector<BeamInstr> imports) {

    for (Instruction &inst : instrs) {
        this->inst = &inst;
        for (ArgVal &arg : inst.args) {
            if (arg.getType() == ArgVal::q) {
                arg.resolveLiteral(literals);
            }
            if (arg.getType() == ArgVal::e) {
                arg.resolveImport(imports);
            }
        }
        comment(opc[inst.op].name);
        emit_dbg(opc[inst.op].name);
        // #define InstrCnt() a.mov(TMP1, imm((uint64_t)&instrcnt)); a.mov(TMP2, x86::qword_ptr(TMP1)); a.inc(TMP2); a.mov(x86::qword_ptr(TMP1), TMP2);
        #define InstrCnt()
        switch (inst.op) {
        #include "beamasm_emit.h"
        case op_i_func_info_IaaI:
            emit_preamble(inst.args[3]);
            comment("%T:%T/%d", inst.args[1].getValue(), inst.args[2].getValue(), inst.args[3].getValue());
            break;
        case op_label_L:
            a.bind(labels[inst.args[0].getValue()]);
            break;
        case op_int_code_end:
        case op_line_I:
            break;
        default:
            fprintf(stderr,"NYI: %s\r\n", opc[inst.op].name);
            return 0;
        }
        
    }
    return 1;

}

void *BeamModuleAssembler::codegen(std::vector<Literal> literals, std::vector<BeamInstr> imports,
        unsigned *catch_no, void **labels) {

    this->catch_no = *catch_no;

    if (!codegen(literals, imports))
        return nullptr;
    
    typedef void *(*Func)(void **);
    Func module;
    Error err = rt->add(&module,&code);
    ERTS_ASSERT(!err && "Failed to create module");
    for (std::pair<Label, BeamInstr**> p : catches) {
        *p.second = (BeamInstr*)(((char*)module) + code.labelOffset(p.first));
    }
    module(labels);
    *catch_no = this->catch_no;
    return (void*)module;
}