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

#include "beam_asm.hpp"

using namespace asmjit;

void BeamAssembler::emit_light_swapin() {
  a.mov(HTOP,x86::qword_ptr(c_p, offsetof(Process, htop)));
}

void BeamAssembler::emit_light_swapout() {
  a.mov(x86::qword_ptr(c_p, offsetof(Process, htop)), HTOP);
}

void BeamAssembler::emit_swapin() {
  a.mov(E,x86::qword_ptr(c_p, offsetof(Process, stop)));
  a.mov(HTOP,x86::qword_ptr(c_p, offsetof(Process, htop)));
}

void BeamAssembler::emit_swapout() {
  a.mov(x86::qword_ptr(c_p, offsetof(Process, stop)), E);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, htop)), HTOP);
}

void BeamAssembler::emit_heavy_swapin() {
  emit_swapin();
  a.mov(FCALLS,x86::qword_ptr(c_p, offsetof(Process, fcalls)));
}

void BeamAssembler::emit_heavy_swapout() {
  emit_swapout();
  a.mov(x86::qword_ptr(c_p, offsetof(Process, fcalls)), FCALLS);
}


static JitRuntime *rt = nullptr;
static BeamGlobalAssembler *bga;
static BeamModuleAssembler *apply, *bma_exit, *continue_exit, *call_error_handler, *call_bif;

typedef enum beamasm_ret (*BAFUN)(BeamAsmContext *ctx,Process *c_p, Eterm *reg, FloatDef *fref, ERL_BITS_DECLARE_STATEP, Sint FCALLS);
static BAFUN beamasm_call_fun;

extern "C" {
    
  extern BeamInstr beam_apply[2];
  extern BeamInstr beam_exit[1];
  extern BeamInstr beam_continue_exit[1];

  void beamasm_init() {
        
    if (rt) return;

    rt = new JitRuntime();
    bga = new BeamGlobalAssembler(rt);
    beamasm_call_fun = (BAFUN)bga->get_call();

    apply = new BeamModuleAssembler(rt, bga, am_erts_internal, 3);
    apply->setDebug(false);
    apply->emit(op_label_L, {ArgVal(ArgVal::i, 1)});
    apply->emit(op_i_func_info_IaaI, {ArgVal(ArgVal::i, 2),ArgVal(ArgVal::i, am_erts_internal),ArgVal(ArgVal::i, am_apply), ArgVal(ArgVal::i, 3)});
    apply->emit(op_label_L, {ArgVal(ArgVal::i, 2)});
    apply->emit(op_i_apply, {});
    apply->emit(op_label_L, {ArgVal(ArgVal::i, 3)});
    apply->emit(op_normal_exit, {});
    apply->emit(op_int_code_end, {});
    apply->codegen();
    beam_apply[0] = apply->getCode(2);
    beam_apply[1] = apply->getCode(3);

    bma_exit = new BeamModuleAssembler(rt, bga, am_erts_internal, 2);
    bma_exit->setDebug(false);
    bma_exit->emit(op_label_L, {ArgVal(ArgVal::i, 1)});
    bma_exit->emit(op_i_func_info_IaaI, {ArgVal(ArgVal::i, 2),ArgVal(ArgVal::i, am_erts_internal),ArgVal(ArgVal::i, am_exit), ArgVal(ArgVal::i, 3)});
    bma_exit->emit(op_label_L, {ArgVal(ArgVal::i, 2)});
    bma_exit->emit(op_error_action_code, {});
    bma_exit->emit(op_int_code_end, {});
    bma_exit->codegen();
    beam_exit[0] = bma_exit->getCode(2);

    continue_exit = new BeamModuleAssembler(rt, bga, am_erts_internal, 2);
    continue_exit->setDebug(false);
    continue_exit->emit(op_label_L, {ArgVal(ArgVal::i, 1)});
    continue_exit->emit(op_i_func_info_IaaI, {ArgVal(ArgVal::i, 2),ArgVal(ArgVal::i, am_erts_internal),ArgVal(ArgVal::i, am_continue_exit), ArgVal(ArgVal::i, 3)});
    continue_exit->emit(op_label_L, {ArgVal(ArgVal::i, 2)});
    continue_exit->emit(op_continue_exit, {});
    continue_exit->emit(op_int_code_end, {});
    continue_exit->codegen();
    beam_continue_exit[0] = continue_exit->getCode(2);

    call_error_handler = new BeamModuleAssembler(rt, bga, am_erts_internal, 1);
    call_error_handler->setDebug(false);
    call_error_handler->emit(op_label_L, {ArgVal(ArgVal::i, 1)});
    call_error_handler->emit(op_call_error_handler, {});
    call_error_handler->codegen();

  }

  enum beamasm_ret
  beamasm_call(BeamAsmContext *ctx,Process *c_p, Eterm *reg, FloatDef *freg, ERL_BITS_DECLARE_STATEP,  Sint FCALLS) {
    return beamasm_call_fun(ctx, c_p, reg, freg, EBS, FCALLS);
  }

  void *beamasm_new_module(Eterm mod, int num_labels, int num_functions) {
    return new BeamModuleAssembler(rt, bga, mod, num_labels, num_functions);
  }

  int beamasm_emit(void *instance, unsigned specific_op, GenOp *op, BeamInstr *I) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    std::vector<ArgVal> args;

    for (int i = 0; i < op->arity; i++) {
      args.push_back(ArgVal(op->a[i].type,op->a[i].val));
    }

    return ba->emit(specific_op, args, I);
  }

  void
  beamasm_emit_patch(Eterm module, unsigned specific_op, GenOp *op,
                     char *buff, unsigned buff_len, int debug) {
    beamasm_init();
    if (specific_op == op_call_error_handler) {
      ERTS_ASSERT(call_error_handler->getCodeSize() <= buff_len);
      memcpy(buff, (BeamInstr*)call_error_handler->getCode(1), call_error_handler->getCodeSize());
    } else {
      BeamModuleAssembler ba(rt, bga, am_erts_internal, 1);
      ba.setDebug(debug);
      ba.emit(op_label_L, {ArgVal(ArgVal::i, 1)});
      beamasm_emit(&ba, specific_op, op, nullptr);
      ba.codegen();
      ERTS_ASSERT(ba.getCodeSize() <= buff_len);
      memcpy(buff, (BeamInstr*)ba.getCode(1), ba.getCodeSize());
    }
  }

  void beamasm_delete_module(void *instance) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    delete ba;
  }

  BeamInstr beamasm_get_code(void *instance, int label) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    return ba->getCode(label);
  }

  byte *beamasm_get_rodata(void *instance, char* label) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    return ba->getCode(label);
  }

  void beamasm_embed_rodata(void *instance, char *labelName, char* buff, size_t size) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    if (size)
      ba->embed_rodata(labelName, buff, size);
  }

  void beamasm_codegen(void *instance) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    ba->codegen();
  }

  Uint beamasm_get_header(void *instance, BeamCodeHeader **hdr) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    ba->getCodeHeader(hdr);
    return ba->getCodeSize();
  }
  unsigned int beamasm_get_catches(void *instance) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    return ba->patchCatches();
  }
  void beamasm_patch_import(void *instance, int index, BeamInstr import) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    ba->patchImport(index, import);
  }
  void beamasm_patch_literal(void *instance, int index, Eterm lit) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    ba->patchLiteral(index, lit);
  }
  void beamasm_patch_strings(void *instance, byte *strtab) {
    BeamModuleAssembler *ba = static_cast<BeamModuleAssembler*>(instance);
    ba->patchStrings(strtab);
  }

}
