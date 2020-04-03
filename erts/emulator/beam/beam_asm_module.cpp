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

#include "beam_asm.hpp"

using namespace asmjit;

std::string getAtom(Eterm atom) {
  // Ugly atom hack to get around that we cannot do global inline in cpp files at the moment...
  Atom *ap = (Atom *) erts_atom_table.seg_table[atom_val(atom)>>INDEX_PAGE_SHIFT][atom_val(atom)&INDEX_PAGE_MASK];
  return std::string((const char*)ap->name, ap->len);
}

BeamModuleAssembler::BeamModuleAssembler(JitRuntime *rt, BeamGlobalAssembler *ga, Eterm mod, unsigned num_labels) : BeamAssembler(rt, getAtom(mod) + ".asm") {

  this->ga = ga;
  this->mod = mod;

  for (unsigned i = 0; i < num_labels + 1; i++) {
    std::string lblName = "label_" + std::to_string(i);
    labels[i] = a.newNamedLabel(lblName.data());
  }

}

BeamModuleAssembler::BeamModuleAssembler(JitRuntime *rt, BeamGlobalAssembler *ga,
                                         Eterm mod, unsigned num_labels,
                                         unsigned num_functions) :
  BeamModuleAssembler(rt, ga, mod, num_labels) {
  void *hdr_mem = erts_alloc(ERTS_ALC_T_TMP, sizeof(BeamCodeHeader) +
                             sizeof(ErtsCodeInfo*) * (num_functions - 1));
  codeHeader = a.newLabel();
  a.align(kAlignCode, 8);
  a.bind(codeHeader);
  a.embed(hdr_mem, sizeof(BeamCodeHeader) + sizeof(ErtsCodeInfo*) * (num_functions - 1));

  /* FIXME: Isn't hdr_mem supposed to be populated?
   *
   * Do we do this later? Is this just a reservation? */
  erts_free(ERTS_ALC_T_TMP, hdr_mem);
}

void *BeamModuleAssembler::getCode(Label label) {
  ASSERT(module);
  return module + code.labelOffsetFromBase(label);
}

BeamInstr BeamModuleAssembler::getCode(unsigned label) {
  return (BeamInstr)getCode(labels[label]);
}

byte *BeamModuleAssembler::getCode(char *labelName) {
  return (byte*)getCode(code.labelByName(labelName,strlen(labelName)));
}

void BeamModuleAssembler::embed_rodata(char *labelName, char *buff, size_t size) {
  Label label = a.newNamedLabel(labelName);
  if (!rodata) {
    Error err = code.newSection(
      &rodata,
      ".rodata",           // Section name
      SIZE_MAX,            // Name length if the name is not null terminated (or SIZE_MAX).
      Section::kFlagConst, // Section flags, see Section::Flags.
      8);                  // Section alignment, must be power of 2.
  }
  a.section(rodata);
  a.bind(label);
  a.embed(buff, size);
  a.section(code.textSection());
}

Label BeamModuleAssembler::embed_instr_rodata(Instruction *instr, int index, int count) {
  Label label = a.newLabel();

  if (!rodata) {
    Error err = code.newSection(
      &rodata,
      ".rodata",           // Section name
      SIZE_MAX,            // Name length if the name is not null terminated (or SIZE_MAX).
      Section::kFlagConst, // Section flags, see Section::Flags.
      8);                  // Section alignment, must be power of 2.
  }

  a.section(rodata);
  a.bind(label);

  for(int i = index; i < (index + count); i++) {
      ArgVal &arg = instr->args[i];

      union {
          BeamInstr as_beam;
          char as_char[1];
      } data;

      switch (arg.getType()) {
          case TAG_x:
            data.as_beam = make_loader_x_reg(arg.getValue());
            a.embed(&data.as_char, sizeof(data.as_beam));
            break;
          case TAG_y:
            data.as_beam = make_loader_y_reg(arg.getValue());
            a.embed(&data.as_char, sizeof(data.as_beam));
            break;
          case TAG_a:
            data.as_beam = make_atom(arg.getValue());
            a.embed(&data.as_char, sizeof(data.as_beam));
            break;
          case TAG_i:
            data.as_beam = make_small(arg.getValue());
            a.embed(&data.as_char, sizeof(data.as_beam));
            break;
          case TAG_q:
            make_word_patch(literals[arg.getValue()].patches);
            break;
          default:
            ERTS_ASSERT(!"error");
      }
  }

  a.section(code.textSection());

  return label;
}

bool BeamModuleAssembler::emit(unsigned specific_op, std::vector<ArgVal> args, BeamInstr *I) {
  Instruction i = {specific_op, args, I};
  if (specific_op == op_i_func_info_IaaI) {
    prev_op = specific_op;
  } if (specific_op == op_label_L && prev_op == op_i_func_info_IaaI) {
    functions.push_back(args[0].getValue() - 1);
    prev_op = specific_op;
  }

  inst = i;

  comment(opc[inst.op].name);
  switch (inst.op) {
  case op_i_func_info_IaaI:
  case op_label_L:
  case op_int_code_end:
  case op_line_I:
    break;
  default:
    emit_dbg(opc[inst.op].name);
  }

  // #define InstrCnt() a.mov(TMP1, imm((uint64_t)&instrcnt)); a.mov(TMP2, x86::qword_ptr(TMP1)); a.inc(TMP2); a.mov(x86::qword_ptr(TMP1), TMP2);
#define InstrCnt()
  switch (inst.op) {
#include "beamasm_emit.h"
  case op_i_func_info_IaaI: {
    ErtsCodeInfo info;

    if (I == nullptr) {
      I = (BeamInstr*)&info;
      info.op = op_i_func_info_IaaI;
      info.mfa.module = inst.args[1].getValue();
      info.mfa.function = inst.args[2].getValue();
      info.mfa.arity = inst.args[3].getValue();
    }

    //code.flatten();
    if (functions.size() > 0) {
      Uint padbuff[BEAM_NATIVE_MIN_FUNC_SZ] = {0};
      uint64_t diff = a.offset() - code.labelOffsetFromBase(functions.back());

      if (diff <= sizeof(padbuff)) {
        comment("pad for nif load");
        a.embed(padbuff, sizeof(padbuff) - diff);
        for (auto l : labels) {
          if (l.second == currLabel) {
            currLabel = a.newLabel();
            labels[l.first] = currLabel;
            break;
          }
        }
        a.bind(currLabel);
      }
      ASSERT(a.offset() - code.labelOffsetFromBase(functions.back())
             >= sizeof(padbuff));
    }

    comment("%T:%T/%d", inst.args[1].getValue(), inst.args[2].getValue(), inst.args[3].getValue());
    a.embed(I, sizeof(ErtsCodeInfo));
    break;
  }
  case op_label_L:
    a.align(kAlignCode, 8);
    a.bind(labels[inst.args[0].getValue()]);
    currLabel = labels[inst.args[0].getValue()];
    break;
  case op_int_code_end: {
    Uint padbuff[BEAM_NATIVE_MIN_FUNC_SZ];
    uint64_t diff = a.offset() - code.labelOffsetFromBase(functions.back());
    if (diff <= sizeof(padbuff))
        a.embed(padbuff, sizeof(padbuff) - diff);
    ASSERT(a.offset() - code.labelOffsetFromBase(functions.back())
             >= sizeof(padbuff));
    emit_nyi();
  }
  case op_line_I:
    break;
  default:
    emit_nyi();
    break;
  }
  return true;
}

void *BeamModuleAssembler::codegen() {

//   if (!codegen(literals))
//     return nullptr;
    
  typedef void *(*Func)(void **);
  Func module;
  Error err = rt->add(&module,&code);
  ERTS_ASSERT(!err && "Failed to create module");

  /* FIXME: */
  for (auto p : catches) {
      BeamInstr *addr = (BeamInstr*)(((char*)module) + code.labelOffset(p.label));
      *p.ptr = addr;
  }

//   module(labels);
//   *catch_no = this->catch_no;
  this->module = (void*)module;
  return nullptr;
}

void BeamModuleAssembler::getCodeHeader(BeamCodeHeader **hdr) {

  BeamCodeHeader *orig_hdr = *hdr;
  BeamCodeHeader *code_hdr = (BeamCodeHeader *)getCode(codeHeader);
  memcpy(code_hdr, orig_hdr, sizeof(BeamCodeHeader));

  for (unsigned i = 0; i < functions.size(); i++) {
    ErtsCodeInfo *ci = (ErtsCodeInfo*)getCode(functions[i]);
    ASSERT(memcmp(&ci->mfa,&orig_hdr->functions[i]->mfa,sizeof(ci->mfa)) == 0);
    code_hdr->functions[i] = ci;
  }
  erts_free(ERTS_ALC_T_CODE, orig_hdr);
  *hdr = code_hdr;
}

void BeamModuleAssembler::patchImport(unsigned index, BeamInstr I) {

  for (auto l : imports[index].patches) {
    /* We patch the movabs instruction with the correct literal */
    char *pp = reinterpret_cast<char*>(getCode(l.where));
    Eterm *where = (Eterm*)(pp+l.ptr_offs);
    ASSERT(LLONG_MAX == *where);
    *where = I + l.val_offs;
  }
}

void BeamModuleAssembler::patchLiteral(unsigned index, Eterm lit) {

  for (auto l : literals[index].patches) {
    /* We patch the movabs instruction with the correct literal */
    char *pp = reinterpret_cast<char*>(getCode(l.where));
    Eterm *where = (Eterm*)(pp+l.ptr_offs);
    ASSERT(LLONG_MAX == *where);
    *where = lit + l.val_offs;
  }
}

void BeamModuleAssembler::patchStrings(byte *strtab) {

  for (auto s : strings) {
    char *pp = reinterpret_cast<char*>(getCode(s.second.where));
    byte **where = (byte**)(pp+2);
    ASSERT(LLONG_MAX == (Eterm)*where);
    *where = strtab + s.first;
  }
}
