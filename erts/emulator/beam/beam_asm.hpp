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

/*
 * Thoughts:
 * - We do not want to do cross-instruction optimizations in the loader as that would make
 *   the difference from the interpreter too much. At most we do the same opts as for the
 *   interpreter. This is mostly in order to keep the emitted close to what gcc emits so that
 *   we can easily implement new instructions.
 *
 * - Should we keep x0-x2 in registers in the CC? We could analyze the beam code and keep
 *   even more instructions in registers in between calls as long as we spill before any calls.
 *
 * - For trace and msacc we should put a jmp + nop sleigh which we will then patch with a
 *   call instruction to some common code. For trace, we could generate code specific for the
 *   trace flags so that we don't have to check so many things when tracing... but that is overkill
 *   to start with.
 *
 * - External calls should use the cached if branch approach in order to improve processor
 *   speculation.
 */

#include <vector>
#include <unordered_map>
#include <asmjit/asmjit.h>

extern "C" {

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "beam_catches.h"
#include "beam_asm.h"

}

class ArgVal {
public:
  enum TYPE {
    u = TAG_u,
    i = TAG_i,
    x = TAG_x,
    y = TAG_y,
    f = TAG_f,
    q = TAG_q,
    e = TAG_r
  };

private:
  BeamInstr value;
  enum TYPE type;
public:
  ArgVal(TYPE t, BeamInstr val) : value(val), type(t) { }
  ArgVal(unsigned t, BeamInstr val) : value(val) {
    switch (t) {
    case TAG_u: type = u; break;
    case TAG_i: type = i; break;
    case TAG_x: type = x; break;
    case TAG_y: type = y; break;
    case TAG_f: type = f; break;
    case TAG_q: type = q; break;
    case TAG_r: type = e; break;
    default:
      ASSERT(0);
    }
  }
  enum TYPE getType() { return type; }
  uint64_t getValue() { return value; }

  bool isMem() { return type == x || type == y; }
  bool isLiteral() { return type == q; }

  ArgVal operator +(const int val)  {
    ArgVal res(type, val + value);
    return res;
  }

  ArgVal operator *(const int val)  {
    ArgVal res(type, val * value);
    return res;
  }
};

using namespace asmjit;

class BeamAssembler : public ErrorHandler {

protected:

  JitRuntime *rt;

  CodeHolder code;                        // Holds code and relocation information.

  // TODO: Want to change this to x86::Builder in order to be able to patch the correct I into the
  //       code after code generation
  x86::Assembler a;                       // Create and attach X86Assembler to `code`.

  FileLogger logger;

  const x86::Gp ARG1 = x86::rdi;
  const x86::Gp ARG2 = x86::rsi;
  const x86::Gp ARG3 = x86::rdx;
  const x86::Gp ARG4 = x86::rcx;
  const x86::Gp ARG5 = x86::r8;
  const x86::Gp ARG6 = x86::r9;
  const x86::Mem ARG7 = x86::qword_ptr(x86::rbp, 2*sizeof(Eterm));
  const x86::Mem ARG8 = x86::qword_ptr(x86::rbp, 3*sizeof(Eterm));
  const x86::Gp RET  = x86::rax;

  const x86::Gp TMP1 = ARG1;
  const x86::Gp TMP2 = ARG2;
  const x86::Gp TMP3 = ARG3;
  const x86::Gp TMP4 = ARG4;
  const x86::Gp TMP5 = ARG5;
  const x86::Gp TMP6 = ARG6;
  const x86::Gp TMP7 = RET;

  const x86::Mem qTMP1_MEM = x86::qword_ptr(x86::rsp, 16);
  const x86::Mem qTMP2_MEM = x86::qword_ptr(x86::rsp, 8);
  const x86::Mem qTMP3_MEM = x86::qword_ptr(x86::rsp, 0);
  const x86::Mem dTMP1_MEM = x86::dword_ptr(x86::rsp, 16);
  const x86::Mem dTMP2_MEM = x86::dword_ptr(x86::rsp, 8);
  const x86::Mem dTMP3_MEM = x86::dword_ptr(x86::rsp, 0);


  // rbx = x_reg
  // rbp = E
  // r12 = c_p
  // r13 = FCALLS
  // r14 = HTOP
  // r15 = f_reg

  const x86::Gp x_reg = x86::rbx;
  const x86::Gp E = x86::r12;
  const x86::Gp c_p = x86::r13;
  const x86::Gp FCALLS = x86::r14;
  const x86::Gp HTOP = x86::r15;
  const x86::Gp f_reg = x86::r11; // This could use RBP but we want to use that as a scratch reg so we dont... for now
  const ArgVal CP = ArgVal(ArgVal::TYPE::y, 0);
  const x86::Mem EBS = x86::qword_ptr(x86::rsp, 0);

  const ArgVal x0 = ArgVal(ArgVal::x, 0);
  const ArgVal x1 = ArgVal(ArgVal::x, 1);
  const ArgVal x2 = ArgVal(ArgVal::x, 2);
  const ArgVal x3 = ArgVal(ArgVal::x, 3);

public:

  BeamAssembler(JitRuntime *rt) : rt(rt), code() {
    code.init(rt->codeInfo());            // Initialize to the same arch as JIT runtime.
    code.attach(&a);
    code.addEmitterOptions(BaseEmitter::kOptionStrictValidation);
  }

  BeamAssembler(JitRuntime *rt, std::string log) : BeamAssembler(rt)  {
    setLogger(log);
  }

  ~BeamAssembler() {
    fclose(logger.file());
  }

protected:

  void handleError(Error err, const char* message, BaseEmitter* origin) {
    comment(message);
    fflush(logger.file());
    ASSERT(0 && "Fault instruction encode");
  }

  void emit_swapin(void);
  void emit_swapout(void);
  void emit_light_swapin(void);
  void emit_light_swapout(void);
  void emit_heavy_swapin(void);
  void emit_heavy_swapout(void);

public:

  /* This is a special call that we have to use if we want to do far calls */
  void call(uint64_t func) {
    a.mov(RET, func);
    a.call(RET);
  }

  x86::Mem getRef(ArgVal val) {
    x86::Gp base;
    switch (val.getType()) {
    case ArgVal::TYPE::x: base = x_reg; break;
    case ArgVal::TYPE::y: base = E; break;
    default:
      int i = *(int*)(0);
      ASSERT(i && "NYI");
    }
    return x86::qword_ptr(base, val.getValue() * sizeof(Eterm));
  }

  void reset() {
    if (logger.file())
      fclose(logger.file());
    code.reset(Globals::kResetHard);
    code.init(rt->codeInfo());
    code.attach(&a);
  }

  void setLogger(std::string log) {
    FILE *f = fopen(log.data(),"w+");
    setvbuf(f, NULL, _IONBF, 0);
    setLogger(f);
  }

  void setLogger(FILE *log) {
    logger.setFile(log);
    logger.setIndentation(FormatOptions::kIndentationCode, 4);
    code.setLogger(&logger);
    code.setErrorHandler(this);
  }

  void comment(const char *msg) {
    a.commentf("# %s", msg);
  }

  void comment(std::string fmt, ...) {
    char buff[1024];
    va_list ap;
    va_start(ap, fmt);
    erts_vsprintf(buff, fmt.data(), ap);
    va_end(ap);
    comment(buff);
  }

  void emit_function_preamble(unsigned pushes = 0) {
    // Have to make sure the stack is 16 byte aligned here
    a.push(x86::rbp);
    a.mov(x86::rbp, x86::rsp);
    if (pushes) {
      if (pushes / 2 == 1) pushes++;
      a.lea(x86::rsp,x86::qword_ptr(x86::rsp, -8 * pushes ));
    }
  }

  void emit_function_postamble(unsigned pops = 0) {
    if (pops) {
      if (pops / 2 == 1) pops++;
      // Adjust the stack back again
      a.lea(x86::rsp,x86::qword_ptr(x86::rsp, 8 * pops));
    }
    a.pop(x86::rbp);
    // Return back to intepreter
    a.ret();
  }

  void align(unsigned alignment = 8) {
    a.align(kAlignCode, alignment);
  }
};

class BeamGlobalAssembler : public BeamAssembler {

#define BEAM_GLOBAL_FUNCS(_)                    \
  _(return)                                     \
  _(garbage_collect)                            \
  _(gc_after_bif)                               \
  _(normal_exit)                                \
  _(continue_exit)                              \
  _(post_error_handling)                        \
  _(error_action_code)                          \
  _(call_error_handler)                         \
  _(i_func_info)                                \
  _(dbg)                                        \
  _(call_nif)                                   \
  _(dispatch_nif)                               \
  _(call)


#define DECL_FUNC(NAME)                         \
  void (*NAME##_code)();                        \
  void emit_##NAME(void);
  BEAM_GLOBAL_FUNCS(DECL_FUNC);
#undef FUNC_DECL

public:
  BeamGlobalAssembler(JitRuntime *rt);

#define GET_FUNC(NAME)                                                  \
  uint64_t get_##NAME() { ASSERT(NAME##_code); return (uint64_t)NAME##_code; }
  BEAM_GLOBAL_FUNCS(GET_FUNC)
#undef GET_FUNCS

  uint64_t get_handle_error() { return get_error_action_code(); }
};

class BeamModuleAssembler : public BeamAssembler {

  typedef unsigned BeamLabel;

  /* Map of label number to asmjit Label */
  typedef std::unordered_map<BeamLabel, Label> LabelMap;
  LabelMap labels;

  struct patch { Label where; int64_t ptr_offs; int64_t val_offs; };

  /* Map of import entry to patch labels and mfa */
  struct patch_import { std::vector<struct patch> patches; ErtsCodeMFA mfa; };
  typedef std::unordered_map<unsigned, struct patch_import> ImportMap;
  ImportMap imports;

  /* Map of literals to patch labels */
  struct patch_literal { std::vector<struct patch> patches; };
  typedef std::unordered_map<unsigned, struct patch_literal> LiteralMap;
  LiteralMap literals;

  /* Map of strings to patch labels */
  typedef std::unordered_map<unsigned, struct patch> StringMap;
  StringMap strings;

  /* All functions that have been seen so far */
  std::vector<BeamLabel> functions;

  BeamGlobalAssembler *ga;

  /* All instructions that have been seen so far */
  typedef struct {
    unsigned op;
    std::vector<ArgVal> args;
    BeamInstr *I;
  } Instruction;
  std::vector<Instruction> instrs;
  Instruction inst; /* Current instruction */

  std::vector<std::pair<Label, unsigned>> catches;
  unsigned catch_no = BEAM_CATCHES_NIL;

  /* Used by emit to populate the labelToMFA map */
  std::vector<ArgVal> currFunction;
  Label currLabel;
  unsigned prev_op = 0;
  Label codeHeader;

  void *module = nullptr;
  Eterm mod;

  Section *rodata = nullptr;

  bool debug = true;

public:
  BeamModuleAssembler(JitRuntime *rt, BeamGlobalAssembler *ga, Eterm mod, unsigned num_labels);
  BeamModuleAssembler(JitRuntime *rt, BeamGlobalAssembler *ga, Eterm mod, unsigned num_labels,
                      unsigned num_functions);

  bool emit(unsigned op, std::vector<ArgVal> args, BeamInstr *I = nullptr);
  void *codegen();
  BeamInstr getCode(unsigned label);
  void *getCode(Label label);
  byte *getCode(char *labelName);
  void embed_rodata(char *labelName, char *buff, size_t size);
  Label embed_instr_rodata(Instruction *instr, int index, int count);
  unsigned getCodeSize() { ASSERT(module); return code.codeSize(); }
  void getCodeHeader(BeamCodeHeader **);
  void patchLiteral(unsigned index, Eterm lit);
  void patchImport(unsigned index, BeamInstr I);
  void patchStrings(byte *strtab);
  static void dbg(char *msg, Process *c_p, Eterm *reg, BeamInstr *I) {
    erts_printf("%T: %s\n",I, msg);
  }

  void setDebug(bool debug) {
    this->debug = debug;
  }

private:

  /* Helpers */
  void emit_gc_test(ArgVal Stack, ArgVal Heap, ArgVal Live);
  void emit_dispatch(x86::Gp where, enum beamasm_ret what = RET_context_switch);
  void emit_dispatch_rel(ArgVal CallDest);
  void emit_dispatch_return(x86::Gp dest);
  void emit_dispatch_export(ArgVal Exp);
  x86::Gp emit_apply(uint64_t deallocate);
  x86::Gp emit_apply(ArgVal arirty, uint64_t deallocate);
  x86::Gp emit_call_fun(ArgVal Fun);
  x86::Gp emit_apply_fun(void);
  void emit_setup_return(x86::Gp dest);
  x86::Mem emit_boxed_val(x86::Gp Src, uint64_t bytes = 0);
  void emit_is_boxed(Label Fail, x86::Gp Src);
  x86::Mem emit_list_val(x86::Gp Src);
  x86::Mem emit_car(x86::Mem Src);
  x86::Mem emit_cdr(x86::Mem Src);
  void emit_is_binary(Label Fail, x86::Gp Src, Label next, Label subbin);
  void emit_is_list(Label Fail, x86::Gp Src);
  void emit_is_integer(Label Fail, Label next, Label BigFail, x86::Gp Src);
  void emit_cmp_spec(x86::Inst::Id jmpOp, Label Fail, Label next, Operand X, Operand Y, unsigned EqOnly);
  void emit_call_guard_bif(std::vector<ArgVal> args, ArgVal bif, ArgVal Dst, Label entry, Label next);
  void emit_guard_bif_error(std::vector<ArgVal> args, ArgVal bif, Instruction *I);
  void emit_bif_arg_error(std::vector<ArgVal> args, Label entry, ErtsCodeMFA *mfa);
  void emit_fail_head_or_body(Label entry, ArgVal Fail);
  void emit_badarg(Label entry, ArgVal Fail);
  template<typename T>
  void emit_yield_error_test(Label entry, T exp, bool only);
  void emit_handle_error(Label I, ErtsCodeMFA *mfa = nullptr);
  void emit_handle_error(Label I, ArgVal exp);

  void emit_select_val(ArgVal Src, ArgVal Fail, ArgVal N, Instruction *I);
  void emit_select_tuple_val(ArgVal Src, ArgVal Fail, ArgVal N, Instruction *I);

  void emit_proc_lc_unrequire(void);
  void emit_proc_lc_require(void);

  void emit_nyi() { comment("NYI"); emit_dbg("NYI"); a.mov(TMP1, x86::ptr_abs(0)); }

#include "beamasm_protos.h"

  static void call_dbg(unsigned arity, Process *c_p, Eterm *reg) {
    erts_printf("Args: ");
    for (unsigned i = 0; i < arity; i++)
      erts_printf("%T; ",reg[i]);
    erts_printf("\n");
  }

  void alloc(Uint slots) {
    a.lea(E, x86::qword_ptr(E, -1*slots*sizeof(Eterm)));
  }

  void alloc(ArgVal slots) {
    // ArgVal gives the value in bytes
    ASSERT(slots.getValue() % sizeof(Eterm) == 0);
    alloc(slots.getValue() / sizeof(Eterm));
  }

  void dealloc(Uint slots) {
    a.lea(E, x86::qword_ptr(E, slots*sizeof(Eterm)));
  }

  void dealloc(ArgVal slots) {
    // ArgVal gives the value in bytes
    ASSERT(slots.getValue() % sizeof(Eterm) == 0);
    dealloc(slots.getValue() / sizeof(Eterm));
  }

  void pushY(Label lbl) {
    alloc(1);
    a.lea(TMP1, x86::qword_ptr(lbl));
    mov(CP, TMP1);
  }

  void popY() {
    dealloc(1);
  }

  void make_move_patch(x86::Gp to, struct patch &patches, int64_t offset = 0) {
    Label lbl = a.newLabel();

    a.bind(lbl);
    a.mov(to, imm(LLONG_MAX));

    /* Offset of 0x2 = movabs */
    patches = {lbl, 0x2, offset};
  }

  void make_move_patch(x86::Gp to, std::vector<struct patch> &patches, int64_t offset = 0) {
    Label lbl = a.newLabel();

    a.bind(lbl);
    a.mov(to, imm(LLONG_MAX));

    /* Offset of 0x2 = movabs */
    patches.push_back({lbl, 0x2, offset});
  }

  void make_word_patch(std::vector<struct patch> &patches) {
    Label lbl = a.newLabel();
    UWord word = LLONG_MAX;

    a.bind(lbl);
    a.embed(reinterpret_cast<char*>(&word), sizeof(word));

    patches.push_back({lbl, 0, 0});
  }

  void mov(x86::Gp to, ArgVal from) {
    if (from.isMem())
      a.mov(to, getRef(from));
    else  if (from.isLiteral()) {
      make_move_patch(to, literals[from.getValue()].patches);
    } else {
      comment("TODO: Optimize move from constant arg to reg");
      a.mov(to, from.getValue());
    }
  }

  void mov(x86::Mem to, ArgVal from) {
    if (from.isMem() || from.isLiteral()) {
      mov(TMP1, from);
      a.mov(to, TMP1);
    } else {
      mov(to, from.getValue());
    }
  }

  void mov(x86::Mem to, BeamInstr imm) {
    int64_t simm = imm;
    // TODO: What is this about? for some reason I cannot move a 64-bit imm to a memory location?
    if (simm < (1ll << 31) && simm > (-1ll << 31)) {
      a.mov(to, imm);
    } else {
      a.mov(TMP1, imm);
      a.mov(to, TMP1);
    }
  }

  void mov(ArgVal to, x86::Gp from) {
    a.mov(getRef(to), from);
  }

  void mov(ArgVal to, BeamInstr from) {
    mov(getRef(to), from);
  }

  void mov(ArgVal to, ArgVal from) {
    if (from.isMem() || from.isLiteral()) {
      mov(TMP1, from);
      mov(to, TMP1);
    } else {
      mov(to, from.getValue());
    }
  }

  void lea(ArgVal to, x86::Mem from) {
    a.lea(TMP1, from);
    mov(to, TMP1);
  }

  void cmp(x86::Mem X, BeamInstr imm) {
    int64_t simm = imm;
    if (simm < (1ll << 31) && simm > (-1ll << 31)) {
      a.cmp(X, imm);
    } else {
      a.mov(TMP1, imm);
      a.cmp(X, TMP1);
    }
  }

  void cmp(x86::Gp X, BeamInstr imm) {
    int64_t simm = imm;
    if (simm < (1ll << 31) && simm > (-1ll << 31)) {
      a.cmp(X, imm);
    } else {
      a.mov(TMP1, imm);
      a.cmp(X, TMP1);
    }
  }

  void call(uint64_t func) {
    BeamAssembler::call(func);
  }

  void call(ArgVal func) {
    mov(RET, func);
    a.call(RET);
  }

  x86::Mem& incr(x86::Mem &mem) {
    mem.addOffset(sizeof(Eterm));
    return mem;
  }

  void emit_dbg(const char *msg) {
    if (debug) {
      a.mov(TMP1, Imm((uint64_t)msg));
      a.mov(qTMP1_MEM, TMP1);
      call(ga->get_dbg());
    }
  }

  void emit_dbg_call(ErtsCodeMFA *mfa) {
    if (debug) {
      //   char *buff = new char[1024];
      //   erts_sprintf(buff, "Called %T:%T/%d", mfa.module, mfa.function, mfa.arity);
      //   a.push(ARG1);
      //   a.push(ARG2);
      //   a.push(ARG3);
      //   emit_swapout();
      //   a.mov(ARG1, (uint64_t)buff);
      //   a.mov(ARG2, c_p);
      //   a.mov(ARG3, x_reg);
      //   a.call((uint64_t)dbg);
      //   a.mov(ARG1, mfa.arity);
      //   a.mov(ARG2, c_p);
      //   a.mov(ARG3, x_reg);
      //   a.call((uint64_t)call_dbg);
      //   a.pop(ARG3);
      //   a.pop(ARG2);
      //   a.pop(ARG1);
    }
  }
};
