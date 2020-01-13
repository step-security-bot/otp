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
#include <unordered_map>
#include <vector>
#include <assert.h>

extern "C" {

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"

}

using namespace asmjit;

/*
 * This structure keeps load-time information about a literal.
 */

typedef struct {
    Eterm term;			/* The tagged term (in the heap). */
    ErlHeapFragment* heap_frags;
} Literal;

class ArgVal {
    public:
        enum TYPE {
            u = TAG_u,
            i = TAG_i,
            a = TAG_a,
            x = TAG_x,
            y = TAG_y,
            f = TAG_f,
            h = TAG_h,
            z = TAG_z,
            n = TAG_n,
            p = TAG_p,
            r = TAG_r,
            v = TAG_v,
            l = TAG_l,
            q = TAG_q,
            o = TAG_o
        };
    private:
        enum TYPE type;
        BeamInstr value;
    public:
        ArgVal(TYPE t, BeamInstr val) {
            type = t;
            switch (type) {
                case n: value = NIL; break;
                default: value = val;
            }
        }
        ArgVal(unsigned t, BeamInstr val) : value(val) {
            type = (enum TYPE)t;
            assert(type == u || type == i || type == x || type == y || type == f || type == q);
        }
        enum TYPE getType() { return type; }
        uint64_t getValue() { assert(type != q); return value; }
        void resolveLiteral(std::vector<Literal> literals) { assert(type == q); type = i; value = literals[value].term; }

        bool isMem() { return type == x || type == y || type == r; }

        ArgVal operator +(const int val)  {
            ArgVal res(type, val + value);
            return res;
        }
};

std::string getAtom(Eterm atom) {
    // Ugly atom hack to get around that we cannot do global inline in cpp files at the moment...
    Atom *ap = (Atom *) erts_atom_table.seg_table[atom_val(atom)>>INDEX_PAGE_SHIFT][atom_val(atom)&INDEX_PAGE_MASK];
    return std::string((const char*)ap->name, ap->len);
}


extern "C" int is_function2(Eterm Term, Uint arity);

static void (*garbage_collect)();
extern "C" enum beamasm_ret (*beamasm_call)(BeamInstr **,Process *, Eterm *,
                                        Eterm **, Eterm **, Sint *, FloatDef *,
                                        BeamAsmFunc);
static void (*swapout)();

class BeamAssembler : public ErrorHandler {

  typedef std::unordered_map<unsigned, Label> LabelMap;

  CodeHolder code;                        // Holds code and relocation information.

  // TODO: Want to change this to x86::Builder in order to be able to patch the correct I into the
  //       code after code generation
  x86::Assembler a;                       // Create and attach X86Assembler to `code`.

  // Label -1 is normal return and label 0 is non-normal return (context switch)
  LabelMap labels;
  Label return_label;
  Label yield_label;
  FileLogger logger;

  std::vector<std::pair<unsigned, std::vector<ArgVal>>> instrs;

  x86::Gp ARG1 = x86::rdi;
  x86::Gp ARG2 = x86::rsi;
  x86::Gp ARG3 = x86::rdx;
  x86::Gp ARG4 = x86::rcx;
  x86::Gp ARG5 = x86::r8;
  x86::Gp ARG6 = x86::r9;
  const x86::Mem ARG7 = x86::qword_ptr(x86::rbp, 2*8);
  const x86::Mem ARG8 = x86::qword_ptr(x86::rbp, 3*8);
  x86::Gp RET  = x86::rax;

  // rbx = x_reg
  // rbp = E
  // r12 = c_p
  // r13 = FCALLS
  // r14 = HTOP
  // r15 = f_reg

  x86::Gp x_reg = x86::rbx;
  x86::Gp E = x86::r12;
  x86::Gp c_p = x86::r13;
  x86::Gp FCALLS = x86::r14;
  x86::Gp HTOP = x86::r15;
  x86::Gp f_reg = x86::r11; // This could use RBP but we want to use that as a scratch reg so we dont... for now
  const ArgVal CP = ArgVal(ArgVal::TYPE::y, 0);

  x86::Gp TMP1 = ARG1;
  x86::Gp TMP2 = ARG2;
  x86::Gp TMP3 = ARG3;
  x86::Gp TMP4 = ARG4;
  x86::Gp TMP5 = ARG5;
  x86::Gp TMP6 = ARG6;
  x86::Gp TMP7 = RET;

  static void dbg(char *msg, Process *c_p, Eterm *reg) {
    erts_printf("%s\n",msg);
  }

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
      alloc(slots.getValue());
  }

  void dealloc(Uint slots) {
      a.lea(E, x86::qword_ptr(E, slots*sizeof(Eterm)));
  }

  void dealloc(ArgVal slots) {
      // ArgVal gives the value in bytes
      assert(slots.getValue() % sizeof(Eterm) == 0);
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

  x86::Mem getRef(ArgVal val) {
      x86::Gp base;
      switch (val.getType()) {
          case ArgVal::TYPE::r:
          case ArgVal::TYPE::x: base = x_reg; break;
          case ArgVal::TYPE::y: base = E; break;
          default:
            int i = *(int*)(0);
            assert(i && "NYI");
      }
      return x86::qword_ptr(base, val.getValue() * sizeof(Eterm));
  }

  void mov(x86::Gp to, ArgVal from) {
      if (from.isMem())
        a.mov(to, getRef(from));
      else {
          comment("TODO: Optimize move from constant arg to reg");
          a.mov(to, from.getValue());
      }
  }

  void mov(x86::Mem to, ArgVal from) {
      if (from.isMem()) {
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
      if (from.isMem()) {
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

  public:

  BeamAssembler(JitRuntime *rt) : code() {
    code.init(rt->codeInfo());            // Initialize to the same arch as JIT runtime.
    code.attach(&a);
  }

  BeamAssembler(JitRuntime *rt, std::string log) : BeamAssembler(rt)  {
      setLogger(log);
  }

  BeamAssembler(JitRuntime *rt, Eterm mod, int num_labels) : BeamAssembler(rt, getAtom(mod) + ".asm") {

    for (int i = 0; i < num_labels; i++) {
        std::string lblName = "label_" + std::to_string(i);
        labels[i] = a.newNamedLabel(lblName.data());
    }
    
    emit_function_preamble();
    for (int i = 0; i < num_labels-1; i++) {
        a.lea(TMP2, x86::qword_ptr(labels[i+1]));
        a.mov(x86::qword_ptr(ARG1, i * sizeof(BeamInstr)), TMP2);
    }
    emit_function_postamble();
    
    emit_postamble();
  }

  ~BeamAssembler() {
      fclose(logger.file());
  }

  void handleError(Error err, const char* message, BaseEmitter* origin) {
    comment(message);
    fflush(logger.file());
    assert(0 && "Fault instruction encode");
  }

  void reset(JitRuntime *rt) {
    fclose(logger.file());
    code.reset(Globals::kResetHard);
    code.init(rt->codeInfo());
    code.attach(&a);
    code.setLogger(&logger);
  }

  void setLogger(std::string log) {
      FILE *f = fopen(log.data(),"w+");
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

  CodeHolder *getCodeHolder() {
      return &code;
  }

  void emit_garbage_collect() {
      /* This is the common stub used for calling garbage_collect. This functiuon is called
         with a custom calling convention where ARG2 and ARG4 are set, but the swapout and
         all other arguments have to be moved.
      */
     emit_function_preamble();
     // TODO: Should set c_p->i to I here....
     emit_swapout();
     a.mov(ARG1, c_p);
     a.mov(ARG3, x_reg);
     a.mov(ARG5, FCALLS);
     a.call((uint64_t)erts_garbage_collect_nobump);
     a.sub(FCALLS, RET);
     emit_swapin();
     emit_function_postamble();
  }

  void emit_asm_swapin() {
    emit_function_preamble();

    // Push all callee save
    a.push(c_p);
    a.push(x_reg);
    a.push(HTOP);
    a.push(E);
    a.push(FCALLS);
    a.push(f_reg);

    // Have to make sure the stack is 16 byte aligned here
    a.push(ARG1); // *I
    a.push(ARG4); // *HTOP
    a.push(ARG5); // *E
    a.push(ARG6); // *FCALLS

    // Move the arguments to the correct registers, ignoring c_p, FCALLS and f_reg for now...
    // typedef void (*BeamAsmFunc)(BeamInstr **,Process *,Eterm *, Eterm **, Eterm **, Sint *, FloatDef *);
    a.mov(c_p, ARG2);
    a.mov(x_reg, ARG3);
    a.mov(HTOP, x86::qword_ptr(ARG4));
    a.mov(E, x86::qword_ptr(ARG5));
    a.mov(FCALLS, x86::qword_ptr(ARG6));
    a.mov(f_reg, ARG7);
    a.jmp(ARG8);
  }

  void emit_asm_swapout() {

    // Below here we simulate a return instruction to get the proper return into
    // the interpreter again

    // Set I to be the return address, the address is stored in TMP3
    a.mov(TMP2, x86::qword_ptr(x86::rsp, 24));
    a.mov(x86::qword_ptr(TMP2, 0), TMP3);

    // Restore the rest of the emulator state
    a.mov(TMP1, x86::qword_ptr(x86::rsp, 16));
    a.mov(x86::qword_ptr(TMP1, 0), HTOP);
    a.mov(TMP1, x86::qword_ptr(x86::rsp, 8));
    a.mov(x86::qword_ptr(TMP1, 0), E);
    a.mov(TMP1, x86::qword_ptr(x86::rsp, 0));
    a.mov(x86::qword_ptr(TMP1, 0), FCALLS);

    // Adjust stack
    a.lea(x86::rsp, x86::qword_ptr(x86::rsp, 4 * 8));

    // Pop all callee save
    a.pop(f_reg);
    a.pop(FCALLS);
    a.pop(E);
    a.pop(HTOP);
    a.pop(x_reg);
    a.pop(c_p);

    emit_function_postamble();
  }

  void emit_dbg(const char *msg) {
    //   a.push(ARG1);
    //   a.push(ARG2);
    //   a.push(ARG3);
    //   emit_swapout();
    //   a.mov(ARG1, (uint64_t)msg);
    //   a.mov(ARG2, c_p);
    //   a.mov(ARG3, x_reg);
    //   a.call((uint64_t)dbg);
    //   a.pop(ARG3);
    //   a.pop(ARG2);
    //   a.pop(ARG1);
  }

  void emit_dbg_call(ErtsCodeMFA mfa) {
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

  void emit_swapin() {
    a.mov(E,x86::qword_ptr(c_p, offsetof(Process, stop)));
    a.mov(HTOP,x86::qword_ptr(c_p, offsetof(Process, htop)));
  }

  void emit_swapout() {
    a.mov(x86::qword_ptr(c_p, offsetof(Process, stop)), E);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, htop)), HTOP);
  }

  void emit_function_preamble() {
    // Have to make sure the stack is 16 byte aligned here
    a.push(x86::rbp);
    a.mov(x86::rbp, x86::rsp);
  }

  void emit_function_postamble(unsigned pops = 0) {
    // Adjust the stack back again
    a.lea(x86::rsp,x86::dword_ptr(x86::rsp, 8 * (pops)));
    a.pop(x86::rbp);
    // Return back to intepreter
    a.ret();
  }

  void emit_preamble() {
    // Setup the erlang stack, we use label 0 as the cleanup and exit label
    pushY(labels[0]);
  }

  void emit_postamble() {
    // Align in order to make value is_CP
    a.align(kAlignCode, 8);

    // This label is used when we are done with the current function call
    a.bind(labels[0]);

    a.mov(RET,RET_dispatch);
    // Pop the frame we pushed in the preamble
    popY();

    mov(TMP3, CP);
    mov(CP, NIL);

    a.jmp((uint64_t)swapout);
    
  }

  std::unordered_map<BeamInstr, ErtsCodeMFA> labelToMFA;
  std::vector<ArgVal> currFunction;
  unsigned prev_op = 0;

    int emit(unsigned specific_op, std::vector<ArgVal> args) {
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

        instrs.push_back(std::make_pair(specific_op, args));
        return 1;
    }

    int codegen(std::vector<Literal> literals) {

        for (std::pair<unsigned,std::vector<ArgVal>> inst : instrs) {
            unsigned specific_op = inst.first;
            std::vector<ArgVal> &args = inst.second;
            for (ArgVal &arg : args) {
                if (arg.getType() == ArgVal::q) {
                    arg.resolveLiteral(literals);
                }
            }
            comment(opc[specific_op].name);
            emit_dbg(opc[specific_op].name);
            switch (inst.first) {
            #include "beamasm_emit.h"
            case op_i_func_info_IaaI:
                emit_preamble();
                comment("%T:%T/%d", args[1].getValue(), args[2].getValue(), args[3].getValue());
                break;
            case op_label_L:
                a.bind(labels[args[0].getValue()]);
                break;
            case op_int_code_end:
            case op_line_I:
                break;
            default:
                return 0;
            }
            
        }
        return 1;
    }
};

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
extern "C" enum beamasm_ret (*beamasm_call)(BeamInstr **,Process *, Eterm *,
                                            Eterm **, Eterm **, Sint *, FloatDef *,
                                            BeamAsmFunc ba);

extern "C" void beamasm_init() {
    Error err;
    rt = new JitRuntime();
    BeamAssembler ba(rt);
    ba.setLogger("garbage_collect.asm");
    ba.emit_garbage_collect();
    err = rt->add(&garbage_collect,ba.getCodeHolder());
    assert(!err && "Failed to create garbage collection function");
    ba.reset(rt);

    ba.setLogger("swapin.asm");
    ba.emit_asm_swapin();
    err = rt->add(&beamasm_call,ba.getCodeHolder());
    assert(!err && "Failed to create asm swampin function");
    ba.reset(rt);

    ba.setLogger("swapout.asm");
    ba.emit_asm_swapout();
    err = rt->add(&swapout,ba.getCodeHolder());
    assert(!err && "Failed to create asm swampout function");
}

extern "C" void *beamasm_new_module(Eterm mod, int num_labels) {
    return new BeamAssembler(rt, mod, num_labels);
}

extern "C" int beamasm_emit(void *instance, int specific_op, GenOp *op) {
    BeamAssembler *ba = static_cast<BeamAssembler*>(instance);
    std::vector<ArgVal> args;
    for (int i = 0; i < op->arity; i++) {
        args.push_back(ArgVal(op->a[i].type,op->a[i].val));
    }
    
    return ba->emit(specific_op, args);
}

extern "C" void *beamasm_get_module(void *instance, void **labels, Literal *literals, int num_literals) {
    typedef void (*Func)(void**);
    Func module;
    std::vector<Literal> lits(literals, literals+num_literals);
    BeamAssembler *ba = static_cast<BeamAssembler*>(instance);
    if (!ba->codegen(lits)) {
        delete ba;
        return nullptr;
    }
    rt->add(&module, ba->getCodeHolder());
    delete ba;
    module(labels);
    return (void*)module;
}

extern "C" void beamasm_delete_module(void *instance) {
    BeamAssembler *ba = static_cast<BeamAssembler*>(instance);
    delete ba;
}
