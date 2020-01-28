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

}

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
        uint64_t getValue() { ASSERT(type != q); return value; }
        void resolveLiteral(std::vector<Literal> literals) {
            ASSERT(type == q);
            type = i;
            value = literals[value].term;
        }

        void resolveImport(std::vector<BeamInstr> imports) {
            ASSERT(type == e);
            type = u;
            value = imports[value];
        }

        bool isMem() { return type == x || type == y; }

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

  protected:

    ~BeamAssembler() {
      fclose(logger.file());
    }

    void handleError(Error err, const char* message, BaseEmitter* origin) {
        comment(message);
        fflush(logger.file());
        ASSERT(0 && "Fault instruction encode");
    }

    void emit_swapin(void);
    void emit_swapout(void);

  public:

    void reset() {
        if (logger.file())
            fclose(logger.file());
        code.reset(Globals::kResetHard);
        code.init(rt->codeInfo());
        code.attach(&a);
        
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

    void emit_function_preamble() {
        // Have to make sure the stack is 16 byte aligned here
        a.push(x86::rbp);
        a.mov(x86::rbp, x86::rsp);
    }

    void emit_function_postamble(unsigned pops = 0) {
        if (pops != 0)
            // Adjust the stack back again
            a.lea(x86::rsp,x86::dword_ptr(x86::rsp, 8 * (pops)));
        a.pop(x86::rbp);
        // Return back to intepreter
        a.ret();
    }
};

class BeamGlobalAssembler : public BeamAssembler {

    void (*garbage_collect)();
    void (*gc_after_bif)();
    void (*swapin)();
    void (*swapout)();

    void emit_garbage_collect(void);
    void emit_gc_efter_bif(void);
    void emit_asm_swapin(void);
    void emit_asm_swapout(void);

  public:
    BeamGlobalAssembler(JitRuntime *rt);

    uint64_t getGarbageCollect() {
        return (uint64_t)garbage_collect;
    }

    uint64_t getGcAfterBif() {
        return (uint64_t)gc_after_bif;
    }

    uint64_t getSwapin() {
        return (uint64_t)swapin;
    }

    uint64_t getSwapout() {
        return (uint64_t)swapout;
    }

};

class BeamModuleAssembler : public BeamAssembler {

    typedef unsigned BeamLabel;

    /* Map of label number to asmjit Label */
    typedef std::unordered_map<BeamLabel, Label> LabelMap;
    LabelMap labels;

    /* All functions that have been seen so far */
    std::unordered_map<BeamLabel, ErtsCodeMFA> labelToMFA;

    BeamGlobalAssembler *ga;

    /* All instructions that have been seen so far */
    typedef struct {
        unsigned op;
        std::vector<ArgVal> args;
        BeamInstr *I;
    } Instruction;
    std::vector<Instruction> instrs;
    Instruction *inst; /* Current instruction during codegen */

    std::vector<std::pair<Label, BeamInstr **>> catches;
    unsigned catch_no = 0;

    /* Used by emit to populate the labelToMFA map */
    std::vector<ArgVal> currFunction;
    unsigned prev_op = 0;
    
  public:
    BeamModuleAssembler(JitRuntime *rt, BeamGlobalAssembler *ga, Eterm mod, unsigned num_labels);

    bool emit(unsigned op, std::vector<ArgVal> args, BeamInstr *I);
    void *codegen(std::vector<Literal>, std::vector<BeamInstr> imports,
                    unsigned *catch_no, void **labels);

  private:

    int codegen(std::vector<Literal> literals, std::vector<BeamInstr> imports);

    /* Helpers */
    void emit_gc_test(ArgVal Stack, ArgVal Heap, ArgVal Live);
    void emit_dispatch_rel(ArgVal CallDest);
    void emit_dispatch_return(x86::Gp dest);
    void emit_dispatch_export(ArgVal Exp);
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
    void emit_call_guard_bif(std::vector<ArgVal> args, ArgVal bif, ArgVal Dst, Instruction *I, Label next);
    void emit_guard_bif_error(std::vector<ArgVal> args, ArgVal bif, Instruction *I);
    void emit_bif_arg_error(std::vector<ArgVal> args, Instruction *inst, ErtsCodeMFA *mfa);
    void emit_fail_head_or_body(ArgVal Fail);
    void emit_badarg(ArgVal Fail);
    void emit_nyi() { a.mov(TMP1, x86::ptr_abs(0)); }

    #include "beamasm_protos.h"

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

    void cmp(x86::Mem X, BeamInstr imm) {
        int64_t simm = imm;
        if (simm < (1ll << 31) && simm > (-1ll << 31)) {
            a.cmp(X, imm);
        } else {
            a.mov(TMP1, imm);
            a.cmp(X, TMP1);
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

    x86::Mem& incr(x86::Mem &mem) {
        mem.addOffset(sizeof(Eterm));
        return mem;
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

    void emit_preamble(ArgVal live) {
        // Setup the erlang stack, we use label 0 as the cleanup and exit label
        emit_allocate(ArgVal(ArgVal::u,0), live);
        a.lea(TMP1, x86::qword_ptr(labels[0]));
        mov(CP, TMP1);
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

        a.jmp((uint64_t)ga->getSwapout());
        
    }

};
