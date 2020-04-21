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

#include "sys.h"
#include "erl_process.h"

#define MAX_OPARGS 8

/*
 * This structure keeps load-time information about a literal.
 */

typedef struct {
    Eterm term;			/* The tagged term (in the heap). */
    ErlHeapFragment* heap_frags;
} Literal;

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


enum beamasm_ret
beamasm_call(BeamAsmContext *,Process *, Eterm *,
             FloatDef *, ERL_BITS_DECLARE_STATEP, 
             Sint);

void beamasm_init(void);
void *beamasm_new_module(Eterm mod, int num_labels, int num_functions);
void beamasm_codegen(void *ba);
void beamasm_delete_module(void *ba);
int beamasm_emit(void *ba, unsigned specific_op, GenOp *op, BeamInstr *I);
BeamInstr beamasm_get_code(void *ba, int label);
byte *beamasm_get_rodata(void *ba, char* label);
void beamasm_embed_rodata(void *ba, char *labelName, char* buff, size_t size);
unsigned int beamasm_get_catches(void *ba);
void beamasm_patch_import(void *ba, int index, BeamInstr import);
void beamasm_patch_literal(void *instance, int index, Eterm lit);
void beamasm_patch_strings(void *instance, byte *strtab);
void beamasm_emit_patch(Eterm module, unsigned specific_op, GenOp *op, char *buff,
                        unsigned buf_len, int debug);
Uint beamasm_get_header(void *ba, BeamCodeHeader **);
BeamInstr beamasm_get_on_load(void *ba);

// Number of bytes emitted at first label in order to support trace and nif load
#  define BEAM_ASM_FUNC_PROLOGUE_SIZE 32


extern BeamInstr *beamasm_call_nif_early;
