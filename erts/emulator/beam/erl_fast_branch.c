/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2015. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "global.h"
#include "erl_fast_branch.h"
#include "erl_fast_branch_symbols.h"

#define ERTS_FAST_BRANCH_DECLARE(name, num) \
    ErtsFastBranches __##name##_variable;
ERTS_FAST_BRANCHES
#undef ERTS_FAST_BRANCH_DECLARE

static void ptr_protect(void *ptr, int flags)
{
    void *page_ptr = (void*)((long)ptr & ~(0xfffl));
    if (mprotect(page_ptr, (ptr - page_ptr) + 5, flags) < 0) {
        abort();
    }
}

static void protect(void *ptr)
{
    ptr_protect(ptr, PROT_READ | PROT_EXEC);
    /* Clear the instruction cache */
    __clear_cache(ptr, ptr + 2);
}

static void unprotect(void *ptr)
{
    ptr_protect(ptr, PROT_READ | PROT_WRITE | PROT_EXEC);
}

static void update_jump_target(ErtsFastBranch *fb,
                               void *start, void *new_target)
{
    Sint32 addr_diff = (char *)new_target - (char *)start - 2;
    UWord *preq_ptr = (UWord*)start, preq_val = *preq_ptr;

    unprotect(start);

/*   e9 == jmpq
     92 fa ff ff == twos-compliment little endian 4 byte integer
   e9 92 fa ff ff

     eb == jmp
     fa == twos-compliment 1 byte integer
   eb fa
*/

    if (addr_diff < 0x80 && addr_diff > -0x80) {
        ((byte *)&preq_val)[0] = 0xeb;
        ((byte *)&preq_val)[1] = addr_diff;
        /* We insert a bunch of nops to overwrite the
           broken asm for gdb disassemble to look nicer */
        ((byte *)&preq_val)[2] = 0x0F;
        ((byte *)&preq_val)[3] = 0x1F;
        ((byte *)&preq_val)[4] = 0x00;
    } else {
        ((byte *)&preq_val)[0] = 0xe9;
        /* subtract 3 extra as jmpq is 3 bytes larger than jmp */
        *(Sint32*)(((byte *)&preq_val)+1) = addr_diff - 3;
    }
    *preq_ptr = preq_val;
    protect(start);
}

void erts_fast_branch_enable(ErtsFastBranches *fb) {
    int i;
    for (i = 0; i < fb->num_branches; i++) {
        ErtsFastBranch *cfb = fb->branches+i;
        update_jump_target(cfb, cfb->start, cfb->trace);
    }
}

void erts_fast_branch_disable(ErtsFastBranches *fb) {
    int i;
    for (i = 0; i < fb->num_branches; i++) {
        ErtsFastBranch *cfb = fb->branches+i;
        void *start = cfb->start;
        UWord *preq_ptr = (UWord*)start, preq_val = *preq_ptr;
        unprotect(start);
        /* Set all instruction to 5 byte nop, i.e. fall through,
           see http://www.felixcloutier.com/x86/NOP.html
           for recommended multi byte nops */
        ((byte *)&preq_val)[0] = 0x0F;
        ((byte *)&preq_val)[1] = 0x1F;
        ((byte *)&preq_val)[2] = 0x44;
        ((byte *)&preq_val)[3] = 0x00;
        ((byte *)&preq_val)[4] = 0x00;
        *preq_ptr = preq_val;
        protect(start);
    }
}

#define ERTS_FAST_BRANCH_SYMBOL(name, num, type, symbol) \
    extern void *symbol

ERTS_FAST_BRANCHES_SYMBOLS

#undef ERTS_FAST_BRANCH_SYMBOL


void erts_pre_init_fast_branch()
{

#define ERTS_FAST_BRANCH_DECLARE(name, num)                             \
    __##name##_variable.branches = malloc(sizeof(ErtsFastBranch)*num);  \
    __##name##_variable.num_branches = num
ERTS_FAST_BRANCHES
#undef ERTS_FAST_BRANCH_DECLARE

#define ERTS_FAST_BRANCH_SYMBOL(name, num, type, symbol) \
    __##name##_variable.branches[num].type = symbol

ERTS_FAST_BRANCHES_SYMBOLS

#undef ERTS_FAST_BRANCH_SYMBOL

}
