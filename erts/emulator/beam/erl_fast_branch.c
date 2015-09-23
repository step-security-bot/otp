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

#include "erl_fast_branch.h"

#undef ERTS_FAST_BRANCH_DECLARE2
#define ERTS_FAST_BRANCH_DECLARE2(name, num)                            \
    ErtsFastBranch __##name##_variable[num];                            \
    int __##name##_variable_cnt = num

ERTS_FAST_BRANCHES

static void protect(void *ptr)
{
    if (mprotect((void*)((long)ptr & ~(0xfffl)), 0x1000,
                 PROT_READ | PROT_EXEC) < 0) {
        abort();
    }
    /* Clear the instruction cache */
    __clear_cache(ptr, ptr + 2);
}

static void unprotect(void *ptr)
{
    if (mprotect((void*)((long)ptr & ~(0xfffl)), 0x1000,
                 PROT_READ | PROT_WRITE | PROT_EXEC) < 0) {
        abort();
    }
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
           movq or compq that we area replacing in order
           for gdb disassemble to look nicer */
        if (fb->orig[5] == 0xe9) {
            ((byte *)&preq_val)[2] = 0x0F;
            ((byte *)&preq_val)[3] = 0x1F;
            ((byte *)&preq_val)[4] = 0x00;
        }
    } else {
        ((byte *)&preq_val)[0] = 0xe9;
        /* subtract 3 extra as jmpq is 3 bytes larger than jmp */
        *(Sint32*)(((byte *)&preq_val)+1) = addr_diff - 3;
    }
    *preq_ptr = preq_val;
    protect(start);
}

void erts_fast_branch_enable(ErtsFastBranch *fb, int number) {
    int i;
    for (i = 0; i < number; i++) {
        ErtsFastBranch *cfb = fb+i;
        update_jump_target(cfb, cfb->start, cfb->trace);
    }
}

void erts_fast_branch_disable(ErtsFastBranch *fb, int number) {
    int i;
    for (i = 0; i < number; i++) {
        ErtsFastBranch *cfb = fb+i;
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

#ifdef ERTS_FAST_BRANCH_SYMBOLS

#define ERTS_FAST_BRANCH_EXPAND_1_(name) ERTS_FAST_BRANCH_EXPAND(name, 0)
#define ERTS_FAST_BRANCH_EXPAND_2_(name) ERTS_FAST_BRANCH_EXPAND_1_(name) ERTS_FAST_BRANCH_EXPAND(name, 1)
#define ERTS_FAST_BRANCH_EXPAND_3_(name) ERTS_FAST_BRANCH_EXPAND_2_(name) ERTS_FAST_BRANCH_EXPAND(name, 2)
#define ERTS_FAST_BRANCH_EXPAND_4_(name) ERTS_FAST_BRANCH_EXPAND_3_(name) ERTS_FAST_BRANCH_EXPAND(name, 3)
#define ERTS_FAST_BRANCH_EXPAND_5_(name) ERTS_FAST_BRANCH_EXPAND_4_(name) ERTS_FAST_BRANCH_EXPAND(name, 4)
#define ERTS_FAST_BRANCH_EXPAND_6_(name) ERTS_FAST_BRANCH_EXPAND_5_(name) ERTS_FAST_BRANCH_EXPAND(name, 5)
#define ERTS_FAST_BRANCH_EXPAND_7_(name) ERTS_FAST_BRANCH_EXPAND_6_(name) ERTS_FAST_BRANCH_EXPAND(name, 6)
#define ERTS_FAST_BRANCH_EXPAND_8_(name) ERTS_FAST_BRANCH_EXPAND_7_(name) ERTS_FAST_BRANCH_EXPAND(name, 7)
#define ERTS_FAST_BRANCH_EXPAND_9_(name) ERTS_FAST_BRANCH_EXPAND_8_(name) ERTS_FAST_BRANCH_EXPAND(name, 8)

#undef ERTS_FAST_BRANCH_DECLARE2
#define ERTS_FAST_BRANCH_DECLARE2(name, num)    \
    ERTS_FAST_BRANCH_EXPAND_##num##_(name)

#define ERTS_FAST_BRANCH_EXPAND(name, num)      \
    extern void * __##num##_##name##_start;     \
    extern void * __##num##_##name##_trace;     \
    extern void * __##num##_##name##_end;

ERTS_FAST_BRANCHES

#undef ERTS_FAST_BRANCH_EXPAND

#endif

void erts_pre_init_fast_branch()
{
#ifdef ERTS_FAST_BRANCH_SYMBOLS

    int i;

#define ERTS_FAST_BRANCH_EXPAND(name, num)                      \
    __##name##_variable[num].start = __##num##_##name##_start;  \
    __##name##_variable[num].trace = __##num##_##name##_trace;  \
    __##name##_variable[num].end = __##num##_##name##_end;

ERTS_FAST_BRANCHES

/*
  Check that the start label is pointing to a 5 byte nop
 */
#undef ERTS_FAST_BRANCH_DECLARE2
#define ERTS_FAST_BRANCH_DECLARE2(name, num)                            \
    for (i = 0; i < num; i++) {                                         \
        byte *start = (byte*)__##name##_variable[i].start;              \
        if (start[0] == 0x0F && start[1] == 0x1F &&                     \
            start[2] == 0x44 && start[3] == 0x00 &&                     \
            start[4] == 0x00)                                           \
            continue;                                                   \
        abort();                                                        \
}

ERTS_FAST_BRANCHES

#endif

}
