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
            ((byte *)&preq_val)[2] = 0x90;
            ((byte *)&preq_val)[3] = 0x90;
            ((byte *)&preq_val)[4] = 0x90;
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
        void *start = (void*)erts_smp_atomic_read_dirty(&cfb->start);
        update_jump_target(cfb, start, cfb->trace);
    }
}

void erts_fast_branch_disable(ErtsFastBranch *fb, int number) {
    int i;
    for (i = 0; i < number; i++) {
        ErtsFastBranch *cfb = fb+i;
        void *start = (void*)erts_smp_atomic_read_dirty(&cfb->start);
        UWord *preq_ptr = (UWord*)start, preq_val = *preq_ptr;
        unprotect(start);
        /* Set all intructions to nob, i.e. fall through */
        ((byte *)&preq_val)[0] = 0x90;
        ((byte *)&preq_val)[1] = 0x90;
        ((byte *)&preq_val)[2] = 0x90;
        ((byte *)&preq_val)[3] = 0x90;
        ((byte *)&preq_val)[4] = 0x90;
        *preq_ptr = preq_val;
        protect(start);
    }
}

void erts_fast_branch_setup(ErtsFastBranch *fb, void *start,
                            void *trace, void *end) {
    ethr_sint_t prev = erts_smp_atomic_xchg_mb(&fb->start, (ethr_sint_t)start);
    if ((void*)prev == NULL) {
        fb->trace = trace;
        fb->end = end;
        if (((byte *)start)[0] == 0xe9 /* || ((byte *)start)[0] == 0xeb*/) {
            /* save original code layout for debugging */
            sys_memcpy(fb->orig, (char*)start - 4, sizeof(fb->orig));
            /* check that this is a cmpq or movq instruction */
            erts_fast_branch_disable(fb, 1);
            return;
        }
    } else {
        return;
    }
    abort();
}

void erts_pre_init_fast_branch()
{
    int i;
#undef ERTS_FAST_BRANCH_DECLARE2
#define ERTS_FAST_BRANCH_DECLARE2(name, num)                            \
    i = 0;                                                              \
    do {                                                                \
        erts_smp_atomic_init_nob(&__##name##_variable[i].start,         \
                                 (ethr_sint_t)NULL);                    \
        __##name##_variable[i].trace = NULL;                            \
        __##name##_variable[i].end = NULL;                              \
    } while(++i < num)

ERTS_FAST_BRANCHES

}
