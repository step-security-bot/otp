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

#ifndef ERL_FAST_BRANCH__
#define ERL_FAST_BRANCH__

#include "global.h"

/* This define with assembly and goto is here in order to attempt to
   influence gcc to layout code so that no branches are needed to be
   made when not branching. When the branch is disabled, the jmp in
   the asm below is replaced with just 8 nops, and since we then have
   a unconditional hot jump to _end, gcc will move the body of the _setup
   and _trace out of the way so that execution just falls through. e.g.

   0x000000000043d41b <process_main+59>:	jmpq   0x43d650 <_setup>
   0x000000000043d420 <process_main+64>:	nop
   0x000000000043d421 <process_main+65>:	nop
   0x000000000043d422 <process_main+66>:	nop
   0x000000000043d423 <process_main+67>:	_end code

   gets transformed to this when branch is disabled:

   0x000000000043d41b <process_main+59>:	nop
   0x000000000043d41c <process_main+60>:	nop
   0x000000000043d41d <process_main+61>:	nop
   0x000000000043d41e <process_main+62>:	nop
   0x000000000043d41f <process_main+63>:	nop
   0x000000000043d420 <process_main+64>:	nop
   0x000000000043d421 <process_main+65>:	nop
   0x000000000043d422 <process_main+66>:	nop
   0x000000000043d423 <process_main+67>:	_end code

   or this when branch is enabled:

   0x000000000043d41b <process_main+59>:	jmpq   0x43d650 <_trace>
   0x000000000043d420 <process_main+64>:	nop
   0x000000000043d421 <process_main+65>:	nop
   0x000000000043d422 <process_main+66>:	nop
   0x000000000043d423 <process_main+67>:	_end code

   to post process the .S file, the following section has to be added
   just before .Letext0

.LC1089y98yh:
        .globl	__0_erts_system_monitor_long_schedule
	.data
	.align 4
	.type	__0_erts_system_monitor_long_schedule, @object
	.size	__0_erts_system_monitor_long_schedule, 4
__0_erts_system_monitor_long_schedule:
	.long	.L0_erts_system_monitor_long_schedule_start


*/

#define FIVE_BYTE_NOP ".byte 0x0F\n\t.byte 0x1F\n\t.byte 0x44"  \
    "\n\t.byte 0x00\n\t.byte 0x00"

#if 0

#define ERTS_FAST_BRANCH_SYMBOLS 1

#define ERTS_FAST_BRANCH_START2(name, num)                              \
    __asm__ __volatile__ goto ("\n.L" #num "_" #name"_start:\n\t"       \
                               FIVE_BYTE_NOP                            \
                               :::: __##num##_##name##_trace);          \
    goto __##num##_##name##_end;                                        \
__##num##_##name##_trace:                                               \
    __asm__ __volatile__("\n.L" #num "_" #name"_trace:")

#define ERTS_FAST_BRANCH_END2(name, num)                                \
__##num##_##name##_end:                                                 \
    __asm__ __volatile__ ("\n.L" #num "_" #name"_end:")

#elif 1

#define ERTS_FAST_BRANCH_START2(name, num)      \
    goto __##num##_##name##_end
#define ERTS_FAST_BRANCH_END2(name, num) __##num##_##name##_end:

#else

#define ERTS_FAST_BRANCH_START2(name, num)                      \
    if (!__##name##_variable[num].end)                          \
        goto __##num##_##name##_end
#define ERTS_FAST_BRANCH_END2(name, num) __##num##_##name##_end:


#endif

#define ERTS_FAST_BRANCH_START(name) ERTS_FAST_BRANCH_START2(name, 0)
#define ERTS_FAST_BRANCH_END(name) ERTS_FAST_BRANCH_END2(name, 0)

#define ERTS_FAST_BRANCH_ENABLE(name) erts_fast_branch_enable(__##name##_variable, __##name##_variable_cnt)
#define ERTS_FAST_BRANCH_DISABLE(name) erts_fast_branch_disable(__##name##_variable, __##name##_variable_cnt)

typedef struct erts_fast_branch_ {
    void *start;
    void *trace;
    void *end;
    byte orig[12];
} ErtsFastBranch;

void erts_pre_init_fast_branch(void);
void erts_fast_branch_setup(ErtsFastBranch *, void *, void *, void *);
void erts_fast_branch_enable(ErtsFastBranch *, int);
void erts_fast_branch_disable(ErtsFastBranch *, int);

#define ERTS_FAST_BRANCH_DECLARE(name) ERTS_FAST_BRANCH_DECLARE2(name, 1)
#define ERTS_FAST_BRANCH_DECLARE2(name, num)            \
    extern ErtsFastBranch __##name##_variable[num];     \
    extern int __##name##_variable_cnt

/* Declaration */
#define ERTS_FAST_BRANCHES \
    ERTS_FAST_BRANCH_DECLARE2(erts_system_monitor_long_schedule, 2); \
    ERTS_FAST_BRANCH_DECLARE2(msacc_bif, 6); \
    ERTS_FAST_BRANCH_DECLARE2(msacc_port, 1);


/* Declare externs, variable defined in erl_fast_branch.c */
ERTS_FAST_BRANCHES

#endif
