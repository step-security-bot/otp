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

#define ERTS_FAST_BRANCH_START2(name, num)                              \
    __##num##_##name##_start:                                           \
    if (ERTS_UNLIKELY(!__##name##_variable[num].end)) {                 \
        erts_fast_branch_setup(__##name##_variable + num,               \
                                   &&__##num##_##name##_start,          \
                                   &&__##num##_##name##_trace,          \
                                   &&__##num##_##name##_end);           \
        goto __##num##_##name##_end;                                    \
    }                                                                   \
__##num##_##name##_trace:                                               \
    __attribute__((cold))

#define ERTS_FAST_BRANCH_START(name) ERTS_FAST_BRANCH_START2(name, 0)

#define ERTS_FAST_BRANCH_END2(name, num) __##num##_##name##_end: __attribute__((hot))

#define ERTS_FAST_BRANCH_END(name) ERTS_FAST_BRANCH_END2(name, 0)

#define ERTS_FAST_BRANCH_ENABLE(name) erts_fast_branch_enable(__##name##_variable, __##name##_variable_cnt)
#define ERTS_FAST_BRANCH_DISABLE(name) erts_fast_branch_disable(__##name##_variable, __##name##_variable_cnt)

typedef struct erts_fast_branch_ {
    erts_smp_atomic_t start;
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
    ERTS_FAST_BRANCH_DECLARE2(erts_system_monitor_long_schedule, 2);

/* Declare externs, variable defined in erl_fast_branch.c */
ERTS_FAST_BRANCHES

#endif
