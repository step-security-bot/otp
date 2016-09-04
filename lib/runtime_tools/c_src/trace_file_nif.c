/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011-2016. All Rights Reserved.
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
 * Purpose:  Dynamically loadable NIF library for DTrace
 */

#include "erl_nif.h"
#include "config.h"
#include "sys.h"
#include "global.h"
#include "erl_process.h"
#include <stdio.h>

#include <x86intrin.h>

/*
  set $i = 0
  set $cnt = 0
  set $dropped = 0
  while $i < 8
  printf "%d: %d + %d = %d\r\n",$i,q[$i].cnt,q[$i].dropped,q[$i].cnt + q[$i].dropped
  set $cnt += q[$i].cnt
  set $dropped += q[$i].dropped
  set $i++
  end
  printf "total: %d + %d = %d\r\n",$cnt,$dropped,$cnt + $dropped
*/


/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

static ERL_NIF_TERM start(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enabled_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"start", 1, start},
    {"stop", 1, stop},
    {"trace_call", 5, trace_call},
    {"enabled_call", 3, enabled_call},
    {"enabled", 3, enabled},
    {"trace", 5, trace}
};


ERL_NIF_INIT(trace_file_nif, nif_funcs, load, NULL, NULL, NULL);

#define ATOMS                                      \
    ATOM_DECL(true);                               \
    ATOM_DECL(discard);                            \
    ATOM_DECL(trace);                              \
    ATOM_DECL(ok);

#define ATOM_DECL(A) static ERL_NIF_TERM atom_##A
ATOMS
#undef ATOM_DECL

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

#define ATOM_DECL(A) atom_##A = enif_make_atom(env, #A)
ATOMS
#undef ATOM_DECL

    *priv_data = NULL;

    return 0;
}

#define QUEUE_SIZE (1024*1024)

typedef struct mfa {
    ERL_NIF_TERM m;
    ERL_NIF_TERM f;
    ERL_NIF_TERM a;
} MFA;

typedef struct trace_queue {
    volatile ErlNifUInt64 head;
    volatile ErlNifUInt64 tail;
    ErlNifUInt64 dropped;
    ErlNifUInt64 cnt;
    char *__align[64 - sizeof(ErlNifUInt64) * 4];
    MFA q[QUEUE_SIZE];
    char *__align2[64 - ((sizeof(MFA) * QUEUE_SIZE) % 64)];
} TraceQueue;

static ErlNifSysInfo sys_info;
static ethr_event evt;
static ErlNifTid tid;
static TraceQueue *q = NULL;
static int flush = 0;
static ErlNifPid flush_pid;

void *loop(void *arg);

void *loop(void *arg)
{
    int schedulers = sys_info.scheduler_threads;
    FILE *fd = fopen("/tmp/log.trace","w");
    ethr_event_init(&evt);
    while (1) {
        int found = 1, i;
        ethr_event_wait(&evt);
        while (found) {
            found = 0;
            for (i = 0; i < schedulers; i++) {
                ErlNifUInt64 head = q[i].head, tail = q[i].tail;
                if (head < tail) {
                    int elems = tail - head;
                    size_t res;
                    if ((head + elems) / QUEUE_SIZE != head / QUEUE_SIZE)
                        elems = QUEUE_SIZE - head % QUEUE_SIZE;
                    /* needs memory barrier? */
                    __sync_synchronize();
                    res = fwrite((void*)q[i].q+head % QUEUE_SIZE,
                                 sizeof(MFA), elems, fd);
                    if (elems != res) {
                        printf("write failed %d\r\n", ferror(fd));
                        abort();
                    }
                    q[i].head += elems;
                    found = 1;
                }
            }
        }
        ethr_event_reset(&evt);
        __sync_synchronize();
        if (flush) {
            ErlNifEnv *env = enif_alloc_env();
            fflush(fd);
            enif_send(env, &flush_pid, NULL, atom_ok);
            flush = 0;
        }
    }
}
// trace_file_nif:start(ok),trace_file_nif:trace_call(call, self(), self(), {a,b,1}, #{}).

static ERL_NIF_TERM start(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (q == NULL) {
        enif_system_info(&sys_info, sizeof(ErlNifSysInfo));
        q = enif_alloc(sizeof(TraceQueue) * sys_info.scheduler_threads);
        memset(q, 0, sizeof(TraceQueue) * sys_info.scheduler_threads);
        enif_thread_create("trace_file_nif", &tid, loop, NULL, NULL);
    }
    return atom_ok;
}

static ERL_NIF_TERM stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_self(env, &flush_pid);
    flush = 1;
    ethr_event_set(&evt);
    return atom_ok;
}

static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return atom_discard;
}

static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return atom_trace;
}

static ERL_NIF_TERM enabled_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Uint scheduler_id = env->proc->scheduler_data->no - 1;
    if (q[scheduler_id].tail - q[scheduler_id].head == QUEUE_SIZE) {
        q[scheduler_id].dropped++;
        return atom_discard;
    }
    return atom_trace;
}

static ERL_NIF_TERM trace_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM *mfa;
    int arity;
    Uint scheduler_id = erts_get_scheduler_data()->no - 1;
    enif_get_tuple(env, argv[3], &arity, &mfa);

    /* needs memory barrier? */
    __sync_synchronize();

    memcpy(q[scheduler_id].q+(q[scheduler_id].tail % QUEUE_SIZE),
           mfa, sizeof(MFA));
    q[scheduler_id].tail++;
    q[scheduler_id].cnt++;

    if (q[scheduler_id].tail - q[scheduler_id].head > 10)
        ethr_event_set(&evt);

    return atom_ok;
}
