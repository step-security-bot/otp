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

#include <archive.h>
#include <archive_entry.h>

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
static ERL_NIF_TERM enabled_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enabled_return_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enabled_running_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enabled_garbage_collection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_garbage_collection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_return_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_running_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"start", 1, start},
    {"stop", 1, stop},
    {"trace_call", 5, trace_call},
    {"enabled_call", 3, enabled_call},
    {"trace_garbage_collection", 5, trace_garbage_collection},
    {"enabled_garbage_collection", 3, enabled_garbage_collection},
    {"trace_return_to", 5, trace_return_to},
    {"enabled_return_to", 3, enabled_return_to},
    {"trace_procs", 5, trace_procs},
    {"enabled_procs", 3, enabled_procs},
    {"trace_running_procs", 5, trace_running_procs},
    {"enabled_running_procs", 3, enabled_running_procs},
    {"enabled", 3, enabled},
    {"trace", 5, trace}
};


ERL_NIF_INIT(trace_file_nif, nif_funcs, load, NULL, NULL, NULL);

#define ATOMS                   \
    ATOM_DECL(true);            \
    ATOM_DECL(discard);         \
    ATOM_DECL(extra);           \
    ATOM_DECL(spawned);         \
    ATOM_DECL(exit);            \
    ATOM_DECL(gc_minor_start);  \
    ATOM_DECL(gc_minor_end);    \
    ATOM_DECL(gc_major_start);  \
    ATOM_DECL(gc_major_end);    \
    ATOM_DECL(trace);           \
    ATOM_DECL(out);             \
    ATOM_DECL(in);              \
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

typedef enum {
    TRACE_NONE = 0,
    TRACE_CALL = 1,
    TRACE_RETURN_TO = 2,
    TRACE_SPAWNED = 3,
    TRACE_EXIT = 4,
    TRACE_IN = 5,
    TRACE_OUT = 6,
    TRACE_GC_MINOR_START = 7,
    TRACE_GC_MINOR_END = 8,
    TRACE_GC_MAJOR_START = 9,
    TRACE_GC_MAJOR_END = 10
} TraceType;

typedef struct mfa {
    ERL_NIF_TERM m;
    ERL_NIF_TERM f;
    ERL_NIF_TERM a;
} MFA;

typedef struct entry {
    TraceType type;
    ERL_NIF_TERM pid;
    ERL_NIF_TERM ts;
    MFA mfa;
} TraceEntry;

typedef struct trace_queue {
    volatile ErlNifUInt64 head;
    volatile ErlNifUInt64 tail;
    ErlNifUInt64 dropped;
    char *__align[64 - sizeof(ErlNifUInt64) * 4];
    TraceEntry q[QUEUE_SIZE];
    char *__align2[64 - ((sizeof(TraceEntry) * QUEUE_SIZE) % 64)];
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
    struct archive *a;
    struct archive_entry *entry;
    const char *filename = "log.tar.gz";
    char tracename[255];
    struct stat st;
    int file_cnt = 1;
    int page_cnt = 0;
    unsigned int cnt = 0;
    ErlNifEnv *env = enif_alloc_env();

    a = archive_write_new();
    archive_write_add_filter_gzip(a);
    archive_write_set_format_ustar(a);
    archive_write_open_filename(a, filename);
    memset(&st, 0, sizeof(st));
    st.st_mode = S_IFREG;
    st.st_size = sizeof(TraceEntry) * QUEUE_SIZE;
    entry = archive_entry_new();
    archive_entry_copy_stat(entry, &st);
    archive_entry_set_pathname(entry, "trace.log.0");
    archive_write_header(a, entry);

    ethr_event_init(&evt);

    enif_send(env, &flush_pid, NULL, atom_ok);
    enif_free_env(env);

    while (1) {
        int found = 1, i;
        ethr_event_wait(&evt);
        while (found) {
            found = 0;
            for (i = 0; i < schedulers; i++) {
                ErlNifUInt64 head = q[i].head, tail = q[i].tail;
                if (head < tail) {
                    int elems = tail - head;
                    TraceEntry *curr_q = q[i].q+(head % QUEUE_SIZE);
                    if ((head + elems) / QUEUE_SIZE != head / QUEUE_SIZE)
                        elems = QUEUE_SIZE - head % QUEUE_SIZE;
                    /* needs memory barrier? */
                    __sync_synchronize();
                    if (page_cnt + elems > QUEUE_SIZE)
                        elems = QUEUE_SIZE - page_cnt;
                    cnt += archive_write_data(a, curr_q , sizeof(TraceEntry) * elems);
                    page_cnt += elems;
                    if (page_cnt == QUEUE_SIZE) {
                        archive_entry_free(entry);
                        entry = archive_entry_new();
                        archive_entry_copy_stat(entry, &st);
                        sprintf(tracename, "trace.log.%d", file_cnt++);
                        archive_entry_set_pathname(entry, tracename);
                        archive_write_header(a, entry);
                        page_cnt = 0;
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
            archive_entry_free(entry);

            entry = archive_entry_new();
            archive_entry_copy_stat(entry, &st);
            archive_entry_set_pathname(entry, "atoms");
            archive_write_header(a, entry);

            i = atom_table_size();
            while (--i >= 0) {
                Atom *ap = atom_tab(i);
                ERL_NIF_TERM atom = make_atom(i);
                archive_write_data(a, &atom, sizeof(atom));
                archive_write_data(a, &ap->len, sizeof(ap->len));
                archive_write_data(a, ap->name, ap->len);
            }

            archive_entry_free(entry);
            archive_write_free(a);
            enif_send(env, &flush_pid, NULL, atom_ok);
            enif_free(q);
            q = NULL;
            flush = 0;
            enif_thread_exit(0);
        }
    }
}
// trace_file_nif:start(ok),trace_file_nif:trace_call(call, self(), self(), {a,b,1}, #{}).

static ERL_NIF_TERM start(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (q == NULL) {
        enif_self(env, &flush_pid);
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

static ERL_NIF_TERM check_queue(ErlNifEnv* env) {
    Uint scheduler_id = erts_get_scheduler_data()->no - 1;
    if (q[scheduler_id].tail - q[scheduler_id].head == QUEUE_SIZE) {
        q[scheduler_id].dropped++;
        return atom_discard;
    }
    return atom_trace;
}

static ERL_NIF_TERM enabled_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return check_queue(env);
}

static ERL_NIF_TERM enabled_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return check_queue(env);
}

static ERL_NIF_TERM enabled_return_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return check_queue(env);
}

static ERL_NIF_TERM enabled_running_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return check_queue(env);
}

static ERL_NIF_TERM enabled_garbage_collection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return check_queue(env);
}

static void enqueue(ErlNifEnv* env, TraceType type, ERL_NIF_TERM pid, const ERL_NIF_TERM *mfa) {
    Uint scheduler_id = erts_get_scheduler_data()->no - 1;
    TraceEntry *te;

    /* needs memory barrier? */
    __sync_synchronize();

    te = q[scheduler_id].q+(q[scheduler_id].tail % QUEUE_SIZE);
    te->ts = enif_monotonic_time(ERL_NIF_NSEC);
    te->type = type;
    te->pid = pid;
    if (mfa)
        memcpy(&te->mfa, mfa, sizeof(MFA));
    else
        memset(&te->mfa, 0, sizeof(MFA));

    q[scheduler_id].tail++;

    if (q[scheduler_id].tail - q[scheduler_id].head > 10)
        ethr_event_set(&evt);

}

static ERL_NIF_TERM trace_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM *mfa;
    int arity;

    enif_get_tuple(env, argv[3], &arity, &mfa);

    enqueue(env, TRACE_CALL, argv[2], mfa);

    return atom_ok;
}

static ERL_NIF_TERM trace_garbage_collection(ErlNifEnv* env, int argc,
                                             const ERL_NIF_TERM argv[])
{
    TraceType type = TRACE_GC_MAJOR_START;
    if (enif_is_identical(argv[1], atom_gc_major_start)) {
        type = TRACE_GC_MAJOR_START;
    } else if (enif_is_identical(argv[1], atom_gc_major_end)) {
        type = TRACE_GC_MAJOR_END;
    } else if (enif_is_identical(argv[1], atom_gc_minor_start)) {
        type = TRACE_GC_MINOR_START;
    } else if (enif_is_identical(argv[1], atom_gc_minor_end)) {
        type = TRACE_GC_MINOR_END;
    }

    enqueue(env, type, argv[2], NULL);

    return atom_ok;
}

static ERL_NIF_TERM trace_return_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM *mfa;
    int arity;

    enif_get_tuple(env, argv[3], &arity, &mfa);

    enqueue(env, TRACE_RETURN_TO, argv[2], mfa);

    return atom_ok;
}

static ERL_NIF_TERM trace_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    if (enif_is_identical(argv[1], atom_spawned)) {
        ERL_NIF_TERM value;
        const ERL_NIF_TERM *mfa;
        int arity;

        enif_get_map_value(env, argv[4], atom_extra, &value);
        enif_get_tuple(env, value, &arity, &mfa);

        enqueue(env, TRACE_SPAWNED, argv[2], mfa);
    } else if (enif_is_identical(argv[1], atom_exit)) {
        enqueue(env, TRACE_EXIT, argv[2], NULL);
    }

    return atom_ok;
}

static ERL_NIF_TERM trace_running_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    TraceType type = TRACE_IN;
    const ERL_NIF_TERM *mfa = NULL;
    int arity;

    if (enif_is_identical(argv[1], atom_in)) {
        type = TRACE_IN;
    } else if (enif_is_identical(argv[1], atom_out)) {
        type = TRACE_OUT;
    }

    enif_get_tuple(env, argv[3], &arity, &mfa);

    enqueue(env, type, argv[2], mfa);

    return atom_ok;
}
