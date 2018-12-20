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
#include <string.h>

#include "erl_nif.h"
#include "erl_driver.h"
#include "config.h"
#include <stdio.h>

#include <x86intrin.h>

/* THIS IS CHEATING!!! */
typedef unsigned long Uint;
typedef struct ErtsSchedulerData_ {
    Uint no;
} ErtsSchedulerData;
ErtsSchedulerData *erts_get_scheduler_data(void);

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

static ERL_NIF_TERM open_archive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM close_archive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM start_tracer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM stop_tracer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
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
    {"open_archive", 2, open_archive},
    {"start_tracer", 1, start_tracer},
    {"stop_tracer", 1, stop_tracer},
    {"close_archive", 3, close_archive},
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

static ErlNifSysInfo sys_info;
static ErlNifResourceType *tracer_resource;
static struct stat st;

static void tracer_resource_dtor(ErlNifEnv* env, void* obj);

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
    TRACE_GC_MAJOR_END = 10,
    TRACE_DROPPED = 11
} TraceType;

typedef struct mfa {
    ERL_NIF_TERM m;
    ERL_NIF_TERM f;
    ERL_NIF_TERM a;
} MFA;

typedef struct entry {
    TraceType type;
    ERL_NIF_TERM pid;
    ErlNifTime ts;
    union {
        MFA mfa;
        int dropped;
    } u;
} TraceEntry;

typedef struct trace_queue {
    volatile ErlNifUInt64 head;
    volatile ErlNifUInt64 tail;
    ErlNifUInt64 dropped;
    char *__align[64 - sizeof(ErlNifUInt64) * 4];
    TraceEntry q[QUEUE_SIZE];
    char *__align2[64 - ((sizeof(TraceEntry) * QUEUE_SIZE) % 64)];
} TraceQueue;

typedef struct tracer {
    ErlDrvCond *cond;
    ErlDrvMutex *mtx;
    ErlNifTid tid;
    TraceQueue *q;
    int flush;
    struct archive *a;

    /* Used during startup */
    ErlNifEnv *env;
    ErlNifPid pid;
    ERL_NIF_TERM ref;
} Tracer;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

#define ATOM_DECL(A) atom_##A = enif_make_atom(env, #A)
ATOMS
#undef ATOM_DECL

    *priv_data = NULL;

    enif_system_info(&sys_info, sizeof(ErlNifSysInfo));

    tracer_resource = enif_open_resource_type(
        env, NULL, "tracer_resource",
        &tracer_resource_dtor,
        ERL_NIF_RT_CREATE,
        NULL
        );

    memset(&st, 0, sizeof(st));
    st.st_mode = S_IFREG;
    st.st_size = sizeof(TraceEntry) * QUEUE_SIZE;

    return 0;
}

static void tracer_resource_dtor(ErlNifEnv* env, void* obj)
{
    Tracer *tracer = (Tracer*)obj;
    if (!tracer->flush) {
        /* dtor called without flush first being called,
           tell tracer thread to stop and cleanup */
        if (tracer->mtx != NULL) {
            erl_drv_mutex_lock(tracer->mtx);
            tracer->flush = 2;
            erl_drv_cond_signal(tracer->cond);
            erl_drv_mutex_unlock(tracer->mtx);
        }
        enif_free_env(tracer->env);
    } else {
        enif_free(tracer->q);
    }
}

void *loop(void *arg);
void *loop(void *arg)
{
    Tracer *tracer = (Tracer*)arg;
    int schedulers = sys_info.scheduler_threads;
    struct archive_entry *entry;
    char tracename[255];
    int file_cnt = 1;
    int page_cnt = 0;
    unsigned int cnt = 0;

    /* tracer may be de-allocated while we are running,
       so use local copies of q and a */
    TraceQueue *q = tracer->q;
    struct archive *a = tracer->a;
    ErlDrvCond *cond = tracer->cond;
    ErlDrvMutex *mtx = tracer->mtx;

    erl_drv_mutex_lock(mtx);

    entry = archive_entry_new();
    archive_entry_copy_stat(entry, &st);
    archive_entry_set_pathname(entry, "trace.log.0");
    archive_write_header(a, entry);

    enif_send(NULL, &tracer->pid, tracer->env,
              enif_make_tuple2(tracer->env, enif_make_atom(tracer->env, "started"),
                               enif_make_resource(tracer->env, tracer)));
    enif_clear_env(tracer->env);

    while (1) {
        int found = 1, i;
        erl_drv_cond_wait(cond, mtx);
        while (found) {
            found = 0;
            for (i = 0; i < schedulers; i++) {
                ErlNifUInt64 head = q[i].head, tail = q[i].tail;
                if (head < tail) {
                    int elems = tail - head;
                    TraceEntry *curr_q = q[i].q+(head % QUEUE_SIZE);
                    if ((head + elems) / QUEUE_SIZE != head / QUEUE_SIZE)
                        elems = QUEUE_SIZE - head % QUEUE_SIZE;

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

        __sync_synchronize();
        if (tracer->flush) {
            archive_entry_free(entry);
            erl_drv_mutex_unlock(mtx);
            if (tracer->flush == 1) {
                enif_send(NULL, &tracer->pid, tracer->env,
                          enif_make_tuple2(tracer->env, enif_make_atom(tracer->env, "stopped"),
                                           enif_make_resource(tracer->env, tracer)));

                enif_free_env(tracer->env);
            } else if (tracer->flush == 2) {
                /* The resource dtor was called before flush,
                   close archive and de-allocate queue */
                erl_drv_mutex_destroy(mtx);
                erl_drv_cond_destroy(cond);
                archive_write_free(a);
                enif_free(q);
            }

            enif_thread_exit(0);
        }
    }
}
// trace_file_nif:start(ok),trace_file_nif:trace_call(call, self(), self(), {a,b,1}, #{}).

static ERL_NIF_TERM open_archive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary filename, filter;
    Tracer *tracer;
    ERL_NIF_TERM ret, options, option, filter_term;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &filename) ||
        !enif_get_map_value(env, argv[1], enif_make_atom(env, "filter"), &filter_term) ||
        !enif_inspect_iolist_as_binary(env, filter_term, &filter) ||
        !enif_get_map_value(env, argv[1], enif_make_atom(env, "filter_options"), &options))
        return enif_make_badarg(env);

    tracer = enif_alloc_resource(tracer_resource, sizeof(Tracer));

    memset(tracer, 0, sizeof(*tracer));

    tracer->env = enif_alloc_env();

    tracer->a = archive_write_new();
    if (archive_write_add_filter_by_name(tracer->a, (char*)filter.data) < ARCHIVE_OK) {
        goto archive_error;
    }

    while (enif_get_list_cell(env, options, &option, &options)) {
        ErlNifBinary key, val;
        const ERL_NIF_TERM *terms;
        int length;
        if (!enif_get_tuple(env, option, &length, &terms) || length != 2 ||
            !enif_inspect_iolist_as_binary(env, terms[0], &key) ||
            !enif_inspect_iolist_as_binary(env, terms[1], &val))
            goto badarg;

        if (archive_write_set_filter_option(tracer->a, (char*)filter.data,
                                            (char*)key.data, (char*)val.data) < ARCHIVE_OK)
            goto archive_error;
    }

    if (archive_write_set_format_ustar(tracer->a) < ARCHIVE_OK ||
        archive_write_open_filename(tracer->a, (char*)filename.data) < ARCHIVE_OK)
        goto archive_error;

    ret = enif_make_resource(env, tracer);
    enif_release_resource(tracer);

    return ret;

archive_error: {
        ERL_NIF_TERM str = enif_make_string(env, archive_error_string(tracer->a), ERL_NIF_LATIN1);
        enif_release_resource(tracer);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), str);
    }

badarg:
    enif_release_resource(tracer);
    return enif_make_badarg(env);
}

static ERL_NIF_TERM start_tracer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Tracer *tracer;
    if (!enif_get_resource(env, argv[0], tracer_resource, (void**)&tracer))
        return enif_make_badarg(env);

    tracer->q = enif_alloc(sizeof(TraceQueue) * sys_info.scheduler_threads);
    memset(tracer->q, 0, sizeof(TraceQueue) * sys_info.scheduler_threads);

    tracer->cond = erl_drv_cond_create("trace_file_nif_cond");
    tracer->mtx = erl_drv_mutex_create("trace_file_nif_mtx");

    enif_self(env, &tracer->pid);

    enif_thread_create("trace_file_nif", &tracer->tid, loop, tracer, NULL);

    return argv[0];
}

static ERL_NIF_TERM stop_tracer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Tracer *tracer;
    if (!enif_get_resource(env, argv[0], tracer_resource, (void**)&tracer))
        return enif_make_badarg(env);

    /* Should this mutex lock be done on a dirty scheduler? probably */
    erl_drv_mutex_lock(tracer->mtx);
    enif_self(env, &tracer->pid);
    tracer->flush = 1;
    erl_drv_cond_signal(tracer->cond);
    erl_drv_mutex_unlock(tracer->mtx);

    return argv[0];
}

static ERL_NIF_TERM close_archive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Tracer *tracer;
    ERL_NIF_TERM hd, tl;
    char atom_buf[255];
    struct archive_entry *entry;

    if (!enif_get_resource(env, argv[0], tracer_resource, (void**)&tracer) ||
        !enif_get_atom(env, argv[1], atom_buf, sizeof(atom_buf), ERL_NIF_LATIN1) ||
        !enif_get_list_cell(env, argv[2], &hd, &tl))
        return enif_make_badarg(env);

    /* Write info about layout of TraceEntry struct */
    entry = archive_entry_new();
    archive_entry_copy_stat(entry, &st);
    archive_entry_set_pathname(entry, "meta");
    archive_write_header(tracer->a, entry);

//    archive_write_int(tracer->a, "ERL_NIF_TERM", sizeof(ERL_NIF_TERM));
//    archive_write_int(tracer->a, "ERL_NIF_TERM", sizeof(ERL_NIF_TERM));

    archive_entry_free(entry);

    /* Dump all atoms */
    entry = archive_entry_new();
    archive_entry_copy_stat(entry, &st);
    archive_entry_set_pathname(entry, "atoms");
    archive_write_header(tracer->a, entry);

    while (!enif_is_empty_list(env, hd)) {
        if (enif_get_atom(env, hd, atom_buf, sizeof(atom_buf), ERL_NIF_LATIN1)) {
            ErlNifUInt64 len = strlen(atom_buf);
            archive_write_data(tracer->a, &hd, sizeof(hd));
            archive_write_data(tracer->a, &len, sizeof(len));
            archive_write_data(tracer->a, atom_buf, len);
        }
        enif_get_list_cell(env, tl, &hd, &tl);
    }

    archive_entry_free(entry);

    archive_write_free(tracer->a);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return atom_discard;
}

static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return atom_trace;
}

static ERL_NIF_TERM check_queue(ErlNifEnv* env, ERL_NIF_TERM tracer_term) {
    Uint scheduler_id = erts_get_scheduler_data()->no - 1;
    Tracer *tracer;

    if (!enif_get_resource(env, tracer_term, tracer_resource, (void**)&tracer))
        return atom_discard;

    if (tracer->q[scheduler_id].tail - tracer->q[scheduler_id].head == QUEUE_SIZE) {
        tracer->q[scheduler_id].dropped++;
        return atom_discard;
    }
    return atom_trace;
}

static ERL_NIF_TERM enabled_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return check_queue(env, argv[1]);
}

static ERL_NIF_TERM enabled_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return check_queue(env, argv[1]);
}

static ERL_NIF_TERM enabled_return_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return check_queue(env, argv[1]);
}

static ERL_NIF_TERM enabled_running_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return check_queue(env, argv[1]);
}

static ERL_NIF_TERM enabled_garbage_collection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return check_queue(env, argv[1]);
}

static void enqueue(ErlNifEnv* env, ERL_NIF_TERM tracer_term,
                    TraceType type, ERL_NIF_TERM pid, const ERL_NIF_TERM *mfa) {
    Uint scheduler_id = erts_get_scheduler_data()->no - 1;
    TraceEntry *te;
    Tracer *tracer;
    TraceQueue *q;

    if (!enif_get_resource(env, tracer_term, tracer_resource, (void**)&tracer))
        return;

    q = &tracer->q[scheduler_id];

    if (q->dropped) {
        te = q->q+(q->tail % QUEUE_SIZE);
        te->ts = enif_monotonic_time(ERL_NIF_NSEC);
        te->type = TRACE_DROPPED;
        te->pid = pid;
        te->u.dropped = q->dropped;
        q->dropped = 0;
        q->tail++;
    }

    te = q->q+(q->tail % QUEUE_SIZE);
    te->ts = enif_monotonic_time(ERL_NIF_NSEC);
    te->pid = pid;
    te->type = type;

    if (mfa) memcpy(&te->u.mfa, mfa, sizeof(MFA));
    else memset(&te->u.mfa, 0, sizeof(MFA));

    q->tail++;

    if (q->tail % (QUEUE_SIZE / 8)  == 0)
        erl_drv_cond_signal(tracer->cond);
}

static ERL_NIF_TERM trace_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM *mfa;
    int arity;

    enif_get_tuple(env, argv[3], &arity, &mfa);

    enqueue(env, argv[1], TRACE_CALL, argv[2], mfa);

    return atom_ok;
}

static ERL_NIF_TERM trace_garbage_collection(ErlNifEnv* env, int argc,
                                             const ERL_NIF_TERM argv[])
{
    TraceType type = TRACE_GC_MAJOR_START;
    if (enif_is_identical(argv[0], atom_gc_major_start)) {
        type = TRACE_GC_MAJOR_START;
    } else if (enif_is_identical(argv[0], atom_gc_major_end)) {
        type = TRACE_GC_MAJOR_END;
    } else if (enif_is_identical(argv[0], atom_gc_minor_start)) {
        type = TRACE_GC_MINOR_START;
    } else if (enif_is_identical(argv[0], atom_gc_minor_end)) {
        type = TRACE_GC_MINOR_END;
    }

    enqueue(env, argv[1], type, argv[2], NULL);

    return atom_ok;
}

static ERL_NIF_TERM trace_return_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM *mfa;
    int arity;

    enif_get_tuple(env, argv[3], &arity, &mfa);

    enqueue(env, argv[1], TRACE_RETURN_TO, argv[2], mfa);

    return atom_ok;
}

static ERL_NIF_TERM trace_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    if (enif_is_identical(argv[0], atom_spawned)) {
        ERL_NIF_TERM value;
        const ERL_NIF_TERM *mfa;
        int arity;

        enif_get_map_value(env, argv[4], atom_extra, &value);
        enif_get_tuple(env, value, &arity, &mfa);

        enqueue(env, argv[1], TRACE_SPAWNED, argv[2], mfa);
    } else if (enif_is_identical(argv[0], atom_exit)) {
        enqueue(env, argv[1], TRACE_EXIT, argv[2], NULL);
    }

    return atom_ok;
}

static ERL_NIF_TERM trace_running_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    TraceType type = TRACE_IN;
    const ERL_NIF_TERM *mfa = NULL;
    int arity;

    if (enif_is_identical(argv[0], atom_in)) {
        type = TRACE_IN;
    } else if (enif_is_identical(argv[0], atom_out)) {
        type = TRACE_OUT;
    }

    enif_get_tuple(env, argv[3], &arity, &mfa);

    enqueue(env, argv[1], type, argv[2], mfa);

    return atom_ok;
}
