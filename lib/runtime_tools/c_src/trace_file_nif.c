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
 *
 * TODO: Tracing on dirty schedulers
 */

#ifdef HAVE_ZSTD
#include <zstd.h>
#endif
#include <string.h>

#include "erl_nif.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

static ERL_NIF_TERM start_tracer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM flush_tracer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM close_tracer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM get_tracer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM uncompress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
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
    {"start_tracer", 2, start_tracer, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"flush_tracer", 1, flush_tracer, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"close_tracer", 4, close_tracer, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"get_tracer", 1, get_tracer},
    {"uncompress", 1, uncompress, ERL_NIF_DIRTY_JOB_CPU_BOUND},
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
    ATOM_DECL(flush);           \
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
/* Number of normal schedulers + 1 queue for all other threads */
#define NUM_TRACE_QUEUES (sys_info.scheduler_threads+1)
static ErlNifResourceType *tracer_resource;

static void tracer_resource_dtor(ErlNifEnv* env, void* obj);

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

static char *type2str(TraceType type) {

    switch (type) {
    case TRACE_NONE: return "NONE";
    case TRACE_CALL: return "CALL";
    case TRACE_RETURN_TO: return "RETURN_TO";
    case TRACE_SPAWNED: return "SPAWNED";
    case TRACE_EXIT: return "EXIT";
    case TRACE_IN: return "IN";
    case TRACE_OUT: return "OUT";
    case TRACE_GC_MINOR_START: return "GC_MINOR_START";
    case TRACE_GC_MINOR_END: return "GC_MINOR_END";
    case TRACE_GC_MAJOR_START: return "GC_MAJOR_START";
    case TRACE_GC_MAJOR_END: return "GC_MAJOR_EWND";
    case TRACE_DROPPED: return "DROPPED";
    }
    return "unkown";
}

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
        ErlNifSInt64 dropped;
    } u;
} TraceEntry;

typedef struct trace_queue {
    volatile ErlNifUInt64 head;
    volatile ErlNifUInt64 tail;
    ErlNifUInt64 dropped;
    TraceEntry q[];
} TraceQueue;

typedef struct tracer {
#ifdef HAVE_ZSTD
    ZSTD_CStream *stream;
    ZSTD_outBuffer out;
    int file;
#else
    FILE *file;
#endif
    int level;
    ErlNifMutex *dirty_mtx;
    ErlNifUInt64 queue_size;
    ErlNifUInt64 queue_mask;
    ErlNifPid pid;
    TraceQueue *q[];
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


    return 0;
}

static void tracer_resource_dtor(ErlNifEnv* env, void* obj)
{
    Tracer *tracer = (Tracer*)obj;
    int i;
#ifdef HAVE_ZSTD
    if (tracer->file != -1) {
        ZSTD_freeCStream(tracer->stream);
        enif_free(tracer->out.dst);
        enif_mutex_destroy(tracer->dirty_mtx);
        close(tracer->file);
        tracer->file = -1;
        for (i = 0; i < NUM_TRACE_QUEUES; i++)
            enif_free(tracer->q[i]);
    }
#else
    if (tracer->file != NULL) {
        enif_mutex_destroy(tracer->dirty_mtx);
        fclose(tracer->file);
        tracer->file = NULL;
        for (i = 0; i < NUM_TRACE_QUEUES; i++)
            enif_free(tracer->q[i]);
    }
#endif
}

#ifdef HAVE_ZSTD
static ssize_t write_outbuffer(int fd, ZSTD_outBuffer *out)
{
    ssize_t wres;
    /* Write out buffer to disk */
    do {
        wres = write(fd, out->dst, out->pos);
    } while (wres == -1 && errno == EINTR);
    out->pos = 0;
    return wres;
}

static ssize_t end_stream(ZSTD_CStream *stream, ZSTD_outBuffer *out, int fd)
{
    size_t res = 1;
    ssize_t wres_acc = 0, wres;
    while (res != 0) {
        res = ZSTD_endStream(stream, out);

        if (ZSTD_isError(res)) {
            fprintf(stderr,"ZSTD endStream error: %s\n",
                    ZSTD_getErrorName(res));
            abort();
        }
        wres = write_outbuffer(fd, out);
        if (wres < 0)
            return -1;
        wres_acc += wres;
    }
    return wres_acc;
}
#endif

static ERL_NIF_TERM flush_tracer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Tracer *tracer;
    const int trace_queues = NUM_TRACE_QUEUES;
    int found = 1, i;
#ifdef HAVE_ZSTD
    ZSTD_inBuffer in;
#endif

    if (!enif_get_resource(env, argv[0], tracer_resource, (void**)&tracer))
        return enif_make_badarg(env);

    while (found) {
        found = 0;
        for (i = 0; i < trace_queues; i++) {
            TraceQueue *qp = tracer->q[i];
            ErlNifUInt64 head = qp->head, tail = qp->tail;
            if (head < tail) {
                int elems = tail - head;
                TraceEntry *curr_q = qp->q+(head & tracer->queue_mask);
                if (curr_q + elems > qp->q + tracer->queue_size)
                    elems = qp->q + tracer->queue_size - curr_q;
#ifdef HAVE_ZSTD
                in.src = (const void*)curr_q;
                in.size = sizeof(TraceEntry) * elems;
                in.pos = 0;

                while (1) {
                    size_t zres = ZSTD_compressStream(tracer->stream, &tracer->out, &in);
                    if (ZSTD_isError(zres)) {
                        fprintf(stderr,"ZSTD compress error: %s\n", ZSTD_getErrorName(zres));
                        abort();
                    }

                    /* Entire in buffer was consumed */
                    if (in.size == in.pos)
                        break;

                    if (write_outbuffer(tracer->file, &tracer->out) < 0)
                        return enif_make_int(env, errno);
                }
#else
                if (fwrite(curr_q, sizeof(TraceEntry), elems, tracer->file) < elems) {
                    return enif_make_int(env, errno);
                }
#endif

                qp->head += elems;
                found = 1;
            }
        }
    }
    return atom_ok;
}
// f(), R = trace_file_nif:start("tmp.zst"),trace_file_nif:trace_call(call, R, self(), {a,b,1}, #{}), trace_file_nif:stop(R).

#define isPow2(x) (((x & ~(x-1))==x)? x : 0)

static ERL_NIF_TERM start_tracer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Tracer *tracer;
    ErlNifBinary filename;
    ERL_NIF_TERM term;
    long queue_size, level;
    int i;
#ifdef HAVE_ZSTD
    int fd;
#else
    FILE *fd = NULL;
#endif

    if (!enif_inspect_iolist_as_binary(env, argv[0], &filename) ||
        !enif_get_map_value(env, argv[1], enif_make_atom(env, "queue_size"), &term) ||
        !enif_get_long(env, term, &queue_size) ||
        !enif_get_map_value(env, argv[1], enif_make_atom(env, "level"), &term) ||
        !enif_get_long(env, term, &level))
        return enif_make_badarg(env);

#ifdef HAVE_ZSTD
    if (level < 0 || level > ZSTD_maxCLevel() || !(isPow2(queue_size)))
        return enif_make_badarg(env);

    fd = open((char*)filename.data, O_WRONLY|O_CREAT|O_TRUNC, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP);
    if (fd == -1) {
        perror("open failed");
        return enif_make_badarg(env);
    }
#else
    fd = fopen((char*)filename.data, "w+");
    if (fd == NULL) {
        perror("open failed");
        return enif_make_badarg(env);
    }
#endif

    tracer = enif_alloc_resource(tracer_resource, sizeof(Tracer) +
                                 sizeof(TraceQueue*) * NUM_TRACE_QUEUES);

    for (i = 0; i < NUM_TRACE_QUEUES; i++) {
        tracer->q[i] = enif_alloc(sizeof(TraceQueue) + sizeof(TraceEntry) * queue_size);
        memset(tracer->q[i], 0, sizeof(TraceQueue) + sizeof(TraceEntry) * queue_size);
    }

    tracer->file = fd;
    tracer->level = level;
    tracer->queue_size = queue_size;
    tracer->queue_mask = queue_size-1;
    tracer->dirty_mtx = enif_mutex_create(__FILE__ "mtx");

#ifdef HAVE_ZSTD
    tracer->stream = ZSTD_createCStream();
    ZSTD_initCStream(tracer->stream, tracer->level);
    tracer->out.dst = enif_alloc(ZSTD_CStreamOutSize());
    tracer->out.size = ZSTD_CStreamOutSize();
    tracer->out.pos = 0;
#endif

    enif_self(env, &tracer->pid);

    term = enif_make_resource(env, tracer);
    enif_release_resource(tracer);

    return term;
}

static ERL_NIF_TERM get_tracer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Tracer *tracer;

    if (!enif_get_resource(env, argv[0], tracer_resource, (void**)&tracer))
        return enif_make_badarg(env);

    return enif_make_pid(env, &tracer->pid);
}

static ERL_NIF_TERM close_tracer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Tracer *tracer;
    ERL_NIF_TERM hd, tl;
    char atom_buf[255];
    char meta = 0;
    ssize_t atom_sizes = 0;
#ifdef HAVE_ZSTD
    size_t zres;
    ZSTD_inBuffer in;
#endif

    if (!enif_get_resource(env, argv[0], tracer_resource, (void**)&tracer) ||
        !enif_get_atom(env, argv[1], atom_buf, sizeof(atom_buf), ERL_NIF_LATIN1 ) ||
        !enif_is_list(env, argv[2]))
        return enif_make_badarg(env);

    tl = argv[2];

    if (strcmp(atom_buf, "little") == 0)
        meta = 1;

#ifdef HAVE_ZSTD
    meta |= 2;
    /* Close the tracing stream */
    end_stream(tracer->stream, &tracer->out, tracer->file);

    ZSTD_initCStream(tracer->stream, tracer->level);

    in.src = enif_alloc(ZSTD_CStreamInSize());
    in.size = ZSTD_CStreamInSize();
    in.pos = 0;
#endif

    while (enif_get_list_cell(env, tl, &hd, &tl)) {
        if (enif_get_atom(env, hd, atom_buf, sizeof(atom_buf), ERL_NIF_LATIN1)) {
            ErlNifUInt64 len = strlen(atom_buf);
#ifdef HAVE_ZSTD
            if (in.pos + len + sizeof(hd) + sizeof(hd) >= in.size) {
                in.size = in.pos;
                in.pos = 0;
                zres = ZSTD_compressStream(tracer->stream, &tracer->out, &in);
                if (ZSTD_isError(zres)) {
                    fprintf(stderr,"ZSTD compress error: %s\n", ZSTD_getErrorName(zres));
                    abort();
                }
                if (!(in.pos == in.size)) abort();

                atom_sizes += write_outbuffer(tracer->file, &tracer->out);

                in.size = ZSTD_CStreamInSize();
                in.pos = 0;
            }
            *(ERL_NIF_TERM*)(in.src + in.pos) = hd;
            in.pos += sizeof(hd);
            *(ERL_NIF_TERM*)(in.src + in.pos) = len;
            in.pos += sizeof(len);
            memcpy((void*)(in.src + in.pos), atom_buf, len);
            in.pos += len;
#else
            fwrite(&hd, sizeof(hd), 1, tracer->file);
            fwrite(&len, sizeof(len), 1, tracer->file);
            fwrite(atom_buf, len, 1, tracer->file);
            atom_sizes += sizeof(hd) + sizeof(len) + len;
#endif
        }
    }
#ifdef HAVE_ZSTD
    in.size = in.pos;
    in.pos = 0;
    zres = ZSTD_compressStream(tracer->stream, &tracer->out, &in);
    if (ZSTD_isError(zres)) {
        fprintf(stderr,"ZSTD compress error: %s\n", ZSTD_getErrorName(zres));
        abort();
    }
    atom_sizes += end_stream(tracer->stream, &tracer->out, tracer->file);
#endif

    /* At the end of the file write write some metadata about the file so
       that we can parse it correctly and efficiently */
#ifdef HAVE_ZSTD
    tracer->out.pos = 0;
    *(long*)(tracer->out.dst+tracer->out.pos) = atom_sizes;
    tracer->out.pos += sizeof(long);
    *(char*)(tracer->out.dst+tracer->out.pos) = meta;
    tracer->out.pos += sizeof(char);
    write_outbuffer(tracer->file, &tracer->out);
#else
    fwrite(&atom_sizes, 1, sizeof(long), tracer->file);
    fwrite(&meta, 1, sizeof(char), tracer->file);
    fflush(tracer->file);
#endif

    return atom_ok;
}

static ERL_NIF_TERM uncompress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_ZSTD
    ErlNifBinary bin, uncompressed;
    unsigned long long size;
    size_t zres;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin))
        return enif_make_badarg(env);
    size = ZSTD_getFrameContentSize(bin.data, bin.size);
    if (size == ZSTD_CONTENTSIZE_UNKNOWN)
        size = 1024*1024;
    else if (size == ZSTD_CONTENTSIZE_ERROR) {
        return enif_make_badarg(env);
    }
    enif_alloc_binary(size, &uncompressed);
    zres = ZSTD_decompress(uncompressed.data, uncompressed.size,
                           bin.data, bin.size);
    if (ZSTD_isError(zres)) {
        fprintf(stderr,"ZSTD compress error: %s\n", ZSTD_getErrorName(zres));
        abort();
    }
    enif_realloc_binary(&uncompressed, zres);
    return enif_make_binary(env, &uncompressed);
#else
    return enif_make_badarg(env);
#endif
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
    Tracer *tracer;
    TraceQueue *q;
    ERL_NIF_TERM res;
    ErlNifUInt id = enif_scheduler_id();

    if (!enif_get_resource(env, tracer_term, tracer_resource, (void**)&tracer))
        return atom_discard;

    q = tracer->q[id];

    if (id == 0)
        enif_mutex_lock(tracer->dirty_mtx);

    if (q->tail - q->head == tracer->queue_size) {
        q->dropped++;
        res = atom_discard;
    } else
        res = atom_trace;

    if (id == 0)
        enif_mutex_unlock(tracer->dirty_mtx);

    return res;
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
    TraceEntry *te;
    Tracer *tracer;
    TraceQueue *q;
    ErlNifUInt id = enif_scheduler_id();
    int flush;

    if (!enif_get_resource(env, tracer_term, tracer_resource, (void**)&tracer))
        return;

    q = tracer->q[id];

    if (id == 0)
        enif_mutex_lock(tracer->dirty_mtx);

    if (q->dropped) {
        te = q->q+(q->tail & tracer->queue_mask);
        te->ts = enif_monotonic_time(ERL_NIF_NSEC);
        te->type = TRACE_DROPPED;
        te->pid = pid;
        te->u.dropped = q->dropped;
        q->dropped = 0;
        q->tail++;
    }

    te = q->q+(q->tail & tracer->queue_mask);
    te->ts = enif_monotonic_time(ERL_NIF_NSEC);
    te->pid = pid;
    te->type = type;

    if (mfa) memcpy(&te->u.mfa, mfa, sizeof(MFA));
    else memset(&te->u.mfa, 0, sizeof(MFA));

    q->tail++;

    flush = (q->tail & tracer->queue_mask>>1) == 0;

    if (id == 0)
        enif_mutex_unlock(tracer->dirty_mtx);

    /* We send a flush message when we've filled half the queue */
    if (flush)
        enif_send(env, &tracer->pid, NULL, atom_flush);
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
