/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson 2015-2021. All Rights Reserved.
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
 * Purpose:  NIF library for process/port tracer
 *
 */

#define STATIC_ERLANG_NIF 1

#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif

#include "config.h"
#include "sys.h"
#include "erl_nif.h"
#include "erl_driver.h"

#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <wchar.h>
#include <stdio.h>
#include <signal.h>
#include <locale.h>
#include <termios.h>
#include <term.h>
#include <curses.h>

#if !defined(HAVE_SETLOCALE) || !defined(HAVE_NL_LANGINFO) || !defined(HAVE_LANGINFO_H)
#define PRIMITIVE_UTF8_CHECK 1
#else
#include <langinfo.h>
#endif

#ifdef VALGRIND
#  include <valgrind/memcheck.h>
#endif

#define DEF_HEIGHT 24
#define DEF_WIDTH 80

typedef struct {
    int ofd; /* stdout */
    int ifd; /* stdin */
    ErlNifPid self;
    int cols;
    int xn;
    char *up;
    char *down;
    char *left;
    char *right;
    ErlNifTid reader_tid;
    struct termios tty_smode;
    struct termios tty_rmode;
} TTYResource;

static ErlNifResourceType *tty_rt;

/* The NIFs: */
static ERL_NIF_TERM isatty_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_termcap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM setlocale_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_select(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM isprint_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM wcwidth_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM wcswidth_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sizeof_wchar(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_window_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"isatty", 1, isatty_nif},
    {"tty_init", 2, tty_init},
    {"tty_set", 1, tty_set},
    {"tty_termcap", 2, tty_termcap},
    {"setlocale", 0, setlocale_nif},
    {"tty_select", 2, tty_select},
    {"tty_window_size", 1, tty_window_size},
    {"write", 2, tty_write},
    {"isprint", 1, isprint_nif},
    {"wcwidth", 1, wcwidth_nif},
    {"wcswidth", 1, wcswidth_nif},
    {"sizeof_wchar", 0, sizeof_wchar}
};

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

ERL_NIF_INIT(prim_tty, nif_funcs, load, NULL, upgrade, unload)

#define ATOMS                                     \
    ATOM_DECL(canon);                             \
    ATOM_DECL(echo);                              \
    ATOM_DECL(error);                             \
    ATOM_DECL(true);                              \
    ATOM_DECL(ok);                                \
    ATOM_DECL(input);                             \
    ATOM_DECL(false);                             \
    ATOM_DECL(sig);


#define ATOM_DECL(A) static ERL_NIF_TERM atom_##A
ATOMS
#undef ATOM_DECL

static ERL_NIF_TERM make_error(ErlNifEnv *env, ERL_NIF_TERM reason) {
    return enif_make_tuple2(env, atom_error, reason);
}

static ERL_NIF_TERM make_errno_error(ErlNifEnv *env, const char *function) {
    return make_error(
        env, enif_make_tuple2(
            env, enif_make_atom(env, function),
            enif_make_atom(env, erl_errno_id(errno))));
}

static ERL_NIF_TERM isatty_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int fd;
    if (enif_get_int(env, argv[0], &fd)) {
        return isatty(fd) ? atom_true : atom_false;
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM isprint_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int i;
    if (enif_get_int(env, argv[0], &i)) {
        ASSERT(i > 0 && i < 256);
        return isprint((char)i) ? atom_true : atom_false;
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM wcwidth_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int i;
    if (enif_get_int(env, argv[0], &i)) {
        int width;
        ASSERT(i > 0 && i < (1 << 16));
        width = wcwidth((wchar_t)i);
        if (width == -1) {
            return make_error(env, enif_make_atom(env, "not_printable"));
        }
        return enif_make_int(env, width);
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM wcswidth_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;
    if (enif_inspect_iolist_as_binary(env, argv[0], &bin)) {
        wchar_t *chars = (wchar_t*)bin.data;
        int width;
#ifdef DEBUG
        for (int i = 0; i < bin.size / sizeof(wchar_t); i++) {
            ASSERT(chars[i] >= 0 && chars < (1 << 16));
        }
#endif
        width = wcswidth(chars, bin.size / sizeof(wchar_t));
        if (width == -1) {
            return make_error(env, enif_make_atom(env, "not_printable"));
        }
        return enif_make_tuple2(env, atom_ok, enif_make_int(env, width));
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM sizeof_wchar(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_int(env, sizeof(wchar_t));
}

static ERL_NIF_TERM tty_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM tail;
    ErlNifIOVec vec, *iovec = &vec;
    TTYResource *tty;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);
    if (!enif_inspect_iovec(env, 64, argv[1], &tail, &iovec))
        return enif_make_badarg(env);
    ssize_t res = writev(tty->ofd, iovec->iov, iovec->iovcnt);
    if (res < 0)
        return make_errno_error(env, "writev");
    return tail;
}

static ERL_NIF_TERM setlocale_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef PRIMITIVE_UTF8_CHECK
    setlocale(LC_CTYPE, "");  /* Set international environment, 
				 ignore result */
    return enif_make_atom(env, "primitive");
#else
    char *l = setlocale(LC_CTYPE, "");  /* Set international environment */
    if (l != NULL) {
	if (strcmp(nl_langinfo(CODESET), "UTF-8") == 0)
            return atom_true;
    }
    return atom_false;
#endif
}

static ERL_NIF_TERM tty_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    ERL_NIF_TERM canon, echo, sig;
    struct termios tty_smode, tty_rmode;
    int fd;

    if (argc != 2 ||
        !enif_get_int(env, argv[0], &fd) ||
        !enif_is_map(env, argv[1])) {
        return enif_make_badarg(env);
    }

    if (!enif_get_map_value(env, argv[1], enif_make_atom(env,"canon"), &canon))
        canon = enif_make_atom(env, "undefined");
    if (!enif_get_map_value(env, argv[1], enif_make_atom(env,"echo"), &echo))
        echo = enif_make_atom(env, "undefined");
    if (!enif_get_map_value(env, argv[1], enif_make_atom(env,"sig"), &sig))
        sig = enif_make_atom(env, "undefined");

    if (tcgetattr(fd, &tty_rmode) < 0) {
        return make_errno_error(env, "tcgetattr");
    }

    tty_smode = tty_rmode;

    /* Default characteristics for all usage including termcap output. */
    tty_smode.c_iflag &= ~ISTRIP;

    /* erts_fprintf(stderr,"canon %T\r\n", canon); */
    /* Turn canonical (line mode) on off. */
    if (enif_is_identical(canon, atom_true)) {
	tty_smode.c_iflag |= ICRNL;
	tty_smode.c_lflag |= ICANON;
	tty_smode.c_oflag |= OPOST;
	tty_smode.c_cc[VEOF] = tty_rmode.c_cc[VEOF];
#ifdef VDSUSP
	tty_smode.c_cc[VDSUSP] = tty_rmode.c_cc[VDSUSP];
#endif
    }
    if (enif_is_identical(canon, atom_false)) {
	tty_smode.c_iflag &= ~ICRNL;
	tty_smode.c_lflag &= ~ICANON;
	tty_smode.c_oflag &= ~OPOST;
	/* Must get these really right or funny effects can occur. */
	tty_smode.c_cc[VMIN] = 1;
	tty_smode.c_cc[VTIME] = 0;
#ifdef VDSUSP
	tty_smode.c_cc[VDSUSP] = 0;
#endif
    }

    /* Turn echo on or off. */
    /* erts_fprintf(stderr,"echo %T\r\n", echo); */
    if (enif_is_identical(echo, atom_true))
        tty_smode.c_lflag |= ECHO;
    if (enif_is_identical(echo, atom_false))
        tty_smode.c_lflag &= ~ECHO;

    /* erts_fprintf(stderr,"sig %T\r\n", sig); */
    /* Set extra characteristics for "RAW" mode, no signals. */
    if (enif_is_identical(sig, atom_true)) {
	/* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
	tty_smode.c_iflag |= (BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
	tty_smode.c_iflag |= (BRKINT|IGNPAR|ICRNL|IXON);
#endif
	tty_smode.c_lflag |= (ISIG|IEXTEN);
    }
    if (enif_is_identical(echo, atom_false)) {
	/* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
	tty_smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
	tty_smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON);
#endif
	tty_smode.c_lflag &= ~(ISIG|IEXTEN);
    }

    TTYResource *tty = enif_alloc_resource(tty_rt, sizeof(TTYResource));
    memcpy(&tty->tty_rmode, &tty_rmode, sizeof(tty_rmode));
    memcpy(&tty->tty_smode, &tty_smode, sizeof(tty_smode));
    tty->ifd = 0;
    tty->ofd = 1;

    ERL_NIF_TERM tty_term = enif_make_resource(env, tty);
    enif_release_resource(tty);

    return enif_make_tuple2(env, atom_ok, tty_term);
}

static ERL_NIF_TERM tty_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);
    if (tcsetattr(tty->ifd, TCSANOW, &tty->tty_smode) < 0) {
        return make_errno_error(env, "tcsetattr");
    }
    return atom_ok;
}

static ERL_NIF_TERM tty_termcap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
    ErlNifBinary TERM;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty) ||
        !enif_inspect_iolist_as_binary(env, argv[1], &TERM))
        return enif_make_badarg(env);
    if (tgetent((char *)tty, (char *)TERM.data) <= 0) {
        return make_errno_error(env, "tgetent");
    }
    tty->cols = tgetnum("co");
    if (tty->cols <= 0)
        tty->cols = DEF_WIDTH;
    tty->xn = tgetflag("xn");
    tty->up = tgetstr("up", &TERM.data);
    if (!(tty->down = tgetstr("do", &TERM.data)))
      tty->down = "\n";
    if (!(tty->left = tgetflag("bs") ? "\b" : tgetstr("bc", &TERM.data)))
      tty->left = "\b";		/* Can't happen - but does on Solaris 2 */
    tty->right = tgetstr("nd", &TERM.data);
    fprintf(stderr,"up: %d down: %d left: %d right: %d\r\n",
            (int)(tty->up[2]), (int)*tty->down, (int)*tty->left, (int)(tty->right[2]));
    if (tty->up && tty->down && tty->left && tty->right) {
        return atom_ok;
    }

    return make_error(env, enif_make_atom(env, "enotsup"));;
}

static ERL_NIF_TERM tty_window_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
    int width = DEF_HEIGHT, height = DEF_WIDTH;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);

#define DEF_HEIGHT 24
#define DEF_WIDTH 80
    {
#ifdef TIOCGWINSZ
        struct winsize ws;
        if (ioctl(ttysl_fd,TIOCGWINSZ,&ws) == 0) {
            if (ws.ws_col > 0)
                width = ws.ws_col;
            if (ws.ws_row > 0)
                height = ws.ws_row;
        } else
#endif
        {
            width = DEF_WIDTH;
            height = DEF_HEIGHT;
        }
    }
    return enif_make_tuple2(
        env, atom_ok,
        enif_make_tuple2(
            env,
            enif_make_int(env, width),
            enif_make_int(env, height)));
}

static RETSIGTYPE tty_cont(int sig)
{
    /* if (tty_set(ttysl_fd) < 0) { */
    /*     exit(1); */
    /* } */
}

static RETSIGTYPE tty_winch(int sig)
{
    // cols_needs_update = TRUE;
}

struct tty_reader_init {
    ErlNifEnv *env;
    ERL_NIF_TERM tty;
};

#define TTY_READER_BUF_SIZE 1024

static void *tty_reader_thread(void *args) {
    struct tty_reader_init *tty_reader_init = (struct tty_reader_init*)args;
    TTYResource *tty;
    ErlNifBinary binary;
    ErlNifEnv *env = NULL;
    ERL_NIF_TERM data[10];
    int cnt = 0;

    enif_alloc_binary(TTY_READER_BUF_SIZE, &binary);

    enif_get_resource(tty_reader_init->env, tty_reader_init->tty, tty_rt, (void **)&tty);

    SET_BLOCKING(tty->ifd);

    while(true) {
        ssize_t i = read(tty->ifd, binary.data, TTY_READER_BUF_SIZE);
        /* fprintf(stderr,"Read: %ld bytes from %d\r\n", i, tty->ifd); */
        if (i < 0) {
            int saved_errno = errno;
            if (env) {
                ERL_NIF_TERM msg = enif_make_list_from_array(env, data, cnt);
                enif_send(env, &tty->self, NULL, enif_make_tuple2(env, atom_input, msg));
                cnt = 0;
                env = NULL;
            }
            if (saved_errno != EAGAIN) {
                env = enif_alloc_env();
                errno = saved_errno;
                enif_send(env, &tty->self, NULL, make_errno_error(env, "read"));
                break;
            }
        } else {
            if (!env) {
                env = enif_alloc_env();
            }
            enif_realloc_binary(&binary, i);
            data[cnt++] = enif_make_binary(env, &binary);
            if (cnt == 10 || i != TTY_READER_BUF_SIZE) {
                ERL_NIF_TERM msg = enif_make_list_from_array(env, data, cnt);
                enif_send(env, &tty->self, NULL, enif_make_tuple2(env, atom_input, msg));
                cnt = 0;
                env = NULL;
            }
            enif_alloc_binary(TTY_READER_BUF_SIZE, &binary);
        }
    }

    enif_free_env(tty_reader_init->env);
    enif_free(tty_reader_init);
    return (void*)0;
}

static ERL_NIF_TERM tty_select(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
    struct tty_reader_init *tty_reader_init;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);

    tty_reader_init = enif_alloc(sizeof(struct tty_reader_init));
    tty_reader_init->env = enif_alloc_env();
    tty_reader_init->tty = enif_make_copy(tty_reader_init->env, argv[0]);
    enif_self(env, &tty->self);

    sys_signal(SIGCONT, tty_cont);
    sys_signal(SIGWINCH, tty_winch);

    if (enif_thread_create(
            "stdin_reader",
            &tty->reader_tid,
            tty_reader_thread, tty_reader_init, NULL)) {
        return make_errno_error(env, "enif_thread_create");
    }
    return atom_ok;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

    ErlNifResourceTypeInit rt = {
        NULL /* dtor */,
        NULL /* select stop */,
        NULL /* monitor down */};

#define ATOM_DECL(A) atom_##A = enif_make_atom(env, #A)
ATOMS
#undef ATOM_DECL

    *priv_data = NULL;

    tty_rt = enif_open_resource_type_x(env, "tty", &rt, ERL_NIF_RT_CREATE, NULL);

    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{

}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    if (*old_priv_data != NULL) {
	return -1; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
	return -1; /* Don't know how to do that */
    }
    if (load(env, priv_data, load_info)) {
	return -1;
    }
    return 0;
}
