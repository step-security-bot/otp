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
 * Purpose:  NIF library for interacting with the tty
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

#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <wchar.h>
#include <stdio.h>
#include <signal.h>
#include <locale.h>
#ifndef __WIN32__
  #include <unistd.h>
  #include <termios.h>
  #include <term.h>
  #include <curses.h>
  #include <sys/ioctl.h>
#endif

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
#ifdef __WIN32__
    HANDLE ofd;
    HANDLE ifd;
#else
    int ofd;       /* stdout */
    int ifd;       /* stdin */
#endif
    ErlNifPid self;
#ifdef THREADED_READER
    ErlNifTid reader_tid;
#endif
#ifndef __WIN32__
    int signal[2]; /* Pipe used for signal (winch + cont) notifications */
    struct termios tty_smode;
    struct termios tty_rmode;
#endif
} TTYResource;

static ErlNifResourceType *tty_rt;

/* The NIFs: */
static ERL_NIF_TERM isatty_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM setlocale_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_select(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM isprint_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM wcwidth_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM wcswidth_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sizeof_wchar(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_window_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_tgetent(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_tgetnum(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_tgetflag(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_tgetstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_tgoto(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_read_signal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"isatty", 1, isatty_nif},
    {"tty_init", 2, tty_init},
    {"tty_set", 1, tty_set},
    {"tty_read_signal", 2, tty_read_signal},
    {"setlocale", 0, setlocale_nif},
    {"tty_select", 3, tty_select},
    {"tty_window_size", 1, tty_window_size},
    {"write_nif", 2, tty_write, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_nif", 2, tty_read, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"isprint", 1, isprint_nif},
    {"wcwidth", 1, wcwidth_nif},
    {"wcswidth", 1, wcswidth_nif},
    {"sizeof_wchar", 0, sizeof_wchar},
    {"tgetent_nif", 1, tty_tgetent},
    {"tgetnum_nif", 1, tty_tgetnum},
    {"tgetflag_nif", 1, tty_tgetflag},
    {"tgetstr_nif", 1, tty_tgetstr},
    {"tgoto_nif", 2, tty_tgoto},
    {"tgoto_nif", 3, tty_tgoto}
};

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

ERL_NIF_INIT(prim_tty, nif_funcs, load, NULL, upgrade, unload)

#define ATOMS                                     \
    ATOM_DECL(canon);                             \
    ATOM_DECL(echo);                              \
    ATOM_DECL(undefined);                         \
    ATOM_DECL(error);                             \
    ATOM_DECL(true);                              \
    ATOM_DECL(ok);                                \
    ATOM_DECL(input);                             \
    ATOM_DECL(false);                             \
    ATOM_DECL(stdin);                             \
    ATOM_DECL(stdout);                            \
    ATOM_DECL(stderr);                            \
    ATOM_DECL(sig);


#define ATOM_DECL(A) static ERL_NIF_TERM atom_##A
ATOMS
#undef ATOM_DECL

static ERL_NIF_TERM make_error(ErlNifEnv *env, ERL_NIF_TERM reason) {
    return enif_make_tuple2(env, atom_error, reason);
}

static ERL_NIF_TERM make_enotsup(ErlNifEnv *env) {
    return make_error(env, enif_make_atom(env, "enotsup"));
}

static ERL_NIF_TERM make_errno_error(ErlNifEnv *env, const char *function) {
    ERL_NIF_TERM errorInfo;
    #ifdef __WIN32__
    errorInfo = enif_make_atom(env, last_error());
    #else
    errorInfo = enif_make_atom(env, erl_errno_id(errno));
    #endif
    return make_error(
        env, enif_make_tuple2(
            env, enif_make_atom(env, function), errorInfo));
}

static int tty_get_fd(ErlNifEnv *env, ERL_NIF_TERM atom, int *fd) {
    if (enif_is_identical(atom, atom_stdout)) {
        *fd = fileno(stdout);
    } else if (enif_is_identical(atom, atom_stdin)) {
        *fd =  fileno(stdin);
    } else if (enif_is_identical(atom, atom_stderr)) {
        *fd =  fileno(stderr);
    } else {
        return 0;
    }
    return 1;
}

static ERL_NIF_TERM isatty_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int fd;
    if (tty_get_fd(env, argv[0], &fd)) {
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
#ifndef __WIN32__
        int width;
        ASSERT(i > 0 && i < (1l << 21));
        width = wcwidth((wchar_t)i);
        if (width == -1) {
            return make_error(env, enif_make_atom(env, "not_printable"));
        }
        return enif_make_int(env, width);
#else
        return make_error(env, enif_make_atom(env, "enotsup"));
#endif
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
            ASSERT(chars[i] >= 0 && chars[i] < (1l << 21));
        }
#endif
#ifndef __WIN32__
        width = wcswidth(chars, bin.size / sizeof(wchar_t));
#else
        width = bin.size / sizeof(wchar_t);
#endif
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
#ifndef __WIN32__
    ssize_t res = writev(tty->ofd, iovec->iov, iovec->iovcnt);
#else
    ssize_t res = 0;
    for (int i = 0; i < iovec->iovcnt; i++) {
        ssize_t written;
        BOOL r = WriteFile(tty->ofd, iovec->iov[i].iov_base, iovec->iov[i].iov_len, &written, NULL);
        if (!r) {
            res = -1;
            break;
        }
        res += written;
    }
#endif
    if (res < 0)
        return make_errno_error(env, "writev");
    return tail;
}

static ERL_NIF_TERM tty_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
    ErlNifBinary bin;
    ERL_NIF_TERM res_term;
    ssize_t res = 0;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);
    #ifdef __WIN32__
    {
        ssize_t inputs_read, num_characters = 0;
        wchar_t *characters = NULL;
        INPUT_RECORD inputs[128];
        if (!ReadConsoleInputW(tty->ifd, inputs, sizeof(inputs)/sizeof(*inputs),
                              &inputs_read)) {
            return make_errno_error(env, "ReadConsoleInput");
        }
        for (int i = 0; i < inputs_read; i++) {
            if (inputs[i].EventType == KEY_EVENT) {
                if (inputs[i].Event.KeyEvent.bKeyDown && inputs[i].Event.KeyEvent.uChar.UnicodeChar < 256 && inputs[i].Event.KeyEvent.uChar.UnicodeChar != 0) {
                    num_characters++;
                }
                if (!inputs[i].Event.KeyEvent.bKeyDown && inputs[i].Event.KeyEvent.uChar.UnicodeChar > 255 && inputs[i].Event.KeyEvent.uChar.UnicodeChar != 0) {
                    num_characters++;
                }
            }
        }
        if (num_characters > 0) {
            enif_alloc_binary(num_characters * sizeof(wchar_t), &bin);
            characters = (wchar_t*)bin.data;
        }
        for (int i = 0; i < inputs_read; i++) {
            switch (inputs[i].EventType)
            {
            case KEY_EVENT:
                if (inputs[i].Event.KeyEvent.bKeyDown && inputs[i].Event.KeyEvent.uChar.UnicodeChar < 256 && inputs[i].Event.KeyEvent.uChar.UnicodeChar != 0) {
                    characters[res++] = inputs[i].Event.KeyEvent.uChar.UnicodeChar;
                }
                if (!inputs[i].Event.KeyEvent.bKeyDown && inputs[i].Event.KeyEvent.uChar.UnicodeChar > 255 && inputs[i].Event.KeyEvent.uChar.UnicodeChar != 0) {
                    characters[res++] = inputs[i].Event.KeyEvent.uChar.UnicodeChar;
                }
                break;
            case WINDOW_BUFFER_SIZE_EVENT:
                enif_send(env, &tty->self, NULL,
                    enif_make_tuple2(env, enif_make_atom(env, "resize"),
                        enif_make_tuple2(env, enif_make_int(env, inputs[i].Event.WindowBufferSizeEvent.dwSize.Y),
                                            enif_make_int(env, inputs[i].Event.WindowBufferSizeEvent.dwSize.X))));
                break;
            default:
                fprintf(stderr,"Unknown event: %d\r\n", inputs[i].EventType);
                break;
            }
        }
        res *= sizeof(wchar_t);
    }
    #else
    ssize_t res;
    ErlNifBinary bin;
    enif_alloc_binary(1024, &bin);
    res = read(tty->ifd, bin.data, bin.size);
    if (res < 0) {
        if (1 || errno != EAGAIN && errno != EINTR) {
            return make_errno_error(env, "read");
        }
        res = 0;
    }
    #endif
    enif_select(env, tty->ifd, ERL_NIF_SELECT_READ, tty, NULL, argv[1]);
    if (res < bin.size / 2) {
        unsigned char *buff = enif_make_new_binary(env, res, &res_term);
        if (res > 0) {
            memcpy(buff, bin.data, res);
            enif_release_binary(&bin);
        }
    } else {
        enif_realloc_binary(&bin, res);
        res_term = enif_make_binary(env, &bin);
    }

    return enif_make_tuple2(env, atom_ok, res_term);
}

static ERL_NIF_TERM setlocale_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef __WIN32__
    if (!SetConsoleOutputCP(CP_UTF8)) {
        return make_errno_error(env, "SetConsoleOutputCP");
    }
    // if (!SetConsoleCP(CP_UTF8)) {
    //     return make_errno_error(env, "SetConsoleOutputCP");
    // }
    return atom_true;
#elif defined(PRIMITIVE_UTF8_CHECK)
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

static ERL_NIF_TERM tty_tgetent(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary TERM;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &TERM))
        return enif_make_badarg(env);
#ifndef __WIN32__
    if (tgetent((char *)NULL /* ignored */, (char *)TERM.data) <= 0) {
        return make_errno_error(env, "tgetent");
    }
    return atom_ok;
#else
    return make_enotsup(env);
#endif
}

static ERL_NIF_TERM tty_tgetnum(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary TERM;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &TERM))
        return enif_make_badarg(env);
#ifndef __WIN32__
    return enif_make_int(env, tgetnum((char*)TERM.data));
#else
    return make_enotsup(env);
#endif
}

static ERL_NIF_TERM tty_tgetflag(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary TERM;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &TERM))
        return enif_make_badarg(env);
#ifndef __WIN32__
    if (tgetflag((char*)TERM.data))
        return atom_true;
    return atom_false;
#else
    return make_enotsup(env);
#endif
}

static ERL_NIF_TERM tty_tgetstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary TERM, ret;
    /* tgetstr seems to use a lot of stack buffer space,
       so buff needs to be relatively "small" */
    char *str = NULL;
    char buff[BUFSIZ] = {0};

    if (!enif_inspect_iolist_as_binary(env, argv[0], &TERM))
        return enif_make_badarg(env);
#ifndef __WIN32__
    str = tgetstr((char*)TERM.data, (char**)&buff);
    if (!str) return atom_false;
    enif_alloc_binary(strlen(str), &ret);
    memcpy(ret.data, str, strlen(str));
    return enif_make_tuple2(
        env, atom_ok, enif_make_binary(env, &ret));
#else
    return make_enotsup(env);
#endif
}

#ifndef __WIN32__
static int tputs_buffer_index;
static unsigned char tputs_buffer[1024];

static int tty_puts_putc(int c) {
    tputs_buffer[tputs_buffer_index++] = (unsigned char)c;
    return 0;
}
#endif

static ERL_NIF_TERM tty_tgoto(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary TERM;
    char *ent;
    int value1, value2;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &TERM) ||
        !enif_get_int(env, argv[1], &value1))
        return enif_make_badarg(env);
#ifndef __WIN32__
    if (argc == 2) {
        ent = tgoto((char*)TERM.data, 0, value1);
    } else {
        ASSERT(argc == 3);
        ent = tgoto((char*)TERM.data, value1, value2);
    }
    if (!ent) return make_errno_error(env, "tgoto");

    tputs_buffer_index = 0;
    if (tputs(ent, 1, tty_puts_putc)) {
        return make_errno_error(env, "tputs");
    } else {
        ERL_NIF_TERM ret;
        unsigned char *buff = enif_make_new_binary(env, tputs_buffer_index, &ret);
        memcpy(buff, tputs_buffer, tputs_buffer_index);
        return enif_make_tuple2(env, atom_ok, ret);
    }
#else
    return make_enotsup(env);
#endif
}

static ERL_NIF_TERM tty_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    ERL_NIF_TERM canon, echo, sig;
    int fd;
    #ifndef __WIN32__
    struct termios tty_smode, tty_rmode;
    #endif

    if (argc != 2 ||
        !tty_get_fd(env, argv[0], &fd) ||
        !enif_is_map(env, argv[1])) {
        return enif_make_badarg(env);
    }

    if (!enif_get_map_value(env, argv[1], enif_make_atom(env,"canon"), &canon))
        canon = enif_make_atom(env, "undefined");
    if (!enif_get_map_value(env, argv[1], enif_make_atom(env,"echo"), &echo))
        echo = enif_make_atom(env, "undefined");
    if (!enif_get_map_value(env, argv[1], enif_make_atom(env,"sig"), &sig))
        sig = enif_make_atom(env, "undefined");

#ifndef __WIN32__
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
    if (enif_is_identical(sig, atom_false)) {
	/* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
	tty_smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
	tty_smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON);
#endif
	tty_smode.c_lflag &= ~(ISIG|IEXTEN);
    }
#else
        // Set output mode to handle virtual terminal sequences
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hOut == INVALID_HANDLE_VALUE)
    {
        return make_errno_error(env, "GetStdHandle");
    }
    HANDLE hIn = GetStdHandle(STD_INPUT_HANDLE);
    if (hIn == INVALID_HANDLE_VALUE)
    {
        return make_errno_error(env, "GetStdHandle");
    }

    DWORD dwOriginalOutMode = 0;
    DWORD dwOriginalInMode = 0;
    if (!GetConsoleMode(hOut, &dwOriginalOutMode))
    {
        return make_errno_error(env, "GetConsoleMode");
    }
    if (!GetConsoleMode(hIn, &dwOriginalInMode))
    {
        return make_errno_error(env, "GetConsoleMode");
    }

    // fprintf(stderr, "origOutMode: %x origInMode: %x\r\n",
    //     dwOriginalOutMode, dwOriginalInMode);

    DWORD dwRequestedOutModes = ENABLE_VIRTUAL_TERMINAL_PROCESSING | DISABLE_NEWLINE_AUTO_RETURN;
    DWORD dwRequestedInModes = ENABLE_VIRTUAL_TERMINAL_INPUT;
    DWORD dwDisabledInModes = ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT;

    DWORD dwOutMode = dwOriginalOutMode | dwRequestedOutModes;
    if (!SetConsoleMode(hOut, dwOutMode))
    {
        // we failed to set both modes, try to step down mode gracefully.
        dwRequestedOutModes = ENABLE_VIRTUAL_TERMINAL_PROCESSING;
        dwOutMode = dwOriginalOutMode | dwRequestedOutModes;
        if (!SetConsoleMode(hOut, dwOutMode))
        {
            // Failed to set any VT mode, can't do anything here.
            return make_errno_error(env, "SetConsoleMode");
        }
    }

    DWORD dwInMode = (dwOriginalInMode | dwRequestedInModes) & ~dwDisabledInModes;
    if (!SetConsoleMode(hIn, dwInMode))
    {
        // Failed to set VT input mode, can't do anything here.
        return make_errno_error(env, "SetConsoleMode");
    }

#endif /* __WIN32__ */
    
    TTYResource *tty = enif_alloc_resource(tty_rt, sizeof(TTYResource));
#ifndef __WIN32__
    memcpy(&tty->tty_rmode, &tty_rmode, sizeof(tty_rmode));
    memcpy(&tty->tty_smode, &tty_smode, sizeof(tty_smode));
    tty->ifd = 0;
    tty->ofd = 1;
#else
    tty->ifd = GetStdHandle(STD_INPUT_HANDLE);
    tty->ofd = GetStdHandle(STD_OUTPUT_HANDLE);
#endif

    ERL_NIF_TERM tty_term = enif_make_resource(env, tty);
    enif_release_resource(tty);

    return enif_make_tuple2(env, atom_ok, tty_term);
}

static ERL_NIF_TERM tty_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);
#ifndef __WIN32__
    if (tcsetattr(tty->ifd, TCSANOW, &tty->tty_smode) < 0) {
        return make_errno_error(env, "tcsetattr");
    }
#endif
    return atom_ok;
}

static ERL_NIF_TERM tty_window_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
    int width = -1, height = -1;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);
    {
#ifdef TIOCGWINSZ
        struct winsize ws;
        if (ioctl(tty->ifd,TIOCGWINSZ,&ws) == 0) {
            if (ws.ws_col > 0)
                width = ws.ws_col;
            if (ws.ws_row > 0)
                height = ws.ws_row;
        }
#elif defined(__WIN32__)
        CONSOLE_SCREEN_BUFFER_INFOEX buffer_info;
        buffer_info.cbSize = sizeof(buffer_info);
        if (GetConsoleScreenBufferInfoEx(tty->ofd, &buffer_info)) {
            height = buffer_info.dwSize.Y;
            width = buffer_info.dwSize.X;
        } else {
            return make_errno_error(env,"GetConsoleScreenBufferInfoEx");
        }

#endif
    }
    if (width == -1 && height == -1) {
        return make_enotsup(env);
    }
    return enif_make_tuple2(
                env, atom_ok,
                enif_make_tuple2(
                    env,
                    enif_make_int(env, height),
                    enif_make_int(env, width)));
    
}

#ifndef __WIN32__

static int tty_signal_fd = -1;

static RETSIGTYPE tty_cont(int sig)
{
    if (tty_signal_fd != 1) {
        write(tty_signal_fd, "c", 1);
    }
}


static RETSIGTYPE tty_winch(int sig)
{
    if (tty_signal_fd != 1) {
        write(tty_signal_fd, "w", 1);
    }
}

#endif

static ERL_NIF_TERM tty_read_signal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
    char buff[1];
    ssize_t ret;
    ERL_NIF_TERM res;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);
#ifndef __WIN32__
    do {
        ret = read(tty->signal[0], buff, 1);
    } while (ret < 0 && errno == EAGAIN);

    if (ret < 0) {
        return make_errno_error(env, "read");
    } else if (ret == 0) {
        return make_error(env, enif_make_atom(env,"empty"));
    }

    enif_select(env, tty->signal[0], ERL_NIF_SELECT_READ, tty, NULL, argv[1]);

    if (buff[0] == 'w') {
        res = enif_make_atom(env, "winch");
    } else if (buff[0] == 'c') {
        res = enif_make_atom(env, "cont");
    } else {
        res = enif_make_string_len(env, buff, 1, ERL_NIF_LATIN1);
    }
    return enif_make_tuple2(env, atom_ok, res);
#else
    return make_enotsup(env);
#endif
}

#ifdef THREADED_READED
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

#endif

static ERL_NIF_TERM tty_select(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
#ifdef THREADED_READER
    struct tty_reader_init *tty_reader_init;
#endif
#ifndef __WIN32__
    extern int using_oldshell; /* set this to let the rest of erts know */
#endif
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);

#ifdef THREADED_READER
    tty_reader_init = enif_alloc(sizeof(struct tty_reader_init));
    tty_reader_init->env = enif_alloc_env();
    tty_reader_init->tty = enif_make_copy(tty_reader_init->env, argv[0]);
#endif

    enif_self(env, &tty->self);

#ifndef __WIN32__
    pipe(tty->signal);
    SET_NONBLOCKING(tty->signal[0]);
    enif_select(env, tty->signal[0], ERL_NIF_SELECT_READ, tty, NULL, argv[1]);
    tty_signal_fd = tty->signal[1];

    sys_signal(SIGCONT, tty_cont);
    sys_signal(SIGWINCH, tty_winch);

    using_oldshell = 0;
#endif

    enif_select(env, tty->ifd, ERL_NIF_SELECT_READ, tty, NULL, argv[2]);
    enif_monitor_process(env, tty, &tty->self, NULL);

#ifdef THREADED_READER
    if (enif_thread_create(
            "stdin_reader",
            &tty->reader_tid,
            tty_reader_thread, tty_reader_init, NULL)) {
        return make_errno_error(env, "enif_thread_create");
    }
#endif
    return atom_ok;
}

static void tty_monitor_down(ErlNifEnv* caller_env, void* obj, ErlNifPid* pid, ErlNifMonitor* mon) {
    TTYResource *tty = obj;
#ifndef __WIN32__
    tcsetattr(tty->ifd, TCSANOW, &tty->tty_rmode);
    enif_select(caller_env, tty->signal[0], ERL_NIF_SELECT_STOP, tty, NULL, atom_undefined);
    close(tty->signal[1]);
    sys_signal(SIGCONT, SIG_DFL);
    sys_signal(SIGWINCH, SIG_DFL);
#endif

}

static void tty_select_stop(ErlNifEnv* caller_env, void* obj, ErlNifEvent event, int is_direct_call) {
    close(event);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

    ErlNifResourceTypeInit rt = {
        NULL /* dtor */,
        tty_select_stop,
        tty_monitor_down};

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
