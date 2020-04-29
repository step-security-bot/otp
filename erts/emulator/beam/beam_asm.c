/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2020. All Rights Reserved.
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

#include <stddef.h> /* offsetof() */
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "big.h"
#include "beam_load.h"
#include "erl_binary.h"
#include "erl_map.h"
#include "erl_bits.h"
#include "dist.h"
#include "beam_bp.h"
#include "beam_catches.h"
#include "erl_thr_progress.h"
#include "erl_nfunc_sched.h"
#include "beam_asm.h"
#ifdef HIPE
#include "hipe_mode_switch.h"
#include "hipe_bif1.h"
#endif
#include "dtrace-wrapper.h"
#include "erl_proc_sig_queue.h"
#include "beam_common.h"

/* #define HARDDEBUG 1 */

#ifdef ERTS_ENABLE_LOCK_CHECK
#    define PROCESS_MAIN_CHK_LOCKS(P)                   \
do {                                                    \
    if ((P))                                            \
	erts_proc_lc_chk_only_proc_main((P));           \
    ERTS_LC_ASSERT(!erts_thr_progress_is_blocking());   \
} while (0)
#    define ERTS_REQ_PROC_MAIN_LOCK(P)				\
do {                                                            \
    if ((P))                                                    \
	erts_proc_lc_require_lock((P), ERTS_PROC_LOCK_MAIN,     \
				  __FILE__, __LINE__);          \
} while (0)
#    define ERTS_UNREQ_PROC_MAIN_LOCK(P)				\
do {									\
    if ((P))								\
	erts_proc_lc_unrequire_lock((P), ERTS_PROC_LOCK_MAIN);		\
} while (0)
#else
#  define PROCESS_MAIN_CHK_LOCKS(P)
#  define ERTS_REQ_PROC_MAIN_LOCK(P)
#  define ERTS_UNREQ_PROC_MAIN_LOCK(P)
#endif

/*
 * Define macros for deep checking of terms.
 */

/*
 * We reuse some of fields in the save area in the process structure.
 * This is safe to do, since this space is only actively used when
 * the process is switched out.
 */
#define REDS_IN(p)  ((p)->def_arg_reg[5])

/*
 * Special Beam instructions.
 */

BeamInstr *beam_apply;
BeamInstr *beam_normal_exit;
BeamInstr *beam_exit;
BeamInstr *beam_continue_exit;


/* NOTE These should be the only variables containing trace instructions.
**      Sometimes tests are for the instruction value, and sometimes
**      for the referring variable (one of these), and rouge references
**      will most likely cause chaos.
*/
BeamInstr *beam_return_to_trace;   /* OpCode(i_return_to_trace) */
BeamInstr *beam_return_trace;      /* OpCode(i_return_trace) */
BeamInstr *beam_exception_trace;   /* UGLY also OpCode(i_return_trace) */
BeamInstr *beam_return_time_trace; /* OpCode(i_return_time_trace) */

/*
 * process_main() is already huge, so we want to avoid inlining
 * seldom used functions into it.
 */
static void init_emulator_finish(void) ERTS_NOINLINE;


/*
 * Functions not directly called by process_main(). OK to inline.
 */


void
init_emulator(void)
{
    process_main(0, 0);
}

/*
 * process_main() is called twice:
 * The first call performs some initialisation, including exporting
 * the instructions' C labels to the loader.
 * The second call starts execution of BEAM code. This call never returns.
 */
void process_main(Eterm * x_reg_array, FloatDef* f_reg_array)
{
    static int init_done = 0;
    Process* c_p = NULL;
    int reds_used;
#ifdef DEBUG
    ERTS_DECLARE_DUMMY(Eterm pid);
#endif

    /* Number of reductions left.  This function
     * returns to the scheduler when FCALLS reaches zero.
     */
    Sint FCALLS = 0;

    /*
     * For keeping the negative old value of 'reds' when call saving is active.
     */
    int neg_o_reds = 0;

    Uint64 start_time = 0;          /* Monitor long schedule */
    BeamInstr* start_time_i = NULL;

    ERTS_MSACC_DECLARE_CACHE_X() /* a cached value of the tsd pointer for msacc */

    ERL_BITS_DECLARE_STATEP; /* Has to be last declaration */


    /*
     * Note: In this function, we attempt to place rarely executed code towards
     * the end of the function, in the hope that the cache hit rate will be better.
     * The initialization code is only run once, so it is at the very end.
     *
     * Note: c_p->arity must be set to reflect the number of useful terms in
     * c_p->arg_reg before calling the scheduler.
     */
    if (ERTS_UNLIKELY(!init_done)) {
       /* This should only be reached during the init phase when only the main
        * process is running. I.e. there is no race for init_done.
        */
	init_done = 1;
	goto init_emulator;
    }

    c_p = NULL;
    reds_used = 0;

    goto do_schedule1;

 do_schedule:
    ASSERT(c_p->arity < 6);
    ASSERT(c_p->debug_reds_in == REDS_IN(c_p));
    if (!ERTS_PROC_GET_SAVED_CALLS_BUF(c_p))
	reds_used = REDS_IN(c_p) - FCALLS;
    else
	reds_used = REDS_IN(c_p) - (CONTEXT_REDS + FCALLS);
    ASSERT(reds_used >= 0);
 do_schedule1:

    if (start_time != 0) {
        Sint64 diff = erts_timestamp_millis() - start_time;
	if (diff > 0 && (Uint) diff >  erts_system_monitor_long_schedule) {
	    ErtsCodeMFA *inptr = find_function_from_pc(start_time_i);
	    ErtsCodeMFA *outptr = find_function_from_pc(c_p->i);
	    monitor_long_schedule_proc(c_p,inptr,outptr,(Uint) diff);
	}
    }

    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    c_p = erts_schedule(NULL, c_p, reds_used);
    ASSERT(!(c_p->flags & F_HIPE_MODE));
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    start_time = 0;
#ifdef DEBUG
    pid = c_p->common.id; /* Save for debugging purposes */
#endif
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
    PROCESS_MAIN_CHK_LOCKS(c_p);

    ERTS_MSACC_UPDATE_CACHE_X();

    if (erts_system_monitor_long_schedule != 0) {
	start_time = erts_timestamp_millis();
	start_time_i = c_p->i;
    }

    ERL_BITS_RELOAD_STATEP(c_p);
    {
	int reds;
	Eterm* argp;
	int i;

	argp = c_p->arg_reg;
	for (i = c_p->arity - 1; i >= 0; i--) {
	    x_reg_array[i] = argp[i];
	    CHECK_TERM(x_reg_array[i]);
	}

	/*
	 * We put the original reduction count in the process structure, to reduce
	 * the code size (referencing a field in a struct through a pointer stored
	 * in a register gives smaller code than referencing a global variable).
	 */

	REDS_IN(c_p) = reds = c_p->fcalls;
#ifdef DEBUG
	c_p->debug_reds_in = reds;
#endif

	if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)) {
	    neg_o_reds = -CONTEXT_REDS;
	    FCALLS = neg_o_reds + reds;
	} else {
	    neg_o_reds = 0;
	    FCALLS = reds;
	}

	ERTS_DBG_CHK_REDS(c_p, FCALLS);

#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(process_scheduled)) {
            DTRACE_CHARBUF(process_buf, DTRACE_TERM_BUF_SIZE);
            DTRACE_CHARBUF(fun_buf, DTRACE_TERM_BUF_SIZE);
            dtrace_proc_str(c_p, process_buf);

            if (ERTS_PROC_IS_EXITING(c_p)) {
                sys_strcpy(fun_buf, "<exiting>");
            } else {
                ErtsCodeMFA *cmfa = find_function_from_pc(c_p->i);
                if (cmfa) {
                    dtrace_fun_decode(c_p, cmfa,
                                      NULL, fun_buf);
                } else {
                    erts_snprintf(fun_buf, sizeof(DTRACE_CHARBUF_NAME(fun_buf)),
                                  "<unknown/%p>", next);
                }
            }

            DTRACE2(process_scheduled, process_buf, fun_buf);
        }
#endif
        {
            BeamAsmContext ctx = { .FCALLS = FCALLS };
            enum beamasm_ret res = beamasm_call(&ctx, c_p, x_reg_array, f_reg_array, EBS, neg_o_reds);
            FCALLS = ctx.FCALLS;
            switch (res) {
            case RET_do_schedule: goto do_schedule; break;
            case RET_do_wait: {
                c_p->arity = 0;
                if (!ERTS_PTMR_IS_TIMED_OUT(c_p)) {
                    erts_atomic32_read_band_relb(&c_p->state,
                                                 ~ERTS_PSFLG_ACTIVE);
                }
                ASSERT(!ERTS_PROC_IS_EXITING(c_p));
                erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
                c_p->current = NULL;
                goto do_schedule;
                break;
            }
            case RET_context_switch: goto context_switch;
            case RET_context_switch2: goto context_switch2;
            case RET_context_switch3: goto context_switch3;
            case RET_context_switch_fun: goto context_switch_fun;
            default: ASSERT(0 && "NYI");
            }
        }
    }
    /*
     * The labels are jumped to from the $DISPATCH() macros when the reductions
     * are used up.
     *
     * Since the I register points just beyond the FuncBegin instruction, we
     * can get the module, function, and arity for the function being
     * called from I[-3], I[-2], and I[-1] respectively.
     */
 context_switch_fun:
    /* Add one for the environment of the fun */
    c_p->arity = erts_code_to_codemfa(c_p->i)->arity + 1;
    goto context_switch2;

 context_switch:
    c_p->arity = erts_code_to_codemfa(c_p->i)->arity;

 context_switch2: 		/* Entry for fun calls. */
    c_p->current = erts_code_to_codemfa(c_p->i);

 context_switch3:

 {
     Eterm* argp;
     int i;

     if (erts_atomic32_read_nob(&c_p->state) & ERTS_PSFLG_EXITING) {
         c_p->i = beam_exit;
         c_p->arity = 0;
         c_p->current = NULL;
         goto do_schedule;
     }

     /*
      * Make sure that there is enough room for the argument registers to be saved.
      */
     if (c_p->arity > c_p->max_arg_reg) {
	 /*
	  * Yes, this is an expensive operation, but you only pay it the first
	  * time you call a function with more than 6 arguments which is
	  * scheduled out.  This is better than paying for 26 words of wasted
	  * space for most processes which never call functions with more than
	  * 6 arguments.
	  */
	 Uint size = c_p->arity * sizeof(c_p->arg_reg[0]);
	 if (c_p->arg_reg != c_p->def_arg_reg) {
	     c_p->arg_reg = (Eterm *) erts_realloc(ERTS_ALC_T_ARG_REG,
						   (void *) c_p->arg_reg,
						   size);
	 } else {
	     c_p->arg_reg = (Eterm *) erts_alloc(ERTS_ALC_T_ARG_REG, size);
	 }
	 c_p->max_arg_reg = c_p->arity;
     }

     /*
      * Since REDS_IN(c_p) is stored in the save area (c_p->arg_reg) we must read it
      * now before saving registers.
      *
      * The '+ 1' compensates for the last increment which was not done
      * (beacuse the code for the Dispatch() macro becomes shorter that way).
      */

     ASSERT(c_p->debug_reds_in == REDS_IN(c_p));
    if (!ERTS_PROC_GET_SAVED_CALLS_BUF(c_p))
	reds_used = REDS_IN(c_p) - FCALLS;
    else
	reds_used = REDS_IN(c_p) - (CONTEXT_REDS + FCALLS);
    ASSERT(reds_used >= 0);

     /*
      * Save the argument registers and everything else.
      */

     argp = c_p->arg_reg;
     for (i = c_p->arity - 1; i >= 0; i--) {
	 argp[i] = x_reg_array[i];
     }
     goto do_schedule1;
 }

 init_emulator:
 {
     init_emulator_finish();
     return;
 }
 return;			/* Never executed */
}

/*
 * Enter all BIFs into the export table.
 *
 * Note that they will all call the error_handler until their modules have been
 * loaded, which may prevent the system from booting if BIFs from non-preloaded
 * modules are apply/3'd while loading code. Ordinary BIF calls will work fine
 * however since they won't go through export entries.
 */
static void install_bifs(void) {
    int i;

    for (i = 0; i < BIF_SIZE; i++) {
        BifEntry *entry;
        Export *ep;
        int j;

        entry = &bif_table[i];

        ep = erts_export_put(entry->module, entry->name, entry->arity);

        ep->info.op = op_i_func_info_IaaI;
        ep->info.mfa.module = entry->module;
        ep->info.mfa.function = entry->name;
        ep->info.mfa.arity = entry->arity;
        ep->bif_number = i;

        for (j = 0; j < ERTS_NUM_CODE_IX; j++) {
            ep->addressv[j] = &ep->trampoline.raw[0];
        }

        ERTS_CT_ASSERT(sizeof(ep->trampoline) >= BEAM_NATIVE_MIN_FUNC_SZ);
        erts_write_bif_wrapper(ep, &ep->trampoline.raw[0]);

        /* Set up a hidden export entry so we can trap to this BIF without
         * it being seen when tracing. */
        erts_init_trap_export(&BIF_TRAP_EXPORT(i),
                              entry->module, entry->name, entry->arity,
                              entry->f);
    }
}

/*
 * One-time initialization of emulator. Does not need to be
 * in process_main().
 */
BeamInstr *beamasm_get_beam_apply(void);
BeamInstr *beamasm_get_beam_exit(void);

static void
init_emulator_finish(void)
{

    /* beam_return_to_trace[0]   = BeamOpCodeAddr(op_i_return_to_trace); */
    /* beam_return_trace[0]      = BeamOpCodeAddr(op_return_trace); */
    /* beam_exception_trace[0]   = BeamOpCodeAddr(op_return_trace); /\* UGLY *\/ */
    /* beam_return_time_trace[0] = BeamOpCodeAddr(op_i_return_time_trace); */

    install_bifs();
}

int
erts_beam_jump_table(void)
{
#if defined(NO_JUMP_TABLE)
    return 0;
#else
    return 1;
#endif
}
