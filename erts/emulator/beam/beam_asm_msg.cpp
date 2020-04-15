/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2020. All Rights Reserved.
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

#include "beam_asm.hpp"

extern "C" {
#include "bif.h"
#include "erl_proc_sig_queue.h"
}

static bool decode_dist(Process *c_p, ErtsMessage *msgp, Eterm *HTOP, Eterm *E) {
  if (!erts_proc_sig_decode_dist(c_p, ERTS_PROC_LOCK_MAIN, msgp, 0)) {
    /*
     * A corrupt distribution message that we weren't able to decode;
     * remove it...
     */
    /* No swapin should be needed */
    ASSERT(HTOP == c_p->htop && E == c_p->stop);
    /* TODO: Add DTrace probe for this bad message situation? */
    UNLINK_MESSAGE(c_p, msgp);
    msgp->next = NULL;
    erts_cleanup_messages(msgp);
    return false;
  }
  return true;
}

static void recv_mark_save(Process *p) {
    ERTS_RECV_MARK_SAVE(p);
}

static void recv_mark_set(Process *p) {
    ERTS_RECV_MARK_SET(p);
}

void BeamModuleAssembler::emit_i_recv_mark(Instruction *I) {
    a.mov(ARG1, c_p);
    call((uint64_t)recv_mark_save);
}

void BeamModuleAssembler::emit_i_recv_set(Instruction *I) {
    a.mov(ARG1, c_p);
    call((uint64_t)recv_mark_set);
}

void BeamModuleAssembler::emit_i_loop_rec(ArgVal Dest, Instruction *I) {
  Label check_is_distributed = a.newLabel(), next = a.newLabel(),
    restart = a.newLabel(), await = a.newLabel();
  x86::Mem msgp = qTMP1_MEM, get_out = dTMP2_MEM;
  a.or_(x86::dword_ptr(c_p, offsetof(Process, flags)), F_DELAY_GC);
  a.align(kAlignCode, 8);
  a.bind(restart);
  // TODO: Add FCALLS check
  comment("PEAK_MESSAGE");
  a.mov(TMP1, x86::qword_ptr(c_p, offsetof(Process, sig_qs.save)));
  a.mov(TMP1, x86::qword_ptr(TMP1));
  a.test(TMP1,TMP1);
  a.jne(check_is_distributed);
  comment("Get next message in queue");
  emit_heavy_swapout();
  comment("SET_CP(I)");
  a.lea(TMP1, x86::qword_ptr(restart));
  a.mov(x86::qword_ptr(E), TMP1);

  a.mov(x86::qword_ptr(c_p, offsetof(Process, arity)), 0);
  a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), 0);
  a.mov(msgp, 0);
  a.mov(ARG1, c_p);
  a.mov(ARG2, FCALLS);
  a.mov(ARG3, 0); // TODO: neg_o_reds
  a.lea(ARG4, msgp);
  a.lea(ARG5, get_out);
  call((uint64_t)erts_proc_sig_receive_helper);
  a.sub(FCALLS, RET);
  emit_swapin();
  // Need to move msgp to TMP1 as check_is_distributed uses it
  a.mov(TMP1, msgp);
  a.cmp(TMP1, 0);
  a.jne(check_is_distributed);
  a.cmp(get_out, 0);
  a.je(await);
  // We have run out of reductions, jump to restart
  a.jl(restart);
  // We are got an exit signal while looking for new messages, schedule out process
  a.mov(RET, RET_do_schedule);
  farjmp(ga->get_return());
  a.bind(await);
  a.and_(x86::dword_ptr(c_p, offsetof(Process, flags)), ~F_DELAY_GC);
  a.jmp(labels[Dest.getValue()]);
  a.bind(check_is_distributed);
  a.cmp(x86::qword_ptr(TMP1, offsetof(ErtsSignal, common.tag)), THE_NON_VALUE);
  a.jne(next);
  a.sub(FCALLS, 10);
  emit_swapout();
  a.mov(ARG1, c_p);
  a.mov(ARG2, TMP1);
#ifdef DEBUG
  a.mov(ARG3, HTOP);
  a.mov(ARG4, E);
#endif
  call((uint64_t)decode_dist);
  comment("Check if decode failed");
  a.cmp(RET, false);
  a.je(restart);
  emit_swapin();
  a.bind(next);
  a.mov(TMP1, x86::qword_ptr(TMP1, offsetof(ErtsMessage, m[0])));
  mov(x0, TMP1);
}

static Sint remove_message(Process *c_p, Sint FCALLS, Eterm *HTOP, Eterm *E) {
  /*
   * Remove a (matched) message from the message queue.
   */

  ErtsMessage* msgp;

  ERTS_CHK_MBUF_SZ(c_p);

  msgp = PEEK_MESSAGE(c_p);

  if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)) {
    save_calls(c_p, &exp_receive);
  }
  if (ERL_MESSAGE_TOKEN(msgp) == NIL) {
#ifdef USE_VM_PROBES
    if (DT_UTAG(c_p) != NIL) {
      if (DT_UTAG_FLAGS(c_p) & DT_UTAG_PERMANENT) {
        SEQ_TRACE_TOKEN(c_p) = am_have_dt_utag;
      } else {
        DT_UTAG(c_p) = NIL;
        SEQ_TRACE_TOKEN(c_p) = NIL;
      }
    } else {
#endif
      SEQ_TRACE_TOKEN(c_p) = NIL;
#ifdef USE_VM_PROBES
    }
    DT_UTAG_FLAGS(c_p) &= ~DT_UTAG_SPREADING;
#endif
  } else if (ERL_MESSAGE_TOKEN(msgp) != am_undefined) {
    Eterm msg;
    SEQ_TRACE_TOKEN(c_p) = ERL_MESSAGE_TOKEN(msgp);
#ifdef USE_VM_PROBES
    if (ERL_MESSAGE_TOKEN(msgp) == am_have_dt_utag) {
      if (DT_UTAG(c_p) == NIL) {
        DT_UTAG(c_p) = ERL_MESSAGE_DT_UTAG(msgp);
      }
      DT_UTAG_FLAGS(c_p) |= DT_UTAG_SPREADING;
    } else {
#endif
      ASSERT(is_tuple(SEQ_TRACE_TOKEN(c_p)));
      ASSERT(SEQ_TRACE_TOKEN_ARITY(c_p) == 5);
      ASSERT(is_small(SEQ_TRACE_TOKEN_SERIAL(c_p)));
      ASSERT(is_small(SEQ_TRACE_TOKEN_LASTCNT(c_p)));
      ASSERT(is_small(SEQ_TRACE_TOKEN_FLAGS(c_p)));
      ASSERT(is_pid(SEQ_TRACE_TOKEN_SENDER(c_p))
             || is_atom(SEQ_TRACE_TOKEN_SENDER(c_p)));
      c_p->seq_trace_lastcnt = unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p));
      if (c_p->seq_trace_clock < unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p))) {
        c_p->seq_trace_clock = unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p));
      }
      msg = ERL_MESSAGE_TERM(msgp);
      seq_trace_output(SEQ_TRACE_TOKEN(c_p), msg, SEQ_TRACE_RECEIVE, 
                       c_p->common.id, c_p);
#ifdef USE_VM_PROBES
    }
#endif
  }
#ifdef USE_VM_PROBES
  if (DTRACE_ENABLED(message_receive)) {
    Eterm token2 = NIL;
    DTRACE_CHARBUF(receiver_name, DTRACE_TERM_BUF_SIZE);
    Sint tok_label = 0;
    Sint tok_lastcnt = 0;
    Sint tok_serial = 0;
    Sint len = erts_proc_sig_privqs_len(c_p);

    dtrace_proc_str(c_p, receiver_name);
    token2 = SEQ_TRACE_TOKEN(c_p);
    if (have_seqtrace(token2)) {
      tok_label = SEQ_TRACE_T_DTRACE_LABEL(token2);
      tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(token2));
      tok_serial = signed_val(SEQ_TRACE_T_SERIAL(token2));
    }
    DTRACE6(message_receive,
            receiver_name, size_object(ERL_MESSAGE_TERM(msgp)),
            len, /* This is NOT message queue len, but its something... */
            tok_label, tok_lastcnt, tok_serial);
  }
#endif
  UNLINK_MESSAGE(c_p, msgp);
  JOIN_MESSAGE(c_p);
  CANCEL_TIMER(c_p);

  erts_save_message_in_proc(c_p, msgp);
  c_p->flags &= ~F_DELAY_GC;

  if (ERTS_IS_GC_DESIRED_INTERNAL(c_p, HTOP, E)) {
    /*
     * We want to GC soon but we leave a few
     * reductions giving the message some time
     * to turn into garbage.
     */
    ERTS_VBUMP_LEAVE_REDS_INTERNAL(c_p, 5, FCALLS);
  }

    
  ERTS_CHK_MBUF_SZ(c_p);

  ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
  return FCALLS;
}

void BeamModuleAssembler::emit_remove_message(Instruction *I) {
  a.mov(ARG1, c_p);
  a.mov(ARG2, FCALLS);
  a.mov(ARG3, HTOP);
  a.mov(ARG4, E);
  call((uint64_t)remove_message);
  a.mov(FCALLS, RET);
}

void BeamModuleAssembler::emit_loop_rec_end(ArgVal Dest, Instruction *I) {
  ERTS_CT_ASSERT(0 == offsetof(ErtsMessage,next));
  a.mov(TMP1, x86::qword_ptr(c_p, offsetof(Process, sig_qs.save)));
  a.mov(TMP1, x86::qword_ptr(TMP1));
  a.mov(x86::qword_ptr(c_p, offsetof(Process, sig_qs.save)), TMP1);
  a.dec(FCALLS);
  a.jmp(labels[Dest.getValue()]);
}

static void take_receive_lock(Process *c_p) {
    erts_proc_lock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
}

void BeamModuleAssembler::emit_wait_unlocked(ArgVal Dest, Instruction *I) {
  a.mov(ARG1, c_p);
  call((uint64_t)take_receive_lock);

  emit_wait_locked(Dest, I);
}

void BeamModuleAssembler::emit_wait_locked(ArgVal Dest, Instruction *I) {
  a.lea(TMP3, x86::qword_ptr(labels[Dest.getValue()]));
  a.mov(RET, RET_do_wait);
  farjmp(ga->get_return());
}

enum tmo_ret {
  RET_next,
  RET_wait,
  RET_func_info
};

static enum tmo_ret
wait_timeout(Process *c_p, Eterm timeout_value, BeamInstr *next)
{
    /*
     * If we have already set the timer, we must NOT set it again.  Therefore,
     * we must test the F_INSLPQUEUE flag as well as the F_TIMO flag.
     */
    if ((c_p->flags & (F_INSLPQUEUE | F_TIMO)) == 0) {
        if (timeout_value == make_small(0)) {
            erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
            return RET_next;
        } else if (timeout_value == am_infinity) {
            c_p->flags |= F_TIMO;
        } else {
            int tres = erts_set_proc_timer_term(c_p, timeout_value);
            if (tres == 0) {
                /*
                 * The timer routiner will set c_p->i to the value in
                 * c_p->def_arg_reg[0].  Note that it is safe to use this
                 * location because there are no living x registers in
                 * a receive statement.
                 */
                BeamInstr** pi = (BeamInstr**) c_p->def_arg_reg;
                *pi = next;
            } else { /* Wrong time */
                erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
                c_p->freason = EXC_TIMEOUT_VALUE;
                return RET_func_info;
            }
        }
    }
    return RET_wait;
}

void BeamModuleAssembler::emit_wait_timeout_unlocked(ArgVal Src, ArgVal Dest, Instruction *I) {
  a.mov(ARG1, c_p);
  call((uint64_t)take_receive_lock);

  emit_wait_timeout_locked(Src, Dest, I);
}

void BeamModuleAssembler::emit_wait_timeout_locked(ArgVal Src, ArgVal Dest, Instruction *I) {
  Label wait = a.newLabel(), next = a.newLabel();
  a.mov(ARG1, c_p);
  mov(ARG2, Src);
  a.lea(ARG3, x86::qword_ptr(next));
  call((uint64_t)wait_timeout);
  a.cmp(RET, RET_wait);
  a.je(wait);
  a.cmp(RET, RET_next);
  a.je(next);
  emit_handle_error(currLabel, (ErtsCodeMFA*)nullptr);
  a.bind(wait);
  emit_wait_locked(Dest, I);
  align();
  a.bind(next);
}

static void timeout(Process *c_p)
{
    c_p->flags &= ~F_TIMO;
    JOIN_MESSAGE(c_p);
}

static void timeout_locked(Process *c_p)
{
    erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
    timeout(c_p);
}

void BeamModuleAssembler::emit_timeout_locked(Instruction *I) {
  a.mov(ARG1, c_p);
  call((uint64_t)timeout_locked);
}

void BeamModuleAssembler::emit_timeout(Instruction *I) {
  a.mov(ARG1, c_p);
  call((uint64_t)timeout);
}
