/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2007-2011. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

#ifndef __ERL_GC_H__
#define __ERL_GC_H__

#include "erl_map.h"

/* GC declarations shared by beam/erl_gc.c and hipe/hipe_gc.c */

#if defined(DEBUG) && !ERTS_GLB_INLINE_INCL_FUNC_DEF
#  define HARDDEBUG 1
#endif

#define IS_MOVED_BOXED(x)	(is_non_value((x)))

#define BOXED_FORWARD_WORD 1

#define MOVE_BOXED(PTR,HDR,HTOP,ORIG) move_boxed(&PTR,&HDR,&HTOP,&ORIG)

static void move_boxed(Eterm **rptr,Eterm *rhdr, Eterm **rhtop, Eterm **rgptr) {

    Eterm gval, *orig_ptr;
    Sint nelts;
    Eterm *PTR, *HTOP, *ORIG, HDR;
    PTR = *rptr; HTOP = *rhtop; ORIG = *rgptr; HDR = *rhdr;
    if (is_header(HDR)) {
      nelts = header_arity(HDR);
      switch ((HDR) & _HEADER_SUBTAG_MASK) {
      case SUB_BINARY_SUBTAG: nelts++; break;
      case MAP_SUBTAG: nelts+=map_get_size(PTR) + 1; break;
      case FUN_SUBTAG: nelts+=((ErlFunThing*)(PTR))->num_free+1; break;
      }
    } else {
      /* cons cell */
      /* TODO: Probably want to make a faster way for conses */
      nelts = 1;
    }
    gval    = make_boxed(HTOP);
    *ORIG   = gval;
    *HTOP++ = HDR;
    orig_ptr = PTR;
    PTR++;
    while (nelts--) *HTOP++ = *PTR++;
    orig_ptr[0] = THE_NON_VALUE;
    orig_ptr[1] = gval;

    *rptr = PTR; *rhtop = HTOP; *rgptr = ORIG; *rhdr = HDR;
}

#define in_area(ptr,start,nbytes) \
 ((UWord)((char*)(ptr) - (char*)(start)) < (nbytes))

extern Uint erts_test_long_gc_sleep;

#if defined(DEBUG) || defined(ERTS_OFFHEAP_DEBUG)
int within(Eterm *ptr, Process *p);
#endif

ERTS_GLB_INLINE Eterm follow_moved(Eterm term);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Eterm follow_moved(Eterm term)
{
    Eterm* ptr;
    switch (primary_tag(term)) {
    case TAG_PRIMARY_IMMED1:
	break;
    case TAG_PRIMARY_BOXED:
	ptr = boxed_val(term);
	if (IS_MOVED_BOXED(*ptr)) term = ptr[BOXED_FORWARD_WORD];
	break;
    default:
	ASSERT(!"strange tag in follow_moved");
    }
    return term;
}
#endif

#endif /* __ERL_GC_H__ */
