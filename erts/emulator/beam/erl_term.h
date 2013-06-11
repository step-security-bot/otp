/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2013. All Rights Reserved.
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

#ifndef __ERL_TERM_H
#define __ERL_TERM_H

/* Some tag ideas
 * * tiny is 32 bit int on 64 bit. Only useable in list, only possible with 
 *   tag in first 32 bit of word. For NaN encoding max tiny in 1 bsl 16 - 1. 
 * * vlists
 * * * maybe keep offset in bit 48+ on 64 bit?
 * * lists as boxed
 */

#include "sys.h"

#if defined(ARCH_64) && !HALFWORD_HEAP
#include "erl_term_64.h"
#else
#include "erl_term_32.h"
#endif


#endif	/* __ERL_TERM_H */

