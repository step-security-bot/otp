/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2024. All Rights Reserved.
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


#ifndef __ERL_DB_CTRIE_H__
#define __ERL_DB_CTRIE_H__

#include "erl_db_util.h" /* DbTableCommon */

#define ERTS_CTRIE_PREFIX ets_ctrie
#define ERTS_CTRIE_WANT_SNAPSHOTS
#define ERTS_CTRIE_BRANCH_FACTOR 5

#define ERTS_CTRIE_INCLUDE_TYPES_ONLY
#include "erl_ctrie.h"

typedef struct {
    DbTableCommon common;
    ets_ctrie_Trie trie;
    ets_ctrie_Trie fixed;
} DbTableCTrie;

void db_initialize_ctrie(void);

size_t erts_db_ctrie_branch_fix_alloc_size(void);
size_t erts_db_ctrie_structure_fix_alloc_size(void);

#endif /* __ERL_DB_CTRIE_H__ */
