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

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"

#include "erl_db_ctrie.h"
#include "erl_process.h"
#include "erl_threads.h"
#include "erl_db.h"

#include <stdbool.h>

typedef struct {
    ets_ctrie_SingletonNode base;

    Eterm key;
    erts_ihash_t hash;

    bool compressed;
    DbTerm term;
} DbCTrieObject;

static void db_ctrie_destroy_object(DbCTrieObject *object);

#define ERTS_CTRIE_WANT_RELEASE
#define ERTS_CTRIE_WANT_KEEP

#define ERTS_CTRIE_WANT_CLEAR
#define ERTS_CTRIE_WANT_SNAPSHOTS

#define ERTS_CTRIE_WANT_LOOKUP
#define ERTS_CTRIE_WANT_MEMBER

#define ERTS_CTRIE_WANT_ERASE
#define ERTS_CTRIE_WANT_TAKE
#define ERTS_CTRIE_WANT_REMOVE

#define ERTS_CTRIE_WANT_INSERT
#define ERTS_CTRIE_WANT_UPSERT
#define ERTS_CTRIE_WANT_UPDATE
#define ERTS_CTRIE_WANT_REPLACE

#define ERTS_CTRIE_PREFIX ets_ctrie
#define ERTS_CTRIE_KEY_TYPE Eterm
#define ERTS_CTRIE_HASH_TYPE erts_ihash_t
#define ERTS_CTRIE_KEY_EQ(a, b) eq((a),(b))
#define ERTS_CTRIE_HASH_EQ(a, b) ((a) == (b))
#define ERTS_CTRIE_HASH_GET(singleton) (((DbCTrieObject*)(singleton))->hash)
#define ERTS_CTRIE_KEY_GET(singleton) (((DbCTrieObject*)(singleton))->key)
#define ERTS_CTRIE_BRANCH_FACTOR 5
#define ERTS_CTRIE_SINGLETON_CLEANUP(singleton) ((void)(singleton))
#define ERTS_CTRIE_SINGLETON_DESTRUCTOR(singleton)                             \
    db_ctrie_destroy_object((DbCTrieObject*)(singleton))

#define ERTS_CTRIE_FIX_ALLOC_NODE
#define ERTS_CTRIE_FIX_ALLOC_BRANCH

#define ERTS_CTRIE_NODE_ALLOC_TYPE ERTS_ALC_T_ETS_CTRIE_STRUCTURE
#define ERTS_CTRIE_BRANCH_ALLOC_TYPE ERTS_ALC_T_ETS_CTRIE_BRANCH

#define ERTS_CTRIE_INCLUDE_IMPLEMENTATION_ONLY
#include "erl_ctrie.h"


size_t erts_db_ctrie_branch_fix_alloc_size() {
    return ets_ctrie_fix_alloc_branch_size();
}

size_t erts_db_ctrie_structure_fix_alloc_size() {
    return ets_ctrie_fix_alloc_node_size();
}

void db_initialize_ctrie(void) {
}

static int db_ctrie_create(Process *p, DbTable *tbl) {
    DbTableCTrie *const tb = &tbl->ctrie;
    ets_ctrie_init(&tb->trie);
    return DB_ERROR_NONE;
}

static void db_ctrie_destroy_object(DbCTrieObject *object) {
    /* FIXME: once we've moved everything to a header, we should register ETS
     * memory stuff on _cleanup_ instead of destruction. This will ensure that
     * ETS memory appears consistent, and removes the need for an indirection
     * table. */
    db_free_term_no_tab(object->compressed,
                        object,
                        offsetof(DbCTrieObject, term));
}

static Eterm db_ctrie_copy_key(Process *p, DbCTrieObject *object) {
    const Eterm key = object->key;
    Uint size;
    Eterm *hp;

    if (is_immed(object->key)) {
        return key;
    }

    size = size_object(key);
    hp = HAlloc(p, size);

    return copy_struct(key, size, &hp, &MSO(p));
}

static Eterm db_copy_object(Process *p,
                            DbTable *tbl,
                            DbCTrieObject *object,
                            Eterm *key) {
    Eterm res;

    {
        Eterm *hp = HAlloc(p, object->term.size);
        res = db_copy_object_from_ets(&tbl->common, &object->term, &hp, &MSO(p));
    }

    *key = tuple_val(res)[tbl->common.keypos];

    return res;
}

static int db_copy_element(Process *p,
                           DbTable *tbl,
                           DbCTrieObject *object,
                           int index,
                           Eterm *out) {
    Eterm *hp;
    int res;

    if (index > arityval((object->term.tpl[0]))) {
        res = DB_ERROR_BADPARAM;
    } else {
        *out = db_copy_element_from_ets(&tbl->common,
                                        p,
                                        &object->term,
                                        index,
                                        &hp,
                                        0);
        res = DB_ERROR_NONE;
    }

    return res;
}

static int db_ctrie_put(DbTable *tbl,
                        Eterm obj,
                        bool key_clash_fail,
                        SWord *consumed_reds_p) {
    DbTableCTrie *const tb = &tbl->ctrie;

    enum erts_ctrie_result res;
    DbCTrieObject *object;

    if (tb->common.compress) {
        object = db_store_term_comp(&tb->common, tb->common.keypos, NULL, offsetof(DbCTrieObject, term), obj);
    } else {
        object = db_store_term(&tb->common, NULL, offsetof(DbCTrieObject, term), obj);
    }


    object->key = GETKEY(tb, object->term.tpl);
    object->hash = erts_map_hash(object->key);
    object->compressed = tb->common.compress;
    ets_ctrie_singleton_init(&object->base);

    /* This has been manually hoisted as ERTS_SPIN_BODY has a "memory" clobber
     * that prevents the compiler from doing so. */
    if (key_clash_fail) {
        for (;;) {
            res = ets_ctrie_insert(&tb->trie, &object->base);
            if (res != CTRIE_RESTART) {
                break;
            }
            ERTS_SPIN_BODY;
        }
    } else {
        for (;;) {
            res = ets_ctrie_upsert(&tb->trie, &object->base);
            if (res != CTRIE_RESTART) {
                break;
            }
            ERTS_SPIN_BODY;
        }
    }

    if (res == CTRIE_ALREADY_EXISTS) {
        ets_ctrie_singleton_release(&object->base);
        return DB_ERROR_BADKEY;
    }

    return DB_ERROR_NONE;
}

static int db_ctrie_get(Process *p, DbTable *tbl, Eterm key, Eterm *out) {
    DbTableCTrie *const tb = &tbl->ctrie;

    enum erts_ctrie_result res;
    DbCTrieObject *object;
    erts_ihash_t hash;

    hash = erts_map_hash(key);

    for (;;) {
        res = ets_ctrie_lookup(&tb->trie,
                               key,
                               hash,
                               (ets_ctrie_SingletonNode **)&object);
        if (res != CTRIE_RESTART) {
            break;
        }

        ERTS_SPIN_BODY;
    }

    if (res == CTRIE_OK) {
        Eterm *hp = HAlloc(p, 2);
        Eterm ignored, res;
        res = db_copy_object(p, tbl, object, &ignored);
        *out = CONS(hp, res, NIL);
    } else {
        ASSERT(res == CTRIE_NOT_FOUND);
        *out = NIL;
    }

    return DB_ERROR_NONE;
}

static int db_ctrie_erase(DbTable *tbl, Eterm key, Eterm *out) {
    DbTableCTrie *const tb = &tbl->ctrie;

    enum erts_ctrie_result res;
    erts_ihash_t hash;

    hash = erts_map_hash(key);

    for (;;) {
        res = ets_ctrie_remove(&tb->trie,
                               key,
                               hash);
        if (res != CTRIE_RESTART) {
            break;
        }

        ERTS_SPIN_BODY;
    }

    *out = am_true;
    return DB_ERROR_NONE;
}

static SWord db_ctrie_free_table_continue(DbTable *tbl, SWord reds) {
    DbTableCTrie *const tb = &tbl->ctrie;

    ets_ctrie_destroy(&tb->trie);

    return reds;
}

static int db_ctrie_free_table(DbTable *tbl) {
    (void)db_ctrie_free_table_continue(tbl, ERTS_SWORD_MAX);

    return DB_ERROR_NONE;
}

DbTableMethod db_ctrie = {db_ctrie_create,
                          NULL,
                          NULL,
                          NULL, /* last */
                          NULL,  /* prev */
                          db_ctrie_put,
                          db_ctrie_get,
                          NULL,
                          NULL,
                          db_ctrie_erase,
                          NULL,
                          NULL,
                          NULL, /* db_select_chunk_catree, */
                          NULL, /* db_select_catree, */
                          NULL, /* db_select_delete_catree, */
                          NULL, /* db_select_continue_catree, */
                          NULL, /* db_select_delete_continue_catree, */
                          NULL, /* db_select_count_catree, */
                          NULL, /* db_select_count_continue_catree, */
                          NULL, /* db_select_replace_catree, */
                          NULL, /* db_select_replace_continue_catree, */
                          NULL,
                          NULL,
                          NULL,
                          db_ctrie_free_table,
                          db_ctrie_free_table_continue,
                          NULL,
                          NULL,
                          NULL, /* db_ctrie_lookup_dbterm, */
                          NULL, /* db_ctrie_finalize_dbterm, */
                          NULL, /* db_eterm_to_dbterm_tree_common, */
                          NULL, /* db_dbterm_list_append_tree_common, */
                          NULL, /* db_dbterm_list_remove_first_tree_common, */
                          NULL, /* db_put_dbterm_catree, */
                          NULL, /* db_free_dbterm_tree_common, */
                          NULL, /* db_get_dbterm_key_tree_common, */
                          NULL, /* db_get_binary_info_catree, */
                          NULL, /* raw_first same as first */
                          NULL,  /* raw_next same as next */
                          NULL, /* db_first_lookup_catree, */
                          NULL, /* db_next_lookup_catree, */
                          NULL, /* db_last_lookup_catree, */
                          NULL, /* db_prev_lookup_catree, */};