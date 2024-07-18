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

/* This is a configurable implementation of concurrent tries [1] expressed as a
 * header file. Public functions and their documentation can be found near
 * the bottom of the file, before the giant #undef section.
 *
 * There are two public types which are both opaque:
 *
 * - PREFIX_Trie
 *   * Represents a Trie, this is always user-allocated and must be initialized
 *   * with PREFIX_init unless populated by another function like PREFIX_clear
 *   * or PREFIX_snapshot.
 *   *
 *   * Regardless of how the trie was created, PREFIX_trie_destroy must be
 *   * called before deallocation.
 * - PREFIX_SingletonNode
 *   * Must be used as a _base_ for objects in the trie, which are
 *   * user-allocated and must be initialized with PREFIX_singleton_init.
 *
 * To use these types in header files, you *must* define:
 *
 * - ERTS_CTRIE_INCLUDE_TYPES_ONLY in the header
 * - ERTS_CTRIE_INCLUDE_IMPLEMENTATION_ONLY in the implementation file.
 *
 * Neither is necessary if including this in the implementation file alone.
 *
 * Mandatory #defines can be seen in the #error section below, which will let
 * you know if you've missed something. Optional #defines are:
 *
 * - ERTS_CTRIE_SINGLE_THREADED
 *   * Slightly faster version for single-threaded usage (e.g. private tables)
 *   * that doesn't use atomics. The implementation is largely the same to
 *   * support snapshots, but same operations (e.g. `take`) have stricter
 *   * ownership semantics as garbage collection is never deferred. This is
 *   * documented on a per-operation basis where relevant.
 * - ERTS_CTRIE_SINGLETON_CLEANUP(singleton)
 *   * Called when a singleton's reference count reaches zero but before
 *   * destruction (which may wait until thread progress), mainly intended to
 *   * update ETS statistics.
 * - ERTS_CTRIE_FIX_ALLOC_NODE
 *   * Define this when ERTS_CTRIE_NODE_ALLOC_TYPE is a fixed-alloc type. It
 *   * defines the function PREFIX_ctrie_fix_alloc_node_size() for retrieving
 *   * the required size.
 * - ERTS_CTRIE_FIX_ALLOC_BRANCH
 *   * Define this when ERTS_CTRIE_BRANCH_ALLOC_TYPE is a fixed-alloc type. It
 *   * defines the function PREFIX_ctrie_fix_alloc_branch_size() for retrieving
 *   * the required size.
 * - ERTS_CTRIE_HASH_EQ(a, b)
 *   * Whenever a deep comparison of keys is expected to be more expensive than
 *   * a hash comparison, this can be used to speed up inequality checks by
 *   * testing hashes before comparing keys. The default is to go straight for
 *   * a deep comparison of keys.
 * - ERTS_CTRIE_WANT_SNAPSHOTS
 *   * Whether to support atomic snapshots of the entire trie. Performance is
 *   * significantly improved when these are not required.
 * - ERTS_CTRIE_WANT_CLEAR
 *   * Whether to support atomic clear operations of the entire trie.
 *
 * - ERTS_CTRIE_WANT_ERASE
 *   * Whether to support the erase operation, which removes a specific
 *   * *instance* of an object rather than by its key.
 * - ERTS_CTRIE_WANT_REMOVE
 *   * Whether to support removal by key.
 * - ERTS_CTRIE_WANT_TAKE
 *   * Whether to support the `take` operation. In single-threaded mode, this
 *   * transfers ownership to the caller that must release the returned
 *   * object once they're done with it.
 *
 * - ERTS_CTRIE_WANT_INSERT
 *   * Whether to support insertions that fail when an object with the same key
 *   * already exists.
 * - ERTS_CTRIE_WANT_REPLACE
 *   * Whether to support atomic compare-and-swap of an object with another,
 *   * failing if the expected object (by pointer comparison) is not in the
 *   * trie.
 * - ERTS_CTRIE_WANT_UPSERT
 *   * Whether to support insert-or-update operations.
 * - ERTS_CTRIE_WANT_UPDATE
 *   * Whether to support update operations that fail when an object with the
 *   * given key does not exist.
 *
 * [1]: https://en.wikipedia.org/wiki/Ctrie */

#ifndef ERTS_CTRIE_PREFIX
#    error Missing definition of ERTS_CTRIE_PREFIX
#endif

/* This specifies the width of each branch node (in bits) which must be in the
 * range 1..5. Lower values tend to favor write performance, especially in
 * concurrent situations, and higher values tend to favor read performance. */
#if !defined(ERTS_CTRIE_BRANCH_FACTOR)
#    error Missing definition of ERTS_CTRIE_BRANCH_FACTOR
#elif (ERTS_CTRIE_BRANCH_FACTOR < 1 ||                                         \
       ERTS_CTRIE_BRANCH_FACTOR > (defined(ARCH_64) ? 6 : 5))
#    error Branch factor must be in the range 1..5
#endif

#if !defined(ERTS_CTRIE_INCLUDE_TYPES_ONLY) &&                                 \
        !defined(ERTS_CTRIE_INCLUDE_IMPLEMENTATION_ONLY)
#    define ERTS_CTRIE_INCLUDE_TYPES_ONLY
#    define ERTS_CTRIE_INCLUDE_IMPLEMENTATION_ONLY
#endif

#define CTRIE_CONCAT_MACRO_VALUES__(X, Y) X##Y
#define CTRIE_CONCAT_MACRO_VALUES(X, Y) CTRIE_CONCAT_MACRO_VALUES__(X, Y)

#define CTRIE_PUBLIC_FUNC(Name)                                                \
    CTRIE_CONCAT_MACRO_VALUES(ERTS_CTRIE_PREFIX, _##Name)

#define CTRIE_LOCAL_FUNC(Name)                                                 \
    CTRIE_CONCAT_MACRO_VALUES(ERTS_CTRIE_PREFIX, _##Name##__)

#define CTRIE_PUBLIC_TYPE(Name)                                                \
    CTRIE_CONCAT_MACRO_VALUES(ERTS_CTRIE_PREFIX, _##Name)

#define CTRIE_LOCAL_TYPE(Name)                                                 \
    CTRIE_CONCAT_MACRO_VALUES(ERTS_CTRIE_PREFIX, _##Name##__)

/* As these are defined in the type section alone, and #undef'd at the very end
 * of the file, we need to define them here to ensure that the implementation
 * section gets them as well. */
#define CTrieAtomic CTRIE_LOCAL_TYPE(Atomic)
#define CTrieAtomic64 CTRIE_LOCAL_TYPE(Atomic64)
#define CTrie CTRIE_PUBLIC_TYPE(Trie)
#define CTrieNodeBase CTRIE_LOCAL_TYPE(NodeBase)
#define CTrieSingletonNode CTRIE_PUBLIC_TYPE(SingletonNode)

#ifdef ERTS_CTRIE_INCLUDE_TYPES_ONLY

#    ifdef ERTS_CTRIE_SINGLE_THREADED
typedef erts_aint_t CTRIE_LOCAL_TYPE(Atomic);
typedef erts_aint64_t CTRIE_LOCAL_TYPE(Atomic64);
#    else
typedef erts_atomic_t CTRIE_LOCAL_TYPE(Atomic);
typedef erts_atomic64_t CTRIE_LOCAL_TYPE(Atomic64);
#    endif

#    if ERTS_CTRIE_BRANCH_FACTOR == 6
typedef Uint64 CTRIE_LOCAL_TYPE(Bitmap);
#    else
typedef Uint32 CTRIE_LOCAL_TYPE(Bitmap);
#    endif

#    define CTrieBitmap CTRIE_LOCAL_TYPE(Bitmap)

/** @brief Opaque structure representing a trie. Avert your eyes from the
 * internals. */
typedef struct {
    CTrieAtomic root;
#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
    CTrieAtomic64 sequence;
    int read_only;
#    endif
} CTRIE_PUBLIC_TYPE(Trie);

typedef struct {
    erts_refc_t refc;

    /* This is used to support a deferred reference-counting scheme where
     * references are deliberately left untouched until we either abort or
     * commit a change to the trie, greatly reducing the amount of cache
     * ping-pong on restarts.
     *
     * These fields are valid until ctrie_transaction_abort/commit, after
     * which they become meaningless, and they must not be modified once the
     * node is visible to other threads.
     *
     * An annoying wrinkle is that its deferred nature means that a node may
     * already be scheduled for cleanup by the point ctrie_transaction_commit
     * is called by the publishing thread, so we cannot save memory by making
     * this a union with the cleanup part. */
    struct {
        /* Children reachable from this node that are guaranteed not to be
         * visible to other threads. They can be cleaned up immediately on
         * abort. */
        CTrieBitmap unique;

        /* Children reachable from this node that are visible to other
         * threads, and must be reference-bumped before we destroy the CAS
         * node that our current node replaces. */
        CTrieBitmap shared;

        /* Children that are neither in `unique` nor `shared` were eagerly
         * reference-counted, and must be released on abort and left alone on
         * commit. */

        /* Singly-linked list of nodes in this transaction; used during
         * commit to bump the reference count of all `shared` nodes. */
        erts_aint_t next;
    } transaction;

    struct {
#    ifndef ERTS_CTRIE_SINGLE_THREADED
        ErtsThrPrgrLaterOp later_op;
#    endif

        /* Children that have already had their ownership moved to another
         * node.
         *
         * The idea is to cut down on refc traffic in the normal happy case of
         * a successful CAS, but it can otherwise be left zeroed. */
        CTrieBitmap moved;
    } cleanup;
} CTRIE_LOCAL_TYPE(NodeBase);

typedef struct {
    CTrieNodeBase base;

    /* User data follows, including the key and hash. */
} CTRIE_PUBLIC_TYPE(SingletonNode);

#endif /* ERTS_CTRIE_INCLUDE_TYPES_ONLY */

#ifdef ERTS_CTRIE_INCLUDE_IMPLEMENTATION_ONLY

#    ifndef ERTS_CTRIE_KEY_TYPE
#        error Missing definition of ERTS_CTRIE_KEY_TYPE
#    endif

/* ERTS_CTRIE_KEY_GET(singleton) */
#    ifndef ERTS_CTRIE_KEY_GET
#        error Missing definition of ERTS_CTRIE_KEY_GET
#    endif

/* ERTS_CTRIE_KEY_EQ(a, b) */
#    ifndef ERTS_CTRIE_KEY_EQ
#        error Missing definition of ERTS_CTRIE_KEY_EQ
#    endif

/* Hash type, must be integral at the moment. */
#    ifndef ERTS_CTRIE_HASH_TYPE
#        error Missing definition of ERTS_CTRIE_HASH_TYPE
#    endif

/* ERTS_CTRIE_HASH_GET(singleton) */
#    ifndef ERTS_CTRIE_HASH_GET
#        error Missing definition of ERTS_CTRIE_HASH_GET
#    endif

#    ifndef ERTS_CTRIE_NODE_ALLOC_TYPE
#        error Missing definition of ERTS_CTRIE_NODE_ALLOC_TYPE
#    endif

#    ifndef ERTS_CTRIE_BRANCH_ALLOC_TYPE
#        error Missing definition of ERTS_CTRIE_BRANCH_ALLOC_TYPE
#    endif

/* ERTS_CTRIE_SINGLETON_DESTRUCTOR(singleton) */
#    ifndef ERTS_CTRIE_SINGLETON_DESTRUCTOR
#        error Missing definition of ERTS_CTRIE_SINGLETON_DESTRUCTOR
#    endif

#    if !defined(ERTS_CTRIE_WANT_INSERT) && !defined(ERTS_CTRIE_WANT_UPSERT)
#        error Must define at least one of INSERT or UPSERT
#    endif

#    if !defined(ERTS_CTRIE_WANT_LOOKUP) &&                                    \
            !defined(ERTS_CTRIE_WANT_MEMBER) && !defined(ERTS_CTRIE_WANT_TAKE)
#        error Must define at least one of LOOKUP, MEMBER, or TAKE
#    endif

#    ifndef ERTS_CTRIE_HASH_EQ
#        define ERTS_CTRIE_HASH_EQ(a, b) 1
#    endif

#    ifndef __ERTS_CTRIE_COMMON___
#        define __ERTS_CTRIE_COMMON___

#        include "erl_map.h"
#        include <stdbool.h>

enum erts_ctrie_result {
    CTRIE_OK,
    CTRIE_RESTART,
    CTRIE_NOT_FOUND,
    CTRIE_ALREADY_EXISTS
};

enum erts_ctrie_gcas_node_type {
    CTRIE_GCAS_NODE_TYPE_BRANCH,
    CTRIE_GCAS_NODE_TYPE_FAILED,
    CTRIE_GCAS_NODE_TYPE_LIST,
    CTRIE_GCAS_NODE_TYPE_TOMB,

    CTRIE_GCAS_NODE_TYPE_FIRST = CTRIE_GCAS_NODE_TYPE_BRANCH,
    CTRIE_GCAS_NODE_TYPE_LAST = CTRIE_GCAS_NODE_TYPE_TOMB
};

#        define CTRIE_GCAS_NODE_PTR(raw, kind)                                 \
            (&((CTrieGCASNode *)((raw)&CTRIE_NODE_PTR_MASK__))->kind)
#        define CTRIE_GCAS_NODE_TYPE(raw)                                      \
            ((enum erts_ctrie_gcas_node_type)((raw)&CTRIE_NODE_TYPE_MASK__))
#        define CTRIE_GCAS_NODE_AS(node, as)                                   \
            (((erts_aint_t)node) | CTRIE_GCAS_NODE_TYPE_##as)

enum erts_ctrie_main_node_type {
    CTRIE_MAIN_NODE_TYPE_BRANCH = CTRIE_GCAS_NODE_TYPE_BRANCH,
    CTRIE_MAIN_NODE_TYPE_LIST = CTRIE_GCAS_NODE_TYPE_LIST,
    CTRIE_MAIN_NODE_TYPE_TOMB = CTRIE_GCAS_NODE_TYPE_TOMB
};

/* Node addresses are assumed to be at least 4-byte aligned, which ought to
 * hold on all systems that we support. */
#        define CTRIE_NODE_TYPE_MASK__ 3
#        define CTRIE_NODE_PTR_MASK__ ~((erts_aint_t)CTRIE_NODE_TYPE_MASK__)
#        define CTRIE_BASE_NODE_PTR(raw)                                       \
            (((CTrieNodeBase *)((raw)&CTRIE_NODE_PTR_MASK__)))

#        define CTRIE_MAIN_NODE_PTR(raw, kind)                                 \
            (&((CTrieMainNode *)((raw)&CTRIE_NODE_PTR_MASK__))->kind)
#        define CTRIE_MAIN_NODE_TYPE(raw)                                      \
            ((enum erts_ctrie_main_node_type)((raw)&CTRIE_NODE_TYPE_MASK__))
#        define CTRIE_MAIN_NODE_AS(node, as)                                   \
            (((erts_aint_t)node) | CTRIE_MAIN_NODE_TYPE_##as)

enum erts_ctrie_twig_node_type {
    CTRIE_TWIG_NODE_TYPE_INDIRECTION = 0,
    CTRIE_TWIG_NODE_TYPE_SINGLETON = 1,

    CTRIE_TWIG_NODE_TYPE_FIRST = CTRIE_TWIG_NODE_TYPE_INDIRECTION,
    CTRIE_TWIG_NODE_TYPE_LAST = CTRIE_TWIG_NODE_TYPE_SINGLETON
};

#        define CTRIE_TWIG_NODE_PTR(raw, kind)                                 \
            (&((CTrieTwigNode *)((raw)&CTRIE_NODE_PTR_MASK__))->kind)
#        define CTRIE_TWIG_NODE_TYPE(raw)                                      \
            ((enum erts_ctrie_twig_node_type)((raw)&CTRIE_NODE_TYPE_MASK__))
#        define CTRIE_TWIG_NODE_AS(node, as)                                   \
            (((erts_aint_t)node) | CTRIE_TWIG_NODE_TYPE_##as)

enum erts_ctrie_root_node_type {
    CTRIE_ROOT_NODE_TYPE_INDIRECTION = 0,
    CTRIE_ROOT_NODE_TYPE_PROPOSAL = 1,

    CTRIE_ROOT_NODE_TYPE_FIRST = CTRIE_ROOT_NODE_TYPE_INDIRECTION,
    CTRIE_ROOT_NODE_TYPE_LAST = CTRIE_ROOT_NODE_TYPE_PROPOSAL
};

#        define CTRIE_ROOT_NODE_PTR(raw, kind)                                 \
            (&((CTrieRootNode *)((raw)&CTRIE_NODE_PTR_MASK__))->kind)
#        define CTRIE_ROOT_NODE_TYPE(raw)                                      \
            ((enum erts_ctrie_root_node_type)((raw)&CTRIE_NODE_TYPE_MASK__))
#        define CTRIE_ROOT_NODE_AS(node, as)                                   \
            (((erts_aint_t)node) | CTRIE_ROOT_NODE_TYPE_##as)

enum erts_ctrie_ownership {
    ERTS_CTRIE_OWNERSHIP_UNIQUE,
    ERTS_CTRIE_OWNERSHIP_SHARED,
    ERTS_CTRIE_OWNERSHIP_ALREADY_REFERENCED
};

enum erts_ctrie_field {
    ERTS_CTRIE_FIELD_INDIRECTION_NODE = (1 << 0),

    ERTS_CTRIE_FIELD_LIST_SINGLETON = (1 << 1),
    ERTS_CTRIE_FIELD_LIST_NEXT = (1 << 2),

    ERTS_CTRIE_FIELD_PROPOSAL_CURRENT = (1 << 3),
    ERTS_CTRIE_FIELD_PROPOSAL_PROPOSED = (1 << 4),

    ERTS_CTRIE_FIELD_TOMB_SINGLETON = (1 << 5),

    /* Branch fields are addressed by their bitmap as usual. */
};

#    endif /* defined(__ERTS_CTRIE_COMMON___) */

#    define CTRIE_MAX_LEVEL                                                    \
        (((sizeof(ERTS_CTRIE_HASH_TYPE) * CHAR_BIT) /                          \
          ERTS_CTRIE_BRANCH_FACTOR) *                                          \
         ERTS_CTRIE_BRANCH_FACTOR)

#    define CTRIE_HASH_MASK ((1 << ERTS_CTRIE_BRANCH_FACTOR) - 1)

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#        define CTRIE_GENERATION_PARAMETER erts_aint64_t generation,
#        define CTRIE_GENERATION_ARGUMENT generation,
#    else
#        define CTRIE_GENERATION_PARAMETER
#        define CTRIE_GENERATION_ARGUMENT
#    endif

#    ifdef ERTS_CTRIE_SINGLE_THREADED
#        define CTRIE_KEEP_IF_SINGLE_THREADED(base) ctrie_node_reference(base)
#        define CTRIE_UNREACHABLE_IF_SINGLE_THREADED()                         \
            ERTS_UNREACHABLE("Unreachable in ST mode")
#    else
#        define CTRIE_KEEP_IF_SINGLE_THREADED(base) ((void)base)
#        define CTRIE_UNREACHABLE_IF_SINGLE_THREADED() ((void)0)
#    endif

/* ************************************************************************* */

#    ifdef ERTS_CTRIE_SINGLE_THREADED

#        define ctrie_atomic_cmpxchg_nob(atomic, val, old)                     \
            (ASSERT(*(atomic) == (old)), *(atomic) = (val), (old))
#        define ctrie_atomic_cmpxchg_wb(atomic, val, old)                      \
            ctrie_atomic_cmpxchg_nob((atomic), (val), (old))
#        define ctrie_atomic_inc_nob(atomic)                                   \
            do {                                                               \
                *(atomic)++;                                                   \
            } while (0)
#        define ctrie_atomic_inc_read_nob(atomic) (++*(atomic))
#        define ctrie_atomic_init_nob(atomic, val)                             \
            do {                                                               \
                *(atomic) = (val);                                             \
            } while
#        define ctrie_atomic_read_ddrb(atomic) *(atomic)
#        define ctrie_atomic_read_nob(atomic) *(atomic)
#        define ctrie_atomic_read_relb(atomic) *(atomic)
#        define ctrie_atomic_set_nob ctrie_atomic_init_nob
#        define ctrie_atomic_set_dirty ctrie_atomic_init_nob
#        define ctrie_atomic64_inc_read_nob ctrie_atomic_inc_read_nob
#        define ctrie_atomic64_init_nob ctrie_atomic_init_nob
#        define ctrie_atomic64_read_nob(atomic) *(atomic)
#        define ctrie_atomic64_set_nob ctrie_atomic_set_nob

#    else

#        define ctrie_atomic_cmpxchg_nob erts_atomic_cmpxchg_nob
#        define ctrie_atomic_cmpxchg_wb erts_atomic_cmpxchg_wb
#        define ctrie_atomic_inc_nob erts_atomic_inc_nob
#        define ctrie_atomic_inc_read_nob erts_atomic_inc_read_nob
#        define ctrie_atomic_init_nob erts_atomic_init_nob
#        define ctrie_atomic_read_ddrb erts_atomic_read_ddrb
#        define ctrie_atomic_read_nob erts_atomic_read_nob
#        define ctrie_atomic_read_relb erts_atomic_read_relb
#        define ctrie_atomic_set_nob erts_atomic_set_nob
#        define ctrie_atomic_set_dirty erts_atomic_set_dirty

#        define ctrie_atomic64_inc_read_nob erts_atomic64_inc_read_nob
#        define ctrie_atomic64_init_nob erts_atomic64_init_nob
#        define ctrie_atomic64_read_nob erts_atomic64_read_nob
#        define ctrie_atomic64_set_nob erts_atomic64_set_nob

#    endif

typedef struct {
    CTrieNodeBase base;
    CTrieAtomic previous;
#    ifdef DEBUG
    /* See `ctrie_indirection_cas` */
    CTrieAtomic debug;
#    endif
} CTRIE_LOCAL_TYPE(GCASNodeBase);

#    define CTrieGCASNodeBase CTRIE_LOCAL_TYPE(GCASNodeBase)

typedef struct {
    CTrieNodeBase base;

    /* CTrieMainNode union, type in the lowest 2 bits. */
    CTrieAtomic node;

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
    erts_aint64_t generation;
#    endif
} CTRIE_LOCAL_TYPE(IndirectionNode);

#    define CTrieIndirectionNode CTRIE_LOCAL_TYPE(IndirectionNode)

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
typedef struct {
    CTrieNodeBase base;

    CTrieIndirectionNode *proposed;
    CTrieIndirectionNode *current;
    erts_aint_t expected;

    bool committed;
} CTRIE_LOCAL_TYPE(ProposalNode);

#        define CTrieProposalNode CTRIE_LOCAL_TYPE(ProposalNode)
#    endif

typedef union {
    CTrieNodeBase base;

    CTrieIndirectionNode indirection;
#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
    CTrieProposalNode proposal;
#    endif
} CTRIE_LOCAL_TYPE(RootNode);

#    define CTrieRootNode CTRIE_LOCAL_TYPE(RootNode)

typedef struct {
    CTrieGCASNodeBase base;

    CTrieBitmap bitmap;

    /* CTrieTwigNode union, type in the lowest flag. */
#    ifdef ERTS_CTRIE_FIX_ALLOC_BRANCH
    erts_aint_t twigs[1 << ERTS_CTRIE_BRANCH_FACTOR];
#    else
    erts_aint_t twigs[];
#    endif
} CTRIE_LOCAL_TYPE(BranchNode);

#    define CTrieBranchNode CTRIE_LOCAL_TYPE(BranchNode)

typedef struct {
    CTrieGCASNodeBase base;
} CTRIE_LOCAL_TYPE(FailedNode);

#    define CTrieFailedNode CTRIE_LOCAL_TYPE(FailedNode)

typedef struct CTRIE_LOCAL_TYPE(list_node_struct) {
    CTrieGCASNodeBase base;

    struct CTRIE_LOCAL_TYPE(list_node_struct) *next;
    UWord size;

    CTrieSingletonNode *singleton;
} CTRIE_LOCAL_TYPE(ListNode);

#    define CTrieListNode CTRIE_LOCAL_TYPE(ListNode)

typedef struct {
    CTrieGCASNodeBase base;

    CTrieSingletonNode *singleton;
} CTRIE_LOCAL_TYPE(TombNode);

#    define CTrieTombNode CTRIE_LOCAL_TYPE(TombNode)

typedef union {
    CTrieGCASNodeBase base;

    CTrieBranchNode branch;
    CTrieListNode list;
    CTrieTombNode tomb;
} CTRIE_LOCAL_TYPE(MainNode);

#    define CTrieMainNode CTRIE_LOCAL_TYPE(MainNode)

typedef union {
    CTrieGCASNodeBase base;

    CTrieBranchNode branch;
#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
    CTrieFailedNode failed;
#    endif
    CTrieListNode list;
    CTrieTombNode tomb;
} CTRIE_LOCAL_TYPE(GCASNode);

#    define CTrieGCASNode CTRIE_LOCAL_TYPE(GCASNode)

typedef union {
    CTrieNodeBase base;

    CTrieIndirectionNode indirection;
    CTrieSingletonNode singleton;
} CTRIE_LOCAL_TYPE(TwigNode);

#    define CTrieTwigNode CTRIE_LOCAL_TYPE(TwigNode)

#    ifdef ERTS_CTRIE_FIX_ALLOC_NODE
typedef union {
    CTrieIndirectionNode indirection;
#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
    CTrieProposalNode proposal;
    CTrieFailedNode failed;
#        endif
    CTrieListNode list;
    CTrieTombNode tomb;

    /* Allocate at least cache line per node to reduce the risk of
     * ping-ponging on the `refc` field. */
    byte __cache_line__[ERTS_CACHE_LINE_SIZE];
} CTRIE_LOCAL_TYPE(CTrieStructuralNodes);

#        define CTrieStructuralNodes CTRIE_LOCAL_TYPE(CTrieStructuralNodes)
#    endif

typedef struct {
    erts_aint_t gcas_head;
    erts_aint_t *gcas_tail;
    erts_aint_t twig_head;
    erts_aint_t *twig_tail;
    erts_aint_t root_head;
    erts_aint_t *root_tail;
} CTRIE_LOCAL_TYPE(Transaction);

#    define CTrieTransaction CTRIE_LOCAL_TYPE(Transaction)

#    define ctrie_base_node_init CTRIE_LOCAL_FUNC(base_node_init)
static void ctrie_base_node_init(CTrieNodeBase *base) {
    ERTS_CT_ASSERT((CTRIE_GCAS_NODE_TYPE_FIRST | CTRIE_GCAS_NODE_TYPE_LAST) ==
                   CTRIE_GCAS_NODE_TYPE(CTRIE_GCAS_NODE_TYPE_FIRST |
                                        CTRIE_GCAS_NODE_TYPE_LAST));
    ERTS_CT_ASSERT((CTRIE_ROOT_NODE_TYPE_FIRST | CTRIE_ROOT_NODE_TYPE_LAST) ==
                   CTRIE_ROOT_NODE_TYPE(CTRIE_ROOT_NODE_TYPE_FIRST |
                                        CTRIE_ROOT_NODE_TYPE_LAST));
    ERTS_CT_ASSERT((CTRIE_TWIG_NODE_TYPE_FIRST | CTRIE_TWIG_NODE_TYPE_LAST) ==
                   CTRIE_TWIG_NODE_TYPE(CTRIE_TWIG_NODE_TYPE_FIRST |
                                        CTRIE_TWIG_NODE_TYPE_LAST));
    ERTS_CT_ASSERT((ERTS_ALLOC_ALIGN_BYTES & CTRIE_NODE_TYPE_MASK__) == 0);
    ASSERT((((erts_aint_t)base) & CTRIE_NODE_TYPE_MASK__) == 0);

    ctrie_atomic_init_nob(&base->refc, 1);
}

/* ************************************************************************* */

#    define ctrie_node_gcas_destroy CTRIE_LOCAL_FUNC(node_gcas_destroy)
static void ctrie_node_gcas_destroy(erts_aint_t node);

#    define ctrie_node_root_destroy CTRIE_LOCAL_FUNC(node_root_destroy)
static void ctrie_node_root_destroy(erts_aint_t node);

#    define ctrie_node_twig_destroy CTRIE_LOCAL_FUNC(node_twig_destroy)
static void ctrie_node_twig_destroy(erts_aint_t node);

#    define ctrie_node_gcas_abort CTRIE_LOCAL_FUNC(node_gcas_abort)
static void ctrie_node_gcas_abort(erts_aint_t node);

#    define ctrie_node_root_abort CTRIE_LOCAL_FUNC(node_root_abort)
static void ctrie_node_root_abort(erts_aint_t node);

#    define ctrie_node_twig_abort CTRIE_LOCAL_FUNC(node_twig_abort)
static void ctrie_node_twig_abort(erts_aint_t node);

#    define ctrie_node_reference CTRIE_LOCAL_FUNC(node_reference)
static void ctrie_node_reference(CTrieNodeBase *base) {
    erts_refc_inctest(&base->refc, 2);
}

#    define ctrie_node_gcas_release CTRIE_LOCAL_FUNC(node_gcas_release)
static void ctrie_node_gcas_release(erts_aint_t gcas) {
    CTrieNodeBase *base = CTRIE_BASE_NODE_PTR(gcas);

    if (erts_refc_dectest(&base->refc, 0) == 0) {
#    ifdef ERTS_CTRIE_WANT_SINGLE_THREADED
        ctrie_node_gcas_destroy(gcas);
#    else
        base->cleanup.moved = 0;
        erts_schedule_thr_prgr_later_op(
                (void (*)(void *))ctrie_node_gcas_destroy,
                (void *)gcas,
                &base->cleanup.later_op);
#    endif
    }
}

#    define ctrie_node_root_release CTRIE_LOCAL_FUNC(node_root_release)
static void ctrie_node_root_release(erts_aint_t root) {
    CTrieNodeBase *base = CTRIE_BASE_NODE_PTR(root);

    if (erts_refc_dectest(&base->refc, 0) == 0) {
#    ifdef ERTS_CTRIE_WANT_SINGLE_THREADED
        ctrie_node_root_destroy(root);
#    else
        base->cleanup.moved = 0;
        erts_schedule_thr_prgr_later_op(
                (void (*)(void *))ctrie_node_root_destroy,
                (void *)root,
                &base->cleanup.later_op);
#    endif
    }
}

#    define ctrie_node_twig_release CTRIE_LOCAL_FUNC(node_twig_release)
static void ctrie_node_twig_release(erts_aint_t twig) {
    CTrieNodeBase *base = CTRIE_BASE_NODE_PTR(twig);

    if (erts_refc_dectest(&base->refc, 0) == 0) {
#    ifdef ERTS_CTRIE_SINGLETON_CLEANUP
        if (CTRIE_TWIG_NODE_TYPE(twig) == CTRIE_TWIG_NODE_TYPE_SINGLETON) {
            ERTS_CTRIE_SINGLETON_CLEANUP(CTRIE_TWIG_NODE_PTR(twig, singleton));
        }
#    endif

#    ifdef ERTS_CTRIE_WANT_SINGLE_THREADED
        ctrie_node_twig_destroy(twig);
#    else
        base->cleanup.moved = 0;
        erts_schedule_thr_prgr_later_op(
                (void (*)(void *))ctrie_node_twig_destroy,
                (void *)twig,
                &base->cleanup.later_op);
#    endif
    }
}

#    define ctrie_transaction_init CTRIE_LOCAL_FUNC(transaction_init)
static void ctrie_transaction_init(CTrieTransaction *ts) {
    ts->gcas_tail = &ts->gcas_head;
    ts->gcas_head = ERTS_AINT_NULL;
    ts->root_tail = &ts->root_head;
    ts->root_head = ERTS_AINT_NULL;
    ts->twig_tail = &ts->twig_head;
    ts->twig_head = ERTS_AINT_NULL;
}

#    define ctrie_node_branch_commit CTRIE_LOCAL_FUNC(node_branch_commit)
static void ctrie_node_branch_commit(CTrieBranchNode *branch) {
    const CTrieBitmap shared = branch->base.base.transaction.shared;
    const CTrieBitmap unique = branch->base.base.transaction.unique;

    ASSERT((shared & unique) == 0);
    (void)unique;

    /* The vast majority of committed branches will have no shared entries at
     * all because of ownership transfers. */
    if (ERTS_UNLIKELY(shared)) {
        for (CTrieBitmap iterator = branch->bitmap, index = 0; iterator > 0;
             index++) {
            const CTrieBitmap next = iterator & (iterator - 1);
            const CTrieBitmap flag = iterator ^ next;

            ASSERT(index < (1 << ERTS_CTRIE_BRANCH_FACTOR));

            ASSERT((flag & (flag - 1)) == 0);
            if (shared & flag) {
                ctrie_node_reference(CTRIE_BASE_NODE_PTR(branch->twigs[index]));
            }

            iterator = next;
        }
    }
}

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#        define ctrie_node_failed_commit CTRIE_LOCAL_FUNC(node_failed_commit)
static void ctrie_node_failed_commit(CTrieFailedNode *failed) {
    (void)failed;
}
#    endif

#    define ctrie_node_list_commit CTRIE_LOCAL_FUNC(node_list_commit)
static void ctrie_node_list_commit(CTrieListNode *list) {
    const CTrieBitmap shared = list->base.base.transaction.shared;
    const CTrieBitmap unique = list->base.base.transaction.unique;

    ASSERT((shared & unique) == 0);
    (void)unique;

    if (shared & ERTS_CTRIE_FIELD_LIST_SINGLETON) {
        ctrie_node_reference(&(list->singleton)->base);
    }

    if ((shared & ERTS_CTRIE_FIELD_LIST_NEXT) && list->next != NULL) {
        ctrie_node_reference(&(list->next)->base.base);
    }
}

#    define ctrie_node_tomb_commit CTRIE_LOCAL_FUNC(node_tomb_commit)
static void ctrie_node_tomb_commit(CTrieTombNode *tomb) {
    const CTrieBitmap shared = tomb->base.base.transaction.shared;
    const CTrieBitmap unique = tomb->base.base.transaction.unique;

    ASSERT((shared & unique) == 0);
    (void)unique;
    (void)shared;

    ASSERT(shared & ERTS_CTRIE_FIELD_TOMB_SINGLETON);
    ctrie_node_reference(&(tomb->singleton)->base);
}

#    define ctrie_node_gcas_commit CTRIE_LOCAL_FUNC(node_gcas_commit)
static void ctrie_node_gcas_commit(erts_aint_t node) {
    switch (CTRIE_GCAS_NODE_TYPE(node)) {
    case CTRIE_GCAS_NODE_TYPE_BRANCH:
        ctrie_node_branch_commit(CTRIE_GCAS_NODE_PTR(node, branch));
        return;
    case CTRIE_GCAS_NODE_TYPE_FAILED:
#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
        ctrie_node_failed_commit(CTRIE_GCAS_NODE_PTR(node, failed));
        return;
#    else
        break;
#    endif
    case CTRIE_GCAS_NODE_TYPE_LIST:
        ctrie_node_list_commit(CTRIE_GCAS_NODE_PTR(node, list));
        return;
    case CTRIE_GCAS_NODE_TYPE_TOMB:
        ctrie_node_tomb_commit(CTRIE_GCAS_NODE_PTR(node, tomb));
        return;
    }

    ERTS_UNREACHABLE("Invalid GCAS node type");
}

#    define ctrie_node_indirection_commit                                      \
        CTRIE_LOCAL_FUNC(node_indirection_commit)
static void ctrie_node_indirection_commit(CTrieIndirectionNode *indirection) {
    const CTrieBitmap shared = indirection->base.transaction.shared;
    const CTrieBitmap unique = indirection->base.transaction.unique;
    erts_aint_t node = ctrie_atomic_read_ddrb(&indirection->node);

    ASSERT((shared & unique) == 0);
    (void)shared;
    (void)unique;

    if ((shared & ERTS_CTRIE_FIELD_INDIRECTION_NODE)) {
        ctrie_node_twig_release(node);
    }
}

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#        define ctrie_node_proposal_commit                                     \
            CTRIE_LOCAL_FUNC(node_proposal_commit)
static void ctrie_node_proposal_commit(CTrieProposalNode *proposal) {
    const CTrieBitmap shared = proposal->base.transaction.shared;
    const CTrieBitmap unique = proposal->base.transaction.unique;

    ASSERT((shared & unique) == 0);
    (void)unique;

    if (shared & ERTS_CTRIE_FIELD_PROPOSAL_CURRENT) {
        ctrie_node_reference(&(proposal->current)->base);
    }

    if (shared & ERTS_CTRIE_FIELD_PROPOSAL_PROPOSED) {
        ctrie_node_reference(&(proposal->proposed)->base);
    }
}
#    endif /* ERTS_CTRIE_WANT_SNAPSHOTS */

#    define ctrie_node_root_commit CTRIE_LOCAL_FUNC(node_root_commit)
static void ctrie_node_root_commit(erts_aint_t node) {
    switch (CTRIE_ROOT_NODE_TYPE(node)) {
    case CTRIE_ROOT_NODE_TYPE_INDIRECTION:
        ctrie_node_indirection_commit(CTRIE_ROOT_NODE_PTR(node, indirection));
        return;
    case CTRIE_ROOT_NODE_TYPE_PROPOSAL:
#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
        ctrie_node_proposal_commit(CTRIE_ROOT_NODE_PTR(node, proposal));
        return;
#    else
        /* Only valid with snapshots. */
        break;
#    endif
    }

    ERTS_UNREACHABLE("Invalid root node type");
}

#    define ctrie_node_twig_commit CTRIE_LOCAL_FUNC(node_twig_commit)
static void ctrie_node_twig_commit(erts_aint_t node) {
    switch (CTRIE_TWIG_NODE_TYPE(node)) {
    case CTRIE_TWIG_NODE_TYPE_INDIRECTION:
        ctrie_node_indirection_commit(CTRIE_TWIG_NODE_PTR(node, indirection));
        return;
    case CTRIE_TWIG_NODE_TYPE_SINGLETON:
        /* Should never happen with singletons. */
        break;
    }

    ERTS_UNREACHABLE("Invalid twig node type");
}

#    define ctrie_transaction_commit CTRIE_LOCAL_FUNC(transaction_commit)
static void ERTS_NOINLINE ctrie_transaction_commit(CTrieTransaction *ts) {
    erts_aint_t iterator;

    iterator = ts->gcas_head;
    while (iterator != ERTS_AINT_NULL) {
        erts_aint_t next = CTRIE_BASE_NODE_PTR(iterator)->transaction.next;
        ctrie_node_gcas_commit(iterator);
        iterator = next;
    }

    iterator = ts->root_head;
    while (iterator != ERTS_AINT_NULL) {
        erts_aint_t next = CTRIE_BASE_NODE_PTR(iterator)->transaction.next;
        ctrie_node_root_commit(iterator);
        iterator = next;
    }

    iterator = ts->twig_head;
    while (iterator != ERTS_AINT_NULL) {
        erts_aint_t next = CTRIE_BASE_NODE_PTR(iterator)->transaction.next;
        ctrie_node_twig_commit(iterator);
        iterator = next;
    }
}

/* ************************************************************************* */

#    define ctrie_branch_abort CTRIE_LOCAL_FUNC(branch_abort)
static void ctrie_branch_abort(CTrieBranchNode *branch) {
    const CTrieBitmap shared = branch->base.base.transaction.shared;
    const CTrieBitmap unique = branch->base.base.transaction.unique;

    ASSERT((shared & unique) == 0);

    for (CTrieBitmap iterator = branch->bitmap, index = 0; iterator > 0;
         index++) {
        const CTrieBitmap next = iterator & (iterator - 1);
        const CTrieBitmap flag = iterator ^ next;
        erts_aint_t twig;

        ASSERT(index < (1 << ERTS_CTRIE_BRANCH_FACTOR));
        twig = branch->twigs[index];

        ASSERT((flag & (flag - 1)) == 0);

        if (unique & flag) {
            /* Only seen through us, get rid of it immediately. */
            ctrie_node_twig_abort(twig);
        } else if (!(shared & flag)) {
            /* Already referenced, release it. */
            ctrie_node_twig_release(twig);
        }

        /* Shared entries are left alone; they were intended to be referenced
         * on commit, but that is now moot. */

        iterator = next;
    }

    erts_free(ERTS_CTRIE_BRANCH_ALLOC_TYPE, branch);
}

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#        define ctrie_failed_abort CTRIE_LOCAL_FUNC(failed_abort)
static void ctrie_failed_abort(CTrieFailedNode *failed) {
    erts_free(ERTS_CTRIE_NODE_ALLOC_TYPE, failed);
}
#    endif /* ERTS_CTRIE_WANT_SNAPSHOTS */

#    define ctrie_list_abort CTRIE_LOCAL_FUNC(list_abort)
static void ctrie_list_abort(CTrieListNode *list) {
    const CTrieBitmap shared = list->base.base.transaction.shared;
    const CTrieBitmap unique = list->base.base.transaction.unique;

    ASSERT((shared & unique) == 0);

    /* Aborts on singletons no-op, so we won't touch the field unless it was
     * already referenced. */

    if (!((shared | unique) & ERTS_CTRIE_FIELD_LIST_SINGLETON)) {
        ctrie_node_twig_release(CTRIE_TWIG_NODE_AS(list->singleton, SINGLETON));
    }

    if (list->next != NULL) {
        if (unique & ERTS_CTRIE_FIELD_LIST_NEXT) {
            ctrie_list_abort(list->next);
        } else if (!(shared & ERTS_CTRIE_FIELD_LIST_NEXT)) {
            ctrie_node_gcas_release(CTRIE_GCAS_NODE_AS(list->next, LIST));
        }
    }

    erts_free(ERTS_CTRIE_NODE_ALLOC_TYPE, list);
}

#    define ctrie_tomb_abort CTRIE_LOCAL_FUNC(tomb_abort)
static void ctrie_tomb_abort(CTrieTombNode *tomb) {
    const CTrieBitmap unique = tomb->base.base.transaction.unique;
    const CTrieBitmap shared = tomb->base.base.transaction.shared;

    ASSERT((unique & shared) == 0);
    (void)unique;
    (void)shared;

    /* Singletons in tomb nodes are ALWAYS shared, never unique or already
     * referenced, and should thus not be released on aborts. */
    ASSERT(shared & ERTS_CTRIE_FIELD_TOMB_SINGLETON);

    erts_free(ERTS_CTRIE_NODE_ALLOC_TYPE, tomb);
}

static void ctrie_node_gcas_abort(erts_aint_t node) {
    switch (CTRIE_GCAS_NODE_TYPE(node)) {
    case CTRIE_GCAS_NODE_TYPE_BRANCH:
        ctrie_branch_abort(CTRIE_GCAS_NODE_PTR(node, branch));
        return;
    case CTRIE_GCAS_NODE_TYPE_FAILED:
#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
        ctrie_failed_abort(CTRIE_GCAS_NODE_PTR(node, failed));
        return;
#    else
        break;
#    endif
    case CTRIE_GCAS_NODE_TYPE_LIST:
        ctrie_list_abort(CTRIE_GCAS_NODE_PTR(node, list));
        return;
    case CTRIE_GCAS_NODE_TYPE_TOMB:
        ctrie_tomb_abort(CTRIE_GCAS_NODE_PTR(node, tomb));
        return;
    }

    ERTS_UNREACHABLE("Invalid GCAS node type");
}

#    define ctrie_indirection_abort CTRIE_LOCAL_FUNC(indirection_abort)
static void ctrie_indirection_abort(CTrieIndirectionNode *indirection) {
    const CTrieBitmap unique = indirection->base.transaction.unique;
    const CTrieBitmap shared = indirection->base.transaction.shared;
    erts_aint_t node;

    ASSERT((unique & shared) == 0);

    /* We KNOW that no one else could have modified this field as we haven't
     * published the node to the outside world. */
    node = ctrie_atomic_read_ddrb(&indirection->node);

    if (unique & ERTS_CTRIE_FIELD_INDIRECTION_NODE) {
        ctrie_node_gcas_abort(node);
    } else if (!(shared & ERTS_CTRIE_FIELD_INDIRECTION_NODE)) {
        ctrie_node_gcas_release(node);
    }

    erts_free(ERTS_CTRIE_NODE_ALLOC_TYPE, indirection);
}

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#        define ctrie_proposal_abort CTRIE_LOCAL_FUNC(proposal_abort)
static void ctrie_proposal_abort(CTrieProposalNode *proposal) {
    const CTrieBitmap unique = proposal->base.transaction.unique;
    const CTrieBitmap shared = proposal->base.transaction.shared;
    erts_aint_t current, proposed;

    current = CTRIE_ROOT_NODE_AS(proposal->current, INDIRECTION);
    proposed = CTRIE_ROOT_NODE_AS(proposal->proposed, INDIRECTION);

    if (unique & ERTS_CTRIE_FIELD_PROPOSAL_CURRENT) {
        ctrie_node_root_abort(current);
    } else if (!(shared & ERTS_CTRIE_FIELD_PROPOSAL_CURRENT)) {
        ctrie_node_root_release(current);
    }

    if (unique & ERTS_CTRIE_FIELD_PROPOSAL_PROPOSED) {
        ctrie_node_root_abort(proposed);
    } else if (!(shared & ERTS_CTRIE_FIELD_PROPOSAL_PROPOSED)) {
        ctrie_node_root_release(proposed);
    }

    erts_free(ERTS_CTRIE_NODE_ALLOC_TYPE, proposal);
}
#    endif /* ERTS_CTRIE_WANT_SNAPSHOTS */

static void ctrie_node_root_abort(erts_aint_t node) {
    switch (CTRIE_ROOT_NODE_TYPE(node)) {
    case CTRIE_ROOT_NODE_TYPE_INDIRECTION:
        ctrie_indirection_abort(CTRIE_ROOT_NODE_PTR(node, indirection));
        return;
    case CTRIE_ROOT_NODE_TYPE_PROPOSAL:
#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
        ctrie_proposal_abort(CTRIE_ROOT_NODE_PTR(node, proposal));
        return;
#    else
        /* Only valid with snapshots. */
        break;
#    endif
    }

    ERTS_UNREACHABLE("Invalid root node type");
}

static void ctrie_node_twig_abort(erts_aint_t node) {
    switch (CTRIE_TWIG_NODE_TYPE(node)) {
    case CTRIE_TWIG_NODE_TYPE_INDIRECTION:
        ctrie_indirection_abort(CTRIE_TWIG_NODE_PTR(node, indirection));
        return;
    case CTRIE_TWIG_NODE_TYPE_SINGLETON:
        /* Deliberate no-op to simplify ownership transfers, see
         * ctrie_branch_add_singleton for more details. */
        return;
    }

    ERTS_UNREACHABLE("Invalid twig node type");
}

/* ************************************************************************* */

#    define ctrie_branch_destroy CTRIE_LOCAL_FUNC(branch_destroy)
static void ctrie_branch_destroy(CTrieBranchNode *branch) {
    const CTrieBitmap moved = branch->base.base.cleanup.moved;

    /* The vast majority of old branches transfer ownership of all but a single
     * twig to its replacement. Opt for a slower variant that does not need
     * to visit every single twig. */
    for (CTrieBitmap iterator = branch->bitmap & ~moved; iterator > 0;) {
        const CTrieBitmap next = iterator & (iterator - 1);
        const CTrieBitmap flag = iterator ^ next;

        int index = hashmap_bitcount(branch->bitmap & (flag - 1));
        ctrie_node_twig_release(branch->twigs[index]);

        iterator = next;
    }

    erts_free(ERTS_CTRIE_BRANCH_ALLOC_TYPE, branch);
}

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#        define ctrie_failed_destroy CTRIE_LOCAL_FUNC(failed_destroy)
static void ctrie_failed_destroy(CTrieFailedNode *failed) {
    erts_free(ERTS_CTRIE_NODE_ALLOC_TYPE, failed);
}
#    endif

#    define ctrie_list_destroy CTRIE_LOCAL_FUNC(list_destroy)
static void ctrie_list_destroy(CTrieListNode *list) {
    const CTrieBitmap moved = list->base.base.cleanup.moved;
    CTrieListNode *next = list->next;

    if (!(moved & ERTS_CTRIE_FIELD_LIST_SINGLETON)) {
        ctrie_node_twig_release(CTRIE_TWIG_NODE_AS(list->singleton, SINGLETON));
    }

    if (!(moved & ERTS_CTRIE_FIELD_LIST_NEXT) && next != NULL) {
        ctrie_node_gcas_release(CTRIE_GCAS_NODE_AS(next, LIST));
    }

    erts_free(ERTS_CTRIE_NODE_ALLOC_TYPE, list);
}

#    define ctrie_tomb_destroy CTRIE_LOCAL_FUNC(tomb_destroy)
static void ctrie_tomb_destroy(CTrieTombNode *tomb) {
    const CTrieBitmap moved = tomb->base.base.cleanup.moved;

    if (!(moved & ERTS_CTRIE_FIELD_TOMB_SINGLETON)) {
        ctrie_node_twig_release(CTRIE_TWIG_NODE_AS(tomb->singleton, SINGLETON));
    }

    erts_free(ERTS_CTRIE_NODE_ALLOC_TYPE, tomb);
}

static void ctrie_node_gcas_destroy(erts_aint_t node) {
    switch (CTRIE_GCAS_NODE_TYPE(node)) {
    case CTRIE_GCAS_NODE_TYPE_BRANCH:
        ctrie_branch_destroy(CTRIE_GCAS_NODE_PTR(node, branch));
        return;
    case CTRIE_GCAS_NODE_TYPE_FAILED:
#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
        ctrie_failed_destroy(CTRIE_GCAS_NODE_PTR(node, failed));
        return;
#    else
        /* Only valid with snapshots. */
        break;
#    endif
    case CTRIE_GCAS_NODE_TYPE_LIST:
        ctrie_list_destroy(CTRIE_GCAS_NODE_PTR(node, list));
        return;
    case CTRIE_GCAS_NODE_TYPE_TOMB:
        ctrie_tomb_destroy(CTRIE_GCAS_NODE_PTR(node, tomb));
        return;
    }

    ERTS_UNREACHABLE("Invalid GCAS node type");
}

#    define ctrie_indirection_destroy CTRIE_LOCAL_FUNC(indirection_destroy)
static void ctrie_indirection_destroy(CTrieIndirectionNode *indirection) {
    const CTrieBitmap moved = indirection->base.cleanup.moved;

    if (!(moved & ERTS_CTRIE_FIELD_INDIRECTION_NODE)) {
        ctrie_node_gcas_release(ctrie_atomic_read_ddrb(&indirection->node));
    }

    erts_free(ERTS_CTRIE_NODE_ALLOC_TYPE, indirection);
}

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#        define ctrie_proposal_destroy CTRIE_LOCAL_FUNC(proposal_destroy)
static void ctrie_proposal_destroy(CTrieProposalNode *proposal) {
    const CTrieBitmap moved = proposal->base.cleanup.moved;
    erts_aint_t current, proposed;

    current = CTRIE_ROOT_NODE_AS(proposal->current, INDIRECTION);
    proposed = CTRIE_ROOT_NODE_AS(proposal->proposed, INDIRECTION);

    if (!(moved & ERTS_CTRIE_FIELD_PROPOSAL_CURRENT)) {
        ctrie_node_root_release(current);
    }

    if (!(moved & ERTS_CTRIE_FIELD_PROPOSAL_PROPOSED)) {
        ctrie_node_root_release(proposed);
    }

    erts_free(ERTS_CTRIE_NODE_ALLOC_TYPE, proposal);
}
#    endif

static void ctrie_node_root_destroy(erts_aint_t node) {
    switch (CTRIE_ROOT_NODE_TYPE(node)) {
    case CTRIE_ROOT_NODE_TYPE_INDIRECTION:
        ctrie_indirection_destroy(CTRIE_ROOT_NODE_PTR(node, indirection));
        return;
    case CTRIE_ROOT_NODE_TYPE_PROPOSAL:
#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
        ctrie_proposal_destroy(CTRIE_ROOT_NODE_PTR(node, proposal));
        return;
#    else
        /* Only valid with snapshots. */
        break;
#    endif
    }

    ERTS_UNREACHABLE("Invalid root node type");
}

static void ctrie_node_twig_destroy(erts_aint_t node) {
    switch (CTRIE_TWIG_NODE_TYPE(node)) {
    case CTRIE_TWIG_NODE_TYPE_INDIRECTION:
        ctrie_indirection_destroy(CTRIE_TWIG_NODE_PTR(node, indirection));
        return;
    case CTRIE_TWIG_NODE_TYPE_SINGLETON:
        ERTS_CTRIE_SINGLETON_DESTRUCTOR(CTRIE_TWIG_NODE_PTR(node, singleton));
        return;
    }
}

/* ************************************************************************* */

#    define ctrie_alloc_gcas CTRIE_LOCAL_FUNC(alloc_gcas)
static CTrieGCASNode *ctrie_alloc_gcas(CTrieTransaction *transaction,
                                       ErtsAlcType_t alloc_type,
                                       size_t size,
                                       enum erts_ctrie_gcas_node_type kind) {
    CTrieGCASNode *res = erts_alloc(alloc_type, size);
    ASSERT(size >= sizeof(CTrieGCASNodeBase));

    ctrie_base_node_init(&res->base.base);

#    ifdef DEBUG
    ctrie_atomic_init_nob(&res->base.debug, ERTS_AINT_NULL);
#    endif

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
    ctrie_atomic_init_nob(&res->base.previous, ERTS_AINT_NULL);
#    endif

#    ifndef ERTS_CTRIE_SINGLE_THREADED
    res->base.base.transaction.next = ERTS_AINT_NULL;
    *transaction->gcas_tail = ((erts_aint_t)res) | kind;
    transaction->gcas_tail = &res->base.base.transaction.next;
#    endif

    return res;
}

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#        define ctrie_alloc_root CTRIE_LOCAL_FUNC(alloc_root)
static CTrieRootNode *ctrie_alloc_root(CTrieTransaction *transaction,
                                       ErtsAlcType_t alloc_type,
                                       size_t size,
                                       enum erts_ctrie_root_node_type kind) {
    CTrieRootNode *res = erts_alloc(alloc_type, size);
    ASSERT(size >= sizeof(CTrieNodeBase));

    ctrie_base_node_init(&res->base);

#        ifndef ERTS_CTRIE_SINGLE_THREADED
    res->base.transaction.next = ERTS_AINT_NULL;
    *transaction->root_tail = ((erts_aint_t)res) | kind;
    transaction->root_tail = &res->base.transaction.next;
#        endif

    return res;
}
#    endif

#    define ctrie_alloc_twig CTRIE_LOCAL_FUNC(alloc_twig)
static CTrieTwigNode *ctrie_alloc_twig(CTrieTransaction *transaction,
                                       ErtsAlcType_t alloc_type,
                                       size_t size,
                                       enum erts_ctrie_twig_node_type kind) {
    CTrieTwigNode *res = erts_alloc(alloc_type, size);
    ASSERT(size >= sizeof(CTrieNodeBase));

    ctrie_base_node_init(&res->base);

#    ifndef ERTS_CTRIE_SINGLE_THREADED
    res->base.transaction.next = ERTS_AINT_NULL;
    *transaction->twig_tail = ((erts_aint_t)res) | kind;
    transaction->twig_tail = &res->base.transaction.next;
#    endif

    return res;
}

#    ifdef ERTS_CTRIE_FIX_ALLOC_NODE
#        define ctrie_node_make(kind, ts, ...)                                 \
            CTRIE_##kind##_MAKE(ts,                                            \
                                ERTS_CTRIE_NODE_ALLOC_TYPE,                    \
                                sizeof(CTrieStructuralNodes),                  \
                                __VA_ARGS__)
#    else
#        define ctrie_node_make(kind, ts, ...)                                 \
            CTRIE_##kind##_MAKE(ts,                                            \
                                ERTS_CTRIE_NODE_ALLOC_TYPE,                    \
                                sizeof(kind),                                  \
                                __VA_ARGS__)
#    endif

#    define ctrie_branch_make CTRIE_LOCAL_FUNC(branch_make)
static CTrieBranchNode *ctrie_branch_make(CTrieTransaction *ts,
                                          CTrieBitmap bitmap,
                                          CTrieBitmap shared,
                                          CTrieBitmap unique) {
    CTrieBranchNode *res;

    ASSERT((bitmap & shared) == shared);
    ASSERT((bitmap & unique) == unique);
    ASSERT((shared & unique) == 0);

    res = (CTrieBranchNode *)ctrie_alloc_gcas(
            ts,
            ERTS_CTRIE_BRANCH_ALLOC_TYPE,
            sizeof(CTrieBranchNode)
#    ifndef ERTS_CTRIE_FIX_ALLOC_BRANCH
                    + sizeof(Eterm) * hashmap_bitcount(bitmap)
#    endif
                    ,
            CTRIE_GCAS_NODE_TYPE_BRANCH);

    res->base.base.transaction.shared = shared;
    res->base.base.transaction.unique = unique;
    res->bitmap = bitmap;

    return res;
}

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#        define CTRIE_CTrieFailedNode_MAKE CTRIE_LOCAL_FUNC(failed_make)
static CTrieFailedNode *CTRIE_CTrieFailedNode_MAKE(CTrieTransaction *ts,
                                                   ErtsAlcType_t alloc_type,
                                                   size_t size,
                                                   erts_aint_t previous) {
    CTrieFailedNode *res = (CTrieFailedNode *)
            ctrie_alloc_gcas(ts, alloc_type, size, CTRIE_GCAS_NODE_TYPE_FAILED);
    ASSERT(size >= sizeof(CTrieFailedNode));

    ctrie_atomic_init_nob(&res->base.previous, previous);

    return res;
}
#    endif

#    define CTRIE_CTrieIndirectionNode_MAKE CTRIE_LOCAL_FUNC(indirection_make)
static CTrieIndirectionNode *CTRIE_CTrieIndirectionNode_MAKE(
        CTrieTransaction *ts,
        ErtsAlcType_t alloc_type,
        size_t size,
        CTRIE_GENERATION_PARAMETER erts_aint_t node,
        enum erts_ctrie_ownership ownership) {
    CTrieIndirectionNode *res = (CTrieIndirectionNode *)ctrie_alloc_twig(
            ts,
            alloc_type,
            size,
            CTRIE_TWIG_NODE_TYPE_INDIRECTION);
    ASSERT(size >= sizeof(CTrieIndirectionNode));

    /* Shared ownership does not work since it's impossible to defer reference
     * counting until after commit: the `node` field could have changed after
     * being published, but before being committed. */
    ASSERT(ownership == ERTS_CTRIE_OWNERSHIP_UNIQUE ||
           ownership == ERTS_CTRIE_OWNERSHIP_ALREADY_REFERENCED);

    ctrie_atomic_init_nob(&res->node, node);

    res->base.transaction.shared = 0;
    res->base.transaction.unique = (ownership == ERTS_CTRIE_OWNERSHIP_UNIQUE)
                                           ? ERTS_CTRIE_FIELD_INDIRECTION_NODE
                                           : 0;

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
    res->generation = generation;
#    endif

    return res;
}

#    define CTRIE_CTrieListNode_MAKE CTRIE_LOCAL_FUNC(list_make)
static CTrieListNode *CTRIE_CTrieListNode_MAKE(
        CTrieTransaction *ts,
        ErtsAlcType_t alloc_type,
        size_t size,
        CTrieSingletonNode *singleton,
        enum erts_ctrie_ownership singleton_ownership,
        CTrieListNode *next,
        enum erts_ctrie_ownership next_ownership) {
    CTrieListNode *res = (CTrieListNode *)
            ctrie_alloc_gcas(ts, alloc_type, size, CTRIE_GCAS_NODE_TYPE_LIST);
    ASSERT(size >= sizeof(CTrieListNode));

    res->base.base.transaction.shared =
            ((singleton_ownership == ERTS_CTRIE_OWNERSHIP_SHARED)
                     ? ERTS_CTRIE_FIELD_LIST_SINGLETON
                     : 0) |
            ((next_ownership == ERTS_CTRIE_OWNERSHIP_SHARED)
                     ? ERTS_CTRIE_FIELD_LIST_NEXT
                     : 0);
    res->base.base.transaction.unique =
            ((singleton_ownership == ERTS_CTRIE_OWNERSHIP_UNIQUE)
                     ? ERTS_CTRIE_FIELD_LIST_SINGLETON
                     : 0) |
            ((next_ownership == ERTS_CTRIE_OWNERSHIP_UNIQUE)
                     ? ERTS_CTRIE_FIELD_LIST_NEXT
                     : 0);

    res->singleton = singleton;
    res->next = next;

    if (next != NULL) {
        res->size = next->size + 1;
    } else {
        res->size = 1;
    }

    return res;
}

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#        define CTRIE_CTrieProposalNode_MAKE CTRIE_LOCAL_FUNC(proposal_make)
static CTrieProposalNode *CTRIE_CTrieProposalNode_MAKE(
        CTrieTransaction *ts,
        ErtsAlcType_t alloc_type,
        size_t size,
        CTrieIndirectionNode *current,
        CTrieIndirectionNode *proposed,
        erts_aint_t expected) {
    CTrieProposalNode *res = (CTrieProposalNode *)ctrie_alloc_root(
            ts,
            alloc_type,
            size,
            CTRIE_ROOT_NODE_TYPE_PROPOSAL);
    ASSERT(size >= sizeof(CTrieProposalNode));

    res->base.transaction.shared = ERTS_CTRIE_FIELD_PROPOSAL_CURRENT;
    res->base.transaction.unique = ERTS_CTRIE_FIELD_PROPOSAL_PROPOSED;

    res->proposed = proposed;
    res->current = current;
    res->expected = expected;
    res->committed = false;

    return res;
}
#    endif

#    define CTRIE_CTrieTombNode_MAKE CTRIE_LOCAL_FUNC(tomb_make)
static CTrieTombNode *CTRIE_CTrieTombNode_MAKE(CTrieTransaction *ts,
                                               ErtsAlcType_t alloc_type,
                                               size_t size,
                                               CTrieSingletonNode *singleton) {
    CTrieTombNode *res = (CTrieTombNode *)
            ctrie_alloc_gcas(ts, alloc_type, size, CTRIE_GCAS_NODE_TYPE_TOMB);
    ASSERT(size >= sizeof(CTrieTombNode));

    res->base.base.transaction.shared = ERTS_CTRIE_FIELD_TOMB_SINGLETON;
    res->base.base.transaction.unique = 0;

    res->singleton = singleton;

    return res;
}

/* *************************************************************************
 */

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS

#        define ctrie_root_commit CTRIE_LOCAL_FUNC(root_commit)
static CTrieIndirectionNode *ctrie_root_commit(CTrie *trie, bool abortable);

#        define ctrie_gcas_commit CTRIE_LOCAL_FUNC(gcas_commit)
static erts_aint_t ctrie_gcas_commit(CTrie *trie,
                                     CTrieIndirectionNode *indirection,
                                     erts_aint_t val);
#    endif

#    define ctrie_indirection_cas CTRIE_LOCAL_FUNC(indirection_cas)
static bool ctrie_indirection_cas(CTrie *trie,
                                  CTrieTransaction *ts,
                                  CTrieIndirectionNode *indirection,
                                  erts_aint_t val,
                                  erts_aint_t old) {
#    if defined(ERTS_CTRIE_WANT_SNAPSHOTS)
    CTrieGCASNodeBase *main = CTRIE_MAIN_NODE_PTR(val, base);

    ctrie_atomic_set_dirty(&main->previous, old);

    /* GCAS only works as long as *one* thread attempts to swap in a given
     * node; if two or more threads try to insert `val`, both of them might
     * think the GCAS succeeded and double-release `old`. Not to mention how
     * things could go sideways if they operate on different indirection nodes
     * (e.g. when racing during tomb cleanup).
     *
     * It's therefore imperative that the `val` argument is always a freshly
     * created node. */
    ASSERT(ctrie_atomic_inc_read_nob(&main->debug) == 1);

    if (ctrie_atomic_cmpxchg_wb(&indirection->node, val, old) == old) {
        /* Since other threads can finish the commit on our behalf, we
         * explicitly ignore the return value -- the latest value of the
         * indirection node -- as it leaves us no way to tell whether `val`
         * was successfully published.
         *
         * The only way to do this is to check whether `main->previous` is
         * NULL after attempting to commit. */
        (void)ctrie_gcas_commit(trie, indirection, val);

        ERTS_THR_READ_MEMORY_BARRIER;

        /* Since other threads could have read `val`, we have to commit it
         * regardless of whether GCAS succeeded or not, costing us a lot of
         * reference ping-pong in the rare case of failure below.
         *
         * The good news is that we're free to do it after our own attempt to
         * commit, shrinking the window for races. */
        ctrie_transaction_commit(ts);

        if (ctrie_atomic_read_nob(&main->previous) == ERTS_AINT_NULL) {
            ctrie_node_gcas_release(old);
            return true;
        }

        ctrie_node_gcas_release(val);
        return false;
    }
#    else
    if (ctrie_atomic_cmpxchg_wb(&indirection->node, val, old) == old) {
        ctrie_transaction_commit(ts);
        ctrie_node_gcas_release(old);
        return true;
    }
#    endif

    ctrie_node_gcas_abort(val);
    return false;
}

#    define ctrie_indirection_read CTRIE_LOCAL_FUNC(indirection_read)
static erts_aint_t ctrie_indirection_read(CTrie *trie,
                                          CTrieIndirectionNode *indirection) {
    erts_aint_t raw = ctrie_atomic_read_ddrb(&indirection->node);

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
    raw = ctrie_gcas_commit(trie, indirection, raw);
#    endif

    return raw;
}

/** @brief As \c ctrie_indirection_read but atomically references the result,
 * restarting if we're racing with a node about to be cleaned up. */
#    define ctrie_indirection_reference CTRIE_LOCAL_FUNC(indirection_reference)
static erts_aint_t ctrie_indirection_reference(
        CTrie *trie,
        CTrieIndirectionNode *indirection,
        int count) {
    CTrieGCASNodeBase *main;
    erts_aint_t raw;

    do {
        raw = ctrie_atomic_read_ddrb(&indirection->node);

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
        raw = ctrie_gcas_commit(trie, indirection, raw);
#    endif

        main = CTRIE_MAIN_NODE_PTR(raw, base);
    } while (erts_refc_add_unless(&main->base.refc, count, 0, 0) == 0);

    return raw;
}

#    define ctrie_root_read CTRIE_LOCAL_FUNC(root_read)
static CTrieIndirectionNode *ctrie_root_read(CTrie *trie, bool abortable) {
    erts_aint_t raw = ctrie_atomic_read_ddrb(&trie->root);

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
    switch (CTRIE_ROOT_NODE_TYPE(raw)) {
    case CTRIE_ROOT_NODE_TYPE_INDIRECTION:
        return CTRIE_ROOT_NODE_PTR(raw, indirection);
    case CTRIE_ROOT_NODE_TYPE_PROPOSAL:
        return ctrie_root_commit(trie, abortable);
    }

    ERTS_UNREACHABLE("Invalid root node type");
#    else
    return CTRIE_ROOT_NODE_PTR(raw, indirection);
#    endif
}

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
static CTrieIndirectionNode *ctrie_root_commit(CTrie *trie, bool abortable) {
    for (;;) {
        erts_aint_t raw = ctrie_atomic_read_ddrb(&trie->root);

        switch (CTRIE_ROOT_NODE_TYPE(raw)) {
        case CTRIE_ROOT_NODE_TYPE_INDIRECTION:
            return CTRIE_ROOT_NODE_PTR(raw, indirection);
        case CTRIE_ROOT_NODE_TYPE_PROPOSAL: {
            CTrieProposalNode *proposal = CTRIE_ROOT_NODE_PTR(raw, proposal);

            if (!abortable) {
                if (proposal->expected ==
                    ctrie_indirection_read(trie, proposal->current)) {
                    if (ctrie_atomic_cmpxchg_wb(
                                &trie->root,
                                CTRIE_ROOT_NODE_AS(proposal->proposed,
                                                   INDIRECTION),
                                raw) == raw) {
                        proposal->committed = true;
                        ERTS_THR_WRITE_MEMORY_BARRIER;

                        ctrie_node_root_release(
                                CTRIE_ROOT_NODE_AS(proposal->current,
                                                   INDIRECTION));
                        ctrie_node_root_release(raw);

                        return proposal->proposed;
                    }

                    continue;
                }
            }

            if (ctrie_atomic_cmpxchg_wb(
                        &trie->root,
                        CTRIE_ROOT_NODE_AS(proposal->current, INDIRECTION),
                        raw) == raw) {
                ctrie_node_root_release(raw);
                return proposal->current;
            }

            continue;
        }
        }

        ERTS_UNREACHABLE("Invalid root node type");
    }
}

static erts_aint_t ctrie_gcas_commit(CTrie *trie,
                                     CTrieIndirectionNode *indirection,
                                     erts_aint_t val) {
    CTrieGCASNodeBase *main;
    erts_aint_t raw;

    for (;;) {
        main = CTRIE_GCAS_NODE_PTR(val, base);
        raw = ctrie_atomic_read_ddrb(&main->previous);

        if (raw == ERTS_AINT_NULL) {
            return val;
        }

        if (CTRIE_GCAS_NODE_TYPE(raw) != CTRIE_GCAS_NODE_TYPE_FAILED) {
            CTrieIndirectionNode *root = ctrie_root_read(trie, true);

            if (root->generation == indirection->generation &&
                !trie->read_only) {
                if (ctrie_atomic_cmpxchg_wb(&main->previous,
                                            ERTS_AINT_NULL,
                                            raw) == raw) {
                    return val;
                }

                /* We've raced with another thread that committed for us, which
                 * should already have set `main->previous` to NULL (or more
                 * rarely, a `CTrieFailedNode` if a snapshot happened after
                 * `ctrie_root_read` above).
                 *
                 * Try again without reading a new `val` from the
                 * indirection. */
                continue;
            } else {
                CTrieFailedNode *marker;
                CTrieTransaction ts;

                ctrie_transaction_init(&ts);
                marker = ctrie_node_make(CTrieFailedNode, &ts, raw);

                if (ctrie_atomic_cmpxchg_wb(&main->previous,
                                            CTRIE_GCAS_NODE_AS(marker, FAILED),
                                            raw) != raw) {
                    /* NOTE: no point in going the long way around through
                     * transaction abort. */
                    ctrie_failed_destroy(marker);
                }
            }
        } else {
            CTrieFailedNode *failed = CTRIE_GCAS_NODE_PTR(raw, failed);
            erts_aint_t old = erts_atomic_read_nob(&failed->base.previous);

            /* A previous swap failed. Try to commit the old value -- note that
             * `old` and `val` are reversed here. */
            if (ctrie_atomic_cmpxchg_wb(&indirection->node, old, val) == val) {
                ctrie_node_gcas_release(val);
                return old;
            }
        }

        val = ctrie_atomic_read_ddrb(&indirection->node);
    }
}
#    endif /* ERTS_CTRIE_WANT_SNAPSHOTS */

#    define ctrie_root_update CTRIE_LOCAL_FUNC(root_update)
static bool ctrie_root_update(CTrie *trie,
                              CTrieTransaction *ts,
                              CTrieIndirectionNode *root,
                              CTrieIndirectionNode *val,
                              erts_aint_t main) {
    const erts_aint_t expected = CTRIE_ROOT_NODE_AS(root, INDIRECTION);

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
    CTrieProposalNode *proposal;

    proposal = ctrie_node_make(CTrieProposalNode, ts, val, root, main);

    if (ctrie_atomic_cmpxchg_wb(&trie->root,
                                CTRIE_ROOT_NODE_AS(proposal, PROPOSAL),
                                expected) == expected) {
        /* We have to commit the proposal node immediately regardless of how
         * the rest goes, as we have published it for other nodes to see. */
        ctrie_transaction_commit(ts);
        ctrie_root_commit(trie, false);

        ERTS_THR_READ_MEMORY_BARRIER;
        return proposal->committed;
    } else {
        ctrie_node_root_abort(CTRIE_ROOT_NODE_AS(proposal, PROPOSAL));
    }
#    else
    if (ctrie_atomic_cmpxchg_wb(&trie->root,
                                CTRIE_ROOT_NODE_AS(val, INDIRECTION),
                                expected) == expected) {
        ctrie_transaction_commit(ts);
        ctrie_node_root_release(expected);
        return true;
    } else {
        ctrie_node_root_abort(CTRIE_ROOT_NODE_AS(val, INDIRECTION));
    }
#    endif

    return false;
}

#    define ctrie_branch_transfer CTRIE_LOCAL_FUNC(branch_transfer)
static void ctrie_branch_transfer(CTrieBranchNode *from, CTrieBranchNode *to) {
    /* Attempt to transfer ownership of the twigs from the old version to the
     * new without needless reference bumping. We rely on the following
     * invariants:
     *
     * - Twigs not affected by an operation are left unchanged.
     * - Merge operations and singleton updates mark a twig as `unique`
     * - Branch renewals mark affected twigs as already referenced (neither
     *   `unique` nor `shared`)
     *
     * Hence, we can say that any twig in the `shared` set of `to` must be
     * present in `from`, and we can safely move ownership of all `shared`
     * twigs as long as `from` is not referenced by a snapshot or the like. */
    if (erts_refc_dectest(&from->base.base.refc, 0) == 0) {
        from->base.base.cleanup.moved = to->base.base.transaction.shared;
        to->base.base.transaction.shared = 0;

#    ifdef ERTS_CTRIE_WANT_SINGLE_THREADED
        ctrie_branch_destroy(from);
#    else
        erts_schedule_thr_prgr_later_op((void (*)(void *))ctrie_branch_destroy,
                                        (void *)from,
                                        &from->base.base.cleanup.later_op);
#    endif
    }
}

/** @brief Ownership-transferring version of \c ctrie_indirection_cas , see
 * that function for more details. */
#    define ctrie_branch_cas CTRIE_LOCAL_FUNC(branch_cas)
static bool ctrie_branch_cas(CTrie *trie,
                             CTrieTransaction *ts,
                             CTrieIndirectionNode *indirection,
                             CTrieBranchNode *val,
                             CTrieBranchNode *old) {
    erts_aint_t tagged_val = CTRIE_MAIN_NODE_AS(val, BRANCH);
    erts_aint_t tagged_old = CTRIE_MAIN_NODE_AS(old, BRANCH);

#    if defined(ERTS_CTRIE_WANT_SNAPSHOTS)
    ctrie_atomic_set_dirty(&val->base.previous, tagged_old);

    ASSERT(ctrie_atomic_inc_read_nob(&val->base.debug) == 1);

    if (ctrie_atomic_cmpxchg_wb(&indirection->node, tagged_val, tagged_old) ==
        tagged_old) {
        bool succeeded = false;

        (void)ctrie_gcas_commit(trie, indirection, tagged_val);

        ERTS_THR_READ_MEMORY_BARRIER;

        if (ctrie_atomic_read_nob(&val->base.previous) == ERTS_AINT_NULL) {
            ctrie_branch_transfer(old, val);
            succeeded = true;
        }

        /* Since we published `val` for other threads to see, we must commit
         * the transaction regardless of whether we succeeded or failed. The
         * good news is that at this point we can only fail because of a
         * snapshot.
         *
         * In case of the former, it must be done after transferring ownership
         * for that to take effect, and in case of the latter it must be done
         * before releasing (i.e. aborting) the proposed value. */
        ctrie_transaction_commit(ts);

        if (!succeeded) {
            ctrie_node_gcas_release(tagged_val);
        }

        return succeeded;
    }
#    else
    if (ctrie_atomic_cmpxchg_wb(&indirection->node, tagged_val, tagged_old) ==
        tagged_old) {
        ctrie_branch_transfer(old, val);
        ctrie_transaction_commit(ts);
        return true;
    }
#    endif

    ctrie_node_gcas_abort(tagged_val);
    return false;
}

/* ************************************************************************* */

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#        define ctrie_branch_renew CTRIE_LOCAL_FUNC(branch_renew)
static bool ctrie_branch_renew(CTrie *trie,
                               CTrieBranchNode *branch,
                               CTrieIndirectionNode *parent,
                               erts_aint64_t generation) {
    CTrieBranchNode *updated;
    CTrieBitmap remaining;
    CTrieTransaction ts;

    updated = ctrie_branch_make(&ts, branch->bitmap, branch->bitmap, 0);
    remaining = branch->bitmap;

    ctrie_transaction_init(&ts);

    for (int index = 0; remaining > 0; index++) {
        const CTrieBitmap next = remaining & (remaining - 1);
        const CTrieBitmap flag = remaining ^ next;
        erts_aint_t twig = branch->twigs[index];

        if (CTRIE_TWIG_NODE_TYPE(twig) == CTRIE_TWIG_NODE_TYPE_INDIRECTION) {
            CTrieIndirectionNode *indirection, *renewed;
            erts_aint_t main;

            indirection = CTRIE_TWIG_NODE_PTR(twig, indirection);
            ASSERT(indirection->generation < generation);

            main = ctrie_indirection_reference(trie, indirection, 1);
            renewed = ctrie_node_make(CTrieIndirectionNode,
                                      &ts,
                                      generation,
                                      main,
                                      ERTS_CTRIE_OWNERSHIP_ALREADY_REFERENCED);

            updated->base.base.transaction.shared &= ~flag;
            updated->base.base.transaction.unique |= flag;

            twig = CTRIE_TWIG_NODE_AS(renewed, INDIRECTION);
        }

        updated->twigs[index] = twig;
        remaining = next;
    }

    return ctrie_branch_cas(trie, &ts, parent, updated, branch);
}
#    endif

#    define ctrie_branch_add_singleton CTRIE_LOCAL_FUNC(branch_add_singleton)
static bool ctrie_branch_add_singleton(CTrie *trie,
                                       CTrieBranchNode *branch,
                                       CTrieIndirectionNode *parent,
                                       CTrieSingletonNode *singleton,
                                       CTrieBitmap flag) {
    CTrieBranchNode *updated;
    CTrieTransaction ts;
    CTrieBitmap bitmap;
    int index;

    ctrie_transaction_init(&ts);

    bitmap = branch->bitmap;
    index = 0;

    ASSERT((bitmap & flag) == 0);

    /* Note that the branch takes unique ownership of the singleton, despite it
     * needing to survive restarts. This is done to simplify ownership transfer
     * on success, and the abort handler for unique singletons is a no-op to
     * support this.
     *
     * Actual ownership is only moved from caller to `trie` when the operation
     * succeeds. */
    updated = ctrie_branch_make(&ts, bitmap | flag, bitmap, flag);

    for (CTrieBitmap iterator = bitmap & (flag - 1); iterator > 0;
         iterator &= (iterator - 1), index++) {
        ASSERT(index < (1 << ERTS_CTRIE_BRANCH_FACTOR));
        updated->twigs[index] = branch->twigs[index];
    }

    updated->twigs[index] = CTRIE_TWIG_NODE_AS(singleton, SINGLETON);

    for (CTrieBitmap iterator = bitmap & ~(flag - 1); iterator > 0;
         iterator &= (iterator - 1), index++) {
        ASSERT(index < ((1 << ERTS_CTRIE_BRANCH_FACTOR) - 1));
        updated->twigs[index + 1] = branch->twigs[index];
    }

    return ctrie_branch_cas(trie, &ts, parent, updated, branch);
}

#    define ctrie_branch_remove_singleton                                      \
        CTRIE_LOCAL_FUNC(branch_remove_singleton)
static bool ctrie_branch_remove_singleton(CTrie *trie,
                                          CTrieBranchNode *branch,
                                          CTrieIndirectionNode *parent,
                                          CTrieBitmap flag,
                                          int level) {
    CTrieBranchNode *updated;
    CTrieTransaction ts;
    CTrieBitmap bitmap;
    int index;

    ctrie_transaction_init(&ts);

    bitmap = branch->bitmap & ~flag;
    index = 0;

    /* It's only legal to remove the last twig if we're at the root -- this
     * routine can only remove singletons, not indirections, and previous
     * singleton removals should have resulted in tomb nodes. */
    ASSERT((level == 0) || (bitmap != 0));

    /* If removal leaves a lonely twig and we're below the root, we need to
     * entomb it so it will be hoisted to its highest possible level. */
    if ((bitmap & (bitmap - 1)) == 0 && level > 0) {
        erts_aint_t twig = branch->twigs[flag < bitmap];

        /* The removed twig MUST be a singleton. */
        ASSERT(CTRIE_TWIG_NODE_TYPE(branch->twigs[flag > bitmap]) ==
               CTRIE_TWIG_NODE_TYPE_SINGLETON);

        if (CTRIE_TWIG_NODE_TYPE(twig) == CTRIE_TWIG_NODE_TYPE_SINGLETON) {
            CTrieTombNode *tomb =
                    ctrie_node_make(CTrieTombNode,
                                    &ts,
                                    CTRIE_TWIG_NODE_PTR(twig, singleton));
            return ctrie_indirection_cas(trie,
                                         &ts,
                                         parent,
                                         CTRIE_MAIN_NODE_AS(tomb, TOMB),
                                         CTRIE_MAIN_NODE_AS(branch, BRANCH));
        }
    }

    updated = ctrie_branch_make(&ts, bitmap, bitmap, 0);

    for (CTrieBitmap iterator = bitmap & (flag - 1); iterator > 0;
         iterator &= iterator - 1, index++) {
        ASSERT(index < (1 << ERTS_CTRIE_BRANCH_FACTOR));
        updated->twigs[index] = branch->twigs[index];
    }

    /* The removed twig MUST be a singleton. */
    ASSERT(CTRIE_TWIG_NODE_TYPE(branch->twigs[index]) ==
           CTRIE_TWIG_NODE_TYPE_SINGLETON);

    for (CTrieBitmap iterator = bitmap & ~(flag - 1); iterator > 0;
         iterator &= iterator - 1, index++) {
        ASSERT(index < ((1 << ERTS_CTRIE_BRANCH_FACTOR) - 1));
        updated->twigs[index] = branch->twigs[index + 1];
    }

    return ctrie_branch_cas(trie, &ts, parent, updated, branch);
}

#    define ctrie_branch_update_twig CTRIE_LOCAL_FUNC(branch_update_twig)
static bool ctrie_branch_update_twig(CTrie *trie,
                                     CTrieBranchNode *branch,
                                     CTrieTransaction *ts,
                                     CTrieIndirectionNode *parent,
                                     erts_aint_t replacement,
                                     enum erts_ctrie_ownership ownership,
                                     const CTrieBitmap flag) {
    CTrieBitmap shared, unique;
    CTrieBranchNode *updated;
    CTrieBitmap bitmap;
    int index;

    bitmap = branch->bitmap;
    ASSERT(bitmap & flag);
    index = 0;

    switch (ownership) {
    case ERTS_CTRIE_OWNERSHIP_SHARED:
        shared = bitmap;
        unique = 0;
        break;
    case ERTS_CTRIE_OWNERSHIP_UNIQUE:
        shared = bitmap & ~flag;
        unique = flag;
        break;
    case ERTS_CTRIE_OWNERSHIP_ALREADY_REFERENCED:
        shared = bitmap & ~flag;
        unique = 0;
        break;
    }

    updated = ctrie_branch_make(ts, bitmap, shared, unique);

    for (CTrieBitmap iterator = bitmap & (flag - 1); iterator > 0;
         iterator &= iterator - 1, index++) {
        ASSERT(index < ((1 << ERTS_CTRIE_BRANCH_FACTOR) - 1));
        updated->twigs[index] = branch->twigs[index];
    }

    ASSERT(index < (1 << ERTS_CTRIE_BRANCH_FACTOR));
    updated->twigs[index] = replacement;
    index++;

    for (CTrieBitmap iterator = bitmap & ~(flag | (flag - 1)); iterator > 0;
         iterator &= iterator - 1, index++) {
        ASSERT(index < (1 << ERTS_CTRIE_BRANCH_FACTOR));
        updated->twigs[index] = branch->twigs[index];
    }

    return ctrie_branch_cas(trie, ts, parent, updated, branch);
}

#    define ctrie_singleton_merge CTRIE_LOCAL_FUNC(singleton_merge)
static CTrieIndirectionNode *ctrie_singleton_merge(
        CTrieTransaction *ts,
        int level,
        CTRIE_GENERATION_PARAMETER CTrieSingletonNode *val,
        CTrieSingletonNode *old) {
    erts_aint_t resolution;

    ASSERT((level % ERTS_CTRIE_BRANCH_FACTOR) == 0 &&
           (level <= CTRIE_MAX_LEVEL + ERTS_CTRIE_BRANCH_FACTOR));

    if (level < CTRIE_MAX_LEVEL) {
        const int val_index =
                (ERTS_CTRIE_HASH_GET(val) >> level) & CTRIE_HASH_MASK;
        const int old_index =
                (ERTS_CTRIE_HASH_GET(old) >> level) & CTRIE_HASH_MASK;
        const int val_flag = ((CTrieBitmap)1) << val_index;
        const int old_flag = ((CTrieBitmap)1) << old_index;

        if (val_flag == old_flag) {
            CTrieIndirectionNode *indirection;
            CTrieBranchNode *branch;

            indirection =
                    ctrie_singleton_merge(ts,
                                          level + ERTS_CTRIE_BRANCH_FACTOR,
                                          CTRIE_GENERATION_ARGUMENT val,
                                          old);

            branch = ctrie_branch_make(ts, val_flag, 0, val_flag);
            branch->twigs[0] = CTRIE_TWIG_NODE_AS(indirection, INDIRECTION);

            resolution = CTRIE_MAIN_NODE_AS(branch, BRANCH);
        } else {
            CTrieBranchNode *branch = ctrie_branch_make(ts,
                                                        val_flag | old_flag,
                                                        old_flag,
                                                        val_flag);

            if (val_index < old_index) {
                branch->twigs[0] = CTRIE_TWIG_NODE_AS(val, SINGLETON);
                branch->twigs[1] = CTRIE_TWIG_NODE_AS(old, SINGLETON);
            } else {
                branch->twigs[0] = CTRIE_TWIG_NODE_AS(old, SINGLETON);
                branch->twigs[1] = CTRIE_TWIG_NODE_AS(val, SINGLETON);
            }

            resolution = CTRIE_MAIN_NODE_AS(branch, BRANCH);
        }
    } else {
        resolution = CTRIE_MAIN_NODE_AS(
                ctrie_node_make(CTrieListNode,
                                ts,
                                val,
                                ERTS_CTRIE_OWNERSHIP_UNIQUE,
                                ctrie_node_make(CTrieListNode,
                                                ts,
                                                old,
                                                ERTS_CTRIE_OWNERSHIP_SHARED,
                                                NULL,
                                                ERTS_CTRIE_OWNERSHIP_UNIQUE),
                                ERTS_CTRIE_OWNERSHIP_UNIQUE),
                LIST);
    }

    return ctrie_node_make(CTrieIndirectionNode,
                           ts,
                           CTRIE_GENERATION_ARGUMENT resolution,
                           ERTS_CTRIE_OWNERSHIP_UNIQUE);
}

/* *************************************************************************
 */

#    define ctrie_clean_grandparent CTRIE_LOCAL_FUNC(clean_grandparent)
static void ctrie_clean_grandparent(
        CTrie *trie,
        CTrieIndirectionNode *const grandparent,
        CTrieIndirectionNode *const parent,
        erts_aint_t parent_node,
        CTRIE_GENERATION_PARAMETER const erts_ihash_t hash,
        const int level) {
    erts_aint_t grandparent_node;

    ASSERT(CTRIE_MAIN_NODE_TYPE(parent_node) == CTRIE_MAIN_NODE_TYPE_TOMB);

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
retry:
#    endif

    grandparent_node = ctrie_indirection_read(trie, grandparent);

    if (CTRIE_MAIN_NODE_TYPE(grandparent_node) == CTRIE_MAIN_NODE_TYPE_BRANCH) {
        const CTrieBitmap flag = ((CTrieBitmap)1)
                                 << ((hash >> level) & CTRIE_HASH_MASK);
        CTrieBranchNode *branch;
        int index;

        branch = CTRIE_MAIN_NODE_PTR(grandparent_node, branch);
        if (!(branch->bitmap & flag)) {
            return;
        }

        index = hashmap_bitcount(branch->bitmap & (flag - 1));

        if (CTRIE_BASE_NODE_PTR(branch->twigs[index]) == &parent->base) {
            const CTrieTombNode *tomb = CTRIE_MAIN_NODE_PTR(parent_node, tomb);
            CTrieTransaction ts;

            ctrie_transaction_init(&ts);

            if (!(branch->bitmap & ~flag) && level > 0) {
                /* Our grandparent is below the root and our parent has a
                 * single tomb entry, hoist the tomb entry to the
                 * grandparent. */
                if (ctrie_indirection_cas(
                            trie,
                            &ts,
                            grandparent,
                            CTRIE_MAIN_NODE_AS(ctrie_node_make(CTrieTombNode,
                                                               &ts,
                                                               tomb->singleton),
                                               TOMB),
                            grandparent_node)) {
                    return;
                }
            } else {
                /* We're either at the root with a single tomb entry, or below
                 * the root with an arbitrary number of entries: resurrect this
                 * particular tomb entry, leaving others untouched.
                 *
                 * The tomb entry is owned by the indirection and not the
                 * original branch, so we must explicitly reference its
                 * contained singleton to avoid messing up the twig ownership
                 * transfer scheme. */
                ctrie_node_reference(&(tomb->singleton)->base);
                if (ctrie_branch_update_twig(
                            trie,
                            branch,
                            &ts,
                            grandparent,
                            CTRIE_TWIG_NODE_AS(tomb->singleton, SINGLETON),
                            ERTS_CTRIE_OWNERSHIP_ALREADY_REFERENCED,
                            flag)) {
                    return;
                }
            }

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
            {
                CTrieIndirectionNode *root = ctrie_root_read(trie, false);
                if (root->generation == generation) {
                    goto retry;
                }
            }
#    endif
        }
    }
}

#    define ctrie_clean CTRIE_LOCAL_FUNC(clean)
static void ctrie_clean(CTrie *trie, CTrieIndirectionNode *parent, int level) {
    CTrieBranchNode *branch;
    CTrieTransaction ts;
    erts_aint_t node;

    node = ctrie_indirection_read(trie, parent);

    if (CTRIE_MAIN_NODE_TYPE(node) != CTRIE_MAIN_NODE_TYPE_BRANCH) {
        return;
    }

    branch = CTRIE_MAIN_NODE_PTR(node, branch);

    ctrie_transaction_init(&ts);

    /* If our parent is below the root, and the branch has a lonely twig
     * pointing at a tomb entry, the tomb entry must be hoisted to the
     * parent. */
    if ((branch->bitmap & (branch->bitmap - 1)) == 0 && level > 0) {
        CTrieIndirectionNode *indirection;
        erts_aint_t twig;
        erts_aint_t raw;

        /* Lonely twigs below the root must be indirections. */
        twig = branch->twigs[0];
        ASSERT(CTRIE_TWIG_NODE_TYPE(twig) == CTRIE_TWIG_NODE_TYPE_INDIRECTION);
        indirection = CTRIE_TWIG_NODE_PTR(twig, indirection);
        raw = ctrie_indirection_read(trie, indirection);

        if (CTRIE_MAIN_NODE_TYPE(raw) == CTRIE_MAIN_NODE_TYPE_TOMB) {
            CTrieTombNode *tomb = CTRIE_MAIN_NODE_PTR(raw, tomb);

            /* While tempting to swap `branch` with `tomb`, GCAS always
             * requires a fresh node.
             *
             * Note that we don't care about success; the operation has to be
             * restarted anyway, and we will land back here on failure. */
            (void)ctrie_indirection_cas(
                    trie,
                    &ts,
                    parent,
                    CTRIE_MAIN_NODE_AS(ctrie_node_make(CTrieTombNode,
                                                       &ts,
                                                       tomb->singleton),
                                       TOMB),
                    node);
        }
    } else {
        /* Resurrect all tombed entries in the branch: there should be at least
         * one since we landed here in the first place, unless someone has
         * already cleaned it up in the meantime. */
        CTrieBranchNode *updated =
                ctrie_branch_make(&ts, branch->bitmap, branch->bitmap, 0);

        for (CTrieBitmap iterator = branch->bitmap, index = 0; iterator > 0;
             index++) {
            const CTrieBitmap next = iterator & (iterator - 1);
            const CTrieBitmap flag = iterator ^ next;
            erts_aint_t twig = branch->twigs[index];

            if (CTRIE_TWIG_NODE_TYPE(twig) ==
                CTRIE_TWIG_NODE_TYPE_INDIRECTION) {
                CTrieIndirectionNode *indirection;
                erts_aint_t raw;

                indirection = CTRIE_TWIG_NODE_PTR(twig, indirection);
                raw = ctrie_indirection_read(trie, indirection);

                if (CTRIE_MAIN_NODE_TYPE(raw) == CTRIE_MAIN_NODE_TYPE_TOMB) {
                    CTrieTombNode *tomb = CTRIE_MAIN_NODE_PTR(raw, tomb);

                    /* The tomb entry is owned by the indirection and not the
                     * original branch, so we must explicitly reference its
                     * contained singleton to avoid messing up the twig
                     * ownership transfer scheme. */
                    ctrie_node_reference(&(tomb->singleton)->base);
                    updated->base.base.transaction.shared &= ~flag;
                    updated->base.base.transaction.unique &= ~flag;

                    twig = CTRIE_TWIG_NODE_AS(tomb->singleton, SINGLETON);
                }
            }

            updated->twigs[index] = twig;
            iterator = next;
        }

        (void)ctrie_branch_cas(trie, &ts, parent, updated, branch);
    }
}

/* ************************************************************************* */

#    define CTRIE_TRAVERSE(name, ...) CTRIE_LOCAL_FUNC(name)(__VA_ARGS__)

#    define CTRIE_TRAVERSE_BRANCH(name, ...)                                   \
        CTRIE_LOCAL_FUNC(name##_branch)(__VA_ARGS__)

#    define CTRIE_TRAVERSE_LIST(name, ...)                                     \
        CTRIE_LOCAL_FUNC(name##_list)(__VA_ARGS__)

#    define CTRIE_TRAVERSE_BRANCH_DECL(name)                                   \
        static enum erts_ctrie_result CTRIE_LOCAL_FUNC(name##_branch)(         \
                CTrie * trie,                                                  \
                CTrieBranchNode * branch,                                      \
                CTrieIndirectionNode * parent,                                 \
                CTrieIndirectionNode * grandparent,                            \
                int level,                                                     \
                CTRIE_GENERATION_PARAMETER CTRIE_TRAVERSE_BRANCH_PARAMETERS)

#    define CTRIE_TRAVERSE_BRANCH_DEF(name)                                    \
        CTRIE_TRAVERSE_BRANCH_DECL(name) {                                     \
            int index =                                                        \
                    (CTRIE_TRAVERSE_BRANCH_HASH >> level) & CTRIE_HASH_MASK;   \
            const CTrieBitmap flag = ((CTrieBitmap)1) << index;                \
            erts_aint_t twig;                                                  \
            if (branch->bitmap != ~((CTrieBitmap)0)) {                         \
                index = hashmap_bitcount(branch->bitmap & (flag - 1));         \
                if (!(flag & branch->bitmap)) {                                \
                    CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY                       \
                }                                                              \
            }                                                                  \
            twig = branch->twigs[index];                                       \
            switch (CTRIE_TWIG_NODE_TYPE(twig)) {                              \
            case CTRIE_TWIG_NODE_TYPE_INDIRECTION: {                           \
                CTrieIndirectionNode *indirection =                            \
                        CTRIE_TWIG_NODE_PTR(twig, indirection);                \
                { CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY }                     \
            }                                                                  \
            case CTRIE_TWIG_NODE_TYPE_SINGLETON: {                             \
                CTrieSingletonNode *candidate =                                \
                        CTRIE_TWIG_NODE_PTR(twig, singleton);                  \
                { CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY }                       \
            }                                                                  \
            }                                                                  \
            ERTS_UNREACHABLE("Invalid twig node type");                        \
        }

#    define CTRIE_TRAVERSE_LIST_DECL(name)                                     \
        static enum erts_ctrie_result CTRIE_LOCAL_FUNC(name##_list)(           \
                CTrie * trie,                                                  \
                CTrieListNode * list,                                          \
                CTRIE_TRAVERSE_LIST_PARAMETERS)

#    define CTRIE_TRAVERSE_LIST_DEF(name)                                      \
        CTRIE_TRAVERSE_LIST_DECL(name) {                                       \
            CTRIE_TRAVERSE_LIST_SEARCH                                         \
            CTRIE_TRAVERSE_LIST_BODY                                           \
        }

#    define CTRIE_TRAVERSE_DECL(name)                                          \
        static enum erts_ctrie_result CTRIE_LOCAL_FUNC(name)(                  \
                CTrie * trie,                                                  \
                CTrieIndirectionNode * indirection,                            \
                CTrieIndirectionNode * parent,                                 \
                int level,                                                     \
                CTRIE_GENERATION_PARAMETER CTRIE_TRAVERSE_PARAMETERS)

#    define CTRIE_TRAVERSE_DEF(name)                                           \
        CTRIE_TRAVERSE_DECL(name) {                                            \
            const erts_aint_t node =                                           \
                    ctrie_indirection_read(trie, indirection);                 \
            switch (CTRIE_MAIN_NODE_TYPE(node)) {                              \
            case CTRIE_MAIN_NODE_TYPE_BRANCH:                                  \
                ASSERT(level <= CTRIE_MAX_LEVEL);                              \
                return CTRIE_TRAVERSE_BRANCH(                                  \
                        name,                                                  \
                        trie,                                                  \
                        CTRIE_MAIN_NODE_PTR(node, branch),                     \
                        indirection,                                           \
                        parent,                                                \
                        level,                                                 \
                        CTRIE_GENERATION_ARGUMENT                              \
                                CTRIE_TRAVERSE_ARGUMENTS_BRANCH);              \
            case CTRIE_MAIN_NODE_TYPE_LIST:                                    \
                ASSERT(level >= CTRIE_MAX_LEVEL);                              \
                return CTRIE_TRAVERSE_LIST(name,                               \
                                           trie,                               \
                                           CTRIE_MAIN_NODE_PTR(node, list),    \
                                           CTRIE_TRAVERSE_ARGUMENTS_LIST);     \
            case CTRIE_MAIN_NODE_TYPE_TOMB:                                    \
                ASSERT(level > 0);                                             \
                {CTRIE_TRAVERSE_TOMB_BODY};                                    \
                ctrie_clean(trie, parent, level - ERTS_CTRIE_BRANCH_FACTOR);   \
                return CTRIE_RESTART;                                          \
            }                                                                  \
            ERTS_UNREACHABLE("Invalid main node type");                        \
        }

#    undef CTRIE_TRAVERSE_BRANCH_PARAMETERS
#    undef CTRIE_TRAVERSE_BRANCH_HASH
#    undef CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY
#    undef CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY
#    undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY
#    undef CTRIE_TRAVERSE_LIST_PARAMETERS
#    undef CTRIE_TRAVERSE_LIST_SEARCH
#    undef CTRIE_TRAVERSE_LIST_BODY

#    undef CTRIE_TRAVERSE_PARAMETERS
#    undef CTRIE_TRAVERSE_ARGUMENTS_BRANCH
#    undef CTRIE_TRAVERSE_ARGUMENTS_LIST
#    undef CTRIE_TRAVERSE_TOMB_BODY

/* ************************************************************************* */

#    define CTRIE_TRAVERSE_LIST_SEARCH_HELPER_EQ(key, hash)                    \
        CTrieSingletonNode *candidate = NULL;                                  \
        CTrieListNode *iterator;                                               \
        for (iterator = list; iterator != NULL; iterator = iterator->next) {   \
            candidate = iterator->singleton;                                   \
            if (ERTS_CTRIE_HASH_EQ(hash, ERTS_CTRIE_HASH_GET(candidate)) &&    \
                ERTS_CTRIE_KEY_EQ(key, ERTS_CTRIE_KEY_GET(candidate))) {       \
                break;                                                         \
            }                                                                  \
        }

#    define CTRIE_TRAVERSE_LIST_SEARCH_HELPER_EXPECTED(expected)               \
        CTrieSingletonNode *candidate = NULL;                                  \
        CTrieListNode *iterator;                                               \
        for (iterator = list; iterator != NULL; iterator = iterator->next) {   \
            candidate = iterator->singleton;                                   \
            if (candidate == expected) {                                       \
                break;                                                         \
            }                                                                  \
        }

#    define CTRIE_TRAVERSE_LIST_REMOVAL_HELPER                                 \
        {                                                                      \
            CTrieListNode *found_at = iterator;                                \
            CTrieTransaction ts;                                               \
            (void)candidate;                                                   \
            if (found_at == NULL) {                                            \
                return CTRIE_NOT_FOUND;                                        \
            }                                                                  \
            ctrie_transaction_init(&ts);                                       \
            ASSERT(list->size >= 2);                                           \
            if (list->size == 2) {                                             \
                /* When there's only one entry left in the list, we need to */ \
                /* entomb it. */                                               \
                CTrieListNode *last;                                           \
                CTrieTombNode *tomb;                                           \
                last = (found_at->next != NULL) ? found_at->next : list;       \
                tomb = ctrie_node_make(CTrieTombNode, &ts, last->singleton);   \
                if (ctrie_indirection_cas(trie,                                \
                                          &ts,                                 \
                                          parent,                              \
                                          CTRIE_MAIN_NODE_AS(tomb, TOMB),      \
                                          CTRIE_MAIN_NODE_AS(list, LIST))) {   \
                    return CTRIE_OK;                                           \
                }                                                              \
            } else {                                                           \
                /* Otherwise, we need to create a new list without the */      \
                /* removed element. Copy all elements before the removed */    \
                /* one, and leave the tail intact modulo the requirement */    \
                /* of a fresh node for GCAS. */                                \
                CTrieListNode *updated = found_at->next;                       \
                if (updated != NULL) {                                         \
                    updated = ctrie_node_make(CTrieListNode,                   \
                                              &ts,                             \
                                              updated->singleton,              \
                                              ERTS_CTRIE_OWNERSHIP_SHARED,     \
                                              updated->next,                   \
                                              ERTS_CTRIE_OWNERSHIP_SHARED);    \
                }                                                              \
                for (iterator = list; iterator != found_at;                    \
                     iterator = iterator->next) {                              \
                    updated = ctrie_node_make(CTrieListNode,                   \
                                              &ts,                             \
                                              iterator->singleton,             \
                                              ERTS_CTRIE_OWNERSHIP_SHARED,     \
                                              updated,                         \
                                              ERTS_CTRIE_OWNERSHIP_UNIQUE);    \
                }                                                              \
                if (ctrie_indirection_cas(trie,                                \
                                          &ts,                                 \
                                          parent,                              \
                                          CTRIE_MAIN_NODE_AS(updated, LIST),   \
                                          CTRIE_MAIN_NODE_AS(list, LIST))) {   \
                    return CTRIE_OK;                                           \
                }                                                              \
            }                                                                  \
            return CTRIE_RESTART;                                              \
        }

#    define CTRIE_TRAVERSE_LIST_MODIFY_HELPER                                  \
        {                                                                      \
            CTrieListNode *insert_after = iterator, *updated;                  \
            CTrieTransaction ts;                                               \
            ctrie_transaction_init(&ts);                                       \
            (void)candidate;                                                   \
            /* If we found an entry, insert ourselves _after_ it, and copy */  \
            /* all elements _before_ it. If we failed to find an entry, */     \
            /* insert ourselves at the end and copy all preceding elements. */ \
            updated =                                                          \
                    ctrie_node_make(CTrieListNode,                             \
                                    &ts,                                       \
                                    replacement,                               \
                                    ERTS_CTRIE_OWNERSHIP_UNIQUE,               \
                                    insert_after ? insert_after->next : NULL,  \
                                    ERTS_CTRIE_OWNERSHIP_SHARED);              \
            for (iterator = list; iterator != insert_after;                    \
                 iterator = iterator->next) {                                  \
                updated = ctrie_node_make(CTrieListNode,                       \
                                          &ts,                                 \
                                          iterator->singleton,                 \
                                          ERTS_CTRIE_OWNERSHIP_SHARED,         \
                                          updated,                             \
                                          ERTS_CTRIE_OWNERSHIP_UNIQUE);        \
            }                                                                  \
            if (ctrie_indirection_cas(trie,                                    \
                                      &ts,                                     \
                                      parent,                                  \
                                      CTRIE_MAIN_NODE_AS(updated, LIST),       \
                                      CTRIE_MAIN_NODE_AS(list, LIST))) {       \
                return CTRIE_OK;                                               \
            }                                                                  \
            return CTRIE_RESTART;                                              \
        }

/* ************************************************************************* */

#    ifdef ERTS_CTRIE_WANT_LOOKUP

#        define CTRIE_TRAVERSE_PARAMETERS                                      \
            ERTS_CTRIE_KEY_TYPE key, ERTS_CTRIE_HASH_TYPE hash,                \
                    CTrieSingletonNode **out

#        define CTRIE_TRAVERSE_LIST_PARAMETERS                                 \
            ERTS_CTRIE_KEY_TYPE key, ERTS_CTRIE_HASH_TYPE hash,                \
                    CTrieSingletonNode **out

#        define CTRIE_TRAVERSE_BRANCH_PARAMETERS                               \
            ERTS_CTRIE_KEY_TYPE key, ERTS_CTRIE_HASH_TYPE hash,                \
                    CTrieSingletonNode **out

#        define ctrie_lookup_branch CTRIE_LOCAL_FUNC(lookup_branch)
CTRIE_TRAVERSE_BRANCH_DECL(lookup);
#        define ctrie_lookup_list CTRIE_LOCAL_FUNC(lookup_list)
CTRIE_TRAVERSE_LIST_DECL(lookup);
#        define ctrie_lookup CTRIE_LOCAL_FUNC(lookup)
CTRIE_TRAVERSE_DECL(lookup);

#        define CTRIE_TRAVERSE_LIST_SEARCH                                     \
            CTRIE_TRAVERSE_LIST_SEARCH_HELPER_EQ(key, hash)

#        define CTRIE_TRAVERSE_LIST_BODY                                       \
            {                                                                  \
                if (candidate == NULL) {                                       \
                    return CTRIE_NOT_FOUND;                                    \
                }                                                              \
                *out = candidate;                                              \
                return CTRIE_OK;                                               \
            }

#        define CTRIE_TRAVERSE_BRANCH_HASH hash

#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#            define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                     \
                if (indirection->generation == generation ||                   \
                    trie->read_only) {                                         \
                    return ctrie_lookup(trie,                                  \
                                        indirection,                           \
                                        parent,                                \
                                        level + ERTS_CTRIE_BRANCH_FACTOR,      \
                                        generation,                            \
                                        hash,                                  \
                                        key,                                   \
                                        out);                                  \
                } else {                                                       \
                    if (ctrie_branch_renew(trie,                               \
                                           branch,                             \
                                           parent,                             \
                                           generation)) {                      \
                        return ctrie_lookup(trie,                              \
                                            parent,                            \
                                            grandparent,                       \
                                            level,                             \
                                            generation,                        \
                                            key,                               \
                                            hash,                              \
                                            out);                              \
                    }                                                          \
                    return CTRIE_RESTART;                                      \
                }
#        else
#            define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                     \
                return ctrie_lookup(trie,                                      \
                                    indirection,                               \
                                    parent,                                    \
                                    level + ERTS_CTRIE_BRANCH_FACTOR,          \
                                    hash,                                      \
                                    key,                                       \
                                    out);
#        endif

#        define CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY return CTRIE_NOT_FOUND;
#        define CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY                           \
            if (ERTS_CTRIE_HASH_EQ(hash, ERTS_CTRIE_HASH_GET(candidate)) &&    \
                ERTS_CTRIE_KEY_EQ(key, ERTS_CTRIE_KEY_GET(candidate))) {       \
                *out = candidate;                                              \
                return CTRIE_OK;                                               \
            }                                                                  \
            return CTRIE_NOT_FOUND;

#        define CTRIE_TRAVERSE_ARGUMENTS_BRANCH key, hash, out
#        define CTRIE_TRAVERSE_ARGUMENTS_LIST key, hash, out

#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#            define CTRIE_TRAVERSE_TOMB_BODY                                   \
                if (trie->read_only) {                                         \
                    CTrieTombNode *tomb = CTRIE_MAIN_NODE_PTR(node, tomb);     \
                    CTrieSingletonNode *candidate = tomb->singleton;           \
                    if (ERTS_CTRIE_HASH_EQ(hash,                               \
                                           ERTS_CTRIE_HASH_GET(candidate)) &&  \
                        ERTS_CTRIE_KEY_EQ(key,                                 \
                                          ERTS_CTRIE_KEY_GET(candidate))) {    \
                        *out = candidate;                                      \
                        return CTRIE_OK;                                       \
                    }                                                          \
                    return CTRIE_NOT_FOUND;                                    \
                }
#        else
#            define CTRIE_TRAVERSE_TOMB_BODY
#        endif

CTRIE_TRAVERSE_BRANCH_DEF(lookup)
CTRIE_TRAVERSE_LIST_DEF(lookup)
CTRIE_TRAVERSE_DEF(lookup)

#        undef CTRIE_TRAVERSE_BRANCH_PARAMETERS
#        undef CTRIE_TRAVERSE_BRANCH_HASH
#        undef CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY
#        undef CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY
#        undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY
#        undef CTRIE_TRAVERSE_LIST_PARAMETERS
#        undef CTRIE_TRAVERSE_LIST_SEARCH
#        undef CTRIE_TRAVERSE_LIST_BODY

#        undef CTRIE_TRAVERSE_PARAMETERS
#        undef CTRIE_TRAVERSE_ARGUMENTS_BRANCH
#        undef CTRIE_TRAVERSE_ARGUMENTS_LIST
#        undef CTRIE_TRAVERSE_TOMB_BODY

#    endif /* ERTS_CTRIE_WANT_LOOKUP */

/* ************************************************************************* */

#    ifdef ERTS_CTRIE_WANT_MEMBER

#        define CTRIE_TRAVERSE_PARAMETERS                                      \
            ERTS_CTRIE_KEY_TYPE key, ERTS_CTRIE_HASH_TYPE hash

#        define CTRIE_TRAVERSE_LIST_PARAMETERS                                 \
            ERTS_CTRIE_KEY_TYPE key, ERTS_CTRIE_HASH_TYPE hash

#        define CTRIE_TRAVERSE_BRANCH_PARAMETERS                               \
            ERTS_CTRIE_KEY_TYPE key, ERTS_CTRIE_HASH_TYPE hash

#        define ctrie_member_branch CTRIE_LOCAL_FUNC(member_branch)
CTRIE_TRAVERSE_BRANCH_DECL(member);
#        define ctrie_member_list CTRIE_LOCAL_FUNC(member_list)
CTRIE_TRAVERSE_LIST_DECL(member);
#        define ctrie_member CTRIE_LOCAL_FUNC(member)
CTRIE_TRAVERSE_DECL(member);

#        define CTRIE_TRAVERSE_LIST_SEARCH                                     \
            CTRIE_TRAVERSE_LIST_SEARCH_HELPER_EQ(key, hash)

#        define CTRIE_TRAVERSE_LIST_BODY                                       \
            {                                                                  \
                if (candidate == NULL) {                                       \
                    return CTRIE_NOT_FOUND;                                    \
                }                                                              \
                return CTRIE_OK;                                               \
            }

#        define CTRIE_TRAVERSE_BRANCH_HASH hash

#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#            define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                     \
                if (indirection->generation == generation ||                   \
                    trie->read_only) {                                         \
                    return ctrie_member(trie,                                  \
                                        indirection,                           \
                                        parent,                                \
                                        level + ERTS_CTRIE_BRANCH_FACTOR,      \
                                        generation,                            \
                                        hash,                                  \
                                        key);                                  \
                } else {                                                       \
                    if (ctrie_branch_renew(trie,                               \
                                           branch,                             \
                                           parent,                             \
                                           generation)) {                      \
                        return ctrie_member(trie,                              \
                                            parent,                            \
                                            grandparent,                       \
                                            level,                             \
                                            generation,                        \
                                            key,                               \
                                            hash);                             \
                    }                                                          \
                    return CTRIE_RESTART;                                      \
                }
#        else
#            define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                     \
                return ctrie_member(trie,                                      \
                                    indirection,                               \
                                    parent,                                    \
                                    level + ERTS_CTRIE_BRANCH_FACTOR,          \
                                    hash,                                      \
                                    key);
#        endif

#        define CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY return CTRIE_NOT_FOUND;
#        define CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY                           \
            if (ERTS_CTRIE_HASH_EQ(hash, ERTS_CTRIE_HASH_GET(candidate)) &&    \
                ERTS_CTRIE_KEY_EQ(key, ERTS_CTRIE_KEY_GET(candidate))) {       \
                return CTRIE_OK;                                               \
            }                                                                  \
            return CTRIE_NOT_FOUND;

#        define CTRIE_TRAVERSE_ARGUMENTS_BRANCH key, hash
#        define CTRIE_TRAVERSE_ARGUMENTS_LIST key, hash

#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#            define CTRIE_TRAVERSE_TOMB_BODY                                   \
                if (trie->read_only) {                                         \
                    CTrieTombNode *tomb = CTRIE_MAIN_NODE_PTR(node, tomb);     \
                    CTrieSingletonNode *candidate = tomb->singleton;           \
                    if (ERTS_CTRIE_HASH_EQ(hash,                               \
                                           ERTS_CTRIE_HASH_GET(candidate)) &&  \
                        ERTS_CTRIE_KEY_EQ(key,                                 \
                                          ERTS_CTRIE_KEY_GET(candidate))) {    \
                        return CTRIE_OK;                                       \
                    }                                                          \
                    return CTRIE_NOT_FOUND;                                    \
                }
#        else
#            define CTRIE_TRAVERSE_TOMB_BODY
#        endif

CTRIE_TRAVERSE_BRANCH_DEF(member)
CTRIE_TRAVERSE_LIST_DEF(member)
CTRIE_TRAVERSE_DEF(member)

#        undef CTRIE_TRAVERSE_BRANCH_PARAMETERS
#        undef CTRIE_TRAVERSE_BRANCH_HASH
#        undef CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY
#        undef CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY
#        undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY
#        undef CTRIE_TRAVERSE_LIST_PARAMETERS
#        undef CTRIE_TRAVERSE_LIST_SEARCH
#        undef CTRIE_TRAVERSE_LIST_BODY

#        undef CTRIE_TRAVERSE_PARAMETERS
#        undef CTRIE_TRAVERSE_ARGUMENTS_BRANCH
#        undef CTRIE_TRAVERSE_ARGUMENTS_LIST
#        undef CTRIE_TRAVERSE_TOMB_BODY

#    endif /* ERTS_CTRIE_WANT_MEMBER */

/* ************************************************************************* */

#    ifdef ERTS_CTRIE_WANT_TAKE

#        define CTRIE_TRAVERSE_PARAMETERS                                      \
            ERTS_CTRIE_KEY_TYPE key, ERTS_CTRIE_HASH_TYPE hash,                \
                    CTrieSingletonNode **out

#        define CTRIE_TRAVERSE_LIST_PARAMETERS                                 \
            CTrieIndirectionNode *parent, ERTS_CTRIE_KEY_TYPE key,             \
                    ERTS_CTRIE_HASH_TYPE hash, CTrieSingletonNode **out

#        define CTRIE_TRAVERSE_BRANCH_PARAMETERS                               \
            ERTS_CTRIE_KEY_TYPE key, ERTS_CTRIE_HASH_TYPE hash,                \
                    CTrieSingletonNode **out

#        define ctrie_take_branch CTRIE_LOCAL_FUNC(take_branch)
CTRIE_TRAVERSE_BRANCH_DECL(take);
#        define ctrie_take_list CTRIE_LOCAL_FUNC(take_list)
CTRIE_TRAVERSE_LIST_DECL(take);
#        define ctrie_take CTRIE_LOCAL_FUNC(take)
CTRIE_TRAVERSE_DECL(take);

#        define CTRIE_TRAVERSE_LIST_SEARCH                                     \
            CTRIE_TRAVERSE_LIST_SEARCH_HELPER_EQ(key, hash)

#        define CTRIE_TRAVERSE_LIST_BODY                                       \
            {                                                                  \
                if (candidate != NULL) {                                       \
                    /* We _KNOW_ that the removal operation below succeeds */  \
                    /* in single-threaded mode. */                             \
                    CTRIE_KEEP_IF_SINGLE_THREADED(&candidate->base);           \
                    *out = candidate;                                          \
                }                                                              \
                CTRIE_TRAVERSE_LIST_REMOVAL_HELPER                             \
            }

#        define CTRIE_TRAVERSE_BRANCH_HASH hash

#        define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY__                       \
            {                                                                  \
                enum erts_ctrie_result res;                                    \
                res = ctrie_take(trie,                                         \
                                 indirection,                                  \
                                 parent,                                       \
                                 level + ERTS_CTRIE_BRANCH_FACTOR,             \
                                 CTRIE_GENERATION_ARGUMENT key,                \
                                 CTRIE_TRAVERSE_BRANCH_HASH,                   \
                                 out);                                         \
                if (res == CTRIE_OK) {                                         \
                    const erts_aint_t node =                                   \
                            ctrie_indirection_read(trie, parent);              \
                    if (CTRIE_MAIN_NODE_TYPE(node) ==                          \
                        CTRIE_MAIN_NODE_TYPE_TOMB) {                           \
                        ctrie_clean_grandparent(                               \
                                trie,                                          \
                                grandparent,                                   \
                                parent,                                        \
                                node,                                          \
                                CTRIE_GENERATION_ARGUMENT                      \
                                        CTRIE_TRAVERSE_BRANCH_HASH,            \
                                level - ERTS_CTRIE_BRANCH_FACTOR);             \
                    }                                                          \
                }                                                              \
                return res;                                                    \
            }

#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#            define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                     \
                if (indirection->generation != generation) {                   \
                    if (ctrie_branch_renew(trie,                               \
                                           branch,                             \
                                           parent,                             \
                                           generation)) {                      \
                        return ctrie_take(trie,                                \
                                          parent,                              \
                                          grandparent,                         \
                                          level,                               \
                                          generation,                          \
                                          key,                                 \
                                          CTRIE_TRAVERSE_BRANCH_HASH,          \
                                          out);                                \
                    }                                                          \
                    return CTRIE_RESTART;                                      \
                }                                                              \
                CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY__
#        else
#            define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                     \
                CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY__
#        endif

#        define CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY return CTRIE_NOT_FOUND;
#        define CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY                           \
            {                                                                  \
                if (ERTS_CTRIE_HASH_EQ(hash,                                   \
                                       ERTS_CTRIE_HASH_GET(candidate)) &&      \
                    ERTS_CTRIE_KEY_EQ(key, ERTS_CTRIE_KEY_GET(candidate))) {   \
                    CTRIE_KEEP_IF_SINGLE_THREADED(&candidate->base);           \
                    if (ctrie_branch_remove_singleton(trie,                    \
                                                      branch,                  \
                                                      parent,                  \
                                                      flag,                    \
                                                      level)) {                \
                        *out = candidate;                                      \
                        return CTRIE_OK;                                       \
                    }                                                          \
                    CTRIE_UNREACHABLE_IF_SINGLE_THREADED();                    \
                    return CTRIE_RESTART;                                      \
                }                                                              \
                return CTRIE_NOT_FOUND;                                        \
            }

#        define CTRIE_TRAVERSE_ARGUMENTS_BRANCH key, hash, out
#        define CTRIE_TRAVERSE_ARGUMENTS_LIST indirection, key, hash, out

#        define CTRIE_TRAVERSE_TOMB_BODY

CTRIE_TRAVERSE_BRANCH_DEF(take)
CTRIE_TRAVERSE_LIST_DEF(take)
CTRIE_TRAVERSE_DEF(take)

#        undef CTRIE_TRAVERSE_BRANCH_PARAMETERS
#        undef CTRIE_TRAVERSE_BRANCH_HASH
#        undef CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY__
#        undef CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY
#        undef CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY
#        undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY
#        undef CTRIE_TRAVERSE_LIST_PARAMETERS
#        undef CTRIE_TRAVERSE_LIST_SEARCH
#        undef CTRIE_TRAVERSE_LIST_BODY

#        undef CTRIE_TRAVERSE_PARAMETERS
#        undef CTRIE_TRAVERSE_ARGUMENTS_BRANCH
#        undef CTRIE_TRAVERSE_ARGUMENTS_LIST
#        undef CTRIE_TRAVERSE_TOMB_BODY

#    endif /* ERTS_CTRIE_WANT_TAKE */

#    ifdef ERTS_CTRIE_WANT_REMOVE

#        define CTRIE_TRAVERSE_PARAMETERS                                      \
            ERTS_CTRIE_KEY_TYPE key, ERTS_CTRIE_HASH_TYPE hash

#        define CTRIE_TRAVERSE_LIST_PARAMETERS                                 \
            CTrieIndirectionNode *parent, ERTS_CTRIE_KEY_TYPE key,             \
                    ERTS_CTRIE_HASH_TYPE hash

#        define CTRIE_TRAVERSE_BRANCH_PARAMETERS                               \
            ERTS_CTRIE_KEY_TYPE key, ERTS_CTRIE_HASH_TYPE hash

#        define ctrie_remove_branch CTRIE_LOCAL_FUNC(remove_branch)
CTRIE_TRAVERSE_BRANCH_DECL(remove);
#        define ctrie_remove_list CTRIE_LOCAL_FUNC(remove_list)
CTRIE_TRAVERSE_LIST_DECL(remove);
#        define ctrie_remove CTRIE_LOCAL_FUNC(remove)
CTRIE_TRAVERSE_DECL(remove);

#        define CTRIE_TRAVERSE_LIST_SEARCH                                     \
            CTRIE_TRAVERSE_LIST_SEARCH_HELPER_EQ(key, hash)

#        define CTRIE_TRAVERSE_LIST_BODY CTRIE_TRAVERSE_LIST_REMOVAL_HELPER

#        define CTRIE_TRAVERSE_BRANCH_HASH hash

#        define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY__                       \
            {                                                                  \
                enum erts_ctrie_result res;                                    \
                res = ctrie_remove(trie,                                       \
                                   indirection,                                \
                                   parent,                                     \
                                   level + ERTS_CTRIE_BRANCH_FACTOR,           \
                                   CTRIE_GENERATION_ARGUMENT key,              \
                                   CTRIE_TRAVERSE_BRANCH_HASH);                \
                if (res == CTRIE_OK) {                                         \
                    const erts_aint_t node =                                   \
                            ctrie_indirection_read(trie, parent);              \
                    if (CTRIE_MAIN_NODE_TYPE(node) ==                          \
                        CTRIE_MAIN_NODE_TYPE_TOMB) {                           \
                        ctrie_clean_grandparent(                               \
                                trie,                                          \
                                grandparent,                                   \
                                parent,                                        \
                                node,                                          \
                                CTRIE_GENERATION_ARGUMENT                      \
                                        CTRIE_TRAVERSE_BRANCH_HASH,            \
                                level - ERTS_CTRIE_BRANCH_FACTOR);             \
                    }                                                          \
                }                                                              \
                return res;                                                    \
            }

#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#            define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                     \
                if (indirection->generation != generation) {                   \
                    if (ctrie_branch_renew(trie,                               \
                                           branch,                             \
                                           parent,                             \
                                           generation)) {                      \
                        return ctrie_remove(trie,                              \
                                            parent,                            \
                                            grandparent,                       \
                                            level,                             \
                                            generation,                        \
                                            key,                               \
                                            CTRIE_TRAVERSE_BRANCH_HASH);       \
                    }                                                          \
                    return CTRIE_RESTART;                                      \
                }                                                              \
                CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY__
#        else
#            define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                     \
                CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY__
#        endif

#        define CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY return CTRIE_NOT_FOUND;
#        define CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY                           \
            {                                                                  \
                if (ERTS_CTRIE_HASH_EQ(hash,                                   \
                                       ERTS_CTRIE_HASH_GET(candidate)) &&      \
                    ERTS_CTRIE_KEY_EQ(key, ERTS_CTRIE_KEY_GET(candidate))) {   \
                    if (ctrie_branch_remove_singleton(trie,                    \
                                                      branch,                  \
                                                      parent,                  \
                                                      flag,                    \
                                                      level)) {                \
                        return CTRIE_OK;                                       \
                    }                                                          \
                    CTRIE_UNREACHABLE_IF_SINGLE_THREADED();                    \
                    return CTRIE_RESTART;                                      \
                }                                                              \
                return CTRIE_NOT_FOUND;                                        \
            }

#        define CTRIE_TRAVERSE_ARGUMENTS_BRANCH key, hash
#        define CTRIE_TRAVERSE_ARGUMENTS_LIST indirection, key, hash

#        define CTRIE_TRAVERSE_TOMB_BODY

CTRIE_TRAVERSE_BRANCH_DEF(remove)
CTRIE_TRAVERSE_LIST_DEF(remove)
CTRIE_TRAVERSE_DEF(remove)

#        undef CTRIE_TRAVERSE_BRANCH_PARAMETERS
#        undef CTRIE_TRAVERSE_BRANCH_HASH
#        undef CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY__
#        undef CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY
#        undef CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY
#        undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY
#        undef CTRIE_TRAVERSE_LIST_PARAMETERS
#        undef CTRIE_TRAVERSE_LIST_SEARCH
#        undef CTRIE_TRAVERSE_LIST_BODY

#        undef CTRIE_TRAVERSE_PARAMETERS
#        undef CTRIE_TRAVERSE_ARGUMENTS_BRANCH
#        undef CTRIE_TRAVERSE_ARGUMENTS_LIST
#        undef CTRIE_TRAVERSE_TOMB_BODY

#    endif /* ERTS_CTRIE_WANT_REMOVE */

#    ifdef ERTS_CTRIE_WANT_ERASE

#        define CTRIE_TRAVERSE_PARAMETERS CTrieSingletonNode *expected

#        define CTRIE_TRAVERSE_LIST_PARAMETERS                                 \
            CTrieIndirectionNode *parent, CTrieSingletonNode *expected

#        define CTRIE_TRAVERSE_BRANCH_PARAMETERS CTrieSingletonNode *expected

#        define ctrie_erase_branch CTRIE_LOCAL_FUNC(erase_branch)
CTRIE_TRAVERSE_BRANCH_DECL(erase);
#        define ctrie_erase_list CTRIE_LOCAL_FUNC(erase_list)
CTRIE_TRAVERSE_LIST_DECL(erase);
#        define ctrie_erase CTRIE_LOCAL_FUNC(erase)
CTRIE_TRAVERSE_DECL(erase);

#        define CTRIE_TRAVERSE_LIST_SEARCH                                     \
            CTRIE_TRAVERSE_LIST_SEARCH_HELPER_EXPECTED(expected)

#        define CTRIE_TRAVERSE_LIST_BODY CTRIE_TRAVERSE_LIST_REMOVAL_HELPER

#        define CTRIE_TRAVERSE_BRANCH_HASH ERTS_CTRIE_HASH_GET(expected)

#        define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY__                       \
            {                                                                  \
                enum erts_ctrie_result res;                                    \
                res = ctrie_erase(trie,                                        \
                                  indirection,                                 \
                                  parent,                                      \
                                  level + ERTS_CTRIE_BRANCH_FACTOR,            \
                                  CTRIE_GENERATION_ARGUMENT expected);         \
                if (res == CTRIE_OK) {                                         \
                    const erts_aint_t node =                                   \
                            ctrie_indirection_read(trie, parent);              \
                    if (CTRIE_MAIN_NODE_TYPE(node) ==                          \
                        CTRIE_MAIN_NODE_TYPE_TOMB) {                           \
                        ctrie_clean_grandparent(                               \
                                trie,                                          \
                                grandparent,                                   \
                                parent,                                        \
                                node,                                          \
                                CTRIE_GENERATION_ARGUMENT                      \
                                        CTRIE_TRAVERSE_BRANCH_HASH,            \
                                level - ERTS_CTRIE_BRANCH_FACTOR);             \
                    }                                                          \
                }                                                              \
                return res;                                                    \
            }

#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#            define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                     \
                if (indirection->generation != generation) {                   \
                    if (ctrie_branch_renew(trie,                               \
                                           branch,                             \
                                           parent,                             \
                                           generation)) {                      \
                        return ctrie_erase(                                    \
                                trie,                                          \
                                parent,                                        \
                                grandparent,                                   \
                                level,                                         \
                                CTRIE_GENERATION_ARGUMENT expected);           \
                    }                                                          \
                    CTRIE_UNREACHABLE_IF_SINGLE_THREADED();                    \
                    return CTRIE_RESTART;                                      \
                }                                                              \
                CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY__
#        else
#            define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                     \
                CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY__
#        endif

#        define CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY return CTRIE_NOT_FOUND;
#        define CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY                           \
            {                                                                  \
                if (candidate == expected) {                                   \
                    if (ctrie_branch_remove_singleton(trie,                    \
                                                      branch,                  \
                                                      parent,                  \
                                                      flag,                    \
                                                      level)) {                \
                        return CTRIE_OK;                                       \
                    }                                                          \
                    CTRIE_UNREACHABLE_IF_SINGLE_THREADED();                    \
                    return CTRIE_RESTART;                                      \
                }                                                              \
                return CTRIE_NOT_FOUND;                                        \
            }

#        define CTRIE_TRAVERSE_ARGUMENTS_BRANCH expected
#        define CTRIE_TRAVERSE_ARGUMENTS_LIST indirection, expected

#        define CTRIE_TRAVERSE_TOMB_BODY

CTRIE_TRAVERSE_BRANCH_DEF(erase)
CTRIE_TRAVERSE_LIST_DEF(erase)
CTRIE_TRAVERSE_DEF(erase)

#        undef CTRIE_TRAVERSE_BRANCH_PARAMETERS
#        undef CTRIE_TRAVERSE_BRANCH_HASH
#        undef CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY__
#        undef CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY
#        undef CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY
#        undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY
#        undef CTRIE_TRAVERSE_LIST_PARAMETERS
#        undef CTRIE_TRAVERSE_LIST_SEARCH
#        undef CTRIE_TRAVERSE_LIST_BODY

#        undef CTRIE_TRAVERSE_PARAMETERS
#        undef CTRIE_TRAVERSE_ARGUMENTS_BRANCH
#        undef CTRIE_TRAVERSE_ARGUMENTS_LIST
#        undef CTRIE_TRAVERSE_TOMB_BODY

#    endif /* ERTS_CTRIE_WANT_ERASE */

/* ------------------------------------------------------------------------- */

#    define CTRIE_TRAVERSE_PARAMETERS CTrieSingletonNode *replacement

#    define CTRIE_TRAVERSE_LIST_PARAMETERS                                     \
        CTrieIndirectionNode *parent, CTrieSingletonNode *replacement

#    define CTRIE_TRAVERSE_BRANCH_PARAMETERS CTrieSingletonNode *replacement

#    ifdef ERTS_CTRIE_WANT_INSERT
#        define ctrie_insert_branch CTRIE_LOCAL_FUNC(insert_branch)
CTRIE_TRAVERSE_BRANCH_DECL(insert);
#        define ctrie_insert_list CTRIE_LOCAL_FUNC(insert_list)
CTRIE_TRAVERSE_LIST_DECL(insert);
#        define ctrie_insert CTRIE_LOCAL_FUNC(insert)
CTRIE_TRAVERSE_DECL(insert);
#    endif

#    ifdef ERTS_CTRIE_WANT_UPDATE
#        define ctrie_update_branch CTRIE_LOCAL_FUNC(update_branch)
CTRIE_TRAVERSE_BRANCH_DECL(update);
#        define ctrie_update_list CTRIE_LOCAL_FUNC(update_list)
CTRIE_TRAVERSE_LIST_DECL(update);
#        define ctrie_update CTRIE_LOCAL_FUNC(update)
CTRIE_TRAVERSE_DECL(update);
#    endif

#    ifdef ERTS_CTRIE_WANT_UPSERT
#        define ctrie_upsert_branch CTRIE_LOCAL_FUNC(upsert_branch)
CTRIE_TRAVERSE_BRANCH_DECL(upsert);
#        define ctrie_upsert_list CTRIE_LOCAL_FUNC(upsert_list)
CTRIE_TRAVERSE_LIST_DECL(upsert);
#        define ctrie_upsert CTRIE_LOCAL_FUNC(upsert)
CTRIE_TRAVERSE_DECL(upsert);
#    endif

#    define CTRIE_TRAVERSE_LIST_SEARCH                                         \
        CTRIE_TRAVERSE_LIST_SEARCH_HELPER_EQ(ERTS_CTRIE_KEY_GET(replacement),  \
                                             ERTS_CTRIE_HASH_GET(replacement))

#    define CTRIE_TRAVERSE_BRANCH_HASH ERTS_CTRIE_HASH_GET(replacement)

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#        define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                         \
            if (indirection->generation == generation) {                       \
                return CTRIE_MODIFY_VARIANT(trie,                              \
                                            indirection,                       \
                                            parent,                            \
                                            level + ERTS_CTRIE_BRANCH_FACTOR,  \
                                            generation,                        \
                                            replacement);                      \
            } else {                                                           \
                if (ctrie_branch_renew(trie, branch, parent, generation)) {    \
                    return CTRIE_MODIFY_VARIANT(trie,                          \
                                                parent,                        \
                                                grandparent,                   \
                                                level,                         \
                                                generation,                    \
                                                replacement);                  \
                }                                                              \
                CTRIE_UNREACHABLE_IF_SINGLE_THREADED();                        \
                return CTRIE_RESTART;                                          \
            }
#    else
#        define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                         \
            return CTRIE_MODIFY_VARIANT(trie,                                  \
                                        indirection,                           \
                                        parent,                                \
                                        level + ERTS_CTRIE_BRANCH_FACTOR,      \
                                        replacement);
#    endif

#    define CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY_INSERT_UPSERT                 \
        {                                                                      \
            if (ctrie_branch_add_singleton(trie,                               \
                                           branch,                             \
                                           parent,                             \
                                           replacement,                        \
                                           flag)) {                            \
                return CTRIE_OK;                                               \
            }                                                                  \
            CTRIE_UNREACHABLE_IF_SINGLE_THREADED();                            \
            return CTRIE_RESTART;                                              \
        }

#    define CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY_INSERT                        \
        {                                                                      \
            CTrieTransaction ts;                                               \
            ctrie_transaction_init(&ts);                                       \
            if (ERTS_CTRIE_HASH_EQ(ERTS_CTRIE_HASH_GET(replacement),           \
                                   ERTS_CTRIE_HASH_GET(candidate)) &&          \
                ERTS_CTRIE_KEY_EQ(ERTS_CTRIE_KEY_GET(replacement),             \
                                  ERTS_CTRIE_KEY_GET(candidate))) {            \
                return CTRIE_ALREADY_EXISTS;                                   \
            } else {                                                           \
                CTrieIndirectionNode *merged = ctrie_singleton_merge(          \
                        &ts,                                                   \
                        level + ERTS_CTRIE_BRANCH_FACTOR,                      \
                        CTRIE_GENERATION_ARGUMENT replacement,                 \
                        candidate);                                            \
                if (ctrie_branch_update_twig(                                  \
                            trie,                                              \
                            branch,                                            \
                            &ts,                                               \
                            parent,                                            \
                            CTRIE_TWIG_NODE_AS(merged, INDIRECTION),           \
                            ERTS_CTRIE_OWNERSHIP_UNIQUE,                       \
                            flag)) {                                           \
                    return CTRIE_OK;                                           \
                }                                                              \
                CTRIE_UNREACHABLE_IF_SINGLE_THREADED();                        \
                return CTRIE_RESTART;                                          \
            }                                                                  \
        }

#    define CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY_UPSERT                        \
        {                                                                      \
            CTrieTransaction ts;                                               \
            ctrie_transaction_init(&ts);                                       \
            if (ERTS_CTRIE_HASH_EQ(ERTS_CTRIE_HASH_GET(candidate),             \
                                   ERTS_CTRIE_HASH_GET(replacement)) &&        \
                ERTS_CTRIE_KEY_EQ(ERTS_CTRIE_KEY_GET(candidate),               \
                                  ERTS_CTRIE_KEY_GET(replacement))) {          \
                if (ctrie_branch_update_twig(                                  \
                            trie,                                              \
                            branch,                                            \
                            &ts,                                               \
                            parent,                                            \
                            CTRIE_TWIG_NODE_AS(replacement, SINGLETON),        \
                            ERTS_CTRIE_OWNERSHIP_UNIQUE,                       \
                            flag)) {                                           \
                    return CTRIE_OK;                                           \
                }                                                              \
            } else {                                                           \
                CTrieIndirectionNode *merged = ctrie_singleton_merge(          \
                        &ts,                                                   \
                        level + ERTS_CTRIE_BRANCH_FACTOR,                      \
                        CTRIE_GENERATION_ARGUMENT replacement,                 \
                        candidate);                                            \
                if (ctrie_branch_update_twig(                                  \
                            trie,                                              \
                            branch,                                            \
                            &ts,                                               \
                            parent,                                            \
                            CTRIE_TWIG_NODE_AS(merged, INDIRECTION),           \
                            ERTS_CTRIE_OWNERSHIP_UNIQUE,                       \
                            flag)) {                                           \
                    return CTRIE_OK;                                           \
                }                                                              \
            }                                                                  \
            CTRIE_UNREACHABLE_IF_SINGLE_THREADED();                            \
            return CTRIE_RESTART;                                              \
        }

#    define CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY_UPDATE                        \
        {                                                                      \
            if (ERTS_CTRIE_HASH_EQ(ERTS_CTRIE_HASH_GET(candidate),             \
                                   ERTS_CTRIE_HASH_GET(replacement)) &&        \
                ERTS_CTRIE_KEY_EQ(ERTS_CTRIE_KEY_GET(candidate),               \
                                  ERTS_CTRIE_KEY_GET(replacement))) {          \
                CTrieTransaction ts;                                           \
                ctrie_transaction_init(&ts);                                   \
                if (ctrie_branch_update_twig(                                  \
                            trie,                                              \
                            branch,                                            \
                            &ts,                                               \
                            parent,                                            \
                            CTRIE_TWIG_NODE_AS(replacement, SINGLETON),        \
                            ERTS_CTRIE_OWNERSHIP_UNIQUE,                       \
                            flag)) {                                           \
                    return CTRIE_OK;                                           \
                }                                                              \
                CTRIE_UNREACHABLE_IF_SINGLE_THREADED();                        \
                return CTRIE_RESTART;                                          \
            }                                                                  \
            return CTRIE_NOT_FOUND;                                            \
        }

#    define CTRIE_TRAVERSE_ARGUMENTS_BRANCH replacement
#    define CTRIE_TRAVERSE_ARGUMENTS_LIST indirection, replacement

#    define CTRIE_TRAVERSE_TOMB_BODY

#    ifdef ERTS_CTRIE_WANT_INSERT

#        undef CTRIE_MODIFY_VARIANT
#        define CTRIE_MODIFY_VARIANT ctrie_insert
#        undef CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY
#        define CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY                           \
            CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY_INSERT_UPSERT
#        undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY
#        define CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY                           \
            CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY_INSERT
#        undef CTRIE_TRAVERSE_LIST_BODY
#        define CTRIE_TRAVERSE_LIST_BODY                                       \
            {                                                                  \
                if (candidate != NULL) {                                       \
                    return CTRIE_ALREADY_EXISTS;                               \
                }                                                              \
                CTRIE_TRAVERSE_LIST_MODIFY_HELPER                              \
            }

CTRIE_TRAVERSE_BRANCH_DEF(insert)
CTRIE_TRAVERSE_LIST_DEF(insert)
CTRIE_TRAVERSE_DEF(insert)

#    endif /* ERTS_CTRIE_WANT_INSERT */

#    ifdef ERTS_CTRIE_WANT_UPDATE

#        undef CTRIE_MODIFY_VARIANT
#        define CTRIE_MODIFY_VARIANT ctrie_update
#        undef CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY
#        define CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY return CTRIE_NOT_FOUND;
#        undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY
#        define CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY                           \
            CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY_UPDATE
#        undef CTRIE_TRAVERSE_LIST_BODY
#        define CTRIE_TRAVERSE_LIST_BODY                                       \
            {                                                                  \
                if (candidate == NULL) {                                       \
                    return CTRIE_NOT_FOUND;                                    \
                }                                                              \
                CTRIE_TRAVERSE_LIST_MODIFY_HELPER                              \
            }

CTRIE_TRAVERSE_LIST_DEF(update)
CTRIE_TRAVERSE_BRANCH_DEF(update)
CTRIE_TRAVERSE_DEF(update)

#    endif /* ERTS_CTRIE_WANT_UPDATE */

#    ifdef ERTS_CTRIE_WANT_UPSERT

#        undef CTRIE_MODIFY_VARIANT
#        define CTRIE_MODIFY_VARIANT ctrie_upsert
#        undef CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY
#        define CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY                           \
            CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY_INSERT_UPSERT
#        undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY
#        define CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY                           \
            CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY_UPSERT
#        undef CTRIE_TRAVERSE_LIST_BODY
#        define CTRIE_TRAVERSE_LIST_BODY CTRIE_TRAVERSE_LIST_MODIFY_HELPER

CTRIE_TRAVERSE_BRANCH_DEF(upsert)
CTRIE_TRAVERSE_LIST_DEF(upsert)
CTRIE_TRAVERSE_DEF(upsert)

#    endif /* ERTS_CTRIE_WANT_UPSERT */

#    undef CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY_INSERT_UPSERT
#    undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY_INSERT
#    undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY_UPDATE
#    undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY_UPSERT

/* ------------------------------------------------------------------------- */

#    undef CTRIE_TRAVERSE_BRANCH_PARAMETERS
#    undef CTRIE_TRAVERSE_BRANCH_HASH
#    undef CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY
#    undef CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY
#    undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY
#    undef CTRIE_TRAVERSE_LIST_PARAMETERS
#    undef CTRIE_TRAVERSE_LIST_SEARCH
#    undef CTRIE_TRAVERSE_LIST_BODY

#    undef CTRIE_TRAVERSE_PARAMETERS
#    undef CTRIE_TRAVERSE_ARGUMENTS_BRANCH
#    undef CTRIE_TRAVERSE_ARGUMENTS_LIST
#    undef CTRIE_TRAVERSE_TOMB_BODY

/* ************************************************************************* */

#    ifdef ERTS_CTRIE_WANT_REPLACE

#        define CTRIE_TRAVERSE_PARAMETERS                                      \
            CTrieSingletonNode *replacement, CTrieSingletonNode *expected

#        define CTRIE_TRAVERSE_LIST_PARAMETERS                                 \
            CTrieIndirectionNode *parent, CTrieSingletonNode *replacement,     \
                    CTrieSingletonNode *expected

#        define CTRIE_TRAVERSE_BRANCH_PARAMETERS                               \
            CTrieSingletonNode *replacement, CTrieSingletonNode *expected

#        define ctrie_replace_list CTRIE_LOCAL_FUNC(replace_list)
CTRIE_TRAVERSE_LIST_DECL(replace);
#        define ctrie_replace_branch CTRIE_LOCAL_FUNC(replace_branch)
CTRIE_TRAVERSE_BRANCH_DECL(replace);
#        define ctrie_replace CTRIE_LOCAL_FUNC(replace)
CTRIE_TRAVERSE_DECL(replace);

#        define CTRIE_TRAVERSE_LIST_SEARCH                                     \
            CTRIE_TRAVERSE_LIST_SEARCH_HELPER_EXPECTED(expected)

#        define CTRIE_TRAVERSE_LIST_BODY                                       \
            {                                                                  \
                if (candidate != expected) {                                   \
                    return CTRIE_NOT_FOUND;                                    \
                }                                                              \
                CTRIE_TRAVERSE_LIST_MODIFY_HELPER                              \
            }

#        define CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY return CTRIE_NOT_FOUND;

#        define CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY                           \
            {                                                                  \
                if (candidate == expected) {                                   \
                    CTrieTransaction ts;                                       \
                    ctrie_transaction_init(&ts);                               \
                    if (ctrie_branch_update_twig(                              \
                                trie,                                          \
                                branch,                                        \
                                &ts,                                           \
                                parent,                                        \
                                CTRIE_TWIG_NODE_AS(replacement, SINGLETON),    \
                                ERTS_CTRIE_OWNERSHIP_UNIQUE,                   \
                                flag)) {                                       \
                        return CTRIE_OK;                                       \
                    }                                                          \
                    CTRIE_UNREACHABLE_IF_SINGLE_THREADED();                    \
                    return CTRIE_RESTART;                                      \
                }                                                              \
                return CTRIE_NOT_FOUND;                                        \
            }

#        define CTRIE_TRAVERSE_BRANCH_HASH ERTS_CTRIE_HASH_GET(replacement)

#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
#            define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                     \
                if (indirection->generation == generation) {                   \
                    return ctrie_replace(trie,                                 \
                                         indirection,                          \
                                         parent,                               \
                                         level + ERTS_CTRIE_BRANCH_FACTOR,     \
                                         generation,                           \
                                         replacement,                          \
                                         expected);                            \
                } else {                                                       \
                    if (ctrie_branch_renew(trie,                               \
                                           branch,                             \
                                           parent,                             \
                                           generation)) {                      \
                        return ctrie_replace(trie,                             \
                                             parent,                           \
                                             grandparent,                      \
                                             level,                            \
                                             generation,                       \
                                             replacement,                      \
                                             expected);                        \
                    }                                                          \
                    CTRIE_UNREACHABLE_IF_SINGLE_THREADED();                    \
                    return CTRIE_RESTART;                                      \
                }
#        else
#            define CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY                     \
                return ctrie_replace(trie,                                     \
                                     indirection,                              \
                                     parent,                                   \
                                     level + ERTS_CTRIE_BRANCH_FACTOR,         \
                                     replacement,                              \
                                     expected);
#        endif

#        define CTRIE_TRAVERSE_ARGUMENTS_BRANCH replacement, expected
#        define CTRIE_TRAVERSE_ARGUMENTS_LIST indirection, replacement, expected

#        define CTRIE_TRAVERSE_TOMB_BODY

CTRIE_TRAVERSE_BRANCH_DEF(replace)
CTRIE_TRAVERSE_LIST_DEF(replace)
CTRIE_TRAVERSE_DEF(replace)

#        undef CTRIE_TRAVERSE_BRANCH_PARAMETERS
#        undef CTRIE_TRAVERSE_BRANCH_HASH
#        undef CTRIE_TRAVERSE_BRANCH_INDIRECTION_BODY
#        undef CTRIE_TRAVERSE_BRANCH_NOT_FOUND_BODY
#        undef CTRIE_TRAVERSE_BRANCH_SINGLETON_BODY
#        undef CTRIE_TRAVERSE_LIST_PARAMETERS
#        undef CTRIE_TRAVERSE_LIST_SEARCH
#        undef CTRIE_TRAVERSE_LIST_BODY

#        undef CTRIE_TRAVERSE_PARAMETERS
#        undef CTRIE_TRAVERSE_ARGUMENTS_BRANCH
#        undef CTRIE_TRAVERSE_ARGUMENTS_LIST
#        undef CTRIE_TRAVERSE_TOMB_BODY

#    endif /* ERTS_CTRIE_WANT_REPLACE */

/* ************************************************************************* */

static void CTRIE_PUBLIC_FUNC(init)(CTrie *trie) {
    CTrieIndirectionNode *root;
    CTrieBranchNode *branch;
    CTrieTransaction ts;

    ctrie_transaction_init(&ts);

    branch = ctrie_branch_make(&ts, 0, 0, 0);
    root = ctrie_node_make(CTrieIndirectionNode,
                           &ts,
#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
                           0,
#    endif
                           CTRIE_MAIN_NODE_AS(branch, BRANCH),
                           ERTS_CTRIE_OWNERSHIP_UNIQUE);

    erts_atomic_init_nob(&trie->root, CTRIE_ROOT_NODE_AS(root, INDIRECTION));
#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
    erts_atomic_init_nob(&trie->sequence, 0);
    trie->read_only = false;
#    endif
}

static void CTRIE_PUBLIC_FUNC(destroy)(CTrie *trie) {
    ctrie_node_root_release(ctrie_atomic_read_ddrb(&trie->root));

#    ifdef DEBUG
    ctrie_atomic_set_nob(&trie->root, ERTS_AINT_NULL);
#    endif
}

/** @brief Initializes a singleton node representing an object, must be called
 * at least once before insertion into a trie.
 *
 * Note that it is entirely valid for an object to live in several tries at the
 * same time, and it will not be destroyed until all references have been
 * removed. */
static void CTRIE_PUBLIC_FUNC(singleton_init)(CTrieSingletonNode *singleton) {
    ctrie_atomic_init_nob(&singleton->base.refc, 1);
}

/** @brief Releases the given object, must be used after operations that insert
 * into or update objects in the trie */
static void CTRIE_PUBLIC_FUNC(singleton_release)(
        CTrieSingletonNode *singleton) {
    if (erts_refc_dectest(&singleton->base.refc, 0) == 0) {
#    ifdef ERTS_CTRIE_WANT_SINGLE_THREADED
        ERTS_CTRIE_SINGLETON_DESTRUCTOR(singleton);
#    else
        erts_schedule_thr_prgr_later_op(
                (void (*)(void *))ctrie_node_twig_destroy,
                (void *)CTRIE_TWIG_NODE_AS(singleton, SINGLETON),
                &singleton->base.cleanup.later_op);
#    endif
    }
}

#    ifdef ERTS_CTRIE_WANT_KEEP
/** @brief Retains the given object after it has been removed from a table. */
static void CTRIE_PUBLIC_FUNC(singleton_keep)(CTrieSingletonNode *singleton) {
    ctrie_node_reference(&singleton->base);
}
#    endif

#    ifdef ERTS_CTRIE_WANT_SNAPSHOTS
/** @brief Atomically creates a snapshot of the given trie. */
static enum erts_ctrie_result CTRIE_PUBLIC_FUNC(snapshot)(CTrie *trie,
                                                          int read_only,
                                                          CTrie *out) {
    CTrieIndirectionNode *root, *update;
    erts_aint_t generation, main;
    CTrieTransaction ts;

    generation = erts_atomic_inc_read_nob(&trie->sequence);

    ctrie_transaction_init(&ts);

    root = ctrie_root_read(trie, false);

    /* Because `main` might disappear in between updating the root and creating
     * the new snapshot, we must explicitly reference it twice to ensure it
     * stays alive until ownership can be moved to the snapshot. */
    main = ctrie_indirection_reference(trie, root, 2);
    update = ctrie_node_make(CTrieIndirectionNode,
                             &ts,
                             generation,
                             main,
                             ERTS_CTRIE_OWNERSHIP_ALREADY_REFERENCED);

    if (!ctrie_root_update(trie, &ts, root, update, main)) {
        /* Release main node once more as we bumped the reference count by 2,
         * and the aborted update only releases by 1. */
        ctrie_node_gcas_release(main);
        return CTRIE_RESTART;
    }

    ctrie_node_reference(CTRIE_BASE_NODE_PTR(main));
    ctrie_atomic_init_nob(
            &out->root,
            CTRIE_ROOT_NODE_AS(
                    ctrie_node_make(CTrieIndirectionNode,
                                    &ts,
                                    generation + 1,
                                    main,
                                    ERTS_CTRIE_OWNERSHIP_ALREADY_REFERENCED),
                    INDIRECTION));
    ctrie_atomic64_init_nob(&out->sequence, generation + 2);
    out->read_only = read_only;

    return CTRIE_OK;
}
#    endif

#    ifdef ERTS_CTRIE_WANT_CLEAR
/** @brief Atomically clears a trie, optionally moving all elements to another
 * trie. */
static enum erts_ctrie_result CTRIE_PUBLIC_FUNC(clear)(CTrie *trie,
                                                       CTrie *out) {
    CTrieIndirectionNode *root, *update;
    CTrieTransaction ts;
    erts_aint_t main;

#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
    /* Note that we don't need to start a new generation as the entire trie is
     * cleared, and could technically just set it to zero. However, we retain
     * the old generation to prevent spurious rebuilds in operations that race
     * with this one. */
    erts_aint64_t generation = ctrie_atomic64_read_nob(&trie->sequence);
#        endif

    ctrie_transaction_init(&ts);

    root = ctrie_root_read(trie, false);

    if (out != NULL) {
        /* We must ensure that this node stays alive until we've populated
         * `out`, see snapshot operation for more details. */
        main = ctrie_indirection_reference(trie, root, 1);
    } else {
        main = ctrie_indirection_read(trie, root);
    }

    update = ctrie_node_make(CTrieIndirectionNode,
                             &ts,
                             CTRIE_GENERATION_ARGUMENT CTRIE_MAIN_NODE_AS(
                                     ctrie_branch_make(&ts, 0, 0, 0),
                                     BRANCH),
                             ERTS_CTRIE_OWNERSHIP_UNIQUE);

    if (!ctrie_root_update(trie, &ts, root, update, main)) {
        CTRIE_UNREACHABLE_IF_SINGLE_THREADED();

        if (out != NULL) {
            ctrie_node_gcas_release(main);
        }

        return CTRIE_RESTART;
    }

    if (out != NULL) {
        ctrie_atomic_init_nob(
                &out->root,
                CTRIE_ROOT_NODE_AS(
                        ctrie_node_make(
                                CTrieIndirectionNode,
                                &ts,
                                CTRIE_GENERATION_ARGUMENT main,
                                ERTS_CTRIE_OWNERSHIP_ALREADY_REFERENCED),
                        INDIRECTION));
#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
        ctrie_atomic64_init_nob(&out->sequence, generation);
        out->read_only = false;
#        endif
    }

    return CTRIE_OK;
}
#    endif

#    ifdef ERTS_CTRIE_WANT_LOOKUP
/** @brief Retrieves an object from the trie, returning \c CTRIE_NOT_FOUND if
 * none can be found.
 *
 * In concurrent mode, the object will be valid until the next thread progress
 * tick, and \c PREFIX_singleton_keep must be called to retain it longer than
 * that.
 *
 * In single-threaded mode, the object will remain valid as long as it remains
 * in the trie. */
static enum erts_ctrie_result CTRIE_PUBLIC_FUNC(lookup)(
        CTrie *trie,
        ERTS_CTRIE_KEY_TYPE key,
        ERTS_CTRIE_HASH_TYPE hash,
        CTrieSingletonNode **out) {
    CTrieIndirectionNode *root = ctrie_root_read(trie, false);

    return ctrie_lookup(trie,
                        root,
                        NULL,
                        0,
#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
                        root->generation,
#        endif
                        key,
                        hash,
                        out);
}
#    endif /* ERTS_CTRIE_WANT_LOOKUP */

#    ifdef ERTS_CTRIE_WANT_MEMBER
/** @brief Returns \c CTRIE_OK when a key can be found in the trie, or \c
 * CTRIE_NOT_FOUND if no such object can be found. */
static enum erts_ctrie_result CTRIE_PUBLIC_FUNC(member)(
        CTrie *trie,
        ERTS_CTRIE_KEY_TYPE key,
        ERTS_CTRIE_HASH_TYPE hash) {
    CTrieIndirectionNode *root = ctrie_root_read(trie, false);

    return ctrie_member(trie,
                        root,
                        NULL,
                        0,
#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
                        root->generation,
#        endif
                        key,
                        hash);
}
#    endif /* ERTS_CTRIE_WANT_MEMBER */

#    ifdef ERTS_CTRIE_WANT_TAKE
/** @brief Removes and retrieves an object by key from the trie, returning \c
 * CTRIE_NOT_FOUND if no such object can be found.
 *
 * In concurrent mode, the object will be valid until the next thread progress
 * tick, and \c PREFIX_singleton_keep must be called to retain it longer than
 * that.
 *
 * In single-threaded mode, ownership is immediately moved to the caller
 * who must call \c PREFIX_singleton_release when they are done with it. */
static enum erts_ctrie_result CTRIE_PUBLIC_FUNC(take)(
        CTrie *trie,
        ERTS_CTRIE_KEY_TYPE key,
        ERTS_CTRIE_HASH_TYPE hash,
        CTrieSingletonNode **out) {
    CTrieIndirectionNode *root = ctrie_root_read(trie, false);

    return ctrie_take(trie,
                      root,
                      NULL,
                      0,
#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
                      root->generation,
#        endif
                      key,
                      hash,
                      out);
}
#    endif /* ERTS_CTRIE_WANT_TAKE */

#    ifdef ERTS_CTRIE_WANT_REMOVE
/** @brief Removes an object by key from the trie, returning \c CTRIE_NOT_FOUND
 * if no such object can be found. */
static enum erts_ctrie_result CTRIE_PUBLIC_FUNC(remove)(
        CTrie *trie,
        ERTS_CTRIE_KEY_TYPE key,
        ERTS_CTRIE_HASH_TYPE hash) {
    CTrieIndirectionNode *root = ctrie_root_read(trie, false);

    return ctrie_remove(trie,
                        root,
                        NULL,
                        0,
#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
                        root->generation,
#        endif
                        key,
                        hash);
}
#    endif /* ERTS_CTRIE_WANT_REMOVE */

#    ifdef ERTS_CTRIE_WANT_ERASE
/** @brief Removes a specific instance of an object from the trie, returning
 * \c CTRIE_NOT_FOUND if it can no longer be found in the trie. */
static enum erts_ctrie_result CTRIE_PUBLIC_FUNC(
        erase)(CTrie *trie, CTrieSingletonNode *singleton) {
    CTrieIndirectionNode *root = ctrie_root_read(trie, false);

    return ctrie_erase(trie,
                       root,
                       NULL,
                       0,
#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
                       root->generation,
#        endif
                       singleton);
}
#    endif /* ERTS_CTRIE_WANT_ERASE */

#    ifdef ERTS_CTRIE_WANT_INSERT
/** @brief Inserts an object, failing if the trie does already contains an
 * object with the same key.
 *
 * This transfers ownership of \c singleton to the trie, to keep it alive
 * longer than that, make sure to call \c PREFIX_singleton_keep first. */
static enum erts_ctrie_result CTRIE_PUBLIC_FUNC(
        insert)(CTrie *trie, CTrieSingletonNode *singleton) {
    CTrieIndirectionNode *root = ctrie_root_read(trie, false);

    return ctrie_insert(trie,
                        root,
                        NULL,
                        0,
#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
                        root->generation,
#        endif
                        singleton);
}
#    endif /* ERTS_CTRIE_WANT_INSERT */

#    ifdef ERTS_CTRIE_WANT_UPDATE
/** @brief Updates an object, failing if the trie does not already contain an
 * object with the same key.
 *
 * This transfers ownership of \c singleton to the trie, to keep it alive
 * longer than that, make sure to call \c PREFIX_singleton_keep first. */
static enum erts_ctrie_result CTRIE_PUBLIC_FUNC(
        update)(CTrie *trie, CTrieSingletonNode *singleton) {
    CTrieIndirectionNode *root = ctrie_root_read(trie, false);

    return ctrie_update(trie,
                        root,
                        NULL,
                        0,
#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
                        root->generation,
#        endif
                        singleton);
}
#    endif /* ERTS_CTRIE_WANT_UPDATE */

#    ifdef ERTS_CTRIE_WANT_UPSERT
/** @brief Inserts or updates (upserts) an object into the trie, similar to
 * `ets:insert/2`
 *
 * This transfers ownership of \c singleton to the trie, to keep it alive
 * longer than that, make sure to call \c PREFIX_singleton_keep first. */
static enum erts_ctrie_result CTRIE_PUBLIC_FUNC(
        upsert)(CTrie *trie, CTrieSingletonNode *singleton) {
    CTrieIndirectionNode *root = ctrie_root_read(trie, false);

    return ctrie_upsert(trie,
                        root,
                        NULL,
                        0,
#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
                        root->generation,
#        endif
                        singleton);
}
#    endif /* ERTS_CTRIE_WANT_UPSERT */

#    ifdef ERTS_CTRIE_WANT_REPLACE
/** @brief Atomically updates an object in the trie, failing with
 * \c CTRIE_NOT_FOUND if the expected object is not present.
 *
 * Needless to say, \c expected and \c replacement must have the same key and
 * hash.
 *
 * This transfers ownership of \c replacement to the trie, to keep it alive
 * longer than that, make sure to call \c PREFIX_singleton_keep first. */
static enum erts_ctrie_result CTRIE_PUBLIC_FUNC(replace)(
        CTrie *trie,
        CTrieSingletonNode *replacement,
        CTrieSingletonNode *expected) {
    CTrieIndirectionNode *root = ctrie_root_read(trie, false);

    ASSERT(replacement != expected);
    ASSERT(ERTS_CTRIE_HASH_EQ(ERTS_CTRIE_HASH_GET(replacement),
                              ERTS_CTRIE_HASH_GET(expected)) &&
           ERTS_CTRIE_KEY_EQ(ERTS_CTRIE_KEY_GET(replacement),
                             ERTS_CTRIE_KEY_GET(expected)));

    return ctrie_replace(trie,
                         root,
                         NULL,
                         0,
#        ifdef ERTS_CTRIE_WANT_SNAPSHOTS
                         root->generation,
#        endif
                         replacement,
                         expected);
}
#    endif /* ERTS_CTRIE_WANT_REPLACE */

#    ifdef ERTS_CTRIE_FIX_ALLOC_NODE
/** @brief Returns the size to be used for \c ERTS_CTRIE_NODE_ALLOC_TYPE when
 * using fix_alloc. */
static size_t CTRIE_PUBLIC_FUNC(fix_alloc_node_size)(void) {
    return sizeof(CTrieStructuralNodes);
}
#    endif

#    ifdef ERTS_CTRIE_FIX_ALLOC_BRANCH
/** @brief Returns the size to be used for \c ERTS_CTRIE_BRANCH_ALLOC_TYPE when
 * using fix_alloc. */
static size_t CTRIE_PUBLIC_FUNC(fix_alloc_branch_size)(void) {
    return sizeof(CTrieBranchNode);
}
#    endif

#endif /* ERTS_CTRIE_INCLUDE_IMPLEMENTATION_ONLY */

/* ************************************************************************* */

/* Undefine everything in this header plus any and all options so that we don't
 * export any garbage. Keep this sorted in ascending order. */

#undef CTrie
#undef ctrie_alloc_gcas
#undef ctrie_alloc_root
#undef ctrie_alloc_twig
#undef ctrie_atomic_cmpxchg_nob
#undef ctrie_atomic_inc_nob
#undef ctrie_atomic_inc_read_nob
#undef ctrie_atomic_init_nob
#undef ctrie_atomic_read_ddrb
#undef ctrie_atomic_read_relb
#undef ctrie_atomic_set_nob
#undef ctrie_atomic64_inc_read_nob
#undef ctrie_atomic64_init_nob
#undef ctrie_atomic64_read_nob
#undef ctrie_atomic64_set_nob
#undef ctrie_base_node_init
#undef ctrie_branch_abort
#undef ctrie_branch_add_singleton
#undef ctrie_branch_cas
#undef ctrie_branch_destroy
#undef ctrie_branch_remove_singleton
#undef ctrie_branch_renew
#undef ctrie_branch_transfer
#undef ctrie_branch_update_twig
#undef ctrie_clean
#undef ctrie_clean_grandparent
#undef CTRIE_CONCAT_MACRO_VALUES
#undef CTRIE_CONCAT_MACRO_VALUES__
#undef CTRIE_CTrieBranchNode_MAKE
#undef CTRIE_CTrieFailedNode_MAKE
#undef CTRIE_CTrieIndirectionNode_MAKE
#undef CTRIE_CTrieListNode_MAKE
#undef CTRIE_CTrieProposalNode_MAKE
#undef CTRIE_CTrieTombNode_MAKE
#undef ctrie_erase
#undef ctrie_erase_branch
#undef ctrie_erase_list
#undef ctrie_failed_abort
#undef ctrie_failed_destroy
#undef ctrie_gcas_commit
#undef CTRIE_GENERATION_ARGUMENT
#undef CTRIE_GENERATION_PARAMETER
#undef CTRIE_HASH_MASK
#undef ctrie_indirection_abort
#undef ctrie_indirection_cas
#undef ctrie_indirection_destroy
#undef ctrie_indirection_read
#undef ctrie_indirection_reference
#undef ctrie_insert
#undef ctrie_insert_branch
#undef ctrie_insert_list
#undef CTRIE_KEEP_IF_SINGLE_THREADED
#undef ctrie_list_abort
#undef ctrie_list_destroy
#undef CTRIE_LOCAL_FUNC
#undef CTRIE_LOCAL_TYPE
#undef ctrie_lookup
#undef ctrie_lookup_branch
#undef ctrie_lookup_list
#undef CTRIE_MAX_LEVEL
#undef ctrie_member
#undef ctrie_member_branch
#undef ctrie_member_list
#undef CTRIE_MODIFY_VARIANT
#undef ctrie_node_branch_commit
#undef ctrie_node_failed_commit
#undef ctrie_node_gcas_abort
#undef ctrie_node_gcas_commit
#undef ctrie_node_gcas_destroy
#undef ctrie_node_gcas_release
#undef ctrie_node_indirection_commit
#undef ctrie_node_list_commit
#undef ctrie_node_make
#undef ctrie_node_proposal_commit
#undef ctrie_node_reference
#undef ctrie_node_root_abort
#undef ctrie_node_root_commit
#undef ctrie_node_root_destroy
#undef ctrie_node_root_release
#undef ctrie_node_tomb_commit
#undef ctrie_node_twig_abort
#undef ctrie_node_twig_commit
#undef ctrie_node_twig_destroy
#undef ctrie_node_twig_release
#undef ctrie_proposal_destroy
#undef CTRIE_PUBLIC_FUNC
#undef CTRIE_PUBLIC_TYPE
#undef ctrie_remove
#undef ctrie_remove_branch
#undef ctrie_remove_list
#undef ctrie_replace
#undef ctrie_replace_branch
#undef ctrie_replace_list
#undef ctrie_root_commit
#undef ctrie_root_read
#undef ctrie_root_update
#undef ctrie_singleton_merge
#undef ctrie_take
#undef ctrie_take_branch
#undef ctrie_take_list
#undef ctrie_tomb_abort
#undef ctrie_tomb_destroy
#undef ctrie_transaction_abort
#undef ctrie_transaction_commit
#undef ctrie_transaction_init
#undef CTRIE_TRAVERSE
#undef CTRIE_TRAVERSE_BRANCH
#undef CTRIE_TRAVERSE_BRANCH_DECL
#undef CTRIE_TRAVERSE_BRANCH_DEF
#undef CTRIE_TRAVERSE_DECL
#undef CTRIE_TRAVERSE_DEF
#undef CTRIE_TRAVERSE_LIST
#undef CTRIE_TRAVERSE_LIST_DECL
#undef CTRIE_TRAVERSE_LIST_DEF
#undef CTRIE_TRAVERSE_LIST_MODIFY_HELPER
#undef CTRIE_TRAVERSE_LIST_REMOVAL_HELPER
#undef CTRIE_TRAVERSE_LIST_SEARCH_HELPER_EQ
#undef CTRIE_TRAVERSE_LIST_SEARCH_HELPER_EXPECTED
#undef CTRIE_UNREACHABLE_IF_SINGLE_THREADED
#undef ctrie_update
#undef ctrie_update_branch
#undef ctrie_update_list
#undef ctrie_upsert
#undef ctrie_upsert_branch
#undef ctrie_upsert_list
#undef CTrieAtomic
#undef CTrieAtomic64
#undef CTrieBranchNode
#undef CTrieFailedNode
#undef CTrieGCASNode
#undef CTrieGCASNodeBase
#undef CTrieIndirectionNode
#undef CTrieListNode
#undef CTrieMainNode
#undef CTrieNodeBase
#undef CTrieProposalNode
#undef CTrieRootNode
#undef CTrieSingletonNode
#undef CTrieTombNode
#undef CTrieTransaction
#undef CTrieTwigNode
#undef ERTS_CTRIE_BRANCH_ALLOC_TYPE
#undef ERTS_CTRIE_BRANCH_FACTOR
#undef ERTS_CTRIE_FIX_ALLOC_BRANCH
#undef ERTS_CTRIE_FIX_ALLOC_NODE
#undef ERTS_CTRIE_HASH_EQ
#undef ERTS_CTRIE_HASH_GET
#undef ERTS_CTRIE_HASH_TYPE
#undef ERTS_CTRIE_INCLUDE_IMPLEMENTATION_ONLY
#undef ERTS_CTRIE_INCLUDE_TYPES_ONLY
#undef ERTS_CTRIE_KEY_EQ
#undef ERTS_CTRIE_KEY_GET
#undef ERTS_CTRIE_KEY_TYPE
#undef ERTS_CTRIE_NODE_ALLOC_TYPE
#undef ERTS_CTRIE_PREFIX
#undef ERTS_CTRIE_SINGLE_THREADED
#undef ERTS_CTRIE_SINGLETON_CLEANUP
#undef ERTS_CTRIE_SINGLETON_DESTRUCTOR
#undef ERTS_CTRIE_WANT_CLEAR
#undef ERTS_CTRIE_WANT_ERASE
#undef ERTS_CTRIE_WANT_INSERT
#undef ERTS_CTRIE_WANT_REMOVE
#undef ERTS_CTRIE_WANT_REPLACE
#undef ERTS_CTRIE_WANT_SNAPSHOTS
#undef ERTS_CTRIE_WANT_TAKE
#undef ERTS_CTRIE_WANT_UPDATE
#undef ERTS_CTRIE_WANT_UPSERT
