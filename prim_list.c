/**
 * @file prim_list.c
 * @brief List operations (append, reverse, cxr, member/assoc, list-tail)
 *
 * Besides append and reverse below, this file holds the car/cdr compositions
 * (caar ... cddddr), memq/memv/member, assq/assv/assoc and list-tail/list-ref.
 * Those were Scheme procedures in stdlib.scm, and on the hot paths of most
 * list code a full procedure call (or a Scheme loop per element) cost far
 * more than the work itself. None of them allocates.
 *
 * Implements standard Scheme list operations:
 *
 * ## append
 * (append list1 list2 ... listN) -> concatenated list
 * - Copies the spine of all lists except the last
 * - Last argument can be any object (result may be improper list)
 * - Shares structure with the last argument (no copy)
 *
 * ## reverse
 * (reverse list) -> reversed copy
 * - Creates a new list with elements in reverse order
 * - Does not modify the original list
 *
 * Both operations are GC-safe: all local variables are protected
 * across allocations using gc_protect/gc_unprotect.
 */

#include "prim_internal.h"

unsigned prim_append(unsigned argc, unsigned *argv)
{
    if (argc == 0)
        return 0;
    GC_GUARD;
    // Any list argument may be moved while the copied spines are allocated.
    // Root the complete argument vector before the first allocation so both
    // the current and not-yet-processed lists remain valid.
    for (unsigned i = 0; i < argc; i++)
        gc_protect(&argv[i]);
    unsigned result = 0, tail = 0;
    gc_protect(&result);
    gc_protect(&tail);

    for (unsigned i = 0; i + 1 < argc; i++) {
        // list_length_checked rejects improper and circular lists, so the
        // walk below always ends at the empty list
        unsigned lst = argv[i];
        if (!list_length_checked(lst, NULL, "append"))
            return TOK_ERROR;
        gc_protect(&lst);
        while (IS_PAIR(lst)) {
            list_append(&result, &tail, car(lst));
            lst = cdr(lst);
        }
        gc_unprotect(1);
    }

    unsigned last = argv[argc - 1];
    if (tail) {
        cell_set_cdr(tail, last);
    }
    return result ? result : last;
}

unsigned prim_reverse(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 1, 1, "reverse");
    GC_GUARD;
    unsigned lst = argv[0];
    if (!list_length_checked(lst, NULL, "reverse"))
        return TOK_ERROR;
    unsigned result = 0;
    gc_protect(&lst);
    gc_protect(&result);
    for (; IS_PAIR(lst); lst = cdr(lst))
        result = alloc_cons(car(lst), result);
    return result;
}

// ============================================================================
// c[ad]{2,4}r
// ============================================================================

static const char *const cxr_names[] = {
    "caar",   "cadr",   "cdar",   "cddr",   "caaar",  "caadr",  "cadar",
    "caddr",  "cdaar",  "cdadr",  "cddar",  "cdddr",  "caaaar", "caaadr",
    "caadar", "caaddr", "cadaar", "cadadr", "caddar", "cadddr", "cdaaar",
    "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr"};

_Static_assert(sizeof(cxr_names) / sizeof(cxr_names[0]) ==
                   PCDDDDR - PCAAR + 1,
               "cxr_names must list every cxr primitive in enum order");

// Within a level, a primitive's offset spells its name in binary, a = 0 and
// d = 1, first letter most significant. The accessors apply right to left, so
// the least significant bit is the first one taken. Like car and cdr, every
// failure is reported under the procedure's own name.
unsigned prim_cxr(unsigned prim_id, unsigned argc, unsigned *argv)
{
    const char *name = cxr_names[prim_id - PCAAR];
    REQUIRE_ARGC(argc, 1, 1, name);
    unsigned level, path;
    if (prim_id <= PCDDR) {
        level = 2;
        path = prim_id - PCAAR;
    } else if (prim_id <= PCDDDR) {
        level = 3;
        path = prim_id - PCAAAR;
    } else {
        level = 4;
        path = prim_id - PCAAAAR;
    }
    unsigned x = argv[0];
    for (unsigned i = 0; i < level; i++, path >>= 1) {
        if (!IS_PAIR(x)) {
            show_error("%s: not a pair", name);
            return TOK_ERROR;
        }
        x = (path & 1) ? cdr(x) : car(x);
    }
    return x;
}

// ============================================================================
// memq, memv, member, assq, assv, assoc
// ============================================================================

// equal? where the first level decides it, else deep_equal. deep_equal makes
// the same first-level decisions, but mallocs a worklist on every call.
static inline bool values_equal(unsigned a, unsigned b)
{
    if (a == b)
        return true;
    if (a == 0 || b == 0)
        return false;
    if (IS_FIXNUM(a) || IS_FIXNUM(b))
        return deep_equal(a, b);
    if (!IS_CELL(a) || !IS_CELL(b) || CELL_TYPE(a) != CELL_TYPE(b))
        return false;
    switch (CELL_TYPE(a)) {
    case BT_NUM:
    case BT_CHAR:
    case BT_ATOM:
    case BT_INEXACT:
        return CELL_ID(a) == CELL_ID(b);
    default:
        return deep_equal(a, b);
    }
}

typedef enum { MATCH_IDENTITY, MATCH_EQ, MATCH_EQV, MATCH_EQUAL } match_kind;

static inline bool is_eqv_number(unsigned x)
{
    if (!IS_CELL(x))
        return false;
    switch (CELL_TYPE(x)) {
    case BT_BIGNUM:
    case BT_INEXACT:
    case BT_RATIONAL:
    case BT_COMPLEX:
        return true;
    default:
        return false;
    }
}

// The cheapest comparison that agrees with the requested one for this key.
// Keys are mostly symbols, characters and small numbers, for which eqv? and
// equal? are eq?, and anything else that eq? compares by identity alone.
static match_kind effective_match(match_kind kind, unsigned key)
{
    if (kind == MATCH_EQUAL && !IS_FIXNUM(key) && IS_CELL(key)) {
        switch (CELL_TYPE(key)) {
        case BT_NUM:
        case BT_CHAR:
        case BT_ATOM:
        case BT_FUNCTION:
        case BT_BUILTIN:
            kind = MATCH_EQ;
            break;
        case BT_BIGNUM:
        case BT_INEXACT:
        case BT_RATIONAL:
        case BT_COMPLEX:
            kind = MATCH_EQV;
            break;
        default:
            break;
        }
    }
    if (kind == MATCH_EQV && !is_eqv_number(key))
        kind = MATCH_EQ;
    if (kind == MATCH_EQ && !eq_compares_payload(key))
        kind = MATCH_IDENTITY;
    return kind;
}

static inline bool keys_match(match_kind kind, unsigned key, unsigned x)
{
    switch (kind) {
    case MATCH_IDENTITY:
        return x == key;
    case MATCH_EQ:
        return values_eq(key, x);
    case MATCH_EQV:
        return values_eqv(key, x);
    default:
        return values_equal(key, x);
    }
}

// One walk, testing each element before moving on. A cycle is detected with
// Brent's algorithm: the walk keeps a checkpoint that it moves to its current
// position whenever the steps taken since the last move reach the next power
// of two, and only a circular list comes back to it. By then the walk has
// been once round the cycle, so every element has been tested and a key that
// is present is found whatever the list's shape. Never allocates.
static unsigned list_search(unsigned key, unsigned lst, match_kind kind,
                            bool assoc, const char *name)
{
    kind = effective_match(kind, key);
    unsigned checkpoint = lst;
    unsigned steps = 0, limit = 1;
    while (IS_PAIR(lst)) {
        unsigned x = car(lst);
        if (assoc) {
            if (!IS_PAIR(x)) {
                show_error("%s: improper association list", name);
                return TOK_ERROR;
            }
            if (keys_match(kind, key, car(x)))
                return x;
        } else if (keys_match(kind, key, x)) {
            return lst;
        }
        lst = cdr(lst);
        if (lst == checkpoint) {
            show_error("%s: circular list", name);
            return TOK_ERROR;
        }
        if (++steps == limit) {
            checkpoint = lst;
            limit <<= 1;
            steps = 0;
        }
    }
    if (lst) {
        if (assoc)
            show_error("%s: improper association list", name);
        else
            show_error("%s: improper list", name);
        return TOK_ERROR;
    }
    return ctx.atom_false;
}

unsigned prim_member_search(unsigned prim_id, unsigned argc, unsigned *argv)
{
    static const struct {
        const char *name;
        match_kind kind;
        bool assoc;
    } searches[] = {
        [PMEMQ - PMEMQ] = {"memq", MATCH_EQ, false},
        [PMEMV - PMEMQ] = {"memv", MATCH_EQV, false},
        [PMEMBER - PMEMQ] = {"member", MATCH_EQUAL, false},
        [PASSQ - PMEMQ] = {"assq", MATCH_EQ, true},
        [PASSV - PMEMQ] = {"assv", MATCH_EQV, true},
        [PASSOC - PMEMQ] = {"assoc", MATCH_EQUAL, true},
    };
    unsigned which = prim_id - PMEMQ;
    REQUIRE_ARGC(argc, 2, 2, searches[which].name);
    return list_search(argv[0], argv[1], searches[which].kind,
                       searches[which].assoc, searches[which].name);
}

// ============================================================================
// list-tail, list-ref
// ============================================================================

// k must be an exact nonnegative integer. *big is set to a k too large for
// int64 (and *small to INT64_MAX): past the end of any list the heap can
// hold, unless the list is circular.
static bool list_index_arg(unsigned k, int64_t *small, const bignum **big,
                           const char *name)
{
    bool valid = false;
    *big = NULL;
    if (IS_FIXNUM(k)) {
        *small = FIXNUM_VALUE(k);
        valid = *small >= 0;
    } else if (IS_NUM(k)) {
        *small = CELL_ID(k);
        valid = *small >= 0;
    } else if (IS_BIGNUM(k) && get_bignum(k)) {
        const bignum *bn = get_bignum(k);
        if (bn_to_int64(bn, small) == 0) {
            valid = *small >= 0;
        } else if (bn_sign(bn) > 0) {
            *big = bn;
            *small = INT64_MAX;
            valid = true;
        }
    }
    if (!valid)
        show_error("%s: expected nonnegative integer", name);
    return valid;
}

// The tail of lst after k cdrs, each of which must be taken of a pair. A walk
// that comes back to its checkpoint (as in list_search) has just gone once
// round a cycle of `steps` pairs, and the rest of the walk only goes round
// it, so k is reduced modulo the cycle's length. That also gives a bignum
// index into a circular list its value.
static unsigned list_tail_cell(unsigned lst, int64_t k, const bignum *big,
                               const char *name)
{
    unsigned checkpoint = lst;
    unsigned steps = 0, limit = 1;
    int64_t taken = 0;
    while (big || taken < k) {
        if (!IS_PAIR(lst)) {
            show_error("%s: index out of bounds", name);
            return TOK_ERROR;
        }
        lst = cdr(lst);
        taken++;
        steps++;
        if (lst == checkpoint) {
            int64_t remaining = 0;
            if (big) {
                bignum *cycle = bn_from_int((int64_t)steps);
                bignum *done = bn_from_int(taken);
                bignum *left = cycle && done ? bn_sub(big, done) : NULL;
                bignum *rem = left ? bn_mod(left, cycle) : NULL;
                bool ok = rem && bn_to_int64(rem, &remaining) == 0;
                bn_free(cycle);
                bn_free(done);
                bn_free(left);
                bn_free(rem);
                if (!ok) {
                    show_error("%s: out of memory", name);
                    return TOK_ERROR;
                }
            } else {
                remaining = (k - taken) % (int64_t)steps;
            }
            while (remaining-- > 0)
                lst = cdr(lst);
            return lst;
        }
        if (steps == limit) {
            checkpoint = lst;
            limit <<= 1;
            steps = 0;
        }
    }
    return lst;
}

unsigned prim_list_tail(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 2, 2, "list-tail");
    int64_t k;
    const bignum *big;
    if (!list_index_arg(argv[1], &k, &big, "list-tail"))
        return TOK_ERROR;
    return list_tail_cell(argv[0], k, big, "list-tail");
}

unsigned prim_list_ref(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 2, 2, "list-ref");
    int64_t k;
    const bignum *big;
    if (!list_index_arg(argv[1], &k, &big, "list-ref"))
        return TOK_ERROR;
    unsigned tail = list_tail_cell(argv[0], k, big, "list-ref");
    if (tail == TOK_ERROR)
        return TOK_ERROR;
    if (!IS_PAIR(tail)) {
        show_error("list-ref: index out of bounds");
        return TOK_ERROR;
    }
    return car(tail);
}
