/**
 * @file prim_type.c
 * @brief Type predicate operations (number?, symbol?, pair?, etc.)
 */

#include "bytecode.h"
#include "prim_internal.h"

static bool integer_predicate(unsigned arg)
{
    if (IS_FIXNUM(arg))
        return true;
    if (!is_numeric(arg))
        return false;
    if (IS_NUM(arg) || IS_BIGNUM(arg))
        return true;
    if (IS_INEXACT(arg)) {
        double d = to_double(arg);
        return isfinite(d) && floor(d) == d;
    }
    return false;
}

static bool real_predicate(unsigned arg)
{
    return is_numeric(arg) && !IS_COMPLEX(arg);
}

static bool procedure_predicate(unsigned arg)
{
    // Check for bytecode closures: cons cell with BT_CLOSURE marker in car.
    if (is_bytecode_closure_object(arg))
        return true;
    return IS_FUNCTION(arg) || IS_BUILTIN(arg) || IS_CONT(arg) ||
           is_vm_continuation_object(arg);
}

static bool proper_list_predicate(unsigned arg)
{
    // Floyd's cycle detection handles circular lists without allocating.
    unsigned slow = arg;
    unsigned fast = arg;
    while (IS_PAIR(fast)) {
        fast = cdr(fast);
        if (!IS_PAIR(fast))
            break;
        fast = cdr(fast);
        slow = cdr(slow);
        if (slow == fast)
            return false;
    }
    return IS_NIL(fast);
}

typedef struct {
    unsigned id;
    bool (*predicate)(unsigned arg);
} type_predicate_entry;

static bool symbol_predicate(unsigned arg) { return IS_ATOM(arg); }
static bool numeric_predicate(unsigned arg) { return is_numeric(arg); }
static bool exact_predicate(unsigned arg)
{
    return is_numeric(arg) && is_exact(arg);
}
static bool inexact_predicate(unsigned arg)
{
    return is_numeric(arg) && !is_exact(arg);
}
static bool rational_predicate(unsigned arg)
{
    if (!is_numeric(arg))
        return false;
    if (IS_FIXNUM(arg) || IS_NUM(arg) || IS_BIGNUM(arg) || IS_RATIONAL(arg))
        return true;
    if (IS_INEXACT(arg))
        return isfinite(to_double(arg));
    return false;
}
static bool pair_predicate(unsigned arg) { return IS_PAIR(arg); }
static bool null_predicate(unsigned arg) { return IS_NIL(arg); }
static bool string_predicate(unsigned arg) { return IS_STRING(arg); }
static bool char_predicate(unsigned arg) { return IS_CHAR(arg); }
static bool vector_predicate(unsigned arg) { return IS_VECTOR(arg); }
static bool boolean_predicate(unsigned arg)
{
    return IS_FALSE(arg) || arg == ctx.atom_true;
}

static const type_predicate_entry type_predicates[] = {
    {PSYMP, symbol_predicate},
    {PNUMP, numeric_predicate},
    {PINTEGERP, integer_predicate},
    {PREALP, real_predicate},
    {PEXACTP, exact_predicate},
    {PINEXACTP, inexact_predicate},
    {PCOMPLEXP, numeric_predicate},
    {PRATIONALP, rational_predicate},
    {PPROCP, procedure_predicate},
    {PCONSP, pair_predicate},
    {PNULLP, null_predicate},
    {PSTRINGP, string_predicate},
    {PCHARP, char_predicate},
    {PVECTORP, vector_predicate},
    {PBOOLP, boolean_predicate},
    {PLISTP, proper_list_predicate},
    {0, NULL},
};

static const type_predicate_entry *find_type_predicate(unsigned prim_id)
{
    for (const type_predicate_entry *entry = type_predicates; entry->predicate;
         entry++) {
        if (entry->id == prim_id)
            return entry;
    }
    return NULL;
}

unsigned apply_type_predicate(unsigned prim_id, unsigned argc,
                              unsigned *argv)
{
    REQUIRE_ARGC(argc, 1, 1, "type predicate");
    const type_predicate_entry *entry = find_type_predicate(prim_id);
    return entry ? scheme_bool(entry->predicate(argv[0])) : TOK_ERROR;
}
