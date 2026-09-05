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
    // Non-NULL for the exactness predicates, which R7RS 6.2.6 defines only
    // over numbers: they answer a question about a number rather than
    // testing membership in a type, so a non-number argument is a type
    // error, not a #f answer. The name is the one reported in that error.
    const char *number_only;
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
    {PSYMP, symbol_predicate, NULL},
    {PNUMP, numeric_predicate, NULL},
    {PINTEGERP, integer_predicate, NULL},
    {PREALP, real_predicate, NULL},
    {PEXACTP, exact_predicate, "exact?"},
    {PINEXACTP, inexact_predicate, "inexact?"},
    {PCOMPLEXP, numeric_predicate, NULL},
    {PRATIONALP, rational_predicate, NULL},
    {PPROCP, procedure_predicate, NULL},
    {PCONSP, pair_predicate, NULL},
    {PNULLP, null_predicate, NULL},
    {PSTRINGP, string_predicate, NULL},
    {PCHARP, char_predicate, NULL},
    {PVECTORP, vector_predicate, NULL},
    {PBOOLP, boolean_predicate, NULL},
    {PLISTP, proper_list_predicate, NULL},
    {0, NULL, NULL},
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
    if (!entry) {
        show_error("type predicate: unknown primitive %u", prim_id);
        return TOK_ERROR;
    }
    if (entry->number_only && !is_numeric(argv[0])) {
        show_error("%s: not a number", entry->number_only);
        return TOK_ERROR;
    }
    return scheme_bool(entry->predicate(argv[0]));
}
