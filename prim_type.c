/**
 * @file prim_type.c
 * @brief Type predicate operations (number?, symbol?, pair?, etc.)
 */

#include "bytecode.h"
#include "prim_internal.h"

unsigned apply_type_predicate(unsigned prim_id, unsigned argc,
                              unsigned *argv)
{
    REQUIRE_ARGC(argc, 1, 1, "type predicate");
    unsigned arg = argv[0];
    switch (prim_id) {
    case PSYMP:
        return IS_ATOM(arg) ? ctx.atom_true : ctx.atom_false;
    case PNUMP:
    case PNUMBERP:
        return is_numeric(arg) ? ctx.atom_true : ctx.atom_false;
    case PINTEGERP: {
        if (IS_NUM(arg) || IS_BIGNUM(arg))
            return ctx.atom_true;
        if (IS_INEXACT(arg)) {
            double d = to_double(arg);
            return (isfinite(d) && floor(d) == d) ? ctx.atom_true
                                                  : ctx.atom_false;
        }
        return ctx.atom_false;
    }
    case PREALP: {
        if (is_numeric(arg) && !IS_COMPLEX(arg))
            return ctx.atom_true;
        return ctx.atom_false;
    }
    case PEXACTP:
        return is_exact(arg) ? ctx.atom_true : ctx.atom_false;
    case PINEXACTP:
        return (is_numeric(arg) && !is_exact(arg)) ? ctx.atom_true
                                                   : ctx.atom_false;
    case PCOMPLEXP:
        return is_numeric(arg) ? ctx.atom_true : ctx.atom_false;
    case PRATIONALP:
        return (IS_NUM(arg) || IS_BIGNUM(arg) || IS_RATIONAL(arg))
                   ? ctx.atom_true
                   : ctx.atom_false;
    case PPROCP:
        // Check for bytecode closures: cons cell with BT_CLOSURE marker in car
        if (IS_PAIR(arg) && CELL_TYPE(car(arg)) == BT_CLOSURE)
            return ctx.atom_true;
        return (IS_FUNCTION(arg) || IS_BUILTIN(arg) || IS_CONT(arg))
                   ? ctx.atom_true
                   : ctx.atom_false;
    case PCONSP:
        return IS_PAIR(arg) ? ctx.atom_true : ctx.atom_false;
    case PNULLP:
        return IS_NIL(arg) ? ctx.atom_true : ctx.atom_false;
    case PSTRINGP:
        return IS_STRING(arg) ? ctx.atom_true : ctx.atom_false;
    case PCHARP:
        return IS_CHAR(arg) ? ctx.atom_true : ctx.atom_false;
    case PVECTORP:
        return IS_VECTOR(arg) ? ctx.atom_true : ctx.atom_false;
    case PBOOLP:
        return (IS_FALSE(arg) || arg == ctx.atom_true) ? ctx.atom_true
                                                       : ctx.atom_false;
    case PLISTP: {
        // Use Floyd's cycle detection (tortoise and hare) to handle cycles
        unsigned slow = arg;
        unsigned fast = arg;
        while (IS_PAIR(fast)) {
            fast = cdr(fast);
            if (!IS_PAIR(fast))
                break;
            fast = cdr(fast);
            slow = cdr(slow);
            // Cycle detected - not a proper list
            if (slow == fast)
                return ctx.atom_false;
        }
        return IS_NIL(fast) ? ctx.atom_true : ctx.atom_false;
    }
    default:
        return TOK_ERROR;
    }
}
