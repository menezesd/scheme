/**
 * @file prim_type.c
 * @brief Type predicate operations (number?, symbol?, pair?, etc.)
 */

#include "bytecode.h"
#include "prim_internal.h"

unsigned apply_type_predicate(unsigned prim_id, unsigned args)
{
    REQUIRE_ARGS(args, 1, 1, "type predicate");
    unsigned arg = car(args);
    switch (prim_id) {
    case PSYMP:
        return IS_ATOM(arg) ? ctx.atom_true : 0;
    case PNUMP:
    case PNUMBERP:
        return is_numeric(arg) ? ctx.atom_true : 0;
    case PINTEGERP: {
        if (IS_NUM(arg) || IS_BIGNUM(arg))
            return ctx.atom_true;
        if (IS_INEXACT(arg)) {
            double d = to_double(arg);
            return (floor(d) == d) ? ctx.atom_true : 0;
        }
        return 0;
    }
    case PREALP: {
        if (is_numeric(arg) && !IS_COMPLEX(arg))
            return ctx.atom_true;
        return 0;
    }
    case PEXACTP:
        return is_exact(arg) ? ctx.atom_true : 0;
    case PINEXACTP:
        return (is_numeric(arg) && !is_exact(arg)) ? ctx.atom_true : 0;
    case PCOMPLEXP:
        return is_numeric(arg) ? ctx.atom_true : 0;
    case PRATIONALP:
        return (IS_NUM(arg) || IS_BIGNUM(arg) || IS_RATIONAL(arg))
                   ? ctx.atom_true
                   : 0;
    case PPROCP:
        // Check for bytecode closures: cons cell with BT_CLOSURE marker in car
        if (IS_PAIR(arg) && CELL_TYPE(car(arg)) == BT_CLOSURE)
            return ctx.atom_true;
        return (IS_FUNCTION(arg) || IS_BUILTIN(arg) || IS_CONT(arg))
                   ? ctx.atom_true
                   : 0;
    case PCONSP:
        return IS_PAIR(arg) ? ctx.atom_true : 0;
    case PNULLP:
        return arg == 0 ? ctx.atom_true : 0;
    case PSTRINGP:
        return IS_STRING(arg) ? ctx.atom_true : 0;
    case PCHARP:
        return IS_CHAR(arg) ? ctx.atom_true : 0;
    case PVECTORP:
        return IS_VECTOR(arg) ? ctx.atom_true : 0;
    case PBOOLP:
        return (arg == 0 || arg == ctx.atom_true) ? ctx.atom_true : 0;
    case PLISTP: {
        unsigned x = arg;
        while (IS_PAIR(x))
            x = cdr(x);
        return x == 0 ? ctx.atom_true : 0;
    }
    default:
        return TOK_ERROR;
    }
}
