/**
 * @file prim_compare.c
 * @brief Comparison operations for numbers, characters, and strings
 */

#include "prim_internal.h"

unsigned numeric_compare(unsigned args, cmp_op op)
{
    if (!args)
        return ctx.atom_true;

    static const char *cmp_names[] = {"=", "<", ">", "<=", ">="};
    const char *name = (op <= CMP_GE) ? cmp_names[op] : "comparison";
    FORLIST(a, args)
    {
        if (!is_numeric(car(a))) {
            show_error("%s: not a number", name);
            return TOK_ERROR;
        }
    }

    // Fast path: all BT_NUM - no type checks in loop
    unsigned first = car(args);
    if (IS_NUM(first)) {
        int64_t prev = CELL_ID(first);
        for (unsigned a = cdr(args); a; a = cdr(a)) {
            unsigned c = car(a);
            if (!IS_NUM(c))
                goto slow_path;
            int64_t curr = CELL_ID(c);
            if (!APPLY_CMP_OP(op, prev, curr))
                return ctx.atom_false;
            prev = curr;
        }
        return ctx.atom_true;
    }

slow_path:;
    unsigned prev = car(args);
    for (unsigned a = cdr(args); a; a = cdr(a)) {
        unsigned curr = car(a);
        bool ok;

        // Use exact comparison for exact integers
        if (IS_EXACT_INT(prev) && IS_EXACT_INT(curr)) {
            int cmp = compare_exact_integers(prev, curr);
            ok = APPLY_CMP_OP(op, cmp, 0);
        } else {
            double f = to_double(prev);
            double v = to_double(curr);
            ok = APPLY_CMP_OP(op, f, v);
        }
        if (!ok)
            return ctx.atom_false;
        prev = curr;
    }
    return ctx.atom_true;
}

unsigned char_compare(unsigned args, cmp_op op, bool case_insensitive)
{
    REQUIRE_ARGS(args, 2, 2, "char comparison");
    CHECK_CHAR(car(args), "char comparison");
    CHECK_CHAR(cadr(args), "char comparison");
    int c1 = (unsigned char)CELL_ID(car(args));
    int c2 = (unsigned char)CELL_ID(cadr(args));
    if (case_insensitive) {
        c1 = tolower(c1);
        c2 = tolower(c2);
    }
    return APPLY_CMP_OP(op, c1, c2) ? ctx.atom_true : ctx.atom_false;
}

unsigned string_compare(unsigned args, cmp_op op, bool case_insensitive)
{
    REQUIRE_ARGS(args, 2, 2, "string comparison");
    CHECK_STRING(car(args), "string comparison");
    CHECK_STRING(cadr(args), "string comparison");
    char *s1 = GET_STRING_PTR(car(args));
    char *s2 = GET_STRING_PTR(cadr(args));
    int cmp = case_insensitive ? strcasecmp(s1, s2) : strcmp(s1, s2);
    return APPLY_CMP_OP(op, cmp, 0) ? ctx.atom_true : ctx.atom_false;
}
