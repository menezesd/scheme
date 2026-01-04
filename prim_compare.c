/**
 * @file prim_compare.c
 * @brief Comparison operations for numbers, characters, and strings
 */

#include "prim_internal.h"

unsigned numeric_compare(unsigned argc, unsigned *argv, cmp_op op)
{
    if (argc == 0)
        return ctx.atom_true;

    static const char *cmp_names[] = {"=", "<", ">", "<=", ">="};
    const char *name = (op <= CMP_GE) ? cmp_names[op] : "comparison";
    if (!check_numeric_argv(argc, argv, name))
        return TOK_ERROR;

    unsigned first = argv[0];
    if (IS_NUM(first)) {
        int64_t prev = CELL_ID(first);
        for (unsigned i = 1; i < argc; i++) {
            unsigned c = argv[i];
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
    unsigned prev = argv[0];
    for (unsigned i = 1; i < argc; i++) {
        unsigned curr = argv[i];
        bool ok;

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

unsigned char_compare(unsigned argc, unsigned *argv, cmp_op op,
                      bool case_insensitive)
{
    REQUIRE_ARGC(argc, 2, 2, "char comparison");
    CHECK_CHAR(argv[0], "char comparison");
    CHECK_CHAR(argv[1], "char comparison");
    int c1 = (unsigned char)CELL_ID(argv[0]);
    int c2 = (unsigned char)CELL_ID(argv[1]);
    if (case_insensitive) {
        c1 = tolower(c1);
        c2 = tolower(c2);
    }
    return APPLY_CMP_OP(op, c1, c2) ? ctx.atom_true : ctx.atom_false;
}

unsigned string_compare(unsigned argc, unsigned *argv, cmp_op op,
                        bool case_insensitive)
{
    REQUIRE_ARGC(argc, 2, 2, "string comparison");
    CHECK_STRING(argv[0], "string comparison");
    CHECK_STRING(argv[1], "string comparison");
    char *s1 = GET_STRING_PTR(argv[0]);
    char *s2 = GET_STRING_PTR(argv[1]);
    int cmp = case_insensitive ? strcasecmp(s1, s2) : strcmp(s1, s2);
    return APPLY_CMP_OP(op, cmp, 0) ? ctx.atom_true : ctx.atom_false;
}
