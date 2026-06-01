/**
 * @file prim_compare.c
 * @brief Comparison operations for numbers, characters, and strings
 */

#include "prim_internal.h"

static bool is_exact_real_number(unsigned x)
{
    return IS_EXACT_INT(x) || IS_RATIONAL(x);
}

static bool compare_exact_reals(unsigned a, unsigned b, int *cmp_out)
{
    GC_GUARD;
    gc_protect(&a);
    gc_protect(&b);

    unsigned an, ad, bn, bd;
    get_rational_cells(a, &an, &ad);
    gc_protect(&an);
    gc_protect(&ad);
    get_rational_cells(b, &bn, &bd);
    gc_protect(&bn);
    gc_protect(&bd);

    unsigned left = multiply_cells(an, bd);
    if (left == TOK_ERROR) {
        gc_unprotect(6);
        return false;
    }
    gc_protect(&left);

    unsigned right = multiply_cells(bn, ad);
    if (right == TOK_ERROR) {
        gc_unprotect(7);
        return false;
    }

    *cmp_out = compare_exact_integers(left, right);
    gc_unprotect(7);
    return true;
}

static unsigned complex_numeq(unsigned a, unsigned b)
{
    GC_GUARD;
    gc_protect(&a);
    gc_protect(&b);

    unsigned ar, ai, br, bi;
    get_complex_cells(a, &ar, &ai);
    gc_protect(&ar);
    gc_protect(&ai);
    get_complex_cells(b, &br, &bi);
    gc_protect(&br);
    gc_protect(&bi);

    unsigned real_args[2] = {ar, br};
    gc_protect(&real_args[0]);
    gc_protect(&real_args[1]);
    unsigned real_eq = numeric_compare(2, real_args, CMP_EQ);
    gc_unprotect(2);
    if (real_eq == TOK_ERROR || real_eq == ctx.atom_false) {
        gc_unprotect(6);
        return real_eq;
    }

    unsigned imag_args[2] = {ai, bi};
    gc_protect(&imag_args[0]);
    gc_protect(&imag_args[1]);
    unsigned imag_eq = numeric_compare(2, imag_args, CMP_EQ);
    gc_unprotect(8);
    return imag_eq;
}

static bool compare_number_pair(unsigned prev, unsigned curr, cmp_op op,
                                const char *name, bool *ok_out)
{
    if (IS_COMPLEX(prev) || IS_COMPLEX(curr)) {
        if (op != CMP_EQ) {
            show_error("%s: expected real numbers", name);
            return false;
        }
        unsigned eq = complex_numeq(prev, curr);
        if (eq == TOK_ERROR)
            return false;
        *ok_out = eq == ctx.atom_true;
        return true;
    }

    if (is_exact_real_number(prev) && is_exact_real_number(curr)) {
        int cmp;
        if (!compare_exact_reals(prev, curr, &cmp))
            return false;
        *ok_out = APPLY_CMP_OP(op, cmp, 0);
        return true;
    }

    double f = to_double(prev);
    double v = to_double(curr);
    *ok_out = APPLY_CMP_OP(op, f, v);
    return true;
}

unsigned numeric_compare(unsigned argc, unsigned *argv, cmp_op op)
{
    if (argc == 0)
        return ctx.atom_true;

    static const char *cmp_names[] = {"=", "<", ">", "<=", ">="};
    const char *name = (op <= CMP_GE) ? cmp_names[op] : "comparison";
    if (!check_numeric_argv(argc, argv, name))
        return TOK_ERROR;

    unsigned first = argv[0];
    int64_t prev_int;
    if (exact_int64_value(first, &prev_int)) {
        for (unsigned i = 1; i < argc; i++) {
            unsigned c = argv[i];
            int64_t curr;
            if (!exact_int64_value(c, &curr))
                goto slow_path;
            if (!APPLY_CMP_OP(op, prev_int, curr))
                return ctx.atom_false;
            prev_int = curr;
        }
        return ctx.atom_true;
    }

slow_path:;
    unsigned prev = argv[0];
    for (unsigned i = 1; i < argc; i++) {
        unsigned curr = argv[i];
        bool ok;
        if (!compare_number_pair(prev, curr, op, name, &ok))
            return TOK_ERROR;
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
    int c1, c2;
    if (!expect_char_value(argv[0], &c1, "char comparison") ||
        !expect_char_value(argv[1], &c2, "char comparison"))
        return TOK_ERROR;
    if (case_insensitive) {
        c1 = (int)unicode_simple_foldcase((uint32_t)c1);
        c2 = (int)unicode_simple_foldcase((uint32_t)c2);
    }
    return scheme_bool(APPLY_CMP_OP(op, c1, c2));
}

unsigned string_compare(unsigned argc, unsigned *argv, cmp_op op,
                        bool case_insensitive)
{
    REQUIRE_ARGC(argc, 2, 2, "string comparison");
    char *s1 = require_string_ptr(argv[0], "string comparison");
    char *s2 = require_string_ptr(argv[1], "string comparison");
    if (!s1 || !s2)
        return TOK_ERROR;
    int cmp = case_insensitive ? strcasecmp(s1, s2) : strcmp(s1, s2);
    return scheme_bool(APPLY_CMP_OP(op, cmp, 0));
}
