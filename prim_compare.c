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

    if (!compare_exact_integers(left, right, cmp_out)) {
        gc_unprotect(7);
        show_error("comparison: invalid exact integer");
        return false;
    }
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

    bool prev_exact = is_exact_real_number(prev);
    bool curr_exact = is_exact_real_number(curr);

    if (prev_exact && curr_exact) {
        int cmp;
        if (!compare_exact_reals(prev, curr, &cmp))
            return false;
        *ok_out = APPLY_CMP_OP(op, cmp, 0);
        return true;
    }

    // Mixed exact/inexact: convert the inexact side to an exact rational and
    // compare mathematically. Converting the exact side to double instead
    // would collapse values that differ beyond 53 bits of precision
    // (e.g. (= (+ (expt 2 53) 1) (exact->inexact (expt 2 53)))).
    if (prev_exact != curr_exact) {
        double d = to_double(prev_exact ? curr : prev);
        if (isnan(d)) {
            *ok_out = false; // every comparison with nan is false
            return true;
        }
        if (isinf(d)) {
            // An infinity is beyond every exact (finite) value
            int cmp = prev_exact ? (d > 0 ? -1 : 1) : (d > 0 ? 1 : -1);
            *ok_out = APPLY_CMP_OP(op, cmp, 0);
            return true;
        }
        GC_GUARD;
        gc_protect(&prev);
        gc_protect(&curr);
        unsigned converted = prim_inexact_to_exact(prev_exact ? curr : prev);
        if (converted == TOK_ERROR)
            return false;
        int cmp;
        bool cmp_ok = prev_exact
                          ? compare_exact_reals(prev, converted, &cmp)
                          : compare_exact_reals(converted, curr, &cmp);
        if (!cmp_ok)
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
    GC_GUARD;
    for (unsigned i = 0; i < argc; i++)
        gc_protect(&argv[i]);
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
    gc_protect(&prev);
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
    if (argc < 2) {
        show_error("char comparison: expected at least 2 arguments, got %u",
                   argc);
        return TOK_ERROR;
    }
    int prev;
    if (!expect_char_value(argv[0], &prev, "char comparison"))
        return TOK_ERROR;
    if (case_insensitive)
        prev = (int)unicode_simple_foldcase((uint32_t)prev);

    for (unsigned i = 1; i < argc; i++) {
        int curr;
        if (!expect_char_value(argv[i], &curr, "char comparison"))
            return TOK_ERROR;
        if (case_insensitive)
            curr = (int)unicode_simple_foldcase((uint32_t)curr);
        if (!APPLY_CMP_OP(op, prev, curr))
            return ctx.atom_false;
        prev = curr;
    }
    return ctx.atom_true;
}

unsigned string_compare(unsigned argc, unsigned *argv, cmp_op op,
                        bool case_insensitive)
{
    if (argc < 2) {
        show_error("string comparison: expected at least 2 arguments, got %u",
                   argc);
        return TOK_ERROR;
    }
    GC_GUARD;
    unsigned prev_cell = argv[0];
    gc_protect(&prev_cell);
    if (case_insensitive) {
        prev_cell = prim_string_normalize(PSTRFOLD, 1, &prev_cell);
        if (prev_cell == TOK_ERROR)
            return TOK_ERROR;
    }
    char *prev = require_string_ptr(prev_cell, "string comparison");
    if (!prev)
        return TOK_ERROR;

    for (unsigned i = 1; i < argc; i++) {
        unsigned curr_cell = argv[i];
        gc_protect(&curr_cell);
        if (case_insensitive) {
            curr_cell = prim_string_normalize(PSTRFOLD, 1, &curr_cell);
            if (curr_cell == TOK_ERROR)
                return TOK_ERROR;
        }
        char *curr = require_string_ptr(curr_cell, "string comparison");
        if (!curr)
            return TOK_ERROR;
        int cmp = strcmp(prev, curr);
        if (!APPLY_CMP_OP(op, cmp, 0))
            return ctx.atom_false;
        prev_cell = curr_cell;
        prev = curr;
        gc_unprotect(1);
    }
    return ctx.atom_true;
}
