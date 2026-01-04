/**
 * @file prim_numeric.c
 * @brief Numeric arithmetic operations (+, -, *, /, mod, quotient, remainder,
 * abs)
 *
 * Implements the full numeric tower for arithmetic:
 * - Fast path: Pure int64_t arithmetic with overflow detection
 * - Bignum: Arbitrary precision for integer overflow
 * - Rational: Exact fractions when division doesn't produce integers
 * - Inexact: IEEE 754 doubles for transcendental operations
 * - Complex: Real + imaginary parts (can be any of the above)
 *
 * ## Exactness Propagation
 * Results are exact if all operands are exact, except for operations that
 * inherently produce inexact results (sqrt of non-perfect-square, etc.).
 *
 * ## Overflow Handling
 * Uses __builtin_*_overflow intrinsics for fast overflow detection,
 * promoting to bignum when int64_t overflows.
 */

#include "prim_internal.h"

unsigned prim_plus(unsigned argc, unsigned *argv)
{
    if (argc == 0)
        return store(0);

    // Fast path: try pure integer arithmetic with builtin overflow detection
    int64_t v = 0;
    for (unsigned i = 0; i < argc; i++) {
        unsigned x = argv[i];
        if (!IS_NUM(x))
            goto slow_path;
        if (__builtin_add_overflow(v, CELL_ID(x), &v))
            goto slow_path;
    }
    return store(v);

slow_path:;
    if (!check_numeric_argv(argc, argv, "+"))
        return TOK_ERROR;
    bool exact;
    numeric_level level = classify_args_argv(argc, argv, &exact);

    switch (level) {
    case NUM_COMPLEX: {
        bool all_complex_exact = true;
        for (unsigned i = 0; i < argc; i++) {
            if (!is_complex_exact(argv[i])) {
                all_complex_exact = false;
                break;
            }
        }

        if (all_complex_exact) {
            GC_GUARD;
            unsigned real_sum = store(0), imag_sum = store(0);
            gc_protect(&real_sum);
            gc_protect(&imag_sum);
            for (unsigned i = 0; i < argc; i++) {
                unsigned r, im;
                get_complex_cells(argv[i], &r, &im);
                real_sum = binary_add(real_sum, r);
                imag_sum = binary_add(imag_sum, im);
            }
            return make_complex_exact(real_sum, imag_sum);
        }

        double real = 0.0, imag = 0.0;
        for (unsigned i = 0; i < argc; i++) {
            double r, im;
            get_complex_parts(argv[i], &r, &im);
            real += r;
            imag += im;
        }
        return make_complex_result(real, imag, exact);
    }
    case NUM_INEXACT: {
        double sum = 0.0;
        for (unsigned i = 0; i < argc; i++)
            sum += to_double(argv[i]);
        return store_inexact(sum);
    }
    case NUM_RATIONAL: {
        GC_GUARD;
        unsigned num = store(0), denom = store(1);
        gc_protect(&num);
        gc_protect(&denom);
        for (unsigned i = 0; i < argc; i++) {
            unsigned n, d;
            get_rational_cells(argv[i], &n, &d);
            unsigned ad = multiply_cells(num, d);
            gc_protect(&ad);
            unsigned bc = multiply_cells(n, denom);
            num = add_cells(ad, bc);
            gc_unprotect(1);
            denom = multiply_cells(denom, d);
        }
        return normalize_rational_cells(num, denom);
    }
    case NUM_BIGNUM: {
        bignum *result = bn_from_int(0);
        for (unsigned i = 0; i < argc; i++) {
            bignum *operand = to_bignum(argv[i]);
            bn_add_ip(result, operand);
            bn_free(operand);
        }
        return store_integer(result);
    }
    case NUM_INTEGER: {
        int64_t sum = 0;
        for (unsigned i = 0; i < argc; i++) {
            int64_t x = CELL_ID(argv[i]);
            int64_t new_sum;
            if (__builtin_add_overflow(sum, x, &new_sum)) {
                bignum *result = bn_from_int(sum);
                bignum *operand = bn_from_int(x);
                bn_add_ip(result, operand);
                bn_free(operand);
                for (unsigned j = i + 1; j < argc; j++) {
                    operand = to_bignum(argv[j]);
                    bn_add_ip(result, operand);
                    bn_free(operand);
                }
                return store_integer(result);
            }
            sum = new_sum;
        }
        return store(sum);
    }
    }
    return store(0);
}

unsigned prim_mult(unsigned argc, unsigned *argv)
{
    if (argc == 0)
        return store(1);

    int64_t v = 1;
    for (unsigned i = 0; i < argc; i++) {
        unsigned x = argv[i];
        if (!IS_NUM(x))
            goto slow_path;
        if (__builtin_mul_overflow(v, CELL_ID(x), &v))
            goto slow_path;
    }
    return store(v);

slow_path:;
    if (!check_numeric_argv(argc, argv, "*"))
        return TOK_ERROR;
    bool exact;
    numeric_level level = classify_args_argv(argc, argv, &exact);

    switch (level) {
    case NUM_COMPLEX: {
        bool all_complex_exact = true;
        for (unsigned i = 0; i < argc; i++) {
            if (!is_complex_exact(argv[i])) {
                all_complex_exact = false;
                break;
            }
        }

        if (all_complex_exact) {
            GC_GUARD;
            unsigned real_prod = store(1), imag_prod = store(0);
            gc_protect(&real_prod);
            gc_protect(&imag_prod);
            for (unsigned i = 0; i < argc; i++) {
                unsigned r, im;
                get_complex_cells(argv[i], &r, &im);
                unsigned ac = binary_mul(real_prod, r);
                gc_protect(&ac);
                unsigned bd = binary_mul(imag_prod, im);
                gc_protect(&bd);
                unsigned ad = binary_mul(real_prod, im);
                gc_protect(&ad);
                unsigned bc = binary_mul(imag_prod, r);
                unsigned new_real = binary_sub(ac, bd);
                unsigned new_imag = binary_add(ad, bc);
                gc_unprotect(3);
                real_prod = new_real;
                imag_prod = new_imag;
            }
            return make_complex_exact(real_prod, imag_prod);
        }

        double real = 1.0, imag = 0.0;
        for (unsigned i = 0; i < argc; i++) {
            double r, im;
            get_complex_parts(argv[i], &r, &im);
            double nr = real * r - imag * im;
            double ni = real * im + imag * r;
            real = nr;
            imag = ni;
        }
        return make_complex_result(real, imag, exact);
    }
    case NUM_INEXACT: {
        double prod = 1.0;
        for (unsigned i = 0; i < argc; i++)
            prod *= to_double(argv[i]);
        return store_inexact(prod);
    }
    case NUM_RATIONAL: {
        GC_GUARD;
        unsigned num = store(1), denom = store(1);
        gc_protect(&num);
        gc_protect(&denom);
        for (unsigned i = 0; i < argc; i++) {
            unsigned n, d;
            get_rational_cells(argv[i], &n, &d);
            num = multiply_cells(num, n);
            denom = multiply_cells(denom, d);
        }
        return normalize_rational_cells(num, denom);
    }
    case NUM_BIGNUM:
    case NUM_INTEGER: {
        bignum *result = bn_from_int(1);
        for (unsigned i = 0; i < argc; i++) {
            bignum *operand = to_bignum(argv[i]);
            if (!operand) {
                show_error("*: not a number");
                bn_free(result);
                return TOK_ERROR;
            }
            bignum *tmp = bn_mul(result, operand);
            bn_free(result);
            bn_free(operand);
            result = tmp;
        }
        return store_integer(result);
    }
    }
    return store(1);
}

unsigned prim_minus(unsigned argc, unsigned *argv)
{
    if (argc == 0) {
        show_error("-: requires at least one argument");
        return TOK_ERROR;
    }

    if (IS_NUM(argv[0])) {
        int64_t res = CELL_ID(argv[0]);
        if (argc == 1) {
            if (res == INT64_MIN)
                goto slow_path;
            return store(-res);
        }
        for (unsigned i = 1; i < argc; i++) {
            unsigned x = argv[i];
            if (!IS_NUM(x))
                goto slow_path;
            if (__builtin_sub_overflow(res, CELL_ID(x), &res))
                goto slow_path;
        }
        return store(res);
    }

slow_path:;
    if (!check_numeric_argv(argc, argv, "-"))
        return TOK_ERROR;
    bool exact;
    numeric_level level = classify_args_argv(argc, argv, &exact);

    switch (level) {
    case NUM_COMPLEX: {
        bool all_complex_exact = true;
        for (unsigned i = 0; i < argc; i++) {
            if (!is_complex_exact(argv[i])) {
                all_complex_exact = false;
                break;
            }
        }

        if (all_complex_exact) {
            GC_GUARD;
            unsigned real_res, imag_res;
            get_complex_cells(argv[0], &real_res, &imag_res);
            gc_protect(&real_res);
            gc_protect(&imag_res);
            if (argc == 1) {
                unsigned neg_real = negate_number(real_res);
                gc_protect(&neg_real);
                unsigned neg_imag = negate_number(imag_res);
                return make_complex_exact(neg_real, neg_imag);
            }
            for (unsigned i = 1; i < argc; i++) {
                unsigned r, im;
                get_complex_cells(argv[i], &r, &im);
                real_res = binary_sub(real_res, r);
                imag_res = binary_sub(imag_res, im);
            }
            return make_complex_exact(real_res, imag_res);
        }

        double real, imag;
        get_complex_parts(argv[0], &real, &imag);
        if (argc == 1) {
            real = -real;
            imag = -imag;
        } else {
            for (unsigned i = 1; i < argc; i++) {
                double r, im;
                get_complex_parts(argv[i], &r, &im);
                real -= r;
                imag -= im;
            }
        }
        return make_complex_result(real, imag, exact);
    }
    case NUM_INEXACT: {
        double res = to_double(argv[0]);
        if (argc == 1)
            return store_inexact(-res);
        for (unsigned i = 1; i < argc; i++)
            res -= to_double(argv[i]);
        return store_inexact(res);
    }
    case NUM_RATIONAL: {
        GC_GUARD;
        unsigned num, denom;
        get_rational_cells(argv[0], &num, &denom);
        gc_protect(&num);
        gc_protect(&denom);
        if (argc == 1) {
            return normalize_rational_cells(negate_number(num), denom);
        }
        for (unsigned i = 1; i < argc; i++) {
            unsigned n, d;
            get_rational_cells(argv[i], &n, &d);
            unsigned ad = multiply_cells(num, d);
            gc_protect(&ad);
            unsigned bc = multiply_cells(n, denom);
            unsigned new_num = subtract_cells(ad, bc);
            gc_unprotect(1);
            num = new_num;
            denom = multiply_cells(denom, d);
        }
        return normalize_rational_cells(num, denom);
    }
    case NUM_BIGNUM: {
        bignum *result = to_bignum(argv[0]);
        if (argc == 1) {
            bn_neg_ip(result);
            return store_integer(result);
        }
        for (unsigned i = 1; i < argc; i++) {
            bignum *operand = to_bignum(argv[i]);
            bn_sub_ip(result, operand);
            bn_free(operand);
        }
        return store_integer(result);
    }
    case NUM_INTEGER: {
        int64_t res = CELL_ID(argv[0]);
        if (argc == 1)
            return store(-res);
        for (unsigned i = 1; i < argc; i++)
            res -= CELL_ID(argv[i]);
        return store(res);
    }
    }
    return store(0);
}

unsigned prim_div(unsigned argc, unsigned *argv)
{
    if (argc == 0) {
        show_error("/: requires at least one argument");
        return TOK_ERROR;
    }

    if (IS_NUM(argv[0])) {
        int64_t num = CELL_ID(argv[0]);
        int64_t denom = 1;
        if (argc == 1) {
            CHECK_DIV_ZERO(num, "/");
            return normalize_rational(1, num);
        }
        for (unsigned i = 1; i < argc; i++) {
            unsigned x = argv[i];
            if (!IS_NUM(x))
                goto slow_path;
            int64_t d = CELL_ID(x);
            CHECK_DIV_ZERO(d, "/");
            if (__builtin_mul_overflow(denom, d, &denom))
                goto slow_path;
        }
        return normalize_rational(num, denom);
    }

slow_path:;
    if (!check_numeric_argv(argc, argv, "/"))
        return TOK_ERROR;
    bool exact;
    numeric_level level = classify_args_argv(argc, argv, &exact);

    switch (level) {
    case NUM_COMPLEX: {
        bool all_complex_exact = true;
        for (unsigned i = 0; i < argc; i++) {
            if (!is_complex_exact(argv[i])) {
                all_complex_exact = false;
                break;
            }
        }

        if (all_complex_exact) {
            GC_GUARD;
            unsigned real_res, imag_res;
            get_complex_cells(argv[0], &real_res, &imag_res);
            gc_protect(&real_res);
            gc_protect(&imag_res);
            if (argc == 1) {
                unsigned a2 = binary_mul(real_res, real_res);
                gc_protect(&a2);
                unsigned b2 = binary_mul(imag_res, imag_res);
                unsigned denom = binary_add(a2, b2);
                gc_unprotect(1);
                gc_protect(&denom);
                if (to_double(denom) == 0.0) {
                    ERROR_RETURN("/: division by zero");
                }
                unsigned new_real = binary_div(real_res, denom);
                gc_protect(&new_real);
                unsigned neg_imag = negate_number(imag_res);
                unsigned new_imag = binary_div(neg_imag, denom);
                return make_complex_exact(new_real, new_imag);
            }
            for (unsigned i = 1; i < argc; i++) {
                unsigned c, d;
                get_complex_cells(argv[i], &c, &d);
                unsigned ac = binary_mul(real_res, c);
                gc_protect(&ac);
                unsigned bd = binary_mul(imag_res, d);
                gc_protect(&bd);
                unsigned bc = binary_mul(imag_res, c);
                gc_protect(&bc);
                unsigned ad = binary_mul(real_res, d);
                gc_protect(&ad);
                unsigned c2 = binary_mul(c, c);
                gc_protect(&c2);
                unsigned d2 = binary_mul(d, d);
                unsigned denom = binary_add(c2, d2);
                gc_unprotect(1);
                gc_protect(&denom);
                if (to_double(denom) == 0.0) {
                    ERROR_RETURN("/: division by zero");
                }
                unsigned num_real = binary_add(ac, bd);
                gc_unprotect(2);
                gc_protect(&num_real);
                unsigned num_imag = binary_sub(bc, ad);
                gc_unprotect(2);
                gc_protect(&num_imag);
                real_res = binary_div(num_real, denom);
                imag_res = binary_div(num_imag, denom);
                gc_unprotect(3);
            }
            return make_complex_exact(real_res, imag_res);
        }

        double real, imag;
        get_complex_parts(argv[0], &real, &imag);
        if (argc == 1) {
            double d = real * real + imag * imag;
            CHECK_DIV_ZERO_DBL(d, "/");
            real = real / d;
            imag = -imag / d;
        } else {
            for (unsigned i = 1; i < argc; i++) {
                double r, im;
                get_complex_parts(argv[i], &r, &im);
                double d = r * r + im * im;
                CHECK_DIV_ZERO_DBL(d, "/");
                double nr = (real * r + imag * im) / d;
                double ni = (imag * r - real * im) / d;
                real = nr;
                imag = ni;
            }
        }
        return make_complex_inexact(real, imag);
    }
    case NUM_INEXACT: {
        double res = to_double(argv[0]);
        if (argc == 1) {
            CHECK_DIV_ZERO_DBL(res, "/");
            return store_inexact(1.0 / res);
        }
        for (unsigned i = 1; i < argc; i++) {
            double divisor = to_double(argv[i]);
            CHECK_DIV_ZERO_DBL(divisor, "/");
            res /= divisor;
        }
        return store_inexact(res);
    }
    case NUM_RATIONAL:
    case NUM_BIGNUM:
    case NUM_INTEGER: {
        GC_GUARD;
        unsigned num, denom;
        get_rational_cells(argv[0], &num, &denom);
        gc_protect(&num);
        gc_protect(&denom);
        if (argc == 1) {
            if (is_zero_cell(num))
                ERROR_RETURN("/: division by zero");
            return normalize_rational_cells(denom, num);
        }
        for (unsigned i = 1; i < argc; i++) {
            unsigned n, d;
            get_rational_cells(argv[i], &n, &d);
            if (is_zero_cell(n)) {
                ERROR_RETURN("/: division by zero");
            }
            num = multiply_cells(num, d);
            denom = multiply_cells(denom, n);
        }
        return normalize_rational_cells(num, denom);
    }
    }
    return store(0);
}

unsigned prim_modulo(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 2, 2, "modulo");
    unsigned xa = argv[0], xb = argv[1];
    if (!IS_EXACT_INT(xa) || !IS_EXACT_INT(xb)) {
        show_error("modulo: expected exact integer");
        return TOK_ERROR;
    }
    // Handle bignums
    if (EITHER_BIGNUM(xa, xb)) {
        bignum *a = to_bignum(xa);
        bignum *b = to_bignum(xb);
        if (bn_is_zero(b)) {
            bn_free(a);
            bn_free(b);
            show_error("modulo: division by zero");
            return TOK_ERROR;
        }
        bignum *r = bn_mod(a, b);
        // Adjust sign for modulo semantics
        if ((bn_sign(r) < 0 && bn_sign(b) > 0) ||
            (bn_sign(r) > 0 && bn_sign(b) < 0)) {
            bignum *tmp = bn_add(r, b);
            bn_free(r);
            r = tmp;
        }
        bn_free(a);
        bn_free(b);
        return store_integer(r);
    }
    int64_t a = CELL_ID(xa);
    int64_t b = CELL_ID(xb);
    CHECK_DIV_ZERO(b, "modulo");
    int64_t r = a % b;
    if ((r < 0 && b > 0) || (r > 0 && b < 0))
        r += b;
    return store(r);
}

unsigned prim_remainder(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 2, 2, "remainder");
    unsigned xa = argv[0], xb = argv[1];
    if (!IS_EXACT_INT(xa) || !IS_EXACT_INT(xb)) {
        show_error("remainder: expected exact integer");
        return TOK_ERROR;
    }
    // Handle bignums
    if (EITHER_BIGNUM(xa, xb)) {
        bignum *a = to_bignum(xa);
        bignum *b = to_bignum(xb);
        if (bn_is_zero(b)) {
            bn_free(a);
            bn_free(b);
            show_error("remainder: division by zero");
            return TOK_ERROR;
        }
        bignum *r = bn_mod(a, b);
        bn_free(a);
        bn_free(b);
        return store_integer(r);
    }
    int64_t a = CELL_ID(xa);
    int64_t b = CELL_ID(xb);
    CHECK_DIV_ZERO(b, "remainder");
    return store(a % b);
}

unsigned prim_quotient(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 2, 2, "quotient");
    unsigned xa = argv[0], xb = argv[1];
    if (!IS_EXACT_INT(xa) || !IS_EXACT_INT(xb)) {
        show_error("quotient: expected exact integer");
        return TOK_ERROR;
    }
    // Handle bignums
    if (EITHER_BIGNUM(xa, xb)) {
        bignum *a = to_bignum(xa);
        bignum *b = to_bignum(xb);
        if (bn_is_zero(b)) {
            bn_free(a);
            bn_free(b);
            show_error("quotient: division by zero");
            return TOK_ERROR;
        }
        bignum *q = bn_div(a, b, NULL);
        bn_free(a);
        bn_free(b);
        return store_integer(q);
    }
    int64_t a = CELL_ID(xa);
    int64_t b = CELL_ID(xb);
    CHECK_DIV_ZERO(b, "quotient");
    return store(a / b);
}

unsigned prim_abs(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 1, 1, "abs");
    unsigned x = argv[0];
    switch (CELL_TYPE(x)) {
    case BT_NUM: {
        int64_t n = CELL_ID(x);
        return store(n < 0 ? -n : n);
    }
    case BT_BIGNUM: {
        bignum *bn = get_bignum(x);
        bignum *result = bn_abs(bn);
        return store_integer(result);
    }
    case BT_INEXACT: {
        double d = to_double(x);
        return store_inexact(fabs(d));
    }
    case BT_RATIONAL: {
        unsigned num_cell = CELL_CAR(x);
        unsigned denom_cell = CELL_CDR(x);
        if (is_negative_number(num_cell)) {
            num_cell = negate_number(num_cell);
        }
        return normalize_rational_cells(num_cell, denom_cell);
    }
    default:
        show_error("abs: not a real number");
        return TOK_ERROR;
    }
}
