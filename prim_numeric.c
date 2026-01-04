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

unsigned prim_plus(unsigned args)
{
    if (!args)
        return store(0);

    // Fast path: try pure integer arithmetic with builtin overflow detection
    int64_t v = 0;
    FORLIST(a, args)
    {
        unsigned x = car(a);
        if (!IS_NUM(x))
            goto slow_path;
        if (__builtin_add_overflow(v, CELL_ID(x), &v))
            goto slow_path;
    }
    return store(v);

slow_path:;
    if (!check_numeric_args(args, "+"))
        return TOK_ERROR;
    bool exact;
    numeric_level level = classify_args(args, &exact);

    switch (level) {
    case NUM_COMPLEX: {
        // Check if all complex operands are exact
        bool all_complex_exact = true;
        FORLIST(a, args)
        {
            if (!is_complex_exact(car(a))) {
                all_complex_exact = false;
                break;
            }
        }

        if (all_complex_exact) {
            // Exact complex addition: (a+bi) + (c+di) = (a+c) + (b+d)i
            GC_GUARD;
            unsigned real_sum = store(0), imag_sum = store(0);
            gc_protect(&real_sum);
            gc_protect(&imag_sum);
            FORLIST(a, args)
            {
                unsigned r, i;
                get_complex_cells(car(a), &r, &i);
                real_sum = binary_add(real_sum, r);
                imag_sum = binary_add(imag_sum, i);
            }
            return make_complex_exact(real_sum, imag_sum);
        }

        // Fall back to inexact
        double real = 0.0, imag = 0.0;
        FORLIST(a, args)
        {
            double r, i;
            get_complex_parts(car(a), &r, &i);
            real += r;
            imag += i;
        }
        return make_complex_result(real, imag, exact);
    }
    case NUM_INEXACT: {
        double sum = 0.0;
        FORLIST(a, args)
        sum += to_double(car(a));
        return store_inexact(sum);
    }
    case NUM_RATIONAL: {
        // Rational addition: a/b + c/d = (ad + bc) / bd
        // Use cell-based arithmetic to support bignum numerators/denominators
        GC_GUARD;
        unsigned num = store(0), denom = store(1);
        gc_protect(&num);
        gc_protect(&denom);
        FORLIST(a, args)
        {
            unsigned n, d;
            get_rational_cells(car(a), &n, &d);
            // num = num * d + n * denom
            unsigned ad = multiply_cells(num, d);
            gc_protect(&ad);
            unsigned bc = multiply_cells(n, denom);
            num = add_cells(ad, bc);
            gc_unprotect(1);
            // denom = denom * d
            denom = multiply_cells(denom, d);
        }
        return normalize_rational_cells(num, denom);
    }
    case NUM_BIGNUM: {
        bignum *result = bn_from_int(0);
        FORLIST(a, args)
        {
            bignum *operand = to_bignum(car(a));
            bn_add_ip(result, operand);
            bn_free(operand);
        }
        return store_integer(result);
    }
    case NUM_INTEGER: {
        // Pure integer arithmetic with builtin overflow detection
        int64_t sum = 0;
        FORLIST(a, args)
        {
            int64_t x = CELL_ID(car(a));
            int64_t new_sum;
            if (__builtin_add_overflow(sum, x, &new_sum)) {
                // Overflow - switch to bignum arithmetic
                bignum *result = bn_from_int(sum);
                bignum *operand = bn_from_int(x);
                bn_add_ip(result, operand);
                bn_free(operand);
                for (a = cdr(a); a; a = cdr(a)) {
                    operand = to_bignum(car(a));
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
    return store(0); // Unreachable
}

unsigned prim_mult(unsigned args)
{
    if (!args)
        return store(1);

    // Fast path: try pure integer arithmetic with builtin overflow detection
    int64_t v = 1;
    FORLIST(a, args)
    {
        unsigned x = car(a);
        if (!IS_NUM(x))
            goto slow_path;
        if (__builtin_mul_overflow(v, CELL_ID(x), &v))
            goto slow_path;
    }
    return store(v);

slow_path:;
    if (!check_numeric_args(args, "*"))
        return TOK_ERROR;
    bool exact;
    numeric_level level = classify_args(args, &exact);

    switch (level) {
    case NUM_COMPLEX: {
        // Check if all complex operands are exact
        bool all_complex_exact = true;
        FORLIST(a, args)
        {
            if (!is_complex_exact(car(a))) {
                all_complex_exact = false;
                break;
            }
        }

        if (all_complex_exact) {
            // Exact complex multiplication: (a+bi) * (c+di) = (ac-bd) +
            // (ad+bc)i
            GC_GUARD;
            unsigned real_prod = store(1), imag_prod = store(0);
            gc_protect(&real_prod);
            gc_protect(&imag_prod);
            FORLIST(a, args)
            {
                unsigned r, i;
                get_complex_cells(car(a), &r, &i);
                // nr = real*r - imag*i
                // ni = real*i + imag*r
                unsigned ac = binary_mul(real_prod, r);
                gc_protect(&ac);
                unsigned bd = binary_mul(imag_prod, i);
                gc_protect(&bd);
                unsigned ad = binary_mul(real_prod, i);
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

        // Fall back to inexact
        double real = 1.0, imag = 0.0;
        FORLIST(a, args)
        {
            double r, i;
            get_complex_parts(car(a), &r, &i);
            double nr = real * r - imag * i;
            double ni = real * i + imag * r;
            real = nr;
            imag = ni;
        }
        return make_complex_result(real, imag, exact);
    }
    case NUM_INEXACT: {
        double prod = 1.0;
        FORLIST(a, args)
        prod *= to_double(car(a));
        return store_inexact(prod);
    }
    case NUM_RATIONAL: {
        // Rational multiplication: a/b * c/d = ac/bd
        // Use cell-based arithmetic to support bignum numerators/denominators
        GC_GUARD;
        unsigned num = store(1), denom = store(1);
        gc_protect(&num);
        gc_protect(&denom);
        FORLIST(a, args)
        {
            unsigned n, d;
            get_rational_cells(car(a), &n, &d);
            num = multiply_cells(num, n);
            denom = multiply_cells(denom, d);
        }
        return normalize_rational_cells(num, denom);
    }
    case NUM_BIGNUM:
    case NUM_INTEGER: {
        // Use bignum for safety with large numbers
        bignum *result = bn_from_int(1);
        FORLIST(a, args)
        {
            bignum *operand = to_bignum(car(a));
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
    return store(1); // Unreachable
}

unsigned prim_minus(unsigned args)
{
    if (!args) {
        show_error("-: requires at least one argument");
        return TOK_ERROR;
    }

    // Fast path: pure integer arithmetic with overflow detection
    if (IS_NUM(car(args))) {
        int64_t res = CELL_ID(car(args));
        unsigned rargs = cdr(args);
        if (!rargs) {
            // Unary negation: check for INT64_MIN overflow
            if (res == INT64_MIN)
                goto slow_path;
            return store(-res);
        }
        for (; rargs; rargs = cdr(rargs)) {
            unsigned x = car(rargs);
            if (!IS_NUM(x))
                goto slow_path;
            if (__builtin_sub_overflow(res, CELL_ID(x), &res))
                goto slow_path;
        }
        return store(res);
    }

slow_path:;
    if (!check_numeric_args(args, "-"))
        return TOK_ERROR;
    bool exact;
    numeric_level level = classify_args(args, &exact);

    switch (level) {
    case NUM_COMPLEX: {
        // Check if all complex operands are exact
        bool all_complex_exact = true;
        FORLIST(a, args)
        {
            if (!is_complex_exact(car(a))) {
                all_complex_exact = false;
                break;
            }
        }

        if (all_complex_exact) {
            // Exact complex subtraction: (a+bi) - (c+di) = (a-c) + (b-d)i
            GC_GUARD;
            unsigned real_res, imag_res;
            get_complex_cells(car(args), &real_res, &imag_res);
            gc_protect(&real_res);
            gc_protect(&imag_res);
            unsigned rargs = cdr(args);
            if (!rargs) {
                // Unary negation
                unsigned neg_real = negate_number(real_res);
                gc_protect(&neg_real);
                unsigned neg_imag = negate_number(imag_res);
                return make_complex_exact(neg_real, neg_imag);
            }
            for (; rargs; rargs = cdr(rargs)) {
                unsigned r, i;
                get_complex_cells(car(rargs), &r, &i);
                real_res = binary_sub(real_res, r);
                imag_res = binary_sub(imag_res, i);
            }
            return make_complex_exact(real_res, imag_res);
        }

        // Fall back to inexact
        double real, imag;
        get_complex_parts(car(args), &real, &imag);
        unsigned rargs = cdr(args);
        if (!rargs) {
            real = -real;
            imag = -imag;
        } else {
            for (; rargs; rargs = cdr(rargs)) {
                double r, i;
                get_complex_parts(car(rargs), &r, &i);
                real -= r;
                imag -= i;
            }
        }
        return make_complex_result(real, imag, exact);
    }
    case NUM_INEXACT: {
        double res = to_double(car(args));
        unsigned rargs = cdr(args);
        if (!rargs)
            return store_inexact(-res);
        for (; rargs; rargs = cdr(rargs))
            res -= to_double(car(rargs));
        return store_inexact(res);
    }
    case NUM_RATIONAL: {
        // Rational subtraction: a/b - c/d = (ad - bc) / bd
        // Use cell-based arithmetic to support bignum numerators/denominators
        GC_GUARD;
        unsigned num, denom;
        get_rational_cells(car(args), &num, &denom);
        gc_protect(&num);
        gc_protect(&denom);
        unsigned rargs = cdr(args);
        if (!rargs) {
            return normalize_rational_cells(negate_number(num), denom);
        }
        for (; rargs; rargs = cdr(rargs)) {
            unsigned n, d;
            get_rational_cells(car(rargs), &n, &d);
            // num = num * d - n * denom
            unsigned ad = multiply_cells(num, d);
            gc_protect(&ad);
            unsigned bc = multiply_cells(n, denom);
            unsigned new_num = subtract_cells(ad, bc);
            gc_unprotect(1);
            num = new_num;
            // denom = denom * d
            denom = multiply_cells(denom, d);
        }
        return normalize_rational_cells(num, denom);
    }
    case NUM_BIGNUM: {
        bignum *result = to_bignum(car(args));
        unsigned rargs = cdr(args);
        if (!rargs) {
            bn_neg_ip(result);
            return store_integer(result);
        }
        for (; rargs; rargs = cdr(rargs)) {
            bignum *operand = to_bignum(car(rargs));
            bn_sub_ip(result, operand);
            bn_free(operand);
        }
        return store_integer(result);
    }
    case NUM_INTEGER: {
        int64_t res = CELL_ID(car(args));
        unsigned rargs = cdr(args);
        if (!rargs)
            return store(-res);
        for (; rargs; rargs = cdr(rargs))
            res -= CELL_ID(car(rargs));
        return store(res);
    }
    }
    return store(0); // Unreachable
}

unsigned prim_div(unsigned args)
{
    if (!args) {
        show_error("/: requires at least one argument");
        return TOK_ERROR;
    }

    // Fast path: pure integer division -> rational
    if (IS_NUM(car(args))) {
        int64_t num = CELL_ID(car(args));
        int64_t denom = 1;
        unsigned rargs = cdr(args);
        if (!rargs) {
            CHECK_DIV_ZERO(num, "/");
            return normalize_rational(1, num);
        }
        for (; rargs; rargs = cdr(rargs)) {
            unsigned x = car(rargs);
            if (!IS_NUM(x))
                goto slow_path;
            int64_t d = CELL_ID(x);
            CHECK_DIV_ZERO(d, "/");
            denom *= d;
        }
        return normalize_rational(num, denom);
    }

slow_path:;
    if (!check_numeric_args(args, "/"))
        return TOK_ERROR;
    bool exact;
    numeric_level level = classify_args(args, &exact);

    switch (level) {
    case NUM_COMPLEX: {
        // Check if all complex operands are exact
        bool all_complex_exact = true;
        FORLIST(a, args)
        {
            if (!is_complex_exact(car(a))) {
                all_complex_exact = false;
                break;
            }
        }

        if (all_complex_exact) {
            // Exact complex division: (a+bi)/(c+di) = (ac+bd)/(c²+d²) +
            // (bc-ad)/(c²+d²)i
            GC_GUARD;
            unsigned real_res, imag_res;
            get_complex_cells(car(args), &real_res, &imag_res);
            gc_protect(&real_res);
            gc_protect(&imag_res);
            unsigned rargs = cdr(args);
            if (!rargs) {
                // Reciprocal: 1/(a+bi) = a/(a²+b²) - b/(a²+b²)i
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
            for (; rargs; rargs = cdr(rargs)) {
                unsigned c, d;
                get_complex_cells(car(rargs), &c, &d);
                // (a+bi)/(c+di) = (ac+bd)/(c²+d²) + (bc-ad)/(c²+d²)i
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

        // Fall back to inexact
        double real, imag;
        get_complex_parts(car(args), &real, &imag);
        unsigned rargs = cdr(args);
        if (!rargs) {
            double d = real * real + imag * imag;
            CHECK_DIV_ZERO_DBL(d, "/");
            real = real / d;
            imag = -imag / d;
        } else {
            for (; rargs; rargs = cdr(rargs)) {
                double r, i;
                get_complex_parts(car(rargs), &r, &i);
                double d = r * r + i * i;
                CHECK_DIV_ZERO_DBL(d, "/");
                double nr = (real * r + imag * i) / d;
                double ni = (imag * r - real * i) / d;
                real = nr;
                imag = ni;
            }
        }
        return make_complex_inexact(real, imag);
    }
    case NUM_INEXACT: {
        double res = to_double(car(args));
        unsigned rargs = cdr(args);
        if (!rargs) {
            CHECK_DIV_ZERO_DBL(res, "/");
            return store_inexact(1.0 / res);
        }
        for (; rargs; rargs = cdr(rargs)) {
            double divisor = to_double(car(rargs));
            CHECK_DIV_ZERO_DBL(divisor, "/");
            res /= divisor;
        }
        return store_inexact(res);
    }
    case NUM_RATIONAL:
    case NUM_BIGNUM:
    case NUM_INTEGER: {
        // Rational division: (a/b) / (c/d) = ad / bc
        // Use cell-based arithmetic to support bignum numerators/denominators
        GC_GUARD;
        unsigned num, denom;
        get_rational_cells(car(args), &num, &denom);
        gc_protect(&num);
        gc_protect(&denom);
        unsigned rargs = cdr(args);
        if (!rargs) {
            // Reciprocal: 1 / (a/b) = b/a
            if (is_zero_cell(num))
                ERROR_RETURN("/: division by zero");
            return normalize_rational_cells(denom, num);
        }
        for (; rargs; rargs = cdr(rargs)) {
            unsigned n, d;
            get_rational_cells(car(rargs), &n, &d);
            if (is_zero_cell(n)) {
                ERROR_RETURN("/: division by zero");
            }
            // num = num * d, denom = denom * n
            num = multiply_cells(num, d);
            denom = multiply_cells(denom, n);
        }
        return normalize_rational_cells(num, denom);
    }
    }
    return store(0); // Unreachable
}

unsigned prim_modulo(unsigned args)
{
    REQUIRE_ARGS(args, 2, 2, "modulo");
    unsigned xa = car(args), xb = cadr(args);
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

unsigned prim_remainder(unsigned args)
{
    REQUIRE_ARGS(args, 2, 2, "remainder");
    unsigned xa = car(args), xb = cadr(args);
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

unsigned prim_quotient(unsigned args)
{
    REQUIRE_ARGS(args, 2, 2, "quotient");
    unsigned xa = car(args), xb = cadr(args);
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

unsigned prim_abs(unsigned args)
{
    REQUIRE_ARGS(args, 1, 1, "abs");
    unsigned x = car(args);
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
