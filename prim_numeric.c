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

#define RETURN_IF_ERROR(value)                                                 \
    do {                                                                       \
        if ((value) == TOK_ERROR)                                              \
            return TOK_ERROR;                                                  \
    } while (0)

static bool all_complex_args_exact(unsigned argc, unsigned *argv)
{
    for (unsigned i = 0; i < argc; i++) {
        if (!is_complex_exact(argv[i]))
            return false;
    }
    return true;
}

static bool require_exact_integer_pair(unsigned xa, unsigned xb,
                                       const char *name)
{
    if (IS_EXACT_INT(xa) && IS_EXACT_INT(xb) && is_numeric(xa) &&
        is_numeric(xb))
        return true;

    show_error("%s: expected exact integer", name);
    return false;
}

static bool bignum_division_operands(unsigned xa, unsigned xb, bignum **a_out,
                                     bignum **b_out, const char *name)
{
    bignum *a = to_bignum(xa);
    bignum *b = to_bignum(xb);
    if (!a || !b) {
        bn_free(a);
        bn_free(b);
        show_error("%s: out of memory", name);
        return false;
    }
    if (bn_is_zero(b)) {
        bn_free(a);
        bn_free(b);
        show_error("%s: division by zero", name);
        return false;
    }

    *a_out = a;
    *b_out = b;
    return true;
}

static unsigned divrem_values(unsigned q, unsigned r)
{
    unsigned values[2] = {q, r};
    return values_from_argv(2, values);
}

static unsigned bignum_truncate_divrem_values(unsigned xa, unsigned xb,
                                              const char *name)
{
    GC_GUARD;
    bignum *a, *b;
    if (!bignum_division_operands(xa, xb, &a, &b, name))
        return TOK_ERROR;

    bignum *r = NULL;
    bignum *q = bn_div(a, b, &r);
    bn_free(a);
    bn_free(b);
    if (!q || !r) {
        bn_free(q);
        bn_free(r);
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }

    unsigned qv = store_integer(q);
    gc_protect(&qv);
    unsigned rv = store_integer(r);
    gc_protect(&rv);
    return divrem_values(qv, rv);
}

static unsigned bignum_floor_divrem_values(unsigned xa, unsigned xb,
                                           const char *name)
{
    GC_GUARD;
    bignum *a, *b;
    if (!bignum_division_operands(xa, xb, &a, &b, name))
        return TOK_ERROR;

    bignum *r = NULL;
    bignum *q = bn_div(a, b, &r);
    if (!q || !r) {
        bn_free(a);
        bn_free(b);
        bn_free(q);
        bn_free(r);
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }

    if (!bn_is_zero(r) && bn_sign(a) != bn_sign(b)) {
        bignum *one = bn_from_int(1);
        bignum *adjusted_q = one ? bn_sub(q, one) : NULL;
        bignum *adjusted_r = adjusted_q ? bn_add(r, b) : NULL;
        bn_free(one);
        if (!adjusted_q || !adjusted_r) {
            bn_free(a);
            bn_free(b);
            bn_free(q);
            bn_free(r);
            bn_free(adjusted_q);
            bn_free(adjusted_r);
            show_error("%s: out of memory", name);
            return TOK_ERROR;
        }
        bn_free(q);
        bn_free(r);
        q = adjusted_q;
        r = adjusted_r;
    }

    bn_free(a);
    bn_free(b);
    unsigned qv = store_integer(q);
    gc_protect(&qv);
    unsigned rv = store_integer(r);
    gc_protect(&rv);
    return divrem_values(qv, rv);
}

static bool bignum_add_args_ip(bignum *result, unsigned argc, unsigned *argv,
                               unsigned start, const char *name)
{
    for (unsigned i = start; i < argc; i++) {
        bignum *operand = to_bignum(argv[i]);
        if (!operand) {
            show_error("%s: out of memory", name);
            return false;
        }
        if (!bn_add_ip_checked(result, operand)) {
            bn_free(operand);
            show_error("%s: out of memory", name);
            return false;
        }
        bn_free(operand);
    }
    return true;
}

static bignum *bignum_multiply_args(unsigned argc, unsigned *argv,
                                    const char *name)
{
    bignum *result = bn_from_int(1);
    if (!result) {
        show_error("%s: out of memory", name);
        return NULL;
    }

    for (unsigned i = 0; i < argc; i++) {
        bignum *operand = to_bignum(argv[i]);
        if (!operand) {
            show_error("%s: out of memory", name);
            bn_free(result);
            return NULL;
        }
        bignum *tmp = bn_mul(result, operand);
        bn_free(result);
        bn_free(operand);
        if (!tmp) {
            show_error("%s: out of memory", name);
            return NULL;
        }
        result = tmp;
    }

    return result;
}

static bool bignum_subtract_args_ip(bignum *result, unsigned argc,
                                    unsigned *argv, unsigned start,
                                    const char *name)
{
    for (unsigned i = start; i < argc; i++) {
        bignum *operand = to_bignum(argv[i]);
        if (!operand) {
            show_error("%s: out of memory", name);
            return false;
        }
        if (!bn_sub_ip_checked(result, operand)) {
            bn_free(operand);
            show_error("%s: out of memory", name);
            return false;
        }
        bn_free(operand);
    }
    return true;
}

static double rescale_complex_quotient_component(double component,
                                                 double numerator_scale,
                                                 double divisor_scale)
{
    if (component == 0.0)
        return component;

    int numerator_exp, divisor_exp;
    double numerator_mantissa = frexp(numerator_scale, &numerator_exp);
    double divisor_mantissa = frexp(divisor_scale, &divisor_exp);
    return scalbn(component * (numerator_mantissa / divisor_mantissa),
                  numerator_exp - divisor_exp);
}

static unsigned inexact_complex_div_value(unsigned argc, unsigned *argv)
{
    double real, imag;
    unsigned start;
    if (argc == 1) {
        real = 1.0;
        imag = 0.0;
        start = 0;
    } else {
        get_complex_parts(argv[0], &real, &imag);
        start = 1;
    }

    for (unsigned i = start; i < argc; i++) {
        double r, im;
        get_complex_parts(argv[i], &r, &im);
        if (r == 0.0 && im == 0.0) {
            // IEEE semantics for inexact division by zero (R7RS 6.2.4)
            real = real / 0.0;
            imag = imag / 0.0;
            continue;
        }

        /*
         * Normalize both operands before combining their components.  This
         * keeps the products in [-1, 1], so it cannot overflow or underflow
         * before the final exponent-aware rescaling.  In particular, this
         * preserves a zero component when enormous intermediate terms cancel.
         */
        double numerator_scale = fmax(fabs(real), fabs(imag));
        if (numerator_scale == 0.0) {
            real = 0.0;
            imag = 0.0;
            continue;
        }
        double divisor_scale = fmax(fabs(r), fabs(im));
        if (isfinite(numerator_scale) && isinf(divisor_scale)) {
            real = 0.0;
            imag = 0.0;
            continue;
        }
        if (!isfinite(numerator_scale) || !isfinite(divisor_scale)) {
            // Preserve IEEE-754 propagation for NaN and infinite numerators.
            double divisor_norm = r * r + im * im;
            double new_real = (real * r + imag * im) / divisor_norm;
            double new_imag = (imag * r - real * im) / divisor_norm;
            real = new_real;
            imag = new_imag;
            continue;
        }
        double real_normalized = real / numerator_scale;
        double imag_normalized = imag / numerator_scale;
        double divisor_real_normalized = r / divisor_scale;
        double divisor_imag_normalized = im / divisor_scale;
        double divisor_norm = divisor_real_normalized * divisor_real_normalized +
                              divisor_imag_normalized * divisor_imag_normalized;
        double new_real = (real_normalized * divisor_real_normalized +
                           imag_normalized * divisor_imag_normalized) /
                          divisor_norm;
        double new_imag = (imag_normalized * divisor_real_normalized -
                           real_normalized * divisor_imag_normalized) /
                          divisor_norm;
        real = rescale_complex_quotient_component(new_real, numerator_scale,
                                                   divisor_scale);
        imag = rescale_complex_quotient_component(new_imag, numerator_scale,
                                                   divisor_scale);
    }
    return make_complex_inexact(real, imag);
}

unsigned prim_plus(unsigned argc, unsigned *argv)
{
    GC_GUARD;
    for (unsigned i = 0; i < argc; i++)
        gc_protect(&argv[i]);
    if (argc == 0)
        return store(0);

    // Fast path: try pure integer arithmetic with builtin overflow detection
    int64_t v = 0;
    for (unsigned i = 0; i < argc; i++) {
        unsigned x = argv[i];
        int64_t xi;
        if (!exact_int64_value(x, &xi))
            goto slow_path;
        if (__builtin_add_overflow(v, xi, &v))
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
        if (all_complex_args_exact(argc, argv)) {
            GC_GUARD;
            unsigned real_sum = store(0);
            gc_protect(&real_sum);
            unsigned imag_sum = store(0);
            gc_protect(&imag_sum);
            for (unsigned i = 0; i < argc; i++) {
                unsigned r, im;
                get_complex_cells(argv[i], &r, &im);
                gc_protect(&r);
                gc_protect(&im);
                real_sum = binary_add(real_sum, r);
                RETURN_IF_ERROR(real_sum);
                imag_sum = binary_add(imag_sum, im);
                RETURN_IF_ERROR(imag_sum);
                gc_unprotect(2);
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
        unsigned num = store(0);
        gc_protect(&num);
        unsigned denom = store(1);
        gc_protect(&denom);
        for (unsigned i = 0; i < argc; i++) {
            unsigned n, d;
            get_rational_cells(argv[i], &n, &d);
            gc_protect(&n);
            gc_protect(&d);
            unsigned ad = multiply_cells(num, d);
            RETURN_IF_ERROR(ad);
            gc_protect(&ad);
            unsigned bc = multiply_cells(n, denom);
            RETURN_IF_ERROR(bc);
            gc_protect(&bc);
            num = add_cells(ad, bc);
            RETURN_IF_ERROR(num);
            gc_unprotect(2);
            denom = multiply_cells(denom, d);
            RETURN_IF_ERROR(denom);
            gc_unprotect(2);
        }
        return normalize_rational_cells(num, denom);
    }
    case NUM_BIGNUM: {
        bignum *result = bn_from_int(0);
        if (!result) {
            show_error("+: out of memory");
            return TOK_ERROR;
        }
        if (!bignum_add_args_ip(result, argc, argv, 0, "+")) {
            bn_free(result);
            return TOK_ERROR;
        }
        return store_integer(result);
    }
    case NUM_INTEGER: {
        int64_t sum = 0;
        for (unsigned i = 0; i < argc; i++) {
            int64_t x;
            if (!exact_int64_value(argv[i], &x)) {
                show_error("+: invalid integer operand");
                return TOK_ERROR;
            }
            int64_t new_sum;
            if (__builtin_add_overflow(sum, x, &new_sum)) {
                bignum *result = bn_from_int(sum);
                bignum *operand = bn_from_int(x);
                if (!result || !operand) {
                    bn_free(result);
                    bn_free(operand);
                    show_error("+: out of memory");
                    return TOK_ERROR;
                }
                if (!bn_add_ip_checked(result, operand)) {
                    bn_free(result);
                    bn_free(operand);
                    show_error("+: out of memory");
                    return TOK_ERROR;
                }
                bn_free(operand);
                if (!bignum_add_args_ip(result, argc, argv, i + 1, "+")) {
                    bn_free(result);
                    return TOK_ERROR;
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
    GC_GUARD;
    for (unsigned i = 0; i < argc; i++)
        gc_protect(&argv[i]);
    if (argc == 0)
        return store(1);

    int64_t v = 1;
    for (unsigned i = 0; i < argc; i++) {
        unsigned x = argv[i];
        int64_t xi;
        if (!exact_int64_value(x, &xi))
            goto slow_path;
        if (__builtin_mul_overflow(v, xi, &v))
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
        if (all_complex_args_exact(argc, argv)) {
            GC_GUARD;
            unsigned real_prod = store(1);
            gc_protect(&real_prod);
            unsigned imag_prod = store(0);
            gc_protect(&imag_prod);
            for (unsigned i = 0; i < argc; i++) {
                unsigned r, im;
                get_complex_cells(argv[i], &r, &im);
                gc_protect(&r);
                gc_protect(&im);
                unsigned ac = binary_mul(real_prod, r);
                RETURN_IF_ERROR(ac);
                gc_protect(&ac);
                unsigned bd = binary_mul(imag_prod, im);
                RETURN_IF_ERROR(bd);
                gc_protect(&bd);
                unsigned ad = binary_mul(real_prod, im);
                RETURN_IF_ERROR(ad);
                gc_protect(&ad);
                unsigned bc = binary_mul(imag_prod, r);
                RETURN_IF_ERROR(bc);
                gc_protect(&bc);
                unsigned new_real = binary_sub(ac, bd);
                RETURN_IF_ERROR(new_real);
                gc_protect(&new_real);
                unsigned new_imag = binary_add(ad, bc);
                RETURN_IF_ERROR(new_imag);
                gc_unprotect(5);
                real_prod = new_real;
                imag_prod = new_imag;
                gc_unprotect(2);
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
        unsigned num = store(1);
        gc_protect(&num);
        unsigned denom = store(1);
        gc_protect(&denom);
        for (unsigned i = 0; i < argc; i++) {
            unsigned n, d;
            get_rational_cells(argv[i], &n, &d);
            gc_protect(&n);
            gc_protect(&d);
            num = multiply_cells(num, n);
            RETURN_IF_ERROR(num);
            denom = multiply_cells(denom, d);
            RETURN_IF_ERROR(denom);
            gc_unprotect(2);
        }
        return normalize_rational_cells(num, denom);
    }
    case NUM_BIGNUM:
    case NUM_INTEGER: {
        bignum *result = bignum_multiply_args(argc, argv, "*");
        if (!result)
            return TOK_ERROR;
        return store_integer(result);
    }
    }
    return store(1);
}

unsigned prim_minus(unsigned argc, unsigned *argv)
{
    GC_GUARD;
    for (unsigned i = 0; i < argc; i++)
        gc_protect(&argv[i]);
    if (argc == 0) {
        show_error("-: requires at least one argument");
        return TOK_ERROR;
    }

    int64_t fast_res;
    if (exact_int64_value(argv[0], &fast_res)) {
        if (argc == 1) {
            if (fast_res == INT64_MIN)
                goto slow_path;
            return store(-fast_res);
        }
        for (unsigned i = 1; i < argc; i++) {
            unsigned x = argv[i];
            int64_t xi;
            if (!exact_int64_value(x, &xi))
                goto slow_path;
            if (__builtin_sub_overflow(fast_res, xi, &fast_res))
                goto slow_path;
        }
        return store(fast_res);
    }

slow_path:;
    if (!check_numeric_argv(argc, argv, "-"))
        return TOK_ERROR;
    bool exact;
    numeric_level level = classify_args_argv(argc, argv, &exact);

    switch (level) {
    case NUM_COMPLEX: {
        if (all_complex_args_exact(argc, argv)) {
            GC_GUARD;
            unsigned real_res, imag_res;
            get_complex_cells(argv[0], &real_res, &imag_res);
            gc_protect(&real_res);
            gc_protect(&imag_res);
            if (argc == 1) {
                unsigned neg_real = negate_number(real_res);
                RETURN_IF_ERROR(neg_real);
                gc_protect(&neg_real);
                unsigned neg_imag = negate_number(imag_res);
                RETURN_IF_ERROR(neg_imag);
                return make_complex_exact(neg_real, neg_imag);
            }
            for (unsigned i = 1; i < argc; i++) {
                unsigned r, im;
                get_complex_cells(argv[i], &r, &im);
                gc_protect(&r);
                gc_protect(&im);
                real_res = binary_sub(real_res, r);
                RETURN_IF_ERROR(real_res);
                imag_res = binary_sub(imag_res, im);
                RETURN_IF_ERROR(imag_res);
                gc_unprotect(2);
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
            unsigned neg_num = negate_number(num);
            RETURN_IF_ERROR(neg_num);
            gc_protect(&neg_num);
            return normalize_rational_cells(neg_num, denom);
        }
        for (unsigned i = 1; i < argc; i++) {
            unsigned n, d;
            get_rational_cells(argv[i], &n, &d);
            gc_protect(&n);
            gc_protect(&d);
            unsigned ad = multiply_cells(num, d);
            RETURN_IF_ERROR(ad);
            gc_protect(&ad);
            unsigned bc = multiply_cells(n, denom);
            RETURN_IF_ERROR(bc);
            gc_protect(&bc);
            unsigned new_num = subtract_cells(ad, bc);
            RETURN_IF_ERROR(new_num);
            gc_unprotect(2);
            num = new_num;
            denom = multiply_cells(denom, d);
            RETURN_IF_ERROR(denom);
            gc_unprotect(2);
        }
        return normalize_rational_cells(num, denom);
    }
    case NUM_BIGNUM: {
        bignum *result = to_bignum(argv[0]);
        if (!result) {
            show_error("-: out of memory");
            return TOK_ERROR;
        }
        if (argc == 1) {
            bn_neg_ip(result);
            return store_integer(result);
        }
        if (!bignum_subtract_args_ip(result, argc, argv, 1, "-")) {
            bn_free(result);
            return TOK_ERROR;
        }
        return store_integer(result);
    }
    case NUM_INTEGER: {
        int64_t res;
        if (!exact_int64_value(argv[0], &res)) {
            show_error("-: invalid integer operand");
            return TOK_ERROR;
        }
        if (argc == 1) {
            if (res == INT64_MIN) {
                bignum *result = bn_from_int(res);
                if (!result) {
                    show_error("-: out of memory");
                    return TOK_ERROR;
                }
                bn_neg_ip(result);
                return store_integer(result);
            }
            return store(-res);
        }
        for (unsigned i = 1; i < argc; i++) {
            int64_t x;
            if (!exact_int64_value(argv[i], &x)) {
                show_error("-: invalid integer operand");
                return TOK_ERROR;
            }
            int64_t new_res;
            if (__builtin_sub_overflow(res, x, &new_res)) {
                bignum *result = bn_from_int(res);
                bignum *operand = bn_from_int(x);
                if (!result || !operand) {
                    bn_free(result);
                    bn_free(operand);
                    show_error("-: out of memory");
                    return TOK_ERROR;
                }
                if (!bn_sub_ip_checked(result, operand)) {
                    bn_free(result);
                    bn_free(operand);
                    show_error("-: out of memory");
                    return TOK_ERROR;
                }
                bn_free(operand);
                if (!bignum_subtract_args_ip(result, argc, argv, i + 1,
                                             "-")) {
                    bn_free(result);
                    return TOK_ERROR;
                }
                return store_integer(result);
            }
            res = new_res;
        }
        return store(res);
    }
    }
    return store(0);
}

unsigned prim_div(unsigned argc, unsigned *argv)
{
    GC_GUARD;
    for (unsigned i = 0; i < argc; i++)
        gc_protect(&argv[i]);
    if (argc == 0) {
        show_error("/: requires at least one argument");
        return TOK_ERROR;
    }

    int64_t fast_num;
    if (exact_int64_value(argv[0], &fast_num)) {
        int64_t denom = 1;
        if (argc == 1) {
            CHECK_DIV_ZERO(fast_num, "/");
            return normalize_rational(1, fast_num);
        }
        for (unsigned i = 1; i < argc; i++) {
            unsigned x = argv[i];
            int64_t d;
            if (!exact_int64_value(x, &d))
                goto slow_path;
            CHECK_DIV_ZERO(d, "/");
            if (__builtin_mul_overflow(denom, d, &denom))
                goto slow_path;
        }
        return normalize_rational(fast_num, denom);
    }

slow_path:;
    if (!check_numeric_argv(argc, argv, "/"))
        return TOK_ERROR;
    bool exact;
    numeric_level level = classify_args_argv(argc, argv, &exact);

    switch (level) {
    case NUM_COMPLEX: {
        if (all_complex_args_exact(argc, argv)) {
            GC_GUARD;
            unsigned real_res, imag_res;
            get_complex_cells(argv[0], &real_res, &imag_res);
            gc_protect(&real_res);
            gc_protect(&imag_res);
            if (argc == 1) {
                unsigned a2 = binary_mul(real_res, real_res);
                RETURN_IF_ERROR(a2);
                gc_protect(&a2);
                unsigned b2 = binary_mul(imag_res, imag_res);
                RETURN_IF_ERROR(b2);
                gc_protect(&b2);
                unsigned denom = binary_add(a2, b2);
                RETURN_IF_ERROR(denom);
                gc_protect(&denom);
                if (is_zero_number(denom)) {
                    ERROR_RETURN("/: division by zero");
                }
                unsigned new_real = binary_div(real_res, denom);
                RETURN_IF_ERROR(new_real);
                gc_protect(&new_real);
                unsigned neg_imag = negate_number(imag_res);
                RETURN_IF_ERROR(neg_imag);
                gc_protect(&neg_imag);
                unsigned new_imag = binary_div(neg_imag, denom);
                RETURN_IF_ERROR(new_imag);
                return make_complex_exact(new_real, new_imag);
            }
            for (unsigned i = 1; i < argc; i++) {
                unsigned c, d;
                get_complex_cells(argv[i], &c, &d);
                gc_protect(&c);
                gc_protect(&d);
                unsigned ac = binary_mul(real_res, c);
                RETURN_IF_ERROR(ac);
                gc_protect(&ac);
                unsigned bd = binary_mul(imag_res, d);
                RETURN_IF_ERROR(bd);
                gc_protect(&bd);
                unsigned bc = binary_mul(imag_res, c);
                RETURN_IF_ERROR(bc);
                gc_protect(&bc);
                unsigned ad = binary_mul(real_res, d);
                RETURN_IF_ERROR(ad);
                gc_protect(&ad);
                unsigned c2 = binary_mul(c, c);
                RETURN_IF_ERROR(c2);
                gc_protect(&c2);
                unsigned d2 = binary_mul(d, d);
                RETURN_IF_ERROR(d2);
                gc_protect(&d2);
                unsigned denom = binary_add(c2, d2);
                RETURN_IF_ERROR(denom);
                gc_protect(&denom);
                if (is_zero_number(denom)) {
                    ERROR_RETURN("/: division by zero");
                }
                unsigned num_real = binary_add(ac, bd);
                RETURN_IF_ERROR(num_real);
                gc_protect(&num_real);
                unsigned num_imag = binary_sub(bc, ad);
                RETURN_IF_ERROR(num_imag);
                gc_protect(&num_imag);
                unsigned new_real = binary_div(num_real, denom);
                RETURN_IF_ERROR(new_real);
                gc_protect(&new_real);
                unsigned new_imag = binary_div(num_imag, denom);
                RETURN_IF_ERROR(new_imag);
                real_res = new_real;
                imag_res = new_imag;
                gc_unprotect(12);
            }
            return make_complex_exact(real_res, imag_res);
        }

        return inexact_complex_div_value(argc, argv);
    }
    case NUM_INEXACT: {
        // Inexact division by zero follows IEEE 754 per R7RS 6.2.4:
        // (/ 1.0 0.0) => +inf.0, (/ -1.0 0.0) => -inf.0, (/ 0.0 0.0) => +nan.0
        double res = to_double(argv[0]);
        if (argc == 1)
            return store_inexact(1.0 / res);
        for (unsigned i = 1; i < argc; i++)
            res /= to_double(argv[i]);
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
            gc_protect(&n);
            gc_protect(&d);
            if (is_zero_cell(n)) {
                ERROR_RETURN("/: division by zero");
            }
            num = multiply_cells(num, d);
            RETURN_IF_ERROR(num);
            denom = multiply_cells(denom, n);
            RETURN_IF_ERROR(denom);
            gc_unprotect(2);
        }
        return normalize_rational_cells(num, denom);
    }
    }
    return store(0);
}

// R7RS 6.2.6: the division operators accept integer-valued inexact
// arguments, and any inexact operand makes the result(s) inexact
// (e.g. (quotient 7.0 2) => 3.0)
unsigned prim_divlike_inexact(unsigned (*fn)(unsigned, unsigned *),
                              unsigned argc, unsigned *argv, const char *name)
{
    if (argc != 2 || (!IS_INEXACT(argv[0]) && !IS_INEXACT(argv[1])))
        return fn(argc, argv);

    GC_GUARD;
    unsigned conv[2];
    for (int i = 0; i < 2; i++) {
        unsigned v = argv[i];
        if (IS_INEXACT(v)) {
            double d = to_double(v);
            if (!isfinite(d) || floor(d) != d) {
                show_error("%s: expected integer", name);
                return TOK_ERROR;
            }
            v = prim_inexact_to_exact(v);
            if (v == TOK_ERROR)
                return TOK_ERROR;
        }
        conv[i] = v;
        gc_protect(&conv[i]);
    }
    unsigned result = fn(2, conv);
    if (result == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&result);
    if (IS_MULTIVAL(result)) {
        unsigned vals = CELL_CAR(result);
        gc_protect(&vals);
        unsigned q = store_inexact(to_double(car(vals)));
        gc_protect(&q);
        unsigned r = store_inexact(to_double(cadr(vals)));
        gc_protect(&r);
        unsigned out[2] = {q, r};
        return values_from_argv(2, out);
    }
    return store_inexact(to_double(result));
}

unsigned prim_modulo(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 2, 2, "modulo");
    unsigned xa = argv[0], xb = argv[1];
    if (!require_exact_integer_pair(xa, xb, "modulo"))
        return TOK_ERROR;
    // Handle bignums
    if (EITHER_BIGNUM(xa, xb)) {
        bignum *a, *b;
        if (!bignum_division_operands(xa, xb, &a, &b, "modulo"))
            return TOK_ERROR;
        bignum *r = bn_mod(a, b);
        if (!r) {
            bn_free(a);
            bn_free(b);
            show_error("modulo: out of memory");
            return TOK_ERROR;
        }
        // Adjust sign for modulo semantics
        if ((bn_sign(r) < 0 && bn_sign(b) > 0) ||
            (bn_sign(r) > 0 && bn_sign(b) < 0)) {
            bignum *tmp = bn_add(r, b);
            if (!tmp) {
                bn_free(r);
                bn_free(a);
                bn_free(b);
                show_error("modulo: out of memory");
                return TOK_ERROR;
            }
            bn_free(r);
            r = tmp;
        }
        bn_free(a);
        bn_free(b);
        return store_integer(r);
    }
    int64_t a, b;
    if (!exact_int64_value(xa, &a) || !exact_int64_value(xb, &b)) {
        show_error("modulo: expected exact integer");
        return TOK_ERROR;
    }
    CHECK_DIV_ZERO(b, "modulo");
    if (a == INT64_MIN && b == -1)
        return store(0);
    int64_t r = a % b;
    if ((r < 0 && b > 0) || (r > 0 && b < 0))
        r += b;
    return store(r);
}

unsigned prim_remainder(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 2, 2, "remainder");
    unsigned xa = argv[0], xb = argv[1];
    if (!require_exact_integer_pair(xa, xb, "remainder"))
        return TOK_ERROR;
    // Handle bignums
    if (EITHER_BIGNUM(xa, xb)) {
        bignum *a, *b;
        if (!bignum_division_operands(xa, xb, &a, &b, "remainder"))
            return TOK_ERROR;
        bignum *r = bn_mod(a, b);
        if (!r) {
            bn_free(a);
            bn_free(b);
            show_error("remainder: out of memory");
            return TOK_ERROR;
        }
        bn_free(a);
        bn_free(b);
        return store_integer(r);
    }
    int64_t a, b;
    if (!exact_int64_value(xa, &a) || !exact_int64_value(xb, &b)) {
        show_error("remainder: expected exact integer");
        return TOK_ERROR;
    }
    CHECK_DIV_ZERO(b, "remainder");
    if (a == INT64_MIN && b == -1)
        return store(0);
    return store(a % b);
}

unsigned prim_quotient(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 2, 2, "quotient");
    unsigned xa = argv[0], xb = argv[1];
    if (!require_exact_integer_pair(xa, xb, "quotient"))
        return TOK_ERROR;
    // Handle bignums
    if (EITHER_BIGNUM(xa, xb)) {
        bignum *a, *b;
        if (!bignum_division_operands(xa, xb, &a, &b, "quotient"))
            return TOK_ERROR;
        bignum *q = bn_div(a, b, NULL);
        if (!q) {
            bn_free(a);
            bn_free(b);
            show_error("quotient: out of memory");
            return TOK_ERROR;
        }
        bn_free(a);
        bn_free(b);
        return store_integer(q);
    }
    int64_t a, b;
    if (!exact_int64_value(xa, &a) || !exact_int64_value(xb, &b)) {
        show_error("quotient: expected exact integer");
        return TOK_ERROR;
    }
    CHECK_DIV_ZERO(b, "quotient");
    if (a == INT64_MIN && b == -1) {
        bignum *result = bn_from_int(a);
        if (!result) {
            show_error("quotient: out of memory");
            return TOK_ERROR;
        }
        bn_neg_ip(result);
        return store_integer(result);
    }
    return store(a / b);
}

unsigned prim_truncate_divrem(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 2, 2, "truncate/");
    unsigned xa = argv[0], xb = argv[1];
    if (!require_exact_integer_pair(xa, xb, "truncate/"))
        return TOK_ERROR;

    if (EITHER_BIGNUM(xa, xb))
        return bignum_truncate_divrem_values(xa, xb, "truncate/");

    int64_t a, b;
    if (!exact_int64_value(xa, &a) || !exact_int64_value(xb, &b)) {
        show_error("truncate/: expected exact integer");
        return TOK_ERROR;
    }
    CHECK_DIV_ZERO(b, "truncate/");
    if (a == INT64_MIN && b == -1) {
        bignum *q = bn_from_int(a);
        if (!q) {
            show_error("truncate/: out of memory");
            return TOK_ERROR;
        }
        bn_neg_ip(q);
        unsigned qv = store_integer(q);
        GC_GUARD;
        gc_protect(&qv);
        unsigned rv = store(0);
        gc_protect(&rv);
        return divrem_values(qv, rv);
    }
    GC_GUARD;
    unsigned qv = store(a / b);
    gc_protect(&qv);
    unsigned rv = store(a % b);
    gc_protect(&rv);
    return divrem_values(qv, rv);
}

unsigned prim_floor_divrem(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 2, 2, "floor/");
    unsigned xa = argv[0], xb = argv[1];
    if (!require_exact_integer_pair(xa, xb, "floor/"))
        return TOK_ERROR;

    if (EITHER_BIGNUM(xa, xb))
        return bignum_floor_divrem_values(xa, xb, "floor/");

    int64_t a, b;
    if (!exact_int64_value(xa, &a) || !exact_int64_value(xb, &b)) {
        show_error("floor/: expected exact integer");
        return TOK_ERROR;
    }
    CHECK_DIV_ZERO(b, "floor/");
    if (a == INT64_MIN && b == -1) {
        bignum *q = bn_from_int(a);
        if (!q) {
            show_error("floor/: out of memory");
            return TOK_ERROR;
        }
        bn_neg_ip(q);
        unsigned qv = store_integer(q);
        GC_GUARD;
        gc_protect(&qv);
        unsigned rv = store(0);
        gc_protect(&rv);
        return divrem_values(qv, rv);
    }

    int64_t q = a / b;
    int64_t r = a % b;
    if (r != 0 && ((a < 0 && b > 0) || (a > 0 && b < 0))) {
        q -= 1;
        r += b;
    }
    GC_GUARD;
    unsigned qv = store(q);
    gc_protect(&qv);
    unsigned rv = store(r);
    gc_protect(&rv);
    return divrem_values(qv, rv);
}

unsigned prim_abs(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 1, 1, "abs");
    unsigned x = argv[0];
    if (IS_FIXNUM(x)) {
        int32_t n = FIXNUM_VALUE(x);
        return n < 0 ? store(-(int64_t)n) : store(n);
    }
    if (!IS_CELL(x)) {
        show_error("abs: not a real number");
        return TOK_ERROR;
    }
    switch (CELL_TYPE(x)) {
    case BT_NUM: {
        int64_t n = CELL_ID(x);
        return n < 0 ? negate_number(x) : x;
    }
    case BT_BIGNUM: {
        bignum *bn = get_bignum(x);
        if (!bn) {
            show_error("abs: invalid bignum");
            return TOK_ERROR;
        }
        bignum *result = bn_abs(bn);
        if (!result) {
            show_error("abs: out of memory");
            return TOK_ERROR;
        }
        return store_integer(result);
    }
    case BT_INEXACT: {
        double d = to_double(x);
        return store_inexact(fabs(d));
    }
    case BT_RATIONAL: {
        GC_GUARD;
        gc_protect(&x);
        unsigned num_cell = CELL_CAR(x);
        unsigned denom_cell = CELL_CDR(x);
        gc_protect(&num_cell);
        gc_protect(&denom_cell);
        if (is_negative_number(num_cell)) {
            num_cell = negate_number(num_cell);
            RETURN_IF_ERROR(num_cell);
            gc_protect(&num_cell);
        }
        return normalize_rational_cells(num_cell, denom_cell);
    }
    default:
        show_error("abs: not a real number");
        return TOK_ERROR;
    }
}
