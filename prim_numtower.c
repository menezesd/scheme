/**
 * @file prim_numtower.c
 * @brief Numeric tower operations (complex, rational, exactness conversions)
 */

#include "prim_internal.h"

// Apply rationalize's exactness contagion to a computed exact result
static unsigned rationalize_result(unsigned value, bool inexact)
{
    if (value == TOK_ERROR || !inexact)
        return value;
    GC_GUARD;
    gc_protect(&value);
    return store_inexact(to_double(value));
}

static bool exact_real_negative(unsigned value)
{
    return is_negative_number(IS_RATIONAL(value) ? CELL_CAR(value) : value);
}

// Find the simplest rational in a closed positive interval by removing its
// common continued-fraction terms. Reciprocal steps reverse the endpoints;
// an integer in the interval ends the search. Unlike a mediant-at-a-time
// search this also handles intervals near 1/N without taking N iterations.
static unsigned simplest_positive_rational(unsigned low, unsigned high)
{
    GC_GUARD;
    gc_protect(&low);
    gc_protect(&high);
    unsigned terms = 0, a = 0, b = 0, value = 0, low_fraction = 0;
    gc_protect(&terms);
    gc_protect(&a);
    gc_protect(&b);
    gc_protect(&value);
    gc_protect(&low_fraction);
    unsigned one = store(1);
    gc_protect(&one);

    for (;;) {
        if (IS_EXACT_INT(low)) {
            value = low;
            break;
        }
        a = apply_math_primitive(PFLOOR, 1, &low);
        if (a == TOK_ERROR)
            return TOK_ERROR;
        b = apply_math_primitive(PFLOOR, 1, &high);
        if (b == TOK_ERROR)
            return TOK_ERROR;
        int cmp;
        if (!compare_exact_integers(a, b, &cmp)) {
            show_error("rationalize: invalid interval");
            return TOK_ERROR;
        }
        if (cmp < 0) {
            value = add_cells(a, one);
            if (value == TOK_ERROR)
                return TOK_ERROR;
            break;
        }

        terms = alloc_cons(a, terms);
        low_fraction = binary_sub(low, a);
        if (low_fraction == TOK_ERROR)
            return TOK_ERROR;
        high = binary_sub(high, a);
        if (high == TOK_ERROR)
            return TOK_ERROR;
        low = binary_div(one, high);
        if (low == TOK_ERROR)
            return TOK_ERROR;
        high = binary_div(one, low_fraction);
        if (high == TOK_ERROR)
            return TOK_ERROR;
    }

    for (; terms; terms = cdr(terms)) {
        value = binary_div(one, value);
        if (value == TOK_ERROR)
            return TOK_ERROR;
        value = binary_add(car(terms), value);
        if (value == TOK_ERROR)
            return TOK_ERROR;
    }
    return value;
}

static unsigned rationalize_value(unsigned x, unsigned tolerance)
{
    if (!require_real(x, "rationalize") ||
        !require_real(tolerance, "rationalize"))
        return TOK_ERROR;
    bool inexact_result = !is_exact(x) || !is_exact(tolerance);
    if ((IS_INEXACT(x) && !isfinite(to_double(x))) ||
        (IS_INEXACT(tolerance) && isnan(to_double(tolerance)))) {
        show_error("rationalize: expected finite real arguments");
        return TOK_ERROR;
    }
    if (IS_INEXACT(tolerance) && isinf(to_double(tolerance)))
        return store_inexact(0.0);

    GC_GUARD;
    gc_protect(&x);
    gc_protect(&tolerance);
    // Compute the endpoints exactly, including for inexact inputs. Rounding
    // x to double would lose exact integers beyond 53 bits before the search.
    if (IS_INEXACT(x)) {
        x = prim_inexact_to_exact(x);
        if (x == TOK_ERROR)
            return TOK_ERROR;
    }
    if (IS_INEXACT(tolerance)) {
        tolerance = prim_inexact_to_exact(tolerance);
        if (tolerance == TOK_ERROR)
            return TOK_ERROR;
    }
    if (exact_real_negative(tolerance)) {
        tolerance = binary_sub(store(0), tolerance);
        if (tolerance == TOK_ERROR)
            return TOK_ERROR;
    }
    unsigned low = binary_sub(x, tolerance);
    if (low == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&low);
    unsigned high = binary_add(x, tolerance);
    if (high == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&high);

    if (!exact_real_negative(high) &&
        (exact_real_negative(low) || is_zero_number(low)))
        return rationalize_result(store(0), inexact_result);

    bool negative = exact_real_negative(high);
    if (negative) {
        unsigned old_low = low;
        gc_protect(&old_low);
        low = binary_sub(store(0), high);
        if (low == TOK_ERROR)
            return TOK_ERROR;
        high = binary_sub(store(0), old_low);
        gc_unprotect(1);
        if (high == TOK_ERROR)
            return TOK_ERROR;
    }
    unsigned result = simplest_positive_rational(low, high);
    if (result == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&result);
    if (negative) {
        result = binary_sub(store(0), result);
        if (result == TOK_ERROR)
            return TOK_ERROR;
    }
    return rationalize_result(result, inexact_result);
}

/**
 * Convert an inexact real number to an exact rational.
 * Uses IEEE754 representation: value = mantissa * 2^exponent
 */
unsigned prim_inexact_to_exact(unsigned x)
{
    double d = to_double(x);

    // Handle special cases
    if (isnan(d) || isinf(d)) {
        show_error("inexact->exact: no exact representation for inf/nan");
        return TOK_ERROR;
    }

    // Zero
    if (d == 0.0)
        return store(0);

    // Handle negative
    bool negative = d < 0;
    if (negative)
        d = -d;

    // If it's a whole number that fits in int64, return as integer
    if (d == floor(d)) {
        const double int64_min_magnitude = 0x1p63;
        if (d < int64_min_magnitude) {
            int64_t n = (int64_t)d;
            return store(negative ? -n : n);
        }
        if (negative && d == int64_min_magnitude) {
            return store(INT64_MIN);
        }
    }

    // Extract IEEE754 components: d = mantissa * 2^exp where 1 <= mantissa < 2
    int exp;
    double mantissa = frexp(d, &exp);
    // frexp returns: d = mantissa * 2^exp where 0.5 <= |mantissa| < 1
    // Multiply by 2^53 to get the exact integer mantissa
    int64_t int_mantissa = (int64_t)(mantissa * (1LL << 53));
    exp -= 53;

    // Now d = int_mantissa * 2^exp exactly
    if (negative)
        int_mantissa = -int_mantissa;

    if (exp >= 0) {
        // Result is int_mantissa * 2^exp (an integer)
        // Use bignum for large shifts
        bignum *bn =
            bn_from_int(int_mantissa < 0 ? -int_mantissa : int_mantissa);
        bignum *shifted = bn ? bn_lshift(bn, exp) : NULL;
        bn_free(bn);
        if (!shifted) {
            show_error("inexact->exact: out of memory");
            return TOK_ERROR;
        }
        if (int_mantissa < 0)
            shifted->sign = 1;
        return store_integer(shifted);
    } else {
        // Result is int_mantissa / 2^|exp| (a rational)
        // Simplify by removing common factors of 2
        int64_t num = int_mantissa;
        int64_t denom_exp = -exp;

        // Remove trailing zeros from numerator (common factors of 2)
        while (num % 2 == 0 && denom_exp > 0) {
            num /= 2;
            denom_exp--;
        }

        if (denom_exp == 0) {
            return store(num);
        } else if (denom_exp <= 62) {
            uint64_t denom_u = UINT64_C(1) << (unsigned)denom_exp;
            int64_t denom = (int64_t)denom_u;
            return normalize_rational(num, denom);
        } else {
            // Large denominator - use bignums
            GC_GUARD;
            unsigned numer = store(num);
            gc_protect(&numer);
            bignum *denom_bn = bn_from_int(1);
            bignum *shifted =
                denom_bn ? bn_lshift(denom_bn, (int)denom_exp) : NULL;
            bn_free(denom_bn);
            if (!shifted) {
                show_error("inexact->exact: out of memory");
                return TOK_ERROR;
            }
            unsigned denom = store_integer(shifted);
            return normalize_rational_cells(numer, denom);
        }
    }
}

static unsigned rational_component(unsigned x, bool numerator, const char *name)
{
    if (!is_numeric(x)) {
        show_error("%s: not a rational", name);
        return TOK_ERROR;
    }
    if (IS_FIXNUM(x))
        return numerator ? x : store(1);

    switch (CELL_TYPE(x)) {
    case BT_NUM:
    case BT_BIGNUM:
        return numerator ? x : store(1);
    case BT_RATIONAL:
        return numerator ? CELL_CAR(x) : CELL_CDR(x);
    case BT_INEXACT: {
        double d = to_double(x);
        if (isfinite(d) && floor(d) == d)
            return numerator ? store_inexact(d) : store_inexact(1.0);
        if (!isfinite(d)) {
            show_error("%s: no rational representation", name);
            return TOK_ERROR;
        }
        // R7RS: convert to exact, take the component, convert back
        // (e.g. (denominator (inexact 6/4)) => 2.0)
        GC_GUARD;
        unsigned exact = prim_inexact_to_exact(x);
        if (exact == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&exact);
        unsigned part;
        if (IS_RATIONAL(exact))
            part = numerator ? CELL_CAR(exact) : CELL_CDR(exact);
        else
            part = numerator ? exact : store(1);
        gc_protect(&part);
        return store_inexact(to_double(part));
    }
    default:
        show_error("%s: not a rational", name);
        return TOK_ERROR;
    }
}

typedef enum {
    REAL_TEST_FINITE,
    REAL_TEST_INFINITE,
    REAL_TEST_NAN,
} real_test;

typedef struct {
    unsigned id;
    real_test test;
    const char *name;
} real_test_entry;

static const real_test_entry real_tests[] = {
    {PFINITE, REAL_TEST_FINITE, "finite?"},
    {PINFINITE, REAL_TEST_INFINITE, "infinite?"},
    {PNAN, REAL_TEST_NAN, "nan?"},
    {0, REAL_TEST_FINITE, NULL},
};

static const real_test_entry *find_real_test(unsigned prim_id)
{
    for (const real_test_entry *entry = real_tests; entry->name; entry++) {
        if (entry->id == prim_id)
            return entry;
    }
    return NULL;
}

static unsigned numeric_real_test(unsigned x, real_test test,
                                  const char *name)
{
    // R7RS 6.2.6 defines finite?/infinite?/nan? over numbers only. Answering
    // #f for a non-number would report "this symbol is not infinite", hiding
    // the type error behind a plausible-looking answer.
    if (!is_numeric(x)) {
        show_error("%s: not a number", name);
        return TOK_ERROR;
    }

    if (IS_INEXACT(x)) {
        double d = to_double(x);
        bool result = false;
        switch (test) {
        case REAL_TEST_FINITE:
            result = isfinite(d);
            break;
        case REAL_TEST_INFINITE:
            result = isinf(d);
            break;
        case REAL_TEST_NAN:
            result = isnan(d);
            break;
        }
        return scheme_bool(result);
    }

    if (IS_COMPLEX(x)) {
        // Exact components are finite regardless of whether they fit in a
        // double. Classify each component before combining the results.
        unsigned real = numeric_real_test(CELL_CAR(x), test, name);
        unsigned imag = numeric_real_test(CELL_CDR(x), test, name);
        if (real == TOK_ERROR || imag == TOK_ERROR)
            return TOK_ERROR;
        bool r = real == ctx.atom_true, i = imag == ctx.atom_true;
        return scheme_bool(test == REAL_TEST_FINITE ? r && i : r || i);
    }

    return scheme_bool(test == REAL_TEST_FINITE);
}

static unsigned complex_part(unsigned x, bool real_part, const char *name)
{
    if (!require_number(x, name))
        return TOK_ERROR;
    if (IS_COMPLEX(x))
        return real_part ? CELL_CAR(x) : CELL_CDR(x);
    return real_part ? x : store(0);
}

static unsigned magnitude_value(unsigned x, const char *name)
{
    if (!require_number(x, name))
        return TOK_ERROR;
    if (IS_FIXNUM(x)) {
        int32_t n = FIXNUM_VALUE(x);
        return n < 0 ? store(-(int64_t)n) : store(n);
    }
    if (IS_COMPLEX(x)) {
        double real = to_double(CELL_CAR(x));
        double imag = to_double(CELL_CDR(x));
        return store_inexact(hypot(real, imag));
    }
    if (IS_NUM(x))
        return CELL_ID(x) < 0 ? negate_number(x) : x;
    if (IS_BIGNUM(x)) {
        bignum *bn = get_bignum(x);
        if (bn->sign) {
            bignum *abs_bn = bn_neg(bn);
            if (!abs_bn) {
                show_error("%s: out of memory", name);
                return TOK_ERROR;
            }
            return store_integer(abs_bn);
        }
        return x;
    }
    if (IS_RATIONAL(x)) {
        unsigned num = CELL_CAR(x);
        if (!is_negative_number(num))
            return x;
        GC_GUARD;
        gc_protect(&x);
        unsigned abs_num = negate_number(num);
        if (abs_num == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&abs_num);
        unsigned denom = CELL_CDR(x);
        return normalize_rational_cells(abs_num, denom);
    }
    return store_inexact(fabs(to_double(x)));
}

unsigned apply_numtower_primitive(unsigned prim_id, unsigned argc,
                                  unsigned *argv)
{
    const real_test_entry *real_test = find_real_test(prim_id);
    if (real_test) {
        REQUIRE_ARGC(argc, 1, 1, real_test->name);
        return numeric_real_test(argv[0], real_test->test, real_test->name);
    }

    switch (prim_id) {
    case PNUMERATOR: {
        REQUIRE_ARGC(argc, 1, 1, "numerator");
        return rational_component(argv[0], true, "numerator");
    }
    case PDENOMINATOR: {
        REQUIRE_ARGC(argc, 1, 1, "denominator");
        return rational_component(argv[0], false, "denominator");
    }
    case PMAKERECT: {
        REQUIRE_ARGC(argc, 2, 2, "make-rectangular");
        unsigned real = argv[0];
        unsigned imag = argv[1];
        if (!require_real(real, "make-rectangular") ||
            !require_real(imag, "make-rectangular"))
            return TOK_ERROR;
        // Only an EXACT zero imaginary part collapses to just the real
        // part (R7RS/MIT: (make-rectangular 3 0.0) is a genuine inexact
        // complex 3+0.i, not the real number 3 - the imaginary part's
        // inexactness is observable, e.g. via (exact? ...)).
        if (is_exact(imag) && is_zero_number(imag))
            return real;
        return store_complex(real, imag);
    }
    case PMAKEPOLAR: {
        REQUIRE_ARGC(argc, 2, 2, "make-polar");
        if (!require_real(argv[0], "make-polar") ||
            !require_real(argv[1], "make-polar"))
            return TOK_ERROR;
        return make_polar_number(argv[0], argv[1]);
    }
    case PREALPART: {
        REQUIRE_ARGC(argc, 1, 1, "real-part");
        return complex_part(argv[0], true, "real-part");
    }
    case PIMAGPART: {
        REQUIRE_ARGC(argc, 1, 1, "imag-part");
        return complex_part(argv[0], false, "imag-part");
    }
    case PMAGNITUDE: {
        REQUIRE_ARGC(argc, 1, 1, "magnitude");
        return magnitude_value(argv[0], "magnitude");
    }
    case PANGLE: {
        REQUIRE_ARGC(argc, 1, 1, "angle");
        if (!require_number(argv[0], "angle"))
            return TOK_ERROR;
        unsigned x = argv[0];
        double real, imag;
        get_complex_parts(x, &real, &imag);
        return store_inexact(atan2(imag, real));
    }
    case PEXACT2INEXACT: {
        REQUIRE_ARGC(argc, 1, 1, "exact->inexact");
        unsigned x = argv[0];
        if (!require_number(x, "exact->inexact"))
            return TOK_ERROR;
        if (!is_exact(x))
            return x;
        if (IS_COMPLEX(x)) {
            return make_complex_inexact(to_double(CELL_CAR(x)),
                                        to_double(CELL_CDR(x)));
        }
        return store_inexact(to_double(x));
    }
    case PINEXACT2EXACT: {
        REQUIRE_ARGC(argc, 1, 1, "inexact->exact");
        unsigned x = argv[0];
        if (!require_number(x, "inexact->exact"))
            return TOK_ERROR;

        // Already exact? Return as-is
        if (is_exact(x))
            return x;

        if (IS_COMPLEX(x)) {
            // A complex number can mix exact and inexact components. Keep
            // exact components intact and root x while converting the real
            // part, since conversion can collect before we read the other.
            GC_GUARD;
            gc_protect(&x);
            unsigned real_exact = CELL_CAR(x);
            if (!is_exact(real_exact))
                real_exact = prim_inexact_to_exact(real_exact);
            if (real_exact == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&real_exact);
            unsigned imag_exact = CELL_CDR(x);
            if (!is_exact(imag_exact))
                imag_exact = prim_inexact_to_exact(imag_exact);
            if (imag_exact == TOK_ERROR)
                return TOK_ERROR;
            return store_complex(real_exact, imag_exact);
        }

        return prim_inexact_to_exact(x);
    }
    case PRATIONALIZE: {
        REQUIRE_ARGC(argc, 2, 2, "rationalize");
        return rationalize_value(argv[0], argv[1]);
    }
    default:
        return TOK_ERROR;
    }
}
