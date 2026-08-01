/**
 * @file prim_numtower.c
 * @brief Numeric tower operations (complex, rational, exactness conversions)
 */

#include "prim_internal.h"

/**
 * Convert an inexact real number to an exact rational.
 * Uses IEEE754 representation: value = mantissa * 2^exponent
 */
static unsigned prim_inexact_to_exact(unsigned x)
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
        show_error("%s: inexact non-integer", name);
        return TOK_ERROR;
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

static unsigned numeric_real_test(unsigned x, real_test test)
{
    if (!is_numeric(x))
        return ctx.atom_false;

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
        double real = to_double(CELL_CAR(x));
        double imag = to_double(CELL_CDR(x));
        bool result = false;
        switch (test) {
        case REAL_TEST_FINITE:
            result = isfinite(real) && isfinite(imag);
            break;
        case REAL_TEST_INFINITE:
            result = isinf(real) || isinf(imag);
            break;
        case REAL_TEST_NAN:
            result = isnan(real) || isnan(imag);
            break;
        }
        return scheme_bool(result);
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
        return numeric_real_test(argv[0], real_test->test);
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
        // If imaginary part is zero, return just the real
        if (is_zero_number(imag))
            return real;
        return store_complex(real, imag);
    }
    case PMAKEPOLAR: {
        REQUIRE_ARGC(argc, 2, 2, "make-polar");
        if (!require_real(argv[0], "make-polar") ||
            !require_real(argv[1], "make-polar"))
            return TOK_ERROR;
        double mag = to_double(argv[0]);
        double ang = to_double(argv[1]);
        double real = mag * cos(ang);
        double imag = mag * sin(ang);
        if (fabs(imag) < 1e-15)
            imag = 0.0;
        return make_complex_inexact(real, imag);
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
            // Convert both parts to exact
            GC_GUARD;
            unsigned real_exact = prim_inexact_to_exact(CELL_CAR(x));
            gc_protect(&real_exact);
            unsigned imag_exact = prim_inexact_to_exact(CELL_CDR(x));
            return store_complex(real_exact, imag_exact);
        }

        return prim_inexact_to_exact(x);
    }
    case PRATIONALIZE: {
        REQUIRE_ARGC(argc, 2, 2, "rationalize");
        if (!require_real(argv[0], "rationalize") ||
            !require_real(argv[1], "rationalize"))
            return TOK_ERROR;
        // Find simplest rational within epsilon using continued fractions
        double x = to_double(argv[0]);
        double epsilon = fabs(to_double(argv[1]));

        // Handle negative numbers
        bool negative = x < 0;
        if (negative)
            x = -x;

        // Integer case
        if (!isfinite(x) || isnan(epsilon)) {
            show_error("rationalize: expected finite real arguments");
            return TOK_ERROR;
        }
        if (x >= 0x1p63) {
            if (negative && x == 0x1p63 && epsilon >= 0.0) {
                return store(INT64_MIN);
            }
            show_error("rationalize: magnitude too large");
            return TOK_ERROR;
        }
        int64_t n = (int64_t)floor(x);
        if (x - n <= epsilon) {
            return store(negative ? -n : n);
        }
        if (n + 1 - x <= epsilon) {
            return store(negative ? -(n + 1) : n + 1);
        }

        // Continued fraction approximation (Stern-Brocot)
        int64_t lo_n = 0, lo_d = 1; // 0/1
        int64_t hi_n = 1, hi_d = 0; // 1/0 = infinity
        int64_t mid_n, mid_d;

        for (int iter = 0; iter < 100; iter++) {
            if (__builtin_add_overflow(lo_n, hi_n, &mid_n) ||
                __builtin_add_overflow(lo_d, hi_d, &mid_d) || mid_d <= 0) {
                show_error("rationalize: result too large");
                return TOK_ERROR;
            }
            double mid = (double)mid_n / mid_d;

            if (fabs(mid - x) <= epsilon) {
                // Found it - return as rational
                if (negative)
                    mid_n = -mid_n;
                return normalize_rational(mid_n, mid_d);
            }

            if (mid < x) {
                lo_n = mid_n;
                lo_d = mid_d;
            } else {
                hi_n = mid_n;
                hi_d = mid_d;
            }
        }

        // Fallback: return best approximation found
        if (negative)
            mid_n = -mid_n;
        return normalize_rational(mid_n, mid_d);
    }
    default:
        return TOK_ERROR;
    }
}
