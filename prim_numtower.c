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
    if (d == floor(d) && d <= (double)INT64_MAX) {
        int64_t n = (int64_t)d;
        return store(negative ? -n : n);
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
        bignum *shifted = bn_lshift(bn, exp);
        bn_free(bn);
        if (int_mantissa < 0)
            shifted->sign = 1;
        return store_integer(shifted);
    } else {
        // Result is int_mantissa / 2^|exp| (a rational)
        // Simplify by removing common factors of 2
        int64_t num = int_mantissa;
        int64_t denom_exp = -exp;

        // Remove trailing zeros from numerator (common factors of 2)
        while ((num & 1) == 0 && denom_exp > 0) {
            num >>= 1;
            denom_exp--;
        }

        if (denom_exp == 0) {
            return store(num);
        } else if (denom_exp <= 62) {
            int64_t denom = 1LL << denom_exp;
            return normalize_rational(num, denom);
        } else {
            // Large denominator - use bignums
            unsigned numer = store(num);
            gc_protect(&numer);
            bignum *denom_bn = bn_from_int(1);
            bignum *shifted = bn_lshift(denom_bn, (int)denom_exp);
            bn_free(denom_bn);
            unsigned denom = store_integer(shifted);
            gc_unprotect(1);
            return normalize_rational_cells(numer, denom);
        }
    }
}

unsigned apply_numtower_primitive(unsigned prim_id, unsigned args)
{
    switch (prim_id) {
    case PNUMERATOR: {
        REQUIRE_ARGS(args, 1, 1, "numerator");
        unsigned x = car(args);
        switch (CELL_TYPE(x)) {
        case BT_NUM:
        case BT_BIGNUM:
            return x; // Integer is its own numerator
        case BT_RATIONAL:
            return CELL_CAR(x);
        case BT_INEXACT: {
            double d = to_double(x);
            if (floor(d) == d)
                return store_inexact(d);
            show_error("numerator: inexact non-integer");
            return TOK_ERROR;
        }
        default:
            show_error("numerator: not a rational");
            return TOK_ERROR;
        }
    }
    case PDENOMINATOR: {
        REQUIRE_ARGS(args, 1, 1, "denominator");
        unsigned x = car(args);
        switch (CELL_TYPE(x)) {
        case BT_NUM:
        case BT_BIGNUM:
            return store(1); // Integer has denominator 1
        case BT_RATIONAL:
            return CELL_CDR(x);
        case BT_INEXACT: {
            double d = to_double(x);
            if (floor(d) == d)
                return store_inexact(1.0);
            show_error("denominator: inexact non-integer");
            return TOK_ERROR;
        }
        default:
            show_error("denominator: not a rational");
            return TOK_ERROR;
        }
    }
    case PMAKERECT: {
        REQUIRE_ARGS(args, 2, 2, "make-rectangular");
        unsigned real = car(args);
        unsigned imag = cadr(args);
        // If imaginary part is zero, return just the real
        if (to_double(imag) == 0.0)
            return real;
        return store_complex(real, imag);
    }
    case PMAKEPOLAR: {
        REQUIRE_ARGS(args, 2, 2, "make-polar");
        double mag = to_double(car(args));
        double ang = to_double(cadr(args));
        double real = mag * cos(ang);
        double imag = mag * sin(ang);
        if (fabs(imag) < 1e-15)
            imag = 0.0;
        return make_complex_inexact(real, imag);
    }
    case PREALPART: {
        REQUIRE_ARGS(args, 1, 1, "real-part");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_COMPLEX) {
            return CELL_CAR(x);
        }
        return x; // Real numbers are their own real part
    }
    case PIMAGPART: {
        REQUIRE_ARGS(args, 1, 1, "imag-part");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_COMPLEX) {
            return CELL_CDR(x);
        }
        return store(0); // Real numbers have 0 imaginary part
    }
    case PMAGNITUDE: {
        REQUIRE_ARGS(args, 1, 1, "magnitude");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_COMPLEX) {
            double real = to_double(CELL_CAR(x));
            double imag = to_double(CELL_CDR(x));
            return store_inexact(sqrt(real * real + imag * imag));
        }
        // For real numbers, magnitude is abs
        double d = to_double(x);
        return is_exact(x) ? store((int64_t)fabs(d)) : store_inexact(fabs(d));
    }
    case PANGLE: {
        REQUIRE_ARGS(args, 1, 1, "angle");
        unsigned x = car(args);
        double real, imag;
        get_complex_parts(x, &real, &imag);
        return store_inexact(atan2(imag, real));
    }
    case PEXACT2INEXACT: {
        REQUIRE_ARGS(args, 1, 1, "exact->inexact");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_COMPLEX) {
            return make_complex_inexact(to_double(CELL_CAR(x)),
                                        to_double(CELL_CDR(x)));
        }
        return store_inexact(to_double(x));
    }
    case PINEXACT2EXACT: {
        REQUIRE_ARGS(args, 1, 1, "inexact->exact");
        unsigned x = car(args);

        // Already exact? Return as-is
        if (is_exact(x))
            return x;

        if (CELL_TYPE(x) == BT_COMPLEX) {
            // Convert both parts to exact
            unsigned real_exact = prim_inexact_to_exact(CELL_CAR(x));
            gc_protect(&real_exact);
            unsigned imag_exact = prim_inexact_to_exact(CELL_CDR(x));
            gc_unprotect(1);
            return store_complex(real_exact, imag_exact);
        }

        return prim_inexact_to_exact(x);
    }
    case PRATIONALIZE: {
        REQUIRE_ARGS(args, 2, 2, "rationalize");
        // Find simplest rational within epsilon using continued fractions
        double x = to_double(car(args));
        double epsilon = fabs(to_double(cadr(args)));

        // Handle negative numbers
        bool negative = x < 0;
        if (negative)
            x = -x;

        // Integer case
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
            mid_n = lo_n + hi_n;
            mid_d = lo_d + hi_d;
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
    case PFINITE: {
        REQUIRE_ARGS(args, 1, 1, "finite?");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_INEXACT) {
            double d = to_double(x);
            return isfinite(d) ? ctx.atom_true : 0;
        }
        if (CELL_TYPE(x) == BT_COMPLEX) {
            double real = to_double(CELL_CAR(x));
            double imag = to_double(CELL_CDR(x));
            return (isfinite(real) && isfinite(imag)) ? ctx.atom_true : 0;
        }
        // All exact numbers are finite
        return ctx.atom_true;
    }
    case PINFINITE: {
        REQUIRE_ARGS(args, 1, 1, "infinite?");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_INEXACT) {
            double d = to_double(x);
            return isinf(d) ? ctx.atom_true : 0;
        }
        if (CELL_TYPE(x) == BT_COMPLEX) {
            double real = to_double(CELL_CAR(x));
            double imag = to_double(CELL_CDR(x));
            return (isinf(real) || isinf(imag)) ? ctx.atom_true : 0;
        }
        // Exact numbers are never infinite
        return 0;
    }
    case PNAN: {
        REQUIRE_ARGS(args, 1, 1, "nan?");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_INEXACT) {
            double d = to_double(x);
            return isnan(d) ? ctx.atom_true : 0;
        }
        if (CELL_TYPE(x) == BT_COMPLEX) {
            double real = to_double(CELL_CAR(x));
            double imag = to_double(CELL_CDR(x));
            return (isnan(real) || isnan(imag)) ? ctx.atom_true : 0;
        }
        // Exact numbers are never NaN
        return 0;
    }
    default:
        return TOK_ERROR;
    }
}
