/**
 * @file prim_numtower.c
 * @brief Numeric tower operations (complex, rational, exactness conversions)
 */

#include "prim_internal.h"

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
        if (CELL_TYPE(x) == BT_COMPLEX) {
            double real = to_double(CELL_CAR(x));
            double imag = to_double(CELL_CDR(x));
            unsigned real_part = store((int64_t)round(real));
            gc_protect(&real_part);
            unsigned imag_part = store((int64_t)round(imag));
            gc_unprotect(1);
            return store_complex(real_part, imag_part);
        }
        double d = to_double(x);
        return store((int64_t)round(d));
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
