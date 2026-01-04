/**
 * @file prim_math.c
 * @brief Math functions (sqrt, sin, cos, exp, log, floor, ceiling, etc.)
 *        and random number generation (SRFI-27 style)
 *
 * Implements transcendental and mathematical functions:
 *
 * ## Trigonometric
 * sin, cos, tan, asin, acos, atan (including 2-argument atan)
 *
 * ## Exponential/Logarithmic
 * exp, log, expt (power function)
 *
 * ## Rounding
 * floor, ceiling, truncate, round
 *
 * ## Other
 * sqrt, abs (delegated from prim_numeric)
 *
 * ## Random Numbers (SRFI-27 compatible)
 * Uses xoshiro256** PRNG for high-quality randomness:
 * - random-integer: Random integer in [0, n)
 * - random-real: Random float in [0, 1)
 * - random-seed!: Seed the generator
 *
 * Results are inexact except where input allows exact result
 * (e.g., expt with integer arguments).
 */

#define _USE_MATH_DEFINES
#include "prim_internal.h"
#include <time.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// xoshiro256** PRNG - fast, high-quality random number generator
// Based on: https://prng.di.unimi.it/
static uint64_t rng_state[4] = {0x853c49e6748fea9bULL, 0xda3e39cb94b95bdbULL,
                                0x647c4677a2884327ULL, 0xc3f5015f73e1f6f4ULL};

static inline uint64_t rotl(uint64_t x, int k)
{
    return (x << k) | (x >> (64 - k));
}

static uint64_t xoshiro256ss(void)
{
    uint64_t result = rotl(rng_state[1] * 5, 7) * 9;
    uint64_t t = rng_state[1] << 17;
    rng_state[2] ^= rng_state[0];
    rng_state[3] ^= rng_state[1];
    rng_state[1] ^= rng_state[2];
    rng_state[0] ^= rng_state[3];
    rng_state[2] ^= t;
    rng_state[3] = rotl(rng_state[3], 45);
    return result;
}

// Math function table for simple unary functions
typedef struct {
    unsigned id;
    double (*func)(double);
    const char *name;
} math_func_entry;

static const math_func_entry math_funcs[] = {
    {PASIN, asin, "asin"}, {PACOS, acos, "acos"}, {0, NULL, NULL}};

/**
 * Compute integer nth root of a bignum using Newton's method.
 * Returns the root if base is a perfect nth power, NULL otherwise.
 * Caller must free the returned bignum.
 */
static bignum *bn_exact_nth_root(const bignum *base, int64_t n)
{
    if (n <= 0 || bn_is_zero(base))
        return NULL;
    if (n == 1)
        return bn_copy(base);
    if (base->sign)
        return NULL; // No real nth root of negative for even n

    // Initial guess using floating point
    double d = 0;
    for (size_t i = base->len; i > 0; i--) {
        d = d * 4294967296.0 + base->limbs[i - 1];
    }
    double guess_d = pow(d, 1.0 / n);
    bignum *guess = bn_from_int((int64_t)guess_d);
    if (bn_is_zero(guess)) {
        bn_free(guess);
        guess = bn_from_int(1);
    }

    // Newton iteration: x_new = ((n-1)*x + base/x^(n-1)) / n
    bignum *n_bn = bn_from_int(n);
    bignum *n_minus_1 = bn_from_int(n - 1);

    for (int iter = 0; iter < 100; iter++) {
        // Compute guess^(n-1)
        bignum *power = bn_from_int(1);
        for (int64_t i = 0; i < n - 1; i++) {
            bignum *temp = bn_mul(power, guess);
            bn_free(power);
            power = temp;
        }

        // Compute base / guess^(n-1)
        bignum *quotient = bn_div(base, power, NULL);
        bn_free(power);

        // Compute (n-1) * guess
        bignum *term1 = bn_mul(n_minus_1, guess);

        // Sum: (n-1)*guess + base/guess^(n-1)
        bignum *sum = bn_add(term1, quotient);
        bn_free(term1);
        bn_free(quotient);

        // New guess: sum / n
        bignum *new_guess = bn_div(sum, n_bn, NULL);
        bn_free(sum);

        // Check convergence
        int cmp = bn_cmp(new_guess, guess);
        if (cmp == 0) {
            bn_free(new_guess);
            break;
        }

        bn_free(guess);
        guess = new_guess;
    }

    bn_free(n_bn);
    bn_free(n_minus_1);

    // Verify: guess^n == base
    bignum *check = bn_from_int(1);
    for (int64_t i = 0; i < n; i++) {
        bignum *temp = bn_mul(check, guess);
        bn_free(check);
        check = temp;
    }

    if (bn_cmp(check, base) == 0) {
        bn_free(check);
        return guess; // Perfect nth power
    }

    bn_free(check);
    bn_free(guess);
    return NULL; // Not a perfect nth power
}

unsigned apply_math_primitive(unsigned prim_id, unsigned args)
{
    // Check simple unary functions via table
    for (const math_func_entry *e = math_funcs; e->func; e++) {
        if (e->id == prim_id) {
            REQUIRE_ARGS(args, 1, 1, e->name);
            return store_inexact(e->func(to_double(car(args))));
        }
    }

    switch (prim_id) {
    case PSQRT: {
        // Complex sqrt: sqrt(a+bi) = sqrt((r+a)/2) + i*sign(b)*sqrt((r-a)/2)
        // where r = |a+bi| = sqrt(a^2+b^2)
        REQUIRE_ARGS(args, 1, 1, "sqrt");
        unsigned arg = car(args);
        enum lisp_type t = CELL_TYPE(arg);

        // Handle complex numbers
        if (t == BT_COMPLEX) {
            double a, b;
            get_complex_parts(arg, &a, &b);
            double r = sqrt(a * a + b * b);
            double real = sqrt((r + a) / 2);
            double imag = (b >= 0 ? 1 : -1) * sqrt((r - a) / 2);
            return make_complex_inexact(real, imag);
        }

        // Handle exact rationals: sqrt(a/b) = sqrt(a)/sqrt(b) if both perfect
        // squares
        if (t == BT_RATIONAL) {
            int64_t num, denom;
            get_rational_parts(arg, &num, &denom);
            if (num >= 0 && denom > 0) {
                int64_t sqrt_num = (int64_t)sqrt((double)num);
                int64_t sqrt_denom = (int64_t)sqrt((double)denom);
                // Verify both are perfect squares
                if (sqrt_num * sqrt_num == num &&
                    sqrt_denom * sqrt_denom == denom) {
                    return store_rational(sqrt_num, sqrt_denom);
                }
            }
            // Fall through to inexact
        }

        // Handle exact integers: return exact if perfect square
        if (t == BT_NUM) {
            int64_t n = CELL_ID(arg);
            if (n >= 0) {
                int64_t s = (int64_t)sqrt((double)n);
                if (s * s == n) {
                    return store(s);
                }
            }
        }

        double x = to_double(arg);
        if (x < 0) {
            return store_complex(store(0), store_inexact(sqrt(-x)));
        }
        return store_inexact(sqrt(x));
    }
    case PEXPT: {
        // z^w = exp(w * log(z)) for complex numbers
        REQUIRE_ARGS(args, 2, 2, "expt");
        unsigned base_arg = car(args);
        unsigned exp_arg = cadr(args);

        // Exact base with exact integer exponent -> exact result
        // Covers integers, rationals, and complex with exact parts
        if (is_exact(base_arg) && IS_EXACT_INT(exp_arg)) {
            int64_t exp_val;
            if (IS_NUM(exp_arg)) {
                exp_val = CELL_ID(exp_arg);
            } else {
                // Bignum exponent - check if it fits in int64
                bignum *exp_bn = get_bignum(exp_arg);
                if (bn_to_int64(exp_bn, &exp_val) != 0) {
                    // Exponent too large - fall through to inexact
                    goto inexact_expt;
                }
            }

            // Use repeated squaring with exact arithmetic
            // result = base^|exp|, then invert if exp < 0
            bool neg_exp = exp_val < 0;
            uint64_t e = neg_exp ? (uint64_t)(-exp_val) : (uint64_t)exp_val;

            GC_GUARD;
            unsigned result = store(1);
            unsigned base = base_arg;
            gc_protect(&result);
            gc_protect(&base);

            while (e > 0) {
                if (e & 1) {
                    // result = result * base (exact multiplication)
                    unsigned mul_args = alloc_cons(result, alloc_cons(base, 0));
                    result = prim_mult(mul_args);
                    if (result == TOK_ERROR) {
                        return TOK_ERROR;
                    }
                }
                e >>= 1;
                if (e > 0) {
                    // base = base * base
                    unsigned sq_args = alloc_cons(base, alloc_cons(base, 0));
                    base = prim_mult(sq_args);
                    if (base == TOK_ERROR) {
                        return TOK_ERROR;
                    }
                }
            }

            if (neg_exp) {
                // Return 1 / result
                unsigned one = store(1);
                gc_protect(&one);
                unsigned div_args = alloc_cons(one, alloc_cons(result, 0));
                return prim_div(div_args);
            }
            return result;
        }

        // Exact integer or rational base with exact rational exponent p/q
        // Try to compute exact (base^(1/q))^p if base is a perfect qth power
        if ((IS_EXACT_INT(base_arg) || CELL_TYPE(base_arg) == BT_RATIONAL) &&
            CELL_TYPE(exp_arg) == BT_RATIONAL) {
            int64_t exp_numer, exp_denom;
            get_rational_parts(exp_arg, &exp_numer, &exp_denom);

            // Only handle small roots (avoid expensive computation)
            if (exp_denom > 0 && exp_denom <= 1000) {
                bignum *num_root = NULL;
                bignum *den_root = NULL;

                if (IS_EXACT_INT(base_arg)) {
                    // Integer base - denominator is implicitly 1
                    bignum *base_bn = to_bignum(base_arg);
                    num_root = bn_exact_nth_root(base_bn, exp_denom);
                    bn_free(base_bn);
                    if (num_root)
                        den_root = bn_from_int(1);
                } else {
                    // Rational base - check both numerator and denominator
                    unsigned base_num = CELL_CAR(base_arg);
                    unsigned base_den = CELL_CDR(base_arg);
                    bignum *num_bn = to_bignum(base_num);
                    bignum *den_bn = to_bignum(base_den);
                    num_root = bn_exact_nth_root(num_bn, exp_denom);
                    if (num_root)
                        den_root = bn_exact_nth_root(den_bn, exp_denom);
                    bn_free(num_bn);
                    bn_free(den_bn);
                }

                if (num_root && den_root) {
                    // Both are perfect qth powers
                    // Now compute (num_root/den_root)^|exp_numer|
                    bool neg_exp = exp_numer < 0;
                    uint64_t p =
                        neg_exp ? (uint64_t)(-exp_numer) : (uint64_t)exp_numer;

                    bignum *num_power = bn_from_int(1);
                    bignum *den_power = bn_from_int(1);
                    for (uint64_t i = 0; i < p; i++) {
                        bignum *temp = bn_mul(num_power, num_root);
                        bn_free(num_power);
                        num_power = temp;
                        temp = bn_mul(den_power, den_root);
                        bn_free(den_power);
                        den_power = temp;
                    }
                    bn_free(num_root);
                    bn_free(den_root);

                    GC_GUARD;
                    unsigned result_num = store_integer(num_power);
                    gc_protect(&result_num);
                    unsigned result_den = store_integer(den_power);

                    if (neg_exp) {
                        // Swap numerator and denominator
                        unsigned temp = result_num;
                        result_num = result_den;
                        result_den = temp;
                    }

                    return normalize_rational_cells(result_num, result_den);
                }

                if (num_root)
                    bn_free(num_root);
                if (den_root)
                    bn_free(den_root);
            }
        }

    inexact_expt:;
        bool base_complex = CELL_TYPE(base_arg) == BT_COMPLEX;
        bool exp_complex = CELL_TYPE(exp_arg) == BT_COMPLEX;

        // If base is negative real and exponent is non-integer, we need complex
        double base_d = to_double(base_arg);
        double exp_d = to_double(exp_arg);
        bool needs_complex = base_complex || exp_complex ||
                             (base_d < 0 && floor(exp_d) != exp_d);

        if (needs_complex) {
            // z^w = exp(w * log(z))
            double b_real, b_imag;
            get_complex_parts(base_arg, &b_real, &b_imag);

            // log(z)
            double mag = sqrt(b_real * b_real + b_imag * b_imag);
            double angle = atan2(b_imag, b_real);
            double log_real = log(mag);
            double log_imag = angle;

            // w * log(z)
            double e_real, e_imag;
            get_complex_parts(exp_arg, &e_real, &e_imag);
            double prod_real = e_real * log_real - e_imag * log_imag;
            double prod_imag = e_real * log_imag + e_imag * log_real;

            // exp(w * log(z))
            double r_mag = exp(prod_real);
            return make_complex_inexact(r_mag * cos(prod_imag),
                                        r_mag * sin(prod_imag));
        }

        double result = pow(base_d, exp_d);
        return store_inexact(result);
    }
    case PATAN: {
        REQUIRE_ARGS(args, 1, 2, "atan");
        double y = to_double(car(args));
        if (cdr(args)) {
            double x = to_double(cadr(args));
            return store_inexact(atan2(y, x));
        }
        return store_inexact(atan(y));
    }
    case PLOG: {
        // Complex logarithm: log(z) = ln|z| + i*arg(z)
        REQUIRE_ARGS(args, 1, 1, "log");
        unsigned arg = car(args);

        if (CELL_TYPE(arg) == BT_COMPLEX) {
            double real, imag;
            get_complex_parts(arg, &real, &imag);
            double mag = sqrt(real * real + imag * imag);
            double angle = atan2(imag, real);
            return make_complex_inexact(log(mag), angle);
        }

        double x = to_double(arg);
        if (x < 0) {
            // log of negative real: log(x) = ln|x| + i*pi
            return make_complex_inexact(log(-x), M_PI);
        }
        return store_inexact(log(x));
    }
    case PEXP: {
        // Complex exp: e^(a+bi) = e^a * (cos(b) + i*sin(b))
        REQUIRE_ARGS(args, 1, 1, "exp");
        unsigned arg = car(args);

        if (CELL_TYPE(arg) == BT_COMPLEX) {
            double real, imag;
            get_complex_parts(arg, &real, &imag);
            double mag = exp(real);
            return make_complex_inexact(mag * cos(imag), mag * sin(imag));
        }
        return store_inexact(exp(to_double(arg)));
    }
    case PSIN: {
        // Complex sin: sin(a+bi) = sin(a)*cosh(b) + i*cos(a)*sinh(b)
        REQUIRE_ARGS(args, 1, 1, "sin");
        unsigned arg = car(args);

        if (CELL_TYPE(arg) == BT_COMPLEX) {
            double a, b;
            get_complex_parts(arg, &a, &b);
            return make_complex_inexact(sin(a) * cosh(b), cos(a) * sinh(b));
        }
        return store_inexact(sin(to_double(arg)));
    }
    case PCOS: {
        // Complex cos: cos(a+bi) = cos(a)*cosh(b) - i*sin(a)*sinh(b)
        REQUIRE_ARGS(args, 1, 1, "cos");
        unsigned arg = car(args);

        if (CELL_TYPE(arg) == BT_COMPLEX) {
            double a, b;
            get_complex_parts(arg, &a, &b);
            return make_complex_inexact(cos(a) * cosh(b), -sin(a) * sinh(b));
        }
        return store_inexact(cos(to_double(arg)));
    }
    case PTAN: {
        // Complex tan: tan(z) = sin(z)/cos(z)
        REQUIRE_ARGS(args, 1, 1, "tan");
        unsigned arg = car(args);

        if (CELL_TYPE(arg) == BT_COMPLEX) {
            double a, b;
            get_complex_parts(arg, &a, &b);
            // tan(a+bi) = (sin(2a) + i*sinh(2b)) / (cos(2a) + cosh(2b))
            double denom = cos(2 * a) + cosh(2 * b);
            return make_complex_inexact(sin(2 * a) / denom,
                                        sinh(2 * b) / denom);
        }
        return store_inexact(tan(to_double(arg)));
    }
    case PFLOOR: {
        REQUIRE_ARGS(args, 1, 1, "floor");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_NUM)
            return x;
        double d = to_double(x);
        return is_exact(x) ? store((int64_t)floor(d)) : store_inexact(floor(d));
    }
    case PCEILING: {
        REQUIRE_ARGS(args, 1, 1, "ceiling");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_NUM)
            return x;
        double d = to_double(x);
        return is_exact(x) ? store((int64_t)ceil(d)) : store_inexact(ceil(d));
    }
    case PTRUNCATE: {
        REQUIRE_ARGS(args, 1, 1, "truncate");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_NUM)
            return x;
        double d = to_double(x);
        return is_exact(x) ? store((int64_t)trunc(d)) : store_inexact(trunc(d));
    }
    case PROUND: {
        REQUIRE_ARGS(args, 1, 1, "round");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_NUM)
            return x;
        double d = to_double(x);
        return is_exact(x) ? store((int64_t)round(d)) : store_inexact(round(d));
    }

    // Random number generation (SRFI-27 style)
    case PRANDOMINTEGER: {
        REQUIRE_ARGS(args, 1, 1, "random-integer");
        unsigned x = car(args);
        enum lisp_type t = CELL_TYPE(x);
        if (t != BT_NUM && t != BT_BIGNUM) {
            show_error("random-integer: expected positive integer");
            return TOK_ERROR;
        }
        // Get value - for simplicity, convert via double for bignums
        int64_t n;
        if (t == BT_NUM) {
            n = CELL_ID(x);
        } else {
            n = (int64_t)to_double(x);
        }
        if (n <= 0) {
            show_error("random-integer: expected positive integer");
            return TOK_ERROR;
        }
        // Use rejection sampling to avoid modulo bias
        uint64_t limit = UINT64_MAX - (UINT64_MAX % (uint64_t)n);
        uint64_t r;
        do {
            r = xoshiro256ss();
        } while (r >= limit);
        return store((int64_t)(r % (uint64_t)n));
    }
    case PRANDOMREAL: {
        REQUIRE_ARGS(args, 0, 0, "random-real");
        // Convert 53 bits of randomness to [0, 1) double
        uint64_t r = xoshiro256ss() >> 11;
        double d = (double)r / (double)(1ULL << 53);
        return store_inexact(d);
    }
    case PRANDOMSEED: {
        REQUIRE_ARGS(args, 1, 1, "random-seed!");
        unsigned x = car(args);
        enum lisp_type t = CELL_TYPE(x);
        uint64_t seed;
        if (t == BT_NUM) {
            seed = (uint64_t)CELL_ID(x);
        } else if (t == BT_BIGNUM) {
            seed = (uint64_t)to_double(x);
        } else {
            seed = (uint64_t)time(NULL);
        }
        // SplitMix64 to generate initial state from seed
        for (int i = 0; i < 4; i++) {
            seed += 0x9e3779b97f4a7c15ULL;
            uint64_t z = seed;
            z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
            z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
            rng_state[i] = z ^ (z >> 31);
        }
        return car(args);
    }

    default:
        return TOK_ERROR;
    }
}
