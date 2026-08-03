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

static unsigned random_integer_value(unsigned x, const char *name)
{
    int64_t n;
    if (!expect_exact_int64(x, &n, name))
        return TOK_ERROR;
    if (n <= 0) {
        show_error("%s: expected positive integer", name);
        return TOK_ERROR;
    }

    // Reject the small prefix that would make the 2^64-sized generator
    // domain unevenly divisible by n.  Computing the threshold in unsigned
    // arithmetic also handles powers of two (where a naive UINT64_MAX-based
    // limit rejects one otherwise valid sample).
    uint64_t threshold = -(uint64_t)n % (uint64_t)n;
    uint64_t r;
    do {
        r = xoshiro256ss();
    } while (r < threshold);
    return store((int64_t)(r % (uint64_t)n));
}

static unsigned random_real_value(void)
{
    uint64_t r = xoshiro256ss() >> 11;
    double d = (double)r / (double)(1ULL << 53);
    return store_inexact(d);
}

static unsigned random_seed_value(unsigned x, const char *name)
{
    uint64_t seed;
    if (IS_FIXNUM(x)) {
        seed = (uint64_t)FIXNUM_VALUE(x);
    } else if (IS_NUM(x)) {
        seed = (uint64_t)CELL_ID(x);
    } else if (IS_BIGNUM(x)) {
        bignum *bn = get_bignum(x);
        if (!bn || bn_to_uint64(bn, &seed) != 0) {
            show_error("%s: %s", name,
                       bn ? "integer out of range" : "invalid bignum");
            return TOK_ERROR;
        }
    } else {
        show_error("%s: expected exact integer seed", name);
        return TOK_ERROR;
    }

    for (int i = 0; i < 4; i++) {
        seed += 0x9e3779b97f4a7c15ULL;
        uint64_t z = seed;
        z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
        z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
        rng_state[i] = z ^ (z >> 31);
    }
    return x;
}

// Math function table for simple unary functions
typedef struct {
    unsigned id;
    double (*func)(double);
    const char *name;
} math_func_entry;

static const math_func_entry math_funcs[] = {
    {PASIN, asin, "asin"}, {PACOS, acos, "acos"}, {0, NULL, NULL}};

typedef enum {
    ROUND_FLOOR,
    ROUND_CEILING,
    ROUND_TRUNCATE,
    ROUND_NEAREST // Ties to even (R7RS round)
} round_mode;

typedef struct {
    unsigned id;
    round_mode mode;
    const char *name;
} rounding_func_entry;

static const rounding_func_entry rounding_funcs[] = {
    {PFLOOR, ROUND_FLOOR, "floor"},
    {PCEILING, ROUND_CEILING, "ceiling"},
    {PTRUNCATE, ROUND_TRUNCATE, "truncate"},
    {PROUND, ROUND_NEAREST, "round"},
    {0, 0, NULL}};

static const math_func_entry *find_math_func(const math_func_entry *entries,
                                             unsigned prim_id)
{
    for (const math_func_entry *entry = entries; entry->func; entry++) {
        if (entry->id == prim_id)
            return entry;
    }
    return NULL;
}

static const rounding_func_entry *find_rounding_func(unsigned prim_id)
{
    for (const rounding_func_entry *entry = rounding_funcs; entry->name;
         entry++) {
        if (entry->id == prim_id)
            return entry;
    }
    return NULL;
}

static bool bn_is_even(const bignum *a)
{
    return a->len == 0 || (a->limbs[0] & 1) == 0;
}

// Round an exact rational to an exact integer without any double conversion
// (doubles have only 53 mantissa bits, which silently corrupts wide values)
static unsigned exact_rational_round(unsigned x, round_mode mode,
                                     const char *name)
{
    bignum *num = to_bignum(CELL_CAR(x));
    bignum *den = to_bignum(CELL_CDR(x));
    bignum *rem = NULL;
    bignum *q = NULL;
    if (!num || !den || bn_is_zero(den) || bn_sign(den) < 0 ||
        !(q = bn_div(num, den, &rem)) || !rem) {
        bn_free(num);
        bn_free(den);
        bn_free(q);
        bn_free(rem);
        show_error("%s: invalid rational", name);
        return TOK_ERROR;
    }

    // q is the truncated quotient; rem has the sign of num, |rem| < den
    bool ok = true;
    if (!bn_is_zero(rem)) {
        switch (mode) {
        case ROUND_TRUNCATE:
            break;
        case ROUND_FLOOR:
            if (bn_sign(num) < 0) {
                bignum one = {.limbs = (limb_t[]){1}, .len = 1, .cap = 1,
                              .sign = 0};
                ok = bn_sub_ip_checked(q, &one);
            }
            break;
        case ROUND_CEILING:
            if (bn_sign(num) > 0) {
                bignum one = {.limbs = (limb_t[]){1}, .len = 1, .cap = 1,
                              .sign = 0};
                ok = bn_add_ip_checked(q, &one);
            }
            break;
        case ROUND_NEAREST: {
            // Work with the floor quotient qf and remainder rf in [0, den)
            bignum one = {.limbs = (limb_t[]){1}, .len = 1, .cap = 1,
                          .sign = 0};
            if (bn_sign(num) < 0) {
                ok = bn_sub_ip_checked(q, &one); // q = floor(num/den)
                if (ok)
                    ok = bn_add_ip_checked(rem, den); // rem in [0, den)
            }
            if (ok) {
                // Compare 2*rem against den
                bignum *twice = bn_add(rem, rem);
                if (!twice) {
                    ok = false;
                } else {
                    int cmp = bn_cmp(twice, den);
                    bn_free(twice);
                    if (cmp > 0 || (cmp == 0 && !bn_is_even(q)))
                        ok = bn_add_ip_checked(q, &one);
                }
            }
            break;
        }
        }
    }

    bn_free(num);
    bn_free(den);
    bn_free(rem);
    if (!ok) {
        bn_free(q);
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    return store_integer(q);
}

static unsigned apply_rounding(unsigned x, round_mode mode, const char *name)
{
    if (!require_real(x, name))
        return TOK_ERROR;
    if (IS_FIXNUM(x) || IS_NUM(x) || IS_BIGNUM(x))
        return x;
    if (IS_RATIONAL(x) && is_exact(x))
        return exact_rational_round(x, mode, name);
    double d = to_double(x);
    double r;
    switch (mode) {
    case ROUND_FLOOR:
        r = floor(d);
        break;
    case ROUND_CEILING:
        r = ceil(d);
        break;
    case ROUND_TRUNCATE:
        r = trunc(d);
        break;
    default:
        r = nearbyint(d); // Ties to even in the default rounding mode
        break;
    }
    return make_real_result(r, is_exact(x));
}

typedef void (*complex_unary_func)(double real, double imag, double *out_real,
                                   double *out_imag);

typedef struct {
    unsigned id;
    double (*real_func)(double);
    complex_unary_func complex_func;
    const char *name;
} unary_number_math_entry;

static unsigned apply_unary_number_math(unsigned x, double (*real_func)(double),
                                        complex_unary_func complex_func,
                                        const char *name)
{
    if (!require_number(x, name))
        return TOK_ERROR;
    if (IS_COMPLEX(x)) {
        double real, imag, out_real, out_imag;
        get_complex_parts(x, &real, &imag);
        complex_func(real, imag, &out_real, &out_imag);
        return make_complex_inexact(out_real, out_imag);
    }
    return store_inexact(real_func(to_double(x)));
}

static unsigned atan_value(unsigned y_arg, unsigned x_arg, bool has_x,
                           const char *name)
{
    if (!require_real(y_arg, name) || (has_x && !require_real(x_arg, name)))
        return TOK_ERROR;
    double y = to_double(y_arg);
    if (has_x)
        return store_inexact(atan2(y, to_double(x_arg)));
    return store_inexact(atan(y));
}

static unsigned log_value(unsigned arg, const char *name)
{
    if (!require_number(arg, name))
        return TOK_ERROR;

    if (IS_COMPLEX(arg)) {
        double real, imag;
        get_complex_parts(arg, &real, &imag);
        double mag = hypot(real, imag);
        double angle = atan2(imag, real);
        return make_complex_inexact(log(mag), angle);
    }

    double x = to_double(arg);
    if (x < 0)
        return make_complex_inexact(log(-x), M_PI);
    return store_inexact(log(x));
}

static double sqrt1pm1_double(double x)
{
    if (x == 0.0)
        return 0.0;
    return x / (sqrt(1.0 + x) + 1.0);
}

static double log1pexp_double(double x)
{
    if (x <= -37.0)
        return exp(x);
    if (x <= 18.0)
        return log1p(exp(x));
    if (x <= 33.3)
        return x + exp(-x);
    return x;
}

static unsigned log1p_value(unsigned arg, const char *name)
{
    if (!require_number(arg, name))
        return TOK_ERROR;
    if (!IS_COMPLEX(arg)) {
        double x = to_double(arg);
        if (x < -1.0)
            return make_complex_inexact(log(-(1.0 + x)), M_PI);
        return store_inexact(log1p(x));
    }

    double real, imag;
    get_complex_parts(arg, &real, &imag);
    real += 1.0;
    return make_complex_inexact(log(hypot(real, imag)), atan2(imag, real));
}

static unsigned expm1_value(unsigned arg, const char *name)
{
    if (!require_number(arg, name))
        return TOK_ERROR;
    if (!IS_COMPLEX(arg))
        return store_inexact(expm1(to_double(arg)));

    double real, imag;
    get_complex_parts(arg, &real, &imag);
    double exp_real = exp(real);
    double out_real = expm1(real) * cos(imag) + (cos(imag) - 1.0);
    double out_imag = exp_real * sin(imag);
    return make_complex_inexact(out_real, out_imag);
}

static void complex_sqrt_inexact(double a, double b, double *out_real,
                                 double *out_imag)
{
    double r = hypot(a, b);
    *out_real = sqrt((r + a) / 2.0);
    *out_imag = (b >= 0.0 ? 1.0 : -1.0) * sqrt((r - a) / 2.0);
}

static unsigned sqrt1pm1_value(unsigned arg, const char *name)
{
    if (!require_number(arg, name))
        return TOK_ERROR;
    if (!IS_COMPLEX(arg)) {
        double x = to_double(arg);
        if (x < -1.0)
            return make_complex_inexact(-1.0, sqrt(-(1.0 + x)));
        return store_inexact(sqrt1pm1_double(x));
    }

    double real, imag;
    get_complex_parts(arg, &real, &imag);
    double wr, wi;
    complex_sqrt_inexact(real + 1.0, imag, &wr, &wi);

    // z / (sqrt(1 + z) + 1)
    double denom_real = wr + 1.0;
    double denom_imag = wi;
    double denom = denom_real * denom_real + denom_imag * denom_imag;
    if (denom == 0.0)
        return make_complex_inexact(wr - 1.0, wi);
    double out_real = (real * denom_real + imag * denom_imag) / denom;
    double out_imag = (imag * denom_real - real * denom_imag) / denom;
    return make_complex_inexact(out_real, out_imag);
}

static unsigned log1pexp_value(unsigned arg, const char *name)
{
    if (!require_number(arg, name))
        return TOK_ERROR;
    if (!IS_COMPLEX(arg))
        return store_inexact(log1pexp_double(to_double(arg)));

    double real, imag;
    get_complex_parts(arg, &real, &imag);
    double cos_imag = cos(imag);
    double sin_imag = sin(imag);
    if (real > 0.0) {
        double exp_neg_real = exp(-real);
        double log_abs =
            real + 0.5 * log1p(2.0 * exp_neg_real * cos_imag +
                               exp_neg_real * exp_neg_real);
        double angle = atan2(sin_imag, cos_imag + exp_neg_real);
        return make_complex_inexact(log_abs, angle);
    }

    double exp_real = exp(real);
    double one_plus_real = 1.0 + exp_real * cos_imag;
    double one_plus_imag = exp_real * sin_imag;
    return make_complex_inexact(log(hypot(one_plus_real, one_plus_imag)),
                                atan2(one_plus_imag, one_plus_real));
}

static void complex_exp_parts(double real, double imag, double *out_real,
                              double *out_imag)
{
    double mag = exp(real);
    *out_real = mag * cos(imag);
    *out_imag = mag * sin(imag);
}

static void complex_sin_parts(double real, double imag, double *out_real,
                              double *out_imag)
{
    *out_real = sin(real) * cosh(imag);
    *out_imag = cos(real) * sinh(imag);
}

static void complex_cos_parts(double real, double imag, double *out_real,
                              double *out_imag)
{
    *out_real = cos(real) * cosh(imag);
    *out_imag = -sin(real) * sinh(imag);
}

static void complex_tan_parts(double real, double imag, double *out_real,
                              double *out_imag)
{
    double denom = cos(2 * real) + cosh(2 * imag);
    *out_real = sin(2 * real) / denom;
    *out_imag = sinh(2 * imag) / denom;
}

static const unary_number_math_entry unary_number_math_funcs[] = {
    {PEXP, exp, complex_exp_parts, "exp"},
    {PSIN, sin, complex_sin_parts, "sin"},
    {PCOS, cos, complex_cos_parts, "cos"},
    {PTAN, tan, complex_tan_parts, "tan"},
    {0, NULL, NULL, NULL},
};

static const unary_number_math_entry *find_unary_number_math(unsigned prim_id)
{
    for (const unary_number_math_entry *entry = unary_number_math_funcs;
         entry->name; entry++) {
        if (entry->id == prim_id)
            return entry;
    }
    return NULL;
}

/**
 * Compute integer nth root of a bignum using Newton's method.
 * Returns the root if base is a perfect nth power, NULL otherwise.
 * Caller must free the returned bignum.
 */
// Newton's method for the integer nth root (approximately floor(base^1/n);
// may be off by one, so callers needing exactness must verify)
static bignum *bn_approx_nth_root(const bignum *base, int64_t n)
{
    if (n <= 0 || bn_is_zero(base))
        return NULL;
    if (n == 1)
        return bn_copy(base);
    if (base->sign)
        return NULL; // No real nth root of negative for even n

    /*
     * Start Newton's method from a bignum estimate instead of converting the
     * entire operand to double.  Large operands turn that conversion into
     * infinity, and converting infinity to int64_t is undefined behavior.
     *
     * If base has L significant bits, 2^ceil(L/n) is an upper bound on its
     * nth root.  It is also close enough for Newton's method to converge
     * quickly, without imposing a floating-point size limit on exact roots.
     */
    if ((uint64_t)n > SIZE_MAX || base->len > SIZE_MAX / LIMB_BITS)
        return NULL;
    limb_t high_limb = base->limbs[base->len - 1];
    if (high_limb == 0)
        return NULL;
    size_t high_bits = 0;
    while (high_limb) {
        high_bits++;
        high_limb >>= 1;
    }
    size_t bit_length = (base->len - 1) * LIMB_BITS + high_bits;
    size_t root_degree = (size_t)n;
    if (bit_length > SIZE_MAX - (root_degree - 1))
        return NULL;
    size_t root_bits = (bit_length + root_degree - 1) / root_degree;

    bignum *one = bn_from_int(1);
    if (!one)
        return NULL;
    bignum *guess = bn_lshift(one, root_bits);
    bn_free(one);
    if (!guess)
        return NULL;

    // Newton iteration: x_new = ((n-1)*x + base/x^(n-1)) / n
    bignum *n_bn = bn_from_int(n);
    bignum *n_minus_1 = bn_from_int(n - 1);
    if (!n_bn || !n_minus_1) {
        bn_free(guess);
        bn_free(n_bn);
        bn_free(n_minus_1);
        return NULL;
    }

    for (int iter = 0; iter < 100; iter++) {
        // Compute guess^(n-1)
        bignum *power = bn_from_int(1);
        if (!power) {
            bn_free(guess);
            bn_free(n_bn);
            bn_free(n_minus_1);
            return NULL;
        }
        for (int64_t i = 0; i < n - 1; i++) {
            bignum *temp = bn_mul(power, guess);
            bn_free(power);
            power = temp;
            if (!power) {
                bn_free(guess);
                bn_free(n_bn);
                bn_free(n_minus_1);
                return NULL;
            }
        }

        // Compute base / guess^(n-1)
        bignum *quotient = bn_div(base, power, NULL);
        bn_free(power);
        if (!quotient) {
            bn_free(guess);
            bn_free(n_bn);
            bn_free(n_minus_1);
            return NULL;
        }

        // Compute (n-1) * guess
        bignum *term1 = bn_mul(n_minus_1, guess);
        if (!term1) {
            bn_free(quotient);
            bn_free(guess);
            bn_free(n_bn);
            bn_free(n_minus_1);
            return NULL;
        }

        // Sum: (n-1)*guess + base/guess^(n-1)
        bignum *sum = bn_add(term1, quotient);
        bn_free(term1);
        bn_free(quotient);
        if (!sum) {
            bn_free(guess);
            bn_free(n_bn);
            bn_free(n_minus_1);
            return NULL;
        }

        // New guess: sum / n
        bignum *new_guess = bn_div(sum, n_bn, NULL);
        bn_free(sum);
        if (!new_guess) {
            bn_free(guess);
            bn_free(n_bn);
            bn_free(n_minus_1);
            return NULL;
        }

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
    return guess;
}

static bignum *bn_exact_nth_root(const bignum *base, int64_t n)
{
    bignum *guess = bn_approx_nth_root(base, n);
    if (!guess)
        return NULL;

    // Verify: guess^n == base
    bignum *check = bn_from_int(1);
    if (!check) {
        bn_free(guess);
        return NULL;
    }
    for (int64_t i = 0; i < n; i++) {
        bignum *temp = bn_mul(check, guess);
        bn_free(check);
        check = temp;
        if (!check) {
            bn_free(guess);
            return NULL;
        }
    }

    if (bn_cmp(check, base) == 0) {
        bn_free(check);
        return guess; // Perfect nth power
    }

    bn_free(check);
    bn_free(guess);
    return NULL; // Not a perfect nth power
}

static unsigned sqrt_value(unsigned arg, const char *name)
{
    if (!require_number(arg, name))
        return TOK_ERROR;

    if (IS_COMPLEX(arg)) {
        double a, b;
        get_complex_parts(arg, &a, &b);
        double r = hypot(a, b);
        double real, imag;
        if (a >= 0) {
            real = sqrt(r / 2 + a / 2);
            imag = b / (2 * real);
        } else {
            imag = (b >= 0 ? 1 : -1) * sqrt(r / 2 - a / 2);
            real = b / (2 * imag);
        }
        return make_complex_inexact(real, imag);
    }

    if (IS_RATIONAL(arg)) {
        unsigned num_cell = CELL_CAR(arg);
        unsigned denom_cell = CELL_CDR(arg);
        if (!is_negative_number(num_cell)) {
            bignum *num_bn = to_bignum(num_cell);
            bignum *denom_bn = to_bignum(denom_cell);
            bignum *sqrt_num = num_bn ? bn_exact_nth_root(num_bn, 2) : NULL;
            bignum *sqrt_denom =
                denom_bn ? bn_exact_nth_root(denom_bn, 2) : NULL;
            bn_free(num_bn);
            bn_free(denom_bn);
            if (sqrt_num && sqrt_denom) {
                GC_GUARD;
                unsigned root_num = store_integer(sqrt_num);
                gc_protect(&root_num);
                unsigned root_denom = store_integer(sqrt_denom);
                gc_protect(&root_denom);
                return normalize_rational_cells(root_num, root_denom);
            }
            bn_free(sqrt_num);
            bn_free(sqrt_denom);
        }
    }

    int64_t n;
    bool exact_integer_arg = false;
    if (IS_FIXNUM(arg)) {
        n = FIXNUM_VALUE(arg);
        exact_integer_arg = true;
    } else if (IS_NUM(arg)) {
        n = CELL_ID(arg);
        exact_integer_arg = true;
    }
    if (exact_integer_arg && n >= 0) {
        int64_t s = (int64_t)sqrt((double)n);
        if (s * s == n)
            return store(s);
    }

    if (IS_BIGNUM(arg)) {
        bignum *bn = get_bignum(arg);
        if (bn && !bn->sign) {
            bignum *root = bn_exact_nth_root(bn, 2);
            if (root)
                return store_integer(root);
            // Operands beyond double range would convert to infinity below;
            // approximate through the integer square root instead, which
            // halves the exponent back into double range
            if (isinf(to_double(arg))) {
                bignum *approx = bn_approx_nth_root(bn, 2);
                if (approx) {
                    GC_GUARD;
                    unsigned root_cell = store_integer(approx);
                    if (root_cell == TOK_ERROR)
                        return TOK_ERROR;
                    gc_protect(&root_cell);
                    return store_inexact(to_double(root_cell));
                }
            }
        }
    }

    double x = to_double(arg);
    if (x < 0) {
        GC_GUARD;
        unsigned real_part = store(0);
        gc_protect(&real_part);
        unsigned imag_part = store_inexact(sqrt(-x));
        return store_complex(real_part, imag_part);
    }
    return store_inexact(sqrt(x));
}

static unsigned inexact_expt_value(unsigned base_arg, unsigned exp_arg)
{
    bool base_complex = IS_COMPLEX(base_arg);
    bool exp_complex = IS_COMPLEX(exp_arg);

    double base_d = to_double(base_arg);
    double exp_d = to_double(exp_arg);
    bool needs_complex = base_complex || exp_complex ||
                         (base_d < 0 && floor(exp_d) != exp_d);

    if (needs_complex) {
        double b_real, b_imag;
        get_complex_parts(base_arg, &b_real, &b_imag);

        double mag = hypot(b_real, b_imag);
        double angle = atan2(b_imag, b_real);
        double log_real = log(mag);
        double log_imag = angle;

        double e_real, e_imag;
        get_complex_parts(exp_arg, &e_real, &e_imag);
        double prod_real = e_real * log_real - e_imag * log_imag;
        double prod_imag = e_real * log_imag + e_imag * log_real;

        double r_mag = exp(prod_real);
        return make_complex_inexact(r_mag * cos(prod_imag),
                                    r_mag * sin(prod_imag));
    }

    return store_inexact(pow(base_d, exp_d));
}

static bool exact_expt_int64_exponent(unsigned exp_arg, int64_t *exp_out)
{
    if (IS_FIXNUM(exp_arg)) {
        *exp_out = FIXNUM_VALUE(exp_arg);
        return true;
    }
    if (IS_NUM(exp_arg)) {
        *exp_out = CELL_ID(exp_arg);
        return true;
    }
    return bn_to_int64(get_bignum(exp_arg), exp_out) == 0;
}

static unsigned exact_integer_expt_value(unsigned base_arg, int64_t exp_val)
{
    bool neg_exp = exp_val < 0;
    uint64_t e = neg_exp ? -(uint64_t)exp_val : (uint64_t)exp_val;

    GC_GUARD;
    unsigned result = store(1);
    unsigned base = base_arg;
    gc_protect(&result);
    gc_protect(&base);

    while (e > 0) {
        if (e & 1) {
            unsigned mul_argv[2] = {result, base};
            gc_protect(&mul_argv[0]);
            gc_protect(&mul_argv[1]);
            result = prim_mult(2, mul_argv);
            gc_unprotect(2);
            if (result == TOK_ERROR)
                return TOK_ERROR;
        }
        e >>= 1;
        if (e > 0) {
            unsigned sq_argv[2] = {base, base};
            gc_protect(&sq_argv[0]);
            gc_protect(&sq_argv[1]);
            base = prim_mult(2, sq_argv);
            gc_unprotect(2);
            if (base == TOK_ERROR)
                return TOK_ERROR;
        }
    }

    if (neg_exp) {
        unsigned one = store(1);
        gc_protect(&one);
        unsigned div_argv[2] = {one, result};
        gc_protect(&div_argv[0]);
        gc_protect(&div_argv[1]);
        return prim_div(2, div_argv);
    }

    return result;
}

static void free_bignum_pair(bignum *a, bignum *b)
{
    bn_free(a);
    bn_free(b);
}

static unsigned exact_rational_expt_result(bignum *num_power,
                                           bignum *den_power, bool neg_exp)
{
    GC_GUARD;
    unsigned result_num = store_integer(num_power);
    gc_protect(&result_num);
    unsigned result_den = store_integer(den_power);

    if (neg_exp) {
        unsigned temp = result_num;
        result_num = result_den;
        result_den = temp;
    }

    return normalize_rational_cells(result_num, result_den);
}

unsigned apply_math_primitive(unsigned prim_id, unsigned argc,
                              unsigned *argv)
{
    const math_func_entry *math_func = find_math_func(math_funcs, prim_id);
    if (math_func) {
        REQUIRE_ARGC(argc, 1, 1, math_func->name);
        if (!require_real(argv[0], math_func->name))
            return TOK_ERROR;
        return store_inexact(math_func->func(to_double(argv[0])));
    }

    const rounding_func_entry *rounding_func = find_rounding_func(prim_id);
    if (rounding_func) {
        REQUIRE_ARGC(argc, 1, 1, rounding_func->name);
        return apply_rounding(argv[0], rounding_func->mode,
                              rounding_func->name);
    }

    const unary_number_math_entry *unary_func =
        find_unary_number_math(prim_id);
    if (unary_func) {
        REQUIRE_ARGC(argc, 1, 1, unary_func->name);
        return apply_unary_number_math(argv[0], unary_func->real_func,
                                       unary_func->complex_func,
                                       unary_func->name);
    }

    switch (prim_id) {
    case PSQRT: {
        REQUIRE_ARGC(argc, 1, 1, "sqrt");
        return sqrt_value(argv[0], "sqrt");
    }
    case PEXPT: {
        // z^w = exp(w * log(z)) for complex numbers
        REQUIRE_ARGC(argc, 2, 2, "expt");
        unsigned base_arg = argv[0];
        unsigned exp_arg = argv[1];
        if (!require_number(base_arg, "expt") ||
            !require_number(exp_arg, "expt"))
            return TOK_ERROR;

        // Exact base with exact integer exponent -> exact result
        // Covers integers, rationals, and complex with exact parts
        if (is_exact(base_arg) && IS_EXACT_INT(exp_arg)) {
            int64_t exp_val;
            if (!exact_expt_int64_exponent(exp_arg, &exp_val))
                goto inexact_expt;
            return exact_integer_expt_value(base_arg, exp_val);
        }

        // Exact integer or rational base with exact rational exponent p/q
        // Try to compute exact (base^(1/q))^p if base is a perfect qth power
        if ((IS_EXACT_INT(base_arg) || IS_RATIONAL(base_arg)) &&
            IS_RATIONAL(exp_arg)) {
            int64_t exp_numer, exp_denom;
            if (!exact_int64_value(CELL_CAR(exp_arg), &exp_numer) ||
                !exact_int64_value(CELL_CDR(exp_arg), &exp_denom)) {
                goto inexact_expt;
            }

            // Only handle small roots (avoid expensive computation)
            if (exp_denom > 0 && exp_denom <= 1000) {
                bignum *num_root = NULL;
                bignum *den_root = NULL;

                if (IS_EXACT_INT(base_arg)) {
                    // Integer base - denominator is implicitly 1
                    bignum *base_bn = to_bignum(base_arg);
                    if (base_bn)
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
                    if (num_bn)
                        num_root = bn_exact_nth_root(num_bn, exp_denom);
                    if (num_root && den_bn)
                        den_root = bn_exact_nth_root(den_bn, exp_denom);
                    bn_free(num_bn);
                    bn_free(den_bn);
                }

                if (num_root && den_root) {
                    // Both are perfect qth powers
                    // Now compute (num_root/den_root)^|exp_numer|
                    bool neg_exp = exp_numer < 0;
                    uint64_t p =
                        neg_exp ? -(uint64_t)exp_numer : (uint64_t)exp_numer;

                    bignum *num_power = bn_from_int(1);
                    bignum *den_power = bn_from_int(1);
                    if (!num_power || !den_power) {
                        free_bignum_pair(num_power, den_power);
                        free_bignum_pair(num_root, den_root);
                        show_error("expt: out of memory");
                        return TOK_ERROR;
                    }
                    for (uint64_t i = 0; i < p; i++) {
                        bignum *temp = bn_mul(num_power, num_root);
                        bn_free(num_power);
                        num_power = temp;
                        if (!num_power) {
                            bn_free(den_power);
                            free_bignum_pair(num_root, den_root);
                            show_error("expt: out of memory");
                            return TOK_ERROR;
                        }
                        temp = bn_mul(den_power, den_root);
                        bn_free(den_power);
                        den_power = temp;
                        if (!den_power) {
                            bn_free(num_power);
                            free_bignum_pair(num_root, den_root);
                            show_error("expt: out of memory");
                            return TOK_ERROR;
                        }
                    }
                    free_bignum_pair(num_root, den_root);

                    return exact_rational_expt_result(num_power, den_power,
                                                      neg_exp);
                }

                free_bignum_pair(num_root, den_root);
            }
        }

    inexact_expt:;
        return inexact_expt_value(base_arg, exp_arg);
    }
    case PATAN: {
        REQUIRE_ARGC(argc, 1, 2, "atan");
        return atan_value(argv[0], argc == 2 ? argv[1] : 0, argc == 2,
                          "atan");
    }
    case PLOG: {
        REQUIRE_ARGC(argc, 1, 1, "log");
        return log_value(argv[0], "log");
    }
    case PLOG1P: {
        REQUIRE_ARGC(argc, 1, 1, "log1p");
        return log1p_value(argv[0], "log1p");
    }
    case PEXPM1: {
        REQUIRE_ARGC(argc, 1, 1, "expm1");
        return expm1_value(argv[0], "expm1");
    }
    case PSQRT1PM1: {
        REQUIRE_ARGC(argc, 1, 1, "sqrt1pm1");
        return sqrt1pm1_value(argv[0], "sqrt1pm1");
    }
    case PLOG1PEXP: {
        REQUIRE_ARGC(argc, 1, 1, "log1pexp");
        return log1pexp_value(argv[0], "log1pexp");
    }
    // Random number generation (SRFI-27 style)
    case PRANDOMINTEGER: {
        REQUIRE_ARGC(argc, 1, 1, "random-integer");
        return random_integer_value(argv[0], "random-integer");
    }
    case PRANDOMREAL: {
        REQUIRE_ARGC(argc, 0, 0, "random-real");
        return random_real_value();
    }
    case PRANDOMSEED: {
        REQUIRE_ARGC(argc, 1, 1, "random-seed!");
        return random_seed_value(argv[0], "random-seed!");
    }

    default:
        return TOK_ERROR;
    }
}
