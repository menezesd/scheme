/**
 * @file primitives.c
 * @brief Built-in primitive procedures (~150 operations)
 *
 * This file implements all built-in Scheme procedures. Primitives are
 * organized by category:
 *
 * ## Numeric Operations
 * - Arithmetic: +, -, *, /, quotient, remainder, modulo
 * - Comparison: =, <, >, <=, >=
 * - Math functions: sqrt, sin, cos, exp, log, expt, etc.
 * - Type predicates: number?, integer?, rational?, real?, complex?
 * - Conversion: exact->inexact, inexact->exact, numerator, denominator
 *
 * ## Numeric Tower
 * Operations automatically promote through the tower:
 *   integer -> bignum -> rational -> real -> complex
 *
 * ## Pair Operations
 * cons, car, cdr, set-car!, set-cdr! (other list ops are in stdlib.scm)
 *
 * ## String/Character Operations
 * string-length, string-ref, string-append, char->integer, etc.
 * Both case-sensitive and case-insensitive comparisons.
 *
 * ## Vector Operations
 * make-vector, vector-ref, vector-set!, vector->list, etc.
 *
 * ## I/O Operations
 * read, write, display, newline, open-input-file, open-output-file,
 * string ports, current-input-port, current-output-port.
 *
 * ## Control
 * apply, call/cc (call-with-current-continuation), values, call-with-values
 */

#include "primitives.h"
#include "context.h"
#include "env.h"
#include "reader.h"
#include "writer.h"
#include <ctype.h>
#include <inttypes.h>
#include <math.h>
#include <string.h>
#include <strings.h>

// ============================================================================
// Numeric Tower Classification
// ============================================================================

// Numeric level determines which arithmetic to use (higher = more general)
typedef enum {
    NUM_INTEGER,  // BT_NUM only
    NUM_BIGNUM,   // Has BT_BIGNUM
    NUM_RATIONAL, // Has BT_RATIONAL
    NUM_INEXACT,  // Has BT_INEXACT
    NUM_COMPLEX   // Has BT_COMPLEX
} numeric_level;

// Classify a list of arguments in a single pass
static numeric_level classify_args(unsigned args, bool *all_exact_out)
{
    numeric_level level = NUM_INTEGER;
    bool all_ex = true;

    FORLIST(a, args) {
        unsigned x = car(a);
        if (x == 0)
            continue;

        switch (CELL_TYPE(x)) {
        case BT_COMPLEX:
            level = NUM_COMPLEX;
            break;
        case BT_INEXACT:
            all_ex = false;
            if (level < NUM_INEXACT)
                level = NUM_INEXACT;
            break;
        case BT_RATIONAL:
            if (level < NUM_RATIONAL)
                level = NUM_RATIONAL;
            break;
        case BT_BIGNUM:
            if (level < NUM_BIGNUM)
                level = NUM_BIGNUM;
            break;
        case BT_NUM:
            // Already at NUM_INTEGER or higher
            break;
        default:
            break;
        }
    }

    if (all_exact_out)
        *all_exact_out = all_ex;
    return level;
}

static bool all_exact(unsigned args)
{
    bool exact;
    classify_args(args, &exact);
    return exact;
}

// ============================================================================
// Numeric Tower Helpers
// ============================================================================

// Get real and imaginary parts of any number
static void get_complex_parts(unsigned x, double *real, double *imag)
{
    if (x == 0) {
        *real = 0.0;
        *imag = 0.0;
        return;
    }
    if (CELL_TYPE(x) == BT_COMPLEX) {
        *real = to_double(CELL_CAR(x));
        *imag = to_double(CELL_CDR(x));
    } else {
        *real = to_double(x);
        *imag = 0.0;
    }
}

// Create result with appropriate exactness
static unsigned make_real_result(double val, bool exact)
{
    if (exact && floor(val) == val && val >= INT64_MIN && val <= INT64_MAX) {
        return store((int64_t)val);
    }
    return store_inexact(val);
}

// Create complex result, simplifying to real if imaginary part is zero
static unsigned make_complex_result(double real, double imag, bool exact)
{
    if (imag == 0.0)
        return make_real_result(real, exact);
    return store_complex(make_real_result(real, exact),
                         make_real_result(imag, exact));
}

// Create inexact complex result (convenience for division etc.)
static inline unsigned make_complex_inexact(double real, double imag)
{
    if (imag == 0.0)
        return store_inexact(real);
    return store_complex(store_inexact(real), store_inexact(imag));
}

// ============================================================================
// Generic Arithmetic Operations
// ============================================================================

// Helper to get rational components (num, denom) from any exact number
static void get_rational_parts(unsigned x, int64_t *num, int64_t *denom)
{
    enum lisp_type t = CELL_TYPE(x);
    if (t == BT_NUM) {
        *num = CELL_ID(x);
        *denom = 1;
    } else if (t == BT_RATIONAL) {
        *num = CELL_ID(CELL_CAR(x));
        *denom = CELL_ID(CELL_CDR(x));
    } else {
        *num = 0;
        *denom = 1;
    }
}

static unsigned prim_plus(unsigned args)
{
    if (!args)
        return store(0);

    // Fast path: try pure integer arithmetic with builtin overflow detection
    int64_t v = 0;
    FORLIST(a, args) {
        unsigned x = car(a);
        if (!IS_NUM(x))
            goto slow_path;
        if (__builtin_add_overflow(v, CELL_ID(x), &v))
            goto slow_path;
    }
    return store(v);

slow_path:;
    bool exact;
    numeric_level level = classify_args(args, &exact);

    switch (level) {
    case NUM_COMPLEX: {
        double real = 0.0, imag = 0.0;
        FORLIST(a, args) {
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
        int64_t num = 0, denom = 1;
        FORLIST(a, args) {
            int64_t n, d;
            get_rational_parts(car(a), &n, &d);
            num = num * d + n * denom;
            denom = denom * d;
        }
        return normalize_rational(num, denom);
    }
    case NUM_BIGNUM: {
        bignum *result = bn_from_int(0);
        FORLIST(a, args) {
            bignum *operand = to_bignum(car(a));
            bn_add_ip(result, operand);
            bn_free(operand);
        }
        return store_integer(result);
    }
    case NUM_INTEGER: {
        // Pure integer arithmetic with builtin overflow detection
        int64_t sum = 0;
        FORLIST(a, args) {
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

static unsigned prim_mult(unsigned args)
{
    if (!args)
        return store(1);

    // Fast path: try pure integer arithmetic with builtin overflow detection
    int64_t v = 1;
    FORLIST(a, args) {
        unsigned x = car(a);
        if (!IS_NUM(x))
            goto slow_path;
        if (__builtin_mul_overflow(v, CELL_ID(x), &v))
            goto slow_path;
    }
    return store(v);

slow_path:;
    bool exact;
    numeric_level level = classify_args(args, &exact);

    switch (level) {
    case NUM_COMPLEX: {
        double real = 1.0, imag = 0.0;
        FORLIST(a, args) {
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
        int64_t num = 1, denom = 1;
        FORLIST(a, args) {
            int64_t n, d;
            get_rational_parts(car(a), &n, &d);
            num *= n;
            denom *= d;
        }
        return normalize_rational(num, denom);
    }
    case NUM_BIGNUM:
    case NUM_INTEGER: {
        // Use bignum for safety with large numbers
        bignum *result = bn_from_int(1);
        FORLIST(a, args) {
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

static unsigned prim_minus(unsigned args)
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
    bool exact;
    numeric_level level = classify_args(args, &exact);

    switch (level) {
    case NUM_COMPLEX: {
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
        int64_t num, denom;
        get_rational_parts(car(args), &num, &denom);
        unsigned rargs = cdr(args);
        if (!rargs)
            return normalize_rational(-num, denom);
        for (; rargs; rargs = cdr(rargs)) {
            int64_t n, d;
            get_rational_parts(car(rargs), &n, &d);
            num = num * d - n * denom;
            denom = denom * d;
        }
        return normalize_rational(num, denom);
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

static unsigned prim_div(unsigned args)
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
    bool exact;
    numeric_level level = classify_args(args, &exact);

    switch (level) {
    case NUM_COMPLEX: {
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
        int64_t num, denom;
        get_rational_parts(car(args), &num, &denom);
        unsigned rargs = cdr(args);
        if (!rargs) {
            CHECK_DIV_ZERO(num, "/");
            return normalize_rational(denom, num);
        }
        for (; rargs; rargs = cdr(rargs)) {
            int64_t n, d;
            get_rational_parts(car(rargs), &n, &d);
            CHECK_DIV_ZERO(n, "/");
            num *= d;
            denom *= n;
        }
        return normalize_rational(num, denom);
    }
    }
    return store(0); // Unreachable
}

static unsigned prim_modulo(unsigned args)
{
    REQUIRE_ARGS(args, 2, 2, "modulo");
    unsigned xa = car(args), xb = cadr(args);
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

static unsigned prim_remainder(unsigned args)
{
    REQUIRE_ARGS(args, 2, 2, "remainder");
    unsigned xa = car(args), xb = cadr(args);
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

static unsigned prim_quotient(unsigned args)
{
    REQUIRE_ARGS(args, 2, 2, "quotient");
    unsigned xa = car(args), xb = cadr(args);
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

static unsigned prim_abs(unsigned args)
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
        int64_t num = CELL_ID(CELL_CAR(x));
        int64_t denom = CELL_ID(CELL_CDR(x));
        return normalize_rational(num < 0 ? -num : num, denom);
    }
    default:
        show_error("abs: not a real number");
        return TOK_ERROR;
    }
}

// ============================================================================
// Numeric Comparison Helpers
// ============================================================================

typedef enum { CMP_EQ, CMP_LT, CMP_GT, CMP_LE, CMP_GE } cmp_op;

// Apply comparison operation to two values
#define APPLY_CMP_OP(op, a, b)                                                 \
    ((op) == CMP_EQ  ? ((a) == (b))                                            \
     : (op) == CMP_LT ? ((a) < (b))                                            \
     : (op) == CMP_GT ? ((a) > (b))                                            \
     : (op) == CMP_LE ? ((a) <= (b))                                           \
                      : ((a) >= (b)))

// Compare two exact integers, returns -1, 0, or 1
static int compare_exact_integers(unsigned a, unsigned b)
{
    enum lisp_type ta = CELL_TYPE(a);
    enum lisp_type tb = CELL_TYPE(b);

    if (ta == BT_NUM && tb == BT_NUM) {
        int64_t va = CELL_ID(a);
        int64_t vb = CELL_ID(b);
        return (va < vb) ? -1 : (va > vb) ? 1 : 0;
    }

    // At least one is bignum
    bignum *ba = to_bignum(a);
    bignum *bb = to_bignum(b);
    int cmp = bn_cmp(ba, bb);
    bn_free(ba);
    bn_free(bb);
    return cmp;
}

static unsigned numeric_compare(unsigned args, cmp_op op)
{
    if (!args)
        return ctx.atom_true;

    // Fast path: all BT_NUM - no type checks in loop
    unsigned first = car(args);
    if (IS_NUM(first)) {
        int64_t prev = CELL_ID(first);
        for (unsigned a = cdr(args); a; a = cdr(a)) {
            unsigned c = car(a);
            if (!IS_NUM(c))
                goto slow_path;
            int64_t curr = CELL_ID(c);
            if (!APPLY_CMP_OP(op, prev, curr))
                return 0;
            prev = curr;
        }
        return ctx.atom_true;
    }

slow_path:;
    unsigned prev = car(args);
    for (unsigned a = cdr(args); a; a = cdr(a)) {
        unsigned curr = car(a);
        bool ok;

        // Use exact comparison for exact integers
        if (IS_EXACT_INT(prev) && IS_EXACT_INT(curr)) {
            int cmp = compare_exact_integers(prev, curr);
            ok = APPLY_CMP_OP(op, cmp, 0);
        } else {
            double f = to_double(prev);
            double v = to_double(curr);
            ok = APPLY_CMP_OP(op, f, v);
        }
        if (!ok)
            return 0;
        prev = curr;
    }
    return ctx.atom_true;
}

// ============================================================================
// Character Comparison Helper
// ============================================================================

static unsigned char_compare(unsigned args, cmp_op op, bool case_insensitive)
{
    REQUIRE_ARGS(args, 2, 2, "char comparison");
    int c1 = (int)CELL_ID(car(args));
    int c2 = (int)CELL_ID(cadr(args));
    if (case_insensitive) {
        c1 = tolower(c1);
        c2 = tolower(c2);
    }
    return APPLY_CMP_OP(op, c1, c2) ? ctx.atom_true : 0;
}

// ============================================================================
// String Comparison Helper
// ============================================================================

static unsigned string_compare(unsigned args, cmp_op op, bool case_insensitive)
{
    REQUIRE_ARGS(args, 2, 2, "string comparison");
    CHECK_STRING(car(args), "string comparison");
    CHECK_STRING(cadr(args), "string comparison");
    char *s1 = GET_STRING_PTR(car(args));
    char *s2 = GET_STRING_PTR(cadr(args));
    int cmp = case_insensitive ? strcasecmp(s1, s2) : strcmp(s1, s2);
    return APPLY_CMP_OP(op, cmp, 0) ? ctx.atom_true : 0;
}

// ============================================================================
// List Primitives
// ============================================================================

static unsigned prim_append(unsigned args)
{
    if (!args)
        return 0;
    unsigned result = 0, tail = 0;
    for (unsigned a = args; cdr(a); a = cdr(a)) {
        unsigned lst = car(a);
        for (; lst && CELL_TYPE(lst) == BT_CONS; lst = cdr(lst)) {
            list_append(&result, &tail, car(lst));
        }
    }
    unsigned last = args;
    last = list_last(last);
    if (tail)
        CELL_CDR(tail) = car(last);
    return result ? result : car(last);
}

static unsigned prim_reverse(unsigned args)
{
    REQUIRE_ARGS(args, 1, 1, "reverse");
    unsigned lst = car(args);
    unsigned result = 0;
    for (; lst && CELL_TYPE(lst) == BT_CONS; lst = cdr(lst))
        result = alloc_cons(car(lst), result);
    return result;
}

// ============================================================================
// String Primitives
// ============================================================================

// Create a string cell from an already-allocated string (takes ownership)
static unsigned make_string_owned(char *s)
{
    unsigned p = alloc();
    CELL_TYPE(p) = BT_STRING;
    CELL_ID(p) = STORE_PTR(s);
    return p;
}

static unsigned prim_string_append(unsigned args)
{
    // First pass: validate and compute total length
    size_t total = 0;
    FORLIST(a, args) {
        CHECK_STRING(car(a), "string-append");
        total += strlen(GET_STRING_PTR(car(a)));
    }

    char *result = malloc(total + 1);
    if (!result) {
        show_error("string-append: out of memory");
        return TOK_ERROR;
    }

    // Second pass: copy strings (use pointer arithmetic to avoid strlen)
    char *pos = result;
    FORLIST(a, args) {
        char *s = GET_STRING_PTR(car(a));
        while (*s)
            *pos++ = *s++;
    }
    *pos = '\0';

    return make_string_owned(result);
}

static unsigned prim_substring(unsigned args)
{
    REQUIRE_ARGS(args, 2, 3, "substring");
    CHECK_STRING(car(args), "substring");
    char *s = GET_STRING_PTR(car(args));
    size_t slen = strlen(s);
    int64_t start = CELL_ID(cadr(args));
    int64_t end = cddr(args) ? CELL_ID(caddr(args)) : (int64_t)slen;
    if (start < 0 || end < start || end > (int64_t)slen) {
        show_error("substring: invalid indices");
        return TOK_ERROR;
    }
    char *result = malloc(end - start + 1);
    if (!result) {
        show_error("substring: out of memory");
        return TOK_ERROR;
    }
    strncpy(result, s + start, end - start);
    result[end - start] = '\0';
    return make_string_owned(result);
}

// ============================================================================
// String Port Helpers
// ============================================================================

// Create a new output string port
static string_port *strport_new(void)
{
    string_port *sp = malloc(sizeof(string_port));
    if (!sp)
        return NULL;
    sp->data = malloc(INITIAL_STRING_CAP);
    if (!sp->data) {
        free(sp);
        return NULL;
    }
    sp->data[0] = '\0';
    sp->len = 0;
    sp->cap = INITIAL_STRING_CAP;
    sp->pos = 0;
    return sp;
}

// Create a string input port from a string (copies the string)
static string_port *strport_from_string(const char *s)
{
    string_port *sp = malloc(sizeof(string_port));
    if (!sp)
        return NULL;
    size_t len = strlen(s);
    sp->data = malloc(len + 1);
    if (!sp->data) {
        free(sp);
        return NULL;
    }
    memcpy(sp->data, s, len + 1);
    sp->len = len;
    sp->cap = len + 1;
    sp->pos = 0;
    return sp;
}

// Write a character to string port (fast amortized O(1))
static void strport_putc(string_port *sp, int c)
{
    if (sp->len + 1 >= sp->cap) {
        sp->cap *= 2;
        sp->data = realloc(sp->data, sp->cap);
    }
    sp->data[sp->len++] = (char)c;
    sp->data[sp->len] = '\0';
}

// Write a string to string port
static void strport_puts(string_port *sp, const char *s)
{
    size_t slen = strlen(s);
    while (sp->len + slen >= sp->cap) {
        sp->cap *= 2;
        sp->data = realloc(sp->data, sp->cap);
    }
    memcpy(sp->data + sp->len, s, slen + 1);
    sp->len += slen;
}

// Read a character from string input port
static int strport_getc(string_port *sp)
{
    if (sp->pos >= sp->len)
        return EOF;
    return (unsigned char)sp->data[sp->pos++];
}

// Peek at next character from string input port
static int strport_peekc(string_port *sp)
{
    if (sp->pos >= sp->len)
        return EOF;
    return (unsigned char)sp->data[sp->pos];
}

// Free a string port
static void strport_free(string_port *sp)
{
    if (sp) {
        free(sp->data);
        free(sp);
    }
}

// ============================================================================
// Port Extraction Helpers
// ============================================================================

// Port direction for extract_port
typedef enum { PORT_INPUT, PORT_OUTPUT } port_dir;

// Unified port extraction - handles both file and string ports
// Returns: 0 = file port, 1 = string port, -1 = error
static int extract_port(unsigned args, port_dir dir, bool use_second_arg,
                        FILE **file_out, string_port **strport_out,
                        const char *fn_name)
{
    *file_out = (dir == PORT_INPUT) ? ctx.current_input : ctx.current_output;
    *strport_out = NULL;

    // Determine which arg contains the port
    unsigned port_arg = use_second_arg ? cdr(args) : args;
    if (!port_arg)
        return 0; // Use default port

    unsigned p = car(port_arg);

    // Check for string port
    bool is_strport = (dir == PORT_INPUT) ? IS_STRINPORT(p) : IS_STROUTPORT(p);
    if (is_strport) {
        *strport_out = GET_STRPORT_PTR(p);
        if (!*strport_out) {
            show_error("%s: port is closed", fn_name);
            return -1;
        }
        return 1;
    }

    // Check for file port
    bool is_fileport = (dir == PORT_INPUT) ? IS_INPORT(p) : IS_OUTPORT(p);
    if (!is_fileport) {
        show_error("%s: argument must be %s port", fn_name,
                   dir == PORT_INPUT ? "input" : "output");
        return -1;
    }

    *file_out = GET_PORT_PTR(p);
    if (!*file_out) {
        show_error("%s: port is closed", fn_name);
        return -1;
    }
    return 0;
}

// Convenience wrappers for backward compatibility
static inline int extract_output_port_ex(unsigned args, FILE **file_out,
                                         string_port **strport_out,
                                         const char *fn_name)
{
    return extract_port(args, PORT_OUTPUT, true, file_out, strport_out,
                        fn_name);
}

static inline int extract_input_port_ex(unsigned args, FILE **file_out,
                                        string_port **strport_out,
                                        const char *fn_name)
{
    return extract_port(args, PORT_INPUT, false, file_out, strport_out,
                        fn_name);
}

// Extract input port (file ports only, simpler interface)
static bool extract_input_port(unsigned args, FILE **port_out,
                               const char *fn_name)
{
    *port_out = ctx.current_input;
    if (args) {
        unsigned p = car(args);
        if (!IS_INPORT(p)) {
            show_error("%s: argument must be input port", fn_name);
            return false;
        }
        *port_out = GET_PORT_PTR(p);
        if (!*port_out) {
            show_error("%s: port is closed", fn_name);
            return false;
        }
    }
    return true;
}

// ============================================================================
// Category Dispatch Functions
// ============================================================================

// Type predicates
static unsigned apply_type_predicate(unsigned prim_id, unsigned args)
{
    unsigned arg = car(args);
    switch (prim_id) {
    case PSYMP:
        return IS_ATOM(arg) ? ctx.atom_true : 0;
    case PNUMP:
    case PNUMBERP:
        return is_numeric(arg) ? ctx.atom_true : 0;
    case PINTEGERP: {
        if (IS_NUM(arg) || IS_BIGNUM(arg))
            return ctx.atom_true;
        if (IS_INEXACT(arg)) {
            double d = to_double(arg);
            return (floor(d) == d) ? ctx.atom_true : 0;
        }
        return 0;
    }
    case PREALP: {
        if (is_numeric(arg) && !IS_COMPLEX(arg))
            return ctx.atom_true;
        return 0;
    }
    case PEXACTP:
        return is_exact(arg) ? ctx.atom_true : 0;
    case PINEXACTP:
        return (is_numeric(arg) && !is_exact(arg)) ? ctx.atom_true : 0;
    case PCOMPLEXP:
        return is_numeric(arg) ? ctx.atom_true : 0;
    case PRATIONALP:
        return (IS_NUM(arg) || IS_BIGNUM(arg) || IS_RATIONAL(arg))
                   ? ctx.atom_true
                   : 0;
    case PPROCP:
        return (IS_FUNCTION(arg) || IS_BUILTIN(arg) || IS_CONT(arg))
                   ? ctx.atom_true
                   : 0;
    case PCONSP:
        return IS_PAIR(arg) ? ctx.atom_true : 0;
    case PNULLP:
        return arg == 0 ? ctx.atom_true : 0;
    case PSTRINGP:
        return IS_STRING(arg) ? ctx.atom_true : 0;
    case PCHARP:
        return IS_CHAR(arg) ? ctx.atom_true : 0;
    case PVECTORP:
        return IS_VECTOR(arg) ? ctx.atom_true : 0;
    case PBOOLP:
        return (arg == 0 || arg == ctx.atom_true) ? ctx.atom_true : 0;
    case PLISTP: {
        unsigned x = arg;
        while (IS_PAIR(x))
            x = cdr(x);
        return x == 0 ? ctx.atom_true : 0;
    }
    default:
        return TOK_ERROR;
    }
}

// Character operations
static unsigned apply_char_primitive(unsigned prim_id, unsigned args)
{
    switch (prim_id) {
    case PCHARCODE:
        REQUIRE_ARGS(args, 1, 1, "char->integer");
        return store(CELL_ID(car(args)));
    case PCODECHAR:
        REQUIRE_ARGS(args, 1, 1, "integer->char");
        return make_char(CELL_ID(car(args)));
    case PCHARUP:
        REQUIRE_ARGS(args, 1, 1, "char-upcase");
        return make_char(toupper((int)CELL_ID(car(args))));
    case PCHARDOWN:
        REQUIRE_ARGS(args, 1, 1, "char-downcase");
        return make_char(tolower((int)CELL_ID(car(args))));
    // Character comparisons (PCHAREQ..PCHARGEI are sequential)
    case PCHAREQ:
    case PCHARLT:
    case PCHARGT:
    case PCHARLE:
    case PCHARGE:
    case PCHAREQI:
    case PCHARLTI:
    case PCHARGTI:
    case PCHARLEI:
    case PCHARGEI: {
        unsigned offset = prim_id - PCHAREQ;
        return char_compare(args, (cmp_op)(offset % 5), offset >= 5);
    }
    // Character predicates
    case PCHARALPHA:
        REQUIRE_ARGS(args, 1, 1, "char-alphabetic?");
        return isalpha((int)CELL_ID(car(args))) ? ctx.atom_true : 0;
    case PCHARNUMERIC:
        REQUIRE_ARGS(args, 1, 1, "char-numeric?");
        return isdigit((int)CELL_ID(car(args))) ? ctx.atom_true : 0;
    case PCHARWHITE:
        REQUIRE_ARGS(args, 1, 1, "char-whitespace?");
        return isspace((int)CELL_ID(car(args))) ? ctx.atom_true : 0;
    case PCHARUPPER:
        REQUIRE_ARGS(args, 1, 1, "char-upper-case?");
        return isupper((int)CELL_ID(car(args))) ? ctx.atom_true : 0;
    case PCHARLOWER:
        REQUIRE_ARGS(args, 1, 1, "char-lower-case?");
        return islower((int)CELL_ID(car(args))) ? ctx.atom_true : 0;
    default:
        return TOK_ERROR;
    }
}

// Vector operations
static unsigned apply_vector_primitive(unsigned prim_id, unsigned args)
{
    switch (prim_id) {
    case PMAKEVEC: {
        REQUIRE_ARGS(args, 1, 2, "make-vector");
        unsigned len = CELL_ID(car(args));
        unsigned fill = cdr(args) ? cadr(args) : 0;
        return make_vector(len, fill);
    }
    case PVECTOR: {
        unsigned len = list_length(args);
        unsigned vec = make_vector(len, 0);
        unsigned *data = vector_data_ptr(vec);
        unsigned i = 0;
        for (unsigned a = args; a; a = cdr(a), i++) {
            data[i] = car(a);
        }
        return vec;
    }
    case PVECREF: {
        REQUIRE_ARGS(args, 2, 2, "vector-ref");
        unsigned vec = car(args);
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector-ref: not a vector");
        int64_t idx = CELL_ID(cadr(args));
        CHECK_VECTOR_BOUNDS(idx, vec, "vector-ref");
        return vector_data_ptr(vec)[idx];
    }
    case PVECSET: {
        REQUIRE_ARGS(args, 3, 3, "vector-set!");
        unsigned vec = car(args);
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector-set!: not a vector");
        int64_t idx = CELL_ID(cadr(args));
        CHECK_VECTOR_BOUNDS(idx, vec, "vector-set!");
        vector_data_ptr(vec)[idx] = caddr(args);
        return caddr(args);
    }
    case PVECLEN: {
        REQUIRE_ARGS(args, 1, 1, "vector-length");
        unsigned vec = car(args);
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector-length: not a vector");
        return store(vector_len(vec));
    }
    case PVECFILL: {
        REQUIRE_ARGS(args, 2, 2, "vector-fill!");
        unsigned vec = car(args);
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector-fill!: not a vector");
        unsigned fill = cadr(args);
        unsigned len = vector_len(vec);
        unsigned *data = vector_data_ptr(vec);
        for (unsigned i = 0; i < len; i++)
            data[i] = fill;
        return 0;
    }
    case PLIST2VEC: {
        REQUIRE_ARGS(args, 1, 1, "list->vector");
        unsigned lst = car(args);
        unsigned len = list_length(lst);
        unsigned vec = make_vector(len, 0);
        unsigned *data = vector_data_ptr(vec);
        for (unsigned i = 0; lst; lst = cdr(lst), i++)
            data[i] = car(lst);
        return vec;
    }
    case PVEC2LIST: {
        REQUIRE_ARGS(args, 1, 1, "vector->list");
        unsigned vec = car(args);
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector->list: not a vector");
        unsigned len = vector_len(vec);
        unsigned *data = vector_data_ptr(vec);
        unsigned result = 0, tail = 0;
        for (unsigned i = 0; i < len; i++) {
            list_append(&result, &tail, data[i]);
        }
        return result;
    }
    default:
        return TOK_ERROR;
    }
}

// Math function table for simple unary functions
typedef struct {
    unsigned id;
    double (*func)(double);
    const char *name;
} math_func_entry;

static const math_func_entry math_funcs[] = {
    {PSIN, sin, "sin"},   {PCOS, cos, "cos"},   {PTAN, tan, "tan"},
    {PASIN, asin, "asin"}, {PACOS, acos, "acos"}, {PLOG, log, "log"},
    {PEXP, exp, "exp"},   {0, NULL, NULL}};

// Math functions
static unsigned apply_math_primitive(unsigned prim_id, unsigned args)
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
        REQUIRE_ARGS(args, 1, 1, "sqrt");
        double x = to_double(car(args));
        if (x < 0) {
            return store_complex(store(0), store_inexact(sqrt(-x)));
        }
        double result = sqrt(x);
        if (is_exact(car(args)) && floor(result) == result) {
            return store((int64_t)result);
        }
        return store_inexact(result);
    }
    case PEXPT: {
        REQUIRE_ARGS(args, 2, 2, "expt");
        double base = to_double(car(args));
        double exp = to_double(cadr(args));
        double result = pow(base, exp);
        if (all_exact(args) && floor(result) == result && result >= INT64_MIN &&
            result <= INT64_MAX) {
            return store((int64_t)result);
        }
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
    default:
        return TOK_ERROR;
    }
}

// ============================================================================
// Main Dispatch Function
// ============================================================================

unsigned apply_primitive(unsigned prim_id, unsigned args)
{
    switch (prim_id) {
    // Arithmetic
    case PPLUS:
        return prim_plus(args);
    case PMINUS:
        return prim_minus(args);
    case PTIMES:
        return prim_mult(args);
    case PDIV:
        return prim_div(args);
    case PMOD:
        return prim_modulo(args);
    case PREMAINDER:
        return prim_remainder(args);
    case PQUOTIENT:
        return prim_quotient(args);
    case PABS:
        return prim_abs(args);

    // Numeric comparison
    case PEQUAL:
        return numeric_compare(args, CMP_EQ);
    case PLT:
        return numeric_compare(args, CMP_LT);
    case PGT:
        return numeric_compare(args, CMP_GT);
    case PLEQ:
        return numeric_compare(args, CMP_LE);
    case PGEQ:
        return numeric_compare(args, CMP_GE);

    // Logic
    case PNOT:
        REQUIRE_ARGS(args, 1, 1, "not");
        return car(args) ? 0 : ctx.atom_true;
    case PEQ: {
        unsigned arg1 = car(args);
        unsigned arg2 = cadr(args);
        if (CELL_TYPE(arg1) != CELL_TYPE(arg2))
            return 0;
        switch (CELL_TYPE(arg1)) {
        case BT_NUM:
        case BT_FUNCTION:
        case BT_BUILTIN:
        case BT_ATOM:
            return CELL_ID(arg1) == CELL_ID(arg2)
                       ? ctx.atom_true
                       : 0;
        default:
            return arg1 == arg2 ? ctx.atom_true : 0;
        }
    }
    case PEQUALP:
        REQUIRE_ARGS(args, 2, 2, "equal?");
        return deep_equal(car(args), cadr(args)) ? ctx.atom_true : 0;

    // List operations
    case PCONS:
        return alloc_cons(car(args), cadr(args));
    case PCAR:
        return caar(args);
    case PCDR:
        return cdar(args);
    case PSETCAR: {
        REQUIRE_ARGS(args, 2, 2, "set-car!");
        unsigned arg1 = car(args);
        if (!IS_PAIR(arg1))
            ERROR_RETURN("set-car!: not a pair");
        unsigned arg2 = cadr(args);
        return CELL_CAR(arg1) = arg2;
    }
    case PSETCDR: {
        REQUIRE_ARGS(args, 2, 2, "set-cdr!");
        unsigned arg1 = car(args);
        if (!IS_PAIR(arg1))
            ERROR_RETURN("set-cdr!: not a pair");
        unsigned arg2 = cadr(args);
        return CELL_CDR(arg1) = arg2;
    }
    case PLIST:
        return args;
    case PLENGTH: {
        REQUIRE_ARGS(args, 1, 1, "length");
        unsigned lst = car(args);
        if (CELL_TYPE(lst) == BT_STRING)
            return store(strlen((char *)(intptr_t)CELL_ID(lst)));
        if (CELL_TYPE(lst) == BT_VECTOR)
            return store(vector_len(lst));
        return store(list_length(lst));
    }
    case PAPPEND:
        return prim_append(args);
    case PREVERSE:
        return prim_reverse(args);
    case PLASTPAIR: {
        REQUIRE_ARGS(args, 1, 1, "last-pair");
        unsigned lst = car(args);
        if (!lst || CELL_TYPE(lst) != BT_CONS) {
            show_error("last-pair: not a pair");
            return TOK_ERROR;
        }
        while (cdr(lst) && CELL_TYPE(cdr(lst)) == BT_CONS)
            lst = cdr(lst);
        return lst;
    }

    // Type predicates - delegated to category function
    case PSYMP:
    case PNUMP:
    case PNUMBERP:
    case PINTEGERP:
    case PREALP:
    case PEXACTP:
    case PINEXACTP:
    case PCOMPLEXP:
    case PRATIONALP:
    case PPROCP:
    case PCONSP:
    case PNULLP:
    case PSTRINGP:
    case PCHARP:
    case PVECTORP:
    case PBOOLP:
    case PLISTP:
        return apply_type_predicate(prim_id, args);

    // I/O - all support optional port argument
    case PDISPLAY: {
        REQUIRE_ARGS(args, 1, 2, "display");
        unsigned arg = car(args);
        FILE *fport;
        string_port *sport;
        int ptype = extract_output_port_ex(args, &fport, &sport, "display");
        if (ptype < 0)
            return TOK_ERROR;
        if (ptype == 1) {
            // String port: use open_memstream to capture output
            char *buf = NULL;
            size_t buflen = 0;
            FILE *memfp = open_memstream(&buf, &buflen);
            if (IS_STRING(arg)) {
                fprintf(memfp, "%s", GET_STRING_PTR(arg));
            } else {
                display_obj_port(arg, memfp);
            }
            fclose(memfp);
            strport_puts(sport, buf);
            free(buf);
        } else {
            if (IS_STRING(arg)) {
                fprintf(fport, "%s", GET_STRING_PTR(arg));
            } else {
                display_obj_port(arg, fport);
            }
            fflush(fport);
        }
        return arg;
    }
    case PWRITE: {
        REQUIRE_ARGS(args, 1, 2, "write");
        unsigned arg = car(args);
        FILE *fport;
        string_port *sport;
        int ptype = extract_output_port_ex(args, &fport, &sport, "write");
        if (ptype < 0)
            return TOK_ERROR;
        if (ptype == 1) {
            // String port: use open_memstream to capture output
            char *buf = NULL;
            size_t buflen = 0;
            FILE *memfp = open_memstream(&buf, &buflen);
            write_obj_port(arg, memfp);
            fclose(memfp);
            strport_puts(sport, buf);
            free(buf);
        } else {
            write_obj_port(arg, fport);
            fflush(fport);
        }
        return arg;
    }
    case PNEWLINE: {
        // newline takes optional port as first arg, not second
        if (args) {
            unsigned p = car(args);
            if (IS_STROUTPORT(p)) {
                string_port *sport = GET_STRPORT_PTR(p);
                if (!sport) {
                    show_error("newline: port is closed");
                    return TOK_ERROR;
                }
                strport_putc(sport, '\n');
                return 0;
            }
            if (!IS_OUTPORT(p)) {
                show_error("newline: argument must be output port");
                return TOK_ERROR;
            }
            FILE *port = GET_PORT_PTR(p);
            fprintf(port, "\n");
            fflush(port);
        } else {
            fprintf(ctx.current_output, "\n");
            fflush(ctx.current_output);
        }
        return 0;
    }
    case PREAD: {
        FILE *port;
        if (!extract_input_port(args, &port, "read"))
            return TOK_ERROR;
        return read_obj_port(port);
    }
    case PREADCHAR: {
        FILE *fport;
        string_port *sport;
        int ptype = extract_input_port_ex(args, &fport, &sport, "read-char");
        if (ptype < 0)
            return TOK_ERROR;
        int c;
        if (ptype == 1) {
            c = strport_getc(sport);
        } else {
            c = fgetc(fport);
        }
        if (c == EOF)
            return atom_from_string("eof-object");
        return make_char(c);
    }
    case PPEEKCHAR: {
        FILE *fport;
        string_port *sport;
        int ptype = extract_input_port_ex(args, &fport, &sport, "peek-char");
        if (ptype < 0)
            return TOK_ERROR;
        int c;
        if (ptype == 1) {
            c = strport_peekc(sport);
        } else {
            c = fgetc(fport);
            if (c != EOF)
                ungetc(c, fport);
        }
        if (c == EOF)
            return atom_from_string("eof-object");
        return make_char(c);
    }
    case PWRITECHAR: {
        REQUIRE_ARGS(args, 1, 2, "write-char");
        int c = (int)CELL_ID(car(args));
        FILE *fport;
        string_port *sport;
        int ptype = extract_output_port_ex(args, &fport, &sport, "write-char");
        if (ptype < 0)
            return TOK_ERROR;
        if (ptype == 1) {
            strport_putc(sport, c);
        } else {
            fputc(c, fport);
            fflush(fport);
        }
        return 0;
    }
    case PEOF: {
        REQUIRE_ARGS(args, 1, 1, "eof-object?");
        unsigned arg = car(args);
        return (CELL_TYPE(arg) == BT_ATOM &&
                strcmp(ctx.atom_table[CELL_ID(arg)], "eof-object") ==
                    0)
                   ? ctx.atom_true
                   : 0;
    }
    case PCHARREADY: {
        FILE *port;
        if (!extract_input_port(args, &port, "char-ready?"))
            return TOK_ERROR;
        // For simplicity, always return true (full implementation would use
        // select/poll)
        (void)port;
        return ctx.atom_true;
    }

    // Ports
    case POPENINPUT: {
        REQUIRE_ARGS(args, 1, 1, "open-input-file");
        CHECK_STRING(car(args), "open-input-file");
        char *filename = GET_STRING_PTR(car(args));
        FILE *f = fopen(filename, "r");
        if (!f) {
            show_error("open-input-file: cannot open %s", filename);
            return TOK_ERROR;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_INPORT;
        CELL_ID(p) = STORE_PTR(f);
        return p;
    }
    case POPENOUTPUT: {
        REQUIRE_ARGS(args, 1, 1, "open-output-file");
        CHECK_STRING(car(args), "open-output-file");
        char *filename = GET_STRING_PTR(car(args));
        FILE *f = fopen(filename, "w");
        if (!f) {
            show_error("open-output-file: cannot open %s", filename);
            return TOK_ERROR;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_OUTPORT;
        CELL_ID(p) = STORE_PTR(f);
        return p;
    }
    case PCLOSEINPUT:
    case PCLOSEOUTPUT: {
        const char *name =
            prim_id == PCLOSEINPUT ? "close-input-port" : "close-output-port";
        REQUIRE_ARGS(args, 1, 1, name);
        unsigned port = car(args);
        // Handle string ports
        if (IS_STRINPORT(port) || IS_STROUTPORT(port)) {
            string_port *sp = GET_STRPORT_PTR(port);
            if (sp)
                strport_free(sp);
            CELL_ID(port) = 0;
            return 0;
        }
        if (!IS_INPORT(port) && !IS_OUTPORT(port)) {
            show_error("%s: not a port", name);
            return TOK_ERROR;
        }
        FILE *f = GET_PORT_PTR(port);
        if (f && f != stdin && f != stdout)
            fclose(f);
        CELL_ID(port) = 0;
        return 0;
    }
    case PINPUTPORTP: {
        REQUIRE_ARGS(args, 1, 1, "input-port?");
        return IS_INPUT_PORT(car(args)) ? ctx.atom_true : 0;
    }
    case POUTPUTPORTP: {
        REQUIRE_ARGS(args, 1, 1, "output-port?");
        return IS_OUTPUT_PORT(car(args)) ? ctx.atom_true : 0;
    }
    case PCURRENTINPUT: {
        unsigned p = alloc();
        CELL_TYPE(p) = BT_INPORT;
        CELL_ID(p) = STORE_PTR(ctx.current_input);
        return p;
    }
    case PCURRENTOUTPUT: {
        unsigned p = alloc();
        CELL_TYPE(p) = BT_OUTPORT;
        CELL_ID(p) = STORE_PTR(ctx.current_output);
        return p;
    }

    // String operations
    case PSTRLEN: {
        REQUIRE_ARGS(args, 1, 1, "string-length");
        CHECK_STRING(car(args), "string-length");
        char *s = GET_STRING_PTR(car(args));
        return store(strlen(s));
    }
    case PSTRREF: {
        REQUIRE_ARGS(args, 2, 2, "string-ref");
        CHECK_STRING(car(args), "string-ref");
        char *s = GET_STRING_PTR(car(args));
        int64_t idx = CELL_ID(cadr(args));
        if (idx < 0 || idx >= (int64_t)strlen(s)) {
            show_error("string-ref: index out of bounds");
            return TOK_ERROR;
        }
        return make_char(s[idx]);
    }
    case PSTRSET: {
        REQUIRE_ARGS(args, 3, 3, "string-set!");
        CHECK_STRING(car(args), "string-set!");
        char *s = GET_STRING_PTR(car(args));
        int64_t idx = CELL_ID(cadr(args));
        char c = (char)CELL_ID(caddr(args));
        if (idx < 0 || idx >= (int64_t)strlen(s)) {
            show_error("string-set!: index out of bounds");
            return TOK_ERROR;
        }
        s[idx] = c;
        return 0;
    }
    case PSTRAPP:
        return prim_string_append(args);
    case PSUBSTR:
        return prim_substring(args);
    case PSTR2SYM: {
        REQUIRE_ARGS(args, 1, 1, "string->symbol");
        CHECK_STRING(car(args), "string->symbol");
        char *s = GET_STRING_PTR(car(args));
        return atom_from_string(s);
    }
    case PSYM2STR: {
        REQUIRE_ARGS(args, 1, 1, "symbol->string");
        CHECK_SYMBOL(car(args), "symbol->string");
        const char *s = ctx.atom_table[CELL_ID(car(args))];
        char *copy = strdup(s);
        if (!copy) {
            show_error("symbol->string: out of memory");
            return TOK_ERROR;
        }
        return make_string_owned(copy);
    }
    case PNUM2STR: {
        REQUIRE_ARGS(args, 1, 2, "number->string");
        unsigned num = car(args);
        int radix = cdr(args) ? (int)CELL_ID(cadr(args)) : 10;
        if (radix < 2 || radix > 36) {
            show_error("number->string: radix must be between 2 and 36");
            return TOK_ERROR;
        }
        char buf[NUMBER_BUF_SIZE];
        if (IS_NUM(num)) {
            int64_t n = CELL_ID(num);
            if (radix == 10) {
                snprintf(buf, sizeof(buf), "%" PRId64, n);
            } else {
                // Convert to specified radix
                char *p = buf + sizeof(buf) - 1;
                *p = '\0';
                bool neg = n < 0;
                if (neg)
                    n = -n;
                if (n == 0) {
                    *--p = '0';
                } else {
                    while (n > 0) {
                        int d = n % radix;
                        *--p = (d < 10) ? '0' + d : 'a' + d - 10;
                        n /= radix;
                    }
                }
                if (neg)
                    *--p = '-';
                memmove(buf, p, buf + sizeof(buf) - p);
            }
        } else if (IS_INEXACT(num)) {
            if (radix != 10) {
                show_error("number->string: inexact numbers require radix 10");
                return TOK_ERROR;
            }
            double d = to_double(num);
            snprintf(buf, sizeof(buf), "%g", d);
        } else {
            show_error("number->string: not a number");
            return TOK_ERROR;
        }
        char *copy = strdup(buf);
        if (!copy) {
            show_error("number->string: out of memory");
            return TOK_ERROR;
        }
        return make_string_owned(copy);
    }
    case PSTR2NUM: {
        REQUIRE_ARGS(args, 1, 2, "string->number");
        CHECK_STRING(car(args), "string->number");
        char *s = GET_STRING_PTR(car(args));
        int radix = cdr(args) ? (int)CELL_ID(cadr(args)) : 10;
        if (radix < 2 || radix > 36) {
            show_error("string->number: radix must be between 2 and 36");
            return TOK_ERROR;
        }
        if (radix == 10) {
            // Use standard parsing which handles floats
            return atom_from_string(s);
        }
        // Parse integer in specified radix
        char *end;
        long long val = strtoll(s, &end, radix);
        if (end == s || *end != '\0') {
            return 0; // Return #f for invalid number
        }
        return store(val);
    }
    case PMAKESTR: {
        REQUIRE_ARGS(args, 1, 2, "make-string");
        int64_t len = CELL_ID(car(args));
        if (len < 0) {
            show_error("make-string: negative length");
            return TOK_ERROR;
        }
        char fill = cdr(args) ? (char)CELL_ID(cadr(args)) : ' ';
        char *s = malloc(len + 1);
        if (!s) {
            show_error("make-string: out of memory");
            return TOK_ERROR;
        }
        memset(s, fill, len);
        s[len] = '\0';
        return make_string_owned(s);
    }
    case PSTRCOPY: {
        REQUIRE_ARGS(args, 1, 1, "string-copy");
        CHECK_STRING(car(args), "string-copy");
        char *s = GET_STRING_PTR(car(args));
        char *copy = strdup(s);
        if (!copy) {
            show_error("string-copy: out of memory");
            return TOK_ERROR;
        }
        return make_string_owned(copy);
    }
    case PSTR2LIST: {
        REQUIRE_ARGS(args, 1, 1, "string->list");
        CHECK_STRING(car(args), "string->list");
        char *s = GET_STRING_PTR(car(args));
        unsigned result = 0;
        for (int i = strlen(s) - 1; i >= 0; i--) {
            result = alloc_cons(make_char(s[i]), result);
        }
        return result;
    }
    case PLIST2STR: {
        REQUIRE_ARGS(args, 1, 1, "list->string");
        unsigned lst = car(args);
        size_t len = list_length(lst);
        char *s = malloc(len + 1);
        if (!s) {
            show_error("list->string: out of memory");
            return TOK_ERROR;
        }
        size_t i = 0;
        for (; lst; lst = cdr(lst), i++) {
            s[i] = (char)CELL_ID(car(lst));
        }
        s[i] = '\0';
        return make_string_owned(s);
    }
    case PSTRFILL: {
        REQUIRE_ARGS(args, 2, 2, "string-fill!");
        CHECK_STRING(car(args), "string-fill!");
        char *s = GET_STRING_PTR(car(args));
        int c = (int)CELL_ID(cadr(args));
        size_t len = strlen(s);
        for (size_t i = 0; i < len; i++)
            s[i] = c;
        return 0;
    }

    // String comparisons (PSTREQ..PSTRGEI are sequential)
    case PSTREQ:
    case PSTRLT:
    case PSTRGT:
    case PSTRLE:
    case PSTRGE:
    case PSTREQI:
    case PSTRLTI:
    case PSTRGTI:
    case PSTRLEI:
    case PSTRGEI: {
        unsigned offset = prim_id - PSTREQ;
        return string_compare(args, (cmp_op)(offset % 5), offset >= 5);
    }

    // Character operations - delegated to category function
    case PCHARCODE:
    case PCODECHAR:
    case PCHARUP:
    case PCHARDOWN:
    case PCHAREQ:
    case PCHARLT:
    case PCHARGT:
    case PCHARLE:
    case PCHARGE:
    case PCHAREQI:
    case PCHARLTI:
    case PCHARGTI:
    case PCHARLEI:
    case PCHARGEI:
    case PCHARALPHA:
    case PCHARNUMERIC:
    case PCHARWHITE:
    case PCHARUPPER:
    case PCHARLOWER:
        return apply_char_primitive(prim_id, args);

    // Vector operations - delegated to category function
    case PMAKEVEC:
    case PVECTOR:
    case PVECREF:
    case PVECSET:
    case PVECLEN:
    case PVECFILL:
    case PLIST2VEC:
    case PVEC2LIST:
        return apply_vector_primitive(prim_id, args);

    // Math functions - delegated to category function
    case PSQRT:
    case PEXPT:
    case PSIN:
    case PCOS:
    case PTAN:
    case PASIN:
    case PACOS:
    case PATAN:
    case PLOG:
    case PEXP:
    case PFLOOR:
    case PCEILING:
    case PTRUNCATE:
    case PROUND:
        return apply_math_primitive(prim_id, args);

    // Misc
    case PERROR: {
        fprintf(stderr, "error: ");
        FORLIST(a, args) {
            display_obj(car(a));
            if (cdr(a))
                fprintf(stderr, " ");
        }
        fprintf(stderr, "\n");
        return TOK_ERROR;
    }
    case PGENSYM: {
        char buf[32];
        snprintf(buf, sizeof(buf), "g%u", gensym_counter++);
        return atom_from_string(buf);
    }
    // PGCFLIP is handled specially in eval.c (needs environment as root)

    // R3RS Numeric Tower
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
            return store_complex(store((int64_t)round(real)),
                                 store((int64_t)round(imag)));
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
        int64_t lo_n = 0, lo_d = 1;  // 0/1
        int64_t hi_n = 1, hi_d = 0;  // 1/0 = infinity
        int64_t mid_n, mid_d;

        for (int iter = 0; iter < 100; iter++) {
            mid_n = lo_n + hi_n;
            mid_d = lo_d + hi_d;
            double mid = (double)mid_n / mid_d;

            if (fabs(mid - x) <= epsilon) {
                // Found it - return as rational
                if (negative)
                    mid_n = -mid_n;
                unsigned num = store(mid_n);
                unsigned den = store(mid_d);
                unsigned r = alloc();
                CELL_TYPE(r) = BT_RATIONAL;
                CELL_CAR(r) = num;
                CELL_CDR(r) = den;
                return r;
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
        unsigned num = store(mid_n);
        unsigned den = store(mid_d);
        unsigned r = alloc();
        CELL_TYPE(r) = BT_RATIONAL;
        CELL_CAR(r) = num;
        CELL_CDR(r) = den;
        return r;
    }

    // String constructor
    case PSTRING: {
        // (string char ...) - construct string from characters
        unsigned len = list_length(args);
        char *s = malloc(len + 1);
        if (!s) {
            show_error("string: out of memory");
            return TOK_ERROR;
        }
        unsigned i = 0;
        for (unsigned a = args; a; a = cdr(a), i++) {
            unsigned ch = car(a);
            if (!IS_CHAR(ch)) {
                free(s);
                show_error("string: argument is not a character");
                return TOK_ERROR;
            }
            s[i] = (char)CELL_ID(ch);
        }
        s[len] = '\0';
        return make_string_owned(s);
    }

    // Transcript
    case PTRANSCRIPTON: {
        REQUIRE_ARGS(args, 1, 1, "transcript-on");
        CHECK_STRING(car(args), "transcript-on");
        if (ctx.transcript) {
            show_error("transcript-on: transcript already active");
            return TOK_ERROR;
        }
        char *filename = GET_STRING_PTR(car(args));
        ctx.transcript = fopen(filename, "w");
        if (!ctx.transcript) {
            show_error("transcript-on: cannot open %s", filename);
            return TOK_ERROR;
        }
        return 0;
    }
    case PTRANSCRIPTOFF: {
        if (!ctx.transcript) {
            show_error("transcript-off: no transcript active");
            return TOK_ERROR;
        }
        fclose(ctx.transcript);
        ctx.transcript = NULL;
        return 0;
    }

    // R5RS multiple values
    case PVALUES: {
        // (values) => single unspecified value
        // (values x) => x
        // (values x y ...) => multiple values object
        if (!args)
            return 0; // No values = unspecified
        if (!cdr(args))
            return car(args); // Single value
        // Multiple values - wrap in BT_MULTIVAL
        unsigned mv = alloc();
        CELL_TYPE(mv) = BT_MULTIVAL;
        CELL_CAR(mv) = args;
        CELL_CDR(mv) = 0;
        return mv;
    }

    // R5RS environment procedures
    case PSCHEMEENV: {
        REQUIRE_ARGS(args, 1, 1, "scheme-report-environment");
        int64_t version = CELL_ID(car(args));
        if (version != 5) {
            show_error("scheme-report-environment: unsupported version %lld",
                       (long long)version);
            return TOK_ERROR;
        }
        return default_environment();
    }
    case PNULLENV: {
        REQUIRE_ARGS(args, 1, 1, "null-environment");
        int64_t version = CELL_ID(car(args));
        if (version != 5) {
            show_error("null-environment: unsupported version %lld",
                       (long long)version);
            return TOK_ERROR;
        }
        // Return environment with only syntax bindings (no procedures)
        // Need to include #t for boolean values
        unsigned env = empty_environment();
        defvar(ctx.atom_true, ctx.atom_true, env);
        return env;
    }

    // Special cases handled elsewhere
    case PAPPLY:
        show_error(
            "apply: internal error - should be handled in apply_function");
        return TOK_ERROR;
    case PLOAD:
        show_error(
            "load: internal error - should be handled in apply_function");
        return TOK_ERROR;
    case PCALLCC:
        show_error("call/cc must be called as a function, not a primitive");
        return TOK_ERROR;
    case PCALLWITHVALUES:
        show_error("call-with-values: internal error - should be handled in "
                   "apply_function");
        return TOK_ERROR;
    case PEVAL:
        show_error(
            "eval: internal error - should be handled in apply_function");
        return TOK_ERROR;
    case PINTERACTIONENV:
        show_error("interaction-environment: internal error - should be "
                   "handled in apply_function");
        return TOK_ERROR;

    // String ports
    case POPENOUTPUTSTRING: {
        REQUIRE_ARGS(args, 0, 0, "open-output-string");
        string_port *sp = strport_new();
        if (!sp) {
            show_error("open-output-string: out of memory");
            return TOK_ERROR;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_STROUTPORT;
        CELL_ID(p) = STORE_PTR(sp);
        return p;
    }
    case PGETOUTPUTSTRING: {
        REQUIRE_ARGS(args, 1, 1, "get-output-string");
        unsigned port = car(args);
        if (!IS_STROUTPORT(port)) {
            show_error("get-output-string: not a string output port");
            return TOK_ERROR;
        }
        string_port *sp = GET_STRPORT_PTR(port);
        if (!sp) {
            show_error("get-output-string: port is closed");
            return TOK_ERROR;
        }
        // Copy the string to a new BT_STRING cell
        char *copy = malloc(sp->len + 1);
        if (!copy) {
            show_error("get-output-string: out of memory");
            return TOK_ERROR;
        }
        memcpy(copy, sp->data, sp->len + 1);
        return make_string_owned(copy);
    }
    case POPENINPUTSTRING: {
        REQUIRE_ARGS(args, 1, 1, "open-input-string");
        unsigned str = car(args);
        if (!IS_STRING(str)) {
            show_error("open-input-string: not a string");
            return TOK_ERROR;
        }
        string_port *sp = strport_from_string(GET_STRING_PTR(str));
        if (!sp) {
            show_error("open-input-string: out of memory");
            return TOK_ERROR;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_STRINPORT;
        CELL_ID(p) = STORE_PTR(sp);
        return p;
    }
    case PSTRINGPORTP: {
        REQUIRE_ARGS(args, 1, 1, "string-port?");
        unsigned a = car(args);
        return (IS_STRINPORT(a) || IS_STROUTPORT(a)) ? ctx.atom_true : 0;
    }

    default:
        show_error("unknown primitive: %u", prim_id);
        return TOK_ERROR;
    }
}
