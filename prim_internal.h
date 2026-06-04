/**
 * @file prim_internal.h
 * @brief Internal header for primitive operation modules
 *
 * This header contains shared types, macros, and helper functions used
 * across the primitive operation modules (prim_*.c files).
 */

#ifndef PRIM_INTERNAL_H
#define PRIM_INTERNAL_H

#define _POSIX_C_SOURCE 200809L

#include "context.h"
#include "env.h"
#include "reader.h"
#include "writer.h"
#include <ctype.h>
#include <inttypes.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

static inline unsigned scheme_bool(bool value)
{
    return value ? ctx.atom_true : ctx.atom_false;
}

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

static inline void classify_arg_value(unsigned x, numeric_level *level,
                                      bool *all_exact)
{
    if (!is_numeric(x) || IS_FIXNUM(x) || !IS_CELL(x))
        return;

    switch (CELL_TYPE(x)) {
    case BT_COMPLEX:
        if (!is_exact(x))
            *all_exact = false;
        *level = NUM_COMPLEX;
        break;
    case BT_INEXACT:
        *all_exact = false;
        if (*level < NUM_INEXACT)
            *level = NUM_INEXACT;
        break;
    case BT_RATIONAL:
        if (*level < NUM_RATIONAL)
            *level = NUM_RATIONAL;
        break;
    case BT_BIGNUM:
        if (*level < NUM_BIGNUM)
            *level = NUM_BIGNUM;
        break;
    case BT_NUM:
        break;
    default:
        break;
    }
}

// Classify a list of arguments in a single pass
static inline numeric_level classify_args(unsigned args, bool *all_exact_out)
{
    numeric_level level = NUM_INTEGER;
    bool all_ex = true;

    FORLIST(a, args)
    {
        classify_arg_value(car(a), &level, &all_ex);
    }

    if (all_exact_out)
        *all_exact_out = all_ex;
    return level;
}

static inline numeric_level classify_args_argv(unsigned argc, unsigned *argv,
                                               bool *all_exact_out)
{
    numeric_level level = NUM_INTEGER;
    bool all_ex = true;

    for (unsigned i = 0; i < argc; i++) {
        classify_arg_value(argv[i], &level, &all_ex);
    }

    if (all_exact_out)
        *all_exact_out = all_ex;
    return level;
}

static inline bool all_exact(unsigned args)
{
    bool exact;
    classify_args(args, &exact);
    return exact;
}

// ============================================================================
// Numeric Tower Helpers
// ============================================================================

// Get real and imaginary parts of any number (as doubles)
static inline void get_complex_parts(unsigned x, double *real, double *imag)
{
    if (x == 0) {
        *real = 0.0;
        *imag = 0.0;
        return;
    }
    if (!is_numeric(x)) {
        *real = 0.0;
        *imag = 0.0;
        return;
    }
    if (IS_COMPLEX(x)) {
        *real = to_double(CELL_CAR(x));
        *imag = to_double(CELL_CDR(x));
    } else {
        *real = to_double(x);
        *imag = 0.0;
    }
}

// Get real and imaginary parts as cells (preserves exactness)
static inline void get_complex_cells(unsigned x, unsigned *real, unsigned *imag)
{
    if (x == 0) {
        *real = store(0);
        gc_protect(real);
        *imag = store(0);
        gc_unprotect(1);
        return;
    }
    if (!is_numeric(x)) {
        *real = store(0);
        gc_protect(real);
        *imag = store(0);
        gc_unprotect(1);
        return;
    }
    if (IS_COMPLEX(x)) {
        *real = CELL_CAR(x);
        *imag = CELL_CDR(x);
    } else {
        *real = x;
        gc_protect(real);
        *imag = store(0);
        gc_unprotect(1);
    }
}

// Check if a complex number (or any number) is exact
static inline bool is_complex_exact(unsigned x)
{
    if (!is_numeric(x))
        return false;
    if (IS_COMPLEX(x)) {
        return is_exact(CELL_CAR(x)) && is_exact(CELL_CDR(x));
    }
    return is_exact(x);
}

static inline bool is_zero_number(unsigned x);

// Create complex result, simplifying to real if imaginary part is zero
// Preserves exactness of components
static inline unsigned make_complex_exact(unsigned real, unsigned imag)
{
    if (is_zero_number(imag))
        return real;
    return store_complex(real, imag);
}

// Create result with appropriate exactness
static inline unsigned make_real_result(double val, bool exact)
{
    const double int64_min_magnitude = 0x1p63;
    if (exact && floor(val) == val && val >= -int64_min_magnitude &&
        val < int64_min_magnitude) {
        return store((int64_t)val);
    }
    return store_inexact(val);
}

// Create complex result, simplifying to real if imaginary part is zero
static inline unsigned make_complex_result(double real, double imag, bool exact)
{
    if (imag == 0.0)
        return make_real_result(real, exact);
    GC_GUARD;
    unsigned real_part = make_real_result(real, exact);
    gc_protect(&real_part);
    unsigned imag_part = make_real_result(imag, exact);
    gc_protect(&imag_part);
    return store_complex(real_part, imag_part);
}

// Create inexact complex result (convenience for division etc.)
static inline unsigned make_complex_inexact(double real, double imag)
{
    if (imag == 0.0)
        return store_inexact(real);
    GC_GUARD;
    unsigned real_part = store_inexact(real);
    gc_protect(&real_part);
    unsigned imag_part = store_inexact(imag);
    gc_protect(&imag_part);
    return store_complex(real_part, imag_part);
}

// Helper to get rational components (num, denom) from any exact number
// NOTE: Only works correctly if num/denom fit in int64_t
static inline void get_rational_parts(unsigned x, int64_t *num, int64_t *denom)
{
    if (IS_FIXNUM(x)) {
        *num = FIXNUM_VALUE(x);
        *denom = 1;
        return;
    }
    if (!IS_CELL(x)) {
        *num = 0;
        *denom = 1;
        return;
    }
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

// Helper to get rational components as cell references (supports bignums)
static inline void get_rational_cells(unsigned x, unsigned *num,
                                      unsigned *denom)
{
    if (IS_FIXNUM(x)) {
        *num = x;
        gc_protect(num);
        *denom = store(1);
        gc_unprotect(1);
        return;
    }
    if (!IS_CELL(x)) {
        *num = store(0);
        gc_protect(num);
        *denom = store(1);
        gc_unprotect(1);
        return;
    }
    enum lisp_type t = CELL_TYPE(x);
    if (t == BT_NUM || t == BT_BIGNUM) {
        *num = x;
        gc_protect(num);
        *denom = store(1);
        gc_unprotect(1);
    } else if (t == BT_RATIONAL) {
        *num = CELL_CAR(x);
        *denom = CELL_CDR(x);
    } else {
        *num = store(0);
        gc_protect(num);
        *denom = store(1);
        gc_unprotect(1);
    }
}

static inline bool to_bignum_pair(unsigned a, unsigned b, bignum **ba,
                                  bignum **bb, const char *name)
{
    *ba = to_bignum(a);
    *bb = to_bignum(b);
    if (!*ba || !*bb) {
        bn_free(*ba);
        bn_free(*bb);
        show_error("%s: out of memory", name);
        return false;
    }
    return true;
}

static inline bool bn_from_int_pair(int64_t a, int64_t b, bignum **ba,
                                    bignum **bb, const char *name)
{
    *ba = bn_from_int(a);
    *bb = bn_from_int(b);
    if (!*ba || !*bb) {
        bn_free(*ba);
        bn_free(*bb);
        show_error("%s: out of memory", name);
        return false;
    }
    return true;
}

// Multiply two integer cells (BT_NUM or BT_BIGNUM), returns new cell.
// NOTE: Allocates memory. Callers should use gc_protect on inputs if they
// need to survive potential GC during allocation.
static inline unsigned multiply_cells(unsigned a, unsigned b)
{
    // Fast path: both are small integers and won't overflow
    if (IS_NUM(a) && IS_NUM(b)) {
        int64_t result;
        if (!__builtin_mul_overflow(CELL_ID(a), CELL_ID(b), &result)) {
            return store(result);
        }
    }
    // Slow path: use bignum arithmetic
    bignum *ba, *bb;
    if (!to_bignum_pair(a, b, &ba, &bb, "*"))
        return TOK_ERROR;
    bignum *result = bn_mul(ba, bb);
    bn_free(ba);
    bn_free(bb);
    if (!result) {
        show_error("*: out of memory");
        return TOK_ERROR;
    }
    return store_integer(result);
}

// Add two integer cells (BT_NUM or BT_BIGNUM), returns new cell.
// NOTE: Allocates memory. Callers should use gc_protect on inputs if they
// need to survive potential GC during allocation.
static inline unsigned add_cells(unsigned a, unsigned b)
{
    // Fast path: both are small integers and won't overflow
    if (IS_NUM(a) && IS_NUM(b)) {
        int64_t result;
        if (!__builtin_add_overflow(CELL_ID(a), CELL_ID(b), &result)) {
            return store(result);
        }
    }
    // Slow path: use bignum arithmetic
    bignum *ba, *bb;
    if (!to_bignum_pair(a, b, &ba, &bb, "+"))
        return TOK_ERROR;
    if (!bn_add_ip_checked(ba, bb)) {
        bn_free(ba);
        bn_free(bb);
        show_error("+: out of memory");
        return TOK_ERROR;
    }
    bn_free(bb);
    return store_integer(ba);
}

// Subtract two integer cells (BT_NUM or BT_BIGNUM), returns new cell.
// NOTE: Allocates memory. Callers should use gc_protect on inputs if they
// need to survive potential GC during allocation.
static inline unsigned subtract_cells(unsigned a, unsigned b)
{
    // Fast path: both are small integers and won't overflow
    if (IS_NUM(a) && IS_NUM(b)) {
        int64_t result;
        if (!__builtin_sub_overflow(CELL_ID(a), CELL_ID(b), &result)) {
            return store(result);
        }
    }
    // Slow path: use bignum arithmetic
    bignum *ba, *bb;
    if (!to_bignum_pair(a, b, &ba, &bb, "-"))
        return TOK_ERROR;
    if (!bn_sub_ip_checked(ba, bb)) {
        bn_free(ba);
        bn_free(bb);
        show_error("-: out of memory");
        return TOK_ERROR;
    }
    bn_free(bb);
    return store_integer(ba);
}

// Check if an integer cell (BT_NUM or BT_BIGNUM) is zero
static inline bool is_zero_cell(unsigned x)
{
    if (IS_FIXNUM(x))
        return FIXNUM_VALUE(x) == 0;
    if (IS_NUM(x))
        return CELL_ID(x) == 0;
    if (IS_BIGNUM(x)) {
        bignum *bn = get_bignum(x);
        return bn && bn_is_zero(bn);
    }
    return false;
}

static inline bool is_zero_number(unsigned x)
{
    if (!is_numeric(x))
        return false;
    if (is_zero_cell(x))
        return true;
    if (IS_RATIONAL(x))
        return is_zero_number(CELL_CAR(x));
    if (IS_INEXACT(x))
        return to_double(x) == 0.0;
    if (IS_COMPLEX(x))
        return is_zero_number(CELL_CAR(x)) &&
               is_zero_number(CELL_CDR(x));
    return false;
}

static inline bool expect_exact_int64(unsigned val, int64_t *out,
                                      const char *name)
{
    if (IS_FIXNUM(val)) {
        *out = FIXNUM_VALUE(val);
        return true;
    }
    if (IS_NUM(val)) {
        *out = CELL_ID(val);
        return true;
    }
    if (IS_BIGNUM(val)) {
        bignum *bn = get_bignum(val);
        if (bn && bn_to_int64(bn, out) == 0)
            return true;
        show_error("%s: %s", name, bn ? "integer too large" : "invalid bignum");
        return false;
    }
    show_error("%s: expected exact integer", name);
    return false;
}

static inline bool exact_int64_value(unsigned val, int64_t *out)
{
    if (IS_FIXNUM(val)) {
        *out = FIXNUM_VALUE(val);
        return true;
    }
    if (IS_NUM(val)) {
        *out = CELL_ID(val);
        return true;
    }
    return false;
}

static inline bool expect_nonneg_int64(unsigned val, int64_t *out,
                                       const char *name)
{
    if (!expect_exact_int64(val, out, name))
        return false;
    if (*out < 0) {
        show_error("%s: expected non-negative integer", name);
        return false;
    }
    return true;
}

static inline bool expect_exit_code(unsigned val, int *out, const char *name)
{
    int64_t code64;
    if (!expect_exact_int64(val, &code64, name))
        return false;
    if (code64 < INT_MIN || code64 > INT_MAX) {
        show_error("%s: exit code out of range", name);
        return false;
    }
    *out = (int)code64;
    return true;
}

static inline bool check_numeric_args(unsigned args, const char *name)
{
    FORLIST(a, args)
    {
        if (!is_numeric(car(a))) {
            show_error("%s: not a number", name);
            return false;
        }
    }
    return true;
}

static inline bool check_numeric_argv(unsigned argc, unsigned *argv,
                                      const char *name)
{
    for (unsigned i = 0; i < argc; i++) {
        if (!is_numeric(argv[i])) {
            show_error("%s: not a number", name);
            return false;
        }
    }
    return true;
}

static inline bool index_in_bounds(int64_t idx, size_t len, const char *name)
{
    if ((uint64_t)idx >= len) {
        show_error("%s: index out of bounds", name);
        return false;
    }
    return true;
}

static inline bool expect_index(unsigned val, size_t len, int64_t *out,
                                const char *name)
{
    return expect_nonneg_int64(val, out, name) &&
           index_in_bounds(*out, len, name);
}

static inline bool require_number(unsigned x, const char *name)
{
    if (is_numeric(x))
        return true;
    show_error("%s: not a number", name);
    return false;
}

static inline bool require_real(unsigned x, const char *name)
{
    if (is_numeric(x) && !IS_COMPLEX(x))
        return true;
    show_error("%s: not a real number", name);
    return false;
}

static inline bool expect_char_value(unsigned val, int *out, const char *name)
{
    if (!IS_CHAR(val)) {
        show_error("%s: not a character", name);
        return false;
    }
    int64_t code = CELL_ID(val);
    if (!is_valid_unicode_scalar(code)) {
        show_error("%s: invalid character code", name);
        return false;
    }
    *out = (int)code;
    return true;
}

static inline char *require_string_ptr(unsigned value, const char *name)
{
    if (!IS_STRING(value)) {
        show_error("%s: not a string", name);
        return NULL;
    }
    char *s = GET_STRING_PTR(value);
    if (!string_is_registered(s)) {
        show_error("%s: invalid string", name);
        return NULL;
    }
    return s;
}

static inline unsigned make_pointer_cell(enum lisp_type type, void *ptr)
{
    unsigned cell = alloc();
    CELL_TYPE(cell) = type;
    CELL_PTR(cell) = ptr;
    return cell;
}

static inline file_port *file_port_new(FILE *file, bool binary, bool input,
                                       bool owns_file)
{
    file_port *port = checked_malloc_size(sizeof(file_port));
    if (!port)
        return NULL;
    port->file = file;
    port->binary = binary;
    port->input = input;
    port->owns_file = owns_file;
    file_port_register(port);
    return port;
}

static inline bool file_port_well_formed(const file_port *port)
{
    return file_port_is_registered(port);
}

static inline FILE *file_port_file(unsigned value)
{
    file_port *port = GET_FILE_PORT_PTR(value);
    return file_port_well_formed(port) ? port->file : NULL;
}

static inline unsigned make_file_port_cell(FILE *file, bool binary, bool input,
                                           bool owns_file,
                                           enum lisp_type type,
                                           const char *name)
{
    file_port *port = file_port_new(file, binary, input, owns_file);
    if (!port) {
        if (owns_file && file)
            fclose(file);
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    return make_pointer_cell(type, port);
}

static inline unsigned open_file_port(const char *filename, const char *mode,
                                      enum lisp_type type, const char *name)
{
    FILE *f = fopen(filename, mode);
    if (!f) {
        show_error("%s: cannot open %s", name, filename);
        return TOK_ERROR;
    }
    return make_file_port_cell(f, strchr(mode, 'b') != NULL,
                               type == BT_INPORT, true, type, name);
}

static inline bool flush_file_port(FILE *fport, const char *name)
{
    if (fflush(fport) != 0) {
        show_error("%s: flush failed", name);
        return false;
    }
    return true;
}

static inline bool check_argc(unsigned argc, unsigned min, unsigned max,
                              const char *name)
{
    if (argc < min) {
        show_error("%s: too few arguments (expected %u, got %u)", name, min,
                   argc);
        return false;
    }
    if (max != (unsigned)-1 && argc > max) {
        show_error("%s: too many arguments (expected at most %u)", name, max);
        return false;
    }
    return true;
}

#define REQUIRE_ARGC(argc, min, max, name)                                    \
    do {                                                                       \
        if (!check_argc(argc, min, max, name))                                 \
            return TOK_ERROR;                                                  \
    } while (0)

// ============================================================================
// Comparison Operations (type needed for binary functions below)
// ============================================================================

typedef enum { CMP_EQ, CMP_LT, CMP_GT, CMP_LE, CMP_GE } cmp_op;

// Apply comparison operation to two values
#define APPLY_CMP_OP(op, a, b)                                                 \
    ((op) == CMP_EQ   ? ((a) == (b))                                           \
     : (op) == CMP_LT ? ((a) < (b))                                            \
     : (op) == CMP_GT ? ((a) > (b))                                            \
     : (op) == CMP_LE ? ((a) <= (b))                                           \
                      : ((a) >= (b)))

// ============================================================================
// Direct Binary Arithmetic (for VM fast paths)
// ============================================================================

// Forward declarations for fallback paths
unsigned prim_plus(unsigned argc, unsigned *argv);
unsigned prim_minus(unsigned argc, unsigned *argv);
unsigned prim_mult(unsigned argc, unsigned *argv);
unsigned prim_div(unsigned argc, unsigned *argv);
unsigned prim_modulo(unsigned argc, unsigned *argv);
unsigned numeric_compare(unsigned argc, unsigned *argv, cmp_op op);

static inline unsigned apply_binary_primitive(unsigned a, unsigned b,
                                              unsigned (*prim)(unsigned,
                                                               unsigned *))
{
    unsigned argv[2] = {a, b};
    gc_protect(&argv[0]);
    gc_protect(&argv[1]);
    unsigned result = prim(2, argv);
    gc_unprotect(2);
    return result;
}

static inline unsigned apply_binary_compare(unsigned a, unsigned b, cmp_op op)
{
    unsigned argv[2] = {a, b};
    gc_protect(&argv[0]);
    gc_protect(&argv[1]);
    unsigned result = numeric_compare(2, argv, op);
    gc_unprotect(2);
    return result;
}

// Binary addition: a + b without list building
// Returns TOK_ERROR on non-numeric operands
static inline unsigned binary_add(unsigned a, unsigned b)
{
    // Fast path: both are small integers
    if (IS_NUM(a) && IS_NUM(b)) {
        int64_t va = CELL_ID(a);
        int64_t vb = CELL_ID(b);
        int64_t result;
        if (!__builtin_add_overflow(va, vb, &result)) {
            return store(result);
        }
        // Overflow - use bignum
        bignum *ba, *bb;
        if (!bn_from_int_pair(va, vb, &ba, &bb, "+"))
            return TOK_ERROR;
        if (!bn_add_ip_checked(ba, bb)) {
            bn_free(ba);
            bn_free(bb);
            show_error("+: out of memory");
            return TOK_ERROR;
        }
        bn_free(bb);
        return store_integer(ba);
    }
    // Fall back to full numeric tower
    return apply_binary_primitive(a, b, prim_plus);
}

// Binary subtraction: a - b without list building
static inline unsigned binary_sub(unsigned a, unsigned b)
{
    // Fast path: both are small integers
    if (IS_NUM(a) && IS_NUM(b)) {
        int64_t va = CELL_ID(a);
        int64_t vb = CELL_ID(b);
        int64_t result;
        if (!__builtin_sub_overflow(va, vb, &result)) {
            return store(result);
        }
        // Overflow - use bignum
        bignum *ba, *bb;
        if (!bn_from_int_pair(va, vb, &ba, &bb, "-"))
            return TOK_ERROR;
        if (!bn_sub_ip_checked(ba, bb)) {
            bn_free(ba);
            bn_free(bb);
            show_error("-: out of memory");
            return TOK_ERROR;
        }
        bn_free(bb);
        return store_integer(ba);
    }
    // Fall back to full numeric tower
    return apply_binary_primitive(a, b, prim_minus);
}

// Binary multiplication: a * b without list building
static inline unsigned binary_mul(unsigned a, unsigned b)
{
    // Fast path: both are small integers
    if (IS_NUM(a) && IS_NUM(b)) {
        int64_t va = CELL_ID(a);
        int64_t vb = CELL_ID(b);
        int64_t result;
        if (!__builtin_mul_overflow(va, vb, &result)) {
            return store(result);
        }
        // Overflow - use bignum
        bignum *ba, *bb;
        if (!bn_from_int_pair(va, vb, &ba, &bb, "*"))
            return TOK_ERROR;
        bignum *br = bn_mul(ba, bb);
        bn_free(ba);
        bn_free(bb);
        if (!br) {
            show_error("*: out of memory");
            return TOK_ERROR;
        }
        return store_integer(br);
    }
    // Fall back to full numeric tower
    return apply_binary_primitive(a, b, prim_mult);
}

// Binary division: a / b without list building
static inline unsigned binary_div(unsigned a, unsigned b)
{
    // Fast path: both are small integers with exact division
    if (IS_NUM(a) && IS_NUM(b)) {
        int64_t va = CELL_ID(a);
        int64_t vb = CELL_ID(b);
        if (vb == 0) {
            show_error("/: division by zero");
            return TOK_ERROR;
        }
        if (va == INT64_MIN && vb == -1) {
            bignum *bn = bn_from_int(va);
            if (!bn) {
                show_error("/: out of memory");
                return TOK_ERROR;
            }
            bn_neg_ip(bn);
            return store_integer(bn);
        }
        if (va % vb == 0) {
            // Exact division
            return store(va / vb);
        }
        // Result is rational
        return normalize_rational(va, vb);
    }
    // Fall back to full numeric tower
    return apply_binary_primitive(a, b, prim_div);
}

// Binary modulo: a mod b
static inline unsigned binary_mod(unsigned a, unsigned b)
{
    // Fast path: both are small integers
    if (IS_NUM(a) && IS_NUM(b)) {
        int64_t va = CELL_ID(a);
        int64_t vb = CELL_ID(b);
        if (vb == 0) {
            show_error("modulo: division by zero");
            return TOK_ERROR;
        }
        if (va == INT64_MIN && vb == -1)
            return store(0);
        // Scheme modulo: result has same sign as divisor
        int64_t r = va % vb;
        if ((r > 0 && vb < 0) || (r < 0 && vb > 0))
            r += vb;
        return store(r);
    }
    // Fall back to full modulo with bignum support
    return apply_binary_primitive(a, b, prim_modulo);
}

// Binary less-than comparison: a < b
// Returns ctx.atom_true or ctx.atom_false
static inline unsigned binary_lt(unsigned a, unsigned b)
{
    // Fast path: both are small integers
    if (IS_NUM(a) && IS_NUM(b)) {
        return CELL_ID(a) < CELL_ID(b) ? ctx.atom_true : ctx.atom_false;
    }
    // Fall back to full comparison
    return apply_binary_compare(a, b, CMP_LT);
}

// Binary numeric equality: a = b
static inline unsigned binary_numeq(unsigned a, unsigned b)
{
    // Fast path: both are small integers
    if (IS_NUM(a) && IS_NUM(b)) {
        return CELL_ID(a) == CELL_ID(b) ? ctx.atom_true : ctx.atom_false;
    }
    // Fall back to full comparison
    return apply_binary_compare(a, b, CMP_EQ);
}

// Binary greater-than comparison: a > b
static inline unsigned binary_gt(unsigned a, unsigned b)
{
    // Fast path: both are small integers
    if (IS_NUM(a) && IS_NUM(b)) {
        return CELL_ID(a) > CELL_ID(b) ? ctx.atom_true : ctx.atom_false;
    }
    // Fall back to full comparison
    return apply_binary_compare(a, b, CMP_GT);
}

// Binary less-than-or-equal comparison: a <= b
static inline unsigned binary_le(unsigned a, unsigned b)
{
    // Fast path: both are small integers
    if (IS_NUM(a) && IS_NUM(b)) {
        return CELL_ID(a) <= CELL_ID(b) ? ctx.atom_true : ctx.atom_false;
    }
    // Fall back to full comparison
    return apply_binary_compare(a, b, CMP_LE);
}

// Binary greater-than-or-equal comparison: a >= b
static inline unsigned binary_ge(unsigned a, unsigned b)
{
    // Fast path: both are small integers
    if (IS_NUM(a) && IS_NUM(b)) {
        return CELL_ID(a) >= CELL_ID(b) ? ctx.atom_true : ctx.atom_false;
    }
    // Fall back to full comparison
    return apply_binary_compare(a, b, CMP_GE);
}

// ============================================================================
// Comparison Helpers
// ============================================================================

// Compare two exact integers. Returns false if either operand cannot be
// converted to a valid bignum.
static inline bool compare_exact_integers(unsigned a, unsigned b, int *cmp_out)
{
    if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
        int32_t va = FIXNUM_VALUE(a);
        int32_t vb = FIXNUM_VALUE(b);
        *cmp_out = (va < vb) ? -1 : (va > vb) ? 1 : 0;
        return true;
    }
    if (IS_FIXNUM(a) && IS_NUM(b)) {
        int64_t va = FIXNUM_VALUE(a);
        int64_t vb = CELL_ID(b);
        *cmp_out = (va < vb) ? -1 : (va > vb) ? 1 : 0;
        return true;
    }
    if (IS_NUM(a) && IS_FIXNUM(b)) {
        int64_t va = CELL_ID(a);
        int64_t vb = FIXNUM_VALUE(b);
        *cmp_out = (va < vb) ? -1 : (va > vb) ? 1 : 0;
        return true;
    }
    if (!IS_CELL(a) || !IS_CELL(b))
        return false;

    enum lisp_type ta = CELL_TYPE(a);
    enum lisp_type tb = CELL_TYPE(b);

    if (ta == BT_NUM && tb == BT_NUM) {
        int64_t va = CELL_ID(a);
        int64_t vb = CELL_ID(b);
        *cmp_out = (va < vb) ? -1 : (va > vb) ? 1 : 0;
        return true;
    }

    // At least one is bignum
    bignum *ba = to_bignum(a);
    bignum *bb = to_bignum(b);
    if (!ba || !bb) {
        bn_free(ba);
        bn_free(bb);
        return false;
    }
    *cmp_out = bn_cmp(ba, bb);
    bn_free(ba);
    bn_free(bb);
    return true;
}

// ============================================================================
// String Port Operations
// ============================================================================

static inline string_port *strport_alloc_with_data(char *data, size_t len,
                                                   size_t cap)
{
    string_port *sp = checked_malloc_size(sizeof(string_port));
    if (!sp) {
        free(data);
        return NULL;
    }
    sp->data = data;
    sp->len = len;
    sp->cap = cap;
    sp->pos = 0;
    string_port_register(sp);
    return sp;
}

// Create a new output string port
static inline string_port *strport_new(void)
{
    char *data = checked_malloc_size(INITIAL_STRING_CAP);
    if (!data)
        return NULL;
    data[0] = '\0';
    return strport_alloc_with_data(data, 0, INITIAL_STRING_CAP);
}

// Create a string input port from a string (copies the string)
static inline string_port *strport_from_string(const char *s)
{
    size_t len = strlen(s);
    char *data = checked_string_copy_len(s, len);
    if (!data)
        return NULL;
    return strport_alloc_with_data(data, len, len + 1);
}

static inline bool strport_grow(string_port *sp)
{
    size_t new_cap;
    if (!checked_grow_capacity_size(sp->cap, 1, &new_cap))
        return false;
    char *new_data = checked_realloc_size(sp->data, new_cap);
    if (!new_data)
        return false;
    sp->data = new_data;
    sp->cap = new_cap;
    return true;
}

static inline bool strport_ensure_capacity(string_port *sp, size_t min_cap)
{
    while (sp->cap < min_cap) {
        if (!strport_grow(sp))
            return false;
    }
    return true;
}

// Write a character to string port (fast amortized O(1))
static inline bool strport_putc(string_port *sp, int c)
{
    if (sp->len > SIZE_MAX - 2)
        return false;
    if (!strport_ensure_capacity(sp, sp->len + 2))
        return false;
    sp->data[sp->len++] = (char)c;
    sp->data[sp->len] = '\0';
    return true;
}

// Write a string to string port
static inline bool strport_puts(string_port *sp, const char *s)
{
    size_t slen = strlen(s);
    if (slen > SIZE_MAX - sp->len - 1)
        return false;
    if (!strport_ensure_capacity(sp, sp->len + slen + 1))
        return false;
    memcpy(sp->data + sp->len, s, slen + 1);
    sp->len += slen;
    return true;
}

// Read a character from string input port
static inline int strport_getc(string_port *sp)
{
    if (sp->pos >= sp->len)
        return EOF;
    return (unsigned char)sp->data[sp->pos++];
}

// Peek at next character from string input port
static inline int strport_peekc(string_port *sp)
{
    if (sp->pos >= sp->len)
        return EOF;
    return (unsigned char)sp->data[sp->pos];
}

// Free a string port
static inline void strport_free(string_port *sp)
{
    if (sp) {
        string_port_unregister(sp);
        free(sp->data);
        free(sp);
    }
}

// ============================================================================
// Port Extraction Helpers
// ============================================================================

// Port direction for extract_port
typedef enum { PORT_INPUT, PORT_OUTPUT } port_dir;

static inline bool string_port_well_formed(const string_port *sp)
{
    return string_port_is_registered(sp) && sp->data && sp->cap > 0 &&
           sp->len < sp->cap && sp->pos <= sp->len;
}

static inline int extract_current_port_cell(unsigned current_cell, port_dir dir,
                                            FILE **file_out,
                                            string_port **strport_out,
                                            const char *fn_name)
{
    bool is_strport =
        (dir == PORT_INPUT) ? IS_STRINPORT(current_cell)
                            : IS_STROUTPORT(current_cell);
    if (is_strport) {
        *strport_out = GET_STRPORT_PTR(current_cell);
        if (!string_port_well_formed(*strport_out)) {
            show_error("%s: current port is closed", fn_name);
            return -1;
        }
        return 1;
    }

    bool is_fileport =
        (dir == PORT_INPUT) ? IS_INPORT(current_cell) : IS_OUTPORT(current_cell);
    if (is_fileport) {
        *file_out = file_port_file(current_cell);
        if (!*file_out) {
            show_error("%s: current port is closed", fn_name);
            return -1;
        }
        return 0;
    }

    show_error("%s: current port has invalid type", fn_name);
    return -1;
}

static inline int extract_explicit_port_cell(unsigned p, port_dir dir,
                                             FILE **file_out,
                                             string_port **strport_out,
                                             const char *fn_name)
{
    bool is_strport = (dir == PORT_INPUT) ? IS_STRINPORT(p) : IS_STROUTPORT(p);
    if (is_strport) {
        *strport_out = GET_STRPORT_PTR(p);
        if (!string_port_well_formed(*strport_out)) {
            show_error("%s: port is closed", fn_name);
            return -1;
        }
        return 1;
    }

    bool is_fileport = (dir == PORT_INPUT) ? IS_INPORT(p) : IS_OUTPORT(p);
    if (!is_fileport) {
        show_error("%s: argument must be %s port", fn_name,
                   dir == PORT_INPUT ? "input" : "output");
        return -1;
    }

    *file_out = file_port_file(p);
    if (!*file_out) {
        show_error("%s: port is closed", fn_name);
        return -1;
    }
    return 0;
}

// Unified port extraction - handles both file and string ports
// Returns: 0 = file port, 1 = string port, -1 = error
static inline int extract_port(unsigned args, port_dir dir, bool use_second_arg,
                               FILE **file_out, string_port **strport_out,
                               const char *fn_name)
{
    *file_out = (dir == PORT_INPUT) ? ctx.current_input : ctx.current_output;
    *strport_out = NULL;

    // Determine which arg contains the port
    unsigned port_arg = use_second_arg ? cdr(args) : args;
    if (!port_arg) {
        unsigned current_cell = (dir == PORT_INPUT) ? ctx.current_input_cell
                                                    : ctx.current_output_cell;
        if (current_cell != 0) {
            return extract_current_port_cell(current_cell, dir, file_out,
                                             strport_out, fn_name);
        }
        return 0; // Use default file port
    }

    unsigned p = car(port_arg);
    return extract_explicit_port_cell(p, dir, file_out, strport_out, fn_name);
}

// Unified port extraction for argv-based primitives
// port_index < 0 means use current port
static inline int extract_port_argv(unsigned *argv, int port_index,
                                    port_dir dir, FILE **file_out,
                                    string_port **strport_out,
                                    const char *fn_name)
{
    *file_out = (dir == PORT_INPUT) ? ctx.current_input : ctx.current_output;
    *strport_out = NULL;

    if (port_index < 0) {
        unsigned current_cell = (dir == PORT_INPUT) ? ctx.current_input_cell
                                                    : ctx.current_output_cell;
        if (current_cell != 0) {
            return extract_current_port_cell(current_cell, dir, file_out,
                                             strport_out, fn_name);
        }
        return 0;
    }

    unsigned p = argv[port_index];
    return extract_explicit_port_cell(p, dir, file_out, strport_out, fn_name);
}

// Convenience wrappers
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
static inline bool extract_input_port(unsigned args, FILE **port_out,
                                      const char *fn_name)
{
    *port_out = ctx.current_input;
    if (args) {
        unsigned p = car(args);
        if (!IS_INPORT(p)) {
            show_error("%s: argument must be input port", fn_name);
            return false;
        }
        *port_out = file_port_file(p);
        if (!*port_out) {
            show_error("%s: port is closed", fn_name);
            return false;
        }
    }
    return true;
}

// ============================================================================
// String Helpers
// ============================================================================

// Create a string cell from an already-allocated string (takes ownership)
static inline unsigned make_string_owned(char *s)
{
    if (!s) {
        show_error("string: out of memory");
        return TOK_ERROR;
    }
    unsigned p = alloc();
    CELL_TYPE(p) = BT_STRING;
    CELL_PTR(p) = s;
    string_register(s);
    return p;
}

// Create a string cell by copying a C string (handles allocation + error)
static inline unsigned make_string_copy(const char *s)
{
    char *copy = checked_string_copy(s);
    if (!copy) {
        show_error("out of memory");
        return TOK_ERROR;
    }
    return make_string_owned(copy);
}

// ============================================================================
// Primitive Module Declarations
// ============================================================================

// Numeric operations (prim_numeric.c)
unsigned prim_remainder(unsigned argc, unsigned *argv);
unsigned prim_quotient(unsigned argc, unsigned *argv);
unsigned prim_truncate_divrem(unsigned argc, unsigned *argv);
unsigned prim_floor_divrem(unsigned argc, unsigned *argv);
unsigned prim_abs(unsigned argc, unsigned *argv);

// Comparison operations (prim_compare.c)
unsigned char_compare(unsigned argc, unsigned *argv, cmp_op op,
                      bool case_insensitive);
unsigned string_compare(unsigned argc, unsigned *argv, cmp_op op,
                        bool case_insensitive);

// List operations (prim_list.c)
unsigned prim_append(unsigned argc, unsigned *argv);
unsigned prim_reverse(unsigned argc, unsigned *argv);

// String operations (prim_string.c)
unsigned prim_string_append(unsigned argc, unsigned *argv);
unsigned prim_substring(unsigned argc, unsigned *argv);
unsigned prim_string_normalize(unsigned prim_id, unsigned argc,
                               unsigned *argv);

// Type predicates (prim_type.c)
unsigned apply_type_predicate(unsigned prim_id, unsigned argc,
                              unsigned *argv);

// Character operations (prim_char.c)
unsigned apply_char_primitive(unsigned prim_id, unsigned argc,
                              unsigned *argv);
uint32_t unicode_simple_foldcase(uint32_t code);
uint32_t unicode_upcase(uint32_t code);
uint32_t unicode_downcase(uint32_t code);
bool unicode_is_cased(uint32_t code);
bool unicode_is_case_ignorable(uint32_t code);
bool unicode_full_foldcase(uint32_t code, const uint32_t **mapping,
                           size_t *length);
bool unicode_full_upcase(uint32_t code, const uint32_t **mapping,
                         size_t *length);
bool unicode_full_downcase(uint32_t code, const uint32_t **mapping,
                           size_t *length);
bool unicode_full_titlecase(uint32_t code, const uint32_t **mapping,
                            size_t *length);

// Vector operations (prim_vector.c)
unsigned apply_vector_primitive(unsigned prim_id, unsigned argc,
                                unsigned *argv);

// Math operations (prim_math.c)
unsigned apply_math_primitive(unsigned prim_id, unsigned argc,
                              unsigned *argv);

// I/O operations (prim_io.c)
unsigned apply_io_primitive(unsigned prim_id, unsigned argc,
                            unsigned *argv);

// Port operations (prim_port.c)
unsigned apply_port_primitive(unsigned prim_id, unsigned argc,
                              unsigned *argv);

// Numeric tower operations (prim_numtower.c)
unsigned apply_numtower_primitive(unsigned prim_id, unsigned argc,
                                  unsigned *argv);

#endif // PRIM_INTERNAL_H
