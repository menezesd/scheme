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
#include <math.h>
#include <stdlib.h>
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
static inline numeric_level classify_args(unsigned args, bool *all_exact_out)
{
    numeric_level level = NUM_INTEGER;
    bool all_ex = true;

    FORLIST(a, args)
    {
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
    if (CELL_TYPE(x) == BT_COMPLEX) {
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
        *imag = store(0);
        return;
    }
    if (CELL_TYPE(x) == BT_COMPLEX) {
        *real = CELL_CAR(x);
        *imag = CELL_CDR(x);
    } else {
        *real = x;
        *imag = store(0);
    }
}

// Check if a complex number (or any number) is exact
static inline bool is_complex_exact(unsigned x)
{
    if (CELL_TYPE(x) == BT_COMPLEX) {
        return is_exact(CELL_CAR(x)) && is_exact(CELL_CDR(x));
    }
    return is_exact(x);
}

// Create complex result, simplifying to real if imaginary part is zero
// Preserves exactness of components
static inline unsigned make_complex_exact(unsigned real, unsigned imag)
{
    // Check if imaginary part is zero
    double imag_d = to_double(imag);
    if (imag_d == 0.0)
        return real;
    return store_complex(real, imag);
}

// Create result with appropriate exactness
static inline unsigned make_real_result(double val, bool exact)
{
    if (exact && floor(val) == val && val >= INT64_MIN && val <= INT64_MAX) {
        return store((int64_t)val);
    }
    return store_inexact(val);
}

// Create complex result, simplifying to real if imaginary part is zero
static inline unsigned make_complex_result(double real, double imag, bool exact)
{
    if (imag == 0.0)
        return make_real_result(real, exact);
    unsigned real_part = make_real_result(real, exact);
    gc_protect(&real_part);
    unsigned imag_part = make_real_result(imag, exact);
    gc_unprotect(1);
    return store_complex(real_part, imag_part);
}

// Create inexact complex result (convenience for division etc.)
static inline unsigned make_complex_inexact(double real, double imag)
{
    if (imag == 0.0)
        return store_inexact(real);
    unsigned real_part = store_inexact(real);
    gc_protect(&real_part);
    unsigned imag_part = store_inexact(imag);
    gc_unprotect(1);
    return store_complex(real_part, imag_part);
}

// Helper to get rational components (num, denom) from any exact number
// NOTE: Only works correctly if num/denom fit in int64_t
static inline void get_rational_parts(unsigned x, int64_t *num, int64_t *denom)
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

// Helper to get rational components as cell references (supports bignums)
static inline void get_rational_cells(unsigned x, unsigned *num,
                                      unsigned *denom)
{
    enum lisp_type t = CELL_TYPE(x);
    if (t == BT_NUM || t == BT_BIGNUM) {
        *num = x;
        *denom = store(1);
    } else if (t == BT_RATIONAL) {
        *num = CELL_CAR(x);
        *denom = CELL_CDR(x);
    } else {
        *num = store(0);
        *denom = store(1);
    }
}

// Multiply two integer cells (BT_NUM or BT_BIGNUM), returns new cell.
// NOTE: Allocates memory. Callers should use gc_protect on inputs if they
// need to survive potential GC during allocation.
static inline unsigned multiply_cells(unsigned a, unsigned b)
{
    bignum *ba = to_bignum(a);
    bignum *bb = to_bignum(b);
    bignum *result = bn_mul(ba, bb);
    bn_free(ba);
    bn_free(bb);
    return store_integer(result);
}

// Add two integer cells (BT_NUM or BT_BIGNUM), returns new cell.
// NOTE: Allocates memory. Callers should use gc_protect on inputs if they
// need to survive potential GC during allocation.
static inline unsigned add_cells(unsigned a, unsigned b)
{
    bignum *ba = to_bignum(a);
    bignum *bb = to_bignum(b);
    bn_add_ip(ba, bb);
    bn_free(bb);
    return store_integer(ba);
}

// Subtract two integer cells (BT_NUM or BT_BIGNUM), returns new cell.
// NOTE: Allocates memory. Callers should use gc_protect on inputs if they
// need to survive potential GC during allocation.
static inline unsigned subtract_cells(unsigned a, unsigned b)
{
    bignum *ba = to_bignum(a);
    bignum *bb = to_bignum(b);
    bn_sub_ip(ba, bb);
    bn_free(bb);
    return store_integer(ba);
}

// Check if an integer cell (BT_NUM or BT_BIGNUM) is zero
static inline bool is_zero_cell(unsigned x)
{
    if (CELL_TYPE(x) == BT_NUM)
        return CELL_ID(x) == 0;
    if (CELL_TYPE(x) == BT_BIGNUM)
        return bn_is_zero(get_bignum(x));
    return false;
}

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
unsigned prim_plus(unsigned args);
unsigned prim_minus(unsigned args);
unsigned prim_mult(unsigned args);
unsigned prim_div(unsigned args);
unsigned numeric_compare(unsigned args, cmp_op op);

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
        bignum *ba = bn_from_int(va);
        bignum *bb = bn_from_int(vb);
        bn_add_ip(ba, bb);
        bn_free(bb);
        return store_integer(ba);
    }
    // Fall back to full numeric tower
    gc_protect(&a);
    gc_protect(&b);
    unsigned args = alloc_cons(a, alloc_cons(b, 0));
    gc_unprotect(2);
    return prim_plus(args);
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
        bignum *ba = bn_from_int(va);
        bignum *bb = bn_from_int(vb);
        bn_sub_ip(ba, bb);
        bn_free(bb);
        return store_integer(ba);
    }
    // Fall back to full numeric tower
    gc_protect(&a);
    gc_protect(&b);
    unsigned args = alloc_cons(a, alloc_cons(b, 0));
    gc_unprotect(2);
    return prim_minus(args);
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
        bignum *ba = bn_from_int(va);
        bignum *bb = bn_from_int(vb);
        bignum *br = bn_mul(ba, bb);
        bn_free(ba);
        bn_free(bb);
        return store_integer(br);
    }
    // Fall back to full numeric tower
    gc_protect(&a);
    gc_protect(&b);
    unsigned args = alloc_cons(a, alloc_cons(b, 0));
    gc_unprotect(2);
    return prim_mult(args);
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
        if (va % vb == 0) {
            // Exact division
            return store(va / vb);
        }
        // Result is rational
        return normalize_rational(va, vb);
    }
    // Fall back to full numeric tower
    gc_protect(&a);
    gc_protect(&b);
    unsigned args = alloc_cons(a, alloc_cons(b, 0));
    gc_unprotect(2);
    return prim_div(args);
}

// Binary less-than comparison: a < b
// Returns ctx.atom_true or 0
static inline unsigned binary_lt(unsigned a, unsigned b)
{
    // Fast path: both are small integers
    if (IS_NUM(a) && IS_NUM(b)) {
        return CELL_ID(a) < CELL_ID(b) ? ctx.atom_true : 0;
    }
    // Fall back to full comparison
    gc_protect(&a);
    gc_protect(&b);
    unsigned args = alloc_cons(a, alloc_cons(b, 0));
    gc_unprotect(2);
    return numeric_compare(args, CMP_LT);
}

// Binary numeric equality: a = b
static inline unsigned binary_numeq(unsigned a, unsigned b)
{
    // Fast path: both are small integers
    if (IS_NUM(a) && IS_NUM(b)) {
        return CELL_ID(a) == CELL_ID(b) ? ctx.atom_true : 0;
    }
    // Fall back to full comparison
    gc_protect(&a);
    gc_protect(&b);
    unsigned args = alloc_cons(a, alloc_cons(b, 0));
    gc_unprotect(2);
    return numeric_compare(args, CMP_EQ);
}

// ============================================================================
// Comparison Helpers
// ============================================================================

// Compare two exact integers, returns -1, 0, or 1
static inline int compare_exact_integers(unsigned a, unsigned b)
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

// ============================================================================
// String Port Operations
// ============================================================================

// Create a new output string port
static inline string_port *strport_new(void)
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
static inline string_port *strport_from_string(const char *s)
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
static inline void strport_putc(string_port *sp, int c)
{
    if (sp->len + 1 >= sp->cap) {
        size_t new_cap = sp->cap * 2;
        if (new_cap <= sp->cap) {
            fprintf(stderr, "string port: capacity overflow\n");
            abort();
        }
        char *new_data = realloc(sp->data, new_cap);
        if (!new_data) {
            fprintf(stderr, "string port: out of memory\n");
            abort();
        }
        sp->data = new_data;
        sp->cap = new_cap;
    }
    sp->data[sp->len++] = (char)c;
    sp->data[sp->len] = '\0';
}

// Write a string to string port
static inline void strport_puts(string_port *sp, const char *s)
{
    size_t slen = strlen(s);
    while (sp->len + slen >= sp->cap) {
        size_t new_cap = sp->cap * 2;
        if (new_cap <= sp->cap) {
            fprintf(stderr, "string port: capacity overflow\n");
            abort();
        }
        char *new_data = realloc(sp->data, new_cap);
        if (!new_data) {
            fprintf(stderr, "string port: out of memory\n");
            abort();
        }
        sp->data = new_data;
        sp->cap = new_cap;
    }
    memcpy(sp->data + sp->len, s, slen + 1);
    sp->len += slen;
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
static inline int extract_port(unsigned args, port_dir dir, bool use_second_arg,
                               FILE **file_out, string_port **strport_out,
                               const char *fn_name)
{
    *file_out = (dir == PORT_INPUT) ? ctx.current_input : ctx.current_output;
    *strport_out = NULL;

    // Determine which arg contains the port
    unsigned port_arg = use_second_arg ? cdr(args) : args;
    if (!port_arg) {
        // Check if current port is a string port
        unsigned current_cell = (dir == PORT_INPUT) ? ctx.current_input_cell
                                                    : ctx.current_output_cell;
        if (current_cell != 0) {
            *strport_out = GET_STRPORT_PTR(current_cell);
            if (!*strport_out) {
                show_error("%s: current port is closed", fn_name);
                return -1;
            }
            return 1; // String port
        }
        return 0; // Use default file port
    }

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
        *port_out = GET_PORT_PTR(p);
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
    unsigned p = alloc();
    CELL_TYPE(p) = BT_STRING;
    CELL_ID(p) = STORE_PTR(s);
    return p;
}

// Create a string cell by copying a C string (handles allocation + error)
static inline unsigned make_string_copy(const char *s)
{
    char *copy = strdup(s);
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
unsigned prim_plus(unsigned args);
unsigned prim_minus(unsigned args);
unsigned prim_mult(unsigned args);
unsigned prim_div(unsigned args);
unsigned prim_modulo(unsigned args);
unsigned prim_remainder(unsigned args);
unsigned prim_quotient(unsigned args);
unsigned prim_abs(unsigned args);

// Comparison operations (prim_compare.c)
unsigned numeric_compare(unsigned args, cmp_op op);
unsigned char_compare(unsigned args, cmp_op op, bool case_insensitive);
unsigned string_compare(unsigned args, cmp_op op, bool case_insensitive);

// List operations (prim_list.c)
unsigned prim_append(unsigned args);
unsigned prim_reverse(unsigned args);

// String operations (prim_string.c)
unsigned prim_string_append(unsigned args);
unsigned prim_substring(unsigned args);

// Type predicates (prim_type.c)
unsigned apply_type_predicate(unsigned prim_id, unsigned args);

// Character operations (prim_char.c)
unsigned apply_char_primitive(unsigned prim_id, unsigned args);

// Vector operations (prim_vector.c)
unsigned apply_vector_primitive(unsigned prim_id, unsigned args);

// Math operations (prim_math.c)
unsigned apply_math_primitive(unsigned prim_id, unsigned args);

// I/O operations (prim_io.c)
unsigned apply_io_primitive(unsigned prim_id, unsigned args);

// Port operations (prim_port.c)
unsigned apply_port_primitive(unsigned prim_id, unsigned args);

// Numeric tower operations (prim_numtower.c)
unsigned apply_numtower_primitive(unsigned prim_id, unsigned args);

#endif // PRIM_INTERNAL_H
