/**
 * @file context.h
 * @brief Memory management, allocation, and garbage collection interface
 *
 * This module provides the core memory infrastructure for the interpreter:
 *
 * ## Memory Layout
 * The heap consists of two semispaces of SEMISPACE_SIZE cells each:
 * - Active semispace: Where new allocations occur (bump pointer at ctx.hptr)
 * - Inactive semispace: Target for copying during GC
 *
 * Cells 0 through HEAP_RESERVED-1 are permanent and never collected.
 * These hold critical atoms like #t, quote, quasiquote, etc.
 *
 * ## Garbage Collection
 * Uses Cheney's copying collector algorithm:
 * 1. Copy root objects to inactive semispace
 * 2. Scan copied objects, copying their children
 * 3. Swap semispaces when done
 *
 * GC is triggered automatically when heap usage exceeds 90%, or manually
 * via gc-flip primitive or :g escape command.
 *
 * ## Numeric Storage
 * Numbers use the numeric tower with automatic promotion:
 * - Small integers (int64_t range): BT_NUM with value in id field
 * - Large integers: BT_BIGNUM with pointer to bignum struct
 * - Rationals: BT_RATIONAL with car/cdr pointing to num/denom cells
 * - Floats: BT_INEXACT with id reinterpreted as double
 * - Complex: BT_COMPLEX with car/cdr pointing to real/imag parts
 */

#ifndef CONTEXT_H
#define CONTEXT_H

#include "bignum.h"
#include "types.h"

// ============================================================================
// Cell Accessors
// ============================================================================

static inline unsigned car(unsigned id) { return ctx.cons_cells[id].car; }
static inline unsigned cdr(unsigned id) { return ctx.cons_cells[id].cdr; }
static inline unsigned caar(unsigned id) { return car(car(id)); }
static inline unsigned cdar(unsigned id) { return cdr(car(id)); }
static inline unsigned cadr(unsigned id) { return car(cdr(id)); }
static inline unsigned cddr(unsigned id) { return cdr(cdr(id)); }
static inline unsigned caddr(unsigned id) { return car(cdr(cdr(id))); }
static inline unsigned cdddr(unsigned id) { return cdr(cdr(cdr(id))); }
static inline unsigned cadddr(unsigned id) { return car(cdr(cdr(cdr(id)))); }

// ============================================================================
// Memory Allocation
// ============================================================================

// Allocate a new cell
unsigned alloc(void);

// Allocate a cons cell with given car and cdr
unsigned alloc_cons(unsigned car_val, unsigned cdr_val);

// Forward declarations for GC protection (defined below)
void gc_protect(unsigned *ptr);
void gc_unprotect(int count);

// Allocate a typed cell with given car and cdr
static inline unsigned make_typed_cell(enum lisp_type type, unsigned car_val,
                                       unsigned cdr_val)
{
    gc_protect(&car_val);
    gc_protect(&cdr_val);
    unsigned p = alloc();
    gc_unprotect(2);
    ctx.cons_cells[p].type = type;
    ctx.cons_cells[p].car = car_val;
    ctx.cons_cells[p].cdr = cdr_val;
    return p;
}

// Store a numeric value (exact integer)
unsigned store(int64_t val);

// Store an inexact (floating-point) value
unsigned store_inexact(double val);

// Store a rational value (numerator/denominator)
unsigned store_rational(int64_t num, int64_t denom);

// Store a complex value (real/imaginary parts)
unsigned store_complex(unsigned real_part, unsigned imag_part);

// Get numeric value as double (works for all numeric types)
double to_double(unsigned x);

// Check if a value is numeric
bool is_numeric(unsigned x);

// Check if a value is exact
bool is_exact(unsigned x);

// Normalize a rational (reduce to lowest terms, handle signs)
unsigned normalize_rational(int64_t num, int64_t denom);

// Normalize a rational from cell references (supports bignum
// numerator/denominator)
unsigned normalize_rational_cells(unsigned num_cell, unsigned denom_cell);

// Check if a numeric cell is negative
bool is_negative_number(unsigned x);

// Negate a numeric cell (returns new cell)
unsigned negate_number(unsigned x);

// Store a bignum (takes ownership of the bignum pointer)
unsigned store_bignum(bignum *bn);

// Get bignum pointer from a BT_BIGNUM cell
bignum *get_bignum(unsigned x);

// Convert any exact integer (BT_NUM or BT_BIGNUM) to bignum
bignum *to_bignum(unsigned x);

// Store an integer, using BT_NUM if it fits, BT_BIGNUM otherwise
unsigned store_integer(bignum *bn);

// Free bignums during GC
void free_bignum_cell(unsigned x);

// ============================================================================
// Type Constructors
// ============================================================================

// Create a character cell
unsigned make_char(int c);

// Create a vector with given length and fill value
unsigned make_vector(unsigned len, unsigned fill);

// Get vector length
unsigned vector_len(unsigned vec);

// Get pointer to vector data
unsigned *vector_data_ptr(unsigned vec);

// ============================================================================
// Continuation Helpers
// ============================================================================

// Create a continuation frame
unsigned make_cont(enum cont_type type, unsigned data, unsigned env,
                   unsigned next);

// Continuation accessors
static inline enum cont_type cont_type(unsigned k)
{
    return (enum cont_type)caar(k);
}
static inline unsigned cont_data(unsigned k) { return cdar(k); }
static inline unsigned cont_env(unsigned k) { return cadr(k); }
static inline unsigned cont_next(unsigned k) { return cddr(k); }

// Create HALT continuation
unsigned make_halt_cont(void);

// ============================================================================
// Trampoline Helpers
// ============================================================================

static inline void tramp_eval(unsigned expr, unsigned env, unsigned cont)
{
    tramp.mode = TRAMP_EVAL;
    tramp.expr = expr;
    tramp.env = env;
    tramp.cont = cont;
}

static inline void tramp_apply(unsigned value, unsigned cont)
{
    tramp.mode = TRAMP_APPLY;
    tramp.value = value;
    tramp.cont = cont;
}

static inline void tramp_done(unsigned value)
{
    tramp.mode = TRAMP_DONE;
    tramp.value = value;
}

static inline void tramp_error(void) { tramp.mode = TRAMP_ERROR; }

// ============================================================================
// Atom/String Interning
// ============================================================================

// Hash function for strings
int hash_function(const char *s);

// Intern a string into the atom table
int intern(const char *s);

// Convert string to atom or number
unsigned atom_from_string(const char *s);

// ============================================================================
// List Utilities
// ============================================================================

// Count list length
unsigned list_length(unsigned lst);

// Find last cons cell in a proper list (returns 0 for empty list)
static inline unsigned list_last(unsigned lst)
{
    if (!lst)
        return 0;
    while (cdr(lst))
        lst = cdr(lst);
    return lst;
}

// Check argument count
bool check_args(unsigned args, unsigned min, unsigned max, const char *name);

// Append element to list being built (modifies head/tail pointers)
void list_append(unsigned *head, unsigned *tail, unsigned elem);

// Deep equality comparison
bool deep_equal(unsigned a, unsigned b);

// ============================================================================
// Garbage Collection
// ============================================================================

// Collect a single cell
unsigned collect(unsigned x);

// Run garbage collection with given root
unsigned gc(unsigned root);

// Get current heap usage as percentage (0-100)
int heap_usage_percent(void);

// Run GC only if heap usage exceeds threshold (default 75%)
unsigned maybe_gc(unsigned root, int threshold_percent);

// Set GC root for automatic collection during alloc
void set_alloc_gc_root(unsigned *root);

// Trigger GC using the registered alloc root (for escape commands)
void trigger_gc(void);

// Shadow stack for protecting local C variables during nested allocations.
// Use gc_protect before a sequence of allocations that depend on local vars,
// then gc_unprotect when done. This ensures GC updates the variables in place.
// (gc_protect and gc_unprotect declared above for inline functions)

// Alternative: mark/release for safer scope-based protection.
// gc_mark() returns current stack position, gc_release() restores it.
// This avoids manual counting errors.
int gc_mark(void);
void gc_release(int mark);

// Helper macro for scope-based GC protection:
//   GC_PROTECTED { gc_protect(&a); gc_protect(&b); ... allocations ... }
// Automatically releases all protected vars when scope exits.
#define GC_PROTECTED                                                           \
    for (int _gc_mark = gc_mark(), _gc_once = 1; _gc_once;                     \
         gc_release(_gc_mark), _gc_once = 0)

// ============================================================================
// Initialization
// ============================================================================

// Initialize the heap (must be called before any other context functions)
void init_heap(void);

// Initialize cached keyword IDs
void init_keywords(void);

#endif // CONTEXT_H
