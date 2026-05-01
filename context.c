/**
 * @file context.c
 * @brief Memory management, garbage collection, and atom interning
 *
 * This file implements the core memory infrastructure:
 *
 * ## Heap Organization
 * - Two semispaces of SEMISPACE_SIZE cells each (32M cells total)
 * - Cells 0 to HEAP_RESERVED-1 are permanent (atoms like #t, quote, etc.)
 * - ctx.hptr is the bump pointer for allocation in current semispace
 * - ctx.mmin/nmin track the start of each semispace
 *
 * ## Garbage Collection (Cheney's Algorithm)
 * 1. Switch to inactive semispace (swap mmin/nmin, reset hptr)
 * 2. Copy root to new space, leaving forwarding pointer (BT_BROKENHEART)
 * 3. Scan copied cells, recursively copying their car/cdr references
 * 4. When scan pointer catches up to hptr, all live data is copied
 * 5. Free external resources (strings, bignums, vectors) in old space
 *
 * ## Atom Interning
 * Symbols are interned using FNV-1a hashing with open addressing.
 * The atom table maps string -> unique ID, ensuring (eq? 'foo 'foo) is #t.
 */

#define _POSIX_C_SOURCE 200809L
#include "context.h"
#include "bignum.h"
#include "bytecode.h"
#include "compiled_pattern.h"
#include "env.h"
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <string.h>

// ============================================================================
// FNV-1a Hash Constants (64-bit)
// ============================================================================

#define FNV_OFFSET_BASIS 14695981039346656037ull
#define FNV_PRIME 1099511628211ull

// ============================================================================
// Global State
// ============================================================================

lisp_context ctx = {.hptr = HEAP_RESERVED,
                    .mmin = HEAP_RESERVED,
                    .nmin = SEMISPACE_SIZE,
                    .cons_cells = NULL,     // Allocated in init_heap
                    .current_input = NULL,  // Set to stdin in init_keywords
                    .current_output = NULL, // Set to stdout in init_keywords
                    .transcript = NULL};

tramp_state tramp;
unsigned gensym_counter = 0;

// Error recovery for fatal errors
jmp_buf panic_jmp;
bool panic_jmp_set = false;

void init_heap(void)
{
    // Allocate cons cells heap (includes nursery space at end of first
    // semispace)
    size_t heap_size = 2 * SEMISPACE_SIZE * sizeof(cons_cell);
    ctx.cons_cells = malloc(heap_size);
    if (!ctx.cons_cells) {
        fprintf(stderr, "Fatal: failed to allocate %zu bytes for heap\n",
                heap_size);
        exit(EXIT_FAILURE);
    }
    memset(ctx.cons_cells, 0, heap_size);

    // Allocate atom table
    ctx.atom_table_cap = TABLE_SIZE;
    size_t table_size = ctx.atom_table_cap * sizeof(const char *);
    ctx.atom_table = malloc(table_size);
    if (!ctx.atom_table) {
        fprintf(stderr, "Fatal: failed to allocate %zu bytes for atom table\n",
                table_size);
        exit(EXIT_FAILURE);
    }
    memset(ctx.atom_table, 0, table_size);
    ctx.atom_count = 0;

    // Generational GC enabled
    ctx.nursery_start = SEMISPACE_SIZE - NURSERY_SIZE;
    ctx.nursery_ptr = ctx.nursery_start;
    // Card table uses bitfields: 1 bit per card (8x memory savings)
    size_t num_cards = (2 * SEMISPACE_SIZE) / CARD_SIZE;
    size_t card_table_size = (num_cards + 7) / 8; // Round up to whole bytes
    ctx.card_table = malloc(card_table_size);
    if (!ctx.card_table) {
        fprintf(stderr, "Fatal: Cannot allocate card table\n");
        exit(EXIT_FAILURE);
    }
    memset(ctx.card_table, 0, card_table_size);
    ctx.minor_gc_count = 0;
    ctx.major_gc_count = 0;
}

void lisp_panic(const char *msg)
{
    fprintf(stderr, "Fatal error: %s\n", msg);
    if (panic_jmp_set) {
        longjmp(panic_jmp, 1);
    } else {
        // No recovery point set, must exit
        exit(EXIT_FAILURE);
    }
}

// ============================================================================
// Memory Allocation
// ============================================================================

// GC root for automatic collection during long computations
// This holds the current evaluation state
static unsigned *alloc_gc_root = NULL;

// Flag to indicate we're in GC mode (alloc should use old gen, not nursery)
static bool in_gc_mode = false;

// Forward declaration for generational GC
unsigned minor_gc(unsigned root);

void set_alloc_gc_root(unsigned *root) { alloc_gc_root = root; }

// Shadow stack for GC roots - protects local C variables during allocation
// When GC runs, these are collected and updated in place
// Now dynamically expandable
static unsigned **shadow_stack = NULL;
static int shadow_stack_size = 0;
static int shadow_stack_top = 0;

static void init_shadow_stack(void)
{
    if (!shadow_stack) {
        shadow_stack_size = 4096;
        shadow_stack = malloc(shadow_stack_size * sizeof(unsigned *));
        if (!shadow_stack) {
            lisp_panic("failed to allocate shadow stack");
        }
    }
}

void gc_protect(unsigned *ptr)
{
    LISP_ASSERT_MSG(ptr != NULL, "gc_protect: null pointer");
    init_shadow_stack();
    if (shadow_stack_top >= shadow_stack_size) {
        // Expand the shadow stack
        int new_size = shadow_stack_size * 2;
        unsigned **new_stack =
            realloc(shadow_stack, new_size * sizeof(unsigned *));
        if (!new_stack) {
            lisp_panic("failed to expand shadow stack");
        }
        shadow_stack = new_stack;
        shadow_stack_size = new_size;
    }
    LISP_ASSERT_FMT(shadow_stack_top >= 0 && shadow_stack_top < shadow_stack_size,
                    "gc_protect: invalid shadow_stack_top=%d (size=%d)",
                    shadow_stack_top, shadow_stack_size);
    shadow_stack[shadow_stack_top++] = ptr;
}

void gc_unprotect(int count)
{
    LISP_ASSERT_FMT(count >= 0, "gc_unprotect: negative count %d", count);
    LISP_ASSERT_FMT(count <= shadow_stack_top,
                    "gc_unprotect: count %d > stack_top %d",
                    count, shadow_stack_top);
    shadow_stack_top -= count;
    if (shadow_stack_top < 0) {
        lisp_panic("shadow stack underflow");
    }
}

int gc_mark(void) { return shadow_stack_top; }

int get_shadow_stack_top(void) { return shadow_stack_top; }

void gc_release(int mark)
{
    if (mark < 0 || mark > shadow_stack_top) {
        lisp_panic("invalid gc_release mark");
    }
    shadow_stack_top = mark;
}

void trigger_gc(void)
{
    if (alloc_gc_root && *alloc_gc_root) {
        *alloc_gc_root = gc(*alloc_gc_root);
    }
}

unsigned alloc(void)
{
    // Check if generational GC is disabled (no card table)
    if (!ctx.card_table) {
        // Simple semispace allocation without generational GC
        unsigned limit =
            (ctx.hptr < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;

        // During GC, don't trigger nested GC
        if (in_gc_mode) {
            if (ctx.hptr >= limit) {
                lisp_panic("out of memory (heap full during GC)");
            }
            unsigned cell = ctx.hptr++;
            LISP_ASSERT_FMT(cell >= HEAP_RESERVED && cell < limit,
                            "alloc: cell %u outside valid range [%u, %u)",
                            cell, HEAP_RESERVED, limit);
            return cell;
        }

        // Check if we're at 90% capacity - trigger major GC
        if (ctx.hptr >= limit * 9 / 10 && alloc_gc_root && *alloc_gc_root) {
            *alloc_gc_root = gc(*alloc_gc_root);
            // Recalculate limit after GC (we may have switched semispaces)
            limit = (ctx.hptr < SEMISPACE_SIZE) ? SEMISPACE_SIZE
                                                : 2 * SEMISPACE_SIZE;
        }

        if (ctx.hptr >= limit) {
            lisp_panic("out of memory (heap exhausted)");
        }
        unsigned cell = ctx.hptr++;
        LISP_ASSERT_FMT(cell >= HEAP_RESERVED && cell < limit,
                        "alloc: cell %u outside valid range [%u, %u)",
                        cell, HEAP_RESERVED, limit);
        return cell;
    }

    // Generational GC enabled - during GC, allocate directly to old generation
    if (in_gc_mode) {
        if (ctx.hptr >= ctx.nursery_start) {
            lisp_panic("out of memory (old gen full during GC)");
        }
        unsigned cell = ctx.hptr++;
        LISP_ASSERT_FMT(cell >= HEAP_RESERVED && cell < ctx.nursery_start,
                        "alloc (gc): cell %u outside old gen [%u, %u)",
                        cell, HEAP_RESERVED, ctx.nursery_start);
        return cell;
    }

    // Generational GC: allocate from nursery
    unsigned nursery_end =
        (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;

    // Check if nursery is full
    if (ctx.nursery_ptr >= nursery_end) {
        // Nursery full - run minor GC to promote survivors
        if (alloc_gc_root && *alloc_gc_root) {
            *alloc_gc_root = minor_gc(*alloc_gc_root);
        } else {
            minor_gc(0);
        }

        // Check if old gen is getting full (> 80%) - trigger major GC
        unsigned old_gen_used = ctx.hptr - ctx.mmin;
        unsigned old_gen_size = ctx.nursery_start - ctx.mmin;
        if (old_gen_used > old_gen_size * 8 / 10 && alloc_gc_root &&
            *alloc_gc_root) {
            *alloc_gc_root = gc(*alloc_gc_root);
        }

        // After GC, nursery should be reset
        nursery_end =
            (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
        if (ctx.nursery_ptr >= nursery_end) {
            lisp_panic("out of memory (nursery still full after GC)");
        }
    }

    unsigned cell = ctx.nursery_ptr++;
    LISP_ASSERT_FMT(cell >= ctx.nursery_start && cell < nursery_end,
                    "alloc (nursery): cell %u outside nursery [%u, %u)",
                    cell, ctx.nursery_start, nursery_end);
    return cell;
}

unsigned alloc_cons(unsigned car_val, unsigned cdr_val)
{
    GC_PROTECT_GUARD2(&car_val, &cdr_val);
    unsigned p = alloc();
    CELL_TYPE(p) = BT_CONS;
    CELL_CAR(p) = car_val;
    CELL_CDR(p) = cdr_val;
    return p;
}

unsigned store(int64_t val)
{
    // Return cached cell for small integers
    if (val >= INT_CACHE_MIN && val <= INT_CACHE_MAX) {
        return INT_CACHE_START + (unsigned)(val - INT_CACHE_MIN);
    }
    unsigned p = alloc();
    CELL_TYPE(p) = BT_NUM;
    CELL_ID(p) = val;
    return p;
}

unsigned store_inexact(double val)
{
    unsigned p = alloc();
    CELL_TYPE(p) = BT_INEXACT;
    // Store double bits in id field
    union {
        double d;
        int64_t i;
    } u;
    u.d = val;
    CELL_ID(p) = u.i;
    return p;
}

unsigned store_bignum(bignum *bn)
{
    unsigned p = alloc();
    CELL_TYPE(p) = BT_BIGNUM;
    CELL_PTR(p) = bn;
    return p;
}

bignum *get_bignum(unsigned x)
{
    if (CELL_TYPE(x) != BT_BIGNUM)
        return NULL;
    return (bignum *)CELL_PTR(x);
}

bignum *to_bignum(unsigned x)
{
    if (CELL_TYPE(x) == BT_NUM) {
        return bn_from_int(CELL_ID(x));
    } else if (CELL_TYPE(x) == BT_BIGNUM) {
        bignum *bn = get_bignum(x);
        if (!bn) {
            // This shouldn't happen - indicates GC corruption
            fprintf(stderr, "Warning: NULL bignum pointer in cell %u\n", x);
            return bn_from_int(0);
        }
        return bn_copy(bn);
    }
    return NULL;
}

unsigned store_integer(bignum *bn)
{
    // If it fits in int64, store as BT_NUM
    int64_t val;
    if (bn_to_int64(bn, &val) == 0) {
        bn_free(bn);
        return store(val);
    }
    // Otherwise store as BT_BIGNUM
    return store_bignum(bn);
}

void free_bignum_cell(unsigned x)
{
    if (CELL_TYPE(x) == BT_BIGNUM) {
        bignum *bn = get_bignum(x);
        if (bn)
            bn_free(bn);
        CELL_ID(x) = 0;
    }
}

// Safe absolute value for int64_t (handles INT64_MIN)
static inline uint64_t safe_abs(int64_t x)
{
    return x < 0 ? -(uint64_t)x : (uint64_t)x;
}

// Helper: compute GCD for rationals
static int64_t gcd_helper(int64_t a, int64_t b)
{
    uint64_t ua = safe_abs(a);
    uint64_t ub = safe_abs(b);
    while (ub != 0) {
        uint64_t t = ub;
        ub = ua % ub;
        ua = t;
    }
    return (int64_t)ua;
}

unsigned normalize_rational(int64_t num, int64_t denom)
{
    if (denom == 0) {
        show_error("division by zero in rational");
        return TOK_ERROR;
    }
    // Handle signs - denominator always positive
    // Use safe negation to avoid overflow when num or denom is INT64_MIN
    if (denom < 0) {
        // If num is INT64_MIN, negating would overflow - use bignum path
        if (num == INT64_MIN || denom == INT64_MIN) {
            bignum *bn_num = bn_from_int(num);
            bignum *bn_denom = bn_from_int(denom);
            if (bn_sign(bn_denom) < 0) {
                bn_neg_ip(bn_num);
                bn_neg_ip(bn_denom);
            }
            GC_GUARD;
            unsigned num_cell = store_integer(bn_num);
            gc_protect(&num_cell);
            unsigned denom_cell = store_integer(bn_denom);
            return normalize_rational_cells(num_cell, denom_cell);
        }
        num = -num;
        denom = -denom;
    }
    // Reduce to lowest terms
    int64_t g = gcd_helper(num, denom);
    num /= g;
    denom /= g;
    // If denominator is 1, return exact integer
    if (denom == 1) {
        return store(num);
    }
    // Create rational - store components first, then allocate cell
    GC_GUARD;
    unsigned num_cell = store(num);
    gc_protect(&num_cell);
    unsigned denom_cell = store(denom);
    gc_protect(&denom_cell);
    unsigned p = alloc();
    CELL_TYPE(p) = BT_RATIONAL;
    CELL_CAR(p) = num_cell;
    CELL_CDR(p) = denom_cell;
    return p;
}

unsigned store_rational(int64_t num, int64_t denom)
{
    return normalize_rational(num, denom);
}

bool is_negative_number(unsigned x)
{
    switch (CELL_TYPE(x)) {
    case BT_NUM:
        return CELL_ID(x) < 0;
    case BT_BIGNUM:
        return bn_sign(get_bignum(x)) < 0;
    default:
        return false;
    }
}

unsigned negate_number(unsigned x)
{
    switch (CELL_TYPE(x)) {
    case BT_NUM: {
        int64_t val = CELL_ID(x);
        // Handle INT64_MIN overflow
        if (val == INT64_MIN) {
            bignum *bn = bn_from_int(val);
            bn_neg_ip(bn);
            return store_integer(bn);
        }
        return store(-val);
    }
    case BT_BIGNUM: {
        bignum *bn = bn_neg(get_bignum(x));
        return store_integer(bn);
    }
    default:
        return x;
    }
}

unsigned normalize_rational_cells(unsigned num_cell, unsigned denom_cell)
{
    // Protect inputs from GC during allocations
    GC_PROTECT_GUARD2(&num_cell, &denom_cell);

    // Convert to bignums for GCD calculation
    bignum *num = to_bignum(num_cell);
    bignum *denom = to_bignum(denom_cell);

    if (!num || !denom) {
        if (num)
            bn_free(num);
        if (denom)
            bn_free(denom);
        show_error("rational: invalid numeric operands");
        return TOK_ERROR;
    }

    // Check for division by zero
    if (bn_is_zero(denom)) {
        bn_free(num);
        bn_free(denom);
        show_error("division by zero in rational");
        return TOK_ERROR;
    }

    // Normalize sign - denominator always positive
    if (bn_sign(denom) < 0) {
        bn_neg_ip(num);
        bn_neg_ip(denom);
    }

    // Compute GCD and reduce
    bignum *g = bn_gcd(num, denom);
    bignum *reduced_num = bn_div(num, g, NULL);
    bignum *reduced_denom = bn_div(denom, g, NULL);
    bn_free(num);
    bn_free(denom);
    bn_free(g);

    // Check if denominator is 1 - return integer
    int64_t denom_val;
    if (bn_to_int64(reduced_denom, &denom_val) == 0 && denom_val == 1) {
        bn_free(reduced_denom);
        return store_integer(reduced_num);
    }

    // Store numerator and denominator as cells
    GC_GUARD;
    unsigned num_stored = store_integer(reduced_num);
    gc_protect(&num_stored);
    unsigned denom_stored = store_integer(reduced_denom);

    // Create rational cell
    unsigned p = alloc();
    CELL_TYPE(p) = BT_RATIONAL;
    CELL_CAR(p) = num_stored;
    CELL_CDR(p) = denom_stored;
    return p;
}

unsigned store_complex(unsigned real_part, unsigned imag_part)
{
    // If imaginary part is zero, return just the real part
    if (CELL_TYPE(imag_part) == BT_NUM && CELL_ID(imag_part) == 0) {
        return real_part;
    }
    GC_PROTECT_GUARD2(&real_part, &imag_part);
    unsigned p = alloc();
    CELL_TYPE(p) = BT_COMPLEX;
    CELL_CAR(p) = real_part;
    CELL_CDR(p) = imag_part;
    return p;
}

double to_double(unsigned x)
{
    if (x == 0)
        return 0.0;
    switch (CELL_TYPE(x)) {
    case BT_NUM:
        return (double)CELL_ID(x);
    case BT_BIGNUM: {
        // Convert bignum to double (may lose precision for large numbers)
        bignum *bn = get_bignum(x);
        if (!bn)
            return 0.0;
        // Build double from limbs
        double result = 0.0;
        double base = (double)((uint64_t)1 << LIMB_BITS);
        double multiplier = 1.0;
        for (size_t i = 0; i < bn->len; i++) {
            result += bn->limbs[i] * multiplier;
            multiplier *= base;
        }
        return bn->sign ? -result : result;
    }
    case BT_INEXACT: {
        union {
            double d;
            int64_t i;
        } u;
        u.i = CELL_ID(x);
        return u.d;
    }
    case BT_RATIONAL: {
        double num = to_double(CELL_CAR(x));
        double denom = to_double(CELL_CDR(x));
        return num / denom;
    }
    case BT_COMPLEX:
        // For complex, just return the real part as double
        return to_double(CELL_CAR(x));
    default:
        return 0.0;
    }
}

bool is_numeric(unsigned x)
{
    if (x == 0)
        return false;
    enum lisp_type t = CELL_TYPE(x);
    return t == BT_NUM || t == BT_BIGNUM || t == BT_INEXACT ||
           t == BT_RATIONAL || t == BT_COMPLEX;
}

bool is_exact(unsigned x)
{
    if (x == 0)
        return true; // 0 is exact
    switch (CELL_TYPE(x)) {
    case BT_NUM:
    case BT_BIGNUM:
    case BT_RATIONAL:
        return true;
    case BT_INEXACT:
        return false;
    case BT_COMPLEX:
        return is_exact(CELL_CAR(x)) && is_exact(CELL_CDR(x));
    default:
        return false;
    }
}

// ============================================================================
// Type Constructors
// ============================================================================

unsigned make_char(int c)
{
    unsigned p = alloc();
    CELL_TYPE(p) = BT_CHAR;
    CELL_ID(p) = c;
    return p;
}

unsigned make_vector(unsigned len, unsigned fill)
{
    vector_data *vd = malloc(sizeof(vector_data) + len * sizeof(unsigned));
    if (!vd) {
        lisp_panic("failed to allocate vector");
    }
    vd->len = len;
    for (unsigned i = 0; i < len; i++) {
        vd->data[i] = fill;
    }
    unsigned p = alloc();
    CELL_TYPE(p) = BT_VECTOR;
    CELL_PTR(p) = vd;
    return p;
}

unsigned vector_len(unsigned vec)
{
    vector_data *vd = (vector_data *)CELL_PTR(vec);
    return vd->len;
}

unsigned *vector_data_ptr(unsigned vec)
{
    vector_data *vd = (vector_data *)CELL_PTR(vec);
    return vd->data;
}

// ============================================================================
// Continuation Helpers
// ============================================================================

// Create a continuation frame with proper GC protection
unsigned make_cont(enum cont_type type, unsigned data, unsigned env,
                   unsigned next)
{
    // Protect ALL values that will be used after allocation
    GC_GUARD;
    gc_protect(&data);
    gc_protect(&env);
    gc_protect(&next);
    unsigned type_data = 0, env_next = 0;
    gc_protect(&type_data);
    gc_protect(&env_next);

    // Allocate cells - GC may run and update all protected vars
    type_data = alloc_cons(type, data);
    env_next = alloc_cons(env, next);
    unsigned p = alloc();

    // Set up continuation structure
    CELL_TYPE(p) = BT_CONT;
    CELL_CAR(p) = type_data;
    CELL_CDR(p) = env_next;
    return p;
}

// GC-safe version: takes pointers to caller's protected variables
// Caller must have already gc_protect'd these variables
unsigned make_cont_from_protected(enum cont_type type, unsigned *data_ptr,
                                  unsigned *env_ptr, unsigned *next_ptr)
{
    // Protect intermediate allocations too
    GC_GUARD;
    unsigned type_data = 0, env_next = 0;
    gc_protect(&type_data);
    gc_protect(&env_next);

    // Allocate cells - GC may run and update all protected vars
    // Read values AFTER each allocation via pointers
    type_data = alloc_cons(type, *data_ptr);
    env_next = alloc_cons(*env_ptr, *next_ptr);
    unsigned p = alloc();

    // Set up continuation structure
    CELL_TYPE(p) = BT_CONT;
    CELL_CAR(p) = type_data;
    CELL_CDR(p) = env_next;
    return p;
}

unsigned make_halt_cont(void) { return make_cont(CONT_HALT, 0, 0, 0); }

// ============================================================================
// Atom/String Interning
// ============================================================================

// Hash function using current table capacity
static int hash_with_cap(const char *s, unsigned cap)
{
    uint64_t h = FNV_OFFSET_BASIS;
    for (const char *p = s; *p; ++p) {
        h ^= (uint64_t)(*p);
        h *= FNV_PRIME;
    }
    return (int)(h % cap);
}

int hash_function(const char *s) { return hash_with_cap(s, ctx.atom_table_cap); }

static bool str_equals(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
}

// Load factor threshold for rehashing (70%)
#define ATOM_TABLE_LOAD_THRESHOLD 70

// Symbol interning cache with move-to-front (LRU approximation)
#define INTERN_CACHE_SIZE 16
typedef struct {
    const char *str; // Pointer into atom_table (not owned)
    int atom_id;
} intern_cache_entry;
static intern_cache_entry intern_cache[INTERN_CACHE_SIZE];

// Move cache entry at index i to front
static inline void intern_cache_move_to_front(int i)
{
    if (i == 0)
        return;
    intern_cache_entry tmp = intern_cache[i];
    for (int j = i; j > 0; j--) {
        intern_cache[j] = intern_cache[j - 1];
    }
    intern_cache[0] = tmp;
}

// Insert new entry at front, shifting others down
static inline void intern_cache_insert_front(const char *str, int atom_id)
{
    for (int j = INTERN_CACHE_SIZE - 1; j > 0; j--) {
        intern_cache[j] = intern_cache[j - 1];
    }
    intern_cache[0].str = str;
    intern_cache[0].atom_id = atom_id;
}

// Check if n is prime (simple trial division)
static bool is_prime(unsigned n)
{
    if (n < 2)
        return false;
    if (n == 2)
        return true;
    if (n % 2 == 0)
        return false;
    for (unsigned i = 3; i * i <= n; i += 2) {
        if (n % i == 0)
            return false;
    }
    return true;
}

// Find next prime >= n
static unsigned next_prime(unsigned n)
{
    while (!is_prime(n))
        n++;
    return n;
}

// Rehash atom table to new capacity
static void rehash_atom_table(void)
{
    unsigned old_cap = ctx.atom_table_cap;
    unsigned new_cap = next_prime(old_cap * 2);

    const char **new_table = calloc(new_cap, sizeof(const char *));
    if (!new_table) {
        // Can't resize - continue with current table
        fprintf(stderr, "Warning: failed to resize atom table\n");
        return;
    }

    // Rehash all existing entries
    for (unsigned i = 0; i < old_cap; i++) {
        if (ctx.atom_table[i]) {
            int h = hash_with_cap(ctx.atom_table[i], new_cap);
            int probe = h;
            int j = 1;
            while (new_table[probe]) {
                probe = (h + j) % new_cap;
                j++;
            }
            new_table[probe] = ctx.atom_table[i];
        }
    }

    // Invalidate interning cache (slot IDs changed)
    for (int i = 0; i < INTERN_CACHE_SIZE; i++) {
        intern_cache[i].str = NULL;
        intern_cache[i].atom_id = 0;
    }

    free(ctx.atom_table);
    ctx.atom_table = new_table;
    ctx.atom_table_cap = new_cap;
}

int intern(const char *s)
{
    // Check cache first - move to front on hit
    for (int i = 0; i < INTERN_CACHE_SIZE; i++) {
        if (intern_cache[i].str && strcmp(intern_cache[i].str, s) == 0) {
            intern_cache_move_to_front(i);
            return intern_cache[0].atom_id;
        }
    }

    // Check if we need to resize (load factor > 70%)
    if (ctx.atom_count * 100 >= ctx.atom_table_cap * ATOM_TABLE_LOAD_THRESHOLD) {
        rehash_atom_table();
    }

    int hash_value = hash_function(s);
    int original_hash = hash_value;

    for (int i = 1; ctx.atom_table[hash_value] &&
                    !str_equals(ctx.atom_table[hash_value], s);
         i++) {
        hash_value = (original_hash + i) % (int)ctx.atom_table_cap;
        // Prevent infinite loop on full table (shouldn't happen with rehashing)
        if ((unsigned)i >= ctx.atom_table_cap) {
            lisp_panic("atom table full");
        }
    }

    if (!ctx.atom_table[hash_value]) {
        ctx.atom_table[hash_value] = strdup(s);
        if (!ctx.atom_table[hash_value]) {
            lisp_panic("failed to allocate memory for atom");
        }
        ctx.atom_count++;
    }

    // Insert at front of cache
    intern_cache_insert_front(ctx.atom_table[hash_value], hash_value);

    return hash_value;
}

// Helper: check if string could be start of a number
static bool is_number_start(const char *s)
{
    if (isdigit(s[0]))
        return true;
    if ((s[0] == '-' || s[0] == '+') && (isdigit(s[1]) || s[1] == '.'))
        return true;
    if (s[0] == '.' && isdigit(s[1]))
        return true;
    return false;
}

unsigned atom_from_string(const char *s)
{
    size_t n = strlen(s);

    // Check for pure imaginary: +i, -i (but not bare "i" - that's a symbol)
    if (strcmp(s, "+i") == 0) {
        return store_complex(store(0), store(1));
    }
    if (strcmp(s, "-i") == 0) {
        return store_complex(store(0), store(-1));
    }

    // Check if the string might be a number
    if (is_number_start(s)) {
        char *endptr;

        // Check for complex number (look for +/- followed by digits and i at
        // end)
        if (n > 1 && (s[n - 1] == 'i' || s[n - 1] == 'I')) {
            // Find the +/- that separates real and imaginary parts
            const char *sep = NULL;
            for (size_t i = 1; i < n - 1; i++) {
                if ((s[i] == '+' || s[i] == '-') &&
                    (isdigit(s[i - 1]) || s[i - 1] == '.' || s[i - 1] == 'e' ||
                     s[i - 1] == 'E') &&
                    (isdigit(s[i + 1]) || s[i + 1] == '.' || s[i + 1] == 'i' ||
                     s[i + 1] == 'I')) {
                    // Check this isn't part of exponent
                    if (i >= 2 && (s[i - 1] == 'e' || s[i - 1] == 'E'))
                        continue;
                    sep = &s[i];
                }
            }
            if (sep) {
                // Parse real part directly using strtod (avoids strndup)
                double real_val = strtod(s, &endptr);
                unsigned real_part =
                    (endptr == sep) ? store_inexact(real_val) : store(0);

                // Parse imaginary part (without the trailing i)
                unsigned imag_part;
                {
                    GC_PROTECT_GUARD;
                    gc_protect(&real_part);
                    double imag_val = strtod(sep, &endptr);
                    imag_part = store_inexact(imag_val);
                }

                return store_complex(real_part, imag_part);
            }
            // Pure imaginary: 5i, 3.14i, etc. - parse without trailing 'i'
            double imag_val = strtod(s, &endptr);
            // Check that we parsed up to the 'i'
            if (endptr == s + n - 1) {
                return store_complex(store(0), store_inexact(imag_val));
            }
            // Fallback for integers: try strtoll
            int64_t imag_int = strtoll(s, &endptr, 10);
            if (endptr == s + n - 1) {
                return store_complex(store(0), store(imag_int));
            }
            // Last resort: use recursive parsing with temp string
            char *imag_str = strndup(s, n - 1);
            if (!imag_str) {
                show_error("out of memory");
                return TOK_ERROR;
            }
            unsigned imag_part = atom_from_string(imag_str);
            free(imag_str);
            return store_complex(store(0), imag_part);
        }

        // Try parsing as integer first
        errno = 0;
        int64_t ival = strtoll(s, &endptr, 0);
        if ((size_t)(endptr - s) == n) {
            // Check for overflow - if so, parse as bignum
            if (errno == ERANGE || (ival == LLONG_MAX || ival == LLONG_MIN)) {
                // Check if it's really a big number (all digits after optional
                // sign)
                const char *p = s;
                if (*p == '-' || *p == '+')
                    p++;
                bool all_digits = true;
                while (*p) {
                    if (!isdigit(*p)) {
                        all_digits = false;
                        break;
                    }
                    p++;
                }
                if (all_digits) {
                    bignum *bn = bn_from_string(s, 10);
                    if (bn)
                        return store_integer(bn);
                }
            }
            return store(ival);
        }

        // Check if it's a large integer that strtoll couldn't fully parse
        // This handles cases like very long digit strings
        {
            const char *p = s;
            if (*p == '-' || *p == '+')
                p++;
            bool all_digits = (p < s + n);
            while (*p && p < s + n) {
                if (!isdigit(*p)) {
                    all_digits = false;
                    break;
                }
                p++;
            }
            if (all_digits && (size_t)(p - s) == n) {
                bignum *bn = bn_from_string(s, 10);
                if (bn)
                    return store_integer(bn);
            }
        }

        // Check for rational: num/denom (supports bignum numerator/denominator)
        const char *slash = strchr(s, '/');
        if (slash && slash != s && slash != s + n - 1) {
            // Parse numerator
            char *end1;
            errno = 0;
            int64_t num_val = strtoll(s, &end1, 10);
            unsigned num_cell;
            if (end1 == slash) {
                // Parsed successfully up to slash
                if (errno == ERANGE) {
                    // Overflow - parse as bignum
                    char *num_str = strndup(s, (size_t)(slash - s));
                    if (!num_str)
                        return TOK_ERROR;
                    bignum *bn = bn_from_string(num_str, 10);
                    free(num_str);
                    if (!bn)
                        return TOK_ERROR;
                    num_cell = store_integer(bn);
                } else {
                    num_cell = store(num_val);
                }
            } else {
                // Didn't parse to slash - might be bignum
                char *num_str = strndup(s, (size_t)(slash - s));
                if (!num_str)
                    return TOK_ERROR;
                bignum *bn = bn_from_string(num_str, 10);
                free(num_str);
                if (!bn)
                    goto not_rational;
                num_cell = store_integer(bn);
            }

            // Parse denominator
            {
                GC_PROTECT_GUARD;
                gc_protect(&num_cell);
                char *end2;
                errno = 0;
                int64_t denom_val = strtoll(slash + 1, &end2, 10);
                unsigned denom_cell;
                if (*end2 == '\0') {
                    // Parsed successfully to end
                    if (errno == ERANGE) {
                        // Overflow - parse as bignum
                        bignum *bn = bn_from_string(slash + 1, 10);
                        if (!bn) {
                            return TOK_ERROR;
                        }
                        denom_cell = store_integer(bn);
                    } else {
                        denom_cell = store(denom_val);
                    }
                    return normalize_rational_cells(num_cell, denom_cell);
                } else {
                    // Didn't parse to end - might be bignum
                    bignum *bn = bn_from_string(slash + 1, 10);
                    if (bn) {
                        denom_cell = store_integer(bn);
                        return normalize_rational_cells(num_cell, denom_cell);
                    }
                }
            }
        }
    not_rational:

        // Try parsing as floating-point
        double dval = strtod(s, &endptr);
        if ((size_t)(endptr - s) == n) {
            return store_inexact(dval);
        }
    }

    // Not a number - it's a symbol
    int pt = intern(s);
    unsigned x = alloc();
    CELL_TYPE(x) = BT_ATOM;
    CELL_ID(x) = pt;
    return x;
}

// ============================================================================
// List Utilities
// ============================================================================

unsigned list_length(unsigned lst)
{
    unsigned len = 0;
    while (lst && CELL_TYPE(lst) == BT_CONS) {
        len++;
        lst = cdr(lst);
    }
    return len;
}

bool list_length_checked(unsigned lst, unsigned *len_out, const char *name)
{
    unsigned len = 0;
    for (unsigned it = lst; it; it = cdr(it)) {
        if (!IS_PAIR(it)) {
            show_error("%s: improper list", name);
            return false;
        }
        len++;
    }
    if (len_out)
        *len_out = len;
    return true;
}

bool check_args(unsigned args, unsigned min, unsigned max, const char *name)
{
    // Early-exit optimization: only count as many args as needed to decide.
    // For variadic (max=-1), stop after min. For fixed-arity, stop after max+1.
    unsigned limit = (max == (unsigned)-1) ? min : max + 1;
    unsigned len = 0;
    unsigned a = args;

    while (a && CELL_TYPE(a) == BT_CONS && len < limit) {
        len++;
        a = cdr(a);
    }

    if (len < min) {
        show_error("%s: too few arguments (expected %u, got %u)", name, min,
                   len);
        return false;
    }

    // If max is bounded and we counted more than max, or there are still more
    if (max != (unsigned)-1 && (len > max || (a && CELL_TYPE(a) == BT_CONS))) {
        show_error("%s: too many arguments (expected at most %u)", name, max);
        return false;
    }

    return true;
}

void list_append(unsigned *head, unsigned *tail, unsigned elem)
{
    // CRITICAL: Protect elem - alloc() can trigger GC which may move elem.
    // If we pass elem by value to alloc_cons, GC could make that value stale.
    // By protecting elem here, we ensure it's updated if GC moves it.
    unsigned cell;
    {
        GC_PROTECT_GUARD;
        gc_protect(&elem);
        cell = alloc();
        CELL_TYPE(cell) = BT_CONS;
        CELL_CAR(cell) = elem; // elem is now up-to-date after any GC
        CELL_CDR(cell) = 0;
    }
    if (!*head) {
        *head = *tail = cell;
    } else {
        write_barrier(*tail, cell); // *tail may be in old gen
        CELL_CDR(*tail) = cell;
        *tail = cell;
    }
}

bool deep_equal(unsigned a, unsigned b)
{
    if (a == b)
        return true;
    if (a == 0 || b == 0)
        return a == b;
    if (CELL_TYPE(a) != CELL_TYPE(b))
        return false;

    switch (CELL_TYPE(a)) {
    case BT_NUM:
    case BT_CHAR:
    case BT_ATOM:
    case BT_INEXACT:
        return CELL_ID(a) == CELL_ID(b);
    case BT_BIGNUM: {
        bignum *ba = get_bignum(a);
        bignum *bb = get_bignum(b);
        return bn_cmp(ba, bb) == 0;
    }
    case BT_RATIONAL:
    case BT_COMPLEX:
        return deep_equal(car(a), car(b)) && deep_equal(cdr(a), cdr(b));
    case BT_STRING:
        return strcmp(GET_STRING_PTR(a), GET_STRING_PTR(b)) == 0;
    case BT_CONS:
        return deep_equal(car(a), car(b)) && deep_equal(cdr(a), cdr(b));
    case BT_VECTOR: {
        unsigned len_a = vector_len(a);
        unsigned len_b = vector_len(b);
        if (len_a != len_b)
            return false;
        unsigned *da = vector_data_ptr(a);
        unsigned *db = vector_data_ptr(b);
        for (unsigned i = 0; i < len_a; i++) {
            if (!deep_equal(da[i], db[i]))
                return false;
        }
        return true;
    }
    default:
        return false;
    }
}

// ============================================================================
// Garbage Collection
// ============================================================================

unsigned collect(unsigned x)
{
    // Reserved cells (< HEAP_RESERVED) are permanent and never collected
    if (x < HEAP_RESERVED)
        return x;
    // Fixnums are immediate values, not heap pointers
    if (IS_FIXNUM(x))
        return x;

    // Check if cell is already in the to-space (doesn't need collection)
    // During GC, we're copying FROM ctx.nmin side TO ctx.hptr side
    // After swap at start of gc(): nmin was old mmin, hptr started at nmin
    // So to-space is: [old nmin, ctx.hptr)
    // From-space is: the other semispace
    unsigned to_space_start =
        (ctx.hptr < SEMISPACE_SIZE) ? HEAP_RESERVED : SEMISPACE_SIZE;
    unsigned to_space_end = ctx.hptr;
    if (x >= to_space_start && x < to_space_end) {
        // Cell is already in to-space, no collection needed
        return x;
    }

    switch (CELL_TYPE(x)) {
    case BT_BROKENHEART:
        return CELL_CAR(x);

    case BT_FREE:
        return 0;

    case BT_STRING:
    case BT_BYTEVEC: {
        // Pointer-based types: share the malloc'd data between old and new cells.
        // The old region is swept after collection, freeing unreachable data.
        unsigned xx = alloc();
        CELL_TYPE(xx) = CELL_TYPE(x);
        CELL_ID(xx) = CELL_ID(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        return xx;
    }

    case BT_BIGNUM: {
        // Bignum GC follows the same invariant as strings: the pointer is
        // shared, not copied. This is safe because bignums are immutable after
        // storage - to_bignum() always returns a fresh copy for mutation.
        unsigned xx = alloc();
        CELL_TYPE(xx) = BT_BIGNUM;
        CELL_ID(xx) = CELL_ID(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        return xx;
    }

    case BT_VECTOR: {
        // Vector GC: share the vector_data pointer and update element
        // references in-place. Must set BROKENHEART before collecting elements
        // to handle cyclic references (vector → cons → same vector).
        unsigned xx = alloc();
        CELL_TYPE(xx) = BT_VECTOR;
        CELL_ID(xx) = CELL_ID(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        // Now safe to collect elements - cycles will see BROKENHEART
        vector_data *vd = (vector_data *)CELL_PTR(xx);
        for (unsigned i = 0; i < vd->len; i++) {
            vd->data[i] = collect(vd->data[i]);
        }
        return xx;
    }

    case BT_STRINPORT:
    case BT_STROUTPORT: {
        // String port GC follows the same invariant as strings above.
        // The string_port struct and its data buffer are shared during
        // collection; only unreachable ports in the old region are freed.
        unsigned xx = alloc();
        CELL_TYPE(xx) = CELL_TYPE(x);
        CELL_PTR(xx) = CELL_PTR(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        return xx;
    }

    case BT_VMCONT: {
        // VM continuation contains cell references that must be updated.
        // The vm_continuation struct has stack values, env, and frame envs.
        unsigned xx = alloc();
        CELL_TYPE(xx) = BT_VMCONT;
        CELL_PTR(xx) = CELL_PTR(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        // Now update all cell references in the continuation
        vm_continuation *cont = (vm_continuation *)CELL_PTR(xx);
        // Update stack values
        for (unsigned i = 0; i < cont->sp; i++) {
            cont->stack[i] = collect(cont->stack[i]);
        }
        // Update environment
        cont->env = collect(cont->env);
        // Update frame environments
        for (unsigned i = 0; i < cont->fp; i++) {
            cont->frames[i].env = collect(cont->frames[i].env);
        }
        return xx;
    }

    default: {
        unsigned xx = alloc();
        ctx.cons_cells[xx] = ctx.cons_cells[x];
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        return xx;
    }
    }
}

unsigned gc(unsigned root)
{
    ctx.major_gc_count++;

    // Enter GC mode - alloc() will use old gen instead of nursery
    in_gc_mode = true;

    // Prevent recursive GC during collection
    unsigned *saved_gc_root = alloc_gc_root;
    alloc_gc_root = NULL;

    // Switch to new semispace
    ctx.hptr = ctx.nmin;
    unsigned scan = ctx.hptr;

    // Nursery disabled - set beyond semispace
    if (ctx.nmin < SEMISPACE_SIZE) {
        ctx.nursery_start = SEMISPACE_SIZE;
    } else {
        ctx.nursery_start = 2 * SEMISPACE_SIZE;
    }
    ctx.nursery_ptr = ctx.nursery_start;

    // Copy initial root to new space (skip reserved cells)
    if (root >= HEAP_RESERVED) {
        unsigned x = alloc();
        ctx.cons_cells[x] = ctx.cons_cells[root];
        CELL_TYPE(root) = BT_BROKENHEART;
        CELL_CAR(root) = x;
        root = x;
    }

    // Collect trampoline state (global evaluator state)
    tramp.expr = collect(tramp.expr);
    tramp.env = collect(tramp.env);
    tramp.cont = collect(tramp.cont);
    tramp.value = collect(tramp.value);

    // Collect shadow stack entries - these are pointers to local C variables
    // that hold cell IDs. After GC, we update them to point to new locations.
    for (int i = 0; i < shadow_stack_top; i++) {
        if (shadow_stack[i] && *shadow_stack[i] >= HEAP_RESERVED) {
            *shadow_stack[i] = collect(*shadow_stack[i]);
        }
    }

    // Update VM roots if VM is active
    gc_update_vm_roots(get_active_vm());

    // Collect code object constants BEFORE scan phase so their
    // CAR/CDR are recursively processed
    gc_update_all_code_objects();
    gc_update_all_patterns();

    while (scan != ctx.hptr) {
        enum lisp_type t = CELL_TYPE(scan);
        if (t == BT_CONS || t == BT_FUNCTION || t == BT_MACRO ||
            t == BT_SYNTAX || t == BT_CONT || t == BT_RATIONAL ||
            t == BT_COMPLEX) {
            CELL_CAR(scan) = collect(CELL_CAR(scan));
            CELL_CDR(scan) = collect(CELL_CDR(scan));
        }
        scan++;
    }

    unsigned tmp = ctx.nmin;
    ctx.nmin = ctx.mmin;
    ctx.mmin = tmp;

    // Note: ctx.atom_* are in reserved space (< HEAP_RESERVED) and don't need
    // collection

    // Update alloc_gc_root before freeing old space (broken hearts still valid)
    alloc_gc_root = saved_gc_root;
    if (alloc_gc_root && *alloc_gc_root >= HEAP_RESERVED) {
        *alloc_gc_root = collect(*alloc_gc_root);
    }

    // Note: gc_sweep_code_objects() is NOT called here because the VM may
    // still be executing code objects that have no closures in the heap
    // (e.g., toplevel code). Call it explicitly when the VM is idle.

    // Sweep phase: Free heap-allocated data in old memory region (now at nmin
    // after the swap). This is safe because:
    //   - Reachable cells were copied and marked as BT_BROKENHEART
    //   - Unreachable cells retain their original type (BT_STRING, etc.)
    //   - We only free data from cells that still have their original type
    //   - All pointer types (bignums, strings, vectors, string ports) share
    //     their heap data with the new cell, so only unreachable data is freed
    // Note: old_end covers the ENTIRE old semispace including its nursery
    unsigned old_start = ctx.nmin;
    unsigned old_end =
        (ctx.nmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
    for (unsigned i = old_start; i < old_end; i++) {
        if (CELL_TYPE(i) == BT_BIGNUM) {
            free_bignum_cell(i);
        } else if (CELL_TYPE(i) == BT_VECTOR) {
            vector_data *vd = (vector_data *)CELL_PTR(i);
            if (vd)
                free(vd);
            CELL_PTR(i) = NULL;
        } else if (CELL_TYPE(i) == BT_STRING || CELL_TYPE(i) == BT_BYTEVEC) {
            if (CELL_PTR(i))
                free(CELL_PTR(i));
            CELL_PTR(i) = NULL;
        } else if (CELL_TYPE(i) == BT_STRINPORT ||
                   CELL_TYPE(i) == BT_STROUTPORT) {
            string_port *sp = (string_port *)CELL_PTR(i);
            if (sp) {
                free(sp->data);
                free(sp);
            }
            CELL_PTR(i) = NULL;
        }
        CELL_TYPE(i) = BT_FREE;
    }

    // Reset nursery to end of new active semispace
    if (ctx.mmin < SEMISPACE_SIZE) {
        ctx.nursery_start = SEMISPACE_SIZE - NURSERY_SIZE;
    } else {
        ctx.nursery_start = 2 * SEMISPACE_SIZE - NURSERY_SIZE;
    }
    ctx.nursery_ptr = ctx.nursery_start;

    // Clear card table for the new semispace (if generational GC is enabled)
    if (ctx.card_table) {
        size_t num_cards = (2 * SEMISPACE_SIZE) / CARD_SIZE;
        memset(ctx.card_table, 0, (num_cards + 7) / 8);
    }

    // Exit GC mode
    in_gc_mode = false;

    // Invalidate environment lookup cache (cell references are now stale)
    env_invalidate_cache();

    return root;
}

int heap_usage_percent(void)
{
    // For old generation usage
    unsigned used = ctx.hptr - ctx.mmin;
    unsigned avail = ctx.nursery_start - ctx.mmin;
    if (avail == 0)
        return 100; // Full
    return (int)((100ULL * used) / avail);
}

// ============================================================================
// Write Barrier (for generational GC) - Card Marking
// ============================================================================

void write_barrier(unsigned target, unsigned value)
{
    // Skip if generational GC is disabled (no card table)
    if (!ctx.card_table)
        return;

    // Only care about old→young pointers
    // target must be in old gen (between mmin and nursery_start)
    if (target < ctx.mmin || target >= ctx.nursery_start)
        return; // target not in old gen
    // value must be in nursery (between nursery_start and nursery_ptr)
    if (value < ctx.nursery_start || value >= ctx.nursery_ptr)
        return; // value not in nursery

    // Mark the card containing target as dirty - O(1) operation
    unsigned card = target / CARD_SIZE;
    CARD_MARK(ctx.card_table, card);
}

// ============================================================================
// Minor GC (Nursery Collection)
// ============================================================================

// Forward declare collect for reuse (for generational GC when enabled)
static unsigned collect_to_old(unsigned x);

// Collect a nursery cell to old generation
static unsigned collect_to_old(unsigned x)
{
    // Reserved cells don't need collection
    if (x < HEAP_RESERVED)
        return x;
    // Fixnums are immediate values, not heap pointers
    if (IS_FIXNUM(x))
        return x;
    // Not in nursery (in old gen)
    if (x < ctx.nursery_start)
        return x;
    // Not in nursery (past current allocation pointer)
    if (x >= ctx.nursery_ptr) {
        return x;
    }

    switch (CELL_TYPE(x)) {
    case BT_BROKENHEART:
        return CELL_CAR(x);

    case BT_FREE:
        return 0;

    case BT_STRING:
    case BT_BYTEVEC: {
        // Copy pointer-based cell to old gen (share malloc'd data)
        if (ctx.hptr >= ctx.nursery_start) {
            lisp_panic("old generation full during minor GC");
        }
        unsigned xx = ctx.hptr++;
        CELL_TYPE(xx) = CELL_TYPE(x);
        CELL_ID(xx) = CELL_ID(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        return xx;
    }

    case BT_BIGNUM: {
        if (ctx.hptr >= ctx.nursery_start) {
            lisp_panic("old generation full during minor GC");
        }
        unsigned xx = ctx.hptr++;
        CELL_TYPE(xx) = BT_BIGNUM;
        CELL_ID(xx) = CELL_ID(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        return xx;
    }

    case BT_VECTOR: {
        if (ctx.hptr >= ctx.nursery_start) {
            lisp_panic("old generation full during minor GC");
        }
        unsigned xx = ctx.hptr++;
        CELL_TYPE(xx) = BT_VECTOR;
        CELL_PTR(xx) = CELL_PTR(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        // Collect vector elements
        vector_data *vd = (vector_data *)CELL_PTR(xx);
        for (unsigned i = 0; i < vd->len; i++) {
            vd->data[i] = collect_to_old(vd->data[i]);
        }
        return xx;
    }

    case BT_STRINPORT:
    case BT_STROUTPORT: {
        if (ctx.hptr >= ctx.nursery_start) {
            lisp_panic("old generation full during minor GC");
        }
        unsigned xx = ctx.hptr++;
        CELL_TYPE(xx) = CELL_TYPE(x);
        CELL_PTR(xx) = CELL_PTR(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        return xx;
    }

    case BT_VMCONT: {
        // VM continuation contains cell references that must be updated.
        if (ctx.hptr >= ctx.nursery_start) {
            lisp_panic("old generation full during minor GC");
        }
        unsigned xx = ctx.hptr++;
        CELL_TYPE(xx) = BT_VMCONT;
        CELL_PTR(xx) = CELL_PTR(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        // Update all cell references in the continuation
        vm_continuation *cont = (vm_continuation *)CELL_PTR(xx);
        for (unsigned i = 0; i < cont->sp; i++) {
            cont->stack[i] = collect_to_old(cont->stack[i]);
        }
        cont->env = collect_to_old(cont->env);
        for (unsigned i = 0; i < cont->fp; i++) {
            cont->frames[i].env = collect_to_old(cont->frames[i].env);
        }
        return xx;
    }

    default: {
        if (ctx.hptr >= ctx.nursery_start) {
            lisp_panic("old generation full during minor GC");
        }
        unsigned xx = ctx.hptr++;
        ctx.cons_cells[xx] = ctx.cons_cells[x];
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        return xx;
    }
    }
}

unsigned minor_gc(unsigned root)
{
    ctx.minor_gc_count++;
    unsigned scan = ctx.hptr; // Start of newly promoted objects

    // Collect root
    root = collect_to_old(root);

    // Collect trampoline state
    tramp.expr = collect_to_old(tramp.expr);
    tramp.env = collect_to_old(tramp.env);
    tramp.cont = collect_to_old(tramp.cont);
    tramp.value = collect_to_old(tramp.value);

    // Collect shadow stack entries
    for (int i = 0; i < shadow_stack_top; i++) {
        if (shadow_stack[i]) {
            *shadow_stack[i] = collect_to_old(*shadow_stack[i]);
        }
    }

    // Update VM roots if VM is active
    {
        vm_state *vm = get_active_vm();
        if (vm) {
            // Update stack values
            for (unsigned i = 0; i < vm->sp; i++) {
                vm->stack[i] = collect_to_old(vm->stack[i]);
            }
            // Update frame environments
            for (unsigned i = 0; i < vm->fp; i++) {
                vm->frames[i].env = collect_to_old(vm->frames[i].env);
            }
            // Update current environment
            vm->env = collect_to_old(vm->env);
        }
    }

    // Update code object constants - they may point to nursery cells
    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        for (unsigned i = 0; i < code->const_len; i++) {
            code->constants[i] = collect_to_old(code->constants[i]);
        }
    }

    // Scan dirty cards in old generation for nursery pointers
    // Skip if generational GC is disabled (no card table)
    if (ctx.card_table) {
        // Use 'scan' as limit - cells from scan onward are being promoted
        // during this GC
        unsigned card_start = ctx.mmin / CARD_SIZE;
        unsigned card_end =
            (scan + CARD_SIZE - 1) / CARD_SIZE; // Cards up to original hptr
        for (unsigned card = card_start; card < card_end; card++) {
            if (!CARD_IS_DIRTY(ctx.card_table, card))
                continue;
            // Scan all cells in this card
            unsigned cell_start = card * CARD_SIZE;
            unsigned cell_end = cell_start + CARD_SIZE;
            if (cell_start < ctx.mmin)
                cell_start = ctx.mmin;
            if (cell_end > scan)
                cell_end = scan; // Only scan cells that existed before this GC
            for (unsigned cell = cell_start; cell < cell_end; cell++) {
                enum lisp_type t = CELL_TYPE(cell);
                if (t == BT_CONS || t == BT_FUNCTION || t == BT_MACRO ||
                    t == BT_SYNTAX || t == BT_CONT || t == BT_RATIONAL ||
                    t == BT_COMPLEX) {
                    // CPS continuations (BT_CONT) use CAR/CDR for structure
                    CELL_CAR(cell) = collect_to_old(CELL_CAR(cell));
                    CELL_CDR(cell) = collect_to_old(CELL_CDR(cell));
                } else if (t == BT_VECTOR) {
                    vector_data *vd = (vector_data *)CELL_PTR(cell);
                    for (unsigned j = 0; j < vd->len; j++) {
                        vd->data[j] = collect_to_old(vd->data[j]);
                    }
                } else if (t == BT_VMCONT) {
                    // VM continuation: update cell references inside struct
                    vm_continuation *cont = (vm_continuation *)CELL_PTR(cell);
                    for (unsigned j = 0; j < cont->sp; j++) {
                        cont->stack[j] = collect_to_old(cont->stack[j]);
                    }
                    cont->env = collect_to_old(cont->env);
                    for (unsigned j = 0; j < cont->fp; j++) {
                        cont->frames[j].env =
                            collect_to_old(cont->frames[j].env);
                    }
                }
            }
            // Clear the card
            CARD_CLEAR(ctx.card_table, card);
        }
    } // end if (ctx.card_table)

    // Scan newly promoted objects in old gen
    while (scan != ctx.hptr) {
        enum lisp_type t = CELL_TYPE(scan);
        if (t == BT_CONS || t == BT_FUNCTION || t == BT_MACRO ||
            t == BT_SYNTAX || t == BT_CONT || t == BT_RATIONAL ||
            t == BT_COMPLEX) {
            // CPS continuations (BT_CONT) use CAR/CDR for structure
            CELL_CAR(scan) = collect_to_old(CELL_CAR(scan));
            CELL_CDR(scan) = collect_to_old(CELL_CDR(scan));
        } else if (t == BT_VMCONT) {
            // VM continuation's internal values were already collected when
            // the cell was promoted - no need to scan again
        }
        scan++;
    }

    // Reset nursery - all survivors are now in old gen
    ctx.nursery_ptr = ctx.nursery_start;

    // Invalidate environment lookup cache (cell references may have moved)
    env_invalidate_cache();

    return root;
}

unsigned maybe_gc(unsigned root, int threshold_percent)
{
    if (heap_usage_percent() >= threshold_percent) {
        return gc(root);
    }
    return root;
}

// ============================================================================
// Initialization
// ============================================================================

// Initialize a permanent atom in reserved space (never garbage collected)
static void init_permanent_atom(unsigned cell_id, const char *name)
{
    CELL_TYPE(cell_id) = BT_ATOM;
    CELL_ID(cell_id) = intern(name);
}

void init_keywords(void)
{
    // Initialize current ports
    ctx.current_input = stdin;
    ctx.current_output = stdout;
    ctx.transcript = NULL;

    // Initialize small integer cache (cells INT_CACHE_START to
    // INT_CACHE_START+255)
    for (int i = INT_CACHE_MIN; i <= INT_CACHE_MAX; i++) {
        unsigned cell = INT_CACHE_START + (i - INT_CACHE_MIN);
        CELL_TYPE(cell) = BT_NUM;
        CELL_ID(cell) = i;
    }

    // Initialize permanent atoms in reserved cells (never GC'd)
    init_permanent_atom(CELL_ATOM_FALSE, "false");
    init_permanent_atom(CELL_ATOM_TRUE, "true");
    init_permanent_atom(CELL_ATOM_QUOTE, "quote");
    init_permanent_atom(CELL_ATOM_QUASIQUOTE, "quasiquote");
    init_permanent_atom(CELL_ATOM_UNQUOTE, "unquote");
    init_permanent_atom(CELL_ATOM_UNQUOTE_SPLICING, "unquote-splicing");

    ctx.atom_true = CELL_ATOM_TRUE;
    ctx.atom_false = CELL_ATOM_FALSE;
    ctx.atom_quote = CELL_ATOM_QUOTE;
    ctx.atom_quasiquote = CELL_ATOM_QUASIQUOTE;
    ctx.atom_unquote = CELL_ATOM_UNQUOTE;
    ctx.atom_unquote_splicing = CELL_ATOM_UNQUOTE_SPLICING;

    // Cache keyword IDs (these are atom table indices, not cell IDs)
    ctx.kw_quote = intern("quote");
    ctx.kw_lambda = intern("lambda");
    ctx.kw_begin = intern("begin");
    ctx.kw_and = intern("and");
    ctx.kw_or = intern("or");
    ctx.kw_cond = intern("cond");
    ctx.kw_set = intern("set!");
    ctx.kw_define = intern("define");
    ctx.kw_if = intern("if");
    ctx.kw_let = intern("let");
    ctx.kw_letstar = intern("let*");
    ctx.kw_letrec = intern("letrec");
    ctx.kw_quasiquote = intern("quasiquote");
    ctx.kw_unquote = intern("unquote");
    ctx.kw_unquote_splicing = intern("unquote-splicing");
    ctx.kw_define_macro = intern("define-macro");
    ctx.kw_define_syntax = intern("define-syntax");
    ctx.kw_syntax_rules = intern("syntax-rules");
    ctx.kw_ellipsis = intern("...");
    ctx.kw_underscore = intern("_");
    ctx.kw_else = intern("else");
    ctx.kw_arrow = intern("=>");
    ctx.kw_let_syntax = intern("let-syntax");
    ctx.kw_letrec_syntax = intern("letrec-syntax");
    ctx.kw_protected = intern("##protected##");
}
