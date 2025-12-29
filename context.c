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
    // Allocate cons cells heap
    size_t heap_size = 2 * SEMISPACE_SIZE * sizeof(cons_cell);
    ctx.cons_cells = malloc(heap_size);
    if (!ctx.cons_cells) {
        fprintf(stderr, "Fatal: failed to allocate %zu bytes for heap\n",
                heap_size);
        exit(EXIT_FAILURE);
    }
    memset(ctx.cons_cells, 0, heap_size);

    // Allocate atom table
    size_t table_size = TABLE_SIZE * sizeof(const char *);
    ctx.atom_table = malloc(table_size);
    if (!ctx.atom_table) {
        fprintf(stderr, "Fatal: failed to allocate %zu bytes for atom table\n",
                table_size);
        exit(EXIT_FAILURE);
    }
    memset(ctx.atom_table, 0, table_size);
    ctx.atom_count = 0;
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

void set_alloc_gc_root(unsigned *root) { alloc_gc_root = root; }

// Shadow stack for GC roots - protects local C variables during allocation
// When GC runs, these are collected and updated in place
#define SHADOW_STACK_SIZE 256
static unsigned *shadow_stack[SHADOW_STACK_SIZE];
static int shadow_stack_top = 0;

void gc_protect(unsigned *ptr)
{
    if (shadow_stack_top >= SHADOW_STACK_SIZE) {
        lisp_panic("shadow stack overflow");
    }
    shadow_stack[shadow_stack_top++] = ptr;
}

void gc_unprotect(int count)
{
    shadow_stack_top -= count;
    if (shadow_stack_top < 0) {
        lisp_panic("shadow stack underflow");
    }
}

void trigger_gc(void)
{
    if (alloc_gc_root && *alloc_gc_root) {
        *alloc_gc_root = gc(*alloc_gc_root);
    }
}

unsigned alloc(void)
{
    unsigned limit =
        (ctx.hptr < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;

    // Check if we're at 90% capacity - trigger GC if we have a root
    if (ctx.hptr >= limit * 9 / 10 && alloc_gc_root && *alloc_gc_root) {
        *alloc_gc_root = gc(*alloc_gc_root);
        // Recalculate limit after GC (we may have switched semispaces)
        limit =
            (ctx.hptr < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
    }

    if (ctx.hptr >= limit) {
        lisp_panic("out of memory (heap exhausted)");
    }
    return ctx.hptr++;
}

unsigned alloc_cons(unsigned car_val, unsigned cdr_val)
{
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
    // Store pointer in id field
    CELL_ID(p) = (int64_t)(intptr_t)bn;
    return p;
}

bignum *get_bignum(unsigned x)
{
    if (CELL_TYPE(x) != BT_BIGNUM)
        return NULL;
    return (bignum *)(intptr_t)CELL_ID(x);
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
    // Debug: report unexpected type
    fprintf(stderr,
            "Warning: to_bignum called with non-integer cell %u type %d\n", x,
            CELL_TYPE(x));
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
    if (denom < 0) {
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
    // Create rational
    unsigned p = alloc();
    CELL_TYPE(p) = BT_RATIONAL;
    CELL_CAR(p) = store(num);
    CELL_CDR(p) = store(denom);
    return p;
}

unsigned store_rational(int64_t num, int64_t denom)
{
    return normalize_rational(num, denom);
}

unsigned store_complex(unsigned real_part, unsigned imag_part)
{
    // If imaginary part is zero, return just the real part
    if (CELL_TYPE(imag_part) == BT_NUM && CELL_ID(imag_part) == 0) {
        return real_part;
    }
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
    CELL_ID(p) = (int64_t)(intptr_t)vd;
    return p;
}

unsigned vector_len(unsigned vec)
{
    vector_data *vd = (vector_data *)(intptr_t)CELL_ID(vec);
    return vd->len;
}

unsigned *vector_data_ptr(unsigned vec)
{
    vector_data *vd = (vector_data *)(intptr_t)CELL_ID(vec);
    return vd->data;
}

// ============================================================================
// Continuation Helpers
// ============================================================================

unsigned make_cont(enum cont_type type, unsigned data, unsigned env,
                   unsigned next)
{
    // Protect input cells from GC during allocations
    gc_protect(&data);
    gc_protect(&env);
    gc_protect(&next);
    unsigned type_data = alloc_cons(type, data);
    unsigned env_next = alloc_cons(env, next);
    unsigned p = alloc();
    CELL_TYPE(p) = BT_CONT;
    CELL_CAR(p) = type_data;
    CELL_CDR(p) = env_next;
    gc_unprotect(3);
    return p;
}

unsigned make_halt_cont(void) { return make_cont(CONT_HALT, 0, 0, 0); }

// ============================================================================
// Atom/String Interning
// ============================================================================

int hash_function(const char *s)
{
    uint64_t h = FNV_OFFSET_BASIS;
    for (const char *p = s; *p; ++p) {
        h ^= (uint64_t)(*p);
        h *= FNV_PRIME;
    }
    return (int)(h % TABLE_SIZE);
}

static bool str_equals(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
}

// Maximum load factor before warning (70%)
#define ATOM_TABLE_LOAD_WARN (TABLE_SIZE * 70 / 100)

int intern(const char *s)
{
    int hash_value = hash_function(s);
    int original_hash = hash_value;

    for (int i = 1; ctx.atom_table[hash_value] &&
                    !str_equals(ctx.atom_table[hash_value], s);
         i++) {
        hash_value = (original_hash + i) % TABLE_SIZE;
        // Prevent infinite loop on full table
        if (i >= TABLE_SIZE) {
            lisp_panic("atom table full");
        }
    }

    if (!ctx.atom_table[hash_value]) {
        ctx.atom_table[hash_value] = strdup(s);
        if (!ctx.atom_table[hash_value]) {
            lisp_panic("failed to allocate memory for atom");
        }
        ctx.atom_count++;

        // Warn once when load factor exceeds threshold
        if (ctx.atom_count == ATOM_TABLE_LOAD_WARN) {
            fprintf(stderr, "Warning: atom table %.0f%% full (%u/%d symbols)\n",
                    100.0 * ctx.atom_count / TABLE_SIZE, ctx.atom_count,
                    TABLE_SIZE);
        }
    }
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
    if (strcmp(s, "nil") == 0) {
        return TOK_NIL;
    }

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
                // Parse real part
                char *real_str = strndup(s, sep - s);
                unsigned real_part = atom_from_string(real_str);
                free(real_str);

                // Parse imaginary part (without the trailing i)
                size_t imag_len = (s + n - 1) - sep;
                char *imag_str = strndup(sep, imag_len);
                unsigned imag_part = atom_from_string(imag_str);
                free(imag_str);

                return store_complex(real_part, imag_part);
            }
            // Pure imaginary: 5i, 3.14i, etc.
            char *imag_str = strndup(s, n - 1);
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

        // Check for rational: num/denom
        const char *slash = strchr(s, '/');
        if (slash && slash != s && slash != s + n - 1) {
            char *num_str = strndup(s, slash - s);
            char *denom_str = strdup(slash + 1);
            char *end1, *end2;
            int64_t num = strtoll(num_str, &end1, 10);
            int64_t denom = strtoll(denom_str, &end2, 10);
            bool valid = (*end1 == '\0' && *end2 == '\0');
            free(num_str);
            free(denom_str);
            if (valid) {
                return store_rational(num, denom);
            }
        }

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

bool check_args(unsigned args, unsigned min, unsigned max, const char *name)
{
    unsigned len = list_length(args);
    if (len < min) {
        show_error("%s: too few arguments (expected %u, got %u)", name, min,
                   len);
        return false;
    }
    if (max != (unsigned)-1 && len > max) {
        show_error("%s: too many arguments (expected %u, got %u)", name, max,
                   len);
        return false;
    }
    return true;
}

void list_append(unsigned *head, unsigned *tail, unsigned elem)
{
    unsigned cell = alloc_cons(elem, 0);
    if (!*head) {
        *head = *tail = cell;
    } else {
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
        return strcmp((char *)(intptr_t)CELL_ID(a),
                      (char *)(intptr_t)CELL_ID(b)) == 0;
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

    switch (CELL_TYPE(x)) {
    case BT_BROKENHEART:
        return CELL_CAR(x);

    case BT_FREE:
        return 0;

    case BT_STRING: {
        // Just copy the pointer - string data is shared with new cell
        // Unreachable strings are freed during sweep
        unsigned xx = alloc();
        CELL_TYPE(xx) = BT_STRING;
        CELL_ID(xx) = CELL_ID(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        return xx;
    }

    case BT_BIGNUM: {
        unsigned xx = alloc();
        bignum *old_bn = get_bignum(x);
        bignum *new_bn = bn_copy(old_bn);
        CELL_TYPE(xx) = BT_BIGNUM;
        CELL_ID(xx) = (int64_t)(intptr_t)new_bn;
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        // Note: old_bn will be freed when we swap memory regions
        return xx;
    }

    case BT_VECTOR: {
        unsigned xx = alloc();
        vector_data *old_vd = (vector_data *)(intptr_t)CELL_ID(x);
        unsigned len = old_vd->len;
        vector_data *new_vd =
            malloc(sizeof(vector_data) + len * sizeof(unsigned));
        if (!new_vd) {
            lisp_panic("failed to allocate vector during GC");
        }
        new_vd->len = len;
        for (unsigned i = 0; i < len; i++) {
            new_vd->data[i] = collect(old_vd->data[i]);
        }
        CELL_TYPE(xx) = BT_VECTOR;
        CELL_ID(xx) = (int64_t)(intptr_t)new_vd;
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        return xx;
    }

    case BT_STRINPORT:
    case BT_STROUTPORT: {
        // Just copy the pointer - string port data is shared with new cell
        // Unreachable string ports are freed during sweep
        unsigned xx = alloc();
        CELL_TYPE(xx) = CELL_TYPE(x);
        CELL_ID(xx) = CELL_ID(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
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
    // Prevent recursive GC during collection
    unsigned *saved_gc_root = alloc_gc_root;
    alloc_gc_root = NULL;

    ctx.hptr = ctx.nmin;
    unsigned scan = ctx.hptr;

    // Copy initial root to new space (skip reserved cells)
    if (root >= HEAP_RESERVED) {
        unsigned x = alloc();
        ctx.cons_cells[x] = ctx.cons_cells[root];
        CELL_TYPE(root) = BT_BROKENHEART;
        CELL_CAR(root) = x;
        root = x;
    }

    // Also collect trampoline state (global evaluator state)
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

    // Free heap-allocated data in old memory region (now at nmin after the
    // swap)
    unsigned old_start = ctx.nmin;
    unsigned old_end =
        (ctx.nmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
    for (unsigned i = old_start; i < old_end; i++) {
        if (CELL_TYPE(i) == BT_BIGNUM) {
            free_bignum_cell(i);
        } else if (CELL_TYPE(i) == BT_VECTOR) {
            vector_data *vd = (vector_data *)(intptr_t)CELL_ID(i);
            if (vd)
                free(vd);
            CELL_ID(i) = 0;
        } else if (CELL_TYPE(i) == BT_STRING) {
            char *str = (char *)(intptr_t)CELL_ID(i);
            if (str)
                free(str);
            CELL_ID(i) = 0;
        } else if (CELL_TYPE(i) == BT_STRINPORT ||
                   CELL_TYPE(i) == BT_STROUTPORT) {
            string_port *sp = (string_port *)(intptr_t)CELL_ID(i);
            if (sp) {
                free(sp->data);
                free(sp);
            }
            CELL_ID(i) = 0;
        }
        CELL_TYPE(i) = BT_FREE;
    }

    return root;
}

int heap_usage_percent(void)
{
    unsigned limit =
        (ctx.hptr < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
    unsigned used = ctx.hptr - ctx.nmin;
    unsigned avail = limit - ctx.nmin;
    if (avail == 0)
        return 100; // Full
    return (int)((100ULL * used) / avail);
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
    init_permanent_atom(CELL_ATOM_TRUE, "t");
    init_permanent_atom(CELL_ATOM_QUOTE, "quote");
    init_permanent_atom(CELL_ATOM_QUASIQUOTE, "quasiquote");
    init_permanent_atom(CELL_ATOM_UNQUOTE, "unquote");
    init_permanent_atom(CELL_ATOM_UNQUOTE_SPLICING, "unquote-splicing");

    ctx.atom_true = CELL_ATOM_TRUE;
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
    ctx.kw_let_syntax = intern("let-syntax");
    ctx.kw_letrec_syntax = intern("letrec-syntax");
}
