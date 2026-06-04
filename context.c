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
#include "feature_table.h"
#include "reader.h"
#include <ctype.h>
#include <errno.h>
#include <locale.h>
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
                    .current_error = NULL,  // Set to stderr in init_keywords
                    .transcript = NULL};

tramp_state tramp;
unsigned gensym_counter = 0;

typedef struct ptr_registry_node {
    void *ptr;
    struct ptr_registry_node *next;
} ptr_registry_node;

typedef struct sized_ptr_registry_node {
    void *ptr;
    unsigned len;
    struct sized_ptr_registry_node *next;
} sized_ptr_registry_node;

static ptr_registry_node *vm_continuation_registry = NULL;
static ptr_registry_node *file_port_registry = NULL;
static ptr_registry_node *string_port_registry = NULL;
static ptr_registry_node *hash_table_registry = NULL;
static ptr_registry_node *bignum_registry = NULL;
static ptr_registry_node *string_registry = NULL;
static sized_ptr_registry_node *vector_registry = NULL;
static sized_ptr_registry_node *bytevec_registry = NULL;

// Error recovery for fatal errors
jmp_buf panic_jmp;
bool panic_jmp_set = false;

static void ptr_registry_register(ptr_registry_node **registry, void *ptr,
                                  const char *malloc_error)
{
    if (!ptr)
        return;
    for (ptr_registry_node *node = *registry; node; node = node->next) {
        if (node->ptr == ptr)
            return;
    }

    ptr_registry_node *node = checked_malloc_size(sizeof(*node));
    LISP_ASSERT_MSG(node != NULL, malloc_error);
    node->ptr = ptr;
    node->next = *registry;
    *registry = node;
}

static void ptr_registry_unregister(ptr_registry_node **registry, void *ptr)
{
    ptr_registry_node **prev = registry;
    while (*prev) {
        ptr_registry_node *node = *prev;
        if (node->ptr == ptr) {
            *prev = node->next;
            free(node);
            return;
        }
        prev = &node->next;
    }
}

static bool ptr_registry_contains(const ptr_registry_node *registry,
                                  const void *ptr)
{
    if (!ptr)
        return false;
    for (const ptr_registry_node *node = registry; node; node = node->next) {
        if (node->ptr == ptr)
            return true;
    }
    return false;
}

static void sized_ptr_registry_register(sized_ptr_registry_node **registry,
                                        void *ptr, unsigned len,
                                        const char *malloc_error)
{
    if (!ptr)
        return;
    for (sized_ptr_registry_node *node = *registry; node; node = node->next) {
        if (node->ptr == ptr)
            return;
    }

    sized_ptr_registry_node *node = checked_malloc_size(sizeof(*node));
    LISP_ASSERT_MSG(node != NULL, malloc_error);
    node->ptr = ptr;
    node->len = len;
    node->next = *registry;
    *registry = node;
}

static void sized_ptr_registry_unregister(sized_ptr_registry_node **registry,
                                          void *ptr)
{
    sized_ptr_registry_node **prev = registry;
    while (*prev) {
        sized_ptr_registry_node *node = *prev;
        if (node->ptr == ptr) {
            *prev = node->next;
            free(node);
            return;
        }
        prev = &node->next;
    }
}

static const sized_ptr_registry_node *
sized_ptr_registry_find(const sized_ptr_registry_node *registry,
                        const void *ptr)
{
    if (!ptr)
        return NULL;
    for (const sized_ptr_registry_node *node = registry; node;
         node = node->next) {
        if (node->ptr == ptr)
            return node;
    }
    return NULL;
}

void vm_continuation_register(vm_continuation *cont)
{
    if (!cont)
        return;
    ptr_registry_register(&vm_continuation_registry, cont,
                          "vm_continuation_register: malloc failed");
}

void vm_continuation_unregister(vm_continuation *cont)
{
    ptr_registry_unregister(&vm_continuation_registry, cont);
}

bool vm_continuation_is_registered(const vm_continuation *cont)
{
    return ptr_registry_contains(vm_continuation_registry, cont);
}

bool is_vm_continuation_object(unsigned value)
{
    return IS_CELL(value) && CELL_TYPE(value) == BT_VMCONT &&
           vm_continuation_is_registered((const vm_continuation *)CELL_PTR(value));
}

void file_port_register(file_port *port)
{
    if (!port)
        return;
    ptr_registry_register(&file_port_registry, port,
                          "file_port_register: malloc failed");
}

void file_port_unregister(file_port *port)
{
    ptr_registry_unregister(&file_port_registry, port);
}

bool file_port_is_registered(const file_port *port)
{
    return ptr_registry_contains(file_port_registry, port);
}

void string_port_register(string_port *port)
{
    if (!port)
        return;
    ptr_registry_register(&string_port_registry, port,
                          "string_port_register: malloc failed");
}

void string_port_unregister(string_port *port)
{
    ptr_registry_unregister(&string_port_registry, port);
}

bool string_port_is_registered(const string_port *port)
{
    return ptr_registry_contains(string_port_registry, port);
}

void hash_table_register(hash_table_data *table)
{
    if (!table)
        return;
    ptr_registry_register(&hash_table_registry, table,
                          "hash_table_register: malloc failed");
}

void hash_table_unregister(hash_table_data *table)
{
    ptr_registry_unregister(&hash_table_registry, table);
}

bool hash_table_is_registered(const hash_table_data *table)
{
    return ptr_registry_contains(hash_table_registry, table);
}

void bignum_register(bignum *bn)
{
    if (!bn)
        return;
    ptr_registry_register(&bignum_registry, bn,
                          "bignum_register: malloc failed");
}

void bignum_unregister(bignum *bn)
{
    ptr_registry_unregister(&bignum_registry, bn);
}

bool bignum_is_registered(const bignum *bn)
{
    return ptr_registry_contains(bignum_registry, bn);
}

void string_register(char *string)
{
    if (!string)
        return;
    ptr_registry_register(&string_registry, string,
                          "string_register: malloc failed");
}

void string_unregister(char *string)
{
    ptr_registry_unregister(&string_registry, string);
}

bool string_is_registered(const char *string)
{
    return ptr_registry_contains(string_registry, string);
}

void vector_register(vector_data *vector)
{
    if (!vector)
        return;
    sized_ptr_registry_register(&vector_registry, vector, vector->len,
                                "vector_register: malloc failed");
}

void vector_unregister(vector_data *vector)
{
    sized_ptr_registry_unregister(&vector_registry, vector);
}

bool vector_is_registered(const vector_data *vector)
{
    return sized_ptr_registry_find(vector_registry, vector) != NULL;
}

void bytevec_register(bytevec_data *bytevec)
{
    if (!bytevec)
        return;
    sized_ptr_registry_register(&bytevec_registry, bytevec, bytevec->len,
                                "bytevec_register: malloc failed");
}

void bytevec_unregister(bytevec_data *bytevec)
{
    sized_ptr_registry_unregister(&bytevec_registry, bytevec);
}

bool bytevec_is_registered(const bytevec_data *bytevec)
{
    return sized_ptr_registry_find(bytevec_registry, bytevec) != NULL;
}

void init_heap(void)
{
    setlocale(LC_CTYPE, "");
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

void set_alloc_gc_root(unsigned *root)
{
    alloc_gc_root = root;
}

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
        shadow_stack =
            checked_malloc_array((unsigned)shadow_stack_size, sizeof(unsigned *));
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
        if (shadow_stack_size > INT_MAX / 2 ||
            (size_t)shadow_stack_size * 2 > SIZE_MAX / sizeof(unsigned *)) {
            lisp_panic("shadow stack too large");
        }
        int new_size = shadow_stack_size * 2;
        unsigned **new_stack = checked_realloc_array(
            shadow_stack, (unsigned)new_size, sizeof(unsigned *));
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

int gc_mark(void)
{
    return shadow_stack_top;
}

int get_shadow_stack_top(void)
{
    return shadow_stack_top;
}

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
    bignum_register(bn);
    return p;
}

bignum *get_bignum(unsigned x)
{
    if (!IS_CELL(x) || CELL_TYPE(x) != BT_BIGNUM)
        return NULL;
    bignum *bn = (bignum *)CELL_PTR(x);
    return bignum_is_registered(bn) ? bn : NULL;
}

bignum *to_bignum(unsigned x)
{
    if (IS_FIXNUM(x)) {
        return bn_from_int(FIXNUM_VALUE(x));
    } else if (IS_NUM(x)) {
        return bn_from_int(CELL_ID(x));
    } else if (IS_BIGNUM(x)) {
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
        if (bn) {
            bignum_unregister(bn);
            bn_free(bn);
        }
        CELL_ID(x) = 0;
    }
}

bool hash_table_data_well_formed(const hash_table_data *ht)
{
    return hash_table_is_registered(ht) && ht->capacity > 0 && ht->buckets &&
           (ht->equiv == HASH_EQ || ht->equiv == HASH_EQV ||
            ht->equiv == HASH_EQUAL) &&
           ht->size <= ht->capacity;
}

static void free_external_cell_data(unsigned x)
{
    if (CELL_TYPE(x) == BT_BIGNUM) {
        free_bignum_cell(x);
    } else if (CELL_TYPE(x) == BT_HASHTABLE) {
        hash_table_data *ht = (hash_table_data *)CELL_PTR(x);
        if (ht) {
            if (hash_table_data_well_formed(ht)) {
                for (unsigned i = 0; i < ht->capacity; i++) {
                    hash_entry *entry = ht->buckets[i];
                    while (entry) {
                        hash_entry *next = entry->next;
                        free(entry);
                        entry = next;
                    }
                }
            }
            if (hash_table_is_registered(ht)) {
                hash_table_unregister(ht);
                free(ht->buckets);
                free(ht);
            }
        }
        CELL_PTR(x) = NULL;
    } else if (CELL_TYPE(x) == BT_VECTOR) {
        vector_data *vd = (vector_data *)CELL_PTR(x);
        if (vector_is_registered(vd)) {
            vector_unregister(vd);
            free(vd);
        }
        CELL_PTR(x) = NULL;
    } else if (CELL_TYPE(x) == BT_STRING) {
        char *s = (char *)CELL_PTR(x);
        if (string_is_registered(s)) {
            string_unregister(s);
            free(s);
        }
        CELL_PTR(x) = NULL;
    } else if (CELL_TYPE(x) == BT_BYTEVEC) {
        bytevec_data *bv = (bytevec_data *)CELL_PTR(x);
        if (bytevec_is_registered(bv)) {
            bytevec_unregister(bv);
            free(bv);
        }
        CELL_PTR(x) = NULL;
    } else if (CELL_TYPE(x) == BT_STRINPORT ||
               CELL_TYPE(x) == BT_STROUTPORT) {
        string_port *sp = (string_port *)CELL_PTR(x);
        if (string_port_is_registered(sp)) {
            string_port_unregister(sp);
            free(sp->data);
            free(sp);
        }
        CELL_PTR(x) = NULL;
    } else if (CELL_TYPE(x) == BT_INPORT || CELL_TYPE(x) == BT_OUTPORT) {
        file_port *port = (file_port *)CELL_PTR(x);
        if (file_port_is_registered(port)) {
            if (port->file && port->owns_file) {
                reader_forget_port(port->file);
                fclose(port->file);
            }
            file_port_unregister(port);
            free(port);
        }
        CELL_PTR(x) = NULL;
    } else if (CELL_TYPE(x) == BT_VMCONT) {
        if (CELL_PTR(x)) {
            vm_continuation_unregister((vm_continuation *)CELL_PTR(x));
            free(CELL_PTR(x));
        }
        CELL_PTR(x) = NULL;
    }
}

static void collect_vm_continuation_roots(vm_continuation *cont,
                                          unsigned (*collector)(unsigned))
{
    if (!vm_continuation_is_registered(cont))
        return;

    unsigned sp = (cont->stack && cont->sp <= VM_MAX_STACK_SIZE)
                      ? cont->sp
                      : 0;
    unsigned fp = (cont->frames && cont->fp <= VM_MAX_FRAMES_SIZE)
                      ? cont->fp
                      : 0;
    unsigned letrec_saved_len =
        (cont->letrec_saved && cont->letrec_saved_len <= VM_MAX_STACK_SIZE)
            ? cont->letrec_saved_len
            : 0;

    for (unsigned i = 0; i < sp; i++) {
        cont->stack[i] = collector(cont->stack[i]);
    }
    cont->env = collector(cont->env);
    for (unsigned i = 0; i < fp; i++) {
        cont->frames[i].env = collector(cont->frames[i].env);
    }
    cont->letrec_frame = collector(cont->letrec_frame);
    for (unsigned i = 0; i < letrec_saved_len; i++) {
        cont->letrec_saved[i] = collector(cont->letrec_saved[i]);
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
            if (!bn_num || !bn_denom) {
                bn_free(bn_num);
                bn_free(bn_denom);
                show_error("rational: out of memory");
                return TOK_ERROR;
            }
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
    if (IS_FIXNUM(x))
        return FIXNUM_VALUE(x) < 0;
    if (!IS_CELL(x))
        return false;
    switch (CELL_TYPE(x)) {
    case BT_NUM:
        return CELL_ID(x) < 0;
    case BT_BIGNUM: {
        bignum *bn = get_bignum(x);
        return bn && bn_sign(bn) < 0;
    }
    default:
        return false;
    }
}

unsigned negate_number(unsigned x)
{
    if (IS_FIXNUM(x)) {
        int32_t val = FIXNUM_VALUE(x);
        if (val == FIXNUM_MIN)
            return store(-(int64_t)val);
        return MAKE_FIXNUM(-val);
    }
    if (!IS_CELL(x))
        return x;
    switch (CELL_TYPE(x)) {
    case BT_NUM: {
        int64_t val = CELL_ID(x);
        // Handle INT64_MIN overflow
        if (val == INT64_MIN) {
            bignum *bn = bn_from_int(val);
            if (!bn) {
                show_error("negate_number: out of memory");
                return TOK_ERROR;
            }
            bn_neg_ip(bn);
            return store_integer(bn);
        }
        return store(-val);
    }
    case BT_BIGNUM: {
        bignum *orig = get_bignum(x);
        if (!orig)
            return x;
        bignum *bn = bn_neg(orig);
        if (!bn) {
            show_error("negate_number: out of memory");
            return TOK_ERROR;
        }
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
    if (!g) {
        bn_free(num);
        bn_free(denom);
        show_error("rational: out of memory");
        return TOK_ERROR;
    }
    bignum *reduced_num = bn_div(num, g, NULL);
    bignum *reduced_denom = bn_div(denom, g, NULL);
    bn_free(num);
    bn_free(denom);
    bn_free(g);
    if (!reduced_num || !reduced_denom) {
        bn_free(reduced_num);
        bn_free(reduced_denom);
        show_error("rational: out of memory");
        return TOK_ERROR;
    }

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
    gc_protect(&denom_stored);

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
    if (IS_FIXNUM(imag_part) && FIXNUM_VALUE(imag_part) == 0) {
        return real_part;
    }
    if (IS_NUM(imag_part) && CELL_ID(imag_part) == 0) {
        return real_part;
    }
    GC_PROTECT_GUARD2(&real_part, &imag_part);
    unsigned p = alloc();
    CELL_TYPE(p) = BT_COMPLEX;
    CELL_CAR(p) = real_part;
    CELL_CDR(p) = imag_part;
    return p;
}

static bool exact_integer_cell_well_formed(unsigned x)
{
    if (IS_FIXNUM(x) || IS_NUM(x))
        return true;
    if (IS_BIGNUM(x))
        return get_bignum(x) != NULL;
    return false;
}

static bool exact_integer_cell_is_zero(unsigned x)
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

static bool numeric_cell_well_formed(unsigned x, bool allow_complex,
                                     unsigned depth)
{
    if (depth == 0 || x == 0)
        return false;
    if (IS_FIXNUM(x))
        return true;
    if (!IS_CELL(x))
        return false;

    switch (CELL_TYPE(x)) {
    case BT_NUM:
    case BT_INEXACT:
        return true;
    case BT_BIGNUM:
        return get_bignum(x) != NULL;
    case BT_RATIONAL:
        return exact_integer_cell_well_formed(CELL_CAR(x)) &&
               exact_integer_cell_well_formed(CELL_CDR(x)) &&
               !exact_integer_cell_is_zero(CELL_CDR(x));
    case BT_COMPLEX:
        return allow_complex &&
               numeric_cell_well_formed(CELL_CAR(x), false, depth - 1) &&
               numeric_cell_well_formed(CELL_CDR(x), false, depth - 1);
    default:
        return false;
    }
}

double to_double(unsigned x)
{
    if (x == 0)
        return 0.0;
    if (IS_FIXNUM(x))
        return (double)FIXNUM_VALUE(x);
    if (!IS_CELL(x))
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
        if (!numeric_cell_well_formed(x, false, 4))
            return 0.0;
        double num = to_double(CELL_CAR(x));
        double denom = to_double(CELL_CDR(x));
        return num / denom;
    }
    case BT_COMPLEX:
        if (!numeric_cell_well_formed(x, true, 4))
            return 0.0;
        // For complex, just return the real part as double
        return to_double(CELL_CAR(x));
    default:
        return 0.0;
    }
}

bool is_numeric(unsigned x)
{
    return numeric_cell_well_formed(x, true, 4);
}

static unsigned parse_real_number_slice(const char *start, size_t len)
{
    char *slice = checked_string_copy_len(start, len);
    if (!slice) {
        show_error("out of memory");
        return TOK_ERROR;
    }
    unsigned parsed = atom_from_string(slice);
    free(slice);
    return is_numeric(parsed) && !IS_COMPLEX(parsed) ? parsed : TOK_ERROR;
}

bool is_exact(unsigned x)
{
    if (!is_numeric(x))
        return false;

    if (IS_FIXNUM(x))
        return true;

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

bool is_valid_unicode_scalar(int64_t code)
{
    return code >= 0 && code <= 0x10FFFF &&
           !(code >= 0xD800 && code <= 0xDFFF);
}

unsigned make_vector(unsigned len, unsigned fill)
{
    GC_GUARD;
    gc_protect(&fill);

    if (!checked_flex_size(sizeof(vector_data), len, sizeof(unsigned), NULL)) {
        show_error("make-vector: length too large");
        return TOK_ERROR;
    }
    vector_data *vd =
        checked_malloc_flex(sizeof(vector_data), len, sizeof(unsigned));
    if (!vd) {
        show_error("make-vector: out of memory");
        return TOK_ERROR;
    }
    vd->len = len;
    vector_register(vd);

    unsigned p = alloc();
    CELL_TYPE(p) = BT_VECTOR;
    CELL_PTR(p) = vd;
    for (unsigned i = 0; i < len; i++) {
        vd->data[i] = fill;
    }
    return p;
}

unsigned vector_len(unsigned vec)
{
    vector_data *vd = (vector_data *)CELL_PTR(vec);
    return vector_data_well_formed(vd) ? vd->len : 0;
}

unsigned *vector_data_ptr(unsigned vec)
{
    vector_data *vd = (vector_data *)CELL_PTR(vec);
    return vector_data_well_formed(vd) ? vd->data : NULL;
}

bool vector_data_well_formed(const vector_data *vd)
{
    const sized_ptr_registry_node *node =
        sized_ptr_registry_find(vector_registry, vd);
    if (!node)
        return false;
    return node->len == vd->len &&
           checked_flex_size(sizeof(vector_data), vd->len, sizeof(unsigned),
                             NULL);
}

bool bytevec_data_well_formed(const bytevec_data *bv)
{
    const sized_ptr_registry_node *node =
        sized_ptr_registry_find(bytevec_registry, bv);
    if (!node)
        return false;
    return node->len == bv->len &&
           checked_flex_size(sizeof(bytevec_data), bv->len, 1, NULL);
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

unsigned make_halt_cont(void)
{
    return make_cont(CONT_HALT, 0, 0, 0);
}

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

int hash_function(const char *s)
{
    return hash_with_cap(s, ctx.atom_table_cap);
}

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
    for (unsigned i = 3; i <= n / i; i += 2) {
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
    if (old_cap > UINT_MAX / 2) {
        fprintf(stderr, "Warning: atom table too large to resize\n");
        return;
    }
    unsigned new_cap = next_prime(old_cap * 2);

    const char **new_table = checked_calloc_array(new_cap, sizeof(const char *));
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
        ctx.atom_table[hash_value] = checked_string_copy(s);
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
        GC_GUARD;
        unsigned real_part = store(0);
        gc_protect(&real_part);
        unsigned imag_part = store(1);
        return store_complex(real_part, imag_part);
    }
    if (strcmp(s, "-i") == 0) {
        GC_GUARD;
        unsigned real_part = store(0);
        gc_protect(&real_part);
        unsigned imag_part = store(-1);
        return store_complex(real_part, imag_part);
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
                unsigned real_part =
                    parse_real_number_slice(s, (size_t)(sep - s));
                if (real_part == TOK_ERROR)
                    goto not_rational;

                unsigned imag_part;
                {
                    GC_PROTECT_GUARD;
                    gc_protect(&real_part);
                    if ((sep[1] == 'i' || sep[1] == 'I') && sep[2] == '\0') {
                        imag_part = store(*sep == '-' ? -1 : 1);
                    } else {
                        imag_part =
                            parse_real_number_slice(
                                sep, (size_t)(s + n - 1 - sep));
                        if (imag_part == TOK_ERROR)
                            goto not_rational;
                    }
                }

                return store_complex(real_part, imag_part);
            }
            // Pure imaginary: 5i, 3.14i, 3/4i, etc.
            unsigned imag_part = parse_real_number_slice(s, n - 1);
            if (imag_part != TOK_ERROR) {
                GC_GUARD;
                unsigned real_part = store(0);
                gc_protect(&real_part);
                return store_complex(real_part, imag_part);
            }
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
                    char *num_str =
                        checked_string_copy_len(s, (size_t)(slash - s));
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
                char *num_str =
                    checked_string_copy_len(s, (size_t)(slash - s));
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
        {
            double dval = strtod(s, &endptr);
            if ((size_t)(endptr - s) == n) {
                return store_inexact(dval);
            }
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
    while (IS_PAIR(lst)) {
        len++;
        lst = cdr(lst);
    }
    return len;
}

bool list_length_checked(unsigned lst, unsigned *len_out, const char *name)
{
    unsigned slow = lst;
    unsigned fast = lst;
    while (IS_PAIR(fast)) {
        fast = cdr(fast);
        if (!IS_PAIR(fast))
            break;
        fast = cdr(fast);
        slow = cdr(slow);
        if (slow == fast) {
            show_error("%s: circular list", name);
            return false;
        }
    }
    if (fast) {
        show_error("%s: improper list", name);
        return false;
    }

    unsigned len = 0;
    for (unsigned it = lst; it; it = cdr(it)) {
        len++;
    }
    if (len_out)
        *len_out = len;
    return true;
}

bool proper_list_silent(unsigned x)
{
    while (IS_PAIR(x))
        x = cdr(x);
    return x == 0;
}

bool let_binding_has_value(unsigned binding)
{
    return IS_PAIR(binding) && IS_PAIR(cdr(binding)) && !cddr(binding);
}

bool binding_list_binds_id(unsigned binding_list, int64_t id)
{
    for (unsigned bl = binding_list; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (let_binding_has_value(binding) && IS_ATOM(car(binding)) &&
            CELL_ID(car(binding)) == id)
            return true;
    }
    return false;
}

bool lambda_params_bind_id(unsigned params, int64_t id)
{
    for (unsigned p = params; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        unsigned param = IS_PAIR(p) ? car(p) : p;
        if (IS_ATOM(param) && CELL_ID(param) == id)
            return true;
        if (!IS_PAIR(p))
            break;
    }
    return false;
}

bool atom_list_contains_id(unsigned list, int64_t id)
{
    for (; IS_PAIR(list); list = cdr(list)) {
        if (IS_ATOM(car(list)) && CELL_ID(car(list)) == id)
            return true;
    }
    return false;
}

bool syntax_is_ellipsis(unsigned x, int64_t ellipsis_id)
{
    if (!ellipsis_id)
        return false;
    return IS_ATOM(x) && CELL_ID(x) == ellipsis_id;
}

bool syntax_is_underscore(unsigned x)
{
    return IS_KEYWORD(x, ctx.kw_underscore);
}

bool is_eof_object(unsigned expr)
{
    return IS_ATOM(expr) &&
           strcmp(ctx.atom_table[CELL_ID(expr)], "eof-object") == 0;
}

bool syntax_rules_form_like(unsigned expr)
{
    return IS_PAIR(expr) && IS_ATOM(car(expr)) &&
           CELL_ID(car(expr)) == ctx.kw_syntax_rules &&
           IS_PAIR(cdr(expr)) && IS_PAIR(cddr(expr));
}

bool assoc_list_has_atom_key_id(unsigned assoc_list, int64_t id)
{
    for (unsigned p = assoc_list; p; p = cdr(p)) {
        unsigned entry = car(p);
        if (IS_PAIR(entry) && IS_ATOM(car(entry)) && CELL_ID(car(entry)) == id)
            return true;
    }
    return false;
}

bool feature_id_available(int64_t id)
{
    if (id < 0 || (unsigned)id >= ctx.atom_table_cap || !ctx.atom_table[id])
        return false;
    const char *const *features = feature_names();
    size_t feature_count = feature_name_count();
    for (size_t i = 0; i < feature_count; i++) {
        if (strcmp(ctx.atom_table[id], features[i]) == 0)
            return true;
    }
    return false;
}

bool cond_expand_requirement_satisfied(unsigned requirement)
{
    if (!requirement)
        return true;
    if (IS_ATOM(requirement))
        return feature_id_available(CELL_ID(requirement));
    if (!IS_PAIR(requirement) || !IS_ATOM(car(requirement)))
        return false;

    int64_t op = CELL_ID(car(requirement));
    unsigned args = cdr(requirement);
    if (op == ctx.kw_and_feature) {
        for (; args; args = cdr(args)) {
            if (!IS_PAIR(args))
                return false;
            if (!cond_expand_requirement_satisfied(car(args)))
                return false;
        }
        return true;
    }
    if (op == ctx.kw_or_feature) {
        for (; args; args = cdr(args)) {
            if (!IS_PAIR(args))
                return false;
            if (cond_expand_requirement_satisfied(car(args)))
                return true;
        }
        return false;
    }
    if (op == ctx.kw_not_feature) {
        return IS_PAIR(args) && !cdr(args) &&
               !cond_expand_requirement_satisfied(car(args));
    }
    return false;
}

bool syntax_arity_checked(unsigned form, unsigned min_args, unsigned max_args,
                          const char *name)
{
    unsigned len = 0;
    if (!list_length_checked(cdr(form), &len, name))
        return false;
    if (len < min_args || (max_args != UINT_MAX && len > max_args)) {
        show_error("%s: invalid syntax", name);
        return false;
    }
    return true;
}

bool identifier_valid(unsigned x)
{
    return IS_ATOM(x) && x != ctx.atom_true && x != ctx.atom_false;
}

bool lambda_params_valid(unsigned params)
{
    GC_GUARD;
    if (!params)
        return true;
    if (identifier_valid(params))
        return true;
    if (!IS_PAIR(params))
        return false;

    unsigned seen = 0;
    gc_protect(&seen);
    while (params) {
        if (identifier_valid(params)) {
            FORLIST(p, seen) {
                if (CELL_ID(car(p)) == CELL_ID(params)) {
                    gc_unprotect(1);
                    return false;
                }
            }
            gc_unprotect(1);
            return true;
        }
        if (IS_ATOM(params)) {
            gc_unprotect(1);
            return false;
        }
        if (!IS_PAIR(params)) {
            gc_unprotect(1);
            return false;
        }
        unsigned param = car(params);
        if (!identifier_valid(param)) {
            gc_unprotect(1);
            return false;
        }
        FORLIST(p, seen) {
            if (CELL_ID(car(p)) == CELL_ID(param)) {
                gc_unprotect(1);
                return false;
            }
        }
        seen = alloc_cons(param, seen);
        params = cdr(params);
    }
    gc_unprotect(1);
    return true;
}

bool binding_list_valid(unsigned bindings, const char *name)
{
    if (!list_length_checked(bindings, NULL, name))
        return false;
    FORLIST(b, bindings) {
        unsigned binding = car(b);
        unsigned len = 0;
        if (!IS_PAIR(binding) ||
            !list_length_checked(binding, &len, name) || len != 2 ||
            !identifier_valid(car(binding))) {
            show_error("%s: invalid binding syntax", name);
            return false;
        }
        if (strcmp(name, "let*") != 0) {
            int64_t id = CELL_ID(car(binding));
            for (unsigned rest = cdr(b); rest; rest = cdr(rest)) {
                unsigned other = car(rest);
                if (IS_PAIR(other) && IS_ATOM(car(other)) &&
                    CELL_ID(car(other)) == id) {
                    show_error("%s: duplicate binding", name);
                    return false;
                }
            }
        }
    }
    return true;
}

bool cond_clauses_valid(unsigned clauses, const char *name, unsigned env)
{
    if (!list_length_checked(clauses, NULL, name))
        return false;
    bool else_unbound = env_find_binding_cell(ctx.kw_else, env) == 0;
    bool arrow_unbound = env_find_binding_cell(ctx.kw_arrow, env) == 0;
    FORLIST(c, clauses) {
        unsigned clause = car(c);
        unsigned len = 0;
        if (!IS_PAIR(clause) ||
            !list_length_checked(clause, &len, name) || len == 0) {
            show_error("%s: invalid clause syntax", name);
            return false;
        }
        unsigned test = car(clause);
        unsigned conseq = cdr(clause);
        if (else_unbound && IS_KEYWORD(test, ctx.kw_else) && cdr(c)) {
            show_error("%s: else clause must be last", name);
            return false;
        }
        if (arrow_unbound && conseq && IS_KEYWORD(car(conseq), ctx.kw_arrow) &&
            len != 3) {
            show_error("%s: invalid => clause", name);
            return false;
        }
    }
    return true;
}

bool check_args(unsigned args, unsigned min, unsigned max, const char *name)
{
    // Early-exit optimization: only count as many args as needed to decide.
    // For variadic (max=-1), stop after min. For fixed-arity, stop after max+1.
    unsigned limit = (max == (unsigned)-1) ? min : max + 1;
    unsigned len = 0;
    unsigned a = args;

    while (IS_PAIR(a) && len < limit) {
        len++;
        a = cdr(a);
    }

    if (len < min) {
        show_error("%s: too few arguments (expected %u, got %u)", name, min,
                   len);
        return false;
    }

    // If max is bounded and we counted more than max, or there are still more
    if (max != (unsigned)-1 && (len > max || IS_PAIR(a))) {
        show_error("%s: too many arguments (expected at most %u)", name, max);
        return false;
    }

    return true;
}

void list_append(unsigned *head, unsigned *tail, unsigned elem)
{
    // Protect in-progress list roots and elem; alloc() can trigger GC.
    unsigned cell;
    {
        GC_PROTECT_GUARD;
        gc_protect(head);
        gc_protect(tail);
        gc_protect(&elem);
        cell = alloc();
        CELL_TYPE(cell) = BT_CONS;
        CELL_CAR(cell) = elem; // elem is now up-to-date after any GC
        CELL_CDR(cell) = 0;
    }
    if (!*head) {
        *head = *tail = cell;
    } else {
        cell_set_cdr(*tail, cell);
        *tail = cell;
    }
}

void *checked_malloc_array(unsigned count, size_t elem_size)
{
    size_t size;
    if (!checked_array_size(count, elem_size, &size))
        return NULL;
    return malloc(size);
}

void *checked_calloc_array(unsigned count, size_t elem_size)
{
    size_t size;
    if (!checked_array_size(count, elem_size, &size))
        return NULL;
    return calloc(size, 1);
}

void *checked_realloc_array(void *ptr, unsigned count, size_t elem_size)
{
    size_t size;
    if (!checked_array_size(count, elem_size, &size))
        return NULL;
    return realloc(ptr, size);
}

void *checked_malloc_size(size_t size)
{
    return malloc(size);
}

void *checked_realloc_size(void *ptr, size_t size)
{
    return realloc(ptr, size);
}

bool checked_flex_size(size_t base_size, size_t count, size_t elem_size,
                       size_t *size_out)
{
    if (elem_size != 0 && count > (SIZE_MAX - base_size) / elem_size)
        return false;
    if (size_out)
        *size_out = base_size + count * elem_size;
    return true;
}

void *checked_malloc_flex(size_t base_size, size_t count, size_t elem_size)
{
    size_t size;
    if (!checked_flex_size(base_size, count, elem_size, &size))
        return NULL;
    return malloc(size);
}

char *checked_string_copy_len(const char *s, size_t len)
{
    if (len == SIZE_MAX)
        return NULL;
    char *copy = malloc(len + 1);
    if (!copy)
        return NULL;
    memcpy(copy, s, len);
    copy[len] = '\0';
    return copy;
}

char *checked_string_copy(const char *s)
{
    return checked_string_copy_len(s, strlen(s));
}

bool checked_array_size(unsigned count, size_t elem_size, size_t *size_out)
{
    if (elem_size != 0 && (size_t)count > SIZE_MAX / elem_size)
        return false;
    *size_out = (size_t)count * elem_size;
    return true;
}

bool checked_add_size(size_t a, size_t b, size_t *out)
{
    if (a > SIZE_MAX - b)
        return false;
    *out = a + b;
    return true;
}

unsigned checked_grow_capacity(unsigned cap, size_t elem_size,
                               const char *error_msg)
{
    if (cap == 0)
        return 1;
    if (cap > UINT_MAX / 2) {
        lisp_panic(error_msg);
    }
    unsigned new_cap = cap * 2;
    if (elem_size != 0 && (size_t)new_cap > SIZE_MAX / elem_size) {
        lisp_panic(error_msg);
    }
    return new_cap;
}

bool checked_grow_capacity_size(size_t cap, size_t elem_size,
                                size_t *new_cap_out)
{
    if (cap == 0) {
        *new_cap_out = 1;
        return true;
    }
    if (cap > SIZE_MAX / 2)
        return false;
    size_t new_cap = cap * 2;
    if (elem_size != 0 && new_cap > SIZE_MAX / elem_size)
        return false;
    *new_cap_out = new_cap;
    return true;
}

static unsigned make_multival(unsigned values)
{
    GC_GUARD;
    gc_protect(&values);
    unsigned mv = alloc();
    CELL_TYPE(mv) = BT_MULTIVAL;
    CELL_CAR(mv) = values;
    CELL_CDR(mv) = 0;
    return mv;
}

unsigned values_from_list(unsigned args)
{
    if (args && !cdr(args))
        return car(args);
    return make_multival(args);
}

unsigned values_from_argv(unsigned argc, unsigned *argv)
{
    LISP_ASSERT_MSG(argc == 0 || argv != NULL, "values_from_argv: null argv");

    if (argc == 1)
        return argv[0];

    GC_GUARD;
    for (unsigned i = 0; i < argc; i++)
        gc_protect(&argv[i]);

    unsigned values = 0;
    gc_protect(&values);
    for (unsigned i = argc; i > 0; i--) {
        values = alloc_cons(argv[i - 1], values);
    }

    return make_multival(values);
}

typedef struct {
    unsigned a;
    unsigned b;
} equal_seen_pair;

typedef struct {
    equal_seen_pair *items;
    size_t len;
    size_t cap;
} equal_seen_set;

static bool deep_equal_seen(unsigned a, unsigned b, equal_seen_set *seen);

static bool equal_seen_contains(equal_seen_set *seen, unsigned a, unsigned b)
{
    for (size_t i = 0; i < seen->len; i++) {
        if (seen->items[i].a == a && seen->items[i].b == b)
            return true;
    }
    return false;
}

static bool equal_seen_add(equal_seen_set *seen, unsigned a, unsigned b)
{
    if (seen->len == seen->cap) {
        size_t new_cap;
        if (seen->cap == 0) {
            new_cap = 16;
        } else if (!checked_grow_capacity_size(seen->cap,
                                               sizeof(*seen->items),
                                               &new_cap)) {
            return false;
        }
        size_t alloc_size;
        if (!checked_flex_size(0, new_cap, sizeof(*seen->items),
                               &alloc_size))
            return false;
        equal_seen_pair *new_items =
            checked_realloc_size(seen->items, alloc_size);
        if (!new_items)
            return false;
        seen->items = new_items;
        seen->cap = new_cap;
    }
    seen->items[seen->len++] = (equal_seen_pair){a, b};
    return true;
}

static bool deep_equal_seen(unsigned a, unsigned b, equal_seen_set *seen)
{
    if (a == b)
        return true;
    if (a == 0 || b == 0)
        return a == b;
    if (IS_FIXNUM(a)) {
        return IS_NUM(b) && CELL_ID(b) == (int64_t)FIXNUM_VALUE(a);
    }
    if (IS_FIXNUM(b)) {
        return IS_NUM(a) && CELL_ID(a) == (int64_t)FIXNUM_VALUE(b);
    }
    if (!IS_CELL(a) || !IS_CELL(b))
        return false;
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
        return ba && bb && bn_cmp(ba, bb) == 0;
    }
    case BT_RATIONAL:
    case BT_COMPLEX:
        if (equal_seen_contains(seen, a, b))
            return true;
        if (!equal_seen_add(seen, a, b))
            return false;
        return deep_equal_seen(car(a), car(b), seen) &&
               deep_equal_seen(cdr(a), cdr(b), seen);
    case BT_STRING: {
        char *sa = GET_STRING_PTR(a);
        char *sb = GET_STRING_PTR(b);
        return string_is_registered(sa) && string_is_registered(sb) &&
               strcmp(sa, sb) == 0;
    }
    case BT_BYTEVEC: {
        bytevec_data *ba = (bytevec_data *)CELL_PTR(a);
        bytevec_data *bb = (bytevec_data *)CELL_PTR(b);
        if (!bytevec_data_well_formed(ba) || !bytevec_data_well_formed(bb))
            return false;
        return ba->len == bb->len &&
               memcmp(ba->data, bb->data, ba->len) == 0;
    }
    case BT_CONS:
        if (equal_seen_contains(seen, a, b))
            return true;
        if (!equal_seen_add(seen, a, b))
            return false;
        return deep_equal_seen(car(a), car(b), seen) &&
               deep_equal_seen(cdr(a), cdr(b), seen);
    case BT_VECTOR: {
        unsigned len_a = vector_len(a);
        unsigned len_b = vector_len(b);
        if (len_a != len_b)
            return false;
        if (equal_seen_contains(seen, a, b))
            return true;
        if (!equal_seen_add(seen, a, b))
            return false;
        unsigned *da = vector_data_ptr(a);
        unsigned *db = vector_data_ptr(b);
        if ((len_a != 0 && !da) || (len_b != 0 && !db))
            return false;
        for (unsigned i = 0; i < len_a; i++) {
            if (!deep_equal_seen(da[i], db[i], seen))
                return false;
        }
        return true;
    }
    default:
        return false;
    }
}

bool deep_equal(unsigned a, unsigned b)
{
    equal_seen_set seen = {0};
    bool result = deep_equal_seen(a, b, &seen);
    free(seen.items);
    return result;
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
    case BT_BYTEVEC:
    case BT_HASHTABLE: {
        // Pointer-based types: share the malloc'd data between old and new cells.
        // The old region is swept after collection, freeing unreachable data.
        unsigned xx = alloc();
        CELL_TYPE(xx) = CELL_TYPE(x);
        CELL_ID(xx) = CELL_ID(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        if (CELL_TYPE(xx) == BT_HASHTABLE) {
            hash_table_data *ht = (hash_table_data *)CELL_PTR(xx);
            if (!hash_table_data_well_formed(ht))
                return xx;
            for (unsigned i = 0; i < ht->capacity; i++) {
                for (hash_entry *entry = ht->buckets[i]; entry;
                     entry = entry->next) {
                    entry->key = collect(entry->key);
                    entry->value = collect(entry->value);
                }
            }
        }
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
        if (!vector_data_well_formed(vd))
            return xx;
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

    case BT_INPORT:
    case BT_OUTPORT: {
        unsigned xx = alloc();
        CELL_TYPE(xx) = CELL_TYPE(x);
        CELL_PTR(xx) = CELL_PTR(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        return xx;
    }

    case BT_VMCONT: {
        // VM continuation contains cell references that must be updated.
        unsigned xx = alloc();
        CELL_TYPE(xx) = BT_VMCONT;
        CELL_PTR(xx) = CELL_PTR(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        if (is_vm_continuation_object(xx))
            collect_vm_continuation_roots((vm_continuation *)CELL_PTR(xx),
                                          collect);
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

    // Copy initial root to new space using the same type-aware path as all
    // other roots. Direct roots may be pointer-bearing cells such as vectors.
    root = collect(root);

    // Collect trampoline state (global evaluator state)
    tramp.expr = collect(tramp.expr);
    tramp.env = collect(tramp.env);
    tramp.cont = collect(tramp.cont);
    tramp.value = collect(tramp.value);
    ctx.current_input_cell = collect(ctx.current_input_cell);
    ctx.current_output_cell = collect(ctx.current_output_cell);
    ctx.current_error_cell = collect(ctx.current_error_cell);
    reader_update_datum_labels(collect);

    // Collect shadow stack entries - these are pointers to local C variables
    // that hold cell IDs. After GC, we update them to point to new locations.
    for (int i = 0; i < shadow_stack_top; i++) {
        if (shadow_stack[i] && *shadow_stack[i] >= HEAP_RESERVED) {
            *shadow_stack[i] = collect(*shadow_stack[i]);
        }
    }
    gc_update_active_pattern_state();

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
            t == BT_COMPLEX || t == BT_BINDING_REF) {
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
        free_external_cell_data(i);
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

// Collect a nursery cell to old generation
unsigned collect_to_old(unsigned x)
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
    case BT_BYTEVEC:
    case BT_HASHTABLE: {
        // Copy pointer-based cell to old gen (share malloc'd data)
        if (ctx.hptr >= ctx.nursery_start) {
            lisp_panic("old generation full during minor GC");
        }
        unsigned xx = ctx.hptr++;
        CELL_TYPE(xx) = CELL_TYPE(x);
        CELL_ID(xx) = CELL_ID(x);
        CELL_TYPE(x) = BT_BROKENHEART;
        CELL_CAR(x) = xx;
        if (CELL_TYPE(xx) == BT_HASHTABLE) {
            hash_table_data *ht = (hash_table_data *)CELL_PTR(xx);
            if (!hash_table_data_well_formed(ht))
                return xx;
            for (unsigned i = 0; i < ht->capacity; i++) {
                for (hash_entry *entry = ht->buckets[i]; entry;
                     entry = entry->next) {
                    entry->key = collect_to_old(entry->key);
                    entry->value = collect_to_old(entry->value);
                }
            }
        }
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
        if (!vector_data_well_formed(vd))
            return xx;
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

    case BT_INPORT:
    case BT_OUTPORT: {
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
        if (is_vm_continuation_object(xx))
            collect_vm_continuation_roots((vm_continuation *)CELL_PTR(xx),
                                          collect_to_old);
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
    ctx.current_input_cell = collect_to_old(ctx.current_input_cell);
    ctx.current_output_cell = collect_to_old(ctx.current_output_cell);
    ctx.current_error_cell = collect_to_old(ctx.current_error_cell);
    reader_update_datum_labels(collect_to_old);

    // Collect shadow stack entries
    for (int i = 0; i < shadow_stack_top; i++) {
        if (shadow_stack[i]) {
            *shadow_stack[i] = collect_to_old(*shadow_stack[i]);
        }
    }
    minor_gc_update_active_pattern_state();

    // Update VM roots if VM is active
    gc_update_vm_roots_minor(get_active_vm(), collect_to_old);

    // Update code object constants - they may point to nursery cells
    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        for (unsigned i = 0; i < code->const_len; i++) {
            code->constants[i] = collect_to_old(code->constants[i]);
        }
    }
    minor_gc_update_all_patterns();

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
                    t == BT_COMPLEX || t == BT_BINDING_REF) {
                    // CPS continuations (BT_CONT) use CAR/CDR for structure
                    CELL_CAR(cell) = collect_to_old(CELL_CAR(cell));
                    CELL_CDR(cell) = collect_to_old(CELL_CDR(cell));
                } else if (t == BT_VECTOR) {
                    vector_data *vd = (vector_data *)CELL_PTR(cell);
                    if (!vector_data_well_formed(vd))
                        continue;
                    for (unsigned j = 0; j < vd->len; j++) {
                        vd->data[j] = collect_to_old(vd->data[j]);
                    }
                } else if (t == BT_HASHTABLE) {
                    hash_table_data *ht = (hash_table_data *)CELL_PTR(cell);
                    if (!hash_table_data_well_formed(ht))
                        continue;
                    for (unsigned j = 0; j < ht->capacity; j++) {
                        for (hash_entry *entry = ht->buckets[j]; entry;
                             entry = entry->next) {
                            entry->key = collect_to_old(entry->key);
                            entry->value = collect_to_old(entry->value);
                        }
                    }
                } else if (t == BT_VMCONT) {
                    // VM continuation: update cell references inside struct
                    collect_vm_continuation_roots(
                        (vm_continuation *)CELL_PTR(cell), collect_to_old);
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
            t == BT_COMPLEX || t == BT_BINDING_REF) {
            // CPS continuations (BT_CONT) use CAR/CDR for structure
            CELL_CAR(scan) = collect_to_old(CELL_CAR(scan));
            CELL_CDR(scan) = collect_to_old(CELL_CDR(scan));
        } else if (t == BT_VMCONT) {
            // VM continuation's internal values were already collected when
            // the cell was promoted - no need to scan again
        }
        scan++;
    }

    // Free external data for unreachable nursery cells. Promoted cells were
    // rewritten as BT_BROKENHEART, so they are skipped here.
    unsigned nursery_end =
        (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
    for (unsigned i = ctx.nursery_start; i < nursery_end; i++) {
        free_external_cell_data(i);
        CELL_TYPE(i) = BT_FREE;
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
    ctx.current_error = stderr;
    ctx.current_input_cell = 0;
    ctx.current_output_cell = 0;
    ctx.current_error_cell = 0;
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
    ctx.kw_cond_expand = intern("cond-expand");
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
    ctx.kw_and_feature = intern("and");
    ctx.kw_or_feature = intern("or");
    ctx.kw_not_feature = intern("not");
    ctx.kw_arrow = intern("=>");
    ctx.kw_let_syntax = intern("let-syntax");
    ctx.kw_letrec_syntax = intern("letrec-syntax");
    ctx.kw_protected = intern("##protected##");
}
