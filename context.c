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
#include "primitives.h"
#include "feature_table.h"
#include "reader.h"
#include "utf8.h"
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <locale.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include <strings.h>

// ============================================================================
// FNV-1a Hash Constants (64-bit)
// ============================================================================

#define FNV_OFFSET_BASIS 14695981039346656037ull
#define FNV_PRIME 1099511628211ull
#define MAX_COND_EXPAND_REQUIREMENT_DEPTH 1024

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

// See enum core_form and context.h.
uint8_t *atom_core_forms = NULL; // Allocated in init_heap
int64_t core_form_keywords[CF_COUNT];
bool core_alias_bound = false;

// Registries below track live heap-allocated external objects (vectors,
// bignums, ports, ...) so a raw pointer stashed in a cell can be validated
// before use (defends against use-after-free / corrupted cells). They are
// hash tables keyed by pointer value, not linked lists: with potentially
// tens of thousands of live objects of a given kind, a linear scan per
// register/unregister/lookup call - and these run on essentially every
// vector/bignum/port operation, not just allocation - would make each call
// O(n), i.e. O(n^2) total for a loop that touches n objects.
typedef struct ptr_registry_node {
    void *ptr;
    unsigned len; // unused (0) for registries that don't need a size
    struct ptr_registry_node *next;
} ptr_registry_node;

typedef struct ptr_registry {
    ptr_registry_node **buckets;
    size_t bucket_count;
    size_t count;
} ptr_registry;

static ptr_registry vm_continuation_registry;
static ptr_registry file_port_registry;
static ptr_registry string_port_registry;
static ptr_registry hash_table_registry;
static ptr_registry bignum_registry;
static ptr_registry string_registry;
static ptr_registry immutable_string_registry;
static ptr_registry string_view_registry;
static ptr_registry vector_registry;
static ptr_registry bytevec_registry;
// Object-hash numbers of printed records, keyed by payload (node->len holds
// the number). Keying by payload rather than cell keeps a number stable while
// the collector moves the cell, and the entry dies with the payload.
static ptr_registry object_hash_registry;

// Error recovery for fatal errors
jmp_buf panic_jmp;
bool panic_jmp_set = false;

#define PTR_REGISTRY_INITIAL_BUCKETS 64

// Every vector, record and string payload registers a node on allocation and
// drops it when swept - a malloc and a free per short-lived record, on top of
// the payload's own. Keep a bounded stock of released nodes instead, large
// enough to cover what a minor collection typically reclaims.
#define SPARE_REGISTRY_NODES_MAX 65536
static ptr_registry_node *spare_registry_nodes;
static unsigned spare_registry_node_count;

static void release_registry_node(ptr_registry_node *node)
{
    if (spare_registry_node_count < SPARE_REGISTRY_NODES_MAX) {
        node->ptr = NULL;
        node->next = spare_registry_nodes;
        spare_registry_nodes = node;
        spare_registry_node_count++;
    } else {
        free(node);
    }
}

static size_t ptr_registry_hash(const void *ptr, size_t bucket_count)
{
    // Pointers from malloc are at least a few bits aligned, so shift those
    // constant low bits out before spreading the rest (Fibonacci hashing).
    uintptr_t v = (uintptr_t)ptr >> 4;
    v *= 0x9E3779B97F4A7C15ULL;
    return (size_t)(v & (bucket_count - 1));
}

static void ptr_registry_rehash(ptr_registry *registry, size_t new_count)
{
    ptr_registry_node **new_buckets =
        checked_malloc_size(sizeof(ptr_registry_node *) * new_count);
    LISP_ASSERT_MSG(new_buckets != NULL, "ptr_registry: rehash malloc failed");
    for (size_t i = 0; i < new_count; i++)
        new_buckets[i] = NULL;
    for (size_t i = 0; i < registry->bucket_count; i++) {
        ptr_registry_node *node = registry->buckets[i];
        while (node) {
            ptr_registry_node *next = node->next;
            size_t idx = ptr_registry_hash(node->ptr, new_count);
            node->next = new_buckets[idx];
            new_buckets[idx] = node;
            node = next;
        }
    }
    free(registry->buckets);
    registry->buckets = new_buckets;
    registry->bucket_count = new_count;
}

static void ptr_registry_register(ptr_registry *registry, void *ptr,
                                  unsigned len, const char *malloc_error)
{
    if (!ptr)
        return;
    if (registry->buckets) {
        size_t idx = ptr_registry_hash(ptr, registry->bucket_count);
        for (ptr_registry_node *node = registry->buckets[idx]; node;
             node = node->next) {
            if (node->ptr == ptr)
                return;
        }
    }

    if (!registry->buckets) {
        ptr_registry_rehash(registry, PTR_REGISTRY_INITIAL_BUCKETS);
    } else if (registry->count + 1 > registry->bucket_count - registry->bucket_count / 4) {
        ptr_registry_rehash(registry, registry->bucket_count * 2);
    }

    ptr_registry_node *node = spare_registry_nodes;
    if (node) {
        spare_registry_nodes = node->next;
        spare_registry_node_count--;
    } else {
        node = checked_malloc_size(sizeof(*node));
        LISP_ASSERT_MSG(node != NULL, malloc_error);
    }
    node->ptr = ptr;
    node->len = len;
    size_t idx = ptr_registry_hash(ptr, registry->bucket_count);
    node->next = registry->buckets[idx];
    registry->buckets[idx] = node;
    registry->count++;
}

static void ptr_registry_unregister(ptr_registry *registry, void *ptr)
{
    if (!ptr || !registry->buckets)
        return;
    size_t idx = ptr_registry_hash(ptr, registry->bucket_count);
    ptr_registry_node **prev = &registry->buckets[idx];
    while (*prev) {
        ptr_registry_node *node = *prev;
        if (node->ptr == ptr) {
            *prev = node->next;
            release_registry_node(node);
            registry->count--;
            return;
        }
        prev = &node->next;
    }
}

static const ptr_registry_node *ptr_registry_find(const ptr_registry *registry,
                                                  const void *ptr)
{
    if (!ptr || !registry->buckets)
        return NULL;
    size_t idx = ptr_registry_hash(ptr, registry->bucket_count);
    for (const ptr_registry_node *node = registry->buckets[idx]; node;
         node = node->next) {
        if (node->ptr == ptr)
            return node;
    }
    return NULL;
}

static bool ptr_registry_contains(const ptr_registry *registry,
                                  const void *ptr)
{
    return ptr_registry_find(registry, ptr) != NULL;
}

void vm_continuation_register(vm_continuation *cont)
{
    if (!cont)
        return;
    ptr_registry_register(&vm_continuation_registry, cont, 0,
                          "vm_continuation_register: malloc failed");
}

void vm_continuation_unregister(vm_continuation *cont)
{
    ptr_registry_unregister(&vm_continuation_registry, cont);
}

bool vm_continuation_is_registered(const vm_continuation *cont)
{
    return ptr_registry_contains(&vm_continuation_registry, cont);
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
    ptr_registry_register(&file_port_registry, port, 0,
                          "file_port_register: malloc failed");
}

void file_port_unregister(file_port *port)
{
    ptr_registry_unregister(&file_port_registry, port);
}

bool file_port_is_registered(const file_port *port)
{
    return ptr_registry_contains(&file_port_registry, port);
}

bool file_port_is_binary_file(FILE *file)
{
    if (!file || !file_port_registry.buckets)
        return false;
    for (size_t i = 0; i < file_port_registry.bucket_count; i++) {
        for (ptr_registry_node *node = file_port_registry.buckets[i]; node;
             node = node->next) {
            file_port *port = (file_port *)node->ptr;
            if (port->file == file)
                return port->binary;
        }
    }
    return false;
}

void string_port_register(string_port *port)
{
    if (!port)
        return;
    ptr_registry_register(&string_port_registry, port, 0,
                          "string_port_register: malloc failed");
}

void string_port_unregister(string_port *port)
{
    ptr_registry_unregister(&string_port_registry, port);
}

bool string_port_is_registered(const string_port *port)
{
    return ptr_registry_contains(&string_port_registry, port);
}

void hash_table_register(hash_table_data *table)
{
    if (!table)
        return;
    ptr_registry_register(&hash_table_registry, table, 0,
                          "hash_table_register: malloc failed");
}

void hash_table_unregister(hash_table_data *table)
{
    ptr_registry_unregister(&hash_table_registry, table);
}

bool hash_table_is_registered(const hash_table_data *table)
{
    return ptr_registry_contains(&hash_table_registry, table);
}

void bignum_register(bignum *bn)
{
    if (!bn)
        return;
    ptr_registry_register(&bignum_registry, bn, 0,
                          "bignum_register: malloc failed");
}

void bignum_unregister(bignum *bn)
{
    ptr_registry_unregister(&bignum_registry, bn);
}

bool bignum_is_registered(const bignum *bn)
{
    return ptr_registry_contains(&bignum_registry, bn);
}

// Bumped whenever a string buffer is unregistered. Strings are never
// modified in place: every mutation installs a fresh buffer and retires the
// old one through string_unregister, and a buffer address can only be reused
// after its unregistration. So an unchanged epoch and buffer pointer mean the
// contents are unchanged. String input ports use this to skip re-deriving
// their byte offsets on every access.
uint64_t string_buffer_epoch;

void string_register(char *string)
{
    if (!string)
        return;
    ptr_registry_register(&string_registry, string, 0,
                          "string_register: malloc failed");
}

void string_unregister(char *string)
{
    string_buffer_epoch++;
    ptr_registry_unregister(&string_registry, string);
}

bool string_is_registered(const char *string)
{
    return ptr_registry_contains(&string_registry, string);
}

unsigned *string_buffer_info_slot(const char *string)
{
    // The registry node's len field is otherwise unused for strings. A node
    // lives exactly as long as its buffer is registered, and strings are
    // never modified in place, so whatever is cached there stays valid.
    ptr_registry_node *node =
        (ptr_registry_node *)ptr_registry_find(&string_registry, string);
    return node ? &node->len : NULL;
}

void string_view_register(string_view_data *view)
{
    if (!view)
        return;
    ptr_registry_register(&string_view_registry, view, 0,
                          "string_view_register: malloc failed");
}

void string_view_unregister(string_view_data *view)
{
    ptr_registry_unregister(&string_view_registry, view);
}

bool string_view_is_registered(const string_view_data *view)
{
    return ptr_registry_contains(&string_view_registry, view);
}

bool string_cell_is_view(unsigned value)
{
    return IS_CELL(value) && CELL_TYPE(value) == BT_STRING &&
           string_view_is_registered(
               (const string_view_data *)CELL_PTR(value));
}

char *string_cell_data(unsigned value)
{
    if (!IS_CELL(value) || CELL_TYPE(value) != BT_STRING)
        return NULL;
    string_view_data *view = (string_view_data *)CELL_PTR(value);
    return string_view_is_registered(view) ? view->data : (char *)view;
}

bool string_cell_is_immutable(unsigned value)
{
    if (string_cell_is_view(value))
        return ((string_view_data *)CELL_PTR(value))->immutable;
    return string_is_immutable(string_cell_data(value));
}

static bool string_view_refresh(string_view_data *view, unsigned depth)
{
    if (!string_view_is_registered(view) || depth > 1024)
        return false;
    if (string_cell_is_view(view->parent)) {
        if (!string_view_refresh(
                (string_view_data *)CELL_PTR(view->parent), depth + 1))
            return false;
    }
    char *parent = string_cell_data(view->parent);
    if (!parent)
        return false;
    size_t start_byte = 0;
    size_t end_byte = 0;
    const char *error = NULL;
    if (!scheme_utf8_byte_offset_for_index(parent, view->start, true,
                                           &start_byte, &error) ||
        !scheme_utf8_byte_offset_for_index(parent, view->end, true,
                                           &end_byte, &error) ||
        end_byte < start_byte)
        return false;
    char *copy = checked_string_copy_len(parent + start_byte,
                                         end_byte - start_byte);
    if (!copy)
        return false;
    string_unregister(view->data);
    free(view->data);
    view->data = copy;
    string_register(copy);
    return true;
}

static bool string_view_has_ancestor(unsigned value, unsigned ancestor,
                                     unsigned depth)
{
    if (!string_cell_is_view(value) || depth > 1024)
        return false;
    string_view_data *view = (string_view_data *)CELL_PTR(value);
    return view->parent == ancestor ||
           string_view_has_ancestor(view->parent, ancestor, depth + 1);
}

void string_views_refresh(unsigned parent)
{
    if (!string_view_registry.buckets)
        return;
    for (size_t i = 0; i < string_view_registry.bucket_count; i++) {
        for (ptr_registry_node *node = string_view_registry.buckets[i]; node;
             node = node->next) {
            string_view_data *view = (string_view_data *)node->ptr;
            // The parent/range metadata is authoritative; refreshing every
            // view whose ancestry contains the changed cell is safe.
            if (view->parent == parent ||
                string_view_has_ancestor(view->parent, parent, 0))
                string_view_refresh(view, 0);
        }
    }
}

void string_mark_immutable(char *string)
{
    if (!string)
        return;
    ptr_registry_register(&immutable_string_registry, string, 0,
                          "string_mark_immutable: malloc failed");
}

bool string_is_immutable(const char *string)
{
    return ptr_registry_contains(&immutable_string_registry, string);
}

void vector_register(vector_data *vector)
{
    if (!vector)
        return;
    ptr_registry_register(&vector_registry, vector, vector->len,
                          "vector_register: malloc failed");
}

void vector_unregister(vector_data *vector)
{
    ptr_registry_unregister(&vector_registry, vector);
}

bool vector_is_registered(const vector_data *vector)
{
    return ptr_registry_find(&vector_registry, vector) != NULL;
}

// Payloads of small vectors and records, kept for reuse when swept instead of
// freed: a program making records (promises, say) in a loop otherwise mallocs
// and frees one per object. Stocked by exact length, so a reused block is the
// size it was allocated at. The length comes from the registry node, which
// recorded it at allocation, not from the payload, which a malformed cell
// could have overwritten.
// The stock is bounded (a few MB at most) but large enough to cover what one
// minor collection reclaims of a loop's records.
#define SPARE_PAYLOAD_MAX_LEN 8
#define SPARE_PAYLOADS_PER_LEN 32768
typedef struct {
    vector_data **blocks;
    unsigned count;
    unsigned cap;
} payload_stock;
static payload_stock spare_payloads[SPARE_PAYLOAD_MAX_LEN + 1];

// An unregistered payload of len slots, contents uninitialized; NULL if the
// length is too large or memory is exhausted.
static vector_data *vector_payload_alloc(unsigned len)
{
    if (len <= SPARE_PAYLOAD_MAX_LEN && spare_payloads[len].count)
        return spare_payloads[len].blocks[--spare_payloads[len].count];
    return checked_malloc_flex(sizeof(vector_data), len, sizeof(unsigned));
}

// Release a registered payload: unregister it, then stock or free it.
static void vector_payload_release(vector_data *vd)
{
    const ptr_registry_node *node = ptr_registry_find(&vector_registry, vd);
    if (!node)
        return;
    unsigned len = node->len;
    vector_unregister(vd);
    if (len <= SPARE_PAYLOAD_MAX_LEN) {
        payload_stock *stock = &spare_payloads[len];
        if (stock->count == stock->cap && stock->cap < SPARE_PAYLOADS_PER_LEN) {
            unsigned cap = stock->cap ? stock->cap * 2 : 256;
            vector_data **blocks =
                checked_realloc_array(stock->blocks, cap, sizeof(*blocks));
            if (blocks) {
                stock->blocks = blocks;
                stock->cap = cap;
            }
        }
        if (stock->count < stock->cap) {
            stock->blocks[stock->count++] = vd;
            return;
        }
    }
    free(vd);
}

void bytevec_register(bytevec_data *bytevec)
{
    if (!bytevec)
        return;
    ptr_registry_register(&bytevec_registry, bytevec, bytevec->len,
                          "bytevec_register: malloc failed");
}

void bytevec_unregister(bytevec_data *bytevec)
{
    ptr_registry_unregister(&bytevec_registry, bytevec);
}

bool bytevec_is_registered(const bytevec_data *bytevec)
{
    return ptr_registry_find(&bytevec_registry, bytevec) != NULL;
}

void init_heap(void)
{
    setlocale(LC_CTYPE, "");
    init_gc_stress();
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
    ctx.atom_uninterned = calloc(ctx.atom_table_cap, sizeof(bool));
    atom_core_forms = calloc(ctx.atom_table_cap, sizeof(uint8_t));
    if (!ctx.atom_uninterned || !atom_core_forms) {
        fprintf(stderr, "Fatal: failed to allocate atom metadata\n");
        exit(EXIT_FAILURE);
    }
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

// True while gc() or minor_gc() is running.
static bool collection_in_progress = false;

void lisp_panic(const char *msg)
{
    fprintf(stderr, "Fatal error: %s\n", msg);
    // A collection that stops partway leaves the heap half copied: some cells
    // forwarded and some not, some roots updated and others still pointing at
    // the old copies. Nothing can resume from that, so a panic raised inside a
    // collection exits even when a recovery point is set.
    if (collection_in_progress) {
        fprintf(stderr, "Fatal error: garbage collection could not complete\n");
        exit(EXIT_FAILURE);
    }
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

// GC stress: force a minor collection every N allocations.
//
// A collection normally only happens when the nursery (256K cells) fills, so
// a reference that was never rooted survives almost every run and fails only
// when a collection lands on exactly the wrong allocation. That makes rooting
// bugs look like flaky, layout-dependent test failures. Setting
// VESPER_GC_STRESS=N collects every N allocations instead, which turns them
// into deterministic ones. N=1 is the most aggressive and the slowest.
static unsigned gc_stress_interval = 0; // 0 disables the whole mechanism
static unsigned gc_stress_countdown = 0;

// VESPER_GC_STRESS_MAJOR=N does the same for full collections. Minor GC only
// ever touches the nursery, so on its own it leaves the semispace flip,
// promotion and the code-object constant walk barely exercised - which is
// where a different set of rooting bugs would live.
static unsigned gc_stress_major_interval = 0;
static unsigned gc_stress_major_countdown = 0;

// VESPER_GC_VERIFY=1 checks the external payload lists against a walk of the
// heap at every collection (see "External payload tracking").
static bool gc_verify_external = false;

#ifdef VESPER_GC_TRAP_FREE_CELLS
void gc_report_free_cell_access(unsigned id, const char *accessor)
{
    fprintf(stderr,
            "GC TRAP: %s() on reclaimed cell %u - a cell index was held "
            "across a collection without being rooted\n",
            accessor, id);
    abort();
}
#endif

static unsigned read_stress_interval(const char *name)
{
    const char *setting = getenv(name);
    if (!setting || !*setting)
        return 0;
    long value = strtol(setting, NULL, 10);
    if (value <= 0)
        return 0;
    return (value > INT_MAX) ? (unsigned)INT_MAX : (unsigned)value;
}

void init_gc_stress(void)
{
    gc_stress_interval = read_stress_interval("VESPER_GC_STRESS");
    gc_stress_countdown = gc_stress_interval;
    gc_stress_major_interval = read_stress_interval("VESPER_GC_STRESS_MAJOR");
    gc_stress_major_countdown = gc_stress_major_interval;
    gc_verify_external = read_stress_interval("VESPER_GC_VERIFY") != 0;
}

// Only stress once there is an allocation root to collect from. Startup interns
// primitives before main.c calls set_alloc_gc_root, and a collection there
// would have nothing to trace from and would discard the environment being
// built. The real collector never runs that early either - that phase
// allocates far less than a nursery.
static inline bool gc_stress_armed(void)
{
    return alloc_gc_root && *alloc_gc_root;
}

// Returns true if a stress collection is due. Kept tiny so the disabled case
// is a single predictable branch on the allocation hot path.
static inline bool gc_stress_due(void)
{
    if (!gc_stress_interval)
        return false;
    if (!gc_stress_armed())
        return false;
    if (--gc_stress_countdown)
        return false;
    gc_stress_countdown = gc_stress_interval;
    return true;
}

static inline bool gc_stress_major_due(void)
{
    if (!gc_stress_major_interval)
        return false;
    if (!gc_stress_armed())
        return false;
    if (--gc_stress_major_countdown)
        return false;
    gc_stress_major_countdown = gc_stress_major_interval;
    return true;
}

// Shadow stack for GC roots - protects local C variables during allocation.
// When GC runs, these are collected and updated in place. The push and pop
// paths are inline in context.h; it starts empty (size 0), so the first
// gc_protect allocates it here.
unsigned **gc_shadow_stack = NULL;
int gc_shadow_stack_size = 0;
int gc_shadow_stack_top = 0;

void gc_shadow_stack_grow(void)
{
    if (gc_shadow_stack_top < gc_shadow_stack_size)
        return;
    if (gc_shadow_stack_size > INT_MAX / 2 ||
        (size_t)gc_shadow_stack_size * 2 > SIZE_MAX / sizeof(unsigned *)) {
        lisp_panic("shadow stack too large");
    }
    int new_size = gc_shadow_stack_size ? gc_shadow_stack_size * 2 : 4096;
    unsigned **new_stack = checked_realloc_array(
        gc_shadow_stack, (unsigned)new_size, sizeof(unsigned *));
    if (!new_stack) {
        lisp_panic(gc_shadow_stack ? "failed to expand shadow stack"
                                   : "failed to allocate shadow stack");
    }
    gc_shadow_stack = new_stack;
    gc_shadow_stack_size = new_size;
}

void gc_shadow_stack_underflow(void)
{
    lisp_panic("shadow stack underflow");
}

void gc_shadow_stack_bad_mark(void)
{
    lisp_panic("invalid gc_release mark");
}

// After a lisp_panic longjmp the C frames that registered shadow-stack
// entries are gone, and so are the vm_run frames whose vm_state the active-VM
// chain points at. Drop both, along with the allocation root, before
// resuming; the caller re-registers whatever roots it still holds. (A panic
// inside a collection exits instead, so the collector state is already idle.)
void report_error(const char *msg)
{
    snprintf(ctx.last_error, sizeof(ctx.last_error), "%s", msg);
    char located[sizeof(ctx.error_diagnostic)];
    snprintf(located, sizeof(located), "%s:%d:%d: error: %s",
             reader_get_filename(), reader_get_line(), reader_get_col(), msg);
    hold_error_diagnostic(located);
}

void hold_error_diagnostic(const char *text)
{
    if (ctx.suppress_error_output)
        return;
    // The same error reported again (a lookup retried on a slower path, an
    // operation redone to word its error) is still one error.
    if (ctx.error_diagnostic_pending && strcmp(ctx.error_diagnostic, text) == 0)
        return;
    flush_error_diagnostic();
    snprintf(ctx.error_diagnostic, sizeof(ctx.error_diagnostic), "%s", text);
    ctx.error_diagnostic_pending = true;
}

void discard_error_diagnostic(void)
{
    ctx.error_diagnostic_pending = false;
}

void flush_error_diagnostic(void)
{
    if (!ctx.error_diagnostic_pending)
        return;
    ctx.error_diagnostic_pending = false;
    fflush(stdout);
    fprintf(stderr, "%s\n", ctx.error_diagnostic);
}

void gc_recover_after_panic(void)
{
    gc_shadow_stack_top = 0;
    in_gc_mode = false;
    collection_in_progress = false;
    alloc_gc_root = NULL;
    // The CPS trampoline registers are GC roots and still describe the
    // abandoned computation; keeping them would retain everything it built.
    tramp.expr = tramp.env = tramp.cont = tramp.value = 0;
    vm_reset_after_panic();
    pattern_reset_after_panic();
}

// After a minor collection, the old generation may be close to full: every
// nursery survivor was just promoted into it, and only a major collection
// reclaims it. The nursery-full trigger has always run this check; the stress
// hooks did not, so a minor collection every N allocations promoted almost
// everything (at that cadence nearly all of it is still live) and old gen
// filled monotonically until minor_gc panicked with "old generation full".
// One helper, called from both paths, so they cannot drift apart.
static void maybe_major_after_minor(void)
{
    unsigned old_gen_used = ctx.hptr - ctx.mmin;
    unsigned old_gen_size = ctx.nursery_start - ctx.mmin;
    if (old_gen_used > old_gen_size * 8 / 10) {
        if (alloc_gc_root && *alloc_gc_root)
            *alloc_gc_root = gc(*alloc_gc_root);
        else
            (void)gc(0);
    }
}

// Collect the nursery on behalf of alloc(), then run a full collection if
// promotion left the old generation nearly full. minor_gc() falls back to a
// full collection itself when the old generation could not take the nursery;
// a second one straight after it would find nothing more to reclaim.
static void collect_nursery_for_alloc(void)
{
    unsigned major_count = ctx.major_gc_count;
    if (alloc_gc_root && *alloc_gc_root)
        *alloc_gc_root = minor_gc(*alloc_gc_root);
    else
        (void)minor_gc(0);
    if (ctx.major_gc_count == major_count)
        maybe_major_after_minor();
}

unsigned alloc(void)
{
    // init_heap always allocates the card table (it exits otherwise), so the
    // heap is always generational.
    // During a major GC, allocate directly to the to-space old generation
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

    // Under stress, collect on schedule rather than waiting for the nursery
    // to fill. in_gc_mode is already excluded above, so this cannot recurse.
    // The major check comes first: a full collection subsumes a minor one, and
    // running both back to back would only waste time.
    if (gc_stress_major_due()) {
        *alloc_gc_root = gc(*alloc_gc_root);
        nursery_end =
            (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
    } else if (gc_stress_due()) {
        collect_nursery_for_alloc();
        nursery_end =
            (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
    }

    // Check if nursery is full
    if (ctx.nursery_ptr >= nursery_end) {
        // Nursery full - promote survivors (and collect old gen if needed)
        collect_nursery_for_alloc();

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
    // The collectors no longer reset every reclaimed cell, so a reused cell
    // still holds whatever was there before. Clear the type, so a cell the
    // caller has not typed yet reads as free, as it did when the sweep reset
    // the whole nursery.
    CELL_TYPE(cell) = BT_FREE;
    return cell;
}

unsigned alloc_cons(unsigned car_val, unsigned cdr_val)
{
    unsigned p;
    // The common case takes the next nursery cell and cannot collect, so it
    // needs no roots: no collection in progress, no stress schedule, room
    // left. Anything else goes through alloc() with the operands rooted.
    unsigned nursery_end =
        (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
    if (__builtin_expect(!in_gc_mode && !gc_stress_interval &&
                             !gc_stress_major_interval &&
                             ctx.nursery_ptr < nursery_end,
                         1)) {
        p = ctx.nursery_ptr++;
    } else {
        GC_PROTECT_GUARD2(&car_val, &cdr_val);
        p = alloc();
    }
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
    gc_note_external(p);
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
        if (!bn)
            return NULL;
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

// The types whose cells own a malloc'd payload, which free_external_cell_data
// releases. Keep the two in step.
static inline bool cell_type_owns_external(int t)
{
    switch (t) {
    case BT_BIGNUM:
    case BT_HASHTABLE:
    case BT_VECTOR:
    case BT_RECORD:
    case BT_RECORD_TYPE:
    case BT_STRING:
    case BT_BYTEVEC:
    case BT_STRINPORT:
    case BT_STROUTPORT:
    case BT_INPORT:
    case BT_OUTPORT:
    case BT_VMCONT:
        return true;
    default:
        return false;
    }
}

static void free_external_cell_data(unsigned x)
{
    if (!cell_type_owns_external(CELL_TYPE(x)))
        return;
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
    } else if (CELL_TYPE(x) == BT_VECTOR || CELL_TYPE(x) == BT_RECORD ||
               CELL_TYPE(x) == BT_RECORD_TYPE) {
        vector_data *vd = (vector_data *)CELL_PTR(x);
        if (vector_is_registered(vd)) {
            if (CELL_TYPE(x) != BT_VECTOR && object_hash_registry.count)
                ptr_registry_unregister(&object_hash_registry, vd);
            vector_payload_release(vd);
        }
        CELL_PTR(x) = NULL;
    } else if (CELL_TYPE(x) == BT_STRING) {
        string_view_data *view = (string_view_data *)CELL_PTR(x);
        if (string_view_is_registered(view)) {
            string_view_unregister(view);
            string_unregister(view->data);
            free(view->data);
            free(view);
            CELL_PTR(x) = NULL;
            return;
        }
        char *s = (char *)CELL_PTR(x);
        if (string_is_registered(s)) {
            string_unregister(s);
            if (string_is_immutable(s))
                ptr_registry_unregister(&immutable_string_registry, s);
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
            if (!sp->borrowed_data)
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
        vm_continuation *cont = (vm_continuation *)CELL_PTR(x);
        if (vm_continuation_is_registered(cont)) {
            vm_continuation_unregister(cont);
            free(cont);
        }
        CELL_PTR(x) = NULL;
    }
}

// ============================================================================
// External payload tracking
// ============================================================================
//
// A copying collector never visits dead cells, yet a dead cell of an owning
// type must have its payload freed. The sweep used to find them by walking
// every cell of the reclaimed space: the whole nursery at each minor
// collection, and the whole 16M-cell semispace at each major one, which took
// ~40ms even with next to nothing live - and the REPL runs a major collection
// after every top-level form. Instead two lists record which cells own a
// payload:
//
//   young_external  every cell given an owning type since the last
//                   collection (gc_note_external); nursery cells, except for
//                   the rare old cell that takes over a payload
//   old_external    old-generation cells of an owning type
//
// A collection walks only the entries in the space it reclaims. An entry whose
// cell was copied (BT_BROKENHEART) is replaced by the copy, a dead one has its
// payload freed, and either way the reclaimed cell is made BT_FREE, which
// turns any duplicate entry into a no-op. An entry whose cell no longer has
// an owning type (a reader placeholder took over its payload) just drops out.
//
// The lists are complete only if every cell is noted when it gets an owning
// type. If one cannot grow, external_tracking_lost makes the collectors fall
// back to sweeping every cell, as they used to, and the next major collection
// rebuilds the old list from the copied heap. VESPER_GC_VERIFY=1 checks the
// lists against a full walk at every collection.

typedef struct {
    unsigned *cells;
    unsigned len;
    unsigned cap;
} cell_list;

static cell_list young_external;
static cell_list old_external;
static cell_list spare_external; // reused as the next old_external
static bool external_tracking_lost = false;

static bool cell_list_push(cell_list *list, unsigned cell)
{
    if (list->len == list->cap) {
        if (list->cap > UINT_MAX / 2)
            return false;
        unsigned new_cap = list->cap ? list->cap * 2 : 1024;
        unsigned *cells =
            checked_realloc_array(list->cells, new_cap, sizeof(unsigned));
        if (!cells)
            return false;
        list->cells = cells;
        list->cap = new_cap;
    }
    list->cells[list->len++] = cell;
    return true;
}

void gc_note_external(unsigned cell)
{
    if (!cell_list_push(&young_external, cell))
        external_tracking_lost = true;
}

static void track_old_external(cell_list *list, unsigned cell)
{
    if (!cell_list_push(list, cell))
        external_tracking_lost = true;
}

void gc_set_verify_external(bool enabled)
{
    gc_verify_external = enabled;
}

void gc_lose_external_tracking(void)
{
    external_tracking_lost = true;
}

unsigned gc_tracked_external_count(bool young)
{
    return young ? young_external.len : old_external.len;
}

// Settle one entry for a collection reclaiming [from_start, from_end): a
// copied cell's copy goes on `keep`, a dead cell's payload is freed. Entries
// outside the reclaimed space that still own a payload are kept as they are.
static void settle_external_entry(unsigned cell, unsigned from_start,
                                  unsigned from_end, cell_list *keep)
{
    if (cell < from_start || cell >= from_end) {
        if (cell >= HEAP_RESERVED && cell < 2 * SEMISPACE_SIZE &&
            cell_type_owns_external(CELL_TYPE(cell)))
            track_old_external(keep, cell);
        return;
    }
    int type = CELL_TYPE(cell);
    if (type == BT_BROKENHEART) {
        unsigned copy = CELL_CAR(cell);
        if (cell_type_owns_external(CELL_TYPE(copy)))
            track_old_external(keep, copy);
        CELL_TYPE(cell) = BT_FREE;
    } else if (cell_type_owns_external(type)) {
        free_external_cell_data(cell);
        CELL_TYPE(cell) = BT_FREE;
    }
}

// The fallback when the lists are incomplete: every cell of the range.
static void sweep_external_full(unsigned start, unsigned end)
{
    for (unsigned i = start; i < end; i++) {
        free_external_cell_data(i);
        CELL_TYPE(i) = BT_FREE;
    }
}

// The collector leaves dead cells' types alone, but a build that traps on
// reclaimed cells needs every one of them marked.
static void mark_reclaimed_cells_free(unsigned start, unsigned end)
{
#ifdef VESPER_GC_TRAP_FREE_CELLS
    for (unsigned i = start; i < end; i++)
        CELL_TYPE(i) = BT_FREE;
#else
    (void)start;
    (void)end;
#endif
}

// VESPER_GC_VERIFY: before a collection's sweep, check that every cell in
// [start, end) that owns a payload, or was copied to a cell that does, is on
// one of the lists. A miss is a constructor that forgot gc_note_external, and
// would leak the payload, so stop right there.
static void verify_external_tracked(unsigned start, unsigned end,
                                    const char *collection)
{
    if (end <= start)
        return;
    unsigned span = end - start;
    unsigned char *listed = calloc(((size_t)span + 7) / 8, 1);
    if (!listed) {
        fprintf(stderr, "GC VERIFY: out of memory\n");
        abort();
    }
    const cell_list *lists[] = {&young_external, &old_external};
    for (size_t l = 0; l < 2; l++)
        for (unsigned i = 0; i < lists[l]->len; i++) {
            unsigned cell = lists[l]->cells[i];
            if (cell >= start && cell < end)
                listed[(cell - start) / 8] |=
                    (unsigned char)(1u << ((cell - start) % 8));
        }
    for (unsigned cell = start; cell < end; cell++) {
        int type = CELL_TYPE(cell);
        if (type == BT_BROKENHEART)
            type = CELL_TYPE(CELL_CAR(cell));
        if (!cell_type_owns_external(type))
            continue;
        if (!(listed[(cell - start) / 8] & (1u << ((cell - start) % 8)))) {
            fprintf(stderr,
                    "GC VERIFY: %s collection: cell %u of type %d owns a "
                    "payload but was never passed to gc_note_external\n",
                    collection, cell, type);
            abort();
        }
    }
    free(listed);
}

// Minor collection: the nursery [start, end) is reclaimed. Promoted copies
// join the old list, and so does any old cell noted since the last
// collection.
static void sweep_nursery_external(unsigned start, unsigned end)
{
    if (external_tracking_lost) {
        // The next major collection rebuilds the old list; all that is
        // needed here is to free the nursery's dead payloads.
        sweep_external_full(start, end);
    } else {
        for (unsigned i = 0; i < young_external.len; i++)
            settle_external_entry(young_external.cells[i], start, end,
                                  &old_external);
        mark_reclaimed_cells_free(start, end);
    }
    young_external.len = 0;
}

// Major collection: the whole other semispace [start, end) is reclaimed and
// the live heap is now [to_start, to_end).
static void sweep_semispace_external(unsigned start, unsigned end,
                                     unsigned to_start, unsigned to_end)
{
    spare_external.len = 0;
    if (external_tracking_lost) {
        sweep_external_full(start, end);
        external_tracking_lost = false;
        for (unsigned cell = to_start; cell < to_end; cell++)
            if (cell_type_owns_external(CELL_TYPE(cell)))
                track_old_external(&spare_external, cell);
    } else {
        for (unsigned i = 0; i < old_external.len; i++)
            settle_external_entry(old_external.cells[i], start, end,
                                  &spare_external);
        for (unsigned i = 0; i < young_external.len; i++)
            settle_external_entry(young_external.cells[i], start, end,
                                  &spare_external);
        mark_reclaimed_cells_free(start, end);
    }
    young_external.len = 0;
    cell_list next = spare_external;
    spare_external = old_external;
    old_external = next;
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
    unsigned letrec_depth =
        (cont->letrecs && cont->letrec_depth <= VM_MAX_STACK_SIZE)
            ? cont->letrec_depth
            : 0;
    unsigned letrec_saved_len =
        (cont->letrec_saved && cont->letrec_saved_len <= VM_MAX_STACK_SIZE)
            ? cont->letrec_saved_len
            : 0;

    for (unsigned i = 0; i < sp; i++) {
        cont->stack[i] = collector(cont->stack[i]);
    }
    cont->env = collector(cont->env);
    cont->signal_handler = collector(cont->signal_handler);
    for (unsigned i = 0; i < fp; i++) {
        cont->frames[i].env = collector(cont->frames[i].env);
    }
    for (unsigned i = 0; i < letrec_depth; i++) {
        cont->letrecs[i].frame = collector(cont->letrecs[i].frame);
    }
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

/*
 * Return enough leading limbs for a double's significand and the number of
 * omitted low limbs. The highest limb may contain only one significant bit,
 * so two 32-bit limbs can give just 33 bits, silently discarding usable double
 * precision. Keeping the scale separate also avoids premature overflow.
 */
bool exact_integer_top_double(unsigned value, double *top,
                                     size_t *omitted_limbs, bool *negative)
{
    int64_t small;
    if (IS_FIXNUM(value)) {
        small = FIXNUM_VALUE(value);
    } else if (IS_NUM(value)) {
        small = CELL_ID(value);
    } else if (IS_BIGNUM(value)) {
        bignum *bn = get_bignum(value);
        if (!bn)
            return false;
        if (bn->len == 0) {
            *top = 0.0;
            *omitted_limbs = 0;
            *negative = false;
            return true;
        }
        size_t needed = 1 + (DBL_MANT_DIG + LIMB_BITS - 1) / LIMB_BITS;
        size_t taken = bn->len < needed ? bn->len : needed;
        size_t start = bn->len - taken;
        double leading = 0.0;
        for (size_t i = bn->len; i > start; i--)
            leading = leading * (double)((uint64_t)1 << LIMB_BITS) +
                      bn->limbs[i - 1];
        *top = leading;
        *omitted_limbs = start;
        *negative = bn->sign != 0;
        return true;
    } else {
        return false;
    }

    uint64_t magnitude =
        small < 0 ? -(uint64_t)small : (uint64_t)small;
    *top = (double)magnitude;
    *omitted_limbs = 0;
    *negative = small < 0;
    return true;
}

static double approximate_rational_double(unsigned numerator,
                                          unsigned denominator)
{
    double num_top, denom_top;
    size_t num_omitted, denom_omitted;
    bool num_negative, denom_negative;
    if (!exact_integer_top_double(numerator, &num_top, &num_omitted,
                                  &num_negative) ||
        !exact_integer_top_double(denominator, &denom_top, &denom_omitted,
                                  &denom_negative) ||
        denom_top == 0.0)
        return to_double(numerator) / to_double(denominator);
    if (num_top == 0.0)
        return 0.0;

    bool negative = num_negative != denom_negative;
    double ratio = num_top / denom_top;
    if (num_omitted >= denom_omitted) {
        size_t limb_delta = num_omitted - denom_omitted;
        if (limb_delta > (size_t)INT_MAX / LIMB_BITS)
            return negative ? -HUGE_VAL : HUGE_VAL;
        ratio = scalbn(ratio, (int)(limb_delta * LIMB_BITS));
    } else {
        size_t limb_delta = denom_omitted - num_omitted;
        if (limb_delta > (size_t)INT_MAX / LIMB_BITS)
            return negative ? -0.0 : 0.0;
        ratio = scalbn(ratio, -(int)(limb_delta * LIMB_BITS));
    }
    return negative ? -ratio : ratio;
}

static const bignum *exact_integer_view(unsigned value, bignum *view,
                                       limb_t limbs[2])
{
    if (IS_BIGNUM(value))
        return get_bignum(value);

    int64_t small = IS_FIXNUM(value) ? FIXNUM_VALUE(value) : CELL_ID(value);
    uint64_t magnitude = small < 0 ? -(uint64_t)small : (uint64_t)small;
    limbs[0] = (limb_t)magnitude;
    limbs[1] = (limb_t)(magnitude >> LIMB_BITS);
    *view = (bignum){
        .limbs = limbs,
        .len = limbs[1] ? 2 : (limbs[0] != 0),
        .cap = 2,
        .sign = small < 0,
    };
    return view;
}

static double exact_rational_to_double(unsigned numerator, unsigned denominator)
{
    bignum numerator_view, denominator_view;
    limb_t numerator_limbs[2], denominator_limbs[2];
    const bignum *a = exact_integer_view(numerator, &numerator_view, numerator_limbs);
    const bignum *b = exact_integer_view(denominator, &denominator_view,
                                        denominator_limbs);
    double result;
    if (a && b && bn_ratio_to_double(a, b, &result))
        return result;
    // Conversion callers lack an allocation failure result. On allocation
    // failure keep the previous scaled approximation instead.
    return approximate_rational_double(numerator, denominator);
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
        bignum *bn = get_bignum(x);
        return bn ? bn_to_double(bn) : 0.0;
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
        return exact_rational_to_double(CELL_CAR(x), CELL_CDR(x));
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
    vector_data *vd = vector_payload_alloc(len);
    if (!vd) {
        show_error("make-vector: out of memory");
        return TOK_ERROR;
    }
    vd->len = len;
    vector_register(vd);
    unsigned p = alloc();
    CELL_TYPE(p) = BT_VECTOR;
    CELL_PTR(p) = vd;
    gc_note_external(p);
    for (unsigned i = 0; i < len; i++) {
        vd->data[i] = fill;
    }
    return p;
}

// ============================================================================
// Records
// ============================================================================

// Allocate a vector_data payload of len slots, every one #f, and a cell of
// the given type for it.
static unsigned make_vector_like(enum lisp_type type, unsigned len,
                                 const char *who)
{
    if (!checked_flex_size(sizeof(vector_data), len, sizeof(unsigned), NULL)) {
        show_error("%s: too many fields", who);
        return TOK_ERROR;
    }
    vector_data *vd = vector_payload_alloc(len);
    if (!vd) {
        show_error("%s: out of memory", who);
        return TOK_ERROR;
    }
    vd->len = len;
    // Registered before alloc(): a collection there must not mistake the
    // payload for garbage, and no cell refers to it yet, so nothing scans its
    // uninitialized slots.
    vector_register(vd);
    unsigned p = alloc();
    CELL_TYPE(p) = type;
    CELL_PTR(p) = vd;
    gc_note_external(p);
    for (unsigned i = 0; i < len; i++)
        vd->data[i] = ctx.atom_false;
    return p;
}

unsigned make_record_type(unsigned name, unsigned field_names, unsigned flags)
{
    GC_GUARD;
    gc_protect(&name);
    gc_protect(&field_names);
    unsigned nfields = 0;
    if (!list_length_checked(field_names, &nfields, "make-record-type"))
        return TOK_ERROR;
    if (nfields > UINT_MAX - RECORD_TYPE_FIELDS_START) {
        show_error("make-record-type: too many fields");
        return TOK_ERROR;
    }
    unsigned rtd = make_vector_like(BT_RECORD_TYPE,
                                    nfields + RECORD_TYPE_FIELDS_START,
                                    "make-record-type");
    if (rtd == TOK_ERROR)
        return TOK_ERROR;
    // Still in the nursery and nothing has allocated since: plain stores.
    unsigned *data = GET_VECTOR_PTR(rtd)->data;
    data[RECORD_TYPE_NAME_SLOT] = name;
    data[RECORD_TYPE_FLAGS_SLOT] = INT_CACHE_START + (flags & INT_CACHE_MAX);
    unsigned i = RECORD_TYPE_FIELDS_START;
    FORLIST(f, field_names)
    data[i++] = car(f);
    return rtd;
}

unsigned make_record(unsigned rtd)
{
    GC_GUARD;
    gc_protect(&rtd);
    if (!record_type_well_formed(rtd)) {
        show_error("make-record: not a record type");
        return TOK_ERROR;
    }
    unsigned rec = make_vector_like(
        BT_RECORD, record_type_field_count(rtd) + RECORD_FIELDS_START,
        "make-record");
    if (rec == TOK_ERROR)
        return TOK_ERROR;
    // rtd is the protected local, so this is its post-collection location.
    GET_VECTOR_PTR(rec)->data[RECORD_TYPE_SLOT] = rtd;
    return rec;
}

// Every record construction and type test checks its type, nearly always one
// of a handful of long-lived types, and the registry probe is most of that
// cost. So remember the last type that passed, as its cell and payload, until
// the next collection - the only thing that moves the cell or frees the
// payload; a type's payload is never written after make_record_type.
static unsigned checked_rtd_cell;
static const vector_data *checked_rtd_payload;
static uint64_t checked_rtd_epoch;

bool record_type_well_formed(unsigned rtd)
{
    if (!IS_RECORD_TYPE(rtd))
        return false;
    vector_data *vd = GET_VECTOR_PTR(rtd);
    if (rtd == checked_rtd_cell && vd == checked_rtd_payload &&
        checked_rtd_epoch == env_gc_epoch)
        return true;
    if (!vector_data_well_formed(vd) || vd->len < RECORD_TYPE_FIELDS_START ||
        !IS_ATOM(vd->data[RECORD_TYPE_NAME_SLOT]))
        return false;
    checked_rtd_cell = rtd;
    checked_rtd_payload = vd;
    checked_rtd_epoch = env_gc_epoch;
    return true;
}

unsigned record_type_field_count(unsigned rtd)
{
    return vector_len(rtd) - RECORD_TYPE_FIELDS_START;
}

unsigned record_type_flags(unsigned rtd)
{
    unsigned flags = vector_data_ptr(rtd)[RECORD_TYPE_FLAGS_SLOT];
    return IS_NUM(flags) ? (unsigned)CELL_ID(flags) : 0;
}

const char *record_type_name_string(unsigned rtd)
{
    unsigned name = vector_data_ptr(rtd)[RECORD_TYPE_NAME_SLOT];
    return atom_is_valid(name) ? ctx.atom_table[CELL_ID(name)] : "record";
}

unsigned record_rtd(unsigned rec)
{
    if (!IS_RECORD(rec))
        return 0;
    vector_data *vd = GET_VECTOR_PTR(rec);
    if (!vector_data_well_formed(vd) || vd->len < RECORD_FIELDS_START)
        return 0;
    unsigned rtd = vd->data[RECORD_TYPE_SLOT];
    // A record's length is fixed by its type, so a type of matching length
    // makes every field index the type accepts valid for the record too.
    if (!record_type_well_formed(rtd) ||
        record_type_field_count(rtd) + RECORD_FIELDS_START != vd->len)
        return 0;
    return rtd;
}

bool record_is_builtin(unsigned rec, enum builtin_record_type which)
{
    unsigned rtd = ctx.builtin_record_types[which];
    return rtd != 0 && IS_RECORD(rec) && record_rtd(rec) == rtd;
}

bool char_set_record_fields(unsigned rec, unsigned *kind, unsigned *payload)
{
    // record_rtd has checked the record's length against its type's field
    // count, so a two-field type makes both reads in bounds.
    if (!record_is_builtin(rec, BRT_CHAR_SET) ||
        record_type_field_count(record_rtd(rec)) != 2)
        return false;
    vector_data *vd = GET_VECTOR_PTR(rec);
    *kind = vd->data[RECORD_FIELDS_START];
    *payload = vd->data[RECORD_FIELDS_START + 1];
    return true;
}

// ============================================================================
// Weak cell maps
// ============================================================================

static weak_cell_map *weak_cell_maps; // every registered map

static unsigned weak_cell_slot(const weak_cell_map *map, unsigned cell)
{
    return (unsigned)((cell * 2654435761u) & (map->index_cap - 1));
}

static void weak_cell_map_index_entry(weak_cell_map *map, unsigned pos)
{
    unsigned i = weak_cell_slot(map, map->keys[pos]);
    while (map->index[i])
        i = (i + 1) & (map->index_cap - 1);
    map->index[i] = pos + 1;
}

// Rebuild the index from the entry arrays, sized for at most half full.
static void weak_cell_map_reindex(weak_cell_map *map)
{
    unsigned cap = 64;
    while (cap / 2 <= map->len)
        cap *= 2;
    if (cap != map->index_cap) {
        unsigned *index = checked_calloc_array(cap, sizeof(unsigned));
        LISP_ASSERT_MSG(index != NULL, "weak_cell_map: index malloc failed");
        free(map->index);
        map->index = index;
        map->index_cap = cap;
    } else {
        memset(map->index, 0, (size_t)cap * sizeof(unsigned));
    }
    for (unsigned pos = 0; pos < map->len; pos++)
        weak_cell_map_index_entry(map, pos);
}

bool weak_cell_map_get(weak_cell_map *map, unsigned cell, uint64_t *val)
{
    if (!map->index_cap)
        return false;
    for (unsigned i = weak_cell_slot(map, cell); map->index[i];
         i = (i + 1) & (map->index_cap - 1)) {
        unsigned pos = map->index[i] - 1;
        if (map->keys[pos] == cell) {
            if (val)
                *val = map->vals[pos];
            return true;
        }
    }
    return false;
}

void weak_cell_map_put(weak_cell_map *map, unsigned cell, uint64_t val)
{
    if (!map->registered) {
        map->registered = true;
        map->next = weak_cell_maps;
        weak_cell_maps = map;
    }
    if (map->index_cap) {
        for (unsigned i = weak_cell_slot(map, cell); map->index[i];
             i = (i + 1) & (map->index_cap - 1)) {
            unsigned pos = map->index[i] - 1;
            if (map->keys[pos] == cell) {
                map->vals[pos] = val;
                return;
            }
        }
    }
    if (map->len == map->cap) {
        unsigned cap = map->cap ? map->cap * 2 : 64;
        unsigned *keys = checked_realloc_array(map->keys, cap, sizeof(unsigned));
        LISP_ASSERT_MSG(keys != NULL, "weak_cell_map: malloc failed");
        map->keys = keys;
        uint64_t *vals = checked_realloc_array(map->vals, cap, sizeof(uint64_t));
        LISP_ASSERT_MSG(vals != NULL, "weak_cell_map: malloc failed");
        map->vals = vals;
        map->cap = cap;
    }
    map->keys[map->len] = cell;
    map->vals[map->len] = val;
    map->len++;
    map->added_since_gc++;
    if (map->index_cap / 2 <= map->len)
        weak_cell_map_reindex(map);
    else
        weak_cell_map_index_entry(map, map->len - 1);
}

// Settle every map after a collection has traced the heap. A key in the
// reclaimed space (all of it for a major collection, the nursery for a minor
// one) was either copied - follow its forwarding pointer - or is dead, and
// its entry goes. A minor collection only moves nursery cells, and a key can
// only be one if its entry was added since the last collection, so a map
// with no new entries is left alone and the common case costs nothing.
void weak_cell_maps_after_gc(bool major)
{
    unsigned lo = major ? HEAP_RESERVED : ctx.nursery_start;
    unsigned hi = major ? UINT_MAX : ctx.nursery_ptr;
    for (weak_cell_map *map = weak_cell_maps; map; map = map->next) {
        if (!major && map->added_since_gc == 0)
            continue;
        unsigned out = 0;
        for (unsigned pos = 0; pos < map->len; pos++) {
            unsigned key = map->keys[pos];
            if (key >= lo && key < hi) {
                if (CELL_TYPE(key) != BT_BROKENHEART)
                    continue; // dead
                key = CELL_CAR(key);
            }
            map->keys[out] = key;
            map->vals[out] = map->vals[pos];
            out++;
        }
        map->len = out;
        map->added_since_gc = 0;
        weak_cell_map_reindex(map);
    }
}

static unsigned next_object_hash = 1;
// Procedures and continuations have no payload to key a number by (or, for
// VM continuations, one the registry below does not cover), so their numbers
// follow the cell itself.
static weak_cell_map object_hash_cells;
// Builtins are numbered per primitive: a primitive's cells are
// interchangeable, and eq? on them compares the primitive.
static unsigned *builtin_hash_numbers;

static bool object_hash_by_cell(unsigned obj)
{
    if (!IS_CELL(obj))
        return false;
    switch (CELL_TYPE(obj)) {
    case BT_FUNCTION:
    case BT_CONT:
    case BT_VMCONT:
    case BT_VMCLOSURE:
        return true;
    default:
        return false;
    }
}

unsigned object_hash_number(unsigned obj)
{
    if (IS_BUILTIN(obj)) {
        int64_t prim = CELL_ID(obj);
        if (prim < 0 || prim >= PRIM_COUNT)
            return 0;
        if (!builtin_hash_numbers) {
            builtin_hash_numbers =
                checked_calloc_array(PRIM_COUNT, sizeof(unsigned));
            if (!builtin_hash_numbers)
                return 0;
        }
        if (!builtin_hash_numbers[prim])
            builtin_hash_numbers[prim] = next_object_hash++;
        return builtin_hash_numbers[prim];
    }
    if (object_hash_by_cell(obj)) {
        uint64_t number;
        if (weak_cell_map_get(&object_hash_cells, obj, &number))
            return (unsigned)number;
        unsigned fresh = next_object_hash++;
        weak_cell_map_put(&object_hash_cells, obj, fresh);
        return fresh;
    }
    if (!IS_RECORD(obj) && !IS_RECORD_TYPE(obj))
        return 0;
    vector_data *vd = GET_VECTOR_PTR(obj);
    if (!vector_data_well_formed(vd))
        return 0;
    const ptr_registry_node *node =
        ptr_registry_find(&object_hash_registry, vd);
    if (node)
        return node->len;
    unsigned number = next_object_hash++;
    ptr_registry_register(&object_hash_registry, vd, number,
                          "object_hash_number: malloc failed");
    return number;
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

bool atom_is_valid(unsigned atom)
{
    if (!IS_ATOM(atom))
        return false;
    int64_t id = CELL_ID(atom);
    return id >= 0 && (uint64_t)id < ctx.atom_table_cap &&
           ctx.atom_table[id] != NULL;
}

bool vector_data_well_formed(const vector_data *vd)
{
    const ptr_registry_node *node = ptr_registry_find(&vector_registry, vd);
    if (!node)
        return false;
    return node->len == vd->len &&
           checked_flex_size(sizeof(vector_data), vd->len, sizeof(unsigned),
                             NULL);
}

bool bytevec_data_well_formed(const bytevec_data *bv)
{
    const ptr_registry_node *node = ptr_registry_find(&bytevec_registry, bv);
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
        h ^= (uint64_t)(unsigned char)*p;
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

static int atom_slot(const char *s, bool uninterned)
{
    // Atom IDs are table slots and are stored throughout the heap and
    // environments.  Rehashing would move entries and silently change the
    // identity of existing symbols, so this table must retain stable slots.
    // The probe loop below reports a controlled failure only if this fixed
    // table is actually full.

    int hash_value = hash_function(s);
    int original_hash = hash_value;

    for (int i = 1; ctx.atom_table[hash_value] &&
                    (uninterned || ctx.atom_uninterned[hash_value] ||
                     !str_equals(ctx.atom_table[hash_value], s));
         i++) {
        hash_value = (original_hash + i) % (int)ctx.atom_table_cap;
        // Prevent an infinite probe loop if the fixed table is full.
        if ((unsigned)i >= ctx.atom_table_cap) {
            lisp_panic("atom table full");
        }
    }

    if (!ctx.atom_table[hash_value]) {
        ctx.atom_table[hash_value] = checked_string_copy(s);
        if (!ctx.atom_table[hash_value]) {
            lisp_panic("failed to allocate memory for atom");
        }
        ctx.atom_uninterned[hash_value] = uninterned;
        ctx.atom_count++;
    }

    return hash_value;
}

int intern(const char *s)
{
    // Only interned symbols participate in the name lookup cache.
    for (int i = 0; i < INTERN_CACHE_SIZE; i++) {
        if (intern_cache[i].str && strcmp(intern_cache[i].str, s) == 0) {
            intern_cache_move_to_front(i);
            return intern_cache[0].atom_id;
        }
    }
    int hash_value = atom_slot(s, false);
    // Insert at front of cache
    intern_cache_insert_front(ctx.atom_table[hash_value], hash_value);
    return hash_value;
}

unsigned make_uninterned_symbol(const char *name)
{
    int id = atom_slot(name, true);
    unsigned symbol = alloc();
    CELL_TYPE(symbol) = BT_ATOM;
    CELL_ID(symbol) = id;
    return symbol;
}

bool atom_is_uninterned(unsigned atom)
{
    // The booleans and the core aliases occupy private slots but are not
    // generated symbols.
    return atom_is_valid(atom) && CELL_ID(atom) != CELL_ID(CELL_ATOM_TRUE) &&
           CELL_ID(atom) != CELL_ID(CELL_ATOM_FALSE) &&
           !atom_is_core_alias(CELL_ID(atom)) &&
           ctx.atom_uninterned[CELL_ID(atom)];
}

unsigned atom_from_string(const char *s)
{
    int id = intern(s);
    unsigned x = alloc();
    CELL_TYPE(x) = BT_ATOM;
    CELL_ID(x) = id;
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
    // Count the fast pointer's steps while it checks for a cycle, so the
    // list is walked once rather than once to validate and again to count.
    unsigned slow = lst;
    unsigned fast = lst;
    unsigned len = 0;
    while (IS_PAIR(fast)) {
        fast = cdr(fast);
        len++;
        if (!IS_PAIR(fast))
            break;
        fast = cdr(fast);
        len++;
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
    if (len_out)
        *len_out = len;
    return true;
}

bool pair_chain_is_circular(unsigned x)
{
    unsigned slow = x;
    unsigned fast = x;
    while (IS_PAIR(fast)) {
        fast = cdr(fast);
        if (!IS_PAIR(fast))
            break;
        fast = cdr(fast);
        slow = cdr(slow);
        if (slow == fast)
            return true;
    }
    return false;
}

bool proper_list_silent(unsigned x)
{
    if (pair_chain_is_circular(x))
        return false;
    while (IS_PAIR(x))
        x = cdr(x);
    return x == 0;
}

bool let_binding_has_value(unsigned binding)
{
    return IS_PAIR(binding) && IS_PAIR(cdr(binding)) && !cddr(binding);
}

void let_bindings_split(unsigned bindings, unsigned *vars, unsigned *inits)
{
    GC_GUARD;
    gc_protect(&bindings);
    unsigned vars_tail = 0, inits_tail = 0;
    gc_protect(&vars_tail);
    gc_protect(&inits_tail);
    *vars = *inits = 0;
    for (; bindings; bindings = cdr(bindings)) {
        unsigned binding = car(bindings);
        gc_protect(&binding);
        list_append(vars, &vars_tail, car(binding));
        list_append(inits, &inits_tail, cadr(binding));
        gc_unprotect(1);
    }
}

bool binding_list_binds_id(unsigned binding_list, int64_t id)
{
    if (pair_chain_is_circular(binding_list))
        return false;
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
    if (pair_chain_is_circular(params))
        return false;
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
    if (pair_chain_is_circular(list))
        return false;
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
    return IS_CELL(expr) && CELL_TYPE(expr) == BT_EOF;
}

bool syntax_rules_form_like(unsigned expr)
{
    return IS_PAIR(expr) && IS_KEYWORD(car(expr), ctx.kw_syntax_rules) &&
           IS_PAIR(cdr(expr)) && IS_PAIR(cddr(expr));
}

bool assoc_list_has_atom_key_id(unsigned assoc_list, int64_t id)
{
    if (pair_chain_is_circular(assoc_list))
        return false;
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

static bool cond_expand_requirement_satisfied_at_depth(unsigned requirement,
                                                       unsigned depth)
{
    if (!requirement)
        return true;
    if (depth >= MAX_COND_EXPAND_REQUIREMENT_DEPTH)
        return false;
    if (IS_ATOM(requirement))
        return feature_id_available(CELL_ID(requirement));
    if (!IS_PAIR(requirement) || !IS_ATOM(car(requirement)))
        return false;

    int64_t op = CELL_ID(car(requirement));
    unsigned args = cdr(requirement);
    if (op == ctx.kw_and_feature) {
        if (!proper_list_silent(args))
            return false;
        for (; args; args = cdr(args)) {
            if (!cond_expand_requirement_satisfied_at_depth(car(args),
                                                            depth + 1))
                return false;
        }
        return true;
    }
    if (op == ctx.kw_or_feature) {
        if (!proper_list_silent(args))
            return false;
        for (; args; args = cdr(args)) {
            if (cond_expand_requirement_satisfied_at_depth(car(args),
                                                            depth + 1))
                return true;
        }
        return false;
    }
    if (op == ctx.kw_not_feature) {
        return IS_PAIR(args) && !cdr(args) &&
               !cond_expand_requirement_satisfied_at_depth(car(args),
                                                            depth + 1);
    }
    return false;
}

bool cond_expand_requirement_satisfied(unsigned requirement)
{
    return cond_expand_requirement_satisfied_at_depth(requirement, 0);
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
    return IS_SYMBOL(x);
}

static bool pair_chain_acyclic(unsigned list)
{
    unsigned slow = list;
    unsigned fast = list;
    while (IS_PAIR(fast)) {
        fast = cdr(fast);
        if (!IS_PAIR(fast))
            return true;
        fast = cdr(fast);
        if (!IS_PAIR(slow))
            return true;
        slow = cdr(slow);
        if (fast == slow)
            return false;
    }
    return true;
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
    if (!pair_chain_acyclic(params))
        return false;

    unsigned seen = 0;
    gc_protect(&seen);
    gc_protect(&params); // alloc_cons below can GC; keep the walk valid
    while (params) {
        if (identifier_valid(params)) {
            FORLIST(p, seen) {
                if (CELL_ID(car(p)) == CELL_ID(params))
                    return false;
            }
            return true;
        }
        if (IS_ATOM(params))
            return false;
        if (!IS_PAIR(params))
            return false;
        unsigned param = car(params);
        if (!identifier_valid(param))
            return false;
        FORLIST(p, seen) {
            if (CELL_ID(car(p)) == CELL_ID(param))
                return false;
        }
        seen = alloc_cons(param, seen);
        params = cdr(params);
    }
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
        // A core alias of else or => is looked up as itself (see IS_KEYWORD).
        if (IS_KEYWORD(test, ctx.kw_else) &&
            (CELL_ID(test) == ctx.kw_else
                 ? else_unbound
                 : env_find_binding_cell(CELL_ID(test), env) == 0) &&
            cdr(c)) {
            show_error("%s: else clause must be last", name);
            return false;
        }
        if (conseq && IS_KEYWORD(car(conseq), ctx.kw_arrow) &&
            (CELL_ID(car(conseq)) == ctx.kw_arrow
                 ? arrow_unbound
                 : env_find_binding_cell(CELL_ID(car(conseq)), env) == 0) &&
            len != 3) {
            show_error("%s: invalid => clause", name);
            return false;
        }
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

// Copy the spine of `list` onto an accumulator whose last cdr is still open,
// the way append copies all but its last argument. False (with the error
// already reported) if `list` is improper: append rejects a dotted argument
// in any but the final position, and so must this.
bool list_append_copy(unsigned *head, unsigned *tail, unsigned list,
                      const char *name)
{
    GC_PROTECT_GUARD;
    gc_protect(head);
    gc_protect(tail);
    gc_protect(&list);
    unsigned cursor = list;
    gc_protect(&cursor);

    while (cursor) {
        if (!IS_PAIR(cursor)) {
            show_error("%s: improper list", name);
            return false;
        }
        unsigned cell;
        {
            unsigned elem = car(cursor);
            GC_PROTECT_GUARD;
            gc_protect(&elem);
            cell = alloc();
            CELL_TYPE(cell) = BT_CONS;
            CELL_CAR(cell) = elem; // elem is up to date after any collection
            CELL_CDR(cell) = 0;
        }
        if (!*head) {
            *head = *tail = cell;
        } else {
            cell_set_cdr(*tail, cell);
            *tail = cell;
        }
        cursor = cdr(cursor);
    }
    return true;
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
} equal_worklist;

typedef struct {
    equal_seen_pair *slots;
    size_t len;
    size_t cap;
} equal_seen_set;

static size_t equal_seen_hash(unsigned a, unsigned b)
{
    uint64_t h = (uint64_t)a * UINT64_C(0x9e3779b185ebca87);
    h ^= (uint64_t)b * UINT64_C(0xc2b2ae3d27d4eb4f);
    h ^= h >> 33;
    return (size_t)h;
}

static bool equal_seen_resize(equal_seen_set *seen, size_t new_cap)
{
    if (new_cap > UINT_MAX)
        return false;
    equal_seen_pair *new_slots =
        checked_calloc_array((unsigned)new_cap, sizeof(*new_slots));
    if (!new_slots)
        return false;

    for (size_t i = 0; i < seen->cap; i++) {
        equal_seen_pair pair = seen->slots[i];
        if (pair.a == 0)
            continue;
        size_t slot = equal_seen_hash(pair.a, pair.b) & (new_cap - 1);
        while (new_slots[slot].a != 0)
            slot = (slot + 1) & (new_cap - 1);
        new_slots[slot] = pair;
    }
    free(seen->slots);
    seen->slots = new_slots;
    seen->cap = new_cap;
    return true;
}

static bool equal_seen_contains(equal_seen_set *seen, unsigned a, unsigned b)
{
    if (seen->cap == 0)
        return false;
    size_t slot = equal_seen_hash(a, b) & (seen->cap - 1);
    while (seen->slots[slot].a != 0) {
        if (seen->slots[slot].a == a && seen->slots[slot].b == b)
            return true;
        slot = (slot + 1) & (seen->cap - 1);
    }
    return false;
}

static bool equal_seen_add(equal_seen_set *seen, unsigned a, unsigned b)
{
    if (a == 0)
        return false;
    if (seen->cap == 0 || (seen->len + 1) * 4 > seen->cap * 3) {
        size_t new_cap = seen->cap == 0 ? 32 : seen->cap * 2;
        if (new_cap <= seen->cap || !equal_seen_resize(seen, new_cap))
            return false;
    }
    size_t slot = equal_seen_hash(a, b) & (seen->cap - 1);
    while (seen->slots[slot].a != 0) {
        if (seen->slots[slot].a == a && seen->slots[slot].b == b)
            return true;
        slot = (slot + 1) & (seen->cap - 1);
    }
    seen->slots[slot] = (equal_seen_pair){a, b};
    seen->len++;
    return true;
}

static bool equal_worklist_push(equal_worklist *work, unsigned a, unsigned b)
{
    if (work->len == work->cap) {
        size_t new_cap;
        if (work->cap == 0) {
            new_cap = 16;
        } else if (!checked_grow_capacity_size(work->cap,
                                                sizeof(*work->items),
                                                &new_cap)) {
            return false;
        }
        size_t alloc_size;
        if (!checked_flex_size(0, new_cap, sizeof(*work->items),
                               &alloc_size))
            return false;
        equal_seen_pair *new_items =
            checked_realloc_size(work->items, alloc_size);
        if (!new_items)
            return false;
        work->items = new_items;
        work->cap = new_cap;
    }
    work->items[work->len++] = (equal_seen_pair){a, b};
    return true;
}

static bool deep_equal_seen(unsigned a, unsigned b, equal_seen_set *seen)
{
    equal_worklist pending = {0};
    bool result = equal_worklist_push(&pending, a, b);

    while (result && pending.len > 0) {
        equal_seen_pair pair = pending.items[--pending.len];
        a = pair.a;
        b = pair.b;
        if (a == b)
            continue;
        if (a == 0 || b == 0) {
            result = false;
            break;
        }
        if (IS_FIXNUM(a)) {
            if (!IS_NUM(b) || CELL_ID(b) != (int64_t)FIXNUM_VALUE(a))
                result = false;
            continue;
        }
        if (IS_FIXNUM(b)) {
            if (!IS_NUM(a) || CELL_ID(a) != (int64_t)FIXNUM_VALUE(b))
                result = false;
            continue;
        }
        if (!IS_CELL(a) || !IS_CELL(b) || CELL_TYPE(a) != CELL_TYPE(b)) {
            result = false;
            break;
        }

        switch (CELL_TYPE(a)) {
        case BT_NUM:
        case BT_CHAR:
        case BT_ATOM:
        case BT_INEXACT:
            result = CELL_ID(a) == CELL_ID(b);
            break;
        case BT_BIGNUM: {
            bignum *ba = get_bignum(a);
            bignum *bb = get_bignum(b);
            result = ba && bb && bn_cmp(ba, bb) == 0;
            break;
        }
        case BT_RATIONAL:
        case BT_COMPLEX:
        case BT_CONS:
            if (equal_seen_contains(seen, a, b))
                break;
            if (!equal_seen_add(seen, a, b) ||
                !equal_worklist_push(&pending, cdr(a), cdr(b)) ||
                !equal_worklist_push(&pending, car(a), car(b)))
                result = false;
            break;
        case BT_STRING: {
            char *sa = GET_STRING_PTR(a);
            char *sb = GET_STRING_PTR(b);
            result = string_is_registered(sa) && string_is_registered(sb) &&
                     strcmp(sa, sb) == 0;
            break;
        }
        case BT_BYTEVEC: {
            bytevec_data *ba = (bytevec_data *)CELL_PTR(a);
            bytevec_data *bb = (bytevec_data *)CELL_PTR(b);
            result = bytevec_data_well_formed(ba) &&
                     bytevec_data_well_formed(bb) && ba->len == bb->len &&
                     memcmp(ba->data, bb->data, ba->len) == 0;
            break;
        }
        case BT_VECTOR: {
            unsigned len_a = vector_len(a);
            unsigned len_b = vector_len(b);
            if (len_a != len_b) {
                result = false;
                break;
            }
            if (equal_seen_contains(seen, a, b))
                break;
            unsigned *da = vector_data_ptr(a);
            unsigned *db = vector_data_ptr(b);
            if ((len_a != 0 && !da) || (len_b != 0 && !db) ||
                !equal_seen_add(seen, a, b)) {
                result = false;
                break;
            }
            for (unsigned i = 0; result && i < len_a; i++)
                result = equal_worklist_push(&pending, da[i], db[i]);
            break;
        }
        case BT_RECORD: {
            // A record is equal? only to itself (as in MIT), except that
            // char-sets compare by contents, as they did before they were
            // records and as MIT's do. A finite set's payload is its range
            // list, kept sorted and coalesced, so equal lists are equal sets;
            // a predicate-based set's payload is a procedure, which only
            // equals itself.
            unsigned kind_a, payload_a, kind_b, payload_b;
            if (!char_set_record_fields(a, &kind_a, &payload_a) ||
                !char_set_record_fields(b, &kind_b, &payload_b) ||
                !equal_worklist_push(&pending, payload_a, payload_b) ||
                !equal_worklist_push(&pending, kind_a, kind_b))
                result = false;
            break;
        }
        default:
            result = false;
            break;
        }
    }

    free(pending.items);
    return result;
}

bool deep_equal(unsigned a, unsigned b)
{
    equal_seen_set seen = {0};
    bool result = deep_equal_seen(a, b, &seen);
    free(seen.slots);
    return result;
}

// ============================================================================
// Garbage Collection
// ============================================================================

// Both collectors copy and trace in separate steps. collect() and
// collect_to_old() only move one cell and leave a forwarding pointer behind;
// the copy's references are traced later, when the collector's Cheney scan
// loop reaches it (gc_scan_cell). Tracing a vector's elements, a hash table's
// entries or a continuation's stack at copy time instead recursed once per
// level of nesting, so a long enough chain overflowed the C stack inside the
// collector - a 300K-node linked list of records (records are vectors) was
// enough. Every copied cell is scanned exactly once, so the walk is bounded by
// the to-space, not by the C stack.

// Move x to dest and leave a forwarding pointer. Pointer-bearing cells share
// their malloc'd payload with the copy; the sweep frees only payloads whose
// cell was never forwarded.
static inline unsigned forward_cell(unsigned x, unsigned dest)
{
    ctx.cons_cells[dest] = ctx.cons_cells[x];
    CELL_TYPE(x) = BT_BROKENHEART;
    CELL_CAR(x) = dest;
    return dest;
}

// Replace every reference held by cell with collector(reference). Called for
// each cell a collection copies, and by a minor collection for each old cell
// on a dirty card.
static void gc_scan_cell(unsigned cell, unsigned (*collector)(unsigned))
{
    enum lisp_type t = CELL_TYPE(cell);
    if (cell_type_has_children(t)) {
        CELL_CAR(cell) = collector(CELL_CAR(cell));
        CELL_CDR(cell) = collector(CELL_CDR(cell));
        return;
    }
    switch (t) {
    // Records keep their fields (and an instance its type) in a vector
    // payload. These cases matter beyond tracing a copy: the minor
    // collection's dirty-card scan comes through here too, and without them an
    // old record mutated to point at a nursery object would keep a dangling
    // reference after the nursery is reclaimed.
    case BT_RECORD:
    case BT_RECORD_TYPE:
    case BT_VECTOR: {
        vector_data *vd = (vector_data *)CELL_PTR(cell);
        if (!vector_data_well_formed(vd))
            return;
        for (unsigned i = 0; i < vd->len; i++)
            vd->data[i] = collector(vd->data[i]);
        return;
    }
    case BT_HASHTABLE: {
        hash_table_data *ht = (hash_table_data *)CELL_PTR(cell);
        if (!hash_table_data_well_formed(ht))
            return;
        for (unsigned i = 0; i < ht->capacity; i++) {
            for (hash_entry *entry = ht->buckets[i]; entry;
                 entry = entry->next) {
                entry->key = collector(entry->key);
                entry->value = collector(entry->value);
            }
        }
        // eq/eqv tables hash compound keys by cell index, so moved keys now
        // belong in other buckets. Rehash once the heap is consistent again.
        hash_table_gc_register_rehash(ht);
        return;
    }
    case BT_STRING:
        // A string view keeps its parent string alive and in sync.
        if (string_cell_is_view(cell)) {
            string_view_data *view = (string_view_data *)CELL_PTR(cell);
            view->parent = collector(view->parent);
        }
        return;
    case BT_VMCONT:
        if (is_vm_continuation_object(cell))
            collect_vm_continuation_roots((vm_continuation *)CELL_PTR(cell),
                                          collector);
        return;
    default:
        // String ports' source strings are forwarded for every registered
        // port by update_all_string_port_sources. Other types hold no cell
        // references.
        return;
    }
}

unsigned collect(unsigned x)
{
    // Reserved cells (< HEAP_RESERVED) are permanent and never collected
    if (x < HEAP_RESERVED)
        return x;
    // Fixnums are immediate values, not heap pointers
    if (IS_FIXNUM(x))
        return x;
    // Preserve malformed non-cell values rather than dereferencing an index
    // outside the managed heap.  This also keeps collection robust when a
    // foreign caller supplies an invalid root.
    if (!IS_CELL(x))
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

    default:
        // Copy only; the scan loop in gc() traces the copy's references.
        return forward_cell(x, alloc());
    }
}


// Forward every registered string port's retained source string.
//
// A string input port keeps its source as a cell reference inside a malloc'd
// string_port, not inside the port cell itself. The collectors only forward it
// while copying the port cell, and the card table's dirty scan does not cover
// port cells at all, so two cases were left stale:
//
//   * open-input-string fills in source_string and only then allocates the
//     port cell with make_pointer_cell. A collection during that allocation
//     moves the source while no cell yet refers to the port, so nothing
//     forwards the reference.
//   * an old-generation port whose source still lives in the nursery is never
//     visited by a minor collection.
//
// Walking the registry covers both: registration is exactly the set of live
// string_port structs, and it does not depend on the port having a cell yet.
static void update_all_string_port_sources(unsigned (*collector)(unsigned))
{
    for (size_t i = 0; i < string_port_registry.bucket_count; i++) {
        for (ptr_registry_node *node = string_port_registry.buckets[i]; node;
             node = node->next) {
            string_port *sp = (string_port *)node->ptr;
            if (!sp || sp->source_string == ctx.atom_false)
                continue;
            sp->source_string = collector(sp->source_string);
        }
    }
}

unsigned gc(unsigned root)
{
    ctx.major_gc_count++;
    collection_in_progress = true;

    // Enter GC mode - alloc() will use old gen instead of nursery
    in_gc_mode = true;

    // Prevent recursive GC during collection
    unsigned *saved_gc_root = alloc_gc_root;
    alloc_gc_root = NULL;

    // The parts of the space being reclaimed that hold allocated cells, for
    // VESPER_GC_VERIFY.
    unsigned from_old_start = ctx.mmin, from_old_end = ctx.hptr;
    unsigned from_nursery_start = ctx.nursery_start;
    unsigned from_nursery_end = ctx.nursery_ptr;

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
    // The allocation root is a root like any other and must be copied before
    // the scan loop below, or what only it reaches is never traced. (It
    // usually equals root - callers pass *alloc_gc_root - in which case this
    // just reads the forwarding pointer.)
    if (saved_gc_root && *saved_gc_root >= HEAP_RESERVED)
        *saved_gc_root = collect(*saved_gc_root);

    // Collect trampoline state (global evaluator state)
    tramp.expr = collect(tramp.expr);
    tramp.env = collect(tramp.env);
    tramp.cont = collect(tramp.cont);
    tramp.value = collect(tramp.value);
    ctx.current_input_cell = collect(ctx.current_input_cell);
    ctx.current_output_cell = collect(ctx.current_output_cell);
    ctx.current_error_cell = collect(ctx.current_error_cell);
    ctx.r7rs_environment = collect(ctx.r7rs_environment);
    ctx.global_environment = collect(ctx.global_environment);
    for (int i = 0; i < BRT_COUNT; i++)
        ctx.builtin_record_types[i] = collect(ctx.builtin_record_types[i]);
    reader_update_datum_labels(collect);

    // Collect shadow stack entries - these are pointers to local C variables
    // that hold cell IDs. After GC, we update them to point to new locations.
    for (int i = 0; i < gc_shadow_stack_top; i++) {
        if (gc_shadow_stack[i] && *gc_shadow_stack[i] >= HEAP_RESERVED) {
            *gc_shadow_stack[i] = collect(*gc_shadow_stack[i]);
        }
    }
    gc_update_active_pattern_state();

    // Update VM roots for the whole chain of active and suspended VMs
    for (vm_state *vm = get_active_vm(); vm; vm = vm->parent)
        gc_update_vm_roots(vm);

    // Collect code object constants BEFORE scan phase so their
    // CAR/CDR are recursively processed
    gc_update_all_code_objects();
    gc_update_all_patterns();
    update_all_string_port_sources(collect);
    env_index_gc_update(collect);

    while (scan != ctx.hptr)
        gc_scan_cell(scan++, collect);
    weak_cell_maps_after_gc(true);

    unsigned tmp = ctx.nmin;
    ctx.nmin = ctx.mmin;
    ctx.mmin = tmp;

    // Note: ctx.atom_* are in reserved space (< HEAP_RESERVED) and don't need
    // collection

    alloc_gc_root = saved_gc_root;

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
    // Only the cells on the external lists are visited (see "External payload
    // tracking"); old_end covers the ENTIRE old semispace including its
    // nursery.
    unsigned old_start = ctx.nmin;
    unsigned old_end =
        (ctx.nmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
    if (gc_verify_external && !external_tracking_lost) {
        verify_external_tracked(from_old_start, from_old_end, "major");
        verify_external_tracked(from_nursery_start, from_nursery_end, "major");
    }
    sweep_semispace_external(old_start, old_end, ctx.mmin, ctx.hptr);

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

    // Heap is consistent again: rebuild buckets of tables whose keys moved
    hash_table_gc_rehash_pending();

    // Exit GC mode
    in_gc_mode = false;

    // Invalidate environment lookup cache (cell references are now stale)
    env_invalidate_cache();
    collection_in_progress = false;

    // If live data reaches into the nursery region, the next allocation would
    // silently overwrite it - fail loudly instead. The collection itself has
    // finished and the heap is consistent, so unlike a failure partway through
    // copying this panic is recoverable: the REPL's recovery runs a full
    // collection before anything allocates, and by then the data of the
    // abandoned computation is unreachable.
    if (ctx.hptr > ctx.nursery_start)
        lisp_panic("heap exhausted: live data overlaps nursery after GC");

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
    if (!IS_CELL(x))
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

    default:
        // minor_gc() checks before it starts that the old generation can
        // take every nursery cell, so this is only a backstop.
        if (ctx.hptr >= ctx.nursery_start)
            lisp_panic("old generation full during minor GC");
        // Copy only; minor_gc()'s scan loop traces the copy's references.
        return forward_cell(x, ctx.hptr++);
    }
}

unsigned minor_gc(unsigned root)
{
    // Every nursery cell might survive. When the old generation cannot take
    // them all, promotion would run out of room partway through, with some of
    // the nursery forwarded and some not - a heap nothing can recover. A full
    // collection copies into the whole other semispace instead, so use one.
    unsigned nursery_used = ctx.nursery_ptr - ctx.nursery_start;
    if (ctx.hptr > ctx.nursery_start ||
        ctx.nursery_start - ctx.hptr < nursery_used)
        return gc(root);

    ctx.minor_gc_count++;
    collection_in_progress = true;
    unsigned scan = ctx.hptr; // Start of newly promoted objects

    // Collect root
    root = collect_to_old(root);
    // Callers pass *alloc_gc_root as root; promote it explicitly anyway so a
    // different root cannot leave it pointing into the reclaimed nursery.
    if (alloc_gc_root && *alloc_gc_root)
        *alloc_gc_root = collect_to_old(*alloc_gc_root);

    // Collect trampoline state
    tramp.expr = collect_to_old(tramp.expr);
    tramp.env = collect_to_old(tramp.env);
    tramp.cont = collect_to_old(tramp.cont);
    tramp.value = collect_to_old(tramp.value);
    ctx.current_input_cell = collect_to_old(ctx.current_input_cell);
    ctx.current_output_cell = collect_to_old(ctx.current_output_cell);
    ctx.current_error_cell = collect_to_old(ctx.current_error_cell);
    ctx.r7rs_environment = collect_to_old(ctx.r7rs_environment);
    ctx.global_environment = collect_to_old(ctx.global_environment);
    for (int i = 0; i < BRT_COUNT; i++)
        ctx.builtin_record_types[i] =
            collect_to_old(ctx.builtin_record_types[i]);
    reader_update_datum_labels(collect_to_old);

    // Collect shadow stack entries
    for (int i = 0; i < gc_shadow_stack_top; i++) {
        if (gc_shadow_stack[i]) {
            *gc_shadow_stack[i] = collect_to_old(*gc_shadow_stack[i]);
        }
    }
    minor_gc_update_active_pattern_state();

    // Update VM roots if VM is active
    for (vm_state *vm = get_active_vm(); vm; vm = vm->parent)
        gc_update_vm_roots_minor(vm, collect_to_old);

    // Update code object constants - they may point to nursery cells.
    minor_gc_update_all_code_objects();
    minor_gc_update_all_patterns();
    update_all_string_port_sources(collect_to_old);
    env_index_gc_update(collect_to_old);

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
            for (unsigned cell = cell_start; cell < cell_end; cell++)
                gc_scan_cell(cell, collect_to_old);
            // Clear the card
            CARD_CLEAR(ctx.card_table, card);
        }
    } // end if (ctx.card_table)

    // Scan newly promoted objects in old gen
    while (scan != ctx.hptr)
        gc_scan_cell(scan++, collect_to_old);
    weak_cell_maps_after_gc(false);

    // Free external data for unreachable nursery cells. Promoted cells were
    // rewritten as BT_BROKENHEART, so their payloads go with the copies.
    // Only the cells on the young external list are visited (see "External
    // payload tracking"), not the whole nursery: that walk was ~5% of the
    // run time of allocation-heavy loops.
    if (gc_verify_external && !external_tracking_lost)
        verify_external_tracked(ctx.nursery_start, ctx.nursery_ptr, "minor");
    sweep_nursery_external(ctx.nursery_start, ctx.nursery_ptr);

    // Reset nursery - all survivors are now in old gen
    ctx.nursery_ptr = ctx.nursery_start;

    // Heap is consistent again: rebuild buckets of tables whose keys moved
    hash_table_gc_rehash_pending();

    // Invalidate environment lookup cache (cell references may have moved)
    env_invalidate_cache();
    collection_in_progress = false;

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

// #t and #f are atoms so the evaluator and writer treat them as constants,
// but they must not share an atom id with any symbol: eq?, case, memq,
// hashing and macro pattern variables all compare atoms by id, so interning
// them as "true"/"false" made (eq? 'true #t) true and let a pattern variable
// named true substitute into a literal #t. A private uninterned slot cannot be
// reached by intern or string->symbol.
static void init_boolean_atom(unsigned cell_id, const char *name)
{
    CELL_TYPE(cell_id) = BT_ATOM;
    CELL_ID(cell_id) = atom_slot(name, true);
}

// Record each core keyword in atom_core_forms and give it its core alias: a
// permanent atom in reserved space (so it needs no GC root) whose private slot
// prints as the keyword, like the booleans' (see enum core_form).
static void init_core_forms(void)
{
    static const char *const names[CF_COUNT] = {
        [CF_QUOTE] = "quote",
        [CF_QUASIQUOTE] = "quasiquote",
        [CF_UNQUOTE] = "unquote",
        [CF_UNQUOTE_SPLICING] = "unquote-splicing",
        [CF_LAMBDA] = "lambda",
        [CF_IF] = "if",
        [CF_DEFINE] = "define",
        [CF_SET] = "set!",
        [CF_BEGIN] = "begin",
        [CF_LET] = "let",
        [CF_LETSTAR] = "let*",
        [CF_LETREC] = "letrec",
        [CF_COND] = "cond",
        [CF_AND] = "and",
        [CF_OR] = "or",
        [CF_COND_EXPAND] = "cond-expand",
        [CF_DEFINE_SYNTAX] = "define-syntax",
        [CF_LET_SYNTAX] = "let-syntax",
        [CF_LETREC_SYNTAX] = "letrec-syntax",
        [CF_SYNTAX_RULES] = "syntax-rules",
        [CF_DEFINE_MACRO] = "define-macro",
        [CF_ELSE] = "else",
        [CF_ARROW] = "=>",
    };
    for (int form = CF_NONE + 1; form < CF_COUNT; form++) {
        int64_t id = intern(names[form]);
        core_form_keywords[form] = id;
        atom_core_forms[id] = (uint8_t)form;
        unsigned alias = core_alias_cell(form);
        CELL_TYPE(alias) = BT_ATOM;
        CELL_ID(alias) = atom_slot(names[form], true);
        atom_core_forms[CELL_ID(alias)] = (uint8_t)(form | CORE_ALIAS_FLAG);
    }
    CELL_TYPE(CELL_EXPANDED_LAMBDA) = BT_ATOM;
    CELL_ID(CELL_EXPANDED_LAMBDA) = atom_slot(names[CF_LAMBDA], true);
    atom_core_forms[CELL_ID(CELL_EXPANDED_LAMBDA)] =
        (uint8_t)(CF_LAMBDA | CORE_ALIAS_FLAG);
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

    // The unique eof object lives in reserved space (never GC'd)
    CELL_TYPE(CELL_EOF_OBJECT) = BT_EOF;
    CELL_ID(CELL_EOF_OBJECT) = 0;

    // So is the unassigned marker (see CELL_UNASSIGNED in types.h)
    CELL_TYPE(CELL_UNASSIGNED) = BT_UNASSIGNED;
    CELL_ID(CELL_UNASSIGNED) = 0;

    // Initialize permanent atoms in reserved cells (never GC'd)
    init_boolean_atom(CELL_ATOM_FALSE, "#f");
    init_boolean_atom(CELL_ATOM_TRUE, "#t");
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
    // This marker is private syntax metadata, never a user-spellable symbol.
    ctx.kw_protected = atom_slot("##protected##", true);

    init_core_forms();
}
