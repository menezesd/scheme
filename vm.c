/**
 * @file vm.c
 * @brief Stack-based virtual machine for compiled Scheme bytecode
 *
 * Executes bytecode produced by the compiler. Uses a value stack for
 * operands and a call stack for function invocations.
 *
 * ## Execution Model
 * - Fetch-decode-execute loop
 * - Stack-based: operations pop operands, push results
 * - Tail calls reuse the current frame (no stack growth)
 * - Continuations capture entire VM state
 */

#include "bytecode.h"
#include "compiled_pattern.h"
#include "compile_internal.h"
#include "context.h"
#include "env.h"
#include "eval.h"
#include "eval_internal.h"
#include "macros.h"
#include "prim_internal.h"
#include "primitives.h"
#include "reader.h"
#include "writer.h"
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ============================================================================
// VM Constants
// ============================================================================

#define INITIAL_STACK_SIZE 1024
#define INITIAL_FRAMES_SIZE 256
#define MAX_STACK_SIZE VM_MAX_STACK_SIZE
#define MAX_FRAMES_SIZE VM_MAX_FRAMES_SIZE

static void protect_and_box2(unsigned *a, unsigned *b)
{
    gc_protect(a);
    gc_protect(b);
    *a = ensure_boxed(*a);
    *b = ensure_boxed(*b);
}

static void protect_and_box3(unsigned *a, unsigned *b, unsigned *c)
{
    gc_protect(a);
    gc_protect(b);
    gc_protect(c);
    *a = ensure_boxed(*a);
    *b = ensure_boxed(*b);
    *c = ensure_boxed(*c);
}

// Error handling macro - reduces boilerplate in dispatch loop
#define VM_ERROR(vm, msg)                                                      \
    do {                                                                       \
        (vm)->error = true;                                                    \
        (vm)->error_msg = (msg);                                               \
        (vm)->running = false;                                                 \
    } while (0)

// Error handling with break for use in switch cases
#define VM_ERROR_BREAK(vm, msg)                                                \
    do {                                                                       \
        VM_ERROR(vm, msg);                                                     \
        goto vm_dispatch_error;                                                \
    } while (0)

static vector_data *vm_expect_vector_data(vm_state *vm, unsigned vec,
                                          const char *msg)
{
    if (!IS_VECTOR(vec)) {
        VM_ERROR(vm, msg);
        return NULL;
    }
    vector_data *vd = GET_VECTOR_PTR(vec);
    if (!vector_data_well_formed(vd)) {
        VM_ERROR(vm, msg);
        return NULL;
    }
    return vd;
}

static bool vm_expect_vector_index(vm_state *vm, unsigned vec, unsigned idx,
                                   const char *not_vector_msg,
                                   const char *not_integer_msg,
                                   const char *out_of_bounds_msg,
                                   vector_data **vd_out, unsigned *idx_out)
{
    vector_data *vd = vm_expect_vector_data(vm, vec, not_vector_msg);
    if (!vd)
        return false;

    int64_t i = 0;
    if (IS_FIXNUM(idx)) {
        i = FIXNUM_VALUE(idx);
    } else if (IS_NUM(idx)) {
        i = CELL_ID(idx);
    } else {
        VM_ERROR(vm, not_integer_msg);
        return false;
    }

    if (i < 0 || (uint64_t)i >= vd->len) {
        VM_ERROR(vm, out_of_bounds_msg);
        return false;
    }

    if (vd_out)
        *vd_out = vd;
    *idx_out = (unsigned)i;
    return true;
}

static bool instruction_boundary(const unsigned char *boundaries,
                                 unsigned offset)
{
    return (boundaries[offset / CHAR_BIT] & (1u << (offset % CHAR_BIT))) != 0;
}

// Only meaningful after vm_code_is_well_formed(code) succeeded, which is what
// fills the boundary map; anything else is treated as "not a boundary".
static bool vm_instruction_starts_at(const code_object *code, unsigned target)
{
    return code && code->verified && code->instruction_boundaries &&
           target <= code->code_len &&
           instruction_boundary(code->instruction_boundaries, target);
}

static bool vm_code_is_well_formed(code_object *code)
{
    // Code objects are GC-managed and their backing arrays are only valid
    // while the object remains registered.  Check the metadata before
    // inspecting any array so malformed C API inputs cannot cause an
    // out-of-bounds access below.
    if (!code || !code_object_is_registered(code) || !code->code ||
        code->code_len == 0 || code->code_len > code->code_cap ||
        code->const_len > code->const_cap ||
        code->children_len > code->children_cap ||
        (code->const_len != 0 && !code->constants) ||
        (code->children_len != 0 && !code->children))
        return false;
    if (code->verified)
        return true;

    free(code->instruction_boundaries);
    code->instruction_boundaries = NULL;
    unsigned char *boundaries =
        checked_calloc_array((size_t)code->code_len / CHAR_BIT + 1,
                             sizeof(*boundaries));
    if (!boundaries)
        return false;

    for (unsigned i = 0; i < code->code_len;) {
        unsigned size = instruction_size(code->code[i]);
        if (size == 0 || size > code->code_len - i)
            goto invalid;
        boundaries[i / CHAR_BIT] |= (unsigned char)(1u << (i % CHAR_BIT));
        i += size;
    }
    unsigned end = code->code_len;
    boundaries[end / CHAR_BIT] |= (unsigned char)(1u << (end % CHAR_BIT));

    for (unsigned i = 0; i < end;) {
        unsigned op = code->code[i];
        if (is_jump_opcode(op)) {
            unsigned target = code->code[i + 1];
            if (target > end || !instruction_boundary(boundaries, target))
                goto invalid;
        }
        i += instruction_size(op);
    }
    code->instruction_boundaries = boundaries;
    code->verified = true;
    return true;

invalid:
    free(boundaries);
    return false;
}

static bool vm_get_const(code_object *code, unsigned idx, unsigned *val_out)
{
    if (!code || idx >= code->const_len)
        return false;
    *val_out = code->constants[idx];
    return true;
}

static bool vm_local_index(vm_state *vm, unsigned slot, unsigned *idx_out)
{
    if (slot > UINT_MAX - vm->bp || vm->bp + slot >= vm->sp) {
        VM_ERROR(vm, "local slot out of bounds");
        return false;
    }
    *idx_out = vm->bp + slot;
    return true;
}

static unsigned vm_unbox_fixnum_if_possible(unsigned val)
{
    if (IS_NUM(val)) {
        int64_t n = CELL_ID(val);
        if (FITS_FIXNUM(n))
            return MAKE_FIXNUM((int32_t)n);
    }
    return val;
}

static unsigned vm_store_integer_result(int64_t n)
{
    return FITS_FIXNUM(n) ? MAKE_FIXNUM((int32_t)n) : store(n);
}

static void vm_show_undefined_variable(int64_t sym_id)
{
    if (sym_id >= 0 && (unsigned)sym_id < ctx.atom_table_cap &&
        ctx.atom_table[sym_id]) {
        show_error("undefined variable: %s", ctx.atom_table[sym_id]);
    } else {
        show_error("undefined variable: <id %ld>", (long)sym_id);
    }
}

static void vm_deopt_binary_op(vm_state *vm, unsigned opcode, unsigned a,
                               unsigned b)
{
    vm->code->code[vm->ip - 1] = opcode;
    vm_push(vm, a);
    vm_push(vm, b);
    vm->ip--;
}

// n + delta for delta = 1 or -1: (+ n 1) or (- n 1), the slow path of
// OP_ADD1 and OP_SUB1. A machine integer is done here; anything else,
// overflow included, goes through the generic primitive. TOK_ERROR,
// reported, if n is not a number.
static unsigned vm_add_one(unsigned n, int delta)
{
    if (IS_FIXNUM(n))
        return vm_store_integer_result((int64_t)FIXNUM_VALUE(n) + delta);
    if (IS_NUM(n) && (delta > 0 ? CELL_ID(n) < INT64_MAX
                                : CELL_ID(n) > INT64_MIN))
        return vm_store_integer_result(CELL_ID(n) + delta);
    GC_GUARD;
    n = ensure_boxed(n);
    gc_protect(&n);
    unsigned one = store(1);
    unsigned argv[2] = {n, one};
    gc_protect(&argv[0]);
    gc_protect(&argv[1]);
    return delta > 0 ? prim_plus(2, argv) : prim_minus(2, argv);
}

static unsigned vm_store_bignum_neg(vm_state *vm, const bignum *bn,
                                    const char *name)
{
    bignum *result = bn_neg(bn);
    if (!result) {
        VM_ERROR(vm, name);
        return TOK_ERROR;
    }
    return store_integer(result);
}

// (= n 0) for an n that is not a fixnum. TOK_ERROR, reported, if n is not a
// number.
static unsigned vm_numeq_zero(unsigned n)
{
    GC_GUARD;
    n = ensure_boxed(n);
    gc_protect(&n);
    unsigned zero = store(0);
    gc_protect(&zero);
    return binary_numeq(n, zero);
}

// Type check macro that breaks on failure (handles fixnums safely)
#define VM_CHECK_PAIR(vm, val, msg)                                            \
    do {                                                                       \
        if (IS_FIXNUM(val) || !IS_PAIR(val))                                   \
            VM_ERROR_BREAK(vm, msg);                                           \
    } while (0)

// ============================================================================
// Inline Cached Lookup
// ============================================================================
//
// OP_LOOKUP and OP_GUARD_PRIMITIVE each carry two cache
// words. The compiler sets the first to 0xFFFFFFFF; once the instruction has
// run it indexes code->ic_entries, whose entry records where the last full
// lookup found the binding - its value cell, the frame holding it, and that
// frame's depth and offset - with the environment it was found from and the
// name's binding stamp at the time (env.h). The second word is unused.
//
// While the stamp is unchanged, none of the frames the entry was validated
// against has gained a binding for the name, and no collection has moved a
// cell, so:
//
//   - From the same environment, the cached cell is the answer: one compare
//     and one load, the common case for loops and global procedures.
//   - From a different environment - another activation of the same code -
//     the frames between this instruction and the binding come from the same
//     lambdas and bodies as before, and differ only in what internal defines
//     have added to them. Until the name has been defined in a nested frame
//     somewhere (env_binding_dynamic), no such define can be shadowing it,
//     so the binding frame is found by following `depth` links, stopping
//     early where the chain joins the one already validated. That frame is
//     the same one (a global, a closure's captured frame) or another
//     activation of it, which must hold the name at `offset`.
//   - Once the name has been defined in a nested frame, the frames of this
//     environment that the validated one does not share are searched for it
//     as well - which is what every hit used to do for every frame.
//
// Anything else falls back to the full lookup, which refills the entry. An
// entry belongs to the instruction whose cache word indexes it (`word`),
// whose name never changes. The cell indices in an entry are not GC roots;
// a collection changes every stamp, so they are never used after one.
//
// This rests on how environments are built and changed. A code object only
// runs in environments laid out by its own lexical structure: top-level and
// eval code runs once, in the environment it was compiled for, and a closure
// runs in its captured environment extended by its own frames, whose
// variables come from its parameter list (or start empty, for a let body).
// After that, environments change shape only through env.c: defvar adds
// bindings (and changes stamps), nothing removes them. Rewiring a frame or
// an environment spine with set-car!/set-cdr! is not tracked - as it is not
// by env.c's frame index - although assigning a binding's value that way is:
// the cell is read on every hit.

#define IC_NO_OFFSET 0xFFFFFFFFu // binding found through a frame index

struct vm_ic_entry {
    uint64_t stamp;      // env_binding_stamp(name) when last validated
    unsigned word;       // the cache word owning this entry
    unsigned env;        // environment last validated
    unsigned cell;       // the binding's value cell
    unsigned frame;      // frame holding it, seen from env
    unsigned depth : 31; // links from env to frame
    unsigned probe : 1;  // search unshared frames (env_binding_dynamic)
    unsigned offset;     // position of the binding in frame, or IC_NO_OFFSET
};
_Static_assert(sizeof(struct vm_ic_entry) == 32, "two entries per cache line");

static bool vm_try_deref_binding_value(unsigned val, unsigned *out)
{
    if (IS_BINDING_REF(val)) {
        unsigned target = CELL_CAR(val);
        if (!IS_PAIR(target))
            return false;
        *out = car(target);
        return true;
    }
    *out = val;
    return true;
}

static bool vm_try_env_frame(unsigned env_cell, unsigned *frame_out,
                             unsigned *next_out)
{
    if (!IS_PAIR(env_cell))
        return false;
    unsigned frame = car(env_cell);
    if (!IS_PAIR(frame))
        return false;
    *frame_out = frame;
    *next_out = cdr(env_cell);
    return true;
}

typedef enum {
    IC_ABSENT, // the frame does not bind the name
    IC_FOUND,  // it does, and *value/*cell/*offset say where
    IC_BAD,    // it is malformed, or its binding cannot be read
} ic_frame_result;

// Search one frame the way lookup does: through its side index when it is
// large enough to have one, else by walking its binding list.
static ic_frame_result ic_find_in_frame(unsigned frame, int64_t symbol,
                                        unsigned *value, unsigned *cell_out,
                                        unsigned *offset_out)
{
    unsigned cell = 0;
    if (env_frame_index_lookup(frame, symbol, &cell)) {
        if (!cell)
            return IC_ABSENT;
        if (!IS_PAIR(cell) || !vm_try_deref_binding_value(car(cell), value))
            return IC_BAD;
        *cell_out = cell;
        *offset_out = IC_NO_OFFSET;
        return IC_FOUND;
    }
    unsigned vars = car(frame);
    unsigned vals = cdr(frame);
    if (!env_binding_list_acyclic(vars))
        return IC_BAD;
    for (unsigned i = 0; vars; i++) {
        bool rest = IS_ATOM(vars);
        if ((!rest && !IS_PAIR(vars)) || !IS_PAIR(vals))
            return IC_BAD;
        unsigned var = rest ? vars : car(vars);
        if (!IS_ATOM(var))
            return IC_BAD;
        if (CELL_ID(var) == symbol) {
            if (!vm_try_deref_binding_value(car(vals), value))
                return IC_BAD;
            *cell_out = vals;
            *offset_out = i;
            return IC_FOUND;
        }
        if (rest)
            break;
        vars = cdr(vars);
        vals = cdr(vals);
    }
    return IC_ABSENT;
}

// Below this many bindings a frame is searched directly: env.c indexes only
// frames several times larger, so its index has nothing to say about one, and
// a walk that reaches the end of the variable list has itself shown the list
// is not cyclic.
#define VM_SHORT_FRAME 16

// Can frame bind symbol? False only when it certainly does not - which is
// when the full lookup would pass over it too. Anything else, a binding or a
// frame too malformed to be sure about, is left to the full lookup.
static bool ic_frame_may_bind(unsigned frame, int64_t symbol)
{
    if (!IS_PAIR(frame))
        return true;
    unsigned vars = car(frame);
    for (unsigned n = 0; IS_PAIR(vars); vars = cdr(vars)) {
        unsigned var = car(vars);
        if (!IS_ATOM(var) || CELL_ID(var) == symbol)
            return true;
        if (++n == VM_SHORT_FRAME) {
            unsigned cell = 0;
            if (env_frame_index_lookup(frame, symbol, &cell))
                return cell != 0;
            if (!env_binding_list_acyclic(car(frame)))
                return true;
            for (vars = cdr(vars); IS_PAIR(vars); vars = cdr(vars)) {
                var = car(vars);
                if (!IS_ATOM(var) || CELL_ID(var) == symbol)
                    return true;
            }
            break;
        }
    }
    return vars && (!IS_ATOM(vars) || CELL_ID(vars) == symbol);
}

// The binding of symbol as the full lookup finds it, and where it was found.
// *cacheable is false when a frame on the way was malformed: such a frame is
// passed over, as lookup does, but nothing about it is safe to remember.
static unsigned ic_resolve(int64_t symbol, unsigned env,
                           struct vm_ic_entry *site, bool *cacheable)
{
    *cacheable = true;
    if (!env_chain_acyclic(env))
        return TOK_ERROR;
    unsigned depth = 0;
    for (unsigned e = env; e; depth++) {
        unsigned frame = 0;
        unsigned next = 0;
        if (!vm_try_env_frame(e, &frame, &next))
            return TOK_ERROR;
        unsigned value = 0, cell = 0, offset = 0;
        switch (ic_find_in_frame(frame, symbol, &value, &cell, &offset)) {
        case IC_FOUND:
            site->env = env;
            site->frame = frame;
            site->cell = cell;
            site->depth = depth;
            site->offset = offset;
            return value;
        case IC_BAD:
            *cacheable = false;
            break;
        case IC_ABSENT:
            break;
        }
        e = next;
    }
    return TOK_ERROR;
}

// The entry for the cache word at code->code[word], allocated on first use.
// NULL if none can be had; the lookup then simply goes uncached.
static struct vm_ic_entry *ic_entry_for(code_object *code, unsigned word)
{
    unsigned idx = code->code[word];
    if (idx < code->ic_len && code->ic_entries[idx].word == word)
        return &code->ic_entries[idx];
    if (code->ic_len == code->ic_cap) {
        // Every cached instruction spans several words, so code_len entries
        // are more than the instructions could ever claim between them.
        if (code->ic_cap >= code->code_len)
            return NULL;
        unsigned cap = code->ic_cap ? code->ic_cap * 2 : 4;
        if (cap > code->code_len)
            cap = code->code_len;
        struct vm_ic_entry *grown = checked_realloc_array(
            code->ic_entries, cap, sizeof(struct vm_ic_entry));
        if (!grown)
            return NULL;
        code->ic_entries = grown;
        code->ic_cap = cap;
    }
    idx = code->ic_len++;
    code->code[word] = idx;
    struct vm_ic_entry *e = &code->ic_entries[idx];
    e->word = word;
    return e;
}

__attribute__((noinline)) static unsigned
ic_lookup_miss(code_object *code, unsigned word, int64_t symbol, unsigned env)
{
    uint64_t stamp = env_binding_stamp(symbol);
    struct vm_ic_entry site;
    bool cacheable = true;
    unsigned value = ic_resolve(symbol, env, &site, &cacheable);
    if (value == TOK_ERROR || !cacheable)
        return value;
    struct vm_ic_entry *e = ic_entry_for(code, word);
    if (e) {
        e->stamp = stamp;
        e->env = site.env;
        e->frame = site.frame;
        e->cell = site.cell;
        e->depth = site.depth;
        e->offset = site.offset;
        e->probe = env_binding_dynamic(symbol);
        if (e->probe)
            env_note_binding_cached(symbol);
    }
    return value;
}

// Walk env out to the entry's depth, alongside the environment the entry
// was last validated against. Returns the link at that depth, or 0 if env
// is too short; *shared is set when the walk met that environment, past
// which everything was validated already: those frames were searched when
// the entry was filled, and a binding added to one since would have changed
// the stamp. A searching entry (probe) checks each frame before that point.
static inline unsigned ic_walk(const struct vm_ic_entry *e, int64_t symbol,
                               unsigned env, bool probe, bool *shared)
{
    unsigned node = env;
    unsigned seen = e->env;
    for (unsigned d = e->depth; d; d--) {
        if (!IS_PAIR(node))
            return 0;
        if (probe && ic_frame_may_bind(car(node), symbol))
            return 0;
        node = cdr(node);
        seen = IS_PAIR(seen) ? cdr(seen) : 0;
        if (node == seen) {
            *shared = true;
            return node;
        }
    }
    return IS_PAIR(node) ? node : 0;
}

// A hit whose stamp matched, from an environment other than the one last
// validated. Returns the binding's value cell, or 0 to take the full lookup.
static unsigned ic_revalidate(struct vm_ic_entry *e, int64_t symbol,
                              unsigned env)
{
    bool shared = false;
    unsigned node = e->probe ? ic_walk(e, symbol, env, true, &shared)
                             : ic_walk(e, symbol, env, false, &shared);
    if (!node)
        return 0;
    unsigned frame = shared ? e->frame : car(node);
    if (frame != e->frame) {
        // Another activation of the frame the binding was found in. Its
        // bindings were laid out by the same code, so the name sits at the
        // same offset unless an internal define has since prepended others;
        // check rather than assume.
        if (e->offset == IC_NO_OFFSET || !IS_PAIR(frame))
            return 0;
        unsigned vars = car(frame);
        unsigned vals = cdr(frame);
        for (unsigned o = e->offset; o; o--) {
            if (!IS_PAIR(vars) || !IS_PAIR(vals))
                return 0;
            vars = cdr(vars);
            vals = cdr(vals);
        }
        unsigned var = IS_PAIR(vars) ? car(vars) : vars;
        if (!IS_PAIR(vals) || !IS_ATOM(var) || CELL_ID(var) != symbol)
            return 0;
        e->frame = frame;
        e->cell = vals;
    }
    e->env = env;
    // Only a searching entry needs the mark: while the name has never been
    // defined in a nested frame, the next such define changes its stamp
    // regardless (note_binding_added in env.c).
    if (e->probe)
        env_note_binding_cached(symbol);
    return e->cell;
}

// Everything but the commonest hit: a different environment, a binding
// reached through a reference, or a miss.
__attribute__((noinline)) static unsigned
ic_lookup_slow(code_object *code, unsigned word, int64_t symbol, unsigned env)
{
    unsigned idx = code->code[word];
    if (idx < code->ic_len) {
        struct vm_ic_entry *e = &code->ic_entries[idx];
        if (e->word == word && e->stamp == env_binding_stamp(symbol)) {
            unsigned cell = env == e->env ? e->cell
                                          : ic_revalidate(e, symbol, env);
            unsigned value = 0;
            if (cell && vm_try_deref_binding_value(car(cell), &value))
                return value;
        }
    }
    return ic_lookup_miss(code, word, symbol, env);
}

// Look up symbol from env through the cache whose first word is
// code->code[word]. TOK_ERROR if unbound.
static inline unsigned ic_lookup(code_object *code, unsigned word,
                                 int64_t symbol, unsigned env)
{
    unsigned idx = code->code[word];
    if (__builtin_expect(idx < code->ic_len, 1)) {
        const struct vm_ic_entry *e = &code->ic_entries[idx];
        if (__builtin_expect(e->word == word && e->env == env &&
                                 e->stamp == env_binding_stamp(symbol),
                             1)) {
            unsigned value = car(e->cell);
            if (__builtin_expect(!IS_BINDING_REF(value), 1))
                return value;
        }
    }
    return ic_lookup_slow(code, word, symbol, env);
}

// Global VM state for GC integration
// When the VM is running, this points to the active VM state
// so that GC can update VM roots
static vm_state *active_vm = NULL;

// Enter the dispatch loop using the execution state already stored in vm.
// Unlike vm_run(), this does not reset code/ip/env/stack/frame state, so it
// can resume a continuation after restore_continuation().
static unsigned vm_dispatch(vm_state *vm);

vm_state *get_active_vm(void)
{
    return active_vm;
}

// A lisp_panic longjmps past every vm_run frame, so active_vm and its parent
// chain point into dead stack memory; the next collection would walk them as
// roots and write through them. The abandoned VMs' stacks are leaked.
void vm_reset_after_panic(void)
{
    active_vm = NULL;
}

// ============================================================================
// VM Initialization
// ============================================================================

void vm_init(vm_state *vm)
{
    memset(vm, 0, sizeof(vm_state));

    vm->stack_cap = INITIAL_STACK_SIZE;
    vm->stack = checked_malloc_array(vm->stack_cap, sizeof(unsigned));
    if (!vm->stack) {
        lisp_panic("vm_init: failed to allocate stack");
    }

    vm->frames_cap = INITIAL_FRAMES_SIZE;
    vm->frames = checked_malloc_array(vm->frames_cap, sizeof(vm_frame));
    if (!vm->frames) {
        free(vm->stack);
        lisp_panic("vm_init: failed to allocate frames");
    }

    vm->running = false;
    vm->error = false;
}

void vm_free(vm_state *vm)
{
    free(vm->stack);
    free(vm->frames);
    free(vm->letrecs);
    memset(vm, 0, sizeof(vm_state));
}

static bool vm_resize_stack(vm_state *vm, unsigned new_cap,
                            const char *error_msg)
{
    unsigned *new_stack =
        checked_realloc_array(vm->stack, new_cap, sizeof(unsigned));
    if (!new_stack) {
        VM_ERROR(vm, error_msg);
        return false;
    }
    vm->stack = new_stack;
    vm->stack_cap = new_cap;
    return true;
}

static bool vm_resize_frames(vm_state *vm, unsigned new_cap,
                             const char *error_msg)
{
    vm_frame *new_frames =
        checked_realloc_array(vm->frames, new_cap, sizeof(vm_frame));
    if (!new_frames) {
        VM_ERROR(vm, error_msg);
        return false;
    }
    vm->frames = new_frames;
    vm->frames_cap = new_cap;
    return true;
}

static bool vm_resize_letrecs(vm_state *vm, unsigned new_cap,
                              const char *error_msg)
{
    vm_letrec_state *new_letrecs =
        checked_realloc_array(vm->letrecs, new_cap, sizeof(vm_letrec_state));
    if (!new_letrecs) {
        VM_ERROR(vm, error_msg);
        return false;
    }
    vm->letrecs = new_letrecs;
    vm->letrec_cap = new_cap;
    return true;
}

static bool vm_push_letrec(vm_state *vm, unsigned frame, unsigned count)
{
    if (vm->letrec_depth >= vm->letrec_cap) {
        if (vm->letrec_cap >= VM_MAX_STACK_SIZE) {
            VM_ERROR(vm, "letrec initialization stack overflow");
            return false;
        }
        unsigned new_cap = checked_grow_capacity(
            vm->letrec_cap, sizeof(vm_letrec_state),
            "letrec initialization capacity overflow");
        if (new_cap > VM_MAX_STACK_SIZE)
            new_cap = VM_MAX_STACK_SIZE;
        if (!vm_resize_letrecs(vm, new_cap,
                               "letrec initialization stack reallocation failed"))
            return false;
    }
    vm->letrecs[vm->letrec_depth].frame = frame;
    vm->letrecs[vm->letrec_depth].count = count;
    vm->letrec_depth++;
    return true;
}

// ============================================================================
// Closure Call Helper (for CPS evaluator interop)
// ============================================================================

/**
 * Call a bytecode closure from the CPS evaluator.
 * This creates a temporary VM to execute the closure body.
 *
 * @param closure The bytecode closure (cons cell with BT_CLOSURE marker)
 * @param args    Argument list
 * @return Result value, or TOK_ERROR on error
 */
unsigned vm_call_closure(unsigned closure, unsigned args)
{
    if (!is_bytecode_closure_object(closure)) {
        show_error("invalid bytecode closure");
        return TOK_ERROR;
    }

    code_object *code = GET_CLOSURE_CODE(closure);
    unsigned closure_env = GET_CLOSURE_ENV(closure);

    if (!vm_code_is_well_formed(code)) {
        show_error("invalid bytecode closure");
        return TOK_ERROR;
    }

    unsigned argc = 0;
    if (!list_length_checked(args, &argc, "closure arguments"))
        return TOK_ERROR;

    if (!code->has_rest && argc != code->arity) {
        show_error("wrong number of arguments to closure");
        return TOK_ERROR;
    }
    if (code->has_rest && argc < code->arity) {
        show_error("too few arguments to closure");
        return TOK_ERROR;
    }

    // Create a temporary VM and run the code
    vm_state vm;
    vm_init(&vm);
    unsigned run_env = closure_env;

    if (code->use_locals && !code->has_rest) {
        for (unsigned a = args; a; a = cdr(a))
            vm_push(&vm, car(a));
    } else {
        // Bind parameters to arguments
        if (code->params >= code->const_len) {
            show_error("invalid bytecode closure parameter metadata");
            vm_free(&vm);
            return TOK_ERROR;
        }
        unsigned params = code->constants[code->params];
        unsigned frame;
        {
            GC_PROTECT_GUARD3(&closure_env, &args, &params);
            frame = bind_params(params, args);
        }
        if (frame == TOK_ERROR) {
            vm_free(&vm);
            return TOK_ERROR;
        }
        {
            GC_PROTECT_GUARD2(&frame, &closure_env);
            run_env = alloc_cons(frame, closure_env);
        }
    }

    unsigned result = vm_run(&vm, code, run_env);
    bool had_error = vm.error;
    const char *error_msg = vm.error_msg;
    vm_free(&vm);

    if (had_error) {
        if (error_msg) {
            show_error("VM error: %s", error_msg);
        }
        return TOK_ERROR;
    }

    return result;
}

// ============================================================================
// Stack Operations
// ============================================================================

void vm_push(vm_state *vm, unsigned val)
{
    if (vm->sp >= vm->stack_cap) {
        if (vm->stack_cap >= MAX_STACK_SIZE) {
            VM_ERROR(vm, "stack overflow");
            return;
        }
        unsigned new_cap =
            checked_grow_capacity(vm->stack_cap, sizeof(unsigned),
                                  "vm_push: stack capacity overflow");
        if (!vm_resize_stack(vm, new_cap, "stack reallocation failed"))
            return;
    }
    LISP_ASSERT_FMT(vm->sp < vm->stack_cap,
                    "vm_push: sp=%u >= stack_cap=%u", vm->sp, vm->stack_cap);
    vm->stack[vm->sp++] = val;
}

unsigned vm_pop(vm_state *vm)
{
    if (vm->sp == 0) {
        VM_ERROR(vm, "stack underflow");
        return 0;
    }
    return vm->stack[--vm->sp];
}

static inline unsigned vm_peek(vm_state *vm, unsigned depth)
{
    if (depth >= vm->sp) {
        VM_ERROR(vm, "stack underflow");
        return 0;
    }
    return vm->stack[vm->sp - 1 - depth];
}

// ============================================================================
// Frame Operations
// ============================================================================

static void push_frame(vm_state *vm, code_object *code, unsigned ip,
                       unsigned bp, unsigned restore_sp, unsigned env)
{
    LISP_ASSERT_MSG(code != NULL, "push_frame: null code object");
    if (vm->fp >= vm->frames_cap) {
        if (vm->frames_cap >= MAX_FRAMES_SIZE) {
            VM_ERROR(vm, "call stack overflow");
            return;
        }
        unsigned new_cap =
            checked_grow_capacity(vm->frames_cap, sizeof(vm_frame),
                                  "push_frame: frame capacity overflow");
        if (!vm_resize_frames(vm, new_cap, "frame stack reallocation failed"))
            return;
    }
    LISP_ASSERT_FMT(vm->fp < vm->frames_cap,
                    "push_frame: fp=%u >= frames_cap=%u", vm->fp, vm->frames_cap);
    vm->frames[vm->fp].code = code;
    vm->frames[vm->fp].ip = ip;
    vm->frames[vm->fp].bp = bp;
    vm->frames[vm->fp].sp = restore_sp;
    vm->frames[vm->fp].env = env;
    vm->fp++;
}

static inline void pop_frame(vm_state *vm)
{
    if (vm->fp == 0) {
        VM_ERROR(vm, "frame stack underflow");
        return;
    }
    vm->fp--;
    vm->code = vm->frames[vm->fp].code;
    vm->ip = vm->frames[vm->fp].ip;
    vm->bp = vm->frames[vm->fp].bp;
    vm->env = vm->frames[vm->fp].env;
}

// ============================================================================
// Continuation Support
// ============================================================================

// vm_continuation is now defined in bytecode.h for GC access

static unsigned capture_continuation(vm_state *vm)
{
    LISP_ASSERT_MSG(vm != NULL, "capture_continuation: null vm");
    LISP_ASSERT_MSG(vm->stack != NULL, "capture_continuation: null stack");
    LISP_ASSERT_MSG(vm->frames != NULL, "capture_continuation: null frames");

    // Calculate sizes for single allocation
    LISP_ASSERT_MSG(vm->letrec_depth <= VM_MAX_STACK_SIZE,
                    "capture_continuation: letrec depth overflow");
    LISP_ASSERT_MSG(vm->letrec_depth == 0 || vm->letrecs != NULL,
                    "capture_continuation: missing letrec stack");
    unsigned letrec_saved_count = 0;
    for (unsigned i = 0; i < vm->letrec_depth; i++) {
        LISP_ASSERT_MSG(vm->letrecs[i].count <=
                            VM_MAX_STACK_SIZE - letrec_saved_count,
                        "capture_continuation: letrec values overflow");
        letrec_saved_count += vm->letrecs[i].count;
    }

    size_t stack_size, frames_size, letrec_states_size, letrec_values_size;
    size_t total_size;
    LISP_ASSERT_MSG(checked_array_size(vm->sp, sizeof(unsigned), &stack_size),
                    "capture_continuation: stack size overflow");
    LISP_ASSERT_MSG(checked_array_size(vm->fp, sizeof(vm_frame), &frames_size),
                    "capture_continuation: frame size overflow");
    LISP_ASSERT_MSG(checked_array_size(vm->letrec_depth,
                                       sizeof(vm_letrec_state),
                                       &letrec_states_size),
                    "capture_continuation: letrec state size overflow");
    LISP_ASSERT_MSG(checked_array_size(letrec_saved_count, sizeof(unsigned),
                                       &letrec_values_size),
                    "capture_continuation: letrec value size overflow");

    // vm_frame starts with a code_object* and needs pointer alignment, but
    // stack_size (a multiple of sizeof(unsigned) == 4) isn't necessarily a
    // multiple of that when vm->sp is odd - pad up so the frames array
    // that follows the stack array in the single allocation stays aligned.
    size_t frames_align = _Alignof(vm_frame);
    size_t stack_padding =
        (frames_align - (stack_size % frames_align)) % frames_align;

    LISP_ASSERT_MSG(checked_add_size(sizeof(vm_continuation), stack_size,
                                     &total_size),
                    "capture_continuation: size overflow");
    LISP_ASSERT_MSG(checked_add_size(total_size, stack_padding, &total_size),
                    "capture_continuation: size overflow");
    LISP_ASSERT_MSG(checked_add_size(total_size, frames_size, &total_size),
                    "capture_continuation: size overflow");
    LISP_ASSERT_MSG(checked_add_size(total_size, letrec_states_size,
                                     &total_size),
                    "capture_continuation: size overflow");
    LISP_ASSERT_MSG(checked_add_size(total_size, letrec_values_size,
                                     &total_size),
                    "capture_continuation: size overflow");

    // Allocate the wrapper cell before copying VM roots. alloc() can trigger
    // GC, which may update vm->stack/env/frame cells; copying after alloc()
    // ensures the continuation captures forwarded cell IDs.
    unsigned cell = alloc();

    // Single allocation for entire continuation
    char *block = checked_malloc_size(total_size);
    LISP_ASSERT_MSG(block != NULL, "capture_continuation: malloc failed");

    // Place struct at start, arrays after it
    vm_continuation *cont = (vm_continuation *)block;
    cont->stack = (unsigned *)(block + sizeof(vm_continuation));
    cont->frames = (vm_frame *)(block + sizeof(vm_continuation) + stack_size +
                                stack_padding);
    char *letrec_base = (char *)cont->frames + frames_size;
    cont->letrecs = vm->letrec_depth > 0
                        ? (vm_letrec_state *)letrec_base
                        : NULL;
    cont->letrec_saved = letrec_saved_count > 0
                             ? (unsigned *)(letrec_base + letrec_states_size)
                             : NULL;

    // Copy stack
    cont->sp = vm->sp;
    memcpy(cont->stack, vm->stack, stack_size);

    // Copy frames
    cont->fp = vm->fp;
    memcpy(cont->frames, vm->frames, frames_size);

    // Copy execution state
    cont->code = vm->code;
    cont->ip = vm->ip;
    cont->env = vm->env;
    cont->bp = vm->bp;
    cont->signal_handler = vm->signal_handler;

    // Save every active letrec frame and its current binding values. An outer
    // initializer remains active while procedures called from it run.
    cont->letrec_depth = vm->letrec_depth;
    cont->letrec_saved_len = letrec_saved_count;
    if (vm->letrec_depth > 0)
        memcpy(cont->letrecs, vm->letrecs, letrec_states_size);
    unsigned saved_index = 0;
    for (unsigned i = 0; i < vm->letrec_depth; i++) {
        unsigned letrec_frame = vm->letrecs[i].frame;
        LISP_ASSERT_MSG(IS_PAIR(letrec_frame) && IS_PAIR(car(letrec_frame)),
                        "capture_continuation: invalid letrec frame");
        unsigned frame = car(letrec_frame);
        unsigned vals = cdr(frame);
        for (unsigned j = 0; j < vm->letrecs[i].count; j++) {
            LISP_ASSERT_MSG(IS_PAIR(vals),
                            "capture_continuation: truncated letrec values");
            cont->letrec_saved[saved_index++] = car(vals);
            vals = cdr(vals);
        }
    }

    CELL_TYPE(cell) = BT_VMCONT;
    CELL_PTR(cell) = cont;
    vm_continuation_register(cont);

    return cell;
}

static void restore_continuation(vm_state *vm, unsigned cont_cell,
                                 unsigned value)
{
    LISP_ASSERT_MSG(vm != NULL, "restore_continuation: null vm");
    LISP_ASSERT_TYPE(cont_cell, BT_VMCONT);

    vm_continuation *cont = (vm_continuation *)CELL_PTR(cont_cell);
    if (!vm_continuation_is_registered(cont)) {
        VM_ERROR(vm, "restore_continuation: invalid continuation");
        return;
    }

    if (!vm_code_is_well_formed(cont->code) ||
        !vm_instruction_starts_at(cont->code, cont->ip)) {
        VM_ERROR(vm, "restore_continuation: invalid execution state");
        return;
    }
    if (cont->sp >= MAX_STACK_SIZE || cont->sp == UINT_MAX) {
        VM_ERROR(vm, "restore_continuation: stack size overflow");
        return;
    }
    if (cont->fp > MAX_FRAMES_SIZE) {
        VM_ERROR(vm, "restore_continuation: frame size overflow");
        return;
    }
    if (cont->sp > 0 && !cont->stack) {
        VM_ERROR(vm, "restore_continuation: missing stack");
        return;
    }
    if (cont->bp > cont->sp) {
        VM_ERROR(vm, "restore_continuation: invalid base pointer");
        return;
    }
    if (cont->fp > 0 && !cont->frames) {
        VM_ERROR(vm, "restore_continuation: missing frames");
        return;
    }
    for (unsigned i = 0; i < cont->fp; i++) {
        vm_frame *frame = &cont->frames[i];
        if (!vm_code_is_well_formed(frame->code) ||
            !vm_instruction_starts_at(frame->code, frame->ip) ||
            frame->bp > frame->sp || frame->sp > cont->sp) {
            VM_ERROR(vm, "restore_continuation: invalid frame state");
            return;
        }
    }

    if (cont->letrec_depth > VM_MAX_STACK_SIZE ||
        (cont->letrec_depth > 0 && !cont->letrecs)) {
        VM_ERROR(vm, "restore_continuation: invalid letrec stack");
        return;
    }
    if (cont->letrec_saved_len > VM_MAX_STACK_SIZE ||
        (cont->letrec_saved_len > 0 && !cont->letrec_saved)) {
        VM_ERROR(vm, "restore_continuation: invalid letrec values");
        return;
    }

    // Validate every active frame and every binding cell before making any
    // Scheme-visible mutation. The flattened saved array must match the sum
    // of the per-frame binding counts exactly.
    unsigned expected_saved = 0;
    for (unsigned i = 0; i < cont->letrec_depth; i++) {
        vm_letrec_state *state = &cont->letrecs[i];
        if (state->count > VM_MAX_STACK_SIZE - expected_saved) {
            VM_ERROR(vm, "restore_continuation: invalid letrec values");
            return;
        }
        expected_saved += state->count;
        if (!state->frame || !IS_PAIR(state->frame) ||
            !IS_PAIR(car(state->frame))) {
            VM_ERROR(vm, "restore_continuation: invalid letrec frame");
            return;
        }
        unsigned vals = cdr(car(state->frame));
        for (unsigned j = 0; j < state->count; j++) {
            if (!IS_PAIR(vals)) {
                VM_ERROR(vm, "restore_continuation: invalid letrec values");
                return;
            }
            vals = cdr(vals);
        }
    }
    if (expected_saved != cont->letrec_saved_len) {
        VM_ERROR(vm, "restore_continuation: invalid letrec values");
        return;
    }

    // Complete any fallible storage growth before restoring mutable letrec
    // bindings, so a failed restore has no partial semantic side effects.
    if (vm->stack_cap < cont->sp + 1) {
        unsigned new_cap = cont->sp + 1;
        if (!vm_resize_stack(vm, new_cap,
                             "restore_continuation: stack realloc failed"))
            return;
    }
    if (vm->frames_cap < cont->fp) {
        unsigned new_cap = cont->fp;
        if (!vm_resize_frames(vm, new_cap,
                              "restore_continuation: frames realloc failed"))
            return;
    }
    if (vm->letrec_cap < cont->letrec_depth) {
        if (!vm_resize_letrecs(vm, cont->letrec_depth,
                               "restore_continuation: letrec realloc failed"))
            return;
    }

    // Restore binding cells only after all validation and allocation succeeds.
    unsigned saved_index = 0;
    for (unsigned i = 0; i < cont->letrec_depth; i++) {
        vm_letrec_state *state = &cont->letrecs[i];
        unsigned vals = cdr(car(state->frame));
        for (unsigned j = 0; j < state->count; j++, vals = cdr(vals)) {
            cell_set_car(vals, cont->letrec_saved[saved_index++]);
        }
    }

    // Restore stack (with return value on top)
    memcpy(vm->stack, cont->stack, cont->sp * sizeof(unsigned));
    vm->sp = cont->sp;
    vm->stack[vm->sp++] = value; // Push return value

    // Restore frames
    memcpy(vm->frames, cont->frames, cont->fp * sizeof(vm_frame));
    vm->fp = cont->fp;

    // Restore active letrec initializations for multi-shot re-entry.
    if (cont->letrec_depth > 0) {
        memcpy(vm->letrecs, cont->letrecs,
               cont->letrec_depth * sizeof(vm_letrec_state));
    }
    vm->letrec_depth = cont->letrec_depth;

    // Restore execution state
    vm->code = cont->code;
    vm->ip = cont->ip;
    vm->env = cont->env;
    vm->bp = cont->bp;
    vm->signal_handler = cont->signal_handler;
}

// Invoke a VM continuation from the CPS evaluator. If the evaluator is
// running as a callback from a VM, restore that VM in place so the callback's
// C frame can hand control back to the normal dispatch loop. Otherwise restore
// the continuation into a temporary VM and resume its saved dispatch state.
vm_cont_call_status vm_call_continuation(unsigned continuation, unsigned args,
                                         unsigned *result_out)
{
    if (result_out)
        *result_out = TOK_ERROR;
    if (!is_vm_continuation_object(continuation)) {
        show_error("invalid continuation");
        return VM_CONT_CALL_ERROR;
    }

    unsigned argc = 0;
    if (!list_length_checked(args, &argc, "continuation arguments"))
        return VM_CONT_CALL_ERROR;

    GC_GUARD;
    gc_protect(&continuation);
    gc_protect(&args);

    if (active_vm) {
        unsigned value = values_from_list(args);
        gc_protect(&value);
        restore_continuation(active_vm, continuation, value);
        if (active_vm->error)
            return VM_CONT_CALL_ERROR;
        active_vm->cps_continuation_transfer = true;
        return VM_CONT_CALL_TRANSFERRED;
    }

    vm_state vm;
    vm_init(&vm);
    unsigned value = values_from_list(args);
    gc_protect(&value);
    restore_continuation(&vm, continuation, value);
    if (vm.error) {
        const char *error_msg = vm.error_msg;
        vm_free(&vm);
        if (error_msg)
            show_error("VM error: %s", error_msg);
        return VM_CONT_CALL_ERROR;
    }
    vm.running = true;
    unsigned result = vm_dispatch(&vm);
    bool had_error = vm.error;
    vm_free(&vm);
    if (had_error || result == TOK_ERROR)
        return VM_CONT_CALL_ERROR;
    if (result_out)
        *result_out = result;
    return VM_CONT_CALL_RETURNED;
}

// ============================================================================
// Function Application
// ============================================================================

// Forward declaration for vm_handle_apply
static void vm_apply(vm_state *vm, unsigned fn, unsigned argc, bool tail);

// Pinned singleton code object that call-with-values producer frames return
// into: [OP_CWV_FINISH, OP_RETURN]. Running the producer in the current VM
// (returning here) instead of a nested VM lets continuations captured inside
// the producer restore the whole computation.
static code_object *cwv_finish_code = NULL;

static code_object *get_cwv_finish_code(void)
{
    if (cwv_finish_code)
        return cwv_finish_code;
    code_object *code = code_new();
    if (!code)
        return NULL;
    code_emit(code, OP_CWV_FINISH);
    code_emit(code, OP_RETURN);
    code->gc_pinned = true;
    cwv_finish_code = code;
    return code;
}

// Pinned singleton code object that exception-handler frames return into:
// [OP_ERROR_RETURN]. When a handler dispatched by vm_signal_error returns
// normally instead of jumping to a continuation, this marker fires and
// raises the R7RS "handler returned from a non-continuable exception"
// error (Scheme's raise semantics).
static code_object *error_return_code = NULL;

static code_object *get_error_return_code(void)
{
    if (error_return_code)
        return error_return_code;
    code_object *code = code_new();
    if (!code)
        return NULL;
    code_emit(code, OP_ERROR_RETURN);
    code->gc_pinned = true;
    error_return_code = code;
    return code;
}

// Route a C-level error (primitive failure, undefined variable) through the
// Scheme exception system instead of aborting the VM: build an error object
// and invoke the current *current-exception-handler* in a new frame whose
// return address is the pinned OP_ERROR_RETURN marker. A handler that jumps
// to a continuation (guard, call/cc) replaces this frame via
// restore_continuation; one that returns normally falls into the marker and
// signals the "handler returned" error. Returns false (falling back to
// VM_ERROR) if the stdlib exception machinery is unavailable.
static bool vm_signal_error(vm_state *vm, const char *msg)
{
    if (!msg)
        msg = "unknown error";
    unsigned handler = lookup_silent(intern("*current-exception-handler*"),
                                     exception_state_env(vm->env));
    if (handler == TOK_ERROR) {
        VM_ERROR(vm, msg);
        return false;
    }
    code_object *marker = get_error_return_code();
    if (!marker) {
        VM_ERROR(vm, msg);
        return false;
    }
    GC_GUARD;
    gc_protect(&handler);
    unsigned error_obj = make_error_object_c(msg);
    if (error_obj == TOK_ERROR) {
        VM_ERROR(vm, msg);
        return false;
    }
    // Run the handler with the marker as its return address.
    vm->signal_handler = handler;
    vm->code = marker;
    vm->ip = 0;
    vm_push(vm, error_obj);
    vm_apply(vm, handler, 1, false);
    return true;
}

// Scheme raise: dispatch OBJ (any object, not just an error object) to the
// current *current-exception-handler* with the pinned OP_ERROR_RETURN marker
// as its return address, so a returning handler falls into the marker which
// distinguishes the default handler (unhandled - already reported, propagate
// silently) from a user handler (R7RS violation). The stdlib raise is
// (raise-now obj); raise-now itself never returns normally - the marker
// terminates, or a handler that jumps to a continuation replaces this frame.
static void vm_handle_raise(vm_state *vm, unsigned argc, unsigned *argv)
{
    if (argc != 1) {
        vm->sp -= argc;
        VM_ERROR(vm, "raise: expected 1 argument");
        return;
    }
    unsigned obj = argv[0];
    vm->sp -= argc;
    unsigned handler = lookup_silent(intern("*current-exception-handler*"),
                                     exception_state_env(vm->env));
    if (handler == TOK_ERROR) {
        VM_ERROR(vm, "raise: no exception handler");
        return;
    }
    code_object *marker = get_error_return_code();
    if (!marker) {
        VM_ERROR(vm, "raise: no exception handler");
        return;
    }
    GC_GUARD;
    gc_protect(&handler);
    gc_protect(&obj);
    vm->signal_handler = handler;
    vm->code = marker;
    vm->ip = 0;
    vm_push(vm, obj);
    vm_apply(vm, handler, 1, false);
}

// eval runs the compiled expression in THIS VM. A frame is pushed so that
// the unit's RETURN lands back here with its value on the stack, exactly as
// a call would, and the dispatch loop simply continues into the new code.
//
// It used to hand the expression to main.c's eval callback, which compiled
// it and ran it in a second vm_state on the C stack. Continuations do not
// survive that boundary. One captured out here and invoked in there was
// restored into the inner VM, which ran the rest of the program to
// completion - and then vm_run returned normally into this handler, which
// resumed the outer VM's stale state and ran the tail of the program a
// second time. (guard (e ...) (eval ...)) is the everyday shape of that.
// In the other direction, a continuation captured inside the eval'd code
// and re-entered after eval had returned found its frames ending in the
// inner VM's HALT. Run in place, both are ordinary frames of one VM.
static void vm_handle_eval(vm_state *vm, unsigned argc, unsigned *argv)
{
    // R7RS (and MIT Scheme, and the CPS interpreter) require the
    // environment argument.
    if (argc != 2) {
        vm->sp -= argc;
        VM_ERROR(vm, "eval: expected 2 arguments");
        return;
    }
    unsigned expr = argv[0];
    unsigned eval_env = argv[1];
    vm->sp -= argc;
    // Safe point: every live code object is in a frame or on the heap, and
    // nothing has been compiled for this eval yet.
    gc_sweep_code_objects_if_due();
    GC_GUARD;
    gc_protect(&expr);
    gc_protect(&eval_env);
    code_object *code = compile_for_eval(expr, eval_env);
    if (!code) {
        VM_ERROR(vm, ctx.last_error[0] ? ctx.last_error
                                       : "eval: compilation failed");
        return;
    }
    if (!vm_code_is_well_formed(code)) {
        VM_ERROR(vm, "eval: invalid bytecode");
        return;
    }
    push_frame(vm, vm->code, vm->ip, vm->bp, vm->sp, vm->env);
    if (vm->error)
        return;
    vm->code = code;
    vm->ip = 0;
    vm->bp = vm->sp;
    vm->env = eval_env;
}

static bool vm_push_arg(vm_state *vm, unsigned val, unsigned *argc)
{
    if (*argc == UINT_MAX) {
        VM_ERROR(vm, "argument count overflow");
        return false;
    }
    vm_push(vm, val);
    if (vm->error)
        return false;
    (*argc)++;
    return true;
}

// Multiple values are represented as a proper list.  Validate that invariant
// before walking it so malformed external payloads cannot trap the VM in a
// circular traversal while dispatching call-with-values.
static bool vm_push_multiple_values(vm_state *vm, unsigned values,
                                    unsigned *argc)
{
    if (!list_length_checked(values, NULL, "call-with-values")) {
        VM_ERROR(vm, "call-with-values: invalid multiple values");
        return false;
    }
    for (; values; values = cdr(values)) {
        if (!vm_push_arg(vm, car(values), argc))
            return false;
    }
    return true;
}

// Maximum middle arguments for apply (proc arg1 ... argN list)
// Most apply calls have few middle args; use heap allocation for more
#define MAX_APPLY_MIDDLE_ARGS 64

// Handle (apply proc arg1 ... args-list) - consolidated helper
// Returns true if apply was handled, false on error
static bool vm_handle_apply(vm_state *vm, unsigned argc, unsigned *argv,
                            bool tail)
{
    if (argc < 2) {
        VM_ERROR(vm, "apply: expected at least 2 arguments");
        return false;
    }

    unsigned proc = argv[0];
    unsigned last_list = argv[argc - 1];
    unsigned last_count = 0;
    if (!list_length_checked(last_list, &last_count, "apply")) {
        VM_ERROR(vm, ctx.last_error); // as the interpreter reports it
        return false;
    }

    // Copy middle args before popping (they'll be overwritten)
    unsigned middle_count = (argc > 2) ? argc - 2 : 0;
    if (middle_count > UINT_MAX - last_count) {
        VM_ERROR(vm, "apply: argument count overflow");
        return false;
    }

    // Use stack allocation for small counts, heap for large
    unsigned middle_args_stack[MAX_APPLY_MIDDLE_ARGS];
    unsigned *middle_args = middle_args_stack;
    unsigned *middle_args_heap = NULL;

    if (middle_count > MAX_APPLY_MIDDLE_ARGS) {
        middle_args_heap = checked_malloc_array(middle_count, sizeof(unsigned));
        if (!middle_args_heap) {
            VM_ERROR(vm, "apply: out of memory for middle arguments");
            return false;
        }
        middle_args = middle_args_heap;
    }

    for (unsigned i = 0; i < middle_count; i++) {
        middle_args[i] = argv[i + 1];
    }

    // Pop args from stack
    vm->sp -= argc;

    // Protect values across allocations
    GC_GUARD;
    gc_protect(&proc);
    gc_protect(&last_list);

    // Push middle args then append last list
    unsigned apply_argc = 0;
    for (unsigned i = 0; i < middle_count; i++) {
        if (!vm_push_arg(vm, middle_args[i], &apply_argc)) {
            free(middle_args_heap);
            return false;
        }
    }

    // Free heap allocation if used
    if (middle_args_heap) {
        free(middle_args_heap);
    }

    // Append the last list's elements
    FORLIST(a, last_list)
    {
        if (!vm_push_arg(vm, car(a), &apply_argc))
            return false;
    }

    vm_apply(vm, proc, apply_argc, tail);
    return true;
}

// Box any fixnum values in the top N stack slots (for passing to primitives/closures)
static bool vm_box_stack_args(vm_state *vm, unsigned argc)
{
    if (argc > vm->sp) {
        VM_ERROR(vm, "stack underflow");
        return false;
    }
    for (unsigned i = 0; i < argc; i++) {
        unsigned idx = vm->sp - argc + i;
        if (IS_FIXNUM(vm->stack[idx])) {
            vm->stack[idx] = store((int64_t)FIXNUM_VALUE(vm->stack[idx]));
        }
    }
    return true;
}

// Pinned singleton code object that drives load: [OP_LOAD_STEP]. Each loaded
// form runs in a frame that returns here, so the forms run in this VM. The
// remaining forms sit on the stack, which continuations capture: escaping
// out of a loaded file, or re-entering one after load returned, behaves like
// any other call instead of crossing into a nested VM.
static code_object *load_driver_code = NULL;

static code_object *get_load_driver_code(void)
{
    if (load_driver_code)
        return load_driver_code;
    code_object *code = code_new();
    if (!code)
        return NULL;
    code_emit(code, OP_LOAD_STEP);
    code->gc_pinned = true;
    load_driver_code = code;
    return code;
}

static FILE *vm_open_load_file(const char *filename, char **opened_name)
{
    *opened_name = NULL;
    FILE *f = fopen(filename, "r");
    if (f) {
        *opened_name = checked_string_copy(filename);
        return f;
    }
    // Try with .scm appended, unless the name already ends in .scm
    size_t len = strlen(filename);
    if ((len >= 4 && strcmp(filename + len - 4, ".scm") == 0) ||
        len > SIZE_MAX - 5)
        return NULL;
    char *with_ext = checked_malloc_flex(0, len + 5, 1);
    if (!with_ext)
        return NULL;
    memcpy(with_ext, filename, len);
    memcpy(with_ext + len, ".scm", 5);
    f = fopen(with_ext, "r");
    if (!f) {
        free(with_ext);
        return NULL;
    }
    *opened_name = with_ext;
    return f;
}

// (load filename): read every form up front (as the interpreter does), then
// hand them to the load driver, which evaluates them one at a time in the
// current environment. Each form is compiled only when its turn comes, so a
// macro defined by one form is visible to the next.
static void vm_handle_load(vm_state *vm, unsigned argc, unsigned *argv)
{
    if (argc != 1) {
        vm->sp -= argc;
        VM_ERROR(vm, "load: expected 1 argument");
        return;
    }
    unsigned filename_cell = argv[0];
    vm->sp -= argc;
    if (!IS_STRING(filename_cell)) {
        VM_ERROR(vm, "load: expected string argument");
        return;
    }
    code_object *driver = get_load_driver_code();
    if (!driver) {
        VM_ERROR(vm, "load: out of memory");
        return;
    }
    char *filename_arg = require_string_ptr(filename_cell, "load");
    if (!filename_arg) {
        VM_ERROR(vm, ctx.last_error);
        return;
    }
    char *filename = checked_string_copy(filename_arg);
    if (!filename) {
        VM_ERROR(vm, "load: out of memory");
        return;
    }
    char *opened_name = NULL;
    FILE *f = vm_open_load_file(filename, &opened_name);
    if (!f) {
        show_error("load: cannot open file: %s", filename);
        free(filename);
        VM_ERROR(vm, ctx.last_error);
        return;
    }
    free(filename);

    const char *old_filename = reader_get_filename();
    reader_set_filename(opened_name ? opened_name : "<load>");
    reader_reset_position();

    GC_GUARD;
    unsigned forms = 0, forms_tail = 0;
    gc_protect(&forms);
    gc_protect(&forms_tail);
    bool failed = false;
    for (;;) {
        unsigned expr = read_obj_port(f);
        if (expr == TOK_ERROR) {
            failed = true;
            break;
        }
        if (is_eof_object(expr))
            break;
        if (expr == TOK_CLOSE || expr == TOK_DOT) {
            show_error("load: unexpected reader token");
            failed = true;
            break;
        }
        list_append(&forms, &forms_tail, expr);
    }
    reader_forget_port(f);
    if (fclose(f) != 0 && !failed) {
        show_error("load: close failed");
        failed = true;
    }
    reader_set_filename(old_filename);
    reader_reset_position();
    free(opened_name);
    if (failed) {
        VM_ERROR(vm, ctx.last_error[0] ? ctx.last_error : "load failed");
        return;
    }

    // The driver returns the last form's value through this frame.
    push_frame(vm, vm->code, vm->ip, vm->bp, vm->sp, vm->env);
    if (vm->error)
        return;
    vm->code = driver;
    vm->ip = 0;
    vm_push(vm, forms);
    vm_push(vm, 0); // value of an empty file
}

// call-with-values: run the producer in THIS VM rather than a nested one,
// stashing the consumer on the stack and arranging for the producer to
// return into the pinned finisher code, so a continuation captured inside
// the producer snapshots the whole computation. In tail position the
// finisher replaces the current activation (it returns straight to our
// caller) and applies the consumer as a tail call, as R7RS requires.
static void vm_handle_call_with_values(vm_state *vm, unsigned argc,
                                       unsigned *argv, bool tail)
{
    if (argc != 2) {
        vm->sp -= argc;
        VM_ERROR(vm, "call-with-values: expected 2 arguments");
        return;
    }
    unsigned producer = argv[0];
    unsigned consumer = argv[1];
    vm->sp -= argc;
    code_object *finish = get_cwv_finish_code();
    if (!finish) {
        VM_ERROR(vm, "call-with-values: out of memory");
        return;
    }
    GC_GUARD;
    gc_protect(&producer);
    gc_protect(&consumer);
    if (tail && vm->fp > 0) {
        // Drop this activation's stack window (locals included); the frame
        // that called it records where its operand stack began.
        unsigned base = vm->frames[vm->fp - 1].sp;
        if (base <= vm->sp)
            vm->sp = base;
    } else {
        // The frame's restore point excludes the stashed consumer: the
        // consumer is tail-applied from the finisher, and a stack-local
        // consumer's RETURN_LOCALS restores exactly this stack pointer.
        push_frame(vm, vm->code, vm->ip, vm->bp, vm->sp, vm->env);
    }
    if (vm->error)
        return;
    vm_push(vm, consumer);
    if (vm->error)
        return;
    vm->code = finish;
    vm->ip = 0;
    vm_apply(vm, producer, 0, false);
}

// Primitives that need the VM itself rather than apply_primitive_argv: they
// call back into Scheme, capture or replace control state, or run code.
// Shared by OP_PRIM (direct calls by name) and vm_apply (first-class uses
// such as (map call-with-values ...) or (apply apply ...)), so the two
// paths cannot disagree. Arguments are boxed and on top of the stack.
// Returns false if prim_id is an ordinary primitive.
static bool vm_apply_special_primitive(vm_state *vm, int64_t prim_id,
                                       unsigned argc, unsigned *argv,
                                       bool tail)
{
    switch (prim_id) {
    case PCALLCC: {
        // call/cc: pass continuation to procedure
        if (argc != 1) {
            vm->sp -= argc;
            VM_ERROR(vm, "call/cc: expected 1 argument");
            return true;
        }
        GC_GUARD;
        unsigned proc = argv[0];
        vm->sp -= argc;
        gc_protect(&proc);
        unsigned cont = capture_continuation(vm);
        // Push continuation as argument and call proc
        vm_push(vm, cont);
        vm_apply(vm, proc, 1, tail);
        return true;
    }
    case PAPPLY:
        vm_handle_apply(vm, argc, argv, tail);
        return true;
    case PRAISENOW:
        vm_handle_raise(vm, argc, argv);
        return true;
    case PCALLWITHVALUES:
        vm_handle_call_with_values(vm, argc, argv, tail);
        return true;
    case PEVAL:
        vm_handle_eval(vm, argc, argv);
        return true;
    case PINTERACTIONENV:
        vm->sp -= argc;
        if (argc != 0) {
            VM_ERROR(vm, "interaction-environment: expected 0 arguments");
            return true;
        }
        vm_push(vm, exception_state_env(vm->env));
        return true;
    case PLOAD:
        vm_handle_load(vm, argc, argv);
        return true;
    default:
        return false;
    }
}

static void vm_apply(vm_state *vm, unsigned fn, unsigned argc, bool tail)
{
    // Ensure all arguments are boxed cell indices before binding.
    // Protect fn across boxing since store() can trigger GC.
    gc_protect(&fn);
    bool boxed_args = vm_box_stack_args(vm, argc);
    gc_unprotect(1);
    if (!boxed_args)
        return;
    if (IS_BUILTIN(fn)) {
        int64_t prim_id = CELL_ID(fn);

        // Get pointer to arguments on stack (no list building needed)
        unsigned *argv = &vm->stack[vm->sp - argc];

        // First-class uses of call/cc, apply, call-with-values, eval, ...
        // (apply_primitive_argv rejects them as "should be handled in
        // apply_function").
        if (vm_apply_special_primitive(vm, prim_id, argc, argv, tail))
            return;

        // Regular primitive - pass stack values directly as argv
        // Note: We keep argv on the stack during the call for GC protection,
        // then pop and push result in one operation (replacing args with result)
        unsigned result = apply_primitive_argv(prim_id, argc, argv);
        vm->sp -= argc; // Pop args after primitive returns
        if (result == TOK_ERROR) {
            vm_signal_error(vm, ctx.last_error[0] ? ctx.last_error
                                                  : "primitive error");
            return;
        }
        vm_push(vm, result);
        return;
    }

    // Check for VM closure: cons cell with BT_CLOSURE marker in car
    if (is_bytecode_closure_object(fn)) {
        code_object *code = GET_CLOSURE_CODE(fn);
        unsigned closure_env = GET_CLOSURE_ENV(fn);

        // Check code object validity
        if (!vm_code_is_well_formed(code)) {
            VM_ERROR(vm, "closure has invalid code object");
            return;
        }

        // Check arity
        if (!code->has_rest && argc != code->arity) {
            VM_ERROR(vm, "wrong number of arguments");
            return;
        }
        if (code->has_rest && argc < code->arity) {
            VM_ERROR(vm, "too few arguments");
            return;
        }

        // For tail calls: if the CURRENT function uses locals, clean them up
        // by shifting args down to overwrite the old locals
        if (tail && vm->fp > 0 && vm->code->use_locals) {
            if (argc > vm->sp || vm->bp > vm->stack_cap ||
                argc > vm->stack_cap - vm->bp) {
                VM_ERROR(vm, "tail call stack window out of bounds");
                return;
            }
            // Shift new args down to overwrite old locals at bp
            memmove(&vm->stack[vm->bp], &vm->stack[vm->sp - argc],
                    argc * sizeof(unsigned));
            vm->sp = vm->bp + argc;
        }

        if (code->use_locals && !code->has_rest) {
            // Stack locals path: args stay on stack, no env frame needed
            unsigned new_bp = vm->sp - argc;

            if (tail && vm->fp > 0) {
                // Tail call to locals fn: args already at bp after shift above
                vm->bp = new_bp;
                vm->code = code;
                vm->ip = 0;
                vm->env = closure_env;
            } else {
                // Regular call: push frame
                // Save new_bp as restore_sp so RETURN_LOCALS restores correctly
                push_frame(vm, vm->code, vm->ip, vm->bp, new_bp, vm->env);
                vm->bp = new_bp;
                vm->code = code;
                vm->ip = 0;
                vm->env = closure_env;
            }
        } else {
            // Environment frame path (original): build frame from stack values
            if (code->params >= code->const_len) {
                VM_ERROR(vm, "closure has invalid parameter metadata");
                return;
            }
            gc_protect(&closure_env);
            unsigned params = code->constants[code->params];
            gc_protect(&params);
            unsigned new_env;

            if (!code->has_rest) {
                unsigned vals = 0;
                gc_protect(&vals);
                for (unsigned i = argc; i > 0; i--) {
                    vals = alloc_cons(vm->stack[vm->sp - argc + i - 1], vals);
                }
                vm->sp -= argc;
                unsigned frame = alloc_cons(params, vals);
                gc_protect(&frame);
                new_env = alloc_cons(frame, closure_env);
                gc_unprotect(4);
            } else {
                unsigned args = 0;
                gc_protect(&args);
                for (unsigned i = argc; i > 0; i--) {
                    args = alloc_cons(vm->stack[vm->sp - argc + i - 1], args);
                }
                vm->sp -= argc;
                unsigned frame = bind_params(params, args);
                if (frame == TOK_ERROR) {
                    gc_unprotect(3); // args, params, closure_env
                    VM_ERROR(vm, ctx.last_error[0] ? ctx.last_error
                                                   : "wrong number of arguments");
                    return;
                }
                gc_protect(&frame);
                new_env = alloc_cons(frame, closure_env);
                gc_unprotect(4);
            }

            if (tail && vm->fp > 0) {
                // If caller used locals, those are still on the stack
                // above the saved frame. They need to be cleaned up.
                // (The frame.bp points to before the caller's locals)
                vm->code = code;
                vm->ip = 0;
                vm->env = new_env;
            } else {
                push_frame(vm, vm->code, vm->ip, vm->bp, vm->sp, vm->env);
                vm->code = code;
                vm->ip = 0;
                vm->env = new_env;
            }
        }
        return;
    }

    if (IS_FUNCTION(fn)) {
        // Legacy interpreted closure
        unsigned params = car(fn);
        unsigned body_env = cdr(fn);
        unsigned body = car(body_env);
        unsigned def_env = cdr(body_env);

        // Protect these values from GC during argument building
        gc_protect(&params);
        gc_protect(&body);
        gc_protect(&def_env);

        // Build argument list in correct order by iterating in reverse
        unsigned args = 0;
        gc_protect(&args);
        for (unsigned i = argc; i > 0; i--) {
            args = alloc_cons(vm->stack[vm->sp - argc + i - 1], args);
        }

        vm->sp -= argc;

        // Bind parameters
        unsigned frame = bind_params(params, args);
        gc_unprotect(1); // args
        if (frame == TOK_ERROR) {
            gc_unprotect(3); // def_env, body, params
            VM_ERROR(vm, ctx.last_error[0] ? ctx.last_error
                                           : "wrong number of arguments");
            return;
        }
        gc_protect(&frame);
        unsigned new_env = alloc_cons(frame, def_env);
        gc_unprotect(4); // frame, def_env, body, params
        gc_protect(&body);
        gc_protect(&new_env);

        // Evaluate body using interpreter
        // For now, fall back to eval_cps
        // This allows mixed mode operation
        unsigned result = 0;
        unsigned expr = body;
        gc_protect(&expr);
        while (expr) {
            result = eval_cps(car(expr), new_env);
            if (vm->cps_continuation_transfer) {
                vm->cps_continuation_transfer = false;
                gc_unprotect(3);
                return;
            }
            expr = cdr(expr);
        }
        gc_unprotect(3);

        if (result == TOK_ERROR) {
            VM_ERROR(vm, "evaluation error");
            return;
        }

        vm_push(vm, result);
        return;
    }

    if (IS_CELL(fn) && CELL_TYPE(fn) == BT_VMCONT) {
        // VM continuation invocation
        if (!is_vm_continuation_object(fn)) {
            VM_ERROR(vm, "invalid continuation");
            return;
        }
        unsigned value = values_from_argv(argc, &vm->stack[vm->sp - argc]);
        vm->sp -= argc;
        restore_continuation(vm, fn, value);
        return;
    }

    show_error("not a procedure, got %s", type_name(fn));
    VM_ERROR(vm, ctx.last_error[0] ? ctx.last_error : "not a procedure");
}

// ============================================================================
// Main Execution Loop
// ============================================================================

// A TRMC call is truly tail-position only while no operation is pending.
static bool vm_can_tailcall(vm_state *vm)
{
    if (vm->code->trmc_mode == TRMC_MODE_NONE)
        return true;
    unsigned slot = vm->bp + vm->code->trmc_slot;
    unsigned last = vm->code->trmc_mode == TRMC_MODE_HOLE ? slot + 1 : slot;
    return slot >= vm->bp && last >= slot && last < vm->sp &&
           vm->stack[last] == 0;
}

// Decide whether a call in tail position may really replace this activation
// (see vm_can_tailcall). Sets vm->error if the TRMC state is malformed.
static bool vm_prepare_tailcall(vm_state *vm)
{
    bool tail = vm_can_tailcall(vm);
    if (!tail && vm->code->trmc_mode == TRMC_MODE_HOLE) {
        unsigned tail_idx;
        if (!vm_local_index(vm, vm->code->trmc_slot + 1, &tail_idx))
            return false;
        // A rebound callee may capture a continuation, unlike the statically
        // proven self-call. Freeze the pending prefix: each eventual return
        // must copy it rather than mutate a chain shared with that capture.
        vm->stack[tail_idx] = ctx.atom_false;
    }
    return tail;
}

static void vm_apply_tailcall(vm_state *vm, unsigned fn, unsigned argc)
{
    bool tail = vm_prepare_tailcall(vm);
    if (vm->error)
        return;
    vm_apply(vm, fn, argc, tail);
}

static unsigned vm_dispatch(vm_state *vm)
{
    // Register VM with GC system. Link to the suspended enclosing VM so the
    // GC can forward the entire chain, not just the innermost VM.
    vm_state *saved_active_vm = active_vm;
    vm->parent = saved_active_vm;
    active_vm = vm;

    while (vm->running) {
        if (!vm->code) {
            VM_ERROR(vm, "null code object");
            break;
        }
        if (!vm->code->code) {
            VM_ERROR(vm, "null code array");
            break;
        }
        if (vm->ip >= vm->code->code_len) {
            VM_ERROR(vm, "instruction pointer out of bounds");
            break;
        }

        unsigned op = vm->code->code[vm->ip++];

        // Fast paths for most common opcodes (skip switch dispatch overhead)
        if (op == OP_CONST) {
            unsigned idx = vm->code->code[vm->ip++];
            unsigned const_val;
            if (!vm_get_const(vm->code, idx, &const_val)) {
                VM_ERROR(vm, "constant index out of bounds");
                break;
            }
            unsigned val = vm_unbox_fixnum_if_possible(const_val);
            if (__builtin_expect(vm->sp < vm->stack_cap, 1)) {
                vm->stack[vm->sp++] = val;
            } else {
                vm_push(vm, val);
            }
            continue;
        }

        if (op == OP_LOOKUP) {
            int64_t sym_id = vm->code->code[vm->ip];
            unsigned cache_word = vm->ip + 1;
            vm->ip += 3;
            unsigned val = ic_lookup(vm->code, cache_word, sym_id, vm->env);
            if (__builtin_expect(val != TOK_ERROR, 1)) {
                val = vm_unbox_fixnum_if_possible(val);
                if (__builtin_expect(vm->sp < vm->stack_cap, 1)) {
                    vm->stack[vm->sp++] = val;
                } else {
                    vm_push(vm, val);
                }
                continue;
            }
            vm_show_undefined_variable(sym_id);
            vm_signal_error(vm, ctx.last_error);
            continue; // continue the dispatch loop (handler may have been set up)
        }

        // OP_CONST and OP_LOOKUP never reach the switch: the fast paths
        // above handle every instance.
        switch (op) {
        case OP_POP:
            vm_pop(vm);
            break;

        case OP_DUP:
            vm_push(vm, vm_peek(vm, 0));
            break;

        case OP_SWAP: {
            unsigned a = vm_pop(vm);
            unsigned b = vm_pop(vm);
            vm_push(vm, a);
            vm_push(vm, b);
            break;
        }

        case OP_DEFINE: {
            int64_t sym_id = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
            if (vm->error)
                break;
            val = ensure_boxed(val);
            // Protect val before alloc() which can trigger GC
            gc_protect(&val);
            // Create atom for variable name
            unsigned atom = alloc();
            CELL_TYPE(atom) = BT_ATOM;
            CELL_ID(atom) = sym_id;
            gc_protect(&atom);
            if (defvar(atom, val, vm->env) == TOK_ERROR) {
                gc_unprotect(2);
                VM_ERROR_BREAK(vm, "define: invalid environment");
            }
            gc_unprotect(2);
            break;
        }

        case OP_DEFINE_ALIAS: {
            int64_t alias_id = vm->code->code[vm->ip++];
            int64_t target_id = vm->code->code[vm->ip++];
            unsigned target_cell = env_find_binding_cell(target_id, vm->env);
            if (!target_cell) {
                VM_ERROR(vm, "unbound variable in alias definition");
                break;
            }
            GC_GUARD;
            gc_protect(&target_cell);
            unsigned alias = alloc();
            gc_protect(&alias);
            CELL_TYPE(alias) = BT_ATOM;
            CELL_ID(alias) = alias_id;
            unsigned target = alloc();
            gc_protect(&target);
            CELL_TYPE(target) = BT_ATOM;
            CELL_ID(target) = target_id;
            if (defvar_alias(alias, target, target_cell, vm->env) == TOK_ERROR)
                VM_ERROR(vm, "define-alias: invalid environment");
            break;
        }

        case OP_DEFINE_ALIAS_REF: {
            // Alias to a binding resolved at macro-expansion time; using the
            // embedded cell (not a runtime name lookup) keeps a macro's
            // reference to a definition-site global from being captured by
            // a use-site local of the same name
            int64_t alias_id = vm->code->code[vm->ip++];
            unsigned const_idx = vm->code->code[vm->ip++];
            if (const_idx >= vm->code->const_len) {
                VM_ERROR(vm, "alias reference constant out of bounds");
                break;
            }
            unsigned binding_ref = vm->code->constants[const_idx];
            if (!IS_BINDING_REF(binding_ref)) {
                VM_ERROR(vm, "invalid alias reference constant");
                break;
            }
            unsigned target_cell = CELL_CAR(binding_ref);
            unsigned target_var = CELL_CDR(binding_ref);
            GC_GUARD;
            gc_protect(&target_cell);
            gc_protect(&target_var);
            unsigned alias = alloc();
            CELL_TYPE(alias) = BT_ATOM;
            CELL_ID(alias) = alias_id;
            gc_protect(&alias);
            if (defvar_alias(alias, target_var, target_cell, vm->env) ==
                TOK_ERROR)
                VM_ERROR(vm, "define-alias: invalid environment");
            break;
        }

        case OP_SET: {
            int64_t sym_id = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
            if (vm->error)
                break;
            val = ensure_boxed(val);
            unsigned result = setvar(sym_id, val, vm->env);
            if (result == TOK_ERROR) {
                VM_ERROR(vm, "unbound variable in set!");
                break;
            }
            vm_push(vm, val);
            break;
        }

        case OP_SET_VOID: {
            // SET + POP fused: set variable, discard old value (no push)
            int64_t sym_id = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
            if (vm->error)
                break;
            val = ensure_boxed(val);
            unsigned result = setvar(sym_id, val, vm->env);
            if (result == TOK_ERROR) {
                VM_ERROR(vm, "unbound variable in set!");
            }
            break;
        }

        case OP_TRMC_INIT: {
            // The accumulator sits just above the parameters, on the operand
            // stack rather than in vm_state: the collectors trace the whole
            // stack, so the slots are rooted for free, and being per-frame
            // each activation of the function gets its own - a vm_state field
            // would be shared between an outer and an inner one.
            // HOLE needs head and tail, FOLD needs only the pending list.
            vm_push(vm, 0);
            if (vm->code->trmc_mode == TRMC_MODE_HOLE)
                vm_push(vm, 0);
            break;
        }

        case OP_TRMC_PUSH: {
            unsigned symbol_index = vm->code->code[vm->ip++];
            unsigned symbol = 0;
            if (!vm_get_const(vm->code, symbol_index, &symbol) || !IS_ATOM(symbol)) {
                VM_ERROR(vm, "invalid deferred operator");
                break;
            }
            // FOLD mode. Prepending allocates but mutates nothing, so a
            // continuation captured anywhere in the body restores its own
            // accumulator value and rebuilds independently. That is the whole
            // reason this mode needs no restriction on the body.
            unsigned v = vm_pop(vm);
            if (vm->error)
                break;
            unsigned slot;
            if (!vm_local_index(vm, vm->code->trmc_slot, &slot))
                break;
            // alloc_cons can collect; v and the accumulator both have to
            // survive it. The slot is on the traced stack, v is not any more.
            GC_GUARD;
            gc_protect(&v);
            // Keep the lexical environment, not the current operator value:
            // arguments (including recursion) finish before callee lookup.
            unsigned pending = alloc_cons(vm->code->constants[symbol_index], vm->env);
            gc_protect(&pending);
            pending = alloc_cons(v, pending);
            vm->stack[slot] = alloc_cons(pending, vm->stack[slot]);
            break;
        }

        case OP_TRMC_APPEND: {
            unsigned v = vm_pop(vm);
            if (vm->error)
                break;
            unsigned base, tail_idx;
            if (!vm_local_index(vm, vm->code->trmc_slot, &base) ||
                !vm_local_index(vm, vm->code->trmc_slot + 1, &tail_idx))
                break;
            // list_append allocates, so it roots head, tail and v itself.
            // The stack slots still hold the pre-collection values while it
            // runs; the collectors forward those in place, and the copies
            // written back afterwards are the forwarded ones.
            unsigned head = vm->stack[base];
            unsigned tail = vm->stack[tail_idx];
            list_append(&head, &tail, v);
            vm->stack[base] = head;
            vm->stack[tail_idx] = tail;
            break;
        }

        case OP_TRMC_SPLICE: {
            // The append case. append copies every argument but the last, so
            // splicing a copy of this one onto the accumulator is the same
            // work the untransformed call would do - just done outermost
            // first, into a chain whose end stays open for the recursion's
            // result. That is also why the left fold is not quadratic here:
            // nothing is recopied on the next iteration.
            unsigned v = vm_pop(vm);
            if (vm->error)
                break;
            unsigned base, tail_idx;
            if (!vm_local_index(vm, vm->code->trmc_slot, &base) ||
                !vm_local_index(vm, vm->code->trmc_slot + 1, &tail_idx))
                break;
            unsigned head = vm->stack[base];
            unsigned tail = vm->stack[tail_idx];
            bool ok = list_append_copy(&head, &tail, v, "append");
            vm->stack[base] = head;
            vm->stack[tail_idx] = tail;
            if (!ok)
                VM_ERROR(vm, ctx.last_error[0] ? ctx.last_error
                                               : "append: improper list");
            break;
        }

        case OP_RETURN_LOCALS: {
            vm->ip++; // skip operand
            unsigned val = vm_pop(vm);
            if (vm->code->trmc_mode == TRMC_MODE_HOLE) {
                // Close the chain: the value this return produces fills the
                // hole at the end of the accumulator. An empty accumulator
                // means the function never appended, so the value stands.
                unsigned base, tail_idx;
                if (!vm_local_index(vm, vm->code->trmc_slot, &base) ||
                    !vm_local_index(vm, vm->code->trmc_slot + 1, &tail_idx))
                    break;
                unsigned tail = vm->stack[tail_idx];
                if (tail == ctx.atom_false) {
                    unsigned args[2] = {vm->stack[base], val};
                    val = apply_primitive_argv(PAPPEND, 2, args);
                    if (val == TOK_ERROR) {
                        VM_ERROR(vm, "recursive call prefix copy failed");
                        break;
                    }
                } else if (tail) {
                    cell_set_cdr(tail, val);
                    val = vm->stack[base];
                }
            } else if (vm->code->trmc_mode == TRMC_MODE_FOLD) {
                // Replay the pending operations. The accumulator holds them
                // innermost-first, which is the order the untransformed
                // function would have applied them in as the recursion
                // unwound - so this is the same sequence of calls on the same
                // values, not a reassociation.
                unsigned acc_slot;
                if (!vm_local_index(vm, vm->code->trmc_slot, &acc_slot))
                    break;
                if (vm->stack[acc_slot]) {
                    unsigned cell = vm->stack[acc_slot];
                    unsigned pending = car(cell);
                    unsigned binding = cdr(pending);
                    int64_t symbol = CELL_ID(car(binding));
                    unsigned fn = lookup_silent(symbol, cdr(binding));
                    if (fn == TOK_ERROR) {
                        vm_show_undefined_variable(symbol);
                        vm_signal_error(vm, ctx.last_error);
                        break;
                    }
                    // Re-enter this return after one pending application.
                    // Normal VM frames make captured continuations replay the
                    // remaining immutable accumulator independently.
                    vm->stack[acc_slot] = cdr(cell);
                    vm_push(vm, car(pending));
                    vm_push(vm, val);
                    vm->ip -= 2;
                    vm_apply(vm, fn, 2, vm->stack[acc_slot] == 0);
                    break;
                }
            }
            if (vm->fp == 0) {
                vm->sp = vm->bp;
                vm_push(vm, val);
                vm->running = false;
            } else {
                unsigned restore_sp = vm->frames[vm->fp - 1].sp;
                pop_frame(vm);
                vm->sp = restore_sp;
                vm_push(vm, val);
            }
            break;
        }

        // Stack locals — direct access without environment lookup
        case OP_LOCAL_GET: {
            unsigned slot = vm->code->code[vm->ip++];
            unsigned idx;
            if (!vm_local_index(vm, slot, &idx))
                break;
            vm_push(vm, vm->stack[idx]);
            break;
        }
        case OP_LOCAL_GET0: {
            unsigned idx;
            if (!vm_local_index(vm, 0, &idx))
                break;
            vm_push(vm, vm->stack[idx]);
            break;
        }
        case OP_LOCAL_GET1: {
            unsigned idx;
            if (!vm_local_index(vm, 1, &idx))
                break;
            vm_push(vm, vm->stack[idx]);
            break;
        }
        case OP_LOCAL_GET2: {
            unsigned idx;
            if (!vm_local_index(vm, 2, &idx))
                break;
            vm_push(vm, vm->stack[idx]);
            break;
        }
        case OP_LOCAL_GET3: {
            unsigned idx;
            if (!vm_local_index(vm, 3, &idx))
                break;
            vm_push(vm, vm->stack[idx]);
            break;
        }
        case OP_LOCAL_SET: {
            unsigned slot = vm->code->code[vm->ip++];
            unsigned idx;
            if (!vm_local_index(vm, slot, &idx))
                break;
            unsigned val = vm_pop(vm);
            if (vm->error)
                break;
            vm->stack[idx] = val;
            vm_push(vm, val);
            break;
        }
        case OP_LOCAL_SET_VOID: {
            unsigned slot = vm->code->code[vm->ip++];
            unsigned idx;
            if (!vm_local_index(vm, slot, &idx))
                break;
            unsigned val = vm_pop(vm);
            if (vm->error)
                break;
            vm->stack[idx] = val;
            break;
        }

        case OP_CLOSURE: {
            unsigned child_idx = vm->code->code[vm->ip++];
            if (child_idx >= vm->code->children_len ||
                !vm_code_is_well_formed(vm->code->children[child_idx])) {
                VM_ERROR(vm, "closure child index out of bounds");
                break;
            }
            code_object *child = vm->code->children[child_idx];

            // Create code pointer cell (stores code_object* in ptr field)
            unsigned code_cell = alloc();
            CELL_TYPE(code_cell) = BT_CLOSURE;
            CELL_PTR(code_cell) = child;

            // Create closure cell: car = code_cell, cdr = env. It is built
            // as a pair and retagged so pair operations do not accept it.
            // Protect code_cell from GC during alloc_cons
            gc_protect(&code_cell);
            unsigned closure = alloc_cons(code_cell, vm->env);
            gc_unprotect(1);
            CELL_TYPE(closure) = BT_VMCLOSURE;

            vm_push(vm, closure);
            break;
        }

        case OP_CALL: {
            unsigned argc = vm->code->code[vm->ip++];
            unsigned fn = vm_pop(vm);
            vm_apply(vm, fn, argc, false);
            break;
        }

        case OP_TAILCALL: {
            unsigned argc = vm->code->code[vm->ip++];
            unsigned fn = vm_pop(vm);
            vm_apply(vm, fn, argc, true);
            break;
        }

        case OP_TAILCALL_TRMC: {
            // A tail call from inside a TRMC body. While the accumulator is
            // empty this function's return would hand the value straight
            // back, so a real tail call is exactly equivalent - and R7RS
            // requires one, which a plain demotion to OP_CALL would break
            // unboundedly for a mutual recursion cycling through here.
            // Once something is pending the value has to come back to be
            // folded in, which is the same reason (cons x (f y)) was never a
            // tail call to begin with.
            unsigned argc = vm->code->code[vm->ip++];
            unsigned fn = vm_pop(vm);
            if (vm->error)
                break;
            vm_apply_tailcall(vm, fn, argc);
            break;
        }

        case OP_GUARD_PRIMITIVE: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned symbol = vm->code->code[vm->ip++];
            unsigned expected = vm->code->code[vm->ip++];
            unsigned argc = vm->code->code[vm->ip++];
            bool tail = vm->code->code[vm->ip++];
            unsigned cache_word = vm->ip;
            vm->ip += 2;
            unsigned fn = ic_lookup(vm->code, cache_word, symbol, vm->env);
            if (fn == TOK_ERROR) {
                vm_show_undefined_variable(symbol);
                vm_signal_error(vm, ctx.last_error);
            } else if (!IS_BUILTIN(fn) || CELL_ID(fn) != expected) {
                vm->ip = target;
                if (tail)
                    vm_apply_tailcall(vm, fn, argc);
                else
                    vm_apply(vm, fn, argc, false);
            }
            break;
        }

        case OP_RECURSE: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned argc = vm->code->code[vm->ip++];
            unsigned depth = vm->code->code[vm->ip++];
            unsigned fn = vm_pop(vm);
            if (vm->error)
                break;
            unsigned entry_env = vm->env;
            while (depth && IS_PAIR(entry_env)) {
                entry_env = cdr(entry_env);
                depth--;
            }
            if (depth) {
                VM_ERROR(vm, "recursive call environment depth out of bounds");
                break;
            }
            if (vm->code->use_locals && !vm->code->has_rest &&
                argc == vm->code->arity && is_bytecode_closure_object(fn) &&
                GET_CLOSURE_CODE(fn) == vm->code &&
                GET_CLOSURE_ENV(fn) == entry_env) {
                unsigned extra = vm->code->trmc_mode == TRMC_MODE_HOLE ? 2 :
                    (vm->code->trmc_mode == TRMC_MODE_FOLD ? 1 : 0);
                if (argc > vm->sp || vm->bp > vm->sp - argc ||
                    argc > vm->sp - argc - vm->bp ||
                    extra > vm->sp - argc - vm->bp - argc) {
                    VM_ERROR(vm, "recursive call stack window out of bounds");
                    break;
                }
                // Keep the TRMC accumulator after the parameter window.
                // Parameter names may be shadowed in the current scope; the
                // callee's original slots are always the first argc locals.
                memmove(&vm->stack[vm->bp], &vm->stack[vm->sp - argc],
                        argc * sizeof(unsigned));
                vm->sp = vm->bp + argc + extra;
                vm->env = entry_env;
                vm->ip = target;
            } else {
                vm_apply_tailcall(vm, fn, argc);
            }
            break;
        }

        case OP_RETURN: {
            unsigned val = vm_pop(vm);
            if (vm->fp == 0) {
                // Top-level return
                vm_push(vm, val);
                vm->running = false;
            } else {
                pop_frame(vm); // restores code, ip, bp, env
                vm_push(vm, val);
            }
            break;
        }

        case OP_ERROR_RETURN: {
            // An exception handler dispatched by vm_signal_error returned
            // normally instead of jumping to a continuation. If it was the
            // default handler, its return IS the designed unhandled-error
            // path (it already printed "Unhandled exception: ..."); propagate
            // TOK_ERROR silently to the C boundary, which restores baseline
            // exception state. Any other handler returning from a
            // non-continuable exception is an R7RS error.
            unsigned dflt = lookup_silent(
                intern("*default-exception-handler*"),
                exception_state_env(vm->env));
            if (dflt != TOK_ERROR && vm->signal_handler == dflt) {
                VM_ERROR_BREAK(vm, NULL);
            } else {
                VM_ERROR_BREAK(
                    vm, "handler returned from non-continuable exception");
            }
        }

        case OP_LOAD_STEP: {
            // Stack: [.. remaining-forms last-value]; see vm_handle_load.
            if (vm->sp < 2)
                VM_ERROR_BREAK(vm, "load: corrupt driver state");
            unsigned forms = vm->stack[vm->sp - 2];
            if (!IS_PAIR(forms)) {
                unsigned val = vm_pop(vm);
                vm_pop(vm);
                if (vm->fp == 0) {
                    vm_push(vm, val);
                    vm->running = false;
                } else {
                    pop_frame(vm); // back to load's caller
                    vm_push(vm, val);
                }
                break;
            }
            // Compile while the form list is still rooted on the stack.
            vm->stack[vm->sp - 1] = 0;
            // Safe point, as in vm_handle_eval: the previous unit has
            // returned and the next is not compiled yet.
            gc_sweep_code_objects_if_due();
            code_object *unit = compile_for_eval(car(forms), vm->env);
            if (!unit)
                VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                     : "load: compilation failed");
            if (!vm_code_is_well_formed(unit))
                VM_ERROR_BREAK(vm, "load: invalid bytecode");
            forms = vm->stack[vm->sp - 2];
            vm->stack[vm->sp - 2] = cdr(forms);
            vm->sp--; // previous value
            // The unit's RETURN lands back on this instruction.
            push_frame(vm, vm->code, vm->ip - 1, vm->bp, vm->sp, vm->env);
            if (vm->error)
                break;
            vm->code = unit;
            vm->ip = 0;
            vm->bp = vm->sp;
            break;
        }

        case OP_CWV_FINISH: {
            // The producer arranged by PCALLWITHVALUES has returned.
            // Stack: [.., consumer, producer-result]. Unpack the result and
            // apply the consumer; its return (or the OP_RETURN following
            // this instruction, for synchronous appliers) pops the frame
            // saved before the producer ran.
            unsigned result = vm_pop(vm);
            unsigned consumer = vm_pop(vm);
            GC_GUARD;
            gc_protect(&result);
            gc_protect(&consumer);
            unsigned consumer_argc = 0;
            if (IS_MULTIVAL(result)) {
                unsigned vals = car(result);
                if (!vm_push_multiple_values(vm, vals, &consumer_argc))
                    break;
            } else {
                vm_push(vm, result);
                consumer_argc = 1;
            }
            // The consumer's continuation is that of call-with-values itself,
            // so it replaces this finisher activation (R7RS requires a tail
            // call). The OP_RETURN after this instruction is still reached
            // for primitives, which push their result synchronously.
            vm_apply(vm, consumer, consumer_argc, true);
            break;
        }

        case OP_JUMP: {
            unsigned target = vm->code->code[vm->ip++];
            vm->ip = target;
            break;
        }

        case OP_JUMPIF: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
            if (IS_TRUTHY(val)) { // Only #f is falsy
                vm->ip = target;
            }
            break;
        }

        case OP_JUMPIFNOT: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
            if (IS_FALSE(val)) { // Only #f is falsy
                vm->ip = target;
            }
            break;
        }

        case OP_PRIM: {
            int64_t prim_id = vm->code->code[vm->ip++];
            unsigned argc_word = vm->code->code[vm->ip++];
            unsigned argc = argc_word & ~PRIM_TAIL_FLAG;

            // Ensure all args are boxed cell indices for primitives
            if (!vm_box_stack_args(vm, argc))
                break;

            // Get pointer to arguments on stack (no list building needed)
            unsigned *argv = &vm->stack[vm->sp - argc];

            // Primitives that need the VM (apply, call-with-values, eval,
            // load, raise-now, ...). The compiler flags a call in tail
            // position; a TRMC body may still have to keep its frame.
            bool tail = (argc_word & PRIM_TAIL_FLAG) && vm_prepare_tailcall(vm);
            if (vm->error)
                break;
            if (vm_apply_special_primitive(vm, prim_id, argc, argv, tail))
                break;

            // Regular primitive - pass stack values directly as argv
            // Note: We keep argv on the stack during the call for GC protection,
            // then pop and push result in one operation (replacing args with result)
            unsigned result = apply_primitive_argv(prim_id, argc, argv);
            vm->sp -= argc; // Pop args after primitive returns
            if (result == TOK_ERROR) {
                vm_signal_error(vm, ctx.last_error[0] ? ctx.last_error
                                                      : "primitive error");
                break;
            }
            vm_push(vm, result);
            break;
        }

        case OP_PUSHCONT: {
            unsigned cont = capture_continuation(vm);
            vm_push(vm, cont);
            break;
        }

        case OP_CALLCC: {
            // Stack: [proc]
            unsigned proc = vm_pop(vm);
            gc_protect(
                &proc); // Protect across capture_continuation which allocs
            unsigned cont = capture_continuation(vm);
            gc_unprotect(1);
            vm_push(vm, cont);
            vm_apply(vm, proc, 1, false);
            break;
        }

        case OP_PUSHENV: {
            // Push new empty frame onto environment
            unsigned frame = alloc_cons(0, 0);
            gc_protect(&frame);
            vm->env = alloc_cons(frame, vm->env);
            gc_unprotect(1);
            break;
        }

        case OP_POPENV: {
            // Pop frame from environment
            unsigned old_env = vm->env;
            if (vm->env) {
                vm->env = cdr(vm->env);
            }
            // Normally OP_LETREC_DONE already removed this mark. Keep this
            // defensive cleanup for malformed/unusual bytecode without
            // disturbing an outer initializer while a callee pops its scopes.
            if (vm->letrec_depth > 0 &&
                vm->letrecs[vm->letrec_depth - 1].frame == old_env)
                vm->letrec_depth--;
            break;
        }

        case OP_LETREC_MARK: {
            // Mark the current environment frame as being letrec-initialized
            unsigned count = vm->code->code[vm->ip++];
            (void)vm_push_letrec(vm, vm->env, count);
            break;
        }

        case OP_LETREC_DONE: {
            // End the innermost active letrec initialization.
            if (vm->letrec_depth == 0) {
                VM_ERROR(vm, "letrec initialization stack underflow");
                break;
            }
            vm->letrec_depth--;
            break;
        }

        // OP_CALLWITHVALUES intentionally has no handler: no compiler path
        // emits it (call-with-values goes through OP_PRIM), and a bytecode
        // producer cannot be run to completion from the middle of the
        // dispatch loop. Emitting it is a compiler bug, reported as an
        // unknown opcode below.

        case OP_DEFSYNTAX: {
            int64_t sym_id = vm->code->code[vm->ip++];
            unsigned transformer_form = vm_pop(vm);

            if (!syntax_rules_valid(transformer_form,
                                    syntax_default_ellipsis_id(vm->env),
                                    "define-syntax")) {
                VM_ERROR(vm, "define-syntax: invalid syntax-rules");
                break;
            }

            // CRITICAL: Protect transformer_form before any allocations
            gc_protect(&transformer_form);
            unsigned transformer = make_syntax_transformer(transformer_form,
                                                           vm->env);
            if (transformer == TOK_ERROR) {
                VM_ERROR(vm, "define-syntax: transformer creation failed");
                gc_unprotect(1);
                break;
            }
            gc_protect(&transformer);

            // Define in current environment
            unsigned atom = alloc();
            CELL_TYPE(atom) = BT_ATOM;
            CELL_ID(atom) = sym_id;
            gc_protect(&atom);
            if (defvar(atom, transformer, vm->env) == TOK_ERROR) {
                gc_unprotect(3);
                VM_ERROR_BREAK(vm, "define-syntax: invalid environment");
            }
            gc_unprotect(3);
            break;
        }

        case OP_HALT: {
            vm->running = false;
            break;
        }

        // Specialized opcodes for common operations
        case OP_CAR: {
            unsigned pair = vm_pop(vm);
            VM_CHECK_PAIR(vm, pair, "car: not a pair");
            vm_push(vm, car(pair));
            break;
        }

        case OP_CDR: {
            unsigned pair = vm_pop(vm);
            VM_CHECK_PAIR(vm, pair, "cdr: not a pair");
            vm_push(vm, cdr(pair));
            break;
        }

        case OP_CONS: {
            unsigned cdr_val = vm_pop(vm);
            unsigned car_val = vm_pop(vm);
            GC_GUARD;
            protect_and_box2(&car_val, &cdr_val);
            unsigned result = alloc_cons(car_val, cdr_val);
            vm_push(vm, result);
            break;
        }

        case OP_NULLP: {
            unsigned val = vm_pop(vm);
            vm_push(vm, IS_NIL(val) ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_PAIRP: {
            unsigned val = vm_pop(vm);
            vm_push(vm, IS_PAIR(val)
                            ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_ADD1: {
            unsigned n = vm_pop(vm);
            if (IS_FIXNUM(n)) {
                int32_t val = FIXNUM_VALUE(n);
                vm_push(vm, val < FIXNUM_MAX ? MAKE_FIXNUM(val + 1)
                                             : store((int64_t)val + 1));
                break;
            }
            unsigned result = vm_add_one(n, 1);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                     : "invalid operand");
            vm_push(vm, result);
            break;
        }

        case OP_SUB1: {
            unsigned n = vm_pop(vm);
            if (IS_FIXNUM(n)) {
                int32_t val = FIXNUM_VALUE(n);
                vm_push(vm, val > FIXNUM_MIN ? MAKE_FIXNUM(val - 1)
                                             : store((int64_t)val - 1));
                break;
            }
            unsigned result = vm_add_one(n, -1);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                     : "invalid operand");
            vm_push(vm, result);
            break;
        }

        case OP_ZEROP: {
            unsigned n = vm_pop(vm);
            if (IS_FIXNUM(n)) {
                vm_push(vm, FIXNUM_VALUE(n) == 0 ? ctx.atom_true : ctx.atom_false);
                break;
            }
            unsigned result = vm_numeq_zero(n);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, "=: invalid operands");
            vm_push(vm, result);
            break;
        }

        case OP_NOT: {
            unsigned val = vm_pop(vm);
            vm_push(vm, IS_FALSE(val) ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_EQ: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            // eq? must compare CELL_ID for atoms/numbers, cell index for others
            bool eq = false;
            if (a == b) {
                // Handles fixnum==fixnum and cell==cell
                eq = true;
            } else if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                eq = false; // Different fixnums (same-value caught above)
            } else if (IS_FIXNUM(a)) {
                // Fixnum vs cell: eq? if cell is BT_NUM with same value
                eq = IS_NUM(b) && CELL_ID(b) == (int64_t)FIXNUM_VALUE(a);
            } else if (IS_FIXNUM(b)) {
                eq = IS_NUM(a) && CELL_ID(a) == (int64_t)FIXNUM_VALUE(b);
            } else if (IS_CELL(a) && IS_CELL(b) &&
                       CELL_TYPE(a) == CELL_TYPE(b)) {
                switch (CELL_TYPE(a)) {
                case BT_ATOM:
                case BT_NUM:
                case BT_CHAR:
                case BT_FUNCTION:
                case BT_BUILTIN:
                    eq = (CELL_ID(a) == CELL_ID(b));
                    break;
                default:
                    break;
                }
            }
            vm_push(vm, eq ? ctx.atom_true : ctx.atom_false);
            break;
        }

        // Specialized arithmetic opcodes (use direct binary operations)
        case OP_ADD: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                int64_t sum = (int64_t)FIXNUM_VALUE(a) + (int64_t)FIXNUM_VALUE(b);
                vm_push(vm, vm_store_integer_result(sum));
                // Quicken: next time, skip type checks
                vm->code->code[vm->ip - 1] = OP_ADD_INT;
                break;
            }
            protect_and_box2(&a, &b);
            unsigned result = binary_add(a, b);
            gc_unprotect(2);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, "+: invalid operands");
            vm_push(vm, result);
            break;
        }

        case OP_SUB: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                int64_t diff = (int64_t)FIXNUM_VALUE(a) - (int64_t)FIXNUM_VALUE(b);
                vm_push(vm, vm_store_integer_result(diff));
                vm->code->code[vm->ip - 1] = OP_SUB_INT;
                break;
            }
            protect_and_box2(&a, &b);
            unsigned result = binary_sub(a, b);
            gc_unprotect(2);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, "-: invalid operands");
            vm_push(vm, result);
            break;
        }

        case OP_MUL: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                int64_t prod = (int64_t)FIXNUM_VALUE(a) * (int64_t)FIXNUM_VALUE(b);
                vm_push(vm, vm_store_integer_result(prod));
                vm->code->code[vm->ip - 1] = OP_MUL_INT;
                break;
            }
            protect_and_box2(&a, &b);
            unsigned result = binary_mul(a, b);
            gc_unprotect(2);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, "*: invalid operands");
            vm_push(vm, result);
            break;
        }

        case OP_DIV: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                int32_t va = FIXNUM_VALUE(a), vb = FIXNUM_VALUE(b);
                if (vb == 0)
                    VM_ERROR_BREAK(vm, "/: division by zero");
                if (va % vb == 0) {
                    int64_t q = (int64_t)va / (int64_t)vb;
                    vm_push(vm, vm_store_integer_result(q));
                    break;
                }
                // Result is rational — box and use full path
                a = store((int64_t)va);
                gc_protect(&a);
                b = store((int64_t)vb);
                gc_protect(&b);
            } else {
                protect_and_box2(&a, &b);
            }
            unsigned result = binary_div(a, b);
            gc_unprotect(2);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, "/: invalid operands or division by zero");
            vm_push(vm, result);
            break;
        }

        case OP_MOD: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                int32_t va = FIXNUM_VALUE(a), vb = FIXNUM_VALUE(b);
                if (vb == 0)
                    VM_ERROR_BREAK(vm, "modulo: division by zero");
                int32_t r = va % vb;
                if ((r > 0 && vb < 0) || (r < 0 && vb > 0))
                    r += vb;
                vm_push(vm, MAKE_FIXNUM(r));
                break;
            }
            protect_and_box2(&a, &b);
            unsigned result = binary_mod(a, b);
            gc_unprotect(2);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, "modulo: invalid operands");
            vm_push(vm, result);
            break;
        }

        // Specialized comparison opcodes (use direct binary operations)
        case OP_LT: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                vm_push(vm, FIXNUM_VALUE(a) < FIXNUM_VALUE(b) ? ctx.atom_true
                                                              : ctx.atom_false);
                break;
            }
            protect_and_box2(&a, &b);
            unsigned result = binary_lt(a, b);
            gc_unprotect(2);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, "<: invalid operands");
            vm_push(vm, result);
            break;
        }

        case OP_GT: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                vm_push(vm, FIXNUM_VALUE(a) > FIXNUM_VALUE(b) ? ctx.atom_true
                                                              : ctx.atom_false);
                break;
            }
            protect_and_box2(&a, &b);
            unsigned result = binary_gt(a, b);
            gc_unprotect(2);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, ">: invalid operands");
            vm_push(vm, result);
            break;
        }

        case OP_LE: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                vm_push(vm, FIXNUM_VALUE(a) <= FIXNUM_VALUE(b) ? ctx.atom_true
                                                               : ctx.atom_false);
                break;
            }
            protect_and_box2(&a, &b);
            unsigned result = binary_le(a, b);
            gc_unprotect(2);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, "<=: invalid operands");
            vm_push(vm, result);
            break;
        }

        case OP_GE: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                vm_push(vm, FIXNUM_VALUE(a) >= FIXNUM_VALUE(b) ? ctx.atom_true
                                                               : ctx.atom_false);
                break;
            }
            protect_and_box2(&a, &b);
            unsigned result = binary_ge(a, b);
            gc_unprotect(2);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, ">=: invalid operands");
            vm_push(vm, result);
            break;
        }

        case OP_NUMEQ: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                vm_push(vm, a == b ? ctx.atom_true : ctx.atom_false);
                break;
            }
            protect_and_box2(&a, &b);
            unsigned result = binary_numeq(a, b);
            gc_unprotect(2);
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, "=: invalid operands");
            vm_push(vm, result);
            break;
        }

        // More list operations
        case OP_CADR: {
            unsigned pair = vm_pop(vm);
            VM_CHECK_PAIR(vm, pair, "cadr: not a pair");
            unsigned inner = cdr(pair);
            VM_CHECK_PAIR(vm, inner, "cadr: cdr not a pair");
            vm_push(vm, car(inner));
            break;
        }

        case OP_CDDR: {
            unsigned pair = vm_pop(vm);
            VM_CHECK_PAIR(vm, pair, "cddr: not a pair");
            unsigned inner = cdr(pair);
            VM_CHECK_PAIR(vm, inner, "cddr: cdr not a pair");
            vm_push(vm, cdr(inner));
            break;
        }

        case OP_SETCAR: {
            unsigned val = vm_pop(vm);
            unsigned pair = vm_pop(vm);
            VM_CHECK_PAIR(vm, pair, "set-car!: not a pair");
            gc_protect(&pair);
            val = ensure_boxed(val);
            gc_unprotect(1);
            cell_set_car(pair, val);
            vm_push(vm, val);
            break;
        }

        case OP_SETCDR: {
            unsigned val = vm_pop(vm);
            unsigned pair = vm_pop(vm);
            VM_CHECK_PAIR(vm, pair, "set-cdr!: not a pair");
            gc_protect(&pair);
            val = ensure_boxed(val);
            gc_unprotect(1);
            cell_set_cdr(pair, val);
            vm_push(vm, val);
            break;
        }

        case OP_LIST1: {
            GC_GUARD;
            unsigned a = vm_pop(vm);
            a = ensure_boxed(a);
            gc_protect(&a);
            unsigned result = alloc_cons(a, 0);
            vm_push(vm, result);
            break;
        }

        case OP_LIST2: {
            GC_GUARD;
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            protect_and_box2(&a, &b);
            // Build bottom-up to avoid unspecified evaluation order
            unsigned tail = alloc_cons(b, 0);
            gc_protect(&tail);
            unsigned result = alloc_cons(a, tail);
            vm_push(vm, result);
            break;
        }

        case OP_LIST3: {
            GC_GUARD;
            unsigned c = vm_pop(vm);
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            protect_and_box3(&a, &b, &c);
            unsigned t2 = alloc_cons(c, 0);
            gc_protect(&t2);
            unsigned t1 = alloc_cons(b, t2);
            gc_protect(&t1);
            unsigned result = alloc_cons(a, t1);
            vm_push(vm, result);
            break;
        }

        // Fused compare-and-jump opcodes
        case OP_JUMPIFNULL: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
            if (val == 0) {
                vm->ip = target;
            }
            break;
        }

        case OP_JUMPIFNOTNULL: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
            if (val != 0) {
                vm->ip = target;
            }
            break;
        }

        case OP_JUMPIFZERO:
        case OP_JUMPIFNOTZERO: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned n = vm_pop(vm);
            bool is_zero;
            if (IS_FIXNUM(n)) {
                is_zero = FIXNUM_VALUE(n) == 0;
            } else {
                unsigned result = vm_numeq_zero(n);
                if (result == TOK_ERROR)
                    VM_ERROR_BREAK(vm, "=: invalid operands");
                is_zero = result == ctx.atom_true;
            }
            if (is_zero == (op == OP_JUMPIFZERO))
                vm->ip = target;
            break;
        }

        // Additional list accessors
        case OP_CAAR: {
            unsigned pair = vm_pop(vm);
            VM_CHECK_PAIR(vm, pair, "caar: not a pair");
            unsigned inner = car(pair);
            VM_CHECK_PAIR(vm, inner, "caar: car not a pair");
            vm_push(vm, car(inner));
            break;
        }

        case OP_CDAR: {
            unsigned pair = vm_pop(vm);
            VM_CHECK_PAIR(vm, pair, "cdar: not a pair");
            unsigned inner = car(pair);
            VM_CHECK_PAIR(vm, inner, "cdar: car not a pair");
            vm_push(vm, cdr(inner));
            break;
        }

        // Type predicates
        case OP_SYMBOLP: {
            unsigned val = vm_pop(vm);
            vm_push(vm, IS_SYMBOL(val) ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_NUMBERP: {
            unsigned val = vm_pop(vm);
            if (IS_FIXNUM(val)) {
                vm_push(vm, ctx.atom_true);
                break;
            }
            bool is_num = (val != 0) && is_numeric(val);
            vm_push(vm, is_num ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_STRINGP: {
            unsigned val = vm_pop(vm);
            vm_push(vm, IS_STRING(val) ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_VECTORP: {
            unsigned val = vm_pop(vm);
            vm_push(vm, IS_VECTOR(val) ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_BOOLEANP: {
            unsigned val = vm_pop(vm);
            vm_push(vm, (val == ctx.atom_true || val == ctx.atom_false)
                            ? ctx.atom_true
                            : ctx.atom_false);
            break;
        }

        case OP_LISTP: {
            unsigned val = vm_pop(vm);
            if (IS_FIXNUM(val)) {
                vm_push(vm, ctx.atom_false);
                break;
            }
            unsigned slow = val;
            unsigned fast = val;
            bool is_list = false;
            while (!IS_FIXNUM(fast) && IS_PAIR(fast)) {
                fast = cdr(fast);
                if (IS_FIXNUM(fast) || !IS_PAIR(fast))
                    break;
                fast = cdr(fast);
                slow = cdr(slow);
                if (slow == fast)
                    break;
            }
            is_list = fast == 0;
            vm_push(vm, is_list ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_INTEGERP: {
            unsigned val = vm_pop(vm);
            if (IS_FIXNUM(val)) {
                vm_push(vm, ctx.atom_true);
                break;
            }
            bool is_int = false;
            if (val != 0 && is_numeric(val)) {
                if (IS_NUM(val) || IS_BIGNUM(val)) {
                    is_int = true;
                } else if (IS_INEXACT(val)) {
                    double d = to_double(val);
                    is_int = isfinite(d) && floor(d) == d;
                }
            }
            vm_push(vm, is_int ? ctx.atom_true : ctx.atom_false);
            break;
        }

        // List operations. These do no more than the primitives, so they
        // call them: the two engines then cannot disagree on what is
        // accepted or on what an error says.
        case OP_LENGTH:
        case OP_APPEND:
        case OP_REVERSE: {
            unsigned argc = op == OP_APPEND ? 2 : 1;
            if (!vm_box_stack_args(vm, argc))
                break;
            // The arguments stay on the stack, rooted, during the call.
            unsigned result = apply_primitive_argv(
                op == OP_LENGTH ? PLENGTH : op == OP_APPEND ? PAPPEND : PREVERSE,
                argc, &vm->stack[vm->sp - argc]);
            vm->sp -= argc;
            if (result == TOK_ERROR)
                VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                     : "invalid list");
            vm_push(vm, result);
            break;
        }

        // Vector operations
        case OP_VECTORREF: {
            unsigned idx = vm_pop(vm);
            unsigned vec = vm_pop(vm);
            vector_data *vd;
            unsigned i;
            if (!vm_expect_vector_index(
                    vm, vec, idx, "vector-ref: not a vector",
                    "vector-ref: index not an integer",
                    "vector-ref: index out of bounds", &vd, &i))
                goto vm_dispatch_error;
            vm_push(vm, vd->data[i]);
            break;
        }

        case OP_VECTORSET: {
            unsigned val = vm_pop(vm);
            unsigned idx = vm_pop(vm);
            unsigned vec = vm_pop(vm);
            unsigned i;
            if (!vm_expect_vector_index(
                    vm, vec, idx, "vector-set!: not a vector",
                    "vector-set!: index not an integer",
                    "vector-set!: index out of bounds", NULL, &i))
                goto vm_dispatch_error;
            gc_protect(&vec);
            val = ensure_boxed(val);
            gc_unprotect(1);
            vector_set_elem(vec, (unsigned)i, val);
            vm_push(vm, val);
            break;
        }

        // Record operations, as prim_record.c's %record-of? and %record-ref
        case OP_RECORDOF: {
            unsigned rtd = vm_pop(vm);
            unsigned obj = vm_pop(vm);
            vm_push(vm, record_of_type(obj, rtd) ? ctx.atom_true
                                                 : ctx.atom_false);
            break;
        }

        case OP_RECORDREF: {
            unsigned who = vm_pop(vm);
            unsigned idx = vm_pop(vm);
            unsigned rtd = vm_pop(vm);
            unsigned rec = vm_pop(vm);
            unsigned *slot = record_field_slot(rec, rtd, idx, who);
            if (!slot) {
                vm_signal_error(vm, ctx.last_error);
                break;
            }
            vm_push(vm, *slot);
            break;
        }

        case OP_VECTORLEN: {
            unsigned vec = vm_pop(vm);
            vector_data *vd =
                vm_expect_vector_data(vm, vec, "vector-length: not a vector");
            if (!vd)
                goto vm_dispatch_error;
            vm_push(vm, store(vd->len));
            break;
        }

        // Numeric operations
        case OP_ABS: {
            unsigned n = vm_pop(vm);
            if (IS_FIXNUM(n)) {
                int32_t val = FIXNUM_VALUE(n);
                if (val >= 0) {
                    vm_push(vm, n);
                } else if (val != FIXNUM_MIN) {
                    vm_push(vm, MAKE_FIXNUM(-val));
                } else {
                    vm_push(vm, store(-(int64_t)val));
                }
                break;
            }
            if (IS_NUM(n)) {
                int64_t val = CELL_ID(n);
                if (val == INT64_MIN) {
                    bignum *bn = bn_from_int(val);
                    if (!bn) {
                        VM_ERROR_BREAK(vm, "abs: out of memory");
                    }
                    bn_neg_ip(bn);
                    vm_push(vm, store_integer(bn));
                } else {
                    vm_push(vm, store(val < 0 ? -val : val));
                }
            } else if (IS_BIGNUM(n)) {
                bignum *bn = get_bignum(n);
                if (!bn) {
                    VM_ERROR_BREAK(vm, "abs: not a number");
                }
                if (bn->sign) {
                    unsigned result =
                        vm_store_bignum_neg(vm, bn, "abs: out of memory");
                    if (result == TOK_ERROR) {
                        VM_ERROR_BREAK(vm, "abs: out of memory");
                    }
                    vm_push(vm, result);
                } else {
                    vm_push(vm, n);
                }
            } else if (IS_INEXACT(n)) {
                vm_push(vm, store_inexact(fabs(to_double(n))));
            } else if (IS_RATIONAL(n)) {
                if (is_negative_number(car(n))) {
                    GC_GUARD;
                    gc_protect(&n);
                    unsigned abs_num = negate_number(car(n));
                    if (abs_num == TOK_ERROR) {
                        VM_ERROR_BREAK(vm, "abs: out of memory");
                    }
                    gc_protect(&abs_num);
                    unsigned denom = cdr(n);
                    gc_protect(&denom);
                    unsigned result = alloc();
                    CELL_TYPE(result) = BT_RATIONAL;
                    CELL_CAR(result) = abs_num;
                    CELL_CDR(result) = denom;
                    vm_push(vm, result);
                } else {
                    vm_push(vm, n);
                }
            } else {
                VM_ERROR(vm, "abs: not a number");
            }
            break;
        }

        // Quickened (type-specialized) arithmetic — assume fixnum operands
        case OP_ADD_INT: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (__builtin_expect(IS_FIXNUM(a) && IS_FIXNUM(b), 1)) {
                int64_t sum = (int64_t)FIXNUM_VALUE(a) + (int64_t)FIXNUM_VALUE(b);
                vm_push(vm, vm_store_integer_result(sum));
            } else {
                // Deopt: rewrite back to generic and re-execute
                vm_deopt_binary_op(vm, OP_ADD, a, b);
                continue;
            }
            break;
        }
        case OP_SUB_INT: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (__builtin_expect(IS_FIXNUM(a) && IS_FIXNUM(b), 1)) {
                int64_t diff = (int64_t)FIXNUM_VALUE(a) - (int64_t)FIXNUM_VALUE(b);
                vm_push(vm, vm_store_integer_result(diff));
            } else {
                vm_deopt_binary_op(vm, OP_SUB, a, b);
                continue;
            }
            break;
        }
        case OP_MUL_INT: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (__builtin_expect(IS_FIXNUM(a) && IS_FIXNUM(b), 1)) {
                int64_t prod = (int64_t)FIXNUM_VALUE(a) * (int64_t)FIXNUM_VALUE(b);
                vm_push(vm, vm_store_integer_result(prod));
            } else {
                vm_deopt_binary_op(vm, OP_MUL, a, b);
                continue;
            }
            break;
        }

        // Quickened compare+branch — assume fixnum operands
        case OP_LT_INT_JUMPIFNOT: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (__builtin_expect(IS_FIXNUM(a) && IS_FIXNUM(b), 1)) {
                if (!(FIXNUM_VALUE(a) < FIXNUM_VALUE(b)))
                    vm->ip = target;
            } else {
                // Deopt: rewrite and re-execute
                vm->code->code[vm->ip - 2] = OP_LT_JUMPIFNOT;
                vm_push(vm, a); vm_push(vm, b);
                vm->ip -= 2;
                continue;
            }
            break;
        }
        case OP_NUMEQ_INT_JUMPIFNOT: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (__builtin_expect(IS_FIXNUM(a) && IS_FIXNUM(b), 1)) {
                if (a != b)
                    vm->ip = target;
            } else {
                vm->code->code[vm->ip - 2] = OP_NUMEQ_JUMPIFNOT;
                vm_push(vm, a); vm_push(vm, b);
                vm->ip -= 2;
                continue;
            }
            break;
        }

        // Fused compare-and-branch
        case OP_NUMEQ_JUMPIFNOT: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            bool eq;
            if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                eq = (a == b);
                vm->code->code[vm->ip - 2] = OP_NUMEQ_INT_JUMPIFNOT;
            } else {
                protect_and_box2(&a, &b);
                unsigned result = binary_numeq(a, b);
                gc_unprotect(2);
                if (result == TOK_ERROR)
                    VM_ERROR_BREAK(vm, "=: invalid operands");
                eq = (result == ctx.atom_true);
            }
            if (!eq)
                vm->ip = target;
            break;
        }

        // Fused comparison + branch opcodes (LT, GT, LE, GE)
        case OP_LT_JUMPIFNOT:
        case OP_GT_JUMPIFNOT:
        case OP_LE_JUMPIFNOT:
        case OP_GE_JUMPIFNOT: {
            unsigned fused_op = vm->code->code[vm->ip - 1]; // already consumed
            unsigned target = vm->code->code[vm->ip++];
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            bool cond;
            if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                int32_t va = FIXNUM_VALUE(a), vb = FIXNUM_VALUE(b);
                switch (fused_op) {
                case OP_LT_JUMPIFNOT: cond = (va < vb);
                    vm->code->code[vm->ip - 2] = OP_LT_INT_JUMPIFNOT; break;
                case OP_GT_JUMPIFNOT: cond = (va > vb); break;
                case OP_LE_JUMPIFNOT: cond = (va <= vb); break;
                case OP_GE_JUMPIFNOT: cond = (va >= vb); break;
                default: cond = false; break;
                }
            } else {
                protect_and_box2(&a, &b);
                unsigned result;
                switch (fused_op) {
                case OP_LT_JUMPIFNOT: result = binary_lt(a, b); break;
                case OP_GT_JUMPIFNOT: result = binary_gt(a, b); break;
                case OP_LE_JUMPIFNOT: result = binary_le(a, b); break;
                case OP_GE_JUMPIFNOT: result = binary_ge(a, b); break;
                default: result = ctx.atom_false; break;
                }
                gc_unprotect(2);
                if (result == TOK_ERROR)
                    VM_ERROR_BREAK(vm, "comparison: invalid operands");
                cond = (result == ctx.atom_true);
            }
            if (!cond)
                vm->ip = target;
            break;
        }

        // Superinstructions: fused LOOKUP + arithmetic
        default:
            VM_ERROR(vm, "unknown opcode");
            break;
        }
vm_dispatch_error:
        // A VM-level error is a Scheme error (R7RS): signal it through the
        // exception system so guard can catch C-level failures such as type
        // errors in quickened opcodes. vm_signal_error dispatches the
        // current handler with the pinned OP_ERROR_RETURN marker as its
        // return address; a handler that jumps to a continuation restores
        // the VM (execution resumes), while one that returns normally falls
        // into the marker and this loop terminates below.
        if (vm->error) {
            if (vm->code == error_return_code) {
                break; // marker: handler returned normally - terminal
            }
            const char *msg = vm->error_msg ? vm->error_msg : "primitive error";
            vm->error = false;
            vm->running = true;
            if (!vm_signal_error(vm, msg)) {
                break; // dispatch failed - terminal
            }
        }
    }

    // Unregister VM from GC system
    active_vm = saved_active_vm;
    vm->parent = NULL;

    if (vm->error) {
        // error_msg is NULL when the default exception handler returned from
        // a raise/error dispatch - that path already printed "Unhandled
        // exception: ..." and propagates TOK_ERROR silently.
        if (vm->error_msg)
            show_error("VM error: %s", vm->error_msg);
        return TOK_ERROR;
    }

    if (vm->sp > 0) {
        unsigned result = vm->stack[vm->sp - 1];
        return IS_FIXNUM(result) ? ensure_boxed(result) : result;
    }
    return 0;
}

unsigned vm_run(vm_state *vm, code_object *code, unsigned env)
{
    LISP_ASSERT_MSG(vm != NULL, "vm_run: null vm");
    if (!vm_code_is_well_formed(code)) {
        vm->error = true;
        vm->error_msg = "invalid bytecode";
        show_error("VM error: %s", vm->error_msg);
        return TOK_ERROR;
    }

    vm->code = code;
    vm->ip = 0;
    vm->env = env;
    // Preserve arguments preloaded by vm_call_closure for stack-local
    // closures; ordinary VM runs always start with an empty operand stack.
    if (!code->use_locals || code->has_rest)
        vm->sp = 0;
    vm->fp = 0;
    vm->bp = 0;
    vm->running = true;
    vm->error = false;
    return vm_dispatch(vm);
}

// ============================================================================
// GC Integration
// ============================================================================

void gc_update_vm_roots(vm_state *vm)
{
    if (!vm)
        return;

    // Update stack values
    for (unsigned i = 0; i < vm->sp; i++) {
        vm->stack[i] = collect(vm->stack[i]);
    }

    // Update frame environments
    for (unsigned i = 0; i < vm->fp; i++) {
        vm->frames[i].env = collect(vm->frames[i].env);
    }

    // Update current environment
    vm->env = collect(vm->env);
    for (unsigned i = 0; i < vm->letrec_depth; i++)
        vm->letrecs[i].frame = collect(vm->letrecs[i].frame);
    vm->signal_handler = collect(vm->signal_handler);

    // Update code object constants
    gc_collect_code(vm->code);
}

// Update VM roots for minor GC (uses collect_to_old instead of collect)
void gc_update_vm_roots_minor(vm_state *vm, unsigned (*collector)(unsigned))
{
    if (!vm)
        return;

    // Update stack values
    for (unsigned i = 0; i < vm->sp; i++) {
        vm->stack[i] = collector(vm->stack[i]);
    }

    // Update frame environments
    for (unsigned i = 0; i < vm->fp; i++) {
        vm->frames[i].env = collector(vm->frames[i].env);
    }

    // Update current environment
    vm->env = collector(vm->env);
    for (unsigned i = 0; i < vm->letrec_depth; i++)
        vm->letrecs[i].frame = collector(vm->letrecs[i].frame);
    vm->signal_handler = collector(vm->signal_handler);
}
