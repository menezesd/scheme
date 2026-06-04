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
#include "utf8.h"
#include "writer.h"
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ============================================================================
// VM Constants
// ============================================================================

#define IC_UNCACHED 0xFFFFFFFF

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

static bool vm_integer_is_odd(vm_state *vm, unsigned n, const char *error_msg,
                              bool *odd_out)
{
    if (IS_FIXNUM(n)) {
        *odd_out = (FIXNUM_VALUE(n) & 1) != 0;
        return true;
    }
    if (IS_NUM(n)) {
        *odd_out = (CELL_ID(n) & 1) != 0;
        return true;
    }
    if (IS_BIGNUM(n)) {
        bignum *bn = get_bignum(n);
        if (!bn) {
            VM_ERROR(vm, error_msg);
            return false;
        }
        *odd_out = bn->len > 0 && (bn->limbs[0] & 1) != 0;
        return true;
    }
    VM_ERROR(vm, error_msg);
    return false;
}

static bool vm_is_jump_opcode(unsigned op)
{
    switch (op) {
    case OP_JUMP:
    case OP_JUMPIF:
    case OP_JUMPIFNOT:
    case OP_JUMPIFNULL:
    case OP_JUMPIFNOTNULL:
    case OP_JUMPIFZERO:
    case OP_JUMPIFNOTZERO:
    case OP_NUMEQ_JUMPIFNOT:
    case OP_LT_JUMPIFNOT:
    case OP_GT_JUMPIFNOT:
    case OP_LE_JUMPIFNOT:
    case OP_GE_JUMPIFNOT:
    case OP_NUMEQ_INT_JUMPIFNOT:
    case OP_LT_INT_JUMPIFNOT:
        return true;
    default:
        return false;
    }
}

static bool vm_instruction_starts_at(const code_object *code, unsigned target)
{
    if (!code || target > code->code_len)
        return false;
    for (unsigned i = 0; i < code->code_len;) {
        if (i == target)
            return true;
        unsigned size = instruction_size(code->code[i]);
        if (size == 0 || size > code->code_len - i)
            return false;
        i += size;
    }
    return target == code->code_len;
}

static bool vm_code_is_well_formed(const code_object *code)
{
    if (!code || !code->code || code->code_len == 0)
        return false;
    for (unsigned i = 0; i < code->code_len;) {
        unsigned op = code->code[i];
        if (op >= OP_COUNT)
            return false;
        unsigned size = instruction_size(op);
        if (size == 0 || size > code->code_len - i)
            return false;
        if (vm_is_jump_opcode(op)) {
            unsigned target = code->code[i + 1];
            if (target > code->code_len ||
                !vm_instruction_starts_at(code, target))
                return false;
        }
        i += size;
    }
    return true;
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

static unsigned vm_apply_with_one(unsigned n,
                                  unsigned (*prim)(unsigned, unsigned *))
{
    GC_GUARD;
    n = ensure_boxed(n);
    gc_protect(&n);
    unsigned one = store(1);
    unsigned argv[2] = {n, one};
    gc_protect(&argv[0]);
    gc_protect(&argv[1]);
    return prim(2, argv);
}

static unsigned vm_store_bignum_add_one(vm_state *vm, bignum *bn)
{
    bignum *one = bn_from_int(1);
    bignum *result = one ? bn_add(bn, one) : NULL;
    bn_free(bn);
    bn_free(one);
    if (!result) {
        VM_ERROR(vm, "add1: out of memory");
        return TOK_ERROR;
    }
    return store_integer(result);
}

static unsigned vm_store_bignum_sub_one(vm_state *vm, bignum *bn)
{
    bignum *one = bn_from_int(1);
    bignum *result = one ? bn_sub(bn, one) : NULL;
    bn_free(bn);
    bn_free(one);
    if (!result) {
        VM_ERROR(vm, "sub1: out of memory");
        return TOK_ERROR;
    }
    return store_integer(result);
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

// Type check macros that break on failure (handle fixnums safely)
#define VM_CHECK_PAIR(vm, val, msg)                                            \
    do {                                                                       \
        if (IS_FIXNUM(val) || !IS_PAIR(val))                                   \
            VM_ERROR_BREAK(vm, msg);                                           \
    } while (0)

#define VM_CHECK_TYPE(vm, val, expected_type, msg)                             \
    do {                                                                       \
        if (IS_FIXNUM(val) || CELL_TYPE(val) != (expected_type))               \
            VM_ERROR_BREAK(vm, msg);                                           \
    } while (0)

// ============================================================================
// Inline Cached Lookup
// ============================================================================

// Perform a variable lookup with inline cache. On cache hit (depth/offset are
// valid and verified), walks depth frames and offset values — O(depth+offset).
// On miss, does a full lookup and fills the cache in the bytecode.
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

static bool vm_try_lookup_frame(int64_t sym_id, unsigned vars, unsigned vals,
                                unsigned *value_out, unsigned *val_cell_out,
                                unsigned *offset_out)
{
    unsigned offset = 0;

    while (vars) {
        if (IS_ATOM(vars)) {
            if (!IS_PAIR(vals))
                return false;
            if (CELL_ID(vars) == sym_id) {
                if (!vm_try_deref_binding_value(car(vals), value_out))
                    return false;
                if (val_cell_out)
                    *val_cell_out = vals;
                if (offset_out)
                    *offset_out = offset;
                return true;
            }
            break;
        }

        if (!IS_PAIR(vars) || !IS_PAIR(vals))
            return false;

        unsigned var = car(vars);
        if (!IS_ATOM(var))
            return false;
        if (CELL_ID(var) == sym_id) {
            if (!vm_try_deref_binding_value(car(vals), value_out))
                return false;
            if (val_cell_out)
                *val_cell_out = vals;
            if (offset_out)
                *offset_out = offset;
            return true;
        }

        vars = cdr(vars);
        vals = cdr(vals);
        offset++;
    }

    return false;
}

static inline unsigned ic_lookup(int64_t sym_id, unsigned env,
                                 unsigned *cache_slot)
{
    unsigned cached_depth = cache_slot[0];
    unsigned cached_offset = cache_slot[1];

    if (cached_depth != IC_UNCACHED) {
        // Try cached path: walk depth frames, offset values, verify sym_id
        unsigned e = env;
        unsigned frame = 0;
        unsigned next = 0;
        bool cache_valid = true;
        for (unsigned d = 0; d <= cached_depth; d++) {
            if (!vm_try_env_frame(e, &frame, &next)) {
                cache_valid = false;
                break;
            }
            if (d < cached_depth)
                e = next;
        }
        if (cache_valid) {
            unsigned vars = car(frame);
            unsigned vals = cdr(frame);
            for (unsigned o = 0; o < cached_offset && vars; o++) {
                if (!IS_PAIR(vars) || !IS_PAIR(vals)) {
                    cache_valid = false;
                    break;
                }
                vars = cdr(vars);
                vals = cdr(vals);
            }
            if (cache_valid && vars) {
                unsigned value = 0;
                unsigned found_offset = 0;
                if (vm_try_lookup_frame(sym_id, vars, vals, &value, NULL,
                                        &found_offset) &&
                    found_offset == 0) {
                    return value; // Verified hit
                }
            }
        }
        // Cache stale — fall through to full lookup
    }

    // Full lookup and fill cache
    unsigned depth = 0;
    for (unsigned e = env; e; depth++) {
        unsigned frame = 0;
        unsigned next = 0;
        if (!vm_try_env_frame(e, &frame, &next))
            return TOK_ERROR;
        unsigned vars = car(frame);
        unsigned vals = cdr(frame);
        unsigned value = 0;
        unsigned val_cell = 0;
        unsigned offset = 0;
        if (vm_try_lookup_frame(sym_id, vars, vals, &value, &val_cell,
                                &offset)) {
            cache_slot[0] = depth;
            cache_slot[1] = offset;
            return value;
        }
        e = next;
    }
    return TOK_ERROR;
}

// Global VM state for GC integration
// When the VM is running, this points to the active VM state
// so that GC can update VM roots
static vm_state *active_vm = NULL;

vm_state *get_active_vm(void)
{
    return active_vm;
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
    unsigned letrec_count =
        (vm->letrec_frame && vm->letrec_count > 0) ? vm->letrec_count : 0;
    size_t stack_size, frames_size, letrec_size, total_size;
    LISP_ASSERT_MSG(checked_array_size(vm->sp, sizeof(unsigned), &stack_size),
                    "capture_continuation: stack size overflow");
    LISP_ASSERT_MSG(checked_array_size(vm->fp, sizeof(vm_frame), &frames_size),
                    "capture_continuation: frame size overflow");
    LISP_ASSERT_MSG(checked_array_size(letrec_count, sizeof(unsigned),
                                       &letrec_size),
                    "capture_continuation: letrec size overflow");
    LISP_ASSERT_MSG(checked_add_size(sizeof(vm_continuation), stack_size,
                                     &total_size),
                    "capture_continuation: size overflow");
    LISP_ASSERT_MSG(checked_add_size(total_size, frames_size, &total_size),
                    "capture_continuation: size overflow");
    LISP_ASSERT_MSG(checked_add_size(total_size, letrec_size, &total_size),
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
    cont->frames = (vm_frame *)(block + sizeof(vm_continuation) + stack_size);

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

    // Save letrec values if we're in letrec initialization
    if (letrec_count > 0) {
        cont->letrec_saved =
            (unsigned *)(block + sizeof(vm_continuation) + stack_size +
                         frames_size);
        cont->letrec_saved_len = letrec_count;
        cont->letrec_frame = vm->letrec_frame;

        // Get the values list and save current values
        unsigned frame = car(vm->letrec_frame);
        unsigned vals = cdr(frame);
        for (unsigned i = 0; i < letrec_count && vals; i++, vals = cdr(vals)) {
            cont->letrec_saved[i] = car(vals);
        }
    } else {
        cont->letrec_saved = NULL;
        cont->letrec_saved_len = 0;
        cont->letrec_frame = 0;
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
    if (cont->fp > 0 && !cont->frames) {
        VM_ERROR(vm, "restore_continuation: missing frames");
        return;
    }

    // Restore letrec values if this continuation has them saved
    // This must happen before restoring the environment
    if (cont->letrec_saved && cont->letrec_saved_len > 0 && cont->letrec_frame) {
        if (!IS_PAIR(cont->letrec_frame) || !IS_PAIR(car(cont->letrec_frame))) {
            VM_ERROR(vm, "restore_continuation: invalid letrec frame");
            return;
        }
        unsigned frame = car(cont->letrec_frame);
        unsigned vals = cdr(frame);

        // Restore the saved values to the frame
        for (unsigned i = 0; i < cont->letrec_saved_len && vals;
             i++, vals = cdr(vals)) {
            if (!IS_PAIR(vals)) {
                VM_ERROR(vm, "restore_continuation: invalid letrec values");
                return;
            }
            cell_set_car(vals, cont->letrec_saved[i]);
        }
    }

    // Restore stack (with return value on top)
    if (vm->stack_cap < cont->sp + 1) {
        unsigned new_cap = cont->sp + 1;
        if (!vm_resize_stack(vm, new_cap,
                             "restore_continuation: stack realloc failed"))
            return;
    }
    memcpy(vm->stack, cont->stack, cont->sp * sizeof(unsigned));
    vm->sp = cont->sp;
    vm->stack[vm->sp++] = value; // Push return value

    // Restore frames
    if (vm->frames_cap < cont->fp) {
        unsigned new_cap = cont->fp;
        if (!vm_resize_frames(vm, new_cap,
                              "restore_continuation: frames realloc failed"))
            return;
    }
    memcpy(vm->frames, cont->frames, cont->fp * sizeof(vm_frame));
    vm->fp = cont->fp;

    // Restore execution state
    vm->code = cont->code;
    vm->ip = cont->ip;
    vm->env = cont->env;
    vm->bp = cont->bp;
    vm->letrec_frame = cont->letrec_frame;
    vm->letrec_count = cont->letrec_saved_len;
}

// ============================================================================
// Function Application
// ============================================================================

// Forward declaration for vm_handle_apply
static void vm_apply(vm_state *vm, unsigned fn, unsigned argc, bool tail);

static bool vm_require_proper_list(vm_state *vm, unsigned list,
                                   const char *msg)
{
    if (!list_length_checked(list, NULL, "apply")) {
        VM_ERROR(vm, msg);
        return false;
    }
    return true;
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
    if (!vm_require_proper_list(vm, last_list, "apply: expected proper list"))
        return false;
    unsigned last_count = 0;
    if (!list_length_checked(last_list, &last_count, "apply")) {
        VM_ERROR(vm, "apply: argument count overflow");
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

        // GC_GUARD for automatic cleanup of any gc_protect calls
        GC_GUARD;

        // Handle special primitives
        if (prim_id == PCALLCC) {
            // call/cc: pass continuation to procedure
            if (argc != 1) {
                vm->sp -= argc;
                VM_ERROR(vm, "call/cc: expected 1 argument");
                return;
            }
            unsigned proc = argv[0];
            vm->sp -= argc;
            gc_protect(&proc);
            unsigned cont = capture_continuation(vm);
            // Push continuation as argument and call proc
            vm_push(vm, cont);
            vm_apply(vm, proc, 1, tail);
            return;
        }

        if (prim_id == PAPPLY) {
            vm_handle_apply(vm, argc, argv, tail);
            return;
        }

        // Regular primitive - pass stack values directly as argv
        // Note: We keep argv on the stack during the call for GC protection,
        // then pop and push result in one operation (replacing args with result)
        unsigned result = apply_primitive_argv(prim_id, argc, argv);
        vm->sp -= argc; // Pop args after primitive returns
        if (result == TOK_ERROR) {
            VM_ERROR(vm, ctx.last_error[0] ? ctx.last_error
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

unsigned vm_run(vm_state *vm, code_object *code, unsigned env)
{
    LISP_ASSERT_MSG(vm != NULL, "vm_run: null vm");
    LISP_ASSERT_MSG(code != NULL, "vm_run: null code object");
    LISP_ASSERT_MSG(code->code != NULL, "vm_run: null bytecode array");
    LISP_ASSERT_FMT(code->code_len > 0, "vm_run: empty bytecode (len=%u)",
                    code->code_len);
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

    // Register VM with GC system
    vm_state *saved_active_vm = active_vm;
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
            unsigned *cache_slot = &vm->code->code[vm->ip + 1];
            vm->ip += 3;
            unsigned val = ic_lookup(sym_id, vm->env, cache_slot);
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
            VM_ERROR(vm, ctx.last_error);
            break;
        }

        switch (op) {
        case OP_CONST: {
            // Fallback (shouldn't reach here normally)
            unsigned idx = vm->code->code[vm->ip++];
            unsigned const_val;
            if (!vm_get_const(vm->code, idx, &const_val)) {
                VM_ERROR(vm, "constant index out of bounds");
                break;
            }
            unsigned val = vm_unbox_fixnum_if_possible(const_val);
            vm_push(vm, val);
            break;
        }

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

        case OP_LOOKUP: {
            // Fallback (shouldn't reach here normally - fast path above)
            int64_t sym_id = vm->code->code[vm->ip];
            unsigned *cache_slot = &vm->code->code[vm->ip + 1];
            vm->ip += 3;
            unsigned val = ic_lookup(sym_id, vm->env, cache_slot);
            if (val == TOK_ERROR) {
                vm_show_undefined_variable(sym_id);
                VM_ERROR(vm, ctx.last_error[0] ? ctx.last_error
                                               : "undefined variable");
                break;
            }
            vm_push(vm, vm_unbox_fixnum_if_possible(val));
            break;
        }

        case OP_DEFINE: {
            int64_t sym_id = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
            val = ensure_boxed(val);
            // Protect val before alloc() which can trigger GC
            gc_protect(&val);
            // Create atom for variable name
            unsigned atom = alloc();
            CELL_TYPE(atom) = BT_ATOM;
            CELL_ID(atom) = sym_id;
            gc_protect(&atom);
            defvar(atom, val, vm->env);
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
            defvar_alias(alias, target, target_cell, vm->env);
            break;
        }

        case OP_SET: {
            int64_t sym_id = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
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
            val = ensure_boxed(val);
            unsigned result = setvar(sym_id, val, vm->env);
            if (result == TOK_ERROR) {
                VM_ERROR(vm, "unbound variable in set!");
            }
            break;
        }

        case OP_RETURN_LOCALS: {
            vm->ip++; // skip operand
            unsigned val = vm_pop(vm);
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

            // Create closure cell: car = code_cell, cdr = env
            // Protect code_cell from GC during alloc_cons
            gc_protect(&code_cell);
            unsigned closure = alloc_cons(code_cell, vm->env);
            gc_unprotect(1);

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
            unsigned argc = vm->code->code[vm->ip++];

            // Ensure all args are boxed cell indices for primitives
            if (!vm_box_stack_args(vm, argc))
                break;

            // Get pointer to arguments on stack (no list building needed)
            unsigned *argv = &vm->stack[vm->sp - argc];

            // Handle special primitives that need full apply treatment
            if (prim_id == PAPPLY) {
                vm_handle_apply(vm, argc, argv, false);
                break;
            }

            // Special handling for load - uses callback to main.c
            if (prim_id == PLOAD) {
                if (argc != 1) {
                    vm->sp -= argc;
                    VM_ERROR_BREAK(vm, "load: expected 1 argument");
                }
                unsigned filename_cell = argv[0];
                vm->sp -= argc;
                if (!IS_STRING(filename_cell)) {
                    VM_ERROR_BREAK(vm, "load: expected string argument");
                }
                if (!ctx.load_callback) {
                    VM_ERROR_BREAK(vm, "load: callback not set");
                }
                char *filename_arg = require_string_ptr(filename_cell, "load");
                if (!filename_arg) {
                    VM_ERROR_BREAK(vm, ctx.last_error);
                }
                char *filename = checked_string_copy(filename_arg);
                if (!filename) {
                    VM_ERROR_BREAK(vm, "load: out of memory");
                }
                unsigned result = ctx.load_callback(filename, &vm->env);
                free(filename);
                if (result == TOK_ERROR) {
                    VM_ERROR_BREAK(vm, "load failed");
                }
                vm_push(vm, result);
                break;
            }

            // Special handling for call-with-values
            if (prim_id == PCALLWITHVALUES) {
                if (argc != 2) {
                    vm->sp -= argc;
                    VM_ERROR_BREAK(vm,
                                   "call-with-values: expected 2 arguments");
                }
                unsigned producer = argv[0];
                unsigned consumer = argv[1];
                vm->sp -= argc;
                unsigned result;

                gc_protect(&consumer);
                gc_protect(&producer);

                // Call producer with 0 args - handle different closure types
                if (is_bytecode_closure_object(producer)) {
                    // Bytecode closure - use vm_call_closure
                    result = vm_call_closure(producer, 0);
                } else if (IS_FUNCTION(producer)) {
                    // CPS closure - use CPS evaluator via callback
                    // Build: (producer)
                    unsigned call_expr = alloc_cons(producer, 0);
                    gc_protect(&call_expr);
                    if (ctx.eval_callback) {
                        result = ctx.eval_callback(call_expr, vm->env);
                    } else {
                        gc_unprotect(3);
                        VM_ERROR(vm,
                                 "call-with-values: cannot call CPS producer");
                        break;
                    }
                    gc_unprotect(1);
                } else if (IS_BUILTIN(producer)) {
                    // Builtin - call directly with no args
                    result = apply_primitive_argv(CELL_ID(producer), 0, NULL);
                } else {
                    gc_unprotect(2);
                    VM_ERROR(vm,
                             "call-with-values: producer is not a procedure");
                    break;
                }

                gc_unprotect(1); // producer

                if (result == TOK_ERROR) {
                    gc_unprotect(1); // consumer
                    VM_ERROR_BREAK(vm, "call-with-values: producer failed");
                }

                // If result is multival, unpack values as args to consumer
                if (IS_MULTIVAL(result)) {
                    unsigned vals = car(result);
                    unsigned consumer_argc = 0;
                    FORLIST(v, vals)
                    {
                        if (!vm_push_arg(vm, car(v), &consumer_argc))
                            break;
                    }
                    if (vm->error)
                        break;
                    vm_apply(vm, consumer, consumer_argc, false);
                } else {
                    // Single value - pass as single argument
                    vm_push(vm, result);
                    vm_apply(vm, consumer, 1, false);
                }
                gc_unprotect(1); // consumer
                break;
            }

            // Special handling for interaction-environment
            if (prim_id == PINTERACTIONENV) {
                vm->sp -= argc;
                if (argc != 0) {
                    VM_ERROR_BREAK(
                        vm, "interaction-environment: expected 0 arguments");
                }
                vm_push(vm, vm->env); // Return current environment
                break;
            }

            // Special handling for eval - uses callback to main.c
            if (prim_id == PEVAL) {
                if (argc < 1 || argc > 2) {
                    vm->sp -= argc;
                    VM_ERROR_BREAK(vm, "eval: expected 1 or 2 arguments");
                }
                unsigned expr = argv[0];
                unsigned eval_env = (argc == 2) ? argv[1] : vm->env;
                vm->sp -= argc;
                GC_GUARD;
                gc_protect(&expr);
                gc_protect(&eval_env);
                if (!ctx.eval_callback) {
                    VM_ERROR_BREAK(vm, "eval: callback not set");
                }
                unsigned result = ctx.eval_callback(expr, eval_env);
                if (result == TOK_ERROR) {
                    VM_ERROR_BREAK(vm, "eval failed");
                }
                vm_push(vm, result);
                break;
            }

            // Regular primitive - pass stack values directly as argv
            // Note: We keep argv on the stack during the call for GC protection,
            // then pop and push result in one operation (replacing args with result)
            unsigned result = apply_primitive_argv(prim_id, argc, argv);
            vm->sp -= argc; // Pop args after primitive returns
            if (result == TOK_ERROR) {
                VM_ERROR(vm, ctx.last_error[0] ? ctx.last_error
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

        case OP_APPLY: {
            // Stack: [proc, args-list]
            unsigned args_list = vm_pop(vm);
            unsigned proc = vm_pop(vm);
            if (!vm_require_proper_list(vm, args_list,
                                        "apply: expected proper list"))
                break;

            // Push args from list
            unsigned argc = 0;
            FORLIST(a, args_list)
            {
                if (!vm_push_arg(vm, car(a), &argc))
                    break;
            }
            if (vm->error)
                break;

            vm_apply(vm, proc, argc, false);
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
            if (vm->env) {
                vm->env = cdr(vm->env);
            }
            // Clear letrec tracking if we're popping the letrec frame
            if (vm->letrec_frame && vm->env != vm->letrec_frame) {
                vm->letrec_frame = 0;
                vm->letrec_count = 0;
            }
            break;
        }

        case OP_LETREC_MARK: {
            // Mark the current environment frame as being letrec-initialized
            unsigned count = vm->code->code[vm->ip++];
            vm->letrec_frame = vm->env;
            vm->letrec_count = count;
            break;
        }

        case OP_LETREC_DONE: {
            // End letrec initialization - clear the tracking
            vm->letrec_frame = 0;
            vm->letrec_count = 0;
            break;
        }

        case OP_VALUES: {
            unsigned n = vm->code->code[vm->ip++];
            // Box fixnums before building value list
            if (!vm_box_stack_args(vm, n))
                break;
            unsigned value = values_from_argv(n, &vm->stack[vm->sp - n]);
            vm->sp -= n;
            vm_push(vm, value);
            break;
        }

        case OP_CALLWITHVALUES: {
            // Stack: [producer, consumer]
            unsigned consumer = vm_pop(vm);
            unsigned producer = vm_pop(vm);
            GC_GUARD;
            gc_protect(&consumer);
            gc_protect(&producer);

            // Call producer with no args
            vm_apply(vm, producer, 0, false);

            // After producer returns, get result
            // If result is multival, unpack it; otherwise use as single arg
            unsigned result = vm_pop(vm);
            gc_protect(&result);
            if (IS_MULTIVAL(result)) {
                unsigned vals = car(result);
                unsigned argc = 0;
                FORLIST(v, vals)
                {
                    if (!vm_push_arg(vm, car(v), &argc))
                        break;
                }
                if (vm->error)
                    break;
                vm_apply(vm, consumer, argc, false);
            } else {
                vm_push(vm, result);
                vm_apply(vm, consumer, 1, false);
            }
            break;
        }

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
            defvar(atom, transformer, vm->env);
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
                if (val < FIXNUM_MAX) {
                    vm_push(vm, MAKE_FIXNUM(val + 1));
                } else {
                    vm_push(vm, store((int64_t)val + 1));
                }
                break;
            }
            if (IS_NUM(n)) {
                int64_t val = CELL_ID(n);
                if (val < INT64_MAX) {
                    vm_push(vm, store(val + 1));
                } else {
                    // Overflow to bignum
                    bignum *bn = bn_from_int(val);
                    unsigned result = bn ? vm_store_bignum_add_one(vm, bn)
                                         : TOK_ERROR;
                    if (result == TOK_ERROR) {
                        VM_ERROR_BREAK(vm, "add1: out of memory");
                    }
                    vm_push(vm, result);
                }
            } else if (IS_BIGNUM(n)) {
                bignum *bn = bn_copy(get_bignum(n));
                unsigned result = bn ? vm_store_bignum_add_one(vm, bn)
                                     : TOK_ERROR;
                if (result == TOK_ERROR) {
                    VM_ERROR_BREAK(vm, get_bignum(n) ? "add1: out of memory"
                                                     : "add1: not a number");
                }
                vm_push(vm, result);
            } else {
                unsigned result = vm_apply_with_one(n, prim_plus);
                if (result == TOK_ERROR) {
                    VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                         : "add1 failed");
                }
                vm_push(vm, result);
            }
            break;
        }

        case OP_SUB1: {
            unsigned n = vm_pop(vm);
            if (IS_FIXNUM(n)) {
                int32_t val = FIXNUM_VALUE(n);
                if (val > FIXNUM_MIN) {
                    vm_push(vm, MAKE_FIXNUM(val - 1));
                } else {
                    vm_push(vm, store((int64_t)val - 1));
                }
                break;
            }
            if (IS_NUM(n)) {
                int64_t val = CELL_ID(n);
                if (val > INT64_MIN) {
                    vm_push(vm, store(val - 1));
                } else {
                    // Underflow to bignum
                    bignum *bn = bn_from_int(val);
                    unsigned result = bn ? vm_store_bignum_sub_one(vm, bn)
                                         : TOK_ERROR;
                    if (result == TOK_ERROR) {
                        VM_ERROR_BREAK(vm, "sub1: out of memory");
                    }
                    vm_push(vm, result);
                }
            } else if (IS_BIGNUM(n)) {
                bignum *bn = bn_copy(get_bignum(n));
                unsigned result = bn ? vm_store_bignum_sub_one(vm, bn)
                                     : TOK_ERROR;
                if (result == TOK_ERROR) {
                    VM_ERROR_BREAK(vm, get_bignum(n) ? "sub1: out of memory"
                                                     : "sub1: not a number");
                }
                vm_push(vm, result);
            } else {
                unsigned result = vm_apply_with_one(n, prim_minus);
                if (result == TOK_ERROR) {
                    VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                         : "sub1 failed");
                }
                vm_push(vm, result);
            }
            break;
        }

        case OP_ZEROP: {
            unsigned n = vm_pop(vm);
            if (IS_FIXNUM(n)) {
                vm_push(vm, FIXNUM_VALUE(n) == 0 ? ctx.atom_true : ctx.atom_false);
                break;
            }
            n = ensure_boxed(n);
            gc_protect(&n);
            unsigned zero = store(0);
            gc_protect(&zero);
            unsigned result = binary_numeq(n, zero);
            gc_unprotect(2);
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

        case OP_JUMPIFZERO: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned n = vm_pop(vm);
            bool is_zero = false;
            if (IS_FIXNUM(n)) {
                is_zero = FIXNUM_VALUE(n) == 0;
            } else {
                n = ensure_boxed(n);
                gc_protect(&n);
                unsigned zero = store(0);
                gc_protect(&zero);
                unsigned result = binary_numeq(n, zero);
                gc_unprotect(2);
                if (result == TOK_ERROR)
                    VM_ERROR_BREAK(vm, "=: invalid operands");
                is_zero = (result == ctx.atom_true);
            }
            if (is_zero) {
                vm->ip = target;
            }
            break;
        }

        case OP_JUMPIFNOTZERO: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned n = vm_pop(vm);
            bool is_zero = false;
            if (IS_FIXNUM(n)) {
                is_zero = FIXNUM_VALUE(n) == 0;
            } else {
                n = ensure_boxed(n);
                gc_protect(&n);
                unsigned zero = store(0);
                gc_protect(&zero);
                unsigned result = binary_numeq(n, zero);
                gc_unprotect(2);
                if (result == TOK_ERROR)
                    VM_ERROR_BREAK(vm, "=: invalid operands");
                is_zero = (result == ctx.atom_true);
            }
            if (!is_zero) {
                vm->ip = target;
            }
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

        case OP_CADDR: {
            unsigned pair = vm_pop(vm);
            VM_CHECK_PAIR(vm, pair, "caddr: not a pair");
            unsigned d = cdr(pair);
            VM_CHECK_PAIR(vm, d, "caddr: cdr not a pair");
            unsigned dd = cdr(d);
            VM_CHECK_PAIR(vm, dd, "caddr: cddr not a pair");
            vm_push(vm, car(dd));
            break;
        }

        case OP_CDDDR: {
            unsigned pair = vm_pop(vm);
            VM_CHECK_PAIR(vm, pair, "cdddr: not a pair");
            unsigned d = cdr(pair);
            VM_CHECK_PAIR(vm, d, "cdddr: cdr not a pair");
            unsigned dd = cdr(d);
            VM_CHECK_PAIR(vm, dd, "cdddr: cddr not a pair");
            vm_push(vm, cdr(dd));
            break;
        }

        // Type predicates
        case OP_SYMBOLP: {
            unsigned val = vm_pop(vm);
            vm_push(vm, (!IS_FIXNUM(val) && IS_ATOM(val)) ? ctx.atom_true
                                                          : ctx.atom_false);
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

        // List operations
        case OP_LENGTH: {
            unsigned val = vm_pop(vm);
            if (IS_FIXNUM(val)) {
                VM_ERROR_BREAK(vm, "length: not a proper list");
            }
            if (IS_STRING(val)) {
                char *s = require_string_ptr(val, "length");
                if (!s) {
                    VM_ERROR_BREAK(vm, ctx.last_error);
                }
                size_t len;
                const char *error_msg = NULL;
                if (!scheme_utf8_count_chars(s, &len, &error_msg)) {
                    if (error_msg)
                        snprintf(ctx.last_error, sizeof(ctx.last_error),
                                 "length: %s", error_msg);
                    VM_ERROR_BREAK(vm, error_msg ? ctx.last_error
                                                 : "length: invalid string");
                }
                vm_push(vm, store(len));
                break;
            }
            if (IS_VECTOR(val)) {
                vm_push(vm, store(vector_len(val)));
                break;
            }
            unsigned len = 0;
            if (!list_length_checked(val, &len, "length"))
                VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                     : "length: invalid list");
            vm_push(vm, store(len));
            break;
        }

        case OP_APPEND: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (a == 0) {
                vm_push(vm, b);
                break;
            }
            if (!list_length_checked(a, NULL, "append"))
                VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                     : "append: invalid list");
            // Build result by copying a, then appending b
            protect_and_box2(&a, &b);
            unsigned result = 0;
            unsigned tail = 0;
            gc_protect(&result);
            gc_protect(&tail);
            unsigned p = a;
            gc_protect(&p);
            for (; p != 0; p = cdr(p)) {
                if (IS_FIXNUM(p) || !IS_PAIR(p)) {
                    gc_unprotect(5);
                    VM_ERROR(vm, "append: not a proper list");
                    break;
                }
                unsigned new_cell = alloc_cons(car(p), 0);
                if (result == 0) {
                    result = new_cell;
                    tail = new_cell;
                } else {
                    cell_set_cdr(tail, new_cell);
                    tail = new_cell;
                }
            }
            if (vm->running) {
                if (tail != 0) {
                    cell_set_cdr(tail, b);
                }
                gc_unprotect(5); // p, tail, result, a, b
                vm_push(vm, result == 0 ? b : result);
            }
            break;
        }

        case OP_REVERSE: {
            unsigned list = vm_pop(vm);
            if (!list_length_checked(list, NULL, "reverse"))
                VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                     : "reverse: invalid list");
            gc_protect(&list);
            unsigned result = 0;
            gc_protect(&result);
            unsigned p = list;
            gc_protect(&p);
            for (; p != 0; p = cdr(p)) {
                if (IS_FIXNUM(p) || !IS_PAIR(p)) {
                    gc_unprotect(3);
                    VM_ERROR(vm, "reverse: not a proper list");
                    break;
                }
                result = alloc_cons(car(p), result);
            }
            if (vm->running) {
                gc_unprotect(3);
                vm_push(vm, result);
            }
            break;
        }

        case OP_MEMQ: {
            unsigned list = vm_pop(vm);
            unsigned obj = vm_pop(vm);
            // Box obj for comparison (list elements are always boxed)
            gc_protect(&list);
            obj = ensure_boxed(obj);
            gc_unprotect(1);
            unsigned result = ctx.atom_false;
            for (unsigned p = list; p != 0; p = cdr(p)) {
                if (IS_FIXNUM(p) || !IS_PAIR(p)) {
                    break;
                }
                unsigned elem = car(p);
                if (elem == obj ||
                    (IS_CELL(elem) && IS_CELL(obj) &&
                     CELL_TYPE(elem) == CELL_TYPE(obj) &&
                     (CELL_TYPE(elem) == BT_ATOM ||
                      CELL_TYPE(elem) == BT_NUM ||
                      CELL_TYPE(elem) == BT_CHAR) &&
                     CELL_ID(elem) == CELL_ID(obj))) {
                    result = p;
                    break;
                }
            }
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
        case OP_NEG: {
            unsigned n = vm_pop(vm);
            if (IS_FIXNUM(n)) {
                int32_t val = FIXNUM_VALUE(n);
                if (val != FIXNUM_MIN) {
                    vm_push(vm, MAKE_FIXNUM(-val));
                } else {
                    vm_push(vm, store(-(int64_t)val));
                }
                break;
            }
            if (IS_NUM(n)) {
                int64_t val = CELL_ID(n);
                if (val == INT64_MIN) {
                    // Overflow to bignum
                    bignum *bn = bn_from_int(val);
                    if (!bn) {
                        VM_ERROR_BREAK(vm, "-: out of memory");
                    }
                    bn_neg_ip(bn);
                    vm_push(vm, store_integer(bn));
                } else {
                    vm_push(vm, store(-val));
                }
            } else if (IS_BIGNUM(n)) {
                bignum *bn = get_bignum(n);
                if (!bn) {
                    VM_ERROR_BREAK(vm, "-: not a number");
                }
                unsigned result = vm_store_bignum_neg(vm, bn, "-: out of memory");
                if (result == TOK_ERROR) {
                    VM_ERROR_BREAK(vm, "-: out of memory");
                }
                vm_push(vm, result);
            } else if (IS_INEXACT(n)) {
                vm_push(vm, store_inexact(-to_double(n)));
            } else if (IS_RATIONAL(n)) {
                GC_GUARD;
                gc_protect(&n);
                unsigned neg_num = negate_number(car(n));
                if (neg_num == TOK_ERROR) {
                    VM_ERROR_BREAK(vm, "-: out of memory");
                }
                gc_protect(&neg_num);
                unsigned denom = cdr(n);
                gc_protect(&denom);
                unsigned result = alloc();
                CELL_TYPE(result) = BT_RATIONAL;
                CELL_CAR(result) = neg_num;
                CELL_CDR(result) = denom;
                vm_push(vm, result);
            } else {
                VM_ERROR(vm, "-: not a number");
            }
            break;
        }

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
                double d = to_double(n);
                vm_push(vm, store_inexact(d < 0 ? -d : d));
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

        case OP_POSITIVE: {
            unsigned n = vm_pop(vm);
            if (IS_FIXNUM(n)) {
                vm_push(vm, FIXNUM_VALUE(n) > 0 ? ctx.atom_true : ctx.atom_false);
                break;
            }
            bool positive = false;
            if (IS_NUM(n)) {
                positive = CELL_ID(n) > 0;
            } else if (IS_BIGNUM(n)) {
                bignum *bn = get_bignum(n);
                if (!bn) {
                    VM_ERROR_BREAK(vm, "positive?: not a real number");
                }
                positive = !bn->sign && bn->len > 0;
            } else if (IS_INEXACT(n)) {
                positive = to_double(n) > 0;
            } else if (IS_RATIONAL(n)) {
                positive = !is_negative_number(car(n)) &&
                           !(IS_NUM(car(n)) && CELL_ID(car(n)) == 0);
            }
            vm_push(vm, positive ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_NEGATIVE: {
            unsigned n = vm_pop(vm);
            if (IS_FIXNUM(n)) {
                vm_push(vm, FIXNUM_VALUE(n) < 0 ? ctx.atom_true : ctx.atom_false);
                break;
            }
            bool negative = false;
            if (IS_NUM(n)) {
                negative = CELL_ID(n) < 0;
            } else if (IS_BIGNUM(n)) {
                bignum *bn = get_bignum(n);
                if (!bn) {
                    VM_ERROR_BREAK(vm, "negative?: not a real number");
                }
                negative = bn_sign(bn) < 0;
            } else if (IS_INEXACT(n)) {
                negative = to_double(n) < 0;
            } else if (IS_RATIONAL(n)) {
                negative = is_negative_number(car(n));
            }
            vm_push(vm, negative ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_EVEN: {
            unsigned n = vm_pop(vm);
            bool odd;
            if (!vm_integer_is_odd(vm, n, "even?: not an integer", &odd))
                goto vm_dispatch_error;
            vm_push(vm, odd ? ctx.atom_false : ctx.atom_true);
            break;
        }

        case OP_ODD: {
            unsigned n = vm_pop(vm);
            bool odd;
            if (!vm_integer_is_odd(vm, n, "odd?: not an integer", &odd))
                goto vm_dispatch_error;
            vm_push(vm, odd ? ctx.atom_true : ctx.atom_false);
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
        case OP_LOOKUP_ADD1: {
            int64_t sym_id = vm->code->code[vm->ip];
            unsigned *cache_slot = &vm->code->code[vm->ip + 1];
            vm->ip += 3;
            unsigned n = ic_lookup(sym_id, vm->env, cache_slot);
            if (n == TOK_ERROR) {
                vm_show_undefined_variable(sym_id);
                VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                     : "undefined variable");
            }
            if (IS_FIXNUM(n)) {
                int32_t val = FIXNUM_VALUE(n);
                vm_push(vm, val < FIXNUM_MAX ? MAKE_FIXNUM(val + 1)
                                             : store((int64_t)val + 1));
            } else if (IS_NUM(n)) {
                int64_t val = CELL_ID(n);
                if (val < INT64_MAX) {
                    int64_t r = val + 1;
                    vm_push(vm, vm_store_integer_result(r));
                } else {
                    bignum *bn = bn_from_int(val);
                    unsigned result = bn ? vm_store_bignum_add_one(vm, bn)
                                         : TOK_ERROR;
                    if (result == TOK_ERROR) {
                        VM_ERROR_BREAK(vm, "add1: out of memory");
                    }
                    vm_push(vm, result);
                }
            } else {
                vm_push(vm, vm_apply_with_one(n, prim_plus));
            }
            break;
        }

        case OP_LOOKUP_SUB1: {
            int64_t sym_id = vm->code->code[vm->ip];
            unsigned *cache_slot = &vm->code->code[vm->ip + 1];
            vm->ip += 3;
            unsigned n = ic_lookup(sym_id, vm->env, cache_slot);
            if (n == TOK_ERROR) {
                vm_show_undefined_variable(sym_id);
                VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                     : "undefined variable");
            }
            if (IS_FIXNUM(n)) {
                int32_t val = FIXNUM_VALUE(n);
                vm_push(vm, val > FIXNUM_MIN ? MAKE_FIXNUM(val - 1)
                                             : store((int64_t)val - 1));
            } else if (IS_NUM(n)) {
                int64_t val = CELL_ID(n);
                if (val > INT64_MIN) {
                    int64_t r = val - 1;
                    vm_push(vm, vm_store_integer_result(r));
                } else {
                    bignum *bn = bn_from_int(val);
                    unsigned result = bn ? vm_store_bignum_sub_one(vm, bn)
                                         : TOK_ERROR;
                    if (result == TOK_ERROR) {
                        VM_ERROR_BREAK(vm, "sub1: out of memory");
                    }
                    vm_push(vm, result);
                }
            } else {
                vm_push(vm, vm_apply_with_one(n, prim_minus));
            }
            break;
        }

        default:
            VM_ERROR(vm, "unknown opcode");
            break;
        }
vm_dispatch_error:
        ;
    }

    // Unregister VM from GC system
    active_vm = saved_active_vm;

    if (vm->error) {
        show_error("VM error: %s", vm->error_msg);
        return TOK_ERROR;
    }

    if (vm->sp > 0) {
        unsigned result = vm->stack[vm->sp - 1];
        return IS_FIXNUM(result) ? ensure_boxed(result) : result;
    }
    return 0;
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
    vm->letrec_frame = collect(vm->letrec_frame);

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
    vm->letrec_frame = collector(vm->letrec_frame);
}
