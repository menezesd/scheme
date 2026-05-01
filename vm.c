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
#include "context.h"
#include "env.h"
#include "eval.h"
#include "macros.h"
#include "prim_internal.h"
#include "primitives.h"
#include "writer.h"
#include <stdlib.h>
#include <string.h>

// ============================================================================
// VM Constants
// ============================================================================

#define IC_UNCACHED 0xFFFFFFFF

#define INITIAL_STACK_SIZE 1024
#define INITIAL_FRAMES_SIZE 256
#define MAX_STACK_SIZE (1024 * 1024)
#define MAX_FRAMES_SIZE (64 * 1024)

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
        break;                                                                 \
    } while (0)

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
static inline unsigned ic_lookup(int64_t sym_id, unsigned env,
                                 unsigned *cache_slot)
{
    unsigned cached_depth = cache_slot[0];
    unsigned cached_offset = cache_slot[1];

    if (cached_depth != IC_UNCACHED) {
        // Try cached path: walk depth frames, offset values, verify sym_id
        unsigned e = env;
        for (unsigned d = 0; d < cached_depth && e; d++)
            e = cdr(e);
        if (e) {
            unsigned frame = car(e);
            unsigned vars = car(frame);
            unsigned vals = cdr(frame);
            for (unsigned o = 0; o < cached_offset && vars; o++) {
                vars = cdr(vars);
                vals = cdr(vals);
            }
            if (vars && vals) {
                int64_t var_id = (CELL_TYPE(vars) == BT_ATOM)
                                     ? CELL_ID(vars)
                                     : CELL_ID(car(vars));
                if (var_id == sym_id)
                    return car(vals); // Verified hit
            }
        }
        // Cache stale — fall through to full lookup
    }

    // Full lookup and fill cache
    unsigned depth = 0;
    for (unsigned e = env; e; e = cdr(e), depth++) {
        unsigned frame = car(e);
        unsigned vars = car(frame);
        unsigned vals = cdr(frame);
        unsigned offset = 0;
        for (; vars; vars = cdr(vars), vals = cdr(vals), offset++) {
            bool match = false;
            if (CELL_TYPE(vars) == BT_ATOM) {
                match = (CELL_ID(vars) == sym_id);
                if (!match) break;
            } else {
                match = (CELL_ID(car(vars)) == sym_id);
            }
            if (match) {
                cache_slot[0] = depth;
                cache_slot[1] = offset;
                return car(vals);
            }
        }
    }
    return TOK_ERROR;
}

// Global VM state for GC integration
// When the VM is running, this points to the active VM state
// so that GC can update VM roots
static vm_state *active_vm = NULL;

vm_state *get_active_vm(void) { return active_vm; }

// ============================================================================
// VM Initialization
// ============================================================================

void vm_init(vm_state *vm)
{
    memset(vm, 0, sizeof(vm_state));

    vm->stack_cap = INITIAL_STACK_SIZE;
    vm->stack = malloc(vm->stack_cap * sizeof(unsigned));
    if (!vm->stack) {
        lisp_panic("vm_init: failed to allocate stack");
    }

    vm->frames_cap = INITIAL_FRAMES_SIZE;
    vm->frames = malloc(vm->frames_cap * sizeof(vm_frame));
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
    code_object *code = GET_CLOSURE_CODE(closure);
    unsigned closure_env = GET_CLOSURE_ENV(closure);

    if (!code || !code->code) {
        show_error("invalid bytecode closure");
        return TOK_ERROR;
    }

    // Check arity
    unsigned argc = 0;
    for (unsigned a = args; a; a = cdr(a))
        argc++;

    if (!code->has_rest && argc != code->arity) {
        show_error("wrong number of arguments to closure");
        return TOK_ERROR;
    }
    if (code->has_rest && argc < code->arity) {
        show_error("too few arguments to closure");
        return TOK_ERROR;
    }

    // Bind parameters to arguments
    unsigned params = code->constants[code->params];
    unsigned frame, new_env;
    {
        GC_PROTECT_GUARD3(&closure_env, &args, &params);
        frame = bind_params(params, args);
    }
    {
        GC_PROTECT_GUARD2(&frame, &closure_env);
        new_env = alloc_cons(frame, closure_env);
    }

    // Create a temporary VM and run the code
    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, new_env);
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
        unsigned new_cap = vm->stack_cap * 2;
        unsigned *new_stack = realloc(vm->stack, new_cap * sizeof(unsigned));
        if (!new_stack) {
            VM_ERROR(vm, "stack reallocation failed");
            return;
        }
        vm->stack = new_stack;
        vm->stack_cap = new_cap;
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
                       unsigned bp, unsigned env)
{
    LISP_ASSERT_MSG(code != NULL, "push_frame: null code object");
    if (vm->fp >= vm->frames_cap) {
        if (vm->frames_cap >= MAX_FRAMES_SIZE) {
            VM_ERROR(vm, "call stack overflow");
            return;
        }
        unsigned new_cap = vm->frames_cap * 2;
        vm_frame *new_frames = realloc(vm->frames, new_cap * sizeof(vm_frame));
        if (!new_frames) {
            VM_ERROR(vm, "frame stack reallocation failed");
            return;
        }
        vm->frames = new_frames;
        vm->frames_cap = new_cap;
    }
    LISP_ASSERT_FMT(vm->fp < vm->frames_cap,
                    "push_frame: fp=%u >= frames_cap=%u", vm->fp, vm->frames_cap);
    vm->frames[vm->fp].code = code;
    vm->frames[vm->fp].ip = ip;
    vm->frames[vm->fp].bp = bp;
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
    size_t stack_size = vm->sp * sizeof(unsigned);
    size_t frames_size = vm->fp * sizeof(vm_frame);
    size_t letrec_size = letrec_count * sizeof(unsigned);
    size_t total_size =
        sizeof(vm_continuation) + stack_size + frames_size + letrec_size;

    // Single allocation for entire continuation
    char *block = malloc(total_size);
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

    // Wrap in a cell
    unsigned cell = alloc();
    CELL_TYPE(cell) = BT_VMCONT;
    CELL_PTR(cell) = cont;

    return cell;
}

static void restore_continuation(vm_state *vm, unsigned cont_cell,
                                 unsigned value)
{
    LISP_ASSERT_MSG(vm != NULL, "restore_continuation: null vm");
    LISP_ASSERT_TYPE(cont_cell, BT_VMCONT);

    vm_continuation *cont = (vm_continuation *)CELL_PTR(cont_cell);
    LISP_ASSERT_MSG(cont != NULL, "restore_continuation: null continuation");

    // Restore letrec values if this continuation has them saved
    // This must happen before restoring the environment
    if (cont->letrec_saved && cont->letrec_saved_len > 0 && cont->letrec_frame) {
        unsigned frame = car(cont->letrec_frame);
        unsigned vals = cdr(frame);

        // Restore the saved values to the frame
        for (unsigned i = 0; i < cont->letrec_saved_len && vals;
             i++, vals = cdr(vals)) {
            write_barrier(vals, cont->letrec_saved[i]);
            CELL_CAR(vals) = cont->letrec_saved[i];
        }
    }

    // Restore stack (with return value on top)
    if (vm->stack_cap < cont->sp + 1) {
        unsigned new_cap = cont->sp + 1;
        unsigned *new_stack = realloc(vm->stack, new_cap * sizeof(unsigned));
        if (!new_stack) {
            VM_ERROR(vm, "restore_continuation: stack realloc failed");
            return;
        }
        vm->stack = new_stack;
        vm->stack_cap = new_cap;
    }
    memcpy(vm->stack, cont->stack, cont->sp * sizeof(unsigned));
    vm->sp = cont->sp;
    vm->stack[vm->sp++] = value; // Push return value

    // Restore frames
    if (vm->frames_cap < cont->fp) {
        unsigned new_cap = cont->fp;
        vm_frame *new_frames = realloc(vm->frames, new_cap * sizeof(vm_frame));
        if (!new_frames) {
            VM_ERROR(vm, "restore_continuation: frames realloc failed");
            return;
        }
        vm->frames = new_frames;
        vm->frames_cap = new_cap;
    }
    memcpy(vm->frames, cont->frames, cont->fp * sizeof(vm_frame));
    vm->fp = cont->fp;

    // Restore execution state
    vm->code = cont->code;
    vm->ip = cont->ip;
    vm->env = cont->env;
}

// ============================================================================
// Function Application
// ============================================================================

// Forward declaration for vm_handle_apply
static void vm_apply(vm_state *vm, unsigned fn, unsigned argc, bool tail);

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

    // Copy middle args before popping (they'll be overwritten)
    unsigned middle_count = (argc > 2) ? argc - 2 : 0;

    // Use stack allocation for small counts, heap for large
    unsigned middle_args_stack[MAX_APPLY_MIDDLE_ARGS];
    unsigned *middle_args = middle_args_stack;
    unsigned *middle_args_heap = NULL;

    if (middle_count > MAX_APPLY_MIDDLE_ARGS) {
        middle_args_heap = malloc(middle_count * sizeof(unsigned));
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
        vm_push(vm, middle_args[i]);
        apply_argc++;
    }

    // Free heap allocation if used
    if (middle_args_heap) {
        free(middle_args_heap);
    }

    // Append the last list's elements
    FORLIST(a, last_list)
    {
        vm_push(vm, car(a));
        apply_argc++;
    }

    vm_apply(vm, proc, apply_argc, tail);
    return true;
}

// Box any fixnum values in the top N stack slots (for passing to primitives/closures)
static void vm_box_stack_args(vm_state *vm, unsigned argc)
{
    for (unsigned i = 0; i < argc; i++) {
        unsigned idx = vm->sp - argc + i;
        if (IS_FIXNUM(vm->stack[idx])) {
            vm->stack[idx] = store((int64_t)FIXNUM_VALUE(vm->stack[idx]));
        }
    }
}

static void vm_apply(vm_state *vm, unsigned fn, unsigned argc, bool tail)
{
    // Ensure all arguments are boxed cell indices before binding.
    // Protect fn across boxing since store() can trigger GC.
    gc_protect(&fn);
    vm_box_stack_args(vm, argc);
    gc_unprotect(1);
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
            vm->error = true;
            vm->error_msg =
                ctx.last_error[0] ? ctx.last_error : "primitive error";
            vm->running = false;
            return;
        }
        vm_push(vm, result);
        return;
    }

    // Check for VM closure: cons cell with BT_CLOSURE marker in car
    if (IS_PAIR(fn) && CELL_TYPE(car(fn)) == BT_CLOSURE) {
        code_object *code = GET_CLOSURE_CODE(fn);
        unsigned closure_env = GET_CLOSURE_ENV(fn);

        // Check code object validity
        if (!code || !code->code) {
            vm->error = true;
            vm->error_msg = "closure has invalid code object";
            vm->running = false;
            return;
        }

        // Check arity
        if (!code->has_rest && argc != code->arity) {
            vm->error = true;
            vm->error_msg = "wrong number of arguments";
            vm->running = false;
            return;
        }
        if (code->has_rest && argc < code->arity) {
            vm->error = true;
            vm->error_msg = "too few arguments";
            vm->running = false;
            return;
        }

        // For tail calls: if the CURRENT function uses locals, clean them up
        // by shifting args down to overwrite the old locals
        if (tail && vm->fp > 0 && vm->code->use_locals) {
            unsigned old_locals_count = vm->code->arity;
            // Args are at [sp-argc..sp-1], old locals at [bp..bp+old_count-1]
            // Shift args down to bp position
            for (unsigned i = 0; i < argc; i++)
                vm->stack[vm->bp + i] = vm->stack[vm->sp - argc + i];
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
                push_frame(vm, vm->code, vm->ip, new_bp, vm->env);
                vm->bp = new_bp;
                vm->code = code;
                vm->ip = 0;
                vm->env = closure_env;
            }
        } else {
            // Environment frame path (original): build frame from stack values
            gc_protect(&closure_env);
            unsigned params = code->constants[code->params];
            gc_protect(&params);
            unsigned new_env;

            if (!code->has_rest) {
                unsigned vals = 0;
                gc_protect(&vals);
                for (int i = (int)argc - 1; i >= 0; i--) {
                    vals = alloc_cons(vm->stack[vm->sp - argc + i], vals);
                }
                vm->sp -= argc;
                unsigned frame = alloc_cons(params, vals);
                gc_protect(&frame);
                new_env = alloc_cons(frame, closure_env);
                gc_unprotect(4);
            } else {
                unsigned args = 0;
                for (int i = (int)argc - 1; i >= 0; i--) {
                    args = alloc_cons(vm->stack[vm->sp - argc + i], args);
                }
                vm->sp -= argc;
                gc_protect(&args);
                unsigned frame = bind_params(params, args);
                gc_unprotect(1);
                gc_protect(&frame);
                new_env = alloc_cons(frame, closure_env);
                gc_unprotect(3);
            }

            if (tail && vm->fp > 0) {
                // If caller used locals, those are still on the stack
                // above the saved frame. They need to be cleaned up.
                // (The frame.bp points to before the caller's locals)
                vm->code = code;
                vm->ip = 0;
                vm->env = new_env;
            } else {
                push_frame(vm, vm->code, vm->ip, vm->sp, vm->env);
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
        for (int i = (int)argc - 1; i >= 0; i--) {
            args = alloc_cons(vm->stack[vm->sp - argc + i], args);
        }

        vm->sp -= argc;

        // Bind parameters
        gc_protect(&args);
        unsigned frame = bind_params(params, args);
        gc_unprotect(1); // args
        gc_protect(&frame);
        unsigned new_env = alloc_cons(frame, def_env);
        gc_unprotect(4); // frame, def_env, body, params

        // Evaluate body using interpreter
        // For now, fall back to eval_cps
        // This allows mixed mode operation
        unsigned result = 0;
        FORLIST(expr, body) { result = eval_cps(car(expr), new_env); }

        if (result == TOK_ERROR) {
            vm->error = true;
            vm->error_msg = "evaluation error";
            vm->running = false;
            return;
        }

        vm_push(vm, result);
        return;
    }

    if (CELL_TYPE(fn) == BT_VMCONT) {
        // VM continuation invocation
        if (argc != 1) {
            vm->error = true;
            vm->error_msg = "continuation: expected 1 argument";
            vm->running = false;
            return;
        }
        unsigned value = vm->stack[vm->sp - 1];
        vm->sp -= argc;
        restore_continuation(vm, fn, value);
        return;
    }

    show_error("not a procedure, got %s", type_name(fn));
    vm->error = true;
    vm->error_msg = ctx.last_error[0] ? ctx.last_error : "not a procedure";
    vm->running = false;
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

    vm->code = code;
    vm->ip = 0;
    vm->env = env;
    vm->sp = 0;
    vm->fp = 0;
    vm->running = true;
    vm->error = false;

    // Register VM with GC system
    vm_state *saved_active_vm = active_vm;
    active_vm = vm;

    while (vm->running) {
        if (!vm->code) {
            vm->error = true;
            vm->error_msg = "null code object";
            break;
        }
        if (!vm->code->code) {
            vm->error = true;
            vm->error_msg = "null code array";
            break;
        }
        if (vm->ip >= vm->code->code_len) {
            vm->error = true;
            vm->error_msg = "instruction pointer out of bounds";
            break;
        }

        unsigned op = vm->code->code[vm->ip++];

        // Fast paths for most common opcodes (skip switch dispatch overhead)
        if (op == OP_CONST) {
            unsigned idx = vm->code->code[vm->ip++];
            unsigned val = vm->code->constants[idx];
            // Eagerly unbox small integers to fixnum tags
            if (CELL_TYPE(val) == BT_NUM) {
                int64_t n = CELL_ID(val);
                if (FITS_FIXNUM(n))
                    val = MAKE_FIXNUM((int32_t)n);
            }
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
                // Eagerly unbox small integers to fixnum tags
                if (CELL_TYPE(val) == BT_NUM) {
                    int64_t n = CELL_ID(val);
                    if (FITS_FIXNUM(n))
                        val = MAKE_FIXNUM((int32_t)n);
                }
                if (__builtin_expect(vm->sp < vm->stack_cap, 1)) {
                    vm->stack[vm->sp++] = val;
                } else {
                    vm_push(vm, val);
                }
                continue;
            }
            vm->error = true;
            if (sym_id >= 0 && (unsigned)sym_id < ctx.atom_table_cap &&
                ctx.atom_table[sym_id])
                show_error("undefined variable: %s", ctx.atom_table[sym_id]);
            else
                show_error("undefined variable: <id %ld>", (long)sym_id);
            vm->error_msg = ctx.last_error;
            vm->running = false;
            break;
        }

        switch (op) {
        case OP_CONST: {
            // Fallback (shouldn't reach here normally)
            unsigned idx = vm->code->code[vm->ip++];
            unsigned val = vm->code->constants[idx];
            if (CELL_TYPE(val) == BT_NUM) {
                int64_t n = CELL_ID(val);
                if (FITS_FIXNUM(n))
                    val = MAKE_FIXNUM((int32_t)n);
            }
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
                vm->error = true;
                if (sym_id >= 0 && (unsigned)sym_id < ctx.atom_table_cap &&
                    ctx.atom_table[sym_id])
                    show_error("undefined variable: %s",
                               ctx.atom_table[sym_id]);
                vm->error_msg = ctx.last_error[0] ? ctx.last_error
                                                   : "undefined variable";
                vm->running = false;
                break;
            }
            if (CELL_TYPE(val) == BT_NUM) {
                int64_t n = CELL_ID(val);
                if (FITS_FIXNUM(n))
                    val = MAKE_FIXNUM((int32_t)n);
            }
            vm_push(vm, val);
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
            defvar(atom, val, vm->env);
            gc_unprotect(1);
            break;
        }

        case OP_SET: {
            int64_t sym_id = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
            val = ensure_boxed(val);
            unsigned result = setvar(sym_id, val, vm->env);
            if (result == TOK_ERROR) {
                vm->error = true;
                vm->error_msg = "unbound variable in set!";
                vm->running = false;
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
                vm->error = true;
                vm->error_msg = "unbound variable in set!";
                vm->running = false;
            }
            break;
        }

        case OP_RETURN_LOCALS: {
            vm->ip++; // skip operand (not needed — bp tracks locals)
            unsigned val = vm_pop(vm);
            if (vm->fp == 0) {
                vm->sp = vm->bp; // remove locals
                vm_push(vm, val);
                vm->running = false;
            } else {
                unsigned callee_bp = vm->bp;
                pop_frame(vm); // restores caller's bp
                vm->sp = callee_bp; // remove callee's locals + any temps
                vm_push(vm, val);
            }
            break;
        }

        // Stack locals — direct access without environment lookup
        case OP_LOCAL_GET: {
            unsigned slot = vm->code->code[vm->ip++];
            vm_push(vm, vm->stack[vm->bp + slot]);
            break;
        }
        case OP_LOCAL_GET0:
            vm_push(vm, vm->stack[vm->bp]);
            break;
        case OP_LOCAL_GET1:
            vm_push(vm, vm->stack[vm->bp + 1]);
            break;
        case OP_LOCAL_GET2:
            vm_push(vm, vm->stack[vm->bp + 2]);
            break;
        case OP_LOCAL_GET3:
            vm_push(vm, vm->stack[vm->bp + 3]);
            break;
        case OP_LOCAL_SET: {
            unsigned slot = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
            unsigned old = vm->stack[vm->bp + slot];
            vm->stack[vm->bp + slot] = val;
            vm_push(vm, old);
            break;
        }
        case OP_LOCAL_SET_VOID: {
            unsigned slot = vm->code->code[vm->ip++];
            vm->stack[vm->bp + slot] = vm_pop(vm);
            break;
        }

        case OP_CLOSURE: {
            unsigned child_idx = vm->code->code[vm->ip++];
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
            vm_box_stack_args(vm, argc);

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
                if (CELL_TYPE(filename_cell) != BT_STRING) {
                    VM_ERROR_BREAK(vm, "load: expected string argument");
                }
                if (!ctx.load_callback) {
                    VM_ERROR_BREAK(vm, "load: callback not set");
                }
                const char *filename = GET_STRING_PTR(filename_cell);
                unsigned result = ctx.load_callback(filename, &vm->env);
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
                if (IS_PAIR(producer) &&
                    CELL_TYPE(car(producer)) == BT_CLOSURE) {
                    // Bytecode closure - use vm_call_closure
                    result = vm_call_closure(producer, 0);
                } else if (IS_FUNCTION(producer)) {
                    // CPS closure - use CPS evaluator via callback
                    // Build: (producer)
                    unsigned call_expr = alloc_cons(producer, 0);
                    if (ctx.eval_callback) {
                        result = ctx.eval_callback(call_expr, vm->env);
                    } else {
                        gc_unprotect(2);
                        vm->error = true;
                        vm->error_msg =
                            "call-with-values: cannot call CPS producer";
                        vm->running = false;
                        break;
                    }
                } else if (CELL_TYPE(producer) == BT_BUILTIN) {
                    // Builtin - call directly with no args
                    result = apply_primitive_argv(CELL_ID(producer), 0, NULL);
                } else {
                    gc_unprotect(2);
                    vm->error = true;
                    vm->error_msg =
                        "call-with-values: producer is not a procedure";
                    vm->running = false;
                    break;
                }

                gc_unprotect(2);

                if (result == TOK_ERROR) {
                    VM_ERROR_BREAK(vm, "call-with-values: producer failed");
                }

                // If result is multival, unpack values as args to consumer
                if (IS_MULTIVAL(result)) {
                    unsigned vals = car(result);
                    unsigned consumer_argc = 0;
                    FORLIST(v, vals)
                    {
                        vm_push(vm, car(v));
                        consumer_argc++;
                    }
                    vm_apply(vm, consumer, consumer_argc, false);
                } else {
                    // Single value - pass as single argument
                    vm_push(vm, result);
                    vm_apply(vm, consumer, 1, false);
                }
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
                vm->error = true;
                vm->error_msg =
                    ctx.last_error[0] ? ctx.last_error : "primitive error";
                vm->running = false;
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

            // Push args from list
            unsigned argc = 0;
            FORLIST(a, args_list)
            {
                vm_push(vm, car(a));
                argc++;
            }

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
            vm_box_stack_args(vm, n);
            // Collect n values into a multival
            unsigned vals = 0;
            for (unsigned i = 0; i < n; i++) {
                vals = alloc_cons(vm->stack[vm->sp - n + i], vals);
            }
            // Reverse
            unsigned rev_vals = 0;
            while (vals) {
                rev_vals = alloc_cons(car(vals), rev_vals);
                vals = cdr(vals);
            }
            vm->sp -= n;

            unsigned multival = make_typed_cell(BT_MULTIVAL, rev_vals, 0);
            vm_push(vm, multival);
            break;
        }

        case OP_CALLWITHVALUES: {
            // Stack: [producer, consumer]
            unsigned consumer = vm_pop(vm);
            unsigned producer = vm_pop(vm);

            // Call producer with no args
            vm_apply(vm, producer, 0, false);

            // After producer returns, get result
            // If result is multival, unpack it; otherwise use as single arg
            unsigned result = vm_pop(vm);
            if (IS_MULTIVAL(result)) {
                unsigned vals = car(result);
                unsigned argc = 0;
                FORLIST(v, vals)
                {
                    vm_push(vm, car(v));
                    argc++;
                }
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

            // Create syntax transformer
            if (!IS_KEYWORD(car(transformer_form), ctx.kw_syntax_rules)) {
                vm->error = true;
                vm->error_msg = "define-syntax: expected syntax-rules";
                vm->running = false;
                break;
            }

            // CRITICAL: Protect transformer_form before any allocations
            gc_protect(&transformer_form);

            // Use default ellipsis for VM path
            int64_t ellipsis_id = ctx.kw_ellipsis;
            unsigned ellipsis_cell = store(ellipsis_id);
            unsigned literals = cadr(transformer_form);
            unsigned rules = cddr(transformer_form);
            gc_protect(&ellipsis_cell);
            gc_protect(&literals);
            gc_protect(&rules);

            // Compile patterns for each rule
            unsigned compiled_rules = 0;
            gc_protect(&compiled_rules);
            unsigned compiled_tail = 0;
            gc_protect(&compiled_tail);

            for (unsigned r = rules; r; r = cdr(r)) {
                unsigned rule = car(r);
                unsigned pattern = car(rule);
                unsigned tmpl = cadr(rule);

                // Skip the macro name in pattern
                if (IS_PAIR(pattern))
                    pattern = cdr(pattern);

                // Compile the pattern
                compiled_pattern *cpat =
                    compile_pattern(pattern, literals, ellipsis_id);

                // Create cell to hold compiled pattern
                unsigned cpat_cell = alloc();
                CELL_TYPE(cpat_cell) = BT_COMPILED_PATTERN;
                CELL_PTR(cpat_cell) = cpat;

                // Build compiled rule: (cpat_cell . template)
                gc_protect(&cpat_cell);
                gc_protect(&tmpl);
                unsigned new_rule = alloc_cons(cpat_cell, tmpl);
                unsigned new_node = alloc_cons(new_rule, 0);
                gc_unprotect(2);

                if (!compiled_rules) {
                    compiled_rules = new_node;
                    compiled_tail = new_node;
                } else {
                    CELL_CDR(compiled_tail) = new_node;
                    compiled_tail = new_node;
                }
            }

            // Build transformer structure
            unsigned lit_rules = alloc_cons(literals, compiled_rules);
            unsigned car_val = alloc_cons(ellipsis_cell, lit_rules);
            unsigned transformer = make_typed_cell(BT_SYNTAX, car_val, vm->env);
            gc_unprotect(6);

            // Define in current environment
            unsigned atom = alloc();
            CELL_TYPE(atom) = BT_ATOM;
            CELL_ID(atom) = sym_id;
            defvar(atom, transformer, vm->env);
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
            car_val = ensure_boxed(car_val);
            gc_protect(&car_val);
            cdr_val = ensure_boxed(cdr_val);
            gc_unprotect(1);
            vm_push(vm, alloc_cons(car_val, cdr_val));
            break;
        }

        case OP_NULLP: {
            unsigned val = vm_pop(vm);
            vm_push(vm, IS_NIL(val) ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_PAIRP: {
            unsigned val = vm_pop(vm);
            vm_push(vm, (!IS_FIXNUM(val) && val && CELL_TYPE(val) == BT_CONS)
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
            if (CELL_TYPE(n) == BT_NUM) {
                int64_t val = CELL_ID(n);
                if (val < INT64_MAX) {
                    vm_push(vm, store(val + 1));
                } else {
                    // Overflow to bignum
                    bignum *bn = bn_from_int(val);
                    bignum *one = bn_from_int(1);
                    bignum *result = bn_add(bn, one);
                    bn_free(bn);
                    bn_free(one);
                    vm_push(vm, store_integer(result));
                }
            } else if (CELL_TYPE(n) == BT_BIGNUM) {
                bignum *bn = bn_copy(get_bignum(n));
                bignum *one = bn_from_int(1);
                bignum *result = bn_add(bn, one);
                bn_free(bn);
                bn_free(one);
                vm_push(vm, store_integer(result));
            } else {
                vm->error = true;
                vm->error_msg = "add1: not a number";
                vm->running = false;
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
            if (CELL_TYPE(n) == BT_NUM) {
                int64_t val = CELL_ID(n);
                if (val > INT64_MIN) {
                    vm_push(vm, store(val - 1));
                } else {
                    // Underflow to bignum
                    bignum *bn = bn_from_int(val);
                    bignum *one = bn_from_int(1);
                    bignum *result = bn_sub(bn, one);
                    bn_free(bn);
                    bn_free(one);
                    vm_push(vm, store_integer(result));
                }
            } else if (CELL_TYPE(n) == BT_BIGNUM) {
                bignum *bn = bn_copy(get_bignum(n));
                bignum *one = bn_from_int(1);
                bignum *result = bn_sub(bn, one);
                bn_free(bn);
                bn_free(one);
                vm_push(vm, store_integer(result));
            } else {
                vm->error = true;
                vm->error_msg = "sub1: not a number";
                vm->running = false;
            }
            break;
        }

        case OP_ZEROP: {
            unsigned n = vm_pop(vm);
            if (IS_FIXNUM(n)) {
                vm_push(vm, FIXNUM_VALUE(n) == 0 ? ctx.atom_true : ctx.atom_false);
                break;
            }
            bool is_zero = false;
            if (n == 0) {
                is_zero = true;
            } else if (CELL_TYPE(n) == BT_NUM) {
                is_zero = CELL_ID(n) == 0;
            } else if (CELL_TYPE(n) == BT_BIGNUM) {
                is_zero = bn_is_zero(get_bignum(n));
            }
            vm_push(vm, is_zero ? ctx.atom_true : ctx.atom_false);
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
                eq = CELL_TYPE(b) == BT_NUM &&
                     CELL_ID(b) == (int64_t)FIXNUM_VALUE(a);
            } else if (IS_FIXNUM(b)) {
                eq = CELL_TYPE(a) == BT_NUM &&
                     CELL_ID(a) == (int64_t)FIXNUM_VALUE(b);
            } else if (CELL_TYPE(a) == CELL_TYPE(b)) {
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
                vm_push(vm, FITS_FIXNUM(sum) ? MAKE_FIXNUM((int32_t)sum)
                                             : store(sum));
                // Quicken: next time, skip type checks
                vm->code->code[vm->ip - 1] = OP_ADD_INT;
                break;
            }
            a = ensure_boxed(a);
            gc_protect(&a);
            b = ensure_boxed(b);
            gc_unprotect(1);
            unsigned result = binary_add(a, b);
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
                vm_push(vm, FITS_FIXNUM(diff) ? MAKE_FIXNUM((int32_t)diff)
                                              : store(diff));
                vm->code->code[vm->ip - 1] = OP_SUB_INT;
                break;
            }
            a = ensure_boxed(a);
            gc_protect(&a);
            b = ensure_boxed(b);
            gc_unprotect(1);
            unsigned result = binary_sub(a, b);
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
                vm_push(vm, FITS_FIXNUM(prod) ? MAKE_FIXNUM((int32_t)prod)
                                              : store(prod));
                vm->code->code[vm->ip - 1] = OP_MUL_INT;
                break;
            }
            a = ensure_boxed(a);
            gc_protect(&a);
            b = ensure_boxed(b);
            gc_unprotect(1);
            unsigned result = binary_mul(a, b);
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
                    int32_t q = va / vb;
                    vm_push(vm, MAKE_FIXNUM(q));
                    break;
                }
                // Result is rational — box and use full path
                a = store((int64_t)va);
                gc_protect(&a);
                b = store((int64_t)vb);
                gc_unprotect(1);
            } else {
                a = ensure_boxed(a);
                gc_protect(&a);
                b = ensure_boxed(b);
                gc_unprotect(1);
            }
            unsigned result = binary_div(a, b);
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
            a = ensure_boxed(a);
            gc_protect(&a);
            b = ensure_boxed(b);
            gc_unprotect(1);
            unsigned result = binary_mod(a, b);
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
            a = ensure_boxed(a); gc_protect(&a);
            b = ensure_boxed(b); gc_unprotect(1);
            unsigned result = binary_lt(a, b);
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
            a = ensure_boxed(a); gc_protect(&a);
            b = ensure_boxed(b); gc_unprotect(1);
            unsigned result = binary_gt(a, b);
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
            a = ensure_boxed(a); gc_protect(&a);
            b = ensure_boxed(b); gc_unprotect(1);
            unsigned result = binary_le(a, b);
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
            a = ensure_boxed(a); gc_protect(&a);
            b = ensure_boxed(b); gc_unprotect(1);
            unsigned result = binary_ge(a, b);
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
            a = ensure_boxed(a); gc_protect(&a);
            b = ensure_boxed(b); gc_unprotect(1);
            unsigned result = binary_numeq(a, b);
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
            write_barrier(pair, val);
            CELL_CAR(pair) = val;
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
            write_barrier(pair, val);
            CELL_CDR(pair) = val;
            vm_push(vm, val);
            break;
        }

        case OP_LIST1: {
            unsigned a = vm_pop(vm);
            a = ensure_boxed(a);
            vm_push(vm, alloc_cons(a, 0));
            break;
        }

        case OP_LIST2: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            a = ensure_boxed(a);
            gc_protect(&a);
            b = ensure_boxed(b);
            // Build bottom-up to avoid unspecified evaluation order
            unsigned tail = alloc_cons(b, 0);
            unsigned result = alloc_cons(a, tail);
            gc_unprotect(1);
            vm_push(vm, result);
            break;
        }

        case OP_LIST3: {
            unsigned c = vm_pop(vm);
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            a = ensure_boxed(a);
            gc_protect(&a);
            b = ensure_boxed(b);
            gc_protect(&b);
            c = ensure_boxed(c);
            unsigned t2 = alloc_cons(c, 0);
            unsigned t1 = alloc_cons(b, t2);
            unsigned result = alloc_cons(a, t1);
            gc_unprotect(2);
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
            bool is_zero;
            if (IS_FIXNUM(n)) {
                is_zero = FIXNUM_VALUE(n) == 0;
            } else {
                is_zero =
                    (n == 0) || (CELL_TYPE(n) == BT_NUM && CELL_ID(n) == 0) ||
                    (CELL_TYPE(n) == BT_BIGNUM && bn_is_zero(get_bignum(n)));
            }
            if (is_zero) {
                vm->ip = target;
            }
            break;
        }

        case OP_JUMPIFNOTZERO: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned n = vm_pop(vm);
            bool is_zero;
            if (IS_FIXNUM(n)) {
                is_zero = FIXNUM_VALUE(n) == 0;
            } else {
                is_zero =
                    (n == 0) || (CELL_TYPE(n) == BT_NUM && CELL_ID(n) == 0) ||
                    (CELL_TYPE(n) == BT_BIGNUM && bn_is_zero(get_bignum(n)));
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
            vm_push(vm, (!IS_FIXNUM(val) && val && CELL_TYPE(val) == BT_STRING)
                            ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_VECTORP: {
            unsigned val = vm_pop(vm);
            vm_push(vm, (!IS_FIXNUM(val) && val && CELL_TYPE(val) == BT_VECTOR)
                            ? ctx.atom_true : ctx.atom_false);
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
            bool is_list = true;
            unsigned p = val;
            while (p != 0) {
                if (IS_FIXNUM(p) || !IS_PAIR(p)) {
                    is_list = false;
                    break;
                }
                p = cdr(p);
            }
            vm_push(vm, is_list ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_INTEGERP: {
            unsigned val = vm_pop(vm);
            if (IS_FIXNUM(val)) {
                vm_push(vm, ctx.atom_true);
                break;
            }
            bool is_int = (val != 0) && (CELL_TYPE(val) == BT_NUM ||
                                         CELL_TYPE(val) == BT_BIGNUM);
            vm_push(vm, is_int ? ctx.atom_true : ctx.atom_false);
            break;
        }

        // List operations
        case OP_LENGTH: {
            unsigned list = vm_pop(vm);
            unsigned len = 0;
            for (unsigned p = list; p != 0; p = cdr(p)) {
                if (IS_FIXNUM(p) || !IS_PAIR(p)) {
                    vm->error = true;
                    vm->error_msg = "length: not a proper list";
                    vm->running = false;
                    break;
                }
                len++;
            }
            if (vm->running) {
                vm_push(vm, store(len));
            }
            break;
        }

        case OP_APPEND: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (a == 0) {
                vm_push(vm, b);
                break;
            }
            // Build result by copying a, then appending b
            gc_protect(&b);
            gc_protect(&a);
            unsigned result = 0;
            unsigned tail = 0;
            gc_protect(&result);
            gc_protect(&tail);
            unsigned p = a;
            gc_protect(&p);
            for (; p != 0; p = cdr(p)) {
                if (IS_FIXNUM(p) || !IS_PAIR(p)) {
                    gc_unprotect(5);
                    vm->error = true;
                    vm->error_msg = "append: not a proper list";
                    vm->running = false;
                    break;
                }
                unsigned new_cell = alloc_cons(car(p), 0);
                if (result == 0) {
                    result = new_cell;
                    tail = new_cell;
                } else {
                    CELL_CDR(tail) = new_cell;
                    tail = new_cell;
                }
            }
            if (vm->running) {
                if (tail != 0) {
                    CELL_CDR(tail) = b;
                }
                gc_unprotect(5); // p, tail, result, a, b
                vm_push(vm, result == 0 ? b : result);
            }
            break;
        }

        case OP_REVERSE: {
            unsigned list = vm_pop(vm);
            gc_protect(&list);
            unsigned result = 0;
            gc_protect(&result);
            unsigned p = list;
            gc_protect(&p);
            for (; p != 0; p = cdr(p)) {
                if (IS_FIXNUM(p) || !IS_PAIR(p)) {
                    gc_unprotect(3);
                    vm->error = true;
                    vm->error_msg = "reverse: not a proper list";
                    vm->running = false;
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
                    (CELL_TYPE(elem) == CELL_TYPE(obj) &&
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
            if (IS_FIXNUM(vec) || CELL_TYPE(vec) != BT_VECTOR) {
                VM_ERROR_BREAK(vm, "vector-ref: not a vector");
            }
            int64_t i = 0;
            if (IS_FIXNUM(idx)) {
                i = FIXNUM_VALUE(idx);
            } else if (CELL_TYPE(idx) == BT_NUM) {
                i = CELL_ID(idx);
            } else {
                VM_ERROR_BREAK(vm, "vector-ref: index not an integer");
            }
            vector_data *vd = GET_VECTOR_PTR(vec);
            if (i < 0 || (uint64_t)i >= vd->len) {
                VM_ERROR_BREAK(vm, "vector-ref: index out of bounds");
            }
            vm_push(vm, vd->data[i]);
            break;
        }

        case OP_VECTORSET: {
            unsigned val = vm_pop(vm);
            unsigned idx = vm_pop(vm);
            unsigned vec = vm_pop(vm);
            if (IS_FIXNUM(vec) || CELL_TYPE(vec) != BT_VECTOR) {
                VM_ERROR_BREAK(vm, "vector-set!: not a vector");
            }
            int64_t i = 0;
            if (IS_FIXNUM(idx)) {
                i = FIXNUM_VALUE(idx);
            } else if (CELL_TYPE(idx) == BT_NUM) {
                i = CELL_ID(idx);
            } else {
                VM_ERROR_BREAK(vm, "vector-set!: index not an integer");
            }
            vector_data *vd = GET_VECTOR_PTR(vec);
            if (i < 0 || (uint64_t)i >= vd->len) {
                VM_ERROR_BREAK(vm, "vector-set!: index out of bounds");
            }
            val = ensure_boxed(val);
            write_barrier(vec, val);
            vd->data[i] = val;
            vm_push(vm, val);
            break;
        }

        case OP_VECTORLEN: {
            unsigned vec = vm_pop(vm);
            if (IS_FIXNUM(vec) || CELL_TYPE(vec) != BT_VECTOR) {
                VM_ERROR_BREAK(vm, "vector-length: not a vector");
            }
            vector_data *vd = GET_VECTOR_PTR(vec);
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
            if (CELL_TYPE(n) == BT_NUM) {
                int64_t val = CELL_ID(n);
                if (val == INT64_MIN) {
                    // Overflow to bignum
                    bignum *bn = bn_from_int(val);
                    bn_neg_ip(bn);
                    vm_push(vm, store_integer(bn));
                } else {
                    vm_push(vm, store(-val));
                }
            } else if (CELL_TYPE(n) == BT_BIGNUM) {
                bignum *bn = bn_neg(get_bignum(n));
                vm_push(vm, store_integer(bn));
            } else if (CELL_TYPE(n) == BT_INEXACT) {
                vm_push(vm, store_inexact(-to_double(n)));
            } else if (CELL_TYPE(n) == BT_RATIONAL) {
                unsigned neg_num = negate_number(car(n));
                gc_protect(&neg_num);
                unsigned denom = cdr(n);
                unsigned result = alloc();
                gc_unprotect(1);
                CELL_TYPE(result) = BT_RATIONAL;
                CELL_CAR(result) = neg_num;
                CELL_CDR(result) = denom;
                vm_push(vm, result);
            } else {
                vm->error = true;
                vm->error_msg = "-: not a number";
                vm->running = false;
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
            if (CELL_TYPE(n) == BT_NUM) {
                int64_t val = CELL_ID(n);
                if (val == INT64_MIN) {
                    bignum *bn = bn_from_int(val);
                    bn_neg_ip(bn);
                    vm_push(vm, store_integer(bn));
                } else {
                    vm_push(vm, store(val < 0 ? -val : val));
                }
            } else if (CELL_TYPE(n) == BT_BIGNUM) {
                bignum *bn = get_bignum(n);
                if (bn->sign) {
                    bignum *result = bn_neg(bn);
                    vm_push(vm, store_integer(result));
                } else {
                    vm_push(vm, n);
                }
            } else if (CELL_TYPE(n) == BT_INEXACT) {
                double d = to_double(n);
                vm_push(vm, store_inexact(d < 0 ? -d : d));
            } else if (CELL_TYPE(n) == BT_RATIONAL) {
                if (is_negative_number(car(n))) {
                    unsigned abs_num = negate_number(car(n));
                    gc_protect(&abs_num);
                    unsigned denom = cdr(n);
                    unsigned result = alloc();
                    gc_unprotect(1);
                    CELL_TYPE(result) = BT_RATIONAL;
                    CELL_CAR(result) = abs_num;
                    CELL_CDR(result) = denom;
                    vm_push(vm, result);
                } else {
                    vm_push(vm, n);
                }
            } else {
                vm->error = true;
                vm->error_msg = "abs: not a number";
                vm->running = false;
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
            if (CELL_TYPE(n) == BT_NUM) {
                positive = CELL_ID(n) > 0;
            } else if (CELL_TYPE(n) == BT_BIGNUM) {
                bignum *bn = get_bignum(n);
                positive = !bn->sign && bn->len > 0;
            } else if (CELL_TYPE(n) == BT_INEXACT) {
                positive = to_double(n) > 0;
            } else if (CELL_TYPE(n) == BT_RATIONAL) {
                positive = !is_negative_number(car(n)) &&
                           !(CELL_TYPE(car(n)) == BT_NUM && CELL_ID(car(n)) == 0);
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
            if (CELL_TYPE(n) == BT_NUM) {
                negative = CELL_ID(n) < 0;
            } else if (CELL_TYPE(n) == BT_BIGNUM) {
                negative = bn_sign(get_bignum(n)) < 0;
            } else if (CELL_TYPE(n) == BT_INEXACT) {
                negative = to_double(n) < 0;
            } else if (CELL_TYPE(n) == BT_RATIONAL) {
                negative = is_negative_number(car(n));
            }
            vm_push(vm, negative ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_EVEN: {
            unsigned n = vm_pop(vm);
            if (IS_FIXNUM(n)) {
                vm_push(vm, (FIXNUM_VALUE(n) & 1) == 0 ? ctx.atom_true : ctx.atom_false);
                break;
            }
            bool even = false;
            if (CELL_TYPE(n) == BT_NUM) {
                even = (CELL_ID(n) & 1) == 0;
            } else if (CELL_TYPE(n) == BT_BIGNUM) {
                bignum *bn = get_bignum(n);
                even = bn->len == 0 || (bn->limbs[0] & 1) == 0;
            } else {
                vm->error = true;
                vm->error_msg = "even?: not an integer";
                vm->running = false;
                break;
            }
            vm_push(vm, even ? ctx.atom_true : ctx.atom_false);
            break;
        }

        case OP_ODD: {
            unsigned n = vm_pop(vm);
            if (IS_FIXNUM(n)) {
                vm_push(vm, (FIXNUM_VALUE(n) & 1) != 0 ? ctx.atom_true : ctx.atom_false);
                break;
            }
            bool odd = false;
            if (CELL_TYPE(n) == BT_NUM) {
                odd = (CELL_ID(n) & 1) == 1;
            } else if (CELL_TYPE(n) == BT_BIGNUM) {
                bignum *bn = get_bignum(n);
                odd = bn->len > 0 && (bn->limbs[0] & 1) == 1;
            } else {
                vm->error = true;
                vm->error_msg = "odd?: not an integer";
                vm->running = false;
                break;
            }
            vm_push(vm, odd ? ctx.atom_true : ctx.atom_false);
            break;
        }

        // Quickened (type-specialized) arithmetic — assume fixnum operands
        case OP_ADD_INT: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (__builtin_expect(IS_FIXNUM(a) && IS_FIXNUM(b), 1)) {
                int64_t sum = (int64_t)FIXNUM_VALUE(a) + (int64_t)FIXNUM_VALUE(b);
                vm_push(vm, FITS_FIXNUM(sum) ? MAKE_FIXNUM((int32_t)sum)
                                             : store(sum));
            } else {
                // Deopt: rewrite back to generic and re-execute
                vm->code->code[vm->ip - 1] = OP_ADD;
                vm_push(vm, a); vm_push(vm, b); vm->ip--;
                continue;
            }
            break;
        }
        case OP_SUB_INT: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (__builtin_expect(IS_FIXNUM(a) && IS_FIXNUM(b), 1)) {
                int64_t diff = (int64_t)FIXNUM_VALUE(a) - (int64_t)FIXNUM_VALUE(b);
                vm_push(vm, FITS_FIXNUM(diff) ? MAKE_FIXNUM((int32_t)diff)
                                              : store(diff));
            } else {
                vm->code->code[vm->ip - 1] = OP_SUB;
                vm_push(vm, a); vm_push(vm, b); vm->ip--;
                continue;
            }
            break;
        }
        case OP_MUL_INT: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            if (__builtin_expect(IS_FIXNUM(a) && IS_FIXNUM(b), 1)) {
                int64_t prod = (int64_t)FIXNUM_VALUE(a) * (int64_t)FIXNUM_VALUE(b);
                vm_push(vm, FITS_FIXNUM(prod) ? MAKE_FIXNUM((int32_t)prod)
                                              : store(prod));
            } else {
                vm->code->code[vm->ip - 1] = OP_MUL;
                vm_push(vm, a); vm_push(vm, b); vm->ip--;
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
            } else if (IS_FIXNUM(a) || IS_FIXNUM(b)) {
                a = ensure_boxed(a);
                gc_protect(&a);
                b = ensure_boxed(b);
                gc_unprotect(1);
                eq = (IS_NUM(a) && IS_NUM(b) && CELL_ID(a) == CELL_ID(b));
            } else if (IS_NUM(a) && IS_NUM(b)) {
                eq = (CELL_ID(a) == CELL_ID(b));
            } else {
                a = ensure_boxed(a);
                gc_protect(&a);
                b = ensure_boxed(b);
                gc_unprotect(1);
                unsigned result = binary_numeq(a, b);
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
                a = ensure_boxed(a);
                gc_protect(&a);
                b = ensure_boxed(b);
                gc_unprotect(1);
                unsigned result;
                switch (fused_op) {
                case OP_LT_JUMPIFNOT: result = binary_lt(a, b); break;
                case OP_GT_JUMPIFNOT: result = binary_gt(a, b); break;
                case OP_LE_JUMPIFNOT: result = binary_le(a, b); break;
                case OP_GE_JUMPIFNOT: result = binary_ge(a, b); break;
                default: result = ctx.atom_false; break;
                }
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
                if (sym_id >= 0 && (unsigned)sym_id < ctx.atom_table_cap &&
                    ctx.atom_table[sym_id])
                    show_error("undefined variable: %s",
                               ctx.atom_table[sym_id]);
                VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                     : "undefined variable");
            }
            if (IS_FIXNUM(n)) {
                int32_t val = FIXNUM_VALUE(n);
                vm_push(vm, val < FIXNUM_MAX ? MAKE_FIXNUM(val + 1)
                                             : store((int64_t)val + 1));
            } else if (CELL_TYPE(n) == BT_NUM) {
                int64_t val = CELL_ID(n);
                int64_t r = val + 1;
                vm_push(vm, FITS_FIXNUM(r) ? MAKE_FIXNUM((int32_t)r)
                                           : store(r));
            } else {
                n = ensure_boxed(n);
                gc_protect(&n);
                unsigned one = store(1);
                gc_unprotect(1);
                unsigned argv[2] = {n, one};
                unsigned result = prim_plus(2, argv);
                vm_push(vm, result);
            }
            break;
        }

        case OP_LOOKUP_SUB1: {
            int64_t sym_id = vm->code->code[vm->ip];
            unsigned *cache_slot = &vm->code->code[vm->ip + 1];
            vm->ip += 3;
            unsigned n = ic_lookup(sym_id, vm->env, cache_slot);
            if (n == TOK_ERROR) {
                if (sym_id >= 0 && (unsigned)sym_id < ctx.atom_table_cap &&
                    ctx.atom_table[sym_id])
                    show_error("undefined variable: %s",
                               ctx.atom_table[sym_id]);
                VM_ERROR_BREAK(vm, ctx.last_error[0] ? ctx.last_error
                                                     : "undefined variable");
            }
            if (IS_FIXNUM(n)) {
                int32_t val = FIXNUM_VALUE(n);
                vm_push(vm, val > FIXNUM_MIN ? MAKE_FIXNUM(val - 1)
                                             : store((int64_t)val - 1));
            } else if (CELL_TYPE(n) == BT_NUM) {
                int64_t val = CELL_ID(n);
                int64_t r = val - 1;
                vm_push(vm, FITS_FIXNUM(r) ? MAKE_FIXNUM((int32_t)r)
                                           : store(r));
            } else {
                n = ensure_boxed(n);
                gc_protect(&n);
                unsigned one = store(1);
                gc_unprotect(1);
                unsigned argv[2] = {n, one};
                unsigned result = prim_minus(2, argv);
                vm_push(vm, result);
            }
            break;
        }

        default:
            vm->error = true;
            vm->error_msg = "unknown opcode";
            vm->running = false;
            break;
        }
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
}
