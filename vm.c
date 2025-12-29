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
#include "context.h"
#include "env.h"
#include "eval.h"
#include "macros.h"
#include "primitives.h"
#include <stdlib.h>
#include <string.h>

// ============================================================================
// VM Constants
// ============================================================================

#define INITIAL_STACK_SIZE 1024
#define INITIAL_FRAMES_SIZE 256
#define MAX_STACK_SIZE (1024 * 1024)
#define MAX_FRAMES_SIZE (64 * 1024)

// ============================================================================
// VM Initialization
// ============================================================================

void vm_init(vm_state *vm)
{
    memset(vm, 0, sizeof(vm_state));

    vm->stack_cap = INITIAL_STACK_SIZE;
    vm->stack = malloc(vm->stack_cap * sizeof(unsigned));

    vm->frames_cap = INITIAL_FRAMES_SIZE;
    vm->frames = malloc(vm->frames_cap * sizeof(vm_frame));

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
// Stack Operations
// ============================================================================

void vm_push(vm_state *vm, unsigned val)
{
    if (vm->sp >= vm->stack_cap) {
        if (vm->stack_cap >= MAX_STACK_SIZE) {
            vm->error = true;
            vm->error_msg = "stack overflow";
            vm->running = false;
            return;
        }
        vm->stack_cap *= 2;
        vm->stack = realloc(vm->stack, vm->stack_cap * sizeof(unsigned));
    }
    vm->stack[vm->sp++] = val;
}

unsigned vm_pop(vm_state *vm)
{
    if (vm->sp == 0) {
        vm->error = true;
        vm->error_msg = "stack underflow";
        vm->running = false;
        return 0;
    }
    return vm->stack[--vm->sp];
}

static unsigned vm_peek(vm_state *vm, unsigned depth)
{
    if (depth >= vm->sp) {
        vm->error = true;
        vm->error_msg = "stack underflow";
        vm->running = false;
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
    if (vm->fp >= vm->frames_cap) {
        if (vm->frames_cap >= MAX_FRAMES_SIZE) {
            vm->error = true;
            vm->error_msg = "call stack overflow";
            vm->running = false;
            return;
        }
        vm->frames_cap *= 2;
        vm->frames = realloc(vm->frames, vm->frames_cap * sizeof(vm_frame));
    }
    vm->frames[vm->fp].code = code;
    vm->frames[vm->fp].ip = ip;
    vm->frames[vm->fp].bp = bp;
    vm->frames[vm->fp].env = env;
    vm->fp++;
}

static void pop_frame(vm_state *vm)
{
    if (vm->fp == 0) {
        vm->error = true;
        vm->error_msg = "frame stack underflow";
        vm->running = false;
        return;
    }
    vm->fp--;
    vm->code = vm->frames[vm->fp].code;
    vm->ip = vm->frames[vm->fp].ip;
    vm->env = vm->frames[vm->fp].env;
}

// ============================================================================
// Continuation Support
// ============================================================================

/**
 * Captured continuation structure.
 * Stored as a cell with type BT_CONT, id = pointer to this struct.
 */
typedef struct {
    unsigned *stack;
    unsigned sp;
    vm_frame *frames;
    unsigned fp;
    code_object *code;
    unsigned ip;
    unsigned env;
} vm_continuation;

static unsigned capture_continuation(vm_state *vm)
{
    vm_continuation *cont = malloc(sizeof(vm_continuation));

    // Copy stack
    cont->sp = vm->sp;
    cont->stack = malloc(cont->sp * sizeof(unsigned));
    memcpy(cont->stack, vm->stack, cont->sp * sizeof(unsigned));

    // Copy frames
    cont->fp = vm->fp;
    cont->frames = malloc(cont->fp * sizeof(vm_frame));
    memcpy(cont->frames, vm->frames, cont->fp * sizeof(vm_frame));

    // Copy execution state
    cont->code = vm->code;
    cont->ip = vm->ip;
    cont->env = vm->env;

    // Wrap in a cell
    unsigned cell = alloc();
    CELL_TYPE(cell) = BT_CONT;
    CELL_ID(cell) = (int64_t)(intptr_t)cont;

    return cell;
}

static void restore_continuation(vm_state *vm, unsigned cont_cell,
                                 unsigned value)
{
    vm_continuation *cont = (vm_continuation *)(intptr_t)CELL_ID(cont_cell);

    // Restore stack (with return value on top)
    if (vm->stack_cap < cont->sp + 1) {
        vm->stack_cap = cont->sp + 1;
        vm->stack = realloc(vm->stack, vm->stack_cap * sizeof(unsigned));
    }
    memcpy(vm->stack, cont->stack, cont->sp * sizeof(unsigned));
    vm->sp = cont->sp;
    vm->stack[vm->sp++] = value; // Push return value

    // Restore frames
    if (vm->frames_cap < cont->fp) {
        vm->frames_cap = cont->fp;
        vm->frames = realloc(vm->frames, vm->frames_cap * sizeof(vm_frame));
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

static void vm_apply(vm_state *vm, unsigned fn, unsigned argc, bool tail)
{
    if (IS_BUILTIN(fn)) {
        int64_t prim_id = CELL_ID(fn);

        // Build argument list (args are on stack, topmost is last arg)
        unsigned args = 0;
        for (unsigned i = 0; i < argc; i++) {
            unsigned val = vm->stack[vm->sp - argc + i];
            args = alloc_cons(val, args);
        }
        // Reverse to get correct order
        unsigned rev_args = 0;
        while (args) {
            rev_args = alloc_cons(car(args), rev_args);
            args = cdr(args);
        }

        // Pop arguments
        vm->sp -= argc;

        // Handle special primitives
        if (prim_id == PCALLCC) {
            // call/cc: pass continuation to procedure
            if (argc != 1) {
                vm->error = true;
                vm->error_msg = "call/cc: expected 1 argument";
                vm->running = false;
                return;
            }
            unsigned proc = car(rev_args);
            unsigned cont = capture_continuation(vm);
            // Push continuation as argument and call proc
            vm_push(vm, cont);
            vm_apply(vm, proc, 1, tail);
            return;
        }

        if (prim_id == PAPPLY) {
            // apply: (apply proc args-list)
            if (argc != 2) {
                vm->error = true;
                vm->error_msg = "apply: expected 2 arguments";
                vm->running = false;
                return;
            }
            unsigned proc = car(rev_args);
            unsigned apply_args = cadr(rev_args);
            // Count and push args
            unsigned apply_argc = list_length(apply_args);
            FORLIST(a, apply_args) { vm_push(vm, car(a)); }
            vm_apply(vm, proc, apply_argc, tail);
            return;
        }

        // Regular primitive
        unsigned result = apply_primitive(prim_id, rev_args);
        if (result == TOK_ERROR) {
            vm->error = true;
            vm->error_msg = "primitive error";
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

        // Build argument list from stack (args are on stack, last arg on top)
        unsigned args = 0;
        for (unsigned i = 0; i < argc; i++) {
            args = alloc_cons(vm->stack[vm->sp - argc + i], args);
        }
        // Reverse to get correct order
        unsigned rev_args = 0;
        while (args) {
            rev_args = alloc_cons(car(args), rev_args);
            args = cdr(args);
        }

        vm->sp -= argc;

        // Get params from constant pool and bind
        unsigned params = code->constants[code->params];
        unsigned frame = bind_params(params, rev_args);
        unsigned new_env = alloc_cons(frame, closure_env);

        if (tail && vm->fp > 0) {
            // Tail call: reuse current frame
            vm->code = code;
            vm->ip = 0;
            vm->env = new_env;
        } else {
            // Regular call: push new frame
            push_frame(vm, vm->code, vm->ip, vm->sp, vm->env);

            // Set new execution state
            vm->code = code;
            vm->ip = 0;
            vm->env = new_env;
        }
        return;
    }

    if (IS_FUNCTION(fn)) {
        // Legacy interpreted closure
        unsigned params = car(fn);
        unsigned body_env = cdr(fn);
        unsigned body = car(body_env);
        unsigned def_env = cdr(body_env);

        // Build argument list
        unsigned args = 0;
        for (unsigned i = 0; i < argc; i++) {
            args = alloc_cons(vm->stack[vm->sp - argc + i], args);
        }
        // Reverse
        unsigned rev_args = 0;
        while (args) {
            rev_args = alloc_cons(car(args), rev_args);
            args = cdr(args);
        }

        vm->sp -= argc;

        // Bind parameters
        unsigned frame = bind_params(params, rev_args);
        unsigned new_env = alloc_cons(frame, def_env);

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

    if (IS_CONT(fn)) {
        // Continuation invocation
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

    vm->error = true;
    vm->error_msg = "not a procedure";
    vm->running = false;
}

// ============================================================================
// Main Execution Loop
// ============================================================================

unsigned vm_run(vm_state *vm, code_object *code, unsigned env)
{
    vm->code = code;
    vm->ip = 0;
    vm->env = env;
    vm->sp = 0;
    vm->fp = 0;
    vm->running = true;
    vm->error = false;

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

        switch (op) {
        case OP_CONST: {
            unsigned idx = vm->code->code[vm->ip++];
            vm_push(vm, vm->code->constants[idx]);
            break;
        }

        case OP_POP:
            vm_pop(vm);
            break;

        case OP_DUP:
            vm_push(vm, vm_peek(vm, 0));
            break;

        case OP_LOOKUP: {
            int64_t sym_id = vm->code->code[vm->ip++];
            unsigned val = lookup(sym_id, vm->env);
            if (val == TOK_ERROR) {
                vm->error = true;
                vm->error_msg = "undefined variable";
                vm->running = false;
                break;
            }
            vm_push(vm, val);
            break;
        }

        case OP_DEFINE: {
            int64_t sym_id = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
            // Create atom for variable name
            unsigned atom = alloc();
            CELL_TYPE(atom) = BT_ATOM;
            CELL_ID(atom) = sym_id;
            defvar(atom, val, vm->env);
            break;
        }

        case OP_SET: {
            int64_t sym_id = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
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

        case OP_CLOSURE: {
            unsigned child_idx = vm->code->code[vm->ip++];
            code_object *child = vm->code->children[child_idx];

            // Create code pointer cell (stores code_object* in id field)
            unsigned code_cell = alloc();
            CELL_TYPE(code_cell) = BT_CLOSURE;
            CELL_ID(code_cell) = (int64_t)(intptr_t)child;

            // Create closure cell: car = code_cell, cdr = env
            unsigned closure = alloc_cons(code_cell, vm->env);

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
                pop_frame(vm);
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
            if (val) { // Anything non-nil is truthy
                vm->ip = target;
            }
            break;
        }

        case OP_JUMPIFNOT: {
            unsigned target = vm->code->code[vm->ip++];
            unsigned val = vm_pop(vm);
            if (!val) { // Only nil is falsy
                vm->ip = target;
            }
            break;
        }

        case OP_PRIM: {
            int64_t prim_id = vm->code->code[vm->ip++];
            unsigned argc = vm->code->code[vm->ip++];

            // Build argument list
            unsigned args = 0;
            for (unsigned i = 0; i < argc; i++) {
                unsigned val = vm->stack[vm->sp - argc + i];
                args = alloc_cons(val, args);
            }
            // Reverse
            unsigned rev_args = 0;
            while (args) {
                rev_args = alloc_cons(car(args), rev_args);
                args = cdr(args);
            }

            vm->sp -= argc;

            unsigned result = apply_primitive(prim_id, rev_args);
            if (result == TOK_ERROR) {
                vm->error = true;
                vm->error_msg = "primitive error";
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
            unsigned cont = capture_continuation(vm);
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
            vm->env = alloc_cons(frame, vm->env);
            break;
        }

        case OP_POPENV: {
            // Pop frame from environment
            if (vm->env) {
                vm->env = cdr(vm->env);
            }
            break;
        }

        case OP_VALUES: {
            unsigned n = vm->code->code[vm->ip++];
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

            unsigned literals = cadr(transformer_form);
            unsigned rules = cddr(transformer_form);
            unsigned transformer =
                make_typed_cell(BT_SYNTAX, alloc_cons(literals, rules), vm->env);

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
            if (!IS_PAIR(pair)) {
                vm->error = true;
                vm->error_msg = "car: not a pair";
                vm->running = false;
                break;
            }
            vm_push(vm, car(pair));
            break;
        }

        case OP_CDR: {
            unsigned pair = vm_pop(vm);
            if (!IS_PAIR(pair)) {
                vm->error = true;
                vm->error_msg = "cdr: not a pair";
                vm->running = false;
                break;
            }
            vm_push(vm, cdr(pair));
            break;
        }

        case OP_CONS: {
            unsigned cdr_val = vm_pop(vm);
            unsigned car_val = vm_pop(vm);
            vm_push(vm, alloc_cons(car_val, cdr_val));
            break;
        }

        case OP_NULLP: {
            unsigned val = vm_pop(vm);
            vm_push(vm, val == 0 ? ctx.atom_true : 0);
            break;
        }

        case OP_PAIRP: {
            unsigned val = vm_pop(vm);
            vm_push(vm, (val && CELL_TYPE(val) == BT_CONS) ? ctx.atom_true : 0);
            break;
        }

        case OP_ADD1: {
            unsigned n = vm_pop(vm);
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
            bool is_zero = false;
            if (n == 0) {
                is_zero = true;
            } else if (CELL_TYPE(n) == BT_NUM) {
                is_zero = CELL_ID(n) == 0;
            } else if (CELL_TYPE(n) == BT_BIGNUM) {
                is_zero = bn_is_zero(get_bignum(n));
            }
            vm_push(vm, is_zero ? ctx.atom_true : 0);
            break;
        }

        case OP_NOT: {
            unsigned val = vm_pop(vm);
            vm_push(vm, val == 0 ? ctx.atom_true : 0);
            break;
        }

        case OP_EQ: {
            unsigned b = vm_pop(vm);
            unsigned a = vm_pop(vm);
            vm_push(vm, a == b ? ctx.atom_true : 0);
            break;
        }

        default:
            vm->error = true;
            vm->error_msg = "unknown opcode";
            vm->running = false;
            break;
        }
    }

    if (vm->error) {
        show_error("VM error: %s", vm->error_msg);
        return TOK_ERROR;
    }

    return vm->sp > 0 ? vm->stack[vm->sp - 1] : 0;
}

// ============================================================================
// GC Integration
// ============================================================================

void gc_update_vm_roots(vm_state *vm)
{
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
