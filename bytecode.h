/**
 * @file bytecode.h
 * @brief Bytecode definitions for the compiled Scheme VM
 *
 * This file defines the bytecode instruction set and associated data structures
 * for a stack-based virtual machine. The compiler translates S-expressions into
 * bytecode, which the VM then executes.
 *
 * ## Design Overview
 * - Stack-based execution model
 * - Separate compilation phase produces code_object structures
 * - Closures capture their defining environment
 * - First-class continuations via stack copying
 * - Tail calls optimized via dedicated TAILCALL instruction
 *
 * ## Memory Model
 * Bytecode instructions use cell indices (unsigned) for all Scheme values,
 * integrating with the existing GC infrastructure.
 */

#ifndef BYTECODE_H
#define BYTECODE_H

#include "types.h"

// ============================================================================
// Bytecode Opcodes
// ============================================================================

/**
 * VM instruction opcodes.
 *
 * Instructions are variable-length: opcode byte followed by operands.
 * Operands are unsigned integers (cell indices or counts).
 */
enum opcode {
    // Stack manipulation
    OP_CONST, // Push constant: CONST idx -> push constants[idx]
    OP_POP,   // Discard top of stack
    OP_DUP,   // Duplicate top of stack
    OP_SWAP,  // Swap top two stack elements

    // Variables
    OP_LOOKUP, // Variable lookup: LOOKUP sym_id -> push lookup(sym_id, env)
    OP_DEFINE, // Define variable: DEFINE sym_id -> pop val, defvar(sym_id, val)
    OP_SET,    // Set variable: SET sym_id -> pop val, setvar(sym_id, val)

    // Closures and functions
    OP_CLOSURE,  // Create closure: CLOSURE code_idx -> push closure(code[idx],
                 // env)
    OP_CALL,     // Call function: CALL argc -> pop fn, pop argc args, apply
    OP_TAILCALL, // Tail call: TAILCALL argc -> like CALL but reuse frame
    OP_RETURN,   // Return from function: RETURN -> pop val, restore frame

    // Control flow
    OP_JUMP,      // Unconditional jump: JUMP offset -> ip = offset
    OP_JUMPIF,    // Jump if true: JUMPIF offset -> pop, if truthy: ip = offset
    OP_JUMPIFNOT, // Jump if false: JUMPIFNOT offset -> pop, if falsy: ip =
                  // offset

    // Primitives (inline for common operations)
    OP_PRIM, // Call primitive: PRIM prim_id argc -> apply primitive

    // Special operations
    OP_PUSHCONT, // Capture continuation: push reified continuation
    OP_CALLCC,   // call/cc: CALLCC -> pop proc, call with captured continuation
    OP_APPLY,    // apply primitive: APPLY -> pop proc, pop args-list, apply

    // Environment
    OP_PUSHENV, // Push new empty frame onto environment
    OP_POPENV,  // Pop frame from environment (for let scope exit)

    // Multiple values
    OP_VALUES,         // Wrap N values: VALUES n -> pop n vals, push multival
    OP_CALLWITHVALUES, // call-with-values: pop consumer, pop producer, call

    // Macros (compile-time only, but needed for dynamic define-syntax)
    OP_DEFSYNTAX, // Define syntax: DEFSYNTAX sym_id -> pop transformer

    // Halt
    OP_HALT, // Stop execution: HALT -> vm returns top of stack

    // Specialized opcodes for common operations (fast paths)
    OP_CAR,   // Inline car: pop pair, push car
    OP_CDR,   // Inline cdr: pop pair, push cdr
    OP_CONS,  // Inline cons: pop cdr, pop car, push cons(car, cdr)
    OP_NULLP, // Inline null?: pop val, push (val == nil)
    OP_PAIRP, // Inline pair?: pop val, push (type == BT_CONS)
    OP_ADD1,  // Increment: pop n, push n+1
    OP_SUB1,  // Decrement: pop n, push n-1
    OP_ZEROP, // Zero check: pop n, push (n == 0)
    OP_NOT,   // Boolean not: pop val, push (val == #f)
    OP_EQ,    // Pointer equality: pop b, pop a, push (a eq? b)

    // Specialized arithmetic opcodes (2 operands)
    OP_ADD, // Add: pop b, pop a, push a+b
    OP_SUB, // Subtract: pop b, pop a, push a-b
    OP_MUL, // Multiply: pop b, pop a, push a*b
    OP_DIV, // Divide: pop b, pop a, push a/b
    OP_MOD, // Modulo: pop b, pop a, push a%b

    // Specialized comparison opcodes (2 operands)
    OP_LT,    // Less than: pop b, pop a, push a<b
    OP_GT,    // Greater than: pop b, pop a, push a>b
    OP_LE,    // Less or equal: pop b, pop a, push a<=b
    OP_GE,    // Greater or equal: pop b, pop a, push a>=b
    OP_NUMEQ, // Numeric equal: pop b, pop a, push a=b

    // More list operations
    OP_CADR,   // Inline cadr: pop pair, push cadr
    OP_CDDR,   // Inline cddr: pop pair, push cddr
    OP_SETCAR, // Set car: pop val, pop pair, set-car!
    OP_SETCDR, // Set cdr: pop val, pop pair, set-cdr!
    OP_LIST1,  // Make 1-element list: pop a, push (a)
    OP_LIST2,  // Make 2-element list: pop b, pop a, push (a b)
    OP_LIST3,  // Make 3-element list: pop c, pop b, pop a, push (a b c)

    // Fused compare-and-jump opcodes (common loop patterns)
    OP_JUMPIFNULL,    // Jump if top is null: pop, if null jump to offset
    OP_JUMPIFNOTNULL, // Jump if top is not null: pop, if not null jump
    OP_JUMPIFZERO,    // Jump if top is zero: pop, if zero jump
    OP_JUMPIFNOTZERO, // Jump if top is not zero: pop, if not zero jump

    // Letrec support for proper continuation behavior
    OP_LETREC_MARK, // Mark letrec frame: LETREC_MARK n -> mark env frame as
                    // letrec with n bindings
    OP_LETREC_DONE, // End letrec init: clear letrec mark

    // Additional list accessors
    OP_CAAR,  // Inline caar: pop pair, push caar
    OP_CDAR,  // Inline cdar: pop pair, push cdar
    OP_CADDR, // Inline caddr: pop pair, push caddr
    OP_CDDDR, // Inline cdddr: pop pair, push cdddr

    // Type predicates
    OP_SYMBOLP,  // Symbol predicate: pop val, push (symbol? val)
    OP_NUMBERP,  // Number predicate: pop val, push (number? val)
    OP_STRINGP,  // String predicate: pop val, push (string? val)
    OP_VECTORP,  // Vector predicate: pop val, push (vector? val)
    OP_BOOLEANP, // Boolean predicate: pop val, push (boolean? val)
    OP_LISTP,    // List predicate: pop val, push (list? val)
    OP_INTEGERP, // Integer predicate: pop val, push (integer? val)

    // List operations
    OP_LENGTH,  // List length: pop list, push (length list)
    OP_APPEND,  // Append lists: pop b, pop a, push (append a b)
    OP_REVERSE, // Reverse list: pop list, push (reverse list)
    OP_MEMQ,    // Member by eq?: pop obj, pop list, push (memq obj list)

    // Vector operations
    OP_VECTORREF, // Vector ref: pop idx, pop vec, push (vector-ref vec idx)
    OP_VECTORSET, // Vector set: pop val, pop idx, pop vec, vector-set!
    OP_VECTORLEN, // Vector length: pop vec, push (vector-length vec)

    // Numeric operations
    OP_NEG,     // Unary negation: pop n, push (- n)
    OP_ABS,     // Absolute value: pop n, push (abs n)
    OP_POSITIVE, // Positive check: pop n, push (positive? n)
    OP_NEGATIVE, // Negative check: pop n, push (negative? n)
    OP_EVEN,    // Even check: pop n, push (even? n)
    OP_ODD,     // Odd check: pop n, push (odd? n)
};

// ============================================================================
// Code Object
// ============================================================================

/**
 * Compiled code object.
 *
 * Contains bytecode instructions and a constant pool. Code objects are
 * immutable after compilation. Multiple closures can share the same
 * code object (with different captured environments).
 */
typedef struct code_object {
    unsigned *code;    // Bytecode instruction array
    unsigned code_len; // Number of instructions
    unsigned code_cap; // Allocated capacity

    unsigned *constants; // Constant pool (cell indices)
    unsigned const_len;  // Number of constants
    unsigned const_cap;  // Allocated capacity

    struct code_object **children; // Nested code objects (for lambdas)
    unsigned children_len;
    unsigned children_cap;

    unsigned params;   // Parameter list (cell index to list of symbols)
    unsigned arity;    // Number of required parameters
    bool has_rest;     // True if has rest parameter
    unsigned rest_idx; // Index of rest parameter (if has_rest)

    // Source info for debugging
    const char *name;     // Function name (if known)
    unsigned source_line; // Source line number

    // GC integration: linked list of all code objects
    struct code_object *gc_next;
    bool gc_marked; // True if reachable during current GC
} code_object;

// Global registry of all code objects (for GC)
extern code_object *code_object_registry;

// ============================================================================
// VM Closure
// ============================================================================

/**
 * Runtime closure representation.
 *
 * A closure pairs a code object with a captured environment.
 * Stored as a cons cell: car = BT_CLOSURE marker cell, cdr = env
 * The marker cell's id holds the code_object pointer.
 */
#define BT_CLOSURE 100 // New cell type for VM closures
#define BT_VMCONT 101  // VM continuation (distinct from CPS BT_CONT)

// Accessor macros for closures - code_object* stored in separate cell's id
#define GET_CLOSURE_CODE(c) ((code_object *)(intptr_t)CELL_ID(CELL_CAR(c)))
#define GET_CLOSURE_ENV(c) (CELL_CDR(c))

// ============================================================================
// VM Call Frame
// ============================================================================

/**
 * Call frame for the VM stack.
 *
 * Each function call pushes a frame. Frames track return state.
 */
typedef struct {
    code_object *code; // Code being executed
    unsigned ip;       // Return address (instruction pointer)
    unsigned bp;       // Base pointer (stack position before args)
    unsigned env;      // Environment to restore
} vm_frame;

// ============================================================================
// VM Continuation (for call/cc)
// ============================================================================

/**
 * Captured continuation structure.
 * Stored as a cell with type BT_VMCONT, id = pointer to this struct.
 * The stack and frames arrays contain cell indices that must be updated by GC.
 */
typedef struct {
    unsigned *stack;   // Saved value stack (cell indices)
    unsigned sp;       // Stack pointer
    vm_frame *frames;  // Saved call frames
    unsigned fp;       // Frame pointer
    code_object *code; // Current code object
    unsigned ip;       // Instruction pointer
    unsigned env;      // Current environment (cell index)

    // Letrec support: save/restore binding values for proper continuation behavior
    unsigned *letrec_saved;    // Saved values of letrec bindings (NULL if none)
    unsigned letrec_saved_len; // Number of saved values
    unsigned letrec_frame;     // The frame that was being initialized
} vm_continuation;

// ============================================================================
// VM State
// ============================================================================

/**
 * Virtual machine execution state.
 */
typedef struct {
    // Value stack
    unsigned *stack;    // Value stack (cell indices)
    unsigned sp;        // Stack pointer (next free slot)
    unsigned stack_cap; // Stack capacity

    // Call stack
    vm_frame *frames;    // Call frame stack
    unsigned fp;         // Frame pointer (current frame index)
    unsigned frames_cap; // Frames capacity

    // Current execution state
    code_object *code; // Current code object
    unsigned ip;       // Instruction pointer
    unsigned env;      // Current environment

    // Letrec initialization tracking
    unsigned letrec_frame;   // Frame being initialized (0 if none)
    unsigned letrec_count;   // Number of bindings in letrec

    // Status
    bool running;          // True while VM is executing
    bool error;            // True if error occurred
    const char *error_msg; // Error message (if error)
} vm_state;

// ============================================================================
// Compilation Context
// ============================================================================

/**
 * Compiler state during code generation.
 */
typedef struct compile_ctx {
    code_object *code;          // Code object being built
    struct compile_ctx *parent; // Parent context (for nested lambdas)
    unsigned env;               // Compile-time environment (for macros)
    bool tail_position;         // True if compiling in tail position
    unsigned known_lambdas;     // Alist of (var-id . lambda-expr) for inlining
} compile_ctx;

// ============================================================================
// API Functions
// ============================================================================

// Code object management
code_object *code_new(void);
void code_free(code_object *code);
void code_emit(code_object *code, unsigned instr);
unsigned code_add_const(code_object *code, unsigned val);
unsigned code_add_child(code_object *code, code_object *child);
unsigned code_current_pos(code_object *code);
void code_patch(code_object *code, unsigned pos, unsigned val);

// Compiler
code_object *compile_toplevel(unsigned expr, unsigned env);
code_object *compile_expr(unsigned expr, compile_ctx *ctx);
void compile_sequence(unsigned exprs, compile_ctx *ctx, bool tail);

// VM
void vm_init(vm_state *vm);
void vm_free(vm_state *vm);
unsigned vm_run(vm_state *vm, code_object *code, unsigned env);
unsigned vm_call_closure(unsigned closure, unsigned args); // CPS interop
void vm_push(vm_state *vm, unsigned val);
unsigned vm_pop(vm_state *vm);

// GC integration
unsigned gc_collect_code(code_object *code);
void gc_update_vm_roots(vm_state *vm);
void gc_update_vm_roots_minor(vm_state *vm, unsigned (*collector)(unsigned));
void gc_update_all_code_objects(
    void); // Update all code object constants during GC
void code_register(code_object *code); // Add code object to GC registry
void gc_sweep_code_objects(void);      // Free unreachable code objects after GC
vm_state *get_active_vm(void);         // Get currently running VM (for GC)

// Disassembler
void disassemble(code_object *code, const char *name);

#endif // BYTECODE_H
