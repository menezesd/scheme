/**
 * @file compile.c
 * @brief Scheme to bytecode compiler
 *
 * Compiles Scheme S-expressions into bytecode for the VM.
 *
 * ## Compilation Strategy
 * - Each expression compiles to code that leaves one value on the stack
 * - Tail position tracking enables tail call optimization
 * - Closures compile to nested code objects
 * - Special forms have dedicated compilation routines
 */

#include "bytecode.h"
#include "context.h"
#include "env.h"
#include "eval.h"
#include "macros.h"
#include "writer.h"
#include <stdlib.h>
#include <string.h>

// Forward declaration from eval.c
unsigned qq_expand_cps(unsigned x, unsigned env);

// ============================================================================
// Code Object Management
// ============================================================================

// Global registry of all code objects for GC integration
code_object *code_object_registry = NULL;

// Register a code object with the GC registry
void code_register(code_object *code)
{
    code->gc_next = code_object_registry;
    code_object_registry = code;
}

code_object *code_new(void)
{
    code_object *c = calloc(1, sizeof(code_object));
    c->code_cap = 64;
    c->code = malloc(c->code_cap * sizeof(unsigned));
    c->const_cap = 16;
    c->constants = malloc(c->const_cap * sizeof(unsigned));
    c->children_cap = 4;
    c->children = malloc(c->children_cap * sizeof(code_object *));
    // Register with GC
    code_register(c);
    return c;
}

void code_free(code_object *code)
{
    if (!code)
        return;
    free(code->code);
    free(code->constants);
    for (unsigned i = 0; i < code->children_len; i++) {
        code_free(code->children[i]);
    }
    free(code->children);
    free(code);
}

void code_emit(code_object *code, unsigned instr)
{
    if (code->code_len >= code->code_cap) {
        code->code_cap *= 2;
        code->code = realloc(code->code, code->code_cap * sizeof(unsigned));
    }
    code->code[code->code_len++] = instr;
}

unsigned code_add_const(code_object *code, unsigned val)
{
    // Check if constant already exists
    for (unsigned i = 0; i < code->const_len; i++) {
        if (code->constants[i] == val)
            return i;
    }
    if (code->const_len >= code->const_cap) {
        code->const_cap *= 2;
        code->constants =
            realloc(code->constants, code->const_cap * sizeof(unsigned));
    }
    code->constants[code->const_len] = val;
    return code->const_len++;
}

unsigned code_add_child(code_object *code, code_object *child)
{
    if (code->children_len >= code->children_cap) {
        code->children_cap *= 2;
        code->children = realloc(code->children,
                                 code->children_cap * sizeof(code_object *));
    }
    code->children[code->children_len] = child;
    return code->children_len++;
}

unsigned code_current_pos(code_object *code) { return code->code_len; }

void code_patch(code_object *code, unsigned pos, unsigned val)
{
    code->code[pos] = val;
}

// ============================================================================
// Compiler Forward Declarations
// ============================================================================

static void compile_expr_internal(unsigned expr, compile_ctx *cctx);
static void compile_if(unsigned expr, compile_ctx *cctx);
static void compile_lambda(unsigned expr, compile_ctx *cctx);
static void compile_begin(unsigned exprs, compile_ctx *cctx);
static void compile_let(unsigned expr, compile_ctx *cctx);
static void compile_letstar(unsigned expr, compile_ctx *cctx);
static void compile_letrec(unsigned expr, compile_ctx *cctx);
static void compile_and(unsigned expr, compile_ctx *cctx);
static void compile_or(unsigned expr, compile_ctx *cctx);
static void compile_cond(unsigned expr, compile_ctx *cctx);
static void compile_define(unsigned expr, compile_ctx *cctx);
static void compile_set(unsigned expr, compile_ctx *cctx);
static void compile_call(unsigned expr, compile_ctx *cctx);
static void compile_quasiquote(unsigned expr, compile_ctx *cctx);

// ============================================================================
// Compiler Context
// ============================================================================

static compile_ctx *cctx_new(compile_ctx *parent, unsigned env)
{
    compile_ctx *cctx = calloc(1, sizeof(compile_ctx));
    cctx->code = code_new();
    cctx->parent = parent;
    cctx->env = env;
    cctx->tail_position = false;
    return cctx;
}

static void cctx_free(compile_ctx *cctx)
{
    // Note: code is transferred out, not freed here
    free(cctx);
}

// ============================================================================
// Helper: Emit with operand
// ============================================================================

static void emit(compile_ctx *cctx, unsigned op)
{
    code_emit(cctx->code, op);
}

static void emit2(compile_ctx *cctx, unsigned op, unsigned arg)
{
    code_emit(cctx->code, op);
    code_emit(cctx->code, arg);
}

static void emit3(compile_ctx *cctx, unsigned op, unsigned arg1, unsigned arg2)
{
    code_emit(cctx->code, op);
    code_emit(cctx->code, arg1);
    code_emit(cctx->code, arg2);
}

// Emit a jump instruction, return position to patch
static unsigned emit_jump(compile_ctx *cctx, unsigned op)
{
    code_emit(cctx->code, op);
    unsigned pos = code_current_pos(cctx->code);
    code_emit(cctx->code, 0); // Placeholder
    return pos;
}

static void patch_jump(compile_ctx *cctx, unsigned pos)
{
    code_patch(cctx->code, pos, code_current_pos(cctx->code));
}

// ============================================================================
// Compile Expression
// ============================================================================

static void compile_expr_internal(unsigned expr, compile_ctx *cctx)
{
    if (!expr) {
        // nil
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
        return;
    }

    switch (CELL_TYPE(expr)) {
    case BT_NUM:
    case BT_BIGNUM:
    case BT_INEXACT:
    case BT_RATIONAL:
    case BT_COMPLEX:
    case BT_STRING:
    case BT_CHAR:
    case BT_VECTOR:
        // Self-evaluating: push as constant
        emit2(cctx, OP_CONST, code_add_const(cctx->code, expr));
        return;

    case BT_ATOM: {
        // Variable reference
        emit2(cctx, OP_LOOKUP, CELL_ID(expr));
        return;
    }

    case BT_CONS: {
        unsigned head = car(expr);

        // Check for special forms
        if (IS_ATOM(head)) {
            int64_t kw = CELL_ID(head);

            if (kw == ctx.kw_quote) {
                emit2(cctx, OP_CONST, code_add_const(cctx->code, cadr(expr)));
                return;
            }
            if (kw == ctx.kw_if) {
                compile_if(expr, cctx);
                return;
            }
            if (kw == ctx.kw_lambda) {
                compile_lambda(expr, cctx);
                return;
            }
            if (kw == ctx.kw_begin) {
                compile_begin(cdr(expr), cctx);
                return;
            }
            if (kw == ctx.kw_let) {
                compile_let(expr, cctx);
                return;
            }
            if (kw == ctx.kw_letstar) {
                compile_letstar(expr, cctx);
                return;
            }
            if (kw == ctx.kw_letrec) {
                compile_letrec(expr, cctx);
                return;
            }
            if (kw == ctx.kw_and) {
                compile_and(expr, cctx);
                return;
            }
            if (kw == ctx.kw_or) {
                compile_or(expr, cctx);
                return;
            }
            if (kw == ctx.kw_cond) {
                compile_cond(expr, cctx);
                return;
            }
            if (kw == ctx.kw_define) {
                compile_define(expr, cctx);
                return;
            }
            if (kw == ctx.kw_set) {
                compile_set(expr, cctx);
                return;
            }
            if (kw == ctx.kw_quasiquote) {
                compile_quasiquote(expr, cctx);
                return;
            }
            if (kw == ctx.kw_define_syntax) {
                // (define-syntax name transformer)
                unsigned name = cadr(expr);
                unsigned transformer_form = caddr(expr);
                // For now, store the transformer form as a constant
                // and use OP_DEFSYNTAX
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, transformer_form));
                emit2(cctx, OP_DEFSYNTAX, CELL_ID(name));
                // Return the name
                emit2(cctx, OP_CONST, code_add_const(cctx->code, name));
                return;
            }

            // Check for macro application (silent - don't warn if not found)
            unsigned mac = lookup_silent(kw, cctx->env);
            if (mac != TOK_ERROR) {
                if (IS_SYNTAX(mac)) {
                    // Expand syntax-rules macro and compile result
                    unsigned transformer = car(mac);
                    unsigned expanded =
                        apply_syntax(transformer, expr, cctx->env);
                    if (expanded == TOK_ERROR) {
                        show_error("macro expansion failed");
                        emit(cctx, OP_HALT);
                        return;
                    }
                    compile_expr_internal(expanded, cctx);
                    return;
                }
                if (IS_MACRO(mac)) {
                    // Expand legacy macro and compile result
                    unsigned params = car(mac);
                    unsigned mbody = car(cdr(mac));
                    unsigned menv = cdr(cdr(mac));
                    unsigned frame = bind_params(params, cdr(expr));
                    unsigned new_env = alloc_cons(frame, menv);
                    unsigned expanded = eval_cps(mbody, new_env);
                    if (expanded == TOK_ERROR) {
                        show_error("macro expansion failed");
                        emit(cctx, OP_HALT);
                        return;
                    }
                    compile_expr_internal(expanded, cctx);
                    return;
                }
            }
        }

        // Regular function call
        compile_call(expr, cctx);
        return;
    }

    default:
        // Self-evaluating (ports, functions, etc.)
        emit2(cctx, OP_CONST, code_add_const(cctx->code, expr));
        return;
    }
}

// ============================================================================
// Special Form Compilation
// ============================================================================

// (if test then else)
static void compile_if(unsigned expr, compile_ctx *cctx)
{
    bool tail = cctx->tail_position;

    // Compile test (not in tail position)
    cctx->tail_position = false;
    compile_expr_internal(cadr(expr), cctx);

    // Jump to else if false
    unsigned else_jump = emit_jump(cctx, OP_JUMPIFNOT);

    // Compile then branch
    cctx->tail_position = tail;
    compile_expr_internal(caddr(expr), cctx);

    // Jump over else
    unsigned end_jump = emit_jump(cctx, OP_JUMP);

    // Compile else branch
    patch_jump(cctx, else_jump);
    cctx->tail_position = tail;
    if (cdddr(expr)) {
        compile_expr_internal(cadddr(expr), cctx);
    } else {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0)); // unspecified
    }

    patch_jump(cctx, end_jump);
}

// (lambda params body...)
static void compile_lambda(unsigned expr, compile_ctx *cctx)
{
    unsigned params = cadr(expr);
    unsigned body = cddr(expr);

    // Create new compilation context for lambda body
    compile_ctx *lambda_cctx = cctx_new(cctx, cctx->env);
    lambda_cctx->tail_position = true;

    // Count parameters
    unsigned arity = 0;
    bool has_rest = false;
    for (unsigned p = params; p; p = cdr(p)) {
        if (CELL_TYPE(p) == BT_ATOM) {
            // Rest parameter
            has_rest = true;
            break;
        }
        arity++;
    }

    // Store params in constant pool so they survive GC
    lambda_cctx->code->params = code_add_const(lambda_cctx->code, params);
    lambda_cctx->code->arity = arity;
    lambda_cctx->code->has_rest = has_rest;

    // Compile body
    compile_begin(body, lambda_cctx);
    emit(lambda_cctx, OP_RETURN);

    // Add child code object
    unsigned child_idx = code_add_child(cctx->code, lambda_cctx->code);

    // Emit closure creation in parent
    emit2(cctx, OP_CLOSURE, child_idx);

    cctx_free(lambda_cctx);
}

// Compile sequence of expressions
static void compile_begin(unsigned exprs, compile_ctx *cctx)
{
    if (!exprs) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0)); // unspecified
        return;
    }

    bool tail = cctx->tail_position;

    while (exprs) {
        bool is_last = !cdr(exprs);
        cctx->tail_position = tail && is_last;

        compile_expr_internal(car(exprs), cctx);

        if (!is_last) {
            emit(cctx, OP_POP); // Discard non-final values
        }

        exprs = cdr(exprs);
    }
}

// (let ((var val) ...) body...)
static void compile_let(unsigned expr, compile_ctx *cctx)
{
    unsigned bindings = cadr(expr);
    unsigned body = cddr(expr);

    // Compile all values first (in current environment)
    unsigned count = 0;
    cctx->tail_position = false;
    FORLIST(b, bindings)
    {
        compile_expr_internal(cadr(car(b)), cctx);
        count++;
    }

    // Push new environment frame
    emit(cctx, OP_PUSHENV);

    // Collect variables
    unsigned vars[256];
    unsigned i = 0;
    FORLIST(b, bindings) { vars[i++] = CELL_ID(car(car(b))); }

    // Define in reverse (pop from stack)
    for (int j = count - 1; j >= 0; j--) {
        emit2(cctx, OP_DEFINE, vars[j]);
    }

    // Compile body in tail position
    cctx->tail_position = true;
    compile_begin(body, cctx);

    // Pop environment frame
    emit(cctx, OP_POPENV);
}

// (let* ((var val) ...) body...)
static void compile_letstar(unsigned expr, compile_ctx *cctx)
{
    unsigned bindings = cadr(expr);
    unsigned body = cddr(expr);

    // Push new environment frame
    emit(cctx, OP_PUSHENV);

    // Compile and define each binding sequentially
    cctx->tail_position = false;
    FORLIST(b, bindings)
    {
        unsigned binding = car(b);
        compile_expr_internal(cadr(binding), cctx);
        emit2(cctx, OP_DEFINE, CELL_ID(car(binding)));
    }

    // Compile body in tail position
    cctx->tail_position = true;
    compile_begin(body, cctx);

    // Pop environment frame
    emit(cctx, OP_POPENV);
}

// (letrec ((var val) ...) body...)
static void compile_letrec(unsigned expr, compile_ctx *cctx)
{
    unsigned bindings = cadr(expr);
    unsigned body = cddr(expr);

    // Push new environment frame
    emit(cctx, OP_PUSHENV);

    // First pass: define all variables with undefined values
    FORLIST(b, bindings)
    {
        unsigned binding = car(b);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
        emit2(cctx, OP_DEFINE, CELL_ID(car(binding)));
    }

    // Second pass: compile values and set variables
    cctx->tail_position = false;
    FORLIST(b, bindings)
    {
        unsigned binding = car(b);
        compile_expr_internal(cadr(binding), cctx);
        emit2(cctx, OP_SET, CELL_ID(car(binding)));
        emit(cctx, OP_POP); // Discard set! result
    }

    // Compile body in tail position
    cctx->tail_position = true;
    compile_begin(body, cctx);

    // Pop environment frame
    emit(cctx, OP_POPENV);
}

// (and expr ...)
static void compile_and(unsigned expr, compile_ctx *cctx)
{
    unsigned args = cdr(expr);

    if (!args) {
        // (and) => #t
        emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_true));
        return;
    }

    bool tail = cctx->tail_position;

    // Compile each expression
    unsigned false_jumps[256];
    unsigned jump_count = 0;

    while (args) {
        bool is_last = !cdr(args);
        cctx->tail_position = tail && is_last;

        compile_expr_internal(car(args), cctx);

        if (!is_last) {
            // Duplicate to test and preserve value
            emit(cctx, OP_DUP);
            false_jumps[jump_count++] = emit_jump(cctx, OP_JUMPIFNOT);
            emit(cctx, OP_POP); // Discard if continuing
        }

        args = cdr(args);
    }

    // Patch all false jumps to here
    for (unsigned i = 0; i < jump_count; i++) {
        patch_jump(cctx, false_jumps[i]);
    }
}

// (or expr ...)
static void compile_or(unsigned expr, compile_ctx *cctx)
{
    unsigned args = cdr(expr);

    if (!args) {
        // (or) => #f
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
        return;
    }

    bool tail = cctx->tail_position;

    // Compile each expression
    unsigned true_jumps[256];
    unsigned jump_count = 0;

    while (args) {
        bool is_last = !cdr(args);
        cctx->tail_position = tail && is_last;

        compile_expr_internal(car(args), cctx);

        if (!is_last) {
            // Duplicate to test and preserve value
            emit(cctx, OP_DUP);
            true_jumps[jump_count++] = emit_jump(cctx, OP_JUMPIF);
            emit(cctx, OP_POP); // Discard if continuing
        }

        args = cdr(args);
    }

    // Patch all true jumps to here
    for (unsigned i = 0; i < jump_count; i++) {
        patch_jump(cctx, true_jumps[i]);
    }
}

// (cond (test expr ...) ... (else expr ...))
static void compile_cond(unsigned expr, compile_ctx *cctx)
{
    unsigned clauses = cdr(expr);

    if (!clauses) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0)); // unspecified
        return;
    }

    bool tail = cctx->tail_position;
    unsigned end_jumps[256];
    unsigned end_count = 0;

    while (clauses) {
        unsigned clause = car(clauses);
        unsigned test = car(clause);
        unsigned conseq = cdr(clause);
        bool is_last = !cdr(clauses);

        // Check for else clause
        if (IS_KEYWORD(test, ctx.kw_else)) {
            cctx->tail_position = tail;
            if (conseq) {
                compile_begin(conseq, cctx);
            } else {
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, ctx.atom_true));
            }
            break;
        }

        // Compile test
        cctx->tail_position = false;
        compile_expr_internal(test, cctx);

        unsigned next_clause = emit_jump(cctx, OP_JUMPIFNOT);

        // Compile consequence
        cctx->tail_position = tail;
        if (conseq) {
            compile_begin(conseq, cctx);
        } else {
            // (cond (test) ...) => test value if true
            emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_true));
        }

        if (!is_last) {
            end_jumps[end_count++] = emit_jump(cctx, OP_JUMP);
        }

        patch_jump(cctx, next_clause);
        clauses = cdr(clauses);
    }

    // If no else and no clause matched, result is unspecified
    if (clauses && !cdr(clauses)) {
        // Already handled else
    } else if (!clauses) {
        // No else clause - emit unspecified for fall-through
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
    }

    // Patch end jumps
    for (unsigned i = 0; i < end_count; i++) {
        patch_jump(cctx, end_jumps[i]);
    }
}

// (define var expr) or (define (name params...) body...)
static void compile_define(unsigned expr, compile_ctx *cctx)
{
    unsigned second = cadr(expr);

    if (IS_PAIR(second)) {
        // (define (name params...) body...) => (define name (lambda ...))
        unsigned name = car(second);
        unsigned params = cdr(second);
        unsigned body = cddr(expr);

        // Build lambda expression
        unsigned lambda_expr = alloc_cons(atom_from_string("lambda"),
                                          alloc_cons(params, body));

        cctx->tail_position = false;
        compile_expr_internal(lambda_expr, cctx);
        emit2(cctx, OP_DEFINE, CELL_ID(name));

        // Return the name
        emit2(cctx, OP_CONST, code_add_const(cctx->code, name));
    } else {
        // (define var expr)
        cctx->tail_position = false;
        compile_expr_internal(caddr(expr), cctx);
        emit2(cctx, OP_DEFINE, CELL_ID(second));

        // Return the name
        emit2(cctx, OP_CONST, code_add_const(cctx->code, second));
    }
}

// (set! var expr)
static void compile_set(unsigned expr, compile_ctx *cctx)
{
    unsigned var = cadr(expr);
    unsigned val_expr = caddr(expr);

    cctx->tail_position = false;
    compile_expr_internal(val_expr, cctx);
    emit2(cctx, OP_SET, CELL_ID(var));
}

// Function/primitive call
static void compile_call(unsigned expr, compile_ctx *cctx)
{
    unsigned fn_expr = car(expr);
    unsigned args = cdr(expr);

    // Check for primitive inlining (silent lookup - ok if not found)
    if (IS_ATOM(fn_expr)) {
        unsigned fn = lookup_silent(CELL_ID(fn_expr), cctx->env);
        if (fn != TOK_ERROR && IS_BUILTIN(fn)) {
            int64_t prim_id = CELL_ID(fn);

            // Check for specialized single-argument opcodes
            unsigned argc = list_length(args);

            if (argc == 1) {
                cctx->tail_position = false;
                compile_expr_internal(car(args), cctx);

                switch (prim_id) {
                case PCAR:
                    emit(cctx, OP_CAR);
                    return;
                case PCDR:
                    emit(cctx, OP_CDR);
                    return;
                case PNULLP:
                    emit(cctx, OP_NULLP);
                    return;
                case PCONSP:
                    emit(cctx, OP_PAIRP);
                    return;
                case PNOT:
                    emit(cctx, OP_NOT);
                    return;
                default:
                    break;
                }

                // Not a specialized opcode, use general primitive call
                emit3(cctx, OP_PRIM, prim_id, 1);
                return;
            }

            // Check for specialized two-argument opcodes
            if (argc == 2) {
                if (prim_id == PCONS) {
                    cctx->tail_position = false;
                    compile_expr_internal(car(args), cctx);
                    compile_expr_internal(cadr(args), cctx);
                    emit(cctx, OP_CONS);
                    return;
                }
                if (prim_id == PEQ) {
                    cctx->tail_position = false;
                    compile_expr_internal(car(args), cctx);
                    compile_expr_internal(cadr(args), cctx);
                    emit(cctx, OP_EQ);
                    return;
                }
            }

            // Compile arguments for general case
            cctx->tail_position = false;
            FORLIST(a, args)
            {
                compile_expr_internal(car(a), cctx);
            }

            // Special handling for call/cc
            if (prim_id == PCALLCC) {
                emit(cctx, OP_CALLCC);
                return;
            }

            // Special handling for apply
            if (prim_id == PAPPLY) {
                emit(cctx, OP_APPLY);
                return;
            }

            // Inline primitive call
            emit3(cctx, OP_PRIM, prim_id, argc);
            return;
        }
    }

    // General case: compile function and arguments
    unsigned argc = 0;
    bool tail = cctx->tail_position;

    // Compile arguments first (left to right)
    cctx->tail_position = false;
    FORLIST(a, args)
    {
        compile_expr_internal(car(a), cctx);
        argc++;
    }

    // Compile function expression
    compile_expr_internal(fn_expr, cctx);

    // Emit call
    if (tail) {
        emit2(cctx, OP_TAILCALL, argc);
    } else {
        emit2(cctx, OP_CALL, argc);
    }
}

// Quasiquote expansion (at compile time)
static void compile_quasiquote(unsigned expr, compile_ctx *cctx)
{
    unsigned tmpl = cadr(expr);

    // Simple implementation: expand at compile time
    unsigned expanded = qq_expand_cps(tmpl, cctx->env);
    if (expanded == TOK_ERROR) {
        show_error("quasiquote expansion failed");
        emit(cctx, OP_HALT);
        return;
    }
    emit2(cctx, OP_CONST, code_add_const(cctx->code, expanded));
}

// ============================================================================
// Top-Level Compilation
// ============================================================================

code_object *compile_expr(unsigned expr, compile_ctx *cctx)
{
    compile_expr_internal(expr, cctx);
    return cctx->code;
}

void compile_sequence(unsigned exprs, compile_ctx *cctx, bool tail)
{
    cctx->tail_position = tail;
    compile_begin(exprs, cctx);
}

code_object *compile_toplevel(unsigned expr, unsigned env)
{
    compile_ctx *cctx = cctx_new(NULL, env);
    cctx->tail_position = false;

    compile_expr_internal(expr, cctx);
    emit(cctx, OP_HALT);

    code_object *result = cctx->code;
    cctx_free(cctx);
    return result;
}

// ============================================================================
// GC Integration
// ============================================================================

unsigned gc_collect_code(code_object *code)
{
    if (!code)
        return 0;

    // Collect constants
    for (unsigned i = 0; i < code->const_len; i++) {
        code->constants[i] = collect(code->constants[i]);
    }

    // Recursively collect children
    for (unsigned i = 0; i < code->children_len; i++) {
        gc_collect_code(code->children[i]);
    }

    return 0;
}

// Update all code object constants during GC
// Called from gc() in context.c BEFORE the scan phase
void gc_update_all_code_objects(void)
{
    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        // Collect constants - the scan phase will then process their CAR/CDR
        for (unsigned i = 0; i < code->const_len; i++) {
            code->constants[i] = collect(code->constants[i]);
        }
    }
}

// Mark a code object and all its children as reachable
static void mark_code_object(code_object *code)
{
    if (!code || code->gc_marked)
        return;
    code->gc_marked = true;
    // Mark children recursively
    for (unsigned i = 0; i < code->children_len; i++) {
        mark_code_object(code->children[i]);
    }
}

// Sweep unreachable code objects after GC
// Call this after the heap GC is complete
void gc_sweep_code_objects(void)
{
    // First, clear all marks
    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        code->gc_marked = false;
    }

    // Walk the new heap and mark code objects referenced by closures
    unsigned start = ctx.mmin;
    unsigned end = ctx.hptr;
    for (unsigned i = start; i < end; i++) {
        if (CELL_TYPE(i) == BT_CLOSURE) {
            // Closure car points to marker cell containing code_object*
            code_object *code = GET_CLOSURE_CODE(i);
            mark_code_object(code);
        }
    }

    // Sweep: remove unreachable code objects from registry
    code_object **prev = &code_object_registry;
    while (*prev) {
        code_object *code = *prev;
        if (!code->gc_marked) {
            // Unlink from registry
            *prev = code->gc_next;
            // Free the code object (but not children - they're in registry too)
            free(code->code);
            free(code->constants);
            free(code->children);
            free(code);
        } else {
            prev = &code->gc_next;
        }
    }
}

// ============================================================================
// Bytecode Disassembler
// ============================================================================

static const char *opcode_names[] = {
    [OP_CONST] = "CONST",
    [OP_POP] = "POP",
    [OP_DUP] = "DUP",
    [OP_LOOKUP] = "LOOKUP",
    [OP_DEFINE] = "DEFINE",
    [OP_SET] = "SET",
    [OP_CLOSURE] = "CLOSURE",
    [OP_CALL] = "CALL",
    [OP_TAILCALL] = "TAILCALL",
    [OP_RETURN] = "RETURN",
    [OP_JUMP] = "JUMP",
    [OP_JUMPIF] = "JUMPIF",
    [OP_JUMPIFNOT] = "JUMPIFNOT",
    [OP_PRIM] = "PRIM",
    [OP_PUSHCONT] = "PUSHCONT",
    [OP_CALLCC] = "CALLCC",
    [OP_APPLY] = "APPLY",
    [OP_PUSHENV] = "PUSHENV",
    [OP_POPENV] = "POPENV",
    [OP_VALUES] = "VALUES",
    [OP_CALLWITHVALUES] = "CALLWITHVALUES",
    [OP_DEFSYNTAX] = "DEFSYNTAX",
    [OP_HALT] = "HALT",
    [OP_CAR] = "CAR",
    [OP_CDR] = "CDR",
    [OP_CONS] = "CONS",
    [OP_NULLP] = "NULLP",
    [OP_PAIRP] = "PAIRP",
    [OP_ADD1] = "ADD1",
    [OP_SUB1] = "SUB1",
    [OP_ZEROP] = "ZEROP",
    [OP_NOT] = "NOT",
    [OP_EQ] = "EQ",
};

void disassemble(code_object *code, const char *name)
{
    printf("=== %s ===\n", name ? name : "<anonymous>");
    printf("arity: %u%s\n", code->arity, code->has_rest ? "+" : "");

    // Print constants
    if (code->const_len > 0) {
        printf("constants:\n");
        for (unsigned i = 0; i < code->const_len; i++) {
            printf("  [%u] ", i);
            unsigned c = code->constants[i];
            if (c == 0) {
                printf("nil");
            } else {
                write_obj(c);
            }
            printf("\n");
        }
    }

    // Print code
    printf("code:\n");
    unsigned ip = 0;
    while (ip < code->code_len) {
        printf("  %04u: ", ip);
        unsigned op = code->code[ip++];

        if (op < sizeof(opcode_names) / sizeof(opcode_names[0]) &&
            opcode_names[op]) {
            printf("%-12s", opcode_names[op]);
        } else {
            printf("UNKNOWN(%u)", op);
            printf("\n");
            continue;
        }

        switch (op) {
        case OP_CONST:
        case OP_CLOSURE:
            printf(" %u", code->code[ip++]);
            break;
        case OP_LOOKUP:
        case OP_DEFINE:
        case OP_SET:
        case OP_DEFSYNTAX:
            printf(" %s", ctx.atom_table[code->code[ip++]]);
            break;
        case OP_CALL:
        case OP_TAILCALL:
        case OP_VALUES:
            printf(" %u", code->code[ip++]);
            break;
        case OP_JUMP:
        case OP_JUMPIF:
        case OP_JUMPIFNOT:
            printf(" -> %u", code->code[ip++]);
            break;
        case OP_PRIM:
            printf(" %u argc=%u", code->code[ip], code->code[ip + 1]);
            ip += 2;
            break;
        default:
            // No operands
            break;
        }
        printf("\n");
    }

    // Recursively disassemble children
    for (unsigned i = 0; i < code->children_len; i++) {
        char child_name[64];
        snprintf(child_name, sizeof(child_name), "%s/lambda%u",
                 name ? name : "<anon>", i);
        disassemble(code->children[i], child_name);
    }
}
