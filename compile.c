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
#include "eval_internal.h"
#include "macros.h"
#include "primitives.h"
#include "writer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declaration from eval.c
unsigned qq_expand_cps(unsigned x, unsigned env);

// Forward declaration for helper
static void emit_gensym_definitions(compile_ctx *cctx, unsigned old_gensym,
                                    unsigned new_gensym);

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
        code->children =
            realloc(code->children, code->children_cap * sizeof(code_object *));
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
// Compile Result - tracks constant propagation
// ============================================================================

typedef struct {
    bool is_const;  // true if result is a compile-time constant
    unsigned value; // the constant value (only valid if is_const)
} compile_result;

// Convenience constructors
static inline compile_result const_result(unsigned val)
{
    return (compile_result){true, val};
}

static inline compile_result dynamic_result(void)
{
    return (compile_result){false, 0};
}

// Check if a keyword is shadowed by a local binding
// Returns true if shadowed (caller should treat as procedure call)
static bool is_keyword_shadowed(int64_t kw, unsigned env)
{
    unsigned val = lookup_silent(kw, env);
    // If not found, not shadowed
    if (val == TOK_ERROR)
        return false;
    // If found but it's a syntax transformer, it's a macro override, not
    // shadowing
    if (IS_SYNTAX(val))
        return false;
    // Otherwise, it's shadowed by a regular binding
    return true;
}

// ============================================================================
// Compiler Forward Declarations
// ============================================================================

static compile_result compile_expr_internal(unsigned expr, compile_ctx *cctx);
static compile_result compile_if(unsigned expr, compile_ctx *cctx);
static compile_result compile_lambda(unsigned expr, compile_ctx *cctx);
static compile_result compile_begin(unsigned exprs, compile_ctx *cctx);
static compile_result compile_let(unsigned expr, compile_ctx *cctx);
static compile_result compile_letstar(unsigned expr, compile_ctx *cctx);
static compile_result compile_letrec(unsigned expr, compile_ctx *cctx);
static compile_result compile_and(unsigned expr, compile_ctx *cctx);
static compile_result compile_or(unsigned expr, compile_ctx *cctx);
static compile_result compile_cond(unsigned expr, compile_ctx *cctx);
static compile_result compile_define(unsigned expr, compile_ctx *cctx);
static compile_result compile_set(unsigned expr, compile_ctx *cctx);
static compile_result compile_call(unsigned expr, compile_ctx *cctx);
static compile_result compile_quasiquote(unsigned expr, compile_ctx *cctx);
static void peephole_optimize(code_object *code);

// ============================================================================
// Constant Folding Helpers
// ============================================================================

// Check if a primitive is pure (no side effects, safe to fold)
static bool is_foldable_primitive(int64_t prim_id)
{
    switch (prim_id) {
    // Arithmetic
    case PPLUS:
    case PMINUS:
    case PTIMES:
    case PDIV:
    case PMOD:
    case PQUOTIENT:
    case PREMAINDER:
    case PABS:
    // Comparison
    case PLT:
    case PGT:
    case PLEQ:
    case PGEQ:
    // List operations (read-only ones)
    case PCAR:
    case PCDR:
    case PLENGTH:
    // Note: PCONS, PLIST, PREVERSE, PAPPEND are NOT foldable because
    // they create mutable cons cells that must not be shared
    // Type predicates
    case PCONSP:
    case PNULLP:
    case PNUMBERP:
    case PSTRINGP:
    case PSYMP:
    case PBOOLP:
    case PCHARP:
    case PVECTORP:
    case PNOT:
    case PEQ:
    case PEQUAL:
    case PEQUALP:
    // String operations (pure ones)
    case PSTRLEN:
    case PSTRREF:
    case PSUBSTR:
    case PSTRAPP:
    case PSTR2LIST:
    case PLIST2STR:
    case PSTR2SYM:
    case PSYM2STR:
    // Char operations
    case PCHARCODE:
    case PCODECHAR:
    case PCHARUP:
    case PCHARDOWN:
    // Math functions
    case PFLOOR:
    case PCEILING:
    case PROUND:
    case PTRUNCATE:
    case PEXP:
    case PLOG:
    case PSIN:
    case PCOS:
    case PTAN:
    case PASIN:
    case PACOS:
    case PATAN:
    case PSQRT:
    case PEXPT:
        return true;
    default:
        return false;
    }
}

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
    // Protect env so it's updated if GC runs during compilation
    gc_protect(&cctx->env);
    return cctx;
}

static void cctx_free(compile_ctx *cctx)
{
    // Unprotect env that was protected in cctx_new
    gc_unprotect(1);
    // Note: code is transferred out, not freed here
    free(cctx);
}

// ============================================================================
// Helper: Emit with operand
// ============================================================================

static void emit(compile_ctx *cctx, unsigned op) { code_emit(cctx->code, op); }

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

// Emit bytecode to define gensyms created during macro expansion
// This ensures gensyms from referential transparency are available at runtime
static void emit_gensym_definitions(compile_ctx *cctx, unsigned old_gensym,
                                    unsigned new_gensym)
{
    for (unsigned g = old_gensym; g < new_gensym; g++) {
        char name[20];
        snprintf(name, sizeof(name), "g%u", g);
        unsigned atom = atom_from_string(name);
        int64_t gensym_id = CELL_ID(atom);

        // Look up the gensym value in the compile-time environment
        unsigned val = lookup_silent(gensym_id, cctx->env);
        if (val != TOK_ERROR) {
            // Emit code to define this gensym at runtime
            emit2(cctx, OP_CONST, code_add_const(cctx->code, val));
            emit2(cctx, OP_DEFINE, gensym_id);
        }
    }
}

// ============================================================================
// Compile Expression
// ============================================================================

static compile_result compile_expr_internal(unsigned expr, compile_ctx *cctx)
{
    if (!expr) {
        // nil - constant
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
        return const_result(0);
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
        return const_result(expr);

    case BT_ATOM: {
        // Variable reference - not constant
        emit2(cctx, OP_LOOKUP, CELL_ID(expr));
        return dynamic_result();
    }

    case BT_CONS: {
        unsigned head = car(expr);

        // Check for special forms
        if (IS_ATOM(head)) {
            int64_t kw = CELL_ID(head);

            // quote can be shadowed by local bindings (R5RS allows this)
            if (kw == ctx.kw_quote && !is_keyword_shadowed(kw, cctx->env)) {
                unsigned val = cadr(expr);
                emit2(cctx, OP_CONST, code_add_const(cctx->code, val));
                return const_result(val);
            }
            if (kw == ctx.kw_if)
                return compile_if(expr, cctx);
            if (kw == ctx.kw_lambda)
                return compile_lambda(expr, cctx);
            // begin can be shadowed by local bindings (R5RS allows this)
            if (kw == ctx.kw_begin && !is_keyword_shadowed(kw, cctx->env))
                return compile_begin(cdr(expr), cctx);
            // For let/let*/letrec, check for macro override first
            // (e.g., stdlib defines let macro for named let)
            if (kw == ctx.kw_let || kw == ctx.kw_letstar ||
                kw == ctx.kw_letrec) {
                unsigned mac = lookup_silent(kw, cctx->env);
                if (mac != TOK_ERROR && IS_SYNTAX(mac)) {
                    unsigned old_gensym = gensym_counter;
                    unsigned expanded = apply_syntax(mac, expr, cctx->env);
                    if (expanded == TOK_ERROR) {
                        show_error("let macro expansion failed");
                        emit(cctx, OP_HALT);
                        return dynamic_result();
                    }
                    emit_gensym_definitions(cctx, old_gensym, gensym_counter);
                    return compile_expr_internal(expanded, cctx);
                }
                // No macro override, use built-in compilation
                if (kw == ctx.kw_let)
                    return compile_let(expr, cctx);
                if (kw == ctx.kw_letstar)
                    return compile_letstar(expr, cctx);
                return compile_letrec(expr, cctx);
            }
            if (kw == ctx.kw_and)
                return compile_and(expr, cctx);
            if (kw == ctx.kw_or)
                return compile_or(expr, cctx);
            if (kw == ctx.kw_cond)
                return compile_cond(expr, cctx);
            if (kw == ctx.kw_define)
                return compile_define(expr, cctx);
            if (kw == ctx.kw_set)
                return compile_set(expr, cctx);
            if (kw == ctx.kw_quasiquote)
                return compile_quasiquote(expr, cctx);
            if (kw == ctx.kw_define_syntax) {
                // (define-syntax name transformer)
                unsigned name = cadr(expr);
                unsigned transformer_form = caddr(expr);
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, transformer_form));
                emit2(cctx, OP_DEFSYNTAX, CELL_ID(name));
                emit2(cctx, OP_CONST, code_add_const(cctx->code, name));
                return dynamic_result();
            }

            if (kw == ctx.kw_let_syntax || kw == ctx.kw_letrec_syntax) {
                // (let-syntax ((name transformer) ...) body ...)
                // (letrec-syntax ((name transformer) ...) body ...)
                unsigned bindings = cadr(expr);
                unsigned body = cddr(expr);

                // If no bindings, just compile body in current env
                if (!bindings) {
                    return compile_begin(body, cctx);
                }

                // Create a new compile-time environment
                gc_protect(&bindings);
                gc_protect(&body);

                unsigned new_env = cctx->env;
                gc_protect(&new_env);
                unsigned frame_vars = 0, frame_vals = 0;
                gc_protect(&frame_vars);
                gc_protect(&frame_vals);

                // For let-syntax, closure env is outer env
                // For letrec-syntax, closure env is new env (set after frame
                // created)
                unsigned closure_env = cctx->env;

                // First pass: create frame with placeholder values for
                // letrec-syntax
                if (kw == ctx.kw_letrec_syntax) {
                    unsigned frame = alloc_cons(0, 0);
                    new_env = alloc_cons(frame, cctx->env);
                    closure_env = new_env;
                }

                // Bind each syntax transformer
                for (unsigned b = bindings; b; b = cdr(b)) {
                    unsigned binding = car(b);
                    unsigned name = car(binding);
                    unsigned transformer_form = cadr(binding);

                    // Check it's a syntax-rules form
                    if (!IS_PAIR(transformer_form) ||
                        !IS_KEYWORD(car(transformer_form),
                                    ctx.kw_syntax_rules)) {
                        show_error("%s: expected syntax-rules",
                                   kw == ctx.kw_let_syntax ? "let-syntax"
                                                           : "letrec-syntax");
                        gc_unprotect(5);
                        emit(cctx, OP_HALT);
                        return dynamic_result();
                    }

                    // Create syntax transformer
                    unsigned transformer =
                        make_syntax_transformer(transformer_form, closure_env);

                    // Add to frame
                    gc_protect(&name);
                    gc_protect(&transformer);
                    unsigned vc = alloc_cons(name, frame_vars);
                    unsigned ac = alloc_cons(transformer, frame_vals);
                    gc_unprotect(2);
                    frame_vars = vc;
                    frame_vals = ac;
                }

                // Create frame if let-syntax (letrec-syntax already has frame)
                if (kw == ctx.kw_let_syntax) {
                    unsigned frame = alloc_cons(frame_vars, frame_vals);
                    new_env = alloc_cons(frame, cctx->env);
                } else {
                    // Update letrec-syntax frame
                    unsigned frame = car(new_env);
                    CELL_CAR(frame) = frame_vars;
                    CELL_CDR(frame) = frame_vals;
                }

                gc_unprotect(5);

                // Push a new runtime frame for the body
                // This ensures internal defines create local bindings
                emit(cctx, OP_PUSHENV);

                // Compile body with new compile-time environment
                compile_ctx new_cctx = *cctx;
                new_cctx.env = new_env;
                compile_begin(body, &new_cctx);

                // Pop the runtime frame
                emit(cctx, OP_POPENV);
                return dynamic_result();
            }

            // Check for macro application (silent - don't warn if not found)
            unsigned mac = lookup_silent(kw, cctx->env);
            if (mac != TOK_ERROR) {
                if (IS_SYNTAX(mac)) {
                    unsigned old_gensym = gensym_counter;
                    unsigned expanded = apply_syntax(mac, expr, cctx->env);
                    if (expanded == TOK_ERROR) {
                        show_error("macro expansion failed");
                        emit(cctx, OP_HALT);
                        return dynamic_result();
                    }
                    emit_gensym_definitions(cctx, old_gensym, gensym_counter);
                    return compile_expr_internal(expanded, cctx);
                }
                if (IS_MACRO(mac)) {
                    unsigned params = car(mac);
                    unsigned mbody = car(cdr(mac));
                    unsigned menv = cdr(cdr(mac));
                    unsigned frame = bind_params(params, cdr(expr));
                    unsigned new_env = alloc_cons(frame, menv);
                    unsigned expanded = eval_cps(mbody, new_env);
                    if (expanded == TOK_ERROR) {
                        show_error("macro expansion failed");
                        emit(cctx, OP_HALT);
                        return dynamic_result();
                    }
                    return compile_expr_internal(expanded, cctx);
                }
            }
        }

        // Regular function call
        return compile_call(expr, cctx);
    }

    default:
        // Self-evaluating (ports, functions, etc.)
        emit2(cctx, OP_CONST, code_add_const(cctx->code, expr));
        return const_result(expr);
    }
}

// ============================================================================
// Special Form Compilation
// ============================================================================

// (if test then else)
static compile_result compile_if(unsigned expr, compile_ctx *cctx)
{
    bool tail = cctx->tail_position;
    unsigned saved_pos = cctx->code->code_len;

    // Compile test (not in tail position)
    cctx->tail_position = false;
    compile_result test_result = compile_expr_internal(cadr(expr), cctx);

    // Branch folding: if test is constant, only compile one branch
    if (test_result.is_const) {
        // Rewind - don't need the test bytecode
        cctx->code->code_len = saved_pos;
        cctx->tail_position = tail;

        if (IS_TRUTHY(test_result.value)) {
            // Test is truthy - compile only then branch
            return compile_expr_internal(caddr(expr), cctx);
        } else {
            // Test is false - compile only else branch
            if (cdddr(expr)) {
                return compile_expr_internal(cadddr(expr), cctx);
            } else {
                emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
                return const_result(0);
            }
        }
    }

    // Non-constant test - emit normal conditional
    // Jump to else if false
    unsigned else_jump = emit_jump(cctx, OP_JUMPIFNOT);

    // Compile then branch
    cctx->tail_position = tail;
    compile_result then_result = compile_expr_internal(caddr(expr), cctx);

    // Jump over else
    unsigned end_jump = emit_jump(cctx, OP_JUMP);

    // Compile else branch
    patch_jump(cctx, else_jump);
    cctx->tail_position = tail;
    compile_result else_result;
    if (cdddr(expr)) {
        else_result = compile_expr_internal(cadddr(expr), cctx);
    } else {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
        else_result = const_result(0);
    }

    patch_jump(cctx, end_jump);

    // Result is constant only if both branches are constant with same value
    // (rare case, but handles things like (if x 1 1))
    if (then_result.is_const && else_result.is_const &&
        then_result.value == else_result.value) {
        return then_result;
    }
    return dynamic_result();
}

// (lambda params body...)
static compile_result compile_lambda(unsigned expr, compile_ctx *cctx)
{
    unsigned params = cadr(expr);
    unsigned body = cddr(expr);

    // Create compile-time environment that shadows lambda parameters
    // This prevents the compiler from inlining builtins that are shadowed
    // by lambda parameters (e.g., (lambda (exit) (exit 'x)) should not
    // call the exit primitive)
    unsigned lambda_env = cctx->env;
    gc_protect(&lambda_env);
    gc_protect(&params);
    gc_protect(&body);

    // Build a dummy frame with parameters as vars, all bound to 0
    unsigned vars = 0, vals = 0;
    gc_protect(&vars);
    gc_protect(&vals);
    unsigned vars_tail = 0, vals_tail = 0;
    gc_protect(&vars_tail);
    gc_protect(&vals_tail);

    for (unsigned p = params; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        unsigned var = IS_PAIR(p) ? car(p) : p;
        unsigned vc = alloc_cons(var, 0);
        unsigned ac = alloc_cons(0, 0); // dummy value
        if (!vars) {
            vars = vc;
            vals = ac;
        } else {
            CELL_CDR(vars_tail) = vc;
            CELL_CDR(vals_tail) = ac;
        }
        vars_tail = vc;
        vals_tail = ac;
        if (!IS_PAIR(p))
            break; // rest param
    }

    if (vars) {
        unsigned frame = alloc_cons(vars, vals);
        lambda_env = alloc_cons(frame, lambda_env);
    }

    gc_unprotect(7);

    // Create new compilation context for lambda body with shadowed params
    compile_ctx *lambda_cctx = cctx_new(cctx, lambda_env);
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

    // Lambdas are never compile-time constants (they capture environment)
    return dynamic_result();
}

// Helper to check if an expression is an internal define
// Returns the variable name if it's a define, 0 otherwise
static unsigned is_internal_define(unsigned expr)
{
    if (!IS_PAIR(expr))
        return 0;
    unsigned kw = car(expr);
    if (!IS_KEYWORD(kw, ctx.kw_define))
        return 0;
    unsigned second = cadr(expr);
    if (IS_PAIR(second)) {
        // (define (name params...) body...)
        return car(second);
    } else {
        // (define name expr)
        return second;
    }
}

// Scan body for internal defines and collect variable names
// Internal defines can appear at the start of a body, including inside begin
// Returns a list of variable names that are defined
static unsigned scan_internal_defines(unsigned body, unsigned env)
{
    unsigned names = 0;
    gc_protect(&names);
    gc_protect(&body);

    while (body) {
        unsigned expr = car(body);
        gc_protect(&expr);

        // Check for direct define
        unsigned name = is_internal_define(expr);
        if (name) {
            unsigned cell = alloc_cons(name, names);
            names = cell;
            body = cdr(body);
            gc_unprotect(1);
            continue;
        }

        // Check for begin containing defines (only if begin is not shadowed)
        if (IS_PAIR(expr) && IS_KEYWORD(car(expr), ctx.kw_begin) &&
            !is_keyword_shadowed(ctx.kw_begin, env)) {
            // Scan inside the begin
            unsigned begin_body = cdr(expr);
            while (begin_body) {
                unsigned inner = car(begin_body);
                unsigned inner_name = is_internal_define(inner);
                if (inner_name) {
                    unsigned cell = alloc_cons(inner_name, names);
                    names = cell;
                    begin_body = cdr(begin_body);
                } else {
                    break; // Non-define in begin, stop scanning begin
                }
            }
            // If begin was all defines, continue to next expr
            if (!begin_body) {
                body = cdr(body);
                gc_unprotect(1);
                continue;
            }
        }

        // Not a define or begin-with-defines, stop scanning
        gc_unprotect(1);
        break;
    }

    gc_unprotect(2);
    return names;
}

// Helper to extend compile-time env with a list of variable names
static unsigned extend_compile_env_with_names(unsigned env, unsigned names)
{
    if (!names)
        return env;

    unsigned frame_vars = 0, frame_vals = 0;
    gc_protect(&frame_vars);
    gc_protect(&frame_vals);
    gc_protect(&env);
    gc_protect(&names);

    FORLIST(n, names)
    {
        unsigned var = car(n);
        gc_protect(&var);
        unsigned vc = alloc_cons(var, frame_vars);
        frame_vars = vc;
        unsigned val = alloc_cons(0, frame_vals); // 0 = non-builtin placeholder
        frame_vals = val;
        gc_unprotect(1);
    }

    unsigned frame = alloc_cons(frame_vars, frame_vals);
    unsigned new_env = alloc_cons(frame, env);
    gc_unprotect(4);
    return new_env;
}

// Compile sequence of expressions
static compile_result compile_begin(unsigned exprs, compile_ctx *cctx)
{
    if (!exprs) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0)); // unspecified
        return const_result(0);
    }

    // Scan for internal defines and extend compile-time environment
    // This ensures defines shadow any macros from enclosing scopes
    gc_protect(&exprs);
    unsigned internal_defs = scan_internal_defines(exprs, cctx->env);
    gc_protect(&internal_defs);

    unsigned saved_env = cctx->env;
    if (internal_defs) {
        cctx->env = extend_compile_env_with_names(cctx->env, internal_defs);
    }
    gc_unprotect(2);

    bool tail = cctx->tail_position;
    compile_result result = dynamic_result();

    while (exprs) {
        bool is_last = !cdr(exprs);
        cctx->tail_position = tail && is_last;

        result = compile_expr_internal(car(exprs), cctx);

        if (!is_last) {
            emit(cctx, OP_POP); // Discard non-final values
        }

        exprs = cdr(exprs);
    }

    // Restore compile-time environment
    cctx->env = saved_env;

    return result;
}

// Helper to extend compile-time environment with variable bindings
// This prevents primitive inlining for shadowed variables
static unsigned extend_compile_env(unsigned env, unsigned bindings)
{
    // Build frame with variables -> 0 (non-builtin placeholder)
    unsigned frame_vars = 0, frame_vals = 0;
    gc_protect(&frame_vars);
    gc_protect(&frame_vals);
    gc_protect(&env);
    gc_protect(&bindings);

    FORLIST(b, bindings)
    {
        unsigned var = car(car(b));
        gc_protect(&var);
        unsigned vc = alloc_cons(var, frame_vars);
        frame_vars = vc;
        unsigned val = alloc_cons(0, frame_vals); // 0 = non-builtin
        frame_vals = val;
        gc_unprotect(1);
    }

    unsigned frame = alloc_cons(frame_vars, frame_vals);
    unsigned new_env = alloc_cons(frame, env);
    gc_unprotect(4);
    return new_env;
}

// (let ((var val) ...) body...)
static compile_result compile_let(unsigned expr, compile_ctx *cctx)
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

    // Extend compile-time environment for body compilation
    unsigned saved_env = cctx->env;
    cctx->env = extend_compile_env(cctx->env, bindings);

    // Compile body in tail position
    cctx->tail_position = true;
    compile_result result = compile_begin(body, cctx);

    // Restore compile-time environment
    cctx->env = saved_env;

    // Pop environment frame
    emit(cctx, OP_POPENV);

    // let introduces bindings, so result is dynamic even if body is constant
    (void)result;
    return dynamic_result();
}

// (let* ((var val) ...) body...)
static compile_result compile_letstar(unsigned expr, compile_ctx *cctx)
{
    unsigned bindings = cadr(expr);
    unsigned body = cddr(expr);

    // Push new environment frame
    emit(cctx, OP_PUSHENV);

    // Save original compile-time environment
    unsigned saved_env = cctx->env;

    // Compile and define each binding sequentially
    // Each binding extends the compile-time environment for subsequent bindings
    cctx->tail_position = false;
    FORLIST(b, bindings)
    {
        unsigned binding = car(b);
        compile_expr_internal(cadr(binding), cctx);
        emit2(cctx, OP_DEFINE, CELL_ID(car(binding)));

        // Extend compile-time env with this binding for next iteration
        unsigned single_binding = alloc_cons(binding, 0);
        gc_protect(&single_binding);
        cctx->env = extend_compile_env(cctx->env, single_binding);
        gc_unprotect(1);
    }

    // Compile body in tail position (with all bindings visible)
    cctx->tail_position = true;
    compile_begin(body, cctx);

    // Restore compile-time environment
    cctx->env = saved_env;

    // Pop environment frame
    emit(cctx, OP_POPENV);

    return dynamic_result();
}

// (letrec ((var val) ...) body...)
static compile_result compile_letrec(unsigned expr, compile_ctx *cctx)
{
    unsigned bindings = cadr(expr);
    unsigned body = cddr(expr);

    // Count bindings
    unsigned binding_count = 0;
    FORLIST(b, bindings) { binding_count++; }

    // Push new environment frame
    emit(cctx, OP_PUSHENV);

    // First pass: define all variables with undefined values
    FORLIST(b, bindings)
    {
        unsigned binding = car(b);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
        emit2(cctx, OP_DEFINE, CELL_ID(car(binding)));
    }

    // Mark this frame as letrec for continuation save/restore
    emit2(cctx, OP_LETREC_MARK, binding_count);

    // Extend compile-time environment with all bindings
    // (letrec bindings are mutually recursive, all visible during value compilation)
    unsigned saved_env = cctx->env;
    cctx->env = extend_compile_env(cctx->env, bindings);

    // Second pass: compile values and set variables
    cctx->tail_position = false;
    FORLIST(b, bindings)
    {
        unsigned binding = car(b);
        compile_expr_internal(cadr(binding), cctx);
        emit2(cctx, OP_SET, CELL_ID(car(binding)));
        emit(cctx, OP_POP); // Discard set! result
    }

    // End letrec initialization - continuations after this won't restore values
    emit(cctx, OP_LETREC_DONE);

    // Compile body in tail position
    cctx->tail_position = true;
    compile_begin(body, cctx);

    // Restore compile-time environment
    cctx->env = saved_env;

    // Pop environment frame
    emit(cctx, OP_POPENV);

    return dynamic_result();
}

// (and expr ...)
static compile_result compile_and(unsigned expr, compile_ctx *cctx)
{
    unsigned args = cdr(expr);

    if (!args) {
        // (and) => #t
        emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_true));
        return const_result(ctx.atom_true);
    }

    bool tail = cctx->tail_position;
    unsigned saved_pos = cctx->code->code_len;

    // Compile each expression with short-circuit folding
    unsigned false_jumps[256];
    unsigned jump_count = 0;

    while (args) {
        bool is_last = !cdr(args);
        cctx->tail_position = tail && is_last;

        unsigned expr_saved_pos = cctx->code->code_len;
        compile_result result = compile_expr_internal(car(args), cctx);

        if (result.is_const) {
            if (IS_FALSE(result.value)) {
                // Constant false - short-circuit, rewind all bytecode
                cctx->code->code_len = saved_pos;
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, ctx.atom_false));
                return const_result(ctx.atom_false);
            }
            // Constant truthy - can skip this expression if not last
            if (!is_last) {
                // Rewind bytecode for this constant true, continue with rest
                cctx->code->code_len = expr_saved_pos;
                args = cdr(args);
                continue;
            }
            // Last expression is constant true - return it
        }

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

    return dynamic_result();
}

// (or expr ...)
static compile_result compile_or(unsigned expr, compile_ctx *cctx)
{
    unsigned args = cdr(expr);

    if (!args) {
        // (or) => #f
        emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_false));
        return const_result(ctx.atom_false);
    }

    bool tail = cctx->tail_position;
    unsigned saved_pos = cctx->code->code_len;

    // Compile each expression with short-circuit folding
    unsigned true_jumps[256];
    unsigned jump_count = 0;

    while (args) {
        bool is_last = !cdr(args);
        cctx->tail_position = tail && is_last;

        unsigned expr_saved_pos = cctx->code->code_len;
        compile_result result = compile_expr_internal(car(args), cctx);

        if (result.is_const) {
            if (IS_TRUTHY(result.value)) {
                // Constant truthy - short-circuit, rewind all bytecode
                cctx->code->code_len = saved_pos;
                emit2(cctx, OP_CONST, code_add_const(cctx->code, result.value));
                return const_result(result.value);
            }
            // Constant false - can skip this expression if not last
            if (!is_last) {
                // Rewind bytecode for this constant false, continue with rest
                cctx->code->code_len = expr_saved_pos;
                args = cdr(args);
                continue;
            }
            // Last expression is constant false - return it
        }

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

    return dynamic_result();
}

// (cond (test expr ...) ... (else expr ...))
static compile_result compile_cond(unsigned expr, compile_ctx *cctx)
{
    unsigned clauses = cdr(expr);

    if (!clauses) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0)); // unspecified
        return const_result(0);
    }

    bool tail = cctx->tail_position;
    unsigned end_jumps[256];
    unsigned end_count = 0;
    bool found_else = false;

    while (clauses) {
        unsigned clause = car(clauses);
        unsigned test = car(clause);
        unsigned conseq = cdr(clause);

        // Check for else clause
        if (IS_KEYWORD(test, ctx.kw_else)) {
            cctx->tail_position = tail;
            if (conseq) {
                compile_begin(conseq, cctx);
            } else {
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, ctx.atom_true));
            }
            found_else = true;
            break;
        }

        // Compile test
        cctx->tail_position = false;
        compile_expr_internal(test, cctx);

        // Check if we need the test value (=> syntax or no consequent)
        bool is_arrow = conseq && IS_KEYWORD(car(conseq), ctx.kw_arrow) &&
                        lookup_silent(ctx.kw_arrow, cctx->env) == TOK_ERROR;
        bool need_test_value = is_arrow || !conseq;

        if (need_test_value) {
            // Duplicate test result for => receiver or (test) form
            emit(cctx, OP_DUP);
        }

        unsigned next_clause = emit_jump(cctx, OP_JUMPIFNOT);

        // Compile consequence
        cctx->tail_position = tail;
        if (is_arrow) {
            // (test => receiver) syntax
            // Stack has: test-value (from DUP)
            unsigned receiver_expr = cadr(conseq);
            compile_expr_internal(receiver_expr, cctx);
            // Stack has: test-value receiver
            // CALL pops fn first, then args - already in correct order
            emit2(cctx, OP_CALL, 1);
        } else if (conseq) {
            // Normal consequent - compile expressions
            compile_begin(conseq, cctx);
        }
        // else: no conseq, test value already on stack from DUP

        // Always jump to end after true branch
        end_jumps[end_count++] = emit_jump(cctx, OP_JUMP);

        // Patch false branch entry point
        patch_jump(cctx, next_clause);

        // Pop test value on false branch if we DUP'd
        if (need_test_value) {
            emit(cctx, OP_POP);
        }

        clauses = cdr(clauses);
    }

    // If no else clause was found, emit unspecified for fall-through
    if (!found_else) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
    }

    // Patch all end jumps
    for (unsigned i = 0; i < end_count; i++) {
        patch_jump(cctx, end_jumps[i]);
    }

    return dynamic_result();
}

// (define var expr) or (define (name params...) body...)
static compile_result compile_define(unsigned expr, compile_ctx *cctx)
{
    unsigned second = cadr(expr);

    if (IS_PAIR(second)) {
        // (define (name params...) body...) => (define name (lambda ...))
        unsigned name = car(second);
        unsigned params = cdr(second);
        unsigned body = cddr(expr);

        // Build lambda expression
        unsigned lambda_expr =
            alloc_cons(atom_from_string("lambda"), alloc_cons(params, body));

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

    return dynamic_result();
}

// (set! var expr)
static compile_result compile_set(unsigned expr, compile_ctx *cctx)
{
    unsigned var = cadr(expr);
    unsigned val_expr = caddr(expr);

    cctx->tail_position = false;
    compile_expr_internal(val_expr, cctx);
    emit2(cctx, OP_SET, CELL_ID(var));

    return dynamic_result();
}

// Function/primitive call with full constant folding
static compile_result compile_call(unsigned expr, compile_ctx *cctx)
{
    unsigned fn_expr = car(expr);
    unsigned args = cdr(expr);

    // Check for primitive inlining (silent lookup - ok if not found)
    if (IS_ATOM(fn_expr)) {
        unsigned fn = lookup_silent(CELL_ID(fn_expr), cctx->env);
        if (fn != TOK_ERROR && IS_BUILTIN(fn)) {
            int64_t prim_id = CELL_ID(fn);
            unsigned argc = list_length(args);

            // For foldable primitives, compile with constant tracking
            if (is_foldable_primitive(prim_id)) {
                unsigned saved_pos = cctx->code->code_len;
                cctx->tail_position = false;

                // Compile all arguments, tracking which are constant
                compile_result arg_results[256];
                unsigned i = 0;
                bool all_const = true;

                FORLIST(a, args)
                {
                    arg_results[i] = compile_expr_internal(car(a), cctx);
                    if (!arg_results[i].is_const)
                        all_const = false;
                    i++;
                }

                if (all_const) {
                    // Rewind bytecode - we don't need it
                    cctx->code->code_len = saved_pos;

                    // Build argument list from constant values
                    unsigned arg_vals = 0;
                    gc_protect(&arg_vals);
                    for (unsigned j = 0; j < argc; j++) {
                        arg_vals = alloc_cons(arg_results[argc - 1 - j].value,
                                              arg_vals);
                    }
                    gc_unprotect(1);

                    // Evaluate primitive at compile time
                    unsigned result = apply_primitive(prim_id, arg_vals);
                    if (result != TOK_ERROR) {
                        // Success! Emit as constant
                        emit2(cctx, OP_CONST,
                              code_add_const(cctx->code, result));
                        return const_result(result);
                    }
                    // If folding failed (e.g., division by zero), re-compile
                    // normally
                    FORLIST(a, args) { compile_expr_internal(car(a), cctx); }
                }

                // Arguments already compiled, emit the operation
                // Check for specialized opcodes
                if (argc == 1) {
                    switch (prim_id) {
                    case PCAR:
                        emit(cctx, OP_CAR);
                        return dynamic_result();
                    case PCDR:
                        emit(cctx, OP_CDR);
                        return dynamic_result();
                    case PNULLP:
                        emit(cctx, OP_NULLP);
                        return dynamic_result();
                    case PCONSP:
                        emit(cctx, OP_PAIRP);
                        return dynamic_result();
                    case PNOT:
                        emit(cctx, OP_NOT);
                        return dynamic_result();
                    default:
                        break;
                    }
                } else if (argc == 2) {
                    if (prim_id == PCONS) {
                        emit(cctx, OP_CONS);
                        return dynamic_result();
                    }
                    if (prim_id == PEQ) {
                        emit(cctx, OP_EQ);
                        return dynamic_result();
                    }

                    // Check for unary arithmetic patterns: (+ x 1), (- x 1),
                    // (= x 0)
                    bool arg0_const = arg_results[0].is_const;
                    bool arg1_const = arg_results[1].is_const;
                    int64_t val0 =
                        arg0_const && CELL_TYPE(arg_results[0].value) == BT_NUM
                            ? CELL_ID(arg_results[0].value)
                            : -999;
                    int64_t val1 =
                        arg1_const && CELL_TYPE(arg_results[1].value) == BT_NUM
                            ? CELL_ID(arg_results[1].value)
                            : -999;

                    // (+ x 1) or (+ 1 x) -> ADD1
                    // (+ x -1) or (+ -1 x) -> SUB1
                    if (prim_id == PPLUS) {
                        if (arg1_const && val1 == 1) {
                            cctx->code->code_len = saved_pos;
                            compile_expr_internal(car(args), cctx);
                            emit(cctx, OP_ADD1);
                            return dynamic_result();
                        }
                        if (arg0_const && val0 == 1) {
                            cctx->code->code_len = saved_pos;
                            compile_expr_internal(cadr(args), cctx);
                            emit(cctx, OP_ADD1);
                            return dynamic_result();
                        }
                        if (arg1_const && val1 == -1) {
                            cctx->code->code_len = saved_pos;
                            compile_expr_internal(car(args), cctx);
                            emit(cctx, OP_SUB1);
                            return dynamic_result();
                        }
                        if (arg0_const && val0 == -1) {
                            cctx->code->code_len = saved_pos;
                            compile_expr_internal(cadr(args), cctx);
                            emit(cctx, OP_SUB1);
                            return dynamic_result();
                        }
                    }

                    // (- x 1) -> SUB1
                    // (- x -1) -> ADD1
                    if (prim_id == PMINUS) {
                        if (arg1_const && val1 == 1) {
                            cctx->code->code_len = saved_pos;
                            compile_expr_internal(car(args), cctx);
                            emit(cctx, OP_SUB1);
                            return dynamic_result();
                        }
                        if (arg1_const && val1 == -1) {
                            cctx->code->code_len = saved_pos;
                            compile_expr_internal(car(args), cctx);
                            emit(cctx, OP_ADD1);
                            return dynamic_result();
                        }
                    }

                    // (= x 0) or (= 0 x) -> ZEROP
                    if (prim_id == PEQUAL) {
                        if (arg1_const && val1 == 0) {
                            cctx->code->code_len = saved_pos;
                            compile_expr_internal(car(args), cctx);
                            emit(cctx, OP_ZEROP);
                            return dynamic_result();
                        }
                        if (arg0_const && val0 == 0) {
                            cctx->code->code_len = saved_pos;
                            compile_expr_internal(cadr(args), cctx);
                            emit(cctx, OP_ZEROP);
                            return dynamic_result();
                        }
                    }
                }

                emit3(cctx, OP_PRIM, prim_id, argc);
                return dynamic_result();
            }

            // Non-foldable primitive - compile normally
            cctx->tail_position = false;

            if (argc == 1) {
                compile_expr_internal(car(args), cctx);

                switch (prim_id) {
                case PCAR:
                    emit(cctx, OP_CAR);
                    return dynamic_result();
                case PCDR:
                    emit(cctx, OP_CDR);
                    return dynamic_result();
                case PNULLP:
                    emit(cctx, OP_NULLP);
                    return dynamic_result();
                case PCONSP:
                    emit(cctx, OP_PAIRP);
                    return dynamic_result();
                case PNOT:
                    emit(cctx, OP_NOT);
                    return dynamic_result();
                case PLIST:
                    emit(cctx, OP_LIST1);
                    return dynamic_result();
                default:
                    emit3(cctx, OP_PRIM, prim_id, 1);
                    return dynamic_result();
                }
            }

            if (argc == 2) {
                compile_expr_internal(car(args), cctx);
                compile_expr_internal(cadr(args), cctx);

                switch (prim_id) {
                case PCONS:
                    emit(cctx, OP_CONS);
                    return dynamic_result();
                case PEQ:
                    emit(cctx, OP_EQ);
                    return dynamic_result();
                case PPLUS:
                    emit(cctx, OP_ADD);
                    return dynamic_result();
                case PMINUS:
                    emit(cctx, OP_SUB);
                    return dynamic_result();
                case PTIMES:
                    emit(cctx, OP_MUL);
                    return dynamic_result();
                case PDIV:
                    emit(cctx, OP_DIV);
                    return dynamic_result();
                case PMOD:
                    emit(cctx, OP_MOD);
                    return dynamic_result();
                case PLT:
                    emit(cctx, OP_LT);
                    return dynamic_result();
                case PGT:
                    emit(cctx, OP_GT);
                    return dynamic_result();
                case PLEQ:
                    emit(cctx, OP_LE);
                    return dynamic_result();
                case PGEQ:
                    emit(cctx, OP_GE);
                    return dynamic_result();
                case PEQUAL:
                    emit(cctx, OP_NUMEQ);
                    return dynamic_result();
                case PSETCAR:
                    emit(cctx, OP_SETCAR);
                    return dynamic_result();
                case PSETCDR:
                    emit(cctx, OP_SETCDR);
                    return dynamic_result();
                case PLIST:
                    emit(cctx, OP_LIST2);
                    return dynamic_result();
                default:
                    emit3(cctx, OP_PRIM, prim_id, 2);
                    return dynamic_result();
                }
            }

            if (argc == 3 && prim_id == PLIST) {
                compile_expr_internal(car(args), cctx);
                compile_expr_internal(cadr(args), cctx);
                compile_expr_internal(caddr(args), cctx);
                emit(cctx, OP_LIST3);
                return dynamic_result();
            }

            // Compile arguments for general case
            FORLIST(a, args) { compile_expr_internal(car(a), cctx); }

            // Special handling for call/cc
            if (prim_id == PCALLCC) {
                emit(cctx, OP_CALLCC);
                return dynamic_result();
            }

            // Special handling for apply - use OP_PRIM to handle variable args
            if (prim_id == PAPPLY) {
                emit3(cctx, OP_PRIM, prim_id, argc);
                return dynamic_result();
            }

            emit3(cctx, OP_PRIM, prim_id, argc);
            return dynamic_result();
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

    return dynamic_result();
}

// Check if quasiquote contains unquote or unquote-splicing
static bool qq_has_unquote(unsigned x, int depth)
{
    if (!IS_PAIR(x))
        return false;

    unsigned head = car(x);

    // Found unquote at depth 1
    if (IS_KEYWORD(head, ctx.kw_unquote) && depth == 1)
        return true;
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) && depth == 1)
        return true;

    // Nested quasiquote increases depth
    if (IS_KEYWORD(head, ctx.kw_quasiquote))
        return qq_has_unquote(cadr(x), depth + 1);

    // Unquote decreases depth
    if (IS_KEYWORD(head, ctx.kw_unquote) ||
        IS_KEYWORD(head, ctx.kw_unquote_splicing))
        return qq_has_unquote(cadr(x), depth - 1);

    // Check all elements of list
    for (unsigned l = x; IS_PAIR(l); l = cdr(l)) {
        if (qq_has_unquote(car(l), depth))
            return true;
    }
    return false;
}

// Compile quasiquote recursively - generates runtime code
static void compile_qq_rec(unsigned x, compile_ctx *cctx, int depth)
{
    if (!IS_PAIR(x)) {
        // Non-pair: just quote it
        emit2(cctx, OP_CONST, code_add_const(cctx->code, x));
        return;
    }

    unsigned head = car(x);

    // (unquote expr) at depth 1: compile the expression
    if (IS_KEYWORD(head, ctx.kw_unquote) && depth == 1) {
        compile_expr_internal(cadr(x), cctx);
        return;
    }

    // (unquote expr) at depth > 1: decrement depth and wrap result
    if (IS_KEYWORD(head, ctx.kw_unquote) && depth > 1) {
        compile_qq_rec(cadr(x), cctx, depth - 1);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_unquote));
        emit(cctx, OP_SWAP);
        emit(cctx, OP_LIST1);
        emit(cctx, OP_CONS);
        return;
    }

    // (unquote-splicing expr) at depth > 1: decrement depth and wrap result
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) && depth > 1) {
        compile_qq_rec(cadr(x), cctx, depth - 1);
        emit2(cctx, OP_CONST,
              code_add_const(cctx->code, ctx.atom_unquote_splicing));
        emit(cctx, OP_SWAP);
        emit(cctx, OP_LIST1);
        emit(cctx, OP_CONS);
        return;
    }

    // Nested quasiquote: keep structure but recurse with depth+1
    if (IS_KEYWORD(head, ctx.kw_quasiquote)) {
        compile_qq_rec(cadr(x), cctx, depth + 1);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_quasiquote));
        emit(cctx, OP_SWAP);
        emit(cctx, OP_LIST1);
        emit(cctx, OP_CONS);
        return;
    }

    // Handle list by compiling each element and building with cons
    // Need to handle unquote-splicing specially
    bool has_splice = false;
    for (unsigned l = x; IS_PAIR(l); l = cdr(l)) {
        unsigned elem = car(l);
        if (IS_PAIR(elem) && IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
            depth == 1) {
            has_splice = true;
            break;
        }
    }

    if (has_splice) {
        // Use append-based building for splicing
        // Start with empty list
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));

        for (unsigned l = x; IS_PAIR(l); l = cdr(l)) {
            unsigned elem = car(l);
            if (IS_PAIR(elem) &&
                IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) && depth == 1) {
                // ,@expr: compile expr and append
                compile_expr_internal(cadr(elem), cctx);
                emit3(cctx, OP_PRIM, PAPPEND, 2);
            } else {
                // Regular element: compile, wrap in list, append
                compile_qq_rec(elem, cctx, depth);
                emit(cctx, OP_LIST1);
                emit3(cctx, OP_PRIM, PAPPEND, 2);
            }
        }
    } else {
        // No splicing: build with cons from the end
        // First compile all elements, then cons them together
        unsigned len = 0;
        for (unsigned l = x; IS_PAIR(l); l = cdr(l)) {
            compile_qq_rec(car(l), cctx, depth);
            len++;
        }
        // Handle improper list tail
        unsigned tail = x;
        while (IS_PAIR(tail))
            tail = cdr(tail);
        if (tail) {
            compile_qq_rec(tail, cctx, depth);
        } else {
            emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
        }
        // Now cons them together from right to left
        for (unsigned i = 0; i < len; i++) {
            emit(cctx, OP_CONS);
        }
    }
}

// Quasiquote expansion
static compile_result compile_quasiquote(unsigned expr, compile_ctx *cctx)
{
    unsigned tmpl = cadr(expr);

    // Check if quasiquote has any unquotes that need runtime evaluation
    if (!qq_has_unquote(tmpl, 1)) {
        // No unquotes - can expand at compile time
        unsigned expanded = qq_expand_cps(tmpl, cctx->env);
        if (expanded == TOK_ERROR) {
            show_error("quasiquote expansion failed");
            emit(cctx, OP_HALT);
            return dynamic_result();
        }
        emit2(cctx, OP_CONST, code_add_const(cctx->code, expanded));
        return const_result(expanded);
    }

    // Has unquotes - generate runtime code
    compile_qq_rec(tmpl, cctx, 1);
    return dynamic_result();
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
    peephole_optimize(result);
    cctx_free(cctx);
    return result;
}

// ============================================================================
// Peephole Optimizer
// ============================================================================

// Get the size of an instruction (opcode + operands)
static unsigned opcode_size(unsigned op)
{
    switch (op) {
    case OP_CONST:
    case OP_LOOKUP:
    case OP_DEFINE:
    case OP_SET:
    case OP_CLOSURE:
    case OP_CALL:
    case OP_TAILCALL:
    case OP_JUMP:
    case OP_JUMPIF:
    case OP_JUMPIFNOT:
    case OP_JUMPIFNULL:
    case OP_JUMPIFNOTNULL:
    case OP_JUMPIFZERO:
    case OP_JUMPIFNOTZERO:
    case OP_VALUES:
    case OP_DEFSYNTAX:
    case OP_LETREC_MARK:
        return 2; // opcode + 1 operand
    case OP_PRIM:
        return 3; // opcode + 2 operands
    default:
        return 1; // opcode only
    }
}

// Peephole optimization with proper jump target fixup
static void peephole_optimize(code_object *code)
{
    if (!code || code->code_len < 2)
        return;

    unsigned *c = code->code;
    unsigned len = code->code_len;

    // Collect all jump targets - needed to avoid unsafe fusions
    bool *is_jump_target = calloc(len + 1, sizeof(bool));
    if (!is_jump_target)
        return;
    for (unsigned i = 0; i < len;) {
        unsigned op = c[i];
        unsigned size = opcode_size(op);
        if (op == OP_JUMP || op == OP_JUMPIF || op == OP_JUMPIFNOT ||
            op == OP_JUMPIFNULL || op == OP_JUMPIFNOTNULL ||
            op == OP_JUMPIFZERO || op == OP_JUMPIFNOTZERO) {
            unsigned target = c[i + 1];
            if (target <= len)
                is_jump_target[target] = true;
        }
        i += size;
    }

    // First pass: identify which bytes to remove and build offset map
    // offset_map[old] = new offset after compaction
    unsigned *offset_map = malloc(len * sizeof(unsigned));
    if (!offset_map) {
        free(is_jump_target);
        return;
    }

    bool *remove = calloc(len, sizeof(bool));
    if (!remove) {
        free(offset_map);
        free(is_jump_target);
        return;
    }

    // Mark patterns to remove
    for (unsigned i = 0; i < len;) {
        unsigned op = c[i];
        unsigned size = opcode_size(op);

        // Pattern: CONST x, POP -> nothing (dead code)
        if (op == OP_CONST && i + 2 < len && c[i + 2] == OP_POP) {
            remove[i] = remove[i + 1] = remove[i + 2] = true;
            i += 3;
            continue;
        }

        // Pattern: LOOKUP x, POP -> nothing (dead code)
        if (op == OP_LOOKUP && i + 2 < len && c[i + 2] == OP_POP) {
            remove[i] = remove[i + 1] = remove[i + 2] = true;
            i += 3;
            continue;
        }

        // Pattern: DUP, POP -> nothing
        if (op == OP_DUP && i + 1 < len && c[i + 1] == OP_POP) {
            remove[i] = remove[i + 1] = true;
            i += 2;
            continue;
        }

        // Pattern: SWAP, SWAP -> nothing (identity)
        if (op == OP_SWAP && i + 1 < len && c[i + 1] == OP_SWAP) {
            remove[i] = remove[i + 1] = true;
            i += 2;
            continue;
        }

        // Pattern: NOT, NOT -> nothing (double negation)
        if (op == OP_NOT && i + 1 < len && c[i + 1] == OP_NOT) {
            remove[i] = remove[i + 1] = true;
            i += 2;
            continue;
        }

        // Pattern: NOT, JUMPIFNOT -> JUMPIF (remove NOT, change jump type)
        if (op == OP_NOT && i + 1 < len && c[i + 1] == OP_JUMPIFNOT) {
            remove[i] = true;
            c[i + 1] = OP_JUMPIF;
            i += 1; // Continue from JUMPIF
            continue;
        }

        // Pattern: NOT, JUMPIF -> JUMPIFNOT (remove NOT, change jump type)
        if (op == OP_NOT && i + 1 < len && c[i + 1] == OP_JUMPIF) {
            remove[i] = true;
            c[i + 1] = OP_JUMPIFNOT;
            i += 1;
            continue;
        }

        // Pattern: JUMP to immediately next instruction -> nothing
        if (op == OP_JUMP && i + 2 < len && c[i + 1] == i + 2) {
            remove[i] = remove[i + 1] = true;
            i += 2;
            continue;
        }

        // Pattern: NULLP, JUMPIFNOT target -> JUMPIFNOTNULL target
        // Only safe if no other code jumps to the JUMPIFNOT instruction
        if (op == OP_NULLP && i + 1 < len && c[i + 1] == OP_JUMPIFNOT &&
            !is_jump_target[i + 1]) {
            remove[i] = true;            // Remove NULLP
            c[i + 1] = OP_JUMPIFNOTNULL; // Replace with fused opcode
            i += 1;
            continue;
        }

        // Pattern: NULLP, JUMPIF target -> JUMPIFNULL target
        // Only safe if no other code jumps to the JUMPIF instruction
        if (op == OP_NULLP && i + 1 < len && c[i + 1] == OP_JUMPIF &&
            !is_jump_target[i + 1]) {
            remove[i] = true;
            c[i + 1] = OP_JUMPIFNULL;
            i += 1;
            continue;
        }

        // Pattern: ZEROP, JUMPIFNOT target -> JUMPIFNOTZERO target
        // Only safe if no other code jumps to the JUMPIFNOT instruction
        if (op == OP_ZEROP && i + 1 < len && c[i + 1] == OP_JUMPIFNOT &&
            !is_jump_target[i + 1]) {
            remove[i] = true;
            c[i + 1] = OP_JUMPIFNOTZERO;
            i += 1;
            continue;
        }

        // Pattern: ZEROP, JUMPIF target -> JUMPIFZERO target
        // Only safe if no other code jumps to the JUMPIF instruction
        if (op == OP_ZEROP && i + 1 < len && c[i + 1] == OP_JUMPIF &&
            !is_jump_target[i + 1]) {
            remove[i] = true;
            c[i + 1] = OP_JUMPIFZERO;
            i += 1;
            continue;
        }

        // Pattern: ADD1, SUB1 -> nothing (identity)
        if (op == OP_ADD1 && i + 1 < len && c[i + 1] == OP_SUB1) {
            remove[i] = remove[i + 1] = true;
            i += 2;
            continue;
        }

        // Pattern: SUB1, ADD1 -> nothing (identity)
        if (op == OP_SUB1 && i + 1 < len && c[i + 1] == OP_ADD1) {
            remove[i] = remove[i + 1] = true;
            i += 2;
            continue;
        }

        // Pattern: CAR, CDR can be fused (if we add CADR) - already have
        // OP_CADR Pattern: CDR, CDR can be fused (if we add CDDR) - already
        // have OP_CDDR

        // Pattern: PAIRP, NOT -> use directly in conditional
        if (op == OP_PAIRP && i + 1 < len && c[i + 1] == OP_NOT &&
            i + 2 < len && c[i + 2] == OP_JUMPIFNOT) {
            // PAIRP, NOT, JUMPIFNOT -> PAIRP, JUMPIF
            remove[i + 1] = true;
            c[i + 2] = OP_JUMPIF;
            i += 2;
            continue;
        }

        i += size;
    }

    // Build offset map
    unsigned new_offset = 0;
    for (unsigned i = 0; i < len; i++) {
        offset_map[i] = new_offset;
        if (!remove[i])
            new_offset++;
    }
    // Map for end-of-code (jump targets can point here)
    unsigned final_len = new_offset;

    // Second pass: compact code and fix jump targets
    unsigned write = 0;
    for (unsigned read = 0; read < len;) {
        unsigned op = c[read];
        unsigned size = opcode_size(op);

        if (remove[read]) {
            read += size;
            continue;
        }

        // Copy opcode
        c[write++] = op;

        // Handle operands, fixing jump targets
        if (op == OP_JUMP || op == OP_JUMPIF || op == OP_JUMPIFNOT ||
            op == OP_JUMPIFNULL || op == OP_JUMPIFNOTNULL ||
            op == OP_JUMPIFZERO || op == OP_JUMPIFNOTZERO) {
            unsigned old_target = c[read + 1];
            unsigned new_target =
                (old_target < len) ? offset_map[old_target] : final_len;
            c[write++] = new_target;
            read += 2;
        } else {
            // Copy remaining operands as-is
            for (unsigned j = 1; j < size; j++) {
                c[write++] = c[read + j];
            }
            read += size;
        }
    }

    code->code_len = write;

    free(offset_map);
    free(remove);
    free(is_jump_target);

    // Recursively optimize children
    for (unsigned i = 0; i < code->children_len; i++) {
        peephole_optimize(code->children[i]);
    }
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

    // Walk the heap and mark code objects referenced by closures
    // Scan old generation: [mmin, hptr)
    for (unsigned i = ctx.mmin; i < ctx.hptr; i++) {
        if (CELL_TYPE(i) == BT_CLOSURE) {
            code_object *code = (code_object *)(intptr_t)CELL_ID(i);
            mark_code_object(code);
        }
    }
    // Scan nursery if generational GC is enabled: [nursery_start, nursery_ptr)
    if (ctx.card_table) {
        for (unsigned i = ctx.nursery_start; i < ctx.nursery_ptr; i++) {
            if (CELL_TYPE(i) == BT_CLOSURE) {
                code_object *code = (code_object *)(intptr_t)CELL_ID(i);
                mark_code_object(code);
            }
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
    [OP_SWAP] = "SWAP",
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
    [OP_ADD] = "ADD",
    [OP_SUB] = "SUB",
    [OP_MUL] = "MUL",
    [OP_DIV] = "DIV",
    [OP_MOD] = "MOD",
    [OP_LT] = "LT",
    [OP_GT] = "GT",
    [OP_LE] = "LE",
    [OP_GE] = "GE",
    [OP_NUMEQ] = "NUMEQ",
    [OP_CADR] = "CADR",
    [OP_CDDR] = "CDDR",
    [OP_SETCAR] = "SETCAR",
    [OP_SETCDR] = "SETCDR",
    [OP_LIST1] = "LIST1",
    [OP_LIST2] = "LIST2",
    [OP_LIST3] = "LIST3",
    [OP_JUMPIFNULL] = "JUMPIFNULL",
    [OP_JUMPIFNOTNULL] = "JUMPIFNOTNULL",
    [OP_JUMPIFZERO] = "JUMPIFZERO",
    [OP_JUMPIFNOTZERO] = "JUMPIFNOTZERO",
    [OP_LETREC_MARK] = "LETREC_MARK",
    [OP_LETREC_DONE] = "LETREC_DONE",
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
        case OP_LETREC_MARK:
            printf(" %u", code->code[ip++]);
            break;
        case OP_JUMP:
        case OP_JUMPIF:
        case OP_JUMPIFNOT:
        case OP_JUMPIFNULL:
        case OP_JUMPIFNOTNULL:
        case OP_JUMPIFZERO:
        case OP_JUMPIFNOTZERO:
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
