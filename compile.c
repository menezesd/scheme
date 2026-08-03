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
#include "compile_internal.h"
#include "context.h"
#include "env.h"
#include "eval.h"
#include "eval_internal.h"
#include "macros.h"
#include "prim_internal.h"
#include "primitives.h"
#include "writer.h"
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declaration from eval.c
unsigned qq_expand_cps(unsigned x, unsigned env);

#define MAX_COMPILE_MACRO_EXPANSION_DEPTH 1000

// Forward declaration for helper
static void emit_gensym_definitions(compile_ctx *cctx, unsigned old_gensym,
                                    unsigned new_gensym);
static void emit(compile_ctx *cctx, unsigned op);
static void emit2(compile_ctx *cctx, unsigned op, unsigned arg);
static void emit3(compile_ctx *cctx, unsigned op, unsigned arg1,
                  unsigned arg2);

static int find_stack_local_slot(compile_ctx *cctx, int64_t var_id)
{
    if (cctx->num_locals <= 0)
        return -1;

    for (int i = 0; i < cctx->num_locals; i++) {
        if (cctx->local_ids[i] == var_id)
            return i;
    }

    return -1;
}

static void emit_local_get_slot(compile_ctx *cctx, int slot)
{
    if (slot < 4)
        emit(cctx, OP_LOCAL_GET0 + slot);
    else
        emit2(cctx, OP_LOCAL_GET, (unsigned)slot);
}

static bool builtin_keyword_unbound(int64_t kw, unsigned env)
{
    return lookup_silent(kw, env) == TOK_ERROR;
}

static bool install_compile_time_define_syntax(unsigned expr, compile_ctx *cctx)
{
    if (!IS_PAIR(expr) || !IS_KEYWORD(car(expr), ctx.kw_define_syntax) ||
        lookup_silent(ctx.kw_define_syntax, cctx->env) != TOK_ERROR ||
        !proper_list_silent(cdr(expr)) || !IS_PAIR(cdr(expr)) ||
        !IS_PAIR(cddr(expr)) || cdddr(expr)) {
        return false;
    }

    unsigned name = cadr(expr);
    unsigned transformer_form = caddr(expr);
    int64_t ellipsis_id = syntax_default_ellipsis_id(cctx->env);
    if (!identifier_valid(name) ||
        !syntax_rules_valid(transformer_form, ellipsis_id, "define-syntax")) {
        return false;
    }

    GC_GUARD;
    gc_protect(&name);
    gc_protect(&transformer_form);
    gc_protect(&cctx->env);
    unsigned transformer = make_syntax_transformer_with_default_ellipsis(
        transformer_form, cctx->env, ellipsis_id);
    if (transformer == TOK_ERROR)
        return false;
    gc_protect(&transformer);
    if (defvar(name, transformer, cctx->env) == TOK_ERROR)
        return false;
    return true;
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

static compile_result emit_syntax_error(compile_ctx *cctx, const char *message)
{
    unsigned msg = make_string_copy(message);
    gc_protect(&msg);
    emit2(cctx, OP_CONST, code_add_const(cctx->code, msg));
    emit3(cctx, OP_PRIM, PERROR, 1);
    gc_unprotect(1);
    return dynamic_result();
}

// Look up a variable in the known_lambdas alist
// Returns the lambda expression if found, 0 otherwise
static unsigned lookup_known_lambda(int64_t var_id, unsigned known_lambdas)
{
    FORLIST(entry, known_lambdas) {
        unsigned pair = car(entry);
        if (IS_PAIR(pair) && IS_ATOM(car(pair)) &&
            CELL_ID(car(pair)) == var_id) {
            return cdr(pair); // Return the lambda expression
        }
    }
    return 0;
}

// Forward declaration
static bool contains_reference(unsigned expr, int64_t var_id);

// Check if var_id is captured by an inner closure. Since this runs on
// unexpanded source, we must check let/let*/letrec too (they expand
// to lambdas via macros). This is conservative: a let that doesn't
// actually escape will still disable locals. But it's safe and correct.
static bool captured_by_inner_lambda(unsigned expr, int64_t var_id, unsigned env)
{
    if (!expr || !IS_PAIR(expr))
        return false;
    if (!IS_ATOM(car(expr)))
        return captured_by_inner_lambda(car(expr), var_id, env) ||
               captured_by_inner_lambda(cdr(expr), var_id, env);

    int64_t kw = CELL_ID(car(expr));
    if (kw == ctx.kw_quote && builtin_keyword_unbound(ctx.kw_quote, env))
        return false;
    // A nested lambda captures this variable if its body refers to the
    // outer binding. Check the full lambda form so shadowing parameters are
    // handled by contains_reference.
    if (kw == ctx.kw_lambda && builtin_keyword_unbound(ctx.kw_lambda, env))
        return contains_reference(expr, var_id);
    // let/let*/letrec binding initializers may create closures over this
    // variable, so scan the whole form for nested lambdas. Direct references
    // do not count as captures because atoms return false above.
    if ((kw == ctx.kw_let || kw == ctx.kw_letstar || kw == ctx.kw_letrec) &&
        builtin_keyword_unbound(kw, env))
        return captured_by_inner_lambda(cdr(expr), var_id, env);
    unsigned binding = lookup_silent(kw, env);
    if (binding != TOK_ERROR && IS_SYNTAX(binding))
        return contains_reference(expr, var_id);
    return captured_by_inner_lambda(car(expr), var_id, env) ||
           captured_by_inner_lambda(cdr(expr), var_id, env);
}

// Check if expr contains a reference to var_id (for detecting self-reference)
static bool contains_reference(unsigned expr, int64_t var_id)
{
    if (!expr)
        return false;
    if (IS_ATOM(expr))
        return CELL_ID(expr) == var_id;
    if (!IS_PAIR(expr))
        return false;

    // Check lambda - don't descend if var_id is shadowed by a parameter
    if (IS_ATOM(car(expr)) && CELL_ID(car(expr)) == ctx.kw_lambda) {
        unsigned params = cadr(expr);
        // Check if var_id is in params
        if (IS_ATOM(params) && CELL_ID(params) == var_id)
            return false; // Shadowed by rest param
        if (IS_PAIR(params)) {
            for (unsigned p = params; p; p = IS_PAIR(p) ? cdr(p) : 0) {
                if (IS_ATOM(p) && CELL_ID(p) == var_id)
                    return false; // Shadowed by rest param
                if (IS_PAIR(p) && IS_ATOM(car(p)) && CELL_ID(car(p)) == var_id)
                    return false; // Shadowed by regular param
                if (!IS_PAIR(p))
                    break;
            }
        }
        // Check body
        return contains_reference(cddr(expr), var_id);
    }

    // Recursively check car and cdr
    return contains_reference(car(expr), var_id) ||
           contains_reference(cdr(expr), var_id);
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
static compile_result compile_cond_expand(unsigned expr, compile_ctx *cctx);
static compile_result compile_define(unsigned expr, compile_ctx *cctx);
static compile_result compile_set(unsigned expr, compile_ctx *cctx);
static compile_result compile_call(unsigned expr, compile_ctx *cctx);
static compile_result compile_quasiquote(unsigned expr, compile_ctx *cctx);

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
    case PNUMP:
    case PSTRINGP:
    case PSYMP:
    case PBOOLP:
    case PCHARP:
    case PVECTORP:
    case PNOT:
    case PEQ:
    case PEQV:
    case PEQUAL:
    case PEQUALP:
    // String operations that return immutable/immediate values. Operations
    // that allocate mutable strings or lists are not foldable because a folded
    // constant would be reused across calls to compiled lambdas.
    case PSTRLEN:
    case PSTRREF:
    case PSTR2SYM:
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
    compile_ctx *cctx = checked_calloc_array(1, sizeof(compile_ctx));
    if (!cctx) {
        show_error("compile: out of memory allocating context");
        return NULL;
    }
    cctx->code = code_new();
    if (!cctx->code) {
        free(cctx);
        show_error("compile: out of memory allocating code object");
        return NULL;
    }
    cctx->parent = parent;
    cctx->env = env;
    cctx->tail_position = false;
    cctx->loop_var_id = -1;
    cctx->num_locals = -1;
    // Inherit known_lambdas from parent (they're still in scope)
    cctx->known_lambdas = parent ? parent->known_lambdas : 0;
    cctx->macro_expansion_depth = parent ? parent->macro_expansion_depth : 0;
    // Protect env and known_lambdas so they're updated if GC runs
    gc_protect(&cctx->env);
    gc_protect(&cctx->known_lambdas);
    return cctx;
}

static void cctx_free(compile_ctx *cctx)
{
    // Unprotect env and known_lambdas that were protected in cctx_new
    gc_unprotect(2);
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

static void emit4(compile_ctx *cctx, unsigned op, unsigned arg1, unsigned arg2,
                  unsigned arg3)
{
    code_emit(cctx->code, op);
    code_emit(cctx->code, arg1);
    code_emit(cctx->code, arg2);
    code_emit(cctx->code, arg3);
}

#define IC_UNCACHED 0xFFFFFFFF

// Track env frame depth for loop optimization safety
static void emit_pushenv(compile_ctx *cctx)
{
    code_emit(cctx->code, OP_PUSHENV);
    cctx->env_depth++;
}

static void emit_popenv(compile_ctx *cctx)
{
    code_emit(cctx->code, OP_POPENV);
    if (cctx->env_depth > 0)
        cctx->env_depth--;
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

// Forward declarations for template helpers
static unsigned collect_template_free_vars(unsigned tmpl, unsigned pattern_vars,
                                           unsigned collected, int64_t ellipsis,
                                           unsigned env);
static unsigned rename_template_vars(unsigned tmpl, unsigned rename_map);

static unsigned add_template_bound_var(unsigned pattern_vars, unsigned var)
{
    if (!IS_ATOM(var))
        return pattern_vars;

    GC_GUARD;
    gc_protect(&pattern_vars);
    gc_protect(&var);
    return alloc_cons(var, pattern_vars);
}

static bool template_id_bound(unsigned pattern_vars, int64_t id)
{
    for (unsigned p = pattern_vars; p; p = cdr(p)) {
        if (IS_ATOM(car(p)) && CELL_ID(car(p)) == id)
            return true;
    }
    return false;
}

static bool template_keyword_bound_as_value(int64_t id, unsigned env)
{
    if (!is_special_form(id))
        return false;
    unsigned value = lookup_silent(id, env);
    return value != TOK_ERROR && !IS_SYNTAX(value) && !IS_MACRO(value);
}

static unsigned add_template_lambda_bounds(unsigned pattern_vars,
                                           unsigned params)
{
    GC_GUARD;
    gc_protect(&pattern_vars);
    gc_protect(&params);

    for (unsigned p = params; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        unsigned param = IS_PAIR(p) ? car(p) : p;
        pattern_vars = add_template_bound_var(pattern_vars, param);
        if (!IS_PAIR(p))
            break;
    }

    return pattern_vars;
}

static unsigned add_template_let_bounds(unsigned pattern_vars,
                                        unsigned binding_list)
{
    GC_GUARD;
    gc_protect(&pattern_vars);
    gc_protect(&binding_list);

    for (unsigned bl = binding_list; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (let_binding_has_value(binding))
            pattern_vars = add_template_bound_var(pattern_vars, car(binding));
    }

    return pattern_vars;
}

static unsigned add_template_syntax_bounds(unsigned pattern_vars,
                                           unsigned bindings)
{
    GC_GUARD;
    gc_protect(&pattern_vars);
    gc_protect(&bindings);

    for (unsigned b = bindings; IS_PAIR(b); b = cdr(b)) {
        unsigned binding = car(b);
        if (IS_PAIR(binding))
            pattern_vars = add_template_bound_var(pattern_vars, car(binding));
    }

    return pattern_vars;
}

static unsigned collect_template_free_vars_let(unsigned tmpl,
                                               unsigned pattern_vars,
                                               unsigned collected,
                                               int64_t ellipsis,
                                               bool is_letstar,
                                               bool is_letrec,
                                               unsigned env)
{
    unsigned binding_list = cadr(tmpl);
    unsigned body = cddr(tmpl);

    GC_GUARD;
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&pattern_vars);
    gc_protect(&collected);

    if (!proper_list_silent(binding_list))
        return collect_template_free_vars(body, pattern_vars, collected,
                                          ellipsis, env);

    unsigned body_vars = pattern_vars;
    gc_protect(&body_vars);
    if (is_letrec)
        body_vars = add_template_let_bounds(body_vars, binding_list);

    for (unsigned bl = binding_list; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding))
            continue;

        unsigned val_vars = (is_letstar || is_letrec) ? body_vars : pattern_vars;
        collected =
            collect_template_free_vars(cadr(binding), val_vars, collected,
                                       ellipsis, env);

        if (is_letstar && !is_letrec)
            body_vars = add_template_bound_var(body_vars, car(binding));
    }

    if (!is_letstar && !is_letrec)
        body_vars = add_template_let_bounds(body_vars, binding_list);

    return collect_template_free_vars(body, body_vars, collected, ellipsis,
                                      env);
}

static unsigned collect_template_free_vars_named_let(unsigned tmpl,
                                                     unsigned pattern_vars,
                                                     unsigned collected,
                                                     int64_t ellipsis,
                                                     unsigned env)
{
    unsigned name = cadr(tmpl);
    unsigned binding_list = caddr(tmpl);
    unsigned body = cdddr(tmpl);

    GC_GUARD;
    gc_protect(&name);
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&pattern_vars);
    gc_protect(&collected);

    if (!proper_list_silent(binding_list))
        return collect_template_free_vars(body, pattern_vars, collected,
                                          ellipsis, env);

    for (unsigned bl = binding_list; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding))
            continue;
        collected = collect_template_free_vars(cadr(binding), pattern_vars,
                                               collected, ellipsis, env);
    }

    unsigned body_vars = add_template_bound_var(pattern_vars, name);
    gc_protect(&body_vars);
    body_vars = add_template_let_bounds(body_vars, binding_list);
    return collect_template_free_vars(body, body_vars, collected, ellipsis,
                                      env);
}

static unsigned collect_template_free_vars_quasiquote(unsigned tmpl,
                                                      unsigned pattern_vars,
                                                      unsigned collected,
                                                      int64_t ellipsis,
                                                      unsigned depth,
                                                      unsigned env)
{
    if (!tmpl)
        return collected;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&pattern_vars);
    gc_protect(&collected);

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if ((head_id == ctx.kw_unquote ||
                 head_id == ctx.kw_unquote_splicing) &&
                IS_PAIR(cdr(tmpl))) {
                unsigned body = cadr(tmpl);
                if (depth == 1) {
                    return collect_template_free_vars(
                        body, pattern_vars, collected, ellipsis, env);
                }
                return collect_template_free_vars_quasiquote(
                    body, pattern_vars, collected, ellipsis, depth - 1, env);
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl))) {
                return collect_template_free_vars_quasiquote(
                    cadr(tmpl), pattern_vars, collected, ellipsis, depth + 1,
                    env);
            }
        }

        // A quasiquoted datum can be wide without being deeply nested.
        // Iterate over its ordinary spine and recurse only into elements or
        // the forms whose nesting depth changes quasiquote semantics.
        for (;;) {
            collected = collect_template_free_vars_quasiquote(
                car(tmpl), pattern_vars, collected, ellipsis, depth, env);
            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next))
                return collect_template_free_vars_quasiquote(
                    next, pattern_vars, collected, ellipsis, depth, env);
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_unquote ||
                    next_id == ctx.kw_unquote_splicing ||
                    next_id == ctx.kw_quasiquote)
                    return collect_template_free_vars_quasiquote(
                        next, pattern_vars, collected, ellipsis, depth, env);
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            collected = collect_template_free_vars_quasiquote(
                data[i], pattern_vars, collected, ellipsis, depth, env);
        }
    }

    return collected;
}

// Collect free variables from a syntax-rules template
// pattern_vars: list of pattern variable atoms
// Returns: list of free variable atoms
static unsigned collect_template_free_vars(unsigned tmpl, unsigned pattern_vars,
                                           unsigned collected, int64_t ellipsis,
                                           unsigned env)
{
    if (!tmpl)
        return collected;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&pattern_vars);
    gc_protect(&collected);

    if (IS_ATOM(tmpl)) {
        int64_t id = CELL_ID(tmpl);
        // Skip if pattern variable, ellipsis, or already collected
        if (id == ellipsis)
            return collected;
        for (unsigned p = pattern_vars; p; p = cdr(p)) {
            if (IS_ATOM(car(p)) && CELL_ID(car(p)) == id)
                return collected;
        }
        for (unsigned c = collected; c; c = cdr(c)) {
            if (IS_ATOM(car(c)) && CELL_ID(car(c)) == id)
                return collected;
        }
        // Core keyword names are still collected here. If they are unbound
        // core syntax, the later definition-site lookup will not alias them;
        // if they are lexically rebound, syntax-rules must preserve that
        // definition-site binding.
        return alloc_cons(tmpl, collected);
    }

    if (IS_PAIR(tmpl)) {
        // Skip quote's contents
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quote &&
            !template_id_bound(pattern_vars, ctx.kw_quote) &&
            !template_keyword_bound_as_value(ctx.kw_quote, env))
            return collected;
        if (syntax_rules_form_like(tmpl) &&
            !template_id_bound(pattern_vars, ctx.kw_syntax_rules) &&
            !template_keyword_bound_as_value(ctx.kw_syntax_rules, env))
            return collected;
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quasiquote &&
            !template_id_bound(pattern_vars, ctx.kw_quasiquote) &&
            !template_keyword_bound_as_value(ctx.kw_quasiquote, env) &&
            IS_PAIR(cdr(tmpl))) {
            return collect_template_free_vars_quasiquote(
                cadr(tmpl), pattern_vars, collected, ellipsis, 1, env);
        }
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_lambda &&
            !template_id_bound(pattern_vars, ctx.kw_lambda) &&
            !template_keyword_bound_as_value(ctx.kw_lambda, env) &&
            IS_PAIR(cdr(tmpl)) && IS_PAIR(cddr(tmpl))) {
            unsigned body_vars =
                add_template_lambda_bounds(pattern_vars, cadr(tmpl));
            gc_protect(&body_vars);
            return collect_template_free_vars(cddr(tmpl), body_vars,
                                              collected, ellipsis, env);
        }
        if (IS_ATOM(car(tmpl)) &&
            (CELL_ID(car(tmpl)) == ctx.kw_let ||
             CELL_ID(car(tmpl)) == ctx.kw_letstar ||
             CELL_ID(car(tmpl)) == ctx.kw_letrec) &&
            !template_id_bound(pattern_vars, CELL_ID(car(tmpl))) &&
            !template_keyword_bound_as_value(CELL_ID(car(tmpl)), env) &&
            IS_PAIR(cdr(tmpl)) && IS_PAIR(cddr(tmpl))) {
            int64_t head_id = CELL_ID(car(tmpl));
            if (head_id == ctx.kw_let && IS_ATOM(cadr(tmpl))) {
                return collect_template_free_vars_named_let(
                    tmpl, pattern_vars, collected, ellipsis, env);
            }
            return collect_template_free_vars_let(
                tmpl, pattern_vars, collected, ellipsis,
                head_id == ctx.kw_letstar, head_id == ctx.kw_letrec, env);
        }
        // Templates may contain very wide flat forms.  Their cdr spine is
        // data, not structural nesting, so do not consume a C stack frame for
        // each element while collecting definition-site bindings.
        for (;;) {
            collected = collect_template_free_vars(
                car(tmpl), pattern_vars, collected, ellipsis, env);
            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next))
                return collect_template_free_vars(
                    next, pattern_vars, collected, ellipsis, env);
            if (IS_ATOM(car(next)) &&
                is_special_form(CELL_ID(car(next))))
                return collect_template_free_vars(
                    next, pattern_vars, collected, ellipsis, env);
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointer each iteration - GC may have moved tmpl
            unsigned *data = vector_data_ptr(tmpl);
            collected = collect_template_free_vars(data[i],
                                                   pattern_vars, collected,
                                                   ellipsis, env);
        }
        return collected;
    }

    return collected;
}

// Rename free variables in template according to rename_map
// rename_map: list of (orig_atom . gensym_atom)
static unsigned rename_template_vars_quasiquote(unsigned tmpl,
                                                unsigned rename_map,
                                                unsigned depth)
{
    if (!tmpl || !rename_map)
        return tmpl;

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if ((head_id == ctx.kw_unquote ||
                 head_id == ctx.kw_unquote_splicing) &&
                IS_PAIR(cdr(tmpl))) {
                unsigned body = cadr(tmpl);
                unsigned new_body;
                if (depth == 1)
                    new_body = rename_template_vars(body, rename_map);
                else
                    new_body = rename_template_vars_quasiquote(
                        body, rename_map, depth - 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                unsigned result = alloc_cons(head, result_tail);
                gc_unprotect(2);
                return result;
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl))) {
                unsigned new_body = rename_template_vars_quasiquote(
                    cadr(tmpl), rename_map, depth + 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                unsigned result = alloc_cons(head, result_tail);
                gc_unprotect(2);
                return result;
            }
        }

        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car =
                rename_template_vars_quasiquote(old_car, rename_map, depth);
            if (new_car == TOK_ERROR) {
                gc_unprotect(5);
                return TOK_ERROR;
            }
            if (new_car != old_car)
                changed = true;
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                unsigned new_tail =
                    rename_template_vars_quasiquote(next, rename_map, depth);
                if (new_tail == TOK_ERROR) {
                    gc_unprotect(5);
                    return TOK_ERROR;
                }
                if (new_tail != next)
                    changed = true;
                if (!changed) {
                    gc_unprotect(5);
                    return original;
                }
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                gc_unprotect(5);
                return result;
            }
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_unquote ||
                    next_id == ctx.kw_unquote_splicing ||
                    next_id == ctx.kw_quasiquote) {
                    unsigned new_tail = rename_template_vars_quasiquote(
                        next, rename_map, depth);
                    if (new_tail == TOK_ERROR) {
                        gc_unprotect(5);
                        return TOK_ERROR;
                    }
                    if (new_tail != next)
                        changed = true;
                    if (!changed) {
                        gc_unprotect(5);
                        return original;
                    }
                    cell_set_cdr(result_tail, new_tail);
                    gc_unprotect(5);
                    return result;
                }
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR) {
            gc_unprotect(2);
            return TOK_ERROR;
        }
        gc_protect(&new_vec);
        bool changed = false;
        for (unsigned i = 0; i < len; i++) {
            unsigned old_elem = vector_data_ptr(tmpl)[i];
            gc_protect(&old_elem);
            unsigned new_elem =
                rename_template_vars_quasiquote(old_elem, rename_map, depth);
            if (new_elem == TOK_ERROR) {
                gc_unprotect(4);
                return TOK_ERROR;
            }
            vector_set_elem(new_vec, i, new_elem);
            if (new_elem != old_elem)
                changed = true;
            gc_unprotect(1);
        }
        gc_unprotect(3);
        return changed ? new_vec : tmpl;
    }

    return tmpl;
}

static unsigned rename_map_without_id(unsigned rename_map, int64_t id)
{
    if (!rename_map)
        return 0;

    GC_GUARD;
    gc_protect(&rename_map);

    unsigned filtered = 0;
    unsigned tail = 0;
    gc_protect(&filtered);
    gc_protect(&tail);
    bool changed = false;

    for (unsigned m = rename_map; m; m = cdr(m)) {
        unsigned entry = car(m);
        if (IS_ATOM(car(entry)) && CELL_ID(car(entry)) == id) {
            changed = true;
            continue;
        }
        list_append(&filtered, &tail, entry);
    }

    return changed ? filtered : rename_map;
}

static unsigned rename_map_without_atom(unsigned rename_map, unsigned var)
{
    if (!IS_ATOM(var))
        return rename_map;
    return rename_map_without_id(rename_map, CELL_ID(var));
}

static unsigned rename_map_without_lambda_bounds(unsigned rename_map,
                                                 unsigned params)
{
    GC_GUARD;
    gc_protect(&rename_map);
    gc_protect(&params);

    for (unsigned p = params; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        unsigned param = IS_PAIR(p) ? car(p) : p;
        rename_map = rename_map_without_atom(rename_map, param);
        if (!IS_PAIR(p))
            break;
    }

    return rename_map;
}

static unsigned rename_map_without_let_bounds(unsigned rename_map,
                                              unsigned binding_list)
{
    GC_GUARD;
    gc_protect(&rename_map);
    gc_protect(&binding_list);

    for (unsigned bl = binding_list; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (let_binding_has_value(binding))
            rename_map = rename_map_without_atom(rename_map, car(binding));
    }

    return rename_map;
}

static unsigned rename_template_vars_shadowed_heads(unsigned tmpl,
                                                    unsigned rename_map,
                                                    bool quote_shadowed,
                                                    bool quasiquote_shadowed);

static unsigned rename_template_vars_lambda(unsigned tmpl, unsigned rename_map)
{
    unsigned keyword = car(tmpl);
    unsigned params = cadr(tmpl);
    unsigned body = cddr(tmpl);

    GC_GUARD;
    gc_protect(&keyword);
    gc_protect(&params);
    gc_protect(&body);
    gc_protect(&rename_map);

    unsigned body_map = rename_map_without_lambda_bounds(rename_map, params);
    gc_protect(&body_map);
    unsigned new_body = rename_template_vars(body, body_map);
    if (new_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&new_body);

    if (new_body == body)
        return tmpl;
    unsigned result_tail = alloc_cons(params, new_body);
    gc_protect(&result_tail);
    return alloc_cons(keyword, result_tail);
}

static unsigned rename_template_vars_let(unsigned tmpl, unsigned rename_map,
                                         bool is_letstar, bool is_letrec)
{
    unsigned keyword = car(tmpl);
    unsigned binding_list = cadr(tmpl);
    unsigned body = cddr(tmpl);

    GC_GUARD;
    gc_protect(&keyword);
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&rename_map);

    if (!proper_list_silent(binding_list))
        return tmpl;

    unsigned body_map = rename_map;
    gc_protect(&body_map);
    if (is_letrec)
        body_map = rename_map_without_let_bounds(body_map, binding_list);

    unsigned new_bindings = 0;
    unsigned new_tail = 0;
    gc_protect(&new_bindings);
    gc_protect(&new_tail);
    bool changed = false;

    for (unsigned bl = binding_list; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding)) {
            list_append(&new_bindings, &new_tail, binding);
            continue;
        }

        unsigned var = car(binding);
        unsigned val = cadr(binding);
        unsigned val_map = (is_letstar || is_letrec) ? body_map : rename_map;
        gc_protect(&var);
        gc_protect(&val);
        gc_protect(&val_map);

        unsigned new_val = rename_template_vars(val, val_map);
        if (new_val == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_val);
        unsigned bind_tail = alloc_cons(new_val, 0);
        gc_protect(&bind_tail);
        unsigned new_binding = alloc_cons(var, bind_tail);
        gc_protect(&new_binding);
        list_append(&new_bindings, &new_tail, new_binding);
        if (new_val != val)
            changed = true;
        gc_unprotect(6);

        if (is_letstar && !is_letrec)
            body_map = rename_map_without_atom(body_map, var);
    }

    if (!is_letstar && !is_letrec)
        body_map = rename_map_without_let_bounds(body_map, binding_list);

    unsigned new_body;
    bool quote_shadowed = binding_list_binds_id(binding_list, ctx.kw_quote);
    bool quasiquote_shadowed =
        binding_list_binds_id(binding_list, ctx.kw_quasiquote);
    if (quote_shadowed || quasiquote_shadowed) {
        new_body = rename_template_vars_shadowed_heads(
            body, body_map, quote_shadowed, quasiquote_shadowed);
    }
    else
        new_body = rename_template_vars(body, body_map);
    if (new_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&new_body);
    if (new_body != body)
        changed = true;

    if (!changed)
        return tmpl;
    unsigned result_tail = alloc_cons(new_bindings, new_body);
    gc_protect(&result_tail);
    return alloc_cons(keyword, result_tail);
}

static unsigned rename_template_vars_named_let(unsigned tmpl,
                                               unsigned rename_map)
{
    unsigned keyword = car(tmpl);
    unsigned name = cadr(tmpl);
    unsigned binding_list = caddr(tmpl);
    unsigned body = cdddr(tmpl);

    GC_GUARD;
    gc_protect(&keyword);
    gc_protect(&name);
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&rename_map);

    if (!proper_list_silent(binding_list))
        return tmpl;

    unsigned new_bindings = 0;
    unsigned new_tail = 0;
    gc_protect(&new_bindings);
    gc_protect(&new_tail);
    bool changed = false;

    for (unsigned bl = binding_list; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding)) {
            list_append(&new_bindings, &new_tail, binding);
            continue;
        }

        unsigned var = car(binding);
        unsigned val = cadr(binding);
        gc_protect(&var);
        gc_protect(&val);
        unsigned new_val = rename_template_vars(val, rename_map);
        if (new_val == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_val);
        unsigned bind_tail = alloc_cons(new_val, 0);
        gc_protect(&bind_tail);
        unsigned new_binding = alloc_cons(var, bind_tail);
        gc_protect(&new_binding);
        list_append(&new_bindings, &new_tail, new_binding);
        if (new_val != val)
            changed = true;
        gc_unprotect(5);
    }

    unsigned body_map = rename_map_without_atom(rename_map, name);
    gc_protect(&body_map);
    body_map = rename_map_without_let_bounds(body_map, binding_list);

    unsigned new_body = rename_template_vars(body, body_map);
    if (new_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&new_body);
    if (new_body != body)
        changed = true;

    if (!changed)
        return tmpl;
    unsigned body_tail = alloc_cons(new_bindings, new_body);
    gc_protect(&body_tail);
    unsigned name_tail = alloc_cons(name, body_tail);
    gc_protect(&name_tail);
    return alloc_cons(keyword, name_tail);
}

static unsigned rename_template_vars(unsigned tmpl, unsigned rename_map)
{
    if (!tmpl || !rename_map)
        return tmpl;

    if (IS_ATOM(tmpl)) {
        int64_t id = CELL_ID(tmpl);
        for (unsigned m = rename_map; m; m = cdr(m)) {
            unsigned entry = car(m);
            if (IS_ATOM(car(entry)) && CELL_ID(car(entry)) == id)
                return cdr(entry);
        }
        return tmpl;
    }

    if (IS_PAIR(tmpl)) {
        bool head_is_alias = false;
        if (IS_ATOM(car(tmpl)))
            head_is_alias =
                assoc_list_has_atom_key_id(rename_map, CELL_ID(car(tmpl)));
        // Skip quote's contents
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quote &&
            !head_is_alias)
            return tmpl;
        if (syntax_rules_form_like(tmpl) && !head_is_alias)
            return tmpl;
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quasiquote &&
            IS_PAIR(cdr(tmpl)) && !head_is_alias) {
            unsigned new_body =
                rename_template_vars_quasiquote(cadr(tmpl), rename_map, 1);
            if (new_body == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&new_body);
            unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
            gc_protect(&result_tail);
            unsigned result = alloc_cons(car(tmpl), result_tail);
            gc_unprotect(2);
            return result;
        }
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_lambda &&
            IS_PAIR(cdr(tmpl)) && IS_PAIR(cddr(tmpl)) && !head_is_alias) {
            return rename_template_vars_lambda(tmpl, rename_map);
        }
        if (IS_ATOM(car(tmpl)) &&
            (CELL_ID(car(tmpl)) == ctx.kw_let ||
             CELL_ID(car(tmpl)) == ctx.kw_letstar ||
             CELL_ID(car(tmpl)) == ctx.kw_letrec) &&
            IS_PAIR(cdr(tmpl)) && IS_PAIR(cddr(tmpl)) && !head_is_alias) {
            int64_t head_id = CELL_ID(car(tmpl));
            if (head_id == ctx.kw_let && IS_ATOM(cadr(tmpl))) {
                return rename_template_vars_named_let(tmpl, rename_map);
            }
            return rename_template_vars_let(tmpl, rename_map,
                                            head_id == ctx.kw_letstar,
                                            head_id == ctx.kw_letrec);
        }
        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car = rename_template_vars(old_car, rename_map);
            if (new_car == TOK_ERROR) {
                gc_unprotect(5);
                return TOK_ERROR;
            }
            if (new_car != old_car)
                changed = true;
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                unsigned new_tail = rename_template_vars(next, rename_map);
                if (new_tail == TOK_ERROR) {
                    gc_unprotect(5);
                    return TOK_ERROR;
                }
                if (new_tail != next)
                    changed = true;
                if (!changed) {
                    gc_unprotect(5);
                    return original;
                }
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                gc_unprotect(5);
                return result;
            }
            if (IS_ATOM(car(next)) && is_special_form(CELL_ID(car(next)))) {
                unsigned new_tail = rename_template_vars(next, rename_map);
                if (new_tail == TOK_ERROR) {
                    gc_unprotect(5);
                    return TOK_ERROR;
                }
                if (new_tail != next)
                    changed = true;
                if (!changed) {
                    gc_unprotect(5);
                    return original;
                }
                cell_set_cdr(result_tail, new_tail);
                gc_unprotect(5);
                return result;
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR) {
            gc_unprotect(2);
            return TOK_ERROR;
        }
        gc_protect(&new_vec);
        bool changed = false;
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointers each iteration - GC may have moved vectors
            unsigned old_elem = vector_data_ptr(tmpl)[i];
            gc_protect(&old_elem);
            unsigned new_elem = rename_template_vars(old_elem, rename_map);
            if (new_elem == TOK_ERROR) {
                gc_unprotect(4);
                return TOK_ERROR;
            }
            vector_set_elem(new_vec, i, new_elem);
            if (new_elem != old_elem)
                changed = true;
            gc_unprotect(1);
        }
        gc_unprotect(3);
        return changed ? new_vec : tmpl;
    }

    return tmpl;
}

static unsigned rename_template_vars_shadowed_heads(unsigned tmpl,
                                                    unsigned rename_map,
                                                    bool quote_shadowed,
                                                    bool quasiquote_shadowed)
{
    if (!tmpl || !rename_map)
        return tmpl;

    if (IS_ATOM(tmpl)) {
        int64_t id = CELL_ID(tmpl);
        for (unsigned m = rename_map; m; m = cdr(m)) {
            unsigned entry = car(m);
            if (IS_ATOM(car(entry)) && CELL_ID(car(entry)) == id)
                return cdr(entry);
        }
        return tmpl;
    }

    if (IS_PAIR(tmpl)) {
        bool head_is_alias = false;
        if (IS_ATOM(car(tmpl)))
            head_is_alias =
                assoc_list_has_atom_key_id(rename_map, CELL_ID(car(tmpl)));
        if (syntax_rules_form_like(tmpl) && !head_is_alias)
            return tmpl;
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quote &&
            !quote_shadowed && !head_is_alias)
            return tmpl;
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quasiquote &&
            IS_PAIR(cdr(tmpl)) && !quasiquote_shadowed && !head_is_alias) {
            unsigned new_body =
                rename_template_vars_quasiquote(cadr(tmpl), rename_map, 1);
            if (new_body == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&new_body);
            unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
            gc_protect(&result_tail);
            unsigned result = alloc_cons(car(tmpl), result_tail);
            gc_unprotect(2);
            return result;
        }

        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car = rename_template_vars_shadowed_heads(
                old_car, rename_map, quote_shadowed, quasiquote_shadowed);
            if (new_car == TOK_ERROR) {
                gc_unprotect(5);
                return TOK_ERROR;
            }
            if (new_car != old_car)
                changed = true;
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                unsigned new_tail = rename_template_vars_shadowed_heads(
                    next, rename_map, quote_shadowed, quasiquote_shadowed);
                if (new_tail == TOK_ERROR) {
                    gc_unprotect(5);
                    return TOK_ERROR;
                }
                if (new_tail != next)
                    changed = true;
                if (!changed) {
                    gc_unprotect(5);
                    return original;
                }
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                gc_unprotect(5);
                return result;
            }
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_quote || next_id == ctx.kw_quasiquote ||
                    syntax_rules_form_like(next)) {
                    unsigned new_tail = rename_template_vars_shadowed_heads(
                        next, rename_map, quote_shadowed,
                        quasiquote_shadowed);
                    if (new_tail == TOK_ERROR) {
                        gc_unprotect(5);
                        return TOK_ERROR;
                    }
                    if (new_tail != next)
                        changed = true;
                    if (!changed) {
                        gc_unprotect(5);
                        return original;
                    }
                    cell_set_cdr(result_tail, new_tail);
                    gc_unprotect(5);
                    return result;
                }
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR) {
            gc_unprotect(2);
            return TOK_ERROR;
        }
        gc_protect(&new_vec);
        bool changed = false;
        for (unsigned i = 0; i < len; i++) {
            unsigned old_elem = vector_data_ptr(tmpl)[i];
            gc_protect(&old_elem);
            unsigned new_elem = rename_template_vars_shadowed_heads(
                old_elem, rename_map, quote_shadowed, quasiquote_shadowed);
            if (new_elem == TOK_ERROR) {
                gc_unprotect(4);
                return TOK_ERROR;
            }
            vector_set_elem(new_vec, i, new_elem);
            if (new_elem != old_elem)
                changed = true;
            gc_unprotect(1);
        }
        gc_unprotect(3);
        return changed ? new_vec : tmpl;
    }

    return tmpl;
}

// Extract pattern variables from a syntax-rules pattern
static unsigned collect_pattern_vars(unsigned pattern, unsigned collected,
                                     int64_t ellipsis, unsigned literals)
{
    if (!pattern)
        return collected;

    GC_GUARD;
    gc_protect(&pattern);
    gc_protect(&collected);
    gc_protect(&literals);

    if (IS_ATOM(pattern)) {
        int64_t id = CELL_ID(pattern);
        // Skip ellipsis, underscore, literals
        if (!identifier_valid(pattern) || id == ellipsis ||
            id == ctx.kw_underscore)
            return collected;
        for (unsigned l = literals; l; l = cdr(l)) {
            if (IS_ATOM(car(l)) && CELL_ID(car(l)) == id)
                return collected;
        }
        // Check if already collected
        for (unsigned c = collected; c; c = cdr(c)) {
            if (IS_ATOM(car(c)) && CELL_ID(car(c)) == id)
                return collected;
        }
        return alloc_cons(pattern, collected);
    }

    if (IS_PAIR(pattern)) {
        // Walk ordinary list spines iteratively.  Macro patterns can be very
        // wide, and relying on tail-call optimization here makes compilation
        // stack-safe only in optimized builds.
        while (IS_PAIR(pattern)) {
            collected =
                collect_pattern_vars(car(pattern), collected, ellipsis, literals);
            pattern = cdr(pattern);
        }
        return collect_pattern_vars(pattern, collected, ellipsis, literals);
    }

    if (IS_VECTOR(pattern)) {
        unsigned len = vector_len(pattern);
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointer each iteration - GC may have moved pattern
            unsigned *data = vector_data_ptr(pattern);
            collected = collect_pattern_vars(data[i], collected,
                                             ellipsis, literals);
        }
        return collected;
    }

    return collected;
}

// Emit bytecode to define gensyms created during macro expansion
// This ensures gensyms from referential transparency are available at runtime
static void emit_gensym_definitions(compile_ctx *cctx, unsigned old_gensym,
                                    unsigned new_gensym)
{
    for (unsigned g = old_gensym; g < new_gensym; g++) {
        char name[20];
        snprintf(name, sizeof(name), "##gensym##%u", g);
        unsigned atom = atom_from_string(name);
        int64_t gensym_id = CELL_ID(atom);

        unsigned binding_cell = env_find_binding_cell(gensym_id, cctx->env);
        if (binding_cell && IS_BINDING_REF(car(binding_cell))) {
            unsigned binding_ref = car(binding_cell);
            unsigned target_cell = CELL_CAR(binding_ref);
            unsigned target_val = car(target_cell);
            unsigned target_var = cdr(binding_ref);
            if (IS_SYNTAX(target_val) || IS_MACRO(target_val)) {
                emit2(cctx, OP_CONST, code_add_const(cctx->code, target_val));
                emit2(cctx, OP_DEFINE, gensym_id);
                continue;
            }
            unsigned resolved_target =
                lookup_silent(CELL_ID(target_var), cctx->env);
            if (resolved_target != TOK_ERROR &&
                (IS_SYNTAX(resolved_target) || IS_MACRO(resolved_target))) {
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, resolved_target));
                emit2(cctx, OP_DEFINE, gensym_id);
                continue;
            }
            int local_slot = find_stack_local_slot(cctx, CELL_ID(target_var));
            if (local_slot >= 0) {
                emit_local_get_slot(cctx, local_slot);
                emit2(cctx, OP_DEFINE, gensym_id);
                continue;
            }
            emit3(cctx, OP_DEFINE_ALIAS, gensym_id, CELL_ID(target_var));
            continue;
        }

        // Look up the gensym value in the compile-time environment
        unsigned val = lookup_silent(gensym_id, cctx->env);
        if (val != TOK_ERROR) {
            // Emit const and define
            emit2(cctx, OP_CONST, code_add_const(cctx->code, val));
            emit2(cctx, OP_DEFINE, gensym_id);
        }
    }
}

static bool compile_macro_binding(unsigned mac, unsigned expr,
                                  compile_ctx *cctx, compile_result *result)
{
    if (!IS_SYNTAX(mac) && !IS_MACRO(mac))
        return false;

    // Macro expansion runs before the ordinary application compiler, so it
    // must enforce the same proper-list invariant itself.  In particular, a
    // legacy macro with a rest parameter would otherwise accept a cyclic form
    // and expose it to the expander.
    if (!list_length_checked(cdr(expr), NULL, "macro application")) {
        *result = emit_syntax_error(cctx, "macro application: invalid syntax");
        return true;
    }

    if (cctx->macro_expansion_depth >= MAX_COMPILE_MACRO_EXPANSION_DEPTH) {
        show_error("macro expansion exceeded maximum depth");
        *result =
            emit_syntax_error(cctx, "macro expansion exceeded maximum depth");
        return true;
    }

    cctx->macro_expansion_depth++;

    if (IS_SYNTAX(mac)) {
        unsigned old_gensym = gensym_counter;
        unsigned expanded = apply_syntax(mac, expr, cctx->env);
        if (expanded == TOK_ERROR) {
            show_error("macro expansion failed");
            *result = emit_syntax_error(cctx, "macro expansion failed");
            cctx->macro_expansion_depth--;
            return true;
        }
        // Expansion may be a large newly allocated tree.  The gensym setup
        // below allocates before compilation starts, so keep that tree rooted
        // across the setup rather than relying on incidental heap headroom.
        gc_protect(&expanded);
        emit_gensym_definitions(cctx, old_gensym, gensym_counter);
        *result = compile_expr_internal(expanded, cctx);
        gc_unprotect(1);
        cctx->macro_expansion_depth--;
        return true;
    }

    if (IS_MACRO(mac)) {
        GC_GUARD;
        unsigned params = car(mac);
        unsigned mbody = car(cdr(mac));
        unsigned menv = cdr(cdr(mac));
        gc_protect(&mbody);
        gc_protect(&menv);
        unsigned frame = bind_params(params, cdr(expr));
        if (frame == TOK_ERROR) {
            *result = emit_syntax_error(cctx, "macro expansion failed");
            cctx->macro_expansion_depth--;
            return true;
        }
        gc_protect(&frame);
        unsigned new_env = alloc_cons(frame, menv);
        gc_protect(&new_env);
        unsigned expanded = eval_cps(mbody, new_env);
        gc_protect(&expanded);
        if (expanded == TOK_ERROR) {
            show_error("macro expansion failed");
            *result = emit_syntax_error(cctx, "macro expansion failed");
            cctx->macro_expansion_depth--;
            return true;
        }
        *result = compile_expr_internal(expanded, cctx);
        cctx->macro_expansion_depth--;
        return true;
    }

    cctx->macro_expansion_depth--;
    return false;
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
    if (IS_FIXNUM(expr)) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, expr));
        return const_result(expr);
    }
    if (!IS_CELL(expr)) {
        show_error("compile: invalid expression");
        emit(cctx, OP_HALT);
        return dynamic_result();
    }

    GC_GUARD;
    gc_protect(&expr);

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
        if (expr == ctx.atom_true || expr == ctx.atom_false) {
            emit2(cctx, OP_CONST, code_add_const(cctx->code, expr));
            return const_result(expr);
        }

        // Check if this variable is a stack local
        int64_t var_id = CELL_ID(expr);
        int local_slot = find_stack_local_slot(cctx, var_id);
        if (local_slot >= 0) {
            emit_local_get_slot(cctx, local_slot);
            return dynamic_result();
        }
        // Fall back to environment lookup
        emit4(cctx, OP_LOOKUP, var_id, IC_UNCACHED, IC_UNCACHED);
        return dynamic_result();
    }

    case BT_CONS: {
        unsigned head = car(expr);

        // Check for special forms
        if (IS_ATOM(head)) {
            int64_t kw = CELL_ID(head);
            unsigned special_binding = lookup_silent(kw, cctx->env);
            compile_result macro_result;
            if (special_binding != TOK_ERROR &&
                compile_macro_binding(special_binding, expr, cctx,
                                      &macro_result))
                return macro_result;

            if (special_binding == TOK_ERROR && kw == ctx.kw_quote) {
                if (!syntax_arity_checked(expr, 1, 1, "quote"))
                    return emit_syntax_error(cctx, "quote: invalid syntax");
                unsigned val = cadr(expr);
                emit2(cctx, OP_CONST, code_add_const(cctx->code, val));
                return const_result(val);
            }
            if (special_binding == TOK_ERROR && kw == ctx.kw_if)
                return compile_if(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_lambda)
                return compile_lambda(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_begin)
                return compile_begin(cdr(expr), cctx);
            if (special_binding == TOK_ERROR &&
                (kw == ctx.kw_let || kw == ctx.kw_letstar ||
                 kw == ctx.kw_letrec)) {
                if (kw == ctx.kw_let)
                    return compile_let(expr, cctx);
                if (kw == ctx.kw_letstar)
                    return compile_letstar(expr, cctx);
                return compile_letrec(expr, cctx);
            }
            if (special_binding == TOK_ERROR && kw == ctx.kw_and)
                return compile_and(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_or)
                return compile_or(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_cond)
                return compile_cond(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_cond_expand)
                return compile_cond_expand(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_define)
                return compile_define(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_set)
                return compile_set(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_quasiquote)
                return compile_quasiquote(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_define_syntax) {
                // (define-syntax name transformer)
                if (!syntax_arity_checked(expr, 2, 2, "define-syntax"))
                    return emit_syntax_error(cctx,
                                             "define-syntax: invalid syntax");
                unsigned name = cadr(expr);
                unsigned transformer_form = caddr(expr);
                if (!identifier_valid(name) ||
                    !syntax_rules_valid(
                        transformer_form,
                        syntax_default_ellipsis_id(cctx->env),
                        "define-syntax"))
                    return emit_syntax_error(cctx,
                                             "define-syntax: invalid syntax");
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, transformer_form));
                emit2(cctx, OP_DEFSYNTAX, CELL_ID(name));
                emit2(cctx, OP_CONST, code_add_const(cctx->code, name));
                install_compile_time_define_syntax(expr, cctx);
                return dynamic_result();
            }

            if (special_binding == TOK_ERROR &&
                (kw == ctx.kw_let_syntax || kw == ctx.kw_letrec_syntax)) {
                // (let-syntax ((name transformer) ...) body ...)
                // (letrec-syntax ((name transformer) ...) body ...)
                const char *form_name = kw == ctx.kw_let_syntax
                                            ? "let-syntax"
                                            : "letrec-syntax";
                if (!syntax_arity_checked(expr, 2, UINT_MAX, form_name))
                    return emit_syntax_error(
                        cctx, "syntax binding: invalid syntax");
                unsigned bindings = cadr(expr);
                unsigned body = cddr(expr);
                if (!binding_list_valid(bindings, form_name) ||
                    !list_length_checked(body, NULL, form_name))
                    return emit_syntax_error(
                        cctx, "syntax binding: invalid syntax");

                // If no bindings, compile body in the current env so internal
                // defines splice into the enclosing body
                if (!bindings) {
                    return compile_begin(body, cctx);
                }

                GC_GUARD;
                gc_protect(&bindings);
                gc_protect(&body);

                unsigned new_env = cctx->env;
                gc_protect(&new_env);
                unsigned frame_vars = 0, frame_vals = 0;
                gc_protect(&frame_vars);
                gc_protect(&frame_vals);

                // Collect free variables from all templates and create rename map
                unsigned rename_map = 0;
                gc_protect(&rename_map);
                unsigned gensym_bindings = 0; // List of (gensym . orig_var_id)
                gc_protect(&gensym_bindings);
                unsigned gensym_const_bindings = 0; // List of (gensym . value)
                gc_protect(&gensym_const_bindings);

                // First pass: create frame with placeholder values for letrec-syntax
                if (kw == ctx.kw_letrec_syntax) {
                    unsigned frame = alloc_cons(0, 0);
                    gc_protect(&frame);
                    new_env = alloc_cons(frame, cctx->env);
                    gc_unprotect(1);
                }

                // First pass: collect all free variables from all templates
                unsigned b = bindings;
                gc_protect(&b);
                while (b) {
                    unsigned binding = car(b);
                    unsigned transformer_form = cadr(binding);

                    int64_t default_ellipsis_id =
                        kw == ctx.kw_let_syntax
                            ? syntax_default_ellipsis_id(cctx->env)
                            : syntax_default_ellipsis_id(new_env);
                    if (!syntax_rules_valid(transformer_form,
                                            default_ellipsis_id, form_name))
                        return emit_syntax_error(
                            cctx, "syntax binding: invalid syntax");

                    int64_t ellipsis_id = 0;
                    unsigned literals = 0;
                    unsigned rules = 0;
                    syntax_rules_parts(transformer_form, default_ellipsis_id,
                                       &ellipsis_id, &literals, &rules);

                    unsigned r = rules;
                    gc_protect(&r);
                    while (r) {
                        unsigned rule = car(r);
                        unsigned pattern = car(rule);
                        unsigned tmpl = cadr(rule);

                        // Skip macro name in pattern
                        if (IS_PAIR(pattern))
                            pattern = cdr(pattern);

                        // Collect pattern variables
                        unsigned pattern_vars =
                            collect_pattern_vars(pattern, 0, ellipsis_id, literals);
                        gc_protect(&pattern_vars);
                        if (kw == ctx.kw_letrec_syntax)
                            pattern_vars =
                                add_template_syntax_bounds(pattern_vars,
                                                           bindings);

                        // Collect free variables from template
                        unsigned free_vars = collect_template_free_vars(
                            tmpl, pattern_vars, 0, ellipsis_id, cctx->env);
                        gc_protect(&free_vars);

                        // For each free var, create gensym if bound in outer scope
                        unsigned fv = free_vars;
                        gc_protect(&fv);
                        while (fv) {
                            unsigned var_atom = car(fv);
                            gc_protect(&var_atom);
                            int64_t var_id = CELL_ID(var_atom);

                            // Check if already in rename_map
                            bool already_mapped = false;
                            for (unsigned m = rename_map; m; m = cdr(m)) {
                                if (CELL_ID(car(car(m))) == var_id) {
                                    already_mapped = true;
                                    break;
                                }
                            }
                            if (already_mapped)
                                goto next_free_var;

                            // Check if bound in outer scope (compile-time env)
                            unsigned val = lookup_silent(var_id, cctx->env);
                            if (val != TOK_ERROR) {
                                // Create gensym
                                extern unsigned gensym_counter;
                                char name[20];
                                snprintf(name, sizeof(name), "##gensym##%u",
                                         gensym_counter++);
                                unsigned gensym_atom = atom_from_string(name);
                                gc_protect(&gensym_atom);
                                gc_protect(&val);

                                // Add to rename map: (var_atom . gensym_atom)
                                unsigned entry = alloc_cons(var_atom, gensym_atom);
                                gc_protect(&entry);
                                rename_map = alloc_cons(entry, rename_map);
                                gc_unprotect(1);

                                if (IS_SYNTAX(val) || IS_MACRO(val)) {
                                    unsigned const_entry =
                                        alloc_cons(gensym_atom, val);
                                    gc_protect(&const_entry);
                                    gensym_const_bindings =
                                        alloc_cons(const_entry,
                                                   gensym_const_bindings);
                                    gc_unprotect(1);
                                } else {
                                    unsigned bind_entry =
                                        alloc_cons(gensym_atom, var_atom);
                                    gc_protect(&bind_entry);
                                    gensym_bindings =
                                        alloc_cons(bind_entry,
                                                   gensym_bindings);
                                    gc_unprotect(1);
                                }

                                gc_unprotect(2);
                            }
next_free_var:
                            gc_unprotect(1);
                            fv = cdr(fv);
                        }
                        gc_unprotect(1);
                        gc_unprotect(2);
                        r = cdr(r);
                    }
                    gc_unprotect(1);
                    b = cdr(b);
                }
                gc_unprotect(1);

                // Second pass: create transformers with renamed templates
                b = bindings;
                gc_protect(&b);
                while (b) {
                    unsigned binding = car(b);
                    unsigned name = car(binding);
                    unsigned transformer_form = cadr(binding);
                    gc_protect(&name);

                    // Check it's a syntax-rules form
                    int64_t default_ellipsis_id =
                        kw == ctx.kw_let_syntax
                            ? syntax_default_ellipsis_id(cctx->env)
                            : syntax_default_ellipsis_id(new_env);
                    if (!syntax_rules_valid(transformer_form,
                                            default_ellipsis_id, form_name)) {
                        return emit_syntax_error(cctx,
                                                 "syntax binding: invalid syntax");
                    }

                    // Rename free vars in templates
                    unsigned renamed_form = transformer_form;
                    if (rename_map) {
                        gc_protect(&transformer_form);
                        gc_protect(&renamed_form);

                        // Rebuild syntax-rules with renamed templates
                        int64_t ellipsis_id = 0;
                        unsigned literals = 0;
                        unsigned rules = 0;
                        syntax_rules_parts(transformer_form,
                                           default_ellipsis_id,
                                           &ellipsis_id, &literals, &rules);
                        gc_protect(&literals);
                        gc_protect(&rules);

                        unsigned renamed_rules = 0;
                        gc_protect(&renamed_rules);
                        unsigned renamed_tail = 0;
                        gc_protect(&renamed_tail);

                        unsigned r = rules;
                        gc_protect(&r);
                        while (r) {
                            unsigned rule = car(r);
                            unsigned pattern = car(rule);
                            unsigned tmpl = cadr(rule);

                            gc_protect(&pattern);
                            unsigned renamed_tmpl = rename_template_vars(tmpl, rename_map);
                            if (renamed_tmpl == TOK_ERROR) {
                                return emit_syntax_error(
                                    cctx, "syntax binding: invalid syntax");
                            }
                            gc_protect(&renamed_tmpl);
                            unsigned new_rule_tail = alloc_cons(renamed_tmpl, 0);
                            gc_protect(&new_rule_tail);
                            unsigned new_rule = alloc_cons(pattern, new_rule_tail);
                            gc_protect(&new_rule);

                            unsigned new_cell = alloc_cons(new_rule, 0);
                            gc_unprotect(4);
                            if (!renamed_rules) {
                                renamed_rules = new_cell;
                                renamed_tail = new_cell;
                            } else {
                                cell_set_cdr(renamed_tail, new_cell);
                                renamed_tail = new_cell;
                            }
                            r = cdr(r);
                        }
                        gc_unprotect(1);

                        unsigned sr_atom = car(transformer_form);
                        unsigned second = cadr(transformer_form);
                        gc_protect(&sr_atom);
                        gc_protect(&second);
                        unsigned sr_tail = 0;
                        gc_protect(&sr_tail);
                        if (IS_ATOM(second)) {
                            unsigned rules_tail =
                                alloc_cons(literals, renamed_rules);
                            gc_protect(&rules_tail);
                            sr_tail = alloc_cons(second, rules_tail);
                            gc_unprotect(1);
                        } else {
                            sr_tail = alloc_cons(literals, renamed_rules);
                        }
                        renamed_form = alloc_cons(sr_atom, sr_tail);
                        gc_unprotect(9);
                    }

                    // Free vars are pre-renamed, but the default ellipsis
                    // decision still belongs to the transformer definition
                    // environment.
                    unsigned transformer_closure_env =
                        kw == ctx.kw_let_syntax ? cctx->env : new_env;
                    unsigned transformer =
                        make_syntax_transformer_with_default_ellipsis(
                            renamed_form, transformer_closure_env,
                            default_ellipsis_id);
                    if (transformer == TOK_ERROR) {
                        return emit_syntax_error(
                            cctx, "syntax binding: invalid syntax");
                    }

                    // Add to frame
                    gc_protect(&transformer);
                    unsigned vc = alloc_cons(name, frame_vars);
                    gc_protect(&vc);
                    unsigned ac = alloc_cons(transformer, frame_vals);
                    gc_unprotect(3);
                    frame_vars = vc;
                    frame_vals = ac;
                    b = cdr(b);
                }
                gc_unprotect(1);

                // Create frame if let-syntax (letrec-syntax already has frame)
                if (kw == ctx.kw_let_syntax) {
                    unsigned frame = alloc_cons(frame_vars, frame_vals);
                    gc_protect(&frame);
                    new_env = alloc_cons(frame, cctx->env);
                    gc_unprotect(1);
                } else {
                    // Update letrec-syntax frame
                    unsigned frame = car(new_env);
                    cell_set_car(frame, frame_vars);
                    cell_set_cdr(frame, frame_vals);
                }

                // Push a new runtime frame for the body
                emit_pushenv(cctx);

                // Emit gensym bindings: look up original var, define gensym
                // This happens at let-syntax entry, BEFORE any inner scopes
                for (unsigned gb = gensym_const_bindings; gb; gb = cdr(gb)) {
                    unsigned entry = car(gb);
                    unsigned gensym_atom = car(entry);
                    unsigned val = cdr(entry);
                    int64_t gensym_id = CELL_ID(gensym_atom);

                    emit2(cctx, OP_CONST, code_add_const(cctx->code, val));
                    emit2(cctx, OP_DEFINE, gensym_id);
                }

                for (unsigned gb = gensym_bindings; gb; gb = cdr(gb)) {
                    unsigned entry = car(gb);
                    unsigned gensym_atom = car(entry);
                    unsigned var_atom = cdr(entry);
                    int64_t gensym_id = CELL_ID(gensym_atom);
                    int64_t var_id = CELL_ID(var_atom);

                    int local_slot = find_stack_local_slot(cctx, var_id);
                    if (local_slot >= 0) {
                        emit_local_get_slot(cctx, local_slot);
                        emit2(cctx, OP_DEFINE, gensym_id);
                    } else {
                        emit3(cctx, OP_DEFINE_ALIAS, gensym_id, var_id);
                    }
                }

                // Add gensyms to compile-time env so they're visible in body
                if (gensym_bindings || gensym_const_bindings) {
                    unsigned gensym_frame_vars = 0, gensym_frame_vals = 0;
                    gc_protect(&gensym_frame_vars);
                    gc_protect(&gensym_frame_vals);

                    unsigned gcb = gensym_const_bindings;
                    gc_protect(&gcb);
                    while (gcb) {
                        unsigned entry = car(gcb);
                        unsigned gensym_atom = car(entry);
                        unsigned val = cdr(entry);
                        gensym_frame_vars =
                            alloc_cons(gensym_atom, gensym_frame_vars);
                        gensym_frame_vals = alloc_cons(val, gensym_frame_vals);
                        gcb = cdr(gcb);
                    }
                    gc_unprotect(1);

                    unsigned gb = gensym_bindings;
                    gc_protect(&gb);
                    while (gb) {
                        unsigned entry = car(gb);
                        unsigned gensym_atom = car(entry);
                        // Use a placeholder value - actual value is set at runtime
                        gensym_frame_vars = alloc_cons(gensym_atom, gensym_frame_vars);
                        gensym_frame_vals = alloc_cons(gensym_atom, gensym_frame_vals);
                        gb = cdr(gb);
                    }
                    gc_unprotect(1);

                    unsigned gensym_frame = alloc_cons(gensym_frame_vars, gensym_frame_vals);
                    gc_protect(&gensym_frame);
                    new_env = alloc_cons(gensym_frame, new_env);
                    gc_unprotect(3);
                }

                // Compile body with new compile-time environment
                compile_ctx new_cctx = *cctx;
                new_cctx.env = new_env;
                gc_protect(&new_cctx.env);
                gc_protect(&new_cctx.known_lambdas);
                compile_begin(body, &new_cctx);
                gc_unprotect(2);

                // Pop the runtime frame
                emit_popenv(cctx);
                return dynamic_result();
            }

            // Check for macro application (silent - don't warn if not found)
            unsigned mac = lookup_silent(kw, cctx->env);
            if (mac != TOK_ERROR) {
                compile_result application_macro_result;
                if (compile_macro_binding(mac, expr, cctx,
                                          &application_macro_result))
                    return application_macro_result;
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
    GC_GUARD;
    gc_protect(&expr);

    if (!syntax_arity_checked(expr, 2, 3, "if"))
        return emit_syntax_error(cctx, "if: invalid syntax");

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
    unsigned branch_env = cctx->env;
    gc_protect(&branch_env);
    unsigned else_jump = emit_jump(cctx, OP_JUMPIFNOT);

    // Compile then branch
    cctx->tail_position = tail;
    compile_result then_result = compile_expr_internal(caddr(expr), cctx);
    cctx->env = branch_env;

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
    cctx->env = branch_env;

    patch_jump(cctx, end_jump);

    (void)then_result;
    (void)else_result;
    return dynamic_result();
}

// (lambda params body...)
static compile_result compile_lambda(unsigned expr, compile_ctx *cctx)
{
    GC_GUARD;
    unsigned rest = cdr(expr);
    if (!IS_PAIR(rest))
        return emit_syntax_error(cctx, "lambda: expected formals and body");
    unsigned params = car(rest);
    unsigned body = cdr(rest);

    // Create compile-time environment that shadows lambda parameters
    // This prevents the compiler from inlining builtins that are shadowed
    // by lambda parameters (e.g., (lambda (exit) (exit 'x)) should not
    // call the exit primitive)
    unsigned lambda_env = cctx->env;

    gc_protect(&params);
    gc_protect(&body);
    gc_protect(&lambda_env);

    // lambda_params_valid allocates, so locals must be protected first
    if (!body || !list_length_checked(body, NULL, "lambda") ||
        !lambda_params_valid(params))
        return emit_syntax_error(cctx, "lambda: invalid syntax");

    // Build a dummy frame with parameters as vars, all bound to 0
    {
        unsigned vars = 0, vals = 0;
        gc_protect(&vars);
        gc_protect(&vals);
        unsigned vars_tail = 0, vals_tail = 0;
        gc_protect(&vars_tail);
        gc_protect(&vals_tail);

        for (unsigned p = params; p; p = IS_PAIR(p) ? cdr(p) : 0) {
            unsigned var = IS_PAIR(p) ? car(p) : p;
            gc_protect(&var);
            unsigned vc = alloc_cons(var, 0);
            gc_protect(&vc);
            unsigned ac = alloc_cons(0, 0); // dummy value
            if (!vars) {
                vars = vc;
                vals = ac;
            } else {
                cell_set_cdr(vars_tail, vc);
                cell_set_cdr(vals_tail, ac);
            }
            vars_tail = vc;
            vals_tail = ac;
            gc_unprotect(2);
            if (!IS_PAIR(p))
                break; // rest param
        }

        if (vars) {
            unsigned frame = alloc_cons(vars, vals);
            gc_protect(&frame);
            lambda_env = alloc_cons(frame, lambda_env);
        }
    }

    // Create new compilation context for lambda body with shadowed params
    compile_ctx *lambda_cctx = cctx_new(cctx, lambda_env);
    if (!lambda_cctx) {
        emit(cctx, OP_HALT);
        return dynamic_result();
    }
    lambda_cctx->tail_position = true;

    // Pick up loop info staged by an enclosing letrec. Only the letrec
    // binding's own lambda may take it (loop_pending); lambdas nested inside
    // the loop body must NOT inherit it, or their tail calls to the loop
    // variable would compile as self-jumps into the wrong code object.
    if (cctx->loop_pending && cctx->loop_var_id >= 0) {
        lambda_cctx->loop_var_id = cctx->loop_var_id;
        lambda_cctx->loop_params = cctx->loop_params;
        lambda_cctx->loop_arity = cctx->loop_arity;
        // Clear parent's loop info so it doesn't leak to other children
        cctx->loop_var_id = -1;
        cctx->loop_pending = false;
    }

    // Count parameters
    unsigned arity = 0;
    bool has_rest = false;
    for (unsigned p = params; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        if (IS_ATOM(p)) {
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

    // Stack locals: keep params on VM stack for direct access (LOCAL_GET)
    // instead of in environment frames. Only for fixed-arity functions where
    // no parameter is captured by an inner closure.
    if (!has_rest && arity > 0 && arity <= 8) {
        bool can_use_locals = true;
        unsigned slot = 0;
        for (unsigned p = params; p && IS_PAIR(p); p = cdr(p), slot++) {
            int64_t pid = CELL_ID(car(p));
            if (captured_by_inner_lambda(body, pid, lambda_env)) {
                can_use_locals = false;
                break;
            }
            lambda_cctx->local_ids[slot] = pid;
        }
        if (can_use_locals) {
            lambda_cctx->num_locals = (int)arity;
            lambda_cctx->code->use_locals = true;
        }
    }

    // Compile body
    compile_begin(body, lambda_cctx);
    if (lambda_cctx->code->use_locals) {
        emit2(lambda_cctx, OP_RETURN_LOCALS, lambda_cctx->num_locals);
    } else {
        emit(lambda_cctx, OP_RETURN);
    }

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
static unsigned is_internal_define(unsigned expr, unsigned env)
{
    if (!IS_PAIR(expr))
        return 0;
    unsigned kw = car(expr);
    if (!IS_KEYWORD(kw, ctx.kw_define))
        return 0;
    if (lookup_silent(ctx.kw_define, env) != TOK_ERROR)
        return 0;
    if (!IS_PAIR(cdr(expr)))
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
    gc_protect(&env);

    while (body) {
        unsigned expr = car(body);
        gc_protect(&expr);

        // Check for direct define
        unsigned name = is_internal_define(expr, env);
        if (name) {
            gc_protect(&name);
            unsigned cell = alloc_cons(name, names);
            names = cell;
            gc_unprotect(1);
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
                unsigned inner_name = is_internal_define(inner, env);
                if (inner_name) {
                    gc_protect(&inner_name);
                    unsigned cell = alloc_cons(inner_name, names);
                    names = cell;
                    gc_unprotect(1);
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

    gc_unprotect(3);
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

    FORLIST(n, names) {
        unsigned var = car(n);
        gc_protect(&var);
        unsigned vc = alloc_cons(var, frame_vars);
        frame_vars = vc;
        unsigned val = alloc_cons(0, frame_vals); // 0 = non-builtin placeholder
        frame_vals = val;
        gc_unprotect(1);
    }

    unsigned frame = alloc_cons(frame_vars, frame_vals);
    gc_protect(&frame);
    unsigned new_env = alloc_cons(frame, env);
    gc_unprotect(5);
    return new_env;
}

// Compile sequence of expressions
static compile_result compile_begin(unsigned exprs, compile_ctx *cctx)
{
    if (!list_length_checked(exprs, NULL, "begin"))
        return emit_syntax_error(cctx, "begin: invalid syntax");

    if (!exprs) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0)); // unspecified
        return const_result(0);
    }

    // Scan for internal defines and extend compile-time environment
    // This ensures defines shadow any macros from enclosing scopes
    GC_GUARD;
    gc_protect(&exprs);
    unsigned internal_defs = scan_internal_defines(exprs, cctx->env);
    gc_protect(&internal_defs);

    if (internal_defs) {
        cctx->env = extend_compile_env_with_names(cctx->env, internal_defs);
    }
    unsigned syntax_frame = alloc_cons(0, 0);
    gc_protect(&syntax_frame);
    cctx->env = alloc_cons(syntax_frame, cctx->env);

    bool tail = cctx->tail_position;
    compile_result result = dynamic_result();
    bool had_prior_expr = false;

    while (exprs) {
        bool is_last = !cdr(exprs);
        cctx->tail_position = tail && is_last;

        result = compile_expr_internal(car(exprs), cctx);
        install_compile_time_define_syntax(car(exprs), cctx);

        if (!is_last) {
            emit(cctx, OP_POP); // Discard non-final values
            had_prior_expr = true;
        }

        exprs = cdr(exprs);
    }

    // A multi-expression begin may contain effects before the final value.
    // Reporting the final value as a constant lets outer folds erase those
    // effects by rewinding emitted bytecode.
    return had_prior_expr ? dynamic_result() : result;
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

    FORLIST(b, bindings) {
        unsigned var = car(car(b));
        gc_protect(&var);
        unsigned vc = alloc_cons(var, frame_vars);
        frame_vars = vc;
        unsigned val = alloc_cons(0, frame_vals); // 0 = non-builtin
        frame_vals = val;
        gc_unprotect(1);
    }

    unsigned frame = alloc_cons(frame_vars, frame_vals);
    gc_protect(&frame);
    unsigned new_env = alloc_cons(frame, env);
    gc_unprotect(5);
    return new_env;
}

static compile_result compile_named_let(unsigned expr, compile_ctx *cctx)
{
    if (!syntax_arity_checked(expr, 3, UINT_MAX, "let"))
        return emit_syntax_error(cctx, "let: invalid syntax");

    unsigned name = cadr(expr);
    unsigned bindings = caddr(expr);
    unsigned body = cdddr(expr);
    if (!identifier_valid(name) || !binding_list_valid(bindings, "let") ||
        !list_length_checked(body, NULL, "let"))
        return emit_syntax_error(cctx, "let: invalid named let syntax");

    GC_GUARD;
    gc_protect(&name);
    gc_protect(&bindings);
    gc_protect(&body);

    unsigned params = 0, params_tail = 0;
    unsigned args = 0, args_tail = 0;
    gc_protect(&params);
    gc_protect(&params_tail);
    gc_protect(&args);
    gc_protect(&args_tail);

    FORLIST(b, bindings) {
        unsigned binding = car(b);
        list_append(&params, &params_tail, car(binding));
        list_append(&args, &args_tail, cadr(binding));
    }

    unsigned argc = list_length(args);
    bool tail = cctx->tail_position;
    cctx->tail_position = false;

    unsigned a = args;
    gc_protect(&a);
    while (a) {
        compile_expr_internal(car(a), cctx);
        a = cdr(a);
    }
    gc_unprotect(1);

    emit_pushenv(cctx);
    emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
    emit2(cctx, OP_DEFINE, CELL_ID(name));

    unsigned saved_env = cctx->env;
    gc_protect(&saved_env);

    unsigned value_list = 0, binding = 0, binding_list = 0;
    gc_protect(&value_list);
    gc_protect(&binding);
    gc_protect(&binding_list);
    value_list = alloc_cons(0, 0);
    binding = alloc_cons(name, value_list);
    binding_list = alloc_cons(binding, 0);
    cctx->env = extend_compile_env(cctx->env, binding_list);

    unsigned lambda_tail = 0, lambda_expr = 0;
    gc_protect(&lambda_tail);
    gc_protect(&lambda_expr);
    lambda_tail = alloc_cons(params, body);
    lambda_expr = alloc_cons(atom_from_string("lambda"), lambda_tail);
    compile_lambda(lambda_expr, cctx);
    emit2(cctx, OP_SET, CELL_ID(name));
    emit2(cctx, OP_CALL, argc);

    cctx->env = saved_env;
    cctx->tail_position = tail;
    emit_popenv(cctx);

    return dynamic_result();
}

// (let ((var val) ...) body...)
static compile_result compile_let(unsigned expr, compile_ctx *cctx)
{
    if (!syntax_arity_checked(expr, 2, UINT_MAX, "let"))
        return emit_syntax_error(cctx, "let: invalid syntax");

    unsigned bindings = cadr(expr);
    if (IS_ATOM(bindings))
        return compile_named_let(expr, cctx);

    unsigned body = cddr(expr);
    if (!binding_list_valid(bindings, "let") ||
        !list_length_checked(body, NULL, "let"))
        return emit_syntax_error(cctx, "let: invalid syntax");

    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&body);

    // Count bindings
    unsigned count = 0;
    FORLIST(b, bindings) { count++; }

    bool tail = cctx->tail_position;

    // Compile all values first (in current environment)
    cctx->tail_position = false;
    unsigned bind_cursor = bindings;
    gc_protect(&bind_cursor);
    while (bind_cursor) {
        compile_expr_internal(cadr(car(bind_cursor)), cctx);
        bind_cursor = cdr(bind_cursor);
    }
    gc_unprotect(1);

    // Push new environment frame
    emit_pushenv(cctx);

    // Dynamically allocate variable array
    unsigned *vars = NULL;
    if (count > 0) {
        vars = checked_malloc_array(count, sizeof(unsigned));
        if (!vars) {
            show_error("let: out of memory for bindings");
            emit(cctx, OP_HALT);
            return dynamic_result();
        }
    }

    // Collect variables
    unsigned i = 0;
    FORLIST(b, bindings) { vars[i++] = CELL_ID(car(car(b))); }

    // Define in reverse (pop from stack)
    for (int j = (int)count - 1; j >= 0; j--) {
        emit2(cctx, OP_DEFINE, vars[j]);
    }

    // Free the variable array
    free(vars);

    // Extend compile-time environment for body compilation
    unsigned saved_env = cctx->env;
    gc_protect(&saved_env);
    cctx->env = extend_compile_env(cctx->env, bindings);

    // Track lambda bindings for escape analysis inlining
    // Save and extend known_lambdas for bindings of form (var (lambda ...))
    // Only track non-self-referential lambdas (self-ref needs letrec semantics)
    unsigned saved_known = cctx->known_lambdas;
    gc_protect(&saved_known);
    FORLIST(b, bindings) {
        unsigned binding = car(b);
        unsigned var = car(binding);
        unsigned val_expr = cadr(binding);

        // Check if value is a lambda expression
        if (IS_PAIR(val_expr) && IS_ATOM(car(val_expr)) &&
            CELL_ID(car(val_expr)) == ctx.kw_lambda &&
            builtin_keyword_unbound(ctx.kw_lambda, cctx->env)) {
            // Only inline if the lambda doesn't reference the bound variable
            // (self-referential lambdas need letrec semantics to work)
            unsigned lambda_body = cddr(val_expr);
            if (!contains_reference(lambda_body, CELL_ID(var))) {
                // Add (var . lambda-expr) to known_lambdas
                unsigned pair = alloc_cons(var, val_expr);
                gc_protect(&pair);
                cctx->known_lambdas = alloc_cons(pair, cctx->known_lambdas);
                gc_unprotect(1);
            }
        }
    }

    // Compile body in the same tail context as the let expression.
    cctx->tail_position = tail;
    compile_result result = compile_begin(body, cctx);

    // Restore compile-time environment and known_lambdas
    cctx->env = saved_env;
    cctx->known_lambdas = saved_known;

    // Pop environment frame
    emit_popenv(cctx);

    // let introduces bindings, so result is dynamic even if body is constant
    (void)result;
    return dynamic_result();
}

// (let* ((var val) ...) body...)
static compile_result compile_letstar(unsigned expr, compile_ctx *cctx)
{
    if (!syntax_arity_checked(expr, 2, UINT_MAX, "let*"))
        return emit_syntax_error(cctx, "let*: invalid syntax");

    unsigned bindings = cadr(expr);
    unsigned body = cddr(expr);
    if (!binding_list_valid(bindings, "let*") ||
        !list_length_checked(body, NULL, "let*"))
        return emit_syntax_error(cctx, "let*: invalid syntax");

    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&body);

    // Push new environment frame
    emit_pushenv(cctx);

    // Save original compile-time environment
    unsigned saved_env = cctx->env;
    gc_protect(&saved_env);

    bool tail = cctx->tail_position;

    // Compile and define each binding sequentially
    // Each binding extends the compile-time environment for subsequent bindings
    cctx->tail_position = false;
    FORLIST(b, bindings) {
        unsigned binding = car(b);
        gc_protect(&binding);
        compile_expr_internal(cadr(binding), cctx);
        emit2(cctx, OP_DEFINE, CELL_ID(car(binding)));

        // Extend compile-time env with this binding for next iteration
        unsigned single_binding = alloc_cons(binding, 0);
        gc_protect(&single_binding);
        cctx->env = extend_compile_env(cctx->env, single_binding);
        gc_unprotect(2);
    }

    // Compile body in the same tail context as the let* expression.
    cctx->tail_position = tail;
    compile_begin(body, cctx);

    // Restore compile-time environment
    cctx->env = saved_env;

    // Pop environment frame
    emit_popenv(cctx);

    return dynamic_result();
}

// (letrec ((var val) ...) body...)
static compile_result compile_letrec(unsigned expr, compile_ctx *cctx)
{
    if (!syntax_arity_checked(expr, 2, UINT_MAX, "letrec"))
        return emit_syntax_error(cctx, "letrec: invalid syntax");

    unsigned bindings = cadr(expr);
    unsigned body = cddr(expr);
    if (!binding_list_valid(bindings, "letrec") ||
        !list_length_checked(body, NULL, "letrec"))
        return emit_syntax_error(cctx, "letrec: invalid syntax");

    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&body);

    // Count bindings
    unsigned binding_count = 0;
    FORLIST(b, bindings) { binding_count++; }

    // Push new environment frame
    emit_pushenv(cctx);

    // First pass: define all variables with undefined values
    FORLIST(b, bindings) {
        unsigned binding = car(b);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
        emit2(cctx, OP_DEFINE, CELL_ID(car(binding)));
    }

    // Mark this frame as letrec for continuation save/restore
    emit2(cctx, OP_LETREC_MARK, binding_count);

    // Extend compile-time environment with all bindings
    // (letrec bindings are mutually recursive, all visible during value compilation)
    unsigned saved_env = cctx->env;
    gc_protect(&saved_env);
    cctx->env = extend_compile_env(cctx->env, bindings);

    // Save tail position from enclosing context
    bool tail = cctx->tail_position;

    // Detect loop pattern: single binding of form (loop (lambda (params) body))
    // Pass loop info to the lambda compiler so recursive tail calls become SET+JUMP
    int64_t loop_var = -1;
    unsigned loop_params = 0;
    unsigned loop_arity = 0;
    if (binding_count == 1) {
        unsigned binding = car(car(bindings));
        unsigned val_expr = cadr(car(bindings));
        if (IS_ATOM(binding) && IS_PAIR(val_expr) && IS_ATOM(car(val_expr)) &&
            CELL_ID(car(val_expr)) == ctx.kw_lambda &&
            builtin_keyword_unbound(ctx.kw_lambda, cctx->env) &&
            IS_PAIR(cdr(val_expr))) {
            unsigned lparams = cadr(val_expr);
            unsigned lbody = cddr(val_expr);
            // The SET+JUMP transform reuses one mutable frame across
            // iterations, so it is only safe when the lambda has no rest
            // parameter (a matching-arity call would leave it stale) and no
            // parameter is captured by an inner closure (each iteration must
            // see a fresh binding).
            bool safe = true;
            unsigned count = 0;
            for (unsigned p = lparams; p; p = IS_PAIR(p) ? cdr(p) : 0) {
                if (!IS_PAIR(p)) {
                    safe = false; // rest parameter
                    break;
                }
                if (!IS_ATOM(car(p))) {
                    safe = false;
                    break;
                }
                count++;
            }
            if (safe) {
                for (unsigned p = lparams; p && IS_PAIR(p); p = cdr(p)) {
                    if (captured_by_inner_lambda(lbody, CELL_ID(car(p)),
                                                 cctx->env)) {
                        safe = false;
                        break;
                    }
                }
            }
            if (safe) {
                loop_var = CELL_ID(binding);
                loop_params = lparams;
                loop_arity = count;
            }
        }
    }

    // Second pass: compile values and set variables
    cctx->tail_position = false;
    // Temporarily set loop info so compile_lambda can pick it up
    int64_t saved_loop_var = cctx->loop_var_id;
    unsigned saved_loop_params = cctx->loop_params;
    unsigned saved_loop_arity = cctx->loop_arity;
    gc_protect(&saved_loop_params);
    cctx->loop_var_id = loop_var;
    cctx->loop_params = loop_params;
    gc_protect(&cctx->loop_params);
    cctx->loop_arity = loop_arity;
    cctx->loop_pending = loop_var >= 0;
    FORLIST(b, bindings) {
        unsigned binding = car(b);
        gc_protect(&binding);
        compile_expr_internal(cadr(binding), cctx);
        emit2(cctx, OP_SET, CELL_ID(car(binding)));
        emit(cctx, OP_POP); // Discard set! result
        gc_unprotect(1);
    }
    cctx->loop_var_id = saved_loop_var;
    cctx->loop_params = saved_loop_params;
    cctx->loop_arity = saved_loop_arity;
    cctx->loop_pending = false;

    // End letrec initialization - continuations after this won't restore values
    emit(cctx, OP_LETREC_DONE);

    // Compile body with correct tail position from enclosing context
    cctx->tail_position = tail;
    compile_begin(body, cctx);

    // Restore compile-time environment
    cctx->env = saved_env;

    // Pop environment frame
    emit_popenv(cctx);

    return dynamic_result();
}

// (and expr ...)
static compile_result compile_and(unsigned expr, compile_ctx *cctx)
{
    unsigned args = cdr(expr);
    if (!list_length_checked(args, NULL, "and"))
        return emit_syntax_error(cctx, "and: invalid syntax");

    GC_GUARD;
    gc_protect(&args);

    if (!args) {
        // (and) => #t
        emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_true));
        return const_result(ctx.atom_true);
    }

    // Count expressions
    unsigned expr_count = 0;
    FORLIST(a, args) { expr_count++; }

    bool tail = cctx->tail_position;
    unsigned saved_pos = cctx->code->code_len;

    // Dynamically allocate jump array
    unsigned *false_jumps = checked_malloc_array(expr_count, sizeof(unsigned));
    if (!false_jumps) {
        show_error("and: out of memory");
        emit(cctx, OP_HALT);
        return dynamic_result();
    }
    unsigned jump_count = 0;
    unsigned merge_env = 0;
    gc_protect(&merge_env);
    bool has_short_circuit_path = false;

    while (args) {
        bool is_last = !cdr(args);
        cctx->tail_position = tail && is_last;

        unsigned expr_saved_pos = cctx->code->code_len;
        compile_result result = compile_expr_internal(car(args), cctx);

        if (result.is_const) {
            if (IS_FALSE(result.value)) {
                // Constant false short-circuits the rest. If no prior runtime
                // tests were emitted, the whole form is constant; otherwise
                // keep the already-emitted code so its effects still happen.
                if (jump_count == 0) {
                    cctx->code->code_len = saved_pos;
                    emit2(cctx, OP_CONST,
                          code_add_const(cctx->code, ctx.atom_false));
                    free(false_jumps);
                    return const_result(ctx.atom_false);
                }
                break;
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
            if (!has_short_circuit_path) {
                merge_env = cctx->env;
                has_short_circuit_path = true;
            }
            emit(cctx, OP_POP); // Discard if continuing
        }

        args = cdr(args);
    }

    // Patch all false jumps to here
    for (unsigned i = 0; i < jump_count; i++) {
        patch_jump(cctx, false_jumps[i]);
    }

    free(false_jumps);
    if (has_short_circuit_path)
        cctx->env = merge_env;
    return dynamic_result();
}

// (or expr ...)
static compile_result compile_or(unsigned expr, compile_ctx *cctx)
{
    unsigned args = cdr(expr);
    if (!list_length_checked(args, NULL, "or"))
        return emit_syntax_error(cctx, "or: invalid syntax");

    GC_GUARD;
    gc_protect(&args);

    if (!args) {
        // (or) => #f
        emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_false));
        return const_result(ctx.atom_false);
    }

    // Count expressions
    unsigned expr_count = 0;
    FORLIST(a, args) { expr_count++; }

    bool tail = cctx->tail_position;
    unsigned saved_pos = cctx->code->code_len;

    // Dynamically allocate jump array
    unsigned *true_jumps = checked_malloc_array(expr_count, sizeof(unsigned));
    if (!true_jumps) {
        show_error("or: out of memory");
        emit(cctx, OP_HALT);
        return dynamic_result();
    }
    unsigned jump_count = 0;
    unsigned merge_env = 0;
    gc_protect(&merge_env);
    bool has_short_circuit_path = false;

    while (args) {
        bool is_last = !cdr(args);
        cctx->tail_position = tail && is_last;

        unsigned expr_saved_pos = cctx->code->code_len;
        compile_result result = compile_expr_internal(car(args), cctx);

        if (result.is_const) {
            if (IS_TRUTHY(result.value)) {
                // Constant truthy short-circuits the rest. If prior runtime
                // tests exist, preserve them because they may have effects or
                // produce the returned truthy value.
                if (jump_count == 0) {
                    cctx->code->code_len = saved_pos;
                    emit2(cctx, OP_CONST,
                          code_add_const(cctx->code, result.value));
                    free(true_jumps);
                    return const_result(result.value);
                }
                break;
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
            if (!has_short_circuit_path) {
                merge_env = cctx->env;
                has_short_circuit_path = true;
            }
            emit(cctx, OP_POP); // Discard if continuing
        }

        args = cdr(args);
    }

    // Patch all true jumps to here
    for (unsigned i = 0; i < jump_count; i++) {
        patch_jump(cctx, true_jumps[i]);
    }

    free(true_jumps);
    if (has_short_circuit_path)
        cctx->env = merge_env;
    return dynamic_result();
}

// (cond (test expr ...) ... (else expr ...))
static compile_result compile_cond_expand(unsigned expr, compile_ctx *cctx)
{
    unsigned clauses = cdr(expr);
    if (!list_length_checked(clauses, NULL, "cond-expand"))
        return emit_syntax_error(cctx, "cond-expand: invalid syntax");

    FORLIST(c, clauses) {
        unsigned clause = car(c);
        if (!IS_PAIR(clause))
            return emit_syntax_error(cctx, "cond-expand: invalid syntax");
        unsigned requirement = car(clause);
        unsigned body = cdr(clause);
        if (!list_length_checked(body, NULL, "cond-expand"))
            return emit_syntax_error(cctx, "cond-expand: invalid syntax");
        bool is_else = IS_KEYWORD(requirement, ctx.kw_else) &&
                       env_find_binding_cell(ctx.kw_else, cctx->env) == 0;
        if (is_else || cond_expand_requirement_satisfied(requirement))
            return compile_begin(body, cctx);
    }

    emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
    return const_result(0);
}

static compile_result compile_cond(unsigned expr, compile_ctx *cctx)
{
    unsigned clauses = cdr(expr);
    if (!cond_clauses_valid(clauses, "cond", cctx->env))
        return emit_syntax_error(cctx, "cond: invalid syntax");

    GC_GUARD;
    gc_protect(&clauses);

    if (!clauses) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0)); // unspecified
        return const_result(0);
    }

    // Count clauses
    unsigned clause_count = 0;
    FORLIST(c, clauses) { clause_count++; }

    bool tail = cctx->tail_position;

    // Dynamically allocate end jump array
    unsigned *end_jumps = checked_malloc_array(clause_count, sizeof(unsigned));
    if (!end_jumps) {
        show_error("cond: out of memory");
        emit(cctx, OP_HALT);
        return dynamic_result();
    }
    unsigned end_count = 0;
    bool found_else = false;
    unsigned merge_env = 0;
    gc_protect(&merge_env);
    bool has_conditional_path = false;

    while (clauses) {
        unsigned clause = car(clauses);
        gc_protect(&clause);
        unsigned test = car(clause);
        unsigned conseq = cdr(clause);
        gc_protect(&test);
        gc_protect(&conseq);

        // Check for else clause
        if (IS_KEYWORD(test, ctx.kw_else) &&
            env_find_binding_cell(ctx.kw_else, cctx->env) == 0) {
            cctx->tail_position = tail;
            if (conseq) {
                compile_begin(conseq, cctx);
            } else {
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, ctx.atom_true));
            }
            found_else = true;
            gc_unprotect(3);
            break;
        }

        // Compile test
        cctx->tail_position = false;
        unsigned test_saved_pos = cctx->code->code_len;
        compile_result test_result = compile_expr_internal(test, cctx);
        if (test_result.is_const) {
            cctx->code->code_len = test_saved_pos;
            if (IS_FALSE(test_result.value)) {
                clauses = cdr(clauses);
                gc_unprotect(3);
                continue;
            }

            cctx->tail_position = tail;
            bool is_arrow = conseq && IS_KEYWORD(car(conseq), ctx.kw_arrow) &&
                            env_find_binding_cell(ctx.kw_arrow, cctx->env) == 0;
            if (is_arrow) {
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, test_result.value));
                unsigned receiver_expr = cadr(conseq);
                gc_protect(&receiver_expr);
                cctx->tail_position = false;
                compile_expr_internal(receiver_expr, cctx);
                cctx->tail_position = tail;
                gc_unprotect(1);
                emit2(cctx, tail ? OP_TAILCALL : OP_CALL, 1);
            } else if (conseq) {
                compile_begin(conseq, cctx);
            } else {
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, test_result.value));
            }
            found_else = true;
            gc_unprotect(3);
            break;
        }
        unsigned false_env = cctx->env;
        gc_protect(&false_env);

        // Check if we need the test value (=> syntax or no consequent)
        bool is_arrow = conseq && IS_KEYWORD(car(conseq), ctx.kw_arrow) &&
                        env_find_binding_cell(ctx.kw_arrow, cctx->env) == 0;
        bool need_test_value = is_arrow || !conseq;

        if (need_test_value) {
            // Duplicate test result for => receiver or (test) form
            emit(cctx, OP_DUP);
        }

        unsigned next_clause = emit_jump(cctx, OP_JUMPIFNOT);
        if (!has_conditional_path) {
            merge_env = false_env;
            has_conditional_path = true;
        }

        // Compile consequence
        cctx->tail_position = tail;
        if (is_arrow) {
            // (test => receiver) syntax
            // Stack has: test-value (from DUP)
            unsigned receiver_expr = cadr(conseq);
            gc_protect(&receiver_expr);
            cctx->tail_position = false;
            compile_expr_internal(receiver_expr, cctx);
            cctx->tail_position = tail;
            gc_unprotect(1);
            // Stack has: test-value receiver
            // CALL pops fn first, then args - already in correct order
            emit2(cctx, tail ? OP_TAILCALL : OP_CALL, 1);
        } else if (conseq) {
            // Normal consequent - compile expressions
            compile_begin(conseq, cctx);
        }
        // else: no conseq, test value already on stack from DUP

        // Always jump to end after true branch
        end_jumps[end_count++] = emit_jump(cctx, OP_JUMP);
        cctx->env = false_env;

        // Patch false branch entry point
        patch_jump(cctx, next_clause);

        // Pop test value on false branch if we DUP'd
        if (need_test_value) {
            emit(cctx, OP_POP);
        }

        clauses = cdr(clauses);
        gc_unprotect(4);
    }

    // If no else clause was found, emit unspecified for fall-through
    if (!found_else) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
    }

    // Patch all end jumps
    for (unsigned i = 0; i < end_count; i++) {
        patch_jump(cctx, end_jumps[i]);
    }

    free(end_jumps);
    if (has_conditional_path)
        cctx->env = merge_env;
    return dynamic_result();
}

// (define var expr) or (define (name params...) body...)
static compile_result compile_define(unsigned expr, compile_ctx *cctx)
{
    if (!syntax_arity_checked(expr, 2, UINT_MAX, "define"))
        return emit_syntax_error(cctx, "define: invalid syntax");

    unsigned second = cadr(expr);

    if (IS_PAIR(second)) {
        // (define (name params...) body...) => (define name (lambda ...))
        unsigned name = car(second);
        unsigned params = cdr(second);
        unsigned body = cddr(expr);
        GC_GUARD;
        gc_protect(&name);
        gc_protect(&params);
        gc_protect(&body);
        // lambda_params_valid allocates, so locals must be protected first
        if (!identifier_valid(name) || !body ||
            !list_length_checked(body, NULL, "define") ||
            !lambda_params_valid(params))
            return emit_syntax_error(cctx, "define: invalid syntax");

        // Build lambda expression
        unsigned lambda_atom = atom_from_string("lambda");
        gc_protect(&lambda_atom);
        unsigned lambda_args = alloc_cons(params, body);
        gc_protect(&lambda_args);
        unsigned lambda_expr = alloc_cons(lambda_atom, lambda_args);
        gc_protect(&lambda_expr);

        cctx->tail_position = false;
        compile_expr_internal(lambda_expr, cctx);
        emit2(cctx, OP_DEFINE, CELL_ID(name));

        // Return the name
        emit2(cctx, OP_CONST, code_add_const(cctx->code, name));
    } else {
        if (!identifier_valid(second) || cdddr(expr))
            return emit_syntax_error(cctx, "define: invalid syntax");

        // (define var expr)
        GC_GUARD;
        gc_protect(&second);
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
    if (!syntax_arity_checked(expr, 2, 2, "set!"))
        return emit_syntax_error(cctx, "set!: invalid syntax");

    unsigned var = cadr(expr);
    unsigned val_expr = caddr(expr);
    if (!identifier_valid(var))
        return emit_syntax_error(cctx, "set!: expected variable");

    GC_GUARD;
    gc_protect(&var);
    gc_protect(&val_expr);

    cctx->tail_position = false;
    compile_expr_internal(val_expr, cctx);

    // Use LOCAL_SET if variable is a stack local
    int64_t var_id = CELL_ID(var);
    if (cctx->num_locals > 0) {
        for (int i = 0; i < cctx->num_locals; i++) {
            if (cctx->local_ids[i] == var_id) {
                emit2(cctx, OP_LOCAL_SET, i);
                return dynamic_result();
            }
        }
    }
    emit2(cctx, OP_SET, var_id);

    return dynamic_result();
}

// Function/primitive call with full constant folding
static compile_result compile_call(unsigned expr, compile_ctx *cctx)
{
    unsigned fn_expr = car(expr);
    unsigned args = cdr(expr);

    GC_GUARD;
    gc_protect(&fn_expr);
    gc_protect(&args);

    if (!list_length_checked(args, NULL, "application")) {
        unsigned msg = make_string_copy("malformed application");
        gc_protect(&msg);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, msg));
        emit3(cctx, OP_PRIM, PERROR, 1);
        return dynamic_result();
    }

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
                compile_result arg_results[256] = {0};
                unsigned i = 0;
                unsigned protected_arg_count = 0;
                bool all_const = true;

                unsigned a = args;
                gc_protect(&a);
                while (a) {
                    if (i >= 256) {
                        all_const = false;
                        compile_expr_internal(car(a), cctx);
                        i++;
                        a = cdr(a);
                        continue;
                    }
                    arg_results[i] = compile_expr_internal(car(a), cctx);
                    if (arg_results[i].is_const) {
                        gc_protect(&arg_results[i].value);
                        protected_arg_count++;
                    }
                    if (!arg_results[i].is_const)
                        all_const = false;
                    i++;
                    a = cdr(a);
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

                    // Evaluate primitive at compile time (suppress error output)
                    static FILE *devnull = NULL;
                    if (!devnull) devnull = fopen("/dev/null", "w");
                    FILE *saved_stderr = stderr;
                    if (devnull)
                        stderr = devnull;
                    unsigned result = apply_primitive(prim_id, arg_vals);
                    stderr = saved_stderr;
                    gc_unprotect(1);
                    gc_unprotect((int)protected_arg_count + 1);
                    ctx.last_error[0] = '\0';
                    if (result != TOK_ERROR) {
                        // Success! Emit as constant
                        emit2(cctx, OP_CONST,
                              code_add_const(cctx->code, result));
                        return const_result(result);
                    }
                    // If folding failed (e.g., division by zero), re-compile
                    // normally
                    a = args;
                    gc_protect(&a);
                    while (a) {
                        compile_expr_internal(car(a), cctx);
                        a = cdr(a);
                    }
                    gc_unprotect(1);
                } else {
                    gc_unprotect((int)protected_arg_count + 1);
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
                        arg0_const && IS_NUM(arg_results[0].value)
                            ? CELL_ID(arg_results[0].value)
                            : -999;
                    int64_t val1 =
                        arg1_const && IS_NUM(arg_results[1].value)
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

                // Emit specialized opcodes when constant folding didn't apply
                if (argc == 2) {
                    switch (prim_id) {
                    case PPLUS: emit(cctx, OP_ADD); return dynamic_result();
                    case PMINUS: emit(cctx, OP_SUB); return dynamic_result();
                    case PTIMES: emit(cctx, OP_MUL); return dynamic_result();
                    case PDIV: emit(cctx, OP_DIV); return dynamic_result();
                    case PMOD: emit(cctx, OP_MOD); return dynamic_result();
                    case PLT: emit(cctx, OP_LT); return dynamic_result();
                    case PGT: emit(cctx, OP_GT); return dynamic_result();
                    case PLEQ: emit(cctx, OP_LE); return dynamic_result();
                    case PGEQ: emit(cctx, OP_GE); return dynamic_result();
                    case PEQUAL: emit(cctx, OP_NUMEQ); return dynamic_result();
                    case PCONS: emit(cctx, OP_CONS); return dynamic_result();
                    case PEQ: emit(cctx, OP_EQ); return dynamic_result();
                    default: break;
                    }
                }
                emit3(cctx, OP_PRIM, prim_id, argc);
                return dynamic_result();
            }

            // Non-foldable primitive - compile normally
            cctx->tail_position = false;

            // call/cc captures the VM continuation, so it cannot go through the
            // normal primitive call path.
            if (prim_id == PCALLCC) {
                unsigned a = args;
                gc_protect(&a);
                while (a) {
                    compile_expr_internal(car(a), cctx);
                    a = cdr(a);
                }
                gc_unprotect(1);
                if (argc == 1) {
                    emit(cctx, OP_CALLCC);
                } else {
                    emit3(cctx, OP_PRIM, prim_id, argc);
                }
                return dynamic_result();
            }

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
                // Type predicates
                case PSYMP:
                    emit(cctx, OP_SYMBOLP);
                    return dynamic_result();
                case PNUMP:
                    emit(cctx, OP_NUMBERP);
                    return dynamic_result();
                case PSTRINGP:
                    emit(cctx, OP_STRINGP);
                    return dynamic_result();
                case PVECTORP:
                    emit(cctx, OP_VECTORP);
                    return dynamic_result();
                case PBOOLP:
                    emit(cctx, OP_BOOLEANP);
                    return dynamic_result();
                case PLISTP:
                    emit(cctx, OP_LISTP);
                    return dynamic_result();
                case PINTEGERP:
                    emit(cctx, OP_INTEGERP);
                    return dynamic_result();
                // List operations
                case PLENGTH:
                    emit(cctx, OP_LENGTH);
                    return dynamic_result();
                case PREVERSE:
                    emit(cctx, OP_REVERSE);
                    return dynamic_result();
                // Vector operations
                case PVECLEN:
                    emit(cctx, OP_VECTORLEN);
                    return dynamic_result();
                // Numeric operations
                case PABS:
                    emit(cctx, OP_ABS);
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
                // List operations
                case PAPPEND:
                    emit(cctx, OP_APPEND);
                    return dynamic_result();
                // Vector operations
                case PVECREF:
                    emit(cctx, OP_VECTORREF);
                    return dynamic_result();
                default:
                    emit3(cctx, OP_PRIM, prim_id, 2);
                    return dynamic_result();
                }
            }

            if (argc == 3) {
                if (prim_id == PLIST) {
                    compile_expr_internal(car(args), cctx);
                    compile_expr_internal(cadr(args), cctx);
                    compile_expr_internal(caddr(args), cctx);
                    emit(cctx, OP_LIST3);
                    return dynamic_result();
                }
                if (prim_id == PVECSET) {
                    compile_expr_internal(car(args), cctx);    // vector
                    compile_expr_internal(cadr(args), cctx);   // index
                    compile_expr_internal(caddr(args), cctx);  // value
                    emit(cctx, OP_VECTORSET);
                    return dynamic_result();
                }
            }

            // Compile arguments for general case
            unsigned a = args;
            gc_protect(&a);
            while (a) {
                compile_expr_internal(car(a), cctx);
                a = cdr(a);
            }
            gc_unprotect(1);

            // Special handling for apply - use OP_PRIM to handle variable args
            if (prim_id == PAPPLY) {
                emit3(cctx, OP_PRIM, prim_id, argc);
                return dynamic_result();
            }

            emit3(cctx, OP_PRIM, prim_id, argc);
            return dynamic_result();
        }
    }

    // ========================================================================
    // Loop Optimization: recursive tail call to letrec-bound lambda
    // ========================================================================
    // Compile as SET+JUMP(0) instead of TAILCALL, avoiding closure lookup,
    // arg list building, and environment frame allocation.
    if (IS_ATOM(fn_expr) && cctx->tail_position &&
        cctx->loop_var_id >= 0 &&
        CELL_ID(fn_expr) == cctx->loop_var_id &&
        cctx->env_depth == 0) {
        // Only safe when not inside nested let/begin frames (env_depth == 0)
        // because JUMP 0 would skip their POPENV instructions
        unsigned argc = list_length(args);
        if (argc == cctx->loop_arity && cctx->loop_arity <= 16) {
            // Compile all arguments first (before any SET)
            cctx->tail_position = false;
            unsigned a = args;
            gc_protect(&a);
            while (a) {
                compile_expr_internal(car(a), cctx);
                a = cdr(a);
            }
            gc_unprotect(1);
            cctx->tail_position = true;

            // SET each parameter in reverse order (stack is LIFO)
            // Use LOCAL_SET_VOID if params are stack locals
            unsigned param_ids[16];
            unsigned pi = 0;
            for (unsigned p = cctx->loop_params;
                 p && IS_PAIR(p) && pi < 16; p = cdr(p)) {
                param_ids[pi++] = CELL_ID(car(p));
            }
            for (int j = (int)pi - 1; j >= 0; j--) {
                int local_slot = -1;
                if (cctx->num_locals > 0) {
                    for (int k = 0; k < cctx->num_locals; k++) {
                        if (cctx->local_ids[k] == (int64_t)param_ids[j]) {
                            local_slot = k;
                            break;
                        }
                    }
                }
                if (local_slot >= 0) {
                    emit2(cctx, OP_LOCAL_SET_VOID, local_slot);
                } else {
                    emit2(cctx, OP_SET, param_ids[j]);
                    emit(cctx, OP_POP);
                }
            }
            // Jump to start of this code object (ip = 0)
            emit2(cctx, OP_JUMP, 0);
            return dynamic_result();
        }
    }

    // ========================================================================
    // Escape Analysis Inlining: (f args) where f is a known lambda
    // ========================================================================
    // If f is bound to a lambda in a let, we can inline it at the call site
    if (IS_ATOM(fn_expr) && cctx->known_lambdas) {
        int64_t var_id = CELL_ID(fn_expr);
        unsigned lambda_expr = lookup_known_lambda(var_id, cctx->known_lambdas);
        if (lambda_expr) {
            GC_GUARD;
            // Found a known lambda - construct ((lambda ...) args) and recurse
            // to use the existing lambda inlining code.
            // Remove this binding from known_lambdas to prevent infinite
            // recursion if the lambda calls itself (recursive functions).
            unsigned saved_known = cctx->known_lambdas;
            gc_protect(&saved_known);
            gc_protect(&lambda_expr);
            gc_protect(&args);
            unsigned new_known = 0;
            gc_protect(&new_known);
            unsigned entry = cctx->known_lambdas;
            gc_protect(&entry);
            while (entry) {
                unsigned pair = car(entry);
                gc_protect(&pair);
                if (!IS_PAIR(pair) || !IS_ATOM(car(pair)) ||
                    CELL_ID(car(pair)) != var_id) {
                    new_known = alloc_cons(pair, new_known);
                }
                gc_unprotect(1);
                entry = cdr(entry);
            }
            gc_unprotect(1);
            cctx->known_lambdas = new_known;

            unsigned inlined_call = alloc_cons(lambda_expr, args);
            gc_protect(&inlined_call);
            compile_result result = compile_call(inlined_call, cctx);

            cctx->known_lambdas = saved_known;
            return result;
        }
    }

    // ========================================================================
    // Lambda Inlining: ((lambda (params) body) args) -> inline as let
    // ========================================================================
    // This avoids closure allocation and function call overhead
    if (IS_PAIR(fn_expr) && IS_ATOM(car(fn_expr)) &&
        CELL_ID(car(fn_expr)) == ctx.kw_lambda &&
        builtin_keyword_unbound(ctx.kw_lambda, cctx->env)) {

        if (!syntax_arity_checked(fn_expr, 2, UINT_MAX, "lambda"))
            return emit_syntax_error(cctx, "lambda: invalid syntax");

        unsigned lambda_params = cadr(fn_expr);
        unsigned lambda_body = cddr(fn_expr);
        gc_protect(&lambda_params);
        gc_protect(&lambda_body);
        // lambda_params_valid allocates, so locals must be protected first
        if (!lambda_params_valid(lambda_params) ||
            !list_length_checked(lambda_body, NULL, "lambda"))
            return emit_syntax_error(cctx, "lambda: invalid syntax");

        // Count params and check for rest parameter
        // Handle two cases:
        // 1. (lambda args body) - params is just an atom, all args go to rest
        // 2. (lambda (x y . rest) body) - some fixed params, then rest
        unsigned param_count = 0;
        bool has_rest = false;
        unsigned rest_param = 0;

        if (IS_ATOM(lambda_params)) {
            // Case 1: (lambda args body) - entire params is the rest param
            has_rest = true;
            rest_param = lambda_params;
            param_count = 0;
        } else {
            // Case 2: (lambda (x y ...) body) or (lambda (x y . rest) body)
            for (unsigned p = lambda_params; p; p = IS_PAIR(p) ? cdr(p) : 0) {
                if (IS_ATOM(p)) {
                    // Found rest param at end of dotted list
                    has_rest = true;
                    rest_param = p;
                    break;
                }
                param_count++;
            }
        }

        // Count arguments
        unsigned arg_count = list_length(args);

        // Inline if: exact match, OR rest param with enough args
        bool can_inline = (!has_rest && param_count == arg_count) ||
                          (has_rest && arg_count >= param_count);

        if (can_inline) {
            // Save tail position before compiling arguments
            bool tail = cctx->tail_position;
            cctx->tail_position = false;

            // Compile fixed arguments first
            unsigned args_remaining = args;
            gc_protect(&args_remaining);
            for (unsigned i = 0; i < param_count; i++) {
                compile_expr_internal(car(args_remaining), cctx);
                args_remaining = cdr(args_remaining);
            }

            // For rest parameters, build a list from remaining args
            if (has_rest) {
                unsigned rest_count = arg_count - param_count;
                if (rest_count == 0) {
                    // Empty rest list
                    emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
                } else {
                    // Compile rest args and build list
                    // Stack after compiling: [arg1 arg2 arg3]
                    // We want (arg1 arg2 arg3) = (cons arg1 (cons arg2 (cons arg3 '())))
                    unsigned a = args_remaining;
                    gc_protect(&a);
                    while (a) {
                        compile_expr_internal(car(a), cctx);
                        a = cdr(a);
                    }
                    gc_unprotect(1);
                    // Push nil: [arg1 arg2 arg3 nil]
                    emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
                    // CONS takes car from second-top, cdr from top
                    // After CONS: [arg1 arg2 (arg3 . nil)] = [arg1 arg2 (arg3)]
                    // After CONS: [arg1 (arg2 . (arg3))] = [arg1 (arg2 arg3)]
                    // After CONS: [(arg1 . (arg2 arg3))] = [(arg1 arg2 arg3)]
                    for (unsigned i = 0; i < rest_count; i++) {
                        emit(cctx, OP_CONS);
                    }
                }
            }
            gc_unprotect(1);

            // Push new environment frame
            emit_pushenv(cctx);

            // Dynamically allocate parameter ID array
            unsigned *param_ids = NULL;
            if (param_count > 0) {
                param_ids = checked_malloc_array(param_count, sizeof(unsigned));
                if (!param_ids) {
                    show_error("lambda: out of memory for parameters");
                    emit(cctx, OP_HALT);
                    return dynamic_result();
                }
            }

            // Collect parameter names (only if params is a list)
            unsigned i = 0;
            if (param_count > 0 && IS_PAIR(lambda_params)) {
                for (unsigned p = lambda_params; p && i < param_count;
                     p = cdr(p)) {
                    if (IS_ATOM(p))
                        break; // Stop at rest param
                    param_ids[i++] = CELL_ID(car(p));
                }
            }
            if (i != param_count) {
                free(param_ids);
                return emit_syntax_error(cctx, "lambda: invalid syntax");
            }

            // Define rest param first (it's on top of stack)
            if (has_rest) {
                emit2(cctx, OP_DEFINE, CELL_ID(rest_param));
            }

            // Define fixed params in reverse order (pop from stack)
            if (param_ids) {
                for (int j = (int)param_count - 1; j >= 0; j--) {
                    emit2(cctx, OP_DEFINE, param_ids[j]);
                }
            }

            // Free the parameter ID array
            free(param_ids);

            GC_GUARD;
            // Extend compile-time environment with params for body compilation
            unsigned saved_env = cctx->env;
            gc_protect(&saved_env);
            {
                GC_GUARD;
                gc_protect(&lambda_params);
                gc_protect(&lambda_body);
                gc_protect(&rest_param);
                unsigned frame_vars = 0, frame_vals = 0;
                gc_protect(&frame_vars);
                gc_protect(&frame_vals);

                // Add fixed params (only if lambda_params is a list)
                if (IS_PAIR(lambda_params)) {
                    for (unsigned p = lambda_params; p;
                         p = IS_PAIR(p) ? cdr(p) : 0) {
                        if (IS_ATOM(p))
                            break;
                        unsigned var = car(p);
                        gc_protect(&var);
                        unsigned vc = alloc_cons(var, frame_vars);
                        frame_vars = vc;
                        unsigned val = alloc_cons(0, frame_vals);
                        frame_vals = val;
                        gc_unprotect(1);
                    }
                }

                // Add rest param if present
                if (has_rest) {
                    unsigned vc = alloc_cons(rest_param, frame_vars);
                    frame_vars = vc;
                    unsigned val = alloc_cons(0, frame_vals);
                    frame_vals = val;
                }

                unsigned frame = alloc_cons(frame_vars, frame_vals);
                gc_protect(&frame);
                cctx->env = alloc_cons(frame, cctx->env);
            }

            // Track lambda arguments for escape analysis inlining
            // When ((lambda (var) body) (lambda ...)) is inlined, we can inline
            // calls to var within body if the lambda doesn't self-reference var
            unsigned saved_known = cctx->known_lambdas;
            gc_protect(&saved_known);
            {
                unsigned a = args;
                gc_protect(&a);
                if (IS_PAIR(lambda_params)) {
                    for (unsigned p = lambda_params; p && a;
                         p = IS_PAIR(p) ? cdr(p) : 0) {
                        if (IS_ATOM(p))
                            break;
                        unsigned var = car(p);
                        unsigned val_expr = car(a);
                        // Check if arg is a lambda that doesn't self-reference
                        if (IS_PAIR(val_expr) && IS_ATOM(car(val_expr)) &&
                            CELL_ID(car(val_expr)) == ctx.kw_lambda &&
                            builtin_keyword_unbound(ctx.kw_lambda,
                                                    cctx->env)) {
                            unsigned arg_body = cddr(val_expr);
                            if (!contains_reference(arg_body, CELL_ID(var))) {
                                unsigned pair = alloc_cons(var, val_expr);
                                gc_protect(&pair);
                                cctx->known_lambdas =
                                    alloc_cons(pair, cctx->known_lambdas);
                                gc_unprotect(1);
                            }
                        }
                        a = cdr(a);
                    }
                }
                gc_unprotect(1);
            }

            // Compile body — IIFE params shadow any same-named stack locals
            int saved_num_locals = cctx->num_locals;
            int64_t saved_local_ids[8];
            if (saved_num_locals > 0)
                memcpy(saved_local_ids, cctx->local_ids,
                       sizeof(int64_t) * saved_num_locals);
            // Remove locals that are shadowed by IIFE params
            if (cctx->num_locals > 0 && IS_PAIR(lambda_params)) {
                for (unsigned p = lambda_params; p && IS_PAIR(p); p = cdr(p)) {
                    int64_t pid = CELL_ID(car(p));
                    for (int k = 0; k < cctx->num_locals; k++) {
                        if (cctx->local_ids[k] == pid)
                            cctx->local_ids[k] = -1; // disable this slot
                    }
                }
            }
            cctx->tail_position = tail;
            compile_begin(lambda_body, cctx);
            cctx->num_locals = saved_num_locals;
            if (saved_num_locals > 0)
                memcpy(cctx->local_ids, saved_local_ids,
                       sizeof(int64_t) * saved_num_locals);

            // Restore compile-time environment and known_lambdas
            cctx->env = saved_env;
            cctx->known_lambdas = saved_known;

            // Pop environment frame
            emit_popenv(cctx);

            return dynamic_result();
        }
    }

    // General case: compile function and arguments
    unsigned argc = 0;
    bool tail = cctx->tail_position;

    // Compile arguments first (left to right)
    cctx->tail_position = false;
    unsigned a = args;
    gc_protect(&a);
    while (a) {
        compile_expr_internal(car(a), cctx);
        argc++;
        a = cdr(a);
    }
    gc_unprotect(1);

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

// True when the rest of a template spine is an unquote/unquote-splicing/
// quasiquote tail form, e.g. `(a . ,b) which reads as (a unquote b)
static bool qq_compile_spine_tail_unquote(unsigned l, compile_ctx *cctx)
{
    unsigned h = car(l);
    return (IS_KEYWORD(h, ctx.kw_unquote) &&
            !is_keyword_shadowed(ctx.kw_unquote, cctx->env)) ||
           (IS_KEYWORD(h, ctx.kw_unquote_splicing) &&
            !is_keyword_shadowed(ctx.kw_unquote_splicing, cctx->env)) ||
           (IS_KEYWORD(h, ctx.kw_quasiquote) &&
            !is_keyword_shadowed(ctx.kw_quasiquote, cctx->env));
}

// Check if quasiquote contains unquote or unquote-splicing
static bool qq_has_unquote(unsigned x, compile_ctx *cctx, int depth)
{
    if (IS_VECTOR(x)) {
        unsigned len = vector_len(x);
        for (unsigned i = 0; i < len; i++) {
            if (qq_has_unquote(vector_data_ptr(x)[i], cctx, depth))
                return true;
        }
        return false;
    }

    if (!IS_PAIR(x))
        return false;

    unsigned head = car(x);

    // Found unquote at depth 1
    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(ctx.kw_unquote, cctx->env) && depth == 1)
        return true;
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(ctx.kw_unquote_splicing, cctx->env) &&
        depth == 1)
        return true;

    // Nested quasiquote increases depth
    if (IS_KEYWORD(head, ctx.kw_quasiquote) &&
        !is_keyword_shadowed(ctx.kw_quasiquote, cctx->env))
        return qq_has_unquote(cadr(x), cctx, depth + 1);

    // Unquote decreases depth
    if ((IS_KEYWORD(head, ctx.kw_unquote) &&
         !is_keyword_shadowed(ctx.kw_unquote, cctx->env)) ||
        (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
         !is_keyword_shadowed(ctx.kw_unquote_splicing, cctx->env)))
        return qq_has_unquote(cadr(x), cctx, depth - 1);

    // Check all elements of list
    unsigned l = x;
    for (; IS_PAIR(l); l = cdr(l)) {
        // (... . ,tail): the rest of the spine is an unquote form
        if (l != x && qq_compile_spine_tail_unquote(l, cctx))
            return qq_has_unquote(l, cctx, depth);
        if (qq_has_unquote(car(l), cctx, depth))
            return true;
    }
    if (l && qq_has_unquote(l, cctx, depth))
        return true;
    return false;
}

static bool qq_syntax_valid(unsigned x, compile_ctx *cctx, int depth,
                            unsigned structural_depth)
{
    if (structural_depth >= 1024)
        return false;
    if (IS_VECTOR(x)) {
        unsigned len = vector_len(x);
        for (unsigned i = 0; i < len; i++) {
            if (!qq_syntax_valid(vector_data_ptr(x)[i], cctx, depth,
                                 structural_depth + 1))
                return false;
        }
        return true;
    }

    if (!IS_PAIR(x))
        return true;
    if (pair_chain_is_circular(x))
        return false;

    unsigned head = car(x);
    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(ctx.kw_unquote, cctx->env)) {
        if (!syntax_arity_checked(x, 1, 1, "unquote"))
            return false;
        if (depth == 1)
            return true;
        return qq_syntax_valid(cadr(x), cctx, depth - 1,
                               structural_depth + 1);
    }
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(ctx.kw_unquote_splicing, cctx->env)) {
        if (!syntax_arity_checked(x, 1, 1, "unquote-splicing"))
            return false;
        if (depth == 1)
            return true;
        return qq_syntax_valid(cadr(x), cctx, depth - 1,
                               structural_depth + 1);
    }
    if (IS_KEYWORD(head, ctx.kw_quasiquote) &&
        !is_keyword_shadowed(ctx.kw_quasiquote, cctx->env)) {
        return syntax_arity_checked(x, 1, 1, "quasiquote") &&
               qq_syntax_valid(cadr(x), cctx, depth + 1,
                               structural_depth + 1);
    }

    unsigned l = x;
    for (; IS_PAIR(l); l = cdr(l)) {
        // (... . ,tail) reads as (... unquote tail): the rest of the spine
        // is an unquote form, not further elements
        if (l != x && qq_compile_spine_tail_unquote(l, cctx))
            return qq_syntax_valid(l, cctx, depth, structural_depth + 1);
        if (!qq_syntax_valid(car(l), cctx, depth, structural_depth + 1))
            return false;
    }
    if (l && !qq_syntax_valid(l, cctx, depth, structural_depth + 1))
        return false;

    return true;
}

static void compile_qq_error(compile_ctx *cctx, const char *message)
{
    unsigned msg = make_string_copy(message);
    gc_protect(&msg);
    emit2(cctx, OP_CONST, code_add_const(cctx->code, msg));
    emit3(cctx, OP_PRIM, PERROR, 1);
    gc_unprotect(1);
}

// Compile quasiquote recursively - generates runtime code
static void compile_qq_rec(unsigned x, compile_ctx *cctx, int depth)
{
    // The compile calls below allocate (macro expansion, env frames), so the
    // template and traversal cursor must stay rooted across them
    GC_GUARD;
    gc_protect(&x);

    if (IS_VECTOR(x)) {
        unsigned len = vector_len(x);
        for (unsigned i = 0; i < len; i++) {
            compile_qq_rec(vector_data_ptr(x)[i], cctx, depth);
        }
        emit3(cctx, OP_PRIM, PVECTOR, len);
        return;
    }

    if (!IS_PAIR(x)) {
        // Non-pair: just quote it
        emit2(cctx, OP_CONST, code_add_const(cctx->code, x));
        return;
    }

    unsigned head = car(x);

    // (unquote expr) at depth 1: compile the expression
    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(ctx.kw_unquote, cctx->env) && depth == 1) {
        compile_expr_internal(cadr(x), cctx);
        return;
    }

    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(ctx.kw_unquote_splicing, cctx->env) &&
        depth == 1) {
        compile_qq_error(cctx, "unquote-splicing not inside list");
        return;
    }

    // (unquote expr) at depth > 1: decrement depth and wrap result
    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(ctx.kw_unquote, cctx->env) && depth > 1) {
        compile_qq_rec(cadr(x), cctx, depth - 1);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_unquote));
        emit(cctx, OP_SWAP);
        emit(cctx, OP_LIST1);
        emit(cctx, OP_CONS);
        return;
    }

    // (unquote-splicing expr) at depth > 1: decrement depth and wrap result
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(ctx.kw_unquote_splicing, cctx->env) &&
        depth > 1) {
        compile_qq_rec(cadr(x), cctx, depth - 1);
        emit2(cctx, OP_CONST,
              code_add_const(cctx->code, ctx.atom_unquote_splicing));
        emit(cctx, OP_SWAP);
        emit(cctx, OP_LIST1);
        emit(cctx, OP_CONS);
        return;
    }

    // Nested quasiquote: keep structure but recurse with depth+1
    if (IS_KEYWORD(head, ctx.kw_quasiquote) &&
        !is_keyword_shadowed(ctx.kw_quasiquote, cctx->env)) {
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
        if (l != x && qq_compile_spine_tail_unquote(l, cctx))
            break; // (... . ,tail): rest of spine is a tail form
        unsigned elem = car(l);
        if (IS_PAIR(elem) && IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
            !is_keyword_shadowed(ctx.kw_unquote_splicing, cctx->env) &&
            depth == 1) {
            has_splice = true;
            break;
        }
    }

    unsigned cursor = 0;
    gc_protect(&cursor);
    if (has_splice) {
        // Use append-based building for splicing
        // Start with empty list
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));

        bool tail_done = false;
        for (cursor = x; IS_PAIR(cursor); cursor = cdr(cursor)) {
            if (cursor != x && qq_compile_spine_tail_unquote(cursor, cctx)) {
                // (... . ,tail): compile the tail form and append it
                compile_qq_rec(cursor, cctx, depth);
                emit3(cctx, OP_PRIM, PAPPEND, 2);
                tail_done = true;
                break;
            }
            unsigned elem = car(cursor);
            if (IS_PAIR(elem) &&
                IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
                !is_keyword_shadowed(ctx.kw_unquote_splicing, cctx->env) &&
                depth == 1) {
                // ,@expr: compile expr and append
                compile_expr_internal(cadr(elem), cctx);
                emit3(cctx, OP_PRIM, PAPPEND, 2);
                // append permits its final argument to be improper, but a
                // quasiquote splice must be a proper list. Appending an empty
                // list makes the splice result a non-final argument on this
                // second call, where append validates it.
                emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
                emit3(cctx, OP_PRIM, PAPPEND, 2);
            } else {
                // Regular element: compile, wrap in list, append
                compile_qq_rec(elem, cctx, depth);
                emit(cctx, OP_LIST1);
                emit3(cctx, OP_PRIM, PAPPEND, 2);
            }
        }

        if (!tail_done) {
            unsigned tail = x;
            while (IS_PAIR(tail))
                tail = cdr(tail);
            if (tail) {
                compile_qq_rec(tail, cctx, depth);
                emit3(cctx, OP_PRIM, PAPPEND, 2);
            }
        }
    } else {
        // No splicing: build with cons from the end
        // First compile all elements, then cons them together
        unsigned len = 0;
        bool tail_done = false;
        for (cursor = x; IS_PAIR(cursor); cursor = cdr(cursor)) {
            if (cursor != x && qq_compile_spine_tail_unquote(cursor, cctx)) {
                // (... . ,tail): the rest of the spine is the tail value
                compile_qq_rec(cursor, cctx, depth);
                tail_done = true;
                break;
            }
            compile_qq_rec(car(cursor), cctx, depth);
            len++;
        }
        if (!tail_done) {
            // Handle improper list tail
            unsigned tail = x;
            while (IS_PAIR(tail))
                tail = cdr(tail);
            if (tail) {
                compile_qq_rec(tail, cctx, depth);
            } else {
                emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
            }
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
    if (!syntax_arity_checked(expr, 1, 1, "quasiquote"))
        return emit_syntax_error(cctx, "quasiquote: invalid syntax");

    unsigned tmpl = cadr(expr);
    if (!qq_syntax_valid(tmpl, cctx, 1, 0))
        return emit_syntax_error(cctx, "quasiquote: invalid syntax");

    // Check if quasiquote has any unquotes that need runtime evaluation
    if (!qq_has_unquote(tmpl, cctx, 1)) {
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
    GC_GUARD;
    gc_protect(&expr);
    gc_protect(&env);

    compile_ctx *cctx = cctx_new(NULL, env);
    if (!cctx)
        return NULL;
    cctx->tail_position = false;

    compile_expr_internal(expr, cctx);
    emit(cctx, OP_HALT);

    code_object *result = cctx->code;
    peephole_optimize(result);
    cctx_free(cctx);
    return result;
}
