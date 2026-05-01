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

// Look up a variable in the known_lambdas alist
// Returns the lambda expression if found, 0 otherwise
static unsigned lookup_known_lambda(int64_t var_id, unsigned known_lambdas)
{
    FORLIST(entry, known_lambdas)
    {
        unsigned pair = car(entry);
        if (IS_PAIR(pair) && IS_ATOM(car(pair)) &&
            CELL_ID(car(pair)) == var_id) {
            return cdr(pair); // Return the lambda expression
        }
    }
    return 0;
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

    // Skip quoted expressions
    if (IS_ATOM(car(expr)) && CELL_ID(car(expr)) == ctx.kw_quote)
        return false;

    // Check lambda - don't descend if var_id is shadowed by a parameter
    if (IS_ATOM(car(expr)) && CELL_ID(car(expr)) == ctx.kw_lambda) {
        unsigned params = cadr(expr);
        // Check if var_id is in params
        if (CELL_TYPE(params) == BT_ATOM && CELL_ID(params) == var_id)
            return false; // Shadowed by rest param
        if (IS_PAIR(params)) {
            for (unsigned p = params; p; p = cdr(p)) {
                if (CELL_TYPE(p) == BT_ATOM && CELL_ID(p) == var_id)
                    return false; // Shadowed by rest param
                if (IS_PAIR(p) && IS_ATOM(car(p)) && CELL_ID(car(p)) == var_id)
                    return false; // Shadowed by regular param
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
    // Inherit known_lambdas from parent (they're still in scope)
    cctx->known_lambdas = parent ? parent->known_lambdas : 0;
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

static void emit4(compile_ctx *cctx, unsigned op, unsigned arg1, unsigned arg2,
                  unsigned arg3)
{
    code_emit(cctx->code, op);
    code_emit(cctx->code, arg1);
    code_emit(cctx->code, arg2);
    code_emit(cctx->code, arg3);
}

#define IC_UNCACHED 0xFFFFFFFF

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
                                           unsigned collected, int64_t ellipsis);
static unsigned rename_template_vars(unsigned tmpl, unsigned rename_map);

// Collect free variables from a syntax-rules template
// pattern_vars: list of pattern variable atoms
// Returns: list of free variable atoms
static unsigned collect_template_free_vars(unsigned tmpl, unsigned pattern_vars,
                                           unsigned collected, int64_t ellipsis)
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
        // Skip special forms
        if (is_special_form(id))
            return collected;
        return alloc_cons(tmpl, collected);
    }

    if (IS_PAIR(tmpl)) {
        // Skip quote's contents
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quote)
            return collected;
        collected =
            collect_template_free_vars(car(tmpl), pattern_vars, collected, ellipsis);
        return collect_template_free_vars(cdr(tmpl), pattern_vars, collected, ellipsis);
    }

    if (CELL_TYPE(tmpl) == BT_VECTOR) {
        unsigned len = vector_len(tmpl);
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointer each iteration - GC may have moved tmpl
            unsigned *data = vector_data_ptr(tmpl);
            collected = collect_template_free_vars(data[i],
                                                   pattern_vars, collected, ellipsis);
        }
        return collected;
    }

    return collected;
}

// Rename free variables in template according to rename_map
// rename_map: list of (orig_atom . gensym_atom)
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
        // Skip quote's contents
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quote)
            return tmpl;
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        unsigned new_car = rename_template_vars(car(tmpl), rename_map);
        gc_protect(&new_car);
        unsigned new_cdr = rename_template_vars(cdr(tmpl), rename_map);
        gc_unprotect(3);
        if (new_car == car(tmpl) && new_cdr == cdr(tmpl))
            return tmpl;
        return alloc_cons(new_car, new_cdr);
    }

    if (CELL_TYPE(tmpl) == BT_VECTOR) {
        unsigned len = vector_len(tmpl);
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        unsigned new_vec = make_vector(len, 0);
        gc_protect(&new_vec);
        bool changed = false;
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointers each iteration - GC may have moved vectors
            unsigned old_elem = vector_data_ptr(tmpl)[i];
            unsigned new_elem = rename_template_vars(old_elem, rename_map);
            vector_data_ptr(new_vec)[i] = new_elem;
            if (new_elem != old_elem)
                changed = true;
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
        if (id == ellipsis || id == ctx.kw_underscore)
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
        collected = collect_pattern_vars(car(pattern), collected, ellipsis, literals);
        return collect_pattern_vars(cdr(pattern), collected, ellipsis, literals);
    }

    if (CELL_TYPE(pattern) == BT_VECTOR) {
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
        snprintf(name, sizeof(name), "g%u", g);
        unsigned atom = atom_from_string(name);
        int64_t gensym_id = CELL_ID(atom);

        // Look up the gensym value in the compile-time environment
        unsigned val = lookup_silent(gensym_id, cctx->env);
        if (val != TOK_ERROR) {
            // Emit const and define
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
        emit4(cctx, OP_LOOKUP, CELL_ID(expr), IC_UNCACHED, IC_UNCACHED);
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

                // First pass: create frame with placeholder values for letrec-syntax
                if (kw == ctx.kw_letrec_syntax) {
                    unsigned frame = alloc_cons(0, 0);
                    new_env = alloc_cons(frame, cctx->env);
                }

                int64_t ellipsis_id = ctx.kw_ellipsis;

                // First pass: collect all free variables from all templates
                for (unsigned b = bindings; b; b = cdr(b)) {
                    unsigned binding = car(b);
                    unsigned transformer_form = cadr(binding);

                    if (!IS_PAIR(transformer_form) ||
                        !IS_KEYWORD(car(transformer_form), ctx.kw_syntax_rules))
                        continue;

                    unsigned literals = cadr(transformer_form);
                    unsigned rules = cddr(transformer_form);

                    for (unsigned r = rules; r; r = cdr(r)) {
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

                        // Collect free variables from template
                        unsigned free_vars = collect_template_free_vars(
                            tmpl, pattern_vars, 0, ellipsis_id);
                        gc_unprotect(1);

                        // For each free var, create gensym if bound in outer scope
                        for (unsigned fv = free_vars; fv; fv = cdr(fv)) {
                            unsigned var_atom = car(fv);
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
                                continue;

                            // Check if bound in outer scope (compile-time env)
                            unsigned val = lookup_silent(var_id, cctx->env);
                            if (val != TOK_ERROR) {
                                // Create gensym
                                extern unsigned gensym_counter;
                                char name[20];
                                snprintf(name, sizeof(name), "g%u", gensym_counter++);
                                unsigned gensym_atom = atom_from_string(name);
                                gc_protect(&gensym_atom);

                                // Add to rename map: (var_atom . gensym_atom)
                                unsigned entry = alloc_cons(var_atom, gensym_atom);
                                rename_map = alloc_cons(entry, rename_map);

                                // Record for runtime binding: (gensym_id . var_id)
                                unsigned bind_entry = alloc_cons(gensym_atom, var_atom);
                                gensym_bindings = alloc_cons(bind_entry, gensym_bindings);

                                gc_unprotect(1);
                            }
                        }
                    }
                }

                // Second pass: create transformers with renamed templates
                for (unsigned b = bindings; b; b = cdr(b)) {
                    unsigned binding = car(b);
                    unsigned name = car(binding);
                    unsigned transformer_form = cadr(binding);

                    // Check it's a syntax-rules form
                    if (!IS_PAIR(transformer_form) ||
                        !IS_KEYWORD(car(transformer_form), ctx.kw_syntax_rules)) {
                        show_error("%s: expected syntax-rules",
                                   kw == ctx.kw_let_syntax ? "let-syntax"
                                                           : "letrec-syntax");
                        emit(cctx, OP_HALT);
                        return dynamic_result();
                    }

                    // Rename free vars in templates
                    unsigned renamed_form = transformer_form;
                    if (rename_map) {
                        gc_protect(&transformer_form);
                        gc_protect(&renamed_form);

                        // Rebuild syntax-rules with renamed templates
                        unsigned literals = cadr(transformer_form);
                        unsigned rules = cddr(transformer_form);
                        gc_protect(&literals);
                        gc_protect(&rules);

                        unsigned renamed_rules = 0;
                        gc_protect(&renamed_rules);
                        unsigned renamed_tail = 0;
                        gc_protect(&renamed_tail);

                        for (unsigned r = rules; r; r = cdr(r)) {
                            unsigned rule = car(r);
                            unsigned pattern = car(rule);
                            unsigned tmpl = cadr(rule);

                            unsigned renamed_tmpl = rename_template_vars(tmpl, rename_map);
                            gc_protect(&renamed_tmpl);
                            unsigned new_rule = alloc_cons(pattern, alloc_cons(renamed_tmpl, 0));
                            gc_unprotect(1);

                            unsigned new_cell = alloc_cons(new_rule, 0);
                            if (!renamed_rules) {
                                renamed_rules = new_cell;
                                renamed_tail = new_cell;
                            } else {
                                CELL_CDR(renamed_tail) = new_cell;
                                renamed_tail = new_cell;
                            }
                        }

                        unsigned sr_atom = car(transformer_form);
                        renamed_form = alloc_cons(sr_atom,
                                          alloc_cons(literals, renamed_rules));
                        gc_unprotect(6);
                    }

                    // Create syntax transformer with renamed form
                    // Use empty closure_env since free vars are pre-renamed
                    unsigned transformer = make_syntax_transformer(renamed_form, 0);

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

                // Push a new runtime frame for the body
                emit(cctx, OP_PUSHENV);

                // Emit gensym bindings: look up original var, define gensym
                // This happens at let-syntax entry, BEFORE any inner scopes
                for (unsigned gb = gensym_bindings; gb; gb = cdr(gb)) {
                    unsigned entry = car(gb);
                    unsigned gensym_atom = car(entry);
                    unsigned var_atom = cdr(entry);
                    int64_t gensym_id = CELL_ID(gensym_atom);
                    int64_t var_id = CELL_ID(var_atom);

                    emit4(cctx, OP_LOOKUP, var_id, IC_UNCACHED, IC_UNCACHED);
                    emit2(cctx, OP_DEFINE, gensym_id);
                }

                // Add gensyms to compile-time env so they're visible in body
                if (gensym_bindings) {
                    unsigned gensym_frame_vars = 0, gensym_frame_vals = 0;
                    gc_protect(&gensym_frame_vars);
                    gc_protect(&gensym_frame_vals);

                    for (unsigned gb = gensym_bindings; gb; gb = cdr(gb)) {
                        unsigned entry = car(gb);
                        unsigned gensym_atom = car(entry);
                        // Use a placeholder value - actual value is set at runtime
                        gensym_frame_vars = alloc_cons(gensym_atom, gensym_frame_vars);
                        gensym_frame_vals = alloc_cons(gensym_atom, gensym_frame_vals);
                    }

                    unsigned gensym_frame = alloc_cons(gensym_frame_vars, gensym_frame_vals);
                    new_env = alloc_cons(gensym_frame, new_env);
                    gc_unprotect(2);
                }

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

    // Build a dummy frame with parameters as vars, all bound to 0
    {
        GC_GUARD;
        gc_protect(&lambda_env);
        gc_protect(&params);
        gc_protect(&body);
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
    }

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

    // Count bindings
    unsigned count = 0;
    FORLIST(b, bindings) { count++; }

    // Compile all values first (in current environment)
    cctx->tail_position = false;
    FORLIST(b, bindings)
    {
        compile_expr_internal(cadr(car(b)), cctx);
    }

    // Push new environment frame
    emit(cctx, OP_PUSHENV);

    // Dynamically allocate variable array
    unsigned *vars = NULL;
    if (count > 0) {
        vars = malloc(count * sizeof(unsigned));
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
    cctx->env = extend_compile_env(cctx->env, bindings);

    // Track lambda bindings for escape analysis inlining
    // Save and extend known_lambdas for bindings of form (var (lambda ...))
    // Only track non-self-referential lambdas (self-ref needs letrec semantics)
    unsigned saved_known = cctx->known_lambdas;
    FORLIST(b, bindings)
    {
        unsigned binding = car(b);
        unsigned var = car(binding);
        unsigned val_expr = cadr(binding);

        // Check if value is a lambda expression
        if (IS_PAIR(val_expr) && IS_ATOM(car(val_expr)) &&
            CELL_ID(car(val_expr)) == ctx.kw_lambda &&
            !is_keyword_shadowed(ctx.kw_lambda, cctx->env)) {
            // Only inline if the lambda doesn't reference the bound variable
            // (self-referential lambdas need letrec semantics to work)
            unsigned lambda_body = cddr(val_expr);
            if (!contains_reference(lambda_body, CELL_ID(var))) {
                // Add (var . lambda-expr) to known_lambdas
                unsigned pair = alloc_cons(var, val_expr);
                cctx->known_lambdas = alloc_cons(pair, cctx->known_lambdas);
            }
        }
    }

    // Compile body in tail position
    cctx->tail_position = true;
    compile_result result = compile_begin(body, cctx);

    // Restore compile-time environment and known_lambdas
    cctx->env = saved_env;
    cctx->known_lambdas = saved_known;

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

    // Count expressions
    unsigned expr_count = 0;
    FORLIST(a, args) { expr_count++; }

    bool tail = cctx->tail_position;
    unsigned saved_pos = cctx->code->code_len;

    // Dynamically allocate jump array
    unsigned *false_jumps = malloc(expr_count * sizeof(unsigned));
    if (!false_jumps) {
        show_error("and: out of memory");
        emit(cctx, OP_HALT);
        return dynamic_result();
    }
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
                free(false_jumps);
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

    free(false_jumps);
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

    // Count expressions
    unsigned expr_count = 0;
    FORLIST(a, args) { expr_count++; }

    bool tail = cctx->tail_position;
    unsigned saved_pos = cctx->code->code_len;

    // Dynamically allocate jump array
    unsigned *true_jumps = malloc(expr_count * sizeof(unsigned));
    if (!true_jumps) {
        show_error("or: out of memory");
        emit(cctx, OP_HALT);
        return dynamic_result();
    }
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
                free(true_jumps);
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

    free(true_jumps);
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

    // Count clauses
    unsigned clause_count = 0;
    FORLIST(c, clauses) { clause_count++; }

    bool tail = cctx->tail_position;

    // Dynamically allocate end jump array
    unsigned *end_jumps = malloc(clause_count * sizeof(unsigned));
    if (!end_jumps) {
        show_error("cond: out of memory");
        emit(cctx, OP_HALT);
        return dynamic_result();
    }
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

    free(end_jumps);
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
                    if (i >= 256) {
                        all_const = false;
                        compile_expr_internal(car(a), cctx);
                        i++;
                        continue;
                    }
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

                    // (* x 0) or (* 0 x) -> 0
                    // (* x 1) or (* 1 x) -> x (identity)
                    if (prim_id == PTIMES) {
                        if ((arg0_const && val0 == 0) ||
                            (arg1_const && val1 == 0)) {
                            cctx->code->code_len = saved_pos;
                            emit2(cctx, OP_CONST,
                                  code_add_const(cctx->code, store(0)));
                            return const_result(store(0));
                        }
                        if (arg1_const && val1 == 1) {
                            cctx->code->code_len = saved_pos;
                            return compile_expr_internal(car(args), cctx);
                        }
                        if (arg0_const && val0 == 1) {
                            cctx->code->code_len = saved_pos;
                            return compile_expr_internal(cadr(args), cctx);
                        }
                    }

                    // (/ x 1) -> x (identity)
                    if (prim_id == PDIV) {
                        if (arg1_const && val1 == 1) {
                            cctx->code->code_len = saved_pos;
                            return compile_expr_internal(car(args), cctx);
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

    // ========================================================================
    // Escape Analysis Inlining: (f args) where f is a known lambda
    // ========================================================================
    // If f is bound to a lambda in a let, we can inline it at the call site
    if (IS_ATOM(fn_expr) && cctx->known_lambdas) {
        int64_t var_id = CELL_ID(fn_expr);
        unsigned lambda_expr = lookup_known_lambda(var_id, cctx->known_lambdas);
        if (lambda_expr) {
            // Found a known lambda - construct ((lambda ...) args) and recurse
            // to use the existing lambda inlining code.
            // Remove this binding from known_lambdas to prevent infinite
            // recursion if the lambda calls itself (recursive functions).
            unsigned saved_known = cctx->known_lambdas;
            unsigned new_known = 0;
            FORLIST(entry, cctx->known_lambdas)
            {
                unsigned pair = car(entry);
                if (!IS_PAIR(pair) || !IS_ATOM(car(pair)) ||
                    CELL_ID(car(pair)) != var_id) {
                    new_known = alloc_cons(pair, new_known);
                }
            }
            cctx->known_lambdas = new_known;

            unsigned inlined_call = alloc_cons(lambda_expr, args);
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
        !is_keyword_shadowed(ctx.kw_lambda, cctx->env)) {

        unsigned lambda_params = cadr(fn_expr);
        unsigned lambda_body = cddr(fn_expr);

        // Count params and check for rest parameter
        // Handle two cases:
        // 1. (lambda args body) - params is just an atom, all args go to rest
        // 2. (lambda (x y . rest) body) - some fixed params, then rest
        unsigned param_count = 0;
        bool has_rest = false;
        unsigned rest_param = 0;

        if (CELL_TYPE(lambda_params) == BT_ATOM) {
            // Case 1: (lambda args body) - entire params is the rest param
            has_rest = true;
            rest_param = lambda_params;
            param_count = 0;
        } else {
            // Case 2: (lambda (x y ...) body) or (lambda (x y . rest) body)
            for (unsigned p = lambda_params; p; p = cdr(p)) {
                if (CELL_TYPE(p) == BT_ATOM) {
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
                    FORLIST(a, args_remaining)
                    {
                        compile_expr_internal(car(a), cctx);
                    }
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

            // Push new environment frame
            emit(cctx, OP_PUSHENV);

            // Dynamically allocate parameter ID array
            unsigned *param_ids = NULL;
            if (param_count > 0) {
                param_ids = malloc(param_count * sizeof(unsigned));
                if (!param_ids) {
                    show_error("lambda: out of memory for parameters");
                    emit(cctx, OP_HALT);
                    return dynamic_result();
                }
            }

            // Collect parameter names (only if params is a list)
            unsigned i = 0;
            if (CELL_TYPE(lambda_params) == BT_CONS) {
                for (unsigned p = lambda_params; p; p = cdr(p)) {
                    if (CELL_TYPE(p) == BT_ATOM)
                        break; // Stop at rest param
                    param_ids[i++] = CELL_ID(car(p));
                }
            }

            // Define rest param first (it's on top of stack)
            if (has_rest) {
                emit2(cctx, OP_DEFINE, CELL_ID(rest_param));
            }

            // Define fixed params in reverse order (pop from stack)
            for (int j = (int)param_count - 1; j >= 0; j--) {
                emit2(cctx, OP_DEFINE, param_ids[j]);
            }

            // Free the parameter ID array
            free(param_ids);

            // Extend compile-time environment with params for body compilation
            unsigned saved_env = cctx->env;
            {
                GC_GUARD;
                gc_protect(&lambda_params);
                gc_protect(&lambda_body);
                gc_protect(&rest_param);
                unsigned frame_vars = 0, frame_vals = 0;
                gc_protect(&frame_vars);
                gc_protect(&frame_vals);

                // Add fixed params (only if lambda_params is a list)
                if (CELL_TYPE(lambda_params) == BT_CONS) {
                    for (unsigned p = lambda_params; p; p = cdr(p)) {
                        if (CELL_TYPE(p) == BT_ATOM)
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
                cctx->env = alloc_cons(frame, cctx->env);
            }

            // Track lambda arguments for escape analysis inlining
            // When ((lambda (var) body) (lambda ...)) is inlined, we can inline
            // calls to var within body if the lambda doesn't self-reference var
            unsigned saved_known = cctx->known_lambdas;
            {
                unsigned a = args;
                if (CELL_TYPE(lambda_params) == BT_CONS) {
                    for (unsigned p = lambda_params; p && a; p = cdr(p)) {
                        if (CELL_TYPE(p) == BT_ATOM)
                            break;
                        unsigned var = car(p);
                        unsigned val_expr = car(a);
                        // Check if arg is a lambda that doesn't self-reference
                        if (IS_PAIR(val_expr) && IS_ATOM(car(val_expr)) &&
                            CELL_ID(car(val_expr)) == ctx.kw_lambda &&
                            !is_keyword_shadowed(ctx.kw_lambda, cctx->env)) {
                            unsigned arg_body = cddr(val_expr);
                            if (!contains_reference(arg_body, CELL_ID(var))) {
                                unsigned pair = alloc_cons(var, val_expr);
                                cctx->known_lambdas =
                                    alloc_cons(pair, cctx->known_lambdas);
                            }
                        }
                        a = cdr(a);
                    }
                }
            }

            // Compile body in tail position
            cctx->tail_position = tail;
            compile_begin(lambda_body, cctx);

            // Restore compile-time environment and known_lambdas
            cctx->env = saved_env;
            cctx->known_lambdas = saved_known;

            // Pop environment frame
            emit(cctx, OP_POPENV);

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


