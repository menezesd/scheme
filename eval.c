/**
 * @file eval.c
 * @brief Trampoline-based CPS evaluator for Scheme
 *
 * This file implements the core evaluation loop using continuation-passing
 * style (CPS) with a trampoline to achieve proper tail call optimization.
 *
 * ## Evaluation Model
 * Instead of recursive C function calls, evaluation proceeds by:
 * 1. Setting the global `tramp` state (mode, expr/value, env, cont)
 * 2. Returning to the main loop
 * 3. The loop dispatches based on tramp.mode:
 *    - TRAMP_EVAL: Call eval_step() to evaluate an expression
 *    - TRAMP_APPLY: Call apply_cont_step() to apply value to continuation
 *    - TRAMP_DONE/TRAMP_ERROR: Exit the loop
 *
 * This design ensures tail calls don't grow the C stack, regardless of
 * recursion depth. A tail call simply sets up tramp state and returns.
 *
 * ## Continuations
 * Continuations are linked lists of frames, each containing:
 * - Type: What operation is pending (CONT_IF, CONT_EVAL_ARGS, etc.)
 * - Data: Operation-specific data (remaining exprs, accumulated values, etc.)
 * - Env: Environment for the pending operation
 * - Next: Link to the outer continuation
 *
 * ## Special Forms
 * Special forms (if, lambda, define, let, etc.) are handled via dispatch
 * to handlers in eval_forms.c.
 *
 * ## Function Application
 * 1. Evaluate function expression (CONT_EVAL_FN)
 * 2. Evaluate arguments left-to-right (CONT_EVAL_ARGS)
 * 3. Apply function to evaluated arguments (apply_function)
 */

#include "eval.h"
#include "bytecode.h"
#include "eval_internal.h"
#include "writer.h"
#include <stdlib.h>

// ============================================================================
// Forward Declarations
// ============================================================================

#define MAX_EVAL_MACRO_EXPANSION_DEPTH 1000
#define MAX_QUASIQUOTE_SYNTAX_DEPTH 1024

static void eval_step(void);
static void apply_cont_step(void);

static unsigned eval_macro_expansion_depth = 0;

bool eval_note_macro_expansion(void)
{
    if (eval_macro_expansion_depth >= MAX_EVAL_MACRO_EXPANSION_DEPTH) {
        show_error("macro expansion exceeded maximum depth");
        return false;
    }

    eval_macro_expansion_depth++;
    return true;
}

void eval_reset_macro_expansion_depth(void)
{
    eval_macro_expansion_depth = 0;
}

// ============================================================================
// Quasiquote Expansion
// ============================================================================

static bool qq_spine_tail_unquote(unsigned l, unsigned env);

static bool qq_syntax_valid(unsigned x, unsigned env, int depth,
                            unsigned structural_depth)
{
    if (structural_depth >= MAX_QUASIQUOTE_SYNTAX_DEPTH) {
        show_error("quasiquote: template nesting too deep");
        return false;
    }
    if (IS_VECTOR(x)) {
        unsigned len = vector_len(x);
        for (unsigned i = 0; i < len; i++) {
            if (!qq_syntax_valid(vector_data_ptr(x)[i], env, depth,
                                 structural_depth + 1))
                return false;
        }
        return true;
    }

    if (!IS_PAIR(x))
        return true;
    if (pair_chain_is_circular(x)) {
        show_error("quasiquote: circular template");
        return false;
    }

    unsigned head = car(x);
    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(ctx.kw_unquote, env)) {
        if (!syntax_arity_checked(x, 1, 1, "unquote"))
            return false;
        if (depth == 1)
            return true;
        return qq_syntax_valid(cadr(x), env, depth - 1,
                               structural_depth + 1);
    }
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(ctx.kw_unquote_splicing, env)) {
        if (!syntax_arity_checked(x, 1, 1, "unquote-splicing"))
            return false;
        if (depth == 1)
            return true;
        return qq_syntax_valid(cadr(x), env, depth - 1,
                               structural_depth + 1);
    }
    if (IS_KEYWORD(head, ctx.kw_quasiquote) &&
        !is_keyword_shadowed(ctx.kw_quasiquote, env)) {
        return syntax_arity_checked(x, 1, 1, "quasiquote") &&
               qq_syntax_valid(cadr(x), env, depth + 1,
                               structural_depth + 1);
    }

    unsigned l = x;
    for (; IS_PAIR(l); l = cdr(l)) {
        // (... . ,tail) reads as (... unquote tail): the rest of the spine
        // is an unquote form, not further elements
        if (l != x && qq_spine_tail_unquote(l, env))
            return qq_syntax_valid(l, env, depth, structural_depth + 1);
        if (!qq_syntax_valid(car(l), env, depth, structural_depth + 1))
            return false;
    }
    if (l && !qq_syntax_valid(l, env, depth, structural_depth + 1))
        return false;

    return true;
}

// Helper for nested quasiquote expansion with depth tracking
static unsigned qq_expand_depth(unsigned x, unsigned env, int depth)
{
    if (IS_VECTOR(x)) {
        GC_GUARD;
        gc_protect(&x);
        gc_protect(&env);
        unsigned len = vector_len(x);
        unsigned result = make_vector(len, 0);
        if (result == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&result);
        for (unsigned i = 0; i < len; i++) {
            unsigned elem = vector_data_ptr(x)[i];
            gc_protect(&elem);
            unsigned expanded = qq_expand_depth(elem, env, depth);
            if (expanded == TOK_ERROR)
                return TOK_ERROR;
            vector_set_elem(result, i, expanded);
            gc_unprotect(1);
        }
        return result;
    }

    if (!IS_PAIR(x)) {
        return x;
    }

    GC_GUARD;
    unsigned head = car(x);
    gc_protect(&x);
    gc_protect(&env);
    gc_protect(&head);

    // Check for (unquote expr) - only if unquote is not shadowed
    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(ctx.kw_unquote, env)) {
        if (depth == 1) {
            // Evaluate the unquote
            return eval_cps(cadr(x), env);
        } else {
            // Keep the unquote but expand inside at lower depth
            unsigned inner = qq_expand_depth(cadr(x), env, depth - 1);
            if (inner == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&inner);
            unsigned result_tail = alloc_cons(inner, 0);
            gc_protect(&result_tail);
            unsigned result = alloc_cons(head, result_tail);
            return result;
        }
    }

    // Check for (unquote-splicing expr) - only if not shadowed
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(ctx.kw_unquote_splicing, env)) {
        if (depth == 1) {
            show_error("unquote-splicing not inside list");
            return TOK_ERROR;
        } else {
            // Keep but expand inside at lower depth
            unsigned inner = qq_expand_depth(cadr(x), env, depth - 1);
            if (inner == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&inner);
            unsigned result_tail = alloc_cons(inner, 0);
            gc_protect(&result_tail);
            unsigned result = alloc_cons(head, result_tail);
            return result;
        }
    }

    // Check for nested (quasiquote expr) - only if not shadowed
    if (IS_KEYWORD(head, ctx.kw_quasiquote) &&
        !is_keyword_shadowed(ctx.kw_quasiquote, env)) {
        unsigned inner = qq_expand_depth(cadr(x), env, depth + 1);
        if (inner == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&inner);
        unsigned result_tail = alloc_cons(inner, 0);
        gc_protect(&result_tail);
        unsigned result = alloc_cons(head, result_tail);
        return result;
    }

    // Recursively process list
    unsigned result = 0, tail = 0;
    gc_protect(&result);
    gc_protect(&tail);
    unsigned l = x;
    gc_protect(&l);
    for (; IS_PAIR(l); l = cdr(l)) {
        // (... . ,tail) reads as (... unquote tail): the rest of the spine
        // is a tail form, handled by the improper-tail expansion below
        if (l != x && qq_spine_tail_unquote(l, env))
            break;
        unsigned elem = car(l);

        // Check if element is (unquote-splicing expr) - only if not shadowed
        if (IS_PAIR(elem) && IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
            !is_keyword_shadowed(ctx.kw_unquote_splicing, env)) {
            if (depth == 1) {
                // Evaluate and splice
                unsigned spliced = eval_cps(cadr(elem), env);
                if (spliced == TOK_ERROR) {
                    return spliced;
                }
                gc_protect(&spliced);
                if (!list_length_checked(spliced, NULL,
                                         "unquote-splicing")) {
                    return TOK_ERROR;
                }
                for (; IS_PAIR(spliced); spliced = cdr(spliced)) {
                    list_append(&result, &tail, car(spliced));
                }
                gc_unprotect(1); // spliced - per-iteration cleanup
            } else {
                // Keep but expand inside. Rebuild with the original head
                // atom cell (consing the raw keyword id would put an
                // invalid cell reference into the result)
                unsigned inner = qq_expand_depth(cadr(elem), env, depth - 1);
                if (inner == TOK_ERROR) {
                    return inner;
                }
                gc_protect(&inner);
                unsigned kept_tail = alloc_cons(inner, 0);
                gc_protect(&kept_tail);
                unsigned kept = alloc_cons(car(elem), kept_tail);
                gc_unprotect(2); // kept_tail, inner - per-iteration cleanup
                list_append(&result, &tail, kept);
            }
        } else {
            unsigned expanded = qq_expand_depth(elem, env, depth);
            if (expanded == TOK_ERROR) {
                return expanded;
            }
            list_append(&result, &tail, expanded);
        }
    }
    if (l) {
        unsigned expanded_tail = qq_expand_depth(l, env, depth);
        if (expanded_tail == TOK_ERROR)
            return TOK_ERROR;
        if (tail)
            cell_set_cdr(tail, expanded_tail);
        else
            result = expanded_tail;
    }
    return result;
}

unsigned qq_expand_cps(unsigned x, unsigned env)
{
    if (!qq_syntax_valid(x, env, 1, 0))
        return TOK_ERROR;
    return qq_expand_depth(x, env, 1);
}

// ============================================================================
// Quasiquote Transformation
// ============================================================================
// Transform a quasiquote template into an ordinary expression built from
// primitive applications (cons/append/vector) and quote forms. Evaluating
// that expression through the normal trampoline keeps first-class
// continuations working across unquotes: the old approach of running a
// nested evaluator inside the expander could neither escape from nor
// re-enter the quasiquote when a continuation fired.

static unsigned qq_quote_expr(unsigned x)
{
    GC_GUARD;
    gc_protect(&x);
    unsigned tail = alloc_cons(x, 0);
    gc_protect(&tail);
    return alloc_cons(ctx.atom_quote, tail);
}

// Build (op a b) with op a builtin primitive value, immune to shadowing
static unsigned qq_call2(int64_t prim, unsigned a, unsigned b)
{
    GC_GUARD;
    gc_protect(&a);
    gc_protect(&b);
    unsigned args = alloc_cons(b, 0);
    gc_protect(&args);
    args = alloc_cons(a, args);
    gc_protect(&args);
    unsigned op = mk_primop(prim);
    gc_protect(&op);
    return alloc_cons(op, args);
}

// Build code producing (sym . (inner-value . ())) for reconstructing
// nested unquote/quasiquote forms at depth > 1
static unsigned qq_keyword_pair_expr(unsigned sym_atom, unsigned inner_expr)
{
    GC_GUARD;
    gc_protect(&sym_atom);
    gc_protect(&inner_expr);
    unsigned nil_expr = qq_quote_expr(0);
    gc_protect(&nil_expr);
    unsigned tail_expr = qq_call2(PCONS, inner_expr, nil_expr);
    gc_protect(&tail_expr);
    unsigned sym_expr = qq_quote_expr(sym_atom);
    gc_protect(&sym_expr);
    return qq_call2(PCONS, sym_expr, tail_expr);
}

// True when the rest of a template spine is an unquote/unquote-splicing/
// quasiquote tail form, e.g. `(a . ,b) which reads as (a unquote b)
static bool qq_spine_tail_unquote(unsigned l, unsigned env)
{
    unsigned h = car(l);
    return (IS_KEYWORD(h, ctx.kw_unquote) &&
            !is_keyword_shadowed(ctx.kw_unquote, env)) ||
           (IS_KEYWORD(h, ctx.kw_unquote_splicing) &&
            !is_keyword_shadowed(ctx.kw_unquote_splicing, env)) ||
           (IS_KEYWORD(h, ctx.kw_quasiquote) &&
            !is_keyword_shadowed(ctx.kw_quasiquote, env));
}

static unsigned qq_transform(unsigned x, unsigned env, int depth)
{
    if (IS_VECTOR(x)) {
        GC_GUARD;
        gc_protect(&x);
        gc_protect(&env);
        unsigned len = vector_len(x);
        unsigned args = 0, args_tail = 0;
        gc_protect(&args);
        gc_protect(&args_tail);
        for (unsigned i = 0; i < len; i++) {
            unsigned elem = vector_data_ptr(x)[i];
            gc_protect(&elem);
            if (depth == 1 && IS_PAIR(elem) &&
                IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
                !is_keyword_shadowed(ctx.kw_unquote_splicing, env)) {
                show_error("unquote-splicing not inside list");
                return TOK_ERROR;
            }
            unsigned e = qq_transform(elem, env, depth);
            if (e == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&e);
            list_append(&args, &args_tail, e);
            gc_unprotect(2);
        }
        unsigned op = mk_primop(PVECTOR);
        gc_protect(&op);
        return alloc_cons(op, args);
    }

    if (!IS_PAIR(x))
        return qq_quote_expr(x);

    GC_GUARD;
    gc_protect(&x);
    gc_protect(&env);
    unsigned head = car(x);
    gc_protect(&head);

    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(ctx.kw_unquote, env)) {
        if (depth == 1)
            return cadr(x);
        unsigned inner = qq_transform(cadr(x), env, depth - 1);
        if (inner == TOK_ERROR)
            return TOK_ERROR;
        return qq_keyword_pair_expr(head, inner);
    }
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(ctx.kw_unquote_splicing, env)) {
        if (depth == 1) {
            show_error("unquote-splicing not inside list");
            return TOK_ERROR;
        }
        unsigned inner = qq_transform(cadr(x), env, depth - 1);
        if (inner == TOK_ERROR)
            return TOK_ERROR;
        return qq_keyword_pair_expr(head, inner);
    }
    if (IS_KEYWORD(head, ctx.kw_quasiquote) &&
        !is_keyword_shadowed(ctx.kw_quasiquote, env)) {
        unsigned inner = qq_transform(cadr(x), env, depth + 1);
        if (inner == TOK_ERROR)
            return TOK_ERROR;
        return qq_keyword_pair_expr(head, inner);
    }

    // General list: collect the spine iteratively (flat templates can be
    // long), transform the tail, then fold back with cons/append
    unsigned rev_elems = 0;
    gc_protect(&rev_elems);
    unsigned l = x;
    gc_protect(&l);
    while (IS_PAIR(l)) {
        if (l != x && qq_spine_tail_unquote(l, env))
            break; // (... . ,tail) - the rest is an unquote form
        rev_elems = alloc_cons(car(l), rev_elems);
        l = cdr(l);
    }

    unsigned result = IS_PAIR(l) ? qq_transform(l, env, depth)
                                 : qq_quote_expr(l);
    if (result == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&result);

    for (unsigned r = rev_elems; r; r = cdr(r)) {
        unsigned elem = car(r);
        gc_protect(&elem);
        if (depth == 1 && IS_PAIR(elem) &&
            IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
            !is_keyword_shadowed(ctx.kw_unquote_splicing, env)) {
            // ,@e: splice - append validates e is a proper list
            result = qq_call2(PAPPEND, cadr(elem), result);
        } else {
            unsigned elem_expr = qq_transform(elem, env, depth);
            if (elem_expr == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&elem_expr);
            result = qq_call2(PCONS, elem_expr, result);
            gc_unprotect(1);
        }
        gc_unprotect(1);
    }
    return result;
}

unsigned qq_transform_cps(unsigned x, unsigned env)
{
    if (!qq_syntax_valid(x, env, 1, 0))
        return TOK_ERROR;
    return qq_transform(x, env, 1);
}

// Route a C-level error through the Scheme exception system (CPS mirror of
// vm_signal_error): build an error object and apply the current
// *current-exception-handler* with a CONT_ERR_RETURN continuation. A handler
// that jumps to a continuation (guard, call/cc) replaces this continuation
// via its captured frame chain; one that returns normally lands in
// handle_cont_err_return and terminates (R7RS raise semantics).
static void cps_signal_error(const char *msg, unsigned env, unsigned cont)
{
    if (!msg)
        msg = "unknown error";
    unsigned handler = lookup_silent(intern("*current-exception-handler*"),
                                     env);
    if (handler == TOK_ERROR) {
        tramp_error();
        return;
    }
    GC_GUARD;
    gc_protect(&handler);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned error_obj = make_error_object_c(msg, env);
    if (error_obj == TOK_ERROR) {
        tramp_error();
        return;
    }
    gc_protect(&error_obj);
    unsigned args = alloc_cons(error_obj, 0);
    gc_protect(&args);
    unsigned err_cont = make_cont(CONT_ERR_RETURN, handler, env, cont);
    apply_function(handler, args, env, err_cont);
}

// ============================================================================
// CPS Evaluator - Eval Step
// ============================================================================

static void eval_step(void)
{
    unsigned id = tramp.expr;
    unsigned env = tramp.env;
    unsigned cont = tramp.cont;

    if (!id) {
        eval_reset_macro_expansion_depth();
        tramp_apply(0, cont);
        return;
    }
    if (IS_FIXNUM(id)) {
        eval_reset_macro_expansion_depth();
        tramp_apply(id, cont);
        return;
    }
    if (!IS_CELL(id)) {
        show_error("invalid expression");
        tramp_error();
        return;
    }

    switch (CELL_TYPE(id)) {
    case BT_NUM:
    case BT_STRING:
    case BT_CHAR:
    case BT_VECTOR:
    case BT_INPORT:
    case BT_OUTPORT:
    case BT_FUNCTION:
    case BT_MACRO:
    case BT_BUILTIN:
    case BT_CONT:
        // Self-evaluating
        eval_reset_macro_expansion_depth();
        tramp_apply(id, cont);
        return;

    case BT_ATOM: {
        if (id == ctx.atom_true || id == ctx.atom_false) {
            eval_reset_macro_expansion_depth();
            tramp_apply(id, cont);
            return;
        }

        eval_reset_macro_expansion_depth();
        unsigned val = lookup(CELL_ID(id), env);
        if (val == TOK_ERROR) {
            cps_signal_error(ctx.last_error[0] ? ctx.last_error
                                               : "undefined variable",
                             env, cont);
            return;
        }
        tramp_apply(val, cont);
        return;
    }

    case BT_CONS: {
        unsigned head = car(id);

        // Check for special forms
        if (IS_ATOM(head)) {
            int64_t kw = CELL_ID(head);
            if (dispatch_special_form(kw, id, env, cont)) {
                return;
            }
        }

        // Not a special form - evaluate function position first
        eval_reset_macro_expansion_depth();
        // Protect head, arg_exprs, env and cont - use pointer version for GC
        // safety
        GC_GUARD;
        unsigned arg_exprs = cdr(id);
        if (!list_length_checked(arg_exprs, NULL, "application")) {
            tramp_error();
            return;
        }
        gc_protect(&head);
        gc_protect(&arg_exprs);
        gc_protect(&env);
        gc_protect(&cont);
        unsigned k =
            make_cont_from_protected(CONT_EVAL_FN, &arg_exprs, &env, &cont);
        tramp_eval(head, env, k);
        return;
    }

    case BT_FREE:
        show_error("attempt to evaluate free memory");
        tramp_error();
        return;

    default:
        tramp_apply(id, cont);
        return;
    }
}

// ============================================================================
// CPS Evaluator - Apply Continuation Step
// ============================================================================

static void apply_cont_step(void)
{
    unsigned val = tramp.value;
    unsigned k = tramp.cont;

    if (!IS_CONT(k)) {
        show_error("invalid continuation");
        tramp_error();
        return;
    }

    enum cont_type type = cont_type(k);
    unsigned data = cont_data(k);
    unsigned env = cont_env(k);
    unsigned next = cont_next(k);

    // Dispatch via function pointer table
    if (type < CONT_COUNT && cont_handlers[type]) {
        cont_handlers[type](val, data, env, next);
    } else {
        show_error("unknown continuation type: %d", type);
        tramp_error();
    }
}

// ============================================================================
// Function Application
// ============================================================================

void apply_function(unsigned fn, unsigned args, unsigned env, unsigned cont)
{
    if (IS_BUILTIN(fn)) {
        int64_t prim_id = CELL_ID(fn);

        // Special handling for call/cc
        if (prim_id == PCALLCC) {
            if (!args || cdr(args)) {
                show_error("call/cc expects exactly one argument");
                tramp_error();
                return;
            }
            GC_GUARD;
            unsigned proc = car(args);
            gc_protect(&proc);
            gc_protect(&cont);
            gc_protect(&env);
            unsigned cont_args = alloc_cons(cont, 0);
            apply_function(proc, cont_args, env, cont);
            return;
        }

        // Special handling for apply
        // (apply proc arg1 arg2 ... arglist) - prepend args to arglist
        if (prim_id == PAPPLY) {
            if (!args || !cdr(args)) {
                show_error("apply expects at least two arguments");
                tramp_error();
                return;
            }
            GC_GUARD;
            unsigned proc = car(args);
            args = cdr(args);

            // Find the last argument (must be a list) and collect preceding
            // args
            unsigned proc_args = 0;
            gc_protect(&proc);
            gc_protect(&proc_args);
            gc_protect(&args);

            // Build list of all args except last, then append last
            unsigned prefix = 0;
            gc_protect(&prefix);
            while (cdr(args)) {
                prefix = alloc_cons(car(args), prefix);
                args = cdr(args);
            }
            // args is now the last element, car(args) is the final list
            proc_args = car(args);
            // Every apply target, including a continuation, must receive a
            // proper argument list.  Builtins and closures validate this
            // later, but continuations otherwise turn a cyclic list into an
            // invalid multiple-values object.
            if (!list_length_checked(proc_args, NULL, "apply")) {
                tramp_error();
                return;
            }

            // Prepend the reversed prefix to proc_args
            while (prefix) {
                proc_args = alloc_cons(car(prefix), proc_args);
                prefix = cdr(prefix);
            }

            apply_function(proc, proc_args, env, cont);
            return;
        }

        // Special handling for call-with-values
        if (prim_id == PCALLWITHVALUES) {
            if (!args || !cdr(args) || cddr(args)) {
                show_error("call-with-values expects exactly two arguments");
                tramp_error();
                return;
            }
            GC_GUARD;
            unsigned producer = car(args);
            unsigned consumer = cadr(args);
            // Protect producer, consumer, env and cont across make_cont
            gc_protect(&producer);
            gc_protect(&consumer);
            gc_protect(&env);
            gc_protect(&cont);
            // Create continuation to handle producer's result
            unsigned k = make_cont(CONT_CALLWITHVALUES, consumer, env, cont);
            // Call producer with no arguments
            apply_function(producer, 0, env, k);
            return;
        }

        // Special handling for raise - CPS mirror of vm_handle_raise: pass the
        // object to the current handler with a CONT_ERR_RETURN continuation
        // whose data records the dispatched handler, so handle_cont_err_return
        // can tell the default handler (silent unhandled) from a user handler
        // (R7RS violation). Cannot go through apply_primitive - raising is
        // engine machinery, not a pure primitive.
        if (prim_id == PRAISENOW) {
            if (!args || cdr(args)) {
                show_error("raise expects exactly one argument");
                tramp_error();
                return;
            }
            unsigned obj = car(args);
            unsigned handler = lookup_silent(
                intern("*current-exception-handler*"), env);
            if (handler == TOK_ERROR) {
                tramp_error();
                return;
            }
            GC_GUARD;
            gc_protect(&handler);
            gc_protect(&obj);
            gc_protect(&env);
            gc_protect(&cont);
            unsigned err_cont = make_cont(CONT_ERR_RETURN, handler, env, cont);
            unsigned raise_args = alloc_cons(obj, 0);
            apply_function(handler, raise_args, env, err_cont);
            return;
        }

        // Special handling for eval
        if (prim_id == PEVAL) {
            if (!args || !cdr(args) || cddr(args)) {
                show_error("eval expects exactly two arguments");
                tramp_error();
                return;
            }
            unsigned expr = car(args);
            unsigned eval_env = cadr(args);
            // Evaluate expression in the given environment
            tramp_eval(expr, eval_env, cont);
            return;
        }

        // Special handling for interaction-environment
        if (prim_id == PINTERACTIONENV) {
            if (args) {
                show_error("interaction-environment expects no arguments");
                tramp_error();
                return;
            }
            // Return the current environment
            tramp_apply(env, cont);
            return;
        }

        // Special handling for gc-flip
        if (prim_id == PGCFLIP) {
            if (args) {
                show_error("gc-flip expects no arguments");
                tramp_error();
                return;
            }
            // Trigger garbage collection with environment as root
            // Set tramp state so gc() collects the right things
            tramp.cont = cont;
            tramp.env = env;
            gc(env);
            // After gc(), use updated tramp values
            tramp_apply(ctx.atom_true, tramp.cont);
            return;
        }

        // Special handling for load
        if (prim_id == PLOAD) {
            if (!args || cdr(args)) {
                show_error("load expects exactly one argument");
                tramp_error();
                return;
            }
            unsigned filename_arg = car(args);
            if (!IS_STRING(filename_arg)) {
                show_error("load: expected string argument");
                tramp_error();
                return;
            }
            const char *filename = GET_STRING_PTR(filename_arg);
            if (!string_is_registered(filename)) {
                show_error("load: invalid string");
                tramp_error();
                return;
            }
            const char *old_filename = reader_get_filename();
            char *filename_copy = checked_string_copy(filename);
            if (!filename_copy) {
                show_error("load: out of memory");
                tramp_error();
                return;
            }
            FILE *f = fopen(filename, "r");
            if (!f) {
                show_error("load: cannot open file: %s", filename);
                free(filename_copy);
                tramp_error();
                return;
            }
            reader_set_filename(filename_copy);
            reader_reset_position();

            unsigned exprs = 0, exprs_tail = 0;
            bool load_failed = false;
            GC_GUARD;
            gc_protect(&exprs);
            gc_protect(&exprs_tail);
            for (;;) {
                unsigned expr = read_obj_port(f);
                if (expr == TOK_ERROR) {
                    load_failed = true;
                    break;
                }
                if (is_eof_object(expr))
                    break;
                if (expr == TOK_CLOSE || expr == TOK_DOT) {
                    show_error("load: unexpected reader token");
                    load_failed = true;
                    break;
                }
                list_append(&exprs, &exprs_tail, expr);
            }
            reader_forget_port(f);
            if (fclose(f) != 0) {
                show_error("load: close failed");
                load_failed = true;
            }
            reader_set_filename(old_filename);
            reader_reset_position();
            free(filename_copy);

            if (load_failed) {
                tramp_error();
                return;
            }

            if (!exprs) {
                tramp_apply(0, cont);
                return;
            }

            if (!cdr(exprs)) {
                tramp_eval(car(exprs), env, cont);
            } else {
                GC_GUARD;
                unsigned first_expr = car(exprs);
                unsigned rest = cdr(exprs);
                gc_protect(&first_expr);
                gc_protect(&rest);
                gc_protect(&env);
                gc_protect(&cont);
                unsigned k = make_cont(CONT_BEGIN, rest, env, cont);
                tramp_eval(first_expr, env, k);
            }
            return;
        }

        // Protect cont across apply_primitive (which can allocate/trigger GC)
        {
            GC_GUARD;
            gc_protect(&args);
            gc_protect(&cont);
            unsigned result = apply_primitive(prim_id, args);
            if (result == TOK_ERROR) {
                cps_signal_error(ctx.last_error[0] ? ctx.last_error
                                                   : "primitive error",
                                 env, cont);
                return;
            }
            tramp_apply(result, cont);
            return;
        }
    }

    if (IS_FUNCTION(fn)) {
        GC_GUARD;
        unsigned params = car(fn);
        unsigned body_env = cdr(fn);
        unsigned body = car(body_env);
        unsigned def_env = cdr(body_env);

        // Protect all values across allocations (including params and args for
        // bind_params)
        gc_protect(&params);
        gc_protect(&args);
        gc_protect(&body);
        gc_protect(&def_env);
        gc_protect(&cont);
        unsigned frame = 0;
        gc_protect(&frame);
        frame = bind_params(params, args);
        if (frame == TOK_ERROR) {
            tramp_error();
            return;
        }
        unsigned new_env = alloc_cons(frame, def_env);

        if (!cdr(body)) {
            tramp_eval(car(body), new_env, cont);
        } else {
            unsigned first_expr = car(body);
            unsigned rest_body = cdr(body);
            gc_protect(&first_expr);
            gc_protect(&rest_body);
            gc_protect(&new_env);
            unsigned k = make_cont(CONT_APPLY_FUNC, rest_body, new_env, cont);
            tramp_eval(first_expr, new_env, k);
        }
        return;
    }

    if (IS_CONT(fn)) {
        GC_GUARD;
        gc_protect(&fn);
        gc_protect(&args);
        unsigned value = values_from_list(args);
        tramp_apply(value, fn);
        return;
    }

    // Handle bytecode VM closures (for CPS/bytecode interop)
    if (is_bytecode_closure_object(fn)) {
        GC_GUARD;
        gc_protect(&fn);
        gc_protect(&args);
        gc_protect(&cont);
        unsigned result = vm_call_closure(fn, args);
        gc_protect(&result);
        if (result == TOK_ERROR) {
            tramp_error();
            return;
        }
        tramp_apply(result, cont);
        return;
    }

    show_error("not a procedure, got %s", type_name(fn));
    tramp_error();
}

// ============================================================================
// Main Evaluator Entry Points
// ============================================================================

unsigned eval_cps(unsigned expr, unsigned env)
{
    // Protect expr and env across make_halt_cont() which can trigger GC
    GC_GUARD;
    gc_protect(&expr);
    gc_protect(&env);
    eval_reset_macro_expansion_depth();
    unsigned halt_k = make_halt_cont();
    tramp_eval(expr, env, halt_k);

    while (tramp.mode != TRAMP_DONE && tramp.mode != TRAMP_ERROR) {
        if (tramp.mode == TRAMP_EVAL) {
            eval_step();
        } else if (tramp.mode == TRAMP_APPLY) {
            apply_cont_step();
        }
    }

    if (tramp.mode == TRAMP_ERROR) {
        return TOK_ERROR;
    }
    return tramp.value;
}

unsigned eval_obj(unsigned id, unsigned env)
{
    return eval_cps(id, env);
}
