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

// ============================================================================
// Forward Declarations
// ============================================================================

static void eval_step(void);
static void apply_cont_step(void);

// ============================================================================
// Quasiquote Expansion
// ============================================================================

// Helper to check if a keyword is shadowed in the environment
static bool is_keyword_shadowed(int64_t kw, unsigned env)
{
    return lookup_silent(kw, env) != TOK_ERROR;
}

// Helper for nested quasiquote expansion with depth tracking
static unsigned qq_expand_depth(unsigned x, unsigned env, int depth)
{
    if (!IS_PAIR(x)) {
        return x;
    }

    unsigned head = car(x);

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
            unsigned result = alloc_cons(head, alloc_cons(inner, 0));
            gc_unprotect(1);
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
            unsigned result = alloc_cons(head, alloc_cons(inner, 0));
            gc_unprotect(1);
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
        unsigned result = alloc_cons(head, alloc_cons(inner, 0));
        gc_unprotect(1);
        return result;
    }

    // Recursively process list
    unsigned result = 0, tail = 0;
    gc_protect(&result);
    gc_protect(&tail);
    for (unsigned l = x; IS_PAIR(l); l = cdr(l)) {
        gc_protect(&l);
        unsigned elem = car(l);

        // Check if element is (unquote-splicing expr) - only if not shadowed
        if (IS_PAIR(elem) && IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
            !is_keyword_shadowed(ctx.kw_unquote_splicing, env)) {
            if (depth == 1) {
                // Evaluate and splice
                unsigned spliced = eval_cps(cadr(elem), env);
                if (spliced == TOK_ERROR) {
                    gc_unprotect(3);
                    return spliced;
                }
                gc_protect(&spliced);
                for (; IS_PAIR(spliced); spliced = cdr(spliced)) {
                    list_append(&result, &tail, car(spliced));
                }
                gc_unprotect(1);
            } else {
                // Keep but expand inside
                unsigned inner = qq_expand_depth(cadr(elem), env, depth - 1);
                if (inner == TOK_ERROR) {
                    gc_unprotect(3);
                    return inner;
                }
                gc_protect(&inner);
                unsigned kept =
                    alloc_cons(ctx.kw_unquote_splicing, alloc_cons(inner, 0));
                gc_unprotect(1);
                list_append(&result, &tail, kept);
            }
        } else {
            unsigned expanded = qq_expand_depth(elem, env, depth);
            if (expanded == TOK_ERROR) {
                gc_unprotect(3);
                return expanded;
            }
            list_append(&result, &tail, expanded);
        }
        gc_unprotect(1); // l
    }
    gc_unprotect(2); // tail, result
    return result;
}

unsigned qq_expand_cps(unsigned x, unsigned env)
{
    return qq_expand_depth(x, env, 1);
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
        tramp_apply(0, cont);
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
        tramp_apply(id, cont);
        return;

    case BT_ATOM: {
        unsigned val = lookup(CELL_ID(id), env);
        if (val == TOK_ERROR) {
            tramp_error();
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
        // Protect head, arg_exprs, env and cont - use pointer version for GC
        // safety
        unsigned arg_exprs = cdr(id);
        gc_protect(&head);
        gc_protect(&arg_exprs);
        gc_protect(&env);
        gc_protect(&cont);
        unsigned k =
            make_cont_from_protected(CONT_EVAL_FN, &arg_exprs, &env, &cont);
        gc_unprotect(4);
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
            unsigned proc = car(args);
            gc_protect(&proc);
            gc_protect(&cont);
            gc_protect(&env);
            unsigned cont_args = alloc_cons(cont, 0);
            gc_unprotect(3);
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

            // Prepend the reversed prefix to proc_args
            while (prefix) {
                proc_args = alloc_cons(car(prefix), proc_args);
                prefix = cdr(prefix);
            }
            gc_unprotect(4);

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
            unsigned producer = car(args);
            unsigned consumer = cadr(args);
            // Protect producer, consumer, env and cont across make_cont
            gc_protect(&producer);
            gc_protect(&consumer);
            gc_protect(&env);
            gc_protect(&cont);
            // Create continuation to handle producer's result
            unsigned k = make_cont(CONT_CALLWITHVALUES, consumer, env, cont);
            gc_unprotect(4);
            // Call producer with no arguments
            apply_function(producer, 0, env, k);
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
            char *filename = GET_STRING_PTR(car(args));
            FILE *old_stdin = stdin;
            FILE *f = fopen(filename, "r");
            if (!f) {
                show_error("load: cannot open file: %s", filename);
                tramp_error();
                return;
            }
            stdin = f;

            unsigned exprs = 0, exprs_tail = 0;
            for (;;) {
                int c = getchar();
                while (c != EOF && isspace(c))
                    c = getchar();
                if (c == EOF)
                    break;
                ungetc(c, stdin);
                reader_reset_labels();
                unsigned expr = read_obj();
                if (expr == TOK_ERROR) {
                    fclose(f);
                    stdin = old_stdin;
                    tramp_error();
                    return;
                }
                list_append(&exprs, &exprs_tail, expr);
            }
            fclose(f);
            stdin = old_stdin;

            if (!exprs) {
                tramp_apply(0, cont);
                return;
            }

            if (!cdr(exprs)) {
                tramp_eval(car(exprs), env, cont);
            } else {
                unsigned first_expr = car(exprs);
                unsigned rest = cdr(exprs);
                gc_protect(&first_expr);
                gc_protect(&rest);
                gc_protect(&env);
                gc_protect(&cont);
                unsigned k = make_cont(CONT_BEGIN, rest, env, cont);
                gc_unprotect(4);
                tramp_eval(first_expr, env, k);
            }
            return;
        }

        // Protect cont across apply_primitive (which can allocate/trigger GC)
        gc_protect(&cont);
        unsigned result = apply_primitive(prim_id, args);
        gc_unprotect(1);
        if (result == TOK_ERROR) {
            tramp_error();
            return;
        }
        tramp_apply(result, cont);
        return;
    }

    if (IS_FUNCTION(fn)) {
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
        unsigned new_env = alloc_cons(frame, def_env);

        if (!cdr(body)) {
            gc_unprotect(6); // params, args, body, def_env, cont, frame
            tramp_eval(car(body), new_env, cont);
        } else {
            unsigned first_expr = car(body);
            unsigned rest_body = cdr(body);
            gc_protect(&first_expr);
            gc_protect(&rest_body);
            gc_protect(&new_env);
            unsigned k = make_cont(CONT_APPLY_FUNC, rest_body, new_env, cont);
            gc_unprotect(9); // first_expr, rest_body, new_env, frame, cont,
                             // def_env, body, args, params
            tramp_eval(first_expr, new_env, k);
        }
        return;
    }

    if (IS_CONT(fn)) {
        if (!args || cdr(args)) {
            show_error("continuation expects exactly one argument");
            tramp_error();
            return;
        }
        tramp_apply(car(args), fn);
        return;
    }

    // Handle bytecode VM closures (for CPS/bytecode interop)
    if (IS_PAIR(fn) && CELL_TYPE(car(fn)) == BT_CLOSURE) {
        unsigned result = vm_call_closure(fn, args);
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
    gc_protect(&expr);
    gc_protect(&env);
    unsigned halt_k = make_halt_cont();
    gc_unprotect(2);
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

unsigned eval_obj(unsigned id, unsigned env) { return eval_cps(id, env); }
