/**
 * @file eval_cont.c
 * @brief Continuation handlers for the evaluator
 *
 * This file implements handlers for each continuation type. When an expression
 * finishes evaluating, its value is passed to the current continuation via
 * apply_cont_step(), which dispatches to the appropriate handler here.
 *
 * ## Handler Parameters
 * All handlers receive:
 * - val: The value being passed to this continuation
 * - data: Operation-specific data saved when continuation was created
 * - env: Environment saved when continuation was created
 * - next: The outer continuation to pass results to
 *
 * ## GC Safety
 * Handlers must protect local variables across allocations. The val, data,
 * env, and next parameters are already on the shadow stack.
 */

#include "eval_internal.h"

// ============================================================================
// Continuation Handlers
// ============================================================================

/**
 * CONT_HALT: Top-level continuation signaling evaluation is complete.
 */
void handle_cont_halt(unsigned val, unsigned data, unsigned env, unsigned next)
{
    (void)data;
    (void)env;
    (void)next;
    tramp_done(val);
}

/**
 * CONT_IF: Choose branch based on test result.
 * data = (then-expr . else-expr)
 */
void handle_cont_if(unsigned val, unsigned data, unsigned env, unsigned next)
{
    if (IS_TRUTHY(val)) {
        tramp_eval(car(data), env, next);
    } else {
        tramp_eval(cdr(data), env, next);
    }
}

/**
 * CONT_BEGIN: Evaluate remaining expressions in sequence.
 * data = remaining expressions list
 */
void handle_cont_begin(unsigned val, unsigned data, unsigned env, unsigned next)
{
    (void)val;
    eval_seq(data, CONT_BEGIN, env, next);
}

/**
 * CONT_SET: Store value in existing binding.
 * data = variable atom
 */
void handle_cont_set(unsigned val, unsigned data, unsigned env, unsigned next)
{
    unsigned result = setvar(CELL_ID(data), val, env);
    if (result == TOK_ERROR) {
        tramp_error();
        return;
    }
    tramp_apply(val, next);
}

/**
 * CONT_DEFINE: Create new binding in current frame.
 * data = variable atom
 */
void handle_cont_define(unsigned val, unsigned data, unsigned env,
                        unsigned next)
{
    gc_protect(&next);
    defvar(data, val, env);
    gc_unprotect(1);
    tramp_apply(data, next);
}

/**
 * CONT_AND: Short-circuit and - stop on #f, continue otherwise.
 * data = remaining expressions
 */
void handle_cont_and(unsigned val, unsigned data, unsigned env, unsigned next)
{
    if (IS_FALSE(val)) {
        tramp_apply(ctx.atom_false, next);
        return;
    }
    eval_seq(data, CONT_AND, env, next);
}

/**
 * CONT_OR: Short-circuit or - stop on truthy, continue otherwise.
 * data = remaining expressions
 */
void handle_cont_or(unsigned val, unsigned data, unsigned env, unsigned next)
{
    if (IS_TRUTHY(val)) {
        tramp_apply(val, next);
        return;
    }
    eval_seq(data, CONT_OR, env, next);
}

/**
 * CONT_COND_TEST: Process cond clause based on test result.
 * data = (consequent-exprs . remaining-clauses)
 * If val is truthy, evaluate consequent. Otherwise try next clause.
 * Supports => receiver syntax: (test => receiver) calls receiver with test
 * value.
 */
void handle_cont_cond_test(unsigned val, unsigned data, unsigned env,
                           unsigned next)
{
    unsigned conseq = car(data);
    unsigned rest = cdr(data);
    if (IS_TRUTHY(val)) {
        if (!conseq) {
            // No consequent: return test value
            tramp_apply(val, next);
        } else if (IS_KEYWORD(car(conseq), ctx.kw_arrow) &&
                   lookup_silent(ctx.kw_arrow, env) == TOK_ERROR) {
            // (test => receiver) syntax - evaluate receiver, then call with val
            // Only if => is not lexically bound (R5RS hygiene)
            unsigned receiver_expr = cadr(conseq);
            gc_protect(&val);
            gc_protect(&receiver_expr);
            gc_protect(&env);
            gc_protect(&next);
            unsigned k2 = make_cont(CONT_COND_ARROW, val, env, next);
            gc_unprotect(4);
            tramp_eval(receiver_expr, env, k2);
        } else if (!cdr(conseq)) {
            tramp_eval(car(conseq), env, next);
        } else {
            unsigned first_expr = car(conseq);
            unsigned rest_conseq = cdr(conseq);
            gc_protect(&first_expr);
            gc_protect(&rest_conseq);
            gc_protect(&env);
            gc_protect(&next);
            unsigned k2 = make_cont(CONT_BEGIN, rest_conseq, env, next);
            gc_unprotect(4);
            tramp_eval(first_expr, env, k2);
        }
    } else {
        if (!rest) {
            tramp_apply(0, next);
        } else {
            unsigned clause = car(rest);
            unsigned test = car(clause);
            if (IS_KEYWORD(test, ctx.kw_else)) {
                unsigned conseq2 = cdr(clause);
                if (!conseq2) {
                    tramp_apply(ctx.atom_true, next);
                } else if (!cdr(conseq2)) {
                    tramp_eval(car(conseq2), env, next);
                } else {
                    // Extract first expr and rest before allocation, protect
                    // all used after
                    unsigned first_expr2 = car(conseq2);
                    unsigned rest_conseq2 = cdr(conseq2);
                    gc_protect(&first_expr2);
                    gc_protect(&rest_conseq2);
                    gc_protect(&env);
                    gc_protect(&next);
                    unsigned k2 =
                        make_cont(CONT_BEGIN, rest_conseq2, env, next);
                    gc_unprotect(4);
                    tramp_eval(first_expr2, env, k2);
                }
                return;
            }
            // Protect test, env, next and computed values across allocations
            unsigned clause_conseq = cdr(clause);
            unsigned rest_clauses = cdr(rest);
            gc_protect(&test);
            gc_protect(&env);
            gc_protect(&next);
            gc_protect(&clause_conseq);
            gc_protect(&rest_clauses);
            unsigned new_data = alloc_cons(clause_conseq, rest_clauses);
            unsigned k2 = make_cont(CONT_COND_TEST, new_data, env, next);
            gc_unprotect(5);
            tramp_eval(test, env, k2);
        }
    }
}

/**
 * CONT_COND_ARROW: Apply receiver procedure to test value.
 * data = test value that was truthy
 * val = evaluated receiver procedure
 * Calls (receiver test-value) and returns the result.
 */
static void handle_cont_cond_arrow(unsigned val, unsigned data, unsigned env,
                                   unsigned next)
{
    // val is the receiver procedure, data is the test value
    // We need to call (val data)
    gc_protect(&val);
    gc_protect(&data);
    gc_protect(&env);
    gc_protect(&next);
    unsigned args = alloc_cons(data, 0); // Build argument list (test-value)
    gc_unprotect(4);
    apply_function(val, args, env, next);
}

/**
 * CONT_LET_VALS: Accumulate let binding values.
 * data = (vars . (vals . (remaining-bindings . body)))
 * Stores val in vals list, then evaluates next binding or body.
 */
void handle_cont_let_vals(unsigned val, unsigned data, unsigned env,
                          unsigned next)
{
    unsigned vars = car(data);
    unsigned vals = cadr(data);
    unsigned rest_and_body = cddr(data);
    unsigned rest_bindings = car(rest_and_body);
    unsigned body = cdr(rest_and_body);

    unsigned v = list_last(vals);
    write_barrier(v, val); // v may be in old gen
    CELL_CAR(v) = val;

    if (!rest_bindings) {
        // Protect body and next across extend_env which allocates
        gc_protect(&body);
        gc_protect(&next);
        unsigned new_env = extend_env(vars, vals, env);
        gc_unprotect(2);
        eval_body(body, new_env, next);
    } else {
        unsigned bind = car(rest_bindings);
        unsigned new_vars = vars;
        unsigned new_vals = vals;

        // Protect everything before any allocations (including next)
        gc_protect(&new_vars);
        gc_protect(&new_vals);
        gc_protect(&bind);
        gc_protect(&v);
        gc_protect(&rest_bindings);
        gc_protect(&body);
        gc_protect(&env);
        gc_protect(&next);
        unsigned vt = 0;
        gc_protect(&vt);

        vt = list_last(new_vars);
        unsigned new_vc = alloc_cons(car(bind), 0);
        write_barrier(vt, new_vc); // vt may be in old gen
        CELL_CDR(vt) = new_vc;

        // v is already list_last(vals), which equals list_last(new_vals)
        unsigned new_valc = alloc_cons(0, 0);
        write_barrier(v, new_valc);
        CELL_CDR(v) = new_valc;

        unsigned next_val_expr = cadr(bind);
        gc_protect(&next_val_expr);
        unsigned inner = 0, middle = 0;
        gc_protect(&inner);
        gc_protect(&middle);
        inner = alloc_cons(cdr(rest_bindings), body);
        middle = alloc_cons(new_vals, inner);
        unsigned new_data = alloc_cons(new_vars, middle);
        unsigned k2 = make_cont(CONT_LET_VALS, new_data, env, next);
        gc_unprotect(12);
        tramp_eval(next_val_expr, env, k2);
    }
}

void handle_cont_let_body(unsigned val, unsigned data, unsigned env,
                          unsigned next)
{
    (void)val;
    if (!cdr(data)) {
        tramp_eval(car(data), env, next);
    } else {
        // Extract first expr and rest before allocation, protect all used after
        unsigned first_expr = car(data);
        unsigned rest = cdr(data);
        gc_protect(&first_expr);
        gc_protect(&rest);
        gc_protect(&env);
        gc_protect(&next);
        unsigned k2 = make_cont(CONT_LET_BODY, rest, env, next);
        gc_unprotect(4);
        tramp_eval(first_expr, env, k2);
    }
}

void handle_cont_letstar_vals(unsigned val, unsigned data, unsigned env,
                              unsigned next)
{
    unsigned bindings = car(data);
    unsigned body = cdr(data);
    unsigned var = caar(bindings);
    // Protect everything needed across allocations (including next)
    gc_protect(&bindings);
    gc_protect(&body);
    gc_protect(&var);
    gc_protect(&val);
    gc_protect(&env);
    gc_protect(&next);
    unsigned var_cell = 0;
    gc_protect(&var_cell);
    var_cell = alloc_cons(var, 0);
    unsigned val_cell = alloc_cons(val, 0);
    unsigned new_env = extend_env(var_cell, val_cell, env);
    gc_unprotect(1);
    unsigned rest = cdr(bindings); // bindings is protected
    gc_unprotect(6);
    if (!rest) {
        eval_body(body, new_env, next);
    } else {
        // Extract next value expr before allocations
        unsigned next_val_expr = cadr(car(rest));
        gc_protect(&next_val_expr);
        gc_protect(&new_env);
        gc_protect(&next);
        unsigned new_data = alloc_cons(rest, body);
        unsigned k2 = make_cont(CONT_LETSTAR_VALS, new_data, new_env, next);
        gc_unprotect(3);
        tramp_eval(next_val_expr, new_env, k2);
    }
}

void handle_cont_letrec_init(unsigned val, unsigned data, unsigned env,
                             unsigned next)
{
    // Data structure: (bindings . (vals_ptr . (all_vals . (saved . body))))
    // saved = list of saved car values for cells from all_vals to vals_ptr
    unsigned bindings = car(data);
    unsigned inner = cdr(data);
    unsigned vals_ptr = car(inner);
    unsigned inner2 = cdr(inner);
    unsigned all_vals = car(inner2);
    unsigned inner3 = cdr(inner2);
    unsigned saved = car(inner3);
    unsigned body = cdr(inner3);

    // Restore all values from the saved state (for reinvoked continuations)
    // This resets any modifications made by set! after initialization
    unsigned v = all_vals;
    unsigned s = saved;
    for (; v && v != vals_ptr && s; v = cdr(v), s = cdr(s)) {
        write_barrier(v, car(s));
        CELL_CAR(v) = car(s);
    }

    // Store the new value
    write_barrier(vals_ptr, val);
    CELL_CAR(vals_ptr) = val;

    unsigned rest = cdr(bindings);
    if (!rest) {
        eval_body(body, env, next);
    } else {
        // Extract next value expr and protect across all allocations
        unsigned next_val_expr = cadr(car(rest));
        gc_protect(&next_val_expr);
        gc_protect(&rest);
        gc_protect(&body);
        gc_protect(&all_vals);
        gc_protect(&saved);
        gc_protect(&env);
        gc_protect(&next);
        gc_protect(&vals_ptr);

        // Build new saved list: prepend current value to saved
        unsigned new_saved = 0;
        gc_protect(&new_saved);
        new_saved = alloc_cons(val, saved);

        // Build new data: (rest . (next_vals_ptr . (all_vals . (new_saved .
        // body))))
        unsigned inner1 = 0, inner2_new = 0, inner3_new = 0;
        gc_protect(&inner1);
        gc_protect(&inner2_new);
        gc_protect(&inner3_new);
        inner1 = alloc_cons(new_saved, body);
        inner2_new = alloc_cons(all_vals, inner1);
        inner3_new = alloc_cons(cdr(vals_ptr), inner2_new);
        unsigned new_data = alloc_cons(rest, inner3_new);
        unsigned k2 = make_cont(CONT_LETREC_INIT, new_data, env, next);
        gc_unprotect(12);
        tramp_eval(next_val_expr, env, k2);
    }
}

void handle_cont_eval_fn(unsigned val, unsigned data, unsigned env,
                         unsigned next)
{
    // Protect all parameters - this function allocates and can trigger GC
    gc_protect(&val);
    gc_protect(&data);
    gc_protect(&env);
    gc_protect(&next);

    unsigned fn = val;
    unsigned arg_exprs = data;

    // Handle macros
    if (IS_MACRO(fn)) {
        unsigned params = car(fn);
        unsigned mbody = cadr(fn);
        unsigned macroenv = cddr(fn);
        // Protect all values across allocations (including params and arg_exprs
        // for bind_params)
        gc_protect(&params);
        gc_protect(&arg_exprs);
        gc_protect(&mbody);
        gc_protect(&macroenv);
        gc_protect(&env);
        gc_protect(&next);
        unsigned frame = 0;
        gc_protect(&frame);
        frame = bind_params(params, arg_exprs);
        unsigned menv = alloc_cons(frame, macroenv);
        if (!cdr(mbody)) {
            unsigned first_expr = car(mbody);
            gc_protect(&first_expr);
            gc_protect(&menv);
            unsigned k2 = make_cont(CONT_MACRO_EXPAND, 0, env, next);
            gc_unprotect(13); // first_expr, menv, frame, next, env, macroenv,
                              // mbody, arg_exprs, params + 4 initial
            tramp_eval(first_expr, menv, k2);
        } else {
            unsigned first_expr = car(mbody);
            unsigned rest_mbody = cdr(mbody);
            gc_protect(&first_expr);
            gc_protect(&rest_mbody);
            gc_protect(&menv);
            unsigned inner_k = 0;
            gc_protect(&inner_k);
            inner_k = make_cont(CONT_MACRO_EXPAND, 0, env, next);
            unsigned k2 = make_cont(CONT_APPLY_FUNC, rest_mbody, menv, inner_k);
            gc_unprotect(15); // inner_k, first_expr, rest_mbody, menv, frame,
                              // next, env, macroenv, mbody, arg_exprs, params +
                              // 4 initial
            tramp_eval(first_expr, menv, k2);
        }
        return;
    }

    // Handle syntax transformers
    if (IS_SYNTAX(fn)) {
        gc_protect(&fn);
        gc_protect(&arg_exprs);
        unsigned input = alloc_cons(0, arg_exprs);
        unsigned expanded = apply_syntax(fn, input, env);
        gc_unprotect(6); // fn, arg_exprs + 4 params
        if (expanded == TOK_ERROR) {
            tramp_error();
            return;
        }
        tramp_eval(expanded, env, next);
        return;
    }

    if (!arg_exprs) {
        gc_unprotect(4); // 4 params
        apply_function(fn, 0, env, next);
        return;
    }

    // Protect fn, arg_exprs for GC during allocations
    // Extract first_arg BEFORE allocations, protect it
    unsigned first_arg = car(arg_exprs);
    gc_protect(&fn);
    gc_protect(&arg_exprs);
    gc_protect(&first_arg);
    unsigned inner = 0, fn_and_args = 0;
    gc_protect(&inner);
    gc_protect(&fn_and_args);
    inner = alloc_cons(0, cdr(arg_exprs));
    fn_and_args = alloc_cons(fn, inner);
    unsigned k2 = make_cont(CONT_EVAL_ARGS, fn_and_args, env, next);
    gc_unprotect(9); // inner, fn_and_args, first_arg, arg_exprs, fn + 4 params
    tramp_eval(first_arg, env, k2);
}

void handle_cont_eval_args(unsigned val, unsigned data, unsigned env,
                           unsigned next)
{
    // Protect all parameters - this function allocates and can trigger GC
    gc_protect(&val);
    gc_protect(&data);
    gc_protect(&env);
    gc_protect(&next);

    unsigned fn = car(data);
    unsigned evaled_and_rest = cdr(data);
    unsigned evaled_rev = car(evaled_and_rest);
    unsigned remaining = cdr(evaled_and_rest);

    // Protect fn, evaled_rev, and remaining before allocation
    gc_protect(&fn);
    gc_protect(&evaled_rev);
    gc_protect(&remaining);
    unsigned new_evaled_rev = alloc_cons(val, evaled_rev);

    if (!remaining) {
        // Reverse the evaluated args list; protect loop vars across allocs
        unsigned args = 0;
        gc_protect(&args);
        for (unsigned l = new_evaled_rev; l; l = cdr(l)) {
            gc_protect(&l);
            args = alloc_cons(car(l), args);
            gc_unprotect(1); // l
        }
        gc_unprotect(
            8); // args, remaining, evaled_rev, fn, next, env, data, val
        apply_function(fn, args, env, next);
    } else {
        gc_protect(&new_evaled_rev);
        unsigned first_arg = car(remaining);
        gc_protect(&first_arg);
        unsigned inner = 0, new_data = 0;
        gc_protect(&inner);
        gc_protect(&new_data);
        inner = alloc_cons(new_evaled_rev, cdr(remaining));
        new_data = alloc_cons(fn, inner);
        unsigned k2 = make_cont(CONT_EVAL_ARGS, new_data, env, next);
        gc_unprotect(11); // new_data, inner, first_arg, new_evaled_rev,
                          // remaining, evaled_rev, fn, next, env, data, val
        tramp_eval(first_arg, env, k2);
    }
}

void handle_cont_apply_func(unsigned val, unsigned data, unsigned env,
                            unsigned next)
{
    (void)val;
    if (!cdr(data)) {
        tramp_eval(car(data), env, next);
    } else {
        // Extract first expr and rest before allocation, protect all used after
        // alloc
        unsigned first_expr = car(data);
        unsigned rest = cdr(data);
        gc_protect(&first_expr);
        gc_protect(&rest);
        gc_protect(&env);
        gc_protect(&next);
        unsigned k2 = make_cont(CONT_APPLY_FUNC, rest, env, next);
        gc_unprotect(4);
        tramp_eval(first_expr, env, k2);
    }
}

void handle_cont_macro_expand(unsigned val, unsigned data, unsigned env,
                              unsigned next)
{
    (void)data;
    tramp_eval(val, env, next);
}

void handle_cont_callwithvalues(unsigned val, unsigned data, unsigned env,
                                unsigned next)
{
    // data = consumer, val = producer's result
    unsigned consumer = data;
    unsigned consumer_args;
    // If producer returned multiple values, unpack them
    if (IS_MULTIVAL(val)) {
        consumer_args = CELL_CAR(val);
    } else {
        // Single value - wrap in a list; protect consumer and next across alloc
        gc_protect(&consumer);
        gc_protect(&next);
        consumer_args = alloc_cons(val, 0);
        gc_unprotect(2);
    }
    apply_function(consumer, consumer_args, env, next);
}

// ============================================================================
// Continuation Handler Dispatch Table
// ============================================================================

// Table indexed by cont_type enum
const cont_handler_t cont_handlers[CONT_COUNT] = {
    [CONT_HALT] = handle_cont_halt,
    [CONT_IF] = handle_cont_if,
    [CONT_BEGIN] = handle_cont_begin,
    [CONT_SET] = handle_cont_set,
    [CONT_DEFINE] = handle_cont_define,
    [CONT_EVAL_FN] = handle_cont_eval_fn,
    [CONT_EVAL_ARGS] = handle_cont_eval_args,
    [CONT_AND] = handle_cont_and,
    [CONT_OR] = handle_cont_or,
    [CONT_COND_TEST] = handle_cont_cond_test,
    [CONT_COND_ARROW] = handle_cont_cond_arrow,
    [CONT_LET_VALS] = handle_cont_let_vals,
    [CONT_LET_BODY] = handle_cont_let_body,
    [CONT_LETSTAR_VALS] = handle_cont_letstar_vals,
    [CONT_LETREC_INIT] = handle_cont_letrec_init,
    [CONT_APPLY_FUNC] = handle_cont_apply_func,
    [CONT_MACRO_EXPAND] = handle_cont_macro_expand,
    [CONT_CALLWITHVALUES] = handle_cont_callwithvalues,
};
