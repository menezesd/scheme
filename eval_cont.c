/**
 * @file eval_cont.c
 * @brief Continuation handlers for the evaluator
 *
 * Each handler processes a specific continuation type and either:
 * - Evaluates the next expression (sets tramp_eval)
 * - Applies a value to the next continuation (sets tramp_apply)
 * - Signals completion or error
 */

#include "eval_internal.h"

// ============================================================================
// Continuation Handlers
// ============================================================================

void handle_cont_halt(unsigned val, unsigned data, unsigned env, unsigned next)
{
    (void)data;
    (void)env;
    (void)next;
    tramp_done(val);
}

void handle_cont_if(unsigned val, unsigned data, unsigned env, unsigned next)
{
    if (val) {
        tramp_eval(car(data), env, next);
    } else {
        tramp_eval(cdr(data), env, next);
    }
}

void handle_cont_begin(unsigned val, unsigned data, unsigned env, unsigned next)
{
    (void)val;
    eval_seq(data, CONT_BEGIN, env, next);
}

void handle_cont_set(unsigned val, unsigned data, unsigned env, unsigned next)
{
    unsigned result = setvar(CELL_ID(data), val, env);
    if (result == TOK_ERROR) {
        tramp_error();
        return;
    }
    tramp_apply(val, next);
}

void handle_cont_define(unsigned val, unsigned data, unsigned env, unsigned next)
{
    defvar(data, val, env);
    tramp_apply(data, next);
}

void handle_cont_and(unsigned val, unsigned data, unsigned env, unsigned next)
{
    if (!val) {
        tramp_apply(0, next);
        return;
    }
    eval_seq(data, CONT_AND, env, next);
}

void handle_cont_or(unsigned val, unsigned data, unsigned env, unsigned next)
{
    if (val) {
        tramp_apply(val, next);
        return;
    }
    eval_seq(data, CONT_OR, env, next);
}

void handle_cont_cond_test(unsigned val, unsigned data, unsigned env, unsigned next)
{
    unsigned conseq = car(data);
    unsigned rest = cdr(data);
    if (val) {
        if (!conseq) {
            tramp_apply(val, next);
        } else if (!cdr(conseq)) {
            tramp_eval(car(conseq), env, next);
        } else {
            unsigned k2 = make_cont(CONT_BEGIN, cdr(conseq), env, next);
            tramp_eval(car(conseq), env, k2);
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
                    unsigned k2 = make_cont(CONT_BEGIN, cdr(conseq2), env, next);
                    tramp_eval(car(conseq2), env, k2);
                }
                return;
            }
            unsigned new_data = alloc_cons(cdr(clause), cdr(rest));
            unsigned k2 = make_cont(CONT_COND_TEST, new_data, env, next);
            tramp_eval(test, env, k2);
        }
    }
}

void handle_cont_let_vals(unsigned val, unsigned data, unsigned env, unsigned next)
{
    unsigned vars = car(data);
    unsigned vals = cadr(data);
    unsigned rest_and_body = cddr(data);
    unsigned rest_bindings = car(rest_and_body);
    unsigned body = cdr(rest_and_body);

    unsigned v = list_last(vals);
    CELL_CAR(v) = val;

    if (!rest_bindings) {
        unsigned new_env = extend_env(vars, vals, env);
        eval_body(body, new_env, next);
    } else {
        unsigned bind = car(rest_bindings);
        unsigned new_vars = vars;
        unsigned vt = list_last(new_vars);
        CELL_CDR(vt) = alloc_cons(car(bind), 0);

        unsigned new_vals = vals;
        // v is already list_last(vals), which equals list_last(new_vals)
        CELL_CDR(v) = alloc_cons(0, 0);

        // Protect vars/vals from GC during nested allocations
        gc_protect(&new_vars);
        gc_protect(&new_vals);
        unsigned inner = alloc_cons(cdr(rest_bindings), body);
        unsigned middle = alloc_cons(new_vals, inner);
        unsigned new_data = alloc_cons(new_vars, middle);
        gc_unprotect(2);
        unsigned k2 = make_cont(CONT_LET_VALS, new_data, env, next);
        tramp_eval(cadr(bind), env, k2);
    }
}

void handle_cont_let_body(unsigned val, unsigned data, unsigned env, unsigned next)
{
    (void)val;
    if (!cdr(data)) {
        tramp_eval(car(data), env, next);
    } else {
        unsigned k2 = make_cont(CONT_LET_BODY, cdr(data), env, next);
        tramp_eval(car(data), env, k2);
    }
}

void handle_cont_letstar_vals(unsigned val, unsigned data, unsigned env, unsigned next)
{
    unsigned bindings = car(data);
    unsigned body = cdr(data);
    unsigned var = caar(bindings);
    // Protect var/val/env from GC during nested allocations
    gc_protect(&var);
    gc_protect(&val);
    gc_protect(&env);
    unsigned var_cell = alloc_cons(var, 0);
    unsigned val_cell = alloc_cons(val, 0);
    unsigned new_env = extend_env(var_cell, val_cell, env);
    gc_unprotect(3);
    unsigned rest = cdr(bindings);
    if (!rest) {
        eval_body(body, new_env, next);
    } else {
        unsigned new_data = alloc_cons(rest, body);
        unsigned k2 = make_cont(CONT_LETSTAR_VALS, new_data, new_env, next);
        tramp_eval(cadr(car(rest)), new_env, k2);
    }
}

void handle_cont_letrec_init(unsigned val, unsigned data, unsigned env, unsigned next)
{
    unsigned bindings = car(data);
    unsigned vals_and_body = cdr(data);
    unsigned vals_ptr = car(vals_and_body);
    unsigned body = cdr(vals_and_body);

    CELL_CAR(vals_ptr) = val;

    unsigned rest = cdr(bindings);
    if (!rest) {
        eval_body(body, env, next);
    } else {
        // Protect rest from GC during nested allocation
        gc_protect(&rest);
        unsigned inner = alloc_cons(cdr(vals_ptr), body);
        unsigned new_data = alloc_cons(rest, inner);
        gc_unprotect(1);
        unsigned k2 = make_cont(CONT_LETREC_INIT, new_data, env, next);
        tramp_eval(cadr(car(rest)), env, k2);
    }
}

void handle_cont_eval_fn(unsigned val, unsigned data, unsigned env, unsigned next)
{
    unsigned fn = val;
    unsigned arg_exprs = data;

    // Handle macros
    if (IS_MACRO(fn)) {
        unsigned params = car(fn);
        unsigned mbody = cadr(fn);
        unsigned macroenv = cddr(fn);
        unsigned frame = bind_params(params, arg_exprs);
        unsigned menv = alloc_cons(frame, macroenv);
        if (!cdr(mbody)) {
            unsigned k2 = make_cont(CONT_MACRO_EXPAND, 0, env, next);
            tramp_eval(car(mbody), menv, k2);
        } else {
            unsigned k2 =
                make_cont(CONT_APPLY_FUNC, cdr(mbody), menv,
                          make_cont(CONT_MACRO_EXPAND, 0, env, next));
            tramp_eval(car(mbody), menv, k2);
        }
        return;
    }

    // Handle syntax transformers
    if (IS_SYNTAX(fn)) {
        unsigned transformer = car(fn);
        unsigned expanded =
            apply_syntax(transformer, alloc_cons(0, arg_exprs), env);
        if (expanded == TOK_ERROR) {
            tramp_error();
            return;
        }
        tramp_eval(expanded, env, next);
        return;
    }

    if (!arg_exprs) {
        apply_function(fn, 0, env, next);
        return;
    }

    // Protect fn from GC during nested allocation
    gc_protect(&fn);
    unsigned inner = alloc_cons(0, cdr(arg_exprs));
    unsigned fn_and_args = alloc_cons(fn, inner);
    gc_unprotect(1);
    unsigned k2 = make_cont(CONT_EVAL_ARGS, fn_and_args, env, next);
    tramp_eval(car(arg_exprs), env, k2);
}

void handle_cont_eval_args(unsigned val, unsigned data, unsigned env, unsigned next)
{
    unsigned fn = car(data);
    unsigned evaled_and_rest = cdr(data);
    unsigned evaled_rev = car(evaled_and_rest);
    unsigned remaining = cdr(evaled_and_rest);

    unsigned new_evaled_rev = alloc_cons(val, evaled_rev);

    if (!remaining) {
        // Protect fn from GC during argument reversal allocations
        gc_protect(&fn);
        unsigned args = 0;
        FORLIST(l, new_evaled_rev) { args = alloc_cons(car(l), args); }
        gc_unprotect(1);
        apply_function(fn, args, env, next);
    } else {
        // Protect fn and new_evaled_rev from GC during nested allocation
        gc_protect(&fn);
        gc_protect(&new_evaled_rev);
        unsigned inner = alloc_cons(new_evaled_rev, cdr(remaining));
        unsigned new_data = alloc_cons(fn, inner);
        gc_unprotect(2);
        unsigned k2 = make_cont(CONT_EVAL_ARGS, new_data, env, next);
        tramp_eval(car(remaining), env, k2);
    }
}

void handle_cont_apply_func(unsigned val, unsigned data, unsigned env, unsigned next)
{
    (void)val;
    if (!cdr(data)) {
        tramp_eval(car(data), env, next);
    } else {
        unsigned k2 = make_cont(CONT_APPLY_FUNC, cdr(data), env, next);
        tramp_eval(car(data), env, k2);
    }
}

void handle_cont_macro_expand(unsigned val, unsigned data, unsigned env, unsigned next)
{
    (void)data;
    tramp_eval(val, env, next);
}

void handle_cont_callwithvalues(unsigned val, unsigned data, unsigned env, unsigned next)
{
    // data = consumer, val = producer's result
    unsigned consumer = data;
    unsigned consumer_args;
    // If producer returned multiple values, unpack them
    if (IS_MULTIVAL(val)) {
        consumer_args = CELL_CAR(val);
    } else {
        // Single value - wrap in a list
        consumer_args = alloc_cons(val, 0);
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
    [CONT_LET_VALS] = handle_cont_let_vals,
    [CONT_LET_BODY] = handle_cont_let_body,
    [CONT_LETSTAR_VALS] = handle_cont_letstar_vals,
    [CONT_LETREC_INIT] = handle_cont_letrec_init,
    [CONT_APPLY_FUNC] = handle_cont_apply_func,
    [CONT_MACRO_EXPAND] = handle_cont_macro_expand,
    [CONT_CALLWITHVALUES] = handle_cont_callwithvalues,
};
