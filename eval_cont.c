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

void handle_cont_define(unsigned val, unsigned data, unsigned env,
                        unsigned next)
{
    // Protect next across defvar (which can allocate)
    gc_protect(&next);
    defvar(data, val, env);
    gc_unprotect(1);
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

void handle_cont_cond_test(unsigned val, unsigned data, unsigned env,
                           unsigned next)
{
    unsigned conseq = car(data);
    unsigned rest = cdr(data);
    if (val) {
        if (!conseq) {
            tramp_apply(val, next);
        } else if (!cdr(conseq)) {
            tramp_eval(car(conseq), env, next);
        } else {
            // Extract first expr and rest before allocation, protect all used
            // after
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

void handle_cont_let_vals(unsigned val, unsigned data, unsigned env,
                          unsigned next)
{
#ifdef DEBUG_GC
    fprintf(stderr, "[LET_VALS] data=%u data_type=%d\n", data, CELL_TYPE(data));
    fflush(stderr);
#endif
    unsigned vars = car(data);
    unsigned vals = cadr(data);
    unsigned rest_and_body = cddr(data);
    unsigned rest_bindings = car(rest_and_body);
    unsigned body = cdr(rest_and_body);
#ifdef DEBUG_GC
    fprintf(stderr, "[LET_VALS] vars=%u vals=%u rest_bindings=%u body=%u\n",
            vars, vals, rest_bindings, body);
#endif

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
#ifdef DEBUG_GC
        fprintf(stderr, "[LET_VALS else] bind=%u bind_type=%d\n", bind,
                CELL_TYPE(bind));
#endif
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
#ifdef DEBUG_GC
        fprintf(stderr, "[LET_VALS else] next_val_expr=%u expr_type=%d\n",
                next_val_expr, CELL_TYPE(next_val_expr));
#endif
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
    unsigned bindings = car(data);
    unsigned vals_and_body = cdr(data);
    unsigned vals_ptr = car(vals_and_body);
    unsigned body = cdr(vals_and_body);

    write_barrier(vals_ptr, val); // vals_ptr may be in old gen
    CELL_CAR(vals_ptr) = val;

    unsigned rest = cdr(bindings);
    if (!rest) {
        eval_body(body, env, next);
    } else {
        // Extract next value expr and protect across all allocations (inc next)
        unsigned next_val_expr = cadr(car(rest));
        gc_protect(&next_val_expr);
        gc_protect(&rest);
        gc_protect(&body);
        gc_protect(&vals_ptr);
        gc_protect(&env);
        gc_protect(&next);
        unsigned inner = 0;
        gc_protect(&inner);
        inner = alloc_cons(cdr(vals_ptr), body);
        unsigned new_data = alloc_cons(rest, inner);
        unsigned k2 = make_cont(CONT_LETREC_INIT, new_data, env, next);
        gc_unprotect(7);
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
        // Protect mbody, macroenv, env and next across allocations
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
            gc_unprotect(11); // first_expr, menv, frame, next, env, macroenv,
                              // mbody + 4 params
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
            gc_unprotect(13); // inner_k, first_expr, rest_mbody, menv, frame,
                              // next, env, macroenv, mbody + 4 params
            tramp_eval(first_expr, menv, k2);
        }
        return;
    }

    // Handle syntax transformers
    if (IS_SYNTAX(fn)) {
        unsigned transformer = car(fn);
        gc_protect(&transformer);
        gc_protect(&arg_exprs);
        unsigned input = alloc_cons(0, arg_exprs);
        unsigned expanded = apply_syntax(transformer, input, env);
        gc_unprotect(6); // transformer, arg_exprs + 4 params
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
    [CONT_LET_VALS] = handle_cont_let_vals,
    [CONT_LET_BODY] = handle_cont_let_body,
    [CONT_LETSTAR_VALS] = handle_cont_letstar_vals,
    [CONT_LETREC_INIT] = handle_cont_letrec_init,
    [CONT_APPLY_FUNC] = handle_cont_apply_func,
    [CONT_MACRO_EXPAND] = handle_cont_macro_expand,
    [CONT_CALLWITHVALUES] = handle_cont_callwithvalues,
};
