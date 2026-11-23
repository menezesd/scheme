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
 * env, and next parameters arrive by value: their caller's roots do not
 * update these local copies. Root every copy used after an allocation.
 */

#include "bytecode.h"
#include "eval_internal.h"
#include "reader.h"

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
        cps_signal_current_error(env, next);
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
    GC_GUARD;
    gc_protect(&val);
    gc_protect(&data);
    gc_protect(&env);
    gc_protect(&next);
    if (defvar(data, val, env) == TOK_ERROR) {
        cps_signal_current_error(env, next);
        return;
    }
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
                   env_find_binding_cell(CELL_ID(car(conseq)), env) == 0) {
            // (test => receiver) syntax - evaluate receiver, then call with val
            // Only if => is not lexically bound (R5RS hygiene)
            GC_GUARD;
            unsigned receiver_expr = cadr(conseq);
            gc_protect(&val);
            gc_protect(&receiver_expr);
            gc_protect(&env);
            gc_protect(&next);
            unsigned k2 = make_cont(CONT_COND_ARROW, val, env, next);
            tramp_eval(receiver_expr, env, k2);
        } else if (!cdr(conseq)) {
            tramp_eval(car(conseq), env, next);
        } else {
            GC_GUARD;
            unsigned first_expr = car(conseq);
            unsigned rest_conseq = cdr(conseq);
            gc_protect(&first_expr);
            gc_protect(&rest_conseq);
            gc_protect(&env);
            gc_protect(&next);
            unsigned k2 = make_cont(CONT_BEGIN, rest_conseq, env, next);
            tramp_eval(first_expr, env, k2);
        }
    } else {
        if (!rest) {
            tramp_apply(0, next);
        } else {
            unsigned clause = car(rest);
            unsigned test = car(clause);
            if (IS_KEYWORD(test, ctx.kw_else) &&
                env_find_binding_cell(CELL_ID(test), env) == 0) {
                unsigned conseq2 = cdr(clause);
                if (!conseq2) {
                    tramp_apply(ctx.atom_true, next);
                } else if (!cdr(conseq2)) {
                    tramp_eval(car(conseq2), env, next);
                } else {
                    // Extract first expr and rest before allocation, protect
                    // all used after
                    GC_GUARD;
                    unsigned first_expr2 = car(conseq2);
                    unsigned rest_conseq2 = cdr(conseq2);
                    gc_protect(&first_expr2);
                    gc_protect(&rest_conseq2);
                    gc_protect(&env);
                    gc_protect(&next);
                    unsigned k2 =
                        make_cont(CONT_BEGIN, rest_conseq2, env, next);
                    tramp_eval(first_expr2, env, k2);
                }
                return;
            }
            // Protect test, env, next and computed values across allocations
            GC_GUARD;
            unsigned clause_conseq = cdr(clause);
            unsigned rest_clauses = cdr(rest);
            gc_protect(&test);
            gc_protect(&env);
            gc_protect(&next);
            gc_protect(&clause_conseq);
            gc_protect(&rest_clauses);
            unsigned new_data = alloc_cons(clause_conseq, rest_clauses);
            unsigned k2 = make_cont(CONT_COND_TEST, new_data, env, next);
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
    GC_GUARD;
    gc_protect(&val);
    gc_protect(&data);
    gc_protect(&env);
    gc_protect(&next);
    unsigned args = alloc_cons(data, 0); // Build argument list (test-value)
    apply_function(val, args, env, next);
}

/**
 * CONT_LET_VALS: Accumulate initializer results without mutating a snapshot.
 * data = (vars . (reversed-values . (remaining-bindings . body)))
 *
 * A continuation captured in an initializer may be resumed after an earlier
 * body has returned closures over its bindings, or after another initializer
 * continuation has run. Keep the pending values immutable and copy them into
 * a fresh binding frame at completion, so neither case changes older frames
 * or the values saved by another continuation.
 */
void handle_cont_let_vals(unsigned val, unsigned data, unsigned env,
                          unsigned next)
{
    GC_GUARD;
    unsigned vars = car(data);
    unsigned reversed = cadr(data);
    unsigned remaining = car(cddr(data));
    unsigned body = cdr(cddr(data));
    gc_protect(&val);
    gc_protect(&vars);
    gc_protect(&reversed);
    gc_protect(&remaining);
    gc_protect(&body);
    gc_protect(&env);
    gc_protect(&next);
    reversed = alloc_cons(val, reversed);

    if (!remaining) {
        unsigned values = 0;
        gc_protect(&values);
        for (; reversed; reversed = cdr(reversed))
            values = alloc_cons(car(reversed), values);
        unsigned new_env = extend_env(vars, values, env);
        eval_scope_body(body, new_env, next);
    } else {
        unsigned expr = cadr(car(remaining));
        gc_protect(&expr);
        unsigned inner = alloc_cons(cdr(remaining), body);
        gc_protect(&inner);
        inner = alloc_cons(reversed, inner);
        unsigned new_data = alloc_cons(vars, inner);
        unsigned k = make_cont(CONT_LET_VALS, new_data, env, next);
        tramp_eval(expr, env, k);
    }
}

// A let/let*/letrec body is a <body>: its leading internal definitions form
// one letrec* scope. Bind their names (to placeholders) before evaluating any
// of them, exactly as apply_function does for a procedure body. Evaluating
// the body form by form instead meant that when the first definition built a
// procedure - expanding its body there and then - a later sibling definition
// was not bound yet, so a sibling that shadows a global macro was expanded as
// that macro: (letrec () (define (bar) (foo)) (define (foo) 'proc) (bar))
// called the global macro foo.
void eval_scope_body(unsigned body, unsigned env, unsigned cont)
{
    GC_GUARD;
    gc_protect(&body);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned scope_env = extend_env_with_internal_defines(env, body);
    if (scope_env == TOK_ERROR) {
        cps_signal_current_error(env, cont);
        return;
    }
    eval_body(body, scope_env, cont);
}

void handle_cont_let_body(unsigned val, unsigned data, unsigned env,
                          unsigned next)
{
    (void)val;
    if (!cdr(data)) {
        tramp_eval(car(data), env, next);
    } else {
        // Extract first expr and rest before allocation, protect all used after
        GC_GUARD;
        unsigned first_expr = car(data);
        unsigned rest = cdr(data);
        gc_protect(&first_expr);
        gc_protect(&rest);
        gc_protect(&env);
        gc_protect(&next);
        unsigned k2 = make_cont(CONT_LET_BODY, rest, env, next);
        tramp_eval(first_expr, env, k2);
    }
}

void handle_cont_letstar_vals(unsigned val, unsigned data, unsigned env,
                              unsigned next)
{
    GC_GUARD;
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
    gc_protect(&val_cell);
    unsigned new_env = extend_env(var_cell, val_cell, env);
    gc_unprotect(2);
    unsigned rest = cdr(bindings); // bindings is protected
    if (!rest) {
        eval_scope_body(body, new_env, next);
    } else {
        // Extract next value expr before allocations
        unsigned next_val_expr = cadr(car(rest));
        gc_protect(&next_val_expr);
        gc_protect(&new_env);
        unsigned new_data = alloc_cons(rest, body);
        unsigned k2 = make_cont(CONT_LETSTAR_VALS, new_data, new_env, next);
        tramp_eval(next_val_expr, new_env, k2);
    }
}

void handle_cont_letrec_init(unsigned val, unsigned data, unsigned env,
                             unsigned next)
{
    // data = (bindings . (vals_ptr . (saved . body)))
    // bindings: this binding and the ones still to initialise.
    // vals_ptr: the frame's value cell for this binding.
    // saved:    one (value-cell . value) entry for every binding initialised
    //           before this one, most recent first - each step prepends the
    //           entry for the value it just stored.
    unsigned bindings = car(data);
    unsigned vals_ptr = cadr(data);
    unsigned saved = car(cddr(data));
    unsigned body = cdr(cddr(data));

    // Put every earlier binding back to the value it had when this step's
    // continuation was created. On an ordinary pass that is a no-op; when a
    // captured continuation re-enters an initialiser, it undoes whatever the
    // later initialisers and the body assigned since, which is what binding
    // the inits to temporaries and assigning them at the end would give.
    // Each entry names its own cell, so the restore needs neither the frame
    // nor any allocation (the old layout kept bare values most-recent-first
    // and had to reverse a copy of them on every step, quadratic in the
    // number of bindings).
    for (unsigned s = saved; s; s = cdr(s)) {
        unsigned entry = car(s);
        cell_set_car(car(entry), cdr(entry));
    }

    // Store the new value
    cell_set_car(vals_ptr, val);
    // A letrec-bound procedure is named after its binding (named let is a
    // letrec in expanded code, where the reader's own record is lost).
    if (IS_FUNCTION(val) && IS_PAIR(bindings) && IS_PAIR(car(bindings)))
        reader_source_name(car(CELL_CDR(val)), car(car(bindings)));

    unsigned rest = cdr(bindings);
    if (!rest) {
        eval_scope_body(body, env, next);
        return;
    }

    GC_GUARD;
    // Extract next value expr and protect across all allocations
    unsigned next_val_expr = cadr(car(rest));
    gc_protect(&next_val_expr);
    gc_protect(&rest);
    gc_protect(&body);
    gc_protect(&saved);
    gc_protect(&env);
    gc_protect(&next);
    gc_protect(&vals_ptr);
    gc_protect(&val);

    unsigned entry = alloc_cons(vals_ptr, val);
    gc_protect(&entry);
    unsigned new_saved = alloc_cons(entry, saved);
    gc_protect(&new_saved);
    unsigned inner = alloc_cons(new_saved, body);
    gc_protect(&inner);
    inner = alloc_cons(cdr(vals_ptr), inner);
    unsigned new_data = alloc_cons(rest, inner);
    unsigned k2 = make_cont(CONT_LETREC_INIT, new_data, env, next);
    tramp_eval(next_val_expr, env, k2);
}

void handle_cont_eval_fn(unsigned val, unsigned data, unsigned env,
                         unsigned next)
{
    GC_GUARD;
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
        if (frame == TOK_ERROR) {
            cps_signal_current_error(env, next);
            return;
        }
        unsigned menv = alloc_cons(frame, macroenv);
        if (!cdr(mbody)) {
            unsigned first_expr = car(mbody);
            gc_protect(&first_expr);
            gc_protect(&menv);
            unsigned k2 = make_cont(CONT_MACRO_EXPAND, 0, env, next);
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
            tramp_eval(first_expr, menv, k2);
        }
        return;
    }

    // Handle syntax transformers
    if (IS_SYNTAX(fn)) {
        GC_GUARD;
        gc_protect(&fn);
        gc_protect(&arg_exprs);
        if (!eval_note_macro_expansion()) {
            cps_signal_current_error(env, next);
            return;
        }
        unsigned bindings = 0;
        gc_protect(&bindings);
        unsigned input = alloc_cons(0, arg_exprs);
        gc_protect(&input);
        unsigned expanded = apply_syntax(fn, input, env, &bindings);
        gc_protect(&expanded);
        if (expanded == TOK_ERROR) {
            cps_signal_current_error(env, next);
            return;
        }
        if (bindings)
            apply_syntax_bindings(env, bindings);
        tramp_eval(expanded, env, next);
        return;
    }

    if (!arg_exprs) {
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
    inner = alloc_cons(0, arg_exprs);
    fn_and_args = alloc_cons(fn, inner);
    unsigned k2 = make_cont(CONT_EVAL_ARGS, fn_and_args, env, next);
    tramp_eval(first_arg, env, k2);
}

void handle_cont_eval_args(unsigned val, unsigned data, unsigned env,
                           unsigned next)
{
    GC_GUARD;
    // Protect all parameters - this function allocates and can trigger GC
    gc_protect(&val);
    gc_protect(&data);
    gc_protect(&env);
    gc_protect(&next);

    unsigned fn = car(data);
    unsigned evaled_and_rest = cdr(data);
    unsigned evaled_rev = car(evaled_and_rest);
    // The frame holds the argument list from the one just evaluated on, not
    // the ones after it: a source pointer into the call even while its last
    // argument is evaluated, which is what lets a backtrace find the
    // procedure the call is in (see cps_capture_backtrace).
    unsigned remaining = cdr(cdr(evaled_and_rest));

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
        apply_function(fn, args, env, next);
    } else {
        gc_protect(&new_evaled_rev);
        unsigned first_arg = car(remaining);
        gc_protect(&first_arg);
        unsigned inner = 0, new_data = 0;
        gc_protect(&inner);
        gc_protect(&new_data);
        inner = alloc_cons(new_evaled_rev, remaining);
        new_data = alloc_cons(fn, inner);
        unsigned k2 = make_cont(CONT_EVAL_ARGS, new_data, env, next);
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
        GC_GUARD;
        // Extract first expr and rest before allocation, protect all used after
        // alloc
        unsigned first_expr = car(data);
        unsigned rest = cdr(data);
        gc_protect(&first_expr);
        gc_protect(&rest);
        gc_protect(&env);
        gc_protect(&next);
        unsigned k2 = make_cont(CONT_APPLY_FUNC, rest, env, next);
        tramp_eval(first_expr, env, k2);
    }
}

void handle_cont_macro_expand(unsigned val, unsigned data, unsigned env,
                              unsigned next)
{
    (void)data;
    if (!eval_note_macro_expansion()) {
        cps_signal_current_error(env, next);
        return;
    }
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
        if (!list_length_checked(consumer_args, NULL, "call-with-values")) {
            cps_signal_current_error(env, next);
            return;
        }
    } else {
        GC_GUARD;
        // Single value - wrap in a list; protect call inputs across alloc
        gc_protect(&val);
        gc_protect(&consumer);
        gc_protect(&env);
        gc_protect(&next);
        consumer_args = alloc_cons(val, 0);
    }
    apply_function(consumer, consumer_args, env, next);
}

/**
 * CONT_ERR_RETURN: An exception handler dispatched by cps_signal_error or
 * raise-now returned normally instead of jumping to a continuation. data
 * holds the dispatched handler. If it was the default handler, the error was
 * already reported ("Unhandled exception: ...") - propagate silently.
 * Otherwise a handler returned from a non-continuable raise, which R7RS
 * makes an error; terminate. Re-dispatching would loop on the returning
 * handler.
 */
void handle_cont_err_return(unsigned val, unsigned data, unsigned env,
                            unsigned next)
{
    (void)val;
    (void)next;
    unsigned dflt = lookup_silent(intern("*default-exception-handler*"),
                                  exception_state_env(env));
    if (dflt != TOK_ERROR && data == dflt) {
        backtrace_print_pending();
        tramp_error();
        return;
    }
    show_error("handler returned from non-continuable exception");
    tramp_error();
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
    [CONT_ERR_RETURN] = handle_cont_err_return,
};
