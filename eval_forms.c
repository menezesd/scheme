/**
 * @file eval_forms.c
 * @brief Special form handlers for the evaluator
 *
 * Each handler processes a specific special form (quote, lambda, if, etc.)
 * and sets up the appropriate continuation for evaluation.
 *
 * ## Handler Contract
 * Each handle_* function:
 * - Receives the full form (e.g., (if test then else)), env, and continuation
 * - Either sets up tramp_eval for sub-expressions, or tramp_apply with result
 * - Returns true if handled, false if not recognized
 * - Must protect local variables across any allocation (gc_protect)
 *
 * ## Continuation Chaining
 * When a form needs to evaluate sub-expressions:
 * 1. Create a continuation frame capturing pending work
 * 2. Call tramp_eval with the sub-expression and new continuation
 * 3. The continuation handler (in eval_cont.c) receives the result
 */

#include "eval_internal.h"
#include "prim_internal.h"
#include <limits.h>

// ============================================================================
// Special Form Handlers
// ============================================================================

/**
 * Handle quote: (quote datum) -> datum
 *
 * Returns the datum unevaluated.
 */
bool handle_quote(unsigned id, unsigned env, unsigned cont)
{
    if (!syntax_arity_checked(id, 1, 1, "quote")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    tramp_apply(cadr(id), cont);
    return true;
}

/**
 * Handle lambda special form: (lambda (params...) body...)
 *
 * Creates a closure capturing the current environment. The closure is
 * represented as a BT_FUNCTION cell where:
 * - car = parameter list (may be dotted for rest params)
 * - cdr = (body . env) where body is the list of body expressions
 */
// Expand a lambda's body against env (plus parameter and internal-define
// frames) and build the closure cell. Returns TOK_ERROR when the formals
// are invalid or an allocation fails mid-build; a macro-expansion failure
// inside the body is NOT returned as an error - it becomes a closure that
// raises the stored message at call time.
static unsigned build_lambda_closure(unsigned params, unsigned body,
                                     unsigned env)
{
    GC_GUARD;
    gc_protect(&params);
    gc_protect(&body);
    gc_protect(&env);

    // Expansion must see parameters and leading internal defines: hygiene
    // decisions (binding-cell comparisons, literal matching, ellipsis
    // shadowing) walk this chain, and missing names would resolve
    // scope-sensitive forms against stale outer bindings.
    unsigned binder_env = extend_env_with_binder_names(env, params);
    gc_protect(&binder_env);
    unsigned expansion_env =
        extend_env_with_internal_defines(binder_env, body);
    gc_protect(&expansion_env);

    unsigned macro_bindings = 0;
    gc_protect(&macro_bindings);
    unsigned expanded_body = expand_form(body, expansion_env, &macro_bindings);
    gc_protect(&expanded_body);

    unsigned closure_env = env;
    gc_protect(&closure_env);
    if (expanded_body == TOK_ERROR) {
        // Defer the expansion error to call time so it fires inside the
        // dynamic extent that invokes the closure (the compiler likewise
        // turns compile-time expansion failures into runtime raises at the
        // call site); raising here instead would fire during argument
        // evaluation, outside any guard established around later calls.
        unsigned msg =
            make_string_copy(ctx.last_error[0] ? ctx.last_error
                                               : "macro expansion failed");
        if (msg == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&msg);
        unsigned raiser = mk_primop(PERROR);
        gc_protect(&raiser);
        unsigned call_args = alloc_cons(msg, 0);
        gc_protect(&call_args);
        unsigned call = alloc_cons(raiser, call_args);
        gc_protect(&call);
        unsigned bad_body = alloc_cons(call, 0);
        gc_protect(&bad_body);
        return make_typed_cell(BT_FUNCTION, params,
                               alloc_cons(bad_body, closure_env));
    }
    if (macro_bindings)
        closure_env = extend_env_empty(env);
    if (macro_bindings)
        apply_syntax_bindings(closure_env, macro_bindings);

    unsigned body_env = alloc_cons(expanded_body, closure_env);
    gc_protect(&body_env);
    return make_typed_cell(BT_FUNCTION, params, body_env);
}

bool handle_lambda(unsigned id, unsigned env, unsigned cont)
{
    GC_GUARD;
    unsigned rest = cdr(id);
    if (!IS_PAIR(rest)) {
        show_error("lambda: expected formals and body");
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned params = car(rest);
    unsigned body = cdr(rest);
    if (!body || !list_length_checked(body, NULL, "lambda")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    gc_protect(&params);
    gc_protect(&body);
    gc_protect(&env);
    gc_protect(&cont);
    if (!lambda_params_valid(params)) {
        show_error("lambda: invalid formals");
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned p = build_lambda_closure(params, body, env);
    if (p == TOK_ERROR) {
        cps_signal_current_error(env, cont);
        return true;
    }
    tramp_apply(p, cont);
    return true;
}

/**
 * Handle if: (if test consequent [alternate])
 *
 * Evaluates test first. The CONT_IF continuation then evaluates
 * either consequent (if test is truthy) or alternate (if test is #f).
 * Missing alternate defaults to unspecified (nil).
 */
bool handle_if(unsigned id, unsigned env, unsigned cont)
{
    GC_GUARD;
    if (!syntax_arity_checked(id, 2, 3, "if")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned test = cadr(id);
    unsigned then_branch = caddr(id);
    unsigned else_branch = cadddr(id);
    gc_protect(&test);
    gc_protect(&then_branch);
    gc_protect(&else_branch);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned branches = alloc_cons(then_branch, else_branch);
    unsigned k = make_cont(CONT_IF, branches, env, cont);
    tramp_eval(test, env, k);
    return true;
}

/**
 * Handle begin: (begin expr1 expr2 ...)
 *
 * Evaluates expressions in sequence, returning the value of the last.
 * Empty begin returns unspecified (nil). Single expression is a tail call.
 */
bool handle_begin(unsigned id, unsigned env, unsigned cont)
{
    unsigned seq = cdr(id);
    if (!syntax_arity_checked(id, 0, UINT_MAX, "begin")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    if (!seq) {
        tramp_apply(0, cont);
        return true;
    }
    if (!cdr(seq)) {
        // Single expression - tail call optimization
        tramp_eval(car(seq), env, cont);
        return true;
    }
    GC_GUARD;
    unsigned first_expr = car(seq);
    unsigned rest = cdr(seq);
    gc_protect(&first_expr);
    gc_protect(&rest);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned k = make_cont(CONT_BEGIN, rest, env, cont);
    tramp_eval(first_expr, env, k);
    return true;
}

/**
 * Handle set!: (set! variable expression)
 *
 * Evaluates expression, then mutates existing binding of variable.
 * Error if variable is not already bound in the environment chain.
 */
bool handle_set(unsigned id, unsigned env, unsigned cont)
{
    GC_GUARD;
    if (!syntax_arity_checked(id, 2, 2, "set!")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned var = cadr(id);
    unsigned val_expr = caddr(id);
    if (!identifier_valid(var)) {
        show_error("set!: expected variable");
        cps_signal_current_error(env, cont);
        return true;
    }
    gc_protect(&var);
    gc_protect(&val_expr);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned k = make_cont(CONT_SET, var, env, cont);
    tramp_eval(val_expr, env, k);
    return true;
}

/**
 * Handle define: (define variable expression) or (define (name params...)
 * body...)
 *
 * Two forms:
 * 1. (define var expr) - Evaluates expr, binds to var in current frame
 * 2. (define (name p1 p2...) body...) - Shorthand for defining a function
 *    Equivalent to (define name (lambda (p1 p2...) body...))
 */
bool handle_define(unsigned id, unsigned env, unsigned cont)
{
    GC_GUARD;
    if (!syntax_arity_checked(id, 2, UINT_MAX, "define")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned vid = cadr(id);
    if (IS_PAIR(vid)) {
        // Function definition shorthand: (define (name params...) body...)
        unsigned name = car(vid);
        unsigned params = cdr(vid);
        unsigned body = cddr(id);
        gc_protect(&name);
        gc_protect(&params);
        gc_protect(&body); // Must protect - used in alloc_cons
        gc_protect(&env);
        gc_protect(&cont);
        // lambda_params_valid allocates, so locals must be protected first
        if (!identifier_valid(name) || !body ||
            !list_length_checked(body, NULL, "define") ||
            !lambda_params_valid(params)) {
            show_error("define: invalid syntax");
            cps_signal_current_error(env, cont);
            return true;
        }
        // Build through the same path as lambda so the body is
        // macro-expanded here, against this environment - a later
        // redefinition of an used syntax binding must not leak into calls.
        unsigned p = build_lambda_closure(params, body, env);
        if (p == TOK_ERROR) {
            cps_signal_current_error(env, cont);
            return true;
        }
        gc_protect(&p);
        if (defvar(name, p, env) == TOK_ERROR) {
            cps_signal_current_error(env, cont);
            return true;
        }
        tramp_apply(name, cont);
        return true;
    }
    if (!identifier_valid(vid) || cdddr(id)) {
        show_error("define: invalid syntax");
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned val_expr = caddr(id);
    gc_protect(&vid); // Must protect - used in make_cont
    gc_protect(&val_expr);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned k = make_cont(CONT_DEFINE, vid, env, cont);
    tramp_eval(val_expr, env, k);
    return true;
}

/**
 * Handle and: (and expr1 expr2 ...)
 *
 * Evaluates expressions left-to-right until one returns #f or all are done.
 * Returns #f if any expression is #f, otherwise returns value of last expr.
 * Empty (and) returns #t. Single expression is a tail call.
 */
bool handle_and(unsigned id, unsigned env, unsigned cont)
{
    unsigned seq = cdr(id);
    if (!syntax_arity_checked(id, 0, UINT_MAX, "and")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    if (!seq) {
        tramp_apply(ctx.atom_true, cont);
        return true;
    }
    if (!cdr(seq)) {
        tramp_eval(car(seq), env, cont);
        return true;
    }
    GC_GUARD;
    unsigned first_expr = car(seq);
    unsigned rest = cdr(seq);
    gc_protect(&first_expr);
    gc_protect(&rest);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned k = make_cont(CONT_AND, rest, env, cont);
    tramp_eval(first_expr, env, k);
    return true;
}

/**
 * Handle or: (or expr1 expr2 ...)
 *
 * Evaluates expressions left-to-right until one returns truthy or all are done.
 * Returns first truthy value, or #f if all are #f.
 * Empty (or) returns #f. Single expression is a tail call.
 */
bool handle_or(unsigned id, unsigned env, unsigned cont)
{
    unsigned seq = cdr(id);
    if (!syntax_arity_checked(id, 0, UINT_MAX, "or")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    if (!seq) {
        tramp_apply(ctx.atom_false, cont);
        return true;
    }
    if (!cdr(seq)) {
        tramp_eval(car(seq), env, cont);
        return true;
    }
    GC_GUARD;
    unsigned first_expr = car(seq);
    unsigned rest = cdr(seq);
    gc_protect(&first_expr);
    gc_protect(&rest);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned k = make_cont(CONT_OR, rest, env, cont);
    tramp_eval(first_expr, env, k);
    return true;
}

/**
 * Handle cond: (cond clause1 clause2 ...)
 *
 * Each clause is (test expr ...) or (else expr ...).
 * Evaluates tests in order until one is truthy, then evaluates its body.
 * (else ...) matches unconditionally and must be last.
 */
bool handle_cond(unsigned id, unsigned env, unsigned cont)
{
    unsigned clauses = cdr(id);
    if (!cond_clauses_valid(clauses, "cond", env)) {
        cps_signal_current_error(env, cont);
        return true;
    }
    if (!clauses) {
        tramp_apply(0, cont);
        return true;
    }
    unsigned clause = car(clauses);
    unsigned test = car(clause);
    unsigned rest_clauses = cdr(clauses);

    if (IS_KEYWORD(test, ctx.kw_else) &&
        env_find_binding_cell(ctx.kw_else, env) == 0) {
        unsigned conseq = cdr(clause);
        if (!conseq) {
            tramp_apply(ctx.atom_true, cont);
        } else if (!cdr(conseq)) {
            tramp_eval(car(conseq), env, cont);
        } else {
            GC_GUARD;
            unsigned first_conseq = car(conseq);
            unsigned rest_conseq = cdr(conseq);
            gc_protect(&first_conseq);
            gc_protect(&rest_conseq);
            gc_protect(&env);
            gc_protect(&cont);
            unsigned k = make_cont(CONT_BEGIN, rest_conseq, env, cont);
            tramp_eval(first_conseq, env, k);
        }
        return true;
    }
    // Protect test, env, cont, and computed values across allocations
    GC_GUARD;
    unsigned conseq = cdr(clause);
    gc_protect(&test);
    gc_protect(&env);
    gc_protect(&cont);
    gc_protect(&conseq);
    gc_protect(&rest_clauses);
    unsigned data = alloc_cons(conseq, rest_clauses);
    unsigned k = make_cont(CONT_COND_TEST, data, env, cont);
    tramp_eval(test, env, k);
    return true;
}

bool handle_cond_expand(unsigned id, unsigned env, unsigned cont)
{
    unsigned clauses = cdr(id);
    if (!list_length_checked(clauses, NULL, "cond-expand")) {
        cps_signal_current_error(env, cont);
        return true;
    }

    FORLIST(c, clauses) {
        unsigned clause = car(c);
        if (!IS_PAIR(clause)) {
            show_error("cond-expand: invalid clause");
            cps_signal_current_error(env, cont);
            return true;
        }
        unsigned requirement = car(clause);
        unsigned body = cdr(clause);
        if (!list_length_checked(body, NULL, "cond-expand")) {
            cps_signal_current_error(env, cont);
            return true;
        }
        bool is_else = IS_KEYWORD(requirement, ctx.kw_else) &&
                       env_find_binding_cell(ctx.kw_else, env) == 0;
        if (is_else || cond_expand_requirement_satisfied(requirement)) {
            if (!body) {
                tramp_apply(0, cont);
            } else {
                eval_body(body, env, cont);
            }
            return true;
        }
    }

    tramp_apply(0, cont);
    return true;
}

static bool handle_named_let(unsigned id, unsigned env, unsigned cont)
{
    if (!syntax_arity_checked(id, 3, UINT_MAX, "let")) {
        cps_signal_current_error(env, cont);
        return true;
    }

    unsigned name = cadr(id);
    unsigned bindings = caddr(id);
    unsigned body = cdddr(id);
    if (!identifier_valid(name) || !binding_list_valid(bindings, "let") ||
        !list_length_checked(body, NULL, "let")) {
        show_error("let: invalid named let syntax");
        cps_signal_current_error(env, cont);
        return true;
    }

    GC_GUARD;
    gc_protect(&name);
    gc_protect(&bindings);
    gc_protect(&body);
    gc_protect(&env);
    gc_protect(&cont);

    unsigned params = 0, params_tail = 0;
    unsigned args = 0, args_tail = 0;
    gc_protect(&params);
    gc_protect(&params_tail);
    gc_protect(&args);
    gc_protect(&args_tail);

    unsigned b = bindings;
    gc_protect(&b);
    for (; b; b = cdr(b)) {
        unsigned binding = car(b);
        list_append(&params, &params_tail, car(binding));
        list_append(&args, &args_tail, cadr(binding));
    }
    gc_unprotect(1);

    unsigned name_cell = 0, val_cell = 0, new_env = 0;
    gc_protect(&name_cell);
    gc_protect(&val_cell);
    gc_protect(&new_env);
    name_cell = alloc_cons(name, 0);
    val_cell = alloc_cons(0, 0);
    new_env = extend_env(name_cell, val_cell, env);

    gc_protect(&params);
    gc_protect(&body);
    gc_protect(&new_env);
    unsigned body_env = alloc_cons(body, new_env);
    unsigned fn = make_typed_cell(BT_FUNCTION, params, body_env);
    gc_protect(&fn);
    cell_set_car(val_cell, fn);

    if (!args) {
        apply_function(fn, 0, env, cont);
        return true;
    }

    unsigned first_arg = car(args);
    unsigned rest_args = cdr(args);
    gc_protect(&first_arg);
    gc_protect(&rest_args);
    unsigned inner = 0, data = 0;
    gc_protect(&inner);
    gc_protect(&data);
    inner = alloc_cons(0, rest_args);
    data = alloc_cons(fn, inner);
    unsigned k = make_cont(CONT_EVAL_ARGS, data, env, cont);
    tramp_eval(first_arg, env, k);
    return true;
}

/**
 * Handle let: (let ((var1 expr1) (var2 expr2) ...) body...)
 * or named let: (let name ((var1 expr1) ...) body...)
 *
 * Parallel binding: all expressions evaluated in outer environment,
 * then all bindings made simultaneously in a new environment.
 * Body is evaluated in the new environment with tail call to last expr.
 */
bool handle_let(unsigned id, unsigned env, unsigned cont)
{
    if (!syntax_arity_checked(id, 2, UINT_MAX, "let")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned bindings = cadr(id);
    if (IS_ATOM(bindings))
        return handle_named_let(id, env, cont);

    unsigned body = cddr(id);
    if (!binding_list_valid(bindings, "let") ||
        !list_length_checked(body, NULL, "let")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    if (!bindings) {
        GC_GUARD;
        gc_protect(&body);
        gc_protect(&env);
        gc_protect(&cont);
        unsigned new_env = extend_env_empty(env);
        eval_body(body, new_env, cont);
        return true;
    }
    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&body);
    gc_protect(&env);
    gc_protect(&cont);

    // Pre-allocate the full vars/vals lists once, up front (mirroring
    // handle_letrec below), so handle_cont_let_vals only ever writes a
    // specific, already-existing cell rather than destructively growing a
    // shared list across steps - see the comment on CONT_LET_VALS in
    // eval_cont.c for why that matters for multi-shot call/cc.
    unsigned vars = 0, vals = 0;
    unsigned vars_tail = 0, vals_tail = 0;
    gc_protect(&vars);
    gc_protect(&vals);
    gc_protect(&vars_tail);
    gc_protect(&vals_tail);

    unsigned b_iter = bindings;
    gc_protect(&b_iter);
    for (; b_iter; b_iter = cdr(b_iter)) {
        unsigned var = caar(b_iter);
        unsigned vc = 0;
        gc_protect(&vc);
        vc = alloc_cons(var, 0);
        unsigned vlc = alloc_cons(0, 0);
        gc_unprotect(1); // vc
        if (!vars) {
            vars = vc;
            vals = vlc;
        } else {
            cell_set_cdr(vars_tail, vc);
            cell_set_cdr(vals_tail, vlc);
        }
        vars_tail = vc;
        vals_tail = vlc;
    }
    gc_unprotect(1); // b_iter

    unsigned first_val_expr = cadr(car(bindings));
    gc_protect(&first_val_expr);

    // data = (vars . (vals_ptr . (rest_bindings . (vals . body))))
    // vals_ptr starts at the head of vals (the cell for binding 0).
    unsigned inner3 = 0, inner2 = 0, inner1 = 0;
    gc_protect(&inner3);
    gc_protect(&inner2);
    gc_protect(&inner1);
    inner3 = alloc_cons(vals, body);
    inner2 = alloc_cons(cdr(bindings), inner3);
    inner1 = alloc_cons(vals, inner2);
    unsigned data = alloc_cons(vars, inner1);
    unsigned k = make_cont(CONT_LET_VALS, data, env, cont);
    tramp_eval(first_val_expr, env, k);
    return true;
}

/**
 * Handle let*: (let* ((var1 expr1) (var2 expr2) ...) body...)
 *
 * Sequential binding: each expression is evaluated in an environment
 * that includes all previous bindings. This allows later bindings
 * to reference earlier ones.
 */
bool handle_letstar(unsigned id, unsigned env, unsigned cont)
{
    if (!syntax_arity_checked(id, 2, UINT_MAX, "let*")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned bindings = cadr(id);
    unsigned body = cddr(id);
    if (!binding_list_valid(bindings, "let*") ||
        !list_length_checked(body, NULL, "let*")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    if (!bindings) {
        // Create empty scope for internal defines
        GC_GUARD;
        gc_protect(&body);
        gc_protect(&env);
        gc_protect(&cont);
        unsigned new_env = extend_env_empty(env);
        eval_body(body, new_env, cont);
        return true;
    }
    GC_GUARD;
    unsigned first = car(bindings);
    unsigned first_val_expr = cadr(first);
    gc_protect(&first_val_expr);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned data = alloc_cons(bindings, body);
    unsigned k = make_cont(CONT_LETSTAR_VALS, data, env, cont);
    tramp_eval(first_val_expr, env, k);
    return true;
}

/**
 * Handle letrec: (letrec ((var1 expr1) (var2 expr2) ...) body...)
 *
 * Recursive binding: all variables are bound before any expressions
 * are evaluated. This allows mutually recursive definitions.
 * Variables initially bound to undefined, then set! to their values.
 */
bool handle_letrec(unsigned id, unsigned env, unsigned cont)
{
    if (!syntax_arity_checked(id, 2, UINT_MAX, "letrec")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned bindings = cadr(id);
    unsigned body = cddr(id);
    if (!binding_list_valid(bindings, "letrec") ||
        !list_length_checked(body, NULL, "letrec")) {
        cps_signal_current_error(env, cont);
        return true;
    }

    if (!bindings) {
        GC_GUARD;
        gc_protect(&body);
        gc_protect(&env);
        gc_protect(&cont);
        unsigned new_env = extend_env_empty(env);
        eval_body(body, new_env, cont);
        return true;
    }

    GC_GUARD;
    // Protect bindings, body, env and cont across all allocations
    gc_protect(&bindings);
    gc_protect(&body);
    gc_protect(&env);
    gc_protect(&cont);

    unsigned vars = 0, vals = 0;
    unsigned vars_tail = 0, vals_tail = 0;
    gc_protect(&vars);
    gc_protect(&vals);
    gc_protect(&vars_tail);
    gc_protect(&vals_tail);

    // Build vars and vals lists using a separate iterator
    // bindings stays protected and unchanged for later use
    unsigned b_iter = bindings;
    gc_protect(&b_iter);
    for (; b_iter; b_iter = cdr(b_iter)) {
        unsigned var = caar(b_iter);
        unsigned vc = 0;
        gc_protect(&vc);
        vc = alloc_cons(var, 0);
        unsigned vlc = alloc_cons(0, 0);
        gc_unprotect(1); // vc
        if (!vars) {
            vars = vc;
            vals = vlc;
        } else {
            cell_set_cdr(vars_tail, vc);
            cell_set_cdr(vals_tail, vlc);
        }
        vars_tail = vc;
        vals_tail = vlc;
    }
    gc_unprotect(1); // b_iter

    unsigned new_env = extend_env(vars, vals, env);
    gc_protect(&new_env);

    unsigned first_val_expr = cadr(car(bindings)); // bindings is protected
    gc_protect(&first_val_expr);

    // Build data structure for CONT_LETREC_INIT:
    // data = (bindings . (vals_ptr . (all_vals . (saved . body))))
    // all_vals is the head of vals list, saved starts empty (0)
    // When continuation is reinvoked, saved values are restored
    unsigned inner1 = 0, inner2 = 0, inner3 = 0;
    gc_protect(&inner1);
    gc_protect(&inner2);
    gc_protect(&inner3);
    inner1 = alloc_cons(0, body);      // (saved . body) - saved starts empty
    inner2 = alloc_cons(vals, inner1); // (all_vals . (saved . body))
    inner3 =
        alloc_cons(vals, inner2); // (vals_ptr . (all_vals . (saved . body)))
    unsigned data = alloc_cons(bindings, inner3);
    unsigned k = make_cont(CONT_LETREC_INIT, data, new_env, cont);
    tramp_eval(first_val_expr, new_env, k);
    return true;
}

bool handle_quasiquote(unsigned id, unsigned env, unsigned cont)
{
    GC_GUARD;
    if (!syntax_arity_checked(id, 1, 1, "quasiquote")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned tmpl = cadr(id);
    gc_protect(&tmpl);
    gc_protect(&env);
    gc_protect(&cont);
    // Transform the template into ordinary code and evaluate it through the
    // regular trampoline so continuations work across unquotes
    unsigned expr = qq_transform_cps(tmpl, env);
    gc_protect(&expr);
    if (expr == TOK_ERROR) {
        cps_signal_current_error(env, cont);
        return true;
    }
    tramp_eval(expr, env, cont);
    return true;
}

bool handle_define_macro(unsigned id, unsigned env, unsigned cont)
{
    GC_GUARD;
    if (!syntax_arity_checked(id, 2, UINT_MAX, "define-macro")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned sig = cadr(id);
    if (!IS_PAIR(sig)) {
        show_error("define-macro: invalid syntax");
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned name = car(sig);
    unsigned params = cdr(sig);
    unsigned mbody = cddr(id);
    // Protect all values before lambda_params_valid, which allocates
    gc_protect(&name);
    gc_protect(&params);
    gc_protect(&mbody); // Must protect - used in alloc_cons
    gc_protect(&env);
    gc_protect(&cont);
    if (!identifier_valid(name) || !mbody ||
        !list_length_checked(mbody, NULL, "define-macro") ||
        !lambda_params_valid(params)) {
        show_error("define-macro: invalid syntax");
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned mbody_env = alloc_cons(mbody, env);
    unsigned p = make_typed_cell(BT_MACRO, params, mbody_env);
    gc_protect(&p);
    if (defvar(name, p, env) == TOK_ERROR) {
        cps_signal_current_error(env, cont);
        return true;
    }
    tramp_apply(name, cont);
    return true;
}

bool handle_define_syntax(unsigned id, unsigned env, unsigned cont)
{
    GC_GUARD;
    if (!syntax_arity_checked(id, 2, 2, "define-syntax")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned name = cadr(id);
    unsigned transformer_form = caddr(id);
    if (!identifier_valid(name)) {
        show_error("define-syntax: expected name");
        cps_signal_current_error(env, cont);
        return true;
    }
    gc_protect(&name);
    gc_protect(&transformer_form);
    gc_protect(&env);
    gc_protect(&cont);
    if (!syntax_rules_valid(transformer_form, syntax_default_ellipsis_id(env),
                            "define-syntax")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned p = make_syntax_transformer(transformer_form, env);
    if (p == TOK_ERROR) {
        cps_signal_current_error(env, cont);
        return true;
    }
    gc_protect(&p);
    if (defvar(name, p, env) == TOK_ERROR) {
        cps_signal_current_error(env, cont);
        return true;
    }
    tramp_apply(name, cont);
    return true;
}

bool handle_let_syntax(unsigned id, unsigned env, unsigned cont)
{
    // (let-syntax ((name (syntax-rules ...)) ...) body ...)
    if (!syntax_arity_checked(id, 2, UINT_MAX, "let-syntax")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned bindings = cadr(id);
    unsigned body = cddr(id);
    if (!binding_list_valid(bindings, "let-syntax") ||
        !list_length_checked(body, NULL, "let-syntax")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    // If no bindings, use outer env directly (transparent for internal defines)
    if (!bindings) {
        eval_body(body, env, cont);
        return true;
    }
    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&body);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned new_env = extend_env_empty(env);
    gc_protect(&new_env);
    // Use outer env for closure (let-syntax semantics)
    if (!bind_syntax_rules(bindings, new_env, env, "let-syntax"))
        return true;
    eval_body(body, new_env, cont);
    return true;
}

bool handle_letrec_syntax(unsigned id, unsigned env, unsigned cont)
{
    // (letrec-syntax ((name (syntax-rules ...)) ...) body ...)
    if (!syntax_arity_checked(id, 2, UINT_MAX, "letrec-syntax")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    unsigned bindings = cadr(id);
    unsigned body = cddr(id);
    if (!binding_list_valid(bindings, "letrec-syntax") ||
        !list_length_checked(body, NULL, "letrec-syntax")) {
        cps_signal_current_error(env, cont);
        return true;
    }
    // If no bindings, use outer env directly (transparent for internal defines)
    if (!bindings) {
        eval_body(body, env, cont);
        return true;
    }
    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&body);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned new_env = extend_env_empty(env);
    gc_protect(&new_env);
    // Use new_env for closure (letrec-syntax semantics)
    if (!bind_syntax_rules(bindings, new_env, new_env, "letrec-syntax"))
        return true;
    eval_body(body, new_env, cont);
    return true;
}

// ============================================================================
// Special Form Dispatch Table
// ============================================================================

typedef struct {
    int64_t *kw_ptr; // Pointer to keyword ID in ctx
    special_form_handler handler;
} special_form_entry;

// Get keyword pointer by offset in lisp_context
#define KW_ENTRY(name, handler_fn) {&ctx.kw_##name, handler_fn}

bool dispatch_special_form(int64_t kw, unsigned id, unsigned env, unsigned cont)
{
    unsigned binding = lookup_silent(kw, env);
    if (binding != TOK_ERROR) {
        if (IS_SYNTAX(binding)) {
            GC_GUARD;
            gc_protect(&id);
            gc_protect(&env);
            gc_protect(&cont);
            gc_protect(&binding);
            if (!eval_note_macro_expansion()) {
                cps_signal_current_error(env, cont);
                return true;
            }
            unsigned bindings = 0;
            gc_protect(&bindings);
            unsigned expanded = apply_syntax(binding, id, env, &bindings);
            gc_protect(&expanded);
            if (expanded == TOK_ERROR) {
                cps_signal_current_error(env, cont);
                return true;
            }
            if (bindings)
                apply_syntax_bindings(env, bindings);
            tramp_eval(expanded, env, cont);
            return true;
        }
        return false;
    }

    // Check each special form
    if (kw == ctx.kw_quote)
        return handle_quote(id, env, cont);
    if (kw == ctx.kw_lambda)
        return handle_lambda(id, env, cont);
    if (kw == ctx.kw_if)
        return handle_if(id, env, cont);
    if (kw == ctx.kw_begin)
        return handle_begin(id, env, cont);
    if (kw == ctx.kw_set)
        return handle_set(id, env, cont);
    if (kw == ctx.kw_define)
        return handle_define(id, env, cont);
    if (kw == ctx.kw_and)
        return handle_and(id, env, cont);
    if (kw == ctx.kw_or)
        return handle_or(id, env, cont);
    if (kw == ctx.kw_cond)
        return handle_cond(id, env, cont);
    if (kw == ctx.kw_cond_expand)
        return handle_cond_expand(id, env, cont);
    if (kw == ctx.kw_let)
        return handle_let(id, env, cont);
    if (kw == ctx.kw_letstar)
        return handle_letstar(id, env, cont);
    if (kw == ctx.kw_letrec)
        return handle_letrec(id, env, cont);
    if (kw == ctx.kw_quasiquote)
        return handle_quasiquote(id, env, cont);
    if (kw == ctx.kw_define_macro)
        return handle_define_macro(id, env, cont);
    if (kw == ctx.kw_define_syntax)
        return handle_define_syntax(id, env, cont);
    if (kw == ctx.kw_let_syntax)
        return handle_let_syntax(id, env, cont);
    if (kw == ctx.kw_letrec_syntax)
        return handle_letrec_syntax(id, env, cont);

    return false; // Not a special form
}
