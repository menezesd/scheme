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
    (void)env;
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
bool handle_lambda(unsigned id, unsigned env, unsigned cont)
{
    unsigned params = cadr(id);
    unsigned body = cddr(id);
    gc_protect(&params);
    gc_protect(&body);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned body_env = alloc_cons(body, env);
    unsigned p = make_typed_cell(BT_FUNCTION, params, body_env);
    gc_unprotect(4);
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
    gc_unprotect(5);
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
    if (!seq) {
        tramp_apply(0, cont);
        return true;
    }
    if (!cdr(seq)) {
        // Single expression - tail call optimization
        tramp_eval(car(seq), env, cont);
        return true;
    }
    unsigned first_expr = car(seq);
    unsigned rest = cdr(seq);
    gc_protect(&first_expr);
    gc_protect(&rest);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned k = make_cont(CONT_BEGIN, rest, env, cont);
    gc_unprotect(4);
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
    unsigned var = cadr(id);
    unsigned val_expr = caddr(id);
    gc_protect(&var);
    gc_protect(&val_expr);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned k = make_cont(CONT_SET, var, env, cont);
    gc_unprotect(4);
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
        unsigned body_env = alloc_cons(body, env);
        unsigned p = make_typed_cell(BT_FUNCTION, params, body_env);
        defvar(name, p, env);
        gc_unprotect(5);
        tramp_apply(name, cont);
        return true;
    }
    unsigned val_expr = caddr(id);
    gc_protect(&vid); // Must protect - used in make_cont
    gc_protect(&val_expr);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned k = make_cont(CONT_DEFINE, vid, env, cont);
    gc_unprotect(4);
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
    if (!seq) {
        tramp_apply(ctx.atom_true, cont);
        return true;
    }
    if (!cdr(seq)) {
        tramp_eval(car(seq), env, cont);
        return true;
    }
    unsigned first_expr = car(seq);
    unsigned rest = cdr(seq);
    gc_protect(&first_expr);
    gc_protect(&rest);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned k = make_cont(CONT_AND, rest, env, cont);
    gc_unprotect(4);
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
    if (!seq) {
        tramp_apply(ctx.atom_false, cont);
        return true;
    }
    if (!cdr(seq)) {
        tramp_eval(car(seq), env, cont);
        return true;
    }
    unsigned first_expr = car(seq);
    unsigned rest = cdr(seq);
    gc_protect(&first_expr);
    gc_protect(&rest);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned k = make_cont(CONT_OR, rest, env, cont);
    gc_unprotect(4);
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
    if (!clauses) {
        tramp_apply(0, cont);
        return true;
    }
    unsigned clause = car(clauses);
    unsigned test = car(clause);
    unsigned rest_clauses = cdr(clauses);

    if (IS_KEYWORD(test, ctx.kw_else)) {
        unsigned conseq = cdr(clause);
        if (!conseq) {
            tramp_apply(ctx.atom_true, cont);
        } else if (!cdr(conseq)) {
            tramp_eval(car(conseq), env, cont);
        } else {
            unsigned first_conseq = car(conseq);
            unsigned rest_conseq = cdr(conseq);
            gc_protect(&first_conseq);
            gc_protect(&rest_conseq);
            gc_protect(&env);
            gc_protect(&cont);
            unsigned k = make_cont(CONT_BEGIN, rest_conseq, env, cont);
            gc_unprotect(4);
            tramp_eval(first_conseq, env, k);
        }
        return true;
    }
    // Protect test, env, cont, and computed values across allocations
    unsigned conseq = cdr(clause);
    gc_protect(&test);
    gc_protect(&env);
    gc_protect(&cont);
    gc_protect(&conseq);
    gc_protect(&rest_clauses);
    unsigned data = alloc_cons(conseq, rest_clauses);
    unsigned k = make_cont(CONT_COND_TEST, data, env, cont);
    gc_unprotect(5);
    tramp_eval(test, env, k);
    return true;
}

/**
 * Handle let: (let ((var1 expr1) (var2 expr2) ...) body...)
 *
 * Parallel binding: all expressions evaluated in outer environment,
 * then all bindings made simultaneously in a new environment.
 * Body is evaluated in the new environment with tail call to last expr.
 */
bool handle_let(unsigned id, unsigned env, unsigned cont)
{
    unsigned bindings = cadr(id);
    unsigned body = cddr(id);
    if (!bindings) {
        eval_body(body, env, cont);
        return true;
    }
    unsigned first_binding = car(bindings);
    unsigned first_val_expr = cadr(first_binding);
    // Protect all values used across allocations
    gc_protect(&first_val_expr);
    gc_protect(&first_binding);
    gc_protect(&bindings);
    gc_protect(&body);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned vars = 0, vals = 0, inner = 0, middle = 0;
    gc_protect(&vars);
    gc_protect(&vals);
    gc_protect(&inner);
    gc_protect(&middle);
    vars = alloc_cons(car(first_binding), 0);
    vals = alloc_cons(0, 0);
    inner = alloc_cons(cdr(bindings), body);
    middle = alloc_cons(vals, inner);
    unsigned data = alloc_cons(vars, middle);
    unsigned k = make_cont(CONT_LET_VALS, data, env, cont);
    gc_unprotect(10);
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
    unsigned bindings = cadr(id);
    unsigned body = cddr(id);
    if (!bindings) {
        // Create empty scope for internal defines
        unsigned new_env = extend_env_empty(env);
        eval_body(body, new_env, cont);
        return true;
    }
    unsigned first = car(bindings);
    unsigned first_val_expr = cadr(first);
    gc_protect(&first_val_expr);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned data = alloc_cons(bindings, body);
    unsigned k = make_cont(CONT_LETSTAR_VALS, data, env, cont);
    gc_unprotect(3);
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
    unsigned bindings = cadr(id);
    unsigned body = cddr(id);

    if (!bindings) {
        eval_body(body, env, cont);
        return true;
    }

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
            write_barrier(vars_tail, vc); // GC may have promoted vars_tail
            write_barrier(vals_tail, vlc);
            CELL_CDR(vars_tail) = vc;
            CELL_CDR(vals_tail) = vlc;
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
    gc_unprotect(
        13); // inner1, inner2, inner3, bindings, body, env, cont, vars, vals,
             // vars_tail, vals_tail, new_env, first_val_expr
    tramp_eval(first_val_expr, new_env, k);
    return true;
}

bool handle_quasiquote(unsigned id, unsigned env, unsigned cont)
{
    unsigned result = qq_expand_cps(cadr(id), env);
    if (result == TOK_ERROR) {
        tramp_error();
        return true;
    }
    tramp_apply(result, cont);
    return true;
}

bool handle_define_macro(unsigned id, unsigned env, unsigned cont)
{
    unsigned sig = cadr(id);
    unsigned name = car(sig);
    unsigned params = cdr(sig);
    unsigned mbody = cddr(id);
    // Protect all values used after allocations
    gc_protect(&name);
    gc_protect(&params);
    gc_protect(&mbody); // Must protect - used in alloc_cons
    gc_protect(&env);
    gc_protect(&cont);
    unsigned mbody_env = alloc_cons(mbody, env);
    unsigned p = make_typed_cell(BT_MACRO, params, mbody_env);
    defvar(name, p, env);
    gc_unprotect(5);
    tramp_apply(name, cont);
    return true;
}

bool handle_define_syntax(unsigned id, unsigned env, unsigned cont)
{
    unsigned name = cadr(id);
    unsigned transformer_form = caddr(id);
    if (!is_syntax_rules(transformer_form)) {
        show_error("define-syntax: expected syntax-rules");
        tramp_error();
        return true;
    }
    unsigned p = make_syntax_transformer(transformer_form, env);
    defvar(name, p, env);
    tramp_apply(name, cont);
    return true;
}

bool handle_let_syntax(unsigned id, unsigned env, unsigned cont)
{
    // (let-syntax ((name (syntax-rules ...)) ...) body ...)
    unsigned bindings = cadr(id);
    unsigned body = cddr(id);
    // If no bindings, use outer env directly (transparent for internal defines)
    if (!bindings) {
        eval_body(body, env, cont);
        return true;
    }
    unsigned new_env = extend_env_empty(env);
    // Use outer env for closure (let-syntax semantics)
    if (!bind_syntax_rules(bindings, new_env, env, "let-syntax"))
        return true;
    eval_body(body, new_env, cont);
    return true;
}

bool handle_letrec_syntax(unsigned id, unsigned env, unsigned cont)
{
    // (letrec-syntax ((name (syntax-rules ...)) ...) body ...)
    unsigned bindings = cadr(id);
    unsigned body = cddr(id);
    // If no bindings, use outer env directly (transparent for internal defines)
    if (!bindings) {
        eval_body(body, env, cont);
        return true;
    }
    unsigned new_env = extend_env_empty(env);
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

bool dispatch_special_form(int64_t kw, unsigned id, unsigned env, unsigned cont)
{
    // Check each special form
    // quote can be shadowed by local bindings (R5RS allows this)
    if (kw == ctx.kw_quote) {
        if (is_keyword_shadowed(kw, env))
            return false; // Fall through to procedure call
        return handle_quote(id, env, cont);
    }
    if (kw == ctx.kw_lambda)
        return handle_lambda(id, env, cont);
    if (kw == ctx.kw_if)
        return handle_if(id, env, cont);
    // begin can be shadowed by local bindings (R5RS allows this)
    if (kw == ctx.kw_begin) {
        if (is_keyword_shadowed(kw, env))
            return false; // Fall through to procedure call
        return handle_begin(id, env, cont);
    }
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

    // Check for macro overrides on let/let*/letrec
    if (kw == ctx.kw_let || kw == ctx.kw_letstar || kw == ctx.kw_letrec) {
        unsigned mac = lookup_silent(kw, env);
        if (mac != TOK_ERROR && IS_SYNTAX(mac)) {
            // Protect all values that might be in nursery before macro
            // expansion
            gc_protect(&id);
            gc_protect(&env);
            gc_protect(&cont);
            gc_protect(&mac);
            unsigned expanded = apply_syntax(mac, id, env);
            gc_unprotect(4);
            if (expanded == TOK_ERROR) {
                tramp_error();
                return true;
            }
            tramp_eval(expanded, env, cont);
            return true;
        }
    }

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
