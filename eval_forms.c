/**
 * @file eval_forms.c
 * @brief Special form handlers for the evaluator
 *
 * Each handler processes a specific special form (quote, lambda, if, etc.)
 * and sets up the appropriate continuation for evaluation.
 */

#include "eval_internal.h"

// ============================================================================
// Special Form Handlers
// ============================================================================

bool handle_quote(unsigned id, unsigned env, unsigned cont)
{
    (void)env;
    tramp_apply(cadr(id), cont);
    return true;
}

bool handle_lambda(unsigned id, unsigned env, unsigned cont)
{
    unsigned p =
        make_typed_cell(BT_FUNCTION, cadr(id), alloc_cons(cddr(id), env));
    tramp_apply(p, cont);
    return true;
}

bool handle_if(unsigned id, unsigned env, unsigned cont)
{
    unsigned branches = alloc_cons(caddr(id), cadddr(id));
    unsigned k = make_cont(CONT_IF, branches, env, cont);
    tramp_eval(cadr(id), env, k);
    return true;
}

bool handle_begin(unsigned id, unsigned env, unsigned cont)
{
    unsigned seq = cdr(id);
    if (!seq) {
        tramp_apply(0, cont);
        return true;
    }
    if (!cdr(seq)) {
        tramp_eval(car(seq), env, cont);
        return true;
    }
    unsigned k = make_cont(CONT_BEGIN, cdr(seq), env, cont);
    tramp_eval(car(seq), env, k);
    return true;
}

bool handle_set(unsigned id, unsigned env, unsigned cont)
{
    unsigned var = cadr(id);
    unsigned k = make_cont(CONT_SET, var, env, cont);
    tramp_eval(caddr(id), env, k);
    return true;
}

bool handle_define(unsigned id, unsigned env, unsigned cont)
{
    unsigned vid = cadr(id);
    if (IS_PAIR(vid)) {
        // Function definition shorthand
        unsigned p =
            make_typed_cell(BT_FUNCTION, cdr(vid), alloc_cons(cddr(id), env));
        defvar(car(vid), p, env);
        tramp_apply(car(vid), cont);
        return true;
    }
    unsigned k = make_cont(CONT_DEFINE, vid, env, cont);
    tramp_eval(caddr(id), env, k);
    return true;
}

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
    unsigned k = make_cont(CONT_AND, cdr(seq), env, cont);
    tramp_eval(car(seq), env, k);
    return true;
}

bool handle_or(unsigned id, unsigned env, unsigned cont)
{
    unsigned seq = cdr(id);
    if (!seq) {
        tramp_apply(0, cont);
        return true;
    }
    if (!cdr(seq)) {
        tramp_eval(car(seq), env, cont);
        return true;
    }
    unsigned k = make_cont(CONT_OR, cdr(seq), env, cont);
    tramp_eval(car(seq), env, k);
    return true;
}

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
            unsigned k = make_cont(CONT_BEGIN, cdr(conseq), env, cont);
            tramp_eval(car(conseq), env, k);
        }
        return true;
    }
    unsigned data = alloc_cons(cdr(clause), rest_clauses);
    unsigned k = make_cont(CONT_COND_TEST, data, env, cont);
    tramp_eval(test, env, k);
    return true;
}

bool handle_let(unsigned id, unsigned env, unsigned cont)
{
    unsigned bindings = cadr(id);
    unsigned body = cddr(id);
    if (!bindings) {
        eval_body(body, env, cont);
        return true;
    }
    unsigned first_binding = car(bindings);
    unsigned vars = alloc_cons(car(first_binding), 0);
    unsigned vals = alloc_cons(0, 0);
    // Protect vars/vals from GC during nested allocations
    gc_protect(&vars);
    gc_protect(&vals);
    unsigned inner = alloc_cons(cdr(bindings), body);
    unsigned middle = alloc_cons(vals, inner);
    unsigned data = alloc_cons(vars, middle);
    gc_unprotect(2);
    unsigned k = make_cont(CONT_LET_VALS, data, env, cont);
    tramp_eval(cadr(first_binding), env, k);
    return true;
}

bool handle_letstar(unsigned id, unsigned env, unsigned cont)
{
    unsigned bindings = cadr(id);
    unsigned body = cddr(id);
    if (!bindings) {
        eval_body(body, env, cont);
        return true;
    }
    unsigned data = alloc_cons(bindings, body);
    unsigned first = car(bindings);
    unsigned k = make_cont(CONT_LETSTAR_VALS, data, env, cont);
    tramp_eval(cadr(first), env, k);
    return true;
}

bool handle_letrec(unsigned id, unsigned env, unsigned cont)
{
    unsigned bindings = cadr(id);
    unsigned body = cddr(id);
    unsigned vars = 0, vals = 0;
    unsigned vars_tail = 0, vals_tail = 0;
    FORLIST(b, bindings)
    {
        unsigned var = caar(b);
        unsigned vc = alloc_cons(var, 0);
        unsigned vlc = alloc_cons(0, 0);
        if (!vars) {
            vars = vc;
            vals = vlc;
        } else {
            CELL_CDR(vars_tail) = vc;
            CELL_CDR(vals_tail) = vlc;
        }
        vars_tail = vc;
        vals_tail = vlc;
    }
    unsigned new_env = extend_env(vars, vals, env);
    if (!bindings) {
        eval_body(body, new_env, cont);
        return true;
    }
    // Protect bindings from GC during nested allocation
    gc_protect(&bindings);
    unsigned inner = alloc_cons(vals, body);
    unsigned data = alloc_cons(bindings, inner);
    gc_unprotect(1);
    unsigned k = make_cont(CONT_LETREC_INIT, data, new_env, cont);
    tramp_eval(cadr(car(bindings)), new_env, k);
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
    unsigned p = make_typed_cell(BT_MACRO, params, alloc_cons(mbody, env));
    defvar(name, p, env);
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

bool dispatch_special_form(int64_t kw, unsigned id, unsigned env, unsigned cont)
{
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

    // Check for macro overrides on let/let*/letrec
    if (kw == ctx.kw_let || kw == ctx.kw_letstar || kw == ctx.kw_letrec) {
        unsigned mac = lookup(kw, env);
        if (mac != TOK_ERROR && IS_SYNTAX(mac)) {
            unsigned transformer = car(mac);
            unsigned expanded = apply_syntax(transformer, id, env);
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
