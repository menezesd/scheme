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
 * Special forms (if, lambda, define, let, etc.) are handled in eval_step().
 * Each creates appropriate continuations for its subexpressions.
 *
 * ## Function Application
 * 1. Evaluate function expression (CONT_EVAL_FN)
 * 2. Evaluate arguments left-to-right (CONT_EVAL_ARGS)
 * 3. Apply function to evaluated arguments (apply_function)
 */

#include "eval.h"
#include "context.h"
#include "env.h"
#include "macros.h"
#include "primitives.h"
#include "reader.h"
#include <ctype.h>
#include <string.h>

// ============================================================================
// Forward Declarations
// ============================================================================

static void eval_step(void);
static void apply_cont_step(void);
static void apply_function(unsigned fn, unsigned args, unsigned env,
                           unsigned cont);
static unsigned qq_expand_cps(unsigned x, unsigned env);

// ============================================================================
// Helper Functions
// ============================================================================

// Check if transformer_form starts with (syntax-rules ...)
static inline bool is_syntax_rules(unsigned transformer_form)
{
    return IS_KEYWORD(car(transformer_form), ctx.kw_syntax_rules);
}

// Evaluate a body sequence (handles single vs multiple expressions)
static inline void eval_body(unsigned body, unsigned env, unsigned cont)
{
    if (!body) {
        tramp_apply(0, cont);
    } else if (!cdr(body)) {
        tramp_eval(car(body), env, cont);
    } else {
        unsigned k = make_cont(CONT_LET_BODY, cdr(body), env, cont);
        tramp_eval(car(body), env, k);
    }
}

// Evaluate a sequence of expressions with given continuation type
static inline void eval_seq(unsigned data, enum cont_type cont_type,
                            unsigned env, unsigned next)
{
    if (!cdr(data)) {
        tramp_eval(car(data), env, next);
    } else {
        unsigned k = make_cont(cont_type, cdr(data), env, next);
        tramp_eval(car(data), env, k);
    }
}

// Create a syntax transformer from a syntax-rules form
static inline unsigned make_syntax_transformer(unsigned transformer_form,
                                               unsigned closure_env)
{
    unsigned literals = cadr(transformer_form);
    unsigned rules = cddr(transformer_form);
    return make_typed_cell(BT_SYNTAX, alloc_cons(literals, rules), closure_env);
}

// Bind syntax transformers from a list of bindings
// Returns true on success, false on error (after calling tramp_error)
static bool bind_syntax_rules(unsigned bindings, unsigned def_env,
                              unsigned closure_env, const char *context)
{
    FORLIST(b, bindings) {
        unsigned binding = car(b);
        unsigned name = car(binding);
        unsigned transformer_form = cadr(binding);
        if (!is_syntax_rules(transformer_form)) {
            show_error("%s: expected syntax-rules", context);
            tramp_error();
            return false;
        }
        unsigned p = make_syntax_transformer(transformer_form, closure_env);
        defvar(name, p, def_env);
    }
    return true;
}

// ============================================================================
// Quasiquote Expansion
// ============================================================================

static unsigned qq_expand_cps(unsigned x, unsigned env)
{
    if (!IS_PAIR(x)) {
        return x;
    }

    // Check for (unquote expr)
    if (IS_KEYWORD(car(x), ctx.kw_unquote)) {
        return eval_cps(cadr(x), env);
    }

    // Check for (unquote-splicing expr) - error at top level
    if (IS_KEYWORD(car(x), ctx.kw_unquote_splicing)) {
        show_error("unquote-splicing not inside list");
        return TOK_ERROR;
    }

    // Recursively process list
    unsigned result = 0, tail = 0;
    for (unsigned l = x; IS_PAIR(l); l = cdr(l)) {
        unsigned elem = car(l);

        // Check if element is (unquote-splicing expr)
        if (IS_PAIR(elem) && IS_KEYWORD(car(elem), ctx.kw_unquote_splicing)) {
            unsigned spliced = eval_cps(cadr(elem), env);
            if (spliced == TOK_ERROR)
                return spliced;
            for (; IS_PAIR(spliced); spliced = cdr(spliced)) {
                list_append(&result, &tail, car(spliced));
            }
        } else {
            unsigned expanded = qq_expand_cps(elem, env);
            if (expanded == TOK_ERROR)
                return expanded;
            list_append(&result, &tail, expanded);
        }
    }
    return result;
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

            if (kw == ctx.kw_quote) {
                tramp_apply(cadr(id), cont);
                return;
            }

            if (kw == ctx.kw_lambda) {
                unsigned p = make_typed_cell(BT_FUNCTION, cadr(id),
                                             alloc_cons(cddr(id), env));
                tramp_apply(p, cont);
                return;
            }

            if (kw == ctx.kw_if) {
                unsigned branches = alloc_cons(caddr(id), cadddr(id));
                unsigned k = make_cont(CONT_IF, branches, env, cont);
                tramp_eval(cadr(id), env, k);
                return;
            }

            if (kw == ctx.kw_begin) {
                unsigned seq = cdr(id);
                if (!seq) {
                    tramp_apply(0, cont);
                    return;
                }
                if (!cdr(seq)) {
                    tramp_eval(car(seq), env, cont);
                    return;
                }
                unsigned k = make_cont(CONT_BEGIN, cdr(seq), env, cont);
                tramp_eval(car(seq), env, k);
                return;
            }

            if (kw == ctx.kw_set) {
                unsigned var = cadr(id);
                unsigned k = make_cont(CONT_SET, var, env, cont);
                tramp_eval(caddr(id), env, k);
                return;
            }

            if (kw == ctx.kw_define) {
                unsigned vid = cadr(id);
                if (IS_PAIR(vid)) {
                    // Function definition shorthand
                    unsigned p = make_typed_cell(BT_FUNCTION, cdr(vid),
                                                 alloc_cons(cddr(id), env));
                    defvar(car(vid), p, env);
                    tramp_apply(car(vid), cont);
                    return;
                }
                unsigned k = make_cont(CONT_DEFINE, vid, env, cont);
                tramp_eval(caddr(id), env, k);
                return;
            }

            if (kw == ctx.kw_and) {
                unsigned seq = cdr(id);
                if (!seq) {
                    tramp_apply(ctx.atom_true, cont);
                    return;
                }
                if (!cdr(seq)) {
                    tramp_eval(car(seq), env, cont);
                    return;
                }
                unsigned k = make_cont(CONT_AND, cdr(seq), env, cont);
                tramp_eval(car(seq), env, k);
                return;
            }

            if (kw == ctx.kw_or) {
                unsigned seq = cdr(id);
                if (!seq) {
                    tramp_apply(0, cont);
                    return;
                }
                if (!cdr(seq)) {
                    tramp_eval(car(seq), env, cont);
                    return;
                }
                unsigned k = make_cont(CONT_OR, cdr(seq), env, cont);
                tramp_eval(car(seq), env, k);
                return;
            }

            if (kw == ctx.kw_cond) {
                unsigned clauses = cdr(id);
                if (!clauses) {
                    tramp_apply(0, cont);
                    return;
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
                        unsigned k =
                            make_cont(CONT_BEGIN, cdr(conseq), env, cont);
                        tramp_eval(car(conseq), env, k);
                    }
                    return;
                }
                unsigned data = alloc_cons(cdr(clause), rest_clauses);
                unsigned k = make_cont(CONT_COND_TEST, data, env, cont);
                tramp_eval(test, env, k);
                return;
            }

            // Check for macro overrides on let/let*/letrec
            if (kw == ctx.kw_let || kw == ctx.kw_letstar ||
                kw == ctx.kw_letrec) {
                unsigned mac = lookup(kw, env);
                if (mac != TOK_ERROR && IS_SYNTAX(mac)) {
                    unsigned transformer = car(mac);
                    unsigned expanded = apply_syntax(transformer, id, env);
                    if (expanded == TOK_ERROR) {
                        tramp_error();
                        return;
                    }
                    tramp_eval(expanded, env, cont);
                    return;
                }
            }

            if (kw == ctx.kw_let) {
                unsigned bindings = cadr(id);
                unsigned body = cddr(id);
                if (!bindings) {
                    eval_body(body, env, cont);
                    return;
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
                return;
            }

            if (kw == ctx.kw_letstar) {
                unsigned bindings = cadr(id);
                unsigned body = cddr(id);
                if (!bindings) {
                    eval_body(body, env, cont);
                    return;
                }
                unsigned data = alloc_cons(bindings, body);
                unsigned first = car(bindings);
                unsigned k = make_cont(CONT_LETSTAR_VALS, data, env, cont);
                tramp_eval(cadr(first), env, k);
                return;
            }

            if (kw == ctx.kw_letrec) {
                unsigned bindings = cadr(id);
                unsigned body = cddr(id);
                unsigned vars = 0, vals = 0;
                unsigned vars_tail = 0, vals_tail = 0;
                FORLIST(b, bindings) {
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
                    return;
                }
                // Protect bindings from GC during nested allocation
                gc_protect(&bindings);
                unsigned inner = alloc_cons(vals, body);
                unsigned data = alloc_cons(bindings, inner);
                gc_unprotect(1);
                unsigned k = make_cont(CONT_LETREC_INIT, data, new_env, cont);
                tramp_eval(cadr(car(bindings)), new_env, k);
                return;
            }

            if (kw == ctx.kw_quasiquote) {
                unsigned result = qq_expand_cps(cadr(id), env);
                if (result == TOK_ERROR) {
                    tramp_error();
                    return;
                }
                tramp_apply(result, cont);
                return;
            }

            if (kw == ctx.kw_define_macro) {
                unsigned sig = cadr(id);
                unsigned name = car(sig);
                unsigned params = cdr(sig);
                unsigned mbody = cddr(id);
                unsigned p = make_typed_cell(BT_MACRO, params,
                                             alloc_cons(mbody, env));
                defvar(name, p, env);
                tramp_apply(name, cont);
                return;
            }

            if (kw == ctx.kw_define_syntax) {
                unsigned name = cadr(id);
                unsigned transformer_form = caddr(id);
                if (!is_syntax_rules(transformer_form)) {
                    show_error("define-syntax: expected syntax-rules");
                    tramp_error();
                    return;
                }
                unsigned p = make_syntax_transformer(transformer_form, env);
                defvar(name, p, env);
                tramp_apply(name, cont);
                return;
            }

            if (kw == ctx.kw_let_syntax) {
                // (let-syntax ((name (syntax-rules ...)) ...) body ...)
                unsigned bindings = cadr(id);
                unsigned body = cddr(id);
                unsigned new_env = extend_env_empty(env);
                // Use outer env for closure (let-syntax semantics)
                if (!bind_syntax_rules(bindings, new_env, env, "let-syntax"))
                    return;
                eval_body(body, new_env, cont);
                return;
            }

            if (kw == ctx.kw_letrec_syntax) {
                // (letrec-syntax ((name (syntax-rules ...)) ...) body ...)
                unsigned bindings = cadr(id);
                unsigned body = cddr(id);
                unsigned new_env = extend_env_empty(env);
                // Use new_env for closure (letrec-syntax semantics)
                if (!bind_syntax_rules(bindings, new_env, new_env,
                                       "letrec-syntax"))
                    return;
                eval_body(body, new_env, cont);
                return;
            }
        }

        // Not a special form - evaluate function position first
        unsigned k = make_cont(CONT_EVAL_FN, cdr(id), env, cont);
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

    switch (type) {
    case CONT_HALT:
        tramp_done(val);
        return;

    case CONT_IF:
        if (val) {
            tramp_eval(car(data), env, next);
        } else {
            tramp_eval(cdr(data), env, next);
        }
        return;

    case CONT_BEGIN:
        eval_seq(data, CONT_BEGIN, env, next);
        return;

    case CONT_SET: {
        unsigned result = setvar(CELL_ID(data), val, env);
        if (result == TOK_ERROR) {
            tramp_error();
            return;
        }
        tramp_apply(val, next);
        return;
    }

    case CONT_DEFINE:
        defvar(data, val, env);
        tramp_apply(data, next);
        return;

    case CONT_AND:
        if (!val) {
            tramp_apply(0, next);
            return;
        }
        eval_seq(data, CONT_AND, env, next);
        return;

    case CONT_OR:
        if (val) {
            tramp_apply(val, next);
            return;
        }
        eval_seq(data, CONT_OR, env, next);
        return;

    case CONT_COND_TEST: {
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
                        unsigned k2 =
                            make_cont(CONT_BEGIN, cdr(conseq2), env, next);
                        tramp_eval(car(conseq2), env, k2);
                    }
                    return;
                }
                unsigned new_data = alloc_cons(cdr(clause), cdr(rest));
                unsigned k2 = make_cont(CONT_COND_TEST, new_data, env, next);
                tramp_eval(test, env, k2);
            }
        }
        return;
    }

    case CONT_LET_VALS: {
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
        return;
    }

    case CONT_LET_BODY:
        if (!cdr(data)) {
            tramp_eval(car(data), env, next);
        } else {
            unsigned k2 = make_cont(CONT_LET_BODY, cdr(data), env, next);
            tramp_eval(car(data), env, k2);
        }
        return;

    case CONT_LETSTAR_VALS: {
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
        return;
    }

    case CONT_LETREC_INIT: {
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
        return;
    }

    case CONT_EVAL_FN: {
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
        return;
    }

    case CONT_EVAL_ARGS: {
        unsigned fn = car(data);
        unsigned evaled_and_rest = cdr(data);
        unsigned evaled_rev = car(evaled_and_rest);
        unsigned remaining = cdr(evaled_and_rest);

        unsigned new_evaled_rev = alloc_cons(val, evaled_rev);

        if (!remaining) {
            // Protect fn from GC during argument reversal allocations
            gc_protect(&fn);
            unsigned args = 0;
            FORLIST(l, new_evaled_rev) {
                args = alloc_cons(car(l), args);
            }
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
        return;
    }

    case CONT_APPLY_FUNC:
        if (!cdr(data)) {
            tramp_eval(car(data), env, next);
        } else {
            unsigned k2 = make_cont(CONT_APPLY_FUNC, cdr(data), env, next);
            tramp_eval(car(data), env, k2);
        }
        return;

    case CONT_MACRO_EXPAND:
        tramp_eval(val, env, next);
        return;

    case CONT_CALLWITHVALUES: {
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
        return;
    }

    default:
        show_error("unknown continuation type: %d", type);
        tramp_error();
        return;
    }
}

// ============================================================================
// Function Application
// ============================================================================

static void apply_function(unsigned fn, unsigned args, unsigned env,
                           unsigned cont)
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
            unsigned cont_args = alloc_cons(cont, 0);
            apply_function(proc, cont_args, env, cont);
            return;
        }

        // Special handling for apply
        if (prim_id == PAPPLY) {
            if (!args || !cdr(args) || cddr(args)) {
                show_error("apply expects exactly two arguments");
                tramp_error();
                return;
            }
            unsigned proc = car(args);
            unsigned proc_args = cadr(args);
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
            // Create continuation to handle producer's result
            unsigned k = make_cont(CONT_CALLWITHVALUES, consumer, env, cont);
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
                unsigned k = make_cont(CONT_BEGIN, cdr(exprs), env, cont);
                tramp_eval(car(exprs), env, k);
            }
            return;
        }

        unsigned result = apply_primitive(prim_id, args);
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

        unsigned frame = bind_params(params, args);
        unsigned new_env = alloc_cons(frame, def_env);

        if (!cdr(body)) {
            tramp_eval(car(body), new_env, cont);
        } else {
            unsigned k = make_cont(CONT_APPLY_FUNC, cdr(body), new_env, cont);
            tramp_eval(car(body), new_env, k);
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

    show_error("not a function");
    tramp_error();
}

// ============================================================================
// Main Evaluator Entry Points
// ============================================================================

unsigned eval_cps(unsigned expr, unsigned env)
{
    tramp_eval(expr, env, make_halt_cont());

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
