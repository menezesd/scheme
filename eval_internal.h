/**
 * @file eval_internal.h
 * @brief Internal header for evaluator - special form and continuation handlers
 */

#ifndef EVAL_INTERNAL_H
#define EVAL_INTERNAL_H

#include "context.h"
#include "env.h"
#include "macros.h"
#include "primitives.h"
#include "reader.h"
#include <ctype.h>
#include <string.h>

// ============================================================================
// Special Form Handler Type
// ============================================================================

// Handler function signature: takes expression, env, continuation
// Returns true if handled, false if not a special form
typedef bool (*special_form_handler)(unsigned id, unsigned env, unsigned cont);

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
static inline bool bind_syntax_rules(unsigned bindings, unsigned def_env,
                                     unsigned closure_env, const char *context)
{
    FORLIST(b, bindings)
    {
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
// Special Form Handlers (defined in eval_forms.c)
// ============================================================================

bool handle_quote(unsigned id, unsigned env, unsigned cont);
bool handle_lambda(unsigned id, unsigned env, unsigned cont);
bool handle_if(unsigned id, unsigned env, unsigned cont);
bool handle_begin(unsigned id, unsigned env, unsigned cont);
bool handle_set(unsigned id, unsigned env, unsigned cont);
bool handle_define(unsigned id, unsigned env, unsigned cont);
bool handle_and(unsigned id, unsigned env, unsigned cont);
bool handle_or(unsigned id, unsigned env, unsigned cont);
bool handle_cond(unsigned id, unsigned env, unsigned cont);
bool handle_let(unsigned id, unsigned env, unsigned cont);
bool handle_letstar(unsigned id, unsigned env, unsigned cont);
bool handle_letrec(unsigned id, unsigned env, unsigned cont);
bool handle_quasiquote(unsigned id, unsigned env, unsigned cont);
bool handle_define_macro(unsigned id, unsigned env, unsigned cont);
bool handle_define_syntax(unsigned id, unsigned env, unsigned cont);
bool handle_let_syntax(unsigned id, unsigned env, unsigned cont);
bool handle_letrec_syntax(unsigned id, unsigned env, unsigned cont);

// Dispatch to special form handler by keyword ID
// Returns true if handled, false if not a special form
bool dispatch_special_form(int64_t kw, unsigned id, unsigned env,
                           unsigned cont);

// ============================================================================
// Continuation Handlers (defined in eval_cont.c)
// ============================================================================

// Handler function type for continuation dispatch
typedef void (*cont_handler_t)(unsigned val, unsigned data, unsigned env,
                               unsigned next);

void handle_cont_halt(unsigned val, unsigned data, unsigned env, unsigned next);
void handle_cont_if(unsigned val, unsigned data, unsigned env, unsigned next);
void handle_cont_begin(unsigned val, unsigned data, unsigned env,
                       unsigned next);
void handle_cont_set(unsigned val, unsigned data, unsigned env, unsigned next);
void handle_cont_define(unsigned val, unsigned data, unsigned env,
                        unsigned next);
void handle_cont_and(unsigned val, unsigned data, unsigned env, unsigned next);
void handle_cont_or(unsigned val, unsigned data, unsigned env, unsigned next);
void handle_cont_cond_test(unsigned val, unsigned data, unsigned env,
                           unsigned next);
void handle_cont_let_vals(unsigned val, unsigned data, unsigned env,
                          unsigned next);
void handle_cont_let_body(unsigned val, unsigned data, unsigned env,
                          unsigned next);
void handle_cont_letstar_vals(unsigned val, unsigned data, unsigned env,
                              unsigned next);
void handle_cont_letrec_init(unsigned val, unsigned data, unsigned env,
                             unsigned next);
void handle_cont_eval_fn(unsigned val, unsigned data, unsigned env,
                         unsigned next);
void handle_cont_eval_args(unsigned val, unsigned data, unsigned env,
                           unsigned next);
void handle_cont_apply_func(unsigned val, unsigned data, unsigned env,
                            unsigned next);
void handle_cont_macro_expand(unsigned val, unsigned data, unsigned env,
                              unsigned next);
void handle_cont_callwithvalues(unsigned val, unsigned data, unsigned env,
                                unsigned next);

// Continuation handler dispatch table (indexed by cont_type)
extern const cont_handler_t cont_handlers[CONT_COUNT];

// ============================================================================
// Quasiquote Expansion
// ============================================================================

unsigned qq_expand_cps(unsigned x, unsigned env);

// ============================================================================
// Function Application
// ============================================================================

void apply_function(unsigned fn, unsigned args, unsigned env, unsigned cont);

#endif // EVAL_INTERNAL_H
