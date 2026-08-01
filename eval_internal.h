/**
 * @file eval_internal.h
 * @brief Internal header for evaluator - special form and continuation handlers
 */

#ifndef EVAL_INTERNAL_H
#define EVAL_INTERNAL_H

#include "compiled_pattern.h"
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
    return IS_PAIR(transformer_form) &&
           IS_KEYWORD(car(transformer_form), ctx.kw_syntax_rules);
}

static inline bool syntax_ellipsis_atom(unsigned x, int64_t ellipsis_id)
{
    return ellipsis_id != 0 && IS_ATOM(x) && CELL_ID(x) == ellipsis_id;
}

static inline int64_t syntax_default_ellipsis_id(unsigned env)
{
    return lookup_silent(ctx.kw_ellipsis, env) == TOK_ERROR ? ctx.kw_ellipsis
                                                            : 0;
}

static inline void syntax_rules_parts(unsigned transformer_form,
                                      int64_t default_ellipsis_id,
                                      int64_t *ellipsis_id,
                                      unsigned *literals, unsigned *rules)
{
    unsigned second = cadr(transformer_form);
    if (IS_ATOM(second)) {
        *ellipsis_id = CELL_ID(second);
        *literals = caddr(transformer_form);
        *rules = cdddr(transformer_form);
    } else {
        *ellipsis_id = default_ellipsis_id;
        *literals = second;
        *rules = cddr(transformer_form);
    }
}

static inline bool syntax_pattern_valid_at(unsigned pattern,
                                           int64_t ellipsis_id,
                                           const char *context,
                                           unsigned structural_depth)
{
    if (structural_depth >= 1024) {
        show_error("%s: pattern nesting too deep", context);
        return false;
    }
    if (syntax_ellipsis_atom(pattern, ellipsis_id)) {
        show_error("%s: invalid ellipsis in pattern", context);
        return false;
    }
    if (IS_PAIR(pattern) && pair_chain_is_circular(pattern)) {
        show_error("%s: circular pattern", context);
        return false;
    }
    if (IS_PAIR(pattern)) {
        unsigned it = pattern;
        while (IS_PAIR(it)) {
            unsigned elem = car(it);
            unsigned rest = cdr(it);
            if (syntax_ellipsis_atom(elem, ellipsis_id)) {
                show_error("%s: invalid ellipsis in pattern", context);
                return false;
            }
            if (IS_PAIR(rest) && syntax_ellipsis_atom(car(rest), ellipsis_id)) {
                if (!syntax_pattern_valid_at(elem, ellipsis_id, context,
                                             structural_depth + 1))
                    return false;
                it = cdr(rest);
                continue;
            }
            if (!syntax_pattern_valid_at(elem, ellipsis_id, context,
                                         structural_depth + 1))
                return false;
            it = rest;
        }
        if (it && !syntax_pattern_valid_at(it, ellipsis_id, context,
                                           structural_depth + 1))
            return false;
    } else if (IS_VECTOR(pattern)) {
        unsigned len = vector_len(pattern);
        unsigned *data = vector_data_ptr(pattern);
        bool saw_ellipsis = false;
        for (unsigned i = 0; i < len; i++) {
            if (syntax_ellipsis_atom(data[i], ellipsis_id)) {
                if (i == 0 || saw_ellipsis) {
                    show_error("%s: invalid ellipsis in pattern", context);
                    return false;
                }
                saw_ellipsis = true;
                continue;
            }
            if (!syntax_pattern_valid_at(data[i], ellipsis_id, context,
                                         structural_depth + 1))
                return false;
        }
    }
    return true;
}

static inline bool syntax_pattern_valid(unsigned pattern, int64_t ellipsis_id,
                                        const char *context)
{
    return syntax_pattern_valid_at(pattern, ellipsis_id, context, 0);
}

static inline bool syntax_pattern_atom_is_variable(unsigned x, unsigned literals,
                                                   int64_t ellipsis_id)
{
    if (!identifier_valid(x) || syntax_ellipsis_atom(x, ellipsis_id) ||
        IS_KEYWORD(x, ctx.kw_underscore))
        return false;

    FORLIST(lit, literals) {
        if (IS_ATOM(car(lit)) && CELL_ID(car(lit)) == CELL_ID(x))
            return false;
    }

    return true;
}

static inline unsigned syntax_pattern_var_count(unsigned pattern,
                                                unsigned literals,
                                                int64_t ellipsis_id,
                                                int64_t id)
{
    if (!pattern || syntax_ellipsis_atom(pattern, ellipsis_id))
        return 0;
    if (IS_ATOM(pattern))
        return syntax_pattern_atom_is_variable(pattern, literals,
                                               ellipsis_id) &&
                       CELL_ID(pattern) == id
                   ? 1
                   : 0;
    if (IS_PAIR(pattern)) {
        return syntax_pattern_var_count(car(pattern), literals, ellipsis_id,
                                        id) +
               syntax_pattern_var_count(cdr(pattern), literals, ellipsis_id,
                                        id);
    }
    if (IS_VECTOR(pattern)) {
        unsigned count = 0;
        unsigned len = vector_len(pattern);
        unsigned *data = vector_data_ptr(pattern);
        for (unsigned i = 0; i < len; i++) {
            count += syntax_pattern_var_count(data[i], literals, ellipsis_id,
                                              id);
        }
        return count;
    }
    return 0;
}

static inline bool syntax_pattern_vars_distinct_from(unsigned pattern,
                                                     unsigned whole_pattern,
                                                     unsigned literals,
                                                     int64_t ellipsis_id,
                                                     const char *context)
{
    if (!pattern || syntax_ellipsis_atom(pattern, ellipsis_id))
        return true;
    if (IS_ATOM(pattern)) {
        if (syntax_pattern_atom_is_variable(pattern, literals, ellipsis_id) &&
            syntax_pattern_var_count(whole_pattern, literals, ellipsis_id,
                                     CELL_ID(pattern)) > 1) {
            show_error("%s: duplicate pattern variable", context);
            return false;
        }
        return true;
    }
    if (IS_PAIR(pattern)) {
        return syntax_pattern_vars_distinct_from(car(pattern), whole_pattern,
                                                 literals, ellipsis_id,
                                                 context) &&
               syntax_pattern_vars_distinct_from(cdr(pattern), whole_pattern,
                                                 literals, ellipsis_id,
                                                 context);
    }
    if (IS_VECTOR(pattern)) {
        unsigned len = vector_len(pattern);
        unsigned *data = vector_data_ptr(pattern);
        for (unsigned i = 0; i < len; i++) {
            if (!syntax_pattern_vars_distinct_from(data[i], whole_pattern,
                                                   literals, ellipsis_id,
                                                   context))
                return false;
        }
    }
    return true;
}

static inline bool syntax_pattern_vars_distinct(unsigned pattern,
                                                unsigned literals,
                                                int64_t ellipsis_id,
                                                const char *context)
{
    return syntax_pattern_vars_distinct_from(pattern, pattern, literals,
                                             ellipsis_id, context);
}

static inline bool syntax_literals_valid(unsigned literals, int64_t ellipsis_id,
                                         const char *context)
{
    FORLIST(lit, literals) {
        unsigned literal = car(lit);
        if (!identifier_valid(literal) ||
            syntax_ellipsis_atom(literal, ellipsis_id)) {
            show_error("%s: invalid literal", context);
            return false;
        }
        for (unsigned rest = cdr(lit); rest; rest = cdr(rest)) {
            if (IS_ATOM(car(rest)) &&
                CELL_ID(car(rest)) == CELL_ID(literal)) {
                show_error("%s: duplicate literal", context);
                return false;
            }
        }
    }
    return true;
}

static inline bool syntax_pattern_var_depth_from(unsigned pattern,
                                                 unsigned literals,
                                                 int64_t ellipsis_id,
                                                 int64_t id, unsigned depth,
                                                 unsigned *depth_out)
{
    if (!pattern || syntax_ellipsis_atom(pattern, ellipsis_id))
        return false;
    if (IS_ATOM(pattern)) {
        if (syntax_pattern_atom_is_variable(pattern, literals, ellipsis_id) &&
            CELL_ID(pattern) == id) {
            *depth_out = depth;
            return true;
        }
        return false;
    }
    if (IS_PAIR(pattern)) {
        unsigned it = pattern;
        while (IS_PAIR(it)) {
            unsigned elem = car(it);
            unsigned rest = cdr(it);
            unsigned elem_depth = depth;
            if (IS_PAIR(rest) && syntax_ellipsis_atom(car(rest),
                                                      ellipsis_id)) {
                elem_depth++;
                it = cdr(rest);
            } else {
                it = rest;
            }
            if (syntax_pattern_var_depth_from(elem, literals, ellipsis_id, id,
                                              elem_depth, depth_out))
                return true;
        }
        return syntax_pattern_var_depth_from(it, literals, ellipsis_id, id,
                                             depth, depth_out);
    }
    if (IS_VECTOR(pattern)) {
        unsigned len = vector_len(pattern);
        unsigned *data = vector_data_ptr(pattern);
        for (unsigned i = 0; i < len; i++) {
            if (syntax_ellipsis_atom(data[i], ellipsis_id))
                continue;
            unsigned elem_depth = depth;
            if (i + 1 < len && syntax_ellipsis_atom(data[i + 1],
                                                    ellipsis_id))
                elem_depth++;
            if (syntax_pattern_var_depth_from(data[i], literals, ellipsis_id,
                                              id, elem_depth, depth_out))
                return true;
        }
    }
    return false;
}

static inline bool syntax_pattern_var_depth(unsigned pattern, unsigned literals,
                                            int64_t ellipsis_id, int64_t id,
                                            unsigned *depth_out)
{
    return syntax_pattern_var_depth_from(pattern, literals, ellipsis_id, id, 0,
                                         depth_out);
}

static inline bool syntax_template_has_depth_var(unsigned tmpl,
                                                unsigned pattern,
                                                unsigned literals,
                                                int64_t ellipsis_id,
                                                unsigned template_depth)
{
    if (!tmpl || syntax_ellipsis_atom(tmpl, ellipsis_id))
        return false;
    if (IS_ATOM(tmpl)) {
        unsigned pattern_depth = 0;
        return syntax_pattern_var_depth(pattern, literals, ellipsis_id,
                                        CELL_ID(tmpl), &pattern_depth) &&
               pattern_depth >= template_depth;
    }
    if (IS_PAIR(tmpl)) {
        unsigned it = tmpl;
        for (; IS_PAIR(it); it = cdr(it)) {
            if (syntax_template_has_depth_var(car(it), pattern, literals,
                                              ellipsis_id, template_depth))
                return true;
        }
        return syntax_template_has_depth_var(it, pattern, literals,
                                             ellipsis_id, template_depth);
    }
    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        unsigned *data = vector_data_ptr(tmpl);
        for (unsigned i = 0; i < len; i++) {
            if (syntax_template_has_depth_var(data[i], pattern, literals,
                                              ellipsis_id, template_depth))
                return true;
        }
    }
    return false;
}

static inline bool syntax_template_valid_at(unsigned tmpl, unsigned pattern,
                                            unsigned literals,
                                            int64_t ellipsis_id,
                                            const char *context,
                                            unsigned template_depth,
                                            unsigned structural_depth)
{
    if (structural_depth >= 1024) {
        show_error("%s: template nesting too deep", context);
        return false;
    }
    if (syntax_ellipsis_atom(tmpl, ellipsis_id)) {
        show_error("%s: invalid ellipsis in template", context);
        return false;
    }
    if (IS_PAIR(tmpl) && pair_chain_is_circular(tmpl)) {
        show_error("%s: circular template", context);
        return false;
    }
    if (IS_ATOM(tmpl)) {
        unsigned pattern_depth = 0;
        if (syntax_pattern_var_depth(pattern, literals, ellipsis_id,
                                     CELL_ID(tmpl), &pattern_depth) &&
            pattern_depth != template_depth) {
            show_error("%s: invalid ellipsis in template", context);
            return false;
        }
        return true;
    }
    if (IS_PAIR(tmpl)) {
        unsigned it = tmpl;
        while (IS_PAIR(it)) {
            unsigned elem = car(it);
            unsigned rest = cdr(it);
            if (syntax_ellipsis_atom(elem, ellipsis_id)) {
                show_error("%s: invalid ellipsis in template", context);
                return false;
            }
            if (IS_PAIR(rest) && syntax_ellipsis_atom(car(rest), ellipsis_id)) {
                if (!syntax_template_valid_at(elem, pattern, literals,
                                              ellipsis_id, context,
                                              template_depth + 1,
                                              structural_depth + 1))
                    return false;
                if (!syntax_template_has_depth_var(elem, pattern, literals,
                                                   ellipsis_id,
                                                   template_depth + 1)) {
                    show_error("%s: invalid ellipsis in template", context);
                    return false;
                }
                it = cdr(rest);
                continue;
            }
            if (!syntax_template_valid_at(elem, pattern, literals, ellipsis_id,
                                          context, template_depth,
                                          structural_depth + 1))
                return false;
            it = rest;
        }
        if (it && !syntax_template_valid_at(it, pattern, literals, ellipsis_id,
                                            context, template_depth,
                                            structural_depth + 1))
            return false;
    } else if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        unsigned *data = vector_data_ptr(tmpl);
        bool saw_ellipsis = false;
        for (unsigned i = 0; i < len; i++) {
            if (syntax_ellipsis_atom(data[i], ellipsis_id)) {
                if (i == 0 || saw_ellipsis) {
                    show_error("%s: invalid ellipsis in template", context);
                    return false;
                }
                if (!syntax_template_has_depth_var(data[i - 1], pattern,
                                                   literals, ellipsis_id,
                                                   template_depth + 1)) {
                    show_error("%s: invalid ellipsis in template", context);
                    return false;
                }
                saw_ellipsis = true;
                continue;
            }
            unsigned elem_depth = template_depth;
            if (i + 1 < len && syntax_ellipsis_atom(data[i + 1],
                                                    ellipsis_id))
                elem_depth++;
            if (!syntax_template_valid_at(data[i], pattern, literals,
                                          ellipsis_id, context, elem_depth,
                                          structural_depth + 1))
                return false;
        }
    }
    return true;
}

static inline bool syntax_template_valid(unsigned tmpl, unsigned pattern,
                                         unsigned literals,
                                         int64_t ellipsis_id,
                                         const char *context)
{
    return syntax_template_valid_at(tmpl, pattern, literals, ellipsis_id,
                                    context, 0, 0);
}

static inline bool syntax_rules_valid(unsigned transformer_form,
                                      int64_t default_ellipsis_id,
                                      const char *context)
{
    if (!is_syntax_rules(transformer_form)) {
        show_error("%s: expected syntax-rules", context);
        return false;
    }

    unsigned len = 0;
    if (!list_length_checked(cdr(transformer_form), &len, context))
        return false;
    if (len < 1) {
        show_error("%s: invalid syntax-rules", context);
        return false;
    }

    unsigned second = cadr(transformer_form);
    if (IS_ATOM(second)) {
        if (len < 3 || !identifier_valid(second)) {
            show_error("%s: invalid syntax-rules", context);
            return false;
        }
    } else if (len < 2) {
        show_error("%s: invalid syntax-rules", context);
        return false;
    }
    int64_t ellipsis_id = 0;
    unsigned literals = 0;
    unsigned rules = 0;
    syntax_rules_parts(transformer_form, default_ellipsis_id, &ellipsis_id,
                       &literals, &rules);

    if (!list_length_checked(literals, NULL, context))
        return false;
    if (!syntax_literals_valid(literals, ellipsis_id, context))
        return false;

    if (!list_length_checked(rules, NULL, context))
        return false;
    FORLIST(r, rules) {
        unsigned rule = car(r);
        unsigned rule_len = 0;
        if (!IS_PAIR(rule) || !list_length_checked(rule, &rule_len, context) ||
            rule_len != 2 || !IS_PAIR(car(rule))) {
            show_error("%s: invalid syntax rule", context);
            return false;
        }
        unsigned rule_pattern = car(rule);
        if (!identifier_valid(car(rule_pattern)) ||
            syntax_ellipsis_atom(car(rule_pattern), ellipsis_id)) {
            show_error("%s: invalid syntax rule", context);
            return false;
        }
        unsigned pattern = cdr(rule_pattern);
        unsigned tmpl = cadr(rule);
        if (!syntax_pattern_valid(pattern, ellipsis_id, context) ||
            !syntax_pattern_vars_distinct(pattern, literals, ellipsis_id,
                                          context) ||
            !syntax_template_valid(tmpl, pattern, literals, ellipsis_id,
                                   context))
            return false;
    }

    return true;
}

// Check if a keyword is shadowed by a local binding
// Returns true if shadowed (caller should treat as procedure call)
static inline bool is_keyword_shadowed(int64_t kw, unsigned env)
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

// Evaluate a body sequence (handles single vs multiple expressions)
static inline void eval_body(unsigned body, unsigned env, unsigned cont)
{
    if (!body) {
        tramp_apply(0, cont);
    } else if (!cdr(body)) {
        tramp_eval(car(body), env, cont);
    } else {
        // Extract first expr and rest before allocation, protect all used after
        unsigned first_expr = car(body);
        unsigned rest = cdr(body);
        gc_protect(&first_expr);
        gc_protect(&rest);
        gc_protect(&env);
        gc_protect(&cont);
        unsigned k = make_cont(CONT_LET_BODY, rest, env, cont);
        gc_unprotect(4);
        tramp_eval(first_expr, env, k);
    }
}

// Evaluate a sequence of expressions with given continuation type
static inline void eval_seq(unsigned data, enum cont_type cont_type,
                            unsigned env, unsigned next)
{
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
        unsigned k = make_cont(cont_type, rest, env, next);
        gc_unprotect(4);
        tramp_eval(first_expr, env, k);
    }
}

// Create a syntax transformer from a syntax-rules form
// Handles: (syntax-rules (<literal> ...) <rule> ...)
//      or: (syntax-rules <ellipsis> (<literal> ...) <rule> ...)
static inline unsigned make_syntax_transformer_with_default_ellipsis(
    unsigned transformer_form, unsigned closure_env, int64_t default_ellipsis_id)
{
    int64_t ellipsis_id = 0; // 0 means no ellipsis (disabled/shadowed)
    unsigned literals = 0;
    unsigned rules = 0;
    syntax_rules_parts(transformer_form, default_ellipsis_id, &ellipsis_id,
                       &literals, &rules);

    gc_protect(&closure_env);
    gc_protect(&literals);
    gc_protect(&rules);

    // Compile patterns for each rule
    unsigned compiled_rules = 0;
    gc_protect(&compiled_rules);
    unsigned compiled_tail = 0;
    gc_protect(&compiled_tail);

    unsigned r = rules;
    gc_protect(&r);
    while (r) {
        unsigned rule = car(r);
        unsigned pattern = car(rule);
        unsigned tmpl = cadr(rule);

        // Skip the macro name in pattern (first element)
        if (IS_PAIR(pattern))
            pattern = cdr(pattern);

        // Compile the pattern
        compiled_pattern *cpat = compile_pattern(pattern, literals, ellipsis_id);
        if (!cpat) {
            show_error("syntax-rules: out of memory");
            gc_unprotect(6);
            return TOK_ERROR;
        }

        gc_protect(&tmpl);

        // Create a cell to hold the compiled pattern pointer
        unsigned cpat_cell = alloc();
        CELL_TYPE(cpat_cell) = BT_COMPILED_PATTERN;
        CELL_PTR(cpat_cell) = cpat;

        // Build compiled rule: ((cpat_cell . pattern) . template). The source
        // pattern is retained so apply_syntax can validate literal identifiers
        // by lexical binding after the fast structural matcher succeeds.
        gc_protect(&cpat_cell);
        unsigned compiled_pattern_pair = alloc_cons(cpat_cell, pattern);
        gc_protect(&compiled_pattern_pair);
        unsigned new_rule = alloc_cons(compiled_pattern_pair, tmpl);
        gc_protect(&new_rule);
        unsigned new_node = alloc_cons(new_rule, 0);
        gc_unprotect(4);

        if (!compiled_rules) {
            compiled_rules = new_node;
            compiled_tail = new_node;
        } else {
            cell_set_cdr(compiled_tail, new_node);
            compiled_tail = new_node;
        }
        r = cdr(r);
    }
    gc_unprotect(1);

    // Store ellipsis_id as a number cell
    unsigned ellipsis_cell = store(ellipsis_id);
    gc_protect(&ellipsis_cell);
    // Store: (ellipsis_cell . (literals . compiled_rules))
    unsigned lit_rules = alloc_cons(literals, compiled_rules);
    gc_protect(&lit_rules);
    unsigned car_val = alloc_cons(ellipsis_cell, lit_rules);
    gc_protect(&car_val);
    unsigned result = make_typed_cell(BT_SYNTAX, car_val, closure_env);
    gc_unprotect(8);
    return result;
}

static inline unsigned make_syntax_transformer(unsigned transformer_form,
                                               unsigned closure_env)
{
    return make_syntax_transformer_with_default_ellipsis(
        transformer_form, closure_env, syntax_default_ellipsis_id(closure_env));
}

// Bind syntax transformers from a list of bindings
// Returns true on success, false on error (after calling tramp_error)
static inline bool bind_syntax_rules(unsigned bindings, unsigned def_env,
                                     unsigned closure_env, const char *context)
{
    unsigned b = bindings;
    gc_protect(&b);
    while (b) {
        unsigned binding = car(b);
        unsigned name = car(binding);
        unsigned transformer_form = cadr(binding);
        if (!syntax_rules_valid(transformer_form,
                                syntax_default_ellipsis_id(closure_env),
                                context)) {
            gc_unprotect(1);
            tramp_error();
            return false;
        }
        gc_protect(&name);
        gc_protect(&def_env);
        unsigned p = make_syntax_transformer(transformer_form, closure_env);
        if (p == TOK_ERROR) {
            gc_unprotect(3);
            tramp_error();
            return false;
        }
        gc_protect(&p);
        if (defvar(name, p, def_env) == TOK_ERROR) {
            gc_unprotect(3);
            tramp_error();
            return false;
        }
        gc_unprotect(3);
        b = cdr(b);
    }
    gc_unprotect(1);
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
bool handle_cond_expand(unsigned id, unsigned env, unsigned cont);
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
bool eval_note_macro_expansion(void);
void eval_reset_macro_expansion_depth(void);

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
