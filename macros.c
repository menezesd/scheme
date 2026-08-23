/**
 * @file macros.c
 * @brief Hygienic macro system (syntax-rules)
 *
 * This file implements R5RS-style hygienic macros using syntax-rules.
 *
 * ## Syntax-Rules Structure
 * A syntax-rules object is stored as:
 *   ((ellipsis . (literals . rules)) . closure_env)
 * where rules is: ((pattern1 . template1) (pattern2 . template2) ...)
 *
 * ## Pattern Matching
 * Patterns can contain:
 * - Literals: Must match exactly (compared by symbol ID)
 * - Pattern variables: Match any expression, bind for template
 * - Underscore (_): Wildcard, matches anything, no binding
 * - Lists: Match list structure recursively
 * - Ellipsis (...): Match zero or more of preceding pattern
 * - Vectors: Match vector literals
 *
 * ## Template Expansion
 * Templates can use:
 * - Pattern variables: Replaced with matched values
 * - Ellipsis: Replicate preceding template for each match
 * - Nested ellipsis: For nested pattern ellipsis
 *
 * ## Hygiene Implementation
 * The macro system uses a combination of techniques for hygiene:
 *
 * 1. **Gensym renaming**: Bindings introduced by the template (in let, lambda,
 *    etc.) are alpha-renamed to fresh gensyms. This prevents
 * template-introduced bindings from capturing references in the macro use site.
 *
 * 2. **Protection markers**: When a pattern variable is substituted with an
 *    identifier, that identifier is wrapped as (##protected## . id). This marks
 *    it as "coming from input" so that nested macro expansions don't rename it.
 *    The protection marker is preserved through:
 *    - hygienize_template: Skips protected wrappers
 *    - rename_in_template: Skips protected wrappers
 *    - unwrap_protected: Removes markers after expansion (but not inside
 *      syntax-rules, which will be processed separately)
 *
 * This handles the challenging case of nested macros where a substituted
 * identifier has the same name as a template-introduced binding:
 *
 *   (let ((x 1))
 *     (let-syntax ((foo (syntax-rules ()
 *                         ((_ y) (let-syntax ((bar (syntax-rules ()
 *                                                    ((_) (let ((x 2)) y)))))
 *                                  (bar))))))
 *       (foo x)))  ; => 1
 *
 * When foo expands, y is substituted with (##protected## . x). When bar's
 * template is hygienized, the let-introduced x is renamed to a reserved
 * gensym, but the protected x is preserved. Result:
 * (let ((##gensym##0 2)) x) => 1.
 *
 * 3. **Referential transparency**: Free identifiers in templates (not pattern
 *    variables, not special forms) are looked up in the macro's definition
 *    environment. If found, they are renamed to gensyms and the gensym is
 *    bound to the definition-time value in the use-site environment. This
 *    ensures that macros "close over" their definition environment:
 *
 *   (let-syntax ((foo (syntax-rules ()
 *                       ((_ expr) (+ expr 1)))))
 *     (let ((+ *))
 *       (foo 3)))  ; => 4, not 3
 *
 *    The + in foo's template refers to the + at definition time (addition),
 *    not the shadowed + at use time (multiplication).
 *
 *    Assignment targets introduced by a macro are renamed to definition-site
 *    aliases, so set! and define preserve referential transparency while still
 *    updating the intended binding.
 */

#include "macros.h"
#include "compiled_pattern.h"
#include "context.h"
#include "eval_internal.h"
#include "env.h"
#include <limits.h>
#include <string.h>

// External gensym counter
extern unsigned gensym_counter;

// Generate a fresh gensym atom
static unsigned do_gensym(void)
{
    char buf[32];
    snprintf(buf, sizeof(buf), "##gensym##%u", gensym_counter++);
    return atom_from_string(buf);
}

static bool generated_gensym_atom(unsigned atom)
{
    if (!atom_is_valid(atom))
        return false;
    const char *name = ctx.atom_table[CELL_ID(atom)];
    const char prefix[] = "##gensym##";
    size_t prefix_len = sizeof(prefix) - 1;
    if (!name || strncmp(name, prefix, prefix_len) != 0 || !name[prefix_len])
        return false;
    for (const char *p = name + prefix_len; *p; p++) {
        if (*p < '0' || *p > '9')
            return false;
    }
    return true;
}

// ============================================================================
// Helper Functions
// ============================================================================

static bool same_literal_binding(unsigned literal, unsigned input,
                                unsigned closure_env, unsigned use_env)
{
    if (!IS_ATOM(input) || CELL_ID(input) != CELL_ID(literal))
        return false;

    int64_t id = CELL_ID(literal);
    unsigned literal_binding = env_find_binding_cell(id, closure_env);
    unsigned input_binding = env_find_binding_cell(id, use_env);
    return literal_binding == input_binding;
}

static void append_bindings_chain(unsigned *dst, unsigned src)
{
    if (!dst)
        return;
    if (!src)
        return;

    if (!*dst) {
        *dst = src;
        return;
    }

    unsigned tail = *dst;
    while (IS_PAIR(tail) && cdr(tail))
        tail = cdr(tail);

    if (IS_PAIR(tail))
        cell_set_cdr(tail, src);
}

unsigned expand_form(unsigned expr, unsigned syn_env, unsigned *bindings_out);

static unsigned expand_form_list(unsigned list, unsigned syn_env,
                                unsigned *bindings_out);

static unsigned expand_form_vector(unsigned vec, unsigned syn_env,
                                  unsigned *bindings_out)
{
    unsigned len = vector_len(vec);
    if (!len)
        return vec;

    GC_GUARD;
    gc_protect(&vec);
    gc_protect(&syn_env);

    // Build the result up front so expanded elements live in a GC-scanned
    // container while later elements are still being expanded.
    unsigned new_vec = make_vector(len, 0);
    if (new_vec == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&new_vec);

    bool changed = false;
    for (unsigned i = 0; i < len; i++) {
        unsigned expanded = expand_form(vector_data_ptr(vec)[i], syn_env,
                                        bindings_out);
        if (expanded == TOK_ERROR)
            return TOK_ERROR;
        if (expanded != vector_data_ptr(vec)[i])
            changed = true;
        vector_set_elem(new_vec, i, expanded);
    }

    return changed ? new_vec : vec;
}

static unsigned expand_form_list(unsigned list, unsigned syn_env,
                                unsigned *bindings_out)
{
    if (!list)
        return 0;

    GC_GUARD;
    gc_protect(&list);
    gc_protect(&syn_env);
    if (!IS_PAIR(list))
        return expand_form(list, syn_env, bindings_out);

    unsigned out = 0;
    unsigned out_tail = 0;
    gc_protect(&out);
    gc_protect(&out_tail);

    unsigned it = list;
    gc_protect(&it);
    while (IS_PAIR(it)) {
        unsigned elem = car(it);
        gc_protect(&elem);
        unsigned expanded = expand_form(elem, syn_env, bindings_out);
        if (expanded == TOK_ERROR)
            return TOK_ERROR;
        unsigned next = alloc_cons(expanded, 0);
        gc_protect(&next);
        if (!out)
            out = next;
        else
            cell_set_cdr(out_tail, next);
        out_tail = next;
        it = cdr(it);
    }

    unsigned tail = expand_form(it, syn_env, bindings_out);
    if (tail == TOK_ERROR)
        return TOK_ERROR;
    if (!out)
        return tail;

    if (tail)
        cell_set_cdr(out_tail, tail);

    return out;
}

static unsigned expand_let_syntax(unsigned form, unsigned syn_env,
                                 unsigned *bindings_out)
{
    const char *syn_arity =
        (CELL_ID(car(form)) == ctx.kw_let_syntax) ? "let-syntax" :
                                                   "letrec-syntax";
    GC_GUARD;
    gc_protect(&form);
    gc_protect(&syn_env);
    if (!syntax_arity_checked(form, 2, UINT_MAX, syn_arity))
        return TOK_ERROR;

    unsigned bindings = cadr(form);
    unsigned body = cddr(form);
    if (!binding_list_valid(bindings, syn_arity) ||
        !list_length_checked(body, NULL, syn_arity)) {
        return TOK_ERROR;
    }

    if (!bindings) {
        unsigned expanded_body = expand_form_list(body, syn_env, bindings_out);
        if (expanded_body == TOK_ERROR)
            return TOK_ERROR;
        return alloc_cons(car(form), alloc_cons(bindings, expanded_body));
    }

    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&body);
    gc_protect(&syn_env);

    unsigned expanded_env = extend_env_empty(syn_env);
    gc_protect(&expanded_env);
    int64_t ellipsis_id =
        syntax_default_ellipsis_id((CELL_ID(car(form)) == ctx.kw_let_syntax)
                                       ? syn_env
                                       : expanded_env);

    unsigned b = bindings;
    gc_protect(&b);
    while (b) {
        unsigned binding = car(b);
        if (!IS_PAIR(binding) || !identifier_valid(car(binding)) ||
            !is_syntax_rules(cadr(binding))) {
            return TOK_ERROR;
        }

        unsigned name = car(binding);
        unsigned transformer_form = cadr(binding);
        if (!syntax_rules_valid(transformer_form, ellipsis_id, syn_arity))
            return TOK_ERROR;

        unsigned p = make_syntax_transformer(
            transformer_form,
            (CELL_ID(car(form)) == ctx.kw_let_syntax) ? syn_env :
                                                        expanded_env);
        if (p == TOK_ERROR)
            return TOK_ERROR;

        if (defvar(name, p, expanded_env) == TOK_ERROR)
            return TOK_ERROR;

        b = cdr(b);
    }

    unsigned expanded_body = expand_form_list(body, expanded_env, bindings_out);
    if (expanded_body == TOK_ERROR)
        return TOK_ERROR;

    return alloc_cons(car(form), alloc_cons(bindings, expanded_body));
}

// The static expander recurses through macro outputs; a self-referential
// macro referenced from inside an explicit lambda body overflows the C
// stack before any engine-level depth check runs, so bound it here.
#define MAX_STATIC_EXPANSION_DEPTH 10000

static int expansion_depth = 0;

void macros_reset_expansion_depth(void)
{
    expansion_depth = 0;
}

static unsigned expand_form_inner(unsigned expr, unsigned syn_env,
                                  unsigned *bindings_out);

unsigned expand_form(unsigned expr, unsigned syn_env, unsigned *bindings_out)
{
    if (expansion_depth >= MAX_STATIC_EXPANSION_DEPTH) {
        show_error("macro expansion exceeded maximum depth");
        return TOK_ERROR;
    }
    expansion_depth++;
    unsigned result = expand_form_inner(expr, syn_env, bindings_out);
    expansion_depth--;
    return result;
}

// Bind every name in a formal-parameter list (proper or dotted) into a
// fresh frame over parent_env. Values are irrelevant placeholders: the
// frame exists so scope-sensitive decisions during expansion (ellipsis
// shadowing, macro-vs-variable dispatch) see the parameters as ordinary
// lexically bound names.
unsigned extend_env_with_binder_names(unsigned parent_env, unsigned params)
{
    GC_GUARD;
    gc_protect(&parent_env);
    gc_protect(&params);
    unsigned frame_env = extend_env_empty(parent_env);
    gc_protect(&frame_env);

    unsigned p = params;
    gc_protect(&p);
    while (IS_PAIR(p)) {
        unsigned b = car(p);
        gc_protect(&b);
        if (IS_ATOM(b) &&
            defvar(b, ctx.atom_false, frame_env) == TOK_ERROR)
            return TOK_ERROR;
        gc_unprotect(1);
        p = cdr(p);
    }
    if (p && IS_ATOM(p) &&
        defvar(p, ctx.atom_false, frame_env) == TOK_ERROR)
        return TOK_ERROR;

    return frame_env;
}

// Layer a frame with the names bound by a body's leading (define ...)
// forms over env, so later forms in that body expand with those names
// shadowing outer bindings. Mirrors the compiler's scan_internal_defines.
unsigned extend_env_with_internal_defines(unsigned env, unsigned body)
{
    GC_GUARD;
    gc_protect(&env);
    gc_protect(&body);
    unsigned frame = 0;
    gc_protect(&frame);

    for (unsigned it = body; IS_PAIR(it); it = cdr(it)) {
        unsigned form = car(it);
        if (!IS_PAIR(form) || !IS_ATOM(car(form)) ||
            CELL_ID(car(form)) != ctx.kw_define)
            break;
        unsigned target = cadr(form);
        unsigned name = IS_PAIR(target) ? car(target) : target;
        if (!IS_ATOM(name))
            break;
        if (!frame) {
            frame = extend_env_empty(env);
        }
        if (defvar(name, ctx.atom_false, frame) == TOK_ERROR)
            return TOK_ERROR;
    }

    return frame ? frame : env;
}

static unsigned expand_form_inner(unsigned expr, unsigned syn_env,
                                  unsigned *bindings_out)
{
    if (!expr || !IS_CELL(expr))
        return expr;

    if (IS_VECTOR(expr))
        return expand_form_vector(expr, syn_env, bindings_out);

    if (!IS_PAIR(expr))
        return expr;

    unsigned head = car(expr);
    if (IS_ATOM(head)) {
        int64_t kw = CELL_ID(head);
        if (kw != ctx.kw_quote && kw != ctx.kw_quasiquote &&
            kw != ctx.kw_cond_expand) {
            unsigned binding = lookup_silent(kw, syn_env);
            if (binding != TOK_ERROR && IS_SYNTAX(binding)) {
                GC_GUARD;
                gc_protect(&expr);
                gc_protect(&syn_env);
                unsigned produced_bindings = 0;
                gc_protect(&produced_bindings);
                unsigned expanded =
                    apply_syntax(binding, expr, syn_env, &produced_bindings);
                if (expanded == TOK_ERROR)
                    return TOK_ERROR;

                if (bindings_out && produced_bindings)
                    append_bindings_chain(bindings_out, produced_bindings);

                return expand_form(expanded, syn_env, bindings_out);
            }
        }

        if (kw == ctx.kw_quote || kw == ctx.kw_quasiquote ||
            kw == ctx.kw_cond_expand)
            return expr;

        if (kw == ctx.kw_define_syntax) {
            GC_GUARD;
            gc_protect(&expr);
            gc_protect(&syn_env);
            if (!syntax_arity_checked(expr, 2, 2, "define-syntax"))
                return TOK_ERROR;

            unsigned name = cadr(expr);
            unsigned transformer_form = caddr(expr);
            gc_protect(&name);
            gc_protect(&transformer_form);
            if (!identifier_valid(name)) {
                show_error("define-syntax: expected name");
                return TOK_ERROR;
            }
            if (!syntax_rules_valid(transformer_form,
                                   syntax_default_ellipsis_id(syn_env),
                                   "define-syntax")) {
                return TOK_ERROR;
            }

            unsigned p =
                make_syntax_transformer(transformer_form, syn_env);
            if (p == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&p);
            if (defvar(name, p, syn_env) == TOK_ERROR)
                return TOK_ERROR;

            return expr;
        }

        if (kw == ctx.kw_let_syntax || kw == ctx.kw_letrec_syntax)
            return expand_let_syntax(expr, syn_env, bindings_out);

        // Lambda bodies expand at their own creation (handle_lambda), so
        // the walker must not descend here: expanding them now would fire
        // syntax errors - and bake hygiene decisions - in the enclosing
        // scope instead of the lambda's own.
        if (kw == ctx.kw_lambda)
            return expr;
    }

    return expand_form_list(expr, syn_env, bindings_out);
}

static bool pattern_literals_match(unsigned pattern, unsigned input,
                                   unsigned literals, int64_t ellipsis_id,
                                   unsigned closure_env, unsigned use_env);

static bool vector_pattern_literals_match(unsigned pattern, unsigned input,
                                          unsigned literals,
                                          int64_t ellipsis_id,
                                          unsigned closure_env,
                                          unsigned use_env)
{
    if (!IS_VECTOR(input))
        return false;

    unsigned pat_len = vector_len(pattern);
    unsigned inp_len = vector_len(input);
    unsigned *pat_data = vector_data_ptr(pattern);
    unsigned ellipsis_pos = pat_len;

    for (unsigned i = 0; i < pat_len; i++) {
        if (syntax_is_ellipsis(pat_data[i], ellipsis_id)) {
            ellipsis_pos = i;
            break;
        }
    }

    if (ellipsis_pos == pat_len) {
        if (pat_len != inp_len)
            return false;
        for (unsigned i = 0; i < pat_len; i++) {
            pat_data = vector_data_ptr(pattern);
            unsigned *inp_data = vector_data_ptr(input);
            if (!pattern_literals_match(pat_data[i], inp_data[i], literals,
                                        ellipsis_id, closure_env, use_env))
                return false;
        }
        return true;
    }

    if (ellipsis_pos == 0)
        return false;

    unsigned pre_count = ellipsis_pos - 1;
    unsigned post_count = pat_len - ellipsis_pos - 1;
    if (inp_len < pre_count + post_count)
        return false;

    for (unsigned i = 0; i < pre_count; i++) {
        pat_data = vector_data_ptr(pattern);
        unsigned *inp_data = vector_data_ptr(input);
        if (!pattern_literals_match(pat_data[i], inp_data[i], literals,
                                    ellipsis_id, closure_env, use_env))
            return false;
    }

    unsigned repeat_count = inp_len - pre_count - post_count;
    for (unsigned i = 0; i < repeat_count; i++) {
        pat_data = vector_data_ptr(pattern);
        unsigned *inp_data = vector_data_ptr(input);
        if (!pattern_literals_match(pat_data[ellipsis_pos - 1],
                                    inp_data[pre_count + i], literals,
                                    ellipsis_id, closure_env, use_env))
            return false;
    }

    for (unsigned i = 0; i < post_count; i++) {
        pat_data = vector_data_ptr(pattern);
        unsigned *inp_data = vector_data_ptr(input);
        unsigned pat_idx = ellipsis_pos + 1 + i;
        unsigned inp_idx = inp_len - post_count + i;
        if (!pattern_literals_match(pat_data[pat_idx], inp_data[inp_idx],
                                    literals, ellipsis_id, closure_env,
                                    use_env))
            return false;
    }

    return true;
}

static bool pattern_literals_match(unsigned pattern, unsigned input,
                                   unsigned literals, int64_t ellipsis_id,
                                   unsigned closure_env, unsigned use_env)
{
    if (!pattern)
        return input == 0;

    if (IS_ATOM(pattern)) {
        int64_t id = CELL_ID(pattern);
        if (atom_list_contains_id(literals, id))
            return same_literal_binding(pattern, input, closure_env, use_env);
        if (syntax_is_underscore(pattern))
            return true;
        if (!identifier_valid(pattern))
            return IS_ATOM(input) && CELL_ID(input) == id;
        return true;
    }

    if (IS_VECTOR(pattern)) {
        return vector_pattern_literals_match(pattern, input, literals,
                                             ellipsis_id, closure_env, use_env);
    }

    if (IS_PAIR(pattern)) {
        if (IS_PAIR(cdr(pattern)) &&
            syntax_is_ellipsis(cadr(pattern), ellipsis_id)) {
            unsigned rest_pattern = cddr(pattern);
            unsigned elem_pattern = car(pattern);
            unsigned rest_input = input;

            while (IS_PAIR(rest_input)) {
                if (pattern_literals_match(rest_pattern, rest_input, literals,
                                           ellipsis_id, closure_env, use_env))
                    return true;
                if (!pattern_literals_match(elem_pattern, car(rest_input),
                                            literals, ellipsis_id, closure_env,
                                            use_env))
                    return false;
                rest_input = cdr(rest_input);
            }
            return pattern_literals_match(rest_pattern, rest_input, literals,
                                          ellipsis_id, closure_env, use_env);
        }

        // Fixed-arity patterns can be wide even when they are not nested.
        // Compare their list spine iteratively rather than recursing through
        // every cdr.
        for (;;) {
            if (!IS_PAIR(input) ||
                !pattern_literals_match(car(pattern), car(input), literals,
                                        ellipsis_id, closure_env, use_env))
                return false;
            pattern = cdr(pattern);
            input = cdr(input);
            if (!IS_PAIR(pattern))
                return pattern_literals_match(pattern, input, literals,
                                              ellipsis_id, closure_env,
                                              use_env);
            if (IS_PAIR(cdr(pattern)) &&
                syntax_is_ellipsis(cadr(pattern), ellipsis_id))
                return pattern_literals_match(pattern, input, literals,
                                              ellipsis_id, closure_env,
                                              use_env);
        }
    }

    // Self-evaluating literal data in a pattern (strings, bignums,
    // bytevectors, inexact numbers, ...) must match by content, not by cell
    // identity: two distinct-but-equal? literal cells (e.g. two separately
    // read "hello" strings) have unrelated CELL_IDs, so comparing those
    // directly rejected every such literal pattern.
    if (!IS_CELL(pattern) || !IS_CELL(input))
        return pattern == input;
    return deep_equal(pattern, input);
}

// Check if a pattern contains a variable (for ellipsis expansion)
static bool pattern_contains_var(unsigned pattern, unsigned var)
{
    if (!pattern)
        return false;
    if (IS_ATOM(pattern)) {
        return CELL_ID(pattern) == CELL_ID(var);
    }
    if (IS_PAIR(pattern)) {
        return pattern_contains_var(car(pattern), var) ||
               pattern_contains_var(cdr(pattern), var);
    }
    if (IS_VECTOR(pattern)) {
        unsigned len = vector_len(pattern);
        unsigned *data = vector_data_ptr(pattern);
        for (unsigned i = 0; i < len; i++) {
            if (pattern_contains_var(data[i], var))
                return true;
        }
    }
    return false;
}

// Look up a pattern variable in bindings
static unsigned syntax_lookup(unsigned var, unsigned bindings)
{
    for (; IS_PAIR(bindings); bindings = cdr(bindings)) {
        unsigned binding = car(bindings);
        if (!IS_PAIR(binding))
            continue;
        unsigned pvar = car(binding);
        if (IS_ATOM(pvar) && IS_ATOM(var) && CELL_ID(pvar) == CELL_ID(var)) {
            return cdr(binding);
        }
    }
    return TOK_ERROR;
}

// Find ellipsis binding that contains a given variable in its pattern
static unsigned find_ellipsis_binding(unsigned var, unsigned bindings)
{
    unsigned cons_match =
        0; // Remember first cons pattern match (lower priority)

    for (unsigned b = bindings; IS_PAIR(b); b = cdr(b)) {
        unsigned binding = car(b);
        if (!IS_PAIR(binding))
            continue;
        unsigned pvar = car(binding);
        unsigned val = cdr(binding);

        // Check for direct atom binding with list value (higher priority)
        if (IS_ATOM(pvar) && IS_ATOM(var) && CELL_ID(pvar) == CELL_ID(var) &&
            (!val || IS_PAIR(val))) {
            return binding;
        }

        // Remember first cons pattern match
        if (!cons_match && IS_PAIR(pvar) && (!val || IS_PAIR(val)) &&
            pattern_contains_var(pvar, var)) {
            cons_match = binding;
        }
    }

    return cons_match;
}

// Collect ALL ellipsis-bound variables from a template into a list
// Returns list of bindings: ((var1 . values1) (var2 . values2) ...)
static unsigned collect_ellipsis_vars(unsigned tmpl, unsigned bindings,
                                      unsigned collected)
{
    if (!tmpl)
        return collected;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    gc_protect(&collected);

    if (IS_ATOM(tmpl)) {
        unsigned binding = find_ellipsis_binding(tmpl, bindings);
        if (binding && IS_PAIR(cdr(binding))) {
            // Check if already collected
            for (unsigned c = collected; IS_PAIR(c); c = cdr(c)) {
                unsigned collected_binding = car(c);
                if (!IS_PAIR(collected_binding))
                    continue;
                unsigned collected_var = car(collected_binding);
                unsigned binding_var = car(binding);
                if (IS_ATOM(collected_var) && IS_ATOM(binding_var) &&
                    CELL_ID(collected_var) == CELL_ID(binding_var))
                    return collected; // Already have it
            }
            gc_protect(&binding);
            unsigned result = alloc_cons(binding, collected);
            return result;
        }
        return collected;
    }

    if (IS_PAIR(tmpl)) {
        while (IS_PAIR(tmpl)) {
            collected = collect_ellipsis_vars(car(tmpl), bindings, collected);
            tmpl = cdr(tmpl);
        }
        return collect_ellipsis_vars(tmpl, bindings, collected);
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            collected = collect_ellipsis_vars(data[i], bindings, collected);
        }
    }

    return collected;
}

static bool ellipsis_vars_iteration_count(unsigned ellipsis_vars,
                                          unsigned *iter_count)
{
    bool have_count = false;
    unsigned expected = 0;

    for (unsigned ev = ellipsis_vars; ev; ev = cdr(ev)) {
        if (!IS_PAIR(ev)) {
            show_error("syntax-rules: invalid ellipsis bindings");
            return false;
        }
        unsigned binding = car(ev);
        if (!IS_PAIR(binding)) {
            show_error("syntax-rules: invalid ellipsis bindings");
            return false;
        }
        unsigned values = cdr(binding);
        if (!proper_list_silent(values)) {
            show_error("syntax-rules: invalid ellipsis bindings");
            return false;
        }
        unsigned count = list_length(values);
        if (!have_count) {
            expected = count;
            have_count = true;
        } else if (count != expected) {
            show_error("syntax-rules: inconsistent ellipsis match lengths");
            return false;
        }
    }

    *iter_count = have_count ? expected : 0;
    return true;
}

// ============================================================================
// Hygienic Renaming
// ============================================================================

// Unwrap protected identifiers: (##protected## . x) → x
// Called after expansion to remove protection markers
static unsigned unwrap_protected(unsigned expr)
{
    if (!expr)
        return 0;

    GC_GUARD;
    gc_protect(&expr);

    // Check for protected wrapper
    if (IS_PAIR(expr)) {
        unsigned head = car(expr);
        if (IS_ATOM(head) && CELL_ID(head) == ctx.kw_protected) {
            // Unwrap: (##protected## . x) → x
            return cdr(expr);
        }

        // Don't unwrap inside syntax-rules - those templates will be
        // processed later by their own apply_syntax
        if (IS_ATOM(head) && CELL_ID(head) == ctx.kw_syntax_rules) {
            return expr;
        }
        // Recursively unwrap car and cdr
        unsigned new_car = unwrap_protected(car(expr));
        if (new_car == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_car);
        unsigned new_cdr = unwrap_protected(cdr(expr));
        if (new_cdr == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_cdr);
        unsigned result;
        if (new_car == car(expr) && new_cdr == cdr(expr))
            result = expr;
        else {
            result = alloc_cons(new_car, new_cdr);
        }
        return result;
    }

    if (IS_VECTOR(expr)) {
        unsigned len = vector_len(expr);
        bool changed = false;

        for (unsigned i = 0; i < len && !changed; i++) {
            // Refresh data pointer - GC may have moved expr
            unsigned *data = vector_data_ptr(expr);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned unwrapped = unwrap_protected(orig);
            if (unwrapped == TOK_ERROR)
                return TOK_ERROR;
            if (unwrapped != orig)
                changed = true;
            gc_unprotect(1);
        }

        if (!changed) {
            return expr;
        }

        // Protect expr across allocation - GC may move vectors
        gc_protect(&expr);
        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR) {
            gc_unprotect(1);
            return TOK_ERROR;
        }
        gc_protect(&new_vec);
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointers - GC may have moved vectors
            unsigned *data = vector_data_ptr(expr);
            unsigned elem = data[i];
            unsigned unwrapped = unwrap_protected(elem);
            if (unwrapped == TOK_ERROR) {
                gc_unprotect(2);
                return TOK_ERROR;
            }
            unsigned *new_data = vector_data_ptr(new_vec);
            new_data[i] = unwrapped;
        }
        gc_unprotect(2);
        return new_vec;
    }

    return expr;
}

// Check if a symbol ID is a pattern variable (bound in bindings list)
static bool is_pattern_var(int64_t sym_id, unsigned bindings)
{
    for (; bindings; bindings = cdr(bindings)) {
        unsigned binding = car(bindings);
        unsigned pvar = car(binding);
        if (IS_ATOM(pvar) && CELL_ID(pvar) == sym_id)
            return true;
        // Also check pattern pairs (nested pattern variables)
        if (IS_PAIR(pvar)) {
            unsigned p = pvar;
            for (; p; p = IS_PAIR(p) ? cdr(p) : 0) {
                unsigned elem = IS_PAIR(p) ? car(p) : p;
                if (IS_ATOM(elem) && CELL_ID(elem) == sym_id)
                    return true;
                if (!IS_PAIR(p))
                    break;
            }
        }
    }
    return false;
}

// Check if an identifier is a special form keyword
bool is_special_form(int64_t id)
{
    return id == ctx.kw_lambda || id == ctx.kw_if || id == ctx.kw_define ||
           id == ctx.kw_set || id == ctx.kw_quote || id == ctx.kw_begin ||
           id == ctx.kw_cond || id == ctx.kw_cond_expand ||
           id == ctx.kw_and || id == ctx.kw_or ||
           id == ctx.kw_let_syntax || id == ctx.kw_letrec_syntax ||
           id == ctx.kw_define_syntax || id == ctx.kw_syntax_rules ||
           id == ctx.kw_quasiquote || id == ctx.kw_unquote ||
           id == ctx.kw_unquote_splicing || id == ctx.kw_else ||
           id == ctx.kw_ellipsis || id == ctx.kw_underscore ||
           id == ctx.kw_let || id == ctx.kw_letstar || id == ctx.kw_letrec;
}

static bool binding_shadows_special(int64_t sym_id, unsigned bindings)
{
    return is_special_form(sym_id) && is_pattern_var(sym_id, bindings);
}

static bool keyword_bound_at_definition(int64_t sym_id, unsigned closure_env)
{
    if (!is_special_form(sym_id))
        return false;
    unsigned value = lookup_silent(sym_id, closure_env);
    return value != TOK_ERROR && !IS_SYNTAX(value);
}

static bool set_form_like(unsigned expr)
{
    return IS_PAIR(expr) && IS_ATOM(car(expr)) &&
           CELL_ID(car(expr)) == ctx.kw_set && IS_PAIR(cdr(expr)) &&
           IS_PAIR(cddr(expr)) && !cdddr(expr);
}

static bool define_form_like(unsigned expr)
{
    return IS_PAIR(expr) && IS_ATOM(car(expr)) &&
           CELL_ID(car(expr)) == ctx.kw_define && IS_PAIR(cdr(expr)) &&
           IS_PAIR(cddr(expr));
}

// Collect free identifiers from a template
// Returns a list of unique atoms that are free (not pattern vars, not special
// forms)
static unsigned collect_free_ids(unsigned tmpl, unsigned bindings,
                                 unsigned collected, int64_t ellipsis_id,
                                 unsigned closure_env);

static unsigned add_lexical_bound(unsigned bindings, unsigned var)
{
    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&var);

    unsigned binding = alloc_cons(var, 0);
    gc_protect(&binding);
    return alloc_cons(binding, bindings);
}

static unsigned add_lambda_bounds(unsigned bindings, unsigned params)
{
    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&params);

    unsigned p = params;
    gc_protect(&p);
    for (; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        unsigned param = IS_PAIR(p) ? car(p) : p;
        if (IS_ATOM(param))
            bindings = add_lexical_bound(bindings, param);
        if (!IS_PAIR(p))
            break;
    }
    gc_unprotect(1);

    return bindings;
}

static unsigned add_let_bounds(unsigned bindings, unsigned binding_list)
{
    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&binding_list);

    unsigned bl = binding_list;
    gc_protect(&bl);
    for (; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding))
            continue;
        unsigned var = car(binding);
        if (IS_ATOM(var))
            bindings = add_lexical_bound(bindings, var);
    }
    gc_unprotect(1);

    return bindings;
}

static unsigned add_named_let_bounds(unsigned bindings, unsigned name,
                                     unsigned binding_list)
{
    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&name);
    gc_protect(&binding_list);

    if (IS_ATOM(name))
        bindings = add_lexical_bound(bindings, name);
    return add_let_bounds(bindings, binding_list);
}

static unsigned collect_free_ids_let(unsigned tmpl, unsigned bindings,
                                     unsigned collected, int64_t ellipsis_id,
                                     bool is_letstar, bool is_letrec,
                                     unsigned closure_env)
{
    unsigned binding_list = cadr(tmpl);
    unsigned body = cddr(tmpl);

    GC_GUARD;
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&bindings);
    gc_protect(&collected);

    if (!proper_list_silent(binding_list))
        return collect_free_ids(body, bindings, collected, ellipsis_id,
                                closure_env);

    unsigned body_bindings = bindings;
    gc_protect(&body_bindings);
    if (is_letrec)
        body_bindings = add_let_bounds(body_bindings, binding_list);

    unsigned bl = binding_list;
    gc_protect(&bl);
    for (; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding))
            continue;

        unsigned val = cadr(binding);
        unsigned val_bindings =
            (is_letstar || is_letrec) ? body_bindings : bindings;
        collected =
            collect_free_ids(val, val_bindings, collected, ellipsis_id,
                             closure_env);

        if (is_letstar && !is_letrec && IS_ATOM(car(binding)))
            body_bindings = add_lexical_bound(body_bindings, car(binding));
    }
    gc_unprotect(1);

    if (!is_letstar && !is_letrec)
        body_bindings = add_let_bounds(body_bindings, binding_list);

    return collect_free_ids(body, body_bindings, collected, ellipsis_id,
                            closure_env);
}

static unsigned collect_free_ids_named_let(unsigned tmpl, unsigned bindings,
                                           unsigned collected,
                                           int64_t ellipsis_id,
                                           unsigned closure_env)
{
    unsigned name = cadr(tmpl);
    unsigned binding_list = caddr(tmpl);
    unsigned body = cdddr(tmpl);

    GC_GUARD;
    gc_protect(&name);
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&bindings);
    gc_protect(&collected);

    if (!proper_list_silent(binding_list))
        return collect_free_ids(body, bindings, collected, ellipsis_id,
                                closure_env);

    unsigned bl = binding_list;
    gc_protect(&bl);
    for (; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding))
            continue;
        collected =
            collect_free_ids(cadr(binding), bindings, collected, ellipsis_id,
                             closure_env);
    }
    gc_unprotect(1);

    unsigned body_bindings = add_named_let_bounds(bindings, name, binding_list);
    gc_protect(&body_bindings);
    return collect_free_ids(body, body_bindings, collected, ellipsis_id,
                            closure_env);
}

static unsigned collect_free_ids_quasiquote(unsigned tmpl, unsigned bindings,
                                            unsigned collected,
                                            int64_t ellipsis_id,
                                            unsigned depth,
                                            unsigned closure_env)
{
    if (!tmpl)
        return collected;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    gc_protect(&collected);

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if ((head_id == ctx.kw_unquote ||
                 head_id == ctx.kw_unquote_splicing) &&
                IS_PAIR(cdr(tmpl))) {
                unsigned body = cadr(tmpl);
                if (depth == 1) {
                    return collect_free_ids(body, bindings, collected,
                                            ellipsis_id, closure_env);
                }
                return collect_free_ids_quasiquote(
                    body, bindings, collected, ellipsis_id, depth - 1,
                    closure_env);
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl))) {
                return collect_free_ids_quasiquote(
                    cadr(tmpl), bindings, collected, ellipsis_id, depth + 1,
                    closure_env);
            }
        }

        // A quasiquoted datum may be a wide, flat list.  Preserve the
        // special unquote handling while avoiding one recursive call per cdr.
        for (;;) {
            collected = collect_free_ids_quasiquote(
                car(tmpl), bindings, collected, ellipsis_id, depth,
                closure_env);
            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next))
                return collect_free_ids_quasiquote(
                    next, bindings, collected, ellipsis_id, depth,
                    closure_env);
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_unquote ||
                    next_id == ctx.kw_unquote_splicing ||
                    next_id == ctx.kw_quasiquote)
                    return collect_free_ids_quasiquote(
                        next, bindings, collected, ellipsis_id, depth,
                        closure_env);
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            collected = collect_free_ids_quasiquote(
                data[i], bindings, collected, ellipsis_id, depth, closure_env);
        }
    }

    return collected;
}

static unsigned collect_free_ids(unsigned tmpl, unsigned bindings,
                                 unsigned collected, int64_t ellipsis_id,
                                 unsigned closure_env)
{
    if (!tmpl)
        return collected;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    gc_protect(&collected);

    if (IS_ATOM(tmpl)) {
        int64_t id = CELL_ID(tmpl);
        // Skip pattern variables, ellipsis, or already collected ids. Core
        // keyword names are still collected so lexical keyword bindings in the
        // macro definition environment remain referentially transparent.
        if (!is_pattern_var(id, bindings) && id != ellipsis_id &&
            !atom_list_contains_id(collected, id)) {
            // Add to collected list
            return alloc_cons(tmpl, collected);
        }
        return collected;
    }

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        // Skip protected wrappers and syntax-rules (they'll be processed later)
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if (head_id == ctx.kw_protected) {
                return collected;
            }
            if (syntax_rules_form_like(tmpl) &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env))
                return collected;
            // For quote, skip the quoted data
            if (head_id == ctx.kw_quote &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env)) {
                return collected;
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl)) &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env)) {
                return collect_free_ids_quasiquote(
                    cadr(tmpl), bindings, collected, ellipsis_id, 1,
                    closure_env);
            }
            if (head_id == ctx.kw_lambda &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env)) {
                if (!IS_PAIR(cdr(tmpl)) || !IS_PAIR(cddr(tmpl)))
                    return collected;
                unsigned body_bindings =
                    add_lambda_bounds(bindings, cadr(tmpl));
                gc_protect(&body_bindings);
                return collect_free_ids(cddr(tmpl), body_bindings, collected,
                                        ellipsis_id, closure_env);
            }
            if ((head_id == ctx.kw_let || head_id == ctx.kw_letstar ||
                 head_id == ctx.kw_letrec) &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env)) {
                if (!IS_PAIR(cdr(tmpl)) || !IS_PAIR(cddr(tmpl)))
                    return collected;
                if (head_id == ctx.kw_let && IS_ATOM(cadr(tmpl))) {
                    return collect_free_ids_named_let(
                        tmpl, bindings, collected, ellipsis_id, closure_env);
                }
                return collect_free_ids_let(
                    tmpl, bindings, collected, ellipsis_id,
                    head_id == ctx.kw_letstar, head_id == ctx.kw_letrec,
                    closure_env);
            }
            if (head_id == ctx.kw_set && set_form_like(tmpl) &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env)) {
                collected = collect_free_ids(cadr(tmpl), bindings, collected,
                                             ellipsis_id, closure_env);
                gc_protect(&collected);
                unsigned expr = cddr(tmpl);
                if (IS_PAIR(expr)) {
                    return collect_free_ids(car(expr), bindings, collected,
                                            ellipsis_id, closure_env);
                }
                return collected;
            }
            // For define, collect the target too. An introduced define target
            // must be renamed with introduced references to preserve hygiene.
            if (head_id == ctx.kw_define && define_form_like(tmpl) &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env)) {
                unsigned target = cadr(tmpl);
                if (IS_ATOM(target)) {
                    collected = collect_free_ids(target, bindings, collected,
                                                 ellipsis_id, closure_env);
                    gc_protect(&collected);
                }
                unsigned expr = cddr(tmpl);
                if (IS_PAIR(expr)) {
                    return collect_free_ids(car(expr), bindings, collected,
                                            ellipsis_id, closure_env);
                }
                return collected;
            }
        }
        // Traverse ordinary list spines iteratively.  Macro templates may
        // contain very large flat executable forms (for example a begin with
        // many expressions), for which recursive cdr traversal overflows the
        // C stack.
        for (;;) {
            collected = collect_free_ids(car(tmpl), bindings, collected,
                                         ellipsis_id, closure_env);
            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next))
                return collect_free_ids(next, bindings, collected,
                                        ellipsis_id, closure_env);

            // Keep specialized handling for a tail that begins a syntactic
            // form; otherwise it is ordinary list data and can stay in this
            // loop.
            if (IS_ATOM(car(next)) && is_special_form(CELL_ID(car(next))))
                return collect_free_ids(next, bindings, collected,
                                        ellipsis_id, closure_env);
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointer - GC may have moved tmpl
            unsigned *data = vector_data_ptr(tmpl);
            collected =
                collect_free_ids(data[i], bindings, collected, ellipsis_id,
                                 closure_env);
        }
        return collected;
    }

    return collected;
}

// Rename free identifiers in template according to rename_map
// rename_map is ((old_atom . new_gensym) ...)
static unsigned rename_free_ids(unsigned tmpl, unsigned rename_map);

static bool id_in_rename_map(int64_t id, unsigned rename_map)
{
    for (unsigned m = rename_map; m; m = cdr(m)) {
        unsigned entry = car(m);
        if (IS_PAIR(entry) && IS_ATOM(car(entry)) && CELL_ID(car(entry)) == id)
            return true;
    }
    return false;
}

static unsigned rename_free_ids_quasiquote(unsigned tmpl, unsigned rename_map,
                                           unsigned depth)
{
    if (!tmpl || !rename_map)
        return tmpl;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&rename_map);

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if ((head_id == ctx.kw_unquote ||
                 head_id == ctx.kw_unquote_splicing) &&
                IS_PAIR(cdr(tmpl))) {
                unsigned body = cadr(tmpl);
                unsigned new_body;
                if (depth == 1)
                    new_body = rename_free_ids(body, rename_map);
                else
                    new_body = rename_free_ids_quasiquote(body, rename_map,
                                                          depth - 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl))) {
                unsigned new_body = rename_free_ids_quasiquote(
                    cadr(tmpl), rename_map, depth + 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
        }

        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car =
                rename_free_ids_quasiquote(old_car, rename_map, depth);
            if (new_car == TOK_ERROR)
                return TOK_ERROR;
            if (new_car != old_car)
                changed = true;
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                unsigned new_tail =
                    rename_free_ids_quasiquote(next, rename_map, depth);
                if (new_tail == TOK_ERROR)
                    return TOK_ERROR;
                if (new_tail != next)
                    changed = true;
                if (!changed)
                    return original;
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                return result;
            }
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_unquote ||
                    next_id == ctx.kw_unquote_splicing ||
                    next_id == ctx.kw_quasiquote) {
                    unsigned new_tail = rename_free_ids_quasiquote(
                        next, rename_map, depth);
                    if (new_tail == TOK_ERROR)
                        return TOK_ERROR;
                    if (new_tail != next)
                        changed = true;
                    if (!changed)
                        return original;
                    cell_set_cdr(result_tail, new_tail);
                    return result;
                }
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        bool changed = false;
        for (unsigned i = 0; i < len && !changed; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned renamed =
                rename_free_ids_quasiquote(orig, rename_map, depth);
            if (renamed == TOK_ERROR)
                return TOK_ERROR;
            if (renamed != orig)
                changed = true;
            gc_unprotect(1);
        }
        if (!changed)
            return tmpl;

        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_vec);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            unsigned elem = data[i];
            unsigned renamed =
                rename_free_ids_quasiquote(elem, rename_map, depth);
            if (renamed == TOK_ERROR)
                return TOK_ERROR;
            unsigned *new_data = vector_data_ptr(new_vec);
            new_data[i] = renamed;
        }
        return new_vec;
    }

    return tmpl;
}

static unsigned rename_free_ids_inner(unsigned tmpl, unsigned rename_map,
                                      bool quote_shadowed,
                                      bool quasiquote_shadowed);

static unsigned rename_free_ids(unsigned tmpl, unsigned rename_map)
{
    return rename_free_ids_inner(tmpl, rename_map, false, false);
}

// Drop rename_map entries whose key is bound by params: within this
// lambda's body those names denote the parameters, not the free
// identifiers the renames were minted for.
static unsigned drop_renames_bound_by_params(unsigned rename_map,
                                             unsigned params)
{
    GC_GUARD;
    gc_protect(&rename_map);
    gc_protect(&params);
    unsigned out = 0;
    unsigned tail = 0;
    gc_protect(&out);
    gc_protect(&tail);
    for (unsigned m = rename_map; m; m = cdr(m)) {
        unsigned entry = car(m);
        if (IS_ATOM(car(entry)) &&
            lambda_params_bind_id(params, CELL_ID(car(entry))))
            continue;
        list_append(&out, &tail, entry);
    }
    return out;
}

static unsigned drop_renames_bound_by_binding_list(unsigned rename_map,
                                                   unsigned binding_list)
{
    GC_GUARD;
    gc_protect(&rename_map);
    gc_protect(&binding_list);
    unsigned out = 0;
    unsigned tail = 0;
    gc_protect(&out);
    gc_protect(&tail);
    for (unsigned m = rename_map; m; m = cdr(m)) {
        unsigned entry = car(m);
        if (IS_ATOM(car(entry)) &&
            binding_list_binds_id(binding_list, CELL_ID(car(entry))))
            continue;
        list_append(&out, &tail, entry);
    }
    return out;
}

static unsigned rename_free_ids_inner(unsigned tmpl, unsigned rename_map,
                                      bool quote_shadowed,
                                      bool quasiquote_shadowed)
{
    if (!tmpl || !rename_map)
        return tmpl;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&rename_map);

    if (IS_ATOM(tmpl)) {
        int64_t id = CELL_ID(tmpl);
        // Look up in rename_map
        for (unsigned m = rename_map; m; m = cdr(m)) {
            unsigned entry = car(m);
            if (IS_ATOM(car(entry)) && CELL_ID(car(entry)) == id) {
                return cdr(entry); // Return the gensym
            }
        }
        return tmpl;
    }

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        // Skip protected wrappers and syntax-rules
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            bool head_is_definition_alias =
                id_in_rename_map(head_id, rename_map);
            if (head_id == ctx.kw_protected ||
                (syntax_rules_form_like(tmpl) && !head_is_definition_alias)) {
                return tmpl;
            }
            if (head_id == ctx.kw_quote && !quote_shadowed &&
                !head_is_definition_alias) {
                return tmpl;
            }
            if (head_id == ctx.kw_lambda && !head_is_definition_alias &&
                IS_PAIR(cdr(tmpl)) && IS_PAIR(cddr(tmpl))) {
                bool body_quote_shadowed =
                    quote_shadowed ||
                    lambda_params_bind_id(cadr(tmpl), ctx.kw_quote);
                bool body_quasiquote_shadowed =
                    quasiquote_shadowed ||
                    lambda_params_bind_id(cadr(tmpl), ctx.kw_quasiquote);
                unsigned body_map =
                    drop_renames_bound_by_params(rename_map, cadr(tmpl));
                gc_protect(&body_map);
                unsigned new_body = rename_free_ids_inner(
                    cddr(tmpl), body_map, body_quote_shadowed,
                    body_quasiquote_shadowed);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                if (new_body == cddr(tmpl))
                    return tmpl;
                unsigned result_tail = alloc_cons(cadr(tmpl), new_body);
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
            if ((head_id == ctx.kw_let || head_id == ctx.kw_letstar ||
                 head_id == ctx.kw_letrec) &&
                !head_is_definition_alias &&
                IS_PAIR(cdr(tmpl)) && IS_PAIR(cddr(tmpl)) &&
                !(head_id == ctx.kw_let && IS_ATOM(cadr(tmpl))) &&
                proper_list_silent(cadr(tmpl))) {
                unsigned binding_list = cadr(tmpl);
                unsigned body = cddr(tmpl);
                unsigned new_bindings = rename_free_ids_inner(
                    binding_list, rename_map, quote_shadowed,
                    quasiquote_shadowed);
                if (new_bindings == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_bindings);
                bool body_quote_shadowed =
                    quote_shadowed ||
                    binding_list_binds_id(binding_list, ctx.kw_quote);
                bool body_quasiquote_shadowed =
                    quasiquote_shadowed ||
                    binding_list_binds_id(binding_list, ctx.kw_quasiquote);
                unsigned body_map =
                    drop_renames_bound_by_binding_list(rename_map,
                                                       binding_list);
                gc_protect(&body_map);
                unsigned new_body = rename_free_ids_inner(
                    body, body_map, body_quote_shadowed,
                    body_quasiquote_shadowed);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                if (new_bindings == binding_list && new_body == body)
                    return tmpl;
                unsigned result_tail = alloc_cons(new_bindings, new_body);
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl)) &&
                !quasiquote_shadowed && !head_is_definition_alias) {
                unsigned new_body =
                    rename_free_ids_quasiquote(cadr(tmpl), rename_map, 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
            if (head_id == ctx.kw_set && set_form_like(tmpl) &&
                !head_is_definition_alias) {
                unsigned var = cadr(tmpl);
                unsigned expr = cddr(tmpl);
                unsigned new_var =
                    rename_free_ids_inner(var, rename_map, quote_shadowed,
                                          quasiquote_shadowed);
                if (new_var == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_var);
                if (expr && IS_PAIR(expr)) {
                    unsigned new_expr = rename_free_ids_inner(
                        car(expr), rename_map, quote_shadowed,
                        quasiquote_shadowed);
                    if (new_expr == TOK_ERROR)
                        return TOK_ERROR;
                    gc_protect(&new_expr);
                    unsigned expr_tail = alloc_cons(new_expr, 0);
                    gc_protect(&expr_tail);
                    unsigned var_tail = alloc_cons(new_var, expr_tail);
                    gc_protect(&var_tail);
                    return alloc_cons(head, var_tail);
                }
                return tmpl;
            }
            // For define, rename the target too. The later hygienize pass
            // decides whether this is an introduced binding or a pattern
            // variable binding.
            if (head_id == ctx.kw_define && define_form_like(tmpl) &&
                !head_is_definition_alias) {
                unsigned var = cadr(tmpl);
                unsigned expr = cddr(tmpl);
                unsigned new_var =
                    rename_free_ids_inner(var, rename_map, quote_shadowed,
                                          quasiquote_shadowed);
                if (new_var == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_var);
                if (expr && IS_PAIR(expr)) {
                    unsigned new_expr = rename_free_ids_inner(
                        car(expr), rename_map, quote_shadowed,
                        quasiquote_shadowed);
                    if (new_expr == TOK_ERROR)
                        return TOK_ERROR;
                    gc_protect(&new_expr);
                    unsigned expr_tail = alloc_cons(new_expr, 0);
                    gc_protect(&expr_tail);
                    unsigned var_tail = alloc_cons(new_var, expr_tail);
                    gc_protect(&var_tail);
                    return alloc_cons(head, var_tail);
                }
                return tmpl;
            }
        }
        // Ordinary template lists can be very wide.  Walking their cdrs
        // recursively makes an otherwise shallow macro expansion consume one
        // C stack frame per element (notably for a large introduced begin).
        // Build the renamed spine iteratively, while delegating a tail that
        // starts a syntactic form to the existing specialized handling.
        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car = rename_free_ids_inner(
                old_car, rename_map, quote_shadowed, quasiquote_shadowed);
            if (new_car == TOK_ERROR)
                return TOK_ERROR;
            if (new_car != old_car)
                changed = true;
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                unsigned new_tail = rename_free_ids_inner(
                    next, rename_map, quote_shadowed, quasiquote_shadowed);
                if (new_tail == TOK_ERROR)
                    return TOK_ERROR;
                if (new_tail != next)
                    changed = true;
                if (!changed)
                    return original;
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                return result;
            }

            if (IS_ATOM(car(next)) && is_special_form(CELL_ID(car(next)))) {
                unsigned new_tail = rename_free_ids_inner(
                    next, rename_map, quote_shadowed, quasiquote_shadowed);
                if (new_tail == TOK_ERROR)
                    return TOK_ERROR;
                if (new_tail != next)
                    changed = true;
                if (!changed)
                    return original;
                cell_set_cdr(result_tail, new_tail);
                return result;
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        bool changed = false;
        for (unsigned i = 0; i < len && !changed; i++) {
            // Refresh data pointer - GC may have moved tmpl
            unsigned *data = vector_data_ptr(tmpl);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned renamed =
                rename_free_ids_inner(orig, rename_map, quote_shadowed,
                                      quasiquote_shadowed);
            if (renamed == TOK_ERROR)
                return TOK_ERROR;
            if (renamed != orig)
                changed = true;
            gc_unprotect(1);
        }
        if (!changed) {
            return tmpl;
        }
        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_vec);
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointers - GC may have moved vectors
            unsigned *data = vector_data_ptr(tmpl);
            unsigned elem = data[i];
            unsigned renamed =
                rename_free_ids_inner(elem, rename_map, quote_shadowed,
                                      quasiquote_shadowed);
            if (renamed == TOK_ERROR)
                return TOK_ERROR;
            unsigned *new_data = vector_data_ptr(new_vec);
            new_data[i] = renamed;
        }
        return new_vec;
    }

    return tmpl;
}

// Rename all occurrences of old_id to new_sym in template
// Returns a new template with substitutions made
static unsigned rename_in_template(unsigned tmpl, int64_t old_id,
                                   unsigned new_sym);
static unsigned rename_in_syntax_rules(unsigned tmpl, int64_t old_id,
                                       unsigned new_sym);

static bool pattern_binds_id(unsigned pattern, int64_t old_id,
                             unsigned literals, int64_t ellipsis_id)
{
    if (!pattern)
        return false;

    if (IS_ATOM(pattern)) {
        int64_t id = CELL_ID(pattern);
        return identifier_valid(pattern) && id == old_id &&
               id != ellipsis_id && !syntax_is_underscore(pattern) &&
               !atom_list_contains_id(literals, id);
    }

    if (IS_PAIR(pattern)) {
        while (IS_PAIR(pattern)) {
            if (pattern_binds_id(car(pattern), old_id, literals, ellipsis_id))
                return true;
            pattern = cdr(pattern);
        }
        return pattern_binds_id(pattern, old_id, literals, ellipsis_id);
    }

    if (IS_VECTOR(pattern)) {
        unsigned len = vector_len(pattern);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(pattern);
            if (pattern_binds_id(data[i], old_id, literals, ellipsis_id))
                return true;
        }
    }

    return false;
}

static bool syntax_rule_pattern_binds_id(unsigned pattern, int64_t old_id,
                                         unsigned literals,
                                         int64_t ellipsis_id)
{
    if (IS_PAIR(pattern))
        return pattern_binds_id(cdr(pattern), old_id, literals, ellipsis_id);
    return pattern_binds_id(pattern, old_id, literals, ellipsis_id);
}

static unsigned rename_in_quasiquote(unsigned tmpl, int64_t old_id,
                                     unsigned new_sym, unsigned depth)
{
    if (!tmpl)
        return 0;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&new_sym);

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if ((head_id == ctx.kw_unquote ||
                 head_id == ctx.kw_unquote_splicing) &&
                IS_PAIR(cdr(tmpl))) {
                unsigned body = cadr(tmpl);
                unsigned new_body;
                if (depth == 1)
                    new_body = rename_in_template(body, old_id, new_sym);
                else
                    new_body =
                        rename_in_quasiquote(body, old_id, new_sym, depth - 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
            if (head_id == ctx.kw_quasiquote && head_id != old_id &&
                IS_PAIR(cdr(tmpl))) {
                unsigned new_body =
                    rename_in_quasiquote(cadr(tmpl), old_id, new_sym,
                                         depth + 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
        }

        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car =
                rename_in_quasiquote(old_car, old_id, new_sym, depth);
            if (new_car == TOK_ERROR)
                return TOK_ERROR;
            if (new_car != old_car)
                changed = true;
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                unsigned new_tail =
                    rename_in_quasiquote(next, old_id, new_sym, depth);
                if (new_tail == TOK_ERROR)
                    return TOK_ERROR;
                if (new_tail != next)
                    changed = true;
                if (!changed)
                    return original;
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                return result;
            }
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_unquote ||
                    next_id == ctx.kw_unquote_splicing ||
                    next_id == ctx.kw_quasiquote) {
                    unsigned new_tail = rename_in_quasiquote(
                        next, old_id, new_sym, depth);
                    if (new_tail == TOK_ERROR)
                        return TOK_ERROR;
                    if (new_tail != next)
                        changed = true;
                    if (!changed)
                        return original;
                    cell_set_cdr(result_tail, new_tail);
                    return result;
                }
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        bool changed = false;
        for (unsigned i = 0; i < len && !changed; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned renamed =
                rename_in_quasiquote(orig, old_id, new_sym, depth);
            if (renamed == TOK_ERROR)
                return TOK_ERROR;
            if (renamed != orig)
                changed = true;
            gc_unprotect(1);
        }
        if (!changed)
            return tmpl;

        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_vec);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            unsigned elem = data[i];
            unsigned renamed =
                rename_in_quasiquote(elem, old_id, new_sym, depth);
            if (renamed == TOK_ERROR)
                return TOK_ERROR;
            unsigned *new_data = vector_data_ptr(new_vec);
            new_data[i] = renamed;
        }
        return new_vec;
    }

    return tmpl;
}

static unsigned rename_in_template(unsigned tmpl, int64_t old_id,
                                   unsigned new_sym)
{
    if (!tmpl)
        return 0;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&new_sym);

    if (IS_ATOM(tmpl)) {
        if (CELL_ID(tmpl) == old_id)
            return new_sym;
        return tmpl;
    }

    if (IS_PAIR(tmpl)) {
        // Skip protected wrappers and quoted data - they should not be renamed
        // as part of an introduced binding's lexical scope.
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if (head_id == ctx.kw_protected ||
                (head_id == ctx.kw_quote && head_id != old_id)) {
                return tmpl;
            }
            if (syntax_rules_form_like(tmpl))
                return rename_in_syntax_rules(tmpl, old_id, new_sym);
            if (head_id == ctx.kw_quasiquote && head_id != old_id &&
                IS_PAIR(cdr(tmpl))) {
                unsigned new_body =
                    rename_in_quasiquote(cadr(tmpl), old_id, new_sym, 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
        }
        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car = rename_in_template(old_car, old_id, new_sym);
            if (new_car == TOK_ERROR)
                return TOK_ERROR;
            if (new_car != old_car)
                changed = true;
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                unsigned new_tail = rename_in_template(next, old_id, new_sym);
                if (new_tail == TOK_ERROR)
                    return TOK_ERROR;
                if (new_tail != next)
                    changed = true;
                if (!changed)
                    return original;
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                return result;
            }
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_protected || next_id == ctx.kw_quote ||
                    next_id == ctx.kw_quasiquote ||
                    syntax_rules_form_like(next)) {
                    unsigned new_tail =
                        rename_in_template(next, old_id, new_sym);
                    if (new_tail == TOK_ERROR)
                        return TOK_ERROR;
                    if (new_tail != next)
                        changed = true;
                    if (!changed)
                        return original;
                    cell_set_cdr(result_tail, new_tail);
                    return result;
                }
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        bool changed = false;

        // First pass: check if any element changes
        for (unsigned i = 0; i < len && !changed; i++) {
            // Refresh data pointer - GC may have moved tmpl
            unsigned *data = vector_data_ptr(tmpl);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned renamed = rename_in_template(orig, old_id, new_sym);
            if (renamed == TOK_ERROR)
                return TOK_ERROR;
            if (renamed != orig)
                changed = true;
            gc_unprotect(1);
        }

        if (!changed) {
            return tmpl;
        }

        // Create new vector with renamed elements
        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_vec);
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointers - GC may have moved vectors
            unsigned *data = vector_data_ptr(tmpl);
            unsigned elem = data[i];
            unsigned renamed = rename_in_template(elem, old_id, new_sym);
            if (renamed == TOK_ERROR)
                return TOK_ERROR;
            unsigned *new_data = vector_data_ptr(new_vec);
            new_data[i] = renamed;
        }
        return new_vec;
    }

    return tmpl;
}

static unsigned rename_in_syntax_rules(unsigned tmpl, int64_t old_id,
                                       unsigned new_sym)
{
    if (!IS_PAIR(tmpl) || !IS_ATOM(car(tmpl)) ||
        CELL_ID(car(tmpl)) != ctx.kw_syntax_rules)
        return tmpl;

    unsigned rest = cdr(tmpl);
    if (!IS_PAIR(rest))
        return tmpl;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&rest);
    gc_protect(&new_sym);

    unsigned prefix = 0, prefix_tail = 0;
    gc_protect(&prefix);
    gc_protect(&prefix_tail);
    list_append(&prefix, &prefix_tail, car(tmpl));

    int64_t ellipsis_id = ctx.kw_ellipsis;
    if (car(rest) && IS_ATOM(car(rest)) && IS_PAIR(cdr(rest))) {
        ellipsis_id = CELL_ID(car(rest));
        list_append(&prefix, &prefix_tail, car(rest));
        rest = cdr(rest);
    }

    if (!IS_PAIR(rest))
        return tmpl;
    list_append(&prefix, &prefix_tail, car(rest));
    unsigned literals = car(rest);
    unsigned rules = cdr(rest);
    gc_protect(&literals);

    unsigned new_rules = 0, new_rules_tail = 0;
    gc_protect(&new_rules);
    gc_protect(&new_rules_tail);

    for (unsigned r = rules; r; r = cdr(r)) {
        unsigned rule = car(r);
        if (!IS_PAIR(rule) || !IS_PAIR(cdr(rule))) {
            list_append(&new_rules, &new_rules_tail, rule);
            continue;
        }

        unsigned pattern = car(rule);
        unsigned template = cadr(rule);
        gc_protect(&pattern);
        gc_protect(&template);
        unsigned new_template;
        if (syntax_rule_pattern_binds_id(pattern, old_id, literals,
                                         ellipsis_id))
            new_template = template;
        else {
            new_template = rename_in_template(template, old_id, new_sym);
        }
        if (new_template == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_template);
        unsigned template_tail = alloc_cons(new_template, 0);
        gc_protect(&template_tail);
        unsigned new_rule = alloc_cons(pattern, template_tail);
        gc_protect(&new_rule);
        list_append(&new_rules, &new_rules_tail, new_rule);
        gc_unprotect(5);
    }

    cell_set_cdr(prefix_tail, new_rules);
    return prefix;
}

// Forward declaration
static unsigned hygienize_template(unsigned tmpl, unsigned bindings);
static unsigned hygienize_body_sequence(unsigned body, unsigned bindings);

static unsigned hygienize_quasiquote(unsigned tmpl, unsigned bindings,
                                     unsigned depth)
{
    if (!tmpl)
        return 0;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if ((head_id == ctx.kw_unquote ||
                 head_id == ctx.kw_unquote_splicing) &&
                IS_PAIR(cdr(tmpl))) {
                unsigned body = cadr(tmpl);
                unsigned new_body;
                if (depth == 1)
                    new_body = hygienize_template(body, bindings);
                else
                    new_body =
                        hygienize_quasiquote(body, bindings, depth - 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl))) {
                unsigned new_body =
                    hygienize_quasiquote(cadr(tmpl), bindings, depth + 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
        }

        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car =
                hygienize_quasiquote(old_car, bindings, depth);
            if (new_car == TOK_ERROR)
                return TOK_ERROR;
            if (new_car != old_car)
                changed = true;
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                unsigned new_tail =
                    hygienize_quasiquote(next, bindings, depth);
                if (new_tail == TOK_ERROR)
                    return TOK_ERROR;
                if (new_tail != next)
                    changed = true;
                if (!changed)
                    return original;
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                return result;
            }
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_unquote ||
                    next_id == ctx.kw_unquote_splicing ||
                    next_id == ctx.kw_quasiquote) {
                    unsigned new_tail =
                        hygienize_quasiquote(next, bindings, depth);
                    if (new_tail == TOK_ERROR)
                        return TOK_ERROR;
                    if (new_tail != next)
                        changed = true;
                    if (!changed)
                        return original;
                    cell_set_cdr(result_tail, new_tail);
                    return result;
                }
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        bool changed = false;
        for (unsigned i = 0; i < len && !changed; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned h = hygienize_quasiquote(orig, bindings, depth);
            if (h == TOK_ERROR)
                return TOK_ERROR;
            if (h != orig)
                changed = true;
            gc_unprotect(1);
        }
        if (!changed)
            return tmpl;

        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_vec);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            unsigned elem = data[i];
            unsigned hygienized = hygienize_quasiquote(elem, bindings, depth);
            if (hygienized == TOK_ERROR)
                return TOK_ERROR;
            unsigned *new_data = vector_data_ptr(new_vec);
            new_data[i] = hygienized;
        }
        return new_vec;
    }

    return tmpl;
}

// Hygienize a let-style binding form: (let ((var val) ...) body ...)
// Renames introduced bindings (not pattern variables) to gensyms
static unsigned hygienize_let(unsigned tmpl, unsigned bindings, bool is_letrec)
{
    // tmpl = (let ((var val) ...) body ...)
    unsigned keyword = car(tmpl);
    unsigned binding_list = cadr(tmpl);
    unsigned body = cddr(tmpl);

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    gc_protect(&keyword);
    gc_protect(&binding_list);
    gc_protect(&body);

    if (!proper_list_silent(binding_list))
        return tmpl;

    // For letrec, we need to rename in all vals too (they can reference each
    // other) Collect all introduced bindings first
    unsigned renames = 0; // list of (old_id . new_sym)
    gc_protect(&renames);

    // First pass: identify introduced bindings and create gensyms
    // Skip ellipsis (...) which appears in macro templates
    unsigned bl = binding_list;
    gc_protect(&bl);
    while (IS_PAIR(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding))
            goto next_intro_binding;
        // Skip ellipsis in binding list
        if (IS_ATOM(binding) && CELL_ID(binding) == ctx.kw_ellipsis)
            goto next_intro_binding;
        unsigned var = car(binding);
        if (identifier_valid(var)) {
            int64_t var_id = CELL_ID(var);
            // Skip ellipsis
            if (var_id == ctx.kw_ellipsis)
                goto next_intro_binding;
            if (!is_pattern_var(var_id, bindings)) {
                // This is an introduced binding - generate gensym
                gc_protect(&var);
                unsigned new_sym = do_gensym();
                gc_protect(&new_sym);
                unsigned rename_pair = alloc_cons(var, new_sym);
                gc_protect(&rename_pair);
                renames = alloc_cons(rename_pair, renames);
                gc_unprotect(3);
            }
        }
next_intro_binding:
        bl = cdr(bl);
    }
    gc_unprotect(1);

    if (!renames) {
        // No introduced bindings - just hygienize nested templates
        unsigned new_bindings = 0, new_bindings_tail = 0;
        gc_protect(&new_bindings);
        gc_protect(&new_bindings_tail);

        bl = binding_list;
        gc_protect(&bl);
        while (IS_PAIR(bl)) {
            unsigned binding = car(bl);
            if (!let_binding_has_value(binding)) {
                list_append(&new_bindings, &new_bindings_tail, binding);
                bl = cdr(bl);
                continue;
            }
            unsigned var = car(binding);
            unsigned val = cadr(binding);
            gc_protect(&var);
            gc_protect(&val);
            unsigned new_val = hygienize_template(val, bindings);
            if (new_val == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&new_val);
            unsigned binding_tail = alloc_cons(new_val, 0);
            gc_protect(&binding_tail);
            unsigned new_binding = alloc_cons(var, binding_tail);
            gc_protect(&new_binding);
            list_append(&new_bindings, &new_bindings_tail, new_binding);
            gc_unprotect(5);
            bl = cdr(bl);
        }
        gc_unprotect(1);

        unsigned new_body = hygienize_body_sequence(body, bindings);
        gc_protect(&new_body);
        unsigned result_tail = alloc_cons(new_bindings, new_body);
        gc_protect(&result_tail);
        unsigned result = alloc_cons(keyword, result_tail);
        gc_unprotect(10);
        return result;
    }

    // Apply renames to binding list and body
    unsigned new_binding_list = binding_list;
    unsigned new_body = body;
    gc_protect(&new_binding_list);
    gc_protect(&new_body);

    unsigned r = renames;
    gc_protect(&r);
    while (r) {
        unsigned rename = car(r);
        unsigned old_sym = car(rename);
        unsigned new_sym = cdr(rename);
        int64_t old_id = CELL_ID(old_sym);
        gc_protect(&new_sym);

        if (is_letrec) {
            // For letrec, rename in all binding vals
            new_binding_list =
                rename_in_template(new_binding_list, old_id, new_sym);
            if (new_binding_list == TOK_ERROR)
                return TOK_ERROR;
        } else {
            // For let/let*, only rename var in binding, body sees the rename
            unsigned renamed_bindings = 0, renamed_tail = 0;
            gc_protect(&renamed_bindings);
            gc_protect(&renamed_tail);
            bl = new_binding_list;
            gc_protect(&bl);
            while (IS_PAIR(bl)) {
                unsigned binding = car(bl);
                if (!let_binding_has_value(binding)) {
                    list_append(&renamed_bindings, &renamed_tail, binding);
                    bl = cdr(bl);
                    continue;
                }
                unsigned var = car(binding);
                unsigned val = cadr(binding);
                unsigned new_var = var;
                if (IS_ATOM(var) && CELL_ID(var) == old_id)
                    new_var = new_sym;
                gc_protect(&new_var);
                gc_protect(&val);
                unsigned bind_tail = alloc_cons(val, 0);
                gc_protect(&bind_tail);
                unsigned new_bind = alloc_cons(new_var, bind_tail);
                gc_protect(&new_bind);
                list_append(&renamed_bindings, &renamed_tail, new_bind);
                gc_unprotect(4);
                bl = cdr(bl);
            }
            gc_unprotect(3);
            new_binding_list = renamed_bindings;
        }
        new_body = rename_in_template(new_body, old_id, new_sym);
        if (new_body == TOK_ERROR)
            return TOK_ERROR;
        gc_unprotect(1);
        r = cdr(r);
    }
    gc_unprotect(1);

    // Now hygienize nested templates in the binding values
    unsigned final_bindings = 0, final_tail = 0;
    gc_protect(&final_bindings);
    gc_protect(&final_tail);

    bl = new_binding_list;
    gc_protect(&bl);
    while (IS_PAIR(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding)) {
            list_append(&final_bindings, &final_tail, binding);
            bl = cdr(bl);
            continue;
        }
        unsigned var = car(binding);
        unsigned val = cadr(binding);
        gc_protect(&var);
        gc_protect(&val);
        unsigned new_val = hygienize_template(val, bindings);
        if (new_val == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_val);
        unsigned binding_tail = alloc_cons(new_val, 0);
        gc_protect(&binding_tail);
        unsigned new_binding = alloc_cons(var, binding_tail);
        gc_protect(&new_binding);
        list_append(&final_bindings, &final_tail, new_binding);
        gc_unprotect(5);
        bl = cdr(bl);
    }
    gc_unprotect(1);

    unsigned final_body = hygienize_body_sequence(new_body, bindings);
    if (final_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&final_body);
    unsigned result_tail = alloc_cons(final_bindings, final_body);
    gc_protect(&result_tail);
    unsigned result = alloc_cons(keyword, result_tail);
    gc_unprotect(12); // 5 initial + renames + new_binding_list + new_body +
                      // final_bindings + final_tail + final_body + result_tail
    return result;
}

static unsigned hygienize_named_let(unsigned tmpl, unsigned bindings)
{
    unsigned keyword = car(tmpl);
    unsigned name = cadr(tmpl);
    unsigned binding_list = caddr(tmpl);
    unsigned body = cdddr(tmpl);

    if (!proper_list_silent(binding_list))
        return tmpl;

    GC_GUARD;
    gc_protect(&keyword);
    gc_protect(&name);
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&bindings);

    unsigned renames = 0;
    gc_protect(&renames);

    if (identifier_valid(name) && CELL_ID(name) != ctx.kw_ellipsis &&
        !is_pattern_var(CELL_ID(name), bindings)) {
        unsigned new_sym = do_gensym();
        gc_protect(&new_sym);
        unsigned rename_pair = alloc_cons(name, new_sym);
        gc_protect(&rename_pair);
        renames = alloc_cons(rename_pair, renames);
        gc_unprotect(2);
    }

    for (unsigned bl = binding_list; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding))
            continue;
        unsigned var = car(binding);
        if (identifier_valid(var) && CELL_ID(var) != ctx.kw_ellipsis &&
            !is_pattern_var(CELL_ID(var), bindings) &&
            !assoc_list_has_atom_key_id(renames, CELL_ID(var))) {
            gc_protect(&var);
            unsigned new_sym = do_gensym();
            gc_protect(&new_sym);
            unsigned rename_pair = alloc_cons(var, new_sym);
            gc_protect(&rename_pair);
            renames = alloc_cons(rename_pair, renames);
            gc_unprotect(3);
        }
    }
    gc_unprotect(1);

    unsigned new_name = name;
    unsigned new_binding_list = binding_list;
    unsigned new_body = body;
    gc_protect(&new_name);
    gc_protect(&new_binding_list);
    gc_protect(&new_body);

    unsigned r = renames;
    gc_protect(&r);
    for (; r; r = cdr(r)) {
        unsigned rename = car(r);
        unsigned old_sym = car(rename);
        unsigned new_sym = cdr(rename);
        // new_sym is reachable only through `r`, so the allocations below
        // (alloc_cons and list_append in the binding loop) can relocate the
        // cell and leave this raw local stale - which is then spliced into
        // the rebuilt binding list and the renamed body. hygienize_let and
        // hygienize_lambda protect the same value for the same reason.
        gc_protect(&new_sym);
        int64_t old_id = CELL_ID(old_sym);

        if (IS_ATOM(new_name) && CELL_ID(new_name) == old_id)
            new_name = new_sym;

        unsigned renamed_bindings = 0, renamed_tail = 0;
        gc_protect(&renamed_bindings);
        gc_protect(&renamed_tail);
        unsigned bl = new_binding_list;
        gc_protect(&bl);
        for (; IS_PAIR(bl); bl = cdr(bl)) {
            unsigned binding = car(bl);
            if (!let_binding_has_value(binding)) {
                list_append(&renamed_bindings, &renamed_tail, binding);
                continue;
            }

            unsigned var = car(binding);
            unsigned val = cadr(binding);
            unsigned new_var = var;
            if (IS_ATOM(var) && CELL_ID(var) == old_id)
                new_var = new_sym;
            gc_protect(&new_var);
            gc_protect(&val);
            unsigned bind_tail = alloc_cons(val, 0);
            gc_protect(&bind_tail);
            unsigned new_bind = alloc_cons(new_var, bind_tail);
            gc_protect(&new_bind);
            list_append(&renamed_bindings, &renamed_tail, new_bind);
            gc_unprotect(4);
        }
        gc_unprotect(1);
        gc_unprotect(2);
        new_binding_list = renamed_bindings;

        new_body = rename_in_template(new_body, old_id, new_sym);
        if (new_body == TOK_ERROR) {
            gc_unprotect(1);
            return TOK_ERROR;
        }
        gc_unprotect(1); // new_sym
    }
    gc_unprotect(1);

    unsigned final_bindings = 0, final_tail = 0;
    gc_protect(&final_bindings);
    gc_protect(&final_tail);
    unsigned bl = new_binding_list;
    gc_protect(&bl);
    for (; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding)) {
            list_append(&final_bindings, &final_tail, binding);
            continue;
        }

        unsigned var = car(binding);
        unsigned val = cadr(binding);
        gc_protect(&var);
        gc_protect(&val);
        unsigned new_val = hygienize_template(val, bindings);
        if (new_val == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_val);
        unsigned binding_tail = alloc_cons(new_val, 0);
        gc_protect(&binding_tail);
        unsigned new_binding = alloc_cons(var, binding_tail);
        gc_protect(&new_binding);
        list_append(&final_bindings, &final_tail, new_binding);
        gc_unprotect(5);
    }
    gc_unprotect(1);

    unsigned final_body = hygienize_body_sequence(new_body, bindings);
    if (final_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&final_body);
    unsigned body_tail = alloc_cons(final_bindings, final_body);
    gc_protect(&body_tail);
    unsigned name_tail = alloc_cons(new_name, body_tail);
    gc_protect(&name_tail);
    return alloc_cons(keyword, name_tail);
}

// Hygienize a lambda form: (lambda (params...) body ...)
static unsigned hygienize_lambda(unsigned tmpl, unsigned bindings)
{
    unsigned keyword = car(tmpl);
    unsigned params = cadr(tmpl);
    unsigned body = cddr(tmpl);

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    gc_protect(&keyword);
    gc_protect(&params);
    gc_protect(&body);

    // Collect renames for introduced parameters
    unsigned renames = 0;
    gc_protect(&renames);

    // Handle both proper list and dotted list params
    // Skip ellipsis (...) which appears in macro templates
    unsigned p = params;
    gc_protect(&p);
    for (; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        unsigned param = IS_PAIR(p) ? car(p) : p;
        if (identifier_valid(param)) {
            int64_t param_id = CELL_ID(param);
            // Skip ellipsis - it's a macro template construct, not a real param
            if (param_id == ctx.kw_ellipsis)
                continue;
            if (!is_pattern_var(param_id, bindings)) {
                unsigned new_sym = do_gensym();
                gc_protect(&new_sym);
                unsigned rename_pair = alloc_cons(param, new_sym);
                gc_protect(&rename_pair);
                renames = alloc_cons(rename_pair, renames);
                gc_unprotect(2);
            }
        }
        if (!IS_PAIR(p))
            break;
    }
    gc_unprotect(1);

    if (!renames) {
        // No introduced params - just hygienize body
        unsigned new_body = hygienize_body_sequence(body, bindings);
        if (new_body == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_body);
        unsigned result_tail = alloc_cons(params, new_body);
        gc_protect(&result_tail);
        unsigned result = alloc_cons(keyword, result_tail);
        gc_unprotect(8);
        return result;
    }

    // Apply renames to params and body
    unsigned new_params = params;
    unsigned new_body = body;
    gc_protect(&new_params);
    gc_protect(&new_body);

    unsigned r = renames;
    gc_protect(&r);
    while (r) {
        unsigned rename = car(r);
        int64_t old_id = CELL_ID(car(rename));
        unsigned new_sym = cdr(rename);
        gc_protect(&new_sym);
        new_params = rename_in_template(new_params, old_id, new_sym);
        if (new_params == TOK_ERROR)
            return TOK_ERROR;
        new_body = rename_in_template(new_body, old_id, new_sym);
        if (new_body == TOK_ERROR)
            return TOK_ERROR;
        gc_unprotect(1);
        r = cdr(r);
    }
    gc_unprotect(1);

    // Hygienize body
    unsigned final_body = hygienize_body_sequence(new_body, bindings);
    if (final_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&final_body);
    unsigned result_tail = alloc_cons(new_params, final_body);
    gc_protect(&result_tail);
    unsigned result = alloc_cons(keyword, result_tail);
    gc_unprotect(10); // 5 initial + renames + new_params + new_body +
                      // final_body + result_tail
    return result;
}

static unsigned define_target_name(unsigned expr)
{
    if (!IS_PAIR(expr) || !IS_ATOM(car(expr)) ||
        CELL_ID(car(expr)) != ctx.kw_define || !IS_PAIR(cdr(expr)))
        return 0;

    unsigned target = cadr(expr);
    if (IS_ATOM(target))
        return target;
    if (IS_PAIR(target) && IS_ATOM(car(target)))
        return car(target);
    return 0;
}

static unsigned hygienize_define_no_target_rename(unsigned tmpl,
                                                  unsigned bindings)
{
    unsigned keyword = car(tmpl);
    unsigned target = cadr(tmpl);
    unsigned exprs = cddr(tmpl);

    GC_GUARD;
    gc_protect(&keyword);
    gc_protect(&target);
    gc_protect(&exprs);
    gc_protect(&bindings);

    if (!exprs)
        return tmpl;

    if (IS_PAIR(target)) {
        unsigned name = car(target);
        unsigned params = cdr(target);
        gc_protect(&name);
        gc_protect(&params);

        unsigned lambda_atom = atom_from_string("lambda");
        gc_protect(&lambda_atom);
        unsigned lambda_tail = alloc_cons(params, exprs);
        gc_protect(&lambda_tail);
        unsigned lambda_form = alloc_cons(lambda_atom, lambda_tail);
        gc_protect(&lambda_form);

        unsigned hygienic_lambda = hygienize_lambda(lambda_form, bindings);
        if (hygienic_lambda == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&hygienic_lambda);

        unsigned new_params = cadr(hygienic_lambda);
        unsigned new_exprs = cddr(hygienic_lambda);
        gc_protect(&new_params);
        gc_protect(&new_exprs);
        unsigned new_target = alloc_cons(name, new_params);
        gc_protect(&new_target);
        unsigned target_tail = alloc_cons(new_target, new_exprs);
        gc_protect(&target_tail);
        return alloc_cons(keyword, target_tail);
    }

    unsigned new_exprs = hygienize_template(exprs, bindings);
    if (new_exprs == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&new_exprs);
    unsigned target_tail = alloc_cons(target, new_exprs);
    gc_protect(&target_tail);
    return alloc_cons(keyword, target_tail);
}

static unsigned hygienize_define(unsigned tmpl, unsigned bindings)
{
    unsigned target = define_target_name(tmpl);
    if (!target)
        return hygienize_define_no_target_rename(tmpl, bindings);

    int64_t target_id = CELL_ID(target);
    if (target_id == ctx.kw_ellipsis || is_pattern_var(target_id, bindings))
        return hygienize_define_no_target_rename(tmpl, bindings);

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    unsigned new_sym = do_gensym();
    gc_protect(&new_sym);
    unsigned renamed = rename_in_template(tmpl, target_id, new_sym);
    if (renamed == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&renamed);
    return hygienize_define_no_target_rename(renamed, bindings);
}

static unsigned hygienize_body_sequence(unsigned body, unsigned bindings)
{
    if (!proper_list_silent(body))
        return body;

    GC_GUARD;
    gc_protect(&body);
    gc_protect(&bindings);

    unsigned renames = 0;
    gc_protect(&renames);

    unsigned exprs = body;
    gc_protect(&exprs);
    for (; exprs; exprs = cdr(exprs)) {
        unsigned expr = car(exprs);
        unsigned target = define_target_name(expr);
        if (!target)
            break;

        int64_t target_id = CELL_ID(target);
        if (target_id != ctx.kw_ellipsis &&
            !is_pattern_var(target_id, bindings) &&
            !assoc_list_has_atom_key_id(renames, target_id)) {
            gc_protect(&target);
            unsigned new_sym = do_gensym();
            gc_protect(&new_sym);
            unsigned entry = alloc_cons(target, new_sym);
            gc_protect(&entry);
            renames = alloc_cons(entry, renames);
            gc_unprotect(3);
        }
    }
    gc_unprotect(1);

    unsigned renamed_body = body;
    gc_protect(&renamed_body);
    unsigned r = renames;
    gc_protect(&r);
    for (; r; r = cdr(r)) {
        unsigned entry = car(r);
        unsigned old_sym = car(entry);
        unsigned new_sym = cdr(entry);
        renamed_body =
            rename_in_template(renamed_body, CELL_ID(old_sym), new_sym);
        if (renamed_body == TOK_ERROR) {
            gc_unprotect(1);
            return TOK_ERROR;
        }
    }
    gc_unprotect(1);

    unsigned new_body = 0, new_tail = 0;
    gc_protect(&new_body);
    gc_protect(&new_tail);

    exprs = renamed_body;
    gc_protect(&exprs);
    for (; exprs; exprs = cdr(exprs)) {
        unsigned expr = car(exprs);
        gc_protect(&expr);
        unsigned new_expr;
        if (define_target_name(expr))
            new_expr = hygienize_define_no_target_rename(expr, bindings);
        else
            new_expr = hygienize_template(expr, bindings);
        if (new_expr == TOK_ERROR) {
            gc_unprotect(1);
            return TOK_ERROR;
        }
        gc_protect(&new_expr);
        list_append(&new_body, &new_tail, new_expr);
        gc_unprotect(2);
    }

    gc_unprotect(1);

    return new_body;
}

static unsigned hygienize_begin(unsigned tmpl, unsigned bindings)
{
    unsigned keyword = car(tmpl);
    unsigned body = cdr(tmpl);

    GC_GUARD;
    gc_protect(&keyword);
    gc_protect(&body);
    gc_protect(&bindings);

    unsigned new_body = hygienize_body_sequence(body, bindings);
    if (new_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&new_body);

    return alloc_cons(keyword, new_body);
}

// Main hygienize function - walk template and rename introduced bindings
static unsigned hygienize_template(unsigned tmpl, unsigned bindings)
{
    if (!tmpl)
        return 0;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);

    // Atoms don't need hygienization (they're handled by syntax_expand)
    if (IS_ATOM(tmpl)) {
        gc_unprotect(2);
        return tmpl;
    }

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);

        // Check for protected wrapper: (##protected## . identifier)
        // These come from pattern variable substitution and should not be
        // renamed
        if (IS_ATOM(head) && CELL_ID(head) == ctx.kw_protected) {
            gc_unprotect(2);
            return tmpl; // Return protected wrapper unchanged
        }

        // Check for binding forms
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);

            // let, let*, letrec
            if (head_id == ctx.kw_let || head_id == ctx.kw_letstar) {
                if (!IS_PAIR(cdr(tmpl)))
                    return tmpl;
                // Check it's not named let: (let name ((var val) ...) body)
                unsigned second = cadr(tmpl);
                if (head_id == ctx.kw_let && IS_ATOM(second)) {
                    if (!IS_PAIR(cddr(tmpl)))
                        return tmpl;
                    gc_unprotect(2);
                    return hygienize_named_let(tmpl, bindings);
                }
                if (IS_PAIR(cddr(tmpl)) &&
                    (!second || (IS_PAIR(second) && IS_PAIR(car(second))))) {
                    gc_unprotect(2);
                    return hygienize_let(tmpl, bindings, false);
                }
            }
            if (head_id == ctx.kw_letrec) {
                if (!IS_PAIR(cdr(tmpl)) || !IS_PAIR(cddr(tmpl)))
                    return tmpl;
                gc_unprotect(2);
                return hygienize_let(tmpl, bindings, true);
            }

            // lambda
            if (head_id == ctx.kw_lambda) {
                if (!IS_PAIR(cdr(tmpl)) || !IS_PAIR(cddr(tmpl)))
                    return tmpl;
                gc_unprotect(2);
                return hygienize_lambda(tmpl, bindings);
            }

            if (head_id == ctx.kw_define) {
                if (!IS_PAIR(cdr(tmpl)))
                    return tmpl;
                gc_unprotect(2);
                return hygienize_define(tmpl, bindings);
            }

            if (head_id == ctx.kw_begin) {
                gc_unprotect(2);
                return hygienize_begin(tmpl, bindings);
            }

            // Skip syntax-rules - they have their own patterns/templates
            // that will be hygienized when expanded
            if (syntax_rules_form_like(tmpl)) {
                gc_unprotect(2);
                return tmpl;
            }

            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl))) {
                unsigned new_body = hygienize_quasiquote(cadr(tmpl), bindings,
                                                         1);
                if (new_body == TOK_ERROR) {
                    gc_unprotect(2);
                    return TOK_ERROR;
                }
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                unsigned result = alloc_cons(head, result_tail);
                gc_unprotect(4);
                return result;
            }

            // For let-syntax/letrec-syntax, hygienize the body but not the
            // syntax-rules bindings
            if (head_id == ctx.kw_let_syntax ||
                head_id == ctx.kw_letrec_syntax) {
                if (!IS_PAIR(cdr(tmpl)) || !IS_PAIR(cddr(tmpl)))
                    return tmpl;
                // (let-syntax ((name transformer) ...) body ...)
                unsigned syn_bindings = cadr(tmpl);
                unsigned syn_body = cddr(tmpl);
                gc_protect(&syn_bindings);
                gc_protect(&syn_body);
                // Only hygienize the body, not the transformer bindings
                unsigned new_body = hygienize_template(syn_body, bindings);
                if (new_body == TOK_ERROR) {
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(syn_bindings, new_body);
                gc_protect(&result_tail);
                unsigned result = alloc_cons(head, result_tail);
                gc_unprotect(
                    6); // result_tail, new_body, syn_body, syn_bindings, tmpl, bindings
                return result;
            }
        }

        // Walk ordinary list spines iteratively.  A macro template may hold
        // a very large flat datum, for which recursing through each cdr would
        // overflow the C stack despite shallow structural nesting.
        unsigned result = 0, result_tail = 0;
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned new_car = hygienize_template(car(tmpl), bindings);
            if (new_car == TOK_ERROR) {
                gc_unprotect(4);
                return TOK_ERROR;
            }
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                if (next) {
                    unsigned new_tail = hygienize_template(next, bindings);
                    if (new_tail == TOK_ERROR) {
                        gc_unprotect(4);
                        return TOK_ERROR;
                    }
                    cell_set_cdr(result_tail, new_tail);
                }
                gc_unprotect(4);
                return result;
            }

            // Preserve the existing special-form handling if a cdr happens
            // to begin a new syntactic form rather than being list data.
            if (IS_ATOM(car(next)) && is_special_form(CELL_ID(car(next)))) {
                unsigned new_tail = hygienize_template(next, bindings);
                if (new_tail == TOK_ERROR) {
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
                cell_set_cdr(result_tail, new_tail);
                gc_unprotect(4);
                return result;
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        bool changed = false;

        for (unsigned i = 0; i < len && !changed; i++) {
            // Refresh data pointer - GC may have moved tmpl
            unsigned *data = vector_data_ptr(tmpl);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned h = hygienize_template(orig, bindings);
            if (h == TOK_ERROR) {
                gc_unprotect(3);
                return TOK_ERROR;
            }
            if (h != orig)
                changed = true;
            gc_unprotect(1);
        }

        if (!changed) {
            gc_unprotect(2);
            return tmpl;
        }

        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR) {
            gc_unprotect(2);
            return TOK_ERROR;
        }
        gc_protect(&new_vec);
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointers - GC may have moved vectors
            unsigned *data = vector_data_ptr(tmpl);
            unsigned elem = data[i];
            unsigned hygienized = hygienize_template(elem, bindings);
            if (hygienized == TOK_ERROR) {
                gc_unprotect(3);
                return TOK_ERROR;
            }
            unsigned *new_data = vector_data_ptr(new_vec);
            new_data[i] = hygienized;
        }
        gc_unprotect(3);
        return new_vec;
    }

    gc_unprotect(2);
    return tmpl;
}

// ============================================================================
// Template Expansion
// ============================================================================

unsigned syntax_expand(unsigned tmpl, unsigned bindings, unsigned mark,
                       int64_t ellipsis_id)
{
    (void)mark; // For now, we don't do full hygiene renaming

    // Protect parameters - this function is recursive and allocates
    gc_protect(&tmpl);
    gc_protect(&bindings);

    if (!tmpl) {
        gc_unprotect(2);
        return 0;
    }

    // Atom - check if it's a pattern variable
    if (IS_ATOM(tmpl)) {
        unsigned lookup_result = syntax_lookup(tmpl, bindings);
        if (lookup_result != TOK_ERROR) {
            // Protect lookup_result BEFORE any allocations - this is critical!
            gc_protect(&lookup_result);
            // If substituting with an identifier, wrap it to protect from
            // future renaming by nested macros (mark-based hygiene)
            if (IS_ATOM(lookup_result)) {
                unsigned protected_marker = alloc();
                CELL_TYPE(protected_marker) = BT_ATOM;
                CELL_ID(protected_marker) = ctx.kw_protected;
                gc_protect(&protected_marker);
                unsigned wrapped = alloc_cons(protected_marker, lookup_result);
                gc_unprotect(
                    4); // protected_marker, lookup_result, bindings, tmpl
                return wrapped;
            }
            gc_unprotect(3); // lookup_result, bindings, tmpl
            return lookup_result;
        }
        gc_unprotect(2);
        return tmpl;
    }

    // List template
    if (IS_PAIR(tmpl)) {
        // R7RS ellipsis escape: (<ellipsis> <template>) expands to
        // <template> with ellipses inside treated as ordinary identifiers.
        // Passing -1 as the ellipsis id disables ellipsis handling below.
        if (syntax_is_ellipsis(car(tmpl), ellipsis_id) &&
            IS_PAIR(cdr(tmpl)) && !cddr(tmpl)) {
            unsigned escaped = syntax_expand(cadr(tmpl), bindings, mark, -1);
            gc_unprotect(2); // bindings, tmpl
            return escaped;
        }
        // Check for ellipsis: (tmpl ... rest)
        if (IS_PAIR(cdr(tmpl)) && syntax_is_ellipsis(cadr(tmpl), ellipsis_id)) {
            unsigned elem_tmpl = car(tmpl);
            unsigned rest_tmpl = cddr(tmpl);

            // Collect ALL ellipsis-bound variables in the template
            unsigned ellipsis_vars =
                collect_ellipsis_vars(elem_tmpl, bindings, 0);
            gc_protect(&ellipsis_vars);

            // Find iteration count from first variable's values
            unsigned iter_count = 0;
            if (!ellipsis_vars_iteration_count(ellipsis_vars, &iter_count)) {
                gc_unprotect(3);
                return TOK_ERROR;
            }

            // Expand template for each iteration
            unsigned result = 0, result_tail = 0;
            gc_protect(&result);
            gc_protect(&result_tail);
            gc_protect(&elem_tmpl);
            gc_protect(&rest_tmpl);

            for (unsigned i = 0; i < iter_count; i++) {
                // Create iteration bindings for ALL ellipsis variables
                unsigned iter_bindings = bindings;
                gc_protect(&iter_bindings);

                // For each ellipsis variable, bind to i-th value
                unsigned ev = ellipsis_vars;
                gc_protect(&ev);
                while (ev) {
                    unsigned binding = car(ev);
                    unsigned var = car(binding);
                    unsigned values = cdr(binding);

                    // Get i-th value
                    unsigned val = values;
                    for (unsigned j = 0; j < i && val; j++)
                        val = cdr(val);
                    if (val)
                        val = car(val);

                    gc_protect(&var);
                    gc_protect(&val);
                    unsigned new_binding = alloc_cons(var, val);
                    gc_protect(&new_binding);
                    iter_bindings = alloc_cons(new_binding, iter_bindings);
                    gc_unprotect(3);
                    ev = cdr(ev);
                }
                gc_unprotect(1);

                unsigned expanded =
                    syntax_expand(elem_tmpl, iter_bindings, mark, ellipsis_id);
                if (expanded == TOK_ERROR) {
                    gc_unprotect(8);
                    return TOK_ERROR;
                }
                gc_protect(&expanded);
                list_append(&result, &result_tail, expanded);
                gc_unprotect(2); // expanded, iter_bindings
            }
            gc_unprotect(5); // rest_tmpl, elem_tmpl, result_tail, result,
                             // ellipsis_vars

            // Append expanded rest
            gc_protect(&result);
            gc_protect(&result_tail);
            unsigned rest_expanded =
                syntax_expand(rest_tmpl, bindings, mark, ellipsis_id);
            if (rest_expanded == TOK_ERROR) {
                gc_unprotect(4);
                return TOK_ERROR;
            }
            if (result) {
                cell_set_cdr(result_tail, rest_expanded);
                gc_unprotect(4); // result_tail, result, bindings, tmpl
                return result;
            }
            gc_unprotect(4); // result_tail, result, bindings, tmpl
            return rest_expanded;
        }

        // Expand ordinary list spines iteratively.  Flat templates can be
        // arbitrarily long even though their structural nesting is shallow;
        // recursively expanding every cdr used to overflow the C stack.
        unsigned result = 0, result_tail = 0;
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            if (!IS_PAIR(tmpl)) {
                if (tmpl) {
                    unsigned rest =
                        syntax_expand(tmpl, bindings, mark, ellipsis_id);
                    if (rest == TOK_ERROR) {
                        gc_unprotect(4);
                        return TOK_ERROR;
                    }
                    if (result)
                        cell_set_cdr(result_tail, rest);
                    else
                        result = rest;
                }
                gc_unprotect(4);
                return result;
            }

            // Delegate an ellipsis group to its dedicated expansion path.
            // That path appends its expanded tail, while this loop handles
            // every ordinary cons before it without growing the C stack.
            if (IS_PAIR(cdr(tmpl)) &&
                syntax_is_ellipsis(cadr(tmpl), ellipsis_id)) {
                unsigned rest = syntax_expand(tmpl, bindings, mark,
                                              ellipsis_id);
                if (rest == TOK_ERROR) {
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
                if (result)
                    cell_set_cdr(result_tail, rest);
                else
                    result = rest;
                gc_unprotect(4);
                return result;
            }

            unsigned new_car =
                syntax_expand(car(tmpl), bindings, mark, ellipsis_id);
            if (new_car == TOK_ERROR) {
                gc_unprotect(4);
                return TOK_ERROR;
            }
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);
            tmpl = cdr(tmpl);
        }
    }

    // Vector template - expand each element
    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        unsigned *data = vector_data_ptr(tmpl);

        // Check for ellipsis in vector template
        unsigned ellipsis_pos = len;
        for (unsigned i = 0; i < len; i++) {
            if (syntax_is_ellipsis(data[i], ellipsis_id)) {
                ellipsis_pos = i;
                break;
            }
        }

        if (ellipsis_pos < len && ellipsis_pos > 0) {
            // Template has ellipsis - need to expand repeated element
            unsigned elem_tmpl = data[ellipsis_pos - 1];
            unsigned pre_count = ellipsis_pos - 1;
            unsigned post_count = len - ellipsis_pos - 1;

            unsigned ellipsis_vars =
                collect_ellipsis_vars(elem_tmpl, bindings, 0);
            gc_protect(&ellipsis_vars);

            // Count expanded elements
            unsigned expanded_count = pre_count + post_count;
            unsigned iter_count = 0;
            if (!ellipsis_vars_iteration_count(ellipsis_vars, &iter_count)) {
                gc_unprotect(3);
                return TOK_ERROR;
            }
            expanded_count += iter_count;

            unsigned result = make_vector(expanded_count, 0);
            if (result == TOK_ERROR) {
                gc_unprotect(3);
                return TOK_ERROR;
            }
            gc_protect(&result); // Protect result from GC
            unsigned idx = 0;

            // Expand elements before ellipsis
            for (unsigned i = 0; i < pre_count; i++) {
                // Refresh pointers - GC may have moved vectors
                data = vector_data_ptr(tmpl);
                unsigned elem = data[i];
                unsigned expanded =
                    syntax_expand(elem, bindings, mark, ellipsis_id);
                if (expanded == TOK_ERROR) {
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
                unsigned *result_data = vector_data_ptr(result);
                result_data[idx++] = expanded;
            }

            // Expand repeated element for each ellipsis value
            // Refresh data pointer before accessing elem_tmpl
            data = vector_data_ptr(tmpl);
            elem_tmpl = data[ellipsis_pos - 1];

            // Protect loop variables across allocations
            gc_protect(&elem_tmpl);
            gc_protect(&bindings);

            for (unsigned i = 0; i < iter_count; i++) {
                unsigned iter_bindings = bindings;
                gc_protect(&iter_bindings);

                unsigned ev = ellipsis_vars;
                gc_protect(&ev);
                while (ev) {
                    unsigned binding = car(ev);
                    unsigned var = car(binding);
                    unsigned values = cdr(binding);

                    unsigned val = values;
                    for (unsigned j = 0; j < i && val; j++)
                        val = cdr(val);
                    if (val)
                        val = car(val);

                    gc_protect(&var);
                    gc_protect(&val);
                    unsigned new_binding = alloc_cons(var, val);
                    gc_protect(&new_binding);
                    iter_bindings = alloc_cons(new_binding, iter_bindings);
                    gc_unprotect(3);
                    ev = cdr(ev);
                }
                gc_unprotect(1);

                // Refresh result_data pointer - GC may have moved result
                unsigned expanded =
                    syntax_expand(elem_tmpl, iter_bindings, mark, ellipsis_id);
                if (expanded == TOK_ERROR) {
                    gc_unprotect(6);
                    return TOK_ERROR;
                }
                unsigned *result_data = vector_data_ptr(result);
                result_data[idx++] = expanded;
                gc_unprotect(1); // iter_bindings
            }
            gc_unprotect(2); // bindings, elem_tmpl

            // Expand elements after ellipsis
            for (unsigned i = 0; i < post_count; i++) {
                // Refresh pointers - GC may have moved vectors
                data = vector_data_ptr(tmpl);
                unsigned elem = data[ellipsis_pos + 1 + i];
                unsigned expanded =
                    syntax_expand(elem, bindings, mark, ellipsis_id);
                if (expanded == TOK_ERROR) {
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
                unsigned *result_data = vector_data_ptr(result);
                result_data[idx++] = expanded;
            }

            gc_unprotect(4); // result, ellipsis_vars, tmpl, bindings
            return result;
        }

        // No ellipsis - expand each element
        unsigned result = make_vector(len, 0);
        if (result == TOK_ERROR) {
            gc_unprotect(2);
            return TOK_ERROR;
        }
        gc_protect(&result); // Protect result from GC
        for (unsigned i = 0; i < len; i++) {
            // Refresh pointers - GC may have moved vectors
            data = vector_data_ptr(tmpl);
            unsigned elem = data[i];
            unsigned expanded = syntax_expand(elem, bindings, mark, ellipsis_id);
            if (expanded == TOK_ERROR) {
                gc_unprotect(3);
                return TOK_ERROR;
            }
            unsigned *result_data = vector_data_ptr(result);
            result_data[i] = expanded;
        }
        gc_unprotect(3); // result, tmpl, bindings
        return result;
    }

    // Other values pass through unchanged
    gc_unprotect(2);
    return tmpl;
}

// ============================================================================
// Transformer Application
// ============================================================================

static bool syntax_transformer_well_formed(unsigned transformer, unsigned input,
                                           unsigned *syn_data_out,
                                           unsigned *closure_env_out,
                                           int64_t *ellipsis_id_out,
                                           unsigned *literals_out,
                                           unsigned *rules_out)
{
    if (!IS_SYNTAX(transformer) || !IS_PAIR(input) ||
        pair_chain_is_circular(input))
        return false;

    unsigned syn_data = car(transformer);
    if (!IS_PAIR(syn_data))
        return false;

    unsigned ellipsis_cell = car(syn_data);
    int64_t ellipsis_id = 0;
    if (IS_FIXNUM(ellipsis_cell)) {
        ellipsis_id = FIXNUM_VALUE(ellipsis_cell);
    } else if (IS_CELL(ellipsis_cell)) {
        ellipsis_id = CELL_ID(ellipsis_cell);
    } else {
        return false;
    }
    if (!IS_PAIR(cdr(syn_data)))
        return false;

    unsigned literals = cadr(syn_data);
    unsigned rules = cddr(syn_data);
    if (!proper_list_silent(literals) || !proper_list_silent(rules))
        return false;

    for (unsigned r = rules; r; r = cdr(r)) {
        unsigned rule = car(r);
        if (!IS_PAIR(rule))
            return false;

        unsigned rule_head = car(rule);
        unsigned cpat_cell = IS_PAIR(rule_head) ? car(rule_head) : rule_head;
        if (!is_compiled_pattern_object(cpat_cell))
            return false;
    }

    *syn_data_out = syn_data;
    *closure_env_out = cdr(transformer);
    *ellipsis_id_out = ellipsis_id;
    *literals_out = literals;
    *rules_out = rules;
    return true;
}

unsigned apply_syntax(unsigned transformer, unsigned input, unsigned use_env,
                      unsigned *bindings_out)
{
    // Protect parameters that may be in nursery - GC can run during
    // expansion. GC_GUARD releases every protection below on any return
    // path, so no manual unprotect accounting is needed at the exits.
    GC_GUARD;
    gc_protect(&transformer);
    gc_protect(&input);
    gc_protect(&use_env);

    if (bindings_out)
        *bindings_out = 0;

    // Extract transformer structure:
    // CAR = (ellipsis_cell . (literals . rules)), CDR = closure_env
    unsigned syn_data = 0;
    unsigned closure_env = 0;
    int64_t ellipsis_id = 0;
    unsigned literals = 0;
    unsigned rules = 0;
    if (!syntax_transformer_well_formed(transformer, input, &syn_data,
                                        &closure_env, &ellipsis_id, &literals,
                                        &rules)) {
        show_error("syntax transformer: invalid structure");
        return TOK_ERROR;
    }
    gc_protect(&syn_data);
    gc_protect(&literals);
    gc_protect(&closure_env);

    gc_protect(&rules);

    unsigned tmpl = 0;
    gc_protect(&tmpl);
    unsigned pattern = 0;
    unsigned input_args = 0;
    gc_protect(&pattern);
    gc_protect(&input_args);

    unsigned produced_bindings = 0;
    gc_protect(&produced_bindings);

    // Try each rule (rules now contain compiled patterns)
    for (; rules; rules = cdr(rules)) {
        unsigned rule = car(rules);
        unsigned rule_head = car(rule);
        unsigned cpat_cell;
        if (IS_PAIR(rule_head)) {
            cpat_cell = car(rule_head);
            pattern = cdr(rule_head);
        } else {
            cpat_cell = rule_head;
        }
        tmpl = cdr(rule);

        // Get compiled pattern from cell
        compiled_pattern *cpat = (compiled_pattern *)CELL_PTR(cpat_cell);

        // Skip the keyword in input
        input_args = cdr(input);

        // Execute compiled pattern
        unsigned bindings = execute_pattern(cpat, input_args);

        if (bindings != TOK_ERROR &&
            (!pattern || pattern_literals_match(pattern, input_args, literals,
                                                ellipsis_id, closure_env,
                                                use_env))) {
            // Generate unique mark for hygiene
            static unsigned syntax_mark = 0;
            unsigned mark = ++syntax_mark;

            gc_protect(&bindings);

            // Referential transparency: find free identifiers in template
            // and bind them to their definition-time values
            unsigned free_ids =
                collect_free_ids(tmpl, bindings, 0, ellipsis_id, closure_env);
            gc_protect(&free_ids);

            unsigned rename_map = 0;
            gc_protect(&rename_map);

            // For each free identifier, check if it's bound in closure_env
            unsigned f = free_ids;
            gc_protect(&f);
            while (f) {
                unsigned free_atom = car(f);
                gc_protect(&free_atom);
                int64_t free_id = CELL_ID(free_atom);

                // Look up in closure environment (macro definition site)
                unsigned closure_val = lookup_silent(free_id, closure_env);

                // If the identifier already denotes the very same binding at
                // the use site as it does at the definition site, renaming it
                // would be a no-op: the alias we would install in use_env
                // resolves to exactly the cell the original name already
                // reaches. Skipping it keeps referential transparency intact
                // and, critically, mints no gensym - so a macro expanded in a
                // loop (the CPS interpreter re-expands on every evaluation)
                // does not consume one permanent atom-table slot per call.
                // Only the cells are compared, never retained: neither lookup
                // allocates, so no GC can intervene before the comparison.
                bool same_binding_at_use_site = false;
                if (closure_val != TOK_ERROR) {
                    unsigned def_cell =
                        env_find_binding_cell(free_id, closure_env);
                    same_binding_at_use_site =
                        def_cell != 0 &&
                        def_cell == env_find_binding_cell(free_id, use_env);
                }

                if (same_binding_at_use_site) {
                    // No rename needed - fall through to the next free id.
                } else if (closure_val != TOK_ERROR &&
                           !generated_gensym_atom(free_atom)) {
                    // Found in closure env - create gensym and record the
                    // binding needed to materialize it at the call site.
                    // An identifier that is ALREADY a generated gensym (e.g.
                    // from the compiler's own definition-time hygiene
                    // renaming for define-syntax/let-syntax) is left alone:
                    // it's already hygienic by construction (its name can't
                    // collide with anything a user could write), and
                    // aliasing it again would create a second layer of
                    // BT_BINDING_REF indirection that the single-level
                    // dereference in try_deref_binding_value doesn't unwind,
                    // leaving the reference resolving to a binding-ref cell
                    // instead of the real value.
                    unsigned gensym = do_gensym();
                    gc_protect(&gensym);
                    gc_protect(&closure_val);

                    unsigned target_val_cell =
                        env_find_binding_cell(free_id, closure_env);
                    if (target_val_cell) {
                        unsigned binding_ref = make_binding_ref_cell(
                            free_atom, target_val_cell);
                        gc_protect(&binding_ref);
                        unsigned entry = alloc_cons(gensym, binding_ref);
                        gc_protect(&entry);
                        produced_bindings = alloc_cons(entry, produced_bindings);
                        gc_unprotect(2);
                    } else {
                        unsigned entry = alloc_cons(gensym, closure_val);
                        gc_protect(&entry);
                        produced_bindings = alloc_cons(entry,
                                                      produced_bindings);
                        gc_unprotect(1);
                    }

                    // Add to rename map: (free_atom . gensym)
                    unsigned entry = alloc_cons(free_atom, gensym);
                    gc_protect(&entry);
                    rename_map = alloc_cons(entry, rename_map);

                    gc_unprotect(2); // gensym, closure_val
                } else if (!generated_gensym_atom(free_atom) &&
                           lookup_silent(free_id, use_env) != TOK_ERROR) {
                    // The identifier was introduced by the transformer, but
                    // is only bound at the expansion site. Rename it to an
                    // unbound gensym so use-site locals cannot capture it.
                    unsigned gensym = do_gensym();
                    gc_protect(&gensym);
                    unsigned entry = alloc_cons(free_atom, gensym);
                    gc_protect(&entry);
                    rename_map = alloc_cons(entry, rename_map);
                    gc_unprotect(2);
                }
                gc_unprotect(1);
                f = cdr(f);
            }
            gc_unprotect(1);

            // Apply free variable renames to template
            unsigned renamed_tmpl = rename_free_ids(tmpl, rename_map);
            gc_protect(&renamed_tmpl);
            if (renamed_tmpl == TOK_ERROR)
                return TOK_ERROR;

            // Hygienize template: rename introduced bindings to gensyms
            unsigned hygienic_tmpl = hygienize_template(renamed_tmpl, bindings);
            gc_protect(&hygienic_tmpl);
            if (hygienic_tmpl == TOK_ERROR)
                return TOK_ERROR;
            unsigned result =
                syntax_expand(hygienic_tmpl, bindings, mark, ellipsis_id);
            gc_protect(&result);
            if (result == TOK_ERROR)
                return TOK_ERROR;

            if (bindings_out)
                append_bindings_chain(bindings_out, produced_bindings);
            return unwrap_protected(result);
        }
    }

    show_error("syntax-rules: no matching pattern");
    return TOK_ERROR;
}

void apply_syntax_bindings(unsigned env, unsigned bindings)
{
    if (!bindings)
        return;

    GC_GUARD;
    unsigned b = bindings;
    gc_protect(&b);
    for (; b; b = cdr(b)) {
        unsigned entry = car(b);
        gc_protect(&entry);
        if (!IS_PAIR(entry) || !IS_ATOM(car(entry))) {
            gc_unprotect(1);
            continue;
        }

        unsigned gensym = car(entry);
        unsigned target = cdr(entry);
        gc_protect(&target);
        if (IS_BINDING_REF(target)) {
            unsigned target_var = cdr(target);
            unsigned target_val_cell = CELL_CAR(target);
            defvar_alias(gensym, target_var, target_val_cell, env);
        } else {
            defvar(gensym, target, env);
        }
        gc_unprotect(2);
    }
}
