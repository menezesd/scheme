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
 * template is hygienized, the let-introduced x is renamed to a gensym, but
 * the protected x is preserved. Result: (let ((g0 2)) x) => 1.
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
 *    Note: Assignment targets (set!, define) are NOT renamed to preserve
 *    the ability to mutate bindings from the use-site environment.
 */

#include "macros.h"
#include "compiled_pattern.h"
#include "context.h"
#include "env.h"
#include <string.h>

// External gensym counter
extern unsigned gensym_counter;

// Generate a fresh gensym atom
static unsigned do_gensym(void)
{
    char buf[32];
    snprintf(buf, sizeof(buf), "g%u", gensym_counter++);
    return atom_from_string(buf);
}

// ============================================================================
// Helper Functions
// ============================================================================

// Check if a symbol is in a list of literals
static bool is_literal(int64_t sym, unsigned literals)
{
    for (; literals; literals = cdr(literals)) {
        if (IS_ATOM(car(literals)) && CELL_ID(car(literals)) == sym)
            return true;
    }
    return false;
}

// Check if symbol is the ellipsis (0 means no ellipsis/disabled)
static bool is_ellipsis(unsigned x, int64_t ellipsis_id)
{
    if (!ellipsis_id)
        return false; // Ellipsis disabled (shadowed)
    return IS_ATOM(x) && CELL_ID(x) == ellipsis_id;
}

// Check if symbol is underscore (wildcard)
static bool is_underscore(unsigned x)
{
    return IS_KEYWORD(x, ctx.kw_underscore);
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
    for (; bindings; bindings = cdr(bindings)) {
        unsigned binding = car(bindings);
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

    FORLIST(b, bindings)
    {
        unsigned binding = car(b);
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
            for (unsigned c = collected; c; c = cdr(c)) {
                if (car(car(c)) == car(binding))
                    return collected; // Already have it
            }
            gc_protect(&binding);
            unsigned result = alloc_cons(binding, collected);
            return result;
        }
        return collected;
    }

    if (IS_PAIR(tmpl)) {
        collected = collect_ellipsis_vars(car(tmpl), bindings, collected);
        return collect_ellipsis_vars(cdr(tmpl), bindings, collected);
    }

    return collected;
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
           id == ctx.kw_cond || id == ctx.kw_and || id == ctx.kw_or ||
           id == ctx.kw_let_syntax || id == ctx.kw_letrec_syntax ||
           id == ctx.kw_define_syntax || id == ctx.kw_syntax_rules ||
           id == ctx.kw_quasiquote || id == ctx.kw_unquote ||
           id == ctx.kw_unquote_splicing || id == ctx.kw_else ||
           id == ctx.kw_ellipsis || id == ctx.kw_underscore ||
           id == ctx.kw_let || id == ctx.kw_letstar || id == ctx.kw_letrec;
}

// Check if identifier is in a list (used for free_ids collection)
static bool id_in_list(int64_t id, unsigned list)
{
    for (; list; list = cdr(list)) {
        if (IS_ATOM(car(list)) && CELL_ID(car(list)) == id)
            return true;
    }
    return false;
}

// Collect free identifiers from a template
// Returns a list of unique atoms that are free (not pattern vars, not special
// forms)
static unsigned collect_free_ids(unsigned tmpl, unsigned bindings,
                                 unsigned collected, int64_t ellipsis_id)
{
    if (!tmpl)
        return collected;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    gc_protect(&collected);

    if (IS_ATOM(tmpl)) {
        int64_t id = CELL_ID(tmpl);
        // Skip if special form, pattern variable, ellipsis, or already
        // collected
        if (!is_special_form(id) && !is_pattern_var(id, bindings) &&
            id != ellipsis_id && !id_in_list(id, collected)) {
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
            if (head_id == ctx.kw_protected || head_id == ctx.kw_syntax_rules) {
                return collected;
            }
            // For quote, skip the quoted data
            if (head_id == ctx.kw_quote) {
                return collected;
            }
            // For set! and define, skip the target variable but collect from
            // expr (set! var expr) - skip var, collect from expr (define var
            // expr) - skip var, collect from expr
            if (head_id == ctx.kw_set || head_id == ctx.kw_define) {
                // Collect only from the expression part (cddr), not the
                // variable (cadr)
                unsigned expr = cddr(tmpl);
                if (expr) {
                    return collect_free_ids(car(expr), bindings, collected,
                                            ellipsis_id);
                }
                return collected;
            }
        }
        // Recursively collect from car and cdr
        collected =
            collect_free_ids(car(tmpl), bindings, collected, ellipsis_id);
        gc_protect(&collected);
        return collect_free_ids(cdr(tmpl), bindings, collected, ellipsis_id);
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointer - GC may have moved tmpl
            unsigned *data = vector_data_ptr(tmpl);
            collected =
                collect_free_ids(data[i], bindings, collected, ellipsis_id);
        }
        return collected;
    }

    return collected;
}

// Rename free identifiers in template according to rename_map
// rename_map is ((old_atom . new_gensym) ...)
static unsigned rename_free_ids(unsigned tmpl, unsigned rename_map)
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
            if (head_id == ctx.kw_protected || head_id == ctx.kw_syntax_rules) {
                return tmpl;
            }
            if (head_id == ctx.kw_quote) {
                return tmpl;
            }
            // For set! and define, don't rename the target variable
            // (set! var expr) -> (set! var renamed-expr)
            // (define var expr) -> (define var renamed-expr)
            if (head_id == ctx.kw_set || head_id == ctx.kw_define) {
                // Validate that tmpl has at least (set!/define var)
                if (!IS_PAIR(cdr(tmpl))) {
                    return tmpl;  // Malformed, return as-is
                }
                unsigned var = cadr(tmpl);
                unsigned expr = cddr(tmpl);
                if (expr && IS_PAIR(expr)) {
                    unsigned new_expr = rename_free_ids(car(expr), rename_map);
                    if (new_expr == TOK_ERROR)
                        return TOK_ERROR;
                    gc_protect(&new_expr);
                    unsigned expr_tail = alloc_cons(new_expr, 0);
                    gc_protect(&expr_tail);
                    unsigned var_tail = alloc_cons(var, expr_tail);
                    gc_protect(&var_tail);
                    return alloc_cons(head, var_tail);
                }
                return tmpl;
            }
        }
        unsigned new_car = rename_free_ids(car(tmpl), rename_map);
        if (new_car == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_car);
        unsigned new_cdr = rename_free_ids(cdr(tmpl), rename_map);
        if (new_cdr == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_cdr);
        if (new_car == car(tmpl) && new_cdr == cdr(tmpl))
            return tmpl;
        return alloc_cons(new_car, new_cdr);
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        bool changed = false;
        for (unsigned i = 0; i < len && !changed; i++) {
            // Refresh data pointer - GC may have moved tmpl
            unsigned *data = vector_data_ptr(tmpl);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned renamed = rename_free_ids(orig, rename_map);
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
            unsigned renamed = rename_free_ids(elem, rename_map);
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
        // Skip protected wrappers - they should not be renamed
        unsigned head = car(tmpl);
        if (IS_ATOM(head) && CELL_ID(head) == ctx.kw_protected) {
            return tmpl;
        }
        unsigned new_car = rename_in_template(car(tmpl), old_id, new_sym);
        if (new_car == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_car);
        unsigned new_cdr = rename_in_template(cdr(tmpl), old_id, new_sym);
        if (new_cdr == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_cdr);
        if (new_car == car(tmpl) && new_cdr == cdr(tmpl))
            return tmpl;
        return alloc_cons(new_car, new_cdr);
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

// Forward declaration
static unsigned hygienize_template(unsigned tmpl, unsigned bindings);

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

    // For letrec, we need to rename in all vals too (they can reference each
    // other) Collect all introduced bindings first
    unsigned renames = 0; // list of (old_id . new_sym)
    gc_protect(&renames);

    // First pass: identify introduced bindings and create gensyms
    // Skip ellipsis (...) which appears in macro templates
    unsigned bl = binding_list;
    gc_protect(&bl);
    while (bl) {
        unsigned binding = car(bl);
        // Skip ellipsis in binding list
        if (IS_ATOM(binding) && CELL_ID(binding) == ctx.kw_ellipsis)
            goto next_intro_binding;
        unsigned var = car(binding);
        if (IS_ATOM(var)) {
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
        while (bl) {
            unsigned binding = car(bl);
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

        unsigned new_body = hygienize_template(body, bindings);
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
            while (bl) {
                unsigned binding = car(bl);
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
    while (bl) {
        unsigned binding = car(bl);
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

    unsigned final_body = hygienize_template(new_body, bindings);
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
    for (unsigned p = params; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        unsigned param = IS_PAIR(p) ? car(p) : p;
        if (IS_ATOM(param)) {
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

    if (!renames) {
        // No introduced params - just hygienize body
        unsigned new_body = hygienize_template(body, bindings);
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
    unsigned final_body = hygienize_template(new_body, bindings);
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
                // Check it's not named let: (let name ((var val) ...) body)
                unsigned second = cadr(tmpl);
                if (IS_PAIR(second) && IS_PAIR(car(second))) {
                    gc_unprotect(2);
                    return hygienize_let(tmpl, bindings, false);
                }
            }
            if (head_id == ctx.kw_letrec) {
                gc_unprotect(2);
                return hygienize_let(tmpl, bindings, true);
            }

            // lambda
            if (head_id == ctx.kw_lambda) {
                gc_unprotect(2);
                return hygienize_lambda(tmpl, bindings);
            }

            // Skip syntax-rules - they have their own patterns/templates
            // that will be hygienized when expanded
            if (head_id == ctx.kw_syntax_rules) {
                gc_unprotect(2);
                return tmpl;
            }

            // For let-syntax/letrec-syntax, hygienize the body but not the
            // syntax-rules bindings
            if (head_id == ctx.kw_let_syntax ||
                head_id == ctx.kw_letrec_syntax) {
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

        // Regular list - hygienize each element
        unsigned new_car = hygienize_template(car(tmpl), bindings);
        if (new_car == TOK_ERROR) {
            gc_unprotect(2);
            return TOK_ERROR;
        }
        gc_protect(&new_car);
        unsigned new_cdr = hygienize_template(cdr(tmpl), bindings);
        if (new_cdr == TOK_ERROR) {
            gc_unprotect(3);
            return TOK_ERROR;
        }
        gc_protect(&new_cdr);
        // Now 4 protected: tmpl, bindings, new_car, new_cdr
        unsigned result;
        if (new_car == car(tmpl) && new_cdr == cdr(tmpl))
            result = tmpl;
        else
            result = alloc_cons(new_car, new_cdr);
        gc_unprotect(4);
        return result;
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
// Pattern Matching
// ============================================================================

unsigned syntax_match(unsigned pattern, unsigned input, unsigned literals,
                      unsigned bindings, int64_t ellipsis_id)
{
    // Protect all parameters - this function is recursive and allocates
    gc_protect(&pattern);
    gc_protect(&input);
    gc_protect(&literals);
    gc_protect(&bindings);

    if (!pattern) {
        gc_unprotect(4);
        return input ? TOK_ERROR : bindings;
    }

    if (is_underscore(pattern)) {
        gc_unprotect(4);
        return bindings;
    }

    // Atom pattern
    if (IS_ATOM(pattern)) {
        int64_t sym = CELL_ID(pattern);

        // Literal must match exactly
        if (is_literal(sym, literals)) {
            gc_unprotect(4);
            if (IS_ATOM(input) && CELL_ID(input) == sym)
                return bindings;
            return TOK_ERROR;
        }

        // Pattern variable - bind it
        unsigned binding = 0;
        gc_protect(&binding);
        binding = alloc_cons(pattern, input);
        unsigned result = alloc_cons(binding, bindings);
        gc_unprotect(5);
        return result;
    }

    // Vector pattern - must match vector input element by element
    if (IS_VECTOR(pattern)) {
        if (!IS_VECTOR(input)) {
            gc_unprotect(4);
            return TOK_ERROR;
        }

        unsigned pat_len = vector_len(pattern);
        unsigned inp_len = vector_len(input);
        unsigned *pat_data = vector_data_ptr(pattern);

        // Check for ellipsis in vector pattern
        unsigned ellipsis_pos = pat_len;
        for (unsigned i = 0; i < pat_len; i++) {
            if (is_ellipsis(pat_data[i], ellipsis_id)) {
                ellipsis_pos = i;
                break;
            }
        }

        if (ellipsis_pos < pat_len) {
            // Pattern has ellipsis at position ellipsis_pos
            // Element before ellipsis is the repeated pattern
            if (ellipsis_pos == 0) {
                gc_unprotect(4);
                return TOK_ERROR; // No pattern before ellipsis
            }

            unsigned elem_pattern = pat_data[ellipsis_pos - 1];
            unsigned pre_count =
                ellipsis_pos - 1; // Elements before repeated pattern
            unsigned post_count =
                pat_len - ellipsis_pos - 1; // Elements after ellipsis

            if (inp_len < pre_count + post_count) {
                gc_unprotect(4);
                return TOK_ERROR;
            }

            // Match elements before the ellipsis pattern
            for (unsigned i = 0; i < pre_count; i++) {
                // Refresh pointers - GC may have moved vectors
                pat_data = vector_data_ptr(pattern);
                unsigned *inp_data = vector_data_ptr(input);
                bindings = syntax_match(pat_data[i], inp_data[i], literals,
                                        bindings, ellipsis_id);
                if (bindings == TOK_ERROR) {
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
            }

            // Collect matches for the ellipsis pattern
            unsigned matches = 0, matches_tail = 0;
            gc_protect(&matches);
            gc_protect(&matches_tail);
            unsigned repeat_count = inp_len - pre_count - post_count;
            for (unsigned i = 0; i < repeat_count; i++) {
                // Refresh pointer - GC may have moved input vector
                unsigned *inp_data = vector_data_ptr(input);
                unsigned inp_elem = inp_data[pre_count + i];
                unsigned elem_bindings = syntax_match(elem_pattern, inp_elem,
                                                      literals, 0, ellipsis_id);
                if (elem_bindings == TOK_ERROR) {
                    gc_unprotect(6);
                    return TOK_ERROR;
                }
                list_append(&matches, &matches_tail, inp_elem);
            }

            // Add ellipsis binding
            // Refresh elem_pattern - GC may have moved pattern vector
            pat_data = vector_data_ptr(pattern);
            elem_pattern = pat_data[ellipsis_pos - 1];
            unsigned ellipsis_binding = 0;
            gc_protect(&ellipsis_binding);
            ellipsis_binding = alloc_cons(elem_pattern, matches);
            bindings = alloc_cons(ellipsis_binding, bindings);
            gc_unprotect(1);

            // Match elements after ellipsis
            for (unsigned i = 0; i < post_count; i++) {
                // Refresh pointers - GC may have moved vectors
                pat_data = vector_data_ptr(pattern);
                unsigned *inp_data = vector_data_ptr(input);
                unsigned pat_idx = ellipsis_pos + 1 + i;
                unsigned inp_idx = inp_len - post_count + i;
                bindings = syntax_match(pat_data[pat_idx], inp_data[inp_idx],
                                        literals, bindings, ellipsis_id);
                if (bindings == TOK_ERROR) {
                    gc_unprotect(6);
                    return TOK_ERROR;
                }
            }

            gc_unprotect(6);
            return bindings;
        }

        // No ellipsis - lengths must match exactly
        if (pat_len != inp_len) {
            gc_unprotect(4);
            return TOK_ERROR;
        }

        for (unsigned i = 0; i < pat_len; i++) {
            // Refresh pointers - GC may have moved vectors
            pat_data = vector_data_ptr(pattern);
            unsigned *inp_data = vector_data_ptr(input);
            bindings = syntax_match(pat_data[i], inp_data[i], literals,
                                    bindings, ellipsis_id);
            if (bindings == TOK_ERROR) {
                gc_unprotect(4);
                return TOK_ERROR;
            }
        }
        gc_unprotect(4);
        return bindings;
    }

    // List pattern
    if (IS_PAIR(pattern)) {
        // Check for ellipsis in pattern: (pat ... rest)
        if (cdr(pattern) && is_ellipsis(cadr(pattern), ellipsis_id)) {
            // Collect matches for elem_pattern (zero or more)
            unsigned matches = 0, matches_tail = 0;
            gc_protect(&matches);
            gc_protect(&matches_tail);
            while (IS_PAIR(input)) {
                // Refresh pattern references - GC may have moved pattern
                unsigned rest_pattern = cddr(pattern);
                // Try to match remaining input against rest pattern
                unsigned rest_input = input;
                unsigned tentative = syntax_match(
                    rest_pattern, rest_input, literals, bindings, ellipsis_id);
                if (tentative != TOK_ERROR) {
                    // Refresh elem_pattern - GC may have moved pattern
                    unsigned elem_pattern = car(pattern);
                    unsigned ellipsis_binding = 0;
                    gc_protect(&ellipsis_binding);
                    gc_protect(&tentative); // MUST protect before alloc - GC
                                            // could happen!
                    ellipsis_binding = alloc_cons(elem_pattern, matches);
                    unsigned result = alloc_cons(ellipsis_binding, tentative);
                    gc_unprotect(8);
                    return result;
                }

                // Refresh elem_pattern - GC may have moved pattern
                unsigned elem_pattern = car(pattern);
                // Try to match one more element
                unsigned car_input = car(input);
                unsigned elem_bindings = syntax_match(elem_pattern, car_input,
                                                      literals, 0, ellipsis_id);
                if (elem_bindings == TOK_ERROR) {
                    break;
                }

                // Add this match to the list
                list_append(&matches, &matches_tail, car(input));
                input = cdr(input);
            }

            // Try matching empty ellipsis
            // Refresh pattern references - GC may have moved pattern
            unsigned rest_pattern = cddr(pattern);
            unsigned tentative = syntax_match(rest_pattern, input, literals,
                                              bindings, ellipsis_id);
            if (tentative != TOK_ERROR) {
                // Refresh elem_pattern - GC may have moved pattern
                unsigned elem_pattern = car(pattern);
                unsigned ellipsis_binding = 0;
                gc_protect(&ellipsis_binding);
                gc_protect(
                    &tentative); // MUST protect before alloc - GC could happen!
                ellipsis_binding = alloc_cons(elem_pattern, matches);
                unsigned result = alloc_cons(ellipsis_binding, tentative);
                gc_unprotect(8);
                return result;
            }
            gc_unprotect(6);
            return TOK_ERROR;
        }

        // Regular cons pattern
        if (!IS_PAIR(input)) {
            gc_unprotect(4);
            return TOK_ERROR;
        }

        // Match car
        bindings = syntax_match(car(pattern), car(input), literals, bindings,
                                ellipsis_id);
        if (bindings == TOK_ERROR) {
            gc_unprotect(4);
            return TOK_ERROR;
        }

        // Match cdr - tail call, unprotect first
        gc_unprotect(4);
        return syntax_match(cdr(pattern), cdr(input), literals, bindings,
                            ellipsis_id);
    }

    // Other patterns (numbers, strings, etc.) must match exactly
    gc_unprotect(4);
    if (!IS_CELL(pattern) || !IS_CELL(input))
        return pattern == input ? bindings : TOK_ERROR;
    if (CELL_TYPE(pattern) == CELL_TYPE(input) &&
        CELL_ID(pattern) == CELL_ID(input))
        return bindings;

    return TOK_ERROR;
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
        // Check for ellipsis: (tmpl ... rest)
        if (cdr(tmpl) && is_ellipsis(cadr(tmpl), ellipsis_id)) {
            unsigned elem_tmpl = car(tmpl);
            unsigned rest_tmpl = cddr(tmpl);

            // Collect ALL ellipsis-bound variables in the template
            unsigned ellipsis_vars = collect_ellipsis_vars(elem_tmpl, bindings, 0);
            gc_protect(&ellipsis_vars);

            // Find iteration count from first variable's values
            unsigned iter_count = 0;
            if (ellipsis_vars) {
                unsigned first_values = cdr(car(ellipsis_vars));
                for (unsigned v = first_values; v; v = cdr(v))
                    iter_count++;
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

        // Regular cons - expand both parts
        unsigned new_car = 0;
        gc_protect(&new_car);
        new_car = syntax_expand(car(tmpl), bindings, mark, ellipsis_id);
        if (new_car == TOK_ERROR) {
            gc_unprotect(3);
            return TOK_ERROR;
        }
        unsigned new_cdr =
            syntax_expand(cdr(tmpl), bindings, mark, ellipsis_id);
        if (new_cdr == TOK_ERROR) {
            gc_unprotect(3);
            return TOK_ERROR;
        }
        gc_protect(&new_cdr);
        unsigned result = alloc_cons(new_car, new_cdr);
        gc_unprotect(4);
        return result;
    }

    // Vector template - expand each element
    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        unsigned *data = vector_data_ptr(tmpl);

        // Check for ellipsis in vector template
        unsigned ellipsis_pos = len;
        for (unsigned i = 0; i < len; i++) {
            if (is_ellipsis(data[i], ellipsis_id)) {
                ellipsis_pos = i;
                break;
            }
        }

        if (ellipsis_pos < len && ellipsis_pos > 0) {
            // Template has ellipsis - need to expand repeated element
            unsigned elem_tmpl = data[ellipsis_pos - 1];
            unsigned pre_count = ellipsis_pos - 1;
            unsigned post_count = len - ellipsis_pos - 1;

            // Find the ellipsis binding for this template element
            unsigned ellipsis_binding = 0;
            unsigned ellipsis_values = 0;

            if (IS_ATOM(elem_tmpl)) {
                ellipsis_binding = find_ellipsis_binding(elem_tmpl, bindings);
            } else {
                for (unsigned b = bindings; b && !ellipsis_binding;
                     b = cdr(b)) {
                    unsigned binding = car(b);
                    unsigned pvar = car(binding);
                    if (IS_PAIR(pvar) || IS_VECTOR(pvar)) {
                        ellipsis_binding = binding;
                    }
                }
            }

            if (ellipsis_binding) {
                ellipsis_values = cdr(ellipsis_binding);
            }

            // Count expanded elements
            unsigned expanded_count = pre_count + post_count;
            FORLIST(ev, ellipsis_values) { expanded_count++; }

            unsigned result = make_vector(expanded_count, 0);
            if (result == TOK_ERROR) {
                gc_unprotect(2);
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
                    gc_unprotect(3);
                    return TOK_ERROR;
                }
                unsigned *result_data = vector_data_ptr(result);
                result_data[idx++] = expanded;
            }

            // Expand repeated element for each ellipsis value
            // Refresh data pointer before accessing elem_tmpl
            data = vector_data_ptr(tmpl);
            elem_tmpl = data[ellipsis_pos - 1];
            unsigned ellipsis_pattern =
                ellipsis_binding ? car(ellipsis_binding) : 0;

            // Protect loop variables across allocations
            gc_protect(&ellipsis_values);
            gc_protect(&ellipsis_pattern);
            gc_protect(&elem_tmpl);
            gc_protect(&bindings);

            for (; ellipsis_values; ellipsis_values = cdr(ellipsis_values)) {
                unsigned current_value = car(ellipsis_values);
                unsigned iter_bindings = bindings;
                gc_protect(&iter_bindings);

                if (ellipsis_pattern && (IS_PAIR(ellipsis_pattern) ||
                                         IS_VECTOR(ellipsis_pattern))) {
                    // Re-match to extract nested bindings
                    unsigned sub_bindings = syntax_match(
                        ellipsis_pattern, current_value, 0, 0, ellipsis_id);
                    if (sub_bindings != TOK_ERROR) {
                        gc_protect(&sub_bindings);
                        for (; sub_bindings; sub_bindings = cdr(sub_bindings)) {
                            unsigned sb_car = car(sub_bindings);
                            gc_protect(&sb_car);
                            iter_bindings = alloc_cons(sb_car, iter_bindings);
                            gc_unprotect(1);
                        }
                        gc_unprotect(1); // sub_bindings
                    }
                } else if (IS_ATOM(elem_tmpl)) {
                    unsigned temp_binding = 0;
                    gc_protect(&temp_binding);
                    temp_binding = alloc_cons(elem_tmpl, current_value);
                    iter_bindings = alloc_cons(temp_binding, bindings);
                    gc_unprotect(1);
                }
                // Refresh result_data pointer - GC may have moved result
                unsigned expanded =
                    syntax_expand(elem_tmpl, iter_bindings, mark, ellipsis_id);
                if (expanded == TOK_ERROR) {
                    gc_unprotect(8);
                    return TOK_ERROR;
                }
                unsigned *result_data = vector_data_ptr(result);
                result_data[idx++] = expanded;
                gc_unprotect(1); // iter_bindings
            }
            gc_unprotect(
                4); // bindings, elem_tmpl, ellipsis_pattern, ellipsis_values

            // Expand elements after ellipsis
            for (unsigned i = 0; i < post_count; i++) {
                // Refresh pointers - GC may have moved vectors
                data = vector_data_ptr(tmpl);
                unsigned elem = data[ellipsis_pos + 1 + i];
                unsigned expanded =
                    syntax_expand(elem, bindings, mark, ellipsis_id);
                if (expanded == TOK_ERROR) {
                    gc_unprotect(3);
                    return TOK_ERROR;
                }
                unsigned *result_data = vector_data_ptr(result);
                result_data[idx++] = expanded;
            }

            gc_unprotect(3); // result, tmpl, bindings
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

unsigned apply_syntax(unsigned transformer, unsigned input, unsigned use_env)
{
    // Protect parameters that may be in nursery - GC can run during expansion
    gc_protect(&transformer);
    gc_protect(&input);
    gc_protect(&use_env);

    // Extract transformer structure:
    // CAR = (ellipsis_cell . (literals . rules)), CDR = closure_env
    unsigned syn_data = car(transformer);
    unsigned closure_env = cdr(transformer);
    unsigned ellipsis_cell = car(syn_data);
    int64_t ellipsis_id = CELL_ID(ellipsis_cell);
    unsigned literals = cadr(syn_data);
    gc_protect(&literals);
    gc_protect(&closure_env);

    unsigned rules = cddr(syn_data);
    gc_protect(&rules);

    unsigned tmpl = 0;
    gc_protect(&tmpl);

    // Try each rule (rules now contain compiled patterns)
    for (; rules; rules = cdr(rules)) {
        unsigned rule = car(rules);
        unsigned cpat_cell = car(rule);
        tmpl = cdr(rule);

        // Get compiled pattern from cell
        compiled_pattern *cpat = (compiled_pattern *)CELL_PTR(cpat_cell);

        // Skip the keyword in input
        unsigned input_args = cdr(input);

        // Execute compiled pattern
        unsigned bindings = execute_pattern(cpat, input_args);

        if (bindings != TOK_ERROR) {
            // Generate unique mark for hygiene
            static unsigned syntax_mark = 0;
            unsigned mark = ++syntax_mark;

            gc_protect(&bindings);

            // Referential transparency: find free identifiers in template
            // and bind them to their definition-time values
            unsigned free_ids =
                collect_free_ids(tmpl, bindings, 0, ellipsis_id);
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

                if (closure_val != TOK_ERROR) {
                    // Found in closure env - create gensym and bind in use_env
                    unsigned gensym = do_gensym();
                    gc_protect(&gensym);
                    gc_protect(&closure_val);

                    // Define gensym in use_env with the closure value
                    defvar(gensym, closure_val, use_env);

                    // Add to rename map: (free_atom . gensym)
                    unsigned entry = alloc_cons(free_atom, gensym);
                    gc_protect(&entry);
                    rename_map = alloc_cons(entry, rename_map);

                    gc_unprotect(3); // entry, gensym, closure_val
                }
                gc_unprotect(1);
                f = cdr(f);
            }
            gc_unprotect(1);

            // Apply free variable renames to template
            unsigned renamed_tmpl = rename_free_ids(tmpl, rename_map);
            gc_protect(&renamed_tmpl);
            if (renamed_tmpl == TOK_ERROR) {
                gc_unprotect(11);
                return TOK_ERROR;
            }

            // Hygienize template: rename introduced bindings to gensyms
            unsigned hygienic_tmpl = hygienize_template(renamed_tmpl, bindings);
            gc_protect(&hygienic_tmpl);
            if (hygienic_tmpl == TOK_ERROR) {
                gc_unprotect(12);
                return TOK_ERROR;
            }
            unsigned result =
                syntax_expand(hygienic_tmpl, bindings, mark, ellipsis_id);
            gc_protect(&result);
            if (result == TOK_ERROR) {
                gc_unprotect(13);
                return TOK_ERROR;
            }
            result = unwrap_protected(result);
            if (result == TOK_ERROR) {
                gc_unprotect(13);
                return TOK_ERROR;
            }
            gc_unprotect(
                13); // result, hygienic_tmpl, renamed_tmpl, rename_map,
                     // free_ids, bindings, tmpl, rules, closure_env, literals,
                     // use_env, input, transformer
            return result;
        }
    }
    gc_unprotect(
        7); // tmpl, rules, closure_env, literals, use_env, input, transformer

    show_error("syntax-rules: no matching pattern");
    return TOK_ERROR;
}
