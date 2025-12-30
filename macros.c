/**
 * @file macros.c
 * @brief Hygienic macro system (syntax-rules)
 *
 * This file implements R5RS-style hygienic macros using syntax-rules.
 *
 * ## Syntax-Rules Structure
 * A syntax-rules object is stored as:
 *   (literals . ((pattern1 . template1) (pattern2 . template2) ...))
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
 * ## Hygiene
 * Pattern variables are alpha-renamed using gensym to prevent capture.
 * Literals are compared by identity, not by name.
 */

#include "macros.h"
#include "context.h"

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

// Check if symbol is ellipsis
static bool is_ellipsis(unsigned x) { return IS_KEYWORD(x, ctx.kw_ellipsis); }

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
        if (IS_ATOM(pvar) && IS_ATOM(var) && CELL_ID(pvar) == CELL_ID(var))
            return cdr(binding);
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

// ============================================================================
// Pattern Matching
// ============================================================================

unsigned syntax_match(unsigned pattern, unsigned input, unsigned literals,
                      unsigned bindings)
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
        unsigned *inp_data = vector_data_ptr(input);

        // Check for ellipsis in vector pattern
        unsigned ellipsis_pos = pat_len;
        for (unsigned i = 0; i < pat_len; i++) {
            if (is_ellipsis(pat_data[i])) {
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
                bindings =
                    syntax_match(pat_data[i], inp_data[i], literals, bindings);
                if (bindings == TOK_ERROR) {
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
            }

            // Collect matches for the ellipsis pattern
            unsigned matches = 0, matches_tail = 0;
            unsigned repeat_count = inp_len - pre_count - post_count;
            for (unsigned i = 0; i < repeat_count; i++) {
                unsigned inp_elem = inp_data[pre_count + i];
                unsigned elem_bindings =
                    syntax_match(elem_pattern, inp_elem, literals, 0);
                if (elem_bindings == TOK_ERROR) {
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
                list_append(&matches, &matches_tail, inp_elem);
            }

            // Add ellipsis binding
            unsigned ellipsis_binding = 0;
            gc_protect(&ellipsis_binding);
            ellipsis_binding = alloc_cons(elem_pattern, matches);
            bindings = alloc_cons(ellipsis_binding, bindings);
            gc_unprotect(1);

            // Match elements after ellipsis
            for (unsigned i = 0; i < post_count; i++) {
                unsigned pat_idx = ellipsis_pos + 1 + i;
                unsigned inp_idx = inp_len - post_count + i;
                bindings = syntax_match(pat_data[pat_idx], inp_data[inp_idx],
                                        literals, bindings);
                if (bindings == TOK_ERROR) {
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
            }

            gc_unprotect(4);
            return bindings;
        }

        // No ellipsis - lengths must match exactly
        if (pat_len != inp_len) {
            gc_unprotect(4);
            return TOK_ERROR;
        }

        for (unsigned i = 0; i < pat_len; i++) {
            bindings =
                syntax_match(pat_data[i], inp_data[i], literals, bindings);
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
        if (cdr(pattern) && is_ellipsis(cadr(pattern))) {
            unsigned elem_pattern = car(pattern);
            unsigned rest_pattern = cddr(pattern);

            // Collect matches for elem_pattern (zero or more)
            unsigned matches = 0, matches_tail = 0;
            while (IS_PAIR(input)) {
                // Try to match remaining input against rest pattern
                unsigned rest_input = input;
                unsigned tentative =
                    syntax_match(rest_pattern, rest_input, literals, bindings);
                if (tentative != TOK_ERROR) {
                    unsigned ellipsis_binding = 0;
                    gc_protect(&ellipsis_binding);
                    ellipsis_binding = alloc_cons(elem_pattern, matches);
                    gc_protect(&tentative);
                    unsigned result = alloc_cons(ellipsis_binding, tentative);
                    gc_unprotect(6);
                    return result;
                }

                // Try to match one more element
                unsigned elem_bindings =
                    syntax_match(elem_pattern, car(input), literals, 0);
                if (elem_bindings == TOK_ERROR)
                    break;

                // Add this match to the list
                list_append(&matches, &matches_tail, car(input));
                input = cdr(input);
            }

            // Try matching empty ellipsis
            unsigned tentative =
                syntax_match(rest_pattern, input, literals, bindings);
            if (tentative != TOK_ERROR) {
                unsigned ellipsis_binding = 0;
                gc_protect(&ellipsis_binding);
                ellipsis_binding = alloc_cons(elem_pattern, matches);
                gc_protect(&tentative);
                unsigned result = alloc_cons(ellipsis_binding, tentative);
                gc_unprotect(6);
                return result;
            }
            gc_unprotect(4);
            return TOK_ERROR;
        }

        // Regular cons pattern
        if (!IS_PAIR(input)) {
            gc_unprotect(4);
            return TOK_ERROR;
        }

        // Match car
        bindings = syntax_match(car(pattern), car(input), literals, bindings);
        if (bindings == TOK_ERROR) {
            gc_unprotect(4);
            return TOK_ERROR;
        }

        // Match cdr - tail call, unprotect first
        gc_unprotect(4);
        return syntax_match(cdr(pattern), cdr(input), literals, bindings);
    }

    // Other patterns (numbers, strings, etc.) must match exactly
    gc_unprotect(4);
    if (CELL_TYPE(pattern) == CELL_TYPE(input) &&
        CELL_ID(pattern) == CELL_ID(input))
        return bindings;

    return TOK_ERROR;
}

// ============================================================================
// Template Expansion
// ============================================================================

unsigned syntax_expand(unsigned tmpl, unsigned bindings, unsigned mark)
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
        gc_unprotect(2);
        if (lookup_result != TOK_ERROR)
            return lookup_result;
        return tmpl;
    }

    // List template
    if (IS_PAIR(tmpl)) {
        // Check for ellipsis: (tmpl ... rest)
        if (cdr(tmpl) && is_ellipsis(cadr(tmpl))) {
            unsigned elem_tmpl = car(tmpl);
            unsigned rest_tmpl = cddr(tmpl);

            // Find the ellipsis binding
            unsigned ellipsis_binding = 0;
            unsigned ellipsis_pattern = 0;
            unsigned ellipsis_values = 0;

            if (IS_ATOM(elem_tmpl)) {
                ellipsis_binding = find_ellipsis_binding(elem_tmpl, bindings);
            } else if (IS_PAIR(elem_tmpl)) {
                for (unsigned b = bindings; b && !ellipsis_binding;
                     b = cdr(b)) {
                    unsigned binding = car(b);
                    unsigned pvar = car(binding);
                    if (IS_PAIR(pvar)) {
                        ellipsis_binding = binding;
                    }
                }
            }

            if (ellipsis_binding) {
                ellipsis_pattern = car(ellipsis_binding);
                ellipsis_values = cdr(ellipsis_binding);
            } else {
                // Fallback: look for any list-valued binding
                FORLIST(b, bindings)
                {
                    unsigned binding = car(b);
                    unsigned val = cdr(binding);
                    if (IS_PAIR(val)) {
                        ellipsis_pattern = car(binding);
                        ellipsis_values = val;
                        break;
                    }
                }
            }

            // Expand template for each ellipsis value
            // Protect loop variables that survive across allocations
            unsigned result = 0, result_tail = 0;
            gc_protect(&result);
            gc_protect(&result_tail);
            gc_protect(&ellipsis_values);
            gc_protect(&elem_tmpl);
            gc_protect(&rest_tmpl);
            gc_protect(&ellipsis_pattern);
            for (; ellipsis_values; ellipsis_values = cdr(ellipsis_values)) {
                unsigned current_value = car(ellipsis_values);

                // Create iteration bindings
                unsigned iter_bindings = bindings;
                gc_protect(&iter_bindings);
                gc_protect(&current_value);
                if (ellipsis_pattern && IS_PAIR(ellipsis_pattern)) {
                    unsigned sub_bindings =
                        syntax_match(ellipsis_pattern, current_value, 0, 0);
                    if (sub_bindings != TOK_ERROR) {
                        // Protect sub_bindings across allocations
                        unsigned sb = sub_bindings;
                        gc_protect(&sb);
                        while (sb) {
                            unsigned sb_car = car(sb);
                            gc_protect(&sb_car);
                            iter_bindings = alloc_cons(sb_car, iter_bindings);
                            gc_unprotect(1); // sb_car
                            sb = cdr(sb);
                        }
                        gc_unprotect(1); // sb
                    }
                } else if (IS_ATOM(elem_tmpl)) {
                    unsigned temp_binding = 0;
                    gc_protect(&temp_binding);
                    temp_binding = alloc_cons(elem_tmpl, current_value);
                    iter_bindings = alloc_cons(temp_binding, bindings);
                    gc_unprotect(1);
                }

                unsigned expanded =
                    syntax_expand(elem_tmpl, iter_bindings, mark);
                gc_protect(&expanded);
                list_append(&result, &result_tail, expanded);
                gc_unprotect(3); // expanded, current_value, iter_bindings
            }
            gc_unprotect(6); // ellipsis_pattern, rest_tmpl, elem_tmpl,
                             // ellipsis_values, result_tail, result

            // Append expanded rest
            gc_protect(&result);
            gc_protect(&result_tail);
            unsigned rest_expanded = syntax_expand(rest_tmpl, bindings, mark);
            if (result) {
                write_barrier(result_tail,
                              rest_expanded); // result_tail may be in old gen
                CELL_CDR(result_tail) = rest_expanded;
                gc_unprotect(4); // result_tail, result, bindings, tmpl
                return result;
            }
            gc_unprotect(4); // result_tail, result, bindings, tmpl
            return rest_expanded;
        }

        // Regular cons - expand both parts
        unsigned new_car = 0;
        gc_protect(&new_car);
        new_car = syntax_expand(car(tmpl), bindings, mark);
        unsigned new_cdr = syntax_expand(cdr(tmpl), bindings, mark);
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
            if (is_ellipsis(data[i])) {
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
            unsigned *result_data = vector_data_ptr(result);
            unsigned idx = 0;

            // Expand elements before ellipsis
            for (unsigned i = 0; i < pre_count; i++) {
                result_data[idx++] = syntax_expand(data[i], bindings, mark);
            }

            // Expand repeated element for each ellipsis value
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
                    unsigned sub_bindings =
                        syntax_match(ellipsis_pattern, current_value, 0, 0);
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
                gc_unprotect(1); // iter_bindings

                result_data[idx++] =
                    syntax_expand(elem_tmpl, iter_bindings, mark);
            }
            gc_unprotect(
                4); // bindings, elem_tmpl, ellipsis_pattern, ellipsis_values

            // Expand elements after ellipsis
            for (unsigned i = 0; i < post_count; i++) {
                result_data[idx++] =
                    syntax_expand(data[ellipsis_pos + 1 + i], bindings, mark);
            }

            gc_unprotect(2);
            return result;
        }

        // No ellipsis - expand each element
        unsigned result = make_vector(len, 0);
        unsigned *result_data = vector_data_ptr(result);
        for (unsigned i = 0; i < len; i++) {
            result_data[i] = syntax_expand(data[i], bindings, mark);
        }
        gc_unprotect(2);
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
    (void)use_env;

    // Protect parameters that may be in nursery - GC can run during expansion
    gc_protect(&transformer);
    gc_protect(&input);

    unsigned literals = car(transformer);
    gc_protect(&literals);

    unsigned rules = cdr(transformer);
    gc_protect(&rules);

    unsigned tmpl = 0;
    gc_protect(&tmpl);

    // Try each rule
    for (; rules; rules = cdr(rules)) {
        unsigned rule = car(rules);
        unsigned pattern = car(rule);
        tmpl = cadr(rule);

        // Skip the keyword in pattern (first element is macro name)
        if (IS_PAIR(pattern))
            pattern = cdr(pattern);

        // Skip the keyword in input
        unsigned input_args = cdr(input);

        unsigned bindings = syntax_match(pattern, input_args, literals, 0);

        if (bindings != TOK_ERROR) {
            // Generate unique mark for hygiene
            static unsigned syntax_mark = 0;
            unsigned mark = ++syntax_mark;

            gc_protect(&bindings);
            unsigned result = syntax_expand(tmpl, bindings, mark);
            gc_unprotect(
                6); // bindings, tmpl, rules, literals, input, transformer
            return result;
        }
    }
    gc_unprotect(5); // tmpl, rules, literals, input, transformer

    show_error("syntax-rules: no matching pattern");
    return TOK_ERROR;
}
