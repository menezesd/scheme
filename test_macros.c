// Unit tests for macro module
#define _POSIX_C_SOURCE 200809L
#include "compiled_pattern.h"
#include "context.h"
#include "eval_internal.h"
#include "macros.h"
#include "reader.h"
#include "test_framework.h"
#include "types.h"
#include <stdint.h>

// ============================================================================
// Helper: Create patterns and templates from atoms
// ============================================================================

static unsigned atom(const char *s)
{
    return atom_from_string(s);
}

// Read the first datum from a Scheme source string
static unsigned read_from_string(const char *str)
{
    FILE *f = fmemopen((void *)str, strlen(str), "r");
    if (!f)
        return TOK_ERROR;
    unsigned result = read_obj_port(f);
    reader_forget_port(f);
    fclose(f);
    return result;
}

// Build a transformer from a (syntax-rules ...) form and apply it to a
// call form - the production entry points, so these tests exercise the
// real matcher rather than an internal helper.
static unsigned expand_with(const char *rules, const char *call)
{
    unsigned tform = read_from_string(rules);
    if (tform == TOK_ERROR)
        return TOK_ERROR;
    unsigned transformer = make_syntax_transformer(tform, 0);
    if (transformer == TOK_ERROR)
        return TOK_ERROR;
    unsigned input = read_from_string(call);
    if (input == TOK_ERROR)
        return TOK_ERROR;
    return apply_syntax(transformer, input, 0, 0);
}

static int is_fixnum_value(unsigned x, int64_t val)
{
    return IS_NUM(x) && CELL_ID(x) == val;
}

// ============================================================================
// Pattern Matching Tests - Basic (via production apply_syntax)
// ============================================================================

TEST(match_empty_pattern)
{
    // Zero-argument rule matches exactly-zero arguments
    ASSERT(is_fixnum_value(expand_with(
               "(syntax-rules () ((_) 7))", "(m)"),
           7));
    ASSERT(expand_with("(syntax-rules () ((_) 7))", "(m 1)") == TOK_ERROR);
    PASS();
}

TEST(match_atom_binds_value)
{
    // Atom pattern binds the input value
    ASSERT(is_fixnum_value(expand_with("(syntax-rules () ((_ x) x))",
                                       "(m 42)"),
                           42));
    PASS();
}

TEST(match_literal_exact)
{
    // Literal pattern requires exact match
    unsigned r = expand_with("(syntax-rules (foo) ((_ foo) foo))", "(m foo)");
    ASSERT(IS_ATOM(r));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(r)], "foo");
    PASS();
}

TEST(match_literal_fails)
{
    // Literal pattern fails on mismatch
    ASSERT(expand_with("(syntax-rules (foo) ((_ foo) (quote yes)))",
                       "(m bar)") == TOK_ERROR);
    PASS();
}

TEST(match_non_list_literal_spec_rejected)
{
    // Production validates that the literals position is a proper list
    ASSERT(expand_with("(syntax-rules 7 ((_ x) x))", "(m 6)") == TOK_ERROR);
    PASS();
}

TEST(match_direct_fixnum_literal)
{
    ASSERT(is_fixnum_value(expand_with("(syntax-rules () ((_ 42) 42))",
                                       "(m 42)"),
                           42));

    ASSERT(expand_with("(syntax-rules () ((_ 42) (quote yes)))",
                       "(m 41)") == TOK_ERROR);
    PASS();
}

TEST(match_underscore)
{
    // Underscore matches anything without binding
    unsigned r = expand_with("(syntax-rules () ((_ _) _))", "(m 99)");
    ASSERT(IS_ATOM(r));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(r)], "_");
    PASS();
}

// ============================================================================
// Pattern Matching Tests - Lists
// ============================================================================

TEST(match_simple_list)
{
    // Pattern (x y) matches input (1 2); echoed in reversed order
    unsigned r = expand_with("(syntax-rules () ((_ x y) (y x)))", "(m 1 2)");
    ASSERT(IS_PAIR(r));
    ASSERT(is_fixnum_value(car(r), 2));
    ASSERT(is_fixnum_value(cadr(r), 1));
    PASS();
}

TEST(match_nested_list)
{
    // Pattern (x) destructures a one-element list; template rebuilds it
    unsigned r = expand_with("(syntax-rules () ((_ (x)) (x)))", "(m (42))");
    ASSERT(IS_PAIR(r));
    ASSERT(is_fixnum_value(car(r), 42));
    ASSERT(cdr(r) == 0);
    PASS();
}

TEST(match_dotted_pair_tail)
{
    // Dotted formals accept improper input tails
    ASSERT(is_fixnum_value(expand_with("(syntax-rules () ((_ x . y) x))",
                                       "(m 1 . 2)"),
                           1));
    PASS();
}

TEST(match_list_length_mismatch)
{
    // Pattern (x y) doesn't match input (1 2 3)
    ASSERT(expand_with("(syntax-rules () ((_ x y) (quote no)))",
                       "(m 1 2 3)") == TOK_ERROR);
    PASS();
}

// ============================================================================
// Pattern Matching Tests - Ellipsis
// ============================================================================

TEST(match_ellipsis_zero)
{
    // Pattern (x ...) matches empty input; bare x inserts the empty list
    unsigned r = expand_with("(syntax-rules () ((_ x ...) x))", "(m)");
    ASSERT(r == 0);
    PASS();
}

TEST(match_ellipsis_multiple)
{
    // Pattern (x ...) matches (1 2 3); bare x inserts the whole list
    unsigned r = expand_with("(syntax-rules () ((_ x ...) x))", "(m 1 2 3)");
    ASSERT(IS_PAIR(r));
    ASSERT(list_length(r) == 3);
    ASSERT(is_fixnum_value(car(r), 1));
    ASSERT(is_fixnum_value(cadr(r), 2));
    ASSERT(is_fixnum_value(cddr(r) ? car(cddr(r)) : 0, 3));
    PASS();
}

TEST(match_ellipsis_with_tail)
{
    // Pattern (x ... y) splits input (1 2 3) with y bound to 3
    unsigned r =
        expand_with("(syntax-rules () ((_ x ... y) (+ 40 y)))", "(m 1 2 3)");
    ASSERT(IS_PAIR(r));
    ASSERT(is_fixnum_value(cadr(r), 40));
    ASSERT(is_fixnum_value(car(cddr(r)), 3));
    PASS();
}

// ============================================================================
// Pattern Matching Tests - Vectors
// ============================================================================

TEST(match_vector_simple)
{
    // Vector pattern binds element-wise; vector template echoes values
    unsigned r = expand_with("(syntax-rules () ((_ #(x y)) #(y x)))",
                             "(m #(1 2))");
    ASSERT(IS_VECTOR(r));
    ASSERT(vector_len(r) == 2);
    ASSERT(is_fixnum_value(vector_data_ptr(r)[0], 2));
    ASSERT(is_fixnum_value(vector_data_ptr(r)[1], 1));
    PASS();
}

TEST(match_vector_length_mismatch)
{
    // Pattern #(_ _) doesn't match input #(1)
    ASSERT(expand_with("(syntax-rules () ((_ #(_ _)) (quote no)))",
                       "(m #(1))") == TOK_ERROR);
    PASS();
}

// ============================================================================
// Template Expansion Tests
// ============================================================================

TEST(expand_atom_passthrough)
{
    // Unbound atom passes through unchanged
    unsigned tmpl = atom("foo");
    unsigned result = syntax_expand(tmpl, 0, 0, ctx.kw_ellipsis);
    ASSERT(result == tmpl);
    PASS();
}

TEST(expand_atom_substitute)
{
    // Bound atom substitutes its value
    unsigned tmpl = atom("x");
    unsigned val = store(42);
    unsigned binding = alloc_cons(tmpl, val);
    unsigned bindings = alloc_cons(binding, 0);
    unsigned result = syntax_expand(tmpl, bindings, 0, ctx.kw_ellipsis);
    ASSERT_EQ(CELL_ID(result), 42);
    PASS();
}

TEST(expand_list)
{
    // List template expands elements
    unsigned x = atom("x");
    unsigned y = atom("y");
    unsigned tmpl = alloc_cons(x, alloc_cons(y, 0));

    unsigned bx = alloc_cons(x, store(1));
    unsigned by = alloc_cons(y, store(2));
    unsigned bindings = alloc_cons(bx, alloc_cons(by, 0));

    unsigned result = syntax_expand(tmpl, bindings, 0, ctx.kw_ellipsis);
    ASSERT(IS_PAIR(result));
    ASSERT_EQ(CELL_ID(car(result)), 1);
    ASSERT_EQ(CELL_ID(cadr(result)), 2);
    PASS();
}

TEST(expand_nested_list)
{
    // Nested list template expands recursively
    unsigned x = atom("x");
    unsigned tmpl = alloc_cons(alloc_cons(x, 0), 0);

    unsigned binding = alloc_cons(x, store(42));
    unsigned bindings = alloc_cons(binding, 0);

    unsigned result = syntax_expand(tmpl, bindings, 0, ctx.kw_ellipsis);
    ASSERT(IS_PAIR(result));
    ASSERT(IS_PAIR(car(result)));
    ASSERT_EQ(CELL_ID(car(car(result))), 42);
    PASS();
}

TEST(expand_vector)
{
    // Vector template expands elements
    unsigned x = atom("x");
    unsigned tmpl = make_vector(1, 0);
    vector_data_ptr(tmpl)[0] = x;

    unsigned binding = alloc_cons(x, store(42));
    unsigned bindings = alloc_cons(binding, 0);

    unsigned result = syntax_expand(tmpl, bindings, 0, ctx.kw_ellipsis);
    ASSERT(IS_VECTOR(result));
    ASSERT_EQ(vector_len(result), 1);
    ASSERT_EQ(CELL_ID(vector_data_ptr(result)[0]), 42);
    PASS();
}

TEST(expand_malformed_binding_list_ignores_bindings)
{
    unsigned x = atom("x");
    unsigned result = syntax_expand(x, store(1), 0, ctx.kw_ellipsis);

    ASSERT_EQ(result, x);
    PASS();
}

TEST(expand_malformed_binding_entry_ignores_entry)
{
    unsigned x = atom("x");
    unsigned bindings = alloc_cons(store(1), 0);
    unsigned result = syntax_expand(x, bindings, 0, ctx.kw_ellipsis);

    ASSERT_EQ(result, x);
    PASS();
}

TEST(apply_syntax_rejects_non_syntax_transformer)
{
    unsigned input = alloc_cons(atom("m"), 0);
    unsigned result = apply_syntax(store(1), input, 0, 0);

    ASSERT_EQ(result, TOK_ERROR);
    PASS();
}

TEST(apply_syntax_rejects_malformed_input)
{
    unsigned transformer = alloc_cons(store(1), 0);
    CELL_TYPE(transformer) = BT_SYNTAX;
    unsigned result = apply_syntax(transformer, store(2), 0, 0);

    ASSERT_EQ(result, TOK_ERROR);
    PASS();
}

TEST(apply_syntax_rejects_malformed_rules)
{
    unsigned ellipsis = atom("...");
    unsigned syn_data = alloc_cons(ellipsis, alloc_cons(0, store(1)));
    unsigned transformer = alloc_cons(syn_data, 0);
    CELL_TYPE(transformer) = BT_SYNTAX;
    unsigned input = alloc_cons(atom("m"), 0);

    unsigned result = apply_syntax(transformer, input, 0, 0);

    ASSERT_EQ(result, TOK_ERROR);
    PASS();
}

TEST(apply_syntax_rejects_unregistered_compiled_pattern)
{
    unsigned input = alloc_cons(atom("m"), 0);

    unsigned cpat_cell = alloc();
    CELL_TYPE(cpat_cell) = BT_COMPILED_PATTERN;
    CELL_PTR(cpat_cell) = (void *)(uintptr_t)1;

    unsigned rule = alloc_cons(cpat_cell, alloc_cons(store(1), 0));
    unsigned rules = alloc_cons(rule, 0);
    unsigned ellipsis = atom("...");
    unsigned syn_data = alloc_cons(ellipsis, alloc_cons(0, rules));
    unsigned transformer = alloc_cons(syn_data, 0);
    CELL_TYPE(transformer) = BT_SYNTAX;

    unsigned result = apply_syntax(transformer, input, 0, 0);

    ASSERT_EQ(result, TOK_ERROR);
    CELL_TYPE(cpat_cell) = BT_FREE;
    CELL_PTR(cpat_cell) = NULL;
    PASS();
}

// ============================================================================
// Main
// ============================================================================

int main(void)
{
    // Initialize interpreter infrastructure
    init_heap();
    init_keywords();

    printf("=== Macro Unit Tests ===\n");

    // Pattern matching through the production path
    RUN_TEST(match_empty_pattern);
    RUN_TEST(match_atom_binds_value);
    RUN_TEST(match_literal_exact);
    RUN_TEST(match_literal_fails);
    RUN_TEST(match_non_list_literal_spec_rejected);
    RUN_TEST(match_direct_fixnum_literal);
    RUN_TEST(match_underscore);

    // List pattern matching
    RUN_TEST(match_simple_list);
    RUN_TEST(match_nested_list);
    RUN_TEST(match_dotted_pair_tail);
    RUN_TEST(match_list_length_mismatch);

    // Ellipsis pattern matching
    RUN_TEST(match_ellipsis_zero);
    RUN_TEST(match_ellipsis_multiple);
    RUN_TEST(match_ellipsis_with_tail);

    // Vector pattern matching
    RUN_TEST(match_vector_simple);
    RUN_TEST(match_vector_length_mismatch);

    // Template expansion
    RUN_TEST(expand_atom_passthrough);
    RUN_TEST(expand_atom_substitute);
    RUN_TEST(expand_list);
    RUN_TEST(expand_nested_list);
    RUN_TEST(expand_vector);
    RUN_TEST(expand_malformed_binding_list_ignores_bindings);
    RUN_TEST(expand_malformed_binding_entry_ignores_entry);
    RUN_TEST(apply_syntax_rejects_non_syntax_transformer);
    RUN_TEST(apply_syntax_rejects_malformed_input);
    RUN_TEST(apply_syntax_rejects_malformed_rules);
    RUN_TEST(apply_syntax_rejects_unregistered_compiled_pattern);

    TEST_SUMMARY("macros");
}
