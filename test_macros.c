// Unit tests for macro module
#define _POSIX_C_SOURCE 200809L
#include "context.h"
#include "macros.h"
#include "test_framework.h"
#include "types.h"

// ============================================================================
// Helper: Create patterns and templates from atoms
// ============================================================================

static unsigned atom(const char *s) { return atom_from_string(s); }

// ============================================================================
// Pattern Matching Tests - Basic
// ============================================================================

TEST(match_empty_pattern)
{
    // Empty pattern matches empty input
    unsigned bindings = syntax_match(0, 0, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings != TOK_ERROR);
    ASSERT(bindings == 0); // No bindings
    PASS();
}

TEST(match_empty_pattern_fails_nonempty)
{
    // Empty pattern doesn't match non-empty input
    unsigned input = store(42);
    unsigned bindings = syntax_match(0, input, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings == TOK_ERROR);
    PASS();
}

TEST(match_atom_binds_value)
{
    // Atom pattern binds to input value
    unsigned pattern = atom("x");
    unsigned input = store(42);
    unsigned bindings = syntax_match(pattern, input, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings != TOK_ERROR);
    // Bindings should be ((x . 42))
    ASSERT(IS_PAIR(bindings));
    ASSERT(IS_PAIR(car(bindings)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(car(car(bindings)))], "x");
    ASSERT_EQ(CELL_ID(cdr(car(bindings))), 42);
    PASS();
}

TEST(match_literal_exact)
{
    // Literal pattern requires exact match
    unsigned pattern = atom("foo");
    unsigned input = atom("foo");
    unsigned literals = alloc_cons(atom("foo"), 0);
    unsigned bindings =
        syntax_match(pattern, input, literals, 0, ctx.kw_ellipsis);
    ASSERT(bindings != TOK_ERROR);
    PASS();
}

TEST(match_literal_fails)
{
    // Literal pattern fails on mismatch
    unsigned pattern = atom("foo");
    unsigned input = atom("bar");
    unsigned literals = alloc_cons(atom("foo"), 0);
    unsigned bindings =
        syntax_match(pattern, input, literals, 0, ctx.kw_ellipsis);
    ASSERT(bindings == TOK_ERROR);
    PASS();
}

TEST(match_underscore)
{
    // Underscore matches anything without binding
    unsigned pattern = atom("_");
    unsigned input = store(42);
    unsigned bindings = syntax_match(pattern, input, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings != TOK_ERROR);
    ASSERT(bindings == 0); // No bindings for underscore
    PASS();
}

// ============================================================================
// Pattern Matching Tests - Lists
// ============================================================================

TEST(match_simple_list)
{
    // Pattern (x y) matches input (1 2)
    unsigned pattern = alloc_cons(atom("x"), alloc_cons(atom("y"), 0));
    unsigned input = alloc_cons(store(1), alloc_cons(store(2), 0));
    unsigned bindings = syntax_match(pattern, input, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings != TOK_ERROR);
    // Should have bindings for x and y
    ASSERT(list_length(bindings) == 2);
    PASS();
}

TEST(match_nested_list)
{
    // Pattern ((x)) matches input ((42))
    unsigned pattern = alloc_cons(alloc_cons(atom("x"), 0), 0);
    unsigned input = alloc_cons(alloc_cons(store(42), 0), 0);
    unsigned bindings = syntax_match(pattern, input, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings != TOK_ERROR);
    PASS();
}

TEST(match_dotted_pair)
{
    // Pattern (x . y) matches input (1 . 2)
    unsigned pattern = alloc_cons(atom("x"), atom("y"));
    unsigned input = alloc_cons(store(1), store(2));
    unsigned bindings = syntax_match(pattern, input, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings != TOK_ERROR);
    PASS();
}

TEST(match_list_length_mismatch)
{
    // Pattern (x y) doesn't match input (1 2 3)
    unsigned pattern = alloc_cons(atom("x"), alloc_cons(atom("y"), 0));
    unsigned input =
        alloc_cons(store(1), alloc_cons(store(2), alloc_cons(store(3), 0)));
    unsigned bindings = syntax_match(pattern, input, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings == TOK_ERROR);
    PASS();
}

// ============================================================================
// Pattern Matching Tests - Ellipsis
// ============================================================================

TEST(match_ellipsis_zero)
{
    // Pattern (x ...) matches empty input ()
    unsigned pattern = alloc_cons(atom("x"), alloc_cons(atom("..."), 0));
    unsigned input = 0;
    unsigned bindings = syntax_match(pattern, input, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings != TOK_ERROR);
    PASS();
}

TEST(match_ellipsis_multiple)
{
    // Pattern (x ...) matches input (1 2 3)
    unsigned pattern = alloc_cons(atom("x"), alloc_cons(atom("..."), 0));
    unsigned input =
        alloc_cons(store(1), alloc_cons(store(2), alloc_cons(store(3), 0)));
    unsigned bindings = syntax_match(pattern, input, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings != TOK_ERROR);
    PASS();
}

TEST(match_ellipsis_with_tail)
{
    // Pattern (x ... y) matches input (1 2 3) with y=3
    unsigned pattern = alloc_cons(
        atom("x"), alloc_cons(atom("..."), alloc_cons(atom("y"), 0)));
    unsigned input =
        alloc_cons(store(1), alloc_cons(store(2), alloc_cons(store(3), 0)));
    unsigned bindings = syntax_match(pattern, input, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings != TOK_ERROR);
    PASS();
}

// ============================================================================
// Pattern Matching Tests - Vectors
// ============================================================================

TEST(match_vector_simple)
{
    // Pattern #(x y) matches input #(1 2)
    unsigned pattern = make_vector(2, 0);
    unsigned *pat_data = vector_data_ptr(pattern);
    pat_data[0] = atom("x");
    pat_data[1] = atom("y");

    unsigned input = make_vector(2, 0);
    unsigned *inp_data = vector_data_ptr(input);
    inp_data[0] = store(1);
    inp_data[1] = store(2);

    unsigned bindings = syntax_match(pattern, input, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings != TOK_ERROR);

    // Check bindings contain ((y . 2) (x . 1))
    ASSERT(IS_PAIR(bindings));
    // First binding should be (y . 2)
    unsigned b1 = car(bindings);
    ASSERT(IS_PAIR(b1));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(car(b1))], "y");
    ASSERT_EQ(CELL_ID(cdr(b1)), 2);
    // Second binding should be (x . 1)
    ASSERT(IS_PAIR(cdr(bindings)));
    unsigned b2 = car(cdr(bindings));
    ASSERT(IS_PAIR(b2));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(car(b2))], "x");
    ASSERT_EQ(CELL_ID(cdr(b2)), 1);

    PASS();
}

TEST(match_vector_length_mismatch)
{
    // Pattern #(x y) doesn't match input #(1)
    unsigned pattern = make_vector(2, 0);
    unsigned *pat_data = vector_data_ptr(pattern);
    pat_data[0] = atom("x");
    pat_data[1] = atom("y");

    unsigned input = make_vector(1, 0);
    unsigned *inp_data = vector_data_ptr(input);
    inp_data[0] = store(1);

    unsigned bindings = syntax_match(pattern, input, 0, 0, ctx.kw_ellipsis);
    ASSERT(bindings == TOK_ERROR);
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

// ============================================================================
// Main
// ============================================================================

int main(void)
{
    // Initialize interpreter infrastructure
    init_heap();
    init_keywords();

    printf("=== Macro Unit Tests ===\n");

    // Basic pattern matching
    RUN_TEST(match_empty_pattern);
    RUN_TEST(match_empty_pattern_fails_nonempty);
    RUN_TEST(match_atom_binds_value);
    RUN_TEST(match_literal_exact);
    RUN_TEST(match_literal_fails);
    RUN_TEST(match_underscore);

    // List pattern matching
    RUN_TEST(match_simple_list);
    RUN_TEST(match_nested_list);
    RUN_TEST(match_dotted_pair);
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

    TEST_SUMMARY("macros");
}
