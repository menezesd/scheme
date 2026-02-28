/**
 * @file test_pattern.c
 * @brief Unit tests for compiled pattern matching
 */

#define _POSIX_C_SOURCE 200809L
#include "compiled_pattern.h"
#include "context.h"
#include "test_framework.h"
#include "types.h"

// ============================================================================
// Helpers
// ============================================================================

static unsigned atom(const char *s) { return atom_from_string(s); }

// Create a list: (a b c) from varargs
static unsigned list2(unsigned a, unsigned b)
{
    gc_protect(&a);
    gc_protect(&b);
    unsigned result = alloc_cons(b, 0);
    result = alloc_cons(a, result);
    gc_unprotect(2);
    return result;
}

static unsigned list3(unsigned a, unsigned b, unsigned c)
{
    gc_protect(&a);
    gc_protect(&b);
    gc_protect(&c);
    unsigned result = alloc_cons(c, 0);
    result = alloc_cons(b, result);
    result = alloc_cons(a, result);
    gc_unprotect(3);
    return result;
}

// ============================================================================
// Compilation Tests
// ============================================================================

TEST(compile_single_var)
{
    // Pattern: x (single variable)
    unsigned pat = atom("x");
    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    ASSERT(cpat->var_count == 1);
    ASSERT(cpat->code[0].opcode == PAT_BIND_VAR);
    ASSERT(cpat->code[cpat->code_len - 1].opcode == PAT_SUCCESS);
    PASS();
}

TEST(compile_pair)
{
    // Pattern: (a b)
    unsigned pat = list2(atom("a"), atom("b"));
    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    ASSERT(cpat->var_count == 2);
    ASSERT(cpat->code[0].opcode == PAT_CHECK_PAIR);
    PASS();
}

TEST(compile_underscore)
{
    // Pattern: _
    unsigned pat = atom("_");
    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    ASSERT(cpat->var_count == 0);
    ASSERT(cpat->code[0].opcode == PAT_BIND_UNDERSCORE);
    PASS();
}

TEST(compile_null)
{
    // Pattern: () (empty list / null)
    unsigned pat = 0;
    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    ASSERT(cpat->code[0].opcode == PAT_CHECK_NULL);
    PASS();
}

TEST(compile_literal)
{
    // Pattern: (foo x) with foo as literal
    unsigned foo = atom("foo");
    unsigned x = atom("x");
    unsigned pat = list2(foo, x);
    unsigned literals = alloc_cons(foo, 0);
    compiled_pattern *cpat = compile_pattern(pat, literals, ctx.kw_ellipsis);

    ASSERT(cpat->var_count == 1);  // Only x is a variable

    // Should have MATCH_ATOM_ID for foo
    bool has_match = false;
    for (unsigned i = 0; i < cpat->code_len; i++) {
        if (cpat->code[i].opcode == PAT_MATCH_ATOM_ID)
            has_match = true;
    }
    ASSERT(has_match);
    PASS();
}

TEST(compile_ellipsis)
{
    // Pattern: (a ...)
    unsigned a = atom("a");
    unsigned ellipsis = atom("...");
    gc_protect(&a);
    gc_protect(&ellipsis);
    unsigned pat = alloc_cons(ellipsis, 0);
    pat = alloc_cons(a, pat);
    gc_unprotect(2);

    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    ASSERT(cpat->var_count == 1);
    ASSERT(cpat->var_slots[0].is_ellipsis);

    // Should have CHOICE_POINT for backtracking
    bool has_choice = false;
    for (unsigned i = 0; i < cpat->code_len; i++) {
        if (cpat->code[i].opcode == PAT_CHOICE_POINT)
            has_choice = true;
    }
    ASSERT(has_choice);
    PASS();
}

// ============================================================================
// Execution Tests
// ============================================================================

TEST(execute_single_var)
{
    // Pattern: x
    unsigned pat = atom("x");
    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    // Input: 42
    unsigned input = store(42);
    unsigned bindings = execute_pattern(cpat, input);

    ASSERT(bindings != TOK_ERROR);
    ASSERT(IS_PAIR(bindings));
    // Check the binding value is 42
    unsigned binding = car(bindings);
    ASSERT(IS_PAIR(binding));
    ASSERT(IS_NUM(cdr(binding)));
    ASSERT_EQ(CELL_ID(cdr(binding)), 42);
    PASS();
}

TEST(execute_pair)
{
    // Pattern: (a b)
    unsigned pat = list2(atom("a"), atom("b"));
    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    // Input: (1 2)
    unsigned input = list2(store(1), store(2));
    unsigned bindings = execute_pattern(cpat, input);

    ASSERT(bindings != TOK_ERROR);
    // Should have 2 bindings
    ASSERT(IS_PAIR(bindings));
    ASSERT(IS_PAIR(cdr(bindings)));
    PASS();
}

TEST(execute_pair_fail_extra)
{
    // Pattern: (a b)
    unsigned pat = list2(atom("a"), atom("b"));
    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    // Input: (1 2 3) - too many elements
    unsigned input = list3(store(1), store(2), store(3));
    unsigned bindings = execute_pattern(cpat, input);

    ASSERT(bindings == TOK_ERROR);
    PASS();
}

TEST(execute_pair_fail_type)
{
    // Pattern: (a b)
    unsigned pat = list2(atom("a"), atom("b"));
    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    // Input: 42 - not a pair
    unsigned input = store(42);
    unsigned bindings = execute_pattern(cpat, input);

    ASSERT(bindings == TOK_ERROR);
    PASS();
}

TEST(execute_null)
{
    // Pattern: ()
    unsigned pat = 0;
    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    // Input: () - should match
    unsigned bindings = execute_pattern(cpat, 0);
    ASSERT(bindings != TOK_ERROR);

    // Input: (1) - should fail
    unsigned bad = alloc_cons(store(1), 0);
    unsigned bad_bindings = execute_pattern(cpat, bad);
    ASSERT(bad_bindings == TOK_ERROR);
    PASS();
}

TEST(execute_literal)
{
    // Pattern: (foo x) with foo as literal
    unsigned foo = atom("foo");
    unsigned x = atom("x");
    unsigned pat = list2(foo, x);
    unsigned literals = alloc_cons(foo, 0);
    compiled_pattern *cpat = compile_pattern(pat, literals, ctx.kw_ellipsis);

    // Input: (foo 42) - should match
    unsigned input = list2(foo, store(42));
    unsigned bindings = execute_pattern(cpat, input);
    ASSERT(bindings != TOK_ERROR);

    // Input: (bar 42) - should fail (bar != foo)
    unsigned bar = atom("bar");
    unsigned bad_input = list2(bar, store(42));
    unsigned bad_bindings = execute_pattern(cpat, bad_input);
    ASSERT(bad_bindings == TOK_ERROR);
    PASS();
}

TEST(execute_ellipsis_simple)
{
    // Pattern: (a ...)
    unsigned a = atom("a");
    unsigned ellipsis = atom("...");
    gc_protect(&a);
    gc_protect(&ellipsis);
    unsigned pat = alloc_cons(ellipsis, 0);
    pat = alloc_cons(a, pat);
    gc_unprotect(2);

    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    // Input: (1 2 3)
    unsigned input = list3(store(1), store(2), store(3));
    unsigned bindings = execute_pattern(cpat, input);

    ASSERT(bindings != TOK_ERROR);
    // a should be bound to a list
    unsigned binding = car(bindings);
    unsigned value = cdr(binding);
    ASSERT(IS_PAIR(value) || value == 0);
    PASS();
}

TEST(execute_ellipsis_empty)
{
    // Pattern: (a ...)
    unsigned a = atom("a");
    unsigned ellipsis = atom("...");
    gc_protect(&a);
    gc_protect(&ellipsis);
    unsigned pat = alloc_cons(ellipsis, 0);
    pat = alloc_cons(a, pat);
    gc_unprotect(2);

    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    // Input: () - empty, should match with a = ()
    unsigned bindings = execute_pattern(cpat, 0);
    ASSERT(bindings != TOK_ERROR);
    PASS();
}

// ============================================================================
// Disassembly Test (visual inspection)
// ============================================================================

TEST(disassemble_ellipsis)
{
    // Pattern: (x y ...)
    unsigned x = atom("x");
    unsigned y = atom("y");
    unsigned ellipsis = atom("...");
    gc_protect(&x);
    gc_protect(&y);
    gc_protect(&ellipsis);

    // Build (x y ...)
    unsigned pat = alloc_cons(ellipsis, 0);
    pat = alloc_cons(y, pat);
    pat = alloc_cons(x, pat);
    gc_unprotect(3);

    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    printf("\n  Disassembly of (x y ...):\n");
    pattern_disassemble(cpat);
    PASS();
}

// ============================================================================
// Main
// ============================================================================

int main(void)
{
    printf("=== Pattern Compiler/Executor Tests ===\n\n");

    init_heap();
    init_keywords();  // Initialize ctx.kw_* values

    // Compilation tests
    printf("Compilation:\n");
    RUN_TEST(compile_single_var);
    RUN_TEST(compile_pair);
    RUN_TEST(compile_underscore);
    RUN_TEST(compile_null);
    RUN_TEST(compile_literal);
    RUN_TEST(compile_ellipsis);

    // Execution tests
    printf("\nExecution:\n");
    RUN_TEST(execute_single_var);
    RUN_TEST(execute_pair);
    RUN_TEST(execute_pair_fail_extra);
    RUN_TEST(execute_pair_fail_type);
    RUN_TEST(execute_null);
    RUN_TEST(execute_literal);
    RUN_TEST(execute_ellipsis_simple);
    RUN_TEST(execute_ellipsis_empty);

    // Visual inspection
    printf("\nVisual Inspection:\n");
    RUN_TEST(disassemble_ellipsis);

    TEST_SUMMARY("Pattern");
}
