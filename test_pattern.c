/**
 * @file test_pattern.c
 * @brief Unit tests for compiled pattern matching
 */

#define _POSIX_C_SOURCE 200809L
#include "compiled_pattern.h"
#include "context.h"
#include "test_framework.h"
#include "types.h"
#include "writer.h"
#include <stdlib.h>
#include <string.h>

// ============================================================================
// Helpers
// ============================================================================

static unsigned atom(const char *s)
{
    return atom_from_string(s);
}

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

static unsigned binding_value(unsigned bindings, const char *name)
{
    int64_t id = intern(name);
    for (unsigned p = bindings; p; p = cdr(p)) {
        unsigned binding = car(p);
        if (CELL_ID(car(binding)) == id)
            return cdr(binding);
    }
    return TOK_ERROR;
}

static bool pattern_registry_contains(compiled_pattern *needle)
{
    for (compiled_pattern *p = compiled_pattern_registry; p; p = p->gc_next) {
        if (p == needle)
            return true;
    }
    return false;
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

TEST(compiled_pattern_free_unregisters)
{
    compiled_pattern *cpat = compiled_pattern_new();
    ASSERT(cpat != NULL);
    compiled_pattern_free(cpat);

    ASSERT(!pattern_registry_contains(cpat));
    PASS();
}

TEST(compiled_pattern_free_is_idempotent)
{
    compiled_pattern *cpat = compiled_pattern_new();
    ASSERT(cpat != NULL);
    compiled_pattern_free(cpat);
    compiled_pattern_free(cpat);
    PASS();
}

TEST(pattern_register_is_idempotent)
{
    compiled_pattern *cpat = compiled_pattern_new();
    ASSERT(cpat != NULL);
    pattern_register(cpat);
    compiled_pattern_free(cpat);
    ASSERT(!pattern_registry_contains(cpat));
    PASS();
}

TEST(gc_sweep_patterns_preserves_heap_references)
{
    unsigned pat = atom("x");
    compiled_pattern *kept = compile_pattern(pat, 0, ctx.kw_ellipsis);
    compiled_pattern *swept = compile_pattern(pat, 0, ctx.kw_ellipsis);
    ASSERT(kept != NULL);
    ASSERT(swept != NULL);

    unsigned cell = alloc();
    CELL_TYPE(cell) = BT_COMPILED_PATTERN;
    CELL_PTR(cell) = kept;

    gc_sweep_patterns();
    ASSERT(pattern_registry_contains(kept));
    ASSERT(!pattern_registry_contains(swept));

    CELL_TYPE(cell) = BT_FREE;
    CELL_PTR(cell) = NULL;
    gc_sweep_patterns();
    ASSERT(!pattern_registry_contains(kept));
    PASS();
}

TEST(writer_handles_compiled_pattern_cell)
{
    compiled_pattern *cpat = compiled_pattern_new();
    ASSERT(cpat != NULL);

    unsigned cell = alloc();
    CELL_TYPE(cell) = BT_COMPILED_PATTERN;
    CELL_PTR(cell) = cpat;

    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(cell, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "[compiled-pattern]");
    ASSERT_STR_EQ(type_name(cell), "compiled-pattern");
    free(buf);

    CELL_TYPE(cell) = BT_FREE;
    CELL_PTR(cell) = NULL;
    compiled_pattern_free(cpat);
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

TEST(execute_ellipsis_with_tail_binding)
{
    // Pattern: (x ... y), input: (1 2 3)
    unsigned x = atom("x");
    unsigned y = atom("y");
    unsigned ellipsis = atom("...");
    gc_protect(&x);
    gc_protect(&y);
    gc_protect(&ellipsis);
    unsigned pat = alloc_cons(y, 0);
    pat = alloc_cons(ellipsis, pat);
    pat = alloc_cons(x, pat);
    gc_unprotect(3);

    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);
    ASSERT(cpat->var_count == 2);
    // Greedy compilation emits the element pattern before the tail
    ASSERT(cpat->var_slots[0].is_ellipsis);  // x is repeated
    ASSERT(!cpat->var_slots[1].is_ellipsis); // y is post-ellipsis tail

    unsigned input = list3(store(1), store(2), store(3));
    unsigned bindings = execute_pattern(cpat, input);
    ASSERT(bindings != TOK_ERROR);

    unsigned y_value = binding_value(bindings, "y");
    ASSERT(IS_NUM(y_value));
    ASSERT_EQ(CELL_ID(y_value), 3);

    unsigned x_values = binding_value(bindings, "x");
    ASSERT(IS_PAIR(x_values));
    ASSERT_EQ(CELL_ID(car(x_values)), 1);
    ASSERT_EQ(CELL_ID(cadr(x_values)), 2);
    ASSERT(cddr(x_values) == 0);
    PASS();
}

// ============================================================================
// Vector Tests
// ============================================================================

TEST(execute_vector_simple)
{
    // Pattern: #(x y)
    unsigned pat = make_vector(2, 0);
    unsigned *data = vector_data_ptr(pat);
    data[0] = atom("x");
    data[1] = atom("y");

    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    // Input: #(1 2)
    unsigned input = make_vector(2, 0);
    unsigned *inp_data = vector_data_ptr(input);
    inp_data[0] = store(1);
    inp_data[1] = store(2);

    unsigned bindings = execute_pattern(cpat, input);
    ASSERT(bindings != TOK_ERROR);

    // Check bindings
    unsigned b1 = car(bindings);
    ASSERT(CELL_ID(cdr(b1)) == 2);  // y = 2
    unsigned b2 = car(cdr(bindings));
    ASSERT(CELL_ID(cdr(b2)) == 1);  // x = 1
    PASS();
}

TEST(execute_vector_length_mismatch)
{
    // Pattern: #(x y)
    unsigned pat = make_vector(2, 0);
    unsigned *data = vector_data_ptr(pat);
    data[0] = atom("x");
    data[1] = atom("y");

    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    // Input: #(1 2 3) - too many elements
    unsigned input = make_vector(3, 0);
    unsigned *inp_data = vector_data_ptr(input);
    inp_data[0] = store(1);
    inp_data[1] = store(2);
    inp_data[2] = store(3);

    unsigned bindings = execute_pattern(cpat, input);
    ASSERT(bindings == TOK_ERROR);
    PASS();
}

TEST(execute_vector_ellipsis)
{
    // Pattern: #(x ...)
    unsigned pat = make_vector(2, 0);
    unsigned *data = vector_data_ptr(pat);
    data[0] = atom("x");
    data[1] = atom("...");

    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    // Input: #(1 2 3)
    unsigned input = make_vector(3, 0);
    unsigned *inp_data = vector_data_ptr(input);
    inp_data[0] = store(1);
    inp_data[1] = store(2);
    inp_data[2] = store(3);

    unsigned bindings = execute_pattern(cpat, input);
    ASSERT(bindings != TOK_ERROR);

    // x should be bound to (1 2 3)
    unsigned binding = car(bindings);
    unsigned value = cdr(binding);
    ASSERT(IS_PAIR(value));
    PASS();
}

TEST(execute_vector_ellipsis_empty)
{
    // Pattern: #(x ...)
    unsigned pat = make_vector(2, 0);
    unsigned *data = vector_data_ptr(pat);
    data[0] = atom("x");
    data[1] = atom("...");

    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    // Input: #() - empty vector
    unsigned input = make_vector(0, 0);

    unsigned bindings = execute_pattern(cpat, input);
    ASSERT(bindings != TOK_ERROR);

    // x should be bound to ()
    unsigned binding = car(bindings);
    unsigned value = cdr(binding);
    ASSERT(value == 0);  // Empty list
    PASS();
}

TEST(execute_vector_ellipsis_with_pre_post)
{
    // Pattern: #(first x ... last)
    unsigned pat = make_vector(4, 0);
    unsigned *data = vector_data_ptr(pat);
    data[0] = atom("first");
    data[1] = atom("x");
    data[2] = atom("...");
    data[3] = atom("last");

    compiled_pattern *cpat = compile_pattern(pat, 0, ctx.kw_ellipsis);

    // Input: #(1 2 3 4 5)
    unsigned input = make_vector(5, 0);
    unsigned *inp_data = vector_data_ptr(input);
    inp_data[0] = store(1);
    inp_data[1] = store(2);
    inp_data[2] = store(3);
    inp_data[3] = store(4);
    inp_data[4] = store(5);

    unsigned bindings = execute_pattern(cpat, input);
    ASSERT(bindings != TOK_ERROR);

    // Should have bindings for first, x, last
    // Order depends on implementation - just check we got them
    int found_first = 0, found_last = 0, found_x = 0;
    for (unsigned b = bindings; b; b = cdr(b)) {
        unsigned binding = car(b);
        unsigned name = car(binding);
        if (IS_ATOM(name)) {
            const char *s = ctx.atom_table[CELL_ID(name)];
            if (strcmp(s, "first") == 0) found_first = 1;
            if (strcmp(s, "last") == 0) found_last = 1;
            if (strcmp(s, "x") == 0) found_x = 1;
        }
    }
    ASSERT(found_first && found_last && found_x);
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
    RUN_TEST(compiled_pattern_free_unregisters);
    RUN_TEST(compiled_pattern_free_is_idempotent);
    RUN_TEST(pattern_register_is_idempotent);
    RUN_TEST(gc_sweep_patterns_preserves_heap_references);
    RUN_TEST(writer_handles_compiled_pattern_cell);

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
    RUN_TEST(execute_ellipsis_with_tail_binding);

    // Vector tests
    printf("\nVector:\n");
    RUN_TEST(execute_vector_simple);
    RUN_TEST(execute_vector_length_mismatch);
    RUN_TEST(execute_vector_ellipsis);
    RUN_TEST(execute_vector_ellipsis_empty);
    RUN_TEST(execute_vector_ellipsis_with_pre_post);

    // Visual inspection
    printf("\nVisual Inspection:\n");
    RUN_TEST(disassemble_ellipsis);

    TEST_SUMMARY("Pattern");
}
