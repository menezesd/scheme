// Unit tests for context and env modules
#define _POSIX_C_SOURCE 200809L
#include "context.h"
#include "env.h"
#include "test_framework.h"
#include "types.h"

// ============================================================================
// Context Tests - Allocation
// ============================================================================

TEST(alloc_returns_valid_cell)
{
    unsigned x = alloc();
    ASSERT(x > 0);
    ASSERT(x < SEMISPACE_SIZE);
    PASS();
}

TEST(alloc_cells_are_distinct)
{
    unsigned x = alloc();
    unsigned y = alloc();
    unsigned z = alloc();
    ASSERT(x != y);
    ASSERT(y != z);
    ASSERT(x != z);
    PASS();
}

TEST(alloc_cons_creates_pair)
{
    unsigned x = alloc_cons(store(1), store(2));
    ASSERT(CELL_TYPE(x) == BT_CONS);
    ASSERT_EQ(CELL_ID(car(x)), 1);
    ASSERT_EQ(CELL_ID(cdr(x)), 2);
    PASS();
}

// ============================================================================
// Context Tests - Numeric Storage
// ============================================================================

TEST(store_positive)
{
    unsigned x = store(42);
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

TEST(store_negative)
{
    unsigned x = store(-123);
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), -123);
    PASS();
}

TEST(store_zero)
{
    unsigned x = store(0);
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 0);
    PASS();
}

TEST(store_inexact)
{
    unsigned x = store_inexact(3.14159);
    ASSERT(CELL_TYPE(x) == BT_INEXACT);
    double val = to_double(x);
    ASSERT(val > 3.14 && val < 3.15);
    PASS();
}

TEST(store_rational)
{
    unsigned x = store_rational(1, 2);
    ASSERT(CELL_TYPE(x) == BT_RATIONAL);
    // Rational stores num in car(x), denom in cdr(x) - both are cells
    ASSERT_EQ(CELL_ID(car(x)), 1);
    ASSERT_EQ(CELL_ID(cdr(x)), 2);
    PASS();
}

TEST(store_rational_normalized)
{
    // 4/6 should normalize to 2/3
    unsigned x = normalize_rational(4, 6);
    ASSERT(CELL_TYPE(x) == BT_RATIONAL);
    ASSERT_EQ(CELL_ID(car(x)), 2);
    ASSERT_EQ(CELL_ID(cdr(x)), 3);
    PASS();
}

TEST(store_rational_to_integer)
{
    // 6/3 should normalize to integer 2
    unsigned x = normalize_rational(6, 3);
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 2);
    PASS();
}

TEST(store_bignum_test)
{
    bignum *bn = bn_from_string("12345678901234567890", 10);
    unsigned x = store_bignum(bn);
    ASSERT(CELL_TYPE(x) == BT_BIGNUM);
    bignum *retrieved = get_bignum(x);
    ASSERT(retrieved != NULL);
    ASSERT_EQ(bn_cmp(retrieved, bn), 0);
    PASS();
}

// ============================================================================
// Context Tests - Interning
// ============================================================================

TEST(intern_same_string)
{
    int a = intern("hello");
    int b = intern("hello");
    ASSERT_EQ(a, b);
    PASS();
}

TEST(intern_different_strings)
{
    int a = intern("foo");
    int b = intern("bar");
    ASSERT(a != b);
    PASS();
}

TEST(atom_from_string_creates_atom)
{
    unsigned x = atom_from_string("test-symbol");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "test-symbol");
    PASS();
}

TEST(atom_from_string_parses_number)
{
    unsigned x = atom_from_string("42");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

// ============================================================================
// Context Tests - Characters
// ============================================================================

TEST(make_char_test)
{
    unsigned x = make_char('A');
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), 'A');
    PASS();
}

TEST(make_char_special)
{
    unsigned x = make_char('\n');
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), '\n');
    PASS();
}

// ============================================================================
// Context Tests - Vectors
// ============================================================================

TEST(make_vector_empty)
{
    unsigned v = make_vector(0, 0);
    ASSERT(CELL_TYPE(v) == BT_VECTOR);
    ASSERT_EQ(vector_len(v), 0);
    PASS();
}

TEST(make_vector_with_fill)
{
    unsigned fill = store(42);
    unsigned v = make_vector(5, fill);
    ASSERT(CELL_TYPE(v) == BT_VECTOR);
    ASSERT_EQ(vector_len(v), 5);
    unsigned *data = vector_data_ptr(v);
    for (int i = 0; i < 5; i++) {
        ASSERT_EQ(CELL_ID(data[i]), 42);
    }
    PASS();
}

TEST(vector_data_access)
{
    unsigned v = make_vector(3, 0);
    unsigned *data = vector_data_ptr(v);
    data[0] = store(10);
    data[1] = store(20);
    data[2] = store(30);
    ASSERT_EQ(CELL_ID(data[0]), 10);
    ASSERT_EQ(CELL_ID(data[1]), 20);
    ASSERT_EQ(CELL_ID(data[2]), 30);
    PASS();
}

// ============================================================================
// Context Tests - List Utilities
// ============================================================================

TEST(list_length_empty)
{
    ASSERT_EQ(list_length(0), 0);
    PASS();
}

TEST(list_length_three)
{
    unsigned lst = alloc_cons(store(1), alloc_cons(store(2), alloc_cons(store(3), 0)));
    ASSERT_EQ(list_length(lst), 3);
    PASS();
}

TEST(list_append_builds_list)
{
    unsigned head = 0, tail = 0;
    list_append(&head, &tail, store(1));
    list_append(&head, &tail, store(2));
    list_append(&head, &tail, store(3));
    ASSERT_EQ(list_length(head), 3);
    ASSERT_EQ(CELL_ID(car(head)), 1);
    ASSERT_EQ(CELL_ID(cadr(head)), 2);
    ASSERT_EQ(CELL_ID(caddr(head)), 3);
    PASS();
}

// ============================================================================
// Context Tests - Deep Equality
// ============================================================================

TEST(deep_equal_numbers)
{
    ASSERT(deep_equal(store(42), store(42)));
    ASSERT(!deep_equal(store(42), store(43)));
    PASS();
}

TEST(deep_equal_atoms)
{
    unsigned a = atom_from_string("foo");
    unsigned b = atom_from_string("foo");
    unsigned c = atom_from_string("bar");
    ASSERT(deep_equal(a, b));
    ASSERT(!deep_equal(a, c));
    PASS();
}

TEST(deep_equal_lists)
{
    unsigned l1 = alloc_cons(store(1), alloc_cons(store(2), 0));
    unsigned l2 = alloc_cons(store(1), alloc_cons(store(2), 0));
    unsigned l3 = alloc_cons(store(1), alloc_cons(store(3), 0));
    ASSERT(deep_equal(l1, l2));
    ASSERT(!deep_equal(l1, l3));
    PASS();
}

TEST(deep_equal_nested)
{
    unsigned inner1 = alloc_cons(store(1), alloc_cons(store(2), 0));
    unsigned inner2 = alloc_cons(store(1), alloc_cons(store(2), 0));
    unsigned outer1 = alloc_cons(inner1, 0);
    unsigned outer2 = alloc_cons(inner2, 0);
    ASSERT(deep_equal(outer1, outer2));
    PASS();
}

// ============================================================================
// Context Tests - Continuations
// ============================================================================

TEST(make_cont_test)
{
    unsigned k = make_cont(CONT_IF, store(42), 0, 0);
    ASSERT_EQ(cont_type(k), CONT_IF);
    ASSERT_EQ(CELL_ID(cont_data(k)), 42);
    PASS();
}

TEST(make_halt_cont_test)
{
    unsigned k = make_halt_cont();
    ASSERT_EQ(cont_type(k), CONT_HALT);
    PASS();
}

// ============================================================================
// Env Tests
// ============================================================================

TEST(empty_environment_test)
{
    unsigned env = empty_environment();
    ASSERT(env != 0);
    ASSERT(CELL_TYPE(env) == BT_CONS);
    PASS();
}

TEST(defvar_and_lookup)
{
    unsigned env = empty_environment();
    unsigned var = atom_from_string("x");
    unsigned val = store(42);
    defvar(var, val, env);

    unsigned result = lookup(CELL_ID(var), env);
    ASSERT_EQ(CELL_ID(result), 42);
    PASS();
}

TEST(defvar_multiple)
{
    unsigned env = empty_environment();
    unsigned x = atom_from_string("x");
    unsigned y = atom_from_string("y");
    defvar(x, store(1), env);
    defvar(y, store(2), env);

    ASSERT_EQ(CELL_ID(lookup(CELL_ID(x), env)), 1);
    ASSERT_EQ(CELL_ID(lookup(CELL_ID(y), env)), 2);
    PASS();
}

TEST(defvar_overwrites)
{
    unsigned env = empty_environment();
    unsigned x = atom_from_string("x");
    defvar(x, store(1), env);
    defvar(x, store(2), env);

    ASSERT_EQ(CELL_ID(lookup(CELL_ID(x), env)), 2);
    PASS();
}

TEST(setvar_updates)
{
    unsigned env = empty_environment();
    unsigned x = atom_from_string("x");
    defvar(x, store(1), env);
    setvar(CELL_ID(x), store(99), env);

    ASSERT_EQ(CELL_ID(lookup(CELL_ID(x), env)), 99);
    PASS();
}

TEST(bind_params_simple)
{
    unsigned params = alloc_cons(atom_from_string("a"),
                     alloc_cons(atom_from_string("b"), 0));
    unsigned args = alloc_cons(store(1), alloc_cons(store(2), 0));

    unsigned frame = bind_params(params, args);
    ASSERT(frame != 0);
    // Frame is (vars . vals)
    unsigned vars = car(frame);
    unsigned vals = cdr(frame);
    ASSERT_EQ(list_length(vars), 2);
    ASSERT_EQ(list_length(vals), 2);
    PASS();
}

TEST(mk_primop_test)
{
    unsigned p = mk_primop(PPLUS);
    ASSERT(CELL_TYPE(p) == BT_BUILTIN);
    ASSERT_EQ(CELL_ID(p), PPLUS);
    PASS();
}

TEST(default_environment_has_primitives)
{
    unsigned env = default_environment();

    // Look up '+' primitive
    unsigned plus = atom_from_string("+");
    unsigned val = lookup(CELL_ID(plus), env);
    ASSERT(val != TOK_ERROR);
    ASSERT(CELL_TYPE(val) == BT_BUILTIN);
    ASSERT_EQ(CELL_ID(val), PPLUS);
    PASS();
}

// ============================================================================
// Context Tests - GC
// ============================================================================

TEST(gc_preserves_root)
{
    unsigned root = alloc_cons(store(42), 0);
    unsigned preserved = gc(root);
    ASSERT(preserved != 0);
    ASSERT_EQ(CELL_ID(car(preserved)), 42);
    PASS();
}

TEST(gc_preserves_list)
{
    unsigned lst = alloc_cons(store(1),
                  alloc_cons(store(2),
                  alloc_cons(store(3), 0)));
    unsigned preserved = gc(lst);
    ASSERT_EQ(list_length(preserved), 3);
    ASSERT_EQ(CELL_ID(car(preserved)), 1);
    ASSERT_EQ(CELL_ID(cadr(preserved)), 2);
    ASSERT_EQ(CELL_ID(caddr(preserved)), 3);
    PASS();
}

TEST(gc_preserves_vector)
{
    unsigned v = make_vector(3, store(42));
    unsigned root = alloc_cons(v, 0);
    unsigned preserved = gc(root);
    unsigned vec = car(preserved);
    ASSERT(CELL_TYPE(vec) == BT_VECTOR);
    ASSERT_EQ(vector_len(vec), 3);
    PASS();
}

TEST(gc_stress_many_allocations)
{
    // Allocate a long list of numbers
    unsigned lst = 0;
    for (int i = 0; i < 1000; i++) {
        lst = alloc_cons(store(i), lst);
    }
    // GC should preserve all of them
    lst = gc(lst);
    ASSERT_EQ(list_length(lst), 1000);
    // Verify first and last values
    ASSERT_EQ(CELL_ID(car(lst)), 999);
    for (int i = 0; i < 999; i++) {
        lst = cdr(lst);
    }
    ASSERT_EQ(CELL_ID(car(lst)), 0);
    PASS();
}

TEST(gc_stress_deep_nesting)
{
    // Create deeply nested structure: ((((1))))
    unsigned x = store(42);
    for (int i = 0; i < 100; i++) {
        x = alloc_cons(x, 0);
    }
    x = gc(x);
    // Navigate down and verify
    for (int i = 0; i < 100; i++) {
        ASSERT(CELL_TYPE(x) == BT_CONS);
        x = car(x);
    }
    ASSERT_EQ(CELL_TYPE(x), BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

TEST(gc_stress_multiple_collections)
{
    unsigned root = alloc_cons(store(1), alloc_cons(store(2), 0));
    // Run GC multiple times
    for (int i = 0; i < 10; i++) {
        root = gc(root);
        ASSERT_EQ(list_length(root), 2);
        ASSERT_EQ(CELL_ID(car(root)), 1);
        ASSERT_EQ(CELL_ID(cadr(root)), 2);
    }
    PASS();
}

TEST(gc_preserves_permanent_atoms)
{
    // Permanent atoms should have same cell IDs after GC
    unsigned root = alloc_cons(ctx.atom_true, 0);
    unsigned before_true = ctx.atom_true;
    unsigned before_quote = ctx.atom_quote;
    root = gc(root);
    ASSERT_EQ(ctx.atom_true, before_true);
    ASSERT_EQ(ctx.atom_quote, before_quote);
    ASSERT_EQ(ctx.atom_true, CELL_ATOM_TRUE);
    PASS();
}

TEST(gc_heap_usage_percent)
{
    // heap_usage_percent should return a valid percentage
    int usage = heap_usage_percent();
    ASSERT(usage >= 0 && usage <= 100);
    // After GC with minimal data, usage should decrease
    unsigned root = alloc_cons(store(1), 0);
    root = gc(root);
    int usage_after = heap_usage_percent();
    ASSERT(usage_after >= 0 && usage_after <= 100);
    PASS();
}

TEST(gc_maybe_gc_threshold)
{
    unsigned root = alloc_cons(store(1), 0);
    // With low threshold, should trigger GC
    root = maybe_gc(root, 0);  // 0% threshold = always GC
    ASSERT(root != 0);
    ASSERT_EQ(CELL_ID(car(root)), 1);
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

    printf("=== Context & Env Unit Tests ===\n");

    // Allocation
    RUN_TEST(alloc_returns_valid_cell);
    RUN_TEST(alloc_cells_are_distinct);
    RUN_TEST(alloc_cons_creates_pair);

    // Numeric storage
    RUN_TEST(store_positive);
    RUN_TEST(store_negative);
    RUN_TEST(store_zero);
    RUN_TEST(store_inexact);
    RUN_TEST(store_rational);
    RUN_TEST(store_rational_normalized);
    RUN_TEST(store_rational_to_integer);
    RUN_TEST(store_bignum_test);

    // Interning
    RUN_TEST(intern_same_string);
    RUN_TEST(intern_different_strings);
    RUN_TEST(atom_from_string_creates_atom);
    RUN_TEST(atom_from_string_parses_number);

    // Characters
    RUN_TEST(make_char_test);
    RUN_TEST(make_char_special);

    // Vectors
    RUN_TEST(make_vector_empty);
    RUN_TEST(make_vector_with_fill);
    RUN_TEST(vector_data_access);

    // List utilities
    RUN_TEST(list_length_empty);
    RUN_TEST(list_length_three);
    RUN_TEST(list_append_builds_list);

    // Deep equality
    RUN_TEST(deep_equal_numbers);
    RUN_TEST(deep_equal_atoms);
    RUN_TEST(deep_equal_lists);
    RUN_TEST(deep_equal_nested);

    // Continuations
    RUN_TEST(make_cont_test);
    RUN_TEST(make_halt_cont_test);

    // Environment
    RUN_TEST(empty_environment_test);
    RUN_TEST(defvar_and_lookup);
    RUN_TEST(defvar_multiple);
    RUN_TEST(defvar_overwrites);
    RUN_TEST(setvar_updates);
    RUN_TEST(bind_params_simple);
    RUN_TEST(mk_primop_test);
    RUN_TEST(default_environment_has_primitives);

    // GC
    RUN_TEST(gc_preserves_root);
    RUN_TEST(gc_preserves_list);
    RUN_TEST(gc_preserves_vector);
    RUN_TEST(gc_stress_many_allocations);
    RUN_TEST(gc_stress_deep_nesting);
    RUN_TEST(gc_stress_multiple_collections);
    RUN_TEST(gc_preserves_permanent_atoms);
    RUN_TEST(gc_heap_usage_percent);
    RUN_TEST(gc_maybe_gc_threshold);

    TEST_SUMMARY("context & env");
}
