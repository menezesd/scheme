/**
 * @file test_eval.c
 * @brief Unit tests for the evaluator and GC
 */

#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L
#include "context.h"
#include "env.h"
#include "eval.h"
#include "macros.h"
#include "reader.h"
#include "test_framework.h"
#include "types.h"
#include "writer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declarations
void init_heap(void);
void init_keywords(void);
void set_alloc_gc_root(unsigned *root);

// Helper: evaluate a string and return the result
static unsigned eval_string(const char *src, unsigned env)
{
    // Set up input from string
    FILE *old_stdin = stdin;
    FILE *f = fmemopen((void *)src, strlen(src), "r");
    if (!f) {
        fprintf(stderr, "fmemopen failed\n");
        return TOK_ERROR;
    }
    stdin = f;
    reader_reset_labels();
    unsigned expr = read_obj();
    fclose(f);
    stdin = old_stdin;

    if (expr == TOK_ERROR)
        return TOK_ERROR;
    return eval_obj(expr, env);
}

// Helper: evaluate with env pointer (for GC tests where env may be updated)
static unsigned eval_string_gc(const char *src, unsigned *env_ptr)
{
    FILE *old_stdin = stdin;
    FILE *f = fmemopen((void *)src, strlen(src), "r");
    if (!f) {
        fprintf(stderr, "fmemopen failed\n");
        return TOK_ERROR;
    }
    stdin = f;
    reader_reset_labels();
    unsigned expr = read_obj();
    fclose(f);
    stdin = old_stdin;

    if (expr == TOK_ERROR)
        return TOK_ERROR;
    set_alloc_gc_root(env_ptr);
    unsigned result = eval_obj(expr, *env_ptr);
    return result;
}

// Helper: check if result is an integer with given value
static int is_int(unsigned x, int64_t val)
{
    if (CELL_TYPE(x) != BT_NUM)
        return 0;
    return CELL_ID(x) == val;
}

// Helper: check if result is a boolean with given value
static int is_bool(unsigned x, int val)
{
    if (val)
        return x == ctx.atom_true;
    else
        return x == 0; // #f is represented as 0 (nil)
}

// ============================================================================
// Basic Evaluation Tests
// ============================================================================

TEST(eval_integer)
{
    unsigned env = default_environment();
    unsigned result = eval_string("42", env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(eval_negative_integer)
{
    unsigned env = default_environment();
    unsigned result = eval_string("-17", env);
    ASSERT(is_int(result, -17));
    PASS();
}

TEST(eval_true)
{
    unsigned env = default_environment();
    unsigned result = eval_string("#t", env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_false)
{
    unsigned env = default_environment();
    unsigned result = eval_string("#f", env);
    ASSERT(result == 0);
    PASS();
}

TEST(eval_quote)
{
    unsigned env = default_environment();
    unsigned result = eval_string("'(1 2 3)", env);
    ASSERT(IS_PAIR(result));
    ASSERT(is_int(car(result), 1));
    PASS();
}

// ============================================================================
// Arithmetic Tests
// ============================================================================

TEST(eval_add)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(+ 1 2 3)", env);
    ASSERT(is_int(result, 6));
    PASS();
}

TEST(eval_subtract)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(- 10 3 2)", env);
    ASSERT(is_int(result, 5));
    PASS();
}

TEST(eval_multiply)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(* 2 3 4)", env);
    ASSERT(is_int(result, 24));
    PASS();
}

TEST(eval_divide)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(/ 20 4)", env);
    ASSERT(is_int(result, 5));
    PASS();
}

// ============================================================================
// Comparison Tests
// ============================================================================

TEST(eval_eq_true)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(= 5 5)", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_eq_false)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(= 5 6)", env);
    ASSERT(is_bool(result, 0));
    PASS();
}

TEST(eval_lt_true)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(< 3 5)", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_lt_false)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(< 5 3)", env);
    ASSERT(is_bool(result, 0));
    PASS();
}

// ============================================================================
// If/Cond Tests
// ============================================================================

TEST(eval_if_true)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(if #t 1 2)", env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(eval_if_false)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(if #f 1 2)", env);
    ASSERT(is_int(result, 2));
    PASS();
}

TEST(eval_cond_first)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(cond (#t 42) (#f 99))", env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(eval_cond_else)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(cond (#f 1) (else 2))", env);
    ASSERT(is_int(result, 2));
    PASS();
}

// ============================================================================
// Lambda Tests
// ============================================================================

TEST(eval_lambda_call)
{
    unsigned env = default_environment();
    unsigned result = eval_string("((lambda (x) (+ x 1)) 5)", env);
    ASSERT(is_int(result, 6));
    PASS();
}

TEST(eval_lambda_closure)
{
    unsigned env = default_environment();
    unsigned result =
        eval_string("(((lambda (x) (lambda (y) (+ x y))) 10) 5)", env);
    ASSERT(is_int(result, 15));
    PASS();
}

TEST(eval_lambda_rest_param)
{
    unsigned env = default_environment();
    unsigned result =
        eval_string("((lambda (x . rest) (length rest)) 1 2 3 4)", env);
    ASSERT(is_int(result, 3));
    PASS();
}

// ============================================================================
// Define Tests
// ============================================================================

TEST(eval_define_variable)
{
    unsigned env = default_environment();
    eval_string("(define x 42)", env);
    unsigned result = eval_string("x", env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(eval_define_function)
{
    unsigned env = default_environment();
    eval_string("(define (square x) (* x x))", env);
    unsigned result = eval_string("(square 7)", env);
    ASSERT(is_int(result, 49));
    PASS();
}

TEST(eval_define_recursive)
{
    unsigned env = default_environment();
    eval_string("(define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))", env);
    unsigned result = eval_string("(fact 5)", env);
    ASSERT(is_int(result, 120));
    PASS();
}

// ============================================================================
// Let Tests
// ============================================================================

TEST(eval_let_simple)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(let ((x 1) (y 2)) (+ x y))", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(eval_let_nested)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(let ((x 1)) (let ((y 2)) (+ x y)))", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(eval_letstar)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(let* ((x 1) (y (+ x 1))) (+ x y))", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(eval_letrec)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) "
        "         (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) "
        "  (even? 10))",
        env);
    ASSERT(is_bool(result, 1));
    PASS();
}

// ============================================================================
// And/Or Tests
// ============================================================================

TEST(eval_and_all_true)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(and #t #t #t)", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_and_one_false)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(and #t #f #t)", env);
    ASSERT(is_bool(result, 0));
    PASS();
}

TEST(eval_or_all_false)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(or #f #f #f)", env);
    ASSERT(is_bool(result, 0));
    PASS();
}

TEST(eval_or_one_true)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(or #f #t #f)", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

// ============================================================================
// Begin Tests
// ============================================================================

TEST(eval_begin_sequence)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(begin 1 2 3)", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(eval_begin_side_effects)
{
    unsigned env = default_environment();
    eval_string("(define x 0)", env);
    unsigned result = eval_string("(begin (set! x 1) (set! x 2) x)", env);
    ASSERT(is_int(result, 2));
    PASS();
}

// ============================================================================
// Call/cc Tests
// ============================================================================

TEST(eval_callcc_simple)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(call/cc (lambda (k) (k 42)))", env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(eval_callcc_escape)
{
    unsigned env = default_environment();
    unsigned result =
        eval_string("(+ 1 (call/cc (lambda (k) (+ 10 (k 5)))))", env);
    ASSERT(is_int(result, 6));
    PASS();
}

// ============================================================================
// GC Protection Tests
// ============================================================================

TEST(gc_shadow_stack_balanced)
{
    unsigned env = default_environment();
    int initial = get_shadow_stack_top();

    // Evaluate some expressions that use gc_protect/gc_unprotect
    eval_string("(define (f x) (+ x 1))", env);
    eval_string("(f 5)", env);
    eval_string("(let ((a 1) (b 2)) (+ a b))", env);
    eval_string("((lambda (x y) (+ x y)) 3 4)", env);

    int final = get_shadow_stack_top();
    ASSERT_EQ(initial, final);
    PASS();
}

TEST(gc_shadow_stack_lambda)
{
    unsigned env = default_environment();
    int initial = get_shadow_stack_top();

    eval_string("(lambda (x) x)", env);

    int final = get_shadow_stack_top();
    ASSERT_EQ(initial, final);
    PASS();
}

TEST(gc_shadow_stack_letrec)
{
    unsigned env = default_environment();
    int initial = get_shadow_stack_top();

    eval_string(
        "(letrec ((f (lambda (n) (if (< n 1) 1 (* n (f (- n 1))))))) (f 5))",
        env);

    int final = get_shadow_stack_top();
    ASSERT_EQ(initial, final);
    PASS();
}

TEST(gc_preserves_closures)
{
    unsigned env = default_environment();

    // Create a closure (using gc-aware eval)
    eval_string_gc("(define add-n (lambda (n) (lambda (x) (+ n x))))", &env);
    eval_string_gc("(define add-5 (add-n 5))", &env);

    // Force GC
    eval_string_gc("(gc-flip)", &env);

    // Closure should still work
    unsigned result = eval_string_gc("(add-5 10)", &env);
    ASSERT(is_int(result, 15));
    PASS();
}

TEST(gc_preserves_continuations)
{
    unsigned env = default_environment();

    // Store a continuation (using gc-aware eval)
    eval_string_gc("(define saved #f)", &env);
    eval_string_gc("(+ 1 (call/cc (lambda (k) (set! saved k) 0)))", &env);

    // Force GC
    eval_string_gc("(gc-flip)", &env);

    // Continuation should still work
    unsigned result = eval_string_gc("(saved 10)", &env);
    ASSERT(is_int(result, 11));
    PASS();
}

// ============================================================================
// Apply Tests
// ============================================================================

TEST(eval_apply_simple)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(apply + '(1 2 3))", env);
    ASSERT(is_int(result, 6));
    PASS();
}

TEST(eval_apply_lambda)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(apply (lambda (x y) (+ x y)) '(3 4))", env);
    ASSERT(is_int(result, 7));
    PASS();
}

// ============================================================================
// List Operations
// ============================================================================

TEST(eval_cons)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(cons 1 2)", env);
    ASSERT(IS_PAIR(result));
    ASSERT(is_int(car(result), 1));
    ASSERT(is_int(cdr(result), 2));
    PASS();
}

TEST(eval_car_cdr)
{
    unsigned env = default_environment();
    unsigned r1 = eval_string("(car '(1 2 3))", env);
    ASSERT(is_int(r1, 1));
    unsigned r2 = eval_string("(cdr '(1 2 3))", env);
    ASSERT(IS_PAIR(r2));
    ASSERT(is_int(car(r2), 2));
    PASS();
}

TEST(eval_length)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(length '(1 2 3 4 5))", env);
    ASSERT(is_int(result, 5));
    PASS();
}

TEST(eval_append)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(length (append '(1 2) '(3 4 5)))", env);
    ASSERT(is_int(result, 5));
    PASS();
}

// ============================================================================
// Main
// ============================================================================

int main(void)
{
    printf("=== Evaluator Unit Tests ===\n");

    init_heap();
    init_keywords();

    // Basic evaluation
    RUN_TEST(eval_integer);
    RUN_TEST(eval_negative_integer);
    RUN_TEST(eval_true);
    RUN_TEST(eval_false);
    RUN_TEST(eval_quote);

    // Arithmetic
    RUN_TEST(eval_add);
    RUN_TEST(eval_subtract);
    RUN_TEST(eval_multiply);
    RUN_TEST(eval_divide);

    // Comparison
    RUN_TEST(eval_eq_true);
    RUN_TEST(eval_eq_false);
    RUN_TEST(eval_lt_true);
    RUN_TEST(eval_lt_false);

    // If/Cond
    RUN_TEST(eval_if_true);
    RUN_TEST(eval_if_false);
    RUN_TEST(eval_cond_first);
    RUN_TEST(eval_cond_else);

    // Lambda
    RUN_TEST(eval_lambda_call);
    RUN_TEST(eval_lambda_closure);
    RUN_TEST(eval_lambda_rest_param);

    // Define
    RUN_TEST(eval_define_variable);
    RUN_TEST(eval_define_function);
    RUN_TEST(eval_define_recursive);

    // Let
    RUN_TEST(eval_let_simple);
    RUN_TEST(eval_let_nested);
    RUN_TEST(eval_letstar);
    RUN_TEST(eval_letrec);

    // And/Or
    RUN_TEST(eval_and_all_true);
    RUN_TEST(eval_and_one_false);
    RUN_TEST(eval_or_all_false);
    RUN_TEST(eval_or_one_true);

    // Begin
    RUN_TEST(eval_begin_sequence);
    RUN_TEST(eval_begin_side_effects);

    // Call/cc
    RUN_TEST(eval_callcc_simple);
    RUN_TEST(eval_callcc_escape);

    // GC protection
    RUN_TEST(gc_shadow_stack_balanced);
    RUN_TEST(gc_shadow_stack_lambda);
    RUN_TEST(gc_shadow_stack_letrec);
    RUN_TEST(gc_preserves_closures);
    RUN_TEST(gc_preserves_continuations);

    // Apply
    RUN_TEST(eval_apply_simple);
    RUN_TEST(eval_apply_lambda);

    // List operations
    RUN_TEST(eval_cons);
    RUN_TEST(eval_car_cdr);
    RUN_TEST(eval_length);
    RUN_TEST(eval_append);

    TEST_SUMMARY("evaluator");
}
