/**
 * @file test_eval.c
 * @brief Unit tests for the evaluator and GC
 */

#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L
#include "context.h"
#include "bytecode.h"
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

static unsigned compiled_eval_string(const char *src, unsigned env)
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

    GC_GUARD;
    gc_protect(&expr);
    gc_protect(&env);
    code_object *code = compile_toplevel(expr, env);
    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);
    code_free(code);
    return result;
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
        return x == ctx.atom_false;
}

static int is_stat_entry(unsigned entry, const char *name)
{
    if (CELL_TYPE(entry) != BT_CONS)
        return 0;
    unsigned key = car(entry);
    if (CELL_TYPE(key) != BT_ATOM)
        return 0;
    return strcmp(ctx.atom_table[CELL_ID(key)], name) == 0;
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
    ASSERT(result == ctx.atom_false);
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

TEST(eval_add_rationals)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(+ 1/2 1/3)", env);
    ASSERT(CELL_TYPE(result) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(result), 5));
    ASSERT(is_int(CELL_CDR(result), 6));
    PASS();
}

TEST(eval_subtract)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(- 10 3 2)", env);
    ASSERT(is_int(result, 5));
    PASS();
}

TEST(eval_subtract_rationals)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(- 1/2 1/3)", env);
    ASSERT(CELL_TYPE(result) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(result), 1));
    ASSERT(is_int(CELL_CDR(result), 6));
    PASS();
}

TEST(eval_multiply)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(* 2 3 4)", env);
    ASSERT(is_int(result, 24));
    PASS();
}

TEST(eval_multiply_exact_complex)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(* (make-rectangular 1 1) (make-rectangular 1 1))", env);
    ASSERT(CELL_TYPE(result) == BT_COMPLEX);
    ASSERT(is_int(CELL_CAR(result), 0));
    ASSERT(is_int(CELL_CDR(result), 2));
    PASS();
}

TEST(eval_divide)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(/ 20 4)", env);
    ASSERT(is_int(result, 5));
    PASS();
}

TEST(eval_divide_exact_complex)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(/ (make-rectangular 1 1) (make-rectangular 1 -1))", env);
    ASSERT(CELL_TYPE(result) == BT_COMPLEX);
    ASSERT(is_int(CELL_CAR(result), 0));
    ASSERT(is_int(CELL_CDR(result), 1));
    PASS();
}

TEST(eval_reciprocal_exact_complex)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(/ (make-rectangular 1 1))", env);
    ASSERT(CELL_TYPE(result) == BT_COMPLEX);
    ASSERT(CELL_TYPE(CELL_CAR(result)) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(CELL_CAR(result)), 1));
    ASSERT(is_int(CELL_CDR(CELL_CAR(result)), 2));
    ASSERT(CELL_TYPE(CELL_CDR(result)) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(CELL_CDR(result)), -1));
    ASSERT(is_int(CELL_CDR(CELL_CDR(result)), 2));
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

TEST(eval_gc_stats_shape)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(gc-stats)", env);
    ASSERT(CELL_TYPE(result) == BT_CONS);
    ASSERT(is_stat_entry(car(result), "minor-gc"));
    result = cdr(result);
    ASSERT(CELL_TYPE(result) == BT_CONS);
    ASSERT(is_stat_entry(car(result), "major-gc"));
    result = cdr(result);
    ASSERT(CELL_TYPE(result) == BT_CONS);
    ASSERT(is_stat_entry(car(result), "old-gen"));
    result = cdr(result);
    ASSERT(CELL_TYPE(result) == BT_CONS);
    ASSERT(is_stat_entry(car(result), "nursery"));
    ASSERT(cdr(result) == 0);
    PASS();
}

TEST(eval_bytevector_rejects_out_of_range_constructor)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(bytevector 256)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_make_bytevector_rejects_out_of_range_fill)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(make-bytevector 3 -1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_bytevector_set_rejects_out_of_range)
{
    unsigned env = default_environment();
    unsigned result =
        eval_string("(let ((bv (make-bytevector 1))) "
                    "(bytevector-u8-set! bv 0 300))",
                    env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_read_bytevector_zero_returns_empty)
{
    const char *path = "/tmp/vesper-read-bytevector-zero-test.bin";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    fputs("abc", f);
    fclose(f);

    unsigned env = default_environment();
    unsigned result =
        eval_string("(let ((p (open-binary-input-file "
                    "\"/tmp/vesper-read-bytevector-zero-test.bin\"))) "
                    "(let ((bv (read-bytevector 0 p))) "
                    "(close-input-port p) "
                    "(bytevector-length bv)))",
                    env);
    remove(path);
    ASSERT(is_int(result, 0));
    PASS();
}

TEST(eval_read_bytevector_rejects_large_count)
{
    const char *path = "/tmp/vesper-read-bytevector-large-test.bin";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    fputs("abc", f);
    fclose(f);

    unsigned env = default_environment();
    unsigned result =
        eval_string("(let ((p (open-binary-input-file "
                    "\"/tmp/vesper-read-bytevector-large-test.bin\"))) "
                    "(read-bytevector 4294967296 p))",
                    env);
    remove(path);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_abs_int64_min)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(abs -9223372036854775808)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775808");
    free(s);
    PASS();
}

TEST(eval_abs_negative_rational)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(abs -1/2)", env);
    ASSERT(CELL_TYPE(result) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(result), 1));
    ASSERT(is_int(CELL_CDR(result), 2));
    PASS();
}

TEST(eval_negate_rational)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(- 1/2)", env);
    ASSERT(CELL_TYPE(result) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(result), -1));
    ASSERT(is_int(CELL_CDR(result), 2));
    PASS();
}

TEST(eval_quotient_int64_min_by_negative_one)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(quotient -9223372036854775808 -1)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775808");
    free(s);
    PASS();
}

TEST(eval_remainder_int64_min_by_negative_one)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(remainder -9223372036854775808 -1)", env);
    ASSERT(is_int(result, 0));
    PASS();
}

TEST(eval_modulo_int64_min_by_negative_one)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(modulo -9223372036854775808 -1)", env);
    ASSERT(is_int(result, 0));
    PASS();
}

TEST(eval_inexact_to_exact_int64_min)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(inexact->exact -9.223372036854776e18)", env);
    ASSERT(is_int(result, INT64_MIN));
    PASS();
}

TEST(eval_inexact_to_exact_positive_int64_boundary)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(inexact->exact 9.223372036854776e18)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775808");
    free(s);
    PASS();
}

TEST(eval_number_to_string_int64_min_radix)
{
    unsigned env = default_environment();
    unsigned result =
        eval_string("(number->string -9223372036854775808 16)", env);
    ASSERT(CELL_TYPE(result) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(result), "-8000000000000000");
    PASS();
}

TEST(eval_arithmetic_shift_negative_left)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(arithmetic-shift -1 1)", env);
    ASSERT(is_int(result, -2));
    PASS();
}

TEST(eval_arithmetic_shift_int64_min_count)
{
    unsigned env = default_environment();
    unsigned result =
        eval_string("(arithmetic-shift -8 -9223372036854775808)", env);
    ASSERT(is_int(result, -1));
    PASS();
}

TEST(eval_arithmetic_shift_large_left_promotes)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(arithmetic-shift 1 63)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775808");
    free(s);
    PASS();
}

TEST(eval_arithmetic_shift_overflow_left_promotes)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(arithmetic-shift 2 62)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775808");
    free(s);
    PASS();
}

TEST(eval_arithmetic_shift_negative_large_left_promotes)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(arithmetic-shift -1 63)", env);
    ASSERT(is_int(result, INT64_MIN));
    PASS();
}

TEST(eval_rationalize_rejects_large_inexact)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(rationalize 1e100 0.0)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_floor_preserves_bignum)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(floor 9223372036854775808)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775808");
    free(s);
    PASS();
}

TEST(eval_magnitude_preserves_rational)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(magnitude -1/2)", env);
    ASSERT(CELL_TYPE(result) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(result), 1));
    ASSERT(is_int(CELL_CDR(result), 2));
    PASS();
}

TEST(eval_magnitude_preserves_bignum)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(magnitude -9223372036854775809)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775809");
    free(s);
    PASS();
}

TEST(eval_string_to_number_radix_bignum)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(string->number \"8000000000000000\" 16)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775808");
    free(s);
    PASS();
}

TEST(eval_string_to_number_radix_rejects_invalid)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(string->number \"12abc\" 10)", env);
    ASSERT(result == ctx.atom_false);
    PASS();
}

TEST(eval_integer_rejects_infinity)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(integer? 1e999)", env);
    ASSERT(result == ctx.atom_false);
    PASS();
}

TEST(compiled_div_fixnum_boundary)
{
    unsigned env = default_environment();
    unsigned result =
        compiled_eval_string("(let ((x -1073741824)) (/ x -1))", env);
    ASSERT(is_int(result, 1073741824));
    PASS();
}

TEST(compiled_lookup_add1_int64_max)
{
    unsigned env = default_environment();
    eval_string("(define x 9223372036854775807)", env);
    unsigned result = compiled_eval_string("(+ x 1)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775808");
    free(s);
    PASS();
}

TEST(compiled_lookup_sub1_int64_min)
{
    unsigned env = default_environment();
    eval_string("(define x -9223372036854775808)", env);
    unsigned result = compiled_eval_string("(- x 1)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "-9223372036854775809");
    free(s);
    PASS();
}

TEST(compiled_div_int64_min_by_negative_one)
{
    unsigned env = default_environment();
    eval_string("(define x -9223372036854775808)", env);
    eval_string("(define y -1)", env);
    unsigned result = compiled_eval_string("(/ x y)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775808");
    free(s);
    PASS();
}

TEST(compiled_modulo_int64_min_by_negative_one)
{
    unsigned env = default_environment();
    eval_string("(define x -9223372036854775808)", env);
    eval_string("(define y -1)", env);
    unsigned result = compiled_eval_string("(modulo x y)", env);
    ASSERT(is_int(result, 0));
    PASS();
}

TEST(compiled_letrec_tail_call_many_args)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(letrec ((loop (lambda (a b c d e f g h i j k l m n o p q count) "
        "(if (= count 0) "
        "(+ a b c d e f g h i j k l m n o p q) "
        "(loop a b c d e f g h i j k l m n o p q (- count 1)))))) "
        "(loop 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 2))",
        env);
    ASSERT(is_int(result, 153));
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
    RUN_TEST(eval_add_rationals);
    RUN_TEST(eval_subtract);
    RUN_TEST(eval_subtract_rationals);
    RUN_TEST(eval_multiply);
    RUN_TEST(eval_multiply_exact_complex);
    RUN_TEST(eval_divide);
    RUN_TEST(eval_divide_exact_complex);
    RUN_TEST(eval_reciprocal_exact_complex);

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
    RUN_TEST(eval_gc_stats_shape);

    // Bytevectors
    RUN_TEST(eval_bytevector_rejects_out_of_range_constructor);
    RUN_TEST(eval_make_bytevector_rejects_out_of_range_fill);
    RUN_TEST(eval_bytevector_set_rejects_out_of_range);
    RUN_TEST(eval_read_bytevector_zero_returns_empty);
    RUN_TEST(eval_read_bytevector_rejects_large_count);

    // Numeric edge cases
    RUN_TEST(eval_abs_int64_min);
    RUN_TEST(eval_abs_negative_rational);
    RUN_TEST(eval_negate_rational);
    RUN_TEST(eval_quotient_int64_min_by_negative_one);
    RUN_TEST(eval_remainder_int64_min_by_negative_one);
    RUN_TEST(eval_modulo_int64_min_by_negative_one);
    RUN_TEST(eval_inexact_to_exact_int64_min);
    RUN_TEST(eval_inexact_to_exact_positive_int64_boundary);
    RUN_TEST(eval_number_to_string_int64_min_radix);
    RUN_TEST(eval_arithmetic_shift_negative_left);
    RUN_TEST(eval_arithmetic_shift_int64_min_count);
    RUN_TEST(eval_arithmetic_shift_large_left_promotes);
    RUN_TEST(eval_arithmetic_shift_overflow_left_promotes);
    RUN_TEST(eval_arithmetic_shift_negative_large_left_promotes);
    RUN_TEST(eval_rationalize_rejects_large_inexact);
    RUN_TEST(eval_floor_preserves_bignum);
    RUN_TEST(eval_magnitude_preserves_rational);
    RUN_TEST(eval_magnitude_preserves_bignum);
    RUN_TEST(eval_string_to_number_radix_bignum);
    RUN_TEST(eval_string_to_number_radix_rejects_invalid);
    RUN_TEST(eval_integer_rejects_infinity);
    RUN_TEST(compiled_div_fixnum_boundary);
    RUN_TEST(compiled_lookup_add1_int64_max);
    RUN_TEST(compiled_lookup_sub1_int64_min);
    RUN_TEST(compiled_div_int64_min_by_negative_one);
    RUN_TEST(compiled_modulo_int64_min_by_negative_one);
    RUN_TEST(compiled_letrec_tail_call_many_args);

    TEST_SUMMARY("evaluator");
}
