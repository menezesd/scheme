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

TEST(eval_direct_fixnum_expression)
{
    unsigned env = default_environment();
    unsigned result = eval_obj(MAKE_FIXNUM(42), env);
    ASSERT_EQ(result, MAKE_FIXNUM(42));
    PASS();
}

TEST(compiled_direct_fixnum_expression)
{
    unsigned env = default_environment();
    unsigned expr = MAKE_FIXNUM(42);
    code_object *code = compile_toplevel(expr, env);
    ASSERT(code != NULL);
    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);
    code_free(code);
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

TEST(eval_exact_rational_comparison_preserves_precision)
{
    unsigned env = default_environment();
    ASSERT(eval_string("(= 9007199254740993/2 9007199254740992/2)", env) ==
           ctx.atom_false);
    ASSERT(eval_string("(> 9007199254740993/2 9007199254740992/2)", env) ==
           ctx.atom_true);
    ASSERT(eval_string("(< 9007199254740993/2 9007199254740994/2)", env) ==
           ctx.atom_true);
    PASS();
}

TEST(eval_ordered_comparison_rejects_complex)
{
    unsigned env = default_environment();
    ASSERT(eval_string("(< (make-rectangular 1 1) 2)", env) == TOK_ERROR);
    ASSERT(eval_string("(> 2 (make-rectangular 1 1))", env) == TOK_ERROR);
    PASS();
}

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

TEST(eval_lambda_rejects_wrong_arity)
{
    unsigned env = default_environment();
    ASSERT(eval_string("((lambda (x) x))", env) == TOK_ERROR);
    ASSERT(eval_string("((lambda (x) x) 1 2)", env) == TOK_ERROR);
    ASSERT(eval_string("((lambda (x y . rest) rest) 1)", env) == TOK_ERROR);
    ASSERT(eval_string("((lambda args (length args)) 1 2 3)", env) !=
           TOK_ERROR);
    PASS();
}

TEST(eval_rejects_malformed_lambda)
{
    unsigned env = default_environment();
    ASSERT(eval_string("(lambda . 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(lambda (x . 1) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(lambda (x))", env) == TOK_ERROR);
    ASSERT(eval_string("(lambda (x x) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(lambda (x . x) x)", env) == TOK_ERROR);
    PASS();
}

TEST(eval_rejects_malformed_special_forms)
{
    unsigned env = default_environment();
    ASSERT(eval_string("(quote)", env) == TOK_ERROR);
    ASSERT(eval_string("(quote a b)", env) == TOK_ERROR);
    ASSERT(eval_string("(if #t)", env) == TOK_ERROR);
    ASSERT(eval_string("(if #t 1 2 3)", env) == TOK_ERROR);
    ASSERT(eval_string("(begin . 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(and . 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(or . 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(cond . 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(cond 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(cond (else 1) (x 2))", env) == TOK_ERROR);
    ASSERT(eval_string("(quasiquote)", env) == TOK_ERROR);
    ASSERT(eval_string("(quasiquote a b)", env) == TOK_ERROR);
    ASSERT(eval_string("(set! x)", env) == TOK_ERROR);
    ASSERT(eval_string("(set! 1 2)", env) == TOK_ERROR);
    ASSERT(eval_string("(define x)", env) == TOK_ERROR);
    ASSERT(eval_string("(define 1 2)", env) == TOK_ERROR);
    ASSERT(eval_string("(define (1 x) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax x)", env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax 1 (syntax-rules ()))", env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules))", env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules . 1))", env) ==
           TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () 1))", env) ==
           TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () (m 1)))", env) ==
           TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () ((m) ...)))", env) ==
           TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () ((m) (...))))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () "
                       "((m x ...) (quote (a ...)))))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () "
                       "((m x) (quote (x ...)))))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () ((m) #(... x))))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () "
                       "((m #(x ... y ...)) 1)))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () "
                       "((m x y) #(x ... y ...))))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () ((m ...) 1)))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(letrec ((x)) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(letrec ((1 2)) 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax . 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules))) 1)", env) ==
           TOK_ERROR);
    ASSERT(eval_string("(letrec-syntax . 1)", env) == TOK_ERROR);
    PASS();
}

TEST(eval_load_rejects_non_string)
{
    unsigned env = default_environment();
    ASSERT(eval_string("(load 1)", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_load_rejects_non_string)
{
    unsigned env = default_environment();
    ASSERT(compiled_eval_string("(load 1)", env) == TOK_ERROR);
    PASS();
}

TEST(eval_load_reads_file_with_port_reader)
{
    const char *path = "/tmp/vesper-load-port-reader-test.scm";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    fputs("; leading comment\n"
          "(define loaded-port-reader-value 40)\n"
          "(+ loaded-port-reader-value 2)\n",
          f);
    fclose(f);

    unsigned env = default_environment();
    unsigned result =
        eval_string("(load \"/tmp/vesper-load-port-reader-test.scm\")", env);
    remove(path);
    ASSERT(is_int(result, 42));
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

TEST(eval_callcc_accepts_callcc)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(call/cc call/cc)", env);
    ASSERT(IS_CONT(result));
    PASS();
}

TEST(eval_callcc_result_is_procedure)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(procedure? (call/cc call/cc))", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(compiled_callcc_simple)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(call/cc (lambda (k) (k 42)))", env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(compiled_callcc_escape)
{
    unsigned env = default_environment();
    unsigned result =
        compiled_eval_string("(+ 1 (call/cc (lambda (k) (+ 10 (k 5)))))", env);
    ASSERT(is_int(result, 6));
    PASS();
}

TEST(compiled_callcc_accepts_callcc)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(call/cc call/cc)", env);
    ASSERT(IS_CELL(result) && CELL_TYPE(result) == BT_VMCONT);
    PASS();
}

TEST(compiled_callcc_result_is_procedure)
{
    unsigned env = default_environment();
    unsigned result =
        compiled_eval_string("(procedure? (call/cc call/cc))", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(compiled_callcc_rejects_wrong_arity)
{
    unsigned env = default_environment();
    ASSERT(compiled_eval_string("(call/cc)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(call/cc (lambda (k) k) 1)", env) ==
           TOK_ERROR);
    PASS();
}

TEST(eval_call_with_values_accepts_zero_values)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(call-with-values (lambda () (values)) (lambda () 42))",
        env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(eval_call_with_values_zero_values_to_list)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(call-with-values (lambda () (values)) list)",
        env);
    ASSERT(result == 0);
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

TEST(gc_preserves_current_input_string_port)
{
    unsigned env = default_environment();
    unsigned result = eval_string_gc(
        "(begin "
        "  (set-current-input-port! (open-input-string \"ab\")) "
        "  (gc-flip) "
        "  (char=? (read-char) #\\a))",
        &env);
    ctx.current_input_cell = 0;
    ctx.current_input = stdin;
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_read_string_port_preserves_unread_delimiter)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(let ((p (open-input-string \"1)\"))) "
        "  (and (= (read p) 1) "
        "       (char=? (read-char p) #\\))))",
        env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_read_file_port_preserves_unread_delimiter)
{
    const char *path = "/tmp/vesper-read-delimiter-test.scm";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    ASSERT(fwrite("1)", 1, 2, f) == 2);
    fclose(f);

    unsigned env = default_environment();
    unsigned result = eval_string(
        "(let ((p (open-input-file \"/tmp/vesper-read-delimiter-test.scm\"))) "
        "  (let ((ok (and (= (read p) 1) "
        "                 (char=? (peek-char p) #\\)) "
        "                 (char=? (read-char p) #\\))))) "
        "    (close-input-port p) "
        "    ok))",
        env);
    remove(path);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_read_bytevector_preserves_unread_delimiter)
{
    const char *path = "/tmp/vesper-read-bytevector-delimiter-test.bin";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    ASSERT(fwrite("1)A", 1, 3, f) == 3);
    fclose(f);

    unsigned env = default_environment();
    unsigned result = eval_string(
        "(let ((p (open-binary-input-file "
        "          \"/tmp/vesper-read-bytevector-delimiter-test.bin\"))) "
        "  (let* ((n (read p)) "
        "         (bv (read-bytevector 2 p)) "
        "         (ok (and (= n 1) "
        "                  (= (bytevector-u8-ref bv 0) 41) "
        "                  (= (bytevector-u8-ref bv 1) 65)))) "
        "    (close-input-port p) "
        "    ok))",
        env);
    remove(path);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(gc_preserves_current_output_string_port)
{
    unsigned env = default_environment();
    unsigned result = eval_string_gc(
        "(begin "
        "  (set-current-output-port! (open-output-string)) "
        "  (gc-flip) "
        "  (display \"xy\") "
        "  (get-output-string (current-output-port)))",
        &env);
    ctx.current_output_cell = 0;
    ctx.current_output = stdout;
    ASSERT_STR_EQ(GET_STRING_PTR(result), "xy");
    PASS();
}

TEST(eval_newline_rejects_closed_current_output_port)
{
    unsigned env = default_environment();
    unsigned result = eval_string_gc(
        "(let ((p (open-output-string))) "
        "  (set-current-output-port! p) "
        "  (close-output-port p) "
        "  (newline))",
        &env);
    ctx.current_output_cell = 0;
    ctx.current_output = stdout;
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_flush_rejects_closed_output_port)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(let ((p (open-output-string))) "
        "  (close-output-port p) "
        "  (flush-output-port p))",
        env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_io_rejects_nil_port_argument)
{
    unsigned env = default_environment();
    ASSERT(eval_string("(display \"x\" '())", env) == TOK_ERROR);
    ASSERT(eval_string("(write \"x\" '())", env) == TOK_ERROR);
    ASSERT(eval_string("(newline '())", env) == TOK_ERROR);
    ASSERT(eval_string("(flush-output-port '())", env) == TOK_ERROR);
    ASSERT(eval_string("(read '())", env) == TOK_ERROR);
    ASSERT(eval_string("(read-char '())", env) == TOK_ERROR);
    ASSERT(eval_string("(peek-char '())", env) == TOK_ERROR);
    ASSERT(eval_string("(char-ready? '())", env) == TOK_ERROR);
    ASSERT(eval_string("(read-line '())", env) == TOK_ERROR);
    PASS();
}

TEST(eval_close_port_rejects_wrong_direction)
{
    unsigned env = default_environment();
    unsigned result =
        eval_string("(close-input-port (open-output-string))", env);
    ASSERT(result == TOK_ERROR);

    result = eval_string("(close-output-port (open-input-string \"x\"))", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_set_current_port_rejects_closed_port)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(let ((p (open-output-string))) "
        "  (close-output-port p) "
        "  (set-current-output-port! p))",
        env);
    ctx.current_output_cell = 0;
    ctx.current_output = stdout;
    ASSERT(result == TOK_ERROR);

    result = eval_string(
        "(let ((p (open-input-string \"x\"))) "
        "  (close-input-port p) "
        "  (set-current-input-port! p))",
        env);
    ctx.current_input_cell = 0;
    ctx.current_input = stdin;
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_write_to_string_escapes_strings)
{
    unsigned env = default_environment();
    unsigned result =
        eval_string("(write-to-string \"a\\\"b\\\\c\\n\\t\\r\")", env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "\"a\\\"b\\\\c\\n\\t\\r\"");
    PASS();
}

TEST(compiled_write_to_string_hides_bytecode_closure)
{
    unsigned env = default_environment();
    unsigned result =
        compiled_eval_string("(write-to-string (lambda (x) x))", env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "[function]");
    PASS();
}

TEST(eval_open_output_file_append_argument_is_truthy)
{
    const char *path = "/tmp/vesper-open-output-append-test.txt";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    fputs("a", f);
    fclose(f);

    unsigned env = default_environment();
    unsigned result =
        eval_string("(let ((p (open-output-file "
                    "\"/tmp/vesper-open-output-append-test.txt\" 'append))) "
                    "  (display \"b\" p) "
                    "  (close-output-port p) "
                    "  #t)",
                    env);
    ASSERT(is_bool(result, 1));

    f = fopen(path, "rb");
    ASSERT(f != NULL);
    char buf[4] = {0};
    size_t nread = fread(buf, 1, 3, f);
    fclose(f);
    remove(path);
    ASSERT(nread == 2);
    ASSERT_STR_EQ(buf, "ab");
    PASS();
}

TEST(eval_open_output_file_false_argument_truncates)
{
    const char *path = "/tmp/vesper-open-output-truncate-test.txt";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    fputs("old", f);
    fclose(f);

    unsigned env = default_environment();
    unsigned result =
        eval_string("(let ((p (open-output-file "
                    "\"/tmp/vesper-open-output-truncate-test.txt\" #f))) "
                    "  (display \"n\" p) "
                    "  (close-output-port p) "
                    "  #t)",
                    env);
    ASSERT(is_bool(result, 1));

    f = fopen(path, "rb");
    ASSERT(f != NULL);
    char buf[4] = {0};
    size_t nread = fread(buf, 1, 3, f);
    fclose(f);
    remove(path);
    ASSERT(nread == 1);
    ASSERT_STR_EQ(buf, "n");
    PASS();
}

TEST(gc_preserves_labeled_string)
{
    unsigned env = default_environment();
    unsigned result =
        eval_string_gc("(let ((s '#1=\"abc\")) (gc-flip) (string-length s))",
                       &env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(gc_preserves_labeled_vector)
{
    unsigned env = default_environment();
    unsigned result =
        eval_string_gc("(let ((v '#1=#(10 20))) (gc-flip) (vector-ref v 1))",
                       &env);
    ASSERT(is_int(result, 20));
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

TEST(eval_rejects_improper_application)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(+ . 1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_quasiquote_unquotes_vector_element)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(vector-ref `#(a ,(+ 1 2)) 1)", env);
    ASSERT(is_int(result, 3));
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

TEST(eval_rejects_circular_list_operations)
{
    unsigned env = default_environment();
    ASSERT(eval_string("(let ((x (cons 1 '()))) "
                       "  (set-cdr! x x) "
                       "  (length x))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let ((x (cons 1 '()))) "
                       "  (set-cdr! x x) "
                       "  (reverse x))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let ((x (cons 1 '()))) "
                       "  (set-cdr! x x) "
                       "  (append x '()))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let ((x (cons #\\a '()))) "
                       "  (set-cdr! x x) "
                       "  (list->string x))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let ((x (cons 1 '()))) "
                       "  (set-cdr! x x) "
                       "  (list->vector x))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let ((x (cons 1 '()))) "
                       "  (set-cdr! x x) "
                       "  (apply + x))",
                       env) == TOK_ERROR);
    PASS();
}

TEST(eval_equal_handles_cycles)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(let ((x (cons 1 '())) "
                                  "      (y (cons 1 '()))) "
                                  "  (set-cdr! x x) "
                                  "  (set-cdr! y y) "
                                  "  (equal? x y))",
                                  env);
    ASSERT(is_bool(result, 1));

    result = eval_string("(let ((x (cons 1 '())) "
                         "      (y (cons 2 '()))) "
                         "  (set-cdr! x x) "
                         "  (set-cdr! y y) "
                         "  (equal? x y))",
                         env);
    ASSERT(is_bool(result, 0));

    result = eval_string("(let ((x (make-vector 1 #f)) "
                         "      (y (make-vector 1 #f))) "
                         "  (vector-set! x 0 x) "
                         "  (vector-set! y 0 y) "
                         "  (equal? x y))",
                         env);
    ASSERT(is_bool(result, 1));
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

TEST(eval_string_to_symbol_preserves_numeric_text)
{
    unsigned env = default_environment();
    unsigned result =
        eval_string("(symbol? (string->symbol \"123\"))", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_environment_rejects_non_integer_version)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(scheme-report-environment \"5\")", env);
    ASSERT(result == TOK_ERROR);

    result = eval_string("(null-environment 'r5rs)", env);
    ASSERT(result == TOK_ERROR);
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

TEST(eval_read_bytevector_rejects_closed_port)
{
    const char *path = "/tmp/vesper-read-bytevector-closed-test.bin";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    fputs("abc", f);
    fclose(f);

    unsigned env = default_environment();
    unsigned result =
        eval_string("(let ((p (open-binary-input-file "
                    "\"/tmp/vesper-read-bytevector-closed-test.bin\"))) "
                    "(close-input-port p) "
                    "(read-bytevector 1 p))",
                    env);
    remove(path);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_read_bytevector_zero_rejects_closed_port)
{
    const char *path = "/tmp/vesper-read-bytevector-zero-closed-test.bin";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    fputs("abc", f);
    fclose(f);

    unsigned env = default_environment();
    unsigned result =
        eval_string("(let ((p (open-binary-input-file "
                    "\"/tmp/vesper-read-bytevector-zero-closed-test.bin\"))) "
                    "(close-input-port p) "
                    "(read-bytevector 0 p))",
                    env);
    remove(path);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_char_ready_file_port)
{
    const char *path = "/tmp/vesper-char-ready-test.txt";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    fputs("x", f);
    fclose(f);

    unsigned env = default_environment();
    unsigned result =
        eval_string("(let ((p (open-input-file "
                    "\"/tmp/vesper-char-ready-test.txt\"))) "
                    "(let ((ready (char-ready? p))) "
                    "(close-input-port p) "
                    "ready))",
                    env);
    remove(path);
    ASSERT(is_bool(result, 1));
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

TEST(eval_number_to_string_exact_non_int64)
{
    unsigned env = default_environment();

    unsigned big = eval_string("(number->string 9223372036854775808)", env);
    ASSERT(CELL_TYPE(big) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(big), "9223372036854775808");

    unsigned big_hex =
        eval_string("(number->string 9223372036854775808 16)", env);
    ASSERT(CELL_TYPE(big_hex) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(big_hex), "8000000000000000");

    unsigned big_binary =
        eval_string("(number->string (expt 2 200) 2)", env);
    ASSERT(CELL_TYPE(big_binary) == BT_STRING);
    ASSERT_EQ(strlen(GET_STRING_PTR(big_binary)), 201);
    ASSERT_EQ(GET_STRING_PTR(big_binary)[0], '1');
    ASSERT_STR_EQ(GET_STRING_PTR(big_binary) + 1,
                  "00000000000000000000000000000000000000000000000000"
                  "00000000000000000000000000000000000000000000000000"
                  "00000000000000000000000000000000000000000000000000"
                  "00000000000000000000000000000000000000000000000000");

    unsigned rat = eval_string("(number->string -22/7)", env);
    ASSERT(CELL_TYPE(rat) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(rat), "-22/7");

    unsigned complex =
        eval_string("(number->string (make-rectangular 3 -4))", env);
    ASSERT(CELL_TYPE(complex) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(complex), "3-4i");

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

TEST(eval_sqrt_preserves_exact_bignum_squares)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(number->string "
        " (sqrt 100000000000000000000000000000000000000))",
        env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "10000000000000000000");

    result = eval_string(
        "(number->string "
        " (sqrt 100000000000000000000000000000000000000/4))",
        env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "5000000000000000000");

    result = eval_string(
        "(number->string "
        " (sqrt 4/100000000000000000000000000000000000000))",
        env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "1/5000000000000000000");
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

TEST(eval_complex_reader_accepts_implicit_imaginary_unit)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(and (= (real-part 1+i) 1) "
        "     (= (imag-part 1+i) 1) "
        "     (= (real-part 1-i) 1) "
        "     (= (imag-part 1-i) -1) "
        "     (= (imag-part (string->number \"1+i\")) 1) "
        "     (= (imag-part (string->number \"1-i\")) -1))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_complex_reader_preserves_exact_components)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(and (exact? 1+2i) "
        "     (= (real-part 1+2i) 1) "
        "     (= (imag-part 1+2i) 2) "
        "     (exact? 2i) "
        "     (= (imag-part 2i) 2) "
        "     (exact? 1/2+3/4i) "
        "     (= (real-part 1/2+3/4i) 1/2) "
        "     (= (imag-part 1/2+3/4i) 3/4) "
        "     (equal? (number->string 1/2+3/4i) \"1/2+3/4i\"))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_complex_reader_rejects_nested_imaginary_suffix)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(and (not (number? '1+2ii)) "
        "     (not (number? '2ii)) "
        "     (not (number? '1+2/3ii)) "
        "     (not (string->number \"1+2ii\")) "
        "     (not (string->number \"2ii\")) "
        "     (not (string->number \"1+2/3ii\")))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_integer_rejects_infinity)
{
    unsigned env = default_environment();
    unsigned result = eval_string("(integer? 1e999)", env);
    ASSERT(result == ctx.atom_false);
    PASS();
}

TEST(eval_exact_rejects_non_numbers)
{
    unsigned env = default_environment();
    ASSERT(eval_string("(exact? '())", env) == ctx.atom_false);
    ASSERT(eval_string("(exact? 'foo)", env) == ctx.atom_false);
    PASS();
}

TEST(eval_numtower_rejects_non_numbers)
{
    unsigned env = default_environment();
    ASSERT(eval_string("(real-part 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(imag-part 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(magnitude 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(angle 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(exact->inexact 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(inexact->exact 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(make-rectangular 'foo 0)", env) == TOK_ERROR);
    ASSERT(eval_string("(make-polar 'foo 0)", env) == TOK_ERROR);
    ASSERT(eval_string("(rationalize 'foo 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(finite? 'foo)", env) == ctx.atom_false);
    ASSERT(eval_string("(infinite? 'foo)", env) == ctx.atom_false);
    ASSERT(eval_string("(nan? 'foo)", env) == ctx.atom_false);
    PASS();
}

TEST(eval_exact_tiny_complex_imag_part_is_not_zero)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(= (imag-part (make-rectangular 1 (/ 1 (expt 10 400)))) 0)",
        env);
    ASSERT(result == ctx.atom_false);

    result = eval_string(
        "(= (imag-part "
        "    (/ 1 (make-rectangular 0 (/ 1 (expt 10 400))))) "
        "   0)",
        env);
    ASSERT(result == ctx.atom_false);

    result = eval_string(
        "(= (imag-part "
        "    (+ (make-rectangular 1 (/ 1 (expt 10 400))) 0)) "
        "   0)",
        env);
    ASSERT(result == ctx.atom_false);

    result = eval_string("(exact? (+ (make-rectangular 1.0 2) 0))", env);
    ASSERT(result == ctx.atom_false);

    result = compiled_eval_string("(exact? (+ (make-rectangular 1.0 2) 0))",
                                  env);
    ASSERT(result == ctx.atom_false);
    PASS();
}

TEST(eval_math_rejects_non_numbers)
{
    unsigned env = default_environment();
    ASSERT(eval_string("(asin 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(acos 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(sqrt 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(expt 'foo 2)", env) == TOK_ERROR);
    ASSERT(eval_string("(expt 2 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(atan 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(atan 1 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(log 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(exp 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(sin 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(cos 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(tan 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(floor 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(ceiling 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(truncate 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(round 'foo)", env) == TOK_ERROR);
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

TEST(compiled_constant_folding_releases_gc_roots)
{
    unsigned env = default_environment();
    for (int i = 0; i < 600; i++) {
        unsigned result = compiled_eval_string("(+ 1 2)", env);
        ASSERT(is_int(result, 3));
    }
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

TEST(compiled_string_to_list_allocates_fresh_result)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(let ((f (lambda () (string->list \"ab\")))) "
        "  (let ((x (f)) (y (f))) "
        "    (set-car! x #\\z) "
        "    (char=? (car y) #\\a)))",
        env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(compiled_begin_preserves_unbound_lookup_error)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(begin definitely-unbound-variable 1)",
        env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_multiply_by_zero_preserves_side_effects)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(let ((x 0)) "
        "  (* (begin (set! x 1) 2) 0) "
        "  x)",
        env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(compiled_multiply_by_one_preserves_type_error)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(* \"x\" 1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_divide_by_one_preserves_type_error)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(/ \"x\" 1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_double_not_returns_boolean)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(let ((x 42)) (not (not x)))",
        env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(compiled_add1_sub1_preserves_type_error)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(- (+ \"x\" 1) 1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_zerop_preserves_type_error)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(= \"x\" 0)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_if_numeq_preserves_type_error)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(if (= \"x\" 1) 2 3)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_if_less_than_preserves_type_error)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(if (< \"x\" 1) 2 3)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_if_other_comparisons_preserve_type_error)
{
    unsigned env = default_environment();
    ASSERT(compiled_eval_string("(if (> \"x\" 1) 2 3)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(if (<= \"x\" 1) 2 3)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(if (>= \"x\" 1) 2 3)", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_if_constant_branches_preserve_test_effects)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(let ((x 0)) "
        "  (and (if (begin (set! x 1) #t) 5 5) x))",
        env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(compiled_and_late_constant_false_preserves_prior_effects)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(let ((x 0)) "
        "  (and (begin (set! x 1) #t) #f (set! x 2)) "
        "  x)",
        env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(compiled_or_late_constant_true_preserves_prior_effects)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(let ((x 0)) "
        "  (or (begin (set! x 1) #f) 5 (set! x 2)) "
        "  x)",
        env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(compiled_append_boxes_improper_tail)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(cdr (append '(a) 1))", env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(compiled_apply_rejects_non_list_final_argument)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(apply cons 1 2)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_apply_rejects_improper_final_list)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(apply + '(1 . 2))", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_length_accepts_string)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(length \"abc\")", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(compiled_length_accepts_vector)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(length '#(1 2 3 4))", env);
    ASSERT(is_int(result, 4));
    PASS();
}

TEST(compiled_length_rejects_number)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(length 1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_listp_rejects_circular_list)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(let ((x (cons 1 '()))) "
        "  (set-cdr! x x) "
        "  (list? x))",
        env);
    ASSERT(is_bool(result, 0));
    PASS();
}

TEST(compiled_rejects_circular_list_operations)
{
    unsigned env = default_environment();
    ASSERT(compiled_eval_string("(let ((x (cons 1 '()))) "
                                "  (set-cdr! x x) "
                                "  (length x))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let ((x (cons 1 '()))) "
                                "  (set-cdr! x x) "
                                "  (reverse x))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let ((x (cons 1 '()))) "
                                "  (set-cdr! x x) "
                                "  (append x '()))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let ((x (cons #\\a '()))) "
                                "  (set-cdr! x x) "
                                "  (list->string x))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let ((x (cons 1 '()))) "
                                "  (set-cdr! x x) "
                                "  (list->vector x))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let ((x (cons 1 '()))) "
                                "  (set-cdr! x x) "
                                "  (apply + x))",
                                env) == TOK_ERROR);
    PASS();
}

TEST(compiled_equal_handles_cycles)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(let ((x (cons 1 '())) "
                                           "      (y (cons 1 '()))) "
                                           "  (set-cdr! x x) "
                                           "  (set-cdr! y y) "
                                           "  (equal? x y))",
                                           env);
    ASSERT(is_bool(result, 1));

    result = compiled_eval_string("(let ((x (cons 1 '())) "
                                  "      (y (cons 2 '()))) "
                                  "  (set-cdr! x x) "
                                  "  (set-cdr! y y) "
                                  "  (equal? x y))",
                                  env);
    ASSERT(is_bool(result, 0));

    result = compiled_eval_string("(let ((x (make-vector 1 #f)) "
                                  "      (y (make-vector 1 #f))) "
                                  "  (vector-set! x 0 x) "
                                  "  (vector-set! y 0 y) "
                                  "  (equal? x y))",
                                  env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(compiled_vector_ref_rejects_non_vector)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(vector-ref 1 0)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_call_rejects_fixnum_operator)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(1 2)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_rejects_improper_application)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(+ . 1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_rejects_malformed_lambda)
{
    unsigned env = default_environment();
    ASSERT(compiled_eval_string("(lambda . 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(lambda (x . 1) x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(lambda (x))", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(lambda (x x) x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(lambda (x . x) x)", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_lambda_rejects_wrong_arity)
{
    unsigned env = default_environment();
    ASSERT(compiled_eval_string("((lambda (x) x))", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("((lambda (x) x) 1 2)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("((lambda (x y . rest) rest) 1)", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string("((lambda args (length args)) 1 2 3)", env) !=
           TOK_ERROR);
    PASS();
}

TEST(compiled_rejects_malformed_special_forms)
{
    unsigned env = default_environment();
    ASSERT(compiled_eval_string("(quote)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(quote a b)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(if #t)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(if #t 1 2 3)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(begin . 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(and . 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(or . 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(cond . 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(cond 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(cond (else 1) (x 2))", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(quasiquote)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(quasiquote a b)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(set! x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(set! 1 2)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(define x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(define 1 2)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(define (1 x) x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(define-syntax x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(define-syntax 1 (syntax-rules ()))", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string("(define-syntax m (syntax-rules))", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string("(define-syntax m (syntax-rules . 1))", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string("(define-syntax m (syntax-rules () 1))", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string("(define-syntax m (syntax-rules () (m 1)))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(define-syntax m (syntax-rules () ((m) ...)))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((m) (...))))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((m x ...) (quote (a ...)))))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((m x) (quote (x ...)))))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((m) #(... x))))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((m #(x ... y ...)) 1)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((m x y) #(x ... y ...))))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((m ...) 1)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(letrec ((x)) x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(letrec ((1 2)) 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax . 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules))) 1)", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string("(letrec-syntax . 1)", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_quasiquote_unquotes_vector_element)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(vector-ref `#(a ,(+ 1 2)) 1)", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(compiled_local_set_returns_assigned_value)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(begin "
        "  (define local-set-result (lambda (x) (set! x 2))) "
        "  (local-set-result 1))",
        env);
    ASSERT(is_int(result, 2));
    PASS();
}

TEST(compiled_call_with_values_accepts_zero_values)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(call-with-values (lambda () (values)) (lambda () 42))",
        env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(compiled_call_with_values_zero_values_to_list)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(call-with-values (lambda () (values)) list)",
        env);
    ASSERT(result == 0);
    PASS();
}

TEST(compiled_call_with_values_rejects_non_producer)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string("(call-with-values '() list)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_define_syntax_preserves_custom_ellipsis)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(define-syntax foo "
        "  (syntax-rules ::: () "
        "    ((foo ... args :::) (args ::: ...))))",
        env);
    ASSERT(result != TOK_ERROR);

    result = eval_string("(foo 3 - 5)", env);
    ASSERT(is_int(result, 2));
    PASS();
}

TEST(compiled_let_syntax_preserves_custom_ellipsis)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(let-syntax "
        "    ((foo (syntax-rules ::: () "
        "            ((foo ... args :::) (args ::: ...))))) "
        "  (foo 3 - 5))",
        env);
    ASSERT(is_int(result, 2));
    PASS();
}

TEST(eval_syntax_rules_respects_shadowed_ellipsis)
{
    unsigned env = default_environment();
    unsigned result = eval_string(
        "(let ((... 2)) "
        "  (let-syntax "
        "      ((s (syntax-rules () "
        "            ((_ x ...) 'bad) "
        "            ((_ x) 'ok)))) "
        "    (s 1)))",
        env);
    ASSERT(IS_ATOM(result));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(result)], "ok");
    PASS();
}

TEST(compiled_syntax_rules_respects_shadowed_ellipsis)
{
    unsigned env = default_environment();
    unsigned result = compiled_eval_string(
        "(let ((... 2)) "
        "  (let-syntax "
        "      ((s (syntax-rules () "
        "            ((_ x ...) 'bad) "
        "            ((_ x) 'ok)))) "
        "    (s 1)))",
        env);
    ASSERT(IS_ATOM(result));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(result)], "ok");
    PASS();
}

TEST(compiled_macro_thunk_captures_stack_local)
{
    unsigned env = default_environment();
    unsigned define_result =
        eval_string("(define (%call-thunk thunk) (thunk))", env);
    ASSERT(define_result != TOK_ERROR);
    unsigned result = compiled_eval_string(
        "(let-syntax "
        "    ((call-thunk "
        "    (syntax-rules () "
        "      ((call-thunk body ...) "
        "       (%call-thunk (lambda () body ...)))))) "
        "  ((lambda (k) (call-thunk (k #t))) (lambda (x) x)))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_calls_bytecode_closure_with_stack_locals)
{
    unsigned env = default_environment();
    FILE *old_stdin = stdin;
    const char *src =
        "(define bytecode-local-set (lambda (x) (set! x 2)))";
    FILE *f = fmemopen((void *)src, strlen(src), "r");
    ASSERT(f != NULL);
    stdin = f;
    reader_reset_labels();
    unsigned expr = read_obj();
    fclose(f);
    stdin = old_stdin;
    ASSERT(expr != TOK_ERROR);

    GC_GUARD;
    gc_protect(&expr);
    gc_protect(&env);
    code_object *code = compile_toplevel(expr, env);
    vm_state vm;
    vm_init(&vm);
    unsigned define_result = vm_run(&vm, code, env);
    vm_free(&vm);
    ASSERT(define_result != TOK_ERROR);

    unsigned result = eval_string("(bytecode-local-set 1)", env);
    ASSERT(is_int(result, 2));
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
    RUN_TEST(eval_direct_fixnum_expression);
    RUN_TEST(compiled_direct_fixnum_expression);
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
    RUN_TEST(eval_exact_rational_comparison_preserves_precision);
    RUN_TEST(eval_ordered_comparison_rejects_complex);
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
    RUN_TEST(eval_lambda_rejects_wrong_arity);
    RUN_TEST(eval_rejects_malformed_lambda);
    RUN_TEST(eval_rejects_malformed_special_forms);
    RUN_TEST(eval_load_rejects_non_string);
    RUN_TEST(compiled_load_rejects_non_string);
    RUN_TEST(eval_load_reads_file_with_port_reader);

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
    RUN_TEST(eval_callcc_accepts_callcc);
    RUN_TEST(eval_callcc_result_is_procedure);
    RUN_TEST(compiled_callcc_simple);
    RUN_TEST(compiled_callcc_escape);
    RUN_TEST(compiled_callcc_accepts_callcc);
    RUN_TEST(compiled_callcc_result_is_procedure);
    RUN_TEST(compiled_callcc_rejects_wrong_arity);
    RUN_TEST(eval_call_with_values_accepts_zero_values);
    RUN_TEST(eval_call_with_values_zero_values_to_list);

    // GC protection
    RUN_TEST(gc_shadow_stack_balanced);
    RUN_TEST(gc_shadow_stack_lambda);
    RUN_TEST(gc_shadow_stack_letrec);
    RUN_TEST(gc_preserves_closures);
    RUN_TEST(gc_preserves_continuations);
    RUN_TEST(gc_preserves_current_input_string_port);
    RUN_TEST(eval_read_string_port_preserves_unread_delimiter);
    RUN_TEST(eval_read_file_port_preserves_unread_delimiter);
    RUN_TEST(eval_read_bytevector_preserves_unread_delimiter);
    RUN_TEST(gc_preserves_current_output_string_port);
    RUN_TEST(eval_newline_rejects_closed_current_output_port);
    RUN_TEST(eval_flush_rejects_closed_output_port);
    RUN_TEST(eval_io_rejects_nil_port_argument);
    RUN_TEST(eval_close_port_rejects_wrong_direction);
    RUN_TEST(eval_set_current_port_rejects_closed_port);
    RUN_TEST(eval_write_to_string_escapes_strings);
    RUN_TEST(compiled_write_to_string_hides_bytecode_closure);
    RUN_TEST(eval_open_output_file_append_argument_is_truthy);
    RUN_TEST(eval_open_output_file_false_argument_truncates);
    RUN_TEST(gc_preserves_labeled_string);
    RUN_TEST(gc_preserves_labeled_vector);

    // Apply
    RUN_TEST(eval_apply_simple);
    RUN_TEST(eval_apply_lambda);
    RUN_TEST(eval_rejects_improper_application);
    RUN_TEST(eval_quasiquote_unquotes_vector_element);

    // List operations
    RUN_TEST(eval_cons);
    RUN_TEST(eval_car_cdr);
    RUN_TEST(eval_length);
    RUN_TEST(eval_rejects_circular_list_operations);
    RUN_TEST(eval_equal_handles_cycles);
    RUN_TEST(eval_append);
    RUN_TEST(eval_gc_stats_shape);
    RUN_TEST(eval_string_to_symbol_preserves_numeric_text);
    RUN_TEST(eval_environment_rejects_non_integer_version);

    // Bytevectors
    RUN_TEST(eval_bytevector_rejects_out_of_range_constructor);
    RUN_TEST(eval_make_bytevector_rejects_out_of_range_fill);
    RUN_TEST(eval_bytevector_set_rejects_out_of_range);
    RUN_TEST(eval_read_bytevector_zero_returns_empty);
    RUN_TEST(eval_read_bytevector_rejects_large_count);
    RUN_TEST(eval_read_bytevector_rejects_closed_port);
    RUN_TEST(eval_read_bytevector_zero_rejects_closed_port);
    RUN_TEST(eval_char_ready_file_port);

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
    RUN_TEST(eval_number_to_string_exact_non_int64);
    RUN_TEST(eval_arithmetic_shift_negative_left);
    RUN_TEST(eval_arithmetic_shift_int64_min_count);
    RUN_TEST(eval_arithmetic_shift_large_left_promotes);
    RUN_TEST(eval_arithmetic_shift_overflow_left_promotes);
    RUN_TEST(eval_arithmetic_shift_negative_large_left_promotes);
    RUN_TEST(eval_rationalize_rejects_large_inexact);
    RUN_TEST(eval_floor_preserves_bignum);
    RUN_TEST(eval_magnitude_preserves_rational);
    RUN_TEST(eval_magnitude_preserves_bignum);
    RUN_TEST(eval_sqrt_preserves_exact_bignum_squares);
    RUN_TEST(eval_string_to_number_radix_bignum);
    RUN_TEST(eval_string_to_number_radix_rejects_invalid);
    RUN_TEST(eval_complex_reader_accepts_implicit_imaginary_unit);
    RUN_TEST(eval_complex_reader_preserves_exact_components);
    RUN_TEST(eval_complex_reader_rejects_nested_imaginary_suffix);
    RUN_TEST(eval_integer_rejects_infinity);
    RUN_TEST(eval_exact_rejects_non_numbers);
    RUN_TEST(eval_numtower_rejects_non_numbers);
    RUN_TEST(eval_exact_tiny_complex_imag_part_is_not_zero);
    RUN_TEST(eval_math_rejects_non_numbers);
    RUN_TEST(compiled_div_fixnum_boundary);
    RUN_TEST(compiled_constant_folding_releases_gc_roots);
    RUN_TEST(compiled_lookup_add1_int64_max);
    RUN_TEST(compiled_lookup_sub1_int64_min);
    RUN_TEST(compiled_div_int64_min_by_negative_one);
    RUN_TEST(compiled_modulo_int64_min_by_negative_one);
    RUN_TEST(compiled_letrec_tail_call_many_args);
    RUN_TEST(compiled_string_to_list_allocates_fresh_result);
    RUN_TEST(compiled_begin_preserves_unbound_lookup_error);
    RUN_TEST(compiled_multiply_by_zero_preserves_side_effects);
    RUN_TEST(compiled_multiply_by_one_preserves_type_error);
    RUN_TEST(compiled_divide_by_one_preserves_type_error);
    RUN_TEST(compiled_double_not_returns_boolean);
    RUN_TEST(compiled_add1_sub1_preserves_type_error);
    RUN_TEST(compiled_zerop_preserves_type_error);
    RUN_TEST(compiled_if_numeq_preserves_type_error);
    RUN_TEST(compiled_if_less_than_preserves_type_error);
    RUN_TEST(compiled_if_other_comparisons_preserve_type_error);
    RUN_TEST(compiled_if_constant_branches_preserve_test_effects);
    RUN_TEST(compiled_and_late_constant_false_preserves_prior_effects);
    RUN_TEST(compiled_or_late_constant_true_preserves_prior_effects);
    RUN_TEST(compiled_append_boxes_improper_tail);
    RUN_TEST(compiled_apply_rejects_non_list_final_argument);
    RUN_TEST(compiled_apply_rejects_improper_final_list);
    RUN_TEST(compiled_length_accepts_string);
    RUN_TEST(compiled_length_accepts_vector);
    RUN_TEST(compiled_length_rejects_number);
    RUN_TEST(compiled_listp_rejects_circular_list);
    RUN_TEST(compiled_rejects_circular_list_operations);
    RUN_TEST(compiled_equal_handles_cycles);
    RUN_TEST(compiled_vector_ref_rejects_non_vector);
    RUN_TEST(compiled_call_rejects_fixnum_operator);
    RUN_TEST(compiled_rejects_improper_application);
    RUN_TEST(compiled_rejects_malformed_lambda);
    RUN_TEST(compiled_lambda_rejects_wrong_arity);
    RUN_TEST(compiled_rejects_malformed_special_forms);
    RUN_TEST(compiled_quasiquote_unquotes_vector_element);
    RUN_TEST(compiled_local_set_returns_assigned_value);
    RUN_TEST(compiled_call_with_values_accepts_zero_values);
    RUN_TEST(compiled_call_with_values_zero_values_to_list);
    RUN_TEST(compiled_call_with_values_rejects_non_producer);
    RUN_TEST(compiled_define_syntax_preserves_custom_ellipsis);
    RUN_TEST(compiled_let_syntax_preserves_custom_ellipsis);
    RUN_TEST(eval_syntax_rules_respects_shadowed_ellipsis);
    RUN_TEST(compiled_syntax_rules_respects_shadowed_ellipsis);
    RUN_TEST(compiled_macro_thunk_captures_stack_local);
    RUN_TEST(eval_calls_bytecode_closure_with_stack_locals);

    TEST_SUMMARY("evaluator");
}
