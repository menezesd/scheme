/**
 * @file test_eval.c
 * @brief Unit tests for the evaluator and GC
 */

#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L
#include "context.h"
#include "bytecode.h"
#include "compile_internal.h"
#include "env.h"
#include "eval.h"
#include "eval_internal.h"
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
    // env arrives by value, so the caller's gc_protect does not cover this
    // copy. read_obj() below allocates and can therefore collect, which would
    // leave this copy pointing at a cell that has since been reused. Root it
    // before reading, not after.
    GC_GUARD;
    gc_protect(&env);

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
    // Same by-value hazard as eval_string: root env before read_obj().
    GC_GUARD;
    gc_protect(&env);

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

    gc_protect(&expr);
    code_object *code = compile_toplevel(expr, env);
    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);
    gc_sweep_code_objects();
    return result;
}

static bool eval_compiled_equal(const char *src)
{
    GC_GUARD;
    unsigned eval_env = default_environment();
    gc_protect(&eval_env);
    unsigned compiled_env = default_environment();
    gc_protect(&compiled_env);
    unsigned eval_result = eval_string(src, eval_env);
    if (eval_result == TOK_ERROR)
        return false;
    gc_protect(&eval_result);
    unsigned compiled_result = compiled_eval_string(src, compiled_env);
    if (compiled_result == TOK_ERROR)
        return false;
    gc_protect(&compiled_result);

    return deep_equal(eval_result, compiled_result);
}

static unsigned read_expr_from_string(const char *src)
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
    return expr;
}

static bool code_contains_opcode(code_object *code, unsigned opcode)
{
    for (unsigned i = 0; i < code->code_len;) {
        unsigned op = code->code[i];
        if (op == opcode)
            return true;
        i += instruction_size(op);
    }
    for (unsigned i = 0; i < code->children_len; i++) {
        if (code_contains_opcode(code->children[i], opcode))
            return true;
    }
    return false;
}

static unsigned code_count_opcode(code_object *code, unsigned opcode)
{
    unsigned count = 0;
    for (unsigned i = 0; i < code->code_len;) {
        unsigned op = code->code[i];
        if (op == opcode)
            count++;
        i += instruction_size(op);
    }
    for (unsigned i = 0; i < code->children_len; i++)
        count += code_count_opcode(code->children[i], opcode);
    return count;
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
    set_alloc_gc_root(NULL);
    return result;
}

// Helper: check if result is an integer with given value
static int is_int(unsigned x, int64_t val)
{
    // Small integers are tagged fixnums rather than heap cells, and CELL_TYPE
    // on one indexes the heap about two billion cells out of bounds. The VM
    // makes them for any arithmetic result that fits, so a number pulled out
    // of a computed structure is routinely one of these.
    if (IS_FIXNUM(x))
        return FIXNUM_VALUE(x) == val;
    if (!IS_CELL(x) || CELL_TYPE(x) != BT_NUM)
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("42", env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(eval_direct_fixnum_expression)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_obj(MAKE_FIXNUM(42), env);
    ASSERT_EQ(result, MAKE_FIXNUM(42));
    PASS();
}

TEST(compiled_direct_fixnum_expression)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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

TEST(compiled_booleans_are_self_evaluating)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("#t", env);
    ASSERT(result == ctx.atom_true);
    result = compiled_eval_string("#f", env);
    ASSERT(result == ctx.atom_false);
    PASS();
}

TEST(eval_negative_integer)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("-17", env);
    ASSERT(is_int(result, -17));
    PASS();
}

TEST(eval_true)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("#t", env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_false)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("#f", env);
    ASSERT(result == ctx.atom_false);
    PASS();
}

TEST(eval_rejects_boolean_binding_names)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(let ((#t 1)) #t)", env) == TOK_ERROR);
    ASSERT(eval_string("(let ((#f 1)) #f)", env) == TOK_ERROR);
    ASSERT(eval_string("((lambda (#t) #t) 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(define #t 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(set! #f 1)", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_rejects_boolean_binding_names)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string("(let ((#t 1)) #t)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let ((#f 1)) #f)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("((lambda (#t) #t) 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(define #t 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(set! #f 1)", env) == TOK_ERROR);
    PASS();
}

TEST(eval_quote)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(+ 1 2 3)", env);
    ASSERT(is_int(result, 6));
    PASS();
}

TEST(eval_add_rationals)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(+ 1/2 1/3)", env);
    ASSERT(CELL_TYPE(result) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(result), 5));
    ASSERT(is_int(CELL_CDR(result), 6));
    PASS();
}

TEST(eval_subtract)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(- 10 3 2)", env);
    ASSERT(is_int(result, 5));
    PASS();
}

TEST(eval_subtract_rationals)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(- 1/2 1/3)", env);
    ASSERT(CELL_TYPE(result) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(result), 1));
    ASSERT(is_int(CELL_CDR(result), 6));
    PASS();
}

TEST(eval_multiply)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(* 2 3 4)", env);
    ASSERT(is_int(result, 24));
    PASS();
}

TEST(eval_multiply_exact_complex)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(/ 20 4)", env);
    ASSERT(is_int(result, 5));
    PASS();
}

TEST(eval_divide_exact_complex)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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

TEST(eval_unary_exact_complex_rationals)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(= (- 1/2+3/4i) -1/2-3/4i)",
        "(= (- -1/2-3/4i) 1/2+3/4i)",
        "(= (/ 1/2+3/4i) 8/13-12/13i)",
        "(= (/ -1/2-3/4i) -8/13+12/13i)",
        "(let ((z (make-rectangular 9007199254740993/2 1/3)))"
        "  (and (= (+ z (- z)) 0) (= (* z (/ z)) 1)"
        "       (exact? (- z)) (exact? (/ z))))",
    };
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    PASS();
}

// ============================================================================
// Comparison Tests
// ============================================================================

TEST(eval_exact_rational_comparison_preserves_precision)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(< (make-rectangular 1 1) 2)", env) == TOK_ERROR);
    ASSERT(eval_string("(> 2 (make-rectangular 1 1))", env) == TOK_ERROR);
    PASS();
}

TEST(eval_eq_true)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(= 5 5)", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_eq_false)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(= 5 6)", env);
    ASSERT(is_bool(result, 0));
    PASS();
}

TEST(eval_lt_true)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(< 3 5)", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_lt_false)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(if #t 1 2)", env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(eval_if_false)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(if #f 1 2)", env);
    ASSERT(is_int(result, 2));
    PASS();
}

TEST(eval_cond_first)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(cond (#t 42) (#f 99))", env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(eval_cond_else)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(cond (#f 1) (else 2))", env);
    ASSERT(is_int(result, 2));

    result = eval_string("(let ((else #f)) (cond (else 1) (#t 2)))", env);
    ASSERT(is_int(result, 2));
    result = compiled_eval_string(
        "(let ((else #f)) (cond (else 1) (#t 2)))", env);
    ASSERT(is_int(result, 2));
    result = eval_string(
        "(let () (define else #f) (cond (else 1) (#t 2)))", env);
    ASSERT(is_int(result, 2));
    result = compiled_eval_string(
        "(let () (define else #f) (cond (else 1) (#t 2)))", env);
    ASSERT(is_int(result, 2));

    result = eval_string("(let ((=> #f)) (cond (#t => 1 2)))", env);
    ASSERT(is_int(result, 2));
    result = compiled_eval_string("(let ((=> #f)) (cond (#t => 1 2)))", env);
    ASSERT(is_int(result, 2));
    result = eval_string("(let () (define => #f) (cond (#t => 1 2)))", env);
    ASSERT(is_int(result, 2));
    result = compiled_eval_string(
        "(let () (define => #f) (cond (#t => 1 2)))", env);
    ASSERT(is_int(result, 2));
    PASS();
}

TEST(cond_expand_rejects_recursive_requirement)
{
    const char *src = "(cond-expand (#0=(and #0#) 1) (else 2))";
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(is_int(eval_string(src, env), 2));
    ASSERT(is_int(compiled_eval_string(src, env), 2));
    PASS();
}

// ============================================================================
// Lambda Tests
// ============================================================================

TEST(eval_lambda_call)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("((lambda (x) (+ x 1)) 5)", env);
    ASSERT(is_int(result, 6));
    PASS();
}

TEST(eval_lambda_closure)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        eval_string("(((lambda (x) (lambda (y) (+ x y))) 10) 5)", env);
    ASSERT(is_int(result, 15));
    PASS();
}

TEST(eval_lambda_rest_param)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        eval_string("((lambda (x . rest) (length rest)) 1 2 3 4)", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(eval_lambda_rejects_wrong_arity)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(quote)", env) == TOK_ERROR);
    ASSERT(eval_string("(quote a b)", env) == TOK_ERROR);
    ASSERT(eval_string("(if #t)", env) == TOK_ERROR);
    ASSERT(eval_string("(if #t 1 2 3)", env) == TOK_ERROR);
    ASSERT(eval_string("(begin . 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (begin 1 . 2))))) (m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (let ((x 1) . y) x))))) (m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (let ((x 1 2)) x))))) (m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (let loop ((x 1) . y) x))))) (m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (let loop ((x 1 2)) x))))) (m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (let* ((x 1) . y) x))))) (m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (let* ((x 1 2)) x))))) (m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (letrec ((x 1 2)) x))))) (m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (lambda . 1))))) (m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (define . 1))))) (m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (set! x . 1))))) (m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (define x . 1))))) (m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules () "
                       "((m) (let-syntax . 1))))) (m))",
                       env) == TOK_ERROR);
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
    ASSERT(eval_string("(define-syntax m (syntax-rules ::: ()))", env) ==
           TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules (... ) ((m) 1)))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules (x x) ((m x) 1)))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () (m 1)))", env) ==
           TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () ((#t) 1)))", env) ==
           TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () ((1 x) x)))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () (((a) x) x)))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () ((#(a) x) x)))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () ((... x) x)))",
                       env) == TOK_ERROR);
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
    // A bare use of an ellipsis-bound variable inserts the whole matched
    // list as an expression (R7RS 4.3.2), so this transformer is legal
    ASSERT(is_int(eval_string("(begin (define-syntax m (syntax-rules () "
                              "((m x ...) (+ 40 (car (quote x))))))"
                              "(m 1 2))",
                              env),
                       41));
    // Ellipsis-bound variable may appear under ellipsis at any depth >= 1
    ASSERT(is_int(eval_string("(begin (define-syntax m (syntax-rules () "
                              "((m (x ...) ...) (+ 40 (length (quote x))))))"
                              "(m (1 2 3) (4)))",
                              env),
                       42));
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
    ASSERT(eval_string("(define-syntax m (syntax-rules () ((m x x) x)))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () "
                       "((m (x ...) x) x)))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(define-syntax m (syntax-rules () ((m #(x x)) x)))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(let ((x 1) (x 2)) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(let ((x . 1)) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(let ((x 1 . 2)) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(let ((x 1) . y) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(let loop ((x 1) . y) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(let* ((x . 1)) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(let* ((x 1 . 2)) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(let* ((x 1) . y) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(letrec ((x)) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(letrec ((1 2)) 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(letrec ((x 1) (x 2)) x)", env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax . 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules))) 1)", env) ==
           TOK_ERROR);
    ASSERT(eval_string("(let-syntax ((m (syntax-rules ::: ()))) 1)", env) ==
           TOK_ERROR);
    ASSERT(eval_string("(let-syntax "
                       "((m (syntax-rules () ((m) 1))) "
                       " (m (syntax-rules () ((m) 2)))) "
                       "(m))",
                       env) == TOK_ERROR);
    ASSERT(eval_string("(letrec-syntax . 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(letrec-syntax "
                       "((m (syntax-rules () ((m) 1))) "
                       " (m (syntax-rules () ((m) 2)))) "
                       "(m))",
                       env) == TOK_ERROR);
    PASS();
}

TEST(eval_load_rejects_non_string)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(load 1)", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_load_rejects_non_string)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        eval_string("(load \"/tmp/vesper-load-port-reader-test.scm\")", env);
    remove(path);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(eval_load_rejects_reader_token_sentinel)
{
    const char *path = "/tmp/vesper-load-reader-token-test.scm";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    ASSERT(fwrite(")", 1, 1, f) == 1);
    fclose(f);

    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        eval_string("(load \"/tmp/vesper-load-reader-token-test.scm\")", env);
    remove(path);
    ASSERT(result == TOK_ERROR);
    PASS();
}

// ============================================================================
// Define Tests
// ============================================================================

TEST(eval_define_variable)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    eval_string("(define x 42)", env);
    unsigned result = eval_string("x", env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(cps_define_continuation_propagates_invalid_environment)
{
    unsigned name = atom_from_string("cps-invalid-define");
    tramp.mode = TRAMP_DONE;
    handle_cont_define(store(1), name, store(2), 0);
    ASSERT_EQ(tramp.mode, TRAMP_ERROR);
    tramp.mode = TRAMP_DONE;
    PASS();
}

TEST(eval_define_function)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    eval_string("(define (square x) (* x x))", env);
    unsigned result = eval_string("(square 7)", env);
    ASSERT(is_int(result, 49));
    PASS();
}

TEST(eval_define_recursive)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(let ((x 1) (y 2)) (+ x y))", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(eval_named_let)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let loop ((n 5) (acc 1)) "
        "  (if (= n 0) acc (loop (- n 1) (* acc n))))",
        env);
    ASSERT(is_int(result, 120));

    result = eval_string(
        "(let ((loop 99)) "
        "  (let loop ((n 0)) "
        "    (if (= n 0) loop (loop (- n 1)))))",
        env);
    ASSERT(IS_FUNCTION(result));

    result = eval_string(
        "(let ((loop (lambda (x) (+ x 1)))) "
        "  (let loop ((n (loop 1))) n))",
        env);
    ASSERT(is_int(result, 2));
    PASS();
}

TEST(compiled_named_let)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let loop ((n 5) (acc 1)) "
        "  (if (= n 0) acc (loop (- n 1) (* acc n))))",
        env);
    ASSERT(is_int(result, 120));

    result = compiled_eval_string(
        "(let ((loop 99)) "
        "  (let loop ((n 0)) "
        "    (if (= n 0) loop (loop (- n 1)))))",
        env);
    ASSERT(IS_PAIR(result) && IS_CELL(car(result)) &&
           CELL_TYPE(car(result)) == BT_CLOSURE);

    result = compiled_eval_string(
        "(let ((loop (lambda (x) (+ x 1)))) "
        "  (let loop ((n (loop 1))) n))",
        env);
    ASSERT(is_int(result, 2));
    PASS();
}

TEST(eval_let_accepts_quoted_cyclic_data)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        eval_string("(let ((x '#0=(1 . #0#))) (list? x))", env);
    ASSERT(result == ctx.atom_false);
    PASS();
}

TEST(compiled_let_accepts_quoted_cyclic_data)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        compiled_eval_string("(let ((x '#0=(1 . #0#))) (list? x))", env);
    ASSERT(result == ctx.atom_false);
    PASS();
}

TEST(eval_let_nested)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(let ((x 1)) (let ((y 2)) (+ x y)))", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(eval_empty_let_forms_do_not_leak_internal_defines)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let () (define eval-empty-let-leak 1) eval-empty-let-leak)",
        env);
    ASSERT(is_int(result, 1));
    ASSERT(eval_string("eval-empty-let-leak", env) == TOK_ERROR);

    result = eval_string(
        "(letrec () (define eval-empty-letrec-leak 1) "
        "eval-empty-letrec-leak)",
        env);
    ASSERT(is_int(result, 1));
    ASSERT(eval_string("eval-empty-letrec-leak", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_empty_let_forms_do_not_leak_internal_defines)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let () (define compiled-empty-let-leak 1) "
        "compiled-empty-let-leak)",
        env);
    ASSERT(is_int(result, 1));
    ASSERT(compiled_eval_string("compiled-empty-let-leak", env) == TOK_ERROR);

    result = compiled_eval_string(
        "(letrec () (define compiled-empty-letrec-leak 1) "
        "compiled-empty-letrec-leak)",
        env);
    ASSERT(is_int(result, 1));
    ASSERT(compiled_eval_string("compiled-empty-letrec-leak", env) ==
           TOK_ERROR);
    PASS();
}

TEST(nested_begin_definitions_keep_function_scope)
{
    unsigned (*engines[])(const char *, unsigned) = {
        eval_string, compiled_eval_string};
    for (unsigned i = 0; i < sizeof(engines) / sizeof(engines[0]); i++) {
        GC_GUARD;
        unsigned env = default_environment();
        gc_protect(&env);
        ASSERT(engines[i]("(define outer 7)", env) != TOK_ERROR);
        ASSERT(engines[i](
            "(define (nested x) "
            "  (begin (begin (begin "
            "    (define outer 42) (define (inner) x)))) "
            "  (+ outer (inner)))", env) != TOK_ERROR);
        ASSERT(is_int(engines[i]("(nested 5)", env), 47));
        ASSERT(is_int(engines[i]("(nested 8)", env), 50));
        ASSERT(is_int(engines[i]("outer", env), 7));
        ASSERT(lookup_silent(intern("inner"), env) == TOK_ERROR);

        // The nested definition must shadow the outer syntax binding even
        // while compiling an earlier local procedure that calls it.
        ASSERT(is_int(engines[i](
            "(let-syntax ((h (syntax-rules () ((_) 99)))) "
            "  (letrec ((f (lambda (x) "
            "                (define (g) (h)) "
            "                (begin (begin (define (h) x))) "
            "                (g)))) "
            "    (f 42)))", env), 42));
    }
    PASS();
}

// Empty let-syntax/letrec-syntax are transparent for internal defines:
// definitions splice into the enclosing scope (MIT Scheme semantics)
TEST(eval_empty_syntax_binding_forms_splice_internal_defines)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let-syntax () "
        "  (define eval-empty-let-syntax-def 1) "
        "  eval-empty-let-syntax-def)",
        env);
    ASSERT(is_int(result, 1));
    ASSERT(is_int(eval_string("eval-empty-let-syntax-def", env), 1));

    result = eval_string(
        "(letrec-syntax () "
        "  (define eval-empty-letrec-syntax-def 2) "
        "  eval-empty-letrec-syntax-def)",
        env);
    ASSERT(is_int(result, 2));
    ASSERT(is_int(eval_string("eval-empty-letrec-syntax-def", env), 2));

    ASSERT(is_int(eval_string(
                      "(begin "
                      "  (let-syntax () "
                      "    (define-syntax eval-empty-let-syntax-macro "
                      "      (syntax-rules () "
                      "        ((eval-empty-let-syntax-macro) 1))) "
                      "    0) "
                      "  (eval-empty-let-syntax-macro))",
                      env),
                  1));
    ASSERT(is_int(eval_string(
                      "(begin "
                      "  (letrec-syntax () "
                      "    (define-syntax eval-empty-letrec-syntax-macro "
                      "      (syntax-rules () "
                      "        ((eval-empty-letrec-syntax-macro) 1))) "
                      "    0) "
                      "  (eval-empty-letrec-syntax-macro))",
                      env),
                  1));

    ASSERT(is_int(eval_string("(let () "
                              "  (let-syntax () "
                              "    (define eval-let-syntax-body-def 3)) "
                              "  eval-let-syntax-body-def)",
                              env),
                  3));
    ASSERT(eval_string("eval-let-syntax-body-def", env) == TOK_ERROR);
    PASS();
}

// Empty let-syntax/letrec-syntax are transparent for internal defines:
// definitions splice into the enclosing scope (MIT Scheme semantics)
TEST(compiled_empty_syntax_binding_forms_splice_internal_defines)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let-syntax () "
        "  (define compiled-empty-let-syntax-def 1) "
        "  compiled-empty-let-syntax-def)",
        env);
    ASSERT(is_int(result, 1));
    ASSERT(is_int(compiled_eval_string("compiled-empty-let-syntax-def", env),
                  1));

    result = compiled_eval_string(
        "(letrec-syntax () "
        "  (define compiled-empty-letrec-syntax-def 2) "
        "  compiled-empty-letrec-syntax-def)",
        env);
    ASSERT(is_int(result, 2));
    ASSERT(is_int(compiled_eval_string("compiled-empty-letrec-syntax-def", env),
                  2));

    ASSERT(is_int(compiled_eval_string(
                      "(begin "
                      "  (let-syntax () "
                      "    (define-syntax compiled-empty-let-syntax-macro "
                      "      (syntax-rules () "
                      "        ((compiled-empty-let-syntax-macro) 1))) "
                      "    0) "
                      "  (compiled-empty-let-syntax-macro))",
                      env),
                  1));
    ASSERT(is_int(compiled_eval_string(
                      "(begin "
                      "  (letrec-syntax () "
                      "    (define-syntax compiled-empty-letrec-syntax-macro "
                      "      (syntax-rules () "
                      "        ((compiled-empty-letrec-syntax-macro) 1))) "
                      "    0) "
                      "  (compiled-empty-letrec-syntax-macro))",
                      env),
                  1));

    ASSERT(is_int(compiled_eval_string(
                      "(let () "
                      "  (let-syntax () "
                      "    (define compiled-let-syntax-body-def 3)) "
                      "  compiled-let-syntax-body-def)",
                      env),
                  3));
    ASSERT(compiled_eval_string("compiled-let-syntax-body-def", env) ==
           TOK_ERROR);
    PASS();
}

TEST(eval_letstar)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(let* ((x 1) (y (+ x 1))) (+ x y))", env);
    ASSERT(is_int(result, 3));
    result = eval_string("(let* ((x 1) (x (+ x 1))) x)", env);
    ASSERT(is_int(result, 2));
    result = compiled_eval_string("(let* ((x 1) (x (+ x 1))) x)", env);
    ASSERT(is_int(result, 2));
    PASS();
}

TEST(eval_letrec)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(and #t #t #t)", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_and_one_false)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(and #t #f #t)", env);
    ASSERT(is_bool(result, 0));
    PASS();
}

TEST(eval_or_all_false)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(or #f #f #f)", env);
    ASSERT(is_bool(result, 0));
    PASS();
}

TEST(eval_or_one_true)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(begin 1 2 3)", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(eval_begin_side_effects)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(call/cc (lambda (k) (k 42)))", env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(eval_callcc_escape)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        eval_string("(+ 1 (call/cc (lambda (k) (+ 10 (k 5)))))", env);
    ASSERT(is_int(result, 6));
    PASS();
}

TEST(eval_callcc_accepts_callcc)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(call/cc call/cc)", env);
    ASSERT(IS_CONT(result));
    PASS();
}

TEST(eval_callcc_result_is_procedure)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(procedure? (call/cc call/cc))", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(compiled_callcc_simple)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(call/cc (lambda (k) (k 42)))", env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(compiled_callcc_escape)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        compiled_eval_string("(+ 1 (call/cc (lambda (k) (+ 10 (k 5)))))", env);
    ASSERT(is_int(result, 6));
    PASS();
}

TEST(compiled_callcc_accepts_callcc)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(call/cc call/cc)", env);
    ASSERT(IS_CELL(result) && CELL_TYPE(result) == BT_VMCONT);
    PASS();
}

TEST(compiled_callcc_result_is_procedure)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        compiled_eval_string("(procedure? (call/cc call/cc))", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_can_invoke_vm_continuation)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned cont =
        compiled_eval_string("(call/cc (lambda (k) k))", env);
    ASSERT(IS_CELL(cont) && CELL_TYPE(cont) == BT_VMCONT);
    gc_protect(&cont);

    unsigned name = atom_from_string("saved-vm-continuation");
    gc_protect(&name);
    defvar(name, cont, env);

    unsigned result = eval_string("(saved-vm-continuation 42)", env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(eval_vm_continuation_preserves_multiple_values)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);

    // Capture a VM continuation inside a call-with-values producer.  The
    // first return gives list one value (the continuation itself), so C can
    // recover and install it for the interpreter to invoke later.
    unsigned captured = compiled_eval_string(
        "(call-with-values (lambda () (call/cc (lambda (k) k))) list)", env);
    ASSERT(IS_PAIR(captured) && cdr(captured) == 0);
    gc_protect(&captured);
    unsigned cont = car(captured);
    ASSERT(IS_CELL(cont) && CELL_TYPE(cont) == BT_VMCONT);
    gc_protect(&cont);

    unsigned name = atom_from_string("saved-vm-values-continuation");
    gc_protect(&name);
    defvar(name, cont, env);

    // With no VM currently active, the evaluator uses the temporary-VM path.
    // Re-entering the producer must still deliver zero or several values to
    // call-with-values exactly as a native VM continuation invocation does.
    unsigned result = eval_string("(saved-vm-values-continuation)", env);
    ASSERT(result == 0);

    result = eval_string("(saved-vm-values-continuation 1 2)", env);
    ASSERT(IS_PAIR(result));
    ASSERT(is_int(car(result), 1));
    ASSERT(IS_PAIR(cdr(result)));
    ASSERT(is_int(cadr(result), 2));
    ASSERT(cddr(result) == 0);
    PASS();
}

TEST(interpreted_vm_continuation_call_transfers_active_vm)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);

    // These are legacy interpreted closures.  A compiled caller therefore
    // enters eval_cps from vm_apply before each saved VM continuation is used.
    ASSERT(eval_string(
               "(define invoke-vm-k (lambda (k) (+ 100 (k 41))))", env) !=
           TOK_ERROR);
    ASSERT(eval_string(
               "(define invoke-vm-k-many (lambda (k) (begin (k 7 8) 999)))",
               env) != TOK_ERROR);
    ASSERT(eval_string(
               "(define invoke-vm-k-zero (lambda (k) (begin (k) 999)))", env) !=
           TOK_ERROR);

    // Invoking k abandons both the interpreted call and the rest of the
    // call/cc body.  The restored VM resumes at the continuation of call/cc.
    unsigned result = compiled_eval_string(
        "(+ 1 (call/cc (lambda (k) (+ 10 (invoke-vm-k k)))))", env);
    ASSERT(is_int(result, 42));

    result = compiled_eval_string(
        "(call-with-values"
        "  (lambda () (call/cc (lambda (k) (invoke-vm-k-many k))))"
        "  list)",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(is_int(car(result), 7));
    ASSERT(IS_PAIR(cdr(result)));
    ASSERT(is_int(cadr(result), 8));
    ASSERT(cddr(result) == 0);

    result = compiled_eval_string(
        "(call-with-values"
        "  (lambda () (call/cc (lambda (k) (invoke-vm-k-zero k))))"
        "  list)",
        env);
    ASSERT(result == 0);
    PASS();
}

TEST(compiled_callcc_rejects_wrong_arity)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string("(call/cc)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(call/cc (lambda (k) k) 1)", env) ==
           TOK_ERROR);
    PASS();
}

TEST(eval_call_with_values_accepts_zero_values)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(call-with-values (lambda () (values)) (lambda () 42))",
        env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(eval_call_with_values_zero_values_to_list)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(call-with-values (lambda () (values)) list)",
        env);
    ASSERT(result == 0);
    PASS();
}

TEST(eval_callcc_accepts_multiple_values)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(call-with-values (lambda () (call/cc (lambda (k) (k 1 2)))) list)",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(is_int(car(result), 1));
    ASSERT(is_int(cadr(result), 2));
    ASSERT(cddr(result) == 0);
    PASS();
}

TEST(eval_callcc_accepts_zero_values)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(call-with-values (lambda () (call/cc (lambda (k) (k)))) list)",
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    int initial = get_shadow_stack_top();

    eval_string("(lambda (x) x)", env);

    int final = get_shadow_stack_top();
    ASSERT_EQ(initial, final);
    PASS();
}

TEST(gc_shadow_stack_macro_expansion)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    eval_string("(define-syntax swap! "
                "  (syntax-rules () "
                "    ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))",
                env);
    eval_string("(define x 1) (define y 2)", env);
    int initial = get_shadow_stack_top();
    for (int i = 0; i < 100; i++)
        eval_string("(swap! x y)", env);
    int final = get_shadow_stack_top();
    ASSERT_EQ(initial, final);
    PASS();
}

TEST(gc_shadow_stack_letrec)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);

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
    GC_GUARD;
    gc_protect(&env);

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

TEST(cps_primitive_error_roots_environment_across_gc)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string_gc(
        "(append (iota 100000) '(2 . 3) '())",
        &env);
    ASSERT_EQ(result, TOK_ERROR);
    PASS();
}

TEST(letstar_binding_cell_survives_gc)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string_gc(
        "(let ((junk (make-vector 200000 0))) "
        "  (let* ((x 1) (y 42)) y))",
        &env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(gc_preserves_current_input_string_port)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let ((p (open-input-string \"1)\"))) "
        "  (and (= (read p) 1) "
        "       (char=? (read-char p) #\\))))",
        env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(textual_port_operations_use_utf8_character_boundaries)
{
    const char *src =
        "(let ((p (open-input-string \"λx\"))) "
        "  (and (char=? (peek-char p) (integer->char 955)) "
        "       (char=? (read-char p) (integer->char 955)) "
        "       (string=? (read-string 1 p) \"x\")))";
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(is_bool(eval_string(src, env), 1));
    ASSERT(is_bool(compiled_eval_string(src, env), 1));

    const char *read_string_src =
        "(let ((p (open-input-string \"λx\"))) (read-string 1 p))";
    unsigned result = eval_string(read_string_src, env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "λ");
    result = compiled_eval_string(read_string_src, env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "λ");
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
    GC_GUARD;
    gc_protect(&env);
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

TEST(file_textual_port_operations_peek_utf8_without_consuming)
{
    const char *path = "/tmp/vesper-read-utf8-char-test.scm";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    ASSERT(fwrite("\xF0\x9D\x84\x9Ex1", 1, 6, f) == 6);
    fclose(f);

    const char *src =
        "(let ((p (open-input-file \"/tmp/vesper-read-utf8-char-test.scm\"))) "
        "  (let ((ok (and (char=? (peek-char p) (integer->char 119070)) "
        "                 (char=? (read-char p) (integer->char 119070)) "
        "                 (string=? (read-string 1 p) \"x\") "
        "                 (= (read p) 1)))) "
        "    (close-input-port p) ok))";
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(is_bool(eval_string(src, env), 1));
    ASSERT(is_bool(compiled_eval_string(src, env), 1));
    remove(path);
    PASS();
}

TEST(read_line_rejects_invalid_utf8_file_content)
{
    const char *path = "/tmp/vesper-read-line-invalid-utf8-test.scm";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    ASSERT(fwrite("a\0b\n", 1, 4, f) == 4);
    fclose(f);

    const char *src =
        "(read-line (open-input-file "
        "\"/tmp/vesper-read-line-invalid-utf8-test.scm\"))";
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string(src, env) == TOK_ERROR);
    ASSERT(compiled_eval_string(src, env) == TOK_ERROR);

    f = fopen(path, "wb");
    ASSERT(f != NULL);
    ASSERT(fwrite("\xC0\x80\n", 1, 3, f) == 3);
    fclose(f);
    ASSERT(eval_string(src, env) == TOK_ERROR);
    ASSERT(compiled_eval_string(src, env) == TOK_ERROR);
    remove(path);
    PASS();
}

TEST(string_operations_reject_null_character)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(string #\\null)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(string #\\null)", env) == TOK_ERROR);
    ASSERT(eval_string("(list->string (list #\\null))", env) == TOK_ERROR);
    ASSERT(eval_string("(make-string 1 #\\null)", env) == TOK_ERROR);
    ASSERT(eval_string("(utf8->string #u8(0))", env) == TOK_ERROR);
    ASSERT(eval_string("(let ((p (open-output-string))) "
                       "(write-char #\\null p))",
                       env) == TOK_ERROR);

    const char *path = "/tmp/vesper-read-string-null-test.scm";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    ASSERT(fwrite("a\0b", 1, 3, f) == 3);
    fclose(f);
    const char *src =
        "(read-string 3 (open-input-file "
        "\"/tmp/vesper-read-string-null-test.scm\"))";
    ASSERT(eval_string(src, env) == TOK_ERROR);
    ASSERT(compiled_eval_string(src, env) == TOK_ERROR);
    remove(path);
    PASS();
}

TEST(eval_read_rejects_reader_token_sentinels)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(read (open-input-string \")\"))", env) == TOK_ERROR);
    ASSERT(eval_string("(read (open-input-string \".\"))", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_read_rejects_reader_token_sentinels)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string("(read (open-input-string \")\"))", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string("(read (open-input-string \".\"))", env) ==
           TOK_ERROR);
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
    GC_GUARD;
    gc_protect(&env);
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

TEST(read_bytevector_into_preserves_unread_delimiter)
{
    const char *path = "/tmp/vesper-read-bytevector-into-delimiter-test.bin";
    FILE *f = fopen(path, "wb");
    ASSERT(f != NULL);
    ASSERT(fwrite("1)A", 1, 3, f) == 3);
    fclose(f);

    const char *src =
        "(let ((p (open-binary-input-file "
        "          \"/tmp/vesper-read-bytevector-into-delimiter-test.bin\")) "
        "      (bv (bytevector 0 0))) "
        "  (let ((n (read p)) (got (read-bytevector! bv p))) "
        "    (close-input-port p) "
        "    (and (= n 1) (= got 2) "
        "         (= (bytevector-u8-ref bv 0) 41) "
        "         (= (bytevector-u8-ref bv 1) 65))))";
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(is_bool(eval_string(src, env), 1));
    ASSERT(is_bool(compiled_eval_string(src, env), 1));
    remove(path);
    PASS();
}

TEST(gc_preserves_current_output_string_port)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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

TEST(write_string_uses_utf8_character_indices)
{
    const char *src =
        "(let ((p (open-output-string))) "
        "  (write-string \"λx\" p 1 2) "
        "  (get-output-string p))";
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(src, env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "x");

    result = compiled_eval_string(src, env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "x");
    PASS();
}

TEST(write_char_encodes_utf8_scalars)
{
    const char *src =
        "(let ((p (open-output-string))) "
        "  (write-char (integer->char 955) p) "
        "  (get-output-string p))";
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(src, env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "λ");
    result = compiled_eval_string(src, env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "λ");

    const char *path = "/tmp/vesper-write-utf8-char-test.scm";
    const char *file_src =
        "(let ((out (open-output-file \"/tmp/vesper-write-utf8-char-test.scm\"))) "
        "  (write-char (integer->char 119070) out) "
        "  (close-output-port out) "
        "  (let ((in (open-input-file \"/tmp/vesper-write-utf8-char-test.scm\"))) "
        "    (let ((s (read-string 1 in))) (close-input-port in) s)))";
    result = eval_string(file_src, env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "𝄞");
    result = compiled_eval_string(file_src, env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "𝄞");
    remove(path);
    PASS();
}

TEST(eval_newline_rejects_closed_current_output_port)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        eval_string("(close-input-port (open-output-string))", env);
    ASSERT(result == TOK_ERROR);

    result = eval_string("(close-output-port (open-input-string \"x\"))", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_set_current_port_accepts_closed_port_but_io_rejects_it)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let ((p (open-output-string))) "
        "  (close-output-port p) "
        "  (set-current-output-port! p) "
        "  (eq? p (current-output-port)))",
        env);
    ASSERT(result == ctx.atom_true);
    result = eval_string("(newline)", env);
    ctx.current_output_cell = 0;
    ctx.current_output = stdout;
    ASSERT(result == TOK_ERROR);

    result = eval_string(
        "(let ((p (open-input-string \"x\"))) "
        "  (close-input-port p) "
        "  (set-current-input-port! p) "
        "  (eq? p (current-input-port)))",
        env);
    ASSERT(result == ctx.atom_true);
    result = eval_string("(read-char)", env);
    ctx.current_input_cell = 0;
    ctx.current_input = stdin;
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_write_to_string_escapes_strings)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        eval_string("(write-to-string \"a\\\"b\\\\c\\n\\t\\r\")", env);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "\"a\\\"b\\\\c\\n\\t\\r\"");
    PASS();
}

TEST(eval_write_large_acyclic_list)
{
    const char *src =
        "(let ((p (open-output-string))) "
        "  (let loop ((n 100000) (xs '())) "
        "    (if (= n 0) "
        "        (begin (write xs p) (write-simple xs p) "
        "               (string-length (get-output-string p))) "
        "        (loop (- n 1) (cons 'x xs)))))";
    unsigned eval_env = default_environment();
    unsigned result = eval_string(src, eval_env);
    ASSERT(IS_NUM(result));
    ASSERT_EQ(CELL_ID(result), 400002);

    unsigned compiled_env = default_environment();
    result = compiled_eval_string(src, compiled_env);
    ASSERT(IS_NUM(result));
    ASSERT_EQ(CELL_ID(result), 400002);
    PASS();
}

TEST(write_simple_rejects_cyclic_data)
{
    unsigned cycle = read_expr_from_string("#0=(x . #0#)");
    ASSERT(cycle != TOK_ERROR);
    char *output = NULL;
    size_t output_len = 0;
    FILE *port = open_memstream(&output, &output_len);
    ASSERT(port != NULL);
    ASSERT(!write_simple_obj_port_checked(cycle, port));
    ASSERT(fclose(port) == 0);
    free(output);

    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(let ((p (open-output-string))) "
                       "(write-simple '#0=(x . #0#) p))",
                       env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let ((p (open-output-string))) "
                                "(write-simple '#0=(x . #0#) p))",
                                env) == TOK_ERROR);
    PASS();
}

TEST(compiled_write_to_string_hides_bytecode_closure)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        eval_string_gc("(let ((s '#1=\"abc\")) (gc-flip) (string-length s))",
                       &env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(gc_preserves_labeled_vector)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(apply + '(1 2 3))", env);
    ASSERT(is_int(result, 6));
    PASS();
}

TEST(eval_apply_lambda)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(apply (lambda (x y) (+ x y)) '(3 4))", env);
    ASSERT(is_int(result, 7));
    PASS();
}

TEST(eval_rejects_improper_application)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(+ . 1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_special_form_keywords_respect_lexical_bindings)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(equal? "
        "  (list "
        "    (let ((if list)) (if 1 2)) "
        "    (let ((lambda list)) (lambda 1 2)) "
        "    (let ((set! list)) (set! 1 2)) "
        "    (let ((define list)) (define 1 2)) "
        "    (let ((and list)) (and 1 2)) "
        "    (let ((or list)) (or 1 2)) "
        "    (let ((cond list)) (cond 1 2)) "
        "    (let ((let list)) (let 1 2)) "
        "    (let ((let* list)) (let* 1 2)) "
        "    (let ((letrec list)) (letrec 1 2)) "
        "    (let ((begin list)) (begin 1 2)) "
        "    (let ((quote list)) (quote 1 2)) "
        "    (let ((quasiquote list)) (quasiquote 1 2)) "
        "    (let-syntax ((if (syntax-rules () "
        "                       ((if x y) (list x y))))) "
        "      (if 1 2))) "
        "  '((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) "
        "    (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2)))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_quasiquote_unquotes_vector_element)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(vector-ref `#(a ,(+ 1 2)) 1)", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(eval_quasiquote_respects_shadowed_keywords)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(equal? (let ((unquote 10)) `(a (unquote 1))) "
        "        '(a (unquote 1)))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(equal? (let ((unquote-splicing 10)) "
        "          `(a (unquote-splicing 1))) "
        "        '(a (unquote-splicing 1)))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(let ((quasiquote (lambda (x) x))) (quasiquote 7))", env);
    ASSERT(is_int(result, 7));
    PASS();
}

TEST(eval_quasiquote_rejects_top_level_splicing)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("`(unquote-splicing)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_quasiquote_splicing_preserves_dotted_tail)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(equal? `(a ,@(list 1 2) . tail) '(a 1 2 . tail))", env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_quasiquote_rejects_improper_splice_value)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("`(,@(cons 1 2) x)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_quasiquote_rejects_circular_splice_value)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(let ((x (cons 1 '()))) "
                                  "  (set-cdr! x x) "
                                  "  `(,@x))",
                                  env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_quasiquote_rejects_circular_template)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("`#1=(a . #1#)", env) == TOK_ERROR);
    ASSERT(eval_string("`#1=#(#1#)", env) == TOK_ERROR);
    PASS();
}

TEST(eval_syntax_rules_rejects_circular_pattern_and_template)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string(
               "(begin "
               "  (define-syntax m "
               "    (syntax-rules () ((m #1=(x . #1#)) 1))) "
               "  (m 1))",
               env) == TOK_ERROR);
    ASSERT(eval_string(
               "(begin "
               "  (define-syntax m "
               "    (syntax-rules () ((m) #1=(x . #1#)))) "
               "  (m))",
               env) == TOK_ERROR);
    ASSERT(eval_string(
               "(begin "
               "  (define-syntax m "
               "    (syntax-rules () ((m) #1=#(#1#)))) "
               "  (m))",
               env) == TOK_ERROR);
    PASS();
}

TEST(eval_syntax_rules_rejects_circular_invocation)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string(
               "(begin "
               "  (define-syntax m (syntax-rules () ((m x) x))) "
               "  #1=(m 1 . #1#))",
               env) == TOK_ERROR);
    PASS();
}

TEST(eval_quasiquote_rejects_malformed_subforms)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("`(unquote)", env) == TOK_ERROR);
    ASSERT(eval_string("`(unquote 1 2)", env) == TOK_ERROR);
    ASSERT(eval_string("`(unquote-splicing)", env) == TOK_ERROR);
    ASSERT(eval_string("`(unquote-splicing (list 1) extra)", env) ==
           TOK_ERROR);
    ASSERT(eval_string("`(quasiquote)", env) == TOK_ERROR);
    ASSERT(eval_string("`(quasiquote a b)", env) == TOK_ERROR);
    ASSERT(eval_string("`(a . #((unquote)))", env) == TOK_ERROR);
    ASSERT(eval_string("`(a . #((unquote 1 2)))", env) == TOK_ERROR);
    ASSERT(eval_string(
               "(let-syntax ((m (syntax-rules () "
               "                 ((m) `(unquote 1 2))))) "
               "  (m))",
               env) == TOK_ERROR);
    ASSERT(eval_string(
               "(let-syntax ((m (syntax-rules () "
               "                 ((m) `(quasiquote a b))))) "
               "  (m))",
               env) == TOK_ERROR);
    ASSERT(eval_string("(let ((unquote 10)) `(unquote))", env) != TOK_ERROR);
    PASS();
}

TEST(eval_quasiquote_allows_data_in_unquote_expression)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(equal? `(a ,(quote (unquote 1 2))) "
        "        '(a (unquote 1 2)))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(equal? `(a ,(list (quote (quasiquote a b)))) "
        "        '(a ((quasiquote a b))))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

// ============================================================================
// List Operations
// ============================================================================

TEST(eval_cons)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(cons 1 2)", env);
    ASSERT(IS_PAIR(result));
    ASSERT(is_int(car(result), 1));
    ASSERT(is_int(cdr(result), 2));
    PASS();
}

TEST(eval_car_cdr)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(length '(1 2 3 4 5))", env);
    ASSERT(is_int(result, 5));
    PASS();
}

TEST(eval_rejects_circular_list_operations)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    ASSERT(eval_string("(let ((x (cons 1 '()))) "
                       "  (set-cdr! x x) "
                       "  (call/cc (lambda (k) (apply k x))))",
                       env) == TOK_ERROR);
    PASS();
}

TEST(eval_equal_handles_cycles)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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

TEST(eval_hash_table_handles_cyclic_equal_keys)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let ((h (make-hash-table)) "
        "      (x (cons 1 '())) "
        "      (y (cons 1 '()))) "
        "  (set-cdr! x x) "
        "  (set-cdr! y y) "
        "  (hash-table-set! h x 42) "
        "  (hash-table-ref h y))",
        env);
    ASSERT(is_int(result, 42));

    result = eval_string("(let ((h (make-hash-table)) "
                         "      (x (make-vector 1 #f)) "
                         "      (y (make-vector 1 #f))) "
                         "  (vector-set! x 0 x) "
                         "  (vector-set! y 0 y) "
                         "  (hash-table-set! h x 43) "
                         "  (hash-table-ref h y))",
                         env);
    ASSERT(is_int(result, 43));
    PASS();
}

TEST(hash_table_enumeration_survives_gc_rehash)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    const char *source =
        "(let ((h (make-strong-eq-hash-table))) "
        "  (let loop ((i 0)) "
        "    (if (= i 4000) "
        "        (= (length (hash-table-keys h)) 4000) "
        "        (begin (hash-table-set! h (cons i i) i) "
        "               (loop (+ i 1))))))";
    unsigned result = eval_string_gc(source, &env);
    ASSERT(is_bool(result, 1));

    result = compiled_eval_string(source, env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_append)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(length (append '(1 2) '(3 4 5)))", env);
    ASSERT(is_int(result, 5));
    PASS();
}

TEST(eval_gc_stats_shape)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    result = cdr(result);
    // Interned-symbol count: atom-table slots are never reclaimed, so this
    // is the direct measure of per-expansion gensym leakage.
    ASSERT(CELL_TYPE(result) == BT_CONS);
    ASSERT(is_stat_entry(car(result), "atoms"));
    ASSERT(cdr(result) == 0);
    PASS();
}

TEST(eval_string_to_symbol_preserves_numeric_text)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        eval_string("(symbol? (string->symbol \"123\"))", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_environment_rejects_non_integer_version)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(scheme-report-environment \"5\")", env);
    ASSERT(result == TOK_ERROR);

    result = eval_string("(null-environment 'r5rs)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_null_environment_booleans_are_self_evaluating)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned null_env = eval_string("(null-environment 5)", env);
    ASSERT(null_env != TOK_ERROR);

    unsigned result = eval_string("#t", null_env);
    ASSERT(result == ctx.atom_true);
    result = eval_string("#f", null_env);
    ASSERT(result == ctx.atom_false);

    result = compiled_eval_string("#t", null_env);
    ASSERT(result == ctx.atom_true);
    result = compiled_eval_string("#f", null_env);
    ASSERT(result == ctx.atom_false);
    PASS();
}

TEST(eval_bytevector_rejects_out_of_range_constructor)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(bytevector 256)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_exit_rejects_out_of_range_code)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(exit 9223372036854775807)", env) == TOK_ERROR);
    ASSERT(eval_string("(emergency-exit 9223372036854775807)", env) ==
           TOK_ERROR);
    PASS();
}

TEST(compiled_exit_rejects_out_of_range_code)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string("(exit 9223372036854775807)", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string("(emergency-exit 9223372036854775807)", env) ==
           TOK_ERROR);
    PASS();
}

TEST(eval_make_bytevector_rejects_out_of_range_fill)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(make-bytevector 3 -1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(eval_bytevector_set_rejects_out_of_range)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(abs -1/2)", env);
    ASSERT(CELL_TYPE(result) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(result), 1));
    ASSERT(is_int(CELL_CDR(result), 2));
    PASS();
}

TEST(eval_negate_rational)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(- 1/2)", env);
    ASSERT(CELL_TYPE(result) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(result), -1));
    ASSERT(is_int(CELL_CDR(result), 2));
    PASS();
}

TEST(eval_quotient_int64_min_by_negative_one)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(remainder -9223372036854775808 -1)", env);
    ASSERT(is_int(result, 0));
    PASS();
}

TEST(eval_modulo_int64_min_by_negative_one)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(modulo -9223372036854775808 -1)", env);
    ASSERT(is_int(result, 0));
    PASS();
}

TEST(eval_inexact_to_exact_int64_min)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(inexact->exact -9.223372036854776e18)", env);
    ASSERT(is_int(result, INT64_MIN));
    PASS();
}

TEST(eval_inexact_to_exact_positive_int64_boundary)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(inexact->exact 9.223372036854776e18)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775808");
    free(s);
    PASS();
}

TEST(eval_inexact_to_exact_complex_components)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(= (inexact->exact (make-rectangular 1/3 0.5)) 1/3+1/2i)",
        "(= (inexact->exact (make-rectangular 0.5 1/3)) 1/2+1/3i)",
        "(= (inexact->exact (make-rectangular 9007199254740993 0.5))"
        "   9007199254740993+1/2i)",
        "(let* ((n (expt 10 400))"
        "       (z (inexact->exact (make-rectangular n 0.5))))"
        "  (and (exact? z) (= (real-part z) n) (= (imag-part z) 1/2)))",
        "(= (inexact->exact (make-rectangular 0.1 0.2))"
        "   (make-rectangular (inexact->exact 0.1) (inexact->exact 0.2)))",
    };
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    const char *errors[] = {
        "(inexact->exact (make-rectangular +inf.0 1.0))",
        "(inexact->exact (make-rectangular 1.0 +inf.0))",
        "(inexact->exact (make-rectangular +nan.0 1.0))",
        "(inexact->exact (make-rectangular 1.0 +nan.0))",
    };
    for (unsigned i = 0; i < sizeof(errors) / sizeof(errors[0]); i++) {
        ASSERT(eval_string(errors[i], env) == TOK_ERROR);
        ASSERT(compiled_eval_string(errors[i], env) == TOK_ERROR);
    }
    PASS();
}

TEST(eval_number_to_string_int64_min_radix)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        eval_string("(number->string -9223372036854775808 16)", env);
    ASSERT(CELL_TYPE(result) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(result), "-8000000000000000");
    PASS();
}

TEST(eval_number_to_string_exact_non_int64)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);

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

TEST(eval_radix_rejects_out_of_range_values)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(number->string 10 9223372036854775807)", env) ==
           TOK_ERROR);
    ASSERT(eval_string("(string->number \"10\" 9223372036854775807)", env) ==
           TOK_ERROR);
    PASS();
}

TEST(compiled_radix_rejects_out_of_range_values)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string("(number->string 10 9223372036854775807)",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(string->number \"10\" 9223372036854775807)",
                                env) == TOK_ERROR);
    PASS();
}

TEST(eval_arithmetic_shift_negative_left)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(arithmetic-shift -1 1)", env);
    ASSERT(is_int(result, -2));
    PASS();
}

TEST(eval_arithmetic_shift_int64_min_count)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        eval_string("(arithmetic-shift -8 -9223372036854775808)", env);
    ASSERT(is_int(result, -1));
    PASS();
}

TEST(eval_arithmetic_shift_large_left_promotes)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(arithmetic-shift -1 63)", env);
    ASSERT(is_int(result, INT64_MIN));
    PASS();
}

TEST(eval_rationalize_preserves_large_inexact)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(rationalize 1e100 0.0)", env);
    ASSERT(IS_INEXACT(result));
    ASSERT(to_double(result) == 1e100);
    result = compiled_eval_string("(rationalize 1e100 0.0)", env);
    ASSERT(IS_INEXACT(result));
    ASSERT(to_double(result) == 1e100);
    PASS();
}

TEST(rationalize_respects_exact_interval)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(= (rationalize 1/1000 0) 1/1000)",
        "(= (rationalize 1/1000 1/1000000) 1/1000)",
        "(= (rationalize -1/1000 -1/1000000) -1/1000)",
        "(= (rationalize 9007199254740993 0) 9007199254740993)",
        "(= (rationalize -9223372036854775809 0) -9223372036854775809)",
        "(= (rationalize (expt 10 400) 0) (expt 10 400))",
        "(let ((x (/ 1 (expt 10 400)))) (= (rationalize x 0) x))",
        "(= (rationalize 3/10 1/10) 1/3)",
        "(= (rationalize 7/10 1/10) 2/3)",
        "(= (rationalize 3/2 1/2) 1)",
        "(= (rationalize -3/2 1/2) -1)",
        "(= (rationalize 9/5 1/5) 2)",
        "(= (rationalize 5 10) 0)",
        "(= (rationalize -5 10) 0)",
        "(= (rationalize 0 0) 0)",
        "(inexact? (rationalize 3/10 0.1))",
        "(= (rationalize 3.5 0.0) 3.5)",
        "(= (rationalize 1e-320 0.0) 1e-320)",
        "(= (rationalize 42 +inf.0) 0.0)",
    };
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    ASSERT(eval_string("(rationalize +inf.0 0)", env) == TOK_ERROR);
    ASSERT(eval_string("(rationalize 1 +nan.0)", env) == TOK_ERROR);
    PASS();
}

TEST(complex_finiteness_preserves_exact_components)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(finite? (make-rectangular (expt 10 400) 1))",
        "(finite? (make-rectangular 1 (expt 10 400)))",
        "(finite? (make-rectangular (expt 10 400) 1.0))",
        "(not (infinite? (make-rectangular (expt 10 400) 1)))",
        "(not (nan? (make-rectangular (expt 10 400) 1)))",
        "(infinite? (make-rectangular (expt 10 400) +inf.0))",
        "(not (finite? (make-rectangular (expt 10 400) +inf.0)))",
        "(nan? (make-rectangular (expt 10 400) +nan.0))",
        "(not (finite? (make-rectangular (expt 10 400) +nan.0)))",
    };
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    PASS();
}

TEST(eval_floor_preserves_bignum)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(magnitude -1/2)", env);
    ASSERT(CELL_TYPE(result) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(result), 1));
    ASSERT(is_int(CELL_CDR(result), 2));
    PASS();
}

TEST(eval_magnitude_preserves_bignum)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(magnitude -9223372036854775809)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775809");
    free(s);
    PASS();
}

TEST(eval_complex_large_components_stay_finite)
{
    const char *src =
        "(let ((z (make-rectangular 1e308 1e308))) "
        "  (and (finite? (magnitude z)) "
        "       (finite? (real-part (log z))) "
        "       (finite? (real-part (sqrt z))) "
        "       (finite? (real-part (expt z 1.0)))))";

    unsigned result = eval_string(src, default_environment());
    ASSERT(is_bool(result, 1));

    result = compiled_eval_string(src, default_environment());
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_complex_math_range_and_branch_cuts)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(< (magnitude (- (atan 1+1i)"
        "                 1.0172219678978514+0.40235947810852507i)) 1e-14)",
        "(< (magnitude (- (atan 0+2i)"
        "                 1.5707963267948966+0.5493061443340549i)) 1e-14)",
        "(let ((z (atan 0+1i)))"
        "  (and (= (real-part z) 0) (= (imag-part z) +inf.0)))",
        "(let ((z (atan 0-1i)))"
        "  (and (= (real-part z) 0) (= (imag-part z) -inf.0)))",
        "(= (imag-part (sqrt (make-rectangular -4.0 -0.0))) -2.0)",
        "(= (imag-part (sqrt (make-rectangular -4.0 0.0))) 2.0)",
        "(let ((z (sqrt 1.7e308+1.7e308i)))"
        "  (and (finite? z)"
        "       (< (abs (- (/ (real-part z) 1.4325088230154573e154) 1)) 1e-14)"
        "       (< (abs (- (/ (imag-part z) 5.933645827121221e153) 1)) 1e-14)))",
        "(let ((z (sqrt (make-rectangular +inf.0 1.0))))"
        "  (and (infinite? (real-part z)) (= (imag-part z) 0)))",
        "(let ((z (sqrt (make-rectangular 1.0 +inf.0))))"
        "  (and (infinite? (real-part z)) (infinite? (imag-part z))))",
        "(< (magnitude (- (log 1.7e308+1.7e308i)"
        "                 710.0734104835082+0.7853981633974483i)) 1e-12)",
    };
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    ASSERT(eval_string("(atan 1+1i 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(atan 1 1+1i)", env) == TOK_ERROR);
    PASS();
}

TEST(eval_complex_transcendentals_avoid_intermediate_overflow)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(< (abs (- (imag-part (asin 0.0+1e100i)) 230.95165647996453)) 1e-12)",
        "(finite? (asin 1e300+1e300i))",
        "(finite? (acos 1e300+1e300i))",
        "(< (magnitude (- (tan 1.0+1000.0i) 0+1i)) 1e-14)",
        "(let ((z (exp (make-rectangular 1000.0 0.0))))"
        "  (and (infinite? (real-part z)) (= (imag-part z) 0)))",
        "(let ((z (sin (make-rectangular 0.0 1000.0))))"
        "  (and (= (real-part z) 0) (infinite? (imag-part z))))",
        "(let ((z (cos (make-rectangular 0.0 1000.0))))"
        "  (and (infinite? (real-part z)) (= (imag-part z) 0)))",
        "(> (imag-part (asin (make-rectangular 2.0 0.0))) 0)",
        "(< (imag-part (asin (make-rectangular 2.0 -0.0))) 0)",
        "(> (imag-part (acos (make-rectangular -2.0 -0.0))) 0)",
        "(< (imag-part (acos (make-rectangular -2.0 0.0))) 0)",
        "(let ((z (log -0.0)))"
        "  (and (= (real-part z) -inf.0)"
        "       (< (abs (- (imag-part z) 3.141592653589793)) 1e-14)))",
        "(= (sqrt1pm1 +inf.0) +inf.0)",
        "(< (abs (- (/ (imag-part (sqrt1pm1 1.0+1e-20i))"
        "              3.5355339059327375e-21) 1)) 1e-14)",
        "(let ((z (sqrt1pm1 0.0+1e-20i)))"
        "  (and (< (abs (- (/ (real-part z) 1.25e-41) 1)) 1e-14)"
        "       (< (abs (- (/ (imag-part z) 5e-21) 1)) 1e-14)))",
        "(finite? (sqrt1pm1 1.7e308+1.7e308i))",
        "(= (imag-part (sqrt1pm1 (make-rectangular -2.0 -0.0))) -1.0)",
    };
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    PASS();
}

TEST(eval_stable_complex_helpers_keep_small_components)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(< (abs (- (/ (real-part (log1p 1e-20+1e-20i)) 1e-20) 1)) 1e-14)",
        "(< (abs (- (/ (real-part (log1p 0.0+1e-20i)) 5e-41) 1)) 1e-14)",
        "(< (abs (- (/ (real-part (expm1 0.0+1e-20i)) -5e-41) 1)) 1e-14)",
        "(finite? (log1p 1.7e308+1.7e308i))",
        "(let ((z (expm1 (make-rectangular 1000.0 0.0))))"
        "  (and (= (real-part z) +inf.0) (= (imag-part z) 0)))",
        "(< (abs (- (/ (real-part (log1pexp -50.0+1.0i))"
        "              1.0421079902977286e-22) 1)) 1e-14)",
        "(< (magnitude (- (log1pexp 1e-20+3.141592653589793i)"
        "                 -36.63870900937511+1.570877982991481i)) 1e-13)",
        "(< (magnitude (- (log1pexp -1e-20+3.141592653589793i)"
        "                 -36.63870900937511+1.570714670598312i)) 1e-13)",
    };
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    PASS();
}

TEST(eval_expt_zero_and_complex_range)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(= (expt 0.0+0.0i 2) 0.0)",
        "(= (expt 0.0+0.0i 0) 1.0)",
        "(= (expt 0.0+0.0i 0.0) 1.0)",
        "(= (expt 0.0+0.0i 2+1i) 0.0)",
        "(exact? (expt 0 2+1i))",
        "(inexact? (expt 0.0+0.0i 2))",
        "(= (expt 0 (/ 1 (expt 10 400))) 0)",
        "(let ((z (expt 1.7e308+1.7e308i 0.5)))"
        "  (and (finite? z)"
        "       (< (abs (- (/ (real-part z) 1.4325088230154573e154) 1)) 1e-12)"
        "       (< (abs (- (/ (imag-part z) 5.933645827121221e153) 1)) 1e-12)))",
    };
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    const char *errors[] = {"(expt 0.0+0.0i -1)", "(expt 0.0+0.0i 0+1i)",
                           "(expt 0 -1/3)", "(expt 0.0 +nan.0)"};
    for (unsigned i = 0; i < sizeof(errors) / sizeof(errors[0]); i++) {
        ASSERT(eval_string(errors[i], env) == TOK_ERROR);
        ASSERT(compiled_eval_string(errors[i], env) == TOK_ERROR);
    }
    PASS();
}

TEST(eval_inexact_preserves_complex_arguments)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(let ((z (make-rectangular -4.0 -0.0)))"
        "  (= (imag-part (sqrt (exact->inexact z))) -2.0))",
        "(let* ((z (make-rectangular 1/3 0.5)) (w (exact->inexact z)))"
        "  (and (eqv? (real-part w) 1/3) (= (imag-part w) 0.5)))",
        "(let* ((n (expt 10 400))"
        "       (z (exact->inexact (make-rectangular n 1.0))))"
        "  (eqv? (real-part z) n))",
    };
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    PASS();
}

TEST(eval_complex_number_text_round_trips)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(let* ((z (make-rectangular 1.0 -0.0))"
        "       (w (string->number (number->string z))))"
        "  (and (number? w) (eqv? (imag-part w) -0.0)))",
        "(let* ((z (make-rectangular 1 (expt 10 400)))"
        "       (w (string->number (number->string z)))) (= z w))",
        "(let* ((z (make-rectangular 1 (- (/ 1 (expt 10 400)))))"
        "       (w (string->number (number->string z)))) (= z w))",
        "(let* ((z (make-rectangular +inf.0 1.0))"
        "       (w (string->number (number->string z))))"
        "  (and (= (real-part w) +inf.0) (= (imag-part w) 1.0)))",
        "(let* ((z (make-rectangular 1.0 +nan.0))"
        "       (w (string->number (number->string z))))"
        "  (and (= (real-part w) 1.0) (nan? (imag-part w))))",
        "(let* ((z (make-rectangular +nan.0 -1.0))"
        "       (w (string->number (number->string z))))"
        "  (and (nan? (real-part w)) (= (imag-part w) -1.0)))",
        "(= (string->number \"+INF.0\") +inf.0)",
        "(= (string->number \"+I\") 0+1i)",
        "(nan? (imag-part (string->number \"1+NaN.0i\")))",
        "(let ((s (string->symbol \"+inf.0+1.0i\")) (p (open-output-string)))"
        "  (write s p)"
        "  (eq? s (read (open-input-string (get-output-string p)))))",
    };
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    PASS();
}

TEST(eval_complex_division_scales_finite_components)
{
    const char *src =
        "(let ((z (make-rectangular 1e308 1e308))) "
        "  (let ((inverse (/ z)) "
        "        (identity (/ z z)) "
        "        (small-inverse (/ (make-rectangular 1e-300 1e-300))) "
        "        (mixed (/ (make-rectangular 1e300 -1e300) "
        "                  (make-rectangular 1e-300 1e-300))) "
        "        (infinite-divisor (/ (make-rectangular 1.0 2.0) "
        "                             (make-rectangular (exp 1000) 1.0)))) "
        "    (and (finite? (real-part inverse)) "
        "         (not (= (real-part inverse) 0.0)) "
        "         (= identity 1) "
        "         (finite? (real-part small-inverse)) "
        "         (= (real-part mixed) 0.0) "
        "         (infinite? (imag-part mixed)) "
        "         (= infinite-divisor 0.0))))";

    unsigned result = eval_string(src, default_environment());
    ASSERT(is_bool(result, 1));

    result = compiled_eval_string(src, default_environment());
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_sqrt_preserves_exact_bignum_squares)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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

TEST(eval_sqrt_preserves_exact_very_large_bignum_squares)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let ((x (expt 10 4000))) (= (sqrt x) (expt 10 2000)))", env);
    ASSERT(is_bool(result, 1));

    result = compiled_eval_string(
        "(let ((x (expt 10 4000))) (= (sqrt x) (expt 10 2000)))", env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(eval_sqrt_negative_exact_values_preserve_magnitude)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(not (exact? (sqrt -18014398509481985)))",
        "(exact? (sqrt -18014398777917441))",
        "(= (imag-part (sqrt -18014398777917441)) 134217729)",
        "(= (expt (sqrt -18014398777917441) 2) -18014398777917441)",
        "(= (imag-part (sqrt -9/16)) 3/4)",
        "(exact? (sqrt -9/16))",
        "(= (imag-part (sqrt (- (expt 10 400)))) (expt 10 200))",
        "(exact? (sqrt (- (expt 10 400))))",
        "(finite? (imag-part (sqrt (- (+ (expt 10 400) 1)))))",
        "(> (imag-part (sqrt -9223372036854775808)) 3000000000)",
        "(not (exact? (sqrt -9223372036854775808)))",
        "(= (sqrt -4) (make-rectangular 0 2))",
        "(= (sqrt -4.0) (make-rectangular 0.0 2.0))",
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    PASS();
}

TEST(eval_sqrt_rational_scales_before_conversion)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(let ((x (sqrt (/ 2 (expt 10 400))))) (and (> x 1.4e-200) (< x 1.5e-200)))",
        "(let ((x (sqrt (/ (expt 10 400) 3)))) (and (> x 5.7e199) (< x 5.8e199)))",
        "(let ((x (sqrt (/ (expt 10 4000) (+ (expt 10 4000) 1))))) "
        "  (and (> x 0.99) (<= x 1.0)))",
        "(let ((x (imag-part (sqrt (/ -2 (expt 10 400)))))) "
        "  (and (> x 1.4e-200) (< x 1.5e-200)))",
        "(let ((x (sqrt 2/3))) (and (> x 0.8164) (< x 0.8165)))",
        "(= (sqrt 9/16) 3/4)",
        "(exact? (sqrt 9/16))",
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    PASS();
}

TEST(eval_exact_to_inexact_huge_bignum_overflows_to_infinity)
{
    const char *src =
        "(let ((x (exact->inexact (expt 2 2000)))) "
        "  (and (infinite? x) (not (nan? x))))";
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(is_bool(eval_string(src, env), 1));
    ASSERT(is_bool(compiled_eval_string(src, env), 1));
    PASS();
}

TEST(eval_rational_to_inexact_preserves_mantissa_bits)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *src =
        "(let ((a (+ (expt 2 96) (expt 2 63))) (b (+ (expt 2 96) 1))) "
        "  (and (= (exact->inexact (/ a b)) 1.0000000001164153) "
        "       (= (exact->inexact (/ b a)) 0.9999999998835847) "
        "       (= (exact->inexact (/ (- a) b)) -1.0000000001164153)))";
    ASSERT(eval_string(src, env) == ctx.atom_true);
    ASSERT(compiled_eval_string(src, env) == ctx.atom_true);
    PASS();
}

TEST(eval_exact_to_inexact_huge_rational_stays_finite)
{
    const char *src =
        "(let* ((x (expt 10 4000)) "
        "       (r (/ (+ x 1) (+ x 2))) "
        "       (d (exact->inexact r))) "
        "  (and (not (infinite? d)) (not (nan? d)) (= d 1.0)))";
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(is_bool(eval_string(src, env), 1));
    ASSERT(is_bool(compiled_eval_string(src, env), 1));
    PASS();
}

TEST(eval_string_to_number_radix_bignum)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(string->number \"8000000000000000\" 16)", env);
    ASSERT(CELL_TYPE(result) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(result), 10);
    ASSERT_STR_EQ(s, "9223372036854775808");
    free(s);
    PASS();
}

TEST(eval_numeric_prefixes_share_reader_semantics)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const struct { const char *text; const char *expected; } cases[] = {
        {"#i1+2i", "1.0+2.0i"},
        {"#i1+0i", "1.0+0.0i"},
        {"#i1-0i", "1.0-0.0i"},
        {"#i-0", "-0.0"},
        {"#i+i", "0.0+1.0i"},
        {"#e1.5+2.5i", "3/2+5/2i"},
        {"#e1.234567890123456789+1.0i", "1234567890123456789/1000000000000000000+1i"},
        {"#e1e400+1.0i", "(make-rectangular (expt 10 400) 1)"},
        {"#e1e-400+1.0i", "(make-rectangular (/ 1 (expt 10 400)) 1)"},
        {"#x1e-2i", "30-2i"},
        {"#x#i1e+ai", "30.0+10.0i"},
        {"#b+i", "0+1i"},
        {"#o-7/10+1/2i", "-7/8+1/2i"},
        {"#x#e-10/3+i", "-16/3+1i"},
        {"#d+inf.0", "+inf.0"},
        {"#x+inf.0", "+inf.0"},
        {".5+2i", "0.5+2i"},
        {"#e.5-2.5i", "1/2-5/2i"},
    };
    char src[1024];
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        snprintf(src, sizeof(src),
                 "(let ((s (string->number \"%s\"))"
                 "      (r (read (open-input-string \"%s\"))))"
                 "  (and (eqv? s r) (eqv? s %s)))",
                 cases[i].text, cases[i].text, cases[i].expected);
        ASSERT(eval_string(src, env) == ctx.atom_true);
        ASSERT(compiled_eval_string(src, env) == ctx.atom_true);
    }
    const char *exact_cases[] = {
        "(exact? (string->number \"#e1.5+2.5i\"))",
        "(inexact? (real-part (string->number \"#i1+2i\")))",
        "(inexact? (imag-part (string->number \"#i1+2i\")))",
        "(= (string->number \"1e-2i\" 16) 30-2i)",
        "(= (string->number \"#d12\" 16) 12)",
    };
    for (unsigned i = 0; i < sizeof(exact_cases) / sizeof(exact_cases[0]); i++) {
        ASSERT(eval_string(exact_cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(exact_cases[i], env) == ctx.atom_true);
    }
    const char *invalid[] = {"#e+inf.0+1i", "#e1.0+nan.0i", "#i#i1",
                             "#x#x1", "#", "#i", "1/0", "1+2/0i", "1+-2i"};
    for (unsigned i = 0; i < sizeof(invalid) / sizeof(invalid[0]); i++) {
        snprintf(src, sizeof(src), "(string->number \"%s\")", invalid[i]);
        ASSERT(eval_string(src, env) == ctx.atom_false);
        ASSERT(compiled_eval_string(src, env) == ctx.atom_false);
    }
    PASS();
}

TEST(eval_string_to_number_radix_rejects_invalid)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(string->number \"12abc\" 10)", env);
    ASSERT(result == ctx.atom_false);
    PASS();
}

TEST(eval_complex_radix_round_trips)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(equal? (number->string 30-2i 16) \"1e-2i\")",
        "(equal? (number->string 1/2+3/4i 2) \"1/10+11/100i\")",
        "(equal? (number->string -15/8+9/2i 8) \"-17/10+11/2i\")",
        "(let ((z (make-rectangular (/ (expt 10 100) 3) -7/11)))"
        "  (and (eqv? z (string->number (number->string z 2) 2))"
        "       (eqv? z (string->number (number->string z 8) 8))"
        "       (eqv? z (string->number (number->string z 16) 16))))",
    };
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    ASSERT(eval_string("(number->string 1.0+2i 16)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(number->string 1+2.0i 16)", env) == TOK_ERROR);
    PASS();
}

TEST(eval_integer_to_char_rejects_surrogates)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(integer->char #xd800)", env) == TOK_ERROR);
    ASSERT(eval_string("(integer->char #xdfff)", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_integer_to_char_rejects_surrogates)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string("(integer->char #xd800)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(integer->char #xdfff)", env) == TOK_ERROR);
    PASS();
}

TEST(eval_complex_reader_accepts_implicit_imaginary_unit)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string("(integer? 1e999)", env);
    ASSERT(result == ctx.atom_false);
    PASS();
}

TEST(eval_rational_accessors_reject_infinity)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(numerator 1e999)", env) == TOK_ERROR);
    ASSERT(eval_string("(denominator 1e999)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(numerator 1e999)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(denominator 1e999)", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_integer_predicate_matches_eval)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(integer? 1.0)", env) == ctx.atom_true);
    ASSERT(compiled_eval_string("(integer? 1.0)", env) == ctx.atom_true);
    ASSERT(eval_string("(integer? 1.5)", env) == ctx.atom_false);
    ASSERT(compiled_eval_string("(integer? 1.5)", env) == ctx.atom_false);
    ASSERT(compiled_eval_string("(integer? 1e999)", env) == ctx.atom_false);
    PASS();
}

TEST(eval_exact_rejects_non_numbers)
{
    // R7RS 6.2.6 defines exact?/inexact? over numbers, so a non-number is a
    // type error rather than a #f answer. exact-integer? and friends are
    // genuine type predicates and stay total - see below.
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(exact? '())", env) == TOK_ERROR);
    ASSERT(eval_string("(exact? 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(inexact? 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(exact? 1/2)", env) == ctx.atom_true);
    ASSERT(eval_string("(inexact? 1.0)", env) == ctx.atom_true);
    PASS();
}

TEST(eval_numtower_rejects_non_numbers)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(real-part 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(imag-part 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(magnitude 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(angle 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(exact->inexact 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(inexact->exact 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(make-rectangular 'foo 0)", env) == TOK_ERROR);
    ASSERT(eval_string("(make-polar 'foo 0)", env) == TOK_ERROR);
    ASSERT(eval_string("(rationalize 'foo 1)", env) == TOK_ERROR);
    ASSERT(eval_string("(finite? 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(infinite? 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(nan? 'foo)", env) == TOK_ERROR);
    ASSERT(eval_string("(finite? 1)", env) == ctx.atom_true);
    ASSERT(eval_string("(nan? (/ 0. 0.))", env) == ctx.atom_true);
    PASS();
}

TEST(eval_exact_tiny_complex_imag_part_is_not_zero)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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

TEST(eval_exact_rational_exponent)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    const char *cases[] = {
        "(= (expt 1 9223372036854775807/3) 1)",
        "(= (expt 1 -9223372036854775808/3) 1)",
        "(= (expt 0 9223372036854775807/3) 0)",
        "(= (expt 16 3/2) 64)",
        "(= (expt 16 -3/2) 1/64)",
        "(= (expt 9/4 3/2) 27/8)",
        "(= (expt 9/4 -3/2) 8/27)",
        "(exact? (expt 9/4 -3/2))",
        "(= (expt (expt 513 999) 1/999) 513)",
        "(exact? (expt (expt 513 999) 1/999))",
        "(= (expt (expt 513/512 999) -1/999) 512/513)",
        "(= (expt 1 1/999) 1)",
        "(and (> (sqrt 3) 1) (< (sqrt 3) 2))",
        "(and (> (expt 7 1/3) 1) (< (expt 7 1/3) 2))",
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        ASSERT(eval_string(cases[i], env) == ctx.atom_true);
        ASSERT(compiled_eval_string(cases[i], env) == ctx.atom_true);
    }
    PASS();
}

TEST(eval_math_rejects_non_numbers)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result =
        compiled_eval_string("(let ((x -1073741824)) (/ x -1))", env);
    ASSERT(is_int(result, 1073741824));
    PASS();
}

TEST(compiled_constant_folding_releases_gc_roots)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    for (int i = 0; i < 600; i++) {
        unsigned result = compiled_eval_string("(+ 1 2)", env);
        ASSERT(is_int(result, 3));
    }
    PASS();
}

TEST(compiled_number_predicate_constant_folds)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned expr = read_expr_from_string("(number? 1)");
    ASSERT(expr != TOK_ERROR);

    GC_GUARD;
    gc_protect(&expr);
    gc_protect(&env);
    code_object *code = compile_toplevel(expr, env);
    ASSERT(code != NULL);
    ASSERT_EQ(code_count_opcode(code, OP_NUMBERP), 0);
    ASSERT_EQ(code_count_opcode(code, OP_CALL), 0);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(compiled_lookup_add1_int64_max)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    eval_string("(define x -9223372036854775808)", env);
    eval_string("(define y -1)", env);
    unsigned result = compiled_eval_string("(modulo x y)", env);
    ASSERT(is_int(result, 0));
    PASS();
}

TEST(eval_compiled_parity_bignum_promotion)
{
    ASSERT(eval_compiled_equal(
        "(let ((x 9223372036854775807) "
        "      (y -9223372036854775808)) "
        "  (list (+ x 1) (- y 1) (* x 4) (/ y -1)))"));
    PASS();
}

TEST(eval_compiled_parity_exact_rationals)
{
    ASSERT(eval_compiled_equal(
        "(let ((x 12345678901234567890/7) "
        "      (y 98765432109876543210/11)) "
        "  (list (+ x y) (- y x) (* x y) (/ y x)))"));
    PASS();
}

TEST(eval_compiled_parity_macro_introduced_bindings)
{
    ASSERT(eval_compiled_equal(
        "(let-syntax ((swap-list "
        "              (syntax-rules () "
        "                ((_ a b) "
        "                 (let ((tmp a)) "
        "                   (let ((a b) (b tmp)) "
        "                     (list a b))))))) "
        "  (let ((tmp 'outer) (a 'left) (b 'right)) "
        "    (list tmp (swap-list a b))))"));
    PASS();
}

TEST(compiled_iife_rest_parameter_shadows_stack_local)
{
    ASSERT(eval_compiled_equal(
        "(begin "
        "  (define (f x) ((lambda x x) 1 2)) "
        "  (f 99))"));
    ASSERT(eval_compiled_equal(
        "(begin "
        "  (define (f x) ((lambda (y . x) x) 1 2 3)) "
        "  (f 99))"));
    PASS();
}

TEST(compiled_macro_pattern_roots_survive_rational_gc)
{
    ASSERT(eval_compiled_equal(
        "(let ((x 12345678901234567890/7) "
        "      (y 98765432109876543210/11)) "
        "  (list (+ x y) (- y x) (* x y) (/ y x)))"));
    ASSERT(eval_compiled_equal(
        "(let-syntax ((swap-list "
        "              (syntax-rules () "
        "                ((_ a b) "
        "                 (let ((tmp a)) "
        "                   (let ((a b) (b tmp)) "
        "                     (list a b))))))) "
        "  (let ((tmp 'outer) (a 'left) (b 'right)) "
        "    (list tmp (swap-list a b))))"));
    PASS();
}

TEST(compiled_letrec_tail_call_many_args)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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

TEST(compiled_let_forms_preserve_enclosing_tail_context)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let ((x 0)) "
        "  (let ((ignore (let ((y 1)) (set! x 1)))) x) "
        "  x)",
        env);
    ASSERT(is_int(result, 1));

    result = compiled_eval_string(
        "(let ((x 0)) "
        "  (let ((ignore (let* ((y 1)) (set! x y)))) x) "
        "  x)",
        env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(compiled_cond_arrow_preserves_tail_context)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned expr = read_expr_from_string(
        "(lambda (receiver g) "
        "  (cond (1 => (g)) "
        "        (else 0)))");
    ASSERT(expr != TOK_ERROR);

    GC_GUARD;
    gc_protect(&expr);
    gc_protect(&env);
    code_object *code = compile_toplevel(expr, env);
    ASSERT(code != NULL);
    ASSERT(code_contains_opcode(code, OP_TAILCALL));
    ASSERT_EQ(code_count_opcode(code, OP_TAILCALL), 1);
    PASS();
}

TEST(compiled_string_to_list_allocates_fresh_result)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(begin definitely-unbound-variable 1)",
        env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_multiply_by_zero_preserves_side_effects)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(* \"x\" 1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_divide_by_one_preserves_type_error)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(/ \"x\" 1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_double_not_returns_boolean)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let ((x 42)) (not (not x)))",
        env);
    ASSERT(is_bool(result, 1));
    PASS();
}

TEST(compiled_add1_sub1_preserves_type_error)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(- (+ \"x\" 1) 1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_lookup_add1_sub1_halt_on_type_error)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string("(define fused-add1-error \"x\")", env) !=
           TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(begin (+ fused-add1-error 1) 42)", env) == TOK_ERROR);

    ASSERT(compiled_eval_string("(define fused-sub1-error \"x\")", env) !=
           TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(begin (- fused-sub1-error 1) 42)", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_add1_sub1_support_non_integer_numbers)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned inexact = compiled_eval_string("(let ((x 0)) (+ 1 (exp x)))", env);
    ASSERT(IS_INEXACT(inexact));
    ASSERT(to_double(inexact) == 2.0);

    unsigned rational = compiled_eval_string("(let ((x 1/2)) (+ 1 x))", env);
    ASSERT(CELL_TYPE(rational) == BT_RATIONAL);
    ASSERT(is_int(CELL_CAR(rational), 3));
    ASSERT(is_int(CELL_CDR(rational), 2));

    unsigned complex =
        compiled_eval_string("(let ((x 1+2i)) (+ 1 x))", env);
    ASSERT(CELL_TYPE(complex) == BT_COMPLEX);
    ASSERT(is_int(CELL_CAR(complex), 2));
    ASSERT(is_int(CELL_CDR(complex), 2));

    unsigned sub =
        compiled_eval_string("(let ((x 1.0)) (- x 1))", env);
    ASSERT(IS_INEXACT(sub));
    ASSERT(to_double(sub) == 0.0);

    unsigned logistic_shape = compiled_eval_string(
        "(let ((x 0)) (/ 1 (+ 1 (exp (- x)))))", env);
    ASSERT(IS_INEXACT(logistic_shape));
    ASSERT(to_double(logistic_shape) == 0.5);
    PASS();
}

TEST(compiled_zerop_preserves_type_error)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(= \"x\" 0)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_if_numeq_preserves_type_error)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(if (= \"x\" 1) 2 3)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_if_less_than_preserves_type_error)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(if (< \"x\" 1) 2 3)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_if_other_comparisons_preserve_type_error)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string("(if (> \"x\" 1) 2 3)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(if (<= \"x\" 1) 2 3)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(if (>= \"x\" 1) 2 3)", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_if_constant_branches_preserve_test_effects)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(cdr (append '(a) 1))", env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(compiled_apply_rejects_non_list_final_argument)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(apply cons 1 2)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_apply_rejects_improper_final_list)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(apply + '(1 . 2))", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_length_accepts_string)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(length \"abc\")", env);
    ASSERT(is_int(result, 3));
    result = compiled_eval_string("(length \"A\\x03bb;B\")", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(compiled_length_accepts_vector)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(length '#(1 2 3 4))", env);
    ASSERT(is_int(result, 4));
    PASS();
}

TEST(compiled_length_rejects_number)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(length 1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_listp_rejects_circular_list)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    ASSERT(compiled_eval_string("(let ((x (cons 1 '()))) "
                                "  (set-cdr! x x) "
                                "  (call/cc (lambda (k) (apply k x))))",
                                env) == TOK_ERROR);
    PASS();
}

TEST(compiled_equal_handles_cycles)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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

TEST(compiled_hash_table_handles_cyclic_equal_keys)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let ((h (make-hash-table)) "
        "      (x (cons 1 '())) "
        "      (y (cons 1 '()))) "
        "  (set-cdr! x x) "
        "  (set-cdr! y y) "
        "  (hash-table-set! h x 42) "
        "  (hash-table-ref h y))",
        env);
    ASSERT(is_int(result, 42));

    result = compiled_eval_string("(let ((h (make-hash-table)) "
                                  "      (x (make-vector 1 #f)) "
                                  "      (y (make-vector 1 #f))) "
                                  "  (vector-set! x 0 x) "
                                  "  (vector-set! y 0 y) "
                                  "  (hash-table-set! h x 43) "
                                  "  (hash-table-ref h y))",
                                  env);
    ASSERT(is_int(result, 43));
    PASS();
}

TEST(compiled_vector_ref_rejects_non_vector)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(vector-ref 1 0)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_call_rejects_fixnum_operator)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(1 2)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_rejects_improper_application)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(+ . 1)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_special_form_keywords_respect_lexical_bindings)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(equal? "
        "  (list "
        "    (let ((if list)) (if 1 2)) "
        "    (let ((lambda list)) (lambda 1 2)) "
        "    (let ((set! list)) (set! 1 2)) "
        "    (let ((define list)) (define 1 2)) "
        "    (let ((and list)) (and 1 2)) "
        "    (let ((or list)) (or 1 2)) "
        "    (let ((cond list)) (cond 1 2)) "
        "    (let ((let list)) (let 1 2)) "
        "    (let ((let* list)) (let* 1 2)) "
        "    (let ((letrec list)) (letrec 1 2)) "
        "    (let ((begin list)) (begin 1 2)) "
        "    (let ((quote list)) (quote 1 2)) "
        "    (let ((quasiquote list)) (quasiquote 1 2)) "
        "    (let-syntax ((if (syntax-rules () "
        "                       ((if x y) (list x y))))) "
        "      (if 1 2))) "
        "  '((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) "
        "    (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2)))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(compiled_lambda_optimizations_respect_syntax_binding)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    const char *program =
        "(let-syntax ((lambda (syntax-rules () "
        "                       ((lambda formals body ...) "
        "                        (quote macro-lambda))))) "
        "  (let ((f (lambda (x) x))) "
        "    (f 1)))";

    ASSERT(eval_string(program, env) == TOK_ERROR);
    ASSERT(compiled_eval_string(program, env) == TOK_ERROR);

    program =
        "(let-syntax ((lambda (syntax-rules () "
        "                       ((lambda formals body ...) "
        "                        (quote macro-lambda))))) "
        "  ((lambda (x) x) 1))";

    ASSERT(eval_string(program, env) == TOK_ERROR);
    ASSERT(compiled_eval_string(program, env) == TOK_ERROR);
    PASS();
}

TEST(eval_macro_expansion_rejects_recursive_expansion)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string(
               "(begin "
               "  (define-syntax loop "
               "    (syntax-rules () ((loop) (loop)))) "
               "  (loop))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(begin "
               "  (define-syntax loop "
               "    (syntax-rules () ((loop) (loop)))) "
               "  (loop))",
               env) == TOK_ERROR);

    ASSERT(eval_string(
               "(let-syntax "
               "    ((quote (syntax-rules () "
               "              ((quote x) 'macro-quote)))) "
               "  (quote a))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(let-syntax "
               "    ((quote (syntax-rules () "
               "              ((quote x) 'macro-quote)))) "
               "  (quote a))",
               env) == TOK_ERROR);
    PASS();
}

TEST(compiled_rejects_malformed_lambda)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string("((lambda (x) x))", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("((lambda (x) x) 1 2)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("((lambda (x y . rest) rest) 1)", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string("((lambda args (length args)) 1 2 3)", env) !=
           TOK_ERROR);
    PASS();
}

TEST(compiled_let_lambda_handles_dotted_formals_in_self_reference_check)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let ((f (lambda (x . rest) x))) (f 1 2 3))", env);
    ASSERT(is_int(result, 1));

    result = compiled_eval_string(
        "(let ((f (lambda (x . rest) rest))) (f 1 2 3))", env);
    ASSERT(IS_PAIR(result));
    ASSERT(is_int(car(result), 2));
    ASSERT(is_int(cadr(result), 3));
    PASS();
}

TEST(compiled_rejects_malformed_special_forms)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string("(quote)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(quote a b)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(if #t)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(if #t 1 2 3)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(begin . 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (begin 1 . 2))))) (m))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (let ((x 1) . y) x))))) (m))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (let ((x 1 2)) x))))) (m))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (let loop ((x 1) . y) x))))) (m))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (let loop ((x 1 2)) x))))) (m))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (let* ((x 1) . y) x))))) (m))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (let* ((x 1 2)) x))))) (m))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (letrec ((x 1 2)) x))))) (m))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (lambda . 1))))) (m))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (define . 1))))) (m))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (set! x . 1))))) (m))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (define x . 1))))) (m))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules () "
                                "((m) (let-syntax . 1))))) (m))",
                                env) == TOK_ERROR);
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
    ASSERT(compiled_eval_string("(define-syntax m (syntax-rules ::: ()))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules (... ) ((m) 1)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules (x x) ((m x) 1)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(define-syntax m (syntax-rules () (m 1)))",
                                env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((#t) 1)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((1 x) x)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () (((a) x) x)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((#(a) x) x)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((... x) x)))",
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
    // Bare ellipsis-variable use inserts the whole matched list as an
    // expression (R7RS)
    ASSERT(is_int(compiled_eval_string(
               "(begin (define-syntax m (syntax-rules () "
               "((m x ...) (+ 40 (car (quote x)))))) (m 1 2))",
               env),
           41));
    ASSERT(is_int(compiled_eval_string(
               "(begin (define-syntax m (syntax-rules () "
               "((m (x ...) ...) (+ 40 (length (quote x))))))"
               "(m (1 2 3) (4)))",
               env),
           42));
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
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((m x x) x)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((m (x ...) x) x)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define-syntax m (syntax-rules () ((m #(x x)) x)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let ((x 1) (x 2)) x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let ((x . 1)) x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let ((x 1 . 2)) x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let ((x 1) . y) x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let loop ((x 1) . y) x)", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string("(let* ((x . 1)) x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let* ((x 1 . 2)) x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let* ((x 1) . y) x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(letrec ((x)) x)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(letrec ((1 2)) 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(letrec ((x 1) (x 2)) x)", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax . 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let-syntax ((m (syntax-rules))) 1)", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(let-syntax ((m (syntax-rules ::: ()))) 1)",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(let-syntax "
               "((m (syntax-rules () ((m) 1))) "
               " (m (syntax-rules () ((m) 2)))) "
               "(m))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(letrec-syntax . 1)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(letrec-syntax "
               "((m (syntax-rules () ((m) 1))) "
               " (m (syntax-rules () ((m) 2)))) "
               "(m))",
               env) == TOK_ERROR);
    PASS();
}

TEST(compiled_quasiquote_unquotes_vector_element)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(vector-ref `#(a ,(+ 1 2)) 1)", env);
    ASSERT(is_int(result, 3));
    PASS();
}

TEST(compiled_quasiquote_respects_shadowed_keywords)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(equal? (let ((unquote 10)) `(a (unquote 1))) "
        "        '(a (unquote 1)))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(equal? (let ((unquote-splicing 10)) "
        "          `(a (unquote-splicing 1))) "
        "        '(a (unquote-splicing 1)))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(let ((quasiquote (lambda (x) x))) (quasiquote 7))", env);
    ASSERT(is_int(result, 7));
    PASS();
}

TEST(compiled_quasiquote_rejects_top_level_splicing)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("`(unquote-splicing)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_quasiquote_splicing_preserves_dotted_tail)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(equal? `(a ,@(list 1 2) . tail) '(a 1 2 . tail))", env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(compiled_quasiquote_rejects_improper_splice_value)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("`(,@(cons 1 2) x)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_quasiquote_rejects_circular_splice_value)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(let ((x (cons 1 '()))) "
                                           "  (set-cdr! x x) "
                                           "  `(,@x))",
                                           env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_quasiquote_rejects_circular_template)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string("`#1=(a . #1#)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("`#1=#(#1#)", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_syntax_rules_rejects_circular_pattern_and_template)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string(
               "(begin "
               "  (define-syntax m "
               "    (syntax-rules () ((m #1=(x . #1#)) 1))) "
               "  (m 1))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(begin "
               "  (define-syntax m "
               "    (syntax-rules () ((m) #1=(x . #1#)))) "
               "  (m))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(begin "
               "  (define-syntax m "
               "    (syntax-rules () ((m) #1=#(#1#)))) "
               "  (m))",
               env) == TOK_ERROR);
    PASS();
}

TEST(compiled_syntax_rules_rejects_circular_invocation)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string(
               "(begin "
               "  (define-syntax m (syntax-rules () ((m x) x))) "
               "  #1=(m 1 . #1#))",
               env) == TOK_ERROR);
    PASS();
}

TEST(compiled_legacy_macro_rejects_circular_invocation)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string("(define-macro (m . args) 1)", env) != TOK_ERROR);
    ASSERT(compiled_eval_string("#1=(m . #1#)", env) == TOK_ERROR);
    PASS();
}

TEST(compiled_quasiquote_rejects_malformed_subforms)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string("`(unquote)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("`(unquote 1 2)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("`(unquote-splicing)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("`(unquote-splicing (list 1) extra)", env) ==
           TOK_ERROR);
    ASSERT(compiled_eval_string("`(quasiquote)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("`(quasiquote a b)", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("`(a . #((unquote)))", env) == TOK_ERROR);
    ASSERT(compiled_eval_string("`(a . #((unquote 1 2)))", env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(let-syntax ((m (syntax-rules () "
               "                 ((m) `(unquote 1 2))))) "
               "  (m))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(let-syntax ((m (syntax-rules () "
               "                 ((m) `(quasiquote a b))))) "
               "  (m))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string("(let ((unquote 10)) `(unquote))", env) !=
           TOK_ERROR);
    PASS();
}

TEST(compiled_quasiquote_allows_data_in_unquote_expression)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(equal? `(a ,(quote (unquote 1 2))) "
        "        '(a (unquote 1 2)))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(equal? `(a ,(list (quote (quasiquote a b)))) "
        "        '(a ((quasiquote a b))))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(compiled_local_set_returns_assigned_value)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(call-with-values (lambda () (values)) (lambda () 42))",
        env);
    ASSERT(is_int(result, 42));
    PASS();
}

TEST(compiled_call_with_values_zero_values_to_list)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(call-with-values (lambda () (values)) list)",
        env);
    ASSERT(result == 0);
    PASS();
}

TEST(compiled_callcc_accepts_multiple_values)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(call-with-values (lambda () (call/cc (lambda (k) (k 1 2)))) list)",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(is_int(car(result), 1));
    ASSERT(is_int(cadr(result), 2));
    ASSERT(cddr(result) == 0);
    PASS();
}

TEST(compiled_callcc_accepts_zero_values)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(call-with-values (lambda () (call/cc (lambda (k) (k)))) list)",
        env);
    ASSERT(result == 0);
    PASS();
}

TEST(compiled_call_with_values_rejects_non_producer)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string("(call-with-values '() list)", env);
    ASSERT(result == TOK_ERROR);
    PASS();
}

TEST(compiled_define_syntax_preserves_custom_ellipsis)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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

TEST(compiled_begin_define_syntax_is_visible_to_later_forms)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(begin "
        "  (define-syntax a (syntax-rules () ((a) (b)))) "
        "  (define-syntax b (syntax-rules () ((b) 42))) "
        "  (a))",
        env);
    ASSERT(is_int(result, 42));

    result = compiled_eval_string(
        "(begin "
        "  (if #f (define-syntax hidden "
        "           (syntax-rules () ((hidden) 99))) "
        "      0) "
        "  (let ((hidden 7)) hidden))",
        env);
    ASSERT(is_int(result, 7));

    result = compiled_eval_string(
        "(begin "
        "  (if #t (define-syntax visible "
        "           (syntax-rules () ((visible) 99))) "
        "      0) "
        "  (visible))",
        env);
    ASSERT(is_int(result, 99));

    result = compiled_eval_string(
        "(begin "
        "  (if #f 0 "
        "      (define-syntax visible "
        "        (syntax-rules () ((visible) 100)))) "
        "  (visible))",
        env);
    ASSERT(is_int(result, 100));

    result = compiled_eval_string(
        "(let ((flag #f)) "
        "  (if flag "
        "      (begin "
        "        (define-syntax hidden "
        "          (syntax-rules () ((hidden) 99))) "
        "        0) "
        "      (hidden)))",
        env);
    ASSERT(result == TOK_ERROR);

    result = compiled_eval_string(
        "(if (begin "
        "      (define-syntax visible "
        "        (syntax-rules () ((visible) 13))) "
        "      #f) "
        "    (visible) "
        "    (visible))",
        env);
    ASSERT(is_int(result, 13));

    result = compiled_eval_string(
        "((lambda (x y) y) "
        "  (begin "
        "    (define-syntax m "
        "      (syntax-rules () ((m) 12))) "
        "    0) "
        "  (m))",
        env);
    ASSERT(is_int(result, 12));

    result = compiled_eval_string(
        "(let* ((x (begin "
        "             (define-syntax m "
        "               (syntax-rules () ((m) 14))) "
        "             0)) "
        "       (y (m))) "
        "  y)",
        env);
    ASSERT(is_int(result, 14));

    result = compiled_eval_string(
        "(and (begin "
        "       (define-syntax m "
        "         (syntax-rules () ((m) 21))) "
        "       #t) "
        "     (m))",
        env);
    ASSERT(is_int(result, 21));

    result = compiled_eval_string(
        "(begin "
        "  (and #f "
        "       (begin "
        "         (define-syntax hidden "
        "           (syntax-rules () ((hidden) 23))) "
        "         #t)) "
        "  (hidden))",
        env);
    ASSERT(result == TOK_ERROR);

    result = compiled_eval_string(
        "(or (begin "
        "      (define-syntax m "
        "        (syntax-rules () ((m) 22))) "
        "      #f) "
        "    (m))",
        env);
    ASSERT(is_int(result, 22));

    result = compiled_eval_string(
        "(begin "
        "  (or #t "
        "      (begin "
        "        (define-syntax hidden "
        "          (syntax-rules () ((hidden) 24))) "
        "        #f)) "
        "  (hidden))",
        env);
    ASSERT(result == TOK_ERROR);

    result = compiled_eval_string(
        "(begin "
        "  (cond (#t 1) "
        "        (#t "
        "         (define-syntax hidden "
        "           (syntax-rules () ((hidden) 31))) "
        "         2)) "
        "  (hidden))",
        env);
    ASSERT(result == TOK_ERROR);

    result = compiled_eval_string(
        "(cond ((begin "
        "          (define-syntax m "
        "            (syntax-rules () ((m) 32))) "
        "          #f) "
        "       0) "
        "      (#t (m)))",
        env);
    ASSERT(is_int(result, 32));

    result = compiled_eval_string(
        "(begin "
        "  (cond (#f 0) "
        "        (else "
        "         (define-syntax m "
        "           (syntax-rules () ((m) 33))) "
        "         0)) "
        "  (m))",
        env);
    ASSERT(is_int(result, 33));

    result = compiled_eval_string(
        "(begin "
        "  (cond (#t "
        "         (define-syntax m "
        "           (syntax-rules () ((m) 34))) "
        "         0) "
        "        (else 0)) "
        "  (m))",
        env);
    ASSERT(is_int(result, 34));
    PASS();
}

TEST(compiled_let_syntax_preserves_custom_ellipsis)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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
    GC_GUARD;
    gc_protect(&env);
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

TEST(eval_macro_hygiene_preserves_quoted_introduced_names)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let-syntax "
        "    ((m (syntax-rules () "
        "          ((m) "
        "           (list (let ((x 1)) 'x) "
        "                 ((lambda (x) 'x) 1) "
        "                 (letrec ((x (lambda () 'x))) (x))))))) "
        "  (m))",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(IS_ATOM(car(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(car(result))], "x");
    ASSERT(IS_PAIR(cdr(result)));
    ASSERT(IS_ATOM(cadr(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(cadr(result))], "x");
    ASSERT(IS_PAIR(cddr(result)));
    ASSERT(IS_ATOM(caddr(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(caddr(result))], "x");
    PASS();
}

TEST(compiled_macro_hygiene_preserves_quoted_introduced_names)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let-syntax "
        "    ((m (syntax-rules () "
        "          ((m) "
        "           (list (let ((x 1)) 'x) "
        "                 ((lambda (x) 'x) 1) "
        "                 (letrec ((x (lambda () 'x))) (x))))))) "
        "  (m))",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(IS_ATOM(car(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(car(result))], "x");
    ASSERT(IS_PAIR(cdr(result)));
    ASSERT(IS_ATOM(cadr(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(cadr(result))], "x");
    ASSERT(IS_PAIR(cddr(result)));
    ASSERT(IS_ATOM(caddr(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(caddr(result))], "x");
    PASS();
}

TEST(eval_macro_hygiene_prevents_use_site_capture)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(eval_string(
               "(let-syntax ((m (syntax-rules () ((m) x)))) "
               "  (let ((x 1)) (m)))",
               env) == TOK_ERROR);
    ASSERT(eval_string(
               "(let-syntax ((m (syntax-rules () ((m) g0)))) "
               "  (let ((g0 1)) (m)))",
               env) == TOK_ERROR);
    ASSERT(eval_string(
               "(let-syntax ((m (syntax-rules () ((m) g123)))) "
               "  (let ((g123 1)) (m)))",
               env) == TOK_ERROR);
    ASSERT(eval_string(
               "(let-syntax "
               "    ((m (syntax-rules () ((m) (n)))) "
               "     (n (syntax-rules () ((n) 9)))) "
               "  (m))",
               env) == TOK_ERROR);

    unsigned result = eval_string(
        "(letrec-syntax "
        "    ((m (syntax-rules () ((m) (n)))) "
        "     (n (syntax-rules () ((n) 9)))) "
        "  (m))",
        env);
    ASSERT(is_int(result, 9));
    PASS();
}

TEST(compiled_macro_hygiene_prevents_use_site_capture)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string(
               "(let-syntax ((m (syntax-rules () ((m) x)))) "
               "  (let ((x 1)) (m)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(let-syntax ((m (syntax-rules () ((m) g0)))) "
               "  (let ((g0 1)) (m)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(let-syntax ((m (syntax-rules () ((m) g123)))) "
               "  (let ((g123 1)) (m)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(let-syntax "
               "    ((m (syntax-rules () ((m) (n)))) "
               "     (n (syntax-rules () ((n) 9)))) "
               "  (m))",
               env) == TOK_ERROR);

    unsigned result = compiled_eval_string(
        "(letrec-syntax "
        "    ((m (syntax-rules () ((m) (n)))) "
        "     (n (syntax-rules () ((n) 9)))) "
        "  (m))",
        env);
    ASSERT(is_int(result, 9));
    PASS();
}

TEST(eval_macro_hygiene_respects_shadowed_quote_in_templates)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(equal? "
        "  (let ((x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () "
        "              ((m) (let ((quote list)) (quote x)))))) "
        "      (let ((x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(equal? "
        "  (let ((x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () "
        "              ((m) "
        "               (let ((quote list)) "
        "                 (let ((x 2)) (quote x))))))) "
        "      (m))) "
        "  '(2))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(equal? "
        "  (let ((x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () "
        "              ((m) (let ((quasiquote list)) (quasiquote x)))))) "
        "      (let ((x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(equal? "
        "  (let ((x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () "
        "              ((m) "
        "               (let ((quasiquote list)) "
        "                 (let ((x 2)) (quasiquote x))))))) "
        "      (m))) "
        "  '(2))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(compiled_macro_hygiene_respects_shadowed_quote_in_templates)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(equal? "
        "  (let ((x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () "
        "              ((m) (let ((quote list)) (quote x)))))) "
        "      (let ((x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(equal? "
        "  (let ((x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () "
        "              ((m) "
        "               (let ((quote list)) "
        "                 (let ((x 2)) (quote x))))))) "
        "      (m))) "
        "  '(2))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(equal? "
        "  (let ((x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () "
        "              ((m) (let ((quasiquote list)) (quasiquote x)))))) "
        "      (let ((x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(equal? "
        "  (let ((x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () "
        "              ((m) "
        "               (let ((quasiquote list)) "
        "                 (let ((x 2)) (quasiquote x))))))) "
        "      (m))) "
        "  '(2))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_macro_hygiene_preserves_definition_site_keyword_bindings)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(equal? "
        "  (let ((if list) (x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (if x))))) "
        "      (let ((if (lambda args 'bad)) (x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(equal? "
        "  (let ((begin list) (x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (begin x))))) "
        "      (let ((begin (lambda args 'bad)) (x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(equal? "
        "  (let ((syntax-rules list) (x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (syntax-rules x))))) "
        "      (let ((syntax-rules (lambda args 'bad)) (x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(equal? "
        "  (let ((quote list) (x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (quote x))))) "
        "      (let ((quote list) (x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(equal? "
        "  (let ((quote list) (x 1) (y 2)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (quote x y))))) "
        "      (let ((quote list) (x 3) (y 4)) (m)))) "
        "  '(1 2))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(equal? "
        "  (let ((define list) (x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (define x))))) "
        "      (let ((define (lambda args 'bad)) (x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(equal? "
        "  (let ((set! list) (x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (set! x))))) "
        "      (let ((set! (lambda args 'bad)) (x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(compiled_macro_hygiene_preserves_definition_site_keyword_bindings)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(equal? "
        "  (let ((if list) (x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (if x))))) "
        "      (let ((if (lambda args 'bad)) (x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(equal? "
        "  (let ((begin list) (x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (begin x))))) "
        "      (let ((begin (lambda args 'bad)) (x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(equal? "
        "  (let ((syntax-rules list) (x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (syntax-rules x))))) "
        "      (let ((syntax-rules (lambda args 'bad)) (x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(equal? "
        "  (let ((quote list) (x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (quote x))))) "
        "      (let ((quote list) (x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(equal? "
        "  (let ((quote list) (x 1) (y 2)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (quote x y))))) "
        "      (let ((quote list) (x 3) (y 4)) (m)))) "
        "  '(1 2))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(equal? "
        "  (let ((define list) (x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (define x))))) "
        "      (let ((define (lambda args 'bad)) (x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(equal? "
        "  (let ((set! list) (x 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules () ((m) (set! x))))) "
        "      (let ((set! (lambda args 'bad)) (x 2)) (m)))) "
        "  '(1))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_syntax_rules_unwraps_pattern_vars_in_quoted_templates)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let-syntax ((m (syntax-rules () "
        "                  ((m x) (quote (a . x)))))) "
        "  (m b))",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(IS_ATOM(car(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(car(result))], "a");
    ASSERT(IS_ATOM(cdr(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(cdr(result))], "b");

    result = eval_string(
        "(let-syntax ((m (syntax-rules () "
        "                  ((m x) (quote (x . b)))))) "
        "  (m a))",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(IS_ATOM(car(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(car(result))], "a");
    ASSERT(IS_ATOM(cdr(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(cdr(result))], "b");
    PASS();
}

TEST(compiled_syntax_rules_unwraps_pattern_vars_in_quoted_templates)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let-syntax ((m (syntax-rules () "
        "                  ((m x) (quote (a . x)))))) "
        "  (m b))",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(IS_ATOM(car(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(car(result))], "a");
    ASSERT(IS_ATOM(cdr(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(cdr(result))], "b");

    result = compiled_eval_string(
        "(let-syntax ((m (syntax-rules () "
        "                  ((m x) (quote (x . b)))))) "
        "  (m a))",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(IS_ATOM(car(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(car(result))], "a");
    ASSERT(IS_ATOM(cdr(result)));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(cdr(result))], "b");
    PASS();
}

TEST(eval_macro_hygiene_preserves_quasiquote_data)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(equal? "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) "
        "             (list (let ((x 1)) `x) "
        "                   (let ((x 1)) `(a ,x)) "
        "                   `(+ 1 2)))))) "
        "    (m)) "
        "  '(x (a 1) (+ 1 2)))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(compiled_macro_hygiene_preserves_quasiquote_data)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(equal? "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) "
        "             (list (let ((x 1)) `x) "
        "                   (let ((x 1)) `(a ,x)) "
        "                   `(+ 1 2)))))) "
        "    (m)) "
        "  '(x (a 1) (+ 1 2)))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_syntax_rules_literals_compare_lexical_bindings)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(list "
        "  (let-syntax "
        "      ((m (syntax-rules (lit) "
        "            ((m lit) 'literal) "
        "            ((m x) 'variable)))) "
        "    (let ((lit 1)) "
        "      (m lit))) "
        "  (let ((lit 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules (lit) "
        "              ((m lit) 'literal) "
        "              ((m x) 'variable)))) "
        "      (m lit))) "
        "  (let ((lit 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules (lit) "
        "              ((m lit) 'literal) "
        "              ((m x) 'variable)))) "
        "      (let ((lit 2)) "
        "        (m lit)))))",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(car(result))], "variable");
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(cadr(result))], "literal");
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(caddr(result))], "variable");
    PASS();
}

TEST(compiled_syntax_rules_literals_compare_lexical_bindings)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(list "
        "  (let-syntax "
        "      ((m (syntax-rules (lit) "
        "            ((m lit) 'literal) "
        "            ((m x) 'variable)))) "
        "    (let ((lit 1)) "
        "      (m lit))) "
        "  (let ((lit 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules (lit) "
        "              ((m lit) 'literal) "
        "              ((m x) 'variable)))) "
        "      (m lit))) "
        "  (let ((lit 1)) "
        "    (let-syntax "
        "        ((m (syntax-rules (lit) "
        "              ((m lit) 'literal) "
        "              ((m x) 'variable)))) "
        "      (let ((lit 2)) "
        "        (m lit)))))",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(car(result))], "variable");
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(cadr(result))], "literal");
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(caddr(result))], "variable");
    PASS();
}

TEST(eval_syntax_rules_underscore_literal_is_not_wildcard)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(equal? "
        "  (list "
        "    (let-syntax "
        "        ((m (syntax-rules (_) "
        "              ((m _) 'literal) "
        "              ((m x) 'variable)))) "
        "      (list (m _) (m a))) "
        "    (let ((_ 1)) "
        "      (let-syntax "
        "          ((m (syntax-rules (_) "
        "                ((m _) 'literal) "
        "                ((m x) 'variable)))) "
        "        (list (m _) (let ((_ 2)) (m _)))))) "
        "  '((literal variable) (literal variable)))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(compiled_syntax_rules_underscore_literal_is_not_wildcard)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(equal? "
        "  (list "
        "    (let-syntax "
        "        ((m (syntax-rules (_) "
        "              ((m _) 'literal) "
        "              ((m x) 'variable)))) "
        "      (list (m _) (m a))) "
        "    (let ((_ 1)) "
        "      (let-syntax "
        "          ((m (syntax-rules (_) "
        "                ((m _) 'literal) "
        "                ((m x) 'variable)))) "
        "        (list (m _) (let ((_ 2)) (m _)))))) "
        "  '((literal variable) (literal variable)))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_syntax_rules_treats_booleans_as_literals)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(equal? "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m #t) 'yes) "
        "            ((m x) 'no)))) "
        "    (list (m #t) (m #f) (m 1))) "
        "  '(yes no no))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(let-syntax ((m (syntax-rules () ((m x) #t)))) (m ignored))",
        env);
    ASSERT(result == ctx.atom_true);

    ASSERT(eval_string(
               "(let-syntax ((m (syntax-rules (#t) ((m #t) 'yes)))) "
               "  (m #t))",
               env) == TOK_ERROR);
    ASSERT(eval_string(
               "(let-syntax ((m (syntax-rules #t () ((m) 'yes)))) "
               "  (m))",
               env) == TOK_ERROR);
    PASS();
}

TEST(compiled_syntax_rules_treats_booleans_as_literals)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(equal? "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m #t) 'yes) "
        "            ((m x) 'no)))) "
        "    (list (m #t) (m #f) (m 1))) "
        "  '(yes no no))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(let-syntax ((m (syntax-rules () ((m x) #t)))) (m ignored))",
        env);
    ASSERT(result == ctx.atom_true);

    ASSERT(compiled_eval_string(
               "(let-syntax ((m (syntax-rules (#t) ((m #t) 'yes)))) "
               "  (m #t))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(let-syntax ((m (syntax-rules #t () ((m) 'yes)))) "
               "  (m))",
               env) == TOK_ERROR);
    PASS();
}

TEST(eval_syntax_rules_ellipsis_allows_tail_patterns)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let-syntax "
        "    ((m (syntax-rules () "
        "          ((m (x ... y z)) "
        "           (list (list x ...) y z))))) "
        "  (m (1 2 3)))",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(IS_PAIR(car(result)));
    ASSERT(is_int(caar(result), 1));
    ASSERT(is_int(cadr(result), 2));
    ASSERT(is_int(caddr(result), 3));

    result = eval_string(
        "(equal? "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m #(x ... y z)) "
        "             (list (list x ...) y z))))) "
        "    (list (m #(1 2 3)) (m #(1 2)))) "
        "  '(((1) 2 3) (() 1 2)))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(eval_syntax_rules_vector_template_repeats_compound_elements)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let ((v (let-syntax "
        "             ((m (syntax-rules () "
        "                   ((m (x ...) (y ...)) "
        "                    #((list x y) ...))))) "
        "           (m (1 2) (3 4))))) "
        "  (and (= (vector-length v) 2) "
        "       (= (car (cdr (vector-ref v 0))) 1) "
        "       (= (car (cdr (cdr (vector-ref v 0)))) 3) "
        "       (= (car (cdr (vector-ref v 1))) 2) "
        "       (= (car (cdr (cdr (vector-ref v 1)))) 4)))",
        env);
    ASSERT(result == ctx.atom_true);

    result = eval_string(
        "(let ((v (let-syntax "
        "             ((m (syntax-rules () "
        "                   ((m ((x y) ...)) "
        "                    #((list x y) ...))))) "
        "           (m ((1 2) (3 4)))))) "
        "  (and (= (vector-length v) 2) "
        "       (= (car (cdr (vector-ref v 0))) 1) "
        "       (= (car (cdr (cdr (vector-ref v 0)))) 2) "
        "       (= (car (cdr (vector-ref v 1))) 3) "
        "       (= (car (cdr (cdr (vector-ref v 1)))) 4)))",
        env);
    ASSERT(result == ctx.atom_true);

    ASSERT(eval_string(
               "(let-syntax "
               "    ((m (syntax-rules () "
               "          ((m (x ...) (y ...)) "
               "           (quote ((x y) ...)))))) "
               "  (m (1 2 3) (4 5)))",
               env) == TOK_ERROR);
    ASSERT(eval_string(
               "(let-syntax "
               "    ((m (syntax-rules () "
               "          ((m (x ...) (y ...)) "
               "           #((x y) ...))))) "
               "  (m (1 2 3) (4 5)))",
               env) == TOK_ERROR);
    PASS();
}

TEST(compiled_syntax_rules_ellipsis_allows_tail_patterns)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let-syntax "
        "    ((m (syntax-rules () "
        "          ((m (x ... y z)) "
        "           (list (list x ...) y z))))) "
        "  (m (1 2 3)))",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(IS_PAIR(car(result)));
    ASSERT(is_int(caar(result), 1));
    ASSERT(is_int(cadr(result), 2));
    ASSERT(is_int(caddr(result), 3));

    result = compiled_eval_string(
        "(equal? "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m #(x ... y z)) "
        "             (list (list x ...) y z))))) "
        "    (list (m #(1 2 3)) (m #(1 2)))) "
        "  '(((1) 2 3) (() 1 2)))",
        env);
    ASSERT(result == ctx.atom_true);

    ASSERT(compiled_eval_string(
               "(let-syntax "
               "    ((m (syntax-rules () "
               "          ((m (x ...) (y ...)) "
               "           (quote ((x y) ...)))))) "
               "  (m (1 2 3) (4 5)))",
               env) == TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(let-syntax "
               "    ((m (syntax-rules () "
               "          ((m (x ...) (y ...)) "
               "           #((x y) ...))))) "
               "  (m (1 2 3) (4 5)))",
               env) == TOK_ERROR);
    PASS();
}

TEST(compiled_syntax_rules_vector_template_repeats_compound_elements)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let ((v (let-syntax "
        "             ((m (syntax-rules () "
        "                   ((m (x ...) (y ...)) "
        "                    #((list x y) ...))))) "
        "           (m (1 2) (3 4))))) "
        "  (and (= (vector-length v) 2) "
        "       (= (car (cdr (vector-ref v 0))) 1) "
        "       (= (car (cdr (cdr (vector-ref v 0)))) 3) "
        "       (= (car (cdr (vector-ref v 1))) 2) "
        "       (= (car (cdr (cdr (vector-ref v 1)))) 4)))",
        env);
    ASSERT(result == ctx.atom_true);

    result = compiled_eval_string(
        "(let ((v (let-syntax "
        "             ((m (syntax-rules () "
        "                   ((m ((x y) ...)) "
        "                    #((list x y) ...))))) "
        "           (m ((1 2) (3 4)))))) "
        "  (and (= (vector-length v) 2) "
        "       (= (car (cdr (vector-ref v 0))) 1) "
        "       (= (car (cdr (cdr (vector-ref v 0)))) 2) "
        "       (= (car (cdr (vector-ref v 1))) 3) "
        "       (= (car (cdr (cdr (vector-ref v 1)))) 4)))",
        env);
    ASSERT(result == ctx.atom_true);
    PASS();
}

TEST(syntax_rules_expands_large_flat_template_without_stack_overflow)
{
    static const char prefix[] =
        "(let-syntax ((m (syntax-rules () ((_ ) (quote (";
    static const char suffix[] = ")))))) (m))";
    const size_t count = 100000;
    size_t prefix_len = strlen(prefix);
    size_t suffix_len = strlen(suffix);
    char *src = malloc(prefix_len + 2 * count + suffix_len + 1);
    ASSERT(src != NULL);

    size_t pos = 0;
    memcpy(src + pos, prefix, prefix_len);
    pos += prefix_len;
    for (size_t i = 0; i < count; i++) {
        src[pos++] = 'x';
        src[pos++] = ' ';
    }
    memcpy(src + pos, suffix, suffix_len + 1);

    unsigned eval_env = default_environment();
    unsigned result = eval_string(src, eval_env);
    unsigned length;
    ASSERT(list_length_checked(result, &length, "test"));
    ASSERT_EQ(length, count);
    unsigned compiled_env = default_environment();
    result = compiled_eval_string(src, compiled_env);
    ASSERT(list_length_checked(result, &length, "test"));
    ASSERT_EQ(length, count);
    free(src);
    PASS();
}

TEST(syntax_rules_expands_large_flat_executable_template_without_stack_overflow)
{
    static const char prefix[] =
        "(let-syntax ((m (syntax-rules () ((_ ) (begin ";
    static const char suffix[] = "7))))) (m))";
    const size_t count = 100000;
    size_t prefix_len = strlen(prefix);
    size_t suffix_len = strlen(suffix);
    char *src = malloc(prefix_len + 2 * count + suffix_len + 1);
    ASSERT(src != NULL);

    size_t pos = 0;
    memcpy(src + pos, prefix, prefix_len);
    pos += prefix_len;
    for (size_t i = 0; i < count; i++) {
        src[pos++] = '0';
        src[pos++] = ' ';
    }
    memcpy(src + pos, suffix, suffix_len + 1);

    unsigned eval_env = default_environment();
    ASSERT(is_int(eval_string(src, eval_env), 7));
    unsigned compiled_env = default_environment();
    ASSERT(is_int(compiled_eval_string(src, compiled_env), 7));
    free(src);
    PASS();
}

TEST(syntax_rules_hygienizes_large_begin_definition_sequence)
{
    static const char prefix[] =
        "(let-syntax ((m (syntax-rules () ((_ ) (begin ";
    static const char suffix[] = "))))) (m))";
    const size_t count = 5000;
    size_t prefix_len = strlen(prefix);
    size_t suffix_len = strlen(suffix);
    size_t capacity = prefix_len + count * 18 + suffix_len + 1;
    char *src = malloc(capacity);
    ASSERT(src != NULL);

    size_t pos = 0;
    memcpy(src + pos, prefix, prefix_len);
    pos += prefix_len;
    for (size_t i = 0; i < count; i++)
        pos += (size_t)snprintf(src + pos, capacity - pos,
                                "(define x%zu 0) ", i);
    pos += (size_t)snprintf(src + pos, capacity - pos, "x%zu", count - 1);
    memcpy(src + pos, suffix, suffix_len + 1);

    unsigned eval_env = default_environment();
    ASSERT(is_int(eval_string(src, eval_env), 0));
    unsigned compiled_env = default_environment();
    ASSERT(is_int(compiled_eval_string(src, compiled_env), 0));
    free(src);
    PASS();
}

TEST(large_named_let_binding_sequence_survives_gc)
{
    static const char prefix[] = "(let loop (";
    static const char suffix[] = ") x2999)";
    const size_t count = 3000;
    size_t capacity = strlen(prefix) + count * 18 + strlen(suffix) + 1;
    char *src = malloc(capacity);
    ASSERT(src != NULL);

    size_t pos = 0;
    memcpy(src + pos, prefix, strlen(prefix));
    pos += strlen(prefix);
    for (size_t i = 0; i < count; i++)
        pos += (size_t)snprintf(src + pos, capacity - pos,
                                "(x%zu 0) ", i);
    memcpy(src + pos, suffix, strlen(suffix) + 1);

    unsigned eval_env = default_environment();
    ASSERT(is_int(eval_string(src, eval_env), 0));
    unsigned compiled_env = default_environment();
    ASSERT(is_int(compiled_eval_string(src, compiled_env), 0));
    free(src);
    PASS();
}

TEST(syntax_rules_matches_large_flat_pattern_without_stack_overflow)
{
    static const char prefix[] = "(let-syntax ((m (syntax-rules () ((m ";
    static const char middle[] = ") 7)))) (m ";
    static const char suffix[] = "))";
    const size_t count = 100000;
    size_t prefix_len = strlen(prefix);
    size_t middle_len = strlen(middle);
    size_t suffix_len = strlen(suffix);
    char *src = malloc(prefix_len + 4 * count + middle_len + suffix_len + 1);
    ASSERT(src != NULL);

    size_t pos = 0;
    memcpy(src + pos, prefix, prefix_len);
    pos += prefix_len;
    for (size_t i = 0; i < count; i++) {
        src[pos++] = '_';
        src[pos++] = ' ';
    }
    memcpy(src + pos, middle, middle_len);
    pos += middle_len;
    for (size_t i = 0; i < count; i++) {
        src[pos++] = '0';
        src[pos++] = ' ';
    }
    memcpy(src + pos, suffix, suffix_len + 1);

    unsigned eval_env = default_environment();
    ASSERT(is_int(eval_string(src, eval_env), 7));
    unsigned compiled_env = default_environment();
    ASSERT(is_int(compiled_eval_string(src, compiled_env), 7));
    free(src);
    PASS();
}

TEST(eval_macro_set_target_is_referentially_transparent)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let ((x 0)) "
        "  (list "
        "    (let-syntax "
        "        ((m (syntax-rules () "
        "              ((m) (set! x 1))))) "
        "      (let ((x 2)) "
        "        (m) "
        "        x)) "
        "    x))",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(is_int(car(result), 2));
    ASSERT(is_int(cadr(result), 1));

    result = eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m y) (set! y 1))))) "
        "    (let ((x 2)) "
        "      (m x) "
        "      x)))",
        env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(compiled_macro_set_target_is_referentially_transparent)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let ((x 0)) "
        "  (list "
        "    (let-syntax "
        "        ((m (syntax-rules () "
        "              ((m) (set! x 1))))) "
        "      (let ((x 2)) "
        "        (m) "
        "        x)) "
        "    x))",
        env);
    ASSERT(IS_PAIR(result));
    ASSERT(is_int(car(result), 2));
    ASSERT(is_int(cadr(result), 1));

    result = compiled_eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m y) (set! y 1))))) "
        "    (let ((x 2)) "
        "      (m x) "
        "      x)))",
        env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(eval_macro_hygiene_renames_nested_syntax_rules_templates)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) "
        "             (let ((x 1)) "
        "               (let-syntax "
        "                   ((n (syntax-rules () ((n) x)))) "
        "                 (let ((x 2)) (n)))))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 1));

    result = eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) "
        "             (let ((x 1)) "
        "               (let-syntax "
        "                   ((n (syntax-rules () ((n x) x)))) "
        "                 (n 9))))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 9));

    ASSERT(eval_string(
               "(define-syntax let "
               "  (syntax-rules () "
               "    ((let name ((var init) ...) body ...) "
               "     (letrec ((name (lambda (var ...) body ...))) "
               "       (name init ...))) "
               "    ((let ((var init) ...) body ...) "
               "     ((lambda (var ...) body ...) init ...))))",
               env) != TOK_ERROR);
    result = eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) "
        "             (let loop ((x 1)) "
        "               (let-syntax "
        "                   ((n (syntax-rules () ((n) x)))) "
        "                 (let ((x 2)) (n)))))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(compiled_macro_hygiene_renames_nested_syntax_rules_templates)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) "
        "             (let ((x 1)) "
        "               (let-syntax "
        "                   ((n (syntax-rules () ((n) x)))) "
        "                 (let ((x 2)) (n)))))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 1));

    result = compiled_eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) "
        "             (let ((x 1)) "
        "               (let-syntax "
        "                   ((n (syntax-rules () ((n x) x)))) "
        "                 (n 9))))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 9));

    ASSERT(eval_string(
               "(define-syntax let "
               "  (syntax-rules () "
               "    ((let name ((var init) ...) body ...) "
               "     (letrec ((name (lambda (var ...) body ...))) "
               "       (name init ...))) "
               "    ((let ((var init) ...) body ...) "
               "     ((lambda (var ...) body ...) init ...))))",
               env) != TOK_ERROR);
    result = compiled_eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) "
        "             (let loop ((x 1)) "
        "               (let-syntax "
        "                   ((n (syntax-rules () ((n) x)))) "
        "                 (let ((x 2)) (n)))))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 1));

    result = compiled_eval_string(
        "(let-syntax ((k (syntax-rules () ((k) 1)))) "
        "  (let-syntax "
        "      ((outer (syntax-rules () "
        "                ((outer) "
        "                 (let-syntax "
        "                     ((inner (syntax-rules () ((inner) k)))) "
        "                   0))))) "
        "    (outer)))",
        env);
    ASSERT(is_int(result, 0));

    result = compiled_eval_string(
        "(let-syntax ((k (syntax-rules () ((k) 1)))) "
        "  (let-syntax "
        "      ((outer (syntax-rules () "
        "                ((outer) "
        "                 (let-syntax "
        "                     ((inner (syntax-rules () ((inner) (k))))) "
        "                   (inner)))))) "
        "    (outer)))",
        env);
    ASSERT(is_int(result, 1));

    result = compiled_eval_string(
        "(letrec-syntax "
        "    ((m (syntax-rules () ((m) (n)))) "
        "     (n (syntax-rules () ((n) 7)))) "
        "  (m))",
        env);
    ASSERT(is_int(result, 7));

    result = compiled_eval_string(
        "(let-syntax ((n (syntax-rules () ((n) 1)))) "
        "  (letrec-syntax "
        "      ((m (syntax-rules () ((m) (n)))) "
        "       (n (syntax-rules () ((n) 7)))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 7));

    PASS();
}

TEST(eval_macro_define_target_is_hygienic)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) (define x 1))))) "
        "    (m) "
        "    x))",
        env);
    ASSERT(is_int(result, 0));

    result = eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) (begin (define x 1) x))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 1));

    result = eval_string(
        "(let ((x 10)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m y) (begin (define (f x) y) (f 1)))))) "
        "    (m x)))",
        env);
    ASSERT(is_int(result, 10));

    result = eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) (let () (define x 1) x))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 1));

    result = eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) (let* () (define x 1) x))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 1));

    ASSERT(eval_string(
               "(define-syntax let "
               "  (syntax-rules () "
               "    ((let name ((var init) ...) body ...) "
               "     (letrec ((name (lambda (var ...) body ...))) "
               "       (name init ...))) "
               "    ((let ((var init) ...) body ...) "
               "     ((lambda (var ...) body ...) init ...))))",
               env) != TOK_ERROR);
    result = eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) (let loop () (define x 1) x))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(compiled_macro_define_target_is_hygienic)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    unsigned result = compiled_eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) (define x 1))))) "
        "    (m) "
        "    x))",
        env);
    ASSERT(is_int(result, 0));

    result = compiled_eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) (begin (define x 1) x))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 1));

    result = compiled_eval_string(
        "(let ((x 10)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m y) (begin (define (f x) y) (f 1)))))) "
        "    (m x)))",
        env);
    ASSERT(is_int(result, 10));

    result = compiled_eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) (let () (define x 1) x))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 1));

    result = compiled_eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) (let* () (define x 1) x))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 1));

    ASSERT(eval_string(
               "(define-syntax let "
               "  (syntax-rules () "
               "    ((let name ((var init) ...) body ...) "
               "     (letrec ((name (lambda (var ...) body ...))) "
               "       (name init ...))) "
               "    ((let ((var init) ...) body ...) "
               "     ((lambda (var ...) body ...) init ...))))",
               env) != TOK_ERROR);
    result = compiled_eval_string(
        "(let ((x 0)) "
        "  (let-syntax "
        "      ((m (syntax-rules () "
        "            ((m) (let loop () (define x 1) x))))) "
        "    (m)))",
        env);
    ASSERT(is_int(result, 1));
    PASS();
}

TEST(compiled_macro_thunk_captures_stack_local)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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

TEST(compiled_binding_initializer_closures_capture_stack_locals)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(is_int(
        compiled_eval_string(
            "(begin "
            "  (define make-let-capture "
            "    (lambda (x) (let ((f (lambda () x))) f))) "
            "  ((make-let-capture 42)))",
            env),
        42));

    ASSERT(is_int(
        compiled_eval_string(
            "(begin "
            "  (define make-letstar-capture "
            "    (lambda (x) (let* ((f (lambda () x))) f))) "
            "  ((make-letstar-capture 43)))",
            env),
        43));

    ASSERT(is_int(
        compiled_eval_string(
            "(begin "
            "  (define make-letrec-capture "
            "    (lambda (x) (letrec ((f (lambda () x))) f))) "
            "  ((make-letrec-capture 44)))",
            env),
        44));

    unsigned shadowed_quote_result = compiled_eval_string(
        "(begin "
        "  (define make-shadowed-quote-capture "
        "    (lambda (x) "
        "      (let ((quote list)) "
        "        (lambda () (quote x))))) "
        "  ((make-shadowed-quote-capture 45)))",
        env);
    ASSERT(IS_PAIR(shadowed_quote_result));
    ASSERT(is_int(car(shadowed_quote_result), 45));
    ASSERT(cdr(shadowed_quote_result) == 0);

    PASS();
}

TEST(eval_calls_bytecode_closure_with_stack_locals)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
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
// Tail recursion modulo cons
// ============================================================================
// Result-only tests would pass for the wrong reason: a body that was never
// transformed still computes the right list, just with a frame per element.
// These look at what was emitted instead.

static code_object *trmc_compile(const char *src, unsigned env)
{
    GC_GUARD;
    gc_protect(&env);
    FILE *old_stdin = stdin;
    FILE *f = fmemopen((void *)src, strlen(src), "r");
    if (!f)
        return NULL;
    stdin = f;
    reader_reset_labels();
    unsigned expr = read_obj();
    fclose(f);
    stdin = old_stdin;
    if (expr == TOK_ERROR)
        return NULL;
    gc_protect(&expr);
    return compile_toplevel(expr, env);
}

// Walks the code object and everything nested under it; the transformed body
// is a child of the toplevel code, not the toplevel code itself.
static bool code_tree_has_opcode(const code_object *code, unsigned op)
{
    if (!code)
        return false;
    for (unsigned ip = 0; ip < code->code_len;) {
        unsigned size = instruction_size(code->code[ip]);
        if (size == 0 || size > code->code_len - ip)
            break;
        if (code->code[ip] == op)
            return true;
        ip += size;
    }
    for (unsigned i = 0; i < code->children_len; i++) {
        if (code_tree_has_opcode(code->children[i], op))
            return true;
    }
    return false;
}

// The transformed body itself, not the enclosing lambda: the outer lambda's
// own call into the loop is an ordinary tail call and stays one.
static const code_object *code_tree_find_trmc(const code_object *code)
{
    if (!code)
        return NULL;
    if (code->trmc_mode != TRMC_MODE_NONE)
        return code;
    for (unsigned i = 0; i < code->children_len; i++) {
        const code_object *found = code_tree_find_trmc(code->children[i]);
        if (found)
            return found;
    }
    return NULL;
}

static bool code_has_opcode(const code_object *code, unsigned op)
{
    if (!code)
        return false;
    for (unsigned ip = 0; ip < code->code_len;) {
        unsigned size = instruction_size(code->code[ip]);
        if (size == 0 || size > code->code_len - ip)
            return false;
        if (code->code[ip] == op)
            return true;
        ip += size;
    }
    return false;
}

static bool code_tree_has_trmc_mode(const code_object *code, unsigned mode)
{
    if (!code)
        return false;
    if (code->trmc_mode == mode)
        return true;
    for (unsigned i = 0; i < code->children_len; i++) {
        if (code_tree_has_trmc_mode(code->children[i], mode))
            return true;
    }
    return false;
}

// The loop jump must clear TRMC_INIT. Targeting ip 0 would re-run it and
// discard the accumulator on every iteration.
static bool code_tree_jump_targets_zero(const code_object *code)
{
    if (!code)
        return false;
    for (unsigned ip = 0; ip < code->code_len;) {
        unsigned size = instruction_size(code->code[ip]);
        if (size == 0 || size > code->code_len - ip)
            break;
        if ((code->code[ip] == OP_JUMP || code->code[ip] == OP_RECURSE) &&
            code->code[ip + 1] == 0)
            return true;
        ip += size;
    }
    for (unsigned i = 0; i < code->children_len; i++) {
        if (code_tree_jump_targets_zero(code->children[i]))
            return true;
    }
    return false;
}

// letrec, not named let: the loop optimization these tests depend on is set
// up by compile_letrec, and the C named-let path never arms it. In a full
// interpreter the stdlib's own let macro expands a named let into exactly
// this letrec, but test_eval runs without the stdlib.
#define TRMC_BUILD                                                             \
    "(define trmc-build (lambda (n)"                                           \
    "  (letrec ((loop (lambda (k)"                                             \
    "    (if (= k 0) '() (cons k (loop (- k 1)))))))"                          \
    "    (loop n))))"

TEST(vm_eval_runs_in_the_calling_vm)
{
    // eval used to compile its expression and run it in a second vm_state on
    // the C stack, reached through main.c's eval callback. Continuations do
    // not cross that boundary: one captured out here and invoked in there was
    // restored into the inner VM, which ran the rest of the program and then
    // returned normally into the outer VM's stale state, so the tail of the
    // program ran twice. Running the expression in the calling VM makes both
    // directions ordinary frames. (No stdlib here, so no guard; call/cc is
    // engine-level, and eval no longer needs the callback at all - in this
    // harness it used to fail with "eval: callback not set".)
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);

    // Captured outside, invoked inside: the value lands in the right frame,
    // 1 + 41 rather than a stale 1 + (1 + 41).
    unsigned r = compiled_eval_string("(define eval-escape-k #f)", env);
    ASSERT(r != TOK_ERROR);
    r = compiled_eval_string(
        "(+ 1 (call/cc (lambda (k) (set! eval-escape-k k)"
        "  (eval '(eval-escape-k 41) (interaction-environment)))))",
        env);
    ASSERT(IS_NUM(r));
    ASSERT_EQ(CELL_ID(r), 42);

    // Captured inside, re-entered after eval has returned: its frames run
    // back through the frame eval pushed instead of into an inner HALT.
    r = compiled_eval_string(
        "(begin (define eval-reentry-k #f) (define eval-reentry-n 0))", env);
    ASSERT(r != TOK_ERROR);
    r = compiled_eval_string(
        "(begin"
        "  (eval '(begin (call/cc (lambda (k) (set! eval-reentry-k k))) 'done)"
        "        (interaction-environment))"
        "  (set! eval-reentry-n (+ eval-reentry-n 1))"
        "  (if (< eval-reentry-n 3) (eval-reentry-k #f))"
        "  eval-reentry-n)",
        env);
    ASSERT(IS_NUM(r));
    ASSERT_EQ(CELL_ID(r), 3);

    // Applied as a first-class procedure it takes the same path; it used to
    // fall through to apply_primitive_argv, which rejects it.
    r = compiled_eval_string(
        "(apply eval (cons '(+ 1 2) (cons (interaction-environment) '())))",
        env);
    ASSERT(IS_NUM(r));
    ASSERT_EQ(CELL_ID(r), 3);
    PASS();
}

TEST(code_object_registry_survives_heavy_churn)
{
    // Membership has to stay correct across repeated register/free cycles,
    // which is how the registry is actually used - every compile makes code
    // objects and the GC sweeps them. A wrong answer here is not an error but
    // silent corruption: code_register skips an object it believes is already
    // registered, so the GC never traces it.
    //
    // This does not reproduce the tombstone saturation that code_set_add
    // guards against. Insertions reuse tombstones opportunistically, and I
    // could not construct a workload that fills the table - the guard is
    // there because open addressing with tombstones needs it, not because a
    // failing case is on record.
    for (unsigned round = 0; round < 200; round++) {
        code_object *batch[64];
        for (unsigned i = 0; i < 64; i++) {
            batch[i] = code_new();
            ASSERT(batch[i] != NULL);
            code_emit(batch[i], OP_RETURN);
            ASSERT(code_object_is_registered(batch[i]));
        }
        for (unsigned i = 0; i < 64; i++) {
            ASSERT(code_object_is_registered(batch[i]));
            code_free(batch[i]);
        }
    }
    code_object *live = code_new();
    ASSERT(live != NULL);
    ASSERT(code_object_is_registered(live));
    code_free(live);
    PASS();
}

TEST(code_object_stays_registered_when_the_index_cannot_grow)
{
    // The membership table is an index over code_object_registry, not the
    // registry itself. If its rebuild allocation fails, entries go missing
    // from the index while the objects are still registered and reachable.
    // A miss then has to mean "ask the list", not "not registered" - the GC
    // decides what to sweep through this predicate, so answering no about a
    // live code object collects something still in use.
    enum { N = 4000 };
    static code_object *objs[N];
    unsigned unregistered_while_incomplete = 0;

    // Arm after startup so the table already exists; the interesting path is
    // a populated index that loses entries, not the empty-table case.
    code_set_force_alloc_failure(true);
    unsigned made = 0;
    for (unsigned i = 0; i < N; i++) {
        objs[i] = code_new();
        if (!objs[i])
            break;
        made++;
        code_emit(objs[i], OP_RETURN);
        if (!code_object_is_registered(objs[i]))
            unregistered_while_incomplete++;
    }

    // Once the allocation can succeed again the next registration rebuilds
    // the index from the registry list, recovering everything it dropped.
    code_set_force_alloc_failure(false);
    code_object *trigger = code_new();
    unsigned missing_after_recovery = 0;
    if (trigger) {
        code_emit(trigger, OP_RETURN);
        for (unsigned i = 0; i < made; i++) {
            if (!code_object_is_registered(objs[i]))
                missing_after_recovery++;
        }
    }

    unsigned still_registered_after_free = 0;
    for (unsigned i = 0; i < made; i++) {
        code_free(objs[i]);
        if (code_object_is_registered(objs[i]))
            still_registered_after_free++;
    }
    if (trigger)
        code_free(trigger);

    // Assert only once the seam is disarmed: ASSERT returns from the test,
    // and leaving it armed would break every test that follows.
    ASSERT(made == N);
    ASSERT(trigger != NULL);
    ASSERT_EQ(0, unregistered_while_incomplete);
    ASSERT_EQ(0, missing_after_recovery);
    ASSERT_EQ(0, still_registered_after_free);
    PASS();
}

TEST(trmc_transforms_cons_over_a_self_tail_call)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // HOLE mode requires stable primitives as well as a capture-free body.
    mark_immutable_environment(env);
    code_object *code = trmc_compile(TRMC_BUILD, env);
    ASSERT(code != NULL);
    ASSERT(code_tree_has_opcode(code, OP_TRMC_INIT));
    ASSERT(code_tree_has_opcode(code, OP_TRMC_APPEND));
    ASSERT(code_tree_has_trmc_mode(code, TRMC_MODE_HOLE));
    // A capture-free body takes the cheaper mode, not the general one.
    ASSERT(!code_tree_has_opcode(code, OP_TRMC_PUSH));
    PASS();
}

TEST(trmc_loop_jump_clears_the_accumulator_init)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    code_object *code = trmc_compile(TRMC_BUILD, env);
    ASSERT(code != NULL);
    ASSERT(code_tree_has_opcode(code, OP_TRMC_INIT));
    ASSERT(!code_tree_jump_targets_zero(code));
    PASS();
}

TEST(compiled_recursive_calls_respect_rebinding)
{
    const char *cases[] = {
        "(let ((saved #f) (first #f) (phase 0)) "
        "  (letrec ((f (lambda (n) (if (= n 0) '() (cons n (f (- n 1))))))) "
        "    (let ((old f)) "
        "      (set! f (lambda (n) (call/cc (lambda (k) (set! saved k) '(a))))) "
        "      (let ((answer (old 2))) "
        "        (if (= phase 0) "
        "            (begin (set! first answer) (set! phase 1) (saved '(b))) "
        "            (list first answer))))))",
        "(letrec ((f (lambda (n) (if (= n 0) 0 "
        "  (begin (set! f (lambda (n) 99)) (f (- n 1))))))) (f 2))",
        "(letrec ((f (lambda (n) (if (= n 0) 0 (f (- n 1)))))) "
        "  (let ((old f)) (set! f (lambda (n) 99)) (old 2)))",
        "(letrec ((f (lambda (n) (if (= n 0) 0 "
        "  (+ (begin (set! f (lambda (n) 99)) n) (f (- n 1))))))) (f 2))",
        "(letrec ((f (lambda (f) (f 2)))) (f (lambda (n) 99)))",
        "(let ((other #f)) "
        "  (let ((make (lambda (x) "
        "    (letrec ((f (lambda (n) (if (= n 0) x "
        "      (begin (set! f other) (f (- n 1))))))) f)))) "
        "    (let ((one (make 1)) (two (make 2))) (set! other two) (one 1))))",
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++)
        ASSERT(eval_compiled_equal(cases[i]));
    PASS();
}

TEST(compiled_primitive_calls_respect_rebinding)
{
    const char *cases[] = {
        "(let ((f (lambda (x) (+ x 1)))) (set! + (lambda (a b) 42)) (f 2))",
        "(let ((f (lambda () (+ 2 3)))) (set! + (lambda (a b) 42)) (f))",
        "(let ((f (lambda (x) (car x)))) (set! car (lambda (x) 42)) (f '(1)))",
        "(let ((f (lambda (x) (list x)))) (set! list (lambda (x) 42)) (f 1))",
        "(let ((f (lambda (x) (vector-ref x 0)))) "
        "  (set! vector-ref (lambda (x i) 42)) (f '#(1)))",
        "(let ((f (lambda (x) (call/cc x)))) "
        "  (set! call/cc (lambda (x) 42)) (f (lambda (k) 1)))",
        "(let ((f (lambda (x) (car x)))) (set! car length) (f '(1 2 3)))",
        "(let ((count 0)) "
        "  (let ((f (lambda (x) (+ x 1)))) "
        "    (set! + (lambda (a b) (set! count (- count -1)) 42)) "
        "    (list (f 1) count)))",
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        if (!eval_compiled_equal(cases[i])) {
            fprintf(stderr, "rebound primitive case %zu: %s\n", i, cases[i]);
            ASSERT(false);
        }
    }
    PASS();
}

TEST(compiled_rebinding_preserves_vm_evaluation_order)
{
    // Vesper's VM evaluates arguments before the callee; the CPS engine
    // evaluates the callee first. Compare transformed and ordinary VM calls
    // here because Scheme permits either order, and the mutation observes it.
    const char *cases[][2] = {
        {"(+ 2 (begin (set! + (lambda (a b) 42)) 3))",
         "((if #t + +) 2 (begin (set! + (lambda (a b) 42)) 3))"},
        {"(letrec ((f (lambda (n) (if (= n 0) (begin (set! cons +) 0) "
         "  (cons n (f (- n 1))))))) (f 3))",
         "(letrec ((f (lambda (n) (if (= n 0) (begin (set! cons +) 0) "
         "  ((if #t cons cons) n (f (- n 1))))))) (f 3))"},
        {"(letrec ((f (lambda (n) (if (= n 0) "
         "  (begin (set! + (lambda (a b) (cons a b))) '()) "
         "  (+ n (f (- n 1))))))) (f 3))",
         "(letrec ((f (lambda (n) (if (= n 0) "
         "  (begin (set! + (lambda (a b) (cons a b))) '()) "
         "  ((if #t + +) n (f (- n 1))))))) (f 3))"},
        {"(letrec ((f (lambda (n) (if (= n 0) "
         "  (begin (set! append (lambda (a b) (cons a b))) '()) "
         "  (append (list n) (f (- n 1))))))) (f 3))",
         "(letrec ((f (lambda (n) (if (= n 0) "
         "  (begin (set! append (lambda (a b) (cons a b))) '()) "
         "  ((if #t append append) (list n) (f (- n 1))))))) (f 3))"},
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        GC_GUARD;
        unsigned env = default_environment();
        gc_protect(&env);
        unsigned actual = compiled_eval_string(cases[i][0], env);
        gc_protect(&actual);
        env = default_environment();
        unsigned expected = compiled_eval_string(cases[i][1], env);
        ASSERT(actual != TOK_ERROR && expected != TOK_ERROR);
        ASSERT(deep_equal(actual, expected));
    }
    PASS();
}

TEST(compiled_lookup_observes_new_nearer_bindings)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    // The original binding is immutable, but a nearer mutable frame can
    // acquire a shadowing definition after a closure has been compiled/run.
    mark_immutable_environment(env);
    env = extend_env_empty(env);
    ASSERT(compiled_eval_string("(define read-plus (lambda () +))", env) != TOK_ERROR);
    ASSERT(compiled_eval_string("(define add (lambda (x) (+ x 1)))", env) != TOK_ERROR);
    ASSERT(IS_BUILTIN(compiled_eval_string("(read-plus)", env)));
    ASSERT(is_int(compiled_eval_string("(add 2)", env), 3));
    ASSERT(compiled_eval_string("(define + (lambda (a b) 42))", env) != TOK_ERROR);
    ASSERT(is_int(compiled_eval_string("((read-plus) 2 3)", env), 42));
    ASSERT(is_int(compiled_eval_string("(add 2)", env), 42));
    PASS();
}

TEST(trmc_rebound_operator_can_capture_and_resume)
{
    const char *prefix =
        "(let ((original-cons cons) (saved #f) (first #f) (phase 0)) "
        "  (letrec ((f (lambda (n) "
        "    (if (= n 0) "
        "      (begin (set! cons (lambda (a b) "
        "        (call/cc (lambda (k) "
        "          (if (= a 2) (set! saved k)) (original-cons a b))))) '()) "
        "      (";
    const char *suffix = " n (f (- n 1))))))) "
        "    (let ((answer (f 3))) "
        "      (if (= phase 0) "
        "        (begin (set! first answer) (set! phase 1) (saved '(x))) "
        "        (list first answer)))))";
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    char source[2048];
    snprintf(source, sizeof(source), "%scons%s", prefix, suffix);
    code_object *code = trmc_compile(source, env);
    ASSERT(code && code_tree_has_opcode(code, OP_TRMC_PUSH));
    unsigned actual = compiled_eval_string(source, env);
    gc_protect(&actual);
    env = default_environment();
    snprintf(source, sizeof(source), "%s(if #t cons cons)%s", prefix, suffix);
    unsigned expected = compiled_eval_string(source, env);
    ASSERT(actual != TOK_ERROR && expected != TOK_ERROR);
    ASSERT(deep_equal(actual, expected));
    unsigned answer = eval_string("'((3 2 1) (3 x))", env);
    ASSERT(deep_equal(actual, answer));
    PASS();
}

TEST(let_initializer_continuations_keep_independent_bindings)
{
    const char *cases[] = {
        "(let ((saved #f) (old #f) (phase 0)) "
        "  (let ((x (call/cc (lambda (k) (set! saved k) 1)))) "
        "    (if (= phase 0) "
        "      (begin (set! old (lambda () x)) (set! phase 1) (saved 2)) "
        "      (list (old) x))))",
        "(let ((kx #f) (ky #f) (old-ky #f) (phase 0)) "
        "  (let ((x (call/cc (lambda (k) (set! kx k) 1))) "
        "        (y (call/cc (lambda (k) (set! ky k) 10)))) "
        "    (cond ((= phase 0) (set! old-ky ky) (set! phase 1) (kx 2)) "
        "          ((= phase 1) (set! phase 2) (old-ky 20)) "
        "          (else (list x y)))))",
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++)
        ASSERT(eval_compiled_equal(cases[i]));
    PASS();
}

TEST(trmc_mixed_operators_share_one_accumulator)
{
    const char *list_source =
        "(letrec ((loop (lambda (k) (if (= k 0) '() "
        "  (if (= (modulo k 2) 0) (cons k (loop (- k 1))) "
        "    (append (list k k) (loop (- k 1)))))))) (loop 40))";
    const char *arithmetic_source =
        "(letrec ((loop (lambda (k) (if (= k 0) 0 "
        "  (if (= (modulo k 2) 0) (+ k (loop (- k 1))) "
        "    (- k (loop (- k 1)))))))) (loop 40))";
    for (unsigned immutable = 0; immutable < 2; immutable++) {
        GC_GUARD;
        unsigned env = default_environment();
        gc_protect(&env);
        if (immutable)
            mark_immutable_environment(env);
        code_object *code = trmc_compile(list_source, env);
        ASSERT(code && code_tree_has_trmc_mode(code, immutable ? TRMC_MODE_HOLE
                                                               : TRMC_MODE_FOLD));
        unsigned result = compiled_eval_string(list_source, env);
        gc_protect(&result);
        unsigned expected = eval_string(list_source, env);
        ASSERT(result != TOK_ERROR && expected != TOK_ERROR);
        ASSERT(deep_equal(result, expected));
        code = trmc_compile(arithmetic_source, env);
        ASSERT(code && code_tree_has_trmc_mode(code, TRMC_MODE_FOLD));
        result = compiled_eval_string(arithmetic_source, env);
        ASSERT(is_int(result, 40));
    }
    PASS();
}

TEST(trmc_uses_fold_mode_when_the_element_can_capture)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // f is an ordinary procedure, so it might capture a continuation, and a
    // mutated chain would be reachable from a second invocation. FOLD mode
    // never mutates, so it takes this body where HOLE cannot.
    code_object *code = trmc_compile(
        "(define trmc-walk (lambda (n f)"
        "  (letrec ((loop (lambda (k)"
        "    (if (= k 0) '() (cons (f k) (loop (- k 1)))))))"
        "    (loop n))))",
        env);
    ASSERT(code != NULL);
    ASSERT(code_tree_has_trmc_mode(code, TRMC_MODE_FOLD));
    ASSERT(code_tree_has_opcode(code, OP_TRMC_PUSH));
    ASSERT(!code_tree_has_opcode(code, OP_TRMC_APPEND));
    PASS();
}

TEST(trmc_uses_fold_mode_when_the_base_case_can_capture)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // The base case runs with the accumulator live, so a capture there rules
    // out HOLE just as much as one in the element expression does.
    code_object *code = trmc_compile(
        "(define trmc-base (lambda (n f)"
        "  (letrec ((loop (lambda (k)"
        "    (if (= k 0) (f 0) (cons k (loop (- k 1)))))))"
        "    (loop n))))",
        env);
    ASSERT(code != NULL);
    ASSERT(code_tree_has_trmc_mode(code, TRMC_MODE_FOLD));
    ASSERT(!code_tree_has_opcode(code, OP_TRMC_APPEND));
    PASS();
}

TEST(trmc_declines_when_cons_is_rebound)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // A user may rebind cons; the transform is only valid for the builtin.
    code_object *code = trmc_compile(
        "(define trmc-shadowed (lambda (n cons)"
        "  (letrec ((loop (lambda (k)"
        "    (if (= k 0) '() (cons k (loop (- k 1)))))))"
        "    (loop n))))",
        env);
    ASSERT(code != NULL);
    ASSERT(!code_tree_has_opcode(code, OP_TRMC_APPEND));
    PASS();
}

TEST(trmc_makes_a_foreign_tail_call_accumulator_conditional)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // A plain TAILCALL would hand back the callee's value without running the
    // return that finishes the accumulator. Demoting it to OP_CALL is not
    // right either - that breaks proper tail calls - so it becomes
    // TAILCALL_TRMC, which tail-calls for real while the accumulator is
    // empty.
    code_object *code = trmc_compile(
        "(define trmc-other (lambda (n g)"
        "  (letrec ((loop (lambda (k)"
        "    (if (= k 0) (g) (cons k (loop (- k 1)))))))"
        "    (loop n))))",
        env);
    ASSERT(code != NULL);
    const code_object *body = code_tree_find_trmc(code);
    ASSERT(body != NULL);
    ASSERT(body->trmc_mode == TRMC_MODE_FOLD);
    ASSERT(!code_has_opcode(body, OP_TAILCALL));
    ASSERT(code_has_opcode(body, OP_TAILCALL_TRMC));
    PASS();
}

TEST(trmc_declines_for_a_top_level_define)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // A global can be redefined between iterations, so its self-call cannot
    // become a jump - which is also what makes it ineligible for TRMC.
    code_object *code = trmc_compile(
        "(define trmc-global (lambda (n)"
        "  (if (= n 0) '() (cons n (trmc-global (- n 1))))))",
        env);
    ASSERT(code != NULL);
    ASSERT(code_tree_has_trmc_mode(code, TRMC_MODE_NONE));
    ASSERT(!code_tree_has_opcode(code, OP_TRMC_INIT));
    PASS();
}

TEST(trmc_follows_cond_clauses)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // cond pushes no environment frame, so a site in a clause body is at the
    // same depth as one directly under an if.
    // HOLE mode requires stable primitives as well as a capture-free body.
    mark_immutable_environment(env);
    code_object *code = trmc_compile(
        "(define trmc-cond (lambda (n)"
        "  (letrec ((loop (lambda (k)"
        "    (cond ((= k 0) '())"
        "          (else (cons k (loop (- k 1))))))))"
        "    (loop n))))",
        env);
    ASSERT(code != NULL);
    ASSERT(code_tree_has_opcode(code, OP_TRMC_APPEND));
    ASSERT(code_tree_has_trmc_mode(code, TRMC_MODE_HOLE));
    unsigned len = compiled_eval_string(
        "(length (letrec ((loop (lambda (k)"
        "  (cond ((= k 0) '()) (else (cons k (loop (- k 1))))))))"
        "  (loop 200000)))", env);
    ASSERT(is_int(len, 200000));
    PASS();
}

TEST(trmc_unwinds_let_frames_before_the_loop_jump)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // A let pushes an environment frame, so jumping straight back to the loop
    // entry point would leak one frame per iteration. The jump is preceded by
    // a POPENV for each pending frame. (test_eval runs without the stdlib, so
    // let here is the compiler's own form rather than the stdlib macro that
    // expands to an immediately applied lambda.)
    code_object *code = trmc_compile(
        "(define trmc-let (lambda (n)"
        "  (letrec ((loop (lambda (k)"
        "    (if (= k 0) '()"
        "        (let ((d (* k 2))) (cons d (loop (- k 1))))))))"
        "    (loop n))))",
        env);
    ASSERT(code != NULL);
    ASSERT(code_tree_has_opcode(code, OP_TRMC_INIT));
    ASSERT(code_tree_has_opcode(code, OP_POPENV));
    ASSERT(compiled_eval_string(
               "(define trmc-let (lambda (n)"
               "  (letrec ((loop (lambda (k)"
               "    (if (= k 0) '()"
               "        (let ((d (* k 2))) (cons d (loop (- k 1))))))))"
               "    (loop n))))",
               env) != TOK_ERROR);
    unsigned three = compiled_eval_string("(trmc-let 3)", env);
    ASSERT(IS_PAIR(three) && is_int(car(three), 6));
    // Past the frame ceiling, and without leaking a frame per iteration -
    // a leak would exhaust the environment long before this finishes.
    unsigned len = compiled_eval_string("(length (trmc-let 1100000))", env);
    ASSERT(is_int(len, 1100000));
    PASS();
}

TEST(trmc_folds_string_append)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // A string has no patchable tail, so there is no hole to leave open;
    // string-append can only be replayed at the return. That turns the
    // quadratic left fold into one pass over the pieces.
    code_object *code = trmc_compile(
        "(define trmc-spell (lambda (n s)"
        "  (letrec ((loop (lambda (k)"
        "    (if (= k 0) \"\" (string-append s (loop (- k 1)))))))"
        "    (loop n))))",
        env);
    ASSERT(code != NULL);
    ASSERT(code_tree_has_trmc_mode(code, TRMC_MODE_FOLD));
    ASSERT(code_tree_has_opcode(code, OP_TRMC_PUSH));
    ASSERT(compiled_eval_string(
               "(define trmc-spell (lambda (n s)"
               "  (letrec ((loop (lambda (k)"
               "    (if (= k 0) \"\" (string-append s (loop (- k 1)))))))"
               "    (loop n))))",
               env) != TOK_ERROR);
    unsigned len = compiled_eval_string("(string-length (trmc-spell 50000 \"ab\"))",
                                        env);
    ASSERT(is_int(len, 100000));
    PASS();
}

TEST(trmc_fold_mode_leaves_an_earlier_result_alone)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // FOLD's accumulator is built by prepending, which allocates but never
    // mutates, so re-entering a continuation captured in the loop rebuilds
    // from that continuation's own accumulator value. The list the first run
    // returned must be untouched - the property HOLE mode cannot offer, and
    // the reason FOLD needs no restriction on the body.
    ASSERT(compiled_eval_string("(define trmc-k #f)", env) != TOK_ERROR);
    ASSERT(compiled_eval_string("(define trmc-saved #f)", env) != TOK_ERROR);
    ASSERT(compiled_eval_string("(define (trmc-id x) x)", env) != TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define trmc-cap (lambda (n)"
               "  (letrec ((loop (lambda (j)"
               "    (if (= j 0) '()"
               "        (cons (trmc-id (call-with-current-continuation"
               "                        (lambda (c)"
               "                          (if (= j 1) (set! trmc-k c)) j)))"
               "              (loop (- j 1)))))))"
               "    (loop n))))",
               env) != TOK_ERROR);
    unsigned out = compiled_eval_string(
        "(call-with-current-continuation"
        "  (lambda (return)"
        "    (let ((lst (trmc-cap 3)))"
        "      (if (not trmc-saved)"
        "          (begin (set! trmc-saved lst)"
        "                 (let ((c trmc-k)) (set! trmc-k #f) (c 99))))"
        "      (return (cons trmc-saved lst)))))",
        env);
    ASSERT(out != TOK_ERROR && IS_PAIR(out));
    unsigned first = car(out), second = cdr(out);
    // first is (3 2 1), second is (3 2 99)
    ASSERT(is_int(car(first), 3));
    ASSERT(is_int(car(cdr(cdr(first))), 1));
    ASSERT(is_int(car(cdr(cdr(second))), 99));
    PASS();
}

TEST(trmc_transforms_append_over_a_self_tail_call)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // HOLE mode requires stable primitives as well as a capture-free body.
    mark_immutable_environment(env);
    code_object *code = trmc_compile(
        "(define trmc-spans (lambda (n)"
        "  (letrec ((loop (lambda (k)"
        "    (if (= k 0) '() (append (list k k) (loop (- k 1)))))))"
        "    (loop n))))",
        env);
    ASSERT(code != NULL);
    ASSERT(code_tree_has_opcode(code, OP_TRMC_SPLICE));
    // The element opcode belongs to the cons shape, not this one.
    ASSERT(!code_tree_has_opcode(code, OP_TRMC_APPEND));
    PASS();
}

TEST(trmc_append_shares_its_final_argument)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // R7RS: append copies every argument but the last, which it shares. The
    // transform has to preserve that - the base case value is spliced in by
    // reference, not copied.
    ASSERT(compiled_eval_string("(define trmc-shared-tail '(9 9))", env) !=
           TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define trmc-shared (lambda (n)"
               "  (letrec ((loop (lambda (k)"
               "    (if (= k 0) trmc-shared-tail"
               "        (append (list k) (loop (- k 1)))))))"
               "    (loop n))))",
               env) != TOK_ERROR);
    unsigned shared = compiled_eval_string(
        "(eq? (cdr (cdr (cdr (trmc-shared 3)))) trmc-shared-tail)", env);
    ASSERT(shared == ctx.atom_true);
    PASS();
}

TEST(trmc_append_rejects_an_improper_operand)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // A dotted argument in any but the final position is an error for
    // append, and stays one after the transform.
    ASSERT(compiled_eval_string(
               "(define trmc-improper (lambda (n)"
               "  (letrec ((loop (lambda (k)"
               "    (if (= k 0) '() (append '(1 . 2) (loop (- k 1)))))))"
               "    (loop n))))",
               env) != TOK_ERROR);
    ASSERT(compiled_eval_string("(trmc-improper 2)", env) == TOK_ERROR);
    PASS();
}

TEST(trmc_result_matches_the_untransformed_function)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string(TRMC_BUILD, env) != TOK_ERROR);
    // Same shape with a rebound cons, which declines the transform.
    ASSERT(compiled_eval_string(
               "(define trmc-plain (lambda (n mycons)"
               "  (letrec ((loop (lambda (k)"
               "    (if (= k 0) '() (mycons k (loop (- k 1)))))))"
               "    (loop n))))",
               env) != TOK_ERROR);
    unsigned same = compiled_eval_string(
        "(let loop ((i 0))"
        "  (if (> i 40) #t"
        "      (if (equal? (trmc-build i) (trmc-plain i cons))"
        "          (loop (+ i 1)) #f)))",
        env);
    ASSERT(same == ctx.atom_true);
    PASS();
}

TEST(trmc_handles_empty_single_and_non_list_base_cases)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string(TRMC_BUILD, env) != TOK_ERROR);
    ASSERT(compiled_eval_string("(trmc-build 0)", env) == 0);
    unsigned one = compiled_eval_string("(trmc-build 1)", env);
    ASSERT(IS_PAIR(one) && is_int(car(one), 1) && cdr(one) == 0);

    // A base case that is not a list leaves a dotted tail, which the return
    // has to splice in unchanged.
    ASSERT(compiled_eval_string(
               "(define trmc-dotted (lambda (n)"
               "  (letrec ((loop (lambda (k)"
               "    (if (= k 0) 'end (cons k (loop (- k 1)))))))"
               "    (loop n))))",
               env) != TOK_ERROR);
    unsigned dotted = compiled_eval_string("(trmc-dotted 2)", env);
    ASSERT(IS_PAIR(dotted));
    ASSERT(is_int(car(dotted), 2));
    ASSERT(IS_PAIR(cdr(dotted)) && is_int(car(cdr(dotted)), 1));
    unsigned dtail = cdr(cdr(dotted));
    ASSERT(CELL_TYPE(dtail) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(dtail)], "end");
    PASS();
}

TEST(trmc_fold_mode_runs_beyond_the_frame_ceiling)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    // FOLD mode has to clear the ceiling too, and its return does real work
    // per level rather than one set-cdr!, so the replay is what is under test
    // here as much as the loop.
    ASSERT(compiled_eval_string("(define (trmc-scale x) (* x 10))", env) !=
           TOK_ERROR);
    ASSERT(compiled_eval_string(
               "(define trmc-walk (lambda (n)"
               "  (letrec ((loop (lambda (k)"
               "    (if (= k 0) '() (cons (trmc-scale k) (loop (- k 1)))))))"
               "    (loop n))))",
               env) != TOK_ERROR);
    unsigned len = compiled_eval_string("(length (trmc-walk 1100000))", env);
    ASSERT(is_int(len, 1100000));

    // Arithmetic, where the accumulator holds numbers rather than list cells.
    ASSERT(compiled_eval_string(
               "(define trmc-total (lambda (n)"
               "  (letrec ((loop (lambda (k)"
               "    (if (= k 0) 0 (+ k (loop (- k 1)))))))"
               "    (loop n))))",
               env) != TOK_ERROR);
    unsigned sum = compiled_eval_string("(trmc-total 1100000)", env);
    ASSERT(is_int(sum, 605000550000LL));
    PASS();
}

TEST(trmc_builds_beyond_the_frame_ceiling)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);
    ASSERT(compiled_eval_string(TRMC_BUILD, env) != TOK_ERROR);
    // Well past VM_MAX_FRAMES_SIZE: without the transform this is a stack
    // overflow, not a slow success.
    unsigned len = compiled_eval_string("(length (trmc-build 2000000))", env);
    ASSERT(is_int(len, 2000000));
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
    RUN_TEST(compiled_booleans_are_self_evaluating);
    RUN_TEST(eval_negative_integer);
    RUN_TEST(eval_true);
    RUN_TEST(eval_false);
    RUN_TEST(eval_rejects_boolean_binding_names);
    RUN_TEST(compiled_rejects_boolean_binding_names);
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
    RUN_TEST(eval_unary_exact_complex_rationals);

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
    RUN_TEST(cond_expand_rejects_recursive_requirement);

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
    RUN_TEST(eval_load_rejects_reader_token_sentinel);

    // Define
    RUN_TEST(eval_define_variable);
    RUN_TEST(cps_define_continuation_propagates_invalid_environment);
    RUN_TEST(eval_define_function);
    RUN_TEST(eval_define_recursive);

    // Let
    RUN_TEST(eval_let_simple);
    RUN_TEST(eval_named_let);
    RUN_TEST(compiled_named_let);
    RUN_TEST(eval_let_accepts_quoted_cyclic_data);
    RUN_TEST(compiled_let_accepts_quoted_cyclic_data);
    RUN_TEST(eval_let_nested);
    RUN_TEST(eval_empty_let_forms_do_not_leak_internal_defines);
    RUN_TEST(compiled_empty_let_forms_do_not_leak_internal_defines);
    RUN_TEST(nested_begin_definitions_keep_function_scope);
    RUN_TEST(eval_empty_syntax_binding_forms_splice_internal_defines);
    RUN_TEST(compiled_empty_syntax_binding_forms_splice_internal_defines);
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
    RUN_TEST(eval_can_invoke_vm_continuation);
    RUN_TEST(eval_vm_continuation_preserves_multiple_values);
    RUN_TEST(interpreted_vm_continuation_call_transfers_active_vm);
    RUN_TEST(compiled_callcc_rejects_wrong_arity);
    RUN_TEST(eval_call_with_values_accepts_zero_values);
    RUN_TEST(eval_call_with_values_zero_values_to_list);
    RUN_TEST(eval_callcc_accepts_multiple_values);
    RUN_TEST(eval_callcc_accepts_zero_values);

    // GC protection
    RUN_TEST(gc_shadow_stack_balanced);
    RUN_TEST(gc_shadow_stack_lambda);
    RUN_TEST(gc_shadow_stack_macro_expansion);
    RUN_TEST(gc_shadow_stack_letrec);
    RUN_TEST(gc_preserves_closures);
    RUN_TEST(gc_preserves_continuations);
    RUN_TEST(cps_primitive_error_roots_environment_across_gc);
    RUN_TEST(letstar_binding_cell_survives_gc);
    RUN_TEST(gc_preserves_current_input_string_port);
    RUN_TEST(eval_read_string_port_preserves_unread_delimiter);
    RUN_TEST(textual_port_operations_use_utf8_character_boundaries);
    RUN_TEST(eval_read_file_port_preserves_unread_delimiter);
    RUN_TEST(file_textual_port_operations_peek_utf8_without_consuming);
    RUN_TEST(read_line_rejects_invalid_utf8_file_content);
    RUN_TEST(string_operations_reject_null_character);
    RUN_TEST(eval_read_rejects_reader_token_sentinels);
    RUN_TEST(compiled_read_rejects_reader_token_sentinels);
    RUN_TEST(eval_read_bytevector_preserves_unread_delimiter);
    RUN_TEST(read_bytevector_into_preserves_unread_delimiter);
    RUN_TEST(gc_preserves_current_output_string_port);
    RUN_TEST(write_string_uses_utf8_character_indices);
    RUN_TEST(write_char_encodes_utf8_scalars);
    RUN_TEST(eval_newline_rejects_closed_current_output_port);
    RUN_TEST(eval_flush_rejects_closed_output_port);
    RUN_TEST(eval_io_rejects_nil_port_argument);
    RUN_TEST(eval_close_port_rejects_wrong_direction);
    RUN_TEST(eval_set_current_port_accepts_closed_port_but_io_rejects_it);
    RUN_TEST(eval_write_to_string_escapes_strings);
    RUN_TEST(eval_write_large_acyclic_list);
    RUN_TEST(write_simple_rejects_cyclic_data);
    RUN_TEST(compiled_write_to_string_hides_bytecode_closure);
    RUN_TEST(eval_open_output_file_append_argument_is_truthy);
    RUN_TEST(eval_open_output_file_false_argument_truncates);
    RUN_TEST(gc_preserves_labeled_string);
    RUN_TEST(gc_preserves_labeled_vector);

    // Apply
    RUN_TEST(eval_apply_simple);
    RUN_TEST(eval_apply_lambda);
    RUN_TEST(eval_rejects_improper_application);
    RUN_TEST(eval_special_form_keywords_respect_lexical_bindings);
    RUN_TEST(eval_quasiquote_unquotes_vector_element);
    RUN_TEST(eval_quasiquote_respects_shadowed_keywords);
    RUN_TEST(eval_quasiquote_rejects_top_level_splicing);
    RUN_TEST(eval_quasiquote_splicing_preserves_dotted_tail);
    RUN_TEST(eval_quasiquote_rejects_improper_splice_value);
    RUN_TEST(eval_quasiquote_rejects_circular_splice_value);
    RUN_TEST(eval_quasiquote_rejects_circular_template);
    RUN_TEST(eval_syntax_rules_rejects_circular_pattern_and_template);
    RUN_TEST(eval_syntax_rules_rejects_circular_invocation);
    RUN_TEST(eval_quasiquote_rejects_malformed_subforms);
    RUN_TEST(eval_quasiquote_allows_data_in_unquote_expression);

    // List operations
    RUN_TEST(eval_cons);
    RUN_TEST(eval_car_cdr);
    RUN_TEST(eval_length);
    RUN_TEST(eval_rejects_circular_list_operations);
    RUN_TEST(eval_equal_handles_cycles);
    RUN_TEST(eval_hash_table_handles_cyclic_equal_keys);
    RUN_TEST(hash_table_enumeration_survives_gc_rehash);
    RUN_TEST(eval_append);
    RUN_TEST(eval_gc_stats_shape);
    RUN_TEST(eval_string_to_symbol_preserves_numeric_text);
    RUN_TEST(eval_environment_rejects_non_integer_version);
    RUN_TEST(eval_null_environment_booleans_are_self_evaluating);

    // Bytevectors
    RUN_TEST(eval_bytevector_rejects_out_of_range_constructor);
    RUN_TEST(eval_exit_rejects_out_of_range_code);
    RUN_TEST(compiled_exit_rejects_out_of_range_code);
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
    RUN_TEST(eval_inexact_to_exact_complex_components);
    RUN_TEST(eval_number_to_string_int64_min_radix);
    RUN_TEST(eval_number_to_string_exact_non_int64);
    RUN_TEST(eval_radix_rejects_out_of_range_values);
    RUN_TEST(compiled_radix_rejects_out_of_range_values);
    RUN_TEST(eval_arithmetic_shift_negative_left);
    RUN_TEST(eval_arithmetic_shift_int64_min_count);
    RUN_TEST(eval_arithmetic_shift_large_left_promotes);
    RUN_TEST(eval_arithmetic_shift_overflow_left_promotes);
    RUN_TEST(eval_arithmetic_shift_negative_large_left_promotes);
    RUN_TEST(eval_rationalize_preserves_large_inexact);
    RUN_TEST(rationalize_respects_exact_interval);
    RUN_TEST(complex_finiteness_preserves_exact_components);
    RUN_TEST(eval_floor_preserves_bignum);
    RUN_TEST(eval_magnitude_preserves_rational);
    RUN_TEST(eval_magnitude_preserves_bignum);
    RUN_TEST(eval_sqrt_preserves_exact_bignum_squares);
    RUN_TEST(eval_sqrt_preserves_exact_very_large_bignum_squares);
    RUN_TEST(eval_sqrt_negative_exact_values_preserve_magnitude);
    RUN_TEST(eval_sqrt_rational_scales_before_conversion);
    RUN_TEST(eval_exact_to_inexact_huge_bignum_overflows_to_infinity);
    RUN_TEST(eval_rational_to_inexact_preserves_mantissa_bits);
    RUN_TEST(eval_exact_to_inexact_huge_rational_stays_finite);
    RUN_TEST(eval_string_to_number_radix_bignum);
    RUN_TEST(eval_numeric_prefixes_share_reader_semantics);
    RUN_TEST(eval_string_to_number_radix_rejects_invalid);
    RUN_TEST(eval_complex_radix_round_trips);
    RUN_TEST(eval_integer_to_char_rejects_surrogates);
    RUN_TEST(compiled_integer_to_char_rejects_surrogates);
    RUN_TEST(eval_complex_reader_accepts_implicit_imaginary_unit);
    RUN_TEST(eval_complex_reader_preserves_exact_components);
    RUN_TEST(eval_complex_reader_rejects_nested_imaginary_suffix);
    RUN_TEST(eval_integer_rejects_infinity);
    RUN_TEST(eval_rational_accessors_reject_infinity);
    RUN_TEST(compiled_integer_predicate_matches_eval);
    RUN_TEST(eval_exact_rejects_non_numbers);
    RUN_TEST(eval_numtower_rejects_non_numbers);
    RUN_TEST(eval_exact_tiny_complex_imag_part_is_not_zero);
    RUN_TEST(eval_exact_rational_exponent);
    RUN_TEST(eval_math_rejects_non_numbers);
    RUN_TEST(compiled_div_fixnum_boundary);
    RUN_TEST(compiled_constant_folding_releases_gc_roots);
    RUN_TEST(compiled_number_predicate_constant_folds);
    RUN_TEST(compiled_lookup_add1_int64_max);
    RUN_TEST(compiled_lookup_sub1_int64_min);
    RUN_TEST(compiled_div_int64_min_by_negative_one);
    RUN_TEST(compiled_modulo_int64_min_by_negative_one);
    RUN_TEST(eval_compiled_parity_bignum_promotion);
    RUN_TEST(eval_compiled_parity_exact_rationals);
    RUN_TEST(eval_compiled_parity_macro_introduced_bindings);
    RUN_TEST(compiled_iife_rest_parameter_shadows_stack_local);
    RUN_TEST(compiled_macro_pattern_roots_survive_rational_gc);
    RUN_TEST(compiled_letrec_tail_call_many_args);
    RUN_TEST(compiled_let_forms_preserve_enclosing_tail_context);
    RUN_TEST(compiled_cond_arrow_preserves_tail_context);
    RUN_TEST(compiled_string_to_list_allocates_fresh_result);
    RUN_TEST(compiled_begin_preserves_unbound_lookup_error);
    RUN_TEST(compiled_multiply_by_zero_preserves_side_effects);
    RUN_TEST(compiled_multiply_by_one_preserves_type_error);
    RUN_TEST(compiled_divide_by_one_preserves_type_error);
    RUN_TEST(compiled_double_not_returns_boolean);
    RUN_TEST(compiled_add1_sub1_preserves_type_error);
    RUN_TEST(compiled_lookup_add1_sub1_halt_on_type_error);
    RUN_TEST(compiled_add1_sub1_support_non_integer_numbers);
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
    RUN_TEST(compiled_hash_table_handles_cyclic_equal_keys);
    RUN_TEST(compiled_vector_ref_rejects_non_vector);
    RUN_TEST(compiled_call_rejects_fixnum_operator);
    RUN_TEST(compiled_rejects_improper_application);
    RUN_TEST(compiled_special_form_keywords_respect_lexical_bindings);
    RUN_TEST(compiled_lambda_optimizations_respect_syntax_binding);
    RUN_TEST(eval_macro_expansion_rejects_recursive_expansion);
    RUN_TEST(compiled_rejects_malformed_lambda);
    RUN_TEST(compiled_lambda_rejects_wrong_arity);
    RUN_TEST(compiled_let_lambda_handles_dotted_formals_in_self_reference_check);
    RUN_TEST(compiled_rejects_malformed_special_forms);
    RUN_TEST(compiled_quasiquote_unquotes_vector_element);
    RUN_TEST(compiled_quasiquote_respects_shadowed_keywords);
    RUN_TEST(compiled_quasiquote_rejects_top_level_splicing);
    RUN_TEST(compiled_quasiquote_splicing_preserves_dotted_tail);
    RUN_TEST(compiled_quasiquote_rejects_improper_splice_value);
    RUN_TEST(compiled_quasiquote_rejects_circular_splice_value);
    RUN_TEST(compiled_quasiquote_rejects_circular_template);
    RUN_TEST(compiled_syntax_rules_rejects_circular_pattern_and_template);
    RUN_TEST(compiled_syntax_rules_rejects_circular_invocation);
    RUN_TEST(compiled_legacy_macro_rejects_circular_invocation);
    RUN_TEST(compiled_quasiquote_rejects_malformed_subforms);
    RUN_TEST(compiled_quasiquote_allows_data_in_unquote_expression);
    RUN_TEST(compiled_local_set_returns_assigned_value);
    RUN_TEST(compiled_call_with_values_accepts_zero_values);
    RUN_TEST(compiled_call_with_values_zero_values_to_list);
    RUN_TEST(compiled_callcc_accepts_multiple_values);
    RUN_TEST(compiled_callcc_accepts_zero_values);
    RUN_TEST(compiled_call_with_values_rejects_non_producer);
    RUN_TEST(compiled_define_syntax_preserves_custom_ellipsis);
    RUN_TEST(compiled_begin_define_syntax_is_visible_to_later_forms);
    RUN_TEST(compiled_let_syntax_preserves_custom_ellipsis);
    RUN_TEST(eval_syntax_rules_respects_shadowed_ellipsis);
    RUN_TEST(compiled_syntax_rules_respects_shadowed_ellipsis);
    RUN_TEST(eval_macro_hygiene_preserves_quoted_introduced_names);
    RUN_TEST(compiled_macro_hygiene_preserves_quoted_introduced_names);
    RUN_TEST(eval_macro_hygiene_prevents_use_site_capture);
    RUN_TEST(compiled_macro_hygiene_prevents_use_site_capture);
    RUN_TEST(eval_macro_hygiene_respects_shadowed_quote_in_templates);
    RUN_TEST(compiled_macro_hygiene_respects_shadowed_quote_in_templates);
    RUN_TEST(eval_macro_hygiene_preserves_definition_site_keyword_bindings);
    RUN_TEST(compiled_macro_hygiene_preserves_definition_site_keyword_bindings);
    RUN_TEST(eval_syntax_rules_unwraps_pattern_vars_in_quoted_templates);
    RUN_TEST(compiled_syntax_rules_unwraps_pattern_vars_in_quoted_templates);
    RUN_TEST(eval_macro_hygiene_preserves_quasiquote_data);
    RUN_TEST(compiled_macro_hygiene_preserves_quasiquote_data);
    RUN_TEST(eval_syntax_rules_literals_compare_lexical_bindings);
    RUN_TEST(compiled_syntax_rules_literals_compare_lexical_bindings);
    RUN_TEST(eval_syntax_rules_underscore_literal_is_not_wildcard);
    RUN_TEST(compiled_syntax_rules_underscore_literal_is_not_wildcard);
    RUN_TEST(eval_syntax_rules_treats_booleans_as_literals);
    RUN_TEST(compiled_syntax_rules_treats_booleans_as_literals);
    RUN_TEST(eval_syntax_rules_ellipsis_allows_tail_patterns);
    RUN_TEST(compiled_syntax_rules_ellipsis_allows_tail_patterns);
    RUN_TEST(eval_syntax_rules_vector_template_repeats_compound_elements);
    RUN_TEST(compiled_syntax_rules_vector_template_repeats_compound_elements);
    RUN_TEST(
        syntax_rules_expands_large_flat_executable_template_without_stack_overflow);
    RUN_TEST(syntax_rules_hygienizes_large_begin_definition_sequence);
    RUN_TEST(large_named_let_binding_sequence_survives_gc);
    RUN_TEST(
        syntax_rules_matches_large_flat_pattern_without_stack_overflow);
    RUN_TEST(syntax_rules_expands_large_flat_template_without_stack_overflow);
    RUN_TEST(eval_complex_large_components_stay_finite);
    RUN_TEST(eval_complex_math_range_and_branch_cuts);
    RUN_TEST(eval_complex_transcendentals_avoid_intermediate_overflow);
    RUN_TEST(eval_stable_complex_helpers_keep_small_components);
    RUN_TEST(eval_expt_zero_and_complex_range);
    RUN_TEST(eval_inexact_preserves_complex_arguments);
    RUN_TEST(eval_complex_number_text_round_trips);
    RUN_TEST(eval_complex_division_scales_finite_components);
    RUN_TEST(eval_macro_set_target_is_referentially_transparent);
    RUN_TEST(compiled_macro_set_target_is_referentially_transparent);
    RUN_TEST(eval_macro_hygiene_renames_nested_syntax_rules_templates);
    RUN_TEST(compiled_macro_hygiene_renames_nested_syntax_rules_templates);
    RUN_TEST(eval_macro_define_target_is_hygienic);
    RUN_TEST(compiled_macro_define_target_is_hygienic);
    RUN_TEST(compiled_macro_thunk_captures_stack_local);
    RUN_TEST(compiled_binding_initializer_closures_capture_stack_locals);
    RUN_TEST(eval_calls_bytecode_closure_with_stack_locals);
    RUN_TEST(vm_eval_runs_in_the_calling_vm);

    // Tail recursion modulo cons
    RUN_TEST(code_object_registry_survives_heavy_churn);
    RUN_TEST(code_object_stays_registered_when_the_index_cannot_grow);
    RUN_TEST(trmc_transforms_cons_over_a_self_tail_call);
    RUN_TEST(trmc_loop_jump_clears_the_accumulator_init);
    RUN_TEST(compiled_recursive_calls_respect_rebinding);
    RUN_TEST(compiled_primitive_calls_respect_rebinding);
    RUN_TEST(compiled_rebinding_preserves_vm_evaluation_order);
    RUN_TEST(compiled_lookup_observes_new_nearer_bindings);
    RUN_TEST(trmc_rebound_operator_can_capture_and_resume);
    RUN_TEST(trmc_mixed_operators_share_one_accumulator);
    RUN_TEST(let_initializer_continuations_keep_independent_bindings);
    RUN_TEST(trmc_uses_fold_mode_when_the_element_can_capture);
    RUN_TEST(trmc_uses_fold_mode_when_the_base_case_can_capture);
    RUN_TEST(trmc_declines_when_cons_is_rebound);
    RUN_TEST(trmc_makes_a_foreign_tail_call_accumulator_conditional);
    RUN_TEST(trmc_declines_for_a_top_level_define);
    RUN_TEST(trmc_follows_cond_clauses);
    RUN_TEST(trmc_unwinds_let_frames_before_the_loop_jump);
    RUN_TEST(trmc_folds_string_append);
    RUN_TEST(trmc_fold_mode_leaves_an_earlier_result_alone);
    RUN_TEST(trmc_transforms_append_over_a_self_tail_call);
    RUN_TEST(trmc_append_shares_its_final_argument);
    RUN_TEST(trmc_append_rejects_an_improper_operand);
    RUN_TEST(trmc_result_matches_the_untransformed_function);
    RUN_TEST(trmc_handles_empty_single_and_non_list_base_cases);
    RUN_TEST(trmc_fold_mode_runs_beyond_the_frame_ceiling);
    RUN_TEST(trmc_builds_beyond_the_frame_ceiling);

    TEST_SUMMARY("evaluator");
}
