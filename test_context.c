// Unit tests for context and env modules
#define _POSIX_C_SOURCE 200809L
#include "bytecode.h"
#include "context.h"
#include "env.h"
#include "prim_internal.h"
#include "primitives.h"
#include "reader.h"
#include "test_framework.h"
#include "types.h"
#include "writer.h"
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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

TEST(numeric_helpers_accept_direct_fixnum)
{
    ASSERT(is_numeric(MAKE_FIXNUM(42)));
    ASSERT(is_exact(MAKE_FIXNUM(-7)));
    ASSERT(to_double(MAKE_FIXNUM(5)) == 5.0);
    PASS();
}

TEST(numeric_helpers_reject_token_sentinels)
{
    ASSERT(!is_numeric(TOK_ERROR));
    ASSERT(!is_exact(TOK_ERROR));
    ASSERT(to_double(TOK_ERROR) == 0.0);
    PASS();
}

TEST(store_complex_accepts_direct_fixnum_parts)
{
    unsigned real = MAKE_FIXNUM(3);
    unsigned zero_imag = store_complex(real, MAKE_FIXNUM(0));
    ASSERT(zero_imag == real);

    unsigned complex = store_complex(real, MAKE_FIXNUM(4));
    ASSERT(CELL_TYPE(complex) == BT_COMPLEX);
    ASSERT(CELL_CAR(complex) == real);
    ASSERT(CELL_CDR(complex) == MAKE_FIXNUM(4));
    ASSERT(to_double(complex) == 3.0);
    PASS();
}

TEST(numeric_sign_helpers_accept_direct_fixnum)
{
    ASSERT(is_negative_number(MAKE_FIXNUM(-3)));
    ASSERT(!is_negative_number(MAKE_FIXNUM(3)));
    ASSERT(negate_number(MAKE_FIXNUM(3)) == MAKE_FIXNUM(-3));
    ASSERT(negate_number(MAKE_FIXNUM(-3)) == MAKE_FIXNUM(3));

    unsigned min_negated = negate_number(MAKE_FIXNUM(FIXNUM_MIN));
    ASSERT(CELL_TYPE(min_negated) == BT_NUM);
    ASSERT_EQ(CELL_ID(min_negated), -(int64_t)FIXNUM_MIN);
    ASSERT(!is_negative_number(TOK_ERROR));
    ASSERT_EQ(negate_number(TOK_ERROR), TOK_ERROR);
    PASS();
}

TEST(bignum_helpers_accept_direct_fixnum)
{
    bignum *bn = to_bignum(MAKE_FIXNUM(-42));
    ASSERT(bn != NULL);
    ASSERT_EQ(bn_sign(bn), -1);
    ASSERT_EQ(bn->len, 1);
    ASSERT_EQ(bn->limbs[0], 42);
    bn_free(bn);

    ASSERT(get_bignum(MAKE_FIXNUM(1)) == NULL);
    ASSERT(to_bignum(TOK_ERROR) == NULL);
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

TEST(writer_handles_direct_fixnum)
{
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(MAKE_FIXNUM(-42), mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "-42");
    free(buf);
    PASS();
}

TEST(writer_handles_vector_containing_direct_fixnum)
{
    unsigned v = make_vector(2, MAKE_FIXNUM(7));
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(v, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "#(7 7)");
    free(buf);
    PASS();
}

TEST(writer_handles_vm_continuation)
{
    unsigned cont = alloc();
    CELL_TYPE(cont) = BT_VMCONT;
    CELL_PTR(cont) = NULL;

    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(cont, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "[continuation]");
    free(buf);
    PASS();
}

TEST(writer_handles_bytecode_closure_marker)
{
    unsigned marker = alloc();
    CELL_TYPE(marker) = BT_CLOSURE;
    CELL_PTR(marker) = NULL;

    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(marker, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "[bytecode-closure]");
    ASSERT_STR_EQ(type_name(marker), "bytecode-closure");
    free(buf);
    PASS();
}

TEST(string_port_write_failures_return_false)
{
    string_port *sp = strport_new();
    ASSERT(sp != NULL);

    sp->cap = SIZE_MAX / 2 + 1;
    sp->len = sp->cap - 1;
    ASSERT(!strport_putc(sp, 'x'));

    sp->len = SIZE_MAX - 2;
    ASSERT(!strport_puts(sp, "xx"));

    sp->len = 0;
    sp->cap = INITIAL_STRING_CAP;
    sp->data[0] = '\0';
    strport_free(sp);
    PASS();
}

TEST(make_string_owned_rejects_null)
{
    ASSERT(make_string_owned(NULL) == TOK_ERROR);
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

TEST(list_length_non_pair_is_zero)
{
    ASSERT_EQ(list_length(MAKE_FIXNUM(42)), 0);
    PASS();
}

TEST(list_length_three)
{
    unsigned lst =
        alloc_cons(store(1), alloc_cons(store(2), alloc_cons(store(3), 0)));
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

TEST(deep_equal_fixnum_and_boxed_integer)
{
    ASSERT(deep_equal(MAKE_FIXNUM(42), store(42)));
    ASSERT(deep_equal(store(-7), MAKE_FIXNUM(-7)));
    ASSERT(!deep_equal(MAKE_FIXNUM(42), store(43)));
    PASS();
}

TEST(deep_equal_rejects_token_sentinels)
{
    ASSERT(!deep_equal(MAKE_FIXNUM(42), TOK_ERROR));
    ASSERT(!deep_equal(TOK_ERROR, store(42)));
    PASS();
}

TEST(primitive_eq_handles_fixnum_and_boxed_integer)
{
    unsigned argv_same[2] = {MAKE_FIXNUM(42), store(42)};
    unsigned argv_diff[2] = {MAKE_FIXNUM(42), store(43)};
    unsigned argv_nil[2] = {MAKE_FIXNUM(42), 0};
    ASSERT(apply_primitive_argv(PEQ, 2, argv_same) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PEQ, 2, argv_diff) == ctx.atom_false);
    ASSERT(apply_primitive_argv(PEQ, 2, argv_nil) == ctx.atom_false);
    PASS();
}

TEST(primitive_arithmetic_handles_direct_fixnums)
{
    unsigned plus_args[2] = {MAKE_FIXNUM(2), MAKE_FIXNUM(3)};
    unsigned plus = apply_primitive_argv(PPLUS, 2, plus_args);
    ASSERT(CELL_TYPE(plus) == BT_NUM);
    ASSERT_EQ(CELL_ID(plus), 5);

    unsigned minus_args[2] = {MAKE_FIXNUM(7), MAKE_FIXNUM(4)};
    unsigned minus = apply_primitive_argv(PMINUS, 2, minus_args);
    ASSERT(CELL_TYPE(minus) == BT_NUM);
    ASSERT_EQ(CELL_ID(minus), 3);

    unsigned times_args[2] = {MAKE_FIXNUM(6), MAKE_FIXNUM(7)};
    unsigned times = apply_primitive_argv(PTIMES, 2, times_args);
    ASSERT(CELL_TYPE(times) == BT_NUM);
    ASSERT_EQ(CELL_ID(times), 42);

    unsigned divide_args[2] = {MAKE_FIXNUM(21), MAKE_FIXNUM(7)};
    unsigned divide = apply_primitive_argv(PDIV, 2, divide_args);
    ASSERT(CELL_TYPE(divide) == BT_NUM);
    ASSERT_EQ(CELL_ID(divide), 3);

    unsigned abs_args[1] = {MAKE_FIXNUM(-8)};
    unsigned abs_result = apply_primitive_argv(PABS, 1, abs_args);
    ASSERT(CELL_TYPE(abs_result) == BT_NUM);
    ASSERT_EQ(CELL_ID(abs_result), 8);

    unsigned modulo_args[2] = {MAKE_FIXNUM(17), MAKE_FIXNUM(5)};
    unsigned modulo = apply_primitive_argv(PMOD, 2, modulo_args);
    ASSERT(CELL_TYPE(modulo) == BT_NUM);
    ASSERT_EQ(CELL_ID(modulo), 2);

    unsigned remainder_args[2] = {MAKE_FIXNUM(17), MAKE_FIXNUM(5)};
    unsigned remainder = apply_primitive_argv(PREMAINDER, 2, remainder_args);
    ASSERT(CELL_TYPE(remainder) == BT_NUM);
    ASSERT_EQ(CELL_ID(remainder), 2);

    unsigned quotient_args[2] = {MAKE_FIXNUM(17), MAKE_FIXNUM(5)};
    unsigned quotient = apply_primitive_argv(PQUOTIENT, 2, quotient_args);
    ASSERT(CELL_TYPE(quotient) == BT_NUM);
    ASSERT_EQ(CELL_ID(quotient), 3);
    PASS();
}

TEST(bytevector_primitives_reject_direct_fixnum)
{
    unsigned args[1] = {MAKE_FIXNUM(1)};
    ASSERT(apply_primitive_argv(PBYTEVECUP, 1, args) == ctx.atom_false);
    ASSERT(apply_primitive_argv(PBYTEVECLEN, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PBYTEVECCOPY, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PBYTEVECAPPEND, 1, args) == TOK_ERROR);
    PASS();
}

TEST(numtower_primitives_handle_direct_fixnums)
{
    unsigned positive_arg[1] = {MAKE_FIXNUM(9)};
    unsigned negative_arg[1] = {MAKE_FIXNUM(-9)};

    ASSERT(apply_primitive_argv(PINTEGERP, 1, positive_arg) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PRATIONALP, 1, positive_arg) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PFINITE, 1, positive_arg) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PINFINITE, 1, positive_arg) ==
           ctx.atom_false);
    ASSERT(apply_primitive_argv(PNAN, 1, positive_arg) == ctx.atom_false);

    unsigned numerator = apply_primitive_argv(PNUMERATOR, 1, positive_arg);
    ASSERT(IS_FIXNUM(numerator));
    ASSERT_EQ(FIXNUM_VALUE(numerator), 9);

    unsigned denominator =
        apply_primitive_argv(PDENOMINATOR, 1, positive_arg);
    ASSERT(CELL_TYPE(denominator) == BT_NUM);
    ASSERT_EQ(CELL_ID(denominator), 1);

    unsigned magnitude = apply_primitive_argv(PMAGNITUDE, 1, negative_arg);
    ASSERT(CELL_TYPE(magnitude) == BT_NUM);
    ASSERT_EQ(CELL_ID(magnitude), 9);

    unsigned real = apply_primitive_argv(PREALPART, 1, positive_arg);
    ASSERT(IS_FIXNUM(real));
    ASSERT_EQ(FIXNUM_VALUE(real), 9);

    unsigned imag = apply_primitive_argv(PIMAGPART, 1, positive_arg);
    ASSERT(CELL_TYPE(imag) == BT_NUM);
    ASSERT_EQ(CELL_ID(imag), 0);

    PASS();
}

TEST(math_primitives_handle_direct_fixnums)
{
    unsigned square_arg[1] = {MAKE_FIXNUM(9)};
    unsigned negative_arg[1] = {MAKE_FIXNUM(-9)};
    unsigned exponent_args[2] = {MAKE_FIXNUM(2), MAKE_FIXNUM(10)};

    unsigned sqrt_result = apply_primitive_argv(PSQRT, 1, square_arg);
    ASSERT(CELL_TYPE(sqrt_result) == BT_NUM);
    ASSERT_EQ(CELL_ID(sqrt_result), 3);

    unsigned expt_result = apply_primitive_argv(PEXPT, 2, exponent_args);
    ASSERT(CELL_TYPE(expt_result) == BT_NUM);
    ASSERT_EQ(CELL_ID(expt_result), 1024);

    unsigned floor_result = apply_primitive_argv(PFLOOR, 1, negative_arg);
    ASSERT(IS_FIXNUM(floor_result));
    ASSERT_EQ(FIXNUM_VALUE(floor_result), -9);

    ASSERT(apply_primitive_argv(PRANDOMSEED, 1, square_arg) ==
           MAKE_FIXNUM(9));
    PASS();
}

TEST(number_to_string_handles_direct_fixnum)
{
    unsigned args[1] = {MAKE_FIXNUM(-123)};
    unsigned result = apply_primitive_argv(PNUM2STR, 1, args);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "-123");
    PASS();
}

TEST(procedure_predicate_rejects_pair_with_fixnum_car)
{
    unsigned pair = alloc_cons(MAKE_FIXNUM(1), 0);
    unsigned args[1] = {pair};
    ASSERT(apply_primitive_argv(PPROCP, 1, args) == ctx.atom_false);
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

TEST(code_free_unregisters_tree)
{
    code_object *parent = code_new();
    code_object *child = code_new();
    ASSERT(parent != NULL);
    ASSERT(child != NULL);
    code_add_child(parent, child);

    code_free(parent);

    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        ASSERT(code != parent);
        ASSERT(code != child);
    }
    PASS();
}

static bool code_registry_contains(code_object *needle)
{
    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        if (code == needle)
            return true;
    }
    return false;
}

TEST(code_sweep_marks_vm_continuation_code)
{
    code_object *code = code_new();
    code_object *child = code_new();
    code_object *frame_code = code_new();
    ASSERT(code != NULL);
    ASSERT(child != NULL);
    ASSERT(frame_code != NULL);
    code_add_child(code, child);

    unsigned cont_cell = alloc();
    size_t block_size = sizeof(vm_continuation) + sizeof(vm_frame);
    vm_continuation *cont = checked_calloc_array(1, block_size);
    ASSERT(cont != NULL);
    cont->code = code;
    cont->fp = 1;
    cont->frames = (vm_frame *)((char *)cont + sizeof(vm_continuation));
    cont->frames[0].code = frame_code;
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    gc_sweep_code_objects();
    ASSERT(code_registry_contains(code));
    ASSERT(code_registry_contains(child));
    ASSERT(code_registry_contains(frame_code));

    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    free(cont);
    code_free(code);
    code_free(frame_code);
    PASS();
}

TEST(gc_updates_vm_continuation_letrec_roots)
{
    unsigned saved = alloc();
    CELL_TYPE(saved) = BT_STRING;
    CELL_PTR(saved) = checked_string_copy("saved");
    ASSERT(CELL_PTR(saved) != NULL);

    GC_GUARD;
    gc_protect(&saved);
    unsigned vals = alloc_cons(saved, 0);
    gc_protect(&vals);
    unsigned frame = alloc_cons(0, vals);
    gc_protect(&frame);
    unsigned letrec_frame = alloc_cons(frame, 0);
    gc_protect(&letrec_frame);

    unsigned cont_cell = alloc();
    size_t block_size = sizeof(vm_continuation) + sizeof(unsigned);
    vm_continuation *cont = checked_calloc_array(1, block_size);
    ASSERT(cont != NULL);
    cont->letrec_frame = letrec_frame;
    cont->letrec_saved_len = 1;
    cont->letrec_saved =
        (unsigned *)((char *)cont + sizeof(vm_continuation));
    cont->letrec_saved[0] = saved;
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    cont_cell = gc(cont_cell);
    cont = (vm_continuation *)CELL_PTR(cont_cell);
    ASSERT(CELL_TYPE(cont->letrec_frame) == BT_CONS);
    ASSERT(CELL_TYPE(cont->letrec_saved[0]) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(cont->letrec_saved[0]), "saved");
    PASS();
}

TEST(vm_continuation_capture_copies_after_gc)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_PUSHCONT);
    code_emit(code, OP_HALT);

    unsigned env = empty_environment();
    unsigned old_env = env;

    if (ctx.card_table) {
        unsigned nursery_end =
            (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
        ctx.nursery_ptr = nursery_end;
    }

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);

    ASSERT(CELL_TYPE(result) == BT_VMCONT);
    vm_continuation *cont = (vm_continuation *)CELL_PTR(result);
    ASSERT(cont != NULL);
    ASSERT(cont->env != old_env);
    ASSERT(CELL_TYPE(cont->env) == BT_CONS);

    free(cont);
    CELL_TYPE(result) = BT_FREE;
    CELL_PTR(result) = NULL;
    code_free(code);
    PASS();
}

TEST(gc_closes_unreachable_file_port)
{
    FILE *f = tmpfile();
    ASSERT(f != NULL);
    int fd = fileno(f);
    ASSERT(fd >= 0);

    unsigned port = alloc();
    CELL_TYPE(port) = BT_INPORT;
    CELL_PTR(port) = file_port_new(f, false, true, true);
    ASSERT(CELL_PTR(port) != NULL);

    gc(0);

    errno = 0;
    ASSERT(fcntl(fd, F_GETFD) == -1);
    ASSERT(errno == EBADF);
    PASS();
}

TEST(gc_forgets_unreachable_file_port_reader_state)
{
    FILE *f = fmemopen((void *)"12.\n", 4, "r");
    ASSERT(f != NULL);

    unsigned port = alloc();
    CELL_TYPE(port) = BT_INPORT;
    CELL_PTR(port) = file_port_new(f, false, true, true);
    ASSERT(CELL_PTR(port) != NULL);

    unsigned value = read_obj_port(f);
    ASSERT(value != TOK_ERROR);
    ASSERT(reader_port_pending_bytes(f) > 0);

    gc(0);

    ASSERT_EQ(reader_port_pending_bytes(f), 0);
    PASS();
}

TEST(gc_preserves_current_file_port_until_replaced)
{
    FILE *f = tmpfile();
    ASSERT(f != NULL);
    int fd = fileno(f);
    ASSERT(fd >= 0);

    unsigned port = alloc();
    CELL_TYPE(port) = BT_INPORT;
    CELL_PTR(port) = file_port_new(f, false, true, true);
    ASSERT(CELL_PTR(port) != NULL);
    ctx.current_input = f;
    ctx.current_input_cell = port;

    gc(0);
    ASSERT(fcntl(fd, F_GETFD) != -1);

    ctx.current_input = stdin;
    ctx.current_input_cell = 0;
    gc(0);

    errno = 0;
    ASSERT(fcntl(fd, F_GETFD) == -1);
    ASSERT(errno == EBADF);
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
    unsigned params =
        alloc_cons(atom_from_string("a"), alloc_cons(atom_from_string("b"), 0));
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
    unsigned lst =
        alloc_cons(store(1), alloc_cons(store(2), alloc_cons(store(3), 0)));
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

TEST(gc_preserves_direct_vector_root)
{
    unsigned v = make_vector(3, store(42));
    unsigned preserved = gc(v);
    ASSERT(CELL_TYPE(preserved) == BT_VECTOR);
    ASSERT_EQ(vector_len(preserved), 3);
    for (unsigned i = 0; i < vector_len(preserved); i++) {
        unsigned elem = vector_data_ptr(preserved)[i];
        ASSERT(CELL_TYPE(elem) == BT_NUM);
        ASSERT_EQ(CELL_ID(elem), 42);
    }
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
    ASSERT(root != 0);
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
    ASSERT(root != 0);
    int usage_after = heap_usage_percent();
    ASSERT(usage_after >= 0 && usage_after <= 100);
    PASS();
}

TEST(gc_maybe_gc_threshold)
{
    unsigned root = alloc_cons(store(1), 0);
    // With low threshold, should trigger GC
    root = maybe_gc(root, 0); // 0% threshold = always GC
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
    RUN_TEST(numeric_helpers_accept_direct_fixnum);
    RUN_TEST(numeric_helpers_reject_token_sentinels);
    RUN_TEST(store_complex_accepts_direct_fixnum_parts);
    RUN_TEST(numeric_sign_helpers_accept_direct_fixnum);
    RUN_TEST(bignum_helpers_accept_direct_fixnum);
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
    RUN_TEST(writer_handles_direct_fixnum);
    RUN_TEST(writer_handles_vector_containing_direct_fixnum);
    RUN_TEST(writer_handles_vm_continuation);
    RUN_TEST(writer_handles_bytecode_closure_marker);
    RUN_TEST(string_port_write_failures_return_false);
    RUN_TEST(make_string_owned_rejects_null);

    // List utilities
    RUN_TEST(list_length_empty);
    RUN_TEST(list_length_non_pair_is_zero);
    RUN_TEST(list_length_three);
    RUN_TEST(list_append_builds_list);

    // Deep equality
    RUN_TEST(deep_equal_numbers);
    RUN_TEST(deep_equal_fixnum_and_boxed_integer);
    RUN_TEST(deep_equal_rejects_token_sentinels);
    RUN_TEST(primitive_eq_handles_fixnum_and_boxed_integer);
    RUN_TEST(primitive_arithmetic_handles_direct_fixnums);
    RUN_TEST(bytevector_primitives_reject_direct_fixnum);
    RUN_TEST(numtower_primitives_handle_direct_fixnums);
    RUN_TEST(math_primitives_handle_direct_fixnums);
    RUN_TEST(number_to_string_handles_direct_fixnum);
    RUN_TEST(procedure_predicate_rejects_pair_with_fixnum_car);
    RUN_TEST(deep_equal_atoms);
    RUN_TEST(deep_equal_lists);
    RUN_TEST(deep_equal_nested);

    // Continuations
    RUN_TEST(make_cont_test);
    RUN_TEST(make_halt_cont_test);
    RUN_TEST(code_free_unregisters_tree);
    RUN_TEST(code_sweep_marks_vm_continuation_code);
    RUN_TEST(gc_updates_vm_continuation_letrec_roots);
    RUN_TEST(vm_continuation_capture_copies_after_gc);
    RUN_TEST(gc_closes_unreachable_file_port);
    RUN_TEST(gc_forgets_unreachable_file_port_reader_state);
    RUN_TEST(gc_preserves_current_file_port_until_replaced);

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
    RUN_TEST(gc_preserves_direct_vector_root);
    RUN_TEST(gc_stress_many_allocations);
    RUN_TEST(gc_stress_deep_nesting);
    RUN_TEST(gc_stress_multiple_collections);
    RUN_TEST(gc_preserves_permanent_atoms);
    RUN_TEST(gc_heap_usage_percent);
    RUN_TEST(gc_maybe_gc_threshold);

    TEST_SUMMARY("context & env");
}
