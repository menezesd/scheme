// Unit tests for context and env modules
#define _POSIX_C_SOURCE 200809L
#include "bytecode.h"
#include "compile_internal.h"
#include "compiled_pattern.h"
#include "context.h"
#include "env.h"
#include "feature_table.h"
#include "prim_internal.h"
#include "primitive_table.h"
#include "primitives.h"
#include "reader.h"
#include "test_framework.h"
#include "types.h"
#include "writer.h"
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <limits.h>
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

TEST(malformed_bignum_payload_is_safe)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_BIGNUM;
    CELL_PTR(bad) = NULL;
    unsigned other_bad = alloc();
    CELL_TYPE(other_bad) = BT_BIGNUM;
    CELL_PTR(other_bad) = NULL;

    ASSERT(!is_negative_number(bad));
    ASSERT_EQ(negate_number(bad), bad);
    ASSERT(!deep_equal(bad, other_bad));

    unsigned args[1] = {bad};
    ASSERT_EQ(apply_primitive_argv(PNUMP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PINTEGERP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PRATIONALP, 1, args), ctx.atom_false);
    ASSERT(apply_primitive_argv(PNUM2STR, 1, args) == TOK_ERROR);
    unsigned cmp_args[2] = {bad, store(0)};
    ASSERT(apply_primitive_argv(PEQUAL, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PLT, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PMOD, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PQUOTIENT, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PREMAINDER, 2, cmp_args) == TOK_ERROR);

    CELL_PTR(bad) = (void *)(uintptr_t)1;
    ASSERT(!is_negative_number(bad));
    ASSERT_EQ(negate_number(bad), bad);
    ASSERT(!deep_equal(bad, other_bad));
    ASSERT_EQ(apply_primitive_argv(PNUMP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PINTEGERP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PRATIONALP, 1, args), ctx.atom_false);
    ASSERT(apply_primitive_argv(PNUM2STR, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PEQUAL, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PLT, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PMOD, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PQUOTIENT, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PREMAINDER, 2, cmp_args) == TOK_ERROR);

    FILE *mem = tmpfile();
    ASSERT(mem != NULL);
    display_obj_port(bad, mem);
    fclose(mem);

    PASS();
}

TEST(malformed_rational_payload_is_not_numeric)
{
    unsigned bad_num = alloc();
    CELL_TYPE(bad_num) = BT_RATIONAL;
    CELL_CAR(bad_num) = ctx.atom_true;
    CELL_CDR(bad_num) = store(1);

    unsigned bad_den = alloc();
    CELL_TYPE(bad_den) = BT_RATIONAL;
    CELL_CAR(bad_den) = store(1);
    CELL_CDR(bad_den) = store(0);

    ASSERT(!is_numeric(bad_num));
    ASSERT(!is_exact(bad_num));
    ASSERT(!is_numeric(bad_den));
    ASSERT(!is_exact(bad_den));
    ASSERT_EQ(to_double(bad_num), 0.0);
    ASSERT_EQ(to_double(bad_den), 0.0);

    unsigned args[1] = {bad_num};
    ASSERT_EQ(apply_primitive_argv(PNUMP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PRATIONALP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PNUMERATOR, 1, args), TOK_ERROR);
    ASSERT_EQ(apply_primitive_argv(PABS, 1, args), TOK_ERROR);

    args[0] = bad_den;
    ASSERT_EQ(apply_primitive_argv(PNUMP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PRATIONALP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PDENOMINATOR, 1, args), TOK_ERROR);
    ASSERT_EQ(apply_primitive_argv(PSQRT, 1, args), TOK_ERROR);
    PASS();
}

TEST(malformed_complex_payload_is_not_numeric)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_COMPLEX;
    CELL_CAR(bad) = ctx.atom_true;
    CELL_CDR(bad) = store(0);

    unsigned nested = alloc();
    CELL_TYPE(nested) = BT_COMPLEX;
    CELL_CAR(nested) = store(1);
    CELL_CDR(nested) = bad;

    ASSERT(!is_numeric(bad));
    ASSERT(!is_exact(bad));
    ASSERT(!is_numeric(nested));
    ASSERT(!is_exact(nested));
    ASSERT_EQ(to_double(bad), 0.0);
    ASSERT_EQ(to_double(nested), 0.0);

    unsigned args[1] = {bad};
    ASSERT_EQ(apply_primitive_argv(PREALPART, 1, args), TOK_ERROR);
    ASSERT_EQ(apply_primitive_argv(PMAGNITUDE, 1, args), TOK_ERROR);

    args[0] = nested;
    ASSERT_EQ(apply_primitive_argv(PIMAGPART, 1, args), TOK_ERROR);
    ASSERT_EQ(apply_primitive_argv(PEXACT2INEXACT, 1, args), TOK_ERROR);
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

TEST(writer_escapes_control_strings_with_scalar_escape)
{
    char bytes[] = {'a', 1, 0x7f, 'b', '\0'};
    unsigned s = make_string_copy(bytes);
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(s, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "\"a\\x01;\\x7F;b\"");
    free(buf);
    PASS();
}

TEST(writer_writes_unicode_character_literal)
{
    unsigned ch = make_char(0x1D11E);
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(ch, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "#\\x1D11E");
    free(buf);
    PASS();
}

TEST(writer_displays_unicode_character_as_utf8)
{
    unsigned ch = make_char(0x03BB);
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    display_obj_port(ch, mem);
    fclose(mem);
    ASSERT_EQ((unsigned char)buf[0], 0xCE);
    ASSERT_EQ((unsigned char)buf[1], 0xBB);
    ASSERT_EQ(buf[2], 0);
    free(buf);
    PASS();
}

TEST(writer_escapes_non_roundtripping_symbols)
{
    unsigned sym = atom_from_string("Hello World");
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(sym, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "|Hello World|");
    free(buf);

    sym = alloc();
    CELL_TYPE(sym) = BT_ATOM;
    CELL_ID(sym) = intern("123");
    buf = NULL;
    len = 0;
    mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);
    write_obj_port(sym, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "|123|");
    free(buf);
    PASS();
}

TEST(writer_escapes_symbol_bar_and_backslash)
{
    unsigned sym = atom_from_string("a|b\\c");
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(sym, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "|a\\|b\\\\c|");
    free(buf);
    PASS();
}

TEST(write_simple_rejects_cycles)
{
    unsigned cell = alloc_cons(store(1), 0);
    cell_set_cdr(cell, cell);

    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    ASSERT(!write_simple_obj_port_checked(cell, mem));
    fclose(mem);
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

TEST(bytevector_primitives_reject_malformed_payload)
{
    unsigned bytevec = alloc();
    CELL_TYPE(bytevec) = BT_BYTEVEC;
    CELL_PTR(bytevec) = NULL;

    unsigned args[1] = {bytevec};
    ASSERT(apply_primitive_argv(PBYTEVECUP, 1, args) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PBYTEVECLEN, 1, args) == TOK_ERROR);

    CELL_PTR(bytevec) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PBYTEVECUP, 1, args) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PBYTEVECLEN, 1, args) == TOK_ERROR);

    bytevec_data *bv = checked_malloc_flex(sizeof(bytevec_data), 4, 1);
    ASSERT(bv != NULL);
    bv->len = 4;
    bytevec_register(bv);
    bv->len = UINT_MAX;
    CELL_PTR(bytevec) = bv;
    ASSERT(apply_primitive_argv(PBYTEVECLEN, 1, args) == TOK_ERROR);

    CELL_PTR(bytevec) = NULL;
    bytevec_unregister(bv);
    free(bv);
    PASS();
}

TEST(string_primitives_reject_malformed_payload)
{
    unsigned string = alloc();
    CELL_TYPE(string) = BT_STRING;
    CELL_PTR(string) = NULL;

    unsigned args[1] = {string};
    ASSERT(apply_primitive_argv(PSTRINGP, 1, args) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PSTRLEN, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PDISPLAY, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PLOAD, 1, args) == TOK_ERROR);

    CELL_PTR(string) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PSTRINGP, 1, args) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PSTRLEN, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PDISPLAY, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PLOAD, 1, args) == TOK_ERROR);

    PASS();
}

TEST(vector_primitives_reject_malformed_payload)
{
    unsigned vector = alloc();
    CELL_TYPE(vector) = BT_VECTOR;
    CELL_PTR(vector) = NULL;

    unsigned args[1] = {vector};
    ASSERT(apply_primitive_argv(PVECTORP, 1, args) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PVECLEN, 1, args) == TOK_ERROR);

    CELL_PTR(vector) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PVECTORP, 1, args) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PVECLEN, 1, args) == TOK_ERROR);

    vector_data *vd = checked_malloc_flex(sizeof(vector_data), 4,
                                          sizeof(unsigned));
    ASSERT(vd != NULL);
    vd->len = 4;
    vector_register(vd);
    vd->len = UINT_MAX;
    CELL_PTR(vector) = vd;
    ASSERT(apply_primitive_argv(PVECLEN, 1, args) == TOK_ERROR);

    CELL_PTR(vector) = NULL;
    vector_unregister(vd);
    free(vd);
    PASS();
}

TEST(port_primitives_reject_malformed_file_payload)
{
    unsigned port = alloc();
    CELL_TYPE(port) = BT_INPORT;
    CELL_PTR(port) = NULL;

    unsigned pred_args[1] = {port};
    ASSERT(apply_primitive_argv(PPORTOPENP, 1, pred_args) == ctx.atom_false);
    ASSERT(apply_primitive_argv(PINPUTPORTOPENP, 1, pred_args) ==
           ctx.atom_false);

    unsigned read_args[2] = {MAKE_FIXNUM(1), port};
    ASSERT(apply_primitive_argv(PREADBYTEVEC, 2, read_args) == TOK_ERROR);

    CELL_PTR(port) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PPORTOPENP, 1, pred_args) == ctx.atom_false);
    ASSERT(apply_primitive_argv(PINPUTPORTOPENP, 1, pred_args) ==
           ctx.atom_false);
    ASSERT(apply_primitive_argv(PREADBYTEVEC, 2, read_args) == TOK_ERROR);
    CELL_PTR(port) = NULL;

    PASS();
}

TEST(port_primitives_reject_malformed_string_payload)
{
    string_port *sp = checked_calloc_array(1, sizeof(string_port));
    ASSERT(sp != NULL);
    sp->data = NULL;
    sp->len = 0;
    sp->cap = 1;
    sp->pos = 0;

    unsigned port = alloc();
    CELL_TYPE(port) = BT_STROUTPORT;
    CELL_PTR(port) = sp;

    unsigned args[1] = {port};
    ASSERT(apply_primitive_argv(PPORTOPENP, 1, args) == ctx.atom_false);
    ASSERT(apply_primitive_argv(PGETOUTPUTSTRING, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PSETCURRENTOUTPUT, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PFLUSHOUTPUT, 1, args) == TOK_ERROR);

    CELL_PTR(port) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PPORTOPENP, 1, args) == ctx.atom_false);
    ASSERT(apply_primitive_argv(PGETOUTPUTSTRING, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PSETCURRENTOUTPUT, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PFLUSHOUTPUT, 1, args) == TOK_ERROR);

    CELL_PTR(port) = NULL;
    free(sp);
    PASS();
}

TEST(hash_table_primitives_reject_malformed_payload)
{
    hash_table_data *ht = checked_malloc_size(sizeof(hash_table_data));
    ASSERT(ht != NULL);
    ht->size = 0;
    ht->capacity = 16;
    ht->equiv = HASH_EQUAL;
    ht->buckets = NULL;
    hash_table_register(ht);

    unsigned table = alloc();
    CELL_TYPE(table) = BT_HASHTABLE;
    CELL_PTR(table) = ht;

    unsigned args[1] = {table};
    ASSERT(apply_primitive_argv(PHASHTABLESIZE, 1, args) == TOK_ERROR);

    CELL_PTR(table) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PHASHTABLESIZE, 1, args) == TOK_ERROR);

    CELL_PTR(table) = NULL;
    hash_table_unregister(ht);
    free(ht);
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

TEST(random_seed_rejects_malformed_bignum)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_BIGNUM;
    CELL_PTR(bad) = NULL;

    unsigned args[1] = {bad};
    ASSERT_EQ(apply_primitive_argv(PRANDOMSEED, 1, args), TOK_ERROR);
    CELL_PTR(bad) = (void *)(uintptr_t)1;
    ASSERT_EQ(apply_primitive_argv(PRANDOMSEED, 1, args), TOK_ERROR);
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

TEST(procedure_predicate_rejects_malformed_bytecode_closure)
{
    unsigned marker = alloc();
    CELL_TYPE(marker) = BT_CLOSURE;
    CELL_PTR(marker) = NULL;
    unsigned closure = alloc_cons(marker, empty_environment());

    unsigned args[1] = {closure};
    ASSERT(apply_primitive_argv(PPROCP, 1, args) == ctx.atom_false);
    ASSERT_EQ(vm_call_closure(closure, 0), TOK_ERROR);

    code_object *unregistered = checked_calloc_array(1, sizeof(code_object));
    ASSERT(unregistered != NULL);
    CELL_PTR(marker) = unregistered;
    ASSERT(apply_primitive_argv(PPROCP, 1, args) == ctx.atom_false);
    ASSERT_EQ(vm_call_closure(closure, 0), TOK_ERROR);
    free(unregistered);
    CELL_PTR(marker) = NULL;
    PASS();
}

TEST(procedure_predicate_rejects_malformed_vm_continuation)
{
    unsigned cont = alloc();
    CELL_TYPE(cont) = BT_VMCONT;
    CELL_PTR(cont) = NULL;

    unsigned args[1] = {cont};
    ASSERT(apply_primitive_argv(PPROCP, 1, args) == ctx.atom_false);

    code_object *code = code_new();
    ASSERT(code != NULL);
    unsigned cont_idx = code_add_const(code, cont);
    code_emit(code, OP_CONST);
    code_emit(code, cont_idx);
    code_emit(code, OP_CALL);
    code_emit(code, 0);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);
    code_free(code);
    ASSERT_EQ(result, TOK_ERROR);

    CELL_PTR(cont) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PPROCP, 1, args) == ctx.atom_false);

    code = code_new();
    ASSERT(code != NULL);
    cont_idx = code_add_const(code, cont);
    code_emit(code, OP_CONST);
    code_emit(code, cont_idx);
    code_emit(code, OP_CALL);
    code_emit(code, 0);
    code_emit(code, OP_HALT);

    vm_init(&vm);
    result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);
    code_free(code);
    ASSERT_EQ(result, TOK_ERROR);
    CELL_PTR(cont) = NULL;

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

static bool context_pattern_registry_contains(compiled_pattern *needle)
{
    for (compiled_pattern *pat = compiled_pattern_registry; pat;
         pat = pat->gc_next) {
        if (pat == needle)
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
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    gc_sweep_code_objects();
    ASSERT(code_registry_contains(code));
    ASSERT(code_registry_contains(child));
    ASSERT(code_registry_contains(frame_code));

    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    code_free(code);
    code_free(frame_code);
    PASS();
}

TEST(code_sweep_ignores_unregistered_closure_pointer)
{
    code_object *unreferenced = code_new();
    ASSERT(unreferenced != NULL);

    unsigned marker = alloc();
    CELL_TYPE(marker) = BT_CLOSURE;
    CELL_PTR(marker) = (void *)(uintptr_t)1;

    gc_sweep_code_objects();

    ASSERT(!code_registry_contains(unreferenced));
    CELL_TYPE(marker) = BT_FREE;
    CELL_PTR(marker) = NULL;
    PASS();
}

TEST(code_sweep_ignores_unregistered_vm_continuation_pointer)
{
    code_object *unreferenced = code_new();
    ASSERT(unreferenced != NULL);

    unsigned cont_cell = alloc();
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = (void *)(uintptr_t)1;

    gc_sweep_code_objects();

    ASSERT(!code_registry_contains(unreferenced));
    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    PASS();
}

TEST(pattern_sweep_ignores_unregistered_pattern_pointer)
{
    compiled_pattern *unreferenced = compiled_pattern_new();
    ASSERT(unreferenced != NULL);

    unsigned cell = alloc();
    CELL_TYPE(cell) = BT_COMPILED_PATTERN;
    CELL_PTR(cell) = (void *)(uintptr_t)1;

    gc_sweep_patterns();

    ASSERT(!context_pattern_registry_contains(unreferenced));
    CELL_TYPE(cell) = BT_FREE;
    CELL_PTR(cell) = NULL;
    PASS();
}

TEST(gc_mark_pattern_ignores_unregistered_pointer)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    ASSERT(context_pattern_registry_contains(pat));

    gc_mark_pattern((compiled_pattern *)(uintptr_t)1);
    ASSERT(!pat->gc_marked);

    compiled_pattern_free(pat);
    PASS();
}

TEST(code_sweep_ignores_malformed_vm_continuation_frames)
{
    code_object *code = code_new();
    ASSERT(code != NULL);

    unsigned cont_cell = alloc();
    vm_continuation *cont = checked_calloc_array(1, sizeof(vm_continuation));
    ASSERT(cont != NULL);
    cont->code = code;
    cont->fp = UINT_MAX;
    cont->frames = NULL;
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    gc_sweep_code_objects();
    ASSERT(code_registry_contains(code));

    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    code_free(code);
    PASS();
}

TEST(peephole_optimize_ignores_truncated_instruction)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_JUMP);

    peephole_optimize(code);

    ASSERT_EQ(code->code_len, 1);
    ASSERT_EQ(code->code[0], OP_JUMP);

    code_free(code);
    PASS();
}

TEST(peephole_optimize_ignores_truncated_child_instruction)
{
    code_object *parent = code_new();
    code_object *child = code_new();
    ASSERT(parent != NULL);
    ASSERT(child != NULL);
    code_add_child(parent, child);
    code_emit(parent, OP_HALT);
    code_emit(child, OP_CONST);

    peephole_optimize(parent);

    ASSERT_EQ(parent->code_len, 1);
    ASSERT_EQ(parent->code[0], OP_HALT);
    ASSERT_EQ(child->code_len, 1);
    ASSERT_EQ(child->code[0], OP_CONST);

    code_free(parent);
    PASS();
}

TEST(peephole_optimize_does_not_thread_into_operand)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_JUMPIF);
    code_emit(code, 3);
    code_emit(code, OP_CONST);
    code_emit(code, OP_JUMP);

    peephole_optimize(code);

    ASSERT_EQ(code->code_len, 4);
    ASSERT_EQ(code->code[0], OP_JUMPIF);
    ASSERT_EQ(code->code[1], 3);
    ASSERT_EQ(code->code[2], OP_CONST);
    ASSERT_EQ(code->code[3], OP_JUMP);

    code_free(code);
    PASS();
}

TEST(vm_run_rejects_truncated_bytecode)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CONST);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_invalid_constant_index)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CONST);
    code_emit(code, 99);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_unknown_opcode)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_COUNT);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_jump_into_operand)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    unsigned value_idx = code_add_const(code, store(1));
    code_emit(code, OP_JUMP);
    code_emit(code, 3);
    code_emit(code, OP_CONST);
    code_emit(code, value_idx);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_invalid_closure_child_index)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CLOSURE);
    code_emit(code, 0);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_primitive_stack_underflow)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_PRIM);
    code_emit(code, PPLUS);
    code_emit(code, 1);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_values_stack_underflow)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_VALUES);
    code_emit(code, 1);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_local_get_out_of_bounds)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_LOCAL_GET);
    code_emit(code, 0);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_local_get_fast_out_of_bounds)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_LOCAL_GET3);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_local_set_out_of_bounds)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    unsigned value_idx = code_add_const(code, store(1));
    code_emit(code, OP_CONST);
    code_emit(code, value_idx);
    code_emit(code, OP_LOCAL_SET);
    code_emit(code, 1);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

#define TEST_IC_UNCACHED 0xFFFFFFFF

static code_object *make_test_lookup_code(int64_t sym_id, unsigned depth,
                                          unsigned offset)
{
    code_object *code = code_new();
    if (!code)
        return NULL;
    code_emit(code, OP_LOOKUP);
    code_emit(code, (unsigned)sym_id);
    code_emit(code, depth);
    code_emit(code, offset);
    code_emit(code, OP_HALT);
    return code;
}

static unsigned run_unary_opcode_on_const(unsigned opcode, unsigned value)
{
    code_object *code = code_new();
    if (!code)
        return TOK_ERROR;
    unsigned idx = code_add_const(code, value);
    code_emit(code, OP_CONST);
    code_emit(code, idx);
    code_emit(code, opcode);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);
    code_free(code);
    return result;
}

TEST(vm_unary_numeric_ops_reject_malformed_bignum)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_BIGNUM;
    CELL_PTR(bad) = NULL;

    unsigned opcodes[] = {OP_ADD1,     OP_SUB1,    OP_NEG, OP_ABS,
                          OP_POSITIVE, OP_NEGATIVE, OP_EVEN, OP_ODD};
    for (size_t i = 0; i < sizeof(opcodes) / sizeof(opcodes[0]); i++) {
        ASSERT_EQ(run_unary_opcode_on_const(opcodes[i], bad), TOK_ERROR);
    }
    CELL_PTR(bad) = (void *)(uintptr_t)1;
    for (size_t i = 0; i < sizeof(opcodes) / sizeof(opcodes[0]); i++) {
        ASSERT_EQ(run_unary_opcode_on_const(opcodes[i], bad), TOK_ERROR);
    }
    PASS();
}

TEST(vm_vector_ops_reject_malformed_payload)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_VECTOR;
    CELL_PTR(bad) = NULL;

    ASSERT_EQ(run_unary_opcode_on_const(OP_VECTORLEN, bad), TOK_ERROR);

    code_object *ref_code = code_new();
    ASSERT(ref_code != NULL);
    unsigned bad_idx = code_add_const(ref_code, bad);
    unsigned zero_idx = code_add_const(ref_code, store(0));
    code_emit(ref_code, OP_CONST);
    code_emit(ref_code, bad_idx);
    code_emit(ref_code, OP_CONST);
    code_emit(ref_code, zero_idx);
    code_emit(ref_code, OP_VECTORREF);
    code_emit(ref_code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, ref_code, empty_environment());
    vm_free(&vm);
    code_free(ref_code);
    ASSERT_EQ(result, TOK_ERROR);

    code_object *set_code = code_new();
    ASSERT(set_code != NULL);
    bad_idx = code_add_const(set_code, bad);
    zero_idx = code_add_const(set_code, store(0));
    unsigned value_idx = code_add_const(set_code, store(1));
    code_emit(set_code, OP_CONST);
    code_emit(set_code, bad_idx);
    code_emit(set_code, OP_CONST);
    code_emit(set_code, zero_idx);
    code_emit(set_code, OP_CONST);
    code_emit(set_code, value_idx);
    code_emit(set_code, OP_VECTORSET);
    code_emit(set_code, OP_HALT);

    vm_init(&vm);
    result = vm_run(&vm, set_code, empty_environment());
    vm_free(&vm);
    code_free(set_code);
    ASSERT_EQ(result, TOK_ERROR);

    PASS();
}

TEST(vm_lookup_rejects_malformed_environment)
{
    unsigned sym = atom_from_string("malformed-env-var");
    code_object *code =
        make_test_lookup_code(CELL_ID(sym), TEST_IC_UNCACHED,
                              TEST_IC_UNCACHED);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, store(1));
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_lookup_rejects_malformed_frame)
{
    unsigned sym = atom_from_string("malformed-frame-var");
    code_object *code =
        make_test_lookup_code(CELL_ID(sym), TEST_IC_UNCACHED,
                              TEST_IC_UNCACHED);
    unsigned env = alloc_cons(store(1), 0);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_lookup_rejects_malformed_frame_variables)
{
    unsigned sym = atom_from_string("malformed-vars-var");
    code_object *code =
        make_test_lookup_code(CELL_ID(sym), TEST_IC_UNCACHED,
                              TEST_IC_UNCACHED);
    unsigned frame = alloc_cons(store(1), 0);
    unsigned env = alloc_cons(frame, 0);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_lookup_ignores_stale_cached_depth)
{
    unsigned env = empty_environment();
    unsigned sym = atom_from_string("stale-depth-var");
    unsigned val = store(42);
    defvar(sym, val, env);
    code_object *code = make_test_lookup_code(CELL_ID(sym), 99, 0);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);

    ASSERT(result != TOK_ERROR);
    ASSERT_EQ(CELL_ID(result), 42);
    code_free(code);
    PASS();
}

TEST(vm_lookup_ignores_stale_cached_offset)
{
    unsigned env = empty_environment();
    unsigned sym = atom_from_string("stale-offset-var");
    unsigned val = store(43);
    defvar(sym, val, env);
    code_object *code = make_test_lookup_code(CELL_ID(sym), 0, 99);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);

    ASSERT(result != TOK_ERROR);
    ASSERT_EQ(CELL_ID(result), 43);
    code_free(code);
    PASS();
}

static unsigned make_test_bytecode_closure(code_object *code, unsigned env)
{
    unsigned marker = alloc();
    CELL_TYPE(marker) = BT_CLOSURE;
    CELL_PTR(marker) = code;
    gc_protect(&marker);
    unsigned closure = alloc_cons(marker, env);
    gc_unprotect(1);
    return closure;
}

TEST(vm_call_closure_rejects_invalid_bytecode)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CONST);
    code->arity = 0;

    unsigned closure = make_test_bytecode_closure(code, empty_environment());
    unsigned result = vm_call_closure(closure, 0);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_call_closure_rejects_invalid_parameter_metadata)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_HALT);
    code->arity = 0;
    code->use_locals = false;
    code->params = 99;

    unsigned closure = make_test_bytecode_closure(code, empty_environment());
    unsigned result = vm_call_closure(closure, 0);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

static code_object *make_test_continuation_call_code(unsigned cont_cell)
{
    code_object *code = code_new();
    if (!code)
        return NULL;
    unsigned cont_idx = code_add_const(code, cont_cell);
    code_emit(code, OP_CONST);
    code_emit(code, cont_idx);
    code_emit(code, OP_CALL);
    code_emit(code, 0);
    code_emit(code, OP_HALT);
    return code;
}

TEST(vm_continuation_rejects_oversized_stack)
{
    code_object *restore_code = code_new();
    ASSERT(restore_code != NULL);
    code_emit(restore_code, OP_HALT);

    unsigned cont_cell = alloc();
    vm_continuation *cont = checked_calloc_array(1, sizeof(vm_continuation));
    ASSERT(cont != NULL);
    cont->sp = UINT_MAX;
    cont->code = restore_code;
    cont->ip = 0;
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    code_object *call_code = make_test_continuation_call_code(cont_cell);
    ASSERT(call_code != NULL);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, call_code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    code_free(call_code);
    code_free(restore_code);
    PASS();
}

TEST(vm_continuation_rejects_oversized_frame_stack)
{
    code_object *restore_code = code_new();
    ASSERT(restore_code != NULL);
    code_emit(restore_code, OP_HALT);

    unsigned cont_cell = alloc();
    vm_continuation *cont = checked_calloc_array(1, sizeof(vm_continuation));
    ASSERT(cont != NULL);
    cont->fp = UINT_MAX;
    cont->code = restore_code;
    cont->ip = 0;
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    code_object *call_code = make_test_continuation_call_code(cont_cell);
    ASSERT(call_code != NULL);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, call_code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    code_free(call_code);
    code_free(restore_code);
    PASS();
}

TEST(execute_pattern_rejects_unknown_opcode)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_OPCODE_COUNT, 0);

    unsigned result = execute_pattern(pat, 0);

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_invalid_literal_index)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_MATCH_LITERAL, 0);
    pattern_emit(pat, PAT_SUCCESS, 0);

    unsigned result = execute_pattern(pat, store(1));

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_invalid_binding_slot)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_BIND_VAR, 0);
    pattern_emit(pat, PAT_SUCCESS, 0);

    unsigned result = execute_pattern(pat, store(1));

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_invalid_jump_target)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_JUMP, 99);

    unsigned result = execute_pattern(pat, 0);

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_input_car_on_non_pair)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_INPUT_CAR, 0);
    pattern_emit(pat, PAT_SUCCESS, 0);

    unsigned result = execute_pattern(pat, store(1));

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_vector_iteration_on_non_vector)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_VEC_ELLIPSIS_INIT, 0);
    pattern_emit(pat, PAT_SUCCESS, 0);

    unsigned result = execute_pattern(pat, store(1));

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_malformed_vector_payload)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_VECTOR;
    CELL_PTR(bad) = NULL;

    unsigned opcodes[] = {PAT_CHECK_VECTOR, PAT_CHECK_VECLEN,
                          PAT_CHECK_VECLEN_MIN, PAT_INPUT_VECREF,
                          PAT_VEC_ELLIPSIS_INIT, PAT_INPUT_VECREF_END};
    for (size_t i = 0; i < sizeof(opcodes) / sizeof(opcodes[0]); i++) {
        compiled_pattern *pat = compiled_pattern_new();
        ASSERT(pat != NULL);
        pattern_emit(pat, opcodes[i], 0);
        pattern_emit(pat, PAT_SUCCESS, 0);
        ASSERT_EQ(execute_pattern(pat, bad), TOK_ERROR);
        compiled_pattern_free(pat);
    }

    compiled_pattern *iter_pat = compiled_pattern_new();
    ASSERT(iter_pat != NULL);
    pattern_emit(iter_pat, PAT_INPUT_VEC_ITER, 0);
    pattern_emit(iter_pat, PAT_SUCCESS, 0);
    ASSERT_EQ(execute_pattern(iter_pat, bad), TOK_ERROR);
    compiled_pattern_free(iter_pat);

    PASS();
}

TEST(gc_updates_vm_continuation_letrec_roots)
{
    unsigned saved = alloc();
    CELL_TYPE(saved) = BT_STRING;
    CELL_PTR(saved) = checked_string_copy("saved");
    ASSERT(CELL_PTR(saved) != NULL);
    string_register(GET_STRING_PTR(saved));

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
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    cont_cell = gc(cont_cell);
    cont = (vm_continuation *)CELL_PTR(cont_cell);
    ASSERT(CELL_TYPE(cont->letrec_frame) == BT_CONS);
    ASSERT(CELL_TYPE(cont->letrec_saved[0]) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(cont->letrec_saved[0]), "saved");
    PASS();
}

TEST(gc_ignores_malformed_vm_continuation_arrays)
{
    unsigned env_marker = alloc();
    CELL_TYPE(env_marker) = BT_STRING;
    CELL_PTR(env_marker) = checked_string_copy("env-marker");
    ASSERT(CELL_PTR(env_marker) != NULL);
    string_register(GET_STRING_PTR(env_marker));

    unsigned cont_cell = alloc();
    vm_continuation *cont = checked_calloc_array(1, sizeof(vm_continuation));
    ASSERT(cont != NULL);
    cont->sp = UINT_MAX;
    cont->fp = UINT_MAX;
    cont->letrec_saved_len = UINT_MAX;
    cont->env = env_marker;
    cont->letrec_frame = env_marker;
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    cont_cell = gc(cont_cell);
    ASSERT(CELL_TYPE(cont_cell) == BT_VMCONT);
    cont = (vm_continuation *)CELL_PTR(cont_cell);
    ASSERT(cont != NULL);
    ASSERT(CELL_TYPE(cont->env) == BT_STRING);
    ASSERT(CELL_TYPE(cont->letrec_frame) == BT_STRING);

    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
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

    vm_continuation_unregister(cont);
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

TEST(lookup_rejects_malformed_environment)
{
    unsigned x = atom_from_string("malformed-lookup");
    unsigned result = lookup(CELL_ID(x), store(1));

    ASSERT_EQ(result, TOK_ERROR);
    PASS();
}

TEST(lookup_silent_rejects_malformed_frame)
{
    unsigned x = atom_from_string("malformed-silent-lookup");
    unsigned env = alloc_cons(store(1), 0);
    unsigned result = lookup_silent(CELL_ID(x), env);

    ASSERT_EQ(result, TOK_ERROR);
    PASS();
}

TEST(setvar_rejects_malformed_environment)
{
    unsigned x = atom_from_string("malformed-setvar");
    unsigned result = setvar(CELL_ID(x), store(1), store(2));

    ASSERT_EQ(result, TOK_ERROR);
    PASS();
}

TEST(env_find_binding_cell_rejects_malformed_environment)
{
    unsigned x = atom_from_string("malformed-env-find");
    unsigned result = env_find_binding_cell(CELL_ID(x), store(1));

    ASSERT_EQ(result, 0);
    PASS();
}

TEST(defvar_rejects_malformed_environment)
{
    unsigned x = atom_from_string("malformed-defvar");
    unsigned result = defvar(x, store(1), store(2));

    ASSERT_EQ(result, TOK_ERROR);
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

TEST(bind_params_rejects_invalid_formals)
{
    unsigned params = alloc_cons(atom_from_string("x"), store(1));
    unsigned args = alloc_cons(store(1), 0);

    unsigned frame = bind_params(params, args);

    ASSERT_EQ(frame, TOK_ERROR);
    PASS();
}

TEST(bind_params_rejects_duplicate_formals)
{
    unsigned x = atom_from_string("dup-formal");
    unsigned params = alloc_cons(x, alloc_cons(x, 0));
    unsigned args = alloc_cons(store(1), alloc_cons(store(2), 0));

    unsigned frame = bind_params(params, args);

    ASSERT_EQ(frame, TOK_ERROR);
    PASS();
}

TEST(bind_params_rest_only_collects_all_args)
{
    unsigned rest = atom_from_string("rest-only");
    unsigned args = alloc_cons(store(1), alloc_cons(store(2), 0));

    unsigned frame = bind_params(rest, args);

    ASSERT(frame != TOK_ERROR);
    ASSERT(IS_PAIR(car(frame)));
    ASSERT_EQ(car(car(frame)), rest);
    ASSERT(IS_PAIR(cdr(frame)));
    ASSERT_EQ(car(cdr(frame)), args);
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

TEST(primitive_table_is_well_formed)
{
    const primitive_binding *bindings = primitive_bindings();
    size_t count = primitive_binding_count();

    ASSERT(bindings != NULL);
    ASSERT(count > 0);
    ASSERT(bindings[count].name == NULL);

    for (size_t i = 0; i < count; i++) {
        ASSERT(bindings[i].name != NULL);
        ASSERT(bindings[i].name[0] != '\0');
        ASSERT(bindings[i].prim >= 0);
        ASSERT(bindings[i].prim < PRIM_COUNT);
        for (size_t j = i + 1; j < count; j++) {
            ASSERT(strcmp(bindings[i].name, bindings[j].name) != 0);
        }
    }

    PASS();
}

TEST(feature_table_is_well_formed)
{
    const char *const *features = feature_names();
    size_t count = feature_name_count();

    ASSERT(features != NULL);
    ASSERT(count > 0);
    ASSERT(features[count] == NULL);

    for (size_t i = 0; i < count; i++) {
        ASSERT(features[i] != NULL);
        ASSERT(features[i][0] != '\0');
        for (size_t j = i + 1; j < count; j++) {
            ASSERT(strcmp(features[i], features[j]) != 0);
        }
    }

    PASS();
}

TEST(cond_expand_rejects_malformed_and_requirement)
{
    unsigned req = alloc_cons(atom_from_string("and"), store(1));
    ASSERT(!cond_expand_requirement_satisfied(req));
    PASS();
}

TEST(cond_expand_rejects_malformed_or_requirement)
{
    unsigned req = alloc_cons(atom_from_string("or"), store(1));
    ASSERT(!cond_expand_requirement_satisfied(req));
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

TEST(gc_ignores_malformed_hash_table_payload)
{
    hash_table_data *ht = checked_malloc_size(sizeof(hash_table_data));
    ASSERT(ht != NULL);
    ht->size = 0;
    ht->capacity = 16;
    ht->equiv = HASH_EQUAL;
    ht->buckets = NULL;
    hash_table_register(ht);

    unsigned table = alloc();
    CELL_TYPE(table) = BT_HASHTABLE;
    CELL_PTR(table) = ht;

    unsigned preserved = gc(table);
    ASSERT(CELL_TYPE(preserved) == BT_HASHTABLE);
    ASSERT(GET_HASHTABLE_PTR(preserved) == ht);

    CELL_TYPE(preserved) = BT_FREE;
    CELL_PTR(preserved) = NULL;
    hash_table_unregister(ht);
    free(ht);
    PASS();
}

TEST(gc_ignores_malformed_vector_payload)
{
    unsigned vector = alloc();
    CELL_TYPE(vector) = BT_VECTOR;
    CELL_PTR(vector) = NULL;

    unsigned preserved = gc(vector);
    ASSERT(CELL_TYPE(preserved) == BT_VECTOR);
    ASSERT(GET_VECTOR_PTR(preserved) == NULL);

    CELL_PTR(preserved) = (void *)(uintptr_t)1;
    preserved = gc(preserved);
    ASSERT(CELL_TYPE(preserved) == BT_VECTOR);
    ASSERT(GET_VECTOR_PTR(preserved) == (void *)(uintptr_t)1);

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
    RUN_TEST(malformed_bignum_payload_is_safe);
    RUN_TEST(malformed_rational_payload_is_not_numeric);
    RUN_TEST(malformed_complex_payload_is_not_numeric);
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
    RUN_TEST(writer_escapes_control_strings_with_scalar_escape);
    RUN_TEST(writer_writes_unicode_character_literal);
    RUN_TEST(writer_displays_unicode_character_as_utf8);
    RUN_TEST(writer_escapes_non_roundtripping_symbols);
    RUN_TEST(writer_escapes_symbol_bar_and_backslash);
    RUN_TEST(write_simple_rejects_cycles);
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
    RUN_TEST(bytevector_primitives_reject_malformed_payload);
    RUN_TEST(string_primitives_reject_malformed_payload);
    RUN_TEST(vector_primitives_reject_malformed_payload);
    RUN_TEST(port_primitives_reject_malformed_file_payload);
    RUN_TEST(port_primitives_reject_malformed_string_payload);
    RUN_TEST(hash_table_primitives_reject_malformed_payload);
    RUN_TEST(numtower_primitives_handle_direct_fixnums);
    RUN_TEST(math_primitives_handle_direct_fixnums);
    RUN_TEST(random_seed_rejects_malformed_bignum);
    RUN_TEST(number_to_string_handles_direct_fixnum);
    RUN_TEST(procedure_predicate_rejects_pair_with_fixnum_car);
    RUN_TEST(procedure_predicate_rejects_malformed_bytecode_closure);
    RUN_TEST(procedure_predicate_rejects_malformed_vm_continuation);
    RUN_TEST(deep_equal_atoms);
    RUN_TEST(deep_equal_lists);
    RUN_TEST(deep_equal_nested);

    // Continuations
    RUN_TEST(make_cont_test);
    RUN_TEST(make_halt_cont_test);
    RUN_TEST(code_free_unregisters_tree);
    RUN_TEST(code_sweep_marks_vm_continuation_code);
    RUN_TEST(code_sweep_ignores_unregistered_closure_pointer);
    RUN_TEST(code_sweep_ignores_unregistered_vm_continuation_pointer);
    RUN_TEST(pattern_sweep_ignores_unregistered_pattern_pointer);
    RUN_TEST(gc_mark_pattern_ignores_unregistered_pointer);
    RUN_TEST(code_sweep_ignores_malformed_vm_continuation_frames);
    RUN_TEST(peephole_optimize_ignores_truncated_instruction);
    RUN_TEST(peephole_optimize_ignores_truncated_child_instruction);
    RUN_TEST(peephole_optimize_does_not_thread_into_operand);
    RUN_TEST(vm_run_rejects_truncated_bytecode);
    RUN_TEST(vm_run_rejects_invalid_constant_index);
    RUN_TEST(vm_run_rejects_unknown_opcode);
    RUN_TEST(vm_run_rejects_jump_into_operand);
    RUN_TEST(vm_run_rejects_invalid_closure_child_index);
    RUN_TEST(vm_run_rejects_primitive_stack_underflow);
    RUN_TEST(vm_run_rejects_values_stack_underflow);
    RUN_TEST(vm_run_rejects_local_get_out_of_bounds);
    RUN_TEST(vm_run_rejects_local_get_fast_out_of_bounds);
    RUN_TEST(vm_run_rejects_local_set_out_of_bounds);
    RUN_TEST(vm_unary_numeric_ops_reject_malformed_bignum);
    RUN_TEST(vm_vector_ops_reject_malformed_payload);
    RUN_TEST(vm_lookup_rejects_malformed_environment);
    RUN_TEST(vm_lookup_rejects_malformed_frame);
    RUN_TEST(vm_lookup_rejects_malformed_frame_variables);
    RUN_TEST(vm_lookup_ignores_stale_cached_depth);
    RUN_TEST(vm_lookup_ignores_stale_cached_offset);
    RUN_TEST(vm_call_closure_rejects_invalid_bytecode);
    RUN_TEST(vm_call_closure_rejects_invalid_parameter_metadata);
    RUN_TEST(vm_continuation_rejects_oversized_stack);
    RUN_TEST(vm_continuation_rejects_oversized_frame_stack);
    RUN_TEST(execute_pattern_rejects_unknown_opcode);
    RUN_TEST(execute_pattern_rejects_invalid_literal_index);
    RUN_TEST(execute_pattern_rejects_invalid_binding_slot);
    RUN_TEST(execute_pattern_rejects_invalid_jump_target);
    RUN_TEST(execute_pattern_rejects_input_car_on_non_pair);
    RUN_TEST(execute_pattern_rejects_vector_iteration_on_non_vector);
    RUN_TEST(execute_pattern_rejects_malformed_vector_payload);
    RUN_TEST(gc_updates_vm_continuation_letrec_roots);
    RUN_TEST(gc_ignores_malformed_vm_continuation_arrays);
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
    RUN_TEST(lookup_rejects_malformed_environment);
    RUN_TEST(lookup_silent_rejects_malformed_frame);
    RUN_TEST(setvar_rejects_malformed_environment);
    RUN_TEST(env_find_binding_cell_rejects_malformed_environment);
    RUN_TEST(defvar_rejects_malformed_environment);
    RUN_TEST(bind_params_simple);
    RUN_TEST(bind_params_rejects_invalid_formals);
    RUN_TEST(bind_params_rejects_duplicate_formals);
    RUN_TEST(bind_params_rest_only_collects_all_args);
    RUN_TEST(mk_primop_test);
    RUN_TEST(default_environment_has_primitives);
    RUN_TEST(primitive_table_is_well_formed);
    RUN_TEST(feature_table_is_well_formed);
    RUN_TEST(cond_expand_rejects_malformed_and_requirement);
    RUN_TEST(cond_expand_rejects_malformed_or_requirement);

    // GC
    RUN_TEST(gc_preserves_root);
    RUN_TEST(gc_preserves_list);
    RUN_TEST(gc_preserves_vector);
    RUN_TEST(gc_preserves_direct_vector_root);
    RUN_TEST(gc_ignores_malformed_hash_table_payload);
    RUN_TEST(gc_ignores_malformed_vector_payload);
    RUN_TEST(gc_stress_many_allocations);
    RUN_TEST(gc_stress_deep_nesting);
    RUN_TEST(gc_stress_multiple_collections);
    RUN_TEST(gc_preserves_permanent_atoms);
    RUN_TEST(gc_heap_usage_percent);
    RUN_TEST(gc_maybe_gc_threshold);

    TEST_SUMMARY("context & env");
}
