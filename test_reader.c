// Unit tests for reader module
#define _POSIX_C_SOURCE 200809L
#include "context.h"
#include "reader.h"
#include "test_framework.h"
#include "writer.h"
#include "types.h"
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// ============================================================================
// Helper: Read from string
// ============================================================================

static unsigned read_from_string(const char *str)
{
    FILE *f = fmemopen((void *)str, strlen(str), "r");
    if (!f)
        return TOK_ERROR;
    unsigned result = read_obj_port(f);
    reader_forget_port(f);
    fclose(f);
    return result;
}

// ============================================================================
// Reader Tests - Numbers
// ============================================================================

TEST(read_positive_integer)
{
    unsigned x = read_from_string("42");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

TEST(read_negative_integer)
{
    unsigned x = read_from_string("-123");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), -123);
    PASS();
}

TEST(read_zero)
{
    unsigned x = read_from_string("0");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 0);
    PASS();
}

TEST(read_floating_point)
{
    unsigned x = read_from_string("3.14");
    ASSERT(CELL_TYPE(x) == BT_INEXACT);
    double val = to_double(x);
    ASSERT(val > 3.13 && val < 3.15);
    PASS();
}

TEST(read_complex_nonfinite_components)
{
    const char *cases[] = {"+inf.0+1.0i", "-inf.0+1.0i", "+nan.0+1.0i",
                           "1.0+nan.0i", "+inf.0i", "-nan.0i",
                           "+INF.0+NaN.0i", "+I", "-I"};
    for (unsigned i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        unsigned value = read_from_string(cases[i]);
        ASSERT(IS_COMPLEX(value));
        ASSERT(is_numeric(value));
    }
    unsigned value = read_from_string("+inf.0-1.0i");
    ASSERT(to_double(CELL_CAR(value)) == HUGE_VAL);
    ASSERT(to_double(CELL_CDR(value)) == -1.0);
    PASS();
}

TEST(read_rational)
{
    unsigned x = read_from_string("1/3");
    ASSERT(CELL_TYPE(x) == BT_RATIONAL);
    // Rational stores num in car(x), denom in cdr(x) - both are cells
    ASSERT_EQ(CELL_ID(car(x)), 1);
    ASSERT_EQ(CELL_ID(cdr(x)), 3);
    PASS();
}

TEST(read_bignum)
{
    unsigned x = read_from_string("10000000000000000000");
    ASSERT(CELL_TYPE(x) == BT_BIGNUM);
    bignum *bn = get_bignum(x);
    ASSERT(bn != NULL);
    PASS();
}

TEST(read_hex_integer)
{
    unsigned x = read_from_string("#x2a");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

TEST(read_octal_integer)
{
    unsigned x = read_from_string("#o52");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

TEST(read_binary_integer)
{
    unsigned x = read_from_string("#b101010");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

TEST(read_prefixed_signed_integer)
{
    unsigned x = read_from_string("#b-101010");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), -42);
    x = read_from_string("#o+52");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

TEST(read_prefixed_uppercase_radix)
{
    unsigned x = read_from_string("#X2A");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    x = read_from_string("#O52");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    x = read_from_string("#B101010");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

TEST(read_prefixed_bignum)
{
    unsigned x = read_from_string("#x8000000000000000");
    ASSERT(CELL_TYPE(x) == BT_BIGNUM);
    char *s = bn_to_string(get_bignum(x), 10);
    ASSERT_STR_EQ(s, "9223372036854775808");
    free(s);
    PASS();
}

TEST(read_prefixed_int64_min)
{
    unsigned x = read_from_string("#x-8000000000000000");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), INT64_MIN);
    PASS();
}

TEST(read_prefixed_integer_rejects_trailing_junk)
{
    ASSERT(read_from_string("#x1z") == TOK_ERROR);
    ASSERT(read_from_string("#o18") == TOK_ERROR);
    ASSERT(read_from_string("#b102") == TOK_ERROR);
    PASS();
}

TEST(read_exactness_and_radix_prefixes)
{
    unsigned x = read_from_string("#d42");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);

    x = read_from_string("#i#x10");
    ASSERT(IS_INEXACT(x));
    ASSERT(to_double(x) == 16.0);

    x = read_from_string("#x#i10");
    ASSERT(IS_INEXACT(x));
    ASSERT(to_double(x) == 16.0);

    x = read_from_string("#e#b1010");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 10);

    x = read_from_string("#e1.50");
    ASSERT(IS_RATIONAL(x));
    ASSERT_EQ(CELL_ID(CELL_CAR(x)), 3);
    ASSERT_EQ(CELL_ID(CELL_CDR(x)), 2);

    x = read_from_string("#e1.25e2");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 125);
    PASS();
}

TEST(read_prefixed_complex_exactness)
{
    unsigned x = read_from_string("#e1.5+2.5i");
    ASSERT(IS_COMPLEX(x));
    ASSERT(is_exact(x));
    ASSERT(to_double(CELL_CAR(x)) == 1.5);
    ASSERT(to_double(CELL_CDR(x)) == 2.5);

    x = read_from_string("#i#x1e+ai");
    ASSERT(IS_COMPLEX(x));
    ASSERT(IS_INEXACT(CELL_CAR(x)));
    ASSERT(IS_INEXACT(CELL_CDR(x)));
    ASSERT(to_double(CELL_CAR(x)) == 30.0);
    ASSERT(to_double(CELL_CDR(x)) == 10.0);

    x = read_from_string(".5+2i");
    ASSERT(IS_COMPLEX(x));
    ASSERT(to_double(CELL_CAR(x)) == 0.5);
    ASSERT(to_double(CELL_CDR(x)) == 2.0);
    ASSERT(read_from_string("#e+inf.0+1i") == TOK_ERROR);
    ASSERT(read_from_string("#e1+nan.0i") == TOK_ERROR);
    ASSERT(read_from_string("#i#i1+2i") == TOK_ERROR);
    PASS();
}

TEST(read_repeated_imaginary_suffix_does_not_recurse)
{
    char input[8192];
    input[0] = '2';
    memset(input + 1, 'i', sizeof(input) - 2);
    input[sizeof(input) - 1] = '\0';
    unsigned value = read_from_string(input);
    ASSERT(CELL_TYPE(value) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(value)], input);
    PASS();
}

TEST(read_polar_numbers)
{
    unsigned x = read_from_string("1/2@0");
    ASSERT(IS_RATIONAL(x));
    ASSERT(is_exact(x));
    ASSERT(to_double(x) == 0.5);

    x = read_from_string("#x10@0");
    ASSERT(is_exact(x));
    ASSERT(to_double(x) == 16.0);

    x = read_from_string(".5@0");
    ASSERT(IS_INEXACT(x));
    ASSERT(to_double(x) == 0.5);

    x = read_from_string("#i2@0");
    ASSERT(IS_COMPLEX(x));
    ASSERT(to_double(CELL_CAR(x)) == 2.0);
    ASSERT(IS_INEXACT(CELL_CDR(x)));

    x = read_from_string("1@-0.0");
    ASSERT(IS_COMPLEX(x));
    ASSERT(signbit(to_double(CELL_CDR(x))));
    ASSERT(parse_number_string("1@2@3", 10) == ctx.atom_false);
    ASSERT(parse_number_string("1@+i", 10) == ctx.atom_false);
    PASS();
}

TEST(read_malformed_rational_as_symbol)
{
    unsigned x = read_from_string("12abc/2");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    PASS();
}

TEST(read_malformed_denominator_as_symbol)
{
    unsigned x = read_from_string("1/abc");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    PASS();
}

TEST(read_exact_decimal_rejects_scale_overflow)
{
    ASSERT(read_from_string("#e1.0e-9223372036854775807") == TOK_ERROR);
    ASSERT(read_from_string("#e1e9223372036854775807") == TOK_ERROR);
    PASS();
}

// ============================================================================
// Reader Tests - Strings
// ============================================================================

TEST(read_simple_string)
{
    unsigned x = read_from_string("\"hello\"");
    ASSERT(CELL_TYPE(x) == BT_STRING);
    const char *str = GET_STRING_PTR(x);
    ASSERT_STR_EQ(str, "hello");
    ASSERT(string_is_immutable(str));
    PASS();
}

TEST(read_string_with_escapes)
{
    unsigned x = read_from_string("\"hello\\nworld\"");
    ASSERT(CELL_TYPE(x) == BT_STRING);
    const char *str = GET_STRING_PTR(x);
    ASSERT_STR_EQ(str, "hello\nworld");
    PASS();
}

TEST(read_string_hex_scalar_escape)
{
    unsigned x = read_from_string("\"\\x1D11E;\"");
    ASSERT(CELL_TYPE(x) == BT_STRING);
    const unsigned char *str = (const unsigned char *)GET_STRING_PTR(x);
    ASSERT_EQ(str[0], 0xF0);
    ASSERT_EQ(str[1], 0x9D);
    ASSERT_EQ(str[2], 0x84);
    ASSERT_EQ(str[3], 0x9E);
    ASSERT_EQ(str[4], 0);
    PASS();
}

TEST(read_string_rejects_invalid_scalar_escapes)
{
    ASSERT(read_from_string("\"\\x0;\"") == TOK_ERROR);
    ASSERT(read_from_string("\"\\000\"") == TOK_ERROR);
    ASSERT(read_from_string("\"\\xD800;\"") == TOK_ERROR);
    ASSERT(read_from_string("\"\\x110000;\"") == TOK_ERROR);
    PASS();
}

TEST(read_string_rejects_invalid_utf8)
{
    const char invalid[] = {'"', (char)0xC0, (char)0x80, '"', '\0'};
    ASSERT(read_from_string(invalid) == TOK_ERROR);
    PASS();
}

TEST(read_token_rejects_null_character)
{
    const char invalid[] = {'a', '\0', 'b', '\0'};
    FILE *f = fmemopen((void *)invalid, 3, "r");
    ASSERT(f != NULL);
    ASSERT(read_obj_port(f) == TOK_ERROR);
    reader_forget_port(f);
    fclose(f);
    PASS();
}

TEST(read_numeric_token_rejects_null_character)
{
    const char invalid[] = {'#', 'd', '1', '\0', '2', '\0'};
    FILE *f = fmemopen((void *)invalid, 5, "r");
    ASSERT(f != NULL);
    ASSERT(read_obj_port(f) == TOK_ERROR);
    reader_forget_port(f);
    fclose(f);
    PASS();
}

TEST(read_string_rejects_null_character)
{
    const char invalid[] = {'"', 'a', '\0', 'b', '"', '\0'};
    FILE *f = fmemopen((void *)invalid, 5, "r");
    ASSERT(f != NULL);
    ASSERT(read_obj_port(f) == TOK_ERROR);
    reader_forget_port(f);
    fclose(f);
    PASS();
}

TEST(read_empty_string)
{
    unsigned x = read_from_string("\"\"");
    ASSERT(CELL_TYPE(x) == BT_STRING);
    const char *str = GET_STRING_PTR(x);
    ASSERT_STR_EQ(str, "");
    PASS();
}

// ============================================================================
// Reader Tests - Characters
// ============================================================================

TEST(read_char_literal)
{
    unsigned x = read_from_string("#\\a");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), 'a');
    PASS();
}

TEST(read_char_space)
{
    unsigned x = read_from_string("#\\space");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), ' ');
    PASS();
}

TEST(read_char_newline)
{
    unsigned x = read_from_string("#\\newline");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), '\n');
    PASS();
}

TEST(read_char_tab)
{
    unsigned x = read_from_string("#\\tab");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), '\t');
    PASS();
}

TEST(read_char_named_controls)
{
    unsigned x = read_from_string("#\\alarm");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), '\a');
    x = read_from_string("#\\backspace");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), '\b');
    x = read_from_string("#\\delete");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), 0x7f);
    x = read_from_string("#\\escape");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), 27);
    x = read_from_string("#\\null");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), 0);
    x = read_from_string("#\\return");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), '\r');
    PASS();
}

TEST(read_char_hex_scalar)
{
    unsigned x = read_from_string("#\\x1D11E");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), 0x1D11E);
    x = read_from_string("#\\x0");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), 0);
    PASS();
}

TEST(read_char_utf8_literal)
{
    unsigned x = read_from_string("#\\λ");
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), 0x03BB);
    PASS();
}

TEST(read_char_rejects_invalid_scalar)
{
    ASSERT(read_from_string("#\\xD800") == TOK_ERROR);
    ASSERT(read_from_string("#\\x110000") == TOK_ERROR);
    ASSERT(read_from_string("#\\x1g") == TOK_ERROR);
    PASS();
}

TEST(read_char_rejects_multi_char_utf8_literal)
{
    ASSERT(read_from_string("#\\λλ") == TOK_ERROR);
    PASS();
}

TEST(read_char_rejects_eof)
{
    unsigned x = read_from_string("#\\");
    ASSERT(x == TOK_ERROR);
    PASS();
}

TEST(read_char_rejects_unknown_name)
{
    unsigned x = read_from_string("#\\spacebar");
    ASSERT(x == TOK_ERROR);
    PASS();
}

// ============================================================================
// Reader Tests - Symbols
// ============================================================================

TEST(read_symbol)
{
    unsigned x = read_from_string("foo");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "foo");
    PASS();
}

TEST(read_symbol_case_insensitive)
{
    unsigned x = read_from_string("FOO");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "foo");
    PASS();
}

TEST(read_fold_case_directive)
{
    unsigned x = read_from_string("#!fold-case FOO");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "foo");
    PASS();
}

TEST(read_no_fold_case_directive)
{
    unsigned x = read_from_string("#!no-fold-case FOO");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "FOO");
    PASS();
}

TEST(read_case_directive_persists_for_port)
{
    const char *src = "#!no-fold-case FOO BAR";
    FILE *f = fmemopen((void *)src, strlen(src), "r");
    ASSERT(f != NULL);

    unsigned first = read_obj_port(f);
    ASSERT(CELL_TYPE(first) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(first)], "FOO");

    unsigned second = read_obj_port(f);
    ASSERT(CELL_TYPE(second) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(second)], "BAR");

    reader_forget_port(f);
    fclose(f);
    PASS();
}

TEST(read_case_directive_can_toggle)
{
    const char *src = "#!no-fold-case FOO #!fold-case BAR";
    FILE *f = fmemopen((void *)src, strlen(src), "r");
    ASSERT(f != NULL);

    unsigned first = read_obj_port(f);
    ASSERT(CELL_TYPE(first) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(first)], "FOO");

    unsigned second = read_obj_port(f);
    ASSERT(CELL_TYPE(second) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(second)], "bar");

    reader_forget_port(f);
    fclose(f);
    PASS();
}

TEST(read_unknown_directive_rejected)
{
    ASSERT(read_from_string("#!wat FOO") == TOK_ERROR);
    PASS();
}

TEST(read_datum_comment_skips_one_object)
{
    unsigned x = read_from_string("#;FOO BAR");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "bar");
    PASS();
}

TEST(read_datum_comment_skips_nested_object)
{
    unsigned x = read_from_string("#;(1 (2 3) #(4 5)) done");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "done");
    PASS();
}

TEST(read_datum_comment_rejects_missing_datum)
{
    ASSERT(read_from_string("#;") == TOK_ERROR);
    PASS();
}

TEST(read_nested_block_comment)
{
    unsigned x = read_from_string("#| outer #| inner |# done |# kept");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "kept");
    PASS();
}

TEST(read_block_comment_rejects_unterminated)
{
    ASSERT(read_from_string("#| outer #| inner |#") == TOK_ERROR);
    PASS();
}

TEST(read_symbol_with_special_chars)
{
    unsigned x = read_from_string("foo-bar?");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "foo-bar?");
    PASS();
}

TEST(read_symbol_preserves_utf8_identifier)
{
    unsigned x = read_from_string("λ");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "λ");
    PASS();
}

TEST(read_symbol_ascii_folds_without_mangling_utf8)
{
    unsigned x = read_from_string("Fooλ");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "fooλ");
    PASS();
}

TEST(read_symbol_unicode_foldcase)
{
    unsigned x = read_from_string("ẞİKſµΣςσﬃ");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "ssi̇ksμσσσffi");
    PASS();
}

TEST(read_no_fold_case_preserves_unicode_identifier)
{
    unsigned x = read_from_string("#!no-fold-case ẞİKſµΣςσﬃ");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "ẞİKſµΣςσﬃ");
    PASS();
}

TEST(read_symbol_rejects_invalid_utf8)
{
    const char invalid[] = {(char)0xC0, (char)0x80, '\0'};
    ASSERT(read_from_string(invalid) == TOK_ERROR);
    PASS();
}

TEST(read_escaped_identifier)
{
    unsigned x = read_from_string("|Hello World|");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "Hello World");
    x = read_from_string("|\\x3BB;|");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "λ");
    x = read_from_string("|a\\|b\\\\c|");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "a|b\\c");
    PASS();
}

TEST(read_escaped_numeric_identifier)
{
    const char *inputs[] = {"|123|", "|1/3|", "|+i|", "|+inf.0+1.0i|",
                            "|+nan.0|", "||"};
    const char *names[] = {"123", "1/3", "+i", "+inf.0+1.0i", "+nan.0", ""};
    for (unsigned i = 0; i < sizeof(inputs) / sizeof(inputs[0]); i++) {
        unsigned value = read_from_string(inputs[i]);
        ASSERT(CELL_TYPE(value) == BT_ATOM);
        ASSERT_STR_EQ(ctx.atom_table[CELL_ID(value)], names[i]);
    }
    PASS();
}

TEST(read_escaped_identifier_rejects_invalid_escape)
{
    ASSERT(read_from_string("|abc") == TOK_ERROR);
    ASSERT(read_from_string("|\\q|") == TOK_ERROR);
    ASSERT(read_from_string("|\\xD800;|") == TOK_ERROR);
    PASS();
}

// ============================================================================
// Reader Tests - Booleans
// ============================================================================

TEST(read_true)
{
    unsigned x = read_from_string("#t");
    ASSERT(x == ctx.atom_true);
    PASS();
}

TEST(read_false)
{
    unsigned x = read_from_string("#f");
    ASSERT(x == ctx.atom_false);
    PASS();
}

TEST(read_boolean_aliases)
{
    ASSERT(read_from_string("#T") == ctx.atom_true);
    ASSERT(read_from_string("#TRUE") == ctx.atom_true);
    ASSERT(read_from_string("#true") == ctx.atom_true);
    ASSERT(read_from_string("#F") == ctx.atom_false);
    ASSERT(read_from_string("#FALSE") == ctx.atom_false);
    ASSERT(read_from_string("#false") == ctx.atom_false);
    PASS();
}

TEST(read_boolean_rejects_invalid_suffix)
{
    ASSERT(read_from_string("#tfoo") == TOK_ERROR);
    ASSERT(read_from_string("#f0") == TOK_ERROR);
    ASSERT(read_from_string("#truex") == TOK_ERROR);
    ASSERT(read_from_string("#falsey") == TOK_ERROR);
    PASS();
}

// ============================================================================
// Reader Tests - Lists
// ============================================================================

TEST(read_empty_list)
{
    unsigned x = read_from_string("()");
    ASSERT(x == 0);
    PASS();
}

TEST(read_simple_list)
{
    unsigned x = read_from_string("(1 2 3)");
    ASSERT(CELL_TYPE(x) == BT_CONS);
    ASSERT(CELL_TYPE(car(x)) == BT_NUM);
    ASSERT_EQ(CELL_ID(car(x)), 1);
    ASSERT(CELL_TYPE(cadr(x)) == BT_NUM);
    ASSERT_EQ(CELL_ID(cadr(x)), 2);
    ASSERT(CELL_TYPE(caddr(x)) == BT_NUM);
    ASSERT_EQ(CELL_ID(caddr(x)), 3);
    ASSERT(cdddr(x) == 0);
    PASS();
}

TEST(read_dotted_pair)
{
    unsigned x = read_from_string("(1 . 2)");
    ASSERT(CELL_TYPE(x) == BT_CONS);
    ASSERT(CELL_TYPE(car(x)) == BT_NUM);
    ASSERT_EQ(CELL_ID(car(x)), 1);
    ASSERT(CELL_TYPE(cdr(x)) == BT_NUM);
    ASSERT_EQ(CELL_ID(cdr(x)), 2);
    PASS();
}

TEST(read_rejects_dot_without_preceding_object)
{
    ASSERT(read_from_string("(. a)") == TOK_ERROR);
    PASS();
}

TEST(read_large_flat_list_without_recursion)
{
    const size_t count = 100000;
    char *src = malloc(2 * count + 3);
    ASSERT(src != NULL);
    src[0] = '(';
    for (size_t i = 0; i < count; i++) {
        src[1 + 2 * i] = 'x';
        src[2 + 2 * i] = ' ';
    }
    src[1 + 2 * count] = ')';
    src[2 + 2 * count] = '\0';

    unsigned x = read_from_string(src);
    free(src);
    ASSERT(x != TOK_ERROR);
    unsigned length = 0;
    ASSERT(list_length_checked(x, &length, "test"));
    ASSERT_EQ(length, count);
    PASS();
}

TEST(read_rejects_excessive_nesting)
{
    const size_t depth = 1100;
    char *src = malloc(2 * depth + 2);
    ASSERT(src != NULL);
    memset(src, '(', depth);
    src[depth] = 'x';
    memset(src + depth + 1, ')', depth);
    src[2 * depth + 1] = '\0';

    ASSERT(read_from_string(src) == TOK_ERROR);
    free(src);
    PASS();
}

TEST(read_nested_list)
{
    unsigned x = read_from_string("((1 2) (3 4))");
    ASSERT(CELL_TYPE(x) == BT_CONS);
    ASSERT(CELL_TYPE(car(x)) == BT_CONS);
    ASSERT(CELL_TYPE(cadr(x)) == BT_CONS);
    ASSERT(cddr(x) == 0);
    PASS();
}

TEST(read_allows_eof_object_symbol_in_list)
{
    unsigned x = read_from_string("(eof-object)");
    ASSERT(CELL_TYPE(x) == BT_CONS);
    ASSERT(IS_ATOM(car(x)));
    ASSERT(strcmp(ctx.atom_table[CELL_ID(car(x))], "eof-object") == 0);
    ASSERT(cdr(x) == 0);
    PASS();
}

TEST(read_rejects_unterminated_list)
{
    unsigned x = read_from_string("(1 2");
    ASSERT(x == TOK_ERROR);
    PASS();
}

TEST(read_rejects_unterminated_dotted_pair)
{
    unsigned x = read_from_string("(1 .");
    ASSERT(x == TOK_ERROR);
    PASS();
}

// ============================================================================
// Reader Tests - Vectors
// ============================================================================

TEST(read_vector)
{
    unsigned x = read_from_string("#(1 2 3)");
    ASSERT(CELL_TYPE(x) == BT_VECTOR);
    ASSERT_EQ(vector_len(x), 3);
    unsigned *data = vector_data_ptr(x);
    ASSERT_EQ(CELL_ID(data[0]), 1);
    ASSERT_EQ(CELL_ID(data[1]), 2);
    ASSERT_EQ(CELL_ID(data[2]), 3);
    PASS();
}

TEST(read_empty_vector)
{
    unsigned x = read_from_string("#()");
    ASSERT(CELL_TYPE(x) == BT_VECTOR);
    ASSERT_EQ(vector_len(x), 0);
    PASS();
}

TEST(read_rejects_unterminated_vector)
{
    unsigned x = read_from_string("#(1 2");
    ASSERT(x == TOK_ERROR);
    PASS();
}

TEST(read_bytevector)
{
    unsigned x = read_from_string("#u8(0 127 255)");
    ASSERT(CELL_TYPE(x) == BT_BYTEVEC);
    bytevec_data *bv = (bytevec_data *)CELL_PTR(x);
    ASSERT_EQ(bv->len, 3);
    ASSERT_EQ(bv->data[0], 0);
    ASSERT_EQ(bv->data[1], 127);
    ASSERT_EQ(bv->data[2], 255);
    PASS();
}

TEST(read_bytevector_rejects_out_of_range)
{
    unsigned x = read_from_string("#u8(256)");
    ASSERT(x == TOK_ERROR);
    PASS();
}

TEST(read_bytevector_rejects_non_integer)
{
    unsigned x = read_from_string("#u8(foo)");
    ASSERT(x == TOK_ERROR);
    PASS();
}

TEST(read_bytevector_rejects_unterminated_literal)
{
    unsigned x = read_from_string("#u8(1 2");
    ASSERT(x == TOK_ERROR);
    PASS();
}

TEST(read_unknown_dispatch_has_valid_error_text)
{
    ASSERT(read_from_string("#") == TOK_ERROR);
    ASSERT_STR_EQ(ctx.last_error, "unexpected end of file after #");
    ASSERT(read_from_string("#\xff") == TOK_ERROR);
    ASSERT_STR_EQ(ctx.last_error, "unknown # syntax: #\\xff");
    ASSERT(read_from_string("#u8\xc3\xa9") == TOK_ERROR);
    ASSERT_STR_EQ(ctx.last_error, "unknown # syntax: #u8\\xc3");
    ASSERT(read_from_string("#z") == TOK_ERROR);
    ASSERT_STR_EQ(ctx.last_error, "unknown # syntax: #z");
    PASS();
}

TEST(read_bytevector_rejects_incomplete_prefix)
{
    ASSERT(read_from_string("#u") == TOK_ERROR);
    ASSERT(read_from_string("#u8") == TOK_ERROR);
    PASS();
}

TEST(read_datum_label_number)
{
    unsigned x = read_from_string("#0=42");
    ASSERT(IS_NUM(x));
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

TEST(read_datum_label_empty_list)
{
    unsigned x = read_from_string("#0=()");
    ASSERT_EQ(x, 0);
    PASS();
}

TEST(read_datum_label_cyclic_pair)
{
    unsigned x = read_from_string("#0=(a . #0#)");
    ASSERT(IS_PAIR(x));
    ASSERT(CELL_TYPE(car(x)) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(car(x))], "a");
    ASSERT_EQ(cdr(x), x);
    PASS();
}

TEST(read_datum_label_cyclic_vector)
{
    unsigned x = read_from_string("#0=#(#0#)");
    ASSERT(CELL_TYPE(x) == BT_VECTOR);
    ASSERT_EQ(vector_len(x), 1);
    ASSERT_EQ(vector_data_ptr(x)[0], x);
    PASS();
}

TEST(read_chained_labels_in_large_shared_graph)
{
    const unsigned count = 40000;
    char *source = malloc((size_t)count * 2 + 64);
    ASSERT(source != NULL);
    char *cursor = source;
    const char *prefix = "#1=#2=(";
    memcpy(cursor, prefix, strlen(prefix));
    cursor += strlen(prefix);
    for (unsigned i = 1; i < count; i++) {
        *cursor++ = '0';
        *cursor++ = ' ';
    }
    strcpy(cursor, "#(#1# #2#))");
    unsigned value = read_from_string(source);
    free(source);
    ASSERT(IS_PAIR(value));
    unsigned tail = value;
    for (unsigned i = 1; i < count; i++) {
        ASSERT(IS_PAIR(tail));
        tail = cdr(tail);
    }
    ASSERT(IS_PAIR(tail));
    ASSERT_EQ(cdr(tail), 0);
    unsigned shared = car(tail);
    ASSERT(IS_VECTOR(shared));
    ASSERT_EQ(vector_data_ptr(shared)[0], value);
    ASSERT_EQ(vector_data_ptr(shared)[1], value);
    PASS();
}

TEST(read_datum_label_rejects_missing_datum)
{
    unsigned x = read_from_string("#0=");
    ASSERT(x == TOK_ERROR);
    PASS();
}

TEST(read_datum_label_rejects_bare_self_reference)
{
    ASSERT(read_from_string("#0=#0#") == TOK_ERROR);
    PASS();
}

TEST(read_datum_label_rejects_special_tokens)
{
    ASSERT(read_from_string("#0=)") == TOK_ERROR);
    ASSERT(read_from_string("#0=.") == TOK_ERROR);
    PASS();
}

// ============================================================================
// Reader Tests - Quotes
// ============================================================================

TEST(read_quote)
{
    unsigned x = read_from_string("'foo");
    ASSERT(CELL_TYPE(x) == BT_CONS);
    ASSERT(car(x) == ctx.atom_quote);
    ASSERT(CELL_TYPE(cadr(x)) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(cadr(x))], "foo");
    PASS();
}

TEST(read_quasiquote)
{
    unsigned x = read_from_string("`foo");
    ASSERT(CELL_TYPE(x) == BT_CONS);
    ASSERT(car(x) == ctx.atom_quasiquote);
    PASS();
}

TEST(read_unquote)
{
    unsigned x = read_from_string(",foo");
    ASSERT(CELL_TYPE(x) == BT_CONS);
    ASSERT(car(x) == ctx.atom_unquote);
    PASS();
}

TEST(read_unquote_splicing)
{
    unsigned x = read_from_string(",@foo");
    ASSERT(CELL_TYPE(x) == BT_CONS);
    ASSERT(car(x) == ctx.atom_unquote_splicing);
    PASS();
}

TEST(read_prefix_rejects_missing_datum)
{
    ASSERT(read_from_string("'") == TOK_ERROR);
    ASSERT(read_from_string("`") == TOK_ERROR);
    ASSERT(read_from_string(",") == TOK_ERROR);
    ASSERT(read_from_string(",@") == TOK_ERROR);
    PASS();
}

TEST(read_prefix_rejects_special_tokens)
{
    ASSERT(read_from_string("')") == TOK_ERROR);
    ASSERT(read_from_string("'.") == TOK_ERROR);
    ASSERT(read_from_string("`)") == TOK_ERROR);
    ASSERT(read_from_string("`,)") == TOK_ERROR);
    ASSERT(read_from_string(",)") == TOK_ERROR);
    ASSERT(read_from_string(",@)") == TOK_ERROR);
    PASS();
}

// ============================================================================
// Reader Tests - Comments
// ============================================================================

TEST(read_skip_comment)
{
    unsigned x = read_from_string("; this is a comment\n42");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

// ============================================================================
// Reader Tests - Ellipsis
// ============================================================================

TEST(read_ellipsis)
{
    unsigned x = read_from_string("...");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "...");
    PASS();
}

// ============================================================================
// Reader Tests - Position Tracking
// ============================================================================

TEST(reader_tracks_line)
{
    FILE *f = fmemopen((void *)"line1\nline2\n42", 14, "r");
    read_obj_port(f);
    reader_forget_port(f);
    fclose(f);
    // After reading, line should have advanced
    // (We can't directly test internal state, but test API works)
    ASSERT(reader_get_line() >= 1);
    PASS();
}

TEST(reader_preserves_multi_char_pushback_across_port_reads)
{
    // Reading "ab" leaves the peeked delimiter in the port's pushback; an
    // unread of 'b' (as unread-char does) stacks a second byte on it. Both
    // must survive into the next read of the same port.
    FILE *f = fmemopen((void *)"ab cd", 5, "r");
    ASSERT(f != NULL);

    unsigned first = read_obj_port(f);
    ASSERT(IS_SYMBOL(first));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(first)], "ab");
    ASSERT_EQ(reader_port_pending_bytes(f), 1);

    ASSERT(reader_port_ungetc(f, 'b'));
    ASSERT_EQ(reader_port_pending_bytes(f), 2);

    unsigned second = read_obj_port(f);
    ASSERT(IS_SYMBOL(second));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(second)], "b");

    unsigned third = read_obj_port(f);
    ASSERT(IS_SYMBOL(third));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(third)], "cd");

    reader_forget_port(f);
    fclose(f);
    PASS();
}

TEST(read_trailing_dot_decimal)
{
    unsigned x = read_from_string("12.");
    ASSERT(IS_INEXACT(x));
    ASSERT(to_double(x) == 12.0);
    PASS();
}


// ============================================================================
// Regression tests (reader/writer review)
// ============================================================================

// Read every datum in str, up to max; returns how many were read.
static unsigned read_all_from_string(const char *str, unsigned *out,
                                     unsigned max)
{
    FILE *f = fmemopen((void *)str, strlen(str), "r");
    if (!f)
        return 0;
    unsigned n = 0;
    while (n < max) {
        unsigned x = read_obj_port(f);
        if (x == TOK_ERROR || is_eof_object(x))
            break;
        out[n++] = x;
    }
    reader_forget_port(f);
    fclose(f);
    return n;
}

static bool int_value_is(unsigned x, int64_t n)
{
    return (IS_FIXNUM(x) && FIXNUM_VALUE(x) == n) ||
           (IS_NUM(x) && CELL_ID(x) == n);
}

static bool symbol_named(unsigned x, const char *name)
{
    return IS_SYMBOL(x) && strcmp(ctx.atom_table[CELL_ID(x)], name) == 0;
}

// Render with one of the writer entry points into a malloc'd string.
static char *render(bool (*writer)(unsigned, FILE *), unsigned x, bool *ok)
{
    char *buf = NULL;
    size_t len = 0;
    FILE *f = open_memstream(&buf, &len);
    if (!f)
        return NULL;
    bool result = writer(x, f);
    fclose(f);
    if (ok)
        *ok = result;
    return buf;
}

TEST(read_labeled_booleans_stay_booleans)
{
    unsigned x = read_from_string("#0=#t");
    ASSERT(x == ctx.atom_true);
    x = read_from_string("#0=#f");
    ASSERT(x == ctx.atom_false);
    x = read_from_string("(#0=#f #0#)");
    ASSERT(IS_PAIR(x) && car(x) == ctx.atom_false &&
           car(cdr(x)) == ctx.atom_false);
    x = read_from_string("(#1=#t #0=#1#)");
    ASSERT(IS_PAIR(x) && car(x) == ctx.atom_true &&
           car(cdr(x)) == ctx.atom_true);
    PASS();
}

TEST(read_dot_inside_token_is_a_constituent)
{
    unsigned x = read_from_string("(-a. b)");
    ASSERT(IS_PAIR(x) && symbol_named(car(x), "-a."));
    ASSERT(IS_PAIR(cdr(x)) && symbol_named(car(cdr(x)), "b"));
    ASSERT(cdr(cdr(x)) == 0);
    x = read_from_string("(x ->. y)");
    ASSERT(IS_PAIR(cdr(x)) && symbol_named(car(cdr(x)), "->."));
    ASSERT(symbol_named(read_from_string("+."), "+."));
    ASSERT(symbol_named(read_from_string("1+."), "1+."));
    // Numbers containing '.' and the delimited dot marker are unchanged
    ASSERT(IS_INEXACT(read_from_string("-5.")));
    ASSERT(IS_INEXACT(read_from_string("1.e5")));
    x = read_from_string("(1 . 2)");
    ASSERT(IS_PAIR(x) && int_value_is(cdr(x), 2));
    PASS();
}

TEST(read_dot_initial_identifiers)
{
    ASSERT(symbol_named(read_from_string("...x"), "...x"));
    ASSERT(symbol_named(read_from_string("...."), "...."));
    unsigned x = read_from_string("(a ...)");
    ASSERT(IS_PAIR(cdr(x)) && symbol_named(car(cdr(x)), "..."));
    ASSERT(cdr(cdr(x)) == 0);
    PASS();
}

TEST(read_rejected_numeric_spellings_are_symbols)
{
    ASSERT(symbol_named(read_from_string("1/-2"), "1/-2"));
    ASSERT(symbol_named(read_from_string("1/+2"), "1/+2"));
    ASSERT(symbol_named(read_from_string("1i"), "1i"));
    ASSERT(read_from_string("1/0") == TOK_ERROR);
    ASSERT(read_from_string("-3/00") == TOK_ERROR);
    ASSERT(parse_number_string("1/-2", 10) == ctx.atom_false);
    ASSERT(parse_number_string("1i", 10) == ctx.atom_false);
    ASSERT(parse_number_string("1/2i", 10) == ctx.atom_false);
    ASSERT(parse_number_string("#xai", 10) == ctx.atom_false);
    ASSERT(IS_COMPLEX(parse_number_string("+1i", 10)));
    ASSERT(IS_COMPLEX(parse_number_string("-1/2i", 10)));
    ASSERT(IS_COMPLEX(parse_number_string("#x+ai", 10)));
    ASSERT(IS_COMPLEX(parse_number_string("+inf.0i", 10)));
    PASS();
}

TEST(parse_number_any_radix)
{
    unsigned x = parse_number_string("10", 3);
    ASSERT(int_value_is(x, 3));
    x = parse_number_string("-zz", 36);
    ASSERT(int_value_is(x, -1295));
    x = parse_number_string("1i", 20); // 'i' is a digit in radix 20
    ASSERT(int_value_is(x, 38));
    x = parse_number_string("12/21", 3); // 5/7
    ASSERT(IS_RATIONAL(x));
    x = parse_number_string("#x10", 3); // prefix overrides the radix
    ASSERT(int_value_is(x, 16));
    x = parse_number_string("#i11", 7);
    ASSERT(IS_INEXACT(x) && to_double(x) == 8.0);
    ASSERT(parse_number_string("3", 3) == ctx.atom_false);
    ASSERT(parse_number_string("1.5", 3) == ctx.atom_false);
    ASSERT(parse_number_string("10", 1) == ctx.atom_false);
    ASSERT(parse_number_string("10", 37) == ctx.atom_false);
    PASS();
}

TEST(parse_exact_polar_is_exact)
{
    unsigned x = parse_number_string("#e1e2@1", 10);
    ASSERT(is_numeric(x) && is_exact(x) && IS_COMPLEX(x));
    x = parse_number_string("#e2@0", 10);
    ASSERT(int_value_is(x, 2));
    x = parse_number_string("1e2@1", 10);
    ASSERT(IS_COMPLEX(x) && !is_exact(x));
    PASS();
}

TEST(read_escaped_identifier_rejects_nul_scalar)
{
    ASSERT(read_from_string("|a\\x0;b|") == TOK_ERROR);
    PASS();
}

TEST(read_vertical_line_is_a_delimiter)
{
    unsigned xs[4];
    ASSERT_EQ(read_all_from_string("a|b|c", xs, 4), 3);
    ASSERT(symbol_named(xs[0], "a"));
    ASSERT(symbol_named(xs[1], "b"));
    ASSERT(symbol_named(xs[2], "c"));
    ASSERT_EQ(read_all_from_string("#t|x y|", xs, 4), 2);
    ASSERT(xs[0] == ctx.atom_true && symbol_named(xs[1], "x y"));
    PASS();
}

TEST(read_many_and_large_datum_labels)
{
    // Large label numbers bypass the direct index
    unsigned x = read_from_string("(#300000=(a) #300000# #7=(b) #7#)");
    ASSERT(IS_PAIR(x) && car(x) == car(cdr(x)));
    ASSERT(car(cdr(cdr(x))) == car(cdr(cdr(cdr(x)))));
    // Chained labels share one object
    x = read_from_string("#0=#1=(#0# #1#)");
    ASSERT(IS_PAIR(x) && car(x) == x && car(cdr(x)) == x);
    // Labels do not leak between reads
    ASSERT(read_from_string("#0#") == TOK_ERROR);
    PASS();
}

TEST(write_labels_cycles_only)
{
    GC_GUARD;
    unsigned shared = alloc_cons(MAKE_FIXNUM(1), 0);
    gc_protect(&shared);
    unsigned tail = alloc_cons(shared, 0);
    gc_protect(&tail);
    unsigned list = alloc_cons(shared, tail);
    gc_protect(&list);

    bool ok = false;
    char *s = render(write_obj_port_checked, list, &ok);
    ASSERT(ok && s && strcmp(s, "((1) (1))") == 0);
    free(s);
    s = render(display_obj_port_checked, list, &ok);
    ASSERT(ok && s && strcmp(s, "((1) (1))") == 0);
    free(s);
    s = render(write_shared_obj_port_checked, list, &ok);
    ASSERT(ok && s && strcmp(s, "(#0=(1) #0#)") == 0);
    free(s);

    // A cycle still gets a label from write
    unsigned cyc = alloc_cons(MAKE_FIXNUM(1), 0);
    gc_protect(&cyc);
    cell_set_cdr(cyc, cyc);
    s = render(write_obj_port_checked, cyc, &ok);
    ASSERT(ok && s && strcmp(s, "#0=(1 . #0#)") == 0);
    free(s);
    // Shared but acyclic substructure next to a cycle is printed in full
    unsigned both = alloc_cons(shared, 0);
    gc_protect(&both);
    both = alloc_cons(shared, both);
    both = alloc_cons(cyc, both);
    s = render(write_obj_port_checked, both, &ok);
    ASSERT(ok && s && strcmp(s, "(#0=(1 . #0#) (1) (1))") == 0);
    free(s);
    PASS();
}

TEST(display_does_not_escape_symbols)
{
    unsigned sym = read_from_string("|Hello World|");
    ASSERT(IS_SYMBOL(sym));
    bool ok = false;
    char *s = render(display_obj_port_checked, sym, &ok);
    ASSERT(ok && s && strcmp(s, "Hello World") == 0);
    free(s);
    s = render(write_obj_port_checked, sym, &ok);
    ASSERT(ok && s && strcmp(s, "|Hello World|") == 0);
    free(s);
    PASS();
}

TEST(written_dotted_symbols_read_back)
{
    const char *names[] = {"-a.", "+.", "->.", "1+."};
    for (unsigned i = 0; i < sizeof(names) / sizeof(names[0]); i++) {
        unsigned sym = alloc();
        CELL_TYPE(sym) = BT_ATOM;
        CELL_ID(sym) = intern(names[i]);
        bool ok = false;
        char *s = render(write_obj_port_checked, sym, &ok);
        ASSERT(ok && s);
        unsigned back = read_from_string(s);
        free(s);
        ASSERT(symbol_named(back, names[i]));
    }
    PASS();
}

TEST(write_reports_nesting_failure)
{
    GC_GUARD;
    unsigned x = 0;
    gc_protect(&x);
    for (int i = 0; i < 20000; i++)
        x = alloc_cons(x, 0);
    bool ok = true;
    char *s = render(write_obj_port_checked, x, &ok);
    ASSERT(!ok);
    free(s);
    ok = true;
    s = render(display_obj_port_checked, x, &ok);
    ASSERT(!ok);
    free(s);
    ok = true;
    s = render(write_shared_obj_port_checked, x, &ok);
    ASSERT(!ok);
    free(s);
    // The writer recovers for the next call
    s = render(write_obj_port_checked, MAKE_FIXNUM(7), &ok);
    ASSERT(ok && s && strcmp(s, "7") == 0);
    free(s);
    PASS();
}

TEST(reader_recovers_after_panic_state)
{
    // reader_recover_after_panic must leave a reader that treats EOF at top
    // level as the EOF object again.
    reader_recover_after_panic();
    ASSERT(is_eof_object(read_from_string(" ")));
    ASSERT(int_value_is(read_from_string("300"), 300));
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

    printf("=== Reader Unit Tests ===\n");

    // Numbers
    RUN_TEST(read_positive_integer);
    RUN_TEST(read_negative_integer);
    RUN_TEST(read_zero);
    RUN_TEST(read_floating_point);
    RUN_TEST(read_complex_nonfinite_components);
    RUN_TEST(read_rational);
    RUN_TEST(read_bignum);
    RUN_TEST(read_hex_integer);
    RUN_TEST(read_octal_integer);
    RUN_TEST(read_binary_integer);
    RUN_TEST(read_prefixed_signed_integer);
    RUN_TEST(read_prefixed_uppercase_radix);
    RUN_TEST(read_prefixed_bignum);
    RUN_TEST(read_prefixed_int64_min);
    RUN_TEST(read_prefixed_integer_rejects_trailing_junk);
    RUN_TEST(read_exactness_and_radix_prefixes);
    RUN_TEST(read_prefixed_complex_exactness);
    RUN_TEST(read_repeated_imaginary_suffix_does_not_recurse);
    RUN_TEST(read_polar_numbers);
    RUN_TEST(read_malformed_rational_as_symbol);
    RUN_TEST(read_malformed_denominator_as_symbol);
    RUN_TEST(read_exact_decimal_rejects_scale_overflow);

    // Strings
    RUN_TEST(read_simple_string);
    RUN_TEST(read_string_with_escapes);
    RUN_TEST(read_string_hex_scalar_escape);
    RUN_TEST(read_string_rejects_invalid_scalar_escapes);
    RUN_TEST(read_string_rejects_invalid_utf8);
    RUN_TEST(read_token_rejects_null_character);
    RUN_TEST(read_numeric_token_rejects_null_character);
    RUN_TEST(read_string_rejects_null_character);
    RUN_TEST(read_empty_string);

    // Characters
    RUN_TEST(read_char_literal);
    RUN_TEST(read_char_space);
    RUN_TEST(read_char_newline);
    RUN_TEST(read_char_tab);
    RUN_TEST(read_char_named_controls);
    RUN_TEST(read_char_hex_scalar);
    RUN_TEST(read_char_utf8_literal);
    RUN_TEST(read_char_rejects_invalid_scalar);
    RUN_TEST(read_char_rejects_multi_char_utf8_literal);
    RUN_TEST(read_char_rejects_eof);
    RUN_TEST(read_char_rejects_unknown_name);

    // Symbols
    RUN_TEST(read_symbol);
    RUN_TEST(read_symbol_case_insensitive);
    RUN_TEST(read_fold_case_directive);
    RUN_TEST(read_no_fold_case_directive);
    RUN_TEST(read_case_directive_persists_for_port);
    RUN_TEST(read_case_directive_can_toggle);
    RUN_TEST(read_unknown_directive_rejected);
    RUN_TEST(read_datum_comment_skips_one_object);
    RUN_TEST(read_datum_comment_skips_nested_object);
    RUN_TEST(read_datum_comment_rejects_missing_datum);
    RUN_TEST(read_nested_block_comment);
    RUN_TEST(read_block_comment_rejects_unterminated);
    RUN_TEST(read_symbol_with_special_chars);
    RUN_TEST(read_symbol_preserves_utf8_identifier);
    RUN_TEST(read_symbol_ascii_folds_without_mangling_utf8);
    RUN_TEST(read_symbol_unicode_foldcase);
    RUN_TEST(read_no_fold_case_preserves_unicode_identifier);
    RUN_TEST(read_symbol_rejects_invalid_utf8);
    RUN_TEST(read_escaped_identifier);
    RUN_TEST(read_escaped_numeric_identifier);
    RUN_TEST(read_escaped_identifier_rejects_invalid_escape);

    // Booleans
    RUN_TEST(read_true);
    RUN_TEST(read_false);
    RUN_TEST(read_boolean_aliases);
    RUN_TEST(read_boolean_rejects_invalid_suffix);

    // Lists
    RUN_TEST(read_empty_list);
    RUN_TEST(read_simple_list);
    RUN_TEST(read_dotted_pair);
    RUN_TEST(read_rejects_dot_without_preceding_object);
    RUN_TEST(read_large_flat_list_without_recursion);
    RUN_TEST(read_rejects_excessive_nesting);
    RUN_TEST(read_nested_list);
    RUN_TEST(read_allows_eof_object_symbol_in_list);
    RUN_TEST(read_rejects_unterminated_list);
    RUN_TEST(read_rejects_unterminated_dotted_pair);

    // Vectors
    RUN_TEST(read_vector);
    RUN_TEST(read_empty_vector);
    RUN_TEST(read_rejects_unterminated_vector);
    RUN_TEST(read_bytevector);
    RUN_TEST(read_bytevector_rejects_out_of_range);
    RUN_TEST(read_bytevector_rejects_non_integer);
    RUN_TEST(read_bytevector_rejects_unterminated_literal);
    RUN_TEST(read_unknown_dispatch_has_valid_error_text);
    RUN_TEST(read_bytevector_rejects_incomplete_prefix);
    RUN_TEST(read_datum_label_number);
    RUN_TEST(read_datum_label_empty_list);
    RUN_TEST(read_datum_label_cyclic_pair);
    RUN_TEST(read_datum_label_cyclic_vector);
    RUN_TEST(read_chained_labels_in_large_shared_graph);
    RUN_TEST(read_datum_label_rejects_missing_datum);
    RUN_TEST(read_datum_label_rejects_bare_self_reference);
    RUN_TEST(read_datum_label_rejects_special_tokens);

    // Quotes
    RUN_TEST(read_quote);
    RUN_TEST(read_quasiquote);
    RUN_TEST(read_unquote);
    RUN_TEST(read_unquote_splicing);
    RUN_TEST(read_prefix_rejects_missing_datum);
    RUN_TEST(read_prefix_rejects_special_tokens);

    // Comments
    RUN_TEST(read_skip_comment);

    // Ellipsis
    RUN_TEST(read_ellipsis);

    // Position tracking
    RUN_TEST(reader_tracks_line);
    RUN_TEST(reader_preserves_multi_char_pushback_across_port_reads);
    RUN_TEST(read_trailing_dot_decimal);
    RUN_TEST(read_labeled_booleans_stay_booleans);
    RUN_TEST(read_dot_inside_token_is_a_constituent);
    RUN_TEST(read_dot_initial_identifiers);
    RUN_TEST(read_rejected_numeric_spellings_are_symbols);
    RUN_TEST(parse_number_any_radix);
    RUN_TEST(parse_exact_polar_is_exact);
    RUN_TEST(read_escaped_identifier_rejects_nul_scalar);
    RUN_TEST(read_vertical_line_is_a_delimiter);
    RUN_TEST(read_many_and_large_datum_labels);
    RUN_TEST(write_labels_cycles_only);
    RUN_TEST(display_does_not_escape_symbols);
    RUN_TEST(written_dotted_symbols_read_back);
    RUN_TEST(write_reports_nesting_failure);
    RUN_TEST(reader_recovers_after_panic_state);

    TEST_SUMMARY("reader");
}
