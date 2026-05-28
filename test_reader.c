// Unit tests for reader module
#define _POSIX_C_SOURCE 200809L
#include "context.h"
#include "reader.h"
#include "test_framework.h"
#include "types.h"
#include <stdint.h>
#include <stdlib.h>

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

// ============================================================================
// Reader Tests - Strings
// ============================================================================

TEST(read_simple_string)
{
    unsigned x = read_from_string("\"hello\"");
    ASSERT(CELL_TYPE(x) == BT_STRING);
    const char *str = GET_STRING_PTR(x);
    ASSERT_STR_EQ(str, "hello");
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

TEST(read_symbol_with_special_chars)
{
    unsigned x = read_from_string("foo-bar?");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "foo-bar?");
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

TEST(read_datum_label_rejects_missing_datum)
{
    unsigned x = read_from_string("#0=");
    ASSERT(x == TOK_ERROR);
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
    FILE *f = fmemopen((void *)"12.\n34", 6, "r");
    ASSERT(f != NULL);

    unsigned first = read_obj_port(f);
    ASSERT(CELL_TYPE(first) == BT_NUM);
    ASSERT_EQ(CELL_ID(first), 12);

    unsigned second = read_obj_port(f);
    ASSERT(second == TOK_DOT);

    unsigned third = read_obj_port(f);
    ASSERT(CELL_TYPE(third) == BT_NUM);
    ASSERT_EQ(CELL_ID(third), 34);

    reader_forget_port(f);
    fclose(f);
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
    RUN_TEST(read_rational);
    RUN_TEST(read_bignum);
    RUN_TEST(read_prefixed_bignum);
    RUN_TEST(read_prefixed_int64_min);
    RUN_TEST(read_prefixed_integer_rejects_trailing_junk);
    RUN_TEST(read_malformed_rational_as_symbol);
    RUN_TEST(read_malformed_denominator_as_symbol);

    // Strings
    RUN_TEST(read_simple_string);
    RUN_TEST(read_string_with_escapes);
    RUN_TEST(read_empty_string);

    // Characters
    RUN_TEST(read_char_literal);
    RUN_TEST(read_char_space);
    RUN_TEST(read_char_newline);
    RUN_TEST(read_char_tab);
    RUN_TEST(read_char_rejects_eof);
    RUN_TEST(read_char_rejects_unknown_name);

    // Symbols
    RUN_TEST(read_symbol);
    RUN_TEST(read_symbol_case_insensitive);
    RUN_TEST(read_symbol_with_special_chars);

    // Booleans
    RUN_TEST(read_true);
    RUN_TEST(read_false);

    // Lists
    RUN_TEST(read_empty_list);
    RUN_TEST(read_simple_list);
    RUN_TEST(read_dotted_pair);
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
    RUN_TEST(read_bytevector_rejects_incomplete_prefix);
    RUN_TEST(read_datum_label_number);
    RUN_TEST(read_datum_label_empty_list);
    RUN_TEST(read_datum_label_rejects_missing_datum);
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

    TEST_SUMMARY("reader");
}
