/**
 * @file test_unicode_case.c
 * @brief Unicode casing and character-property tests generated from UCD data.
 */

#include "context.h"
#include "prim_internal.h"
#include "test_framework.h"
#include "types.h"
#include "unicode_case_test_data.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void init_heap(void);

static int append_utf8(char *out, uint32_t code)
{
    if (code < 0x80) {
        out[0] = (char)code;
        return 1;
    }
    if (code < 0x800) {
        out[0] = (char)(0xC0 | (code >> 6));
        out[1] = (char)(0x80 | (code & 0x3F));
        return 2;
    }
    if (code < 0x10000) {
        out[0] = (char)(0xE0 | (code >> 12));
        out[1] = (char)(0x80 | ((code >> 6) & 0x3F));
        out[2] = (char)(0x80 | (code & 0x3F));
        return 3;
    }
    out[0] = (char)(0xF0 | (code >> 18));
    out[1] = (char)(0x80 | ((code >> 12) & 0x3F));
    out[2] = (char)(0x80 | ((code >> 6) & 0x3F));
    out[3] = (char)(0x80 | (code & 0x3F));
    return 4;
}

static char *utf8_from_codepoints(const uint32_t *codes, size_t length)
{
    char *out = malloc(length * 4 + 1);
    if (!out)
        return NULL;
    size_t pos = 0;
    for (size_t i = 0; i < length; i++)
        pos += (size_t)append_utf8(out + pos, codes[i]);
    out[pos] = '\0';
    return out;
}

static char *utf8_from_mapping(const unicode_case_mapping_row *row,
                               const uint32_t *data)
{
    return utf8_from_codepoints(&data[row->offset], row->length);
}

static bool string_mapping_matches(const unicode_case_mapping_row *row,
                                   const uint32_t *data, unsigned prim_id)
{
    char *input = utf8_from_codepoints(&row->code, 1);
    char *expected = utf8_from_mapping(row, data);
    if (!input || !expected) {
        free(input);
        free(expected);
        return false;
    }
    unsigned arg = make_string_copy(input);
    if (arg == TOK_ERROR) {
        free(input);
        free(expected);
        return false;
    }
    unsigned actual = prim_string_normalize(prim_id, 1, &arg);
    bool matches = actual != TOK_ERROR && CELL_TYPE(actual) == BT_STRING &&
                   strcmp(GET_STRING_PTR(actual), expected) == 0;
    free(input);
    free(expected);
    return matches;
}

static bool char_predicate_range_matches(const unicode_case_property_range *range,
                                         unsigned prim_id)
{
    unsigned start = make_char((int)range->start);
    unsigned end = make_char((int)range->end);
    unsigned start_result = apply_char_primitive(prim_id, 1, &start);
    unsigned end_result = apply_char_primitive(prim_id, 1, &end);
    return start_result == ctx.atom_true && end_result == ctx.atom_true;
}

static void assert_string_mapping(const uint32_t *input_codes,
                                  size_t input_length,
                                  const uint32_t *expected_codes,
                                  size_t expected_length,
                                  unsigned prim_id)
{
    char *input = utf8_from_codepoints(input_codes, input_length);
    char *expected = utf8_from_codepoints(expected_codes, expected_length);
    ASSERT(input != NULL);
    ASSERT(expected != NULL);

    unsigned arg = make_string_copy(input);
    ASSERT(arg != TOK_ERROR);
    unsigned actual = prim_string_normalize(prim_id, 1, &arg);
    ASSERT(actual != TOK_ERROR);
    ASSERT(CELL_TYPE(actual) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(actual), expected);

    free(input);
    free(expected);
}

static void assert_char_mapping(uint32_t input, uint32_t expected,
                                unsigned prim_id)
{
    unsigned arg = make_char((int)input);
    unsigned actual = apply_char_primitive(prim_id, 1, &arg);
    ASSERT(actual != TOK_ERROR);
    ASSERT(IS_CHAR(actual));
    ASSERT((uint32_t)GET_CHAR_CODE(actual) == expected);
}

TEST(unicode_case_folding_fixture)
{
    for (size_t i = 0; i < UNICODE_CASE_FOLD_ROW_COUNT; i++)
        ASSERT(string_mapping_matches(&unicode_case_fold_rows[i],
                                      unicode_case_fold_data, PSTRFOLD));
    PASS();
}

TEST(unicode_case_mapping_fixture)
{
    for (size_t i = 0; i < UNICODE_CASE_LOWER_ROW_COUNT; i++)
        ASSERT(string_mapping_matches(&unicode_case_lower_rows[i],
                                      unicode_case_lower_data, PSTRDOWN));
    for (size_t i = 0; i < UNICODE_CASE_TITLE_ROW_COUNT; i++)
        ASSERT(string_mapping_matches(&unicode_case_title_rows[i],
                                      unicode_case_title_data, PSTRTITLE));
    for (size_t i = 0; i < UNICODE_CASE_UPPER_ROW_COUNT; i++)
        ASSERT(string_mapping_matches(&unicode_case_upper_rows[i],
                                      unicode_case_upper_data, PSTRUP));
    PASS();
}

TEST(unicode_character_property_fixture)
{
    for (size_t i = 0; i < UNICODE_CASE_ALPHABETIC_COUNT; i++)
        ASSERT(char_predicate_range_matches(&unicode_case_alphabetic[i],
                                            PCHARALPHA));
    for (size_t i = 0; i < UNICODE_CASE_DECIMAL_COUNT; i++)
        ASSERT(char_predicate_range_matches(&unicode_case_decimal[i],
                                            PCHARNUMERIC));
    for (size_t i = 0; i < UNICODE_CASE_WHITESPACE_COUNT; i++)
        ASSERT(char_predicate_range_matches(&unicode_case_whitespace[i],
                                            PCHARWHITE));
    for (size_t i = 0; i < UNICODE_CASE_UPPERCASE_COUNT; i++)
        ASSERT(char_predicate_range_matches(&unicode_case_uppercase[i],
                                            PCHARUPPER));
    for (size_t i = 0; i < UNICODE_CASE_LOWERCASE_COUNT; i++)
        ASSERT(char_predicate_range_matches(&unicode_case_lowercase[i],
                                            PCHARLOWER));
    PASS();
}

TEST(unicode_targeted_foldcase)
{
    uint32_t input[] = {0x1E9E, 0x0130, 0x212A, 0x017F,
                        0x00B5, 0x03A3, 0x03C2, 0x03C3,
                        0xFB03};
    uint32_t expected[] = {'s', 's', 'i', 0x0307, 'k', 's',
                           0x03BC, 0x03C3, 0x03C3, 0x03C3,
                           'f', 'f', 'i'};
    assert_string_mapping(input, 9, expected, 13, PSTRFOLD);
    PASS();
}

TEST(unicode_targeted_char_foldcase)
{
    assert_char_mapping(0x03C2, 0x03C3, PCHARFOLD);
    assert_char_mapping(0x212A, 'k', PCHARFOLD);
    assert_char_mapping(0x017F, 's', PCHARFOLD);
    assert_char_mapping(0x00B5, 0x03BC, PCHARFOLD);
    PASS();
}

TEST(unicode_final_sigma_context)
{
    uint32_t upper_final[] = {0x039F, 0x03A3};
    uint32_t lower_final[] = {0x03BF, 0x03C2};
    uint32_t upper_nonfinal[] = {0x039F, 0x03A3, 0x0391};
    uint32_t lower_nonfinal[] = {0x03BF, 0x03C3, 0x03B1};
    uint32_t title_final[] = {0x039F, 0x03C2};

    char *input = utf8_from_codepoints(upper_final, 2);
    char *expected = utf8_from_codepoints(lower_final, 2);
    unsigned arg = make_string_copy(input);
    unsigned actual = prim_string_normalize(PSTRDOWN, 1, &arg);
    ASSERT(actual != TOK_ERROR);
    ASSERT_STR_EQ(GET_STRING_PTR(actual), expected);
    free(input);
    free(expected);

    input = utf8_from_codepoints(upper_nonfinal, 3);
    expected = utf8_from_codepoints(lower_nonfinal, 3);
    arg = make_string_copy(input);
    actual = prim_string_normalize(PSTRDOWN, 1, &arg);
    ASSERT(actual != TOK_ERROR);
    ASSERT_STR_EQ(GET_STRING_PTR(actual), expected);
    free(input);
    free(expected);

    input = utf8_from_codepoints(upper_final, 2);
    expected = utf8_from_codepoints(title_final, 2);
    arg = make_string_copy(input);
    actual = prim_string_normalize(PSTRTITLE, 1, &arg);
    ASSERT(actual != TOK_ERROR);
    ASSERT_STR_EQ(GET_STRING_PTR(actual), expected);
    free(input);
    free(expected);

    PASS();
}

int main(void)
{
    init_heap();
    RUN_TEST(unicode_case_folding_fixture);
    RUN_TEST(unicode_case_mapping_fixture);
    RUN_TEST(unicode_character_property_fixture);
    RUN_TEST(unicode_targeted_foldcase);
    RUN_TEST(unicode_targeted_char_foldcase);
    RUN_TEST(unicode_final_sigma_context);
    TEST_SUMMARY("unicode case");
}
