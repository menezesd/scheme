/**
 * @file test_unicode_norm.c
 * @brief Unicode normalization tests generated from Unicode NormalizationTest.
 */

#include "context.h"
#include "prim_internal.h"
#include "test_framework.h"
#include "types.h"
#include "unicode_norm_test_data.h"
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

static char *utf8_from_test_column(const unicode_norm_test_row *row, int col)
{
    size_t cap = (size_t)row->length[col] * 4 + 1;
    char *out = malloc(cap);
    if (!out)
        return NULL;
    size_t pos = 0;
    for (uint16_t i = 0; i < row->length[col]; i++) {
        pos += (size_t)append_utf8(
            out + pos, unicode_norm_test_data[row->offset[col] + i]);
    }
    out[pos] = '\0';
    return out;
}

static unsigned normalize_c_string(const char *s, unsigned prim_id)
{
    unsigned arg = make_string_copy(s);
    if (arg == TOK_ERROR)
        return TOK_ERROR;
    return prim_string_normalize(prim_id, 1, &arg);
}

static void assert_normalized(const unicode_norm_test_row *row, int input_col,
                              int expected_col, unsigned prim_id)
{
    char *input = utf8_from_test_column(row, input_col);
    char *expected = utf8_from_test_column(row, expected_col);
    ASSERT(input != NULL);
    ASSERT(expected != NULL);

    unsigned actual = normalize_c_string(input, prim_id);
    ASSERT(actual != TOK_ERROR);
    ASSERT(CELL_TYPE(actual) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(actual), expected);

    free(input);
    free(expected);
}

TEST(unicode_normalization_fixture)
{
    for (size_t i = 0; i < UNICODE_NORM_TEST_ROW_COUNT; i++) {
        const unicode_norm_test_row *row = &unicode_norm_test_rows[i];

        assert_normalized(row, 0, 1, PSTRNFC);
        assert_normalized(row, 1, 1, PSTRNFC);
        assert_normalized(row, 2, 1, PSTRNFC);
        assert_normalized(row, 3, 3, PSTRNFC);
        assert_normalized(row, 4, 3, PSTRNFC);

        assert_normalized(row, 0, 2, PSTRNFD);
        assert_normalized(row, 1, 2, PSTRNFD);
        assert_normalized(row, 2, 2, PSTRNFD);
        assert_normalized(row, 3, 4, PSTRNFD);
        assert_normalized(row, 4, 4, PSTRNFD);

        assert_normalized(row, 0, 3, PSTRNFKC);
        assert_normalized(row, 1, 3, PSTRNFKC);
        assert_normalized(row, 2, 3, PSTRNFKC);
        assert_normalized(row, 3, 3, PSTRNFKC);
        assert_normalized(row, 4, 3, PSTRNFKC);

        assert_normalized(row, 0, 4, PSTRNFKD);
        assert_normalized(row, 1, 4, PSTRNFKD);
        assert_normalized(row, 2, 4, PSTRNFKD);
        assert_normalized(row, 3, 4, PSTRNFKD);
        assert_normalized(row, 4, 4, PSTRNFKD);
    }
    PASS();
}

static void assert_normalized_codepoints(const uint32_t *input_codes,
                                         size_t input_length,
                                         const uint32_t *expected_codes,
                                         size_t expected_length,
                                         unsigned prim_id)
{
    char *input = malloc(input_length * 4 + 1);
    char *expected = malloc(expected_length * 4 + 1);
    ASSERT(input != NULL);
    ASSERT(expected != NULL);

    size_t pos = 0;
    for (size_t i = 0; i < input_length; i++)
        pos += (size_t)append_utf8(input + pos, input_codes[i]);
    input[pos] = '\0';
    pos = 0;
    for (size_t i = 0; i < expected_length; i++)
        pos += (size_t)append_utf8(expected + pos, expected_codes[i]);
    expected[pos] = '\0';

    unsigned actual = normalize_c_string(input, prim_id);
    ASSERT(actual != TOK_ERROR);
    ASSERT(CELL_TYPE(actual) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(actual), expected);

    free(input);
    free(expected);
}

TEST(unicode_targeted_normalization)
{
    uint32_t composed_e[] = {0x00E9};
    uint32_t decomposed_e[] = {'e', 0x0301};
    uint32_t unordered_marks[] = {'a', 0x0301, 0x0327};
    uint32_t ordered_marks[] = {'a', 0x0327, 0x0301};
    uint32_t ligature[] = {0xFB03};
    uint32_t ffi[] = {'f', 'f', 'i'};
    uint32_t fullwidth[] = {0xFF21, 0xFF22, 0xFF23,
                            0xFF11, 0xFF12, 0xFF13};
    uint32_t ascii[] = {'A', 'B', 'C', '1', '2', '3'};

    assert_normalized_codepoints(composed_e, 1, decomposed_e, 2, PSTRNFD);
    assert_normalized_codepoints(decomposed_e, 2, composed_e, 1, PSTRNFC);
    assert_normalized_codepoints(unordered_marks, 3, ordered_marks, 3,
                                 PSTRNFD);
    assert_normalized_codepoints(ligature, 1, ffi, 3, PSTRNFKD);
    assert_normalized_codepoints(fullwidth, 6, ascii, 6, PSTRNFKC);
    PASS();
}

int main(void)
{
    init_heap();
    RUN_TEST(unicode_normalization_fixture);
    RUN_TEST(unicode_targeted_normalization);
    TEST_SUMMARY("unicode normalization");
}
