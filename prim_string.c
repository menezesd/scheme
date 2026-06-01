/**
 * @file prim_string.c
 * @brief String operations (string-append, substring, etc.)
 */

#include "prim_internal.h"

#define STRING_APPEND_STACK_LENGTHS 64

static void free_string_append_lengths(size_t *lengths, size_t *stack_lengths)
{
    if (lengths != stack_lengths)
        free(lengths);
}

unsigned prim_string_append(unsigned argc, unsigned *argv)
{
    // First pass: validate and compute total length, cache individual lengths
    size_t total = 0;
    size_t stack_lengths[STRING_APPEND_STACK_LENGTHS];
    size_t *lengths = (argc <= STRING_APPEND_STACK_LENGTHS)
                          ? stack_lengths
                          : checked_malloc_array(argc, sizeof(size_t));
    if (!lengths) {
        show_error("string-append: out of memory");
        return TOK_ERROR;
    }

    for (unsigned i = 0; i < argc; i++) {
        char *s = require_string_ptr(argv[i], "string-append");
        if (!s) {
            free_string_append_lengths(lengths, stack_lengths);
            return TOK_ERROR;
        }
        lengths[i] = strlen(s);
        if (lengths[i] > SIZE_MAX - total - 1) {
            free_string_append_lengths(lengths, stack_lengths);
            show_error("string-append: result too large");
            return TOK_ERROR;
        }
        total += lengths[i];
    }

    char *result = checked_malloc_flex(0, total + 1, 1);
    if (!result) {
        free_string_append_lengths(lengths, stack_lengths);
        show_error("string-append: out of memory");
        return TOK_ERROR;
    }

    // Second pass: copy using cached lengths and memcpy
    char *pos = result;
    for (unsigned i = 0; i < argc; i++) {
        memcpy(pos, GET_STRING_PTR(argv[i]), lengths[i]);
        pos += lengths[i];
    }
    *pos = '\0';

    free_string_append_lengths(lengths, stack_lengths);

    return make_string_owned(result);
}

static bool utf8_decode_next_string(const char *s, size_t byte_len,
                                    size_t *offset, const char *name)
{
    if (*offset >= byte_len) {
        show_error("%s: invalid UTF-8 offset", name);
        return false;
    }
    const unsigned char *bytes = (const unsigned char *)s;
    unsigned char b0 = bytes[*offset];
    size_t needed;
    int value;
    if (b0 == 0) {
        show_error("%s: null character in string", name);
        return false;
    } else if (b0 < 0x80) {
        (*offset)++;
        return true;
    } else if (b0 >= 0xC2 && b0 <= 0xDF) {
        needed = 2;
        value = b0 & 0x1F;
    } else if (b0 >= 0xE0 && b0 <= 0xEF) {
        needed = 3;
        value = b0 & 0x0F;
    } else if (b0 >= 0xF0 && b0 <= 0xF4) {
        needed = 4;
        value = b0 & 0x07;
    } else {
        show_error("%s: invalid UTF-8 leading byte", name);
        return false;
    }
    if (byte_len - *offset < needed) {
        show_error("%s: truncated UTF-8 sequence", name);
        return false;
    }
    for (size_t i = 1; i < needed; i++) {
        unsigned char b = bytes[*offset + i];
        if ((b & 0xC0) != 0x80) {
            show_error("%s: invalid UTF-8 continuation byte", name);
            return false;
        }
        value = (value << 6) | (b & 0x3F);
    }
    if ((needed == 3 && value < 0x800) ||
        (needed == 4 && value < 0x10000) ||
        value > 0x10FFFF || (value >= 0xD800 && value <= 0xDFFF)) {
        show_error("%s: invalid UTF-8 sequence", name);
        return false;
    }
    *offset += needed;
    return true;
}

static bool utf8_count_chars_string(const char *s, size_t *chars,
                                    const char *name)
{
    size_t byte_len = strlen(s);
    size_t offset = 0;
    size_t count = 0;
    while (offset < byte_len) {
        if (!utf8_decode_next_string(s, byte_len, &offset, name))
            return false;
        count++;
    }
    *chars = count;
    return true;
}

static bool utf8_byte_offset_for_index_string(const char *s,
                                              size_t char_index,
                                              size_t *byte_offset,
                                              const char *name)
{
    size_t byte_len = strlen(s);
    size_t offset = 0;
    size_t count = 0;
    while (offset < byte_len && count < char_index) {
        if (!utf8_decode_next_string(s, byte_len, &offset, name))
            return false;
        count++;
    }
    if (count != char_index) {
        show_error("%s: index out of bounds", name);
        return false;
    }
    *byte_offset = offset;
    return true;
}

unsigned prim_substring(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 1, 3, "substring");
    char *s = require_string_ptr(argv[0], "substring");
    if (!s)
        return TOK_ERROR;
    size_t char_len;
    if (!utf8_count_chars_string(s, &char_len, "substring"))
        return TOK_ERROR;
    int64_t start;
    if (argc > 1) {
        if (!expect_nonneg_int64(argv[1], &start, "substring"))
            return TOK_ERROR;
    } else {
        start = 0;
    }
    int64_t end;
    if (argc == 3) {
        if (!expect_nonneg_int64(argv[2], &end, "substring"))
            return TOK_ERROR;
    } else {
        end = (int64_t)char_len;
    }
    if (start < 0 || end < start || end > (int64_t)char_len) {
        show_error("substring: invalid indices");
        return TOK_ERROR;
    }
    size_t start_byte;
    size_t end_byte;
    if (!utf8_byte_offset_for_index_string(s, (size_t)start, &start_byte,
                                           "substring") ||
        !utf8_byte_offset_for_index_string(s, (size_t)end, &end_byte,
                                           "substring"))
        return TOK_ERROR;
    size_t result_len = end_byte - start_byte;
    char *result = checked_string_copy_len(s + start_byte, result_len);
    if (!result) {
        show_error("substring: out of memory");
        return TOK_ERROR;
    }
    return make_string_owned(result);
}
