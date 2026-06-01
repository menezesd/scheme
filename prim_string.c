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

unsigned prim_substring(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 2, 3, "substring");
    char *s = require_string_ptr(argv[0], "substring");
    if (!s)
        return TOK_ERROR;
    size_t slen = strlen(s);
    int64_t start;
    if (!expect_nonneg_int64(argv[1], &start, "substring"))
        return TOK_ERROR;
    int64_t end;
    if (argc == 3) {
        if (!expect_nonneg_int64(argv[2], &end, "substring"))
            return TOK_ERROR;
    } else {
        end = (int64_t)slen;
    }
    if (start < 0 || end < start || end > (int64_t)slen) {
        show_error("substring: invalid indices");
        return TOK_ERROR;
    }
    size_t result_len = end - start;
    char *result = checked_string_copy_len(s + start, result_len);
    if (!result) {
        show_error("substring: out of memory");
        return TOK_ERROR;
    }
    return make_string_owned(result);
}
