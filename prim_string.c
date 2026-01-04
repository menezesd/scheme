/**
 * @file prim_string.c
 * @brief String operations (string-append, substring, etc.)
 */

#include "prim_internal.h"

unsigned prim_string_append(unsigned argc, unsigned *argv)
{
    // First pass: validate and compute total length, cache individual lengths
    size_t total = 0;
    size_t lens[64];    // Stack-allocated for common case
    size_t *lengths = (argc <= 64) ? lens : malloc(argc * sizeof(size_t));
    if (!lengths) {
        show_error("string-append: out of memory");
        return TOK_ERROR;
    }

    for (unsigned i = 0; i < argc; i++) {
        CHECK_STRING(argv[i], "string-append");
        lengths[i] = strlen(GET_STRING_PTR(argv[i]));
        total += lengths[i];
    }

    char *result = malloc(total + 1);
    if (!result) {
        if (argc > 64)
            free(lengths);
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

    if (argc > 64)
        free(lengths);

    return make_string_owned(result);
}

unsigned prim_substring(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 2, 3, "substring");
    CHECK_STRING(argv[0], "substring");
    char *s = GET_STRING_PTR(argv[0]);
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
    char *result = malloc(result_len + 1);
    if (!result) {
        show_error("substring: out of memory");
        return TOK_ERROR;
    }
    memcpy(result, s + start, result_len);
    result[result_len] = '\0';
    return make_string_owned(result);
}
