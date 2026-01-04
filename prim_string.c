/**
 * @file prim_string.c
 * @brief String operations (string-append, substring, etc.)
 */

#include "prim_internal.h"

unsigned prim_string_append(unsigned argc, unsigned *argv)
{
    size_t total = 0;
    for (unsigned i = 0; i < argc; i++) {
        CHECK_STRING(argv[i], "string-append");
        total += strlen(GET_STRING_PTR(argv[i]));
    }

    char *result = malloc(total + 1);
    if (!result) {
        show_error("string-append: out of memory");
        return TOK_ERROR;
    }

    char *pos = result;
    for (unsigned i = 0; i < argc; i++) {
        char *s = GET_STRING_PTR(argv[i]);
        while (*s)
            *pos++ = *s++;
    }
    *pos = '\0';

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
