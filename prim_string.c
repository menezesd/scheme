/**
 * @file prim_string.c
 * @brief String operations (string-append, substring, etc.)
 */

#include "prim_internal.h"

unsigned prim_string_append(unsigned args)
{
    // First pass: validate and compute total length
    size_t total = 0;
    FORLIST(a, args)
    {
        CHECK_STRING(car(a), "string-append");
        total += strlen(GET_STRING_PTR(car(a)));
    }

    char *result = malloc(total + 1);
    if (!result) {
        show_error("string-append: out of memory");
        return TOK_ERROR;
    }

    // Second pass: copy strings (use pointer arithmetic to avoid strlen)
    char *pos = result;
    FORLIST(a, args)
    {
        char *s = GET_STRING_PTR(car(a));
        while (*s)
            *pos++ = *s++;
    }
    *pos = '\0';

    return make_string_owned(result);
}

unsigned prim_substring(unsigned args)
{
    REQUIRE_ARGS(args, 2, 3, "substring");
    CHECK_STRING(car(args), "substring");
    char *s = GET_STRING_PTR(car(args));
    size_t slen = strlen(s);
    int64_t start;
    if (!expect_nonneg_int64(cadr(args), &start, "substring"))
        return TOK_ERROR;
    int64_t end;
    if (cddr(args)) {
        if (!expect_nonneg_int64(caddr(args), &end, "substring"))
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
