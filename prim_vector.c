/**
 * @file prim_vector.c
 * @brief Vector operations (make-vector, vector-ref, vector-set!, etc.)
 */

#include "prim_internal.h"
#include <limits.h>

static bool require_vector(unsigned value, const char *name)
{
    if (IS_VECTOR(value) && vector_data_well_formed(GET_VECTOR_PTR(value)))
        return true;
    show_error("%s: not a vector", name);
    return false;
}

static bool vector_index(unsigned value, unsigned vec, int64_t *idx,
                         const char *name)
{
    return expect_index(value, vector_len(vec), idx, name);
}

static unsigned make_vector_from_argv(unsigned len, unsigned *argv)
{
    unsigned vec = make_vector(len, 0);
    if (vec == TOK_ERROR)
        return TOK_ERROR;
    unsigned *data = vector_data_ptr(vec);
    for (unsigned i = 0; i < len; i++)
        data[i] = argv[i];
    return vec;
}

static unsigned make_filled_vector(unsigned len_arg, unsigned fill,
                                   const char *name)
{
    int64_t len64;
    if (!expect_nonneg_int64(len_arg, &len64, name))
        return TOK_ERROR;
    if ((uint64_t)len64 > UINT_MAX) {
        show_error("%s: length too large", name);
        return TOK_ERROR;
    }
    unsigned vec = make_vector((unsigned)len64, fill);
    return vec == TOK_ERROR ? TOK_ERROR : vec;
}

// Parse optional start/end arguments (argv[first], argv[first+1]) against
// the vector's length, defaulting to the full range
static bool vector_range(unsigned argc, unsigned *argv, unsigned first,
                         unsigned vec, int64_t *start, int64_t *end,
                         const char *name)
{
    unsigned len = vector_len(vec);
    *start = 0;
    *end = len;
    if (argc > first && !expect_nonneg_int64(argv[first], start, name))
        return false;
    if (argc > first + 1 &&
        !expect_nonneg_int64(argv[first + 1], end, name))
        return false;
    if (*start > *end || *end > len) {
        show_error("%s: invalid range", name);
        return false;
    }
    return true;
}

static unsigned make_vector_from_list(unsigned lst, const char *name)
{
    GC_GUARD;
    gc_protect(&lst);
    unsigned len = 0;
    if (!list_length_checked(lst, &len, name))
        return TOK_ERROR;
    unsigned vec = make_vector(len, 0);
    if (vec == TOK_ERROR)
        return TOK_ERROR;
    unsigned *data = vector_data_ptr(vec);
    for (unsigned i = 0; lst; lst = cdr(lst), i++)
        data[i] = car(lst);
    return vec;
}

static unsigned make_list_from_vector(unsigned vec, int64_t start,
                                      int64_t end)
{
    GC_GUARD;
    gc_protect(&vec);
    unsigned result = 0, tail = 0;
    gc_protect(&result);
    gc_protect(&tail);
    for (int64_t i = start; i < end; i++) {
        unsigned elem = vector_data_ptr(vec)[i];
        gc_protect(&elem);
        list_append(&result, &tail, elem);
        gc_unprotect(1);
    }
    return result;
}

unsigned apply_vector_primitive(unsigned prim_id, unsigned argc,
                                unsigned *argv)
{
    switch (prim_id) {
    case PMAKEVEC: {
        REQUIRE_ARGC(argc, 1, 2, "make-vector");
        unsigned fill = (argc == 2) ? argv[1] : 0;
        return make_filled_vector(argv[0], fill, "make-vector");
    }
    case PVECTOR: {
        return make_vector_from_argv(argc, argv);
    }
    case PVECREF: {
        REQUIRE_ARGC(argc, 2, 2, "vector-ref");
        unsigned vec = argv[0];
        if (!require_vector(vec, "vector-ref"))
            return TOK_ERROR;
        int64_t idx;
        if (!vector_index(argv[1], vec, &idx, "vector-ref"))
            return TOK_ERROR;
        return vector_data_ptr(vec)[idx];
    }
    case PVECSET: {
        REQUIRE_ARGC(argc, 3, 3, "vector-set!");
        unsigned vec = argv[0];
        if (!require_vector(vec, "vector-set!"))
            return TOK_ERROR;
        int64_t idx;
        if (!vector_index(argv[1], vec, &idx, "vector-set!"))
            return TOK_ERROR;
        unsigned val = argv[2];
        vector_set_elem(vec, (unsigned)idx, val);
        return val;
    }
    case PVECLEN: {
        REQUIRE_ARGC(argc, 1, 1, "vector-length");
        unsigned vec = argv[0];
        if (!require_vector(vec, "vector-length"))
            return TOK_ERROR;
        return store(vector_len(vec));
    }
    case PVECFILL: {
        REQUIRE_ARGC(argc, 2, 4, "vector-fill!");
        unsigned vec = argv[0];
        if (!require_vector(vec, "vector-fill!"))
            return TOK_ERROR;
        int64_t start, end;
        if (!vector_range(argc, argv, 2, vec, &start, &end, "vector-fill!"))
            return TOK_ERROR;
        unsigned fill = argv[1];
        for (int64_t i = start; i < end; i++)
            vector_set_elem(vec, (unsigned)i, fill);
        return 0;
    }
    case PLIST2VEC: {
        REQUIRE_ARGC(argc, 1, 1, "list->vector");
        return make_vector_from_list(argv[0], "list->vector");
    }
    case PVEC2LIST: {
        REQUIRE_ARGC(argc, 1, 3, "vector->list");
        unsigned vec = argv[0];
        if (!require_vector(vec, "vector->list"))
            return TOK_ERROR;
        int64_t start, end;
        if (!vector_range(argc, argv, 1, vec, &start, &end, "vector->list"))
            return TOK_ERROR;
        return make_list_from_vector(vec, start, end);
    }
    default:
        return TOK_ERROR;
    }
}
