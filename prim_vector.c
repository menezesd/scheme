/**
 * @file prim_vector.c
 * @brief Vector operations (make-vector, vector-ref, vector-set!, etc.)
 */

#include "prim_internal.h"
#include <limits.h>

unsigned apply_vector_primitive(unsigned prim_id, unsigned argc,
                                unsigned *argv)
{
    switch (prim_id) {
    case PMAKEVEC: {
        REQUIRE_ARGC(argc, 1, 2, "make-vector");
        int64_t len64;
        if (!expect_nonneg_int64(argv[0], &len64, "make-vector"))
            return TOK_ERROR;
        if ((uint64_t)len64 > UINT_MAX) {
            show_error("make-vector: length too large");
            return TOK_ERROR;
        }
        unsigned len = (unsigned)len64;
        unsigned fill = (argc == 2) ? argv[1] : 0;
        unsigned vec = make_vector(len, fill);
        return vec == TOK_ERROR ? TOK_ERROR : vec;
    }
    case PVECTOR: {
        unsigned len = argc;
        unsigned vec = make_vector(len, 0);
        if (vec == TOK_ERROR)
            return TOK_ERROR;
        unsigned *data = vector_data_ptr(vec);
        for (unsigned i = 0; i < len; i++)
            data[i] = argv[i];
        return vec;
    }
    case PVECREF: {
        REQUIRE_ARGC(argc, 2, 2, "vector-ref");
        unsigned vec = argv[0];
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector-ref: not a vector");
        int64_t idx;
        if (!expect_nonneg_int64(argv[1], &idx, "vector-ref"))
            return TOK_ERROR;
        CHECK_VECTOR_BOUNDS(idx, vec, "vector-ref");
        return vector_data_ptr(vec)[idx];
    }
    case PVECSET: {
        REQUIRE_ARGC(argc, 3, 3, "vector-set!");
        unsigned vec = argv[0];
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector-set!: not a vector");
        int64_t idx;
        if (!expect_nonneg_int64(argv[1], &idx, "vector-set!"))
            return TOK_ERROR;
        CHECK_VECTOR_BOUNDS(idx, vec, "vector-set!");
        unsigned val = argv[2];
        vector_set_elem(vec, (unsigned)idx, val);
        return val;
    }
    case PVECLEN: {
        REQUIRE_ARGC(argc, 1, 1, "vector-length");
        unsigned vec = argv[0];
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector-length: not a vector");
        return store(vector_len(vec));
    }
    case PVECFILL: {
        REQUIRE_ARGC(argc, 2, 2, "vector-fill!");
        unsigned vec = argv[0];
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector-fill!: not a vector");
        unsigned fill = argv[1];
        unsigned len = vector_len(vec);
        for (unsigned i = 0; i < len; i++)
            vector_set_elem(vec, i, fill);
        return 0;
    }
    case PLIST2VEC: {
        REQUIRE_ARGC(argc, 1, 1, "list->vector");
        GC_GUARD;
        unsigned lst = argv[0];
        gc_protect(&lst);
        unsigned len = 0;
        if (!list_length_checked(lst, &len, "list->vector"))
            return TOK_ERROR;
        unsigned vec = make_vector(len, 0);
        if (vec == TOK_ERROR)
            return TOK_ERROR;
        unsigned *data = vector_data_ptr(vec);
        for (unsigned i = 0; lst; lst = cdr(lst), i++)
            data[i] = car(lst);
        return vec;
    }
    case PVEC2LIST: {
        REQUIRE_ARGC(argc, 1, 1, "vector->list");
        GC_GUARD;
        unsigned vec = argv[0];
        gc_protect(&vec);
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector->list: not a vector");
        unsigned len = vector_len(vec);
        unsigned result = 0, tail = 0;
        gc_protect(&result);
        gc_protect(&tail);
        for (unsigned i = 0; i < len; i++) {
            unsigned elem = vector_data_ptr(vec)[i];
            gc_protect(&elem);
            list_append(&result, &tail, elem);
            gc_unprotect(1);
        }
        return result;
    }
    default:
        return TOK_ERROR;
    }
}
