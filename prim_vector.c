/**
 * @file prim_vector.c
 * @brief Vector operations (make-vector, vector-ref, vector-set!, etc.)
 */

#include "prim_internal.h"

unsigned apply_vector_primitive(unsigned prim_id, unsigned args)
{
    switch (prim_id) {
    case PMAKEVEC: {
        REQUIRE_ARGS(args, 1, 2, "make-vector");
        unsigned len = CELL_ID(car(args));
        unsigned fill = cdr(args) ? cadr(args) : 0;
        return make_vector(len, fill);
    }
    case PVECTOR: {
        unsigned len = list_length(args);
        unsigned vec = make_vector(len, 0);
        unsigned *data = vector_data_ptr(vec);
        unsigned i = 0;
        for (unsigned a = args; a; a = cdr(a), i++) {
            data[i] = car(a);
        }
        return vec;
    }
    case PVECREF: {
        REQUIRE_ARGS(args, 2, 2, "vector-ref");
        unsigned vec = car(args);
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector-ref: not a vector");
        int64_t idx = CELL_ID(cadr(args));
        CHECK_VECTOR_BOUNDS(idx, vec, "vector-ref");
        return vector_data_ptr(vec)[idx];
    }
    case PVECSET: {
        REQUIRE_ARGS(args, 3, 3, "vector-set!");
        unsigned vec = car(args);
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector-set!: not a vector");
        int64_t idx = CELL_ID(cadr(args));
        CHECK_VECTOR_BOUNDS(idx, vec, "vector-set!");
        vector_data_ptr(vec)[idx] = caddr(args);
        return caddr(args);
    }
    case PVECLEN: {
        REQUIRE_ARGS(args, 1, 1, "vector-length");
        unsigned vec = car(args);
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector-length: not a vector");
        return store(vector_len(vec));
    }
    case PVECFILL: {
        REQUIRE_ARGS(args, 2, 2, "vector-fill!");
        unsigned vec = car(args);
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector-fill!: not a vector");
        unsigned fill = cadr(args);
        unsigned len = vector_len(vec);
        unsigned *data = vector_data_ptr(vec);
        for (unsigned i = 0; i < len; i++)
            data[i] = fill;
        return 0;
    }
    case PLIST2VEC: {
        REQUIRE_ARGS(args, 1, 1, "list->vector");
        unsigned lst = car(args);
        unsigned len = list_length(lst);
        unsigned vec = make_vector(len, 0);
        unsigned *data = vector_data_ptr(vec);
        for (unsigned i = 0; lst; lst = cdr(lst), i++)
            data[i] = car(lst);
        return vec;
    }
    case PVEC2LIST: {
        REQUIRE_ARGS(args, 1, 1, "vector->list");
        unsigned vec = car(args);
        if (!IS_VECTOR(vec))
            ERROR_RETURN("vector->list: not a vector");
        unsigned len = vector_len(vec);
        unsigned *data = vector_data_ptr(vec);
        unsigned result = 0, tail = 0;
        for (unsigned i = 0; i < len; i++) {
            list_append(&result, &tail, data[i]);
        }
        return result;
    }
    default:
        return TOK_ERROR;
    }
}
