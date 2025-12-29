/**
 * @file prim_math.c
 * @brief Math functions (sqrt, sin, cos, exp, log, floor, ceiling, etc.)
 */

#include "prim_internal.h"

// Math function table for simple unary functions
typedef struct {
    unsigned id;
    double (*func)(double);
    const char *name;
} math_func_entry;

static const math_func_entry math_funcs[] = {
    {PSIN, sin, "sin"},    {PCOS, cos, "cos"},    {PTAN, tan, "tan"},
    {PASIN, asin, "asin"}, {PACOS, acos, "acos"}, {PLOG, log, "log"},
    {PEXP, exp, "exp"},    {0, NULL, NULL}};

unsigned apply_math_primitive(unsigned prim_id, unsigned args)
{
    // Check simple unary functions via table
    for (const math_func_entry *e = math_funcs; e->func; e++) {
        if (e->id == prim_id) {
            REQUIRE_ARGS(args, 1, 1, e->name);
            return store_inexact(e->func(to_double(car(args))));
        }
    }

    switch (prim_id) {
    case PSQRT: {
        REQUIRE_ARGS(args, 1, 1, "sqrt");
        double x = to_double(car(args));
        if (x < 0) {
            return store_complex(store(0), store_inexact(sqrt(-x)));
        }
        double result = sqrt(x);
        if (is_exact(car(args)) && floor(result) == result) {
            return store((int64_t)result);
        }
        return store_inexact(result);
    }
    case PEXPT: {
        REQUIRE_ARGS(args, 2, 2, "expt");
        double base = to_double(car(args));
        double exp = to_double(cadr(args));
        double result = pow(base, exp);
        if (all_exact(args) && floor(result) == result && result >= INT64_MIN &&
            result <= INT64_MAX) {
            return store((int64_t)result);
        }
        return store_inexact(result);
    }
    case PATAN: {
        REQUIRE_ARGS(args, 1, 2, "atan");
        double y = to_double(car(args));
        if (cdr(args)) {
            double x = to_double(cadr(args));
            return store_inexact(atan2(y, x));
        }
        return store_inexact(atan(y));
    }
    case PFLOOR: {
        REQUIRE_ARGS(args, 1, 1, "floor");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_NUM)
            return x;
        double d = to_double(x);
        return is_exact(x) ? store((int64_t)floor(d)) : store_inexact(floor(d));
    }
    case PCEILING: {
        REQUIRE_ARGS(args, 1, 1, "ceiling");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_NUM)
            return x;
        double d = to_double(x);
        return is_exact(x) ? store((int64_t)ceil(d)) : store_inexact(ceil(d));
    }
    case PTRUNCATE: {
        REQUIRE_ARGS(args, 1, 1, "truncate");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_NUM)
            return x;
        double d = to_double(x);
        return is_exact(x) ? store((int64_t)trunc(d)) : store_inexact(trunc(d));
    }
    case PROUND: {
        REQUIRE_ARGS(args, 1, 1, "round");
        unsigned x = car(args);
        if (CELL_TYPE(x) == BT_NUM)
            return x;
        double d = to_double(x);
        return is_exact(x) ? store((int64_t)round(d)) : store_inexact(round(d));
    }
    default:
        return TOK_ERROR;
    }
}
