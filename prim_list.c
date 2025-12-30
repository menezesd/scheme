/**
 * @file prim_list.c
 * @brief List operations (append, reverse)
 */

#include "prim_internal.h"

unsigned prim_append(unsigned args)
{
    if (!args)
        return 0;
    unsigned result = 0, tail = 0;
    for (unsigned a = args; cdr(a); a = cdr(a)) {
        unsigned lst = car(a);
        for (; lst && CELL_TYPE(lst) == BT_CONS; lst = cdr(lst)) {
            list_append(&result, &tail, car(lst));
        }
    }
    unsigned last = args;
    last = list_last(last);
    if (tail) {
        write_barrier(tail, car(last)); // tail may be in old gen
        CELL_CDR(tail) = car(last);
    }
    return result ? result : car(last);
}

unsigned prim_reverse(unsigned args)
{
    REQUIRE_ARGS(args, 1, 1, "reverse");
    unsigned lst = car(args);
    unsigned result = 0;
    gc_protect(&lst);
    gc_protect(&result);
    for (; lst && CELL_TYPE(lst) == BT_CONS; lst = cdr(lst))
        result = alloc_cons(car(lst), result);
    gc_unprotect(2);
    return result;
}
