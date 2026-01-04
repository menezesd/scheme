/**
 * @file prim_list.c
 * @brief List operations (append, reverse)
 *
 * Implements standard Scheme list operations:
 *
 * ## append
 * (append list1 list2 ... listN) -> concatenated list
 * - Copies the spine of all lists except the last
 * - Last argument can be any object (result may be improper list)
 * - Shares structure with the last argument (no copy)
 *
 * ## reverse
 * (reverse list) -> reversed copy
 * - Creates a new list with elements in reverse order
 * - Does not modify the original list
 *
 * Both operations are GC-safe: all local variables are protected
 * across allocations using gc_protect/gc_unprotect.
 */

#include "prim_internal.h"

unsigned prim_append(unsigned args)
{
    if (!args)
        return 0;
    GC_GUARD;
    unsigned result = 0, tail = 0;
    unsigned a = args;
    // Protect all variables that survive across allocations
    gc_protect(&args);
    gc_protect(&result);
    gc_protect(&tail);
    gc_protect(&a);
    for (; IS_PAIR(a) && cdr(a); a = cdr(a)) {
        unsigned lst = car(a);
        gc_protect(&lst);
        while (lst && CELL_TYPE(lst) == BT_CONS) {
            list_append(&result, &tail, car(lst));
            lst = cdr(lst);
        }
        if (lst) {
            gc_unprotect(1);
            show_error("append: improper list");
            return TOK_ERROR;
        }
        gc_unprotect(1);
    }
    if (!IS_PAIR(a)) {
        show_error("append: improper argument list");
        return TOK_ERROR;
    }
    unsigned last = car(a);
    if (tail) {
        write_barrier(tail, last); // tail may be in old gen
        CELL_CDR(tail) = last;
    }
    return result ? result : last;
}

unsigned prim_reverse(unsigned args)
{
    REQUIRE_ARGS(args, 1, 1, "reverse");
    GC_GUARD;
    unsigned lst = car(args);
    unsigned result = 0;
    gc_protect(&lst);
    gc_protect(&result);
    for (; lst && CELL_TYPE(lst) == BT_CONS; lst = cdr(lst))
        result = alloc_cons(car(lst), result);
    if (lst) {
        show_error("reverse: improper list");
        return TOK_ERROR;
    }
    return result;
}
