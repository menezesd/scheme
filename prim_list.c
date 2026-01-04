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

unsigned prim_append(unsigned argc, unsigned *argv)
{
    if (argc == 0)
        return 0;
    GC_GUARD;
    unsigned result = 0, tail = 0;
    gc_protect(&result);
    gc_protect(&tail);

    for (unsigned i = 0; i + 1 < argc; i++) {
        unsigned lst = argv[i];
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

    unsigned last = argv[argc - 1];
    if (tail) {
        write_barrier(tail, last);
        CELL_CDR(tail) = last;
    }
    return result ? result : last;
}

unsigned prim_reverse(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 1, 1, "reverse");
    GC_GUARD;
    unsigned lst = argv[0];
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
