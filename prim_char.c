/**
 * @file prim_char.c
 * @brief Character operations with table-driven predicates
 */

#include "prim_internal.h"

// Table-driven character predicates
typedef struct {
    unsigned id;
    int (*predicate)(int);
    const char *name;
} char_pred_entry;

static const char_pred_entry char_predicates[] = {
    {PCHARALPHA, isalpha, "char-alphabetic?"},
    {PCHARNUMERIC, isdigit, "char-numeric?"},
    {PCHARWHITE, isspace, "char-whitespace?"},
    {PCHARUPPER, isupper, "char-upper-case?"},
    {PCHARLOWER, islower, "char-lower-case?"},
    {0, NULL, NULL}};

unsigned apply_char_primitive(unsigned prim_id, unsigned args)
{
    // Check table-driven predicates first
    for (const char_pred_entry *e = char_predicates; e->predicate; e++) {
        if (e->id == prim_id) {
            REQUIRE_ARGS(args, 1, 1, e->name);
            return e->predicate((int)CELL_ID(car(args))) ? ctx.atom_true
                                                         : ctx.atom_false;
        }
    }

    switch (prim_id) {
    case PCHARCODE:
        REQUIRE_ARGS(args, 1, 1, "char->integer");
        return store(CELL_ID(car(args)));
    case PCODECHAR:
        REQUIRE_ARGS(args, 1, 1, "integer->char");
        return make_char(CELL_ID(car(args)));
    case PCHARUP:
        REQUIRE_ARGS(args, 1, 1, "char-upcase");
        return make_char(toupper((int)CELL_ID(car(args))));
    case PCHARDOWN:
        REQUIRE_ARGS(args, 1, 1, "char-downcase");
        return make_char(tolower((int)CELL_ID(car(args))));
    // Character comparisons (PCHAREQ..PCHARGEI are sequential)
    case PCHAREQ:
    case PCHARLT:
    case PCHARGT:
    case PCHARLE:
    case PCHARGE:
    case PCHAREQI:
    case PCHARLTI:
    case PCHARGTI:
    case PCHARLEI:
    case PCHARGEI: {
        unsigned offset = prim_id - PCHAREQ;
        return char_compare(args, (cmp_op)(offset % 5), offset >= 5);
    }
    default:
        return TOK_ERROR;
    }
}
