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

typedef struct {
    unsigned id;
    int (*transform)(int);
    const char *name;
} char_transform_entry;

static const char_pred_entry char_predicates[] = {
    {PCHARALPHA, isalpha, "char-alphabetic?"},
    {PCHARNUMERIC, isdigit, "char-numeric?"},
    {PCHARWHITE, isspace, "char-whitespace?"},
    {PCHARUPPER, isupper, "char-upper-case?"},
    {PCHARLOWER, islower, "char-lower-case?"},
    {0, NULL, NULL}};

static const char_transform_entry char_transforms[] = {
    {PCHARUP, toupper, "char-upcase"},
    {PCHARDOWN, tolower, "char-downcase"},
    {0, NULL, NULL}};

static const char_pred_entry *find_char_predicate(unsigned prim_id)
{
    for (const char_pred_entry *e = char_predicates; e->predicate; e++) {
        if (e->id == prim_id)
            return e;
    }
    return NULL;
}

static const char_transform_entry *find_char_transform(unsigned prim_id)
{
    for (const char_transform_entry *e = char_transforms; e->transform; e++) {
        if (e->id == prim_id)
            return e;
    }
    return NULL;
}

static unsigned char_predicate_value(unsigned arg, const char *name,
                                     int (*predicate)(int))
{
    int c;
    if (!expect_char_value(arg, &c, name))
        return TOK_ERROR;
    if (c > UCHAR_MAX)
        return ctx.atom_false;
    return scheme_bool(predicate(c));
}

static unsigned char_transform(unsigned arg, const char *name,
                               int (*transform)(int))
{
    int c;
    if (!expect_char_value(arg, &c, name))
        return TOK_ERROR;
    if (c > UCHAR_MAX)
        return arg;
    return make_char(transform(c));
}

static unsigned char_code_value(unsigned arg, const char *name)
{
    int c;
    if (!expect_char_value(arg, &c, name))
        return TOK_ERROR;
    return store(c);
}

static unsigned integer_to_char_value(unsigned arg, const char *name)
{
    int64_t code;
    if (!expect_exact_int64(arg, &code, name))
        return TOK_ERROR;
    if (code < 0 || code > 0x10FFFF) {
        show_error("%s: code point out of range", name);
        return TOK_ERROR;
    }
    return make_char((int)code);
}

unsigned apply_char_primitive(unsigned prim_id, unsigned argc, unsigned *argv)
{
    const char_pred_entry *pred = find_char_predicate(prim_id);
    if (pred) {
        REQUIRE_ARGC(argc, 1, 1, pred->name);
        return char_predicate_value(argv[0], pred->name, pred->predicate);
    }

    const char_transform_entry *transform = find_char_transform(prim_id);
    if (transform) {
        REQUIRE_ARGC(argc, 1, 1, transform->name);
        return char_transform(argv[0], transform->name,
                              transform->transform);
    }

    switch (prim_id) {
    case PCHARCODE:
        REQUIRE_ARGC(argc, 1, 1, "char->integer");
        return char_code_value(argv[0], "char->integer");
    case PCODECHAR:
        REQUIRE_ARGC(argc, 1, 1, "integer->char");
        return integer_to_char_value(argv[0], "integer->char");
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
        return char_compare(argc, argv, (cmp_op)(offset % 5), offset >= 5);
    }
    default:
        return TOK_ERROR;
    }
}
