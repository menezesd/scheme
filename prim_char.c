/**
 * @file prim_char.c
 * @brief Character operations with table-driven predicates
 */

#include "prim_internal.h"
#include "unicode_char_tables.h"
#include "unicode_norm_tables.h"

static bool unicode_range_contains(const unicode_range *ranges, size_t count,
                                   uint32_t code)
{
    size_t lo = 0;
    size_t hi = count;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (ranges[mid].end < code)
            lo = mid + 1;
        else
            hi = mid;
    }
    return lo < count && ranges[lo].start <= code && code <= ranges[lo].end;
}

static bool unicode_char_has_combining_class(uint32_t code)
{
    size_t lo = 0;
    size_t hi = UNICODE_CCC_COUNT;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (unicode_ccc_table[mid].code < code)
            lo = mid + 1;
        else
            hi = mid;
    }
    return lo < UNICODE_CCC_COUNT && unicode_ccc_table[lo].code == code &&
           unicode_ccc_table[lo].combining_class != 0;
}

static unsigned unicode_assigned_value(unsigned arg, const char *name)
{
    int code;
    if (!expect_char_value(arg, &code, name))
        return TOK_ERROR;
    return scheme_bool(unicode_range_contains(
        unicode_assigned_ranges, UNICODE_ASSIGNED_RANGES_COUNT,
        (uint32_t)code));
}

static uint32_t lookup_simple_mapping(const unicode_simple_fold_entry *entries,
                                      size_t count, uint32_t code)
{
    size_t lo = 0;
    size_t hi = count;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (entries[mid].code < code)
            lo = mid + 1;
        else
            hi = mid;
    }
    if (lo < count && entries[lo].code == code)
        return entries[lo].folded;
    return code;
}

static bool lookup_full_mapping(const unicode_full_case_entry *entries,
                                const uint32_t *data, size_t count,
                                uint32_t code, const uint32_t **mapping,
                                size_t *length)
{
    size_t lo = 0;
    size_t hi = count;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (entries[mid].code < code)
            lo = mid + 1;
        else
            hi = mid;
    }
    if (lo < count && entries[lo].code == code) {
        *mapping = &data[entries[lo].offset];
        *length = entries[lo].length;
        return true;
    }
    *mapping = NULL;
    *length = 0;
    return false;
}

uint32_t unicode_simple_foldcase(uint32_t code)
{
    return lookup_simple_mapping(unicode_simple_fold_table,
                                 UNICODE_SIMPLE_FOLD_COUNT, code);
}

uint32_t unicode_upcase(uint32_t code)
{
    return lookup_simple_mapping(unicode_upcase_table, UNICODE_UPCASE_COUNT,
                                 code);
}

uint32_t unicode_downcase(uint32_t code)
{
    return lookup_simple_mapping(unicode_downcase_table,
                                 UNICODE_DOWNCASE_COUNT, code);
}

bool unicode_is_cased(uint32_t code)
{
    return unicode_range_contains(unicode_cased_ranges,
                                  UNICODE_CASED_RANGES_COUNT, code);
}

bool unicode_is_case_ignorable(uint32_t code)
{
    return unicode_range_contains(unicode_case_ignorable_ranges,
                                  UNICODE_CASE_IGNORABLE_RANGES_COUNT, code);
}

bool unicode_full_foldcase(uint32_t code, const uint32_t **mapping,
                           size_t *length)
{
    return lookup_full_mapping(unicode_full_fold_table, unicode_full_fold_data,
                               UNICODE_FULL_FOLD_COUNT, code, mapping,
                               length);
}

bool unicode_full_upcase(uint32_t code, const uint32_t **mapping,
                         size_t *length)
{
    return lookup_full_mapping(unicode_full_upper_table, unicode_full_upper_data,
                               UNICODE_FULL_UPPER_COUNT, code, mapping,
                               length);
}

bool unicode_full_downcase(uint32_t code, const uint32_t **mapping,
                           size_t *length)
{
    return lookup_full_mapping(unicode_full_lower_table, unicode_full_lower_data,
                               UNICODE_FULL_LOWER_COUNT, code, mapping,
                               length);
}

bool unicode_full_titlecase(uint32_t code, const uint32_t **mapping,
                            size_t *length)
{
    return lookup_full_mapping(unicode_full_title_table, unicode_full_title_data,
                               UNICODE_FULL_TITLE_COUNT, code, mapping,
                               length);
}

static bool unicode_char_predicate(unsigned prim_id, uint32_t code)
{
    switch (prim_id) {
    case PCHARALPHA:
        return unicode_range_contains(unicode_alphabetic_ranges,
                                      UNICODE_ALPHABETIC_RANGES_COUNT, code);
    case PCHARNUMERIC:
        return unicode_range_contains(unicode_decimal_ranges,
                                      UNICODE_DECIMAL_RANGES_COUNT, code);
    case PCHARWHITE:
        return unicode_range_contains(unicode_whitespace_ranges,
                                      UNICODE_WHITESPACE_RANGES_COUNT, code);
    case PCHARUPPER:
        return unicode_range_contains(unicode_uppercase_ranges,
                                      UNICODE_UPPERCASE_RANGES_COUNT, code);
    case PCHARLOWER:
        return unicode_range_contains(unicode_lowercase_ranges,
                                      UNICODE_LOWERCASE_RANGES_COUNT, code);
    default:
        return false;
    }
}

static const char *char_predicate_name(unsigned prim_id)
{
    switch (prim_id) {
    case PCHARALPHA:
        return "char-alphabetic?";
    case PCHARNUMERIC:
        return "char-numeric?";
    case PCHARWHITE:
        return "char-whitespace?";
    case PCHARUPPER:
        return "char-upper-case?";
    case PCHARLOWER:
        return "char-lower-case?";
    default:
        return NULL;
    }
}

static unsigned char_predicate_value(unsigned prim_id, unsigned arg,
                                     const char *name)
{
    int c;
    if (!expect_char_value(arg, &c, name))
        return TOK_ERROR;
    return scheme_bool(unicode_char_predicate(prim_id, (uint32_t)c));
}

static unsigned char_transform(unsigned prim_id, unsigned arg, const char *name)
{
    int c;
    if (!expect_char_value(arg, &c, name))
        return TOK_ERROR;
    uint32_t mapped;
    if (prim_id == PCHARUP)
        mapped = unicode_upcase((uint32_t)c);
    else if (prim_id == PCHARDOWN)
        mapped = unicode_downcase((uint32_t)c);
    else
        mapped = unicode_simple_foldcase((uint32_t)c);
    return make_char((int)mapped);
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
    if (!is_valid_unicode_scalar(code)) {
        show_error("%s: code point out of range", name);
        return TOK_ERROR;
    }
    return make_char((int)code);
}

unsigned apply_char_primitive(unsigned prim_id, unsigned argc, unsigned *argv)
{
    const char *pred_name = char_predicate_name(prim_id);
    if (pred_name) {
        REQUIRE_ARGC(argc, 1, 1, pred_name);
        return char_predicate_value(prim_id, argv[0], pred_name);
    }

    if (prim_id == PCHARUP || prim_id == PCHARDOWN || prim_id == PCHARFOLD) {
        const char *name = prim_id == PCHARUP      ? "char-upcase"
                           : prim_id == PCHARDOWN ? "char-downcase"
                                                  : "char-foldcase";
        REQUIRE_ARGC(argc, 1, 1, name);
        return char_transform(prim_id, argv[0], name);
    }

    switch (prim_id) {
    case PUNICODEASSIGNED:
        REQUIRE_ARGC(argc, 1, 1, "%unicode-assigned?");
        return unicode_assigned_value(argv[0], "%unicode-assigned?");
    case PUNICODEPUNCTUATION:
    case PUNICODESYMBOL: {
        const char *name = prim_id == PUNICODEPUNCTUATION
                               ? "%unicode-punctuation?"
                               : "%unicode-symbol?";
        REQUIRE_ARGC(argc, 1, 1, name);
        int c;
        if (!expect_char_value(argv[0], &c, name))
            return TOK_ERROR;
        const unicode_range *ranges = prim_id == PUNICODEPUNCTUATION
                                           ? unicode_punctuation_ranges
                                           : unicode_symbol_ranges;
        size_t count = prim_id == PUNICODEPUNCTUATION
                           ? UNICODE_PUNCTUATION_RANGES_COUNT
                           : UNICODE_SYMBOL_RANGES_COUNT;
        return scheme_bool(unicode_range_contains(ranges, count, (uint32_t)c));
    }
    case PCHARCOMBINING: {
        REQUIRE_ARGC(argc, 1, 1, "%char-combining?");
        int c;
        if (!expect_char_value(argv[0], &c, "%char-combining?"))
            return TOK_ERROR;
        return scheme_bool(unicode_char_has_combining_class((uint32_t)c));
    }
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
