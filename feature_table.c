#include "feature_table.h"

static const char *features[] = {
    "vesper",
    "r5rs",
    "r7rs-small-subset",
    "srfi-1",
    "srfi-2",
    "srfi-8",
    "srfi-9",
    "srfi-26",
    "srfi-27",
    "srfi-28",
    "bytevectors",
    "exact-rationals",
    "complex",
    "unicode-normalization",
    "unicode-case-folding",
    "unicode-character-properties",
    "full-unicode-absent",
    NULL,
};

const char *const *feature_names(void)
{
    return features;
}

size_t feature_name_count(void)
{
    size_t count = 0;
    while (features[count])
        count++;
    return count;
}
