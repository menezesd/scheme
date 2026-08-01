/**
 * @file reader.c
 * @brief S-expression parser and tokenizer
 *
 * This file implements the Scheme reader, which parses text into the internal
 * cell representation. It handles:
 *
 * ## Lexical Elements
 * - Numbers: integers, decimals, rationals (1/3), bignums
 * - Strings: with escape sequences (\n, \t, \\, \")
 * - Characters: #\a, #\space, #\newline, #\tab
 * - Symbols: case-insensitive, interned for fast comparison
 * - Booleans: #t, #f, #true, #false
 * - Vectors: #(1 2 3)
 *
 * ## Special Syntax
 * - Quote: 'x -> (quote x)
 * - Quasiquote: `x -> (quasiquote x)
 * - Unquote: ,x -> (unquote x)
 * - Unquote-splicing: ,@x -> (unquote-splicing x)
 * - Datum labels: #n= (define) and #n# (reference) for cyclic structures
 *
 * ## Error Handling
 * Line and column tracking for meaningful error messages.
 */

#include "reader.h"
#include "bignum.h"
#include "context.h"
#include "prim_internal.h"
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

// Current input port for reading (NULL means use stdin)
static FILE *reader_port = NULL;

// Source location tracking for error messages
static int reader_line = 1;
static int reader_col = 0;
static const char *reader_filename = "<stdin>";
static bool reader_fold_case = true;

#define READER_PUSHBACK_CAP 16
#define READER_MAX_OBJECT_DEPTH 1024

typedef struct {
    int c;
    int line_before;
    int col_before;
} reader_char_state;

static reader_char_state reader_pushback[READER_PUSHBACK_CAP];
static int reader_pushback_len = 0;
static reader_char_state reader_history[READER_PUSHBACK_CAP];
static int reader_history_len = 0;

static FILE *saved_pushback_port = NULL;
static long saved_pushback_pos = -1;
static int saved_reader_line = 1;
static int saved_reader_col = 0;
static reader_char_state saved_pushback[READER_PUSHBACK_CAP];
static int saved_pushback_len = 0;
static reader_char_state saved_history[READER_PUSHBACK_CAP];
static int saved_history_len = 0;
static bool saved_reader_fold_case = true;

static void reader_record_history(reader_char_state state)
{
    if (reader_history_len == READER_PUSHBACK_CAP) {
        memmove(reader_history, reader_history + 1,
                (READER_PUSHBACK_CAP - 1) * sizeof(reader_history[0]));
        reader_history_len--;
    }
    reader_history[reader_history_len++] = state;
}

static void saved_reader_record_history(reader_char_state state)
{
    if (saved_history_len == READER_PUSHBACK_CAP) {
        memmove(saved_history, saved_history + 1,
                (READER_PUSHBACK_CAP - 1) * sizeof(saved_history[0]));
        saved_history_len--;
    }
    saved_history[saved_history_len++] = state;
}

static void advance_position_for_char(int c, int *line, int *col)
{
    if (c == '\n') {
        (*line)++;
        *col = 0;
    } else if (c != EOF) {
        (*col)++;
    }
}

// ============================================================================
// Datum Labels (#n= and #n#)
// ============================================================================

// Dynamic datum label storage - grows as needed
#define INITIAL_DATUM_LABELS 64

typedef struct {
    int label;
    unsigned value;
    bool defined;
} datum_label_entry;

static datum_label_entry *datum_labels = NULL;
static int datum_label_count = 0;
static int datum_label_cap = 0;
static int reader_obj_depth = 0;

static void reset_datum_labels(void)
{
    datum_label_count = 0;
    // Keep the allocated buffer for reuse, just reset count
}

static int find_datum_label(int label)
{
    for (int i = 0; i < datum_label_count; i++) {
        if (datum_labels[i].label == label)
            return i;
    }
    return -1;
}

static int add_datum_label(int label)
{
    // Grow the array if needed
    if (datum_label_count >= datum_label_cap) {
        if (datum_label_cap > INT_MAX / 2) {
            show_error("too many datum labels");
            return -1;
        }
        int new_cap = datum_label_cap == 0 ? INITIAL_DATUM_LABELS
                                           : datum_label_cap * 2;
        datum_label_entry *new_labels = checked_realloc_array(
            datum_labels, (unsigned)new_cap, sizeof(datum_label_entry));
        if (!new_labels) {
            show_error("out of memory for datum labels");
            return -1;
        }
        datum_labels = new_labels;
        datum_label_cap = new_cap;
    }
    int idx = datum_label_count++;
    datum_labels[idx].label = label;
    datum_labels[idx].value = 0;
    datum_labels[idx].defined = false;
    return idx;
}

void reader_update_datum_labels(unsigned (*collector)(unsigned))
{
    if (reader_obj_depth == 0)
        return;
    for (int i = 0; i < datum_label_count; i++) {
        if (datum_labels[i].defined)
            datum_labels[i].value = collector(datum_labels[i].value);
    }
}

static void resolve_datum_label_placeholder(unsigned placeholder,
                                            unsigned datum)
{
    if (IS_PAIR(datum)) {
        cell_set_car(placeholder, car(datum));
        cell_set_cdr(placeholder, cdr(datum));
        return;
    }

    enum lisp_type type = CELL_TYPE(datum);
    CELL_TYPE(placeholder) = type;

    if (type == BT_RATIONAL || type == BT_COMPLEX ||
        type == BT_FUNCTION || type == BT_MACRO ||
        type == BT_SYNTAX || type == BT_CONT) {
        cell_set_car(placeholder, car(datum));
        cell_set_cdr(placeholder, cdr(datum));
        return;
    }

    CELL_ID(placeholder) = CELL_ID(datum);

    if (type == BT_VECTOR) {
        unsigned len = vector_len(placeholder);
        for (unsigned i = 0; i < len; i++) {
            unsigned elem = vector_data_ptr(placeholder)[i];
            vector_set_elem(placeholder, i, elem);
        }
    }

    if (datum >= HEAP_RESERVED) {
        CELL_TYPE(datum) = BT_FREE;
        CELL_ID(datum) = 0;
    }
}

static int reader_getchar(void)
{
    reader_char_state state;
    int c;
    if (reader_pushback_len > 0) {
        state = reader_pushback[--reader_pushback_len];
        c = state.c;
        reader_line = state.line_before;
        reader_col = state.col_before;
    } else {
        FILE *fp = reader_port ? reader_port : stdin;
        c = fgetc(fp);
        state.c = c;
        state.line_before = reader_line;
        state.col_before = reader_col;
    }

    advance_position_for_char(c, &reader_line, &reader_col);
    if (c != EOF)
        reader_record_history(state);
    return c;
}

static void reader_ungetc(int c)
{
    if (c == EOF)
        return;

    if (reader_pushback_len >= READER_PUSHBACK_CAP ||
        reader_history_len == 0) {
        show_error("reader: pushback buffer overflow");
        return;
    }

    reader_char_state state = reader_history[reader_history_len - 1];
    if (state.c != c) {
        show_error("reader: pushback order mismatch");
        return;
    }
    reader_history_len--;
    reader_pushback[reader_pushback_len++] = state;
    reader_line = state.line_before;
    reader_col = state.col_before;
}

// Reset line tracking (call when switching input sources)
void reader_reset_position(void)
{
    reader_line = 1;
    reader_col = 0;
    reader_pushback_len = 0;
    reader_history_len = 0;
    saved_pushback_port = NULL;
    saved_pushback_pos = -1;
    saved_reader_line = 1;
    saved_reader_col = 0;
    saved_pushback_len = 0;
    saved_history_len = 0;
    reader_fold_case = true;
    saved_reader_fold_case = true;
}

// Get current reader position for error messages
int reader_get_line(void)
{
    return reader_line;
}

int reader_get_col(void)
{
    return reader_col;
}

const char *reader_get_filename(void)
{
    return reader_filename;
}
void reader_set_filename(const char *name)
{
    reader_filename = name ? name : "<stdin>";
}

// ============================================================================
// String Buffer - eliminates repeated malloc/realloc pattern
// ============================================================================

typedef struct {
    char *data;
    size_t len;
    size_t cap;
} string_buffer;

static void sb_free(string_buffer *sb);
static inline bool is_delimiter(int c);
static bool reject_invalid_prefixed_tail(int c, const char *name);

static void sb_init(string_buffer *sb)
{
    sb->data = checked_malloc_size(INITIAL_STRING_CAP);
    if (!sb->data) {
        lisp_panic("failed to allocate string buffer");
    }
    sb->data[0] = '\0';
    sb->len = 0;
    sb->cap = INITIAL_STRING_CAP;
}

static void sb_append(string_buffer *sb, int ch)
{
    size_t needed;
    if (!checked_add_size(sb->len, 2, &needed)) {
        lisp_panic("string buffer too large");
    }
    if (needed > sb->cap) {
        size_t new_cap;
        if (!checked_grow_capacity_size(sb->cap, 1, &new_cap)) {
            lisp_panic("string buffer too large");
        }
        char *new_data = checked_realloc_size(sb->data, new_cap);
        if (!new_data) {
            lisp_panic("failed to grow string buffer");
        }
        sb->data = new_data;
        sb->cap = new_cap;
    }
    sb->data[sb->len++] = (char)ch;
    sb->data[sb->len] = '\0';
}

static bool valid_string_scalar(int64_t code)
{
    return code > 0 && is_valid_unicode_scalar(code);
}

static bool sb_append_utf8(string_buffer *sb, int64_t code)
{
    if (!valid_string_scalar(code)) {
        show_error("invalid string character scalar value");
        return false;
    }
    if (code <= 0x7F) {
        sb_append(sb, (int)code);
    } else if (code <= 0x7FF) {
        sb_append(sb, 0xC0 | (int)(code >> 6));
        sb_append(sb, 0x80 | (int)(code & 0x3F));
    } else if (code <= 0xFFFF) {
        sb_append(sb, 0xE0 | (int)(code >> 12));
        sb_append(sb, 0x80 | (int)((code >> 6) & 0x3F));
        sb_append(sb, 0x80 | (int)(code & 0x3F));
    } else {
        sb_append(sb, 0xF0 | (int)(code >> 18));
        sb_append(sb, 0x80 | (int)((code >> 12) & 0x3F));
        sb_append(sb, 0x80 | (int)((code >> 6) & 0x3F));
        sb_append(sb, 0x80 | (int)(code & 0x3F));
    }
    return true;
}

static bool read_hex_scalar_value(const char *digits, int64_t *out,
                                  const char *name)
{
    if (!digits[0]) {
        show_error("%s: empty scalar value", name);
        return false;
    }
    errno = 0;
    char *end = NULL;
    int64_t scalar = strtoll(digits, &end, 16);
    if (errno == ERANGE || !end || *end != '\0' ||
        !is_valid_unicode_scalar(scalar)) {
        show_error("%s: invalid Unicode scalar value", name);
        return false;
    }
    *out = scalar;
    return true;
}

static bool read_utf8_tail_bytes(int first, int64_t *codepoint,
                                 const char *name)
{
    int needed;
    int value;
    if (first >= 0xC2 && first <= 0xDF) {
        needed = 1;
        value = first & 0x1F;
    } else if (first >= 0xE0 && first <= 0xEF) {
        needed = 2;
        value = first & 0x0F;
    } else if (first >= 0xF0 && first <= 0xF4) {
        needed = 3;
        value = first & 0x07;
    } else {
        show_error("%s: invalid UTF-8 leading byte", name);
        return false;
    }

    for (int i = 0; i < needed; i++) {
        int b = reader_getchar();
        if ((b & 0xC0) != 0x80) {
            if (b != EOF)
                reader_ungetc(b);
            show_error("%s: invalid UTF-8 continuation byte", name);
            return false;
        }
        value = (value << 6) | (b & 0x3F);
    }

    if ((needed == 2 && value < 0x800) ||
        (needed == 3 && value < 0x10000) ||
        !is_valid_unicode_scalar(value)) {
        show_error("%s: invalid UTF-8 sequence", name);
        return false;
    }
    *codepoint = value;
    return true;
}

static bool validate_utf8_string(const char *s)
{
    const unsigned char *p = (const unsigned char *)s;
    while (*p) {
        unsigned char first = *p++;
        if (first < 0x80)
            continue;
        int needed;
        int value;
        if (first >= 0xC2 && first <= 0xDF) {
            needed = 1;
            value = first & 0x1F;
        } else if (first >= 0xE0 && first <= 0xEF) {
            needed = 2;
            value = first & 0x0F;
        } else if (first >= 0xF0 && first <= 0xF4) {
            needed = 3;
            value = first & 0x07;
        } else {
            return false;
        }
        for (int i = 0; i < needed; i++) {
            if ((p[i] & 0xC0) != 0x80)
                return false;
            value = (value << 6) | (p[i] & 0x3F);
        }
        if ((needed == 2 && value < 0x800) ||
            (needed == 3 && value < 0x10000) ||
            !valid_string_scalar(value))
            return false;
        p += needed;
    }
    return true;
}

static void sb_append_utf8_scalar(string_buffer *sb, uint32_t code)
{
    if (code < 0x80) {
        sb_append(sb, (int)code);
    } else if (code < 0x800) {
        sb_append(sb, 0xC0 | (int)(code >> 6));
        sb_append(sb, 0x80 | (int)(code & 0x3F));
    } else if (code < 0x10000) {
        sb_append(sb, 0xE0 | (int)(code >> 12));
        sb_append(sb, 0x80 | (int)((code >> 6) & 0x3F));
        sb_append(sb, 0x80 | (int)(code & 0x3F));
    } else {
        sb_append(sb, 0xF0 | (int)(code >> 18));
        sb_append(sb, 0x80 | (int)((code >> 12) & 0x3F));
        sb_append(sb, 0x80 | (int)((code >> 6) & 0x3F));
        sb_append(sb, 0x80 | (int)(code & 0x3F));
    }
}

static void sb_append_token_char(string_buffer *sb, int c)
{
    if (c < 0x80) {
        if (reader_fold_case && c >= 'A' && c <= 'Z')
            c = c - 'A' + 'a';
        sb_append(sb, c);
        return;
    }

    unsigned char first = (unsigned char)c;
    size_t needed;
    uint32_t code;
    if (first >= 0xC2 && first <= 0xDF) {
        needed = 2;
        code = first & 0x1F;
    } else if (first >= 0xE0 && first <= 0xEF) {
        needed = 3;
        code = first & 0x0F;
    } else if (first >= 0xF0 && first <= 0xF4) {
        needed = 4;
        code = first & 0x07;
    } else {
        sb_append(sb, c);
        return;
    }

    unsigned char bytes[4] = {first, 0, 0, 0};
    for (size_t i = 1; i < needed; i++) {
        int next = reader_getchar();
        if (next == EOF) {
            sb_append(sb, c);
            return;
        }
        bytes[i] = (unsigned char)next;
        if ((bytes[i] & 0xC0) != 0x80) {
            for (size_t j = 0; j <= i; j++)
                sb_append(sb, bytes[j]);
            return;
        }
        code = (code << 6) | (uint32_t)(bytes[i] & 0x3F);
    }

    if ((needed == 3 && code < 0x800) ||
        (needed == 4 && code < 0x10000) || !valid_string_scalar(code)) {
        for (size_t i = 0; i < needed; i++)
            sb_append(sb, bytes[i]);
        return;
    }

    if (!reader_fold_case) {
        for (size_t i = 0; i < needed; i++)
            sb_append(sb, bytes[i]);
        return;
    }

    const uint32_t *mapping;
    size_t length;
    if (unicode_full_foldcase(code, &mapping, &length)) {
        for (size_t i = 0; i < length; i++)
            sb_append_utf8_scalar(sb, mapping[i]);
    } else {
        sb_append_utf8_scalar(sb, unicode_simple_foldcase(code));
    }
}

static bool read_reader_directive(void)
{
    string_buffer sb;
    sb_init(&sb);
    int c = reader_getchar();
    while (!is_delimiter(c)) {
        if (c == 0) {
            show_error("reader directive: invalid null character");
            sb_free(&sb);
            return false;
        }
        sb_append(&sb, c);
        c = reader_getchar();
    }
    if (!is_delimiter(c)) {
        sb_free(&sb);
        show_error("reader directive: invalid syntax");
        return false;
    }
    reader_ungetc(c);

    if (strcmp(sb.data, "fold-case") == 0) {
        reader_fold_case = true;
        sb_free(&sb);
        return true;
    }
    if (strcmp(sb.data, "no-fold-case") == 0) {
        reader_fold_case = false;
        sb_free(&sb);
        return true;
    }
    show_error("unknown reader directive: #!%s", sb.data);
    sb_free(&sb);
    return false;
}

static bool read_datum_comment(void)
{
    unsigned skipped = read_obj();
    if (skipped == TOK_ERROR)
        return false;
    if (skipped == TOK_EOF) {
        show_error("unexpected end of file after #;");
        return false;
    }
    if (skipped == TOK_CLOSE || skipped == TOK_DOT) {
        show_error("#; must be followed by an object");
        return false;
    }
    return true;
}

static bool skip_nested_block_comment(void)
{
    int depth = 1;
    while (depth > 0) {
        int c = reader_getchar();
        if (c == EOF) {
            show_error("unterminated block comment");
            return false;
        }
        if (c == '#') {
            int next = reader_getchar();
            if (next == '|') {
                depth++;
            } else {
                reader_ungetc(next);
            }
        } else if (c == '|') {
            int next = reader_getchar();
            if (next == '#') {
                depth--;
            } else {
                reader_ungetc(next);
            }
        }
    }
    return true;
}

static char *sb_finish(string_buffer *sb)
{
    return sb->data; // Caller takes ownership
}

static void sb_free(string_buffer *sb)
{
    free(sb->data);
    sb->data = NULL;
    sb->len = sb->cap = 0;
}

static unsigned read_prefixed_integer(const char *digits, int base, bool neg)
{
    errno = 0;
    char *end = NULL;
    int64_t val = strtoll(digits, &end, base);
    if (end && *end == '\0' && errno != ERANGE) {
        if (neg) {
            bignum *bn = bn_from_int(val);
            if (!bn)
                return TOK_ERROR;
            bn_neg_ip(bn);
            return store_integer(bn);
        }
        return store(val);
    }

    bignum *bn = bn_from_string(digits, base);
    if (!bn)
        return TOK_ERROR;
    if (neg)
        bn_neg_ip(bn);
    return store_integer(bn);
}

typedef enum {
    READER_EXACTNESS_UNSPECIFIED,
    READER_EXACTNESS_EXACT,
    READER_EXACTNESS_INEXACT
} reader_exactness;

static bool prefix_radix(int c, int *base)
{
    switch (c) {
    case 'b':
    case 'B':
        *base = 2;
        return true;
    case 'o':
    case 'O':
        *base = 8;
        return true;
    case 'd':
    case 'D':
        *base = 10;
        return true;
    case 'x':
    case 'X':
        *base = 16;
        return true;
    default:
        return false;
    }
}

static bool prefix_exactness(int c, reader_exactness *exactness)
{
    switch (c) {
    case 'e':
    case 'E':
        *exactness = READER_EXACTNESS_EXACT;
        return true;
    case 'i':
    case 'I':
        *exactness = READER_EXACTNESS_INEXACT;
        return true;
    default:
        return false;
    }
}

static bool radix_digit_p(int c, int base)
{
    if (base <= 10)
        return c >= '0' && c < '0' + base;
    return isxdigit(c) &&
           (isdigit(c) || ((c | 32) >= 'a' && (c | 32) < 'a' + base - 10));
}

static unsigned maybe_apply_exactness(unsigned value, reader_exactness exactness)
{
    if (exactness == READER_EXACTNESS_INEXACT && is_numeric(value) &&
        !IS_INEXACT(value) && !IS_COMPLEX(value))
        return store_inexact(to_double(value));
    return value;
}

static bignum *reader_pow10(uint64_t exp)
{
    bignum *ten = bn_from_int(10);
    if (!ten)
        return NULL;
    bignum *result = bn_pow(ten, exp);
    bn_free(ten);
    return result;
}

static unsigned read_exact_decimal_number(const char *s, bool *handled)
{
    *handled = false;
    const char *p = s;
    bool neg = false;
    if (*p == '+' || *p == '-') {
        neg = *p == '-';
        p++;
    }

    size_t input_len = strlen(p);
    if (input_len > SIZE_MAX - 2) {
        show_error("exact decimal literal: too large");
        return TOK_ERROR;
    }
    size_t digits_cap = input_len + 2;
    char *digits = checked_malloc_size(digits_cap);
    if (!digits) {
        show_error("exact decimal literal: out of memory");
        return TOK_ERROR;
    }
    size_t digits_len = 0;
    bool saw_digit = false;
    bool saw_decimal_syntax = false;
    int64_t frac_digits = 0;

    while (isdigit((unsigned char)*p)) {
        digits[digits_len++] = *p++;
        saw_digit = true;
    }
    if (*p == '.') {
        saw_decimal_syntax = true;
        p++;
        while (isdigit((unsigned char)*p)) {
            digits[digits_len++] = *p++;
            saw_digit = true;
            frac_digits++;
        }
    }
    if (!saw_digit) {
        free(digits);
        return ctx.atom_false;
    }

    int64_t exponent = 0;
    if (*p == 'e' || *p == 'E') {
        saw_decimal_syntax = true;
        p++;
        bool exp_neg = false;
        if (*p == '+' || *p == '-') {
            exp_neg = *p == '-';
            p++;
        }
        if (!isdigit((unsigned char)*p)) {
            free(digits);
            return ctx.atom_false;
        }
        while (isdigit((unsigned char)*p)) {
            if (exponent > (INT64_MAX - 9) / 10) {
                free(digits);
                show_error("exact decimal literal: exponent too large");
                return TOK_ERROR;
            }
            exponent = exponent * 10 + (*p++ - '0');
        }
        if (exp_neg)
            exponent = -exponent;
    }
    if (*p != '\0') {
        free(digits);
        return ctx.atom_false;
    }
    if (!saw_decimal_syntax) {
        free(digits);
        return ctx.atom_false;
    }
    *handled = true;

    digits[digits_len] = '\0';
    char *num_str = checked_malloc_size(digits_len + 2);
    if (!num_str) {
        free(digits);
        show_error("exact decimal literal: out of memory");
        return TOK_ERROR;
    }
    size_t offset = 0;
    if (neg)
        num_str[offset++] = '-';
    memcpy(num_str + offset, digits, digits_len + 1);
    free(digits);

    bignum *num = bn_from_string(num_str, 10);
    free(num_str);
    if (!num) {
        show_error("invalid exact decimal literal");
        return TOK_ERROR;
    }

    int64_t scale;
    if (__builtin_sub_overflow(frac_digits, exponent, &scale)) {
        bn_free(num);
        show_error("exact decimal literal: exponent too large");
        return TOK_ERROR;
    }
    if (scale <= 0) {
        // Avoid undefined signed negation when scale is INT64_MIN.  The
        // two-step form represents its magnitude exactly in uint64_t.
        uint64_t mul_exp = (uint64_t)(-(scale + 1)) + 1;
        bignum *factor = reader_pow10(mul_exp);
        if (!factor) {
            bn_free(num);
            show_error("exact decimal literal: out of memory");
            return TOK_ERROR;
        }
        bignum *scaled = bn_mul(num, factor);
        bn_free(num);
        bn_free(factor);
        if (!scaled) {
            show_error("exact decimal literal: out of memory");
            return TOK_ERROR;
        }
        return store_integer(scaled);
    }

    bignum *den = reader_pow10((uint64_t)scale);
    if (!den) {
        bn_free(num);
        show_error("exact decimal literal: out of memory");
        return TOK_ERROR;
    }
    GC_GUARD;
    unsigned num_cell = store_integer(num);
    gc_protect(&num_cell);
    unsigned den_cell = store_integer(den);
    return normalize_rational_cells(num_cell, den_cell);
}

static unsigned read_prefixed_number(int prefix)
{
    int base = 10;
    reader_exactness exactness = READER_EXACTNESS_UNSPECIFIED;
    bool have_radix = false;

    if (prefix_radix(prefix, &base)) {
        have_radix = true;
    } else if (!prefix_exactness(prefix, &exactness)) {
        show_error("unknown # syntax: #%c", prefix);
        return TOK_ERROR;
    }

    int c = reader_getchar();
    if (c == '#') {
        int second = reader_getchar();
        if (!have_radix && prefix_radix(second, &base)) {
            c = reader_getchar();
        } else if (have_radix && exactness == READER_EXACTNESS_UNSPECIFIED &&
                   prefix_exactness(second, &exactness)) {
            c = reader_getchar();
        } else {
            show_error("invalid numeric prefix");
            return TOK_ERROR;
        }
    }

    bool neg = false;
    if (c == '-') {
        neg = true;
        c = reader_getchar();
    } else if (c == '+') {
        c = reader_getchar();
    }

    string_buffer sb;
    sb_init(&sb);
    if (base == 10) {
        if (neg)
            sb_append(&sb, '-');
        while (!is_delimiter(c)) {
            if (c == 0) {
                show_error("null character in numeric literal");
                sb_free(&sb);
                return TOK_ERROR;
            }
            sb_append(&sb, c);
            c = reader_getchar();
        }
        reader_ungetc(c);
        if (sb.len == (neg ? 1U : 0U)) {
            sb_free(&sb);
            show_error("invalid decimal literal");
            return TOK_ERROR;
        }
        unsigned value;
        if (exactness == READER_EXACTNESS_EXACT) {
            bool handled = false;
            value = read_exact_decimal_number(sb.data, &handled);
            if (value == TOK_ERROR) {
                sb_free(&sb);
                return TOK_ERROR;
            }
            if (!handled)
                value = atom_from_string(sb.data);
        } else {
            value = atom_from_string(sb.data);
        }
        sb_free(&sb);
        if (!is_numeric(value)) {
            show_error("invalid decimal literal");
            return TOK_ERROR;
        }
        return maybe_apply_exactness(value, exactness);
    }

    while (radix_digit_p(c, base)) {
        sb_append(&sb, c);
        c = reader_getchar();
    }
    if (reject_invalid_prefixed_tail(c, "prefixed integer")) {
        sb_free(&sb);
        return TOK_ERROR;
    }
    if (sb.len == 0) {
        sb_free(&sb);
        show_error("invalid prefixed integer literal");
        return TOK_ERROR;
    }
    unsigned value = read_prefixed_integer(sb.data, base, neg);
    sb_free(&sb);
    return maybe_apply_exactness(value, exactness);
}

static bool read_byte_value(unsigned obj, uint8_t *out)
{
    int64_t val;
    if (IS_FIXNUM(obj)) {
        val = FIXNUM_VALUE(obj);
    } else if (IS_NUM(obj)) {
        val = CELL_ID(obj);
    } else {
        return false;
    }

    if (val < 0 || val > UINT8_MAX)
        return false;

    *out = (uint8_t)val;
    return true;
}

// ============================================================================
// Token Delimiter Check
// ============================================================================

static inline bool is_delimiter(int c)
{
    return isspace(c) || c == '(' || c == ')' || c == '"' || c == ';' ||
           c == EOF;
}

static inline bool is_ascii_alpha(int c)
{
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

/* Read the part following the initial t or f in a boolean literal.  The
 * short spellings (#t and #f) and their long aliases must both end at a
 * delimiter so that, for example, #truex is not accepted as #true followed
 * by a symbol. */
static unsigned read_boolean_literal(bool truth)
{
    const char *suffix = truth ? "rue" : "alse";
    int c = reader_getchar();

    if (is_delimiter(c)) {
        reader_ungetc(c);
        return truth ? ctx.atom_true : ctx.atom_false;
    }

    for (size_t i = 0; suffix[i] != '\0'; i++) {
        if (c == EOF || tolower((unsigned char)c) != suffix[i])
            goto invalid;
        c = reader_getchar();
    }

    if (is_delimiter(c)) {
        reader_ungetc(c);
        return truth ? ctx.atom_true : ctx.atom_false;
    }

invalid:
    while (!is_delimiter(c))
        c = reader_getchar();
    reader_ungetc(c);
    show_error("invalid boolean literal");
    return TOK_ERROR;
}

static bool reject_invalid_prefixed_tail(int c, const char *name)
{
    if (is_delimiter(c)) {
        reader_ungetc(c);
        return false;
    }

    while (!is_delimiter(c))
        c = reader_getchar();
    reader_ungetc(c);
    show_error("invalid %s literal", name);
    return true;
}

// ============================================================================
// Character Name Lookup Table
// ============================================================================

static const struct {
    const char *name;
    int value;
} char_names[] = {
    {"alarm", '\a'},     {"backspace", '\b'}, {"delete", 0x7f},
    {"escape", 27},      {"newline", '\n'},   {"null", 0},
    {"return", '\r'},    {"space", ' '},      {"tab", '\t'},
    {NULL, 0},
};

static int lookup_char_name(const char *name)
{
    for (int i = 0; char_names[i].name; i++) {
        if (strcasecmp(name, char_names[i].name) == 0) {
            return char_names[i].value;
        }
    }
    return -1;
}

// ============================================================================
// Token Reading
// ============================================================================

static unsigned read_character_literal(void)
{
    int c = reader_getchar();
    if (c == EOF) {
        show_error("incomplete character literal");
        return TOK_ERROR;
    }

    if (c == 'x' || c == 'X') {
        int h = reader_getchar();
        if (is_delimiter(h)) {
            reader_ungetc(h);
            return make_char(c);
        }
        if (!isxdigit(h)) {
            show_error("invalid character scalar literal");
            return TOK_ERROR;
        }

        string_buffer hex;
        sb_init(&hex);
        do {
            sb_append(&hex, h);
            h = reader_getchar();
        } while (isxdigit(h));

        if (!is_delimiter(h)) {
            sb_free(&hex);
            show_error("invalid character scalar literal");
            return TOK_ERROR;
        }
        reader_ungetc(h);

        int64_t scalar;
        if (!read_hex_scalar_value(hex.data, &scalar,
                                   "character scalar literal")) {
            sb_free(&hex);
            return TOK_ERROR;
        }
        sb_free(&hex);
        return make_char((int)scalar);
    }

    // Try to read a named character
    if (is_ascii_alpha(c)) {
        char buf[CHAR_NAME_BUF_SIZE];
        buf[0] = c;
        int i = 1;
        bool too_long = false;
        int c2 = reader_getchar();
        while (!is_delimiter(c2)) {
            if (i < CHAR_NAME_BUF_SIZE - 1) {
                buf[i++] = c2;
            } else {
                too_long = true;
            }
            c2 = reader_getchar();
        }
        buf[i] = '\0';
        reader_ungetc(c2);

        if (i > 1) {
            if (too_long) {
                show_error("character name too long");
                return TOK_ERROR;
            }
            int char_val = lookup_char_name(buf);
            if (char_val >= 0) {
                return make_char(char_val);
            }
            show_error("unknown character name: %s", buf);
            return TOK_ERROR;
        }
    }

    if ((unsigned char)c >= 0x80) {
        int64_t codepoint;
        if (!read_utf8_tail_bytes((unsigned char)c, &codepoint,
                                  "character literal"))
            return TOK_ERROR;
        int next = reader_getchar();
        if (!is_delimiter(next)) {
            show_error("character literal must contain one character");
            return TOK_ERROR;
        }
        reader_ungetc(next);
        return make_char((int)codepoint);
    }

    return make_char(c);
}

static unsigned read_string_literal(void)
{
    string_buffer sb;
    sb_init(&sb);

    for (;;) {
        int c = reader_getchar();
        if (c == EOF) {
            show_error("unterminated string");
            sb_free(&sb);
            return TOK_ERROR;
        }
        if (c == '"')
            break;
        if (c == '\\') {
            c = reader_getchar();
            switch (c) {
            case 'n': c = '\n'; break;
            case 't': c = '\t'; break;
            case 'r': c = '\r'; break;
            case '\\': c = '\\'; break;
            case '"': c = '"'; break;
            case 'a': c = '\a'; break;
            case 'b': c = '\b'; break;
            case 'e': c = 27; break; // ESC
            case 'x': {
                string_buffer hex;
                sb_init(&hex);
                int h = reader_getchar();
                while (isxdigit(h)) {
                    sb_append(&hex, h);
                    h = reader_getchar();
                }
                if (h == ';') {
                    if (hex.len == 0) {
                        sb_free(&hex);
                        show_error("invalid hex scalar escape: \\x;");
                        sb_free(&sb);
                        return TOK_ERROR;
                    }
                    int64_t scalar;
                    if (!read_hex_scalar_value(hex.data, &scalar,
                                               "hex scalar escape")) {
                        sb_free(&hex);
                        sb_free(&sb);
                        return TOK_ERROR;
                    }
                    if (!valid_string_scalar(scalar)) {
                        show_error("invalid string character scalar value");
                        sb_free(&hex);
                        sb_free(&sb);
                        return TOK_ERROR;
                    }
                    if (!sb_append_utf8(&sb, scalar)) {
                        sb_free(&hex);
                        sb_free(&sb);
                        return TOK_ERROR;
                    }
                    sb_free(&hex);
                    continue;
                }
                if (h != EOF)
                    reader_ungetc(h);
                if (hex.len == 2) {
                    int hi = hex.data[0];
                    int lo = hex.data[1];
                    c = (hi <= '9' ? hi - '0' : (hi | 32) - 'a' + 10) * 16 +
                        (lo <= '9' ? lo - '0' : (lo | 32) - 'a' + 10);
                    if (c == 0) {
                        sb_free(&hex);
                        show_error("invalid string character scalar value");
                        sb_free(&sb);
                        return TOK_ERROR;
                    }
                    sb_free(&hex);
                    break;
                }
                show_warning("invalid hex escape");
                for (size_t i = hex.len; i > 0; i--)
                    reader_ungetc(hex.data[i - 1]);
                sb_free(&hex);
                c = 'x';
                break;
            }
            default:
                if (c >= '0' && c <= '7') {
                    // Octal escape: \NNN (1-3 digits)
                    int val = c - '0';
                    c = reader_getchar();
                    if (c >= '0' && c <= '7') {
                        val = val * 8 + (c - '0');
                        c = reader_getchar();
                        if (c >= '0' && c <= '7') {
                            val = val * 8 + (c - '0');
                        } else {
                            reader_ungetc(c);
                        }
                    } else {
                        reader_ungetc(c);
                    }
                    c = val;
                } else {
                    show_warning("unknown escape sequence: \\%c", c);
                }
            }
        }
        if (c == 0) {
            show_error("invalid string character scalar value");
            sb_free(&sb);
            return TOK_ERROR;
        }
        sb_append(&sb, c);
    }

    if (!validate_utf8_string(sb.data)) {
        show_error("string literal contains invalid UTF-8");
        sb_free(&sb);
        return TOK_ERROR;
    }

    unsigned x = alloc();
    CELL_TYPE(x) = BT_STRING;
    char *s = sb_finish(&sb);
    CELL_PTR(x) = s;
    string_register(s);
    return x;
}

static unsigned read_escaped_identifier(void)
{
    string_buffer sb;
    sb_init(&sb);

    for (;;) {
        int c = reader_getchar();
        if (c == EOF) {
            show_error("unterminated escaped identifier");
            sb_free(&sb);
            return TOK_ERROR;
        }
        if (c == '|')
            break;
        if (c == '\\') {
            c = reader_getchar();
            switch (c) {
            case 'a':
                c = '\a';
                break;
            case 'b':
                c = '\b';
                break;
            case 'n':
                c = '\n';
                break;
            case 'r':
                c = '\r';
                break;
            case 't':
                c = '\t';
                break;
            case '\\':
                c = '\\';
                break;
            case '|':
                c = '|';
                break;
            case 'x': {
                string_buffer hex;
                sb_init(&hex);
                int h = reader_getchar();
                while (isxdigit(h)) {
                    sb_append(&hex, h);
                    h = reader_getchar();
                }
                if (h != ';') {
                    sb_free(&hex);
                    show_error("escaped identifier: invalid hex scalar escape");
                    sb_free(&sb);
                    return TOK_ERROR;
                }
                int64_t scalar;
                if (!read_hex_scalar_value(hex.data, &scalar,
                                           "escaped identifier")) {
                    sb_free(&hex);
                    sb_free(&sb);
                    return TOK_ERROR;
                }
                if (!valid_string_scalar(scalar) ||
                    !sb_append_utf8(&sb, scalar)) {
                    sb_free(&hex);
                    sb_free(&sb);
                    return TOK_ERROR;
                }
                sb_free(&hex);
                continue;
            }
            default:
                show_error("escaped identifier: unknown escape sequence");
                sb_free(&sb);
                return TOK_ERROR;
            }
        }
        if (c == 0) {
            show_error("escaped identifier: invalid null character");
            sb_free(&sb);
            return TOK_ERROR;
        }
        sb_append(&sb, c);
    }

    if (!validate_utf8_string(sb.data)) {
        show_error("escaped identifier contains invalid UTF-8");
        sb_free(&sb);
        return TOK_ERROR;
    }
    unsigned res = atom_from_string(sb.data);
    sb_free(&sb);
    return res;
}

// Read a number starting with decimal point (e.g., .5)
static unsigned read_decimal_number(void)
{
    string_buffer sb;
    sb_init(&sb);
    sb_append(&sb, '.');

    int c;
    while (isdigit(c = reader_getchar()) || c == 'e' || c == 'E' || c == '+' ||
           c == '-') {
        sb_append(&sb, c);
    }
    if (c == 0) {
        show_error("null character in numeric literal");
        sb_free(&sb);
        return TOK_ERROR;
    }
    reader_ungetc(c);

    unsigned res = atom_from_string(sb.data);
    sb_free(&sb);
    return res;
}

unsigned read_token(void)
{
    int c;
    for (;;) {
        c = reader_getchar();
        while (isspace(c))
            c = reader_getchar();

        // Comments
        if (c == ';') {
            while ((c = reader_getchar()) != '\n' && c != EOF)
                ;
            if (c == EOF) {
                if (reader_port == NULL && reader_obj_depth <= 1)
                    exit(0);
                return TOK_EOF;
            }
            continue;
        }

        if (c == EOF) {
            if (reader_port == NULL && reader_obj_depth <= 1)
                exit(0);
            return TOK_EOF;
        }

        switch (c) {
        case '(':
            return TOK_OPEN;
        case ')':
            return TOK_CLOSE;
        case '.': {
            // Check for ellipsis (...) or number starting with .
            int c2 = reader_getchar();
            if (c2 == '.') {
                int c3 = reader_getchar();
                if (c3 == '.') {
                    return atom_from_string("...");
                }
                reader_ungetc(c3);
            } else if (isdigit(c2)) {
                reader_ungetc(c2);
                return read_decimal_number();
            }
            reader_ungetc(c2);
            return TOK_DOT;
        }
        case '\'':
            return TOK_QUOTE;
        case '`':
            return TOK_QUASIQUOTE;
        case ',': {
            c = reader_getchar();
            if (c == '@') {
                return TOK_UNQUOTE_SPLICING;
            }
            reader_ungetc(c);
            return TOK_UNQUOTE;
        }
        case '#': {
            c = reader_getchar();
            if (c == '(') {
                return TOK_VECTOR_OPEN;
            } else if (c == ';') {
                if (!read_datum_comment())
                    return TOK_ERROR;
                continue;
            } else if (c == '|') {
                if (!skip_nested_block_comment())
                    return TOK_ERROR;
                continue;
            } else if (c == '!') {
                if (!read_reader_directive())
                    return TOK_ERROR;
                continue;
            } else if (c == '\\') {
                return read_character_literal();
            } else if (c == 't' || c == 'T') {
                return read_boolean_literal(true);
            } else if (c == 'f' || c == 'F') {
                return read_boolean_literal(false);
            } else if (c == 'x' || c == 'X' || c == 'o' || c == 'O' ||
                       c == 'b' || c == 'B' || c == 'd' || c == 'D' ||
                       c == 'e' || c == 'E' || c == 'i' || c == 'I') {
                return read_prefixed_number(c);
            } else if (c == 'u') {
                // Bytevector literal: #u8(...)
                c = reader_getchar();
                if (c == EOF) {
                    show_error("unexpected end of file after #u");
                    return TOK_ERROR;
                }
                if (c == '8') {
                    c = reader_getchar();
                    if (c == EOF) {
                        show_error("unexpected end of file after #u8");
                        return TOK_ERROR;
                    }
                    if (c == '(') {
                        // Read bytes by reading a list then converting
                        reader_ungetc(c); // put back '('
                        unsigned list = read_obj(); // read (b1 b2 ...)
                        if (list == TOK_ERROR)
                            return TOK_ERROR;
                        if (list == TOK_EOF) {
                            show_error("unexpected end of file in bytevector literal");
                            return TOK_ERROR;
                        }
                        unsigned count = 0;
                        for (unsigned p = list; p; p = cdr(p)) {
                            if (!IS_PAIR(p)) {
                                show_error("bytevector literal: improper list");
                                return TOK_ERROR;
                            }
                            unsigned byte = car(p);
                            uint8_t byte_value;
                            if (!read_byte_value(byte, &byte_value)) {
                                show_error("bytevector literal: byte out of range");
                                return TOK_ERROR;
                            }
                            count++;
                        }
                        if (count > 1024 * 1024) {
                            show_error("bytevector literal too large");
                            return TOK_ERROR;
                        }
                        bytevec_data *bv =
                            checked_malloc_flex(sizeof(bytevec_data), count, 1);
                        if (!bv) {
                            show_error("bytevector literal: out of memory");
                            return TOK_ERROR;
                        }
                        bv->len = count;
                        bytevec_register(bv);
                        unsigned i = 0;
                        for (unsigned p = list; p && IS_PAIR(p); p = cdr(p)) {
                            uint8_t byte_value;
                            if (!read_byte_value(car(p), &byte_value)) {
                                bytevec_unregister(bv);
                                free(bv);
                                show_error("bytevector literal: byte out of range");
                                return TOK_ERROR;
                            }
                            bv->data[i++] = byte_value;
                        }
                        unsigned cell = alloc();
                        CELL_TYPE(cell) = BT_BYTEVEC;
                        CELL_PTR(cell) = bv;
                        return cell;
                    }
                    reader_ungetc(c);
                }
                show_error("unknown # syntax: #u%c", c);
                return TOK_ERROR;
            } else if (isdigit(c)) {
                // Datum label: #n= or #n#
                int label = c - '0';
                while (isdigit(c = reader_getchar())) {
                    // Check for integer overflow before multiplication
                    int digit = c - '0';
                    if (label > (INT_MAX - digit) / 10) {
                        show_error("datum label too large");
                        return TOK_ERROR;
                    }
                    label = label * 10 + digit;
                }
                if (c == '=') {
                    // Define label: #n=<datum>
                    int idx = find_datum_label(label);
                    if (idx >= 0 && datum_labels[idx].defined) {
                        show_error("duplicate datum label #%d=", label);
                        return TOK_ERROR;
                    }
                    // Pre-allocate a cell for forward references
                    unsigned placeholder = alloc_cons(0, 0);
                    if (idx < 0) {
                        idx = add_datum_label(label);
                        if (idx < 0)
                            return TOK_ERROR;
                    }
                    datum_labels[idx].value = placeholder;
                    datum_labels[idx].defined = true;
                    // Read the datum
                    GC_GUARD;
                    gc_protect(&placeholder);
                    unsigned datum = read_obj();
                    gc_protect(&datum);
                    if (datum == TOK_ERROR)
                        return TOK_ERROR;
                    if (datum == TOK_EOF) {
                        show_error("unexpected end of file after datum label");
                        return TOK_ERROR;
                    }
                    if (datum == TOK_CLOSE || datum == TOK_DOT) {
                        show_error("datum label must be followed by an object");
                        return TOK_ERROR;
                    }
                    // A bare self-reference has no datum whose structure can
                    // be filled into the placeholder.  Treating it as the
                    // placeholder itself used to overwrite that placeholder
                    // with its initial empty-list contents, silently reading
                    // #n=#n# as ().  Cycles must be rooted in an actual
                    // compound datum, such as #n=(x . #n#).
                    if (datum == placeholder) {
                        show_error("datum label cannot directly reference itself");
                        return TOK_ERROR;
                    }
                    if (!IS_CELL(datum)) {
                        datum_labels[idx].value = datum;
                        return datum;
                    }
                    resolve_datum_label_placeholder(placeholder, datum);
                    return placeholder;
                } else if (c == '#') {
                    // Reference label: #n#
                    int idx = find_datum_label(label);
                    if (idx < 0) {
                        show_error("undefined datum label #%d#", label);
                        return TOK_ERROR;
                    }
                    return datum_labels[idx].value;
                } else {
                    show_error("expected = or # after #%d", label);
                    return TOK_ERROR;
                }
            }
            show_error("unknown # syntax: #%c", c);
            return TOK_ERROR;
        }
        case '"':
            return read_string_literal();
        case '|':
            return read_escaped_identifier();
        default: {
            // Symbol or number
            if (c == 0) {
                show_error("null character in token");
                return TOK_ERROR;
            }
            string_buffer sb;
            sb_init(&sb);
            bool is_number = isdigit(c) || c == '-' || c == '+';
            bool delimiter_unread = false;
            sb_append_token_char(&sb, c);

            for (;;) {
                c = reader_getchar();
                if (c == 0) {
                    show_error("null character in token");
                    sb_free(&sb);
                    return TOK_ERROR;
                }
                // Allow . in numbers (for decimals like 1.5)
                if (c == '.' && is_number) {
                    int c2 = reader_getchar();
                    if (isdigit(c2) || c2 == 'e' || c2 == 'E') {
                        // It's a decimal number
                        sb_append(&sb, '.');
                        sb_append_token_char(&sb, c2);
                        continue;
                    } else if (is_delimiter(c2)) {
                        // End of number followed by dot (for dotted pairs)
                        reader_ungetc(c2);
                        reader_ungetc(c);
                        delimiter_unread = true;
                        break;
                    }
                    reader_ungetc(c2);
                    sb_append(&sb, '.');
                    continue;
                }
                if (is_delimiter(c))
                    break;
                // R5RS: . + - @ are valid subsequent characters in identifiers
                // The . as dotted-pair marker is handled in the switch above
                sb_append_token_char(&sb, c);
            }
            if (!delimiter_unread)
                reader_ungetc(c);
            if (!validate_utf8_string(sb.data)) {
                show_error("symbol contains invalid UTF-8");
                sb_free(&sb);
                return TOK_ERROR;
            }
            unsigned res = atom_from_string(sb.data);
            sb_free(&sb);
            return res;
        }
        }
    }
}

// ============================================================================
// Object and List Reading
// ============================================================================

unsigned read_vector(void)
{
    GC_GUARD;
    unsigned head = 0, tail = 0;
    unsigned count = 0;
    gc_protect(&head);
    gc_protect(&tail);

    for (;;) {
        unsigned elem = read_obj();
        if (elem == TOK_CLOSE)
            break;
        if (elem == TOK_ERROR)
            return TOK_ERROR;
        if (elem == TOK_EOF) {
            show_error("unexpected end of file in vector");
            return TOK_ERROR;
        }
        if (elem == TOK_DOT) {
            show_error("dot not allowed in vector literal");
            return TOK_ERROR;
        }

        list_append(&head, &tail, elem);
        count++;
    }

    unsigned vec = make_vector(count, 0);
    if (vec == TOK_ERROR)
        return TOK_ERROR;
    unsigned *data = vector_data_ptr(vec);
    unsigned i = 0;
    FORLIST(l, head) { data[i++] = car(l); }
    return vec;
}

static unsigned read_obj_inner(void)
{
    unsigned tok = read_token();
    switch (tok) {
    case TOK_OPEN:
        return read_list();
    case TOK_VECTOR_OPEN:
        return read_vector();
    case TOK_QUOTE:
        tok = read_obj();
        switch (tok) {
        case TOK_CLOSE:
            show_error("quote must be followed by an object");
            return TOK_ERROR;
        case TOK_DOT:
            show_error("quote must be followed by an object");
            return TOK_ERROR;
        case TOK_ERROR:
            return tok;
        case TOK_EOF:
            show_error("unexpected end of file after quote");
            return TOK_ERROR;
        default: {
            GC_GUARD;
            gc_protect(&tok);
            unsigned args = alloc_cons(tok, 0);
            gc_protect(&args);
            return alloc_cons(ctx.atom_quote, args);
        }
        }
    case TOK_QUASIQUOTE:
        tok = read_obj();
        if (tok == TOK_EOF) {
            show_error("unexpected end of file after quasiquote");
            return TOK_ERROR;
        }
        if (tok == TOK_CLOSE || tok == TOK_DOT || tok == TOK_ERROR) {
            show_error("quasiquote must be followed by an object");
            return TOK_ERROR;
        }
        {
            GC_GUARD;
            gc_protect(&tok);
            unsigned args = alloc_cons(tok, 0);
            gc_protect(&args);
            return alloc_cons(ctx.atom_quasiquote, args);
        }
    case TOK_UNQUOTE:
        tok = read_obj();
        if (tok == TOK_EOF) {
            show_error("unexpected end of file after unquote");
            return TOK_ERROR;
        }
        if (tok == TOK_CLOSE || tok == TOK_DOT || tok == TOK_ERROR) {
            show_error("unquote must be followed by an object");
            return TOK_ERROR;
        }
        {
            GC_GUARD;
            gc_protect(&tok);
            unsigned args = alloc_cons(tok, 0);
            gc_protect(&args);
            return alloc_cons(ctx.atom_unquote, args);
        }
    case TOK_UNQUOTE_SPLICING:
        tok = read_obj();
        if (tok == TOK_EOF) {
            show_error("unexpected end of file after unquote-splicing");
            return TOK_ERROR;
        }
        if (tok == TOK_CLOSE || tok == TOK_DOT || tok == TOK_ERROR) {
            show_error("unquote-splicing must be followed by an object");
            return TOK_ERROR;
        }
        {
            GC_GUARD;
            gc_protect(&tok);
            unsigned args = alloc_cons(tok, 0);
            gc_protect(&args);
            return alloc_cons(ctx.atom_unquote_splicing, args);
        }
    default:
        return tok;
    }
}

unsigned read_obj(void)
{
    if (reader_obj_depth >= READER_MAX_OBJECT_DEPTH) {
        show_error("reader nesting too deep");
        return TOK_ERROR;
    }
    bool outermost = reader_obj_depth == 0;
    reader_obj_depth++;
    unsigned result = read_obj_inner();
    reader_obj_depth--;
    if (outermost && result == TOK_EOF)
        return atom_from_string("eof-object");
    return result;
}

unsigned read_list(void)
{
    GC_GUARD;
    unsigned head = 0, tail = 0;
    gc_protect(&head);
    gc_protect(&tail);

    for (;;) {
        unsigned item = read_obj();
        gc_protect(&item);
        if (item == TOK_ERROR)
            return TOK_ERROR;
        if (item == TOK_CLOSE)
            return head;
        if (item == TOK_EOF) {
            show_error("unexpected end of file in list");
            return TOK_ERROR;
        }
        if (item != TOK_DOT) {
            list_append(&head, &tail, item);
            gc_unprotect(1);
            continue;
        }

        gc_unprotect(1);
        if (!tail) {
            show_error("a dot must follow an object");
            return TOK_ERROR;
        }

        unsigned rest = read_obj();
        gc_protect(&rest);
        if (rest == TOK_ERROR)
            return TOK_ERROR;
        if (rest == TOK_DOT || rest == TOK_CLOSE) {
            show_error("a dot must be followed by an object");
            return TOK_ERROR;
        }
        if (rest == TOK_EOF) {
            show_error("unexpected end of file after dot");
            return TOK_ERROR;
        }

        unsigned end = read_obj();
        if (end != TOK_CLOSE) {
            show_error("only one object may follow a dot");
            return TOK_ERROR;
        }
        cell_set_cdr(tail, rest);
        return head;
    }
}

void reader_reset_labels(void)
{
    reset_datum_labels();
    reader_pushback_len = 0;
    reader_history_len = 0;
}

unsigned read_obj_port(FILE *port)
{
    FILE *old_port = reader_port;
    int old_line = reader_line;
    int old_col = reader_col;
    reader_char_state old_pushback[READER_PUSHBACK_CAP];
    reader_char_state old_history[READER_PUSHBACK_CAP];
    int old_pushback_len = reader_pushback_len;
    int old_history_len = reader_history_len;
    bool old_fold_case = reader_fold_case;
    memcpy(old_pushback, reader_pushback, sizeof(old_pushback));
    memcpy(old_history, reader_history, sizeof(old_history));

    reader_port = port;
    long port_pos = ftell(port);
    if (saved_pushback_port == port &&
        (port_pos < 0 || port_pos == saved_pushback_pos)) {
        reader_line = saved_reader_line;
        reader_col = saved_reader_col;
        memcpy(reader_pushback, saved_pushback, sizeof(reader_pushback));
        memcpy(reader_history, saved_history, sizeof(reader_history));
        reader_pushback_len = saved_pushback_len;
        reader_history_len = saved_history_len;
        reader_fold_case = saved_reader_fold_case;
    } else {
        reader_line = 1;
        reader_col = 0;
        reader_pushback_len = 0;
        reader_history_len = 0;
        reader_fold_case = true;
    }
    reset_datum_labels();
    unsigned result = read_obj();
    if (ferror(port)) {
        show_error("read: input error");
        result = TOK_ERROR;
    }
    saved_pushback_port = port;
    saved_pushback_pos = ftell(port);
    saved_reader_line = reader_line;
    saved_reader_col = reader_col;
    memcpy(saved_pushback, reader_pushback, sizeof(saved_pushback));
    memcpy(saved_history, reader_history, sizeof(saved_history));
    saved_pushback_len = reader_pushback_len;
    saved_history_len = reader_history_len;
    saved_reader_fold_case = reader_fold_case;

    reader_port = old_port;
    reader_line = old_line;
    reader_col = old_col;
    reader_fold_case = old_fold_case;
    memcpy(reader_pushback, old_pushback, sizeof(reader_pushback));
    memcpy(reader_history, old_history, sizeof(reader_history));
    reader_pushback_len = old_pushback_len;
    reader_history_len = old_history_len;
    return result;
}

size_t reader_port_pending_bytes(FILE *port)
{
    if (reader_port == port)
        return (size_t)reader_pushback_len;
    if (saved_pushback_port == port)
        return (size_t)saved_pushback_len;
    return 0;
}

int reader_port_getc(FILE *port)
{
    if (reader_port == port && reader_pushback_len > 0) {
        reader_char_state state = reader_pushback[--reader_pushback_len];
        int c = state.c;
        advance_position_for_char(c, &reader_line, &reader_col);
        reader_record_history(state);
        return c;
    }
    if (saved_pushback_port == port && saved_pushback_len > 0) {
        reader_char_state state = saved_pushback[--saved_pushback_len];
        int c = state.c;
        advance_position_for_char(c, &saved_reader_line, &saved_reader_col);
        saved_reader_record_history(state);
        return c;
    }

    int c = fgetc(port);
    if (c == EOF)
        return EOF;

    if (reader_port == port) {
        reader_char_state state = {c, reader_line, reader_col};
        advance_position_for_char(c, &reader_line, &reader_col);
        reader_record_history(state);
        return c;
    }

    if (saved_pushback_port != port) {
        saved_pushback_port = port;
        saved_pushback_pos = -1;
        saved_reader_line = 1;
        saved_reader_col = 0;
        saved_pushback_len = 0;
        saved_history_len = 0;
        saved_reader_fold_case = true;
    }
    reader_char_state state = {c, saved_reader_line, saved_reader_col};
    advance_position_for_char(c, &saved_reader_line, &saved_reader_col);
    saved_reader_record_history(state);
    saved_pushback_pos = ftell(port);
    return c;
}

int reader_port_peekc(FILE *port)
{
    if (reader_port == port && reader_pushback_len > 0)
        return reader_pushback[reader_pushback_len - 1].c;
    if (saved_pushback_port == port && saved_pushback_len > 0)
        return saved_pushback[saved_pushback_len - 1].c;

    int c = fgetc(port);
    if (c != EOF && ungetc(c, port) == EOF)
        return EOF;
    return c;
}

bool reader_port_ungetc(FILE *port, int c)
{
    if (c == EOF)
        return true;

    if (reader_port == port) {
        if (reader_pushback_len >= READER_PUSHBACK_CAP ||
            reader_history_len == 0)
            return false;
        reader_char_state state = reader_history[reader_history_len - 1];
        if (state.c != c)
            return false;
        reader_history_len--;
        reader_pushback[reader_pushback_len++] = state;
        reader_line = state.line_before;
        reader_col = state.col_before;
        return true;
    }

    if (saved_pushback_port != port ||
        saved_pushback_len >= READER_PUSHBACK_CAP ||
        saved_history_len == 0)
        return false;
    reader_char_state state = saved_history[saved_history_len - 1];
    if (state.c != c)
        return false;
    saved_history_len--;
    saved_pushback[saved_pushback_len++] = state;
    saved_reader_line = state.line_before;
    saved_reader_col = state.col_before;
    saved_pushback_pos = ftell(port);
    return true;
}

void reader_forget_port(FILE *port)
{
    if (saved_pushback_port == port) {
        saved_pushback_port = NULL;
        saved_pushback_pos = -1;
        saved_reader_line = 1;
        saved_reader_col = 0;
        saved_pushback_len = 0;
        saved_history_len = 0;
        saved_reader_fold_case = true;
    }
    if (reader_port == port) {
        reader_pushback_len = 0;
        reader_history_len = 0;
    }
}
