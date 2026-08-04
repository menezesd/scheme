/**
 * @file primitives.c
 * @brief Built-in primitive procedures - main dispatch
 *
 * This file contains the main dispatch function for primitives and remaining
 * operations not extracted to separate modules. Most primitives are now in:
 *
 * - prim_numeric.c: Arithmetic (+, -, *, /, mod, quotient, remainder, abs)
 * - prim_compare.c: Comparison operations (=, <, >, <=, >=, char/string
 * compare)
 * - prim_list.c: List operations (append, reverse)
 * - prim_string.c: String operations (string-append, substring)
 * - prim_type.c: Type predicates (number?, symbol?, etc.)
 * - prim_char.c: Character operations (char->integer, char predicates)
 * - prim_vector.c: Vector operations (make-vector, vector-ref, etc.)
 * - prim_math.c: Math functions (sqrt, sin, cos, floor, etc.)
 * - prim_io.c: I/O operations (read, write, display)
 * - prim_port.c: Port operations (open/close, string ports)
 * - prim_numtower.c: Numeric tower (complex, rational, exactness)
 */

#include "primitives.h"
#include "feature_table.h"
#include "prim_internal.h"
#include "utf8.h"
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#define PRIM_FNV_OFFSET_BASIS 14695981039346656037ull
#define PRIM_FNV_PRIME 1099511628211ull
#define JIFFIES_PER_SECOND 1000000000LL

extern char **environ;

// Command line args (set by main.c, defaults for test binaries)
int saved_argc __attribute__((weak)) = 0;
char **saved_argv __attribute__((weak)) = NULL;

// ============================================================================
// Local primitive helpers
// ============================================================================

static bool expect_u8(unsigned value, int64_t *out, const char *name)
{
    int64_t byte;
    if (!expect_exact_int64(value, &byte, name))
        return false;
    if (byte < 0 || byte > 255) {
        show_error("%s: byte out of range", name);
        return false;
    }
    *out = byte;
    return true;
}

static char *exact_integer_to_string(unsigned num, int radix)
{
    if (IS_FIXNUM(num) || IS_NUM(num)) {
        int64_t n = IS_FIXNUM(num) ? FIXNUM_VALUE(num) : CELL_ID(num);
        char buf[NUMBER_BUF_SIZE];
        if (radix == 10) {
            snprintf(buf, sizeof(buf), "%" PRId64, n);
        } else {
            char *p = buf + sizeof(buf) - 1;
            *p = '\0';
            bool neg = n < 0;
            uint64_t magnitude = neg ? -(uint64_t)n : (uint64_t)n;
            if (magnitude == 0) {
                *--p = '0';
            } else {
                while (magnitude > 0) {
                    int d = (int)(magnitude % (uint64_t)radix);
                    *--p = (d < 10) ? '0' + d : 'a' + d - 10;
                    magnitude /= (uint64_t)radix;
                }
            }
            if (neg)
                *--p = '-';
            memmove(buf, p, buf + sizeof(buf) - p);
        }
        return checked_string_copy(buf);
    }
    if (IS_BIGNUM(num)) {
        bignum *bn = get_bignum(num);
        return bn ? bn_to_string(bn, radix) : NULL;
    }
    return NULL;
}

static char *rational_to_string(unsigned num, int radix, bool *too_large)
{
    *too_large = false;
    char *numer = exact_integer_to_string(CELL_CAR(num), radix);
    char *denom = exact_integer_to_string(CELL_CDR(num), radix);
    if (!numer || !denom) {
        free(numer);
        free(denom);
        return NULL;
    }

    size_t numer_len = strlen(numer);
    size_t denom_len = strlen(denom);
    if (numer_len > SIZE_MAX - denom_len - 2) {
        free(numer);
        free(denom);
        *too_large = true;
        return NULL;
    }

    char *s = checked_malloc_flex(0, numer_len + denom_len + 2, 1);
    if (s) {
        memcpy(s, numer, numer_len);
        s[numer_len] = '/';
        memcpy(s + numer_len + 1, denom, denom_len + 1);
    }
    free(numer);
    free(denom);
    return s;
}

static bool parse_optional_radix(unsigned argc, unsigned *argv,
                                 const char *name, int *radix)
{
    *radix = 10;
    if (argc > 1) {
        int64_t radix64;
        if (!expect_exact_int64(argv[1], &radix64, name))
            return false;
        if (radix64 < 2 || radix64 > 36) {
            show_error("%s: radix must be between 2 and 36", name);
            return false;
        }
        *radix = (int)radix64;
    }
    return true;
}

static bytevec_data *bytevector_data_new(unsigned len)
{
    bytevec_data *bv = checked_malloc_flex(sizeof(bytevec_data), len, 1);
    if (bv) {
        bv->len = len;
        bytevec_register(bv);
    }
    return bv;
}

static unsigned make_bytevector_owned(bytevec_data *bv)
{
    unsigned cell = alloc();
    CELL_TYPE(cell) = BT_BYTEVEC;
    CELL_PTR(cell) = bv;
    return cell;
}

static unsigned make_bytevector_copy(const uint8_t *data, unsigned len,
                                     const char *name)
{
    bytevec_data *bv = bytevector_data_new(len);
    if (!bv) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    if (len > 0)
        memcpy(bv->data, data, len);
    return make_bytevector_owned(bv);
}

static unsigned make_bytevector_from_u8_argv(unsigned argc, unsigned *argv,
                                             const char *name)
{
    bytevec_data *bv = bytevector_data_new(argc);
    if (!bv) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }

    for (unsigned i = 0; i < argc; i++) {
        int64_t val;
        if (!expect_u8(argv[i], &val, name)) {
            bytevec_unregister(bv);
            free(bv);
            return TOK_ERROR;
        }
        bv->data[i] = (uint8_t)val;
    }
    return make_bytevector_owned(bv);
}

static unsigned make_bytevector_filled(unsigned len, uint8_t fill,
                                       const char *name)
{
    bytevec_data *bv = bytevector_data_new(len);
    if (!bv) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    memset(bv->data, fill, len);
    return make_bytevector_owned(bv);
}

static bytevec_data *require_bytevector(unsigned value, const char *name);
static bool bytevector_length_fits(uint64_t len, const char *name,
                                   const char *noun);

static unsigned make_bytevector_append(unsigned argc, unsigned *argv,
                                       const char *name)
{
    unsigned total = 0;
    for (unsigned i = 0; i < argc; i++) {
        bytevec_data *src = require_bytevector(argv[i], name);
        if (!src)
            return TOK_ERROR;
        if (src->len > UINT_MAX - total) {
            show_error("%s: result too large", name);
            return TOK_ERROR;
        }
        total += src->len;
    }

    bytevec_data *bv = bytevector_data_new(total);
    if (!bv) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }

    unsigned pos = 0;
    for (unsigned i = 0; i < argc; i++) {
        bytevec_data *src = (bytevec_data *)CELL_PTR(argv[i]);
        memcpy(bv->data + pos, src->data, src->len);
        pos += src->len;
    }
    return make_bytevector_owned(bv);
}

static unsigned read_bytevector_from_port(unsigned count_arg,
                                          unsigned port_arg,
                                          const char *name)
{
    int64_t count;
    if (!expect_nonneg_int64(count_arg, &count, name))
        return TOK_ERROR;
    if (!IS_INPORT(port_arg)) {
        show_error("%s: not an input port", name);
        return TOK_ERROR;
    }
    if (!bytevector_length_fits((uint64_t)count, name, "count"))
        return TOK_ERROR;
    FILE *f = file_port_file(port_arg);
    if (!f) {
        show_error("%s: port is closed", name);
        return TOK_ERROR;
    }
    if (count == 0)
        return make_bytevector_copy(NULL, 0, name);

    uint8_t *buf = checked_malloc_array((unsigned)count, 1);
    if (!buf) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }

    size_t n = 0;
    while (n < (size_t)count && reader_port_pending_bytes(f) > 0) {
        int c = reader_port_getc(f);
        if (c == EOF)
            break;
        buf[n++] = (uint8_t)c;
    }
    if (n < (size_t)count)
        n += fread(buf + n, 1, (size_t)count - n, f);
    if (ferror(f)) {
        free(buf);
        show_error("%s: read failed", name);
        return TOK_ERROR;
    }
    if (n == 0) {
        free(buf);
        return CELL_EOF_OBJECT;
    }

    unsigned result = make_bytevector_copy(buf, (unsigned)n, name);
    free(buf);
    return result;
}

static bool bytevector_length_fits(uint64_t len, const char *name,
                                   const char *noun)
{
    if (len <= UINT_MAX && len <= SIZE_MAX - sizeof(bytevec_data))
        return true;
    show_error("%s: %s too large", name, noun);
    return false;
}

static char *string_buffer_new(uint64_t len, const char *name,
                               const char *too_large_noun)
{
    if (len > SIZE_MAX - 1) {
        show_error("%s: %s too large", name, too_large_noun);
        return NULL;
    }
    size_t size = (size_t)len;
    char *s = checked_malloc_flex(0, size + 1, 1);
    if (!s)
        show_error("%s: out of memory", name);
    return s;
}

static unsigned list_cell_at(unsigned lst, int64_t idx, const char *name)
{
    for (int64_t i = 0; i < idx; i++) {
        if (!IS_PAIR(lst)) {
            show_error("%s: index out of bounds", name);
            return TOK_ERROR;
        }
        lst = cdr(lst);
    }
    if (!IS_PAIR(lst)) {
        show_error("%s: index out of bounds", name);
        return TOK_ERROR;
    }
    return lst;
}

static bool char_list_length(unsigned lst, unsigned *len_out,
                             const char *name)
{
    if (!list_length_checked(lst, len_out, name))
        return false;
    for (unsigned it = lst; it; it = cdr(it)) {
        if (!IS_CHAR(car(it))) {
            show_error("%s: list elements must be characters", name);
            return false;
        }
    }
    return true;
}

static bool utf8_encode_char(int code, char out[4], size_t *len,
                             const char *name)
{
    // Scheme strings are represented by NUL-terminated C buffers, so an
    // embedded U+0000 would make subsequent string operations truncate them.
    if (code == 0 || !scheme_utf8_encode_scalar((uint32_t)code, out, len)) {
        show_error("%s: character cannot be stored in a string", name);
        return false;
    }
    return true;
}

static bool utf8_decode_next(const char *s, size_t byte_len, size_t *offset,
                             int *code, const char *name)
{
    uint32_t scalar;
    const char *error_msg = NULL;
    if (!scheme_utf8_decode_next(s, byte_len, offset, &scalar, &error_msg)) {
        show_error("%s: %s", name, error_msg);
        return false;
    }
    *code = (int)scalar;
    return true;
}

static bool utf8_count_chars(const char *s, size_t *chars,
                             const char *name)
{
    const char *error_msg = NULL;
    if (scheme_utf8_count_chars(s, chars, &error_msg))
        return true;
    show_error("%s: %s", name, error_msg);
    return false;
}

static bool utf8_byte_offset_for_index(const char *s, size_t char_index,
                                       bool allow_end, size_t *byte_offset,
                                       const char *name)
{
    const char *error_msg = NULL;
    if (scheme_utf8_byte_offset_for_index(s, char_index, allow_end,
                                          byte_offset, &error_msg))
        return true;
    show_error("%s: %s", name, error_msg);
    return false;
}

static bool utf8_range_offsets(const char *s, int64_t start, int64_t end,
                               size_t *start_byte, size_t *end_byte,
                               const char *name)
{
    size_t char_len;
    if (!utf8_count_chars(s, &char_len, name))
        return false;
    if (start < 0 || end < start || (uint64_t)end > char_len) {
        show_error("%s: invalid indices", name);
        return false;
    }
    return utf8_byte_offset_for_index(s, (size_t)start, true, start_byte,
                                      name) &&
           utf8_byte_offset_for_index(s, (size_t)end, true, end_byte, name);
}

static unsigned make_char_list_from_string_range(unsigned argc, unsigned *argv,
                                                 const char *name)
{
    char *s = require_string_ptr(argv[0], name);
    if (!s)
        return TOK_ERROR;
    size_t byte_len = strlen(s);
    size_t char_len;
    if (!utf8_count_chars(s, &char_len, name))
        return TOK_ERROR;
    int64_t start = 0;
    int64_t end = (int64_t)char_len;
    if (argc > 1 && !expect_nonneg_int64(argv[1], &start, name))
        return TOK_ERROR;
    if (argc > 2 && !expect_nonneg_int64(argv[2], &end, name))
        return TOK_ERROR;
    size_t start_byte, end_byte;
    if (!utf8_range_offsets(s, start, end, &start_byte, &end_byte, name))
        return TOK_ERROR;
    size_t range_len = (size_t)(end - start);
    GC_GUARD;
    gc_protect(&argv[0]);
    unsigned result = 0;
    gc_protect(&result);
    size_t offsets_count = range_len == 0 ? 1 : range_len;
    size_t *offsets = checked_malloc_array(offsets_count, sizeof(size_t));
    if (!offsets) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    size_t offset = start_byte;
    for (size_t i = 0; i < range_len; i++) {
        offsets[i] = offset;
        int code;
        if (!utf8_decode_next(s, byte_len, &offset, &code, name)) {
            free(offsets);
            return TOK_ERROR;
        }
    }
    if (offset != end_byte) {
        free(offsets);
        show_error("%s: invalid UTF-8 range", name);
        return TOK_ERROR;
    }
    for (size_t i = range_len; i > 0; i--) {
        offset = offsets[i - 1];
        int code;
        if (!utf8_decode_next(s, byte_len, &offset, &code, name)) {
            free(offsets);
            return TOK_ERROR;
        }
        unsigned ch = make_char(code);
        gc_protect(&ch);
        result = alloc_cons(ch, result);
        gc_unprotect(1);
    }
    free(offsets);
    return result;
}

static unsigned make_string_from_char_list(unsigned lst, const char *name)
{
    unsigned list_len = 0;
    if (!char_list_length(lst, &list_len, name))
        return TOK_ERROR;
    size_t total = 0;
    for (unsigned it = lst; it; it = cdr(it)) {
        char encoded[4];
        size_t encoded_len;
        if (!utf8_encode_char((int)CELL_ID(car(it)), encoded, &encoded_len,
                              name))
            return TOK_ERROR;
        if (encoded_len > SIZE_MAX - total - 1) {
            show_error("%s: result too large", name);
            return TOK_ERROR;
        }
        total += encoded_len;
    }
    char *s = string_buffer_new(total, name, "result");
    if (!s)
        return TOK_ERROR;
    size_t i = 0;
    for (; lst; lst = cdr(lst)) {
        char encoded[4];
        size_t encoded_len;
        if (!utf8_encode_char((int)CELL_ID(car(lst)), encoded, &encoded_len,
                              name)) {
            free(s);
            return TOK_ERROR;
        }
        memcpy(s + i, encoded, encoded_len);
        i += encoded_len;
    }
    s[i] = '\0';
    return make_string_owned(s);
}

static unsigned make_string_from_chars(unsigned argc, unsigned *argv,
                                       const char *name)
{
    size_t total = 0;
    for (unsigned i = 0; i < argc; i++) {
        if (!IS_CHAR(argv[i])) {
            show_error("%s: argument is not a character", name);
            return TOK_ERROR;
        }
        char encoded[4];
        size_t encoded_len;
        if (!utf8_encode_char((int)CELL_ID(argv[i]), encoded, &encoded_len,
                              name))
            return TOK_ERROR;
        if (encoded_len > SIZE_MAX - total - 1) {
            show_error("%s: result too large", name);
            return TOK_ERROR;
        }
        total += encoded_len;
    }
    char *s = string_buffer_new(total, name, "result");
    if (!s)
        return TOK_ERROR;
    size_t pos = 0;
    for (unsigned i = 0; i < argc; i++) {
        char encoded[4];
        size_t encoded_len;
        if (!utf8_encode_char((int)CELL_ID(argv[i]), encoded, &encoded_len,
                              name)) {
            free(s);
            return TOK_ERROR;
        }
        memcpy(s + pos, encoded, encoded_len);
        pos += encoded_len;
    }
    s[pos] = '\0';
    return make_string_owned(s);
}

static unsigned fill_string_range(unsigned argc, unsigned *argv,
                                  const char *name)
{
    char *s = require_string_ptr(argv[0], name);
    if (!s)
        return TOK_ERROR;
    int c;
    if (!expect_char_value(argv[1], &c, name))
        return TOK_ERROR;
    char encoded[4];
    size_t encoded_len;
    if (!utf8_encode_char(c, encoded, &encoded_len, name))
        return TOK_ERROR;
    size_t char_len;
    if (!utf8_count_chars(s, &char_len, name))
        return TOK_ERROR;
    int64_t start = 0;
    int64_t end = (int64_t)char_len;
    if (argc > 2 && !expect_nonneg_int64(argv[2], &start, name))
        return TOK_ERROR;
    if (argc > 3 && !expect_nonneg_int64(argv[3], &end, name))
        return TOK_ERROR;
    size_t start_byte, end_byte;
    if (!utf8_range_offsets(s, start, end, &start_byte, &end_byte, name))
        return TOK_ERROR;
    size_t fill_count = (size_t)(end - start);
    size_t old_byte_len = strlen(s);
    size_t removed = end_byte - start_byte;
    if (encoded_len != 0 && fill_count > (SIZE_MAX - old_byte_len - 1) / encoded_len) {
        show_error("%s: result too large", name);
        return TOK_ERROR;
    }
    size_t inserted = fill_count * encoded_len;
    if (old_byte_len < removed || inserted > SIZE_MAX - (old_byte_len - removed) - 1) {
        show_error("%s: result too large", name);
        return TOK_ERROR;
    }
    size_t new_len = old_byte_len - removed + inserted;
    char *result = checked_malloc_flex(0, new_len + 1, 1);
    if (!result) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    memcpy(result, s, start_byte);
    char *pos = result + start_byte;
    for (size_t i = 0; i < fill_count; i++) {
        memcpy(pos, encoded, encoded_len);
        pos += encoded_len;
    }
    memcpy(pos, s + end_byte, old_byte_len - end_byte + 1);
    string_unregister(s);
    free(s);
    CELL_PTR(argv[0]) = result;
    string_register(result);
    return 0;
}

static unsigned make_filled_string(unsigned len_arg, unsigned fill_arg,
                                   bool has_fill, const char *name)
{
    int64_t len;
    if (!expect_nonneg_int64(len_arg, &len, name))
        return TOK_ERROR;
    int fill = ' ';
    if (has_fill) {
        if (!expect_char_value(fill_arg, &fill, name))
            return TOK_ERROR;
    }
    char encoded[4];
    size_t encoded_len;
    if (!utf8_encode_char(fill, encoded, &encoded_len, name))
        return TOK_ERROR;
    if ((uint64_t)len > (SIZE_MAX - 1) / encoded_len) {
        show_error("%s: result too large", name);
        return TOK_ERROR;
    }
    size_t total = (size_t)len * encoded_len;
    char *s = string_buffer_new(total, name, "length");
    if (!s)
        return TOK_ERROR;
    char *pos = s;
    for (int64_t i = 0; i < len; i++) {
        memcpy(pos, encoded, encoded_len);
        pos += encoded_len;
    }
    s[total] = '\0';
    return make_string_owned(s);
}

static unsigned make_symbol_from_string(unsigned str, const char *name)
{
    char *s = require_string_ptr(str, name);
    if (!s)
        return TOK_ERROR;
    int atom_id = intern(s);
    unsigned result = alloc();
    CELL_TYPE(result) = BT_ATOM;
    CELL_ID(result) = atom_id;
    return result;
}

static unsigned make_string_from_symbol(unsigned sym, const char *name)
{
    if (!atom_is_valid(sym)) {
        show_error("%s: invalid symbol", name);
        return TOK_ERROR;
    }
    return make_string_copy(ctx.atom_table[CELL_ID(sym)]);
}

static unsigned length_value(unsigned value, const char *name)
{
    if (IS_STRING(value)) {
        char *s = require_string_ptr(value, name);
        if (!s) {
            return TOK_ERROR;
        }
        size_t len;
        if (!utf8_count_chars(s, &len, name))
            return TOK_ERROR;
        return store(len);
    }
    if (IS_VECTOR(value))
        return store(vector_len(value));
    unsigned len = 0;
    if (!list_length_checked(value, &len, name))
        return TOK_ERROR;
    return store(len);
}

static unsigned eq_value(unsigned arg1, unsigned arg2)
{
    if (arg1 == arg2)
        return ctx.atom_true;
    if (IS_FIXNUM(arg1)) {
        return scheme_bool(IS_NUM(arg2) &&
                           CELL_ID(arg2) ==
                               (int64_t)FIXNUM_VALUE(arg1));
    }
    if (IS_FIXNUM(arg2)) {
        return scheme_bool(IS_NUM(arg1) &&
                           CELL_ID(arg1) ==
                               (int64_t)FIXNUM_VALUE(arg2));
    }
    if (!IS_CELL(arg1) || !IS_CELL(arg2))
        return ctx.atom_false;
    if (CELL_TYPE(arg1) != CELL_TYPE(arg2))
        return ctx.atom_false;
    switch (CELL_TYPE(arg1)) {
    case BT_NUM:
    case BT_FUNCTION:
    case BT_BUILTIN:
    case BT_ATOM:
    case BT_CHAR:
        return scheme_bool(CELL_ID(arg1) == CELL_ID(arg2));
    default:
        return scheme_bool(arg1 == arg2);
    }
}

// eqv? extends eq? by comparing numbers of the same type by value (R7RS 6.1)
static unsigned eqv_value(unsigned arg1, unsigned arg2)
{
    if (eq_value(arg1, arg2) == ctx.atom_true)
        return ctx.atom_true;
    if (!IS_CELL(arg1) || !IS_CELL(arg2))
        return ctx.atom_false;
    if (CELL_TYPE(arg1) != CELL_TYPE(arg2))
        return ctx.atom_false;
    switch (CELL_TYPE(arg1)) {
    case BT_BIGNUM: {
        bignum *a = get_bignum(arg1);
        bignum *b = get_bignum(arg2);
        return scheme_bool(a && b && bn_cmp(a, b) == 0);
    }
    case BT_INEXACT:
        // Bit equality: distinguishes 0.0 from -0.0, treats nan as eqv to
        // itself
        return scheme_bool(CELL_ID(arg1) == CELL_ID(arg2));
    case BT_RATIONAL:
    case BT_COMPLEX:
        return scheme_bool(
            eqv_value(CELL_CAR(arg1), CELL_CAR(arg2)) == ctx.atom_true &&
            eqv_value(CELL_CDR(arg1), CELL_CDR(arg2)) == ctx.atom_true);
    default:
        return ctx.atom_false;
    }
}

static unsigned pair_car(unsigned pair, const char *name)
{
    CHECK_PAIR(pair, name);
    return car(pair);
}

static unsigned pair_cdr(unsigned pair, const char *name)
{
    CHECK_PAIR(pair, name);
    return cdr(pair);
}

static unsigned set_pair_car(unsigned pair, unsigned value, const char *name)
{
    CHECK_PAIR(pair, name);
    cell_set_car(pair, value);
    return value;
}

static unsigned set_pair_cdr(unsigned pair, unsigned value, const char *name)
{
    CHECK_PAIR(pair, name);
    cell_set_cdr(pair, value);
    return value;
}

static unsigned make_list_from_argv(unsigned argc, unsigned *argv)
{
    if (argc == 0)
        return 0;
    GC_GUARD;
    unsigned result = 0;
    gc_protect(&result);
    for (unsigned i = argc; i > 0; i--)
        result = alloc_cons(argv[i - 1], result);
    return result;
}

// Returns the last pair of a (possibly improper) list, rejecting cycles
static unsigned last_pair(unsigned lst, const char *name)
{
    if (!IS_PAIR(lst)) {
        show_error("%s: not a pair", name);
        return TOK_ERROR;
    }
    unsigned slow = lst, fast = lst;
    for (;;) {
        unsigned next = cdr(fast);
        if (!IS_PAIR(next))
            return fast;
        fast = next;
        next = cdr(fast);
        if (!IS_PAIR(next))
            return fast;
        fast = next;
        slow = cdr(slow);
        if (fast == slow) {
            show_error("%s: circular list", name);
            return TOK_ERROR;
        }
    }
}

static unsigned string_length_value(unsigned str, const char *name)
{
    char *s = require_string_ptr(str, name);
    if (!s)
        return TOK_ERROR;
    size_t len;
    if (!utf8_count_chars(s, &len, name))
        return TOK_ERROR;
    return store(len);
}

static unsigned string_ref_value(unsigned str, unsigned index,
                                 const char *name)
{
    char *s = require_string_ptr(str, name);
    if (!s)
        return TOK_ERROR;
    int64_t idx;
    if (!expect_nonneg_int64(index, &idx, name))
        return TOK_ERROR;
    size_t byte_offset;
    if (!utf8_byte_offset_for_index(s, (size_t)idx, false, &byte_offset,
                                    name))
        return TOK_ERROR;
    size_t decode_offset = byte_offset;
    int code;
    if (!utf8_decode_next(s, strlen(s), &decode_offset, &code, name))
        return TOK_ERROR;
    return make_char(code);
}

static unsigned string_set_value(unsigned str, unsigned index,
                                 unsigned value, const char *name)
{
    char *s = require_string_ptr(str, name);
    if (!s)
        return TOK_ERROR;
    int64_t idx;
    if (!expect_nonneg_int64(index, &idx, name))
        return TOK_ERROR;
    int c;
    if (!expect_char_value(value, &c, name))
        return TOK_ERROR;
    char encoded[4];
    size_t encoded_len;
    if (!utf8_encode_char(c, encoded, &encoded_len, name))
        return TOK_ERROR;
    size_t start_byte;
    if (!utf8_byte_offset_for_index(s, (size_t)idx, false, &start_byte,
                                    name))
        return TOK_ERROR;
    size_t end_byte = start_byte;
    int old_code;
    size_t old_byte_len = strlen(s);
    if (!utf8_decode_next(s, old_byte_len, &end_byte, &old_code, name))
        return TOK_ERROR;
    size_t removed = end_byte - start_byte;
    if (old_byte_len < removed ||
        encoded_len > SIZE_MAX - (old_byte_len - removed) - 1) {
        show_error("%s: result too large", name);
        return TOK_ERROR;
    }
    size_t new_len = old_byte_len - removed + encoded_len;
    char *result = checked_malloc_flex(0, new_len + 1, 1);
    if (!result) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    memcpy(result, s, start_byte);
    memcpy(result + start_byte, encoded, encoded_len);
    memcpy(result + start_byte + encoded_len, s + end_byte,
           old_byte_len - end_byte + 1);
    string_unregister(s);
    free(s);
    CELL_PTR(str) = result;
    string_register(result);
    return 0;
}

static unsigned string_copy_value(unsigned argc, unsigned *argv,
                                  const char *name)
{
    char *s = require_string_ptr(argv[0], name);
    if (!s)
        return TOK_ERROR;
    size_t char_len;
    if (!utf8_count_chars(s, &char_len, name))
        return TOK_ERROR;
    int64_t start = 0;
    int64_t end = (int64_t)char_len;
    if (argc > 1 && !expect_nonneg_int64(argv[1], &start, name))
        return TOK_ERROR;
    if (argc > 2 && !expect_nonneg_int64(argv[2], &end, name))
        return TOK_ERROR;
    size_t start_byte, end_byte;
    if (!utf8_range_offsets(s, start, end, &start_byte, &end_byte, name))
        return TOK_ERROR;
    char *copy = checked_string_copy_len(s + start_byte,
                                         end_byte - start_byte);
    if (!copy) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    return make_string_owned(copy);
}

static bytevec_data *require_bytevector(unsigned value, const char *name)
{
    if (!IS_BYTEVEC(value)) {
        show_error("%s: not a bytevector", name);
        return NULL;
    }
    bytevec_data *bv = (bytevec_data *)CELL_PTR(value);
    if (!bytevec_data_well_formed(bv)) {
        show_error("%s: invalid bytevector", name);
        return NULL;
    }
    return bv;
}

static bool bytevector_index(unsigned value, const bytevec_data *bv,
                             int64_t *idx, const char *name)
{
    return expect_index(value, bv->len, idx, name);
}

static bool bytevector_range(unsigned argc, unsigned *argv,
                             unsigned start_arg, unsigned end_arg,
                             unsigned default_end, int64_t *start,
                             int64_t *end, const char *name)
{
    *start = 0;
    *end = default_end;
    if (argc > start_arg &&
        !expect_nonneg_int64(argv[start_arg], start, name))
        return false;
    if (argc > end_arg && !expect_nonneg_int64(argv[end_arg], end, name))
        return false;
    if (*start > *end) {
        show_error("%s: invalid range", name);
        return false;
    }
    return true;
}

static bool bytevector_bounded_range(unsigned argc, unsigned *argv,
                                     unsigned start_arg, unsigned end_arg,
                                     unsigned default_end, int64_t *start,
                                     int64_t *end, const char *name,
                                     const char *bounds_error)
{
    if (!bytevector_range(argc, argv, start_arg, end_arg, default_end, start,
                          end, name))
        return false;
    if ((uint64_t)*end > default_end) {
        show_error("%s: %s", name, bounds_error);
        return false;
    }
    return true;
}

static unsigned make_bytevector_from_args(unsigned argc, unsigned *argv,
                                          const char *name)
{
    int64_t len;
    if (!expect_nonneg_int64(argv[0], &len, name))
        return TOK_ERROR;
    if (!bytevector_length_fits((uint64_t)len, name, "length"))
        return TOK_ERROR;
    uint8_t fill = 0;
    if (argc > 1) {
        int64_t f;
        if (!expect_u8(argv[1], &f, name))
            return TOK_ERROR;
        fill = (uint8_t)f;
    }
    return make_bytevector_filled((unsigned)len, fill, name);
}

static unsigned bytevector_ref_value(unsigned bv_arg, unsigned index_arg,
                                     const char *name)
{
    bytevec_data *bv = require_bytevector(bv_arg, name);
    if (!bv)
        return TOK_ERROR;
    int64_t idx;
    if (!bytevector_index(index_arg, bv, &idx, name))
        return TOK_ERROR;
    return store(bv->data[idx]);
}

static unsigned bytevector_set_value(unsigned bv_arg, unsigned index_arg,
                                     unsigned value_arg, const char *name)
{
    bytevec_data *bv = require_bytevector(bv_arg, name);
    if (!bv)
        return TOK_ERROR;
    int64_t idx, val;
    if (!bytevector_index(index_arg, bv, &idx, name) ||
        !expect_u8(value_arg, &val, name))
        return TOK_ERROR;
    bv->data[idx] = (uint8_t)val;
    return 0;
}

static unsigned bytevector_length_value(unsigned bv_arg, const char *name)
{
    bytevec_data *bv = require_bytevector(bv_arg, name);
    if (!bv)
        return TOK_ERROR;
    return store(bv->len);
}

static unsigned bytevector_copy_value(unsigned argc, unsigned *argv,
                                      const char *name)
{
    bytevec_data *src = require_bytevector(argv[0], name);
    if (!src)
        return TOK_ERROR;
    int64_t start, end;
    if (!bytevector_bounded_range(argc, argv, 1, 2, src->len, &start,
                                  &end, name, "invalid range"))
        return TOK_ERROR;
    unsigned len = (unsigned)(end - start);
    return make_bytevector_copy(src->data + start, len, name);
}

static unsigned bytevector_copy_to(unsigned argc, unsigned *argv,
                                   const char *name)
{
    bytevec_data *dst = require_bytevector(argv[0], name);
    if (!dst)
        return TOK_ERROR;
    bytevec_data *src = require_bytevector(argv[2], name);
    if (!src)
        return TOK_ERROR;
    int64_t at, start, end;
    if (!expect_nonneg_int64(argv[1], &at, name))
        return TOK_ERROR;
    if (!bytevector_bounded_range(argc, argv, 3, 4, src->len, &start,
                                  &end, name, "out of bounds"))
        return TOK_ERROR;
    unsigned len = (unsigned)(end - start);
    if ((uint64_t)at + len > dst->len) {
        show_error("%s: out of bounds", name);
        return TOK_ERROR;
    }
    memmove(dst->data + at, src->data + start, len);
    return 0;
}

static unsigned string_to_utf8_value(unsigned argc, unsigned *argv,
                                     const char *name)
{
    char *s = require_string_ptr(argv[0], name);
    if (!s)
        return TOK_ERROR;
    int64_t start = 0;
    int64_t end = 0;
    if (argc > 1 && !expect_nonneg_int64(argv[1], &start, name))
        return TOK_ERROR;
    if (argc > 2 && !expect_nonneg_int64(argv[2], &end, name))
        return TOK_ERROR;
    if (argc <= 2) {
        size_t char_len;
        if (!utf8_count_chars(s, &char_len, name))
            return TOK_ERROR;
        end = (int64_t)char_len;
    }
    size_t start_byte, end_byte;
    if (!utf8_range_offsets(s, start, end, &start_byte, &end_byte, name))
        return TOK_ERROR;
    if (end_byte - start_byte > UINT_MAX) {
        show_error("%s: result too large", name);
        return TOK_ERROR;
    }
    return make_bytevector_copy((const uint8_t *)s + start_byte,
                                (unsigned)(end_byte - start_byte), name);
}

static unsigned utf8_to_string_value(unsigned argc, unsigned *argv,
                                     const char *name)
{
    bytevec_data *bv = require_bytevector(argv[0], name);
    if (!bv)
        return TOK_ERROR;
    int64_t start, end;
    if (!bytevector_bounded_range(argc, argv, 1, 2, bv->len, &start,
                                  &end, name, "invalid range"))
        return TOK_ERROR;
    size_t len = (size_t)(end - start);
    const char *bytes = (const char *)bv->data + start;
    size_t offset = 0;
    int code;
    while (offset < len) {
        if (!utf8_decode_next(bytes, len, &offset, &code, name))
            return TOK_ERROR;
        if (code == 0) {
            show_error("%s: null character cannot be stored in a string",
                       name);
            return TOK_ERROR;
        }
    }
    char *copy = checked_string_copy_len(bytes, len);
    if (!copy) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    return make_string_owned(copy);
}

static unsigned apply_bytevector_primitive(unsigned prim_id, unsigned argc,
                                           unsigned *argv)
{
    switch (prim_id) {
    case PMAKEBYTEVEC:
        REQUIRE_ARGC(argc, 1, 2, "make-bytevector");
        return make_bytevector_from_args(argc, argv, "make-bytevector");
    case PBYTEVECREF:
        REQUIRE_ARGC(argc, 2, 2, "bytevector-u8-ref");
        return bytevector_ref_value(argv[0], argv[1],
                                    "bytevector-u8-ref");
    case PBYTEVECSET:
        REQUIRE_ARGC(argc, 3, 3, "bytevector-u8-set!");
        return bytevector_set_value(argv[0], argv[1], argv[2],
                                    "bytevector-u8-set!");
    case PBYTEVECLEN:
        REQUIRE_ARGC(argc, 1, 1, "bytevector-length");
        return bytevector_length_value(argv[0], "bytevector-length");
    case PBYTEVECUP:
        REQUIRE_ARGC(argc, 1, 1, "bytevector?");
        return scheme_bool(IS_BYTEVEC(argv[0]));
    case PBYTEVEC:
        return make_bytevector_from_u8_argv(argc, argv, "bytevector");
    case PBYTEVECCOPY:
        REQUIRE_ARGC(argc, 1, 3, "bytevector-copy");
        return bytevector_copy_value(argc, argv, "bytevector-copy");
    case PBYTEVECCOPYTO:
        REQUIRE_ARGC(argc, 3, 5, "bytevector-copy!");
        return bytevector_copy_to(argc, argv, "bytevector-copy!");
    case PBYTEVECAPPEND:
        return make_bytevector_append(argc, argv, "bytevector-append");
    default:
        show_error("bytevector primitive: unsupported id %u", prim_id);
        return TOK_ERROR;
    }
}

static char *object_to_string(unsigned value, const char *name)
{
    char *buf = NULL;
    size_t buf_len = 0;
    FILE *mem = open_memstream(&buf, &buf_len);
    if (!mem) {
        show_error("%s: out of memory", name);
        return NULL;
    }
    write_obj_port(value, mem);
    if (fclose(mem) != 0) {
        free(buf);
        show_error("%s: write failed", name);
        return NULL;
    }
    return buf;
}

typedef enum {
    BITWISE_AND,
    BITWISE_IOR,
    BITWISE_XOR,
} bitwise_op;

static unsigned bitwise_fold(unsigned argc, unsigned *argv, const char *name,
                             bitwise_op op)
{
    int64_t result;
    if (!expect_exact_int64(argv[0], &result, name))
        return TOK_ERROR;
    for (unsigned i = 1; i < argc; i++) {
        int64_t b;
        if (!expect_exact_int64(argv[i], &b, name))
            return TOK_ERROR;
        switch (op) {
        case BITWISE_AND:
            result &= b;
            break;
        case BITWISE_IOR:
            result |= b;
            break;
        case BITWISE_XOR:
            result ^= b;
            break;
        }
    }
    return store(result);
}

static unsigned bitwise_not_value(unsigned value, const char *name)
{
    int64_t a;
    if (!expect_exact_int64(value, &a, name))
        return TOK_ERROR;
    return store(~a);
}

static unsigned arithmetic_shift_value(unsigned value_arg,
                                       unsigned count_arg,
                                       const char *name)
{
    int64_t val, count;
    if (!expect_exact_int64(value_arg, &val, name) ||
        !expect_exact_int64(count_arg, &count, name))
        return TOK_ERROR;
    if (count >= 0) {
        if ((uint64_t)count > SIZE_MAX) {
            show_error("%s: count too large", name);
            return TOK_ERROR;
        }
        bignum *bn = bn_from_int(val);
        bignum *shifted = bn ? bn_lshift(bn, (size_t)count) : NULL;
        bn_free(bn);
        if (!shifted) {
            show_error("%s: out of memory", name);
            return TOK_ERROR;
        }
        return store_integer(shifted);
    } else {
        uint64_t shift = -(uint64_t)count;
        if (shift >= 63)
            return store(val < 0 ? -1 : 0);
        return store(val >> shift);
    }
}

static unsigned number_to_string_value(unsigned num, int radix,
                                       const char *name)
{
    if (IS_FIXNUM(num) || IS_NUM(num) || IS_BIGNUM(num)) {
        char *s = exact_integer_to_string(num, radix);
        if (!s) {
            show_error("%s: %s", name,
                       IS_BIGNUM(num) && !get_bignum(num)
                           ? "invalid bignum"
                           : "out of memory");
            return TOK_ERROR;
        }
        return make_string_owned(s);
    } else if (IS_RATIONAL(num)) {
        bool too_large;
        char *s = rational_to_string(num, radix, &too_large);
        if (!s) {
            show_error("%s: %s", name,
                       too_large ? "result too large" : "out of memory");
            return TOK_ERROR;
        }
        return make_string_owned(s);
    } else if (IS_INEXACT(num)) {
        if (radix != 10) {
            show_error("%s: inexact numbers require radix 10", name);
            return TOK_ERROR;
        }
        double d = to_double(num);
        char buf[NUMBER_BUF_SIZE];
        format_double_repr(buf, sizeof(buf), d);
        return make_string_copy(buf);
    } else if (IS_COMPLEX(num)) {
        if (radix != 10) {
            show_error("%s: complex numbers require radix 10", name);
            return TOK_ERROR;
        }
        char *buf = object_to_string(num, name);
        if (!buf)
            return TOK_ERROR;
        return make_string_owned(buf);
    } else {
        show_error("%s: not a number", name);
        return TOK_ERROR;
    }
}

// Parse an integer or num/den rational in the given radix. Returns
// ctx.atom_false if the string is not valid in that radix.
static unsigned parse_radix_number(const char *s, int radix, const char *name)
{
    const char *slash = strchr(s, '/');
    if (!slash) {
        bignum *bn = bn_from_string(s, radix);
        if (!bn)
            return ctx.atom_false;
        return store_integer(bn);
    }

    // R7RS rational syntax: sign only on the numerator, denominator unsigned
    if (slash[1] == '+' || slash[1] == '-')
        return ctx.atom_false;
    char *num_str = checked_string_copy_len(s, (size_t)(slash - s));
    if (!num_str) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    bignum *num = bn_from_string(num_str, radix);
    free(num_str);
    bignum *den = num ? bn_from_string(slash + 1, radix) : NULL;
    if (!num || !den || bn_is_zero(den)) {
        bn_free(num);
        bn_free(den);
        return ctx.atom_false;
    }
    GC_GUARD;
    unsigned num_cell = store_integer(num);
    gc_protect(&num_cell);
    unsigned den_cell = store_integer(den);
    if (num_cell == TOK_ERROR || den_cell == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&den_cell);
    return normalize_rational_cells(num_cell, den_cell);
}

static unsigned string_to_number_value(unsigned str, int radix,
                                       const char *name)
{
    char *s = require_string_ptr(str, name);
    if (!s)
        return TOK_ERROR;

    // R7RS number prefixes: one radix (#b #o #d #x) and one exactness
    // (#e #i) marker, in either order, overriding the radix argument
    int exactness = 0; // 1 = exact, -1 = inexact, 0 = as written
    bool have_radix = false, have_exact = false;
    while (s[0] == '#') {
        char p = s[1];
        if (!have_radix && (p == 'b' || p == 'B')) {
            radix = 2;
            have_radix = true;
        } else if (!have_radix && (p == 'o' || p == 'O')) {
            radix = 8;
            have_radix = true;
        } else if (!have_radix && (p == 'd' || p == 'D')) {
            radix = 10;
            have_radix = true;
        } else if (!have_radix && (p == 'x' || p == 'X')) {
            radix = 16;
            have_radix = true;
        } else if (!have_exact && (p == 'e' || p == 'E')) {
            exactness = 1;
            have_exact = true;
        } else if (!have_exact && (p == 'i' || p == 'I')) {
            exactness = -1;
            have_exact = true;
        } else {
            return ctx.atom_false;
        }
        s += 2;
    }

    GC_GUARD;
    unsigned parsed;
    if (radix == 10) {
        char *copy = checked_string_copy(s);
        if (!copy) {
            show_error("%s: out of memory", name);
            return TOK_ERROR;
        }
        parsed = TOK_ERROR;
        if (exactness == 1) {
            // #e1.5 must yield 3/2 exactly, not a converted double
            bool handled = false;
            parsed = read_exact_decimal_number(copy, &handled);
            if (!handled)
                parsed = TOK_ERROR;
        }
        if (parsed == TOK_ERROR)
            parsed = atom_from_string(copy);
        free(copy);
        if (parsed == TOK_ERROR)
            return TOK_ERROR;
        if (!is_numeric(parsed))
            return ctx.atom_false;
    } else {
        parsed = parse_radix_number(s, radix, name);
        if (parsed == TOK_ERROR || parsed == ctx.atom_false)
            return parsed;
    }

    gc_protect(&parsed);
    if (exactness == -1 && is_exact(parsed) && !IS_COMPLEX(parsed))
        return store_inexact(to_double(parsed));
    if (exactness == 1 && IS_INEXACT(parsed)) {
        double d = to_double(parsed);
        if (isnan(d) || isinf(d))
            return ctx.atom_false; // no exact representation
        return prim_inexact_to_exact(parsed);
    }
    return parsed;
}

static unsigned apply_arithmetic_primitive(unsigned prim_id, unsigned argc,
                                           unsigned *argv)
{
    switch (prim_id) {
    case PPLUS:
        return prim_plus(argc, argv);
    case PMINUS:
        return prim_minus(argc, argv);
    case PTIMES:
        return prim_mult(argc, argv);
    case PDIV:
        return prim_div(argc, argv);
    case PMOD:
        return prim_divlike_inexact(prim_modulo, argc, argv, "modulo");
    case PREMAINDER:
        return prim_divlike_inexact(prim_remainder, argc, argv, "remainder");
    case PTRUNCATEDIVREM:
        return prim_divlike_inexact(prim_truncate_divrem, argc, argv,
                                    "truncate/");
    case PFLOORDIVREM:
        return prim_divlike_inexact(prim_floor_divrem, argc, argv, "floor/");
    case PQUOTIENT:
        return prim_divlike_inexact(prim_quotient, argc, argv, "quotient");
    case PABS:
        return prim_abs(argc, argv);
    default:
        show_error("arithmetic primitive: unsupported id %u", prim_id);
        return TOK_ERROR;
    }
}

static unsigned apply_numeric_compare_primitive(unsigned prim_id,
                                                unsigned argc,
                                                unsigned *argv)
{
    switch (prim_id) {
    case PEQUAL:
        return numeric_compare(argc, argv, CMP_EQ);
    case PLT:
        return numeric_compare(argc, argv, CMP_LT);
    case PGT:
        return numeric_compare(argc, argv, CMP_GT);
    case PLEQ:
        return numeric_compare(argc, argv, CMP_LE);
    case PGEQ:
        return numeric_compare(argc, argv, CMP_GE);
    default:
        show_error("numeric comparison primitive: unsupported id %u",
                   prim_id);
        return TOK_ERROR;
    }
}

static unsigned apply_logic_primitive(unsigned prim_id, unsigned argc,
                                      unsigned *argv)
{
    switch (prim_id) {
    case PNOT:
        REQUIRE_ARGC(argc, 1, 1, "not");
        return scheme_bool(IS_FALSE(argv[0]));
    case PEQ:
        REQUIRE_ARGC(argc, 2, 2, "eq?");
        return eq_value(argv[0], argv[1]);
    case PEQV:
        REQUIRE_ARGC(argc, 2, 2, "eqv?");
        return eqv_value(argv[0], argv[1]);
    case PEQUALP:
        REQUIRE_ARGC(argc, 2, 2, "equal?");
        return scheme_bool(deep_equal(argv[0], argv[1]));
    default:
        show_error("logic primitive: unsupported id %u", prim_id);
        return TOK_ERROR;
    }
}

static unsigned apply_list_primitive(unsigned prim_id, unsigned argc,
                                     unsigned *argv)
{
    switch (prim_id) {
    case PCONS:
        REQUIRE_ARGC(argc, 2, 2, "cons");
        return alloc_cons(argv[0], argv[1]);
    case PCAR:
        REQUIRE_ARGC(argc, 1, 1, "car");
        return pair_car(argv[0], "car");
    case PCDR:
        REQUIRE_ARGC(argc, 1, 1, "cdr");
        return pair_cdr(argv[0], "cdr");
    case PSETCAR:
        REQUIRE_ARGC(argc, 2, 2, "set-car!");
        return set_pair_car(argv[0], argv[1], "set-car!");
    case PSETCDR:
        REQUIRE_ARGC(argc, 2, 2, "set-cdr!");
        return set_pair_cdr(argv[0], argv[1], "set-cdr!");
    case PLIST:
        return make_list_from_argv(argc, argv);
    case PLENGTH:
        REQUIRE_ARGC(argc, 1, 1, "length");
        return length_value(argv[0], "length");
    case PAPPEND:
        return prim_append(argc, argv);
    case PREVERSE:
        return prim_reverse(argc, argv);
    case PLASTPAIR:
        REQUIRE_ARGC(argc, 1, 1, "last-pair");
        return last_pair(argv[0], "last-pair");
    default:
        show_error("list primitive: unsupported id %u", prim_id);
        return TOK_ERROR;
    }
}

static unsigned apply_string_primitive(unsigned prim_id, unsigned argc,
                                       unsigned *argv)
{
    switch (prim_id) {
    case PSTRLEN:
        REQUIRE_ARGC(argc, 1, 1, "string-length");
        return string_length_value(argv[0], "string-length");
    case PSTRREF:
        REQUIRE_ARGC(argc, 2, 2, "string-ref");
        return string_ref_value(argv[0], argv[1], "string-ref");
    case PSTRSET:
        REQUIRE_ARGC(argc, 3, 3, "string-set!");
        return string_set_value(argv[0], argv[1], argv[2], "string-set!");
    case PSTRAPP:
        return prim_string_append(argc, argv);
    case PSUBSTR:
        return prim_substring(argc, argv);
    case PSTR2SYM:
        REQUIRE_ARGC(argc, 1, 1, "string->symbol");
        return make_symbol_from_string(argv[0], "string->symbol");
    case PSYM2STR:
        REQUIRE_ARGC(argc, 1, 1, "symbol->string");
        return make_string_from_symbol(argv[0], "symbol->string");
    case PNUM2STR: {
        REQUIRE_ARGC(argc, 1, 2, "number->string");
        int radix;
        if (!parse_optional_radix(argc, argv, "number->string", &radix))
            return TOK_ERROR;
        return number_to_string_value(argv[0], radix, "number->string");
    }
    case PSTR2NUM: {
        REQUIRE_ARGC(argc, 1, 2, "string->number");
        int radix;
        if (!parse_optional_radix(argc, argv, "string->number", &radix))
            return TOK_ERROR;
        return string_to_number_value(argv[0], radix, "string->number");
    }
    case PMAKESTR:
        REQUIRE_ARGC(argc, 1, 2, "make-string");
        return make_filled_string(argv[0], argc > 1 ? argv[1] : 0,
                                  argc > 1, "make-string");
    case PSTRCOPY:
        REQUIRE_ARGC(argc, 1, 3, "string-copy");
        return string_copy_value(argc, argv, "string-copy");
    case PSTR2LIST:
        REQUIRE_ARGC(argc, 1, 3, "string->list");
        return make_char_list_from_string_range(argc, argv, "string->list");
    case PLIST2STR:
        REQUIRE_ARGC(argc, 1, 1, "list->string");
        return make_string_from_char_list(argv[0], "list->string");
    case PSTRFILL:
        REQUIRE_ARGC(argc, 2, 4, "string-fill!");
        return fill_string_range(argc, argv, "string-fill!");
    case PSTRNFD:
    case PSTRNFC:
    case PSTRNFKD:
    case PSTRNFKC:
    case PSTRFOLD:
    case PSTRUP:
    case PSTRDOWN:
    case PSTRTITLE:
        return prim_string_normalize(prim_id, argc, argv);
    case PSTRING:
        return make_string_from_chars(argc, argv, "string");
    case PSTRINGTOUTF8:
        REQUIRE_ARGC(argc, 1, 3, "string->utf8");
        return string_to_utf8_value(argc, argv, "string->utf8");
    case PUTF8TOSTRING:
        REQUIRE_ARGC(argc, 1, 3, "utf8->string");
        return utf8_to_string_value(argc, argv, "utf8->string");
    case PSTREQ:
    case PSTRLT:
    case PSTRGT:
    case PSTRLE:
    case PSTRGE:
    case PSTREQI:
    case PSTRLTI:
    case PSTRGTI:
    case PSTRLEI:
    case PSTRGEI: {
        unsigned offset = prim_id - PSTREQ;
        return string_compare(argc, argv, (cmp_op)(offset % 5), offset >= 5);
    }
    default:
        show_error("string primitive: unsupported id %u", prim_id);
        return TOK_ERROR;
    }
}

static unsigned apply_bitwise_primitive(unsigned prim_id, unsigned argc,
                                        unsigned *argv)
{
    switch (prim_id) {
    case PBITWISEAND:
        REQUIRE_ARGC(argc, 1, 999, "bitwise-and");
        return bitwise_fold(argc, argv, "bitwise-and", BITWISE_AND);
    case PBITWISEIOR:
        REQUIRE_ARGC(argc, 1, 999, "bitwise-ior");
        return bitwise_fold(argc, argv, "bitwise-ior", BITWISE_IOR);
    case PBITWISEXOR:
        REQUIRE_ARGC(argc, 1, 999, "bitwise-xor");
        return bitwise_fold(argc, argv, "bitwise-xor", BITWISE_XOR);
    case PBITWISENOT:
        REQUIRE_ARGC(argc, 1, 1, "bitwise-not");
        return bitwise_not_value(argv[0], "bitwise-not");
    case PARITHSHIFT:
        REQUIRE_ARGC(argc, 2, 2, "arithmetic-shift");
        return arithmetic_shift_value(argv[0], argv[1], "arithmetic-shift");
    default:
        show_error("bitwise primitive: unsupported id %u", prim_id);
        return TOK_ERROR;
    }
}

static unsigned current_second_value(void)
{
    time_t now = time(NULL);
    if (now == (time_t)-1) {
        show_error("current-second: time failed");
        return TOK_ERROR;
    }
    return store_inexact((double)now);
}

static unsigned current_jiffy_value(void)
{
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) {
        show_error("current-jiffy: clock_gettime failed");
        return TOK_ERROR;
    }
    if (ts.tv_nsec < 0 || ts.tv_nsec >= JIFFIES_PER_SECOND ||
        ts.tv_sec > INT64_MAX / JIFFIES_PER_SECOND ||
        (ts.tv_sec == INT64_MAX / JIFFIES_PER_SECOND &&
         ts.tv_nsec > INT64_MAX % JIFFIES_PER_SECOND) ||
        ts.tv_sec < INT64_MIN / JIFFIES_PER_SECOND) {
        show_error("current-jiffy: time out of range");
        return TOK_ERROR;
    }
    return store((int64_t)ts.tv_sec * JIFFIES_PER_SECOND + ts.tv_nsec);
}

static unsigned list_ref_value(unsigned lst, unsigned index_arg,
                               const char *name)
{
    int64_t idx;
    if (!expect_nonneg_int64(index_arg, &idx, name))
        return TOK_ERROR;
    unsigned cell = list_cell_at(lst, idx, name);
    if (cell == TOK_ERROR)
        return TOK_ERROR;
    return car(cell);
}

static void prepend_stat_entry(unsigned *result, const char *key_name,
                               unsigned value)
{
    unsigned key = atom_from_string(key_name);
    gc_protect(&key);
    unsigned entry = alloc_cons(key, value);
    gc_protect(&entry);
    *result = alloc_cons(entry, *result);
    gc_unprotect(2);
}

static unsigned make_gc_stats_list(void)
{
    GC_GUARD;
    unsigned minor = store(ctx.minor_gc_count);
    gc_protect(&minor);
    unsigned major = store(ctx.major_gc_count);
    gc_protect(&major);
    unsigned heap_used = store(ctx.hptr - ctx.mmin);
    gc_protect(&heap_used);
    unsigned nursery_used = store(ctx.nursery_ptr - ctx.nursery_start);
    gc_protect(&nursery_used);
    unsigned result = 0;
    gc_protect(&result);

    prepend_stat_entry(&result, "nursery", nursery_used);
    prepend_stat_entry(&result, "old-gen", heap_used);
    prepend_stat_entry(&result, "major-gc", major);
    prepend_stat_entry(&result, "minor-gc", minor);

    return result;
}

static bool expect_r5rs_environment_version(unsigned value, const char *name)
{
    int64_t version;
    if (!expect_exact_int64(value, &version, name))
        return false;
    if (version != 5) {
        show_error("%s: unsupported version %lld", name,
                   (long long)version);
        return false;
    }
    return true;
}

static unsigned scheme_environment(unsigned version_arg, const char *name)
{
    if (!expect_r5rs_environment_version(version_arg, name))
        return TOK_ERROR;
    return default_environment();
}

static unsigned null_environment(unsigned version_arg, const char *name)
{
    if (!expect_r5rs_environment_version(version_arg, name))
        return TOK_ERROR;
    return empty_environment();
}

static unsigned make_command_line_list(void)
{
    GC_GUARD;
    unsigned result = 0;
    gc_protect(&result);
    for (int i = saved_argc - 1; i >= 0; i--) {
        unsigned s = make_string_copy(saved_argv[i]);
        if (s == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&s);
        result = alloc_cons(s, result);
        gc_unprotect(1);
    }
    return result;
}

static unsigned file_exists_p(unsigned filename_arg, const char *name)
{
    char *filename = require_string_ptr(filename_arg, name);
    if (!filename)
        return TOK_ERROR;
    struct stat st;
    return scheme_bool(stat(filename, &st) == 0);
}

static unsigned file_type_p(unsigned filename_arg, const char *name, int type)
{
    char *filename = require_string_ptr(filename_arg, name);
    if (!filename)
        return TOK_ERROR;
    struct stat st;
    if (stat(filename, &st) != 0)
        return ctx.atom_false;
    if (type == PFILEREGULARP)
        return scheme_bool(S_ISREG(st.st_mode));
    return scheme_bool(S_ISDIR(st.st_mode));
}

static unsigned delete_file_value(unsigned filename_arg, const char *name)
{
    char *filename = require_string_ptr(filename_arg, name);
    if (!filename)
        return TOK_ERROR;
    if (remove(filename) != 0) {
        show_error("%s: cannot delete %s", name, filename);
        return TOK_ERROR;
    }
    return 0;
}

static unsigned rename_file_value(unsigned old_arg, unsigned new_arg,
                                  const char *name)
{
    char *old_name = require_string_ptr(old_arg, name);
    char *new_name = require_string_ptr(new_arg, name);
    if (!old_name || !new_name)
        return TOK_ERROR;
    if (rename(old_name, new_name) != 0) {
        show_error("%s: cannot rename file", name);
        return TOK_ERROR;
    }
    return 0;
}

static unsigned make_directory_value(unsigned dirname_arg, const char *name)
{
    char *dirname = require_string_ptr(dirname_arg, name);
    if (!dirname)
        return TOK_ERROR;
    if (mkdir(dirname, 0777) != 0) {
        show_error("%s: cannot create directory %s", name, dirname);
        return TOK_ERROR;
    }
    return 0;
}

static unsigned delete_directory_value(unsigned dirname_arg, const char *name)
{
    char *dirname = require_string_ptr(dirname_arg, name);
    if (!dirname)
        return TOK_ERROR;
    if (rmdir(dirname) != 0) {
        show_error("%s: cannot delete directory %s", name, dirname);
        return TOK_ERROR;
    }
    return 0;
}

static unsigned temporary_file_path_value(void)
{
    char tmpl[] = "/tmp/vesper-XXXXXX";
    int fd = mkstemp(tmpl);
    if (fd < 0) {
        show_error("temporary-file-path: cannot create temporary path");
        return TOK_ERROR;
    }
    int close_result = close(fd);
    int unlink_result = unlink(tmpl);
    if (close_result != 0 || unlink_result != 0) {
        show_error("temporary-file-path: cannot prepare temporary path");
        return TOK_ERROR;
    }
    return make_string_copy(tmpl);
}

static unsigned current_directory_value(unsigned argc, unsigned *argv,
                                        const char *name)
{
    if (argc == 0) {
        size_t cap = 256;
        for (;;) {
            char *cwd = checked_malloc_size(cap);
            if (!cwd) {
                show_error("%s: out of memory", name);
                return TOK_ERROR;
            }
            if (getcwd(cwd, cap))
                return make_string_owned(cwd);
            int err = errno;
            free(cwd);
            if (err != ERANGE) {
                show_error("%s: cannot read current directory", name);
                return TOK_ERROR;
            }
            size_t new_cap;
            if (!checked_grow_capacity_size(cap, 1, &new_cap)) {
                show_error("%s: path too long", name);
                return TOK_ERROR;
            }
            cap = new_cap;
        }
    }

    char *path = require_string_ptr(argv[0], name);
    if (!path)
        return TOK_ERROR;
    if (chdir(path) != 0) {
        show_error("%s: cannot change directory to %s", name, path);
        return TOK_ERROR;
    }
    return 0;
}

static unsigned directory_files_value(unsigned dirname_arg, const char *name)
{
    char *dirname = require_string_ptr(dirname_arg, name);
    if (!dirname)
        return TOK_ERROR;
    DIR *dir = opendir(dirname);
    if (!dir) {
        show_error("%s: cannot open directory %s", name, dirname);
        return TOK_ERROR;
    }

    GC_GUARD;
    unsigned result = 0;
    gc_protect(&result);
    errno = 0;
    for (struct dirent *ent = readdir(dir); ent; ent = readdir(dir)) {
        unsigned s = make_string_copy(ent->d_name);
        if (s == TOK_ERROR) {
            closedir(dir);
            return TOK_ERROR;
        }
        gc_protect(&s);
        result = alloc_cons(s, result);
        gc_unprotect(1);
        errno = 0;
    }
    if (errno != 0) {
        closedir(dir);
        show_error("%s: directory read failed", name);
        return TOK_ERROR;
    }
    if (closedir(dir) != 0) {
        show_error("%s: directory close failed", name);
        return TOK_ERROR;
    }
    return result;
}

static unsigned getenv_value(unsigned name_arg, const char *proc_name)
{
    char *var = require_string_ptr(name_arg, proc_name);
    if (!var)
        return TOK_ERROR;
    const char *value = getenv(var);
    if (!value)
        return ctx.atom_false;
    return make_string_copy(value);
}

static unsigned get_environment_variables_value(void)
{
    GC_GUARD;
    unsigned result = 0;
    gc_protect(&result);
    for (char **entry = environ; entry && *entry; entry++) {
        char *equals = strchr(*entry, '=');
        if (!equals)
            continue;
        unsigned name =
            make_string_owned(checked_string_copy_len(*entry, equals - *entry));
        if (name == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&name);
        unsigned value = make_string_copy(equals + 1);
        if (value == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&value);
        unsigned pair = alloc_cons(name, value);
        gc_protect(&pair);
        result = alloc_cons(pair, result);
        gc_unprotect(3);
    }
    return result;
}

static unsigned write_to_string_value(unsigned value, const char *name)
{
    char *buf = object_to_string(value, name);
    if (!buf)
        return TOK_ERROR;
    return make_string_owned(buf);
}

static unsigned open_binary_input_file(unsigned filename_arg,
                                       const char *name)
{
    const char *fname = require_string_ptr(filename_arg, name);
    if (!fname)
        return TOK_ERROR;
    return open_file_port(fname, "rb", BT_INPORT, name);
}

static unsigned open_binary_output_file(unsigned filename_arg,
                                        const char *name)
{
    const char *fname = require_string_ptr(filename_arg, name);
    if (!fname)
        return TOK_ERROR;
    return open_file_port(fname, "wb", BT_OUTPORT, name);
}

static unsigned write_bytevector_to_port(unsigned bv_arg, unsigned port_arg,
                                         const char *name)
{
    bytevec_data *bv = require_bytevector(bv_arg, name);
    if (!bv)
        return TOK_ERROR;
    if (!IS_OUTPORT(port_arg)) {
        show_error("%s: not an output port", name);
        return TOK_ERROR;
    }
    FILE *f = file_port_file(port_arg);
    if (!f) {
        show_error("%s: port is closed", name);
        return TOK_ERROR;
    }
    if (bv->len > 0 && fwrite(bv->data, 1, bv->len, f) != bv->len) {
        show_error("%s: write failed", name);
        return TOK_ERROR;
    }
    return 0;
}

static unsigned read_bytevector_into(unsigned argc, unsigned *argv,
                                     const char *name)
{
    unsigned bv_arg = argv[0];
    unsigned port_arg = argv[1];
    bytevec_data *bv = require_bytevector(bv_arg, name);
    if (!bv)
        return TOK_ERROR;
    if (!IS_INPORT(port_arg)) {
        show_error("%s: not an input port", name);
        return TOK_ERROR;
    }
    FILE *f = file_port_file(port_arg);
    if (!f) {
        show_error("%s: port is closed", name);
        return TOK_ERROR;
    }
    int64_t start, end;
    if (!bytevector_bounded_range(argc, argv, 2, 3, bv->len, &start,
                                  &end, name, "out of bounds"))
        return TOK_ERROR;
    size_t len = (size_t)(end - start);
    if (len == 0)
        return store(0);
    size_t n = 0;
    while (n < len && reader_port_pending_bytes(f) > 0) {
        int c = reader_port_getc(f);
        if (c == EOF)
            break;
        bv->data[start + n++] = (uint8_t)c;
    }
    if (n < len)
        n += fread(bv->data + start + n, 1, len - n, f);
    if (ferror(f)) {
        show_error("%s: read failed", name);
        return TOK_ERROR;
    }
    if (n == 0)
        return CELL_EOF_OBJECT;
    return store((int64_t)n);
}

static unsigned make_features_list(void)
{
    GC_GUARD;
    unsigned result = 0;
    gc_protect(&result);
    const char *const *features = feature_names();
    size_t feature_count = feature_name_count();
    for (size_t i = 0; i < feature_count; i++) {
        unsigned sym = atom_from_string(features[i]);
        gc_protect(&sym);
        result = alloc_cons(sym, result);
        gc_unprotect(1);
    }
    return result;
}

static uint64_t hash_string_bytes(const char *s)
{
    uint64_t h = PRIM_FNV_OFFSET_BASIS;
    for (const unsigned char *p = (const unsigned char *)s; *p; p++) {
        h ^= *p;
        h *= PRIM_FNV_PRIME;
    }
    return h;
}

static uint64_t scheme_hash(unsigned key)
{
    if (IS_FIXNUM(key))
        return (uint64_t)FIXNUM_VALUE(key) * PRIM_FNV_PRIME;
    if (!IS_CELL(key))
        return key;
    switch (CELL_TYPE(key)) {
    case BT_ATOM:
        return atom_is_valid(key)
                   ? hash_string_bytes(ctx.atom_table[CELL_ID(key)])
                   : PRIM_FNV_OFFSET_BASIS;
    case BT_NUM:
    case BT_CHAR:
        return (uint64_t)CELL_ID(key) * PRIM_FNV_PRIME;
    case BT_STRING: {
        char *s = GET_STRING_PTR(key);
        return string_is_registered(s) ? hash_string_bytes(s)
                                       : PRIM_FNV_OFFSET_BASIS;
    }
    default:
        return (uint64_t)key * PRIM_FNV_PRIME;
    }
}

static uint64_t hash_combine(uint64_t a, uint64_t b)
{
    return (a ^ (b + 0x9e3779b97f4a7c15ull + (a << 6) + (a >> 2)));
}

#define HASH_RECURSION_MARKER 0x4f1bbcdc9a1322d5ull
#define HASH_SEEN_STACK_MAX 1024

static bool hash_seen_contains(const unsigned *seen, unsigned seen_len,
                               unsigned key)
{
    for (unsigned i = 0; i < seen_len; i++) {
        if (seen[i] == key)
            return true;
    }
    return false;
}

static uint64_t hash_key_for_table_seen(hash_table_data *ht, unsigned key,
                                        unsigned *seen, unsigned seen_len)
{
    if (IS_FIXNUM(key) || !IS_CELL(key))
        return scheme_hash(key);

    enum lisp_type key_type = CELL_TYPE(key);
    bool track_path = key_type == BT_CONS || key_type == BT_VECTOR ||
                      key_type == BT_RATIONAL || key_type == BT_COMPLEX;
    if (track_path) {
        if (hash_seen_contains(seen, seen_len, key))
            return HASH_RECURSION_MARKER;
        if (seen_len >= HASH_SEEN_STACK_MAX)
            return HASH_RECURSION_MARKER;
        seen[seen_len++] = key;
    }

    switch (key_type) {
    case BT_RATIONAL:
    case BT_COMPLEX:
    case BT_CONS:
        return hash_combine(hash_key_for_table_seen(ht, car(key), seen,
                                                    seen_len),
                            hash_key_for_table_seen(ht, cdr(key), seen,
                                                    seen_len));
    case BT_VECTOR: {
        uint64_t h = PRIM_FNV_OFFSET_BASIS;
        unsigned len = vector_len(key);
        unsigned *data = vector_data_ptr(key);
        for (unsigned i = 0; i < len; i++)
            h = hash_combine(h, hash_key_for_table_seen(ht, data[i], seen,
                                                        seen_len));
        return h;
    }
    case BT_BYTEVEC: {
        bytevec_data *bv = (bytevec_data *)CELL_PTR(key);
        if (!bytevec_data_well_formed(bv))
            return PRIM_FNV_OFFSET_BASIS;
        uint64_t h = PRIM_FNV_OFFSET_BASIS;
        for (unsigned i = 0; i < bv->len; i++) {
            h ^= bv->data[i];
            h *= PRIM_FNV_PRIME;
        }
        return h;
    }
    case BT_BIGNUM: {
        bignum *bn = get_bignum(key);
        if (!bn)
            return (uint64_t)key * PRIM_FNV_PRIME;
        char *s = bn_to_string(bn, 10);
        if (!s)
            return (uint64_t)key * PRIM_FNV_PRIME;
        uint64_t h = hash_string_bytes(s);
        free(s);
        return h;
    }
    default:
        return scheme_hash(key);
    }
}

static uint64_t hash_key_for_table(hash_table_data *ht, unsigned key)
{
    if (ht->equiv == HASH_EQ)
        return (IS_FIXNUM(key) || IS_ATOM(key) || IS_NUM(key) || IS_CHAR(key))
                   ? scheme_hash(key)
                   : (uint64_t)key * PRIM_FNV_PRIME;
    if (ht->equiv == HASH_EQV && !is_numeric(key))
        return (IS_FIXNUM(key) || IS_ATOM(key) || IS_CHAR(key))
                   ? scheme_hash(key)
                   : (uint64_t)key * PRIM_FNV_PRIME;

    unsigned seen[HASH_SEEN_STACK_MAX];
    return hash_key_for_table_seen(ht, key, seen, 0);
}

static bool hash_key_equal(hash_table_data *ht, unsigned a, unsigned b)
{
    switch (ht->equiv) {
    case HASH_EQ:
        return IS_TRUTHY(eq_value(a, b));
    case HASH_EQV:
        if (is_numeric(a) && is_numeric(b))
            return deep_equal(a, b);
        return IS_TRUTHY(eq_value(a, b));
    case HASH_EQUAL:
        return deep_equal(a, b);
    }
    return false;
}

// Hash tables whose keys may have moved during GC. eq/eqv tables hash
// compound keys by cell index, so bucket positions go stale when the copying
// collector moves a key. The GC registers affected tables here and calls
// hash_table_gc_rehash_pending() once the heap is consistent again
// (content-based hashing is unsafe mid-GC while broken hearts exist).
static hash_table_data **pending_rehash = NULL;
static size_t pending_rehash_len = 0, pending_rehash_cap = 0;

void hash_table_gc_register_rehash(hash_table_data *ht)
{
    if (!ht)
        return;
    for (size_t i = 0; i < pending_rehash_len; i++)
        if (pending_rehash[i] == ht)
            return;
    if (pending_rehash_len == pending_rehash_cap) {
        size_t cap = pending_rehash_cap ? pending_rehash_cap * 2 : 16;
        hash_table_data **grown = realloc(pending_rehash, cap * sizeof(*grown));
        if (!grown)
            return; // table keeps stale buckets; still memory-safe
        pending_rehash = grown;
        pending_rehash_cap = cap;
    }
    pending_rehash[pending_rehash_len++] = ht;
}

void hash_table_gc_rehash_pending(void)
{
    for (size_t t = 0; t < pending_rehash_len; t++) {
        hash_table_data *ht = pending_rehash[t];
        if (!hash_table_data_well_formed(ht))
            continue;
        unsigned cap = ht->capacity;
        hash_entry **fresh = calloc(cap, sizeof(*fresh));
        if (!fresh)
            continue; // keep stale buckets rather than lose entries
        for (unsigned i = 0; i < cap; i++) {
            hash_entry *e = ht->buckets[i];
            while (e) {
                hash_entry *next = e->next;
                unsigned b = (unsigned)(hash_key_for_table(ht, e->key) % cap);
                e->next = fresh[b];
                fresh[b] = e;
                e = next;
            }
        }
        free(ht->buckets);
        ht->buckets = fresh;
    }
    pending_rehash_len = 0;
}

static unsigned hash_table_capacity_for_size(uint64_t initial_size)
{
    unsigned cap = 16;
    uint64_t needed = initial_size == 0 ? 16 : initial_size;
    while (cap < UINT_MAX / 2 && (uint64_t)cap * 3 / 4 < needed)
        cap *= 2;
    return cap;
}

static bool parse_hash_table_initial_size(unsigned argc, unsigned *argv,
                                          const char *name,
                                          uint64_t *initial_size)
{
    *initial_size = 0;
    if (argc == 0 || IS_FALSE(argv[0]))
        return true;
    int64_t size;
    if (!expect_nonneg_int64(argv[0], &size, name))
        return false;
    *initial_size = (uint64_t)size;
    return true;
}

static hash_table_data *require_hash_table(unsigned value, const char *name)
{
    if (!IS_HASHTABLE(value)) {
        show_error("%s: not a hash table", name);
        return NULL;
    }
    hash_table_data *ht = GET_HASHTABLE_PTR(value);
    if (!hash_table_data_well_formed(ht)) {
        show_error("%s: invalid hash table", name);
        return NULL;
    }
    return ht;
}

static unsigned make_hash_table_value(hash_equiv equiv, unsigned argc,
                                      unsigned *argv, const char *name)
{
    uint64_t initial_size;
    if (!parse_hash_table_initial_size(argc, argv, name, &initial_size))
        return TOK_ERROR;
    hash_table_data *ht = checked_malloc_size(sizeof(hash_table_data));
    if (!ht) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    ht->capacity = hash_table_capacity_for_size(initial_size);
    ht->size = 0;
    ht->equiv = equiv;
    ht->buckets = checked_calloc_array(ht->capacity, sizeof(hash_entry *));
    if (!ht->buckets) {
        free(ht);
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    hash_table_register(ht);
    return make_pointer_cell(BT_HASHTABLE, ht);
}

static hash_entry **hash_entry_slot(hash_table_data *ht, unsigned key)
{
    unsigned bucket = (unsigned)(hash_key_for_table(ht, key) % ht->capacity);
    hash_entry **slot = &ht->buckets[bucket];
    while (*slot && !hash_key_equal(ht, (*slot)->key, key))
        slot = &(*slot)->next;
    return slot;
}

static bool hash_table_resize(hash_table_data *ht, unsigned new_capacity,
                              const char *name)
{
    hash_entry **new_buckets =
        checked_calloc_array(new_capacity, sizeof(hash_entry *));
    if (!new_buckets) {
        show_error("%s: out of memory", name);
        return false;
    }

    for (unsigned i = 0; i < ht->capacity; i++) {
        hash_entry *entry = ht->buckets[i];
        while (entry) {
            hash_entry *next = entry->next;
            unsigned bucket =
                (unsigned)(hash_key_for_table(ht, entry->key) % new_capacity);
            entry->next = new_buckets[bucket];
            new_buckets[bucket] = entry;
            entry = next;
        }
    }
    free(ht->buckets);
    ht->buckets = new_buckets;
    ht->capacity = new_capacity;
    return true;
}

static bool hash_table_grow_if_needed(hash_table_data *ht, const char *name)
{
    if ((uint64_t)(ht->size + 1) * 4 <= (uint64_t)ht->capacity * 3)
        return true;
    if (ht->capacity > UINT_MAX / 2) {
        show_error("%s: hash table too large", name);
        return false;
    }
    return hash_table_resize(ht, ht->capacity * 2, name);
}

static unsigned hash_table_ref_value(unsigned argc, unsigned *argv,
                                     const char *name)
{
    hash_table_data *ht = require_hash_table(argv[0], name);
    if (!ht)
        return TOK_ERROR;
    hash_entry **slot = hash_entry_slot(ht, argv[1]);
    if (*slot)
        return (*slot)->value;
    if (argc == 3)
        return argv[2];
    show_error("%s: key not found", name);
    return TOK_ERROR;
}

static unsigned hash_table_set_value(unsigned table_arg, unsigned key,
                                     unsigned value, const char *name)
{
    hash_table_data *ht = require_hash_table(table_arg, name);
    if (!ht)
        return TOK_ERROR;
    hash_entry **slot = hash_entry_slot(ht, key);
    write_barrier(table_arg, key);
    write_barrier(table_arg, value);
    if (*slot) {
        (*slot)->value = value;
        return 0;
    }
    if (!hash_table_grow_if_needed(ht, name))
        return TOK_ERROR;
    slot = hash_entry_slot(ht, key);
    hash_entry *entry = checked_malloc_size(sizeof(hash_entry));
    if (!entry) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    entry->key = key;
    entry->value = value;
    entry->next = NULL;
    *slot = entry;
    ht->size++;
    return 0;
}

static unsigned hash_table_delete_value(unsigned table_arg, unsigned key,
                                        const char *name)
{
    hash_table_data *ht = require_hash_table(table_arg, name);
    if (!ht)
        return TOK_ERROR;
    hash_entry **slot = hash_entry_slot(ht, key);
    if (*slot) {
        hash_entry *entry = *slot;
        *slot = entry->next;
        free(entry);
        ht->size--;
    }
    return 0;
}

static unsigned hash_table_exists_value(unsigned table_arg, unsigned key,
                                        const char *name)
{
    hash_table_data *ht = require_hash_table(table_arg, name);
    if (!ht)
        return TOK_ERROR;
    return scheme_bool(*hash_entry_slot(ht, key) != NULL);
}

static unsigned hash_table_clear_value(unsigned table_arg, const char *name)
{
    hash_table_data *ht = require_hash_table(table_arg, name);
    if (!ht)
        return TOK_ERROR;
    for (unsigned i = 0; i < ht->capacity; i++) {
        hash_entry *entry = ht->buckets[i];
        while (entry) {
            hash_entry *next = entry->next;
            free(entry);
            entry = next;
        }
        ht->buckets[i] = NULL;
    }
    ht->size = 0;
    return 0;
}

static unsigned hash_table_entries_list(unsigned table_arg, const char *name,
                                        int mode)
{
    hash_table_data *ht = require_hash_table(table_arg, name);
    if (!ht)
        return TOK_ERROR;
    GC_GUARD;
    unsigned result = 0;
    gc_protect(&result);
    for (unsigned i = 0; i < ht->capacity; i++) {
        for (hash_entry *entry = ht->buckets[i]; entry; entry = entry->next) {
            unsigned item = 0;
            gc_protect(&item);
            if (mode == 0) {
                item = entry->key;
            } else if (mode == 1) {
                item = entry->value;
            } else {
                unsigned key = entry->key;
                unsigned value = entry->value;
                gc_protect(&key);
                gc_protect(&value);
                item = alloc_cons(key, value);
                gc_unprotect(2);
            }
            result = alloc_cons(item, result);
            gc_unprotect(1);
        }
    }
    return result;
}

static unsigned make_public_gensym(void)
{
    char buf[32];
    snprintf(buf, sizeof(buf), "g%u", gensym_counter++);
    return atom_from_string(buf);
}

static unsigned transcript_on(unsigned filename_arg, const char *name)
{
    char *filename = require_string_ptr(filename_arg, name);
    if (!filename)
        return TOK_ERROR;
    if (ctx.transcript) {
        show_error("%s: transcript already active", name);
        return TOK_ERROR;
    }
    ctx.transcript = fopen(filename, "w");
    if (!ctx.transcript) {
        show_error("%s: cannot open %s", name, filename);
        return TOK_ERROR;
    }
    return 0;
}

static unsigned transcript_off(const char *name)
{
    if (!ctx.transcript) {
        show_error("%s: no transcript active", name);
        return TOK_ERROR;
    }
    if (fclose(ctx.transcript) != 0) {
        ctx.transcript = NULL;
        show_error("%s: close failed", name);
        return TOK_ERROR;
    }
    ctx.transcript = NULL;
    return 0;
}

static unsigned apply_transcript_primitive(unsigned prim_id, unsigned argc,
                                           unsigned *argv)
{
    switch (prim_id) {
    case PTRANSCRIPTON:
        REQUIRE_ARGC(argc, 1, 1, "transcript-on");
        return transcript_on(argv[0], "transcript-on");
    case PTRANSCRIPTOFF:
        REQUIRE_ARGC(argc, 0, 0, "transcript-off");
        return transcript_off("transcript-off");
    default:
        show_error("transcript primitive: unsupported id %u", prim_id);
        return TOK_ERROR;
    }
}

unsigned make_error_object_c(const char *msg, unsigned env)
{
    GC_GUARD;
    unsigned tag = lookup_silent(intern("*error-object-tag*"), env);
    if (tag == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&tag);
    unsigned error_kind = alloc();
    CELL_TYPE(error_kind) = BT_ATOM;
    CELL_ID(error_kind) = intern("error");
    gc_protect(&error_kind);
    unsigned msg_str = make_string_copy(msg ? msg : "unknown error");
    if (msg_str == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&msg_str);
    unsigned vec = make_vector(4, 0);
    if (vec == TOK_ERROR)
        return TOK_ERROR;
    vector_set_elem(vec, 0, tag);
    vector_set_elem(vec, 1, error_kind);
    vector_set_elem(vec, 2, msg_str);
    return vec;
}

static unsigned report_error_primitive(unsigned argc, unsigned *argv)
{
    fprintf(stderr, "error: ");
    for (unsigned i = 0; i < argc; i++) {
        display_obj_port(argv[i], stderr);
        if (i + 1 < argc)
            fprintf(stderr, " ");
    }
    fprintf(stderr, "\n");
    // Refresh ctx.last_error so a later primitive failure cannot reuse a
    // stale message from an earlier error as its own (see vm_signal_error).
    if (argc > 0 && IS_STRING(argv[0])) {
        snprintf(ctx.last_error, sizeof(ctx.last_error), "%s",
                 GET_STRING_PTR(argv[0]));
    } else {
        snprintf(ctx.last_error, sizeof(ctx.last_error), "error");
    }
    return TOK_ERROR;
}

static unsigned apply_environment_primitive(unsigned prim_id, unsigned argc,
                                            unsigned *argv)
{
    switch (prim_id) {
    case PSCHEMEENV:
        REQUIRE_ARGC(argc, 1, 1, "scheme-report-environment");
        return scheme_environment(argv[0], "scheme-report-environment");
    case PNULLENV:
        REQUIRE_ARGC(argc, 1, 1, "null-environment");
        return null_environment(argv[0], "null-environment");
    default:
        show_error("environment primitive: unsupported id %u", prim_id);
        return TOK_ERROR;
    }
}

static unsigned apply_misc_primitive(unsigned prim_id, unsigned argc,
                                     unsigned *argv)
{
    switch (prim_id) {
    case PCURRENTSECOND:
        REQUIRE_ARGC(argc, 0, 0, "current-second");
        return current_second_value();
    case PCURRENTJIFFY:
        REQUIRE_ARGC(argc, 0, 0, "current-jiffy");
        return current_jiffy_value();
    case PJIFFIESPERSECOND:
        REQUIRE_ARGC(argc, 0, 0, "jiffies-per-second");
        return store(JIFFIES_PER_SECOND);
    case PEMERGENCYEXIT: {
        REQUIRE_ARGC(argc, 0, 1, "emergency-exit");
        int code = 0;
        if (argc == 1 &&
            !expect_exit_code(argv[0], &code, "emergency-exit")) {
                return TOK_ERROR;
        }
        _Exit(code);
    }
    case PERROR:
        return report_error_primitive(argc, argv);
    case PGENSYM:
        REQUIRE_ARGC(argc, 0, 0, "gensym");
        return make_public_gensym();
    case PGCSTATS:
        REQUIRE_ARGC(argc, 0, 0, "gc-stats");
        return make_gc_stats_list();
    case PVALUES:
        return values_from_argv(argc, argv);
    case PCOMMANDLINE:
        REQUIRE_ARGC(argc, 0, 0, "command-line");
        return make_command_line_list();
    case PWRITETOSTRING:
        REQUIRE_ARGC(argc, 1, 1, "write-to-string");
        return write_to_string_value(argv[0], "write-to-string");
    case PLISTREF:
        REQUIRE_ARGC(argc, 2, 2, "list-ref");
        return list_ref_value(argv[0], argv[1], "list-ref");
    case POPENBINARYINPUT:
        REQUIRE_ARGC(argc, 1, 1, "open-binary-input-file");
        return open_binary_input_file(argv[0], "open-binary-input-file");
    case PREADBYTEVEC:
        REQUIRE_ARGC(argc, 2, 2, "read-bytevector");
        return read_bytevector_from_port(argv[0], argv[1],
                                         "read-bytevector");
    case PFILEEXISTS:
        REQUIRE_ARGC(argc, 1, 1, "file-exists?");
        return file_exists_p(argv[0], "file-exists?");
    case PFILEREGULARP:
        REQUIRE_ARGC(argc, 1, 1, "file-regular?");
        return file_type_p(argv[0], "file-regular?", PFILEREGULARP);
    case PFILEDIRECTORYP:
        REQUIRE_ARGC(argc, 1, 1, "file-directory?");
        return file_type_p(argv[0], "file-directory?", PFILEDIRECTORYP);
    case PDELETEFILE:
        REQUIRE_ARGC(argc, 1, 1, "delete-file");
        return delete_file_value(argv[0], "delete-file");
    case PMAKEDIRECTORY:
        REQUIRE_ARGC(argc, 1, 1, "make-directory");
        return make_directory_value(argv[0], "make-directory");
    case PDELETEDIRECTORY:
        REQUIRE_ARGC(argc, 1, 1, "delete-directory");
        return delete_directory_value(argv[0], "delete-directory");
    case PRENAMEFILE:
        REQUIRE_ARGC(argc, 2, 2, "rename-file");
        return rename_file_value(argv[0], argv[1], "rename-file");
    case PCURRENTDIRECTORY:
        REQUIRE_ARGC(argc, 0, 1, "current-directory");
        return current_directory_value(argc, argv, "current-directory");
    case PDIRECTORYFILES:
        REQUIRE_ARGC(argc, 1, 1, "directory-files");
        return directory_files_value(argv[0], "directory-files");
    case PTEMPFILEPATH:
        REQUIRE_ARGC(argc, 0, 0, "temporary-file-path");
        return temporary_file_path_value();
    case PGETENV:
        REQUIRE_ARGC(argc, 1, 1, "get-environment-variable");
        return getenv_value(argv[0], "get-environment-variable");
    case PGETENVS:
        REQUIRE_ARGC(argc, 0, 0, "get-environment-variables");
        return get_environment_variables_value();
    case POPENBINARYOUTPUT:
        REQUIRE_ARGC(argc, 1, 1, "open-binary-output-file");
        return open_binary_output_file(argv[0], "open-binary-output-file");
    case PWRITEBYTEVEC:
        REQUIRE_ARGC(argc, 2, 2, "write-bytevector");
        return write_bytevector_to_port(argv[0], argv[1], "write-bytevector");
    case PREADBYTEVECINTO:
        REQUIRE_ARGC(argc, 2, 4, "read-bytevector!");
        return read_bytevector_into(argc, argv, "read-bytevector!");
    case PFEATURES:
        REQUIRE_ARGC(argc, 0, 0, "features");
        return make_features_list();
    case PMAKEHASHTABLE:
        REQUIRE_ARGC(argc, 0, 1, "make-hash-table");
        return make_hash_table_value(HASH_EQUAL, argc, argv,
                                     "make-hash-table");
    case PMAKESTRONGEQHASHTABLE:
        REQUIRE_ARGC(argc, 0, 1, "make-strong-eq-hash-table");
        return make_hash_table_value(HASH_EQ, argc, argv,
                                     "make-strong-eq-hash-table");
    case PMAKEEQHASHTABLE:
        REQUIRE_ARGC(argc, 0, 1, "make-eq-hash-table");
        return make_hash_table_value(HASH_EQ, argc, argv,
                                     "make-eq-hash-table");
    case PMAKESTRONGEQVHASHTABLE:
        REQUIRE_ARGC(argc, 0, 1, "make-strong-eqv-hash-table");
        return make_hash_table_value(HASH_EQV, argc, argv,
                                     "make-strong-eqv-hash-table");
    case PMAKEEQVHASHTABLE:
        REQUIRE_ARGC(argc, 0, 1, "make-eqv-hash-table");
        return make_hash_table_value(HASH_EQV, argc, argv,
                                     "make-eqv-hash-table");
    case PMAKEEQUALHASHTABLE:
        REQUIRE_ARGC(argc, 0, 1, "make-equal-hash-table");
        return make_hash_table_value(HASH_EQUAL, argc, argv,
                                     "make-equal-hash-table");
    case PHASHTABLEP:
        REQUIRE_ARGC(argc, 1, 1, "hash-table?");
        return scheme_bool(IS_HASHTABLE(argv[0]));
    case PHASHTABLEREF:
        REQUIRE_ARGC(argc, 2, 3, "hash-table-ref");
        return hash_table_ref_value(argc, argv, "hash-table-ref");
    case PHASHTABLESET:
        REQUIRE_ARGC(argc, 3, 3, "hash-table-set!");
        return hash_table_set_value(argv[0], argv[1], argv[2],
                                    "hash-table-set!");
    case PHASHTABLEDELETE:
        REQUIRE_ARGC(argc, 2, 2, "hash-table-delete!");
        return hash_table_delete_value(argv[0], argv[1],
                                       "hash-table-delete!");
    case PHASHTABLEEXISTS:
        REQUIRE_ARGC(argc, 2, 2, "hash-table-exists?");
        return hash_table_exists_value(argv[0], argv[1],
                                       "hash-table-exists?");
    case PHASHTABLESIZE: {
        REQUIRE_ARGC(argc, 1, 1, "hash-table-size");
        hash_table_data *ht = require_hash_table(argv[0], "hash-table-size");
        return ht ? store(ht->size) : TOK_ERROR;
    }
    case PHASHTABLECLEAR:
        REQUIRE_ARGC(argc, 1, 1, "hash-table-clear!");
        return hash_table_clear_value(argv[0], "hash-table-clear!");
    case PHASHTABLEKEYS:
        REQUIRE_ARGC(argc, 1, 1, "hash-table-keys");
        return hash_table_entries_list(argv[0], "hash-table-keys", 0);
    case PHASHTABLEVALUES:
        REQUIRE_ARGC(argc, 1, 1, "hash-table-values");
        return hash_table_entries_list(argv[0], "hash-table-values", 1);
    case PHASHTABLEALIST:
        REQUIRE_ARGC(argc, 1, 1, "hash-table->alist");
        return hash_table_entries_list(argv[0], "hash-table->alist", 2);
    default:
        show_error("misc primitive: unsupported id %u", prim_id);
        return TOK_ERROR;
    }
}

static unsigned unsupported_special_primitive(unsigned prim_id)
{
    const char *message = NULL;
    switch (prim_id) {
    case PAPPLY:
        message = "apply: internal error - should be handled in apply_function";
        break;
    case PLOAD:
        message = "load: internal error - should be handled in apply_function";
        break;
    case PCALLCC:
        message = "call/cc must be called as a function, not a primitive";
        break;
    case PCALLWITHVALUES:
        message = "call-with-values: internal error - should be handled in "
                  "apply_function";
        break;
    case PEVAL:
        message = "eval: internal error - should be handled in apply_function";
        break;
    case PINTERACTIONENV:
        message = "interaction-environment: internal error - should be "
                  "handled in apply_function";
        break;
    case PRAISENOW:
        message = "raise-now: internal error - should be handled in "
                  "apply_function";
        break;
    default:
        message = "primitive: unsupported special primitive";
        break;
    }
    show_error("%s", message);
    return TOK_ERROR;
}

unsigned apply_primitive_argv(unsigned prim_id, unsigned argc, unsigned *argv)
{
    if (argc > 0 && !argv) {
        show_error("primitive: null argv");
        return TOK_ERROR;
    }

    GC_GUARD;
    for (unsigned i = 0; i < argc; i++)
        gc_protect(&argv[i]);

    switch (prim_id) {
    // Arithmetic - delegated to prim_numeric.c
    case PPLUS:
    case PMINUS:
    case PTIMES:
    case PDIV:
    case PMOD:
    case PREMAINDER:
    case PTRUNCATEDIVREM:
    case PFLOORDIVREM:
    case PQUOTIENT:
    case PABS:
        return apply_arithmetic_primitive(prim_id, argc, argv);

    // Numeric comparison - delegated to prim_compare.c
    case PEQUAL:
    case PLT:
    case PGT:
    case PLEQ:
    case PGEQ:
        return apply_numeric_compare_primitive(prim_id, argc, argv);

    // Logic
    case PNOT:
    case PEQ:
    case PEQV:
    case PEQUALP:
        return apply_logic_primitive(prim_id, argc, argv);

    // List operations
    case PCONS:
    case PCAR:
    case PCDR:
    case PSETCAR:
    case PSETCDR:
    case PLIST:
    case PLENGTH:
    case PAPPEND:
    case PREVERSE:
    case PLASTPAIR:
        return apply_list_primitive(prim_id, argc, argv);

    // Type predicates - delegated to prim_type.c
    case PSYMP:
    case PNUMP:
    case PINTEGERP:
    case PREALP:
    case PEXACTP:
    case PINEXACTP:
    case PCOMPLEXP:
    case PRATIONALP:
    case PPROCP:
    case PCONSP:
    case PNULLP:
    case PSTRINGP:
    case PCHARP:
    case PVECTORP:
    case PBOOLP:
    case PLISTP:
        return apply_type_predicate(prim_id, argc, argv);

    // I/O - delegated to prim_io.c
    case PDISPLAY:
    case PWRITE:
    case PWRITESHARED:
    case PWRITESIMPLE:
    case PNEWLINE:
    case PREAD:
    case PREADCHAR:
    case PPEEKCHAR:
    case PWRITECHAR:
    case PEOF:
    case PEOFOBJECT:
    case PCHARREADY:
    case PU8READY:
    case PPEEKU8:
    case PREADLINE:
    case PREADSTRING:
    case PWRITESTRING:
    case PEXIT:
        return apply_io_primitive(prim_id, argc, argv);

    // Ports - delegated to prim_port.c
    case POPENINPUT:
    case POPENOUTPUT:
    case PCLOSEINPUT:
    case PCLOSEOUTPUT:
    case PINPUTPORTP:
    case POUTPUTPORTP:
    case PCURRENTINPUT:
    case PCURRENTOUTPUT:
    case PCURRENTERROR:
    case POPENOUTPUTSTRING:
    case PGETOUTPUTSTRING:
    case POPENINPUTSTRING:
    case PSTRINGPORTP:
    case PSETCURRENTINPUT:
    case PSETCURRENTOUTPUT:
    case PSETCURRENTERROR:
    case PFLUSHOUTPUT:
    case PPORTOPENP:
    case PINPUTPORTOPENP:
    case POUTPUTPORTOPENP:
    case PTEXTUALPORTP:
    case PBINARYPORTP:
        return apply_port_primitive(prim_id, argc, argv);

    // String operations
    case PSTRLEN:
    case PSTRREF:
    case PSTRSET:
    case PSTRAPP:
    case PSUBSTR:
    case PSTR2SYM:
    case PSYM2STR:
    case PNUM2STR:
    case PSTR2NUM:
    case PMAKESTR:
    case PSTRCOPY:
    case PSTR2LIST:
    case PLIST2STR:
    case PSTRFILL:
    case PSTRNFD:
    case PSTRNFC:
    case PSTRNFKD:
    case PSTRNFKC:
    case PSTRFOLD:
    case PSTRUP:
    case PSTRDOWN:
    case PSTRTITLE:
    case PSTRING:
    case PSTRINGTOUTF8:
    case PUTF8TOSTRING:
    case PSTREQ:
    case PSTRLT:
    case PSTRGT:
    case PSTRLE:
    case PSTRGE:
    case PSTREQI:
    case PSTRLTI:
    case PSTRGTI:
    case PSTRLEI:
    case PSTRGEI:
        return apply_string_primitive(prim_id, argc, argv);

    // Character operations - delegated to prim_char.c
    case PCHARCODE:
    case PCODECHAR:
    case PCHARUP:
    case PCHARDOWN:
    case PCHARFOLD:
    case PCHAREQ:
    case PCHARLT:
    case PCHARGT:
    case PCHARLE:
    case PCHARGE:
    case PCHAREQI:
    case PCHARLTI:
    case PCHARGTI:
    case PCHARLEI:
    case PCHARGEI:
    case PCHARALPHA:
    case PCHARNUMERIC:
    case PCHARWHITE:
    case PCHARUPPER:
    case PCHARLOWER:
        return apply_char_primitive(prim_id, argc, argv);

    // Vector operations - delegated to prim_vector.c
    case PMAKEVEC:
    case PVECTOR:
    case PVECREF:
    case PVECSET:
    case PVECLEN:
    case PVECFILL:
    case PLIST2VEC:
    case PVEC2LIST:
        return apply_vector_primitive(prim_id, argc, argv);

    // Math functions - delegated to prim_math.c
    case PSQRT:
    case PEXPT:
    case PSIN:
    case PCOS:
    case PTAN:
    case PASIN:
    case PACOS:
    case PATAN:
    case PLOG:
    case PEXP:
    case PLOG1P:
    case PEXPM1:
    case PSQRT1PM1:
    case PLOG1PEXP:
    case PFLOOR:
    case PCEILING:
    case PTRUNCATE:
    case PROUND:
    case PRANDOMINTEGER:
    case PRANDOMREAL:
    case PRANDOMSEED:
        return apply_math_primitive(prim_id, argc, argv);

    // Miscellaneous local primitives
    case PCURRENTSECOND:
    case PCURRENTJIFFY:
    case PJIFFIESPERSECOND:
    case PEMERGENCYEXIT:
    case PERROR:
    case PGENSYM:
    case PGCSTATS:
    case PVALUES:
    case PCOMMANDLINE:
    case PWRITETOSTRING:
    case PLISTREF:
    case POPENBINARYINPUT:
    case PREADBYTEVEC:
    case PFILEEXISTS:
    case PFILEREGULARP:
    case PFILEDIRECTORYP:
    case PDELETEFILE:
    case PMAKEDIRECTORY:
    case PDELETEDIRECTORY:
    case PRENAMEFILE:
    case PCURRENTDIRECTORY:
    case PDIRECTORYFILES:
    case PTEMPFILEPATH:
    case PGETENV:
    case PGETENVS:
    case POPENBINARYOUTPUT:
    case PWRITEBYTEVEC:
    case PREADBYTEVECINTO:
    case PFEATURES:
    case PMAKEHASHTABLE:
    case PMAKESTRONGEQHASHTABLE:
    case PMAKEEQHASHTABLE:
    case PMAKESTRONGEQVHASHTABLE:
    case PMAKEEQVHASHTABLE:
    case PMAKEEQUALHASHTABLE:
    case PHASHTABLEP:
    case PHASHTABLEREF:
    case PHASHTABLESET:
    case PHASHTABLEDELETE:
    case PHASHTABLEEXISTS:
    case PHASHTABLESIZE:
    case PHASHTABLECLEAR:
    case PHASHTABLEKEYS:
    case PHASHTABLEVALUES:
    case PHASHTABLEALIST:
        return apply_misc_primitive(prim_id, argc, argv);
    // PGCFLIP is handled specially in eval.c (needs environment as root)

    // Numeric tower operations - delegated to prim_numtower.c
    case PNUMERATOR:
    case PDENOMINATOR:
    case PMAKERECT:
    case PMAKEPOLAR:
    case PREALPART:
    case PIMAGPART:
    case PMAGNITUDE:
    case PANGLE:
    case PEXACT2INEXACT:
    case PINEXACT2EXACT:
    case PRATIONALIZE:
    case PFINITE:
    case PINFINITE:
    case PNAN:
        return apply_numtower_primitive(prim_id, argc, argv);

    // Transcript
    case PTRANSCRIPTON:
    case PTRANSCRIPTOFF:
        return apply_transcript_primitive(prim_id, argc, argv);

    // R5RS environment procedures
    case PSCHEMEENV:
    case PNULLENV:
        return apply_environment_primitive(prim_id, argc, argv);

    // Special cases handled elsewhere
    case PAPPLY:
    case PLOAD:
    case PCALLCC:
    case PCALLWITHVALUES:
    case PEVAL:
    case PINTERACTIONENV:
        return unsupported_special_primitive(prim_id);

    // ========================================================================
    // Bitwise operations
    // ========================================================================
    case PBITWISEAND:
    case PBITWISEIOR:
    case PBITWISENOT:
    case PBITWISEXOR:
    case PARITHSHIFT:
        return apply_bitwise_primitive(prim_id, argc, argv);

    // ========================================================================
    // Bytevector operations
    // ========================================================================
    case PMAKEBYTEVEC:
    case PBYTEVECREF:
    case PBYTEVECSET:
    case PBYTEVECLEN:
    case PBYTEVECCOPY:
    case PBYTEVECCOPYTO:
    case PBYTEVECAPPEND:
    case PBYTEVEC:
    case PBYTEVECUP:
        return apply_bytevector_primitive(prim_id, argc, argv);

    default:
        show_error("unknown primitive: %u", prim_id);
        return TOK_ERROR;
    }
}

unsigned apply_primitive(unsigned prim_id, unsigned args)
{
    unsigned argc = 0;
    if (!list_length_checked(args, &argc, "primitive"))
        return TOK_ERROR;

    unsigned argv_stack[8];
    unsigned *argv = argv_stack;
    if (argc > sizeof(argv_stack) / sizeof(argv_stack[0])) {
        argv = checked_malloc_array(argc, sizeof(*argv));
        if (!argv) {
            show_error("primitive: out of memory");
            return TOK_ERROR;
        }
    }

    unsigned i = 0;
    for (unsigned it = args; it; it = cdr(it))
        argv[i++] = car(it);

    unsigned result = apply_primitive_argv(prim_id, argc, argv);

    if (argv != argv_stack)
        free(argv);
    return result;
}
