/**
 * @file prim_string.c
 * @brief String operations (string-append, substring, etc.)
 */

#include "prim_internal.h"
#include "utf8.h"
#include "unicode_norm_tables.h"

#define STRING_APPEND_STACK_LENGTHS 64
#define CP_BUFFER_STACK_CAP 128

static void free_string_append_lengths(size_t *lengths, size_t *stack_lengths)
{
    if (lengths != stack_lengths)
        free(lengths);
}

static bool utf8_decode_next_codepoint_string(const char *s, size_t byte_len,
                                              size_t *offset,
                                              uint32_t *codepoint,
                                              const char *name);

unsigned prim_string_append(unsigned argc, unsigned *argv)
{
    // First pass: validate and compute total length, cache individual lengths
    size_t total = 0;
    size_t stack_lengths[STRING_APPEND_STACK_LENGTHS];
    size_t *lengths = (argc <= STRING_APPEND_STACK_LENGTHS)
                          ? stack_lengths
                          : checked_malloc_array(argc, sizeof(size_t));
    if (!lengths) {
        show_error("string-append: out of memory");
        return TOK_ERROR;
    }

    for (unsigned i = 0; i < argc; i++) {
        char *s = require_string_ptr(argv[i], "string-append");
        if (!s) {
            free_string_append_lengths(lengths, stack_lengths);
            return TOK_ERROR;
        }
        lengths[i] = strlen(s);
        if (lengths[i] > SIZE_MAX - total - 1) {
            free_string_append_lengths(lengths, stack_lengths);
            show_error("string-append: result too large");
            return TOK_ERROR;
        }
        total += lengths[i];
    }

    char *result = checked_malloc_flex(0, total + 1, 1);
    if (!result) {
        free_string_append_lengths(lengths, stack_lengths);
        show_error("string-append: out of memory");
        return TOK_ERROR;
    }

    // Second pass: copy using cached lengths and memcpy
    char *pos = result;
    for (unsigned i = 0; i < argc; i++) {
        memcpy(pos, GET_STRING_PTR(argv[i]), lengths[i]);
        pos += lengths[i];
    }
    *pos = '\0';

    free_string_append_lengths(lengths, stack_lengths);

    return make_string_owned(result);
}

static bool utf8_decode_next_codepoint_string(const char *s, size_t byte_len,
                                              size_t *offset,
                                              uint32_t *codepoint,
                                              const char *name)
{
    const char *error_msg = NULL;
    if (!scheme_utf8_decode_next(s, byte_len, offset, codepoint,
                                 &error_msg)) {
        show_error("%s: %s", name, error_msg);
        return false;
    }
    return true;
}

static bool utf8_count_chars_string(const char *s, size_t *chars,
                                    const char *name)
{
    const char *error_msg = NULL;
    if (scheme_utf8_count_chars(s, chars, &error_msg))
        return true;
    show_error("%s: %s", name, error_msg);
    return false;
}

static bool utf8_byte_offset_for_index_string(const char *s,
                                              size_t char_index,
                                              size_t *byte_offset,
                                              const char *name)
{
    const char *error_msg = NULL;
    if (!scheme_utf8_byte_offset_for_index(s, char_index, true,
                                           byte_offset, &error_msg)) {
        show_error("%s: %s", name, error_msg);
        return false;
    }
    return true;
}

unsigned prim_substring(unsigned argc, unsigned *argv)
{
    REQUIRE_ARGC(argc, 1, 3, "substring");
    char *s = require_string_ptr(argv[0], "substring");
    if (!s)
        return TOK_ERROR;
    size_t char_len;
    if (!utf8_count_chars_string(s, &char_len, "substring"))
        return TOK_ERROR;
    int64_t start;
    if (argc > 1) {
        if (!expect_nonneg_int64(argv[1], &start, "substring"))
            return TOK_ERROR;
    } else {
        start = 0;
    }
    int64_t end;
    if (argc == 3) {
        if (!expect_nonneg_int64(argv[2], &end, "substring"))
            return TOK_ERROR;
    } else {
        end = (int64_t)char_len;
    }
    if (start < 0 || end < start || end > (int64_t)char_len) {
        show_error("substring: invalid indices");
        return TOK_ERROR;
    }
    size_t start_byte;
    size_t end_byte;
    if (!utf8_byte_offset_for_index_string(s, (size_t)start, &start_byte,
                                           "substring") ||
        !utf8_byte_offset_for_index_string(s, (size_t)end, &end_byte,
                                           "substring"))
        return TOK_ERROR;
    size_t result_len = end_byte - start_byte;
    char *result = checked_string_copy_len(s + start_byte, result_len);
    if (!result) {
        show_error("substring: out of memory");
        return TOK_ERROR;
    }
    return make_string_owned(result);
}

typedef struct {
    uint32_t *data;
    size_t length;
    size_t capacity;
    uint32_t stack[CP_BUFFER_STACK_CAP];
} codepoint_buffer;

static void cp_buffer_init(codepoint_buffer *buf)
{
    buf->data = buf->stack;
    buf->length = 0;
    buf->capacity = CP_BUFFER_STACK_CAP;
}

static void cp_buffer_free(codepoint_buffer *buf)
{
    if (buf->data != buf->stack)
        free(buf->data);
}

static bool cp_buffer_reserve(codepoint_buffer *buf, size_t needed)
{
    if (needed <= buf->capacity)
        return true;
    size_t new_cap = buf->capacity;
    while (new_cap < needed) {
        if (new_cap > SIZE_MAX / 2)
            return false;
        new_cap *= 2;
    }
    uint32_t *new_data;
    size_t alloc_size;
    if (!checked_flex_size(0, new_cap, sizeof(uint32_t), &alloc_size))
        return false;
    if (buf->data == buf->stack) {
        new_data = checked_malloc_size(alloc_size);
        if (new_data)
            memcpy(new_data, buf->stack, buf->length * sizeof(uint32_t));
    } else {
        new_data = checked_realloc_size(buf->data, alloc_size);
    }
    if (!new_data)
        return false;
    buf->data = new_data;
    buf->capacity = new_cap;
    return true;
}

static bool cp_buffer_append(codepoint_buffer *buf, uint32_t codepoint)
{
    if (!cp_buffer_reserve(buf, buf->length + 1))
        return false;
    buf->data[buf->length++] = codepoint;
    return true;
}

static bool utf8_encode_codepoint_string(uint32_t codepoint,
                                         codepoint_buffer *bytes)
{
    char encoded[4];
    size_t encoded_len;
    if (!scheme_utf8_encode_scalar(codepoint, encoded, &encoded_len))
        return false;
    for (size_t i = 0; i < encoded_len; i++) {
        if (!cp_buffer_append(bytes, (uint8_t)encoded[i]))
            return false;
    }
    return true;
}

static const unicode_decomp_entry *find_decomp(uint32_t codepoint)
{
    size_t lo = 0;
    size_t hi = UNICODE_DECOMP_COUNT;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (unicode_decomp_table[mid].code < codepoint)
            lo = mid + 1;
        else
            hi = mid;
    }
    if (lo < UNICODE_DECOMP_COUNT && unicode_decomp_table[lo].code == codepoint)
        return &unicode_decomp_table[lo];
    return NULL;
}

static uint8_t unicode_combining_class(uint32_t codepoint)
{
    size_t lo = 0;
    size_t hi = UNICODE_CCC_COUNT;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (unicode_ccc_table[mid].code < codepoint)
            lo = mid + 1;
        else
            hi = mid;
    }
    if (lo < UNICODE_CCC_COUNT && unicode_ccc_table[lo].code == codepoint)
        return unicode_ccc_table[lo].combining_class;
    return 0;
}

static uint32_t unicode_compose_pair(uint32_t starter, uint32_t combiner)
{
    const uint32_t s_base = 0xAC00;
    const uint32_t l_base = 0x1100;
    const uint32_t v_base = 0x1161;
    const uint32_t t_base = 0x11A7;
    const uint32_t l_count = 19;
    const uint32_t v_count = 21;
    const uint32_t t_count = 28;
    const uint32_t n_count = v_count * t_count;
    const uint32_t s_count = l_count * n_count;

    if (starter >= l_base && starter < l_base + l_count &&
        combiner >= v_base && combiner < v_base + v_count) {
        return s_base + ((starter - l_base) * v_count + (combiner - v_base)) *
                            t_count;
    }
    if (starter >= s_base && starter < s_base + s_count &&
        ((starter - s_base) % t_count) == 0 &&
        combiner > t_base && combiner < t_base + t_count) {
        return starter + (combiner - t_base);
    }

    size_t lo = 0;
    size_t hi = UNICODE_COMPOSE_COUNT;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        const unicode_compose_entry *entry = &unicode_compose_table[mid];
        if (entry->starter < starter ||
            (entry->starter == starter && entry->combiner < combiner))
            lo = mid + 1;
        else
            hi = mid;
    }
    if (lo < UNICODE_COMPOSE_COUNT &&
        unicode_compose_table[lo].starter == starter &&
        unicode_compose_table[lo].combiner == combiner)
        return unicode_compose_table[lo].composite;
    return 0;
}

static bool decompose_codepoint(uint32_t codepoint, bool compatibility,
                                codepoint_buffer *out)
{
    const uint32_t s_base = 0xAC00;
    const uint32_t l_base = 0x1100;
    const uint32_t v_base = 0x1161;
    const uint32_t t_base = 0x11A7;
    const uint32_t l_count = 19;
    const uint32_t v_count = 21;
    const uint32_t t_count = 28;
    const uint32_t n_count = v_count * t_count;
    const uint32_t s_count = l_count * n_count;

    if (codepoint >= s_base && codepoint < s_base + s_count) {
        uint32_t s_index = codepoint - s_base;
        uint32_t l = l_base + s_index / n_count;
        uint32_t v = v_base + (s_index % n_count) / t_count;
        uint32_t t = t_base + s_index % t_count;
        if (!cp_buffer_append(out, l) || !cp_buffer_append(out, v))
            return false;
        if (t != t_base && !cp_buffer_append(out, t))
            return false;
        return true;
    }

    const unicode_decomp_entry *entry = find_decomp(codepoint);
    if (!entry || (entry->compatibility && !compatibility))
        return cp_buffer_append(out, codepoint);
    for (uint8_t i = 0; i < entry->length; i++) {
        if (!decompose_codepoint(unicode_decomp_data[entry->offset + i],
                                 compatibility, out))
            return false;
    }
    return true;
}

static void reorder_combining_marks(codepoint_buffer *buf)
{
    size_t segment_start = 0;
    for (size_t i = 0; i <= buf->length; i++) {
        if (i == buf->length || unicode_combining_class(buf->data[i]) == 0) {
            for (size_t j = segment_start + 1; j < i; j++) {
                uint32_t ch = buf->data[j];
                uint8_t ccc = unicode_combining_class(ch);
                size_t k = j;
                while (k > segment_start &&
                       unicode_combining_class(buf->data[k - 1]) > ccc) {
                    buf->data[k] = buf->data[k - 1];
                    k--;
                }
                buf->data[k] = ch;
            }
            segment_start = i + 1;
        }
    }
}

static void compose_codepoints(codepoint_buffer *buf)
{
    if (buf->length == 0)
        return;

    size_t out_len = 1;
    size_t starter_pos = 0;
    uint32_t starter = buf->data[0];
    uint8_t last_ccc = 0;

    for (size_t i = 1; i < buf->length; i++) {
        uint32_t ch = buf->data[i];
        uint8_t ccc = unicode_combining_class(ch);
        uint32_t composite = 0;
        if (starter_pos < out_len && (last_ccc == 0 || last_ccc < ccc))
            composite = unicode_compose_pair(starter, ch);

        if (composite) {
            buf->data[starter_pos] = composite;
            starter = composite;
            continue;
        }

        buf->data[out_len++] = ch;
        if (ccc == 0) {
            starter_pos = out_len - 1;
            starter = ch;
        }
        last_ccc = ccc;
    }
    buf->length = out_len;
}

static unsigned normalized_string_value(unsigned arg, bool compatibility,
                                        bool compose, const char *name)
{
    char *s = require_string_ptr(arg, name);
    if (!s)
        return TOK_ERROR;

    codepoint_buffer decomposed;
    cp_buffer_init(&decomposed);
    size_t byte_len = strlen(s);
    size_t offset = 0;
    while (offset < byte_len) {
        uint32_t codepoint;
        if (!utf8_decode_next_codepoint_string(s, byte_len, &offset,
                                               &codepoint, name)) {
            cp_buffer_free(&decomposed);
            return TOK_ERROR;
        }
        if (!decompose_codepoint(codepoint, compatibility, &decomposed)) {
            cp_buffer_free(&decomposed);
            show_error("%s: out of memory", name);
            return TOK_ERROR;
        }
    }

    reorder_combining_marks(&decomposed);
    if (compose)
        compose_codepoints(&decomposed);

    codepoint_buffer bytes;
    cp_buffer_init(&bytes);
    for (size_t i = 0; i < decomposed.length; i++) {
        if (!utf8_encode_codepoint_string(decomposed.data[i], &bytes)) {
            cp_buffer_free(&bytes);
            cp_buffer_free(&decomposed);
            show_error("%s: invalid Unicode scalar value", name);
            return TOK_ERROR;
        }
    }
    if (!cp_buffer_append(&bytes, 0)) {
        cp_buffer_free(&bytes);
        cp_buffer_free(&decomposed);
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }

    char *result = checked_malloc_size(bytes.length);
    if (!result) {
        cp_buffer_free(&bytes);
        cp_buffer_free(&decomposed);
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    for (size_t i = 0; i < bytes.length; i++)
        result[i] = (char)bytes.data[i];

    cp_buffer_free(&bytes);
    cp_buffer_free(&decomposed);
    return make_string_owned(result);
}

static unsigned folded_string_value(unsigned arg, const char *name)
{
    char *s = require_string_ptr(arg, name);
    if (!s)
        return TOK_ERROR;

    codepoint_buffer folded;
    cp_buffer_init(&folded);
    size_t byte_len = strlen(s);
    size_t offset = 0;
    while (offset < byte_len) {
        uint32_t codepoint;
        if (!utf8_decode_next_codepoint_string(s, byte_len, &offset,
                                               &codepoint, name)) {
            cp_buffer_free(&folded);
            return TOK_ERROR;
        }
        const uint32_t *mapping;
        size_t length;
        if (unicode_full_foldcase(codepoint, &mapping, &length)) {
            for (size_t i = 0; i < length; i++) {
                if (!cp_buffer_append(&folded, mapping[i])) {
                    cp_buffer_free(&folded);
                    show_error("%s: out of memory", name);
                    return TOK_ERROR;
                }
            }
        } else if (!cp_buffer_append(&folded, codepoint)) {
            cp_buffer_free(&folded);
            show_error("%s: out of memory", name);
            return TOK_ERROR;
        }
    }

    codepoint_buffer bytes;
    cp_buffer_init(&bytes);
    for (size_t i = 0; i < folded.length; i++) {
        if (!utf8_encode_codepoint_string(folded.data[i], &bytes)) {
            cp_buffer_free(&bytes);
            cp_buffer_free(&folded);
            show_error("%s: invalid Unicode scalar value", name);
            return TOK_ERROR;
        }
    }
    if (!cp_buffer_append(&bytes, 0)) {
        cp_buffer_free(&bytes);
        cp_buffer_free(&folded);
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }

    char *result = checked_malloc_size(bytes.length);
    if (!result) {
        cp_buffer_free(&bytes);
        cp_buffer_free(&folded);
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    for (size_t i = 0; i < bytes.length; i++)
        result[i] = (char)bytes.data[i];

    cp_buffer_free(&bytes);
    cp_buffer_free(&folded);
    return make_string_owned(result);
}

typedef enum {
    STRING_CASE_UP,
    STRING_CASE_DOWN,
    STRING_CASE_TITLE
} string_case_mode;

static bool final_sigma_context(const codepoint_buffer *input, size_t index)
{
    bool before_cased = false;
    for (size_t i = index; i > 0; i--) {
        uint32_t prev = input->data[i - 1];
        if (unicode_is_case_ignorable(prev))
            continue;
        before_cased = unicode_is_cased(prev);
        break;
    }
    if (!before_cased)
        return false;

    for (size_t i = index + 1; i < input->length; i++) {
        uint32_t next = input->data[i];
        if (unicode_is_case_ignorable(next))
            continue;
        return !unicode_is_cased(next);
    }
    return true;
}

static bool append_case_mapping(codepoint_buffer *out, uint32_t codepoint,
                                string_case_mode mode,
                                const codepoint_buffer *input, size_t index)
{
    static const uint32_t final_sigma[] = {0x03C2};
    const uint32_t *mapping;
    size_t length;

    if (mode == STRING_CASE_DOWN && codepoint == 0x03A3 &&
        final_sigma_context(input, index)) {
        mapping = final_sigma;
        length = 1;
    } else {
        bool found;
        if (mode == STRING_CASE_UP)
            found = unicode_full_upcase(codepoint, &mapping, &length);
        else if (mode == STRING_CASE_DOWN)
            found = unicode_full_downcase(codepoint, &mapping, &length);
        else
            found = unicode_full_titlecase(codepoint, &mapping, &length);
        if (!found) {
            mapping = &codepoint;
            length = 1;
        }
    }

    for (size_t i = 0; i < length; i++) {
        if (!cp_buffer_append(out, mapping[i]))
            return false;
    }
    return true;
}

static bool decode_string_to_codepoints(const char *s, codepoint_buffer *out,
                                        const char *name)
{
    size_t byte_len = strlen(s);
    size_t offset = 0;
    while (offset < byte_len) {
        uint32_t codepoint;
        if (!utf8_decode_next_codepoint_string(s, byte_len, &offset,
                                               &codepoint, name))
            return false;
        if (!cp_buffer_append(out, codepoint)) {
            show_error("%s: out of memory", name);
            return false;
        }
    }
    return true;
}

static unsigned make_string_from_codepoints(const codepoint_buffer *codepoints,
                                            const char *name)
{
    codepoint_buffer bytes;
    cp_buffer_init(&bytes);
    for (size_t i = 0; i < codepoints->length; i++) {
        if (!utf8_encode_codepoint_string(codepoints->data[i], &bytes)) {
            cp_buffer_free(&bytes);
            show_error("%s: invalid Unicode scalar value", name);
            return TOK_ERROR;
        }
    }
    if (!cp_buffer_append(&bytes, 0)) {
        cp_buffer_free(&bytes);
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }

    char *result = checked_malloc_size(bytes.length);
    if (!result) {
        cp_buffer_free(&bytes);
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    for (size_t i = 0; i < bytes.length; i++)
        result[i] = (char)bytes.data[i];

    cp_buffer_free(&bytes);
    return make_string_owned(result);
}

static unsigned cased_string_value(unsigned arg, string_case_mode mode,
                                   const char *name)
{
    char *s = require_string_ptr(arg, name);
    if (!s)
        return TOK_ERROR;

    codepoint_buffer input;
    cp_buffer_init(&input);
    if (!decode_string_to_codepoints(s, &input, name)) {
        cp_buffer_free(&input);
        return TOK_ERROR;
    }

    codepoint_buffer mapped;
    cp_buffer_init(&mapped);
    bool at_word_start = true;
    for (size_t i = 0; i < input.length; i++) {
        uint32_t codepoint = input.data[i];
        string_case_mode char_mode = mode;
        if (mode == STRING_CASE_TITLE) {
            if (unicode_is_cased(codepoint)) {
                char_mode = at_word_start ? STRING_CASE_TITLE : STRING_CASE_DOWN;
                at_word_start = false;
            } else if (!unicode_is_case_ignorable(codepoint)) {
                at_word_start = true;
            }
        }
        if (!append_case_mapping(&mapped, codepoint, char_mode, &input, i)) {
            cp_buffer_free(&mapped);
            cp_buffer_free(&input);
            show_error("%s: out of memory", name);
            return TOK_ERROR;
        }
        if (mode != STRING_CASE_TITLE && unicode_is_cased(codepoint))
            at_word_start = false;
    }

    unsigned result = make_string_from_codepoints(&mapped, name);
    cp_buffer_free(&mapped);
    cp_buffer_free(&input);
    return result;
}

unsigned prim_string_normalize(unsigned prim_id, unsigned argc,
                               unsigned *argv)
{
    const char *name;
    bool compatibility = false;
    bool compose = false;
    switch (prim_id) {
    case PSTRNFD:
        name = "string-normalize-nfd";
        break;
    case PSTRNFC:
        name = "string-normalize-nfc";
        compose = true;
        break;
    case PSTRNFKD:
        name = "string-normalize-nfkd";
        compatibility = true;
        break;
    case PSTRNFKC:
        name = "string-normalize-nfkc";
        compatibility = true;
        compose = true;
        break;
    case PSTRFOLD:
        name = "string-foldcase";
        REQUIRE_ARGC(argc, 1, 1, name);
        return folded_string_value(argv[0], name);
    case PSTRUP:
        name = "string-upcase";
        REQUIRE_ARGC(argc, 1, 1, name);
        return cased_string_value(argv[0], STRING_CASE_UP, name);
    case PSTRDOWN:
        name = "string-downcase";
        REQUIRE_ARGC(argc, 1, 1, name);
        return cased_string_value(argv[0], STRING_CASE_DOWN, name);
    case PSTRTITLE:
        name = "string-titlecase";
        REQUIRE_ARGC(argc, 1, 1, name);
        return cased_string_value(argv[0], STRING_CASE_TITLE, name);
    default:
        show_error("string normalization primitive: unsupported id %u",
                   prim_id);
        return TOK_ERROR;
    }
    REQUIRE_ARGC(argc, 1, 1, name);
    return normalized_string_value(argv[0], compatibility, compose, name);
}
