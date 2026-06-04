#include "utf8.h"
#include "context.h"
#include <string.h>

bool scheme_utf8_decode_next(const char *s, size_t byte_len, size_t *offset,
                             uint32_t *codepoint, const char **error_msg)
{
    if (*offset >= byte_len) {
        *error_msg = "invalid UTF-8 offset";
        return false;
    }

    const unsigned char *bytes = (const unsigned char *)s;
    unsigned char b0 = bytes[*offset];
    if (b0 == 0) {
        *error_msg = "null character in string";
        return false;
    }
    if (b0 < 0x80) {
        *codepoint = b0;
        (*offset)++;
        return true;
    }

    size_t needed;
    uint32_t value;
    if (b0 >= 0xC2 && b0 <= 0xDF) {
        needed = 2;
        value = b0 & 0x1F;
    } else if (b0 >= 0xE0 && b0 <= 0xEF) {
        needed = 3;
        value = b0 & 0x0F;
    } else if (b0 >= 0xF0 && b0 <= 0xF4) {
        needed = 4;
        value = b0 & 0x07;
    } else {
        *error_msg = "invalid UTF-8 leading byte";
        return false;
    }

    if (byte_len - *offset < needed) {
        *error_msg = "truncated UTF-8 sequence";
        return false;
    }
    for (size_t i = 1; i < needed; i++) {
        unsigned char b = bytes[*offset + i];
        if ((b & 0xC0) != 0x80) {
            *error_msg = "invalid UTF-8 continuation byte";
            return false;
        }
        value = (value << 6) | (uint32_t)(b & 0x3F);
    }

    if ((needed == 3 && value < 0x800) ||
        (needed == 4 && value < 0x10000) ||
        !is_valid_unicode_scalar((int64_t)value)) {
        *error_msg = "invalid UTF-8 sequence";
        return false;
    }

    *codepoint = value;
    *offset += needed;
    return true;
}

bool scheme_utf8_count_chars(const char *s, size_t *chars,
                             const char **error_msg)
{
    size_t byte_len = strlen(s);
    size_t offset = 0;
    size_t count = 0;
    while (offset < byte_len) {
        uint32_t codepoint;
        if (!scheme_utf8_decode_next(s, byte_len, &offset, &codepoint,
                                     error_msg))
            return false;
        count++;
    }
    *chars = count;
    return true;
}

bool scheme_utf8_byte_offset_for_index(const char *s, size_t char_index,
                                       bool allow_end, size_t *byte_offset,
                                       const char **error_msg)
{
    size_t byte_len = strlen(s);
    size_t offset = 0;
    size_t count = 0;
    while (offset < byte_len && count < char_index) {
        uint32_t codepoint;
        if (!scheme_utf8_decode_next(s, byte_len, &offset, &codepoint,
                                     error_msg))
            return false;
        count++;
    }
    if (count == char_index && (allow_end || offset < byte_len)) {
        *byte_offset = offset;
        return true;
    }
    *error_msg = "index out of bounds";
    return false;
}

bool scheme_utf8_encode_scalar(uint32_t codepoint, char out[4], size_t *len)
{
    if (codepoint == 0 || !is_valid_unicode_scalar((int64_t)codepoint))
        return false;
    if (codepoint <= 0x7F) {
        out[0] = (char)codepoint;
        *len = 1;
    } else if (codepoint <= 0x7FF) {
        out[0] = (char)(0xC0 | (codepoint >> 6));
        out[1] = (char)(0x80 | (codepoint & 0x3F));
        *len = 2;
    } else if (codepoint <= 0xFFFF) {
        out[0] = (char)(0xE0 | (codepoint >> 12));
        out[1] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
        out[2] = (char)(0x80 | (codepoint & 0x3F));
        *len = 3;
    } else {
        out[0] = (char)(0xF0 | (codepoint >> 18));
        out[1] = (char)(0x80 | ((codepoint >> 12) & 0x3F));
        out[2] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
        out[3] = (char)(0x80 | (codepoint & 0x3F));
        *len = 4;
    }
    return true;
}
