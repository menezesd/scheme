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

// Strings are UTF-8 without a length header, so counting characters or
// finding the byte offset of an index is a scan from the start. For
// registered string buffers the result of the first count is cached in the
// buffer's registry slot: the character count plus one, with the high bit set
// when every character is ASCII (character and byte offsets then coincide).
// Counts that do not fit the slot are simply not cached.
#define STRING_INFO_ASCII 0x80000000u
#define STRING_INFO_COUNT_MASK 0x7FFFFFFFu

static bool count_chars_uncached(const char *s, size_t *chars, bool *ascii,
                                 const char **error_msg)
{
    size_t byte_len = strlen(s);
    size_t offset = 0;
    size_t count = 0;
    bool all_ascii = true;
    while (offset < byte_len) {
        uint32_t codepoint;
        if (!scheme_utf8_decode_next(s, byte_len, &offset, &codepoint,
                                     error_msg))
            return false;
        if (codepoint >= 0x80)
            all_ascii = false;
        count++;
    }
    *chars = count;
    *ascii = all_ascii;
    return true;
}

// Character count and ASCII-ness of a registered buffer, computed at most
// once. Returns false for unregistered pointers and invalid UTF-8, leaving
// the caller to take the uncached path (which reports the error).
static bool cached_string_info(const char *s, size_t *chars, bool *ascii)
{
    unsigned *slot = string_buffer_info_slot(s);
    if (!slot)
        return false;
    if (*slot == 0) {
        size_t count;
        bool all_ascii;
        const char *ignored = NULL;
        if (!count_chars_uncached(s, &count, &all_ascii, &ignored))
            return false;
        if (count >= STRING_INFO_COUNT_MASK)
            return false;
        *slot = (unsigned)(count + 1) | (all_ascii ? STRING_INFO_ASCII : 0);
    }
    *chars = (*slot & STRING_INFO_COUNT_MASK) - 1;
    *ascii = (*slot & STRING_INFO_ASCII) != 0;
    return true;
}

bool scheme_utf8_count_chars(const char *s, size_t *chars,
                             const char **error_msg)
{
    bool ascii;
    if (cached_string_info(s, chars, &ascii))
        return true;
    return count_chars_uncached(s, chars, &ascii, error_msg);
}

// The most recent index lookup in a non-ASCII buffer, so a left-to-right
// loop of string-ref calls resumes from the previous character instead of
// rescanning from the start. Valid while string_buffer_epoch is unchanged:
// a buffer address can only be reused after its string is unregistered.
static struct {
    const char *s;
    uint64_t epoch;
    size_t index;
    size_t offset;
} index_cursor;

static size_t utf8_sequence_length(unsigned char lead)
{
    if (lead < 0x80)
        return 1;
    if (lead < 0xE0)
        return 2;
    if (lead < 0xF0)
        return 3;
    return 4;
}

bool scheme_utf8_byte_offset_for_index(const char *s, size_t char_index,
                                       bool allow_end, size_t *byte_offset,
                                       const char **error_msg)
{
    size_t chars;
    bool ascii;
    if (cached_string_info(s, &chars, &ascii)) {
        if (char_index > chars || (char_index == chars && !allow_end)) {
            *error_msg = "index out of bounds";
            return false;
        }
        if (ascii) {
            *byte_offset = char_index;
            return true;
        }
        // The buffer was fully validated when its count was cached, so lead
        // bytes alone give each character's length.
        size_t index = 0, offset = 0;
        if (index_cursor.s == s && index_cursor.epoch == string_buffer_epoch &&
            index_cursor.index <= char_index) {
            index = index_cursor.index;
            offset = index_cursor.offset;
        }
        for (; index < char_index; index++)
            offset += utf8_sequence_length((unsigned char)s[offset]);
        index_cursor.s = s;
        index_cursor.epoch = string_buffer_epoch;
        index_cursor.index = index;
        index_cursor.offset = offset;
        *byte_offset = offset;
        return true;
    }

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
