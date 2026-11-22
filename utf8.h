#ifndef UTF8_H
#define UTF8_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Length of the UTF-8 sequence a lead byte starts: 1 to 4, or 0 for a byte
// that cannot start one (a continuation byte, or 0xC0, 0xC1, 0xF5-0xFF).
size_t scheme_utf8_sequence_length(unsigned char lead);
bool scheme_utf8_decode_next(const char *s, size_t byte_len, size_t *offset,
                             uint32_t *codepoint, const char **error_msg);
// Validate s[0, byte_len) as UTF-8 without U+0000, storing its character
// count and whether it is all ASCII through whichever pointers are non-NULL.
bool scheme_utf8_count_range(const char *s, size_t byte_len, size_t *chars,
                             bool *ascii, const char **error_msg);
bool scheme_utf8_count_chars(const char *s, size_t *chars,
                             const char **error_msg);
bool scheme_utf8_byte_offset_for_index(const char *s, size_t char_index,
                                       bool allow_end, size_t *byte_offset,
                                       const char **error_msg);
bool scheme_utf8_encode_scalar(uint32_t codepoint, char out[4], size_t *len);

#endif
