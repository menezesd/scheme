#ifndef UTF8_H
#define UTF8_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

bool scheme_utf8_decode_next(const char *s, size_t byte_len, size_t *offset,
                             uint32_t *codepoint, const char **error_msg);
bool scheme_utf8_count_chars(const char *s, size_t *chars,
                             const char **error_msg);
bool scheme_utf8_byte_offset_for_index(const char *s, size_t char_index,
                                       bool allow_end, size_t *byte_offset,
                                       const char **error_msg);
bool scheme_utf8_encode_scalar(uint32_t codepoint, char out[4], size_t *len);

#endif
