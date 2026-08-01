/**
 * @file prim_io.c
 * @brief I/O operations (read, write, display, ports, string ports)
 *
 * Implements Scheme I/O primitives:
 *
 * ## Output Operations
 * - display: Human-readable output (strings unquoted)
 * - write: Machine-readable output (strings quoted, escapes shown)
 * - newline: Output a newline character
 * - write-char: Output a single character
 *
 * ## Input Operations
 * - read: Parse an S-expression from input
 * - read-char: Read a single character
 * - peek-char: Look at next character without consuming
 * - char-ready?: Check if a character is available
 * - eof-object?: Test for end-of-file
 *
 * ## Port Types
 * - File ports: Standard FILE* based I/O
 * - String ports: In-memory I/O using string buffers
 * - Current ports: Dynamically bound current-input/output-port
 *
 * All operations support optional port argument; defaults to current port.
 */

#include "prim_internal.h"
#include "utf8.h"
#include <errno.h>
#include <poll.h>

static unsigned write_arg_to_string_port(unsigned arg, string_port *sport,
                                         const char *name, bool display)
{
    char *buf = NULL;
    size_t buflen = 0;
    FILE *memfp = open_memstream(&buf, &buflen);
    if (!memfp) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }

    if (display && IS_STRING(arg)) {
        char *s = require_string_ptr(arg, name);
        if (!s) {
            fclose(memfp);
            free(buf);
            return TOK_ERROR;
        }
        fprintf(memfp, "%s", s);
    } else if (display) {
        display_obj_port(arg, memfp);
    } else {
        write_obj_port(arg, memfp);
    }

    if (fclose(memfp) != 0) {
        free(buf);
        show_error("%s: write failed", name);
        return TOK_ERROR;
    }
    if (!strport_puts(sport, buf)) {
        free(buf);
        show_error("%s: string port write failed", name);
        return TOK_ERROR;
    }
    free(buf);
    return arg;
}

static bool flush_output_and_transcript(FILE *fport, const char *name)
{
    if (!flush_file_port(fport, name))
        return false;
    if (ctx.transcript && fport == ctx.current_output &&
        !flush_file_port(ctx.transcript, name))
        return false;
    return true;
}

static bool write_arg_to_file_port(unsigned arg, FILE *fport, const char *name,
                                   bool display)
{
    if (display && IS_STRING(arg)) {
        char *s = require_string_ptr(arg, name);
        if (!s)
            return false;
        fprintf(fport, "%s", s);
        if (ctx.transcript && fport == ctx.current_output)
            fprintf(ctx.transcript, "%s", s);
    } else if (display) {
        display_obj_port(arg, fport);
        if (ctx.transcript && fport == ctx.current_output)
            display_obj_port(arg, ctx.transcript);
    } else {
        write_obj_port(arg, fport);
        if (ctx.transcript && fport == ctx.current_output)
            write_obj_port(arg, ctx.transcript);
    }

    return flush_output_and_transcript(fport, name);
}

static unsigned write_arg_to_string_port_mode(unsigned arg, string_port *sport,
                                              const char *name,
                                              void (*writer)(unsigned, FILE *))
{
    char *buf = NULL;
    size_t buflen = 0;
    FILE *memfp = open_memstream(&buf, &buflen);
    if (!memfp) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }

    writer(arg, memfp);

    if (fclose(memfp) != 0) {
        free(buf);
        show_error("%s: write failed", name);
        return TOK_ERROR;
    }
    if (!strport_puts(sport, buf)) {
        free(buf);
        show_error("%s: string port write failed", name);
        return TOK_ERROR;
    }
    free(buf);
    return arg;
}

static unsigned write_arg_to_string_port_checked(
    unsigned arg, string_port *sport, const char *name,
    bool (*writer)(unsigned, FILE *))
{
    char *buf = NULL;
    size_t buflen = 0;
    FILE *memfp = open_memstream(&buf, &buflen);
    if (!memfp) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }

    bool ok = writer(arg, memfp);

    if (fclose(memfp) != 0) {
        free(buf);
        show_error("%s: write failed", name);
        return TOK_ERROR;
    }
    if (!ok) {
        free(buf);
        return TOK_ERROR;
    }
    if (!strport_puts(sport, buf)) {
        free(buf);
        show_error("%s: string port write failed", name);
        return TOK_ERROR;
    }
    free(buf);
    return arg;
}

static bool write_arg_to_file_port_mode(unsigned arg, FILE *fport,
                                        const char *name,
                                        void (*writer)(unsigned, FILE *))
{
    writer(arg, fport);
    if (ctx.transcript && fport == ctx.current_output)
        writer(arg, ctx.transcript);
    return flush_output_and_transcript(fport, name);
}

static bool write_arg_to_file_port_checked(unsigned arg, FILE *fport,
                                           const char *name,
                                           bool (*writer)(unsigned, FILE *))
{
    if (!writer(arg, fport))
        return false;
    if (ctx.transcript && fport == ctx.current_output) {
        if (!writer(arg, ctx.transcript))
            return false;
    }
    return flush_output_and_transcript(fport, name);
}

static bool write_newline_to_file_port(FILE *fport)
{
    fprintf(fport, "\n");
    if (ctx.transcript && fport == ctx.current_output)
        fprintf(ctx.transcript, "\n");
    return flush_output_and_transcript(fport, "newline");
}

static bool encode_char_utf8(int c, char encoded[4], size_t *encoded_len,
                             const char *name)
{
    if (!scheme_utf8_encode_scalar((uint32_t)c, encoded, encoded_len)) {
        show_error("%s: invalid character", name);
        return false;
    }
    return true;
}

static bool write_char_to_string_port(string_port *sport, int c,
                                      const char *name)
{
    if (c == 0) {
        show_error("%s: null character cannot be stored in a string", name);
        return false;
    }
    char encoded[4];
    size_t encoded_len;
    if (!encode_char_utf8(c, encoded, &encoded_len, name))
        return false;
    if (encoded_len > SIZE_MAX - sport->len - 1 ||
        !strport_ensure_capacity(sport, sport->len + encoded_len + 1)) {
        show_error("%s: string port write failed", name);
        return false;
    }
    memcpy(sport->data + sport->len, encoded, encoded_len);
    sport->len += encoded_len;
    sport->data[sport->len] = '\0';
    return true;
}

static bool write_char_to_file_port(FILE *fport, int c, const char *name)
{
    char encoded[4];
    size_t encoded_len;
    if (!encode_char_utf8(c, encoded, &encoded_len, name))
        return false;
    if (fwrite(encoded, 1, encoded_len, fport) != encoded_len ||
        (ctx.transcript && fport == ctx.current_output &&
         fwrite(encoded, 1, encoded_len, ctx.transcript) != encoded_len) ||
        !flush_output_and_transcript(fport, name)) {
        show_error("%s: write failed", name);
        return false;
    }
    return true;
}

static void close_reader_stream(FILE *port)
{
    reader_forget_port(port);
    fclose(port);
}

static int extract_optional_port(unsigned argc, unsigned *argv,
                                 unsigned argc_with_port, port_dir dir,
                                 FILE **fport, string_port **sport,
                                 const char *name)
{
    int port_index = (argc == argc_with_port) ? (int)argc_with_port - 1 : -1;
    return extract_port_argv(argv, port_index, dir, fport, sport, name);
}

static unsigned write_object_with_optional_port(unsigned argc, unsigned *argv,
                                                const char *name, bool display)
{
    unsigned arg = argv[0];
    FILE *fport;
    string_port *sport;
    int ptype = extract_optional_port(argc, argv, 2, PORT_OUTPUT, &fport,
                                      &sport, name);
    if (ptype == -1)
        return TOK_ERROR;
    if (ptype == 1)
        return write_arg_to_string_port(arg, sport, name, display);
    if (!write_arg_to_file_port(arg, fport, name, display))
        return TOK_ERROR;
    return arg;
}

static unsigned write_object_mode_with_optional_port(
    unsigned argc, unsigned *argv, const char *name,
    void (*writer)(unsigned, FILE *))
{
    unsigned arg = argv[0];
    FILE *fport;
    string_port *sport;
    int ptype = extract_optional_port(argc, argv, 2, PORT_OUTPUT, &fport,
                                      &sport, name);
    if (ptype == -1)
        return TOK_ERROR;
    if (ptype == 1)
        return write_arg_to_string_port_mode(arg, sport, name, writer);
    if (!write_arg_to_file_port_mode(arg, fport, name, writer))
        return TOK_ERROR;
    return arg;
}

static unsigned write_object_checked_with_optional_port(
    unsigned argc, unsigned *argv, const char *name,
    bool (*writer)(unsigned, FILE *))
{
    unsigned arg = argv[0];
    FILE *fport;
    string_port *sport;
    int ptype = extract_optional_port(argc, argv, 2, PORT_OUTPUT, &fport,
                                      &sport, name);
    if (ptype == -1)
        return TOK_ERROR;
    if (ptype == 1)
        return write_arg_to_string_port_checked(arg, sport, name, writer);
    if (!write_arg_to_file_port_checked(arg, fport, name, writer))
        return TOK_ERROR;
    return arg;
}

static bool reject_reader_token(unsigned result, const char *name)
{
    if (result == TOK_CLOSE || result == TOK_DOT) {
        show_error("%s: unexpected reader token", name);
        return false;
    }
    return true;
}

static size_t utf8_char_byte_len(unsigned char first)
{
    if (first < 0x80)
        return 1;
    if (first >= 0xC2 && first <= 0xDF)
        return 2;
    if (first >= 0xE0 && first <= 0xEF)
        return 3;
    if (first >= 0xF0 && first <= 0xF4)
        return 4;
    return 0;
}

/* Returns 1 for a character, 0 for EOF, and -1 for an invalid sequence. */
static int read_or_peek_utf8_char(FILE *fport, string_port *sport, int ptype,
                                  bool peek, uint32_t *codepoint,
                                  char bytes[4], size_t *byte_len,
                                  const char **error_msg)
{
    if (peek && ptype == 1) {
        if (sport->pos >= sport->len)
            return 0;
        size_t remaining = sport->len - sport->pos;
        size_t offset = 0;
        if (!scheme_utf8_decode_next(sport->data + sport->pos, remaining,
                                     &offset, codepoint, error_msg))
            return -1;
        *byte_len = offset;
        memcpy(bytes, sport->data + sport->pos, offset);
        return 1;
    }

    int c = ptype == 1 ? strport_getc(sport) : reader_port_getc(fport);
    if (c == EOF)
        return 0;
    bytes[0] = (char)c;
    *byte_len = utf8_char_byte_len((unsigned char)c);
    if (*byte_len == 0) {
        *error_msg = "invalid UTF-8 leading byte";
        *byte_len = 1;
        goto restore_peeked_bytes;
    }
    for (size_t i = 1; i < *byte_len; i++) {
        c = ptype == 1 ? strport_getc(sport) : reader_port_getc(fport);
        if (c == EOF) {
            *error_msg = "truncated UTF-8 sequence";
            *byte_len = i;
            goto restore_peeked_bytes;
        }
        bytes[i] = (char)c;
    }
    size_t offset = 0;
    if (!scheme_utf8_decode_next(bytes, *byte_len, &offset, codepoint,
                                 error_msg))
        goto restore_peeked_bytes;

    if (!peek)
        return 1;

restore_peeked_bytes:
    if (peek) {
        for (size_t i = *byte_len; i > 0; i--) {
            bool restored = ptype == 1
                ? (--sport->pos, true)
                : reader_port_ungetc(fport, (unsigned char)bytes[i - 1]);
            if (!restored) {
                *error_msg = "could not restore input while peeking";
                return -1;
            }
        }
    }
    return *error_msg ? -1 : 1;
}

static unsigned read_or_peek_char(FILE *fport, string_port *sport, int ptype,
                                  bool peek, const char *name)
{
    uint32_t codepoint;
    char bytes[4];
    size_t byte_len = 0;
    const char *utf8_error = NULL;
    int status = read_or_peek_utf8_char(fport, sport, ptype, peek, &codepoint,
                                        bytes, &byte_len, &utf8_error);
    if (status == 0) {
        if (ptype == 0 && ferror(fport)) {
            show_error("%s: read failed", name);
            return TOK_ERROR;
        }
        return atom_from_string("eof-object");
    }
    if (status < 0) {
        show_error("%s: %s", name,
                   utf8_error ? utf8_error : "invalid UTF-8 sequence");
        return TOK_ERROR;
    }
    return make_char((int)codepoint);
}

static unsigned read_or_peek_u8(FILE *fport, string_port *sport, int ptype,
                                bool peek, const char *name)
{
    int c;
    if (ptype == 1) {
        c = peek ? strport_peekc(sport) : strport_getc(sport);
    } else {
        c = peek ? reader_port_peekc(fport) : reader_port_getc(fport);
    }
    if (c == EOF) {
        if (ptype == 0 && ferror(fport)) {
            show_error("%s: read failed", name);
            return TOK_ERROR;
        }
        return atom_from_string("eof-object");
    }
    return store(c & 0xff);
}

static bool grow_read_buffer(char **buf, size_t *cap, const char *name,
                             const char *too_long)
{
    size_t new_cap;
    if (!checked_grow_capacity_size(*cap, 1, &new_cap)) {
        show_error("%s: %s", name, too_long);
        return false;
    }
    char *newbuf = checked_realloc_size(*buf, new_cap);
    if (!newbuf) {
        show_error("%s: out of memory", name);
        return false;
    }
    *buf = newbuf;
    *cap = new_cap;
    return true;
}

static unsigned read_string_value(unsigned count_arg, unsigned argc,
                                  unsigned *argv, const char *name)
{
    int64_t count;
    if (!expect_nonneg_int64(count_arg, &count, name))
        return TOK_ERROR;
    FILE *fport;
    string_port *sport;
    int ptype = extract_optional_port(argc, argv, 2, PORT_INPUT, &fport,
                                      &sport, name);
    if (ptype == -1)
        return TOK_ERROR;
    if (count == 0)
        return make_string_copy("");
    size_t cap = 16;
    char *buf = checked_malloc_size(cap);
    if (!buf) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    size_t n = 0;
    for (int64_t chars = 0; chars < count; chars++) {
        uint32_t codepoint;
        char bytes[4];
        size_t byte_len = 0;
        const char *utf8_error = NULL;
        int status = read_or_peek_utf8_char(fport, sport, ptype, false,
                                            &codepoint, bytes, &byte_len,
                                            &utf8_error);
        if (status == 0)
            break;
        if (status < 0) {
            free(buf);
            show_error("%s: %s", name,
                       utf8_error ? utf8_error : "invalid UTF-8 sequence");
            return TOK_ERROR;
        }
        if (codepoint == 0) {
            free(buf);
            show_error("%s: null character cannot be stored in a string",
                       name);
            return TOK_ERROR;
        }
        while (n > SIZE_MAX - byte_len - 1 || n + byte_len >= cap) {
            if (!grow_read_buffer(&buf, &cap, name, "string too long")) {
                free(buf);
                return TOK_ERROR;
            }
        }
        memcpy(buf + n, bytes, byte_len);
        n += byte_len;
    }
    if (ptype == 0 && ferror(fport)) {
        free(buf);
        show_error("%s: read failed", name);
        return TOK_ERROR;
    }
    if (n == 0) {
        free(buf);
        return atom_from_string("eof-object");
    }
    buf[n] = '\0';
    return make_string_owned(buf);
}

static unsigned write_string_value(unsigned argc, unsigned *argv,
                                   const char *name)
{
    char *str = require_string_ptr(argv[0], name);
    if (!str)
        return TOK_ERROR;
    size_t char_len;
    const char *utf8_error = NULL;
    if (!scheme_utf8_count_chars(str, &char_len, &utf8_error)) {
        show_error("%s: %s", name,
                   utf8_error ? utf8_error : "invalid UTF-8 string");
        return TOK_ERROR;
    }
    if (char_len >= UINT_MAX || char_len > INT64_MAX) {
        show_error("%s: string too long", name);
        return TOK_ERROR;
    }
    int64_t start = 0;
    int64_t end = (int64_t)char_len;
    unsigned port_index = 1;
    if (argc >= 3) {
        port_index = 1;
        if (!expect_index(argv[2], (unsigned)char_len + 1, &start, name))
            return TOK_ERROR;
        if (argc >= 4) {
            if (!expect_index(argv[3], (unsigned)char_len + 1, &end, name))
                return TOK_ERROR;
        }
    }
    if (start > end) {
        show_error("%s: invalid range", name);
        return TOK_ERROR;
    }
    size_t start_byte, end_byte;
    if (!scheme_utf8_byte_offset_for_index(str, (size_t)start, true,
                                           &start_byte, &utf8_error) ||
        !scheme_utf8_byte_offset_for_index(str, (size_t)end, true, &end_byte,
                                           &utf8_error)) {
        show_error("%s: %s", name,
                   utf8_error ? utf8_error : "invalid UTF-8 string");
        return TOK_ERROR;
    }
    FILE *fport;
    string_port *sport;
    int ptype = argc >= 2 ? extract_port_argv(argv, (int)port_index,
                                             PORT_OUTPUT, &fport, &sport, name)
                          : extract_port_argv(argv, -1, PORT_OUTPUT, &fport,
                                             &sport, name);
    if (ptype == -1)
        return TOK_ERROR;
    if (ptype == 1) {
        size_t slice_len = end_byte - start_byte;
        char *slice = checked_string_copy_len(str + start_byte, slice_len);
        if (!slice) {
            show_error("%s: out of memory", name);
            return TOK_ERROR;
        }
        bool ok = strport_puts(sport, slice);
        free(slice);
        if (!ok) {
            show_error("%s: string port write failed", name);
            return TOK_ERROR;
        }
    } else {
        size_t slice_len = end_byte - start_byte;
        if ((slice_len > 0 &&
             fwrite(str + start_byte, 1, slice_len, fport) != slice_len) ||
            (ctx.transcript && fport == ctx.current_output &&
             slice_len > 0 &&
             fwrite(str + start_byte, 1, slice_len, ctx.transcript) !=
                 slice_len) ||
            !flush_output_and_transcript(fport, name)) {
            show_error("%s: write failed", name);
            return TOK_ERROR;
        }
    }
    return 0;
}

unsigned apply_io_primitive(unsigned prim_id, unsigned argc, unsigned *argv)
{
    switch (prim_id) {
    case PDISPLAY: {
        REQUIRE_ARGC(argc, 1, 2, "display");
        return write_object_with_optional_port(argc, argv, "display", true);
    }
    case PWRITE: {
        REQUIRE_ARGC(argc, 1, 2, "write");
        return write_object_with_optional_port(argc, argv, "write", false);
    }
    case PWRITESHARED: {
        REQUIRE_ARGC(argc, 1, 2, "write-shared");
        return write_object_mode_with_optional_port(argc, argv, "write-shared",
                                                    write_shared_obj_port);
    }
    case PWRITESIMPLE: {
        REQUIRE_ARGC(argc, 1, 2, "write-simple");
        return write_object_checked_with_optional_port(
            argc, argv, "write-simple", write_simple_obj_port_checked);
    }
    case PNEWLINE: {
        REQUIRE_ARGC(argc, 0, 1, "newline");
        FILE *port;
        string_port *sport;
        int ptype = extract_optional_port(argc, argv, 1, PORT_OUTPUT, &port,
                                          &sport, "newline");
        if (ptype == -1) return TOK_ERROR;

        if (ptype == 1) {
            if (!write_char_to_string_port(sport, '\n', "newline"))
                return TOK_ERROR;
        } else {
            if (!write_newline_to_file_port(port))
                return TOK_ERROR;
        }
        return 0;
    }
    case PREAD: {
        REQUIRE_ARGC(argc, 0, 1, "read");
        FILE *fport;
        string_port *sport;
        int ptype = extract_optional_port(argc, argv, 1, PORT_INPUT, &fport,
                                          &sport, "read");
        if (ptype == -1) return TOK_ERROR;
        if (ptype == 1) {
            // String port: use fmemopen on remaining content
            size_t remaining = sport->len - sport->pos;
            if (remaining == 0) {
                return atom_from_string("eof-object");
            }
            FILE *mem = fmemopen(sport->data + sport->pos, remaining, "r");
            if (!mem) {
                show_error("read: failed to create memory stream");
                return TOK_ERROR;
            }
            unsigned result = read_obj_port(mem);
            // Update string port position based on how much was consumed
            long consumed = ftell(mem);
            if (consumed < 0) {
                close_reader_stream(mem);
                show_error("read: failed to update string port position");
                return TOK_ERROR;
            }
            size_t pending = reader_port_pending_bytes(mem);
            if ((size_t)consumed < pending) {
                close_reader_stream(mem);
                show_error("read: invalid reader pushback state");
                return TOK_ERROR;
            }
            sport->pos += (size_t)consumed - pending;
            reader_forget_port(mem);
            if (fclose(mem) != 0) {
                show_error("read: close failed");
                return TOK_ERROR;
            }
            if (!reject_reader_token(result, "read"))
                return TOK_ERROR;
            return result;
        }
        unsigned result = read_obj_port(fport);
        if (!reject_reader_token(result, "read"))
            return TOK_ERROR;
        return result;
    }
    case PREADCHAR: {
        REQUIRE_ARGC(argc, 0, 1, "read-char");
        FILE *fport;
        string_port *sport;
        int ptype = extract_optional_port(argc, argv, 1, PORT_INPUT, &fport,
                                          &sport, "read-char");
        if (ptype == -1) return TOK_ERROR;
        return read_or_peek_char(fport, sport, ptype, false, "read-char");
    }
    case PPEEKCHAR: {
        REQUIRE_ARGC(argc, 0, 1, "peek-char");
        FILE *fport;
        string_port *sport;
        int ptype = extract_optional_port(argc, argv, 1, PORT_INPUT, &fport,
                                          &sport, "peek-char");
        if (ptype == -1) return TOK_ERROR;
        return read_or_peek_char(fport, sport, ptype, true, "peek-char");
    }
    case PWRITECHAR: {
        REQUIRE_ARGC(argc, 1, 2, "write-char");
        CHECK_CHAR(argv[0], "write-char");
        int c = GET_CHAR_CODE(argv[0]);
        FILE *fport;
        string_port *sport;
        int ptype = extract_optional_port(argc, argv, 2, PORT_OUTPUT, &fport,
                                          &sport, "write-char");
        if (ptype == -1) return TOK_ERROR;
        if (ptype == 1) {
            if (!write_char_to_string_port(sport, c, "write-char"))
                return TOK_ERROR;
        } else {
            if (!write_char_to_file_port(fport, c, "write-char"))
                return TOK_ERROR;
        }
        return 0;
    }
    case PEOF: {
        REQUIRE_ARGC(argc, 1, 1, "eof-object?");
        unsigned arg = argv[0];
        return scheme_bool(atom_is_valid(arg) &&
                           strcmp(ctx.atom_table[CELL_ID(arg)],
                                  "eof-object") == 0);
    }
    case PEOFOBJECT:
        REQUIRE_ARGC(argc, 0, 0, "eof-object");
        return atom_from_string("eof-object");
    case PCHARREADY: {
        REQUIRE_ARGC(argc, 0, 1, "char-ready?");
        FILE *fport;
        string_port *sport;
        int ptype = extract_optional_port(argc, argv, 1, PORT_INPUT, &fport,
                                          &sport, "char-ready?");
        if (ptype == -1) return TOK_ERROR;

        if (ptype == 1) {
            // String port: ready if there are characters remaining
            return scheme_bool(sport->pos < sport->len);
        }
        if (reader_port_pending_bytes(fport) > 0)
            return ctx.atom_true;

        // File port: use poll() to check if data is available
        int fd = fileno(fport);
        if (fd < 0) {
            // Not a real file descriptor, assume ready
            return ctx.atom_true;
        }

        struct pollfd pfd = {.fd = fd, .events = POLLIN, .revents = 0};
        int ret = poll(&pfd, 1, 0); // Non-blocking check
        if (ret < 0) {
            if (errno == EINTR)
                return ctx.atom_false;
            show_error("char-ready?: poll failed");
            return TOK_ERROR;
        }
        if (pfd.revents & (POLLERR | POLLNVAL)) {
            show_error("char-ready?: port error");
            return TOK_ERROR;
        }
        return scheme_bool(ret > 0 && (pfd.revents & POLLIN));
    }
    case PU8READY: {
        REQUIRE_ARGC(argc, 0, 1, "u8-ready?");
        FILE *fport;
        string_port *sport;
        int ptype = extract_optional_port(argc, argv, 1, PORT_INPUT, &fport,
                                          &sport, "u8-ready?");
        if (ptype == -1) return TOK_ERROR;
        if (ptype == 1)
            return scheme_bool(sport->pos < sport->len);
        if (reader_port_pending_bytes(fport) > 0)
            return ctx.atom_true;
        int fd = fileno(fport);
        if (fd < 0)
            return ctx.atom_true;
        struct pollfd pfd = {.fd = fd, .events = POLLIN, .revents = 0};
        int ret = poll(&pfd, 1, 0);
        if (ret < 0) {
            if (errno == EINTR)
                return ctx.atom_false;
            show_error("u8-ready?: poll failed");
            return TOK_ERROR;
        }
        if (pfd.revents & (POLLERR | POLLNVAL)) {
            show_error("u8-ready?: port error");
            return TOK_ERROR;
        }
        return scheme_bool(ret > 0 && (pfd.revents & POLLIN));
    }
    case PPEEKU8: {
        REQUIRE_ARGC(argc, 0, 1, "peek-u8");
        FILE *fport;
        string_port *sport;
        int ptype = extract_optional_port(argc, argv, 1, PORT_INPUT, &fport,
                                          &sport, "peek-u8");
        if (ptype == -1) return TOK_ERROR;
        return read_or_peek_u8(fport, sport, ptype, true, "peek-u8");
    }
    case PREADSTRING: {
        REQUIRE_ARGC(argc, 1, 2, "read-string");
        return read_string_value(argv[0], argc, argv, "read-string");
    }
    case PWRITESTRING: {
        REQUIRE_ARGC(argc, 1, 4, "write-string");
        return write_string_value(argc, argv, "write-string");
    }
    case PREADLINE: {
        REQUIRE_ARGC(argc, 0, 1, "read-line");
        FILE *fport;
        string_port *sport;
        int ptype = extract_optional_port(argc, argv, 1, PORT_INPUT, &fport,
                                          &sport, "read-line");
        if (ptype == -1) return TOK_ERROR;

        // Build line in temporary buffer
        size_t cap = 128;
        size_t len = 0;
        char *buf = checked_malloc_size(cap);
        if (!buf) {
            show_error("read-line: out of memory");
            return TOK_ERROR;
        }

        int c;
        for (;;) {
            if (ptype == 1) {
                c = strport_getc(sport);
            } else {
                c = reader_port_getc(fport);
            }
            if (c == EOF || c == '\n')
                break;
            if (c == 0) {
                free(buf);
                show_error("read-line: null character in string");
                return TOK_ERROR;
            }
            if (c == '\r') {
                int next = ptype == 1 ? strport_peekc(sport)
                                      : reader_port_peekc(fport);
                if (next == '\n') {
                    if (ptype == 1)
                        (void)strport_getc(sport);
                    else
                        (void)reader_port_getc(fport);
                }
                break;
            }
            if (len + 1 >= cap) {
                if (!grow_read_buffer(&buf, &cap, "read-line", "line too long")) {
                    free(buf);
                    return TOK_ERROR;
                }
            }
            buf[len++] = (char)c;
        }

        if (c == EOF && ptype == 0 && ferror(fport)) {
            free(buf);
            show_error("read-line: read failed");
            return TOK_ERROR;
        }

        // EOF with no characters read -> return eof-object
        if (c == EOF && len == 0) {
            free(buf);
            return atom_from_string("eof-object");
        }

        buf[len] = '\0';
        size_t char_count;
        const char *utf8_error = NULL;
        if (!scheme_utf8_count_chars(buf, &char_count, &utf8_error)) {
            free(buf);
            show_error("read-line: %s",
                       utf8_error ? utf8_error : "invalid UTF-8 sequence");
            return TOK_ERROR;
        }
        return make_string_owned(buf);
    }
    case PEXIT: {
        // (exit) or (exit code)
        REQUIRE_ARGC(argc, 0, 1, "exit");
        int code = 0;
        if (argc == 1 &&
            !expect_exit_code(argv[0], &code, "exit")) {
                return TOK_ERROR;
        }
        exit(code);
    }
    default:
        return TOK_ERROR;
    }
}
