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
        fprintf(memfp, "%s", GET_STRING_PTR(arg));
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

static bool write_arg_to_file_port(unsigned arg, FILE *fport, const char *name,
                                   bool display)
{
    if (display && IS_STRING(arg)) {
        fprintf(fport, "%s", GET_STRING_PTR(arg));
        if (ctx.transcript && fport == ctx.current_output)
            fprintf(ctx.transcript, "%s", GET_STRING_PTR(arg));
    } else if (display) {
        display_obj_port(arg, fport);
        if (ctx.transcript && fport == ctx.current_output)
            display_obj_port(arg, ctx.transcript);
    } else {
        write_obj_port(arg, fport);
        if (ctx.transcript && fport == ctx.current_output)
            write_obj_port(arg, ctx.transcript);
    }

    return flush_file_port(fport, name);
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

static bool write_arg_to_file_port_mode(unsigned arg, FILE *fport,
                                        const char *name,
                                        void (*writer)(unsigned, FILE *))
{
    writer(arg, fport);
    if (ctx.transcript && fport == ctx.current_output)
        writer(arg, ctx.transcript);
    return flush_file_port(fport, name);
}

static bool write_newline_to_file_port(FILE *fport)
{
    fprintf(fport, "\n");
    if (ctx.transcript && fport == ctx.current_output)
        fprintf(ctx.transcript, "\n");
    return flush_file_port(fport, "newline");
}

static bool write_char_to_string_port(string_port *sport, int c,
                                      const char *name)
{
    if (!strport_putc(sport, c)) {
        show_error("%s: string port write failed", name);
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

static bool reject_reader_token(unsigned result, const char *name)
{
    if (result == TOK_CLOSE || result == TOK_DOT) {
        show_error("%s: unexpected reader token", name);
        return false;
    }
    return true;
}

static unsigned read_or_peek_char(FILE *fport, string_port *sport, int ptype,
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
    return make_char(c);
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

static bool grow_read_line_buffer(char **buf, size_t *cap)
{
    size_t new_cap;
    if (!checked_grow_capacity_size(*cap, 1, &new_cap)) {
        show_error("read-line: line too long");
        return false;
    }
    char *newbuf = checked_realloc_size(*buf, new_cap);
    if (!newbuf) {
        show_error("read-line: out of memory");
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
    if ((uint64_t)count > SIZE_MAX - 1) {
        show_error("%s: count too large", name);
        return TOK_ERROR;
    }
    char *buf = checked_malloc_size((size_t)count + 1);
    if (!buf) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    size_t n = 0;
    while (n < (size_t)count) {
        int c = ptype == 1 ? strport_getc(sport) : reader_port_getc(fport);
        if (c == EOF)
            break;
        buf[n++] = (char)c;
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
    size_t len = strlen(str);
    int64_t start = 0;
    int64_t end = (int64_t)len;
    unsigned port_index = 1;
    if (argc >= 3) {
        port_index = 1;
        if (!expect_index(argv[2], (unsigned)len + 1, &start, name))
            return TOK_ERROR;
        if (argc >= 4) {
            if (!expect_index(argv[3], (unsigned)len + 1, &end, name))
                return TOK_ERROR;
        }
    }
    if (start > end) {
        show_error("%s: invalid range", name);
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
        size_t slice_len = (size_t)(end - start);
        char *slice = checked_string_copy_len(str + start, slice_len);
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
        size_t slice_len = (size_t)(end - start);
        if ((slice_len > 0 &&
             fwrite(str + start, 1, slice_len, fport) != slice_len) ||
            !flush_file_port(fport, name)) {
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
        return write_object_mode_with_optional_port(argc, argv, "write-simple",
                                                    write_simple_obj_port);
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
        int c = (unsigned char)CELL_ID(argv[0]);
        FILE *fport;
        string_port *sport;
        int ptype = extract_optional_port(argc, argv, 2, PORT_OUTPUT, &fport,
                                          &sport, "write-char");
        if (ptype == -1) return TOK_ERROR;
        if (ptype == 1) {
            if (!write_char_to_string_port(sport, c, "write-char"))
                return TOK_ERROR;
        } else {
            fputc(c, fport);
            if (!flush_file_port(fport, "write-char"))
                return TOK_ERROR;
        }
        return 0;
    }
    case PEOF: {
        REQUIRE_ARGC(argc, 1, 1, "eof-object?");
        unsigned arg = argv[0];
        return scheme_bool(IS_ATOM(arg) &&
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
                if (!grow_read_line_buffer(&buf, &cap)) {
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
        unsigned result = alloc();
        CELL_TYPE(result) = BT_STRING;
        CELL_PTR(result) = buf; // Transfer ownership
        return result;
    }
    case PEXIT: {
        // (exit) or (exit code)
        REQUIRE_ARGC(argc, 0, 1, "exit");
        int code = 0;
        if (argc == 1) {
            unsigned arg = argv[0];
            int64_t code64;
            if (!expect_exact_int64(arg, &code64, "exit")) {
                return TOK_ERROR;
            }
            code = (int)code64;
        }
        exit(code);
    }
    default:
        return TOK_ERROR;
    }
}
