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

unsigned apply_io_primitive(unsigned prim_id, unsigned argc, unsigned *argv)
{
    switch (prim_id) {
    case PDISPLAY: {
        REQUIRE_ARGC(argc, 1, 2, "display");
        unsigned arg = argv[0];
        FILE *fport;
        string_port *sport;
        int port_index = (argc == 2) ? 1 : -1;
        int ptype = extract_port_argv(argv, port_index, PORT_OUTPUT, &fport,
                                      &sport, "display");
        if (ptype == -1) return TOK_ERROR;
        if (ptype == 1) {
            // String port: use open_memstream to capture output
            char *buf = NULL;
            size_t buflen = 0;
            FILE *memfp = open_memstream(&buf, &buflen);
            if (!memfp) {
                show_error("display: out of memory");
                return TOK_ERROR;
            }
            if (IS_STRING(arg)) {
                fprintf(memfp, "%s", GET_STRING_PTR(arg));
            } else {
                display_obj_port(arg, memfp);
            }
            if (fclose(memfp) != 0) {
                free(buf);
                show_error("display: write failed");
                return TOK_ERROR;
            }
            if (!strport_puts(sport, buf)) {
                free(buf);
                show_error("display: string port write failed");
                return TOK_ERROR;
            }
            free(buf);
        } else {
            if (IS_STRING(arg)) {
                fprintf(fport, "%s", GET_STRING_PTR(arg));
                // Transcript
                if (ctx.transcript && fport == ctx.current_output) {
                    fprintf(ctx.transcript, "%s", GET_STRING_PTR(arg));
                }
            } else {
                display_obj_port(arg, fport);
                // Transcript
                if (ctx.transcript && fport == ctx.current_output) {
                    display_obj_port(arg, ctx.transcript);
                }
            }
            if (fflush(fport) != 0) {
                show_error("display: flush failed");
                return TOK_ERROR;
            }
        }
        return arg;
    }
    case PWRITE: {
        REQUIRE_ARGC(argc, 1, 2, "write");
        unsigned arg = argv[0];
        FILE *fport;
        string_port *sport;
        int port_index = (argc == 2) ? 1 : -1;
        int ptype = extract_port_argv(argv, port_index, PORT_OUTPUT, &fport,
                                      &sport, "write");
        if (ptype == -1) return TOK_ERROR;
        if (ptype == 1) {
            // String port: use open_memstream to capture output
            char *buf = NULL;
            size_t buflen = 0;
            FILE *memfp = open_memstream(&buf, &buflen);
            if (!memfp) {
                show_error("write: out of memory");
                return TOK_ERROR;
            }
            write_obj_port(arg, memfp);
            if (fclose(memfp) != 0) {
                free(buf);
                show_error("write: write failed");
                return TOK_ERROR;
            }
            if (!strport_puts(sport, buf)) {
                free(buf);
                show_error("write: string port write failed");
                return TOK_ERROR;
            }
            free(buf);
        } else {
            write_obj_port(arg, fport);
            // Transcript
            if (ctx.transcript && fport == ctx.current_output) {
                write_obj_port(arg, ctx.transcript);
            }
            if (fflush(fport) != 0) {
                show_error("write: flush failed");
                return TOK_ERROR;
            }
        }
        return arg;
    }
    case PNEWLINE: {
        REQUIRE_ARGC(argc, 0, 1, "newline");
        FILE *port;
        string_port *sport;
        int port_index = (argc == 1) ? 0 : -1;
        int ptype = extract_port_argv(argv, port_index, PORT_OUTPUT, &port,
                                      &sport, "newline");
        if (ptype == -1) return TOK_ERROR;

        if (ptype == 1) {
            if (!strport_putc(sport, '\n')) {
                show_error("newline: string port write failed");
                return TOK_ERROR;
            }
        } else {
            fprintf(port, "\n");
            if (ctx.transcript && port == ctx.current_output) {
                fprintf(ctx.transcript, "\n");
            }
            if (fflush(port) != 0) {
                show_error("newline: flush failed");
                return TOK_ERROR;
            }
        }
        return 0;
    }
    case PREAD: {
        REQUIRE_ARGC(argc, 0, 1, "read");
        FILE *fport;
        string_port *sport;
        int port_index = (argc == 1) ? 0 : -1;
        int ptype = extract_port_argv(argv, port_index, PORT_INPUT, &fport,
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
                reader_forget_port(mem);
                fclose(mem);
                show_error("read: failed to update string port position");
                return TOK_ERROR;
            }
            size_t pending = reader_port_pending_bytes(mem);
            if ((size_t)consumed < pending) {
                reader_forget_port(mem);
                fclose(mem);
                show_error("read: invalid reader pushback state");
                return TOK_ERROR;
            }
            sport->pos += (size_t)consumed - pending;
            reader_forget_port(mem);
            if (fclose(mem) != 0) {
                show_error("read: close failed");
                return TOK_ERROR;
            }
            return result;
        }
        return read_obj_port(fport);
    }
    case PREADCHAR: {
        REQUIRE_ARGC(argc, 0, 1, "read-char");
        FILE *fport;
        string_port *sport;
        int port_index = (argc == 1) ? 0 : -1;
        int ptype = extract_port_argv(argv, port_index, PORT_INPUT, &fport,
                                      &sport, "read-char");
        if (ptype == -1) return TOK_ERROR;
        int c;
        if (ptype == 1) {
            c = strport_getc(sport);
        } else {
            c = reader_port_getc(fport);
        }
        if (c == EOF) {
            if (ptype == 0 && ferror(fport)) {
                show_error("read-char: read failed");
                return TOK_ERROR;
            }
            return atom_from_string("eof-object");
        }
        return make_char(c);
    }
    case PPEEKCHAR: {
        REQUIRE_ARGC(argc, 0, 1, "peek-char");
        FILE *fport;
        string_port *sport;
        int port_index = (argc == 1) ? 0 : -1;
        int ptype = extract_port_argv(argv, port_index, PORT_INPUT, &fport,
                                      &sport, "peek-char");
        if (ptype == -1) return TOK_ERROR;
        int c;
        if (ptype == 1) {
            c = strport_peekc(sport);
        } else {
            c = reader_port_peekc(fport);
        }
        if (c == EOF) {
            if (ptype == 0 && ferror(fport)) {
                show_error("peek-char: read failed");
                return TOK_ERROR;
            }
            return atom_from_string("eof-object");
        }
        return make_char(c);
    }
    case PWRITECHAR: {
        REQUIRE_ARGC(argc, 1, 2, "write-char");
        CHECK_CHAR(argv[0], "write-char");
        int c = (unsigned char)CELL_ID(argv[0]);
        FILE *fport;
        string_port *sport;
        int port_index = (argc == 2) ? 1 : -1;
        int ptype = extract_port_argv(argv, port_index, PORT_OUTPUT, &fport,
                                      &sport, "write-char");
        if (ptype == -1) return TOK_ERROR;
        if (ptype == 1) {
            if (!strport_putc(sport, c)) {
                show_error("write-char: string port write failed");
                return TOK_ERROR;
            }
        } else {
            fputc(c, fport);
            if (fflush(fport) != 0) {
                show_error("write-char: flush failed");
                return TOK_ERROR;
            }
        }
        return 0;
    }
    case PEOF: {
        REQUIRE_ARGC(argc, 1, 1, "eof-object?");
        unsigned arg = argv[0];
        return (IS_ATOM(arg) &&
                strcmp(ctx.atom_table[CELL_ID(arg)], "eof-object") == 0)
                   ? ctx.atom_true
                   : ctx.atom_false;
    }
    case PCHARREADY: {
        REQUIRE_ARGC(argc, 0, 1, "char-ready?");
        FILE *fport;
        string_port *sport;
        int port_index = (argc == 1) ? 0 : -1;
        int ptype = extract_port_argv(argv, port_index, PORT_INPUT, &fport,
                                      &sport, "char-ready?");
        if (ptype == -1) return TOK_ERROR;

        if (ptype == 1) {
            // String port: ready if there are characters remaining
            return (sport->pos < sport->len) ? ctx.atom_true : ctx.atom_false;
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
        return (ret > 0 && (pfd.revents & POLLIN)) ? ctx.atom_true
                                                   : ctx.atom_false;
    }
    case PREADLINE: {
        REQUIRE_ARGC(argc, 0, 1, "read-line");
        FILE *fport;
        string_port *sport;
        int port_index = (argc == 1) ? 0 : -1;
        int ptype = extract_port_argv(argv, port_index, PORT_INPUT, &fport,
                                      &sport, "read-line");
        if (ptype == -1) return TOK_ERROR;

        // Build line in temporary buffer
        size_t cap = 128;
        size_t len = 0;
        char *buf = malloc(cap);
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
            if (len + 1 >= cap) {
                if (cap > SIZE_MAX / 2) {
                    free(buf);
                    show_error("read-line: line too long");
                    return TOK_ERROR;
                }
                cap *= 2;
                char *newbuf = realloc(buf, cap);
                if (!newbuf) {
                    free(buf);
                    show_error("read-line: out of memory");
                    return TOK_ERROR;
                }
                buf = newbuf;
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
