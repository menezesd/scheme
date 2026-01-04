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
#include <poll.h>

unsigned apply_io_primitive(unsigned prim_id, unsigned args)
{
    switch (prim_id) {
    case PDISPLAY: {
        REQUIRE_ARGS(args, 1, 2, "display");
        unsigned arg = car(args);
        FILE *fport;
        string_port *sport;
        int ptype = extract_output_port_ex(args, &fport, &sport, "display");
        if (ptype < 0)
            return TOK_ERROR;
        if (ptype == 1) {
            // String port: use open_memstream to capture output
            char *buf = NULL;
            size_t buflen = 0;
            FILE *memfp = open_memstream(&buf, &buflen);
            if (IS_STRING(arg)) {
                fprintf(memfp, "%s", GET_STRING_PTR(arg));
            } else {
                display_obj_port(arg, memfp);
            }
            fclose(memfp);
            strport_puts(sport, buf);
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
            fflush(fport);
        }
        return arg;
    }
    case PWRITE: {
        REQUIRE_ARGS(args, 1, 2, "write");
        unsigned arg = car(args);
        FILE *fport;
        string_port *sport;
        int ptype = extract_output_port_ex(args, &fport, &sport, "write");
        if (ptype < 0)
            return TOK_ERROR;
        if (ptype == 1) {
            // String port: use open_memstream to capture output
            char *buf = NULL;
            size_t buflen = 0;
            FILE *memfp = open_memstream(&buf, &buflen);
            write_obj_port(arg, memfp);
            fclose(memfp);
            strport_puts(sport, buf);
            free(buf);
        } else {
            write_obj_port(arg, fport);
            // Transcript
            if (ctx.transcript && fport == ctx.current_output) {
                write_obj_port(arg, ctx.transcript);
            }
            fflush(fport);
        }
        return arg;
    }
    case PNEWLINE: {
        REQUIRE_ARGS(args, 0, 1, "newline");
        // newline takes optional port as first arg, not second
        if (args) {
            unsigned p = car(args);
            if (IS_STROUTPORT(p)) {
                string_port *sport = GET_STRPORT_PTR(p);
                if (!sport) {
                    show_error("newline: port is closed");
                    return TOK_ERROR;
                }
                strport_putc(sport, '\n');
                return 0;
            }
            if (!IS_OUTPORT(p)) {
                show_error("newline: argument must be output port");
                return TOK_ERROR;
            }
            FILE *port = GET_PORT_PTR(p);
            fprintf(port, "\n");
            // Transcript
            if (ctx.transcript && port == ctx.current_output) {
                fprintf(ctx.transcript, "\n");
            }
            fflush(port);
        } else {
            // Use current output port (may be string port)
            if (ctx.current_output_cell != 0) {
                string_port *sport = GET_STRPORT_PTR(ctx.current_output_cell);
                if (sport)
                    strport_putc(sport, '\n');
            } else {
                fprintf(ctx.current_output, "\n");
                // Transcript
                if (ctx.transcript) {
                    fprintf(ctx.transcript, "\n");
                }
                fflush(ctx.current_output);
            }
        }
        return 0;
    }
    case PREAD: {
        REQUIRE_ARGS(args, 0, 1, "read");
        FILE *fport;
        string_port *sport;
        int ptype = extract_input_port_ex(args, &fport, &sport, "read");
        if (ptype < 0)
            return TOK_ERROR;
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
            sport->pos += ftell(mem);
            fclose(mem);
            return result;
        }
        return read_obj_port(fport);
    }
    case PREADCHAR: {
        REQUIRE_ARGS(args, 0, 1, "read-char");
        FILE *fport;
        string_port *sport;
        int ptype = extract_input_port_ex(args, &fport, &sport, "read-char");
        if (ptype < 0)
            return TOK_ERROR;
        int c;
        if (ptype == 1) {
            c = strport_getc(sport);
        } else {
            c = fgetc(fport);
        }
        if (c == EOF)
            return atom_from_string("eof-object");
        return make_char(c);
    }
    case PPEEKCHAR: {
        REQUIRE_ARGS(args, 0, 1, "peek-char");
        FILE *fport;
        string_port *sport;
        int ptype = extract_input_port_ex(args, &fport, &sport, "peek-char");
        if (ptype < 0)
            return TOK_ERROR;
        int c;
        if (ptype == 1) {
            c = strport_peekc(sport);
        } else {
            c = fgetc(fport);
            if (c != EOF)
                ungetc(c, fport);
        }
        if (c == EOF)
            return atom_from_string("eof-object");
        return make_char(c);
    }
    case PWRITECHAR: {
        REQUIRE_ARGS(args, 1, 2, "write-char");
        CHECK_CHAR(car(args), "write-char");
        int c = (unsigned char)CELL_ID(car(args));
        FILE *fport;
        string_port *sport;
        int ptype = extract_output_port_ex(args, &fport, &sport, "write-char");
        if (ptype < 0)
            return TOK_ERROR;
        if (ptype == 1) {
            strport_putc(sport, c);
        } else {
            fputc(c, fport);
            fflush(fport);
        }
        return 0;
    }
    case PEOF: {
        REQUIRE_ARGS(args, 1, 1, "eof-object?");
        unsigned arg = car(args);
        return (CELL_TYPE(arg) == BT_ATOM &&
                strcmp(ctx.atom_table[CELL_ID(arg)], "eof-object") == 0)
                   ? ctx.atom_true
                   : ctx.atom_false;
    }
    case PCHARREADY: {
        REQUIRE_ARGS(args, 0, 1, "char-ready?");
        FILE *fport;
        string_port *sport;
        int ptype = extract_input_port_ex(args, &fport, &sport, "char-ready?");
        if (ptype < 0)
            return TOK_ERROR;

        if (ptype == 1) {
            // String port: ready if there are characters remaining
            return (sport->pos < sport->len) ? ctx.atom_true : ctx.atom_false;
        }

        // File port: use poll() to check if data is available
        int fd = fileno(fport);
        if (fd < 0) {
            // Not a real file descriptor, assume ready
            return ctx.atom_true;
        }

        struct pollfd pfd = {.fd = fd, .events = POLLIN, .revents = 0};
        int ret = poll(&pfd, 1, 0); // Non-blocking check
        return (ret > 0 && (pfd.revents & POLLIN)) ? ctx.atom_true
                                                   : ctx.atom_false;
    }
    case PREADLINE: {
        REQUIRE_ARGS(args, 0, 1, "read-line");
        FILE *fport;
        string_port *sport;
        int ptype = extract_input_port_ex(args, &fport, &sport, "read-line");
        if (ptype < 0)
            return TOK_ERROR;

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
                c = fgetc(fport);
            }
            if (c == EOF || c == '\n')
                break;
            if (len + 1 >= cap) {
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

        // EOF with no characters read -> return eof-object
        if (c == EOF && len == 0) {
            free(buf);
            return atom_from_string("eof-object");
        }

        buf[len] = '\0';
        unsigned result = alloc();
        CELL_TYPE(result) = BT_STRING;
        CELL_ID(result) = (int64_t)(intptr_t)buf; // Transfer ownership
        return result;
    }
    case PEXIT: {
        // (exit) or (exit code)
        REQUIRE_ARGS(args, 0, 1, "exit");
        int code = 0;
        if (args) {
            unsigned arg = car(args);
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
