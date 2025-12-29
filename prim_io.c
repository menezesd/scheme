/**
 * @file prim_io.c
 * @brief I/O operations (read, write, display, ports, string ports)
 */

#include "prim_internal.h"

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
            } else {
                display_obj_port(arg, fport);
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
            fflush(fport);
        }
        return arg;
    }
    case PNEWLINE: {
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
            fflush(port);
        } else {
            fprintf(ctx.current_output, "\n");
            fflush(ctx.current_output);
        }
        return 0;
    }
    case PREAD: {
        FILE *port;
        if (!extract_input_port(args, &port, "read"))
            return TOK_ERROR;
        return read_obj_port(port);
    }
    case PREADCHAR: {
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
        int c = (int)CELL_ID(car(args));
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
                   : 0;
    }
    case PCHARREADY: {
        FILE *port;
        if (!extract_input_port(args, &port, "char-ready?"))
            return TOK_ERROR;
        // For simplicity, always return true (full implementation would use
        // select/poll)
        (void)port;
        return ctx.atom_true;
    }
    default:
        return TOK_ERROR;
    }
}
