/**
 * @file prim_port.c
 * @brief Port operations (open/close ports, string ports, port predicates)
 */

#include "prim_internal.h"
#include "writer.h"

unsigned apply_port_primitive(unsigned prim_id, unsigned argc, unsigned *argv)
{
    switch (prim_id) {
    case POPENINPUT: {
        REQUIRE_ARGC(argc, 1, 1, "open-input-file");
        CHECK_STRING(argv[0], "open-input-file");
        char *filename = GET_STRING_PTR(argv[0]);
        FILE *f = fopen(filename, "r");
        if (!f) {
            show_error("open-input-file: cannot open %s", filename);
            return TOK_ERROR;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_INPORT;
        CELL_PTR(p) = f;
        return p;
    }
    case POPENOUTPUT: {
        REQUIRE_ARGC(argc, 1, 2, "open-output-file");
        CHECK_STRING(argv[0], "open-output-file");
        char *filename = GET_STRING_PTR(argv[0]);
        const char *mode = "w";
        if (argc > 1) {
            mode = IS_FALSE(argv[1]) ? "w" : "a";
        }
        FILE *f = fopen(filename, mode);
        if (!f) {
            show_error("open-output-file: cannot open %s", filename);
            return TOK_ERROR;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_OUTPORT;
        CELL_PTR(p) = f;
        return p;
    }
    case PCLOSEINPUT:
    case PCLOSEOUTPUT: {
        const char *name =
            prim_id == PCLOSEINPUT ? "close-input-port" : "close-output-port";
        REQUIRE_ARGC(argc, 1, 1, name);
        unsigned port = argv[0];
        // Handle string ports
        if (prim_id == PCLOSEINPUT && IS_STRINPORT(port)) {
            string_port *sp = GET_STRPORT_PTR(port);
            if (sp)
                strport_free(sp);
            CELL_ID(port) = 0;
            return 0;
        }
        if (prim_id == PCLOSEOUTPUT && IS_STROUTPORT(port)) {
            string_port *sp = GET_STRPORT_PTR(port);
            if (sp)
                strport_free(sp);
            CELL_ID(port) = 0;
            return 0;
        }
        if ((prim_id == PCLOSEINPUT && !IS_INPORT(port)) ||
            (prim_id == PCLOSEOUTPUT && !IS_OUTPORT(port))) {
            show_error("%s: not an %s port", name,
                       prim_id == PCLOSEINPUT ? "input" : "output");
            return TOK_ERROR;
        }
        FILE *f = GET_PORT_PTR(port);
        bool close_failed = false;
        if (f)
            reader_forget_port(f);
        if (f && f != stdin && f != stdout)
            close_failed = fclose(f) != 0;
        CELL_ID(port) = 0;
        if (close_failed) {
            show_error("%s: close failed", name);
            return TOK_ERROR;
        }
        return 0;
    }
    case PINPUTPORTP: {
        REQUIRE_ARGC(argc, 1, 1, "input-port?");
        return IS_INPUT_PORT(argv[0]) ? ctx.atom_true : ctx.atom_false;
    }
    case POUTPUTPORTP: {
        REQUIRE_ARGC(argc, 1, 1, "output-port?");
        return IS_OUTPUT_PORT(argv[0]) ? ctx.atom_true : ctx.atom_false;
    }
    case PCURRENTINPUT: {
        REQUIRE_ARGC(argc, 0, 0, "current-input-port");
        // Return rooted current port cell if active, otherwise wrap FILE*.
        if (ctx.current_input_cell != 0) {
            return ctx.current_input_cell;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_INPORT;
        CELL_PTR(p) = ctx.current_input;
        return p;
    }
    case PCURRENTOUTPUT: {
        REQUIRE_ARGC(argc, 0, 0, "current-output-port");
        // Return rooted current port cell if active, otherwise wrap FILE*.
        if (ctx.current_output_cell != 0) {
            return ctx.current_output_cell;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_OUTPORT;
        CELL_PTR(p) = ctx.current_output;
        return p;
    }
    // String ports
    case POPENOUTPUTSTRING: {
        REQUIRE_ARGC(argc, 0, 0, "open-output-string");
        string_port *sp = strport_new();
        if (!sp) {
            show_error("open-output-string: out of memory");
            return TOK_ERROR;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_STROUTPORT;
        CELL_PTR(p) = sp;
        return p;
    }
    case PGETOUTPUTSTRING: {
        REQUIRE_ARGC(argc, 1, 1, "get-output-string");
        unsigned port = argv[0];
        if (!IS_STROUTPORT(port)) {
            show_error("get-output-string: not a string output port");
            return TOK_ERROR;
        }
        string_port *sp = GET_STRPORT_PTR(port);
        if (!sp) {
            show_error("get-output-string: port is closed");
            return TOK_ERROR;
        }
        // Copy the string to a new BT_STRING cell
        if (sp->len == SIZE_MAX) {
            show_error("get-output-string: result too large");
            return TOK_ERROR;
        }
        char *copy = malloc(sp->len + 1);
        if (!copy) {
            show_error("get-output-string: out of memory");
            return TOK_ERROR;
        }
        memcpy(copy, sp->data, sp->len + 1);
        return make_string_owned(copy);
    }
    case POPENINPUTSTRING: {
        REQUIRE_ARGC(argc, 1, 1, "open-input-string");
        unsigned str = argv[0];
        if (!IS_STRING(str)) {
            show_error("open-input-string: not a string");
            return TOK_ERROR;
        }
        string_port *sp = strport_from_string(GET_STRING_PTR(str));
        if (!sp) {
            show_error("open-input-string: out of memory");
            return TOK_ERROR;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_STRINPORT;
        CELL_PTR(p) = sp;
        return p;
    }
    case PSTRINGPORTP: {
        REQUIRE_ARGC(argc, 1, 1, "string-port?");
        unsigned a = argv[0];
        return (IS_STRINPORT(a) || IS_STROUTPORT(a)) ? ctx.atom_true
                                                     : ctx.atom_false;
    }
    // Internal port setters (used by with-input-from-file etc.)
    case PSETCURRENTINPUT: {
        REQUIRE_ARGC(argc, 1, 1, "set-current-input-port!");
        unsigned port = argv[0];
        if (IS_INPORT(port)) {
            if (!GET_PORT_PTR(port)) {
                show_error("set-current-input-port!: port is closed");
                return TOK_ERROR;
            }
            ctx.current_input = GET_PORT_PTR(port);
            ctx.current_input_cell = port;
        } else if (IS_STRINPORT(port)) {
            if (!GET_STRPORT_PTR(port)) {
                show_error("set-current-input-port!: port is closed");
                return TOK_ERROR;
            }
            ctx.current_input_cell = port;
        } else {
            show_error("set-current-input-port!: not an input port, got %s",
                       type_name(port));
            return TOK_ERROR;
        }
        return port;
    }
    case PSETCURRENTOUTPUT: {
        REQUIRE_ARGC(argc, 1, 1, "set-current-output-port!");
        unsigned port = argv[0];
        if (IS_OUTPORT(port)) {
            if (!GET_PORT_PTR(port)) {
                show_error("set-current-output-port!: port is closed");
                return TOK_ERROR;
            }
            ctx.current_output = GET_PORT_PTR(port);
            ctx.current_output_cell = port;
        } else if (IS_STROUTPORT(port)) {
            if (!GET_STRPORT_PTR(port)) {
                show_error("set-current-output-port!: port is closed");
                return TOK_ERROR;
            }
            ctx.current_output_cell = port;
        } else {
            show_error("set-current-output-port!: not an output port, got %s",
                       type_name(port));
            return TOK_ERROR;
        }
        return port;
    }
    case PFLUSHOUTPUT: {
        REQUIRE_ARGC(argc, 0, 1, "flush-output-port");
        FILE *fport;
        string_port *sport;
        if (argc == 0) {
            int ptype = extract_port_argv(argv, -1, PORT_OUTPUT, &fport,
                                          &sport, "flush-output-port");
            if (ptype == -1) return TOK_ERROR;
            if (ptype == 0 && fflush(fport) != 0) {
                show_error("flush-output-port: flush failed");
                return TOK_ERROR;
            }
        } else {
            unsigned port = argv[0];
            if (IS_OUTPORT(port)) {
                fport = GET_PORT_PTR(port);
                if (!fport) {
                    show_error("flush-output-port: port is closed");
                    return TOK_ERROR;
                }
                if (fflush(fport) != 0) {
                    show_error("flush-output-port: flush failed");
                    return TOK_ERROR;
                }
            } else if (IS_STROUTPORT(port)) {
                sport = GET_STRPORT_PTR(port);
                if (!sport) {
                    show_error("flush-output-port: port is closed");
                    return TOK_ERROR;
                }
            } else {
                show_error("flush-output-port: not an output port, got %s",
                           type_name(port));
                return TOK_ERROR;
            }
        }
        return 0;
    }
    default:
        return TOK_ERROR;
    }
}
