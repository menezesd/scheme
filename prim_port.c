/**
 * @file prim_port.c
 * @brief Port operations (open/close ports, string ports, port predicates)
 */

#include "prim_internal.h"

unsigned apply_port_primitive(unsigned prim_id, unsigned args)
{
    switch (prim_id) {
    case POPENINPUT: {
        REQUIRE_ARGS(args, 1, 1, "open-input-file");
        CHECK_STRING(car(args), "open-input-file");
        char *filename = GET_STRING_PTR(car(args));
        FILE *f = fopen(filename, "r");
        if (!f) {
            show_error("open-input-file: cannot open %s", filename);
            return TOK_ERROR;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_INPORT;
        CELL_ID(p) = STORE_PTR(f);
        return p;
    }
    case POPENOUTPUT: {
        REQUIRE_ARGS(args, 1, 1, "open-output-file");
        CHECK_STRING(car(args), "open-output-file");
        char *filename = GET_STRING_PTR(car(args));
        FILE *f = fopen(filename, "w");
        if (!f) {
            show_error("open-output-file: cannot open %s", filename);
            return TOK_ERROR;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_OUTPORT;
        CELL_ID(p) = STORE_PTR(f);
        return p;
    }
    case PCLOSEINPUT:
    case PCLOSEOUTPUT: {
        const char *name =
            prim_id == PCLOSEINPUT ? "close-input-port" : "close-output-port";
        REQUIRE_ARGS(args, 1, 1, name);
        unsigned port = car(args);
        // Handle string ports
        if (IS_STRINPORT(port) || IS_STROUTPORT(port)) {
            string_port *sp = GET_STRPORT_PTR(port);
            if (sp)
                strport_free(sp);
            CELL_ID(port) = 0;
            return 0;
        }
        if (!IS_INPORT(port) && !IS_OUTPORT(port)) {
            show_error("%s: not a port", name);
            return TOK_ERROR;
        }
        FILE *f = GET_PORT_PTR(port);
        if (f && f != stdin && f != stdout)
            fclose(f);
        CELL_ID(port) = 0;
        return 0;
    }
    case PINPUTPORTP: {
        REQUIRE_ARGS(args, 1, 1, "input-port?");
        return IS_INPUT_PORT(car(args)) ? ctx.atom_true : 0;
    }
    case POUTPUTPORTP: {
        REQUIRE_ARGS(args, 1, 1, "output-port?");
        return IS_OUTPUT_PORT(car(args)) ? ctx.atom_true : 0;
    }
    case PCURRENTINPUT: {
        REQUIRE_ARGS(args, 0, 0, "current-input-port");
        unsigned p = alloc();
        CELL_TYPE(p) = BT_INPORT;
        CELL_ID(p) = STORE_PTR(ctx.current_input);
        return p;
    }
    case PCURRENTOUTPUT: {
        REQUIRE_ARGS(args, 0, 0, "current-output-port");
        unsigned p = alloc();
        CELL_TYPE(p) = BT_OUTPORT;
        CELL_ID(p) = STORE_PTR(ctx.current_output);
        return p;
    }
    // String ports
    case POPENOUTPUTSTRING: {
        REQUIRE_ARGS(args, 0, 0, "open-output-string");
        string_port *sp = strport_new();
        if (!sp) {
            show_error("open-output-string: out of memory");
            return TOK_ERROR;
        }
        unsigned p = alloc();
        CELL_TYPE(p) = BT_STROUTPORT;
        CELL_ID(p) = STORE_PTR(sp);
        return p;
    }
    case PGETOUTPUTSTRING: {
        REQUIRE_ARGS(args, 1, 1, "get-output-string");
        unsigned port = car(args);
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
        char *copy = malloc(sp->len + 1);
        if (!copy) {
            show_error("get-output-string: out of memory");
            return TOK_ERROR;
        }
        memcpy(copy, sp->data, sp->len + 1);
        return make_string_owned(copy);
    }
    case POPENINPUTSTRING: {
        REQUIRE_ARGS(args, 1, 1, "open-input-string");
        unsigned str = car(args);
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
        CELL_ID(p) = STORE_PTR(sp);
        return p;
    }
    case PSTRINGPORTP: {
        REQUIRE_ARGS(args, 1, 1, "string-port?");
        unsigned a = car(args);
        return (IS_STRINPORT(a) || IS_STROUTPORT(a)) ? ctx.atom_true : 0;
    }
    default:
        return TOK_ERROR;
    }
}
