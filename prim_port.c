/**
 * @file prim_port.c
 * @brief Port operations (open/close ports, string ports, port predicates)
 */

#include "prim_internal.h"
#include "utf8.h"
#include "writer.h"

static void close_string_port(unsigned port)
{
    string_port *sp = GET_STRPORT_PTR(port);
    if (!string_port_is_registered(sp) || !string_port_well_formed(sp))
        return;
    sp->closed = true;
    // Output remains available through get-output-string after closing.
    // The GC reclaims the buffer when the port itself becomes unreachable.
    if (IS_STROUTPORT(port))
        return;
    /* Keep the registered backing object alive so a second close is harmless.
       Input ports can release their source immediately. */
    if (!sp->borrowed_data)
        free(sp->data);
    sp->data = NULL;
    sp->len = 0;
    sp->cap = 0;
    sp->pos = 0;
    sp->last_read_pos = 0;
    sp->last_read_len = 0;
    sp->last_read_source_pos = 0;
    sp->last_read_char = 0;
    sp->last_read_valid = false;
    sp->source_string = ctx.atom_false;
    sp->source_start = 0;
    sp->source_end = 0;
    sp->source_pos_chars = 0;
}

static unsigned close_port_arg(unsigned port, bool input)
{
    const char *name = input ? "close-input-port" : "close-output-port";
    const char *direction = input ? "input" : "output";

    if ((input && IS_STRINPORT(port)) || (!input && IS_STROUTPORT(port))) {
        close_string_port(port);
        return 0;
    }

    if ((input && !IS_INPORT(port)) || (!input && !IS_OUTPORT(port))) {
        show_error("%s: not an %s port", name, direction);
        return TOK_ERROR;
    }

    file_port *fp = GET_FILE_PORT_PTR(port);
    FILE *f = file_port_well_formed(fp) ? fp->file : NULL;
    bool close_failed = false;
    if (f)
        reader_forget_port(f);
    if (f && fp->owns_file)
        close_failed = fclose(f) != 0;
    if (file_port_well_formed(fp)) {
        fp->file = NULL;
    }
    if (close_failed) {
        show_error("%s: close failed", name);
        return TOK_ERROR;
    }
    return 0;
}

typedef struct {
    unsigned id;
    bool (*predicate)(unsigned value);
    const char *name;
} port_predicate_entry;

static bool input_port_predicate(unsigned value) { return IS_INPUT_PORT(value); }
static bool output_port_predicate(unsigned value)
{
    return IS_OUTPUT_PORT(value);
}
static bool string_port_predicate(unsigned value)
{
    return IS_STRINPORT(value) || IS_STROUTPORT(value);
}
static bool port_open_predicate(unsigned value)
{
    if (IS_INPORT(value) || IS_OUTPORT(value))
        return file_port_file(value) != NULL;
    if (IS_STRINPORT(value) || IS_STROUTPORT(value))
        return string_port_well_formed(GET_STRPORT_PTR(value));
    return false;
}
static bool input_port_open_predicate(unsigned value)
{
    return IS_INPUT_PORT(value) && port_open_predicate(value);
}
static bool output_port_open_predicate(unsigned value)
{
    return IS_OUTPUT_PORT(value) && port_open_predicate(value);
}
static bool textual_port_predicate(unsigned value)
{
    if (IS_STRINPORT(value) || IS_STROUTPORT(value))
        return true;
    if (IS_INPORT(value) || IS_OUTPORT(value)) {
        file_port *fp = GET_FILE_PORT_PTR(value);
        return file_port_well_formed(fp) && !fp->binary;
    }
    return false;
}
static bool binary_port_predicate(unsigned value)
{
    if (IS_INPORT(value) || IS_OUTPORT(value)) {
        file_port *fp = GET_FILE_PORT_PTR(value);
        return file_port_well_formed(fp) && fp->binary;
    }
    return false;
}

static const port_predicate_entry port_predicates[] = {
    {PINPUTPORTP, input_port_predicate, "input-port?"},
    {POUTPUTPORTP, output_port_predicate, "output-port?"},
    {PSTRINGPORTP, string_port_predicate, "string-port?"},
    {PPORTOPENP, port_open_predicate, "port-open?"},
    {PINPUTPORTOPENP, input_port_open_predicate, "input-port-open?"},
    {POUTPUTPORTOPENP, output_port_open_predicate, "output-port-open?"},
    {PTEXTUALPORTP, textual_port_predicate, "textual-port?"},
    {PBINARYPORTP, binary_port_predicate, "binary-port?"},
    {0, NULL, NULL},
};

static const port_predicate_entry *find_port_predicate(unsigned prim_id)
{
    for (const port_predicate_entry *entry = port_predicates; entry->predicate;
         entry++) {
        if (entry->id == prim_id)
            return entry;
    }
    return NULL;
}

static unsigned output_string_value(unsigned port, const char *name)
{
    if (!IS_STROUTPORT(port)) {
        show_error("%s: not a string output port", name);
        return TOK_ERROR;
    }
    string_port *sp = GET_STRPORT_PTR(port);
    if (!string_port_buffer_valid(sp)) {
        show_error("%s: invalid string output port", name);
        return TOK_ERROR;
    }
    char *copy = checked_string_copy_len(sp->data, sp->len);
    if (!copy) {
        show_error("%s: out of memory", name);
        return TOK_ERROR;
    }
    return make_string_immutable_owned(copy);
}

static unsigned set_current_port(unsigned port, unsigned prim_id)
{
    bool input = prim_id == PSETCURRENTINPUT;
    bool error = prim_id == PSETCURRENTERROR;
    const char *name = input ? "set-current-input-port!"
                            : error ? "set-current-error-port!"
                                    : "set-current-output-port!";
    const char *direction = input ? "input" : "output";
    FILE **current_file = input ? &ctx.current_input
                               : error ? &ctx.current_error : &ctx.current_output;
    unsigned *current_cell = input ? &ctx.current_input_cell
                                   : error ? &ctx.current_error_cell
                                           : &ctx.current_output_cell;

    if ((input && IS_INPORT(port)) || (!input && IS_OUTPORT(port))) {
        file_port *fp = GET_FILE_PORT_PTR(port);
        if (!file_port_well_formed(fp)) {
            show_error("%s: invalid port", name);
            return TOK_ERROR;
        }
        // Closed ports remain valid parameter values. I/O checks openness.
        *current_file = fp->file;
        *current_cell = port;
        return port;
    }

    if ((input && IS_STRINPORT(port)) || (!input && IS_STROUTPORT(port))) {
        if (!string_port_is_registered(GET_STRPORT_PTR(port))) {
            show_error("%s: invalid port", name);
            return TOK_ERROR;
        }
        *current_cell = port;
        return port;
    }

    /* Bytevector ports are represented by vectors in the standard library.
       The Scheme-level binary I/O wrappers use the current-port cell, so the
       setters must accept these ports even though they have no FILE *. */
    unsigned expected_bv_len = input ? 3 : 2;
    if (IS_VECTOR(port) && vector_data_well_formed(GET_VECTOR_PTR(port)) &&
        vector_len(port) == expected_bv_len) {
        unsigned *data = vector_data_ptr(port);
        unsigned tag = data[0];
        bool tag_matches = IS_ATOM(tag) &&
                           ((input && (CELL_ID(tag) == (unsigned)intern("bvin") ||
                                       CELL_ID(tag) == (unsigned)intern("bvin-closed"))) ||
                            (!input && (CELL_ID(tag) == (unsigned)intern("bvout") ||
                                        CELL_ID(tag) == (unsigned)intern("bvout-closed"))));
        bool payload_ok = input ? IS_BYTEVEC(data[1])
                                : (IS_PAIR(data[1]) || IS_NIL(data[1]));
        bool input_state_ok = false;
        if (input && payload_ok && IS_PAIR(data[2])) {
            int64_t start;
            int64_t end;
            bytevec_data *bv = (bytevec_data *)CELL_PTR(data[1]);
            input_state_ok = bytevec_data_well_formed(bv) &&
                             exact_int64_value(CELL_CAR(data[2]), &start) &&
                             exact_int64_value(CELL_CDR(data[2]), &end) &&
                             start >= 0 && end >= start &&
                             (uint64_t)end <= bv->len;
        }
        if (tag_matches && payload_ok && (!input || input_state_ok)) {
            *current_cell = port;
            return port;
        }
    }

    show_error("%s: not an %s port, got %s", name, direction, type_name(port));
    return TOK_ERROR;
}

static unsigned current_port(FILE *file, unsigned *cell, bool input,
                             const char *name)
{
    if (*cell == 0) {
        unsigned port = make_file_port_cell(file, false, input, false,
                                            input ? BT_INPORT : BT_OUTPORT, name);
        if (port == TOK_ERROR)
            return port;
        // Cache the initial wrapper so repeated calls return the same port.
        *cell = port;
    }
    return *cell;
}

static bool flush_output_port_arg(unsigned port)
{
    if (IS_OUTPORT(port)) {
        FILE *fport = file_port_file(port);
        if (!fport) {
            show_error("flush-output-port: port is closed");
            return false;
        }
        return flush_file_port(fport, "flush-output-port");
    }

    if (IS_STROUTPORT(port)) {
        string_port *sport = GET_STRPORT_PTR(port);
        if (!string_port_well_formed(sport)) {
            show_error("flush-output-port: port is closed");
            return false;
        }
        return true;
    }

    show_error("flush-output-port: not an output port, got %s",
               type_name(port));
    return false;
}

static bool flush_current_output_port(void)
{
    FILE *fport;
    string_port *sport;
    int ptype = extract_port_argv(NULL, -1, PORT_OUTPUT, &fport, &sport,
                                  "flush-output-port");
    if (ptype == -1)
        return false;
    return ptype != 0 || flush_file_port(fport, "flush-output-port");
}

unsigned apply_port_primitive(unsigned prim_id, unsigned argc, unsigned *argv)
{
    const port_predicate_entry *predicate = find_port_predicate(prim_id);
    if (predicate) {
        REQUIRE_ARGC(argc, 1, 1, predicate->name);
        return scheme_bool(predicate->predicate(argv[0]));
    }

    switch (prim_id) {
    case POPENINPUT: {
        REQUIRE_ARGC(argc, 1, 1, "open-input-file");
        char *filename = require_string_ptr(argv[0], "open-input-file");
        if (!filename)
            return TOK_ERROR;
        return open_file_port(filename, "r", BT_INPORT, "open-input-file");
    }
    case POPENOUTPUT: {
        REQUIRE_ARGC(argc, 1, 2, "open-output-file");
        char *filename = require_string_ptr(argv[0], "open-output-file");
        if (!filename)
            return TOK_ERROR;
        const char *mode = "w";
        if (argc > 1) {
            mode = IS_FALSE(argv[1]) ? "w" : "a";
        }
        return open_file_port(filename, mode, BT_OUTPORT, "open-output-file");
    }
    case PCLOSEINPUT:
    case PCLOSEOUTPUT: {
        const char *name =
            prim_id == PCLOSEINPUT ? "close-input-port" : "close-output-port";
        REQUIRE_ARGC(argc, 1, 1, name);
        return close_port_arg(argv[0], prim_id == PCLOSEINPUT);
    }
    case PCURRENTINPUT: {
        REQUIRE_ARGC(argc, 0, 0, "current-input-port");
        return current_port(ctx.current_input, &ctx.current_input_cell, true,
                            "current-input-port");
    }
    case PCURRENTOUTPUT: {
        REQUIRE_ARGC(argc, 0, 0, "current-output-port");
        return current_port(ctx.current_output, &ctx.current_output_cell, false,
                            "current-output-port");
    }
    case PCURRENTERROR: {
        REQUIRE_ARGC(argc, 0, 0, "current-error-port");
        return current_port(ctx.current_error, &ctx.current_error_cell, false,
                            "current-error-port");
    }
    // String ports
    case POPENOUTPUTSTRING: {
        REQUIRE_ARGC(argc, 0, 0, "open-output-string");
        string_port *sp = strport_new();
        if (!sp) {
            show_error("open-output-string: out of memory");
            return TOK_ERROR;
        }
        return make_pointer_cell(BT_STROUTPORT, sp);
    }
    case PGETOUTPUTSTRING: {
        REQUIRE_ARGC(argc, 1, 1, "get-output-string");
        return output_string_value(argv[0], "get-output-string");
    }
    case POPENINPUTSTRING: {
        REQUIRE_ARGC(argc, 1, 3, "open-input-string");
        char *str = require_string_ptr(argv[0], "open-input-string");
        if (!str)
            return TOK_ERROR;
        size_t char_len;
        const char *utf8_error = NULL;
        if (!scheme_utf8_count_chars(str, &char_len, &utf8_error) ||
            char_len > INT64_MAX) {
            show_error("open-input-string: %s",
                       utf8_error ? utf8_error : "string too long");
            return TOK_ERROR;
        }
        int64_t start = 0;
        int64_t end = (int64_t)char_len;
        if (argc >= 2 && !expect_nonneg_int64(argv[1], &start,
                                               "open-input-string"))
            return TOK_ERROR;
        if (argc >= 3 && !expect_nonneg_int64(argv[2], &end,
                                               "open-input-string"))
            return TOK_ERROR;
        if (start > end || (uint64_t)end > char_len) {
            show_error("open-input-string: invalid string range");
            return TOK_ERROR;
        }
        string_port *sp = strport_from_string_cell(
            argv[0], (size_t)start, (size_t)end);
        if (!sp) {
            show_error("open-input-string: invalid string range or out of memory");
            return TOK_ERROR;
        }
        return make_pointer_cell(BT_STRINPORT, sp);
    }
    // Internal port setters (used by with-input-from-file etc.)
    case PSETCURRENTINPUT: {
        REQUIRE_ARGC(argc, 1, 1, "set-current-input-port!");
        return set_current_port(argv[0], prim_id);
    }
    case PSETCURRENTOUTPUT: {
        REQUIRE_ARGC(argc, 1, 1, "set-current-output-port!");
        return set_current_port(argv[0], prim_id);
    }
    case PSETCURRENTERROR: {
        REQUIRE_ARGC(argc, 1, 1, "set-current-error-port!");
        return set_current_port(argv[0], prim_id);
    }
    case PFLUSHOUTPUT: {
        REQUIRE_ARGC(argc, 0, 1, "flush-output-port");
        if (argc == 0) {
            if (!flush_current_output_port())
                return TOK_ERROR;
        } else {
            if (!flush_output_port_arg(argv[0]))
                return TOK_ERROR;
        }
        return 0;
    }
    default:
        return TOK_ERROR;
    }
}
