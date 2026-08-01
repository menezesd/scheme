/**
 * @file writer.c
 * @brief S-expression printer with cycle detection
 *
 * This file implements the Scheme writer, converting internal cell
 * representations back to readable text. Features:
 *
 * ## Output Modes
 * - write: Machine-readable output (strings quoted, chars as #\x)
 * - display: Human-readable output (strings unquoted, chars as themselves)
 *
 * ## Cycle Detection
 * Handles cyclic data structures using datum labels (#n= and #n#):
 * 1. First pass: Traverse structure, mark cells visited multiple times
 * 2. Second pass: Print with labels for shared/cyclic references
 *
 * Uses hash table for O(1) average lookup instead of O(n) linear search.
 *
 * ## Numeric Formatting
 * - Integers: Decimal notation
 * - Bignums: Arbitrary precision decimal
 * - Rationals: n/d notation
 * - Inexact: Standard floating point
 * - Complex: a+bi or a-bi notation
 */

#include "writer.h"
#include "bignum.h"
#include "bytecode.h"
#include "context.h"
#include <inttypes.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

// ============================================================================
// Type Names for Error Messages
// ============================================================================

const char *type_name(unsigned cell)
{
    if (IS_FIXNUM(cell))
        return "integer";
    if (cell == 0)
        return "()";
    if (!IS_CELL(cell))
        return "invalid-value";
    switch (CELL_TYPE(cell)) {
    case BT_FREE:
        return "free-cell";
    case BT_ATOM:
        return "symbol";
    case BT_NUM:
        return "integer";
    case BT_BIGNUM:
        return "bignum";
    case BT_INEXACT:
        return "float";
    case BT_RATIONAL:
        return "rational";
    case BT_COMPLEX:
        return "complex";
    case BT_STRING:
        return "string";
    case BT_CHAR:
        return "character";
    case BT_VECTOR:
        return "vector";
    case BT_BYTEVEC:
        return "bytevector";
    case BT_HASHTABLE:
        return "hash-table";
    case BT_CONS:
        return "pair";
    case BT_FUNCTION:
        return "procedure";
    case BT_MACRO:
        return "macro";
    case BT_SYNTAX:
        return "syntax-transformer";
    case BT_CONT:
        return "continuation";
    case BT_INPORT:
        return "input-port";
    case BT_OUTPORT:
        return "output-port";
    case BT_STRINPORT:
        return "string-input-port";
    case BT_STROUTPORT:
        return "string-output-port";
    case BT_MULTIVAL:
        return "multiple-values";
    case BT_BUILTIN:
        return "primitive";
    case BT_CLOSURE:
        return "bytecode-closure";
    case BT_VMCONT:
        return "vm-continuation";
    case BT_COMPILED_PATTERN:
        return "compiled-pattern";
    case BT_BROKENHEART:
        return "broken-heart";
    default:
        return "unknown";
    }
}

// ============================================================================
// Cycle Detection Hash Table
// ============================================================================

#define VISITED_INITIAL_CAP 8192
#define VISITED_EMPTY 0 // 0 is never a valid cell (represents nil)

typedef struct {
    unsigned cell; // Cell address (0 = empty slot)
    int label;     // -1 = visited once, >= 0 = assigned label
    bool printed;  // true if we've printed #n= prefix
    bool active;   // true while simple writer is printing this object
} visited_entry;

static visited_entry *visited_table;
static size_t visited_cap;
static int next_label = 0;
static size_t visited_count;
static bool writer_emit_shared_labels = true;
static bool writer_error = false;

// Hash function - good distribution for cell addresses
static inline unsigned hash_cell(unsigned cell)
{
    unsigned h = cell;
    h ^= h >> 16;
    h *= 0x85ebca6b;
    h ^= h >> 13;
    h *= 0xc2b2ae35;
    h ^= h >> 16;
    return h;
}

static bool resize_visited_table(size_t new_cap)
{
    size_t size;
    if (!checked_flex_size(0, new_cap, sizeof(*visited_table), &size))
        return false;
    visited_entry *new_table = calloc(1, size);
    if (!new_table)
        return false;

    for (size_t i = 0; i < visited_cap; i++) {
        if (visited_table[i].cell == VISITED_EMPTY)
            continue;
        size_t idx = (size_t)hash_cell(visited_table[i].cell) & (new_cap - 1);
        while (new_table[idx].cell != VISITED_EMPTY)
            idx = (idx + 1) & (new_cap - 1);
        new_table[idx] = visited_table[i];
    }

    free(visited_table);
    visited_table = new_table;
    visited_cap = new_cap;
    return true;
}

static bool ensure_visited_capacity(void)
{
    if (visited_cap != 0 && visited_count < visited_cap - visited_cap / 4)
        return true;

    size_t new_cap = VISITED_INITIAL_CAP;
    if (visited_cap != 0 &&
        !checked_grow_capacity_size(visited_cap, sizeof(*visited_table),
                                    &new_cap))
        return false;
    return resize_visited_table(new_cap);
}

static bool reset_visited(void)
{
    if (!ensure_visited_capacity())
        return false;
    memset(visited_table, 0, visited_cap * sizeof(*visited_table));
    next_label = 0;
    visited_count = 0;
    return true;
}

// Find entry in hash table, returns pointer or NULL if not found
static visited_entry *find_visited(unsigned cell)
{
    if (!visited_table)
        return NULL;
    size_t idx = (size_t)hash_cell(cell) & (visited_cap - 1);
    for (size_t i = 0; i < visited_cap; i++) {
        size_t probe = (idx + i) & (visited_cap - 1);
        if (visited_table[probe].cell == VISITED_EMPTY)
            return NULL; // Empty slot = not found
        if (visited_table[probe].cell == cell)
            return &visited_table[probe];
    }
    return NULL; // Table full (shouldn't happen)
}

// Insert new entry into hash table, returns pointer to entry
static visited_entry *insert_visited(unsigned cell)
{
    if (!ensure_visited_capacity()) {
        show_error("writer: structure too large");
        writer_error = true;
        return NULL;
    }

    size_t idx = (size_t)hash_cell(cell) & (visited_cap - 1);
    for (size_t i = 0; i < visited_cap; i++) {
        size_t probe = (idx + i) & (visited_cap - 1);
        if (visited_table[probe].cell == VISITED_EMPTY) {
            visited_table[probe].cell = cell;
            visited_table[probe].label = -1;
            visited_table[probe].printed = false;
            visited_table[probe].active = false;
            visited_count++;
            return &visited_table[probe];
        }
    }
    show_error("writer: structure too large");
    writer_error = true;
    return NULL;
}

static bool active_cell_push(unsigned **cells, size_t *len, size_t *cap,
                             unsigned cell)
{
    if (*len == *cap) {
        size_t new_cap;
        if (*cap == 0) {
            new_cap = 64;
        } else if (!checked_grow_capacity_size(*cap, sizeof(**cells),
                                                &new_cap)) {
            return false;
        }
        size_t size;
        if (!checked_flex_size(0, new_cap, sizeof(**cells), &size))
            return false;
        unsigned *new_cells = checked_realloc_size(*cells, size);
        if (!new_cells)
            return false;
        *cells = new_cells;
        *cap = new_cap;
    }
    (*cells)[(*len)++] = cell;
    return true;
}

// First pass: mark all cells that are visited more than once
static void mark_shared(unsigned s)
{
    // Follow the cdr spine iteratively.  This keeps large flat lists from
    // consuming C stack space while nested cars and vectors retain the normal
    // recursive traversal (the reader bounds their nesting depth).
    for (;;) {
        if (writer_error || s == 0 || s == TOK_ERROR || IS_FIXNUM(s) ||
            !IS_CELL(s) || is_bytecode_closure_object(s))
            return;

        // Only track cons cells and vectors (things that can have sharing).
        enum lisp_type type = CELL_TYPE(s);
        if (type != BT_CONS && type != BT_VECTOR)
            return;

        visited_entry *entry = find_visited(s);
        if (entry) {
            // Seen before - assign a label if not already assigned.
            if (entry->label < 0)
                entry->label = next_label++;
            return;
        }

        // First visit - add to hash table.
        if (!insert_visited(s))
            return;

        if (type == BT_CONS) {
            mark_shared(car(s));
            s = cdr(s);
            continue;
        }

        unsigned len = vector_len(s);
        unsigned *data = vector_data_ptr(s);
        if (len != 0 && !data)
            return;
        for (unsigned i = 0; i < len; i++) {
            mark_shared(data[i]);
            if (writer_error)
                return;
        }
        return;
    }
}

// Forward declaration
static void write_obj_fp(unsigned s, bool with_quotes, FILE *fp);

static void write_escaped_string(FILE *fp, const char *s)
{
    fputc('"', fp);
    for (const unsigned char *p = (const unsigned char *)s; *p; p++) {
        switch (*p) {
        case '"':
            fputs("\\\"", fp);
            break;
        case '\\':
            fputs("\\\\", fp);
            break;
        case '\n':
            fputs("\\n", fp);
            break;
        case '\t':
            fputs("\\t", fp);
            break;
        case '\r':
            fputs("\\r", fp);
            break;
        case '\a':
            fputs("\\a", fp);
            break;
        case '\b':
            fputs("\\b", fp);
            break;
        default:
            if (*p < 0x20 || *p == 0x7f)
                fprintf(fp, "\\x%02X;", *p);
            else
                fputc(*p, fp);
            break;
        }
    }
    fputc('"', fp);
}

static bool symbol_looks_numeric(const char *s)
{
    if (!s[0])
        return false;
    if ((s[0] == '+' || s[0] == '-') && s[1])
        s++;
    if (s[0] == '.' && s[1] == '\0')
        return false;
    return (s[0] >= '0' && s[0] <= '9') ||
           (s[0] == '.' && s[1] >= '0' && s[1] <= '9');
}

static bool symbol_needs_escape(const char *s)
{
    if (!s[0] || symbol_looks_numeric(s))
        return true;
    if (strcmp(s, ".") == 0)
        return true;
    unsigned char first = (unsigned char)s[0];
    if (first == '#' || first == '"' || first == '\'' || first == '`' ||
        first == ',' || first == '(' || first == ')' || first == ';')
        return true;
    for (const unsigned char *p = (const unsigned char *)s; *p; p++) {
        if (*p <= 0x20 || *p == 0x7f || *p == '"' || *p == '\'' ||
            *p == '`' || *p == ',' || *p == '(' || *p == ')' ||
            *p == ';' || *p == '|' || *p == '\\')
            return true;
        if (*p >= 'A' && *p <= 'Z')
            return true;
    }
    return false;
}

static void write_escaped_symbol(FILE *fp, const char *s)
{
    fputc('|', fp);
    for (const unsigned char *p = (const unsigned char *)s; *p; p++) {
        switch (*p) {
        case '|':
            fputs("\\|", fp);
            break;
        case '\\':
            fputs("\\\\", fp);
            break;
        case '\n':
            fputs("\\n", fp);
            break;
        case '\t':
            fputs("\\t", fp);
            break;
        case '\r':
            fputs("\\r", fp);
            break;
        case '\a':
            fputs("\\a", fp);
            break;
        case '\b':
            fputs("\\b", fp);
            break;
        default:
            if (*p < 0x20 || *p == 0x7f)
                fprintf(fp, "\\x%02X;", *p);
            else
                fputc(*p, fp);
            break;
        }
    }
    fputc('|', fp);
}

static void write_utf8_scalar(FILE *fp, int code)
{
    if (!is_valid_unicode_scalar(code))
        return;
    if (code < 0x80) {
        fputc(code, fp);
    } else if (code < 0x800) {
        fputc(0xC0 | (code >> 6), fp);
        fputc(0x80 | (code & 0x3F), fp);
    } else if (code < 0x10000) {
        fputc(0xE0 | (code >> 12), fp);
        fputc(0x80 | ((code >> 6) & 0x3F), fp);
        fputc(0x80 | (code & 0x3F), fp);
    } else {
        fputc(0xF0 | (code >> 18), fp);
        fputc(0x80 | ((code >> 12) & 0x3F), fp);
        fputc(0x80 | ((code >> 6) & 0x3F), fp);
        fputc(0x80 | (code & 0x3F), fp);
    }
}

static void write_character_literal(FILE *fp, int c)
{
    switch (c) {
    case '\a':
        fprintf(fp, "#\\alarm");
        break;
    case '\b':
        fprintf(fp, "#\\backspace");
        break;
    case 0x7f:
        fprintf(fp, "#\\delete");
        break;
    case 27:
        fprintf(fp, "#\\escape");
        break;
    case '\n':
        fprintf(fp, "#\\newline");
        break;
    case 0:
        fprintf(fp, "#\\null");
        break;
    case '\r':
        fprintf(fp, "#\\return");
        break;
    case ' ':
        fprintf(fp, "#\\space");
        break;
    case '\t':
        fprintf(fp, "#\\tab");
        break;
    default:
        if (c > 0x20 && c < 0x7f)
            fprintf(fp, "#\\%c", c);
        else
            fprintf(fp, "#\\x%X", (unsigned)c);
        break;
    }
}

static void write_list_tail_fp(unsigned st, bool with_quotes, FILE *fp)
{
    if (writer_error)
        return;
    if (st == 0)
        return;

    if (writer_emit_shared_labels) {
        unsigned opened_labeled_lists = 0;
        for (;;) {
            if (st == 0)
                break;

            visited_entry *entry = find_visited(st);
            if (entry && entry->label >= 0) {
                if (entry->printed) {
                    fprintf(fp, " . #%d#", entry->label);
                    break;
                }
                fprintf(fp, " . #%d=", entry->label);
                entry->printed = true;
                if (!IS_PAIR(st)) {
                    write_obj_fp(st, with_quotes, fp);
                    break;
                }
                fprintf(fp, "(");
                write_obj_fp(car(st), with_quotes, fp);
                if (writer_error)
                    break;
                opened_labeled_lists++;
                st = cdr(st);
                continue;
            }

            if (!IS_PAIR(st)) {
                fprintf(fp, " . ");
                write_obj_fp(st, with_quotes, fp);
                break;
            }

            fprintf(fp, " ");
            write_obj_fp(car(st), with_quotes, fp);
            if (writer_error)
                break;
            st = cdr(st);
        }
        while (opened_labeled_lists > 0) {
            fprintf(fp, ")");
            opened_labeled_lists--;
        }
        return;
    }

    unsigned *active_cells = NULL;
    size_t active_len = 0, active_cap = 0;
    for (;;) {
        if (st == 0)
            break;
        if (!IS_PAIR(st)) {
            fprintf(fp, " . ");
            write_obj_fp(st, with_quotes, fp);
            break;
        }

        visited_entry *entry = find_visited(st);
        if (entry && entry->active) {
            show_error("write-simple: circular structure");
            writer_error = true;
            break;
        }
        if (!entry)
            entry = insert_visited(st);
        if (!entry) {
            if (!writer_error) {
                show_error("write-simple: structure too large");
                writer_error = true;
            }
            break;
        }
        if (!active_cell_push(&active_cells, &active_len, &active_cap, st)) {
            show_error("write-simple: out of memory");
            writer_error = true;
            break;
        }
        entry->active = true;
        fprintf(fp, " ");
        write_obj_fp(car(st), with_quotes, fp);
        if (writer_error)
            break;
        st = cdr(st);
    }
    for (size_t i = 0; i < active_len; i++) {
        visited_entry *entry = find_visited(active_cells[i]);
        if (entry)
            entry->active = false;
    }
    free(active_cells);
}

static void write_obj_fp(unsigned s, bool with_quotes, FILE *fp)
{
    if (writer_error)
        return;
    if (s == 0) {
        fprintf(fp, "()");
        return;
    }
    if (s == TOK_ERROR) {
        fprintf(fp, "[ERROR]");
        return;
    }
    if (IS_FIXNUM(s)) {
        fprintf(fp, "%" PRId64, (int64_t)FIXNUM_VALUE(s));
        return;
    }
    if (!IS_CELL(s)) {
        fprintf(fp, "[invalid-value]");
        return;
    }
    if (is_bytecode_closure_object(s)) {
        fprintf(fp, "[function]");
        return;
    }

    enum lisp_type type = CELL_TYPE(s);
    bool simple_tracking = false;
    visited_entry *simple_entry = NULL;
    if (!writer_emit_shared_labels &&
        (type == BT_CONS || type == BT_VECTOR)) {
        simple_entry = find_visited(s);
        if (simple_entry && simple_entry->active) {
            show_error("write-simple: circular structure");
            writer_error = true;
            return;
        }
        if (!simple_entry)
            simple_entry = insert_visited(s);
        if (!simple_entry) {
            show_error("write-simple: structure too large");
            writer_error = true;
            return;
        }
        simple_entry->active = true;
        simple_tracking = true;
    }

    // Check for shared/circular structure
    visited_entry *entry = writer_emit_shared_labels ? find_visited(s) : NULL;
    if (entry && entry->label >= 0) {
        if (entry->printed) {
            // Already printed - use reference
            fprintf(fp, "#%d#", entry->label);
            return;
        }
        // First print - mark as printed and add label prefix
        entry->printed = true;
        fprintf(fp, "#%d=", entry->label);
    }

    switch (type) {
    case BT_ATOM:
        // Print #t and #f for boolean atoms
        if (s == ctx.atom_true) {
            fprintf(fp, "#t");
        } else if (s == ctx.atom_false) {
            fprintf(fp, "#f");
        } else if (!atom_is_valid(s)) {
            fprintf(fp, "[invalid-symbol]");
        } else {
            const char *name = ctx.atom_table[CELL_ID(s)];
            if (symbol_needs_escape(name))
                write_escaped_symbol(fp, name);
            else
                fprintf(fp, "%s", name);
        }
        break;
    case BT_NUM:
        fprintf(fp, "%" PRId64, CELL_ID(s));
        break;
    case BT_BIGNUM: {
        bignum *bn = get_bignum(s);
        if (bn) {
            char *str = bn_to_string(bn, 10);
            if (str) {
                fprintf(fp, "%s", str);
                free(str);
            } else {
                fprintf(fp, "[bignum]");
            }
        } else {
            fprintf(fp, "[bignum-null]");
        }
        break;
    }
    case BT_INEXACT: {
        union {
            double d;
            int64_t i;
        } u;
        u.i = CELL_ID(s);
        // Use %g but ensure inexact integers show decimal point (R5RS)
        // Check if value is a whole number
        if (u.d == (double)(long long)u.d && isfinite(u.d) && u.d >= -1e15 &&
            u.d <= 1e15) {
            // It's an inexact integer - show with .0
            fprintf(fp, "%.1f", u.d);
        } else {
            fprintf(fp, "%g", u.d);
        }
        break;
    }
    case BT_RATIONAL:
        if (!is_numeric(s)) {
            fprintf(fp, "[invalid-number]");
            break;
        }
        write_obj_fp(CELL_CAR(s), with_quotes, fp);
        fprintf(fp, "/");
        write_obj_fp(CELL_CDR(s), with_quotes, fp);
        break;
    case BT_COMPLEX: {
        if (!is_numeric(s)) {
            fprintf(fp, "[invalid-number]");
            break;
        }
        write_obj_fp(CELL_CAR(s), with_quotes, fp);
        unsigned imag = CELL_CDR(s);
        double imag_val = to_double(imag);
        if (imag_val >= 0)
            fprintf(fp, "+");
        write_obj_fp(imag, with_quotes, fp);
        fprintf(fp, "i");
        break;
    }
    case BT_STRING: {
        char *str = GET_STRING_PTR(s);
        if (!string_is_registered(str))
            break;
        if (with_quotes) {
            write_escaped_string(fp, str);
        } else {
            fprintf(fp, "%s", str);
        }
        break;
    }
    case BT_CHAR: {
        int c = GET_CHAR_CODE(s);
        if (with_quotes) {
            write_character_literal(fp, c);
        } else {
            write_utf8_scalar(fp, c);
        }
        break;
    }
    case BT_VECTOR: {
        fprintf(fp, "#(");
        unsigned len = vector_len(s);
        unsigned *data = vector_data_ptr(s);
        if (len != 0 && !data) {
            fprintf(fp, ")");
            break;
        }
        for (unsigned i = 0; i < len; i++) {
            if (i > 0)
                fprintf(fp, " ");
            write_obj_fp(data[i], with_quotes, fp);
        }
        fprintf(fp, ")");
        break;
    }
    case BT_BYTEVEC: {
        bytevec_data *bv = (bytevec_data *)CELL_PTR(s);
        fprintf(fp, "#u8(");
        if (bytevec_data_well_formed(bv)) {
            for (unsigned i = 0; i < bv->len; i++) {
                if (i > 0) fprintf(fp, " ");
                fprintf(fp, "%u", bv->data[i]);
            }
        }
        fprintf(fp, ")");
        break;
    }
    case BT_HASHTABLE:
        fprintf(fp, "#<hash-table>");
        break;
    case BT_CONS:
        fprintf(fp, "(");
        write_obj_fp(car(s), with_quotes, fp);
        write_list_tail_fp(cdr(s), with_quotes, fp);
        fprintf(fp, ")");
        break;
    case BT_FREE:
        fprintf(fp, "[NULL]");
        break;
    case BT_INPORT:
        fprintf(fp, "[input-port]");
        break;
    case BT_OUTPORT:
        fprintf(fp, "[output-port]");
        break;
    case BT_STRINPORT:
        fprintf(fp, "[string-input-port]");
        break;
    case BT_STROUTPORT:
        fprintf(fp, "[string-output-port]");
        break;
    case BT_FUNCTION:
        fprintf(fp, "[function]");
        break;
    case BT_MACRO:
        fprintf(fp, "[macro]");
        break;
    case BT_SYNTAX:
        fprintf(fp, "[syntax-transformer]");
        break;
    case BT_CONT:
        fprintf(fp, "[continuation]");
        break;
    case BT_VMCONT:
        fprintf(fp, "[continuation]");
        break;
    case BT_CLOSURE:
        fprintf(fp, "[bytecode-closure]");
        break;
    case BT_COMPILED_PATTERN:
        fprintf(fp, "[compiled-pattern]");
        break;
    case BT_BROKENHEART:
        fprintf(fp, "[broken-heart]");
        break;
    case BT_BUILTIN:
        fprintf(fp, "[builtin]");
        break;
    case BT_MULTIVAL: {
        // Print multiple values
        unsigned vals = CELL_CAR(s);
        if (!proper_list_silent(vals)) {
            fprintf(fp, "[invalid-values]");
            break;
        }
        bool first = true;
        for (; vals; vals = cdr(vals)) {
            if (!first)
                fprintf(fp, "\n");
            write_obj_fp(car(vals), with_quotes, fp);
            first = false;
        }
        break;
    }
    default:
        fprintf(fp, "[???]");
    }

    if (simple_tracking)
        simple_entry->active = false;
}

static bool begin_writer(bool emit_shared_labels)
{
    writer_error = false;
    writer_emit_shared_labels = emit_shared_labels;
    if (reset_visited())
        return true;
    show_error("writer: out of memory");
    writer_error = true;
    return false;
}

void write_obj(unsigned s)
{
    if (!begin_writer(true))
        return;
    mark_shared(s);
    write_obj_fp(s, true, stdout);
}

void display_obj(unsigned s)
{
    if (!begin_writer(true))
        return;
    mark_shared(s);
    write_obj_fp(s, false, stdout);
}

void write_obj_port(unsigned s, FILE *port)
{
    if (!begin_writer(true))
        return;
    mark_shared(s);
    write_obj_fp(s, true, port);
}

void write_shared_obj_port(unsigned s, FILE *port)
{
    if (!begin_writer(true))
        return;
    mark_shared(s);
    write_obj_fp(s, true, port);
}

void write_simple_obj_port(unsigned s, FILE *port)
{
    (void)write_simple_obj_port_checked(s, port);
}

bool write_simple_obj_port_checked(unsigned s, FILE *port)
{
    if (!begin_writer(false)) {
        writer_emit_shared_labels = true;
        return false;
    }
    write_obj_fp(s, true, port);
    writer_emit_shared_labels = true;
    return !writer_error;
}

void display_obj_port(unsigned s, FILE *port)
{
    if (!begin_writer(true))
        return;
    mark_shared(s);
    write_obj_fp(s, false, port);
}
