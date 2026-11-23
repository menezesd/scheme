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
#include "utf8.h"
#include <inttypes.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

// ============================================================================
// Number Formatting
// ============================================================================

void format_double_repr(char *buf, size_t size, double d)
{
    if (isnan(d)) {
        snprintf(buf, size, "+nan.0");
        return;
    }
    if (isinf(d)) {
        snprintf(buf, size, d < 0 ? "-inf.0" : "+inf.0");
        return;
    }
    // Inexact integers show a decimal point (R5RS)
    if (d >= -1e15 && d <= 1e15 && d == (double)(long long)d) {
        snprintf(buf, size, "%.1f", d);
        return;
    }
    // Shortest precision that round-trips through the reader
    for (int prec = 1; prec <= 17; prec++) {
        snprintf(buf, size, "%.*g", prec, d);
        if (strtod(buf, NULL) == d)
            break;
    }
    // Integers beyond 1e15 can come out of %g in fixed notation with no
    // point or exponent, which would read back as an exact integer.
    size_t len = strlen(buf);
    if (!strpbrk(buf, ".e") && len + 2 < size)
        memcpy(buf + len, ".0", 3);
}

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
        return IS_BOOLEAN(cell) ? "boolean" : "symbol";
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
    case BT_RECORD: {
        unsigned rtd = record_rtd(cell);
        return rtd ? record_type_name_string(rtd) : "record";
    }
    case BT_RECORD_TYPE:
        return "record-type";
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
    case BT_EOF:
        return "eof-object";
    case BT_UNASSIGNED:
        return "unassigned";
    case BT_CLOSURE:
        return "bytecode-closure";
    case BT_VMCLOSURE:
        return "procedure";
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
// Records
// ============================================================================

// A promise's state is (#t . value), (#f . thunk) or ('forward . state), see
// stdlib.scm. True, with *value set, if the promise has been forced.
static bool promise_forced_value(unsigned promise, unsigned *value)
{
    // The registered type is the stdlib's, but nothing forces it to have
    // the field.
    if (vector_len(promise) <= RECORD_FIELDS_START)
        return false;
    unsigned state = vector_data_ptr(promise)[RECORD_FIELDS_START];
    unsigned forward = (unsigned)intern("forward");
    // force shortens these chains as it walks them; the bound only keeps a
    // corrupted one from hanging the printer.
    for (unsigned hops = 0; IS_PAIR(state) && hops < 1000000; hops++) {
        unsigned tag = car(state);
        if (IS_ATOM(tag) && (unsigned)CELL_ID(tag) == forward) {
            state = cdr(state);
            continue;
        }
        if (tag != ctx.atom_true)
            return false;
        *value = cdr(state);
        return true;
    }
    return false;
}

// The printed form of a record type's name: MIT drops the brackets of the
// conventional <name>.
static void write_record_type_name(FILE *fp, unsigned rtd)
{
    const char *name = record_type_name_string(rtd);
    size_t len = strlen(name);
    if (len > 2 && name[0] == '<' && name[len - 1] == '>')
        fprintf(fp, "%.*s", (int)(len - 2), name + 1);
    else
        fprintf(fp, "%s", name);
}

// MIT's condition-type names for the kinds of error object.
static const char *condition_type_name(unsigned condition)
{
    unsigned kind = vector_len(condition) > RECORD_FIELDS_START
                        ? vector_data_ptr(condition)[RECORD_FIELDS_START]
                        : 0;
    if (!atom_is_valid(kind))
        return "simple-error";
    const char *name = ctx.atom_table[CELL_ID(kind)];
    if (strcmp(name, "error") == 0)
        return "simple-error";
    if (strcmp(name, "file") == 0)
        return "file-error";
    if (strcmp(name, "read") == 0)
        return "read-error";
    return name;
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
    bool active;   // on the current marking path (cycles-only mode),
                   // or being printed by the simple writer
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
    // One huge write must not make every later write pay to clear a huge
    // table; drop back to the initial size instead.
    if (visited_cap > VISITED_INITIAL_CAP) {
        free(visited_table);
        visited_table = NULL;
        visited_cap = 0;
        visited_count = 0;
    }
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
// Car/vector traversal recurses on the C stack, and data built
// programmatically (unlike reader input) has no depth bound - fail cleanly
// instead of overflowing the stack
#define WRITER_MAX_DEPTH 10000
static int writer_depth = 0;

static void mark_shared_inner(unsigned s);

static void mark_shared(unsigned s)
{
    if (writer_depth >= WRITER_MAX_DEPTH) {
        if (!writer_error)
            show_error("write: nesting too deep");
        writer_error = true;
        return;
    }
    writer_depth++;
    mark_shared_inner(s);
    writer_depth--;
}

// write and display label only cells that are part of a cycle (R7RS: "Datum
// labels must not be used if there are no cycles"); write-shared labels
// every pair and vector that occurs more than once.
static bool writer_label_cycles_only = false;

// Cells on the current marking path, for cycles-only mode. A revisit of a
// cell that is still on the path closes a cycle; a revisit of a finished cell
// is plain sharing. Every cycle contains such a back edge, so labeling its
// target is enough for the printer to terminate.
static unsigned *mark_path = NULL;
static size_t mark_path_len = 0, mark_path_cap = 0;

static void mark_shared_inner(unsigned s)
{
    // Follow the cdr spine iteratively.  This keeps large flat lists from
    // consuming C stack space while nested cars and vectors retain the normal
    // recursive traversal.
    size_t path_base = mark_path_len;
    for (;;) {
        if (writer_error || s == 0 || s == TOK_ERROR || IS_FIXNUM(s) ||
            !IS_CELL(s) || is_bytecode_closure_object(s))
            break;

        // Only track cons cells and vectors (things that can have sharing),
        // and forced promises, which print their value.
        enum lisp_type type = CELL_TYPE(s);
        unsigned promise_value = 0;
        bool is_promise = type == BT_RECORD &&
                          record_is_builtin(s, BRT_PROMISE) &&
                          promise_forced_value(s, &promise_value);
        if (type != BT_CONS && type != BT_VECTOR && !is_promise)
            break;

        visited_entry *entry = find_visited(s);
        if (entry) {
            // Seen before - assign a label if not already assigned.
            if (entry->label < 0 &&
                (!writer_label_cycles_only || entry->active))
                entry->label = next_label++;
            break;
        }

        // First visit - add to hash table.
        entry = insert_visited(s);
        if (!entry)
            break;
        if (writer_label_cycles_only) {
            if (!active_cell_push(&mark_path, &mark_path_len, &mark_path_cap,
                                  s)) {
                show_error("write: out of memory");
                writer_error = true;
                break;
            }
            entry->active = true;
        }

        if (type == BT_CONS) {
            mark_shared(car(s));
            s = cdr(s);
            continue;
        }
        if (is_promise) {
            s = promise_value;
            continue;
        }

        unsigned len = vector_len(s);
        unsigned *data = vector_data_ptr(s);
        if (len != 0 && !data)
            break;
        for (unsigned i = 0; i < len && !writer_error; i++)
            mark_shared(data[i]);
        break;
    }
    // Leaving this spine: its cells are finished. Re-look-up each entry,
    // since inserts above may have resized the table.
    for (size_t i = path_base; i < mark_path_len; i++) {
        visited_entry *entry = find_visited(mark_path[i]);
        if (entry)
            entry->active = false;
    }
    mark_path_len = path_base;
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

// Unicode case folding (defined in prim_char.c, declared in prim_internal.h;
// redeclared here to avoid pulling the whole primitive-internal header)
uint32_t unicode_simple_foldcase(uint32_t code);
bool unicode_full_foldcase(uint32_t code, const uint32_t **mapping,
                           size_t *length);

static bool symbol_looks_numeric(const char *s)
{
    if (!s[0])
        return false;
    if ((s[0] == '+' || s[0] == '-') && s[1]) {
        s++;
        // Include complex literals starting with an infinity or NaN.
        if (strcmp(s, "i") == 0 || strncmp(s, "inf.0", 5) == 0 ||
            strncmp(s, "nan.0", 5) == 0)
            return true;
    }
    if (s[0] == '.' && s[1] == '\0')
        return false;
    return (s[0] >= '0' && s[0] <= '9') ||
           (s[0] == '.' && s[1] >= '0' && s[1] <= '9');
}

// True if the default fold-case reader would read this name back as a
// different symbol, so it must be pipe-escaped for a faithful round-trip
static bool symbol_folds_differently(const char *s)
{
    size_t len = strlen(s);
    size_t offset = 0;
    while (offset < len) {
        unsigned char b = (unsigned char)s[offset];
        if (b < 0x80) {
            if (b >= 'A' && b <= 'Z')
                return true;
            offset++;
            continue;
        }
        uint32_t code;
        if (!scheme_utf8_decode_next(s, len, &offset, &code, NULL))
            return true; // invalid UTF-8: escape to be safe
        const uint32_t *mapping;
        size_t map_len;
        if (unicode_full_foldcase(code, &mapping, &map_len)) {
            if (map_len != 1 || mapping[0] != code)
                return true;
        } else if (unicode_simple_foldcase(code) != code) {
            return true;
        }
    }
    return false;
}

static bool symbol_needs_escape(const char *s)
{
    if (!s[0] || symbol_looks_numeric(s))
        return true;
    // Leading '.' lexes as the dotted-pair marker (only "..." reads back)
    if (s[0] == '.' && strcmp(s, "...") != 0)
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
    }
    return symbol_folds_differently(s);
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
                if (!IS_PAIR(st)) {
                    // Let write_obj_fp emit the "#N=" prefix itself;
                    // marking printed here would make it emit "#N#" instead
                    fprintf(fp, " . ");
                    write_obj_fp(st, with_quotes, fp);
                    break;
                }
                fprintf(fp, " . #%d=", entry->label);
                entry->printed = true;
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

static void write_obj_fp_inner(unsigned s, bool with_quotes, FILE *fp);

static void write_obj_fp(unsigned s, bool with_quotes, FILE *fp)
{
    if (writer_depth >= WRITER_MAX_DEPTH) {
        if (!writer_error)
            show_error("write: nesting too deep");
        writer_error = true;
        return;
    }
    writer_depth++;
    write_obj_fp_inner(s, with_quotes, fp);
    writer_depth--;
}

static void write_obj_fp_inner(unsigned s, bool with_quotes, FILE *fp)
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
        (type == BT_CONS || type == BT_VECTOR ||
         (type == BT_RECORD && record_is_builtin(s, BRT_PROMISE)))) {
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
            // R7RS display: "Symbols are not escaped."
            if (with_quotes && symbol_needs_escape(name))
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
        char numbuf[NUMBER_BUF_SIZE];
        format_double_repr(numbuf, sizeof(numbuf), u.d);
        fprintf(fp, "%s", numbuf);
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
        // Like MIT Scheme, omit an exact zero real part and write an exact
        // unit imaginary part as a bare sign: +i, -2i, 3-i. Both forms read
        // back as the same number.
        unsigned real = CELL_CAR(s);
        if (!exact_integer_equals(real, 0))
            write_obj_fp(real, with_quotes, fp);
        unsigned imag = CELL_CDR(s);
        bool needs_plus;
        if (IS_INEXACT(imag)) {
            double value = to_double(imag);
            // Infinities, NaNs, and negative zero already print a sign.
            needs_plus = isfinite(value) && !signbit(value);
        } else {
            // Exact components can overflow or underflow a double, so
            // determine their sign in the exact tower.
            unsigned sign_value = IS_RATIONAL(imag) ? CELL_CAR(imag) : imag;
            needs_plus = !is_negative_number(sign_value);
        }
        if (needs_plus)
            fprintf(fp, "+");
        if (exact_integer_equals(imag, -1))
            fprintf(fp, "-");
        else if (!exact_integer_equals(imag, 1))
            write_obj_fp(imag, with_quotes, fp);
        fprintf(fp, "i");
        break;
    }
    case BT_STRING: {
        char *str = GET_STRING_PTR(s);
        if (!string_is_registered(str)) {
            fprintf(fp, "[invalid-string]");
            break;
        }
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
    // Records print like MIT's: #[point 12], numbered by object hash. The
    // runtime's own record types have the forms MIT gives them.
    case BT_RECORD: {
        unsigned rtd = record_rtd(s);
        if (!rtd) {
            fprintf(fp, "[invalid-record]");
            break;
        }
        unsigned number = object_hash_number(s);
        if (rtd == ctx.builtin_record_types[BRT_PROMISE]) {
            unsigned value = 0;
            if (promise_forced_value(s, &value)) {
                fprintf(fp, "#[promise %u (evaluated) ", number);
                write_obj_fp(value, with_quotes, fp);
                fprintf(fp, "]");
            } else {
                fprintf(fp, "#[promise %u (unevaluated)]", number);
            }
        } else if (rtd == ctx.builtin_record_types[BRT_CONDITION]) {
            const char *name = condition_type_name(s);
            if (with_quotes)
                fprintf(fp, "#[condition %u \"%s\"]", number, name);
            else
                fprintf(fp, "#[condition %u %s]", number, name);
        } else {
            fprintf(fp, "#[");
            write_record_type_name(fp, rtd);
            fprintf(fp, " %u]", number);
        }
        break;
    }
    case BT_RECORD_TYPE:
        if (!record_type_well_formed(s)) {
            fprintf(fp, "[invalid-record-type]");
            break;
        }
        fprintf(fp, "#[record-type %u ", object_hash_number(s));
        write_record_type_name(fp, s);
        fprintf(fp, "]");
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
    case BT_EOF:
        fprintf(fp, "#[eof]");
        break;
    case BT_UNASSIGNED:
        // Not a value; printed only if a bug lets it escape (or in bytecode
        // listings of letrec placeholders).
        fprintf(fp, "#!unassigned");
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

    if (simple_tracking) {
        // Re-look-up: recursion above may have grown the visited table,
        // freeing the array simple_entry pointed into
        visited_entry *cur = find_visited(s);
        if (cur)
            cur->active = false;
    }
}

static bool begin_writer(bool emit_shared_labels, bool label_cycles_only)
{
    writer_error = false;
    writer_depth = 0;
    writer_emit_shared_labels = emit_shared_labels;
    writer_label_cycles_only = label_cycles_only;
    mark_path_len = 0;
    // Like the visited table, do not keep a huge path buffer alive after
    // one large write.
    if (mark_path_cap > VISITED_INITIAL_CAP) {
        free(mark_path);
        mark_path = NULL;
        mark_path_cap = 0;
    }
    if (reset_visited())
        return true;
    show_error("writer: out of memory");
    writer_error = true;
    return false;
}

// Labeled writers. All share one path: mark (cycles only for write/display,
// all sharing for write-shared), then print. They return false after a
// reported failure (nesting too deep, out of memory), in which case the
// output may be partial.
static bool write_labeled(unsigned s, FILE *port, bool with_quotes,
                          bool label_cycles_only)
{
    if (!begin_writer(true, label_cycles_only))
        return false;
    mark_shared(s);
    write_obj_fp(s, with_quotes, port);
    return !writer_error;
}

bool write_obj_port_checked(unsigned s, FILE *port)
{
    return write_labeled(s, port, true, true);
}

bool display_obj_port_checked(unsigned s, FILE *port)
{
    return write_labeled(s, port, false, true);
}

bool write_shared_obj_port_checked(unsigned s, FILE *port)
{
    return write_labeled(s, port, true, false);
}

void write_obj(unsigned s)
{
    (void)write_obj_port_checked(s, stdout);
}

void display_obj(unsigned s)
{
    (void)display_obj_port_checked(s, stdout);
}

void write_obj_port(unsigned s, FILE *port)
{
    (void)write_obj_port_checked(s, port);
}

void write_shared_obj_port(unsigned s, FILE *port)
{
    (void)write_shared_obj_port_checked(s, port);
}

void write_simple_obj_port(unsigned s, FILE *port)
{
    (void)write_simple_obj_port_checked(s, port);
}

bool write_simple_obj_port_checked(unsigned s, FILE *port)
{
    if (!begin_writer(false, false)) {
        writer_emit_shared_labels = true;
        return false;
    }
    write_obj_fp(s, true, port);
    writer_emit_shared_labels = true;
    return !writer_error;
}

void display_obj_port(unsigned s, FILE *port)
{
    (void)display_obj_port_checked(s, port);
}
