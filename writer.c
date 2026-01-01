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
#include "context.h"
#include <inttypes.h>
#include <math.h>
#include <stdlib.h>

// ============================================================================
// Type Names for Error Messages
// ============================================================================

const char *type_name(unsigned cell)
{
    if (cell == 0)
        return "nil";
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
    default:
        return "unknown";
    }
}

// ============================================================================
// Cycle Detection Hash Table
// ============================================================================

#define VISITED_TABLE_SIZE                                                     \
    8192                // Power of 2, supports ~6000 entries at 75% load
#define VISITED_EMPTY 0 // 0 is never a valid cell (represents nil)

typedef struct {
    unsigned cell; // Cell address (0 = empty slot)
    int label;     // -1 = visited once, >= 0 = assigned label
    bool printed;  // true if we've printed #n= prefix
} visited_entry;

static visited_entry visited_table[VISITED_TABLE_SIZE];
static int next_label = 0;
static int visited_count = 0;

// Hash function - good distribution for cell addresses
static inline unsigned hash_cell(unsigned cell)
{
    unsigned h = cell;
    h ^= h >> 16;
    h *= 0x85ebca6b;
    h ^= h >> 13;
    h *= 0xc2b2ae35;
    h ^= h >> 16;
    return h & (VISITED_TABLE_SIZE - 1);
}

static void reset_visited(void)
{
    // Only clear slots that were used (optimization for sparse tables)
    for (int i = 0; i < VISITED_TABLE_SIZE; i++) {
        visited_table[i].cell = VISITED_EMPTY;
    }
    next_label = 0;
    visited_count = 0;
}

// Find entry in hash table, returns pointer or NULL if not found
static visited_entry *find_visited(unsigned cell)
{
    unsigned idx = hash_cell(cell);
    for (int i = 0; i < VISITED_TABLE_SIZE; i++) {
        unsigned probe = (idx + i) & (VISITED_TABLE_SIZE - 1);
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
    // Warn if approaching capacity
    if (visited_count >= VISITED_TABLE_SIZE * 3 / 4) {
        static bool warned = false;
        if (!warned) {
            fprintf(stderr, "warning: cycle detection table near capacity\n");
            warned = true;
        }
    }

    unsigned idx = hash_cell(cell);
    for (int i = 0; i < VISITED_TABLE_SIZE; i++) {
        unsigned probe = (idx + i) & (VISITED_TABLE_SIZE - 1);
        if (visited_table[probe].cell == VISITED_EMPTY) {
            visited_table[probe].cell = cell;
            visited_table[probe].label = -1;
            visited_table[probe].printed = false;
            visited_count++;
            return &visited_table[probe];
        }
    }
    return NULL; // Table full
}

// First pass: mark all cells that are visited more than once
static void mark_shared(unsigned s)
{
    if (s == 0 || s == TOK_ERROR)
        return;

    // Only track cons cells and vectors (things that can have sharing)
    enum lisp_type type = CELL_TYPE(s);
    if (type != BT_CONS && type != BT_VECTOR)
        return;

    visited_entry *entry = find_visited(s);
    if (entry) {
        // Seen before - assign a label if not already assigned
        if (entry->label < 0) {
            entry->label = next_label++;
        }
        return; // Don't recurse into already-visited cells
    }

    // First visit - add to hash table
    insert_visited(s);

    // Recurse into children
    if (type == BT_CONS) {
        mark_shared(car(s));
        mark_shared(cdr(s));
    } else if (type == BT_VECTOR) {
        unsigned len = vector_len(s);
        unsigned *data = vector_data_ptr(s);
        for (unsigned i = 0; i < len; i++) {
            mark_shared(data[i]);
        }
    }
}

// Forward declaration
static void write_obj_fp(unsigned s, bool with_quotes, FILE *fp);

static void write_list_tail_fp(unsigned st, bool with_quotes, FILE *fp)
{
    if (st == 0)
        return;

    // Check if the tail has a label (shared/circular)
    visited_entry *entry = find_visited(st);
    if (entry && entry->label >= 0) {
        if (entry->printed) {
            // Already printed - use reference
            fprintf(fp, " . #%d#", entry->label);
            return;
        }
        // First print of this shared cell - print with label
        fprintf(fp, " . #%d=", entry->label);
        entry->printed = true;
        if (IS_PAIR(st)) {
            fprintf(fp, "(");
            write_obj_fp(car(st), with_quotes, fp);
            write_list_tail_fp(cdr(st), with_quotes, fp);
            fprintf(fp, ")");
        } else {
            write_obj_fp(st, with_quotes, fp);
        }
        return;
    }

    if (IS_PAIR(st)) {
        fprintf(fp, " ");
        write_obj_fp(car(st), with_quotes, fp);
        write_list_tail_fp(cdr(st), with_quotes, fp);
    } else {
        fprintf(fp, " . ");
        write_obj_fp(st, with_quotes, fp);
    }
}

static void write_obj_fp(unsigned s, bool with_quotes, FILE *fp)
{
    if (s == 0) {
        fprintf(fp, "()");
        return;
    }
    if (s == TOK_ERROR) {
        fprintf(fp, "[ERROR]");
        return;
    }

    // Check for shared/circular structure
    visited_entry *entry = find_visited(s);
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

    switch (CELL_TYPE(s)) {
    case BT_ATOM:
        // Print #t and #f for boolean atoms
        if (s == ctx.atom_true) {
            fprintf(fp, "#t");
        } else if (s == ctx.atom_false) {
            fprintf(fp, "#f");
        } else {
            fprintf(fp, "%s", ctx.atom_table[CELL_ID(s)]);
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
        write_obj_fp(CELL_CAR(s), with_quotes, fp);
        fprintf(fp, "/");
        write_obj_fp(CELL_CDR(s), with_quotes, fp);
        break;
    case BT_COMPLEX: {
        write_obj_fp(CELL_CAR(s), with_quotes, fp);
        unsigned imag = CELL_CDR(s);
        double imag_val = to_double(imag);
        if (imag_val >= 0)
            fprintf(fp, "+");
        write_obj_fp(imag, with_quotes, fp);
        fprintf(fp, "i");
        break;
    }
    case BT_STRING:
        if (with_quotes) {
            fprintf(fp, "\"%s\"", GET_STRING_PTR(s));
        } else {
            fprintf(fp, "%s", GET_STRING_PTR(s));
        }
        break;
    case BT_CHAR: {
        int c = GET_CHAR_CODE(s);
        if (with_quotes) {
            switch (c) {
            case ' ':
                fprintf(fp, "#\\space");
                break;
            case '\n':
                fprintf(fp, "#\\newline");
                break;
            case '\t':
                fprintf(fp, "#\\tab");
                break;
            default:
                fprintf(fp, "#\\%c", c);
            }
        } else {
            fprintf(fp, "%c", c);
        }
        break;
    }
    case BT_VECTOR: {
        fprintf(fp, "#(");
        unsigned len = vector_len(s);
        unsigned *data = vector_data_ptr(s);
        for (unsigned i = 0; i < len; i++) {
            if (i > 0)
                fprintf(fp, " ");
            write_obj_fp(data[i], with_quotes, fp);
        }
        fprintf(fp, ")");
        break;
    }
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
    case BT_BUILTIN:
        fprintf(fp, "[builtin]");
        break;
    case BT_MULTIVAL: {
        // Print multiple values
        unsigned vals = CELL_CAR(s);
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
}

void write_obj(unsigned s)
{
    reset_visited();
    mark_shared(s);
    write_obj_fp(s, true, stdout);
}

void display_obj(unsigned s)
{
    reset_visited();
    mark_shared(s);
    write_obj_fp(s, false, stdout);
}

void write_obj_port(unsigned s, FILE *port)
{
    reset_visited();
    mark_shared(s);
    write_obj_fp(s, true, port);
}

void display_obj_port(unsigned s, FILE *port)
{
    reset_visited();
    mark_shared(s);
    write_obj_fp(s, false, port);
}
