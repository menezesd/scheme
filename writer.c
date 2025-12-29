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
#include <stdlib.h>

// ============================================================================
// Cycle Detection for Datum Labels
// ============================================================================

#define MAX_VISITED 4096

static struct {
    unsigned cell;
    int label;    // -1 = visited once, >= 0 = assigned label
    bool printed; // true if we've printed the #n= prefix
} visited[MAX_VISITED];
static int visited_count = 0;
static int next_label = 0;

static void reset_visited(void)
{
    visited_count = 0;
    next_label = 0;
}

static int find_visited(unsigned cell)
{
    for (int i = 0; i < visited_count; i++) {
        if (visited[i].cell == cell)
            return i;
    }
    return -1;
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

    int idx = find_visited(s);
    if (idx >= 0) {
        // Seen before - assign a label if not already assigned
        if (visited[idx].label < 0) {
            visited[idx].label = next_label++;
        }
        return; // Don't recurse into already-visited cells
    }

    // First visit - add to visited list
    if (visited_count < MAX_VISITED) {
        visited[visited_count].cell = s;
        visited[visited_count].label = -1;
        visited[visited_count].printed = false;
        visited_count++;
    }

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
    int idx = find_visited(st);
    if (idx >= 0 && visited[idx].label >= 0) {
        if (visited[idx].printed) {
            // Already printed - use reference
            fprintf(fp, " . #%d#", visited[idx].label);
            return;
        }
        // First print of this shared cell - print with label
        fprintf(fp, " . #%d=", visited[idx].label);
        visited[idx].printed = true;
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
    int idx = find_visited(s);
    if (idx >= 0 && visited[idx].label >= 0) {
        if (visited[idx].printed) {
            // Already printed - use reference
            fprintf(fp, "#%d#", visited[idx].label);
            return;
        }
        // First print - mark as printed and add label prefix
        visited[idx].printed = true;
        fprintf(fp, "#%d=", visited[idx].label);
    }

    switch (CELL_TYPE(s)) {
    case BT_ATOM:
        // Print #t for the true atom
        if (s == ctx.atom_true) {
            fprintf(fp, "#t");
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
        fprintf(fp, "%g", u.d);
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
    case BT_FORM:
        fprintf(fp, "[syntax]");
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
