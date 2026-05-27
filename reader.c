/**
 * @file reader.c
 * @brief S-expression parser and tokenizer
 *
 * This file implements the Scheme reader, which parses text into the internal
 * cell representation. It handles:
 *
 * ## Lexical Elements
 * - Numbers: integers, decimals, rationals (1/3), bignums
 * - Strings: with escape sequences (\n, \t, \\, \")
 * - Characters: #\a, #\space, #\newline, #\tab
 * - Symbols: case-insensitive, interned for fast comparison
 * - Booleans: #t, #f
 * - Vectors: #(1 2 3)
 *
 * ## Special Syntax
 * - Quote: 'x -> (quote x)
 * - Quasiquote: `x -> (quasiquote x)
 * - Unquote: ,x -> (unquote x)
 * - Unquote-splicing: ,@x -> (unquote-splicing x)
 * - Datum labels: #n= (define) and #n# (reference) for cyclic structures
 *
 * ## Error Handling
 * Line and column tracking for meaningful error messages.
 */

#include "reader.h"
#include "context.h"
#include <ctype.h>
#include <limits.h>
#include <string.h>
#include <strings.h>

// Current input port for reading (NULL means use stdin)
static FILE *reader_port = NULL;

// Source location tracking for error messages
static int reader_line = 1;
static int reader_col = 0;
static const char *reader_filename = "<stdin>";

// ============================================================================
// Datum Labels (#n= and #n#)
// ============================================================================

// Dynamic datum label storage - grows as needed
#define INITIAL_DATUM_LABELS 64

typedef struct {
    int label;
    unsigned value;
    bool defined;
} datum_label_entry;

static datum_label_entry *datum_labels = NULL;
static int datum_label_count = 0;
static int datum_label_cap = 0;

static void reset_datum_labels(void)
{
    datum_label_count = 0;
    // Keep the allocated buffer for reuse, just reset count
}

static int find_datum_label(int label)
{
    for (int i = 0; i < datum_label_count; i++) {
        if (datum_labels[i].label == label)
            return i;
    }
    return -1;
}

static int add_datum_label(int label)
{
    // Grow the array if needed
    if (datum_label_count >= datum_label_cap) {
        int new_cap = datum_label_cap == 0 ? INITIAL_DATUM_LABELS
                                           : datum_label_cap * 2;
        datum_label_entry *new_labels =
            realloc(datum_labels, new_cap * sizeof(datum_label_entry));
        if (!new_labels) {
            show_error("out of memory for datum labels");
            return -1;
        }
        datum_labels = new_labels;
        datum_label_cap = new_cap;
    }
    int idx = datum_label_count++;
    datum_labels[idx].label = label;
    datum_labels[idx].value = 0;
    datum_labels[idx].defined = false;
    return idx;
}

static int reader_getchar(void)
{
    FILE *fp = reader_port ? reader_port : stdin;
    int c = fgetc(fp);
    if (c == '\n') {
        reader_line++;
        reader_col = 0;
    } else if (c != EOF) {
        reader_col++;
    }
    return c;
}

static void reader_ungetc(int c)
{
    FILE *fp = reader_port ? reader_port : stdin;
    if (c == '\n') {
        reader_line--;
    } else if (c != EOF) {
        reader_col--;
    }
    ungetc(c, fp);
}

// Reset line tracking (call when switching input sources)
void reader_reset_position(void)
{
    reader_line = 1;
    reader_col = 0;
}

// Get current reader position for error messages
int reader_get_line(void) { return reader_line; }
int reader_get_col(void) { return reader_col; }
const char *reader_get_filename(void) { return reader_filename; }
void reader_set_filename(const char *name)
{
    reader_filename = name ? name : "<stdin>";
}

// ============================================================================
// String Buffer - eliminates repeated malloc/realloc pattern
// ============================================================================

typedef struct {
    char *data;
    size_t len;
    size_t cap;
} string_buffer;

static void sb_init(string_buffer *sb)
{
    sb->data = malloc(INITIAL_STRING_CAP);
    if (!sb->data) {
        lisp_panic("failed to allocate string buffer");
    }
    sb->data[0] = '\0';
    sb->len = 0;
    sb->cap = INITIAL_STRING_CAP;
}

static void sb_append(string_buffer *sb, int ch)
{
    if (sb->len + 1 >= sb->cap) {
        sb->cap *= 2;
        sb->data = realloc(sb->data, sb->cap);
        if (!sb->data) {
            lisp_panic("failed to grow string buffer");
        }
    }
    sb->data[sb->len++] = (char)ch;
    sb->data[sb->len] = '\0';
}

static char *sb_finish(string_buffer *sb)
{
    return sb->data; // Caller takes ownership
}

static void sb_free(string_buffer *sb)
{
    free(sb->data);
    sb->data = NULL;
    sb->len = sb->cap = 0;
}

// ============================================================================
// Token Delimiter Check
// ============================================================================

static inline bool is_delimiter(int c)
{
    return isspace(c) || c == '(' || c == ')' || c == '"' || c == ';' ||
           c == EOF;
}

// ============================================================================
// Character Name Lookup Table
// ============================================================================

static const struct {
    const char *name;
    int value;
} char_names[] = {{"space", ' '}, {"newline", '\n'}, {"tab", '\t'}, {NULL, 0}};

static int lookup_char_name(const char *name)
{
    for (int i = 0; char_names[i].name; i++) {
        if (strcasecmp(name, char_names[i].name) == 0) {
            return char_names[i].value;
        }
    }
    return -1;
}

// ============================================================================
// Token Reading
// ============================================================================

static unsigned read_character_literal(void)
{
    int c = reader_getchar();

    // Try to read a named character
    if (isalpha(c)) {
        char buf[CHAR_NAME_BUF_SIZE];
        buf[0] = c;
        int i = 1;
        int c2 = EOF;
        while (i < CHAR_NAME_BUF_SIZE - 1 && isalpha(c2 = reader_getchar())) {
            buf[i++] = c2;
        }
        buf[i] = '\0';

        if (i > 1) {
            // Multi-character name - look it up
            if (c2 != EOF)
                reader_ungetc(c2);
            int char_val = lookup_char_name(buf);
            if (char_val >= 0) {
                return make_char(char_val);
            }
            // Unknown character name - just use first character
        } else {
            if (c2 != EOF)
                reader_ungetc(c2);
        }
    }

    return make_char(c);
}

static unsigned read_string_literal(void)
{
    string_buffer sb;
    sb_init(&sb);

    for (;;) {
        int c = reader_getchar();
        if (c == EOF) {
            show_error("unterminated string");
            sb_free(&sb);
            return TOK_ERROR;
        }
        if (c == '"')
            break;
        if (c == '\\') {
            c = reader_getchar();
            switch (c) {
            case 'n': c = '\n'; break;
            case 't': c = '\t'; break;
            case 'r': c = '\r'; break;
            case '\\': c = '\\'; break;
            case '"': c = '"'; break;
            case 'a': c = '\a'; break;
            case 'b': c = '\b'; break;
            case 'e': c = 27; break; // ESC
            case 'x': {
                // Hex escape: \xNN
                int hi = reader_getchar();
                int lo = reader_getchar();
                int val = 0;
                if (isxdigit(hi) && isxdigit(lo)) {
                    val = (hi <= '9' ? hi - '0' : (hi | 32) - 'a' + 10) * 16 +
                          (lo <= '9' ? lo - '0' : (lo | 32) - 'a' + 10);
                } else {
                    show_warning("invalid hex escape: \\x%c%c", hi, lo);
                    reader_ungetc(lo);
                    reader_ungetc(hi);
                }
                c = val;
                break;
            }
            default:
                if (c >= '0' && c <= '7') {
                    // Octal escape: \NNN (1-3 digits)
                    int val = c - '0';
                    c = reader_getchar();
                    if (c >= '0' && c <= '7') {
                        val = val * 8 + (c - '0');
                        c = reader_getchar();
                        if (c >= '0' && c <= '7') {
                            val = val * 8 + (c - '0');
                        } else {
                            reader_ungetc(c);
                        }
                    } else {
                        reader_ungetc(c);
                    }
                    c = val;
                } else {
                    show_warning("unknown escape sequence: \\%c", c);
                }
            }
        }
        sb_append(&sb, c);
    }

    unsigned x = alloc();
    CELL_TYPE(x) = BT_STRING;
    CELL_PTR(x) = sb_finish(&sb);
    return x;
}

// Read a number starting with decimal point (e.g., .5)
static unsigned read_decimal_number(void)
{
    string_buffer sb;
    sb_init(&sb);
    sb_append(&sb, '.');

    int c;
    while (isdigit(c = reader_getchar()) || c == 'e' || c == 'E' || c == '+' ||
           c == '-') {
        sb_append(&sb, c);
    }
    reader_ungetc(c);

    unsigned res = atom_from_string(sb.data);
    sb_free(&sb);
    return res;
}

unsigned read_token(void)
{
    int c;
    for (;;) {
        c = reader_getchar();
        while (isspace(c))
            c = reader_getchar();

        // Comments
        if (c == ';') {
            while ((c = reader_getchar()) != '\n' && c != EOF)
                ;
            if (c == EOF) {
                if (reader_port == NULL)
                    exit(0);
                return atom_from_string("eof-object");
            }
            continue;
        }

        if (c == EOF) {
            if (reader_port == NULL)
                exit(0);
            return atom_from_string("eof-object");
        }

        switch (c) {
        case '(':
            return TOK_OPEN;
        case ')':
            return TOK_CLOSE;
        case '.': {
            // Check for ellipsis (...) or number starting with .
            int c2 = reader_getchar();
            if (c2 == '.') {
                int c3 = reader_getchar();
                if (c3 == '.') {
                    return atom_from_string("...");
                }
                reader_ungetc(c3);
            } else if (isdigit(c2)) {
                reader_ungetc(c2);
                return read_decimal_number();
            }
            reader_ungetc(c2);
            return TOK_DOT;
        }
        case '\'':
            return TOK_QUOTE;
        case '`':
            return TOK_QUASIQUOTE;
        case ',': {
            c = reader_getchar();
            if (c == '@') {
                return TOK_UNQUOTE_SPLICING;
            }
            reader_ungetc(c);
            return TOK_UNQUOTE;
        }
        case '#': {
            c = reader_getchar();
            if (c == '(') {
                return TOK_VECTOR_OPEN;
            } else if (c == '\\') {
                return read_character_literal();
            } else if (c == 't' || c == 'T') {
                return ctx.atom_true;
            } else if (c == 'f' || c == 'F') {
                return ctx.atom_false;
            } else if (c == 'x' || c == 'X') {
                // Hexadecimal literal: #xFF
                string_buffer sb;
                sb_init(&sb);
                bool neg = false;
                c = reader_getchar();
                if (c == '-') { neg = true; c = reader_getchar(); }
                else if (c == '+') { c = reader_getchar(); }
                while (isxdigit(c)) {
                    sb_append(&sb, c);
                    c = reader_getchar();
                }
                reader_ungetc(c);
                if (sb.len == 0) {
                    sb_free(&sb);
                    show_error("invalid hex literal: #x");
                    return TOK_ERROR;
                }
                int64_t val = (int64_t)strtoll(sb.data, NULL, 16);
                sb_free(&sb);
                return store(neg ? -val : val);
            } else if (c == 'o' || c == 'O') {
                // Octal literal: #o77
                string_buffer sb;
                sb_init(&sb);
                bool neg = false;
                c = reader_getchar();
                if (c == '-') { neg = true; c = reader_getchar(); }
                else if (c == '+') { c = reader_getchar(); }
                while (c >= '0' && c <= '7') {
                    sb_append(&sb, c);
                    c = reader_getchar();
                }
                reader_ungetc(c);
                if (sb.len == 0) {
                    sb_free(&sb);
                    show_error("invalid octal literal: #o");
                    return TOK_ERROR;
                }
                int64_t val = (int64_t)strtoll(sb.data, NULL, 8);
                sb_free(&sb);
                return store(neg ? -val : val);
            } else if (c == 'b' || c == 'B') {
                // Binary literal: #b1010
                string_buffer sb;
                sb_init(&sb);
                bool neg = false;
                c = reader_getchar();
                if (c == '-') { neg = true; c = reader_getchar(); }
                else if (c == '+') { c = reader_getchar(); }
                while (c == '0' || c == '1') {
                    sb_append(&sb, c);
                    c = reader_getchar();
                }
                reader_ungetc(c);
                if (sb.len == 0) {
                    sb_free(&sb);
                    show_error("invalid binary literal: #b");
                    return TOK_ERROR;
                }
                int64_t val = (int64_t)strtoll(sb.data, NULL, 2);
                sb_free(&sb);
                return store(neg ? -val : val);
            } else if (c == 'u') {
                // Bytevector literal: #u8(...)
                c = reader_getchar();
                if (c == '8') {
                    c = reader_getchar();
                    if (c == '(') {
                        // Read bytes by reading a list then converting
                        reader_ungetc(c); // put back '('
                        unsigned list = read_obj(); // read (b1 b2 ...)
                        unsigned count = 0;
                        for (unsigned p = list; p; p = cdr(p)) {
                            if (!IS_PAIR(p)) {
                                show_error("bytevector literal: improper list");
                                return TOK_ERROR;
                            }
                            unsigned byte = car(p);
                            if (!IS_NUM(byte) || CELL_ID(byte) < 0 ||
                                CELL_ID(byte) > 255) {
                                show_error("bytevector literal: byte out of range");
                                return TOK_ERROR;
                            }
                            count++;
                        }
                        if (count > 1024 * 1024) {
                            show_error("bytevector literal too large");
                            return TOK_ERROR;
                        }
                        bytevec_data *bv = malloc(sizeof(bytevec_data) + count);
                        if (!bv)
                            return TOK_ERROR;
                        bv->len = count;
                        unsigned i = 0;
                        for (unsigned p = list; p && IS_PAIR(p); p = cdr(p))
                            bv->data[i++] = (uint8_t)CELL_ID(car(p));
                        unsigned cell = alloc();
                        CELL_TYPE(cell) = BT_BYTEVEC;
                        CELL_PTR(cell) = bv;
                        return cell;
                    }
                    reader_ungetc(c);
                }
                show_error("unknown # syntax: #u%c", c);
                return TOK_ERROR;
            } else if (isdigit(c)) {
                // Datum label: #n= or #n#
                int label = c - '0';
                while (isdigit(c = reader_getchar())) {
                    // Check for integer overflow before multiplication
                    int digit = c - '0';
                    if (label > (INT_MAX - digit) / 10) {
                        show_error("datum label too large");
                        return TOK_ERROR;
                    }
                    label = label * 10 + digit;
                }
                if (c == '=') {
                    // Define label: #n=<datum>
                    int idx = find_datum_label(label);
                    if (idx >= 0 && datum_labels[idx].defined) {
                        show_error("duplicate datum label #%d=", label);
                        return TOK_ERROR;
                    }
                    // Pre-allocate a cell for forward references
                    unsigned placeholder = alloc_cons(0, 0);
                    if (idx < 0) {
                        idx = add_datum_label(label);
                        if (idx < 0)
                            return TOK_ERROR;
                    }
                    datum_labels[idx].value = placeholder;
                    datum_labels[idx].defined = true;
                    // Read the datum
                    unsigned datum = read_obj();
                    if (datum == TOK_ERROR)
                        return TOK_ERROR;
                    // Copy datum content into placeholder
                    if (IS_PAIR(datum)) {
                        cell_set_car(placeholder, car(datum));
                        cell_set_cdr(placeholder, cdr(datum));
                    } else {
                        // For non-pairs, copy the cell content
                        ctx.cons_cells[placeholder] = ctx.cons_cells[datum];
                    }
                    return placeholder;
                } else if (c == '#') {
                    // Reference label: #n#
                    int idx = find_datum_label(label);
                    if (idx < 0) {
                        show_error("undefined datum label #%d#", label);
                        return TOK_ERROR;
                    }
                    return datum_labels[idx].value;
                } else {
                    show_error("expected = or # after #%d", label);
                    return TOK_ERROR;
                }
            }
            show_error("unknown # syntax: #%c", c);
            return TOK_ERROR;
        }
        case '"':
            return read_string_literal();
        default: {
            // Symbol or number
            string_buffer sb;
            sb_init(&sb);
            bool is_number = isdigit(c) || c == '-' || c == '+';
            sb_append(&sb, tolower(c));

            for (;;) {
                c = reader_getchar();
                // Allow . in numbers (for decimals like 1.5)
                if (c == '.' && is_number) {
                    int c2 = reader_getchar();
                    if (isdigit(c2) || c2 == 'e' || c2 == 'E') {
                        // It's a decimal number
                        sb_append(&sb, '.');
                        sb_append(&sb, tolower(c2));
                        continue;
                    } else if (is_delimiter(c2)) {
                        // End of number followed by dot (for dotted pairs)
                        reader_ungetc(c2);
                        reader_ungetc(c);
                        break;
                    }
                    reader_ungetc(c2);
                    sb_append(&sb, '.');
                    continue;
                }
                if (is_delimiter(c))
                    break;
                // R5RS: . + - @ are valid subsequent characters in identifiers
                // The . as dotted-pair marker is handled in the switch above
                sb_append(&sb, tolower(c));
            }
            reader_ungetc(c);
            unsigned res = atom_from_string(sb.data);
            sb_free(&sb);
            return res;
        }
        }
    }
}

// ============================================================================
// Object and List Reading
// ============================================================================

unsigned read_vector(void)
{
    unsigned head = 0, tail = 0;
    unsigned count = 0;

    for (;;) {
        unsigned elem = read_obj();
        if (elem == TOK_CLOSE)
            break;
        if (elem == TOK_ERROR)
            return TOK_ERROR;
        if (elem == TOK_DOT) {
            show_error("dot not allowed in vector literal");
            return TOK_ERROR;
        }

        list_append(&head, &tail, elem);
        count++;
    }

    unsigned vec = make_vector(count, 0);
    unsigned *data = vector_data_ptr(vec);
    unsigned i = 0;
    FORLIST(l, head) { data[i++] = car(l); }
    return vec;
}

unsigned read_obj(void)
{
    unsigned tok = read_token();
    switch (tok) {
    case TOK_OPEN:
        return read_list();
    case TOK_VECTOR_OPEN:
        return read_vector();
    case TOK_QUOTE:
        tok = read_obj();
        switch (tok) {
        case TOK_CLOSE:
            show_warning("ignoring quote before close parenthesis");
            return tok;
        case TOK_DOT:
            show_warning("ignoring quote before dot");
            return tok;
        case TOK_ERROR:
            return tok;
        default:
            return alloc_cons(ctx.atom_quote, alloc_cons(tok, 0));
        }
    case TOK_QUASIQUOTE:
        tok = read_obj();
        if (tok == TOK_CLOSE || tok == TOK_DOT || tok == TOK_ERROR) {
            show_warning("ignoring quasiquote before special token");
            return tok;
        }
        return alloc_cons(ctx.atom_quasiquote, alloc_cons(tok, 0));
    case TOK_UNQUOTE:
        tok = read_obj();
        if (tok == TOK_CLOSE || tok == TOK_DOT || tok == TOK_ERROR) {
            show_warning("ignoring unquote before special token");
            return tok;
        }
        return alloc_cons(ctx.atom_unquote, alloc_cons(tok, 0));
    case TOK_UNQUOTE_SPLICING:
        tok = read_obj();
        if (tok == TOK_CLOSE || tok == TOK_DOT || tok == TOK_ERROR) {
            show_warning("ignoring unquote-splicing before special token");
            return tok;
        }
        return alloc_cons(ctx.atom_unquote_splicing, alloc_cons(tok, 0));
    default:
        return tok;
    }
}

unsigned read_list(void)
{
    unsigned sh = read_obj();
    unsigned st;
    switch (sh) {
    case TOK_ERROR:
        return TOK_ERROR;
    case TOK_CLOSE:
        return 0;
    case TOK_DOT:
        sh = read_obj();
        switch (sh) {
        case TOK_ERROR:
            return TOK_ERROR;
        case TOK_DOT:
        case TOK_CLOSE:
            show_error("a dot must be followed by an object");
            return TOK_ERROR;
        }
        st = read_list();
        if (st == TOK_ERROR)
            return TOK_ERROR;
        if (st != 0) {
            show_error("only one object may follow a dot");
            return TOK_ERROR;
        }
        return sh;
    default:
        st = read_list();
        if (st == TOK_ERROR)
            return TOK_ERROR;
        return alloc_cons(sh, st);
    }
}

void reader_reset_labels(void) { reset_datum_labels(); }

unsigned read_obj_port(FILE *port)
{
    FILE *old_port = reader_port;
    int old_line = reader_line;
    int old_col = reader_col;
    reader_port = port;
    reader_line = 1;
    reader_col = 0;
    reset_datum_labels();
    unsigned result = read_obj();
    reader_port = old_port;
    reader_line = old_line;
    reader_col = old_col;
    return result;
}
