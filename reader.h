#ifndef READER_H
#define READER_H

#include "types.h"

// Read and return the next token from input
unsigned read_token(void);

// Parse a decimal literal as an exact number (e.g. "1.5" => 3/2).
// Sets *handled to false if the string is not a plain decimal literal.
unsigned read_exact_decimal_number(const char *s, bool *handled);

// Parse one complete numeric spelling, including optional radix/exactness
// prefixes. Returns #f for invalid syntax, TOK_ERROR for allocation/size errors.
unsigned parse_number_string(const char *s, int default_radix);

// Read and parse the next object from input (from stdin)
unsigned read_obj(void);

// Read and parse the next object from specified port
unsigned read_obj_port(FILE *port);

// Read from a temporary stream that belongs to a persistent string port.
unsigned read_obj_string_port(FILE *port, bool *no_fold_case);

// Read a REPL form using stdin's shared port state and retain its location.
unsigned read_obj_stdin(void);

// The same for a REPL reading from another stream (the line editor's): its
// own port state, location retained across forms.
unsigned read_obj_repl(FILE *port);

// Get the number of internally pushed-back bytes saved for a port
size_t reader_port_pending_bytes(FILE *port);

// Read or peek a byte, honoring the reader's internal port pushback
int reader_port_getc(FILE *port);
int reader_port_peekc(FILE *port);

// Put back the most recently read byte, preserving reader pushback state.
// Returns false if the byte cannot be restored.
bool reader_port_ungetc(FILE *port, int c);

// Discard saved reader state for a port before closing it
void reader_forget_port(FILE *port);

// Read and parse a list from input
unsigned read_list(void);

// Read a vector literal #(...)
unsigned read_vector(void);

// Reset line/column tracking (call when switching input sources)
void reader_reset_position(void);

// Reset datum labels (call before each top-level read)
void reader_reset_labels(void);

// Restore a consistent reader after a lisp_panic longjmp interrupted a read
// (resets nesting depth, current port, and datum labels; saves the
// interrupted port's pushback). Call from the panic-recovery path.
void reader_recover_after_panic(void);

// Update datum-label cells during GC while a read is in progress
void reader_update_datum_labels(unsigned (*collector)(unsigned));

// Get current reader position for error messages
int reader_get_line(void);
int reader_get_col(void);
const char *reader_get_filename(void);

// Set current filename (for error messages)
void reader_set_filename(const char *name);

// Where the reader saw a procedure or a top-level form (see "Source
// positions" in reader.c). Keys are the body list of a lambda, named-lambda,
// procedure define or named let, and each top-level list datum.
typedef struct {
    const char *file; // interned for the life of the process; "?" if unknown
    unsigned line;    // 1-based line of the form's opening parenthesis
    int64_t name;     // atom id of the procedure's name, or -1
} source_info;
bool reader_source_info(unsigned key, source_info *out);
// Give `to` (a macro-expanded copy of the list `from`) from's position.
void reader_source_alias(unsigned from, unsigned to);
// Name the procedure whose body is `body`, if nothing named it yet; a body
// without a recorded position gets an entry with the name alone.
void reader_source_name(unsigned body, unsigned name);

#endif // READER_H
