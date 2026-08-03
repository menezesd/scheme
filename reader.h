#ifndef READER_H
#define READER_H

#include "types.h"

// Read and return the next token from input
unsigned read_token(void);

// Parse a decimal literal as an exact number (e.g. "1.5" => 3/2).
// Sets *handled to false if the string is not a plain decimal literal.
unsigned read_exact_decimal_number(const char *s, bool *handled);

// Read and parse the next object from input (from stdin)
unsigned read_obj(void);

// Read and parse the next object from specified port
unsigned read_obj_port(FILE *port);

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

// Update datum-label cells during GC while a read is in progress
void reader_update_datum_labels(unsigned (*collector)(unsigned));

// Get current reader position for error messages
int reader_get_line(void);
int reader_get_col(void);
const char *reader_get_filename(void);

// Set current filename (for error messages)
void reader_set_filename(const char *name);

#endif // READER_H
