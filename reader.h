#ifndef READER_H
#define READER_H

#include "types.h"

// Read and return the next token from input
unsigned read_token(void);

// Read and parse the next object from input (from stdin)
unsigned read_obj(void);

// Read and parse the next object from specified port
unsigned read_obj_port(FILE *port);

// Read and parse a list from input
unsigned read_list(void);

// Read a vector literal #(...)
unsigned read_vector(void);

// Reset line/column tracking (call when switching input sources)
void reader_reset_position(void);

// Reset datum labels (call before each top-level read)
void reader_reset_labels(void);

// Get current reader position for error messages
int reader_get_line(void);
int reader_get_col(void);

#endif // READER_H
