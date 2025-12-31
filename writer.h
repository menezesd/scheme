#ifndef WRITER_H
#define WRITER_H

#include "types.h"

// Write object with quotes (for write) - to stdout
void write_obj(unsigned s);

// Write object without outer quotes (for display) - to stdout
void display_obj(unsigned s);

// Write object with quotes to specified port
void write_obj_port(unsigned s, FILE *port);

// Write object without outer quotes to specified port
void display_obj_port(unsigned s, FILE *port);

// Get type name for error messages
const char *type_name(unsigned cell);

#endif // WRITER_H
