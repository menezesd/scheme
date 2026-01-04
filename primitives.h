#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include "types.h"

// Apply a primitive function by ID
unsigned apply_primitive(unsigned prim_id, unsigned args);
unsigned apply_primitive_argv(unsigned prim_id, unsigned argc,
                              unsigned *argv);

#endif // PRIMITIVES_H
