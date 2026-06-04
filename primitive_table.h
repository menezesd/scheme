#ifndef PRIMITIVE_TABLE_H
#define PRIMITIVE_TABLE_H

#include <stddef.h>

#include "types.h"

typedef struct {
    const char *name;
    enum primitive_id prim;
} primitive_binding;

const primitive_binding *primitive_bindings(void);
size_t primitive_binding_count(void);

#endif // PRIMITIVE_TABLE_H
