#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include "types.h"

// Apply a primitive function by ID
unsigned apply_primitive(unsigned prim_id, unsigned args);
unsigned apply_primitive_argv(unsigned prim_id, unsigned argc,
                              unsigned *argv);

// GC support: queue a hash table whose keys moved, then rebuild the queued
// tables' buckets once the heap is consistent (called at the end of a GC)
void hash_table_gc_register_rehash(hash_table_data *ht);
void hash_table_gc_rehash_pending(void);

#endif // PRIMITIVES_H
