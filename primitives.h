#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include "types.h"

// Apply a primitive function by ID
unsigned apply_primitive(unsigned prim_id, unsigned args);
unsigned apply_primitive_argv(unsigned prim_id, unsigned argc,
                              unsigned *argv);

// Build a Scheme error object (vector tagged with the stdlib *error-object-tag*,
// kind 'error, message = msg, irritants = '()) — mirrors stdlib's
// make-error-object. Returns TOK_ERROR on failure. May trigger GC.
unsigned make_error_object_c(const char *msg, unsigned env);

// GC support: queue a hash table whose keys moved, then rebuild the queued
// tables' buckets once the heap is consistent (called at the end of a GC)
void hash_table_gc_register_rehash(hash_table_data *ht);
void hash_table_gc_rehash_pending(void);

#endif // PRIMITIVES_H
