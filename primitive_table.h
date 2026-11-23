#ifndef PRIMITIVE_TABLE_H
#define PRIMITIVE_TABLE_H

#include <stddef.h>
#include <stdint.h>

#include "types.h"

// ============================================================================
// Primitive metadata
// ============================================================================
// One entry per enum primitive_id, indexed by the id (primitive_table.c).
// apply_primitive_argv checks the argument count against min_args/max_args
// before dispatching, so a primitive's implementation can index argv up to
// min_args without checking argc itself.

// Never allocates a cell, so nothing can collect while it runs and
// apply_primitive_argv skips rooting its arguments.
#define PRIM_NO_ALLOC (1u << 0)
// No side effects and never calls back into Scheme, so it cannot capture a
// continuation and deferring its application is invisible (the TRMC
// transforms in compile.c rely on this).
#define PRIM_PURE (1u << 1)
// PRIM_PURE, and its result is never a fresh mutable object, so a call with
// constant arguments may be folded to its value at compile time. Always set
// together with PRIM_PURE (the table's FOLD shorthand sets both).
#define PRIM_FOLDABLE (1u << 2)
// Control primitive (apply, call/cc, eval, ...) that both engines apply
// themselves, checking its argument count with their own messages;
// apply_primitive_argv refuses it.
#define PRIM_SPECIAL (1u << 3)
// Reports a wrong argument count itself, with a message that predates the
// table (e.g. "-: requires at least one argument"); apply_primitive_argv
// leaves the count to it. min_args/max_args still describe the arity.
#define PRIM_OWN_ARITY (1u << 4)
// Bound only while bootstrapping the standard library (main.c), not in
// default_environment.
#define PRIM_BOOTSTRAP (1u << 5)

// max_args of a primitive that takes any number of arguments from min_args.
#define PRIM_ARGS_ANY UINT8_MAX

typedef struct {
    const char *name; // the name it is bound to
    uint8_t min_args;
    uint8_t max_args; // PRIM_ARGS_ANY: no upper bound
    uint16_t flags;   // PRIM_* above
    // The name its argument-count errors use when that is not name, e.g. the
    // public procedure the stdlib wraps this %-primitive in. NULL: name.
    const char *who;
} primitive_info;

extern const primitive_info primitive_table[PRIM_COUNT];

static inline const primitive_info *primitive_info_for(unsigned prim_id)
{
    return prim_id < PRIM_COUNT ? &primitive_table[prim_id] : NULL;
}

static inline bool primitive_has_flag(int64_t prim_id, unsigned flag)
{
    return prim_id >= 0 && prim_id < PRIM_COUNT &&
           (primitive_table[prim_id].flags & flag) != 0;
}

// The name a primitive's errors are reported under.
static inline const char *primitive_error_name(const primitive_info *info)
{
    return info->who ? info->who : info->name;
}

// Extra names bound to an existing primitive (call-with-current-continuation
// for call/cc, ...), NULL-terminated.
typedef struct {
    const char *name;
    enum primitive_id prim;
} primitive_alias;

extern const primitive_alias primitive_aliases[];

#endif // PRIMITIVE_TABLE_H
