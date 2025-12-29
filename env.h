#ifndef ENV_H
#define ENV_H

#include "context.h"
#include "types.h"

// ============================================================================
// Frame Construction Helpers
// ============================================================================

// Create a frame from vars and vals lists
static inline unsigned make_frame(unsigned vars, unsigned vals)
{
    return alloc_cons(vars, vals);
}

// Extend environment with a new frame
static inline unsigned extend_env(unsigned vars, unsigned vals, unsigned env)
{
    unsigned frame = make_frame(vars, vals);
    return alloc_cons(frame, env);
}

// Extend environment with an empty frame
static inline unsigned extend_env_empty(unsigned env)
{
    unsigned frame = alloc_cons(0, 0);
    return alloc_cons(frame, env);
}

// ============================================================================
// Environment Operations
// ============================================================================

// Create an empty environment
unsigned empty_environment(void);

// Define a variable in the current frame
unsigned defvar(unsigned var, unsigned aval, unsigned env);

// Set an existing variable (error if not found)
unsigned setvar(int64_t var, unsigned aval, unsigned env);

// Look up a variable
unsigned lookup(int64_t var, unsigned env);

// Bind parameters to arguments, supporting variadic (a b . rest) syntax
unsigned bind_params(unsigned params, unsigned args);

// Create a primitive operation builtin
unsigned mk_primop(int64_t id);

// Create and initialize the default environment with primitives
unsigned default_environment(void);

#endif // ENV_H
