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
    gc_protect(&vars);
    gc_protect(&vals);
    gc_protect(&env);
    unsigned frame = 0;
    gc_protect(&frame);
    frame = make_frame(vars, vals);
    unsigned result = alloc_cons(frame, env);
    gc_unprotect(4);
    return result;
}

// Extend environment with an empty frame
static inline unsigned extend_env_empty(unsigned env)
{
    gc_protect(&env);
    unsigned frame = 0;
    gc_protect(&frame);
    frame = alloc_cons(0, 0);
    unsigned result = alloc_cons(frame, env);
    gc_unprotect(2);
    return result;
}

// ============================================================================
// Environment Operations
// ============================================================================

// Create an empty environment
unsigned empty_environment(void);

// Define a variable in the current frame
unsigned defvar(unsigned var, unsigned aval, unsigned env);

// Define a variable as an alias to an existing environment value cell
unsigned defvar_alias(unsigned var, unsigned target_var, unsigned target_val_cell,
                      unsigned env);

// Build a standalone BT_BINDING_REF cell (CAR = target_val_cell,
// CDR = target_var) without installing it in an environment. Used to embed
// a resolved binding as a code constant, e.g. for hygienic references to a
// macro's free identifiers that must survive expansion-time-only lookup.
unsigned make_binding_ref_cell(unsigned target_var, unsigned target_val_cell);

// Find the value cell for a variable in an environment, or 0 if absent
unsigned env_find_binding_cell(int64_t var, unsigned env);

// Reject cyclic environment parent chains before traversing them.
bool env_chain_acyclic(unsigned env);

// Reject cyclic variable lists in an environment frame before traversing them.
bool env_binding_list_acyclic(unsigned vars);

// Set an existing variable (error if not found)
unsigned setvar(int64_t var, unsigned aval, unsigned env);

// Look up a variable (prints error if not found)
unsigned lookup(int64_t var, unsigned env);

// Look up a variable silently (no error if not found)
unsigned lookup_silent(int64_t var, unsigned env);

// Invalidate lookup cache (call after GC or major environment changes)
void env_invalidate_cache(void);

// Global inline cache epoch — bumped on define to invalidate bytecode ICs
extern unsigned global_ic_epoch;

// Bind parameters to arguments, supporting variadic (a b . rest) syntax
unsigned bind_params(unsigned params, unsigned args);

// Create a primitive operation builtin
unsigned mk_primop(int64_t id);

// Create and initialize the default environment with primitives
unsigned default_environment(void);

// Clone an environment's frame structure for an isolated R7RS eval target.
unsigned clone_environment(unsigned env);

// Build an isolated environment containing only the exports selected by the
// R7RS import-set list. The source environment is not mutated.
unsigned environment_with_imports(unsigned source, unsigned specs);

// Mark an environment root as immutable and query that marker.
void mark_immutable_environment(unsigned env);
bool environment_is_immutable(unsigned env);

#endif // ENV_H
