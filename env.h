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
    for (unsigned v = vars; IS_PAIR(v); v = cdr(v))
        note_binder(car(v));
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

// Binding stamps, for the VM's inline variable caches (see env.c).
//
// env_binding_stamp(var) changes whenever a binding for var - or for another
// name hashing to the same bucket - is added to a frame some cache entry may
// have seen (see env_note_binding_cached for the frames that are exempt),
// and after every collection. A cache entry recorded under the current stamp
// therefore knows that none of the frames it was validated against has
// gained a binding for var since, and that every cell index it holds still
// names the same cell. Both counters are 64-bit and only ever increase, so
// their sum changes whenever either does and never returns to an old value.
//
// env_binding_dynamic(var) is set, for good, once a binding for var has been
// added to a frame that is not the outermost of its environment (an internal
// or let-body define, rather than a top-level one). Until then every frame
// that could stand between a use of var and its binding holds only the names
// it was created with, so the cache need not look for var in them. Setting
// it always changes the stamp as well.
#define ENV_BIND_BUCKETS 8192
extern uint64_t env_bind_stamps[ENV_BIND_BUCKETS];
extern bool env_bind_dynamic[ENV_BIND_BUCKETS];
extern uint64_t env_gc_epoch;

static inline unsigned env_bind_bucket(int64_t var)
{
    return (unsigned)(((uint64_t)var * 0x9E3779B97F4A7C15ull) >> 51);
}

static inline uint64_t env_binding_stamp(int64_t var)
{
    return env_bind_stamps[env_bind_bucket(var)] + env_gc_epoch;
}

static inline bool env_binding_dynamic(int64_t var)
{
    return env_bind_dynamic[env_bind_bucket(var)];
}

// The cache has just recorded a resolution of var against some environment.
// Remember how far allocation had got: a frame allocated after this point is
// in no environment any cache entry for var has seen, so a binding added to
// it cannot make one stale. That lets a let body define its variables into
// its fresh frame on every iteration without changing their stamps. The
// collection count is kept (truncated) beside the allocation pointer because
// the pointer restarts at every collection; see note_binding_added.
extern uint64_t env_bind_marks[ENV_BIND_BUCKETS];

static inline void env_note_binding_cached(int64_t var)
{
    env_bind_marks[env_bind_bucket(var)] =
        ((uint64_t)(uint32_t)env_gc_epoch << 32) | ctx.nursery_ptr;
}

// Bind parameters to arguments, supporting variadic (a b . rest) syntax
unsigned bind_params(unsigned params, unsigned args);

// Create a primitive operation builtin
unsigned mk_primop(int64_t id);

// Bind name to primitive prim_id in env
void define_primitive(const char *name, unsigned prim_id, unsigned env);

// Create and initialize the default environment with primitives
unsigned default_environment(void);

// Clone an environment's frame structure for an isolated R7RS eval target.
unsigned clone_environment(unsigned env);

// Build an isolated environment containing only the exports selected by the
// R7RS import-set list. The source environment is not mutated.
unsigned environment_with_imports(unsigned source, unsigned specs);
// Copy only the syntactic keyword bindings of source (R5RS null-environment).
unsigned syntax_environment(unsigned source);

// Mark an environment root as immutable and query that marker.
void mark_immutable_environment(unsigned env);
bool environment_is_immutable(unsigned env);
bool env_binding_is_immutable(int64_t var, unsigned env);

// The environment to consult for the exception machinery's state:
// *current-exception-handler* and *default-exception-handler*. That state is
// dynamic, but it is stored in ordinary
// variables of the environment the stdlib was loaded into, and
// with-exception-handler is a stdlib closure that set!s them there. So
// reading them from wherever the error happened is wrong whenever that is a
// different environment: a scheme-report-environment clone carries its own
// stale copies, an (environment ...) import set carries none. Returns the
// interaction environment when there is one, else FALLBACK (the C test
// harnesses run without main.c and bind what they need themselves).
unsigned exception_state_env(unsigned fallback);

// GC support: the frame index holds cell indices, which move.
void env_index_gc_update(unsigned (*collector)(unsigned));

// Frame index lookup for the VM's inline cache; see env.c.
bool env_frame_index_lookup(unsigned frame, int64_t var, unsigned *cell_out);

#endif // ENV_H
