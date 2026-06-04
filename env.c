/**
 * @file env.c
 * @brief Environment and variable binding management
 *
 * This file implements lexical environments for variable scoping.
 *
 * ## Environment Structure
 * Environments are represented as a list of frames, where each frame is a
 * cons cell of (vars . vals):
 *
 *   env = (frame1 . (frame2 . (frame3 . nil)))
 *
 * Each frame:
 *   frame = (vars . vals)
 *   vars  = list of variable atoms (or single atom for rest parameter)
 *   vals  = list of corresponding values (parallel structure to vars)
 *
 * ## Operations
 * - lookup: Search frames from innermost to outermost
 * - defvar: Add binding to current (innermost) frame
 * - setvar: Modify existing binding (error if not found)
 * - extend_env: Create new frame for function application
 *
 * ## Rest Parameters
 * For (lambda (a b . rest) ...), the vars list is dotted:
 *   vars = (a . (b . rest-atom))
 * The rest atom receives remaining arguments as a list.
 */

#include "env.h"
#include "context.h"
#include "primitive_table.h"

// ============================================================================
// Lookup Cache
// ============================================================================
// 8-entry associative cache for variable lookups with move-to-front on hit.
// Most code has high locality - the same variables are accessed repeatedly.
// Move-to-front keeps hot entries at the front, approximating LRU eviction.

#define LOOKUP_CACHE_SIZE 8

typedef struct {
    int64_t var;       // Variable atom ID (-1 = empty)
    unsigned env;      // Environment where found
    unsigned val_cell; // Cell containing the value (so mutations are visible)
} lookup_cache_entry;

static lookup_cache_entry lookup_cache[LOOKUP_CACHE_SIZE];

// Global inline cache epoch for bytecode IC invalidation
unsigned global_ic_epoch = 0;

// Invalidate all cache entries
static inline void invalidate_lookup_cache(void)
{
    for (int i = 0; i < LOOKUP_CACHE_SIZE; i++) {
        lookup_cache[i].var = -1;
    }
}

// Public function to invalidate cache (called after GC)
void env_invalidate_cache(void)
{
    invalidate_lookup_cache();
}

static unsigned deref_binding_value(unsigned val)
{
    if (IS_BINDING_REF(val))
        return car(CELL_CAR(val));
    return val;
}

static bool try_deref_binding_value(unsigned val, unsigned *out)
{
    if (IS_BINDING_REF(val)) {
        unsigned target_val_cell = CELL_CAR(val);
        if (!IS_PAIR(target_val_cell))
            return false;
        *out = car(target_val_cell);
        return true;
    }
    *out = val;
    return true;
}

static unsigned set_binding_value(unsigned val_cell, unsigned aval)
{
    if (!IS_PAIR(val_cell))
        return TOK_ERROR;
    unsigned old = car(val_cell);
    if (IS_BINDING_REF(old)) {
        unsigned target_val_cell = CELL_CAR(old);
        if (!IS_PAIR(target_val_cell))
            return TOK_ERROR;
        unsigned target_old = car(target_val_cell);
        cell_set_car(target_val_cell, aval);
        return deref_binding_value(target_old);
    }
    cell_set_car(val_cell, aval);
    return old;
}

static unsigned make_binding_ref(unsigned target_var, unsigned target_val_cell)
{
    unsigned ref = alloc();
    CELL_TYPE(ref) = BT_BINDING_REF;
    CELL_CAR(ref) = target_val_cell;
    CELL_CDR(ref) = target_var;
    return ref;
}

static bool env_frame(unsigned env, unsigned *frame_out, unsigned *next_out)
{
    if (!IS_PAIR(env))
        return false;
    unsigned frame = car(env);
    if (!IS_PAIR(frame))
        return false;
    *frame_out = frame;
    *next_out = cdr(env);
    return true;
}

static bool env_find_in_frame(int64_t var, unsigned vars, unsigned vals,
                              unsigned *val_cell_out, unsigned *value_out)
{
    while (vars) {
        if (IS_ATOM(vars)) {
            if (!IS_PAIR(vals))
                return false;
            if (CELL_ID(vars) == var) {
                if (val_cell_out)
                    *val_cell_out = vals;
                if (value_out &&
                    !try_deref_binding_value(car(vals), value_out))
                    return false;
                return true;
            }
            break;
        }

        if (!IS_PAIR(vars) || !IS_PAIR(vals))
            return false;

        unsigned atom = car(vars);
        if (!IS_ATOM(atom))
            return false;
        if (CELL_ID(atom) == var) {
            if (val_cell_out)
                *val_cell_out = vals;
            if (value_out && !try_deref_binding_value(car(vals), value_out))
                return false;
            return true;
        }

        vars = cdr(vars);
        vals = cdr(vals);
    }

    return false;
}

// ============================================================================
// Environment Structure
// ============================================================================
//
// Environments are represented as a list of frames, where each frame is a
// cons cell of (vars . vals):
//
//   env = (frame1 . (frame2 . (frame3 . nil)))
//
// Each frame:
//   frame = (vars . vals)
//   vars  = list of variable atoms (or single atom for rest parameter)
//   vals  = list of corresponding values (parallel structure to vars)
//
// Example: After (define x 1) (define y 2):
//   env = (((x . (y . nil)) . (1 . (2 . nil))) . parent-env)
//
// Special case - rest parameter (lambda (a . rest) ...):
//   vars = (a . rest-atom)  ; dotted list - rest is an atom, not a cons
//   vals = (1 . (2 3 4))    ; rest parameter gets remaining args as list
//
// ============================================================================
// Environment Operations
// ============================================================================

unsigned empty_environment(void)
{
    GC_GUARD;
    unsigned frame = alloc_cons(0, 0);
    gc_protect(&frame);
    unsigned env = alloc_cons(frame, 0);
    return env;
}

unsigned defvar(unsigned var, unsigned aval, unsigned env)
{
    if (!IS_ATOM(var)) {
        show_error("define: invalid variable");
        return TOK_ERROR;
    }
    unsigned frame = 0;
    unsigned next = 0;
    if (!env_frame(env, &frame, &next)) {
        (void)next;
        show_error("define: invalid environment");
        return TOK_ERROR;
    }

    // Invalidate lookup cache - new binding may shadow outer variables
    invalidate_lookup_cache();
    global_ic_epoch++;

    int64_t vid = CELL_ID(var);
    unsigned vals = cdr(frame);
    unsigned vars = car(frame);

    for (; vars;) {
        if (IS_ATOM(vars)) {
            if (!IS_PAIR(vals)) {
                show_error("define: invalid environment frame");
                return TOK_ERROR;
            }
            if (CELL_ID(vars) == vid) {
                cell_set_car(vals, aval);
                return var;
            } else {
                break;
            }
        }

        if (!IS_PAIR(vars) || !IS_PAIR(vals) || !IS_ATOM(car(vars))) {
            show_error("define: invalid environment frame");
            return TOK_ERROR;
        }
        if (CELL_ID(car(vars)) == vid) {
            cell_set_car(vals, aval);
            return var;
        }
        vars = cdr(vars);
        vals = cdr(vals);
    }

    vars = car(frame);
    vals = cdr(frame);
    // Protect all variables used across allocations
    unsigned new_vars, new_vals;
    {
        GC_GUARD;
        gc_protect(&frame);
        gc_protect(&var);
        gc_protect(&vars);
        gc_protect(&aval);
        gc_protect(&vals);
        new_vars = alloc_cons(var, vars);
        gc_protect(&new_vars);
        new_vals = alloc_cons(aval, vals);
    }
    cell_set_car(frame, new_vars);
    cell_set_cdr(frame, new_vals);
    return var;
}

unsigned defvar_alias(unsigned var, unsigned target_var, unsigned target_val_cell,
                      unsigned env)
{
    GC_GUARD;
    gc_protect(&var);
    gc_protect(&target_var);
    gc_protect(&target_val_cell);
    gc_protect(&env);
    unsigned ref = make_binding_ref(target_var, target_val_cell);
    return defvar(var, ref, env);
}

unsigned env_find_binding_cell(int64_t var, unsigned env)
{
    while (env) {
        unsigned frame = 0;
        unsigned next = 0;
        if (!env_frame(env, &frame, &next))
            return 0;
        unsigned vars = car(frame);
        unsigned vals = cdr(frame);
        unsigned val_cell = 0;
        if (env_find_in_frame(var, vars, vals, &val_cell, NULL))
            return val_cell;
        env = next;
    }

    return 0;
}

unsigned setvar(int64_t var, unsigned aval, unsigned env)
{
    while (env) {
        unsigned frame = 0;
        unsigned next = 0;
        if (!env_frame(env, &frame, &next))
            break;
        unsigned vars = car(frame);
        unsigned vals = cdr(frame);
        unsigned val_cell = 0;
        if (env_find_in_frame(var, vars, vals, &val_cell, NULL))
            return set_binding_value(val_cell, aval);
        env = next;
    }

    // Bounds check before accessing atom_table
    if (var >= 0 && (unsigned)var < ctx.atom_table_cap && ctx.atom_table[var]) {
        show_error("unbound variable: %s", ctx.atom_table[var]);
    } else {
        show_error("unbound variable: <invalid id %ld>", (long)var);
    }
    return TOK_ERROR;
}

// Move cache entry at index i to front (index 0), shifting others down
static inline void cache_move_to_front(int i)
{
    if (i == 0)
        return;
    lookup_cache_entry tmp = lookup_cache[i];
    for (int j = i; j > 0; j--) {
        lookup_cache[j] = lookup_cache[j - 1];
    }
    lookup_cache[0] = tmp;
}

// Insert new entry at front, shifting others down (evicts last)
static inline void cache_insert_front(int64_t var, unsigned env,
                                      unsigned val_cell)
{
    for (int j = LOOKUP_CACHE_SIZE - 1; j > 0; j--) {
        lookup_cache[j] = lookup_cache[j - 1];
    }
    lookup_cache[0].var = var;
    lookup_cache[0].env = env;
    lookup_cache[0].val_cell = val_cell;
}

// Internal lookup - returns TOK_ERROR if not found (no error message)
static unsigned lookup_internal(int64_t var, unsigned env)
{
    // Check cache first - scan all entries for match
    for (int i = 0; i < LOOKUP_CACHE_SIZE; i++) {
        if (lookup_cache[i].var == var && lookup_cache[i].env == env) {
            if (!IS_PAIR(lookup_cache[i].val_cell)) {
                lookup_cache[i].var = -1;
                break;
            }
            unsigned value = 0;
            if (!try_deref_binding_value(car(lookup_cache[i].val_cell),
                                         &value)) {
                lookup_cache[i].var = -1;
                break;
            }
            // Move to front on hit (LRU approximation)
            cache_move_to_front(i);
            return value;
        }
    }

    unsigned orig_env = env;
    while (env) {
        unsigned frame = 0;
        unsigned next = 0;
        if (!env_frame(env, &frame, &next))
            return TOK_ERROR;
        unsigned value = 0;
        unsigned val_cell = 0;
        if (env_find_in_frame(var, car(frame), cdr(frame), &val_cell,
                              &value)) {
            cache_insert_front(var, orig_env, val_cell);
            return value;
        }
        env = next;
    }

    return TOK_ERROR;
}

unsigned lookup(int64_t var, unsigned env)
{
    unsigned result = lookup_internal(var, env);
    if (result == TOK_ERROR) {
        // Bounds check before accessing atom_table
        if (var >= 0 && (unsigned)var < ctx.atom_table_cap && ctx.atom_table[var]) {
            show_error("undefined variable: %s", ctx.atom_table[var]);
        } else {
            show_error("undefined variable: <invalid id %ld>", (long)var);
        }
    }
    return result;
}

// Silent lookup - returns TOK_ERROR if not found (no error message)
// Used by the compiler to check for macros
unsigned lookup_silent(int64_t var, unsigned env)
{
    return lookup_internal(var, env);
}

unsigned bind_params(unsigned params, unsigned args)
{
    GC_GUARD;
    unsigned vars = 0, vals = 0;
    unsigned vars_tail = 0, vals_tail = 0;
    unsigned var = 0, val = 0, vc = 0, ac = 0;

    // Protect all variables once at function entry (not per-iteration).
    // lambda_params_valid may allocate while checking duplicate formals.
    gc_protect(&vars);
    gc_protect(&vals);
    gc_protect(&vars_tail);
    gc_protect(&vals_tail);
    gc_protect(&params);
    gc_protect(&args);
    gc_protect(&var);
    gc_protect(&val);
    gc_protect(&vc);
    gc_protect(&ac);

    if (!lambda_params_valid(params)) {
        show_error("procedure: invalid formals");
        return TOK_ERROR;
    }

    while (IS_PAIR(params)) {
        if (!IS_PAIR(args)) {
            show_error("procedure: too few arguments");
            return TOK_ERROR;
        }
        var = car(params);
        val = car(args);

        vc = alloc_cons(var, 0);
        ac = alloc_cons(val, 0);

        if (!vars) {
            vars = vc;
            vals = ac;
        } else {
            cell_set_cdr(vars_tail, vc);
            cell_set_cdr(vals_tail, ac);
        }
        vars_tail = vc;
        vals_tail = ac;

        params = cdr(params);
        args = cdr(args);
    }

    // Handle rest parameter (dotted notation)
    if (IS_ATOM(params)) {
        vc = alloc_cons(params, 0);
        ac = alloc_cons(args, 0);

        if (!vars) {
            vars = vc;
            vals = ac;
        } else {
            cell_set_cdr(vars_tail, vc);
            cell_set_cdr(vals_tail, ac);
        }
    } else if (args) {
        show_error("procedure: too many arguments");
        return TOK_ERROR;
    }

    return alloc_cons(vars, vals);
}

unsigned mk_primop(int64_t id)
{
    unsigned p = alloc();
    CELL_TYPE(p) = BT_BUILTIN;
    CELL_ID(p) = id;
    return p;
}

unsigned default_environment(void)
{
    GC_GUARD;
    unsigned env = empty_environment();
    gc_protect(&env);

    // Register all builtins from table
    const primitive_binding *bindings = primitive_bindings();
    size_t binding_count = primitive_binding_count();
    for (size_t i = 0; i < binding_count; i++) {
        unsigned name = atom_from_string(bindings[i].name);
        gc_protect(&name);
        unsigned prim = mk_primop(bindings[i].prim);
        gc_protect(&prim);
        defvar(name, prim, env);
        gc_unprotect(2);
    }

    // Register special atoms
    defvar(ctx.atom_true, ctx.atom_true, env);
    defvar(ctx.atom_false, ctx.atom_false, env);
    // Make 't' an alias for true
    unsigned t = atom_from_string("t");
    gc_protect(&t);
    defvar(t, ctx.atom_true, env);

    return env;
}
