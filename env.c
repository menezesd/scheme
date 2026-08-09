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
#include <string.h>

// Atom IDs are allocated from the nonnegative range.  Keep environment
// metadata in the atom-shaped binding list without colliding with any user or
// hygiene symbol.
#define IMMUTABLE_ENV_MARKER_ID INT64_MIN

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

// Bumped whenever a cached binding-list acyclicity verdict could go stale:
// on every defvar (which conses a new frame head) and after every GC (which
// can recycle cell indices). See env_binding_list_acyclic.
// Starts at 1 so that a zero-initialized cache slot never looks valid.
static unsigned acyclic_epoch = 1;

// Invalidate all cache entries
static inline void invalidate_lookup_cache(void)
{
    for (int i = 0; i < LOOKUP_CACHE_SIZE; i++) {
        lookup_cache[i].var = -1;
    }
    acyclic_epoch++;
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

unsigned make_binding_ref_cell(unsigned target_var, unsigned target_val_cell)
{
    GC_GUARD;
    gc_protect(&target_var);
    gc_protect(&target_val_cell);
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

bool environment_is_immutable(unsigned env)
{
    unsigned frame = 0;
    unsigned next = 0;
    if (!env_frame(env, &frame, &next))
        return false;
    unsigned vars = car(frame);
    // The marker, when present, is always the FIRST entry:
    // mark_immutable_environment conses it onto the front of the binding
    // list, and defvar refuses to extend an environment that is already
    // immutable, so nothing can ever be prepended in front of it. Searching
    // the whole list (plus a cycle pre-walk) cost O(frame) on every define
    // and on every successful set!, against a global frame of ~950 bindings.
    // Reading position 0 is equivalent, and needs no cycle check because it
    // dereferences a single cons.
    if (IS_PAIR(vars))
        vars = car(vars);
    return IS_ATOM(vars) && CELL_ID(vars) == IMMUTABLE_ENV_MARKER_ID;
}

void mark_immutable_environment(unsigned env)
{
    GC_GUARD;
    unsigned frame = 0;
    unsigned next = 0;
    unsigned vars = 0;
    unsigned vals = 0;
    unsigned marker = 0;
    unsigned marker_value = ctx.atom_false;
    unsigned new_vars = 0;
    unsigned new_vals = 0;
    if (environment_is_immutable(env) || !env_frame(env, &frame, &next))
        return;
    gc_protect(&env);
    gc_protect(&frame);
    gc_protect(&vars);
    gc_protect(&vals);
    gc_protect(&marker);
    gc_protect(&marker_value);
    gc_protect(&new_vars);
    gc_protect(&new_vals);
    vars = car(frame);
    vals = cdr(frame);
    marker = alloc();
    CELL_TYPE(marker) = BT_ATOM;
    CELL_ID(marker) = IMMUTABLE_ENV_MARKER_ID;
    new_vars = alloc_cons(marker, vars);
    new_vals = alloc_cons(marker_value, vals);
    cell_set_car(frame, new_vars);
    cell_set_cdr(frame, new_vals);
}

bool env_chain_acyclic(unsigned env)
{
    unsigned slow = env;
    unsigned fast = env;
    while (fast) {
        unsigned frame = 0;
        unsigned next = 0;
        if (!env_frame(fast, &frame, &next))
            return true;
        fast = next;
        if (!fast)
            return true;
        if (!env_frame(fast, &frame, &next))
            return true;
        fast = next;
        if (!env_frame(slow, &frame, &next))
            return true;
        slow = next;
        if (fast == slow)
            return false;
    }
    return true;
}

static bool binding_list_acyclic_uncached(unsigned vars)
{
    unsigned slow = vars;
    unsigned fast = vars;
    while (IS_PAIR(fast)) {
        fast = cdr(fast);
        if (!IS_PAIR(fast))
            return true;
        fast = cdr(fast);
        if (!IS_PAIR(slow))
            return true;
        slow = cdr(slow);
        if (fast == slow)
            return false;
    }
    return true;
}

// This runs on EVERY variable lookup, on every frame walked, before a single
// symbol is compared - and the global frame holds ~950 bindings after the
// stdlib loads. Uncached, the Floyd walk dominated the whole interpreter:
// stubbing it out entirely made a fib(25) + 1M-iteration-loop benchmark go
// from 4.77s to 0.30s (~16x).
//
// The verdict is memoized per binding-list head cell. Correctness rests on
// three properties:
//   - Within one epoch a cell index identifies exactly one cell. An index can
//     only be recycled by a GC, and every GC calls env_invalidate_cache(),
//     which bumps the epoch below.
//   - defvar conses a fresh head rather than mutating, and also bumps the
//     epoch, so a frame gaining a binding never reuses a stale verdict.
//   - The only in-place spine mutation is bind_params appending freshly
//     allocated cells (env.c:571,590). Appending fresh cells cannot introduce
//     a cycle, so a cached "acyclic" stays true; and nothing ever removes one,
//     so a cached "cyclic" stays false.
// A frame corrupted into a cycle by direct cell_set_cdr is therefore still
// rejected, provided it was not already probed in the same epoch - which is
// what the environment_frame_binding_cycle_is_rejected robustness test does.
#define ACYCLIC_CACHE_SIZE 64

typedef struct {
    unsigned head;
    unsigned epoch;
    bool acyclic;
} acyclic_cache_entry;

static acyclic_cache_entry acyclic_cache[ACYCLIC_CACHE_SIZE];

bool env_binding_list_acyclic(unsigned vars)
{
    // A non-pair (empty frame, or a rest-parameter atom) is trivially acyclic
    // and is the cheapest case, so it never reaches the cache.
    if (!IS_PAIR(vars))
        return true;

    acyclic_cache_entry *slot = &acyclic_cache[vars % ACYCLIC_CACHE_SIZE];
    if (slot->epoch == acyclic_epoch && slot->head == vars)
        return slot->acyclic;

    bool result = binding_list_acyclic_uncached(vars);
    slot->head = vars;
    slot->epoch = acyclic_epoch;
    slot->acyclic = result;
    return result;
}

static bool env_find_in_frame(int64_t var, unsigned vars, unsigned vals,
                              unsigned *val_cell_out, unsigned *value_out)
{
    if (!env_binding_list_acyclic(vars))
        return false;
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
    if (environment_is_immutable(env)) {
        show_error("define: environment is immutable");
        return TOK_ERROR;
    }

    // Invalidate lookup cache - new binding may shadow outer variables
    invalidate_lookup_cache();
    global_ic_epoch++;

    int64_t vid = CELL_ID(var);
    unsigned vals = cdr(frame);
    unsigned vars = car(frame);

    if (!env_binding_list_acyclic(vars)) {
        show_error("define: invalid environment frame");
        return TOK_ERROR;
    }

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
    unsigned ref = make_binding_ref_cell(target_var, target_val_cell);
    return defvar(var, ref, env);
}

unsigned env_find_binding_cell(int64_t var, unsigned env)
{
    if (!env_chain_acyclic(env))
        return 0;
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
    if (!env_chain_acyclic(env))
        return TOK_ERROR;
    while (env) {
        unsigned frame = 0;
        unsigned next = 0;
        if (!env_frame(env, &frame, &next))
            break;
        unsigned vars = car(frame);
        unsigned vals = cdr(frame);
        unsigned val_cell = 0;
        if (env_find_in_frame(var, vars, vals, &val_cell, NULL)) {
            if (environment_is_immutable(env)) {
                show_error("set!: environment is immutable");
                return TOK_ERROR;
            }
            return set_binding_value(val_cell, aval);
        }
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

    if (!env_chain_acyclic(env))
        return TOK_ERROR;

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

static unsigned clone_binding_chain(unsigned list)
{
    GC_GUARD;
    unsigned source = list;
    unsigned result = 0;
    unsigned tail = 0;
    gc_protect(&source);
    gc_protect(&result);
    gc_protect(&tail);

    while (IS_PAIR(source)) {
        unsigned value = car(source);
        gc_protect(&value);
        unsigned cell = alloc_cons(value, 0);
        gc_protect(&cell);
        if (tail)
            cell_set_cdr(tail, cell);
        else
            result = cell;
        tail = cell;
        source = cdr(source);
        gc_unprotect(2);
    }
    if (tail)
        cell_set_cdr(tail, source);
    return result;
}

unsigned clone_environment(unsigned env)
{
    GC_GUARD;
    if (!env_chain_acyclic(env)) {
        show_error("environment: invalid environment chain");
        return TOK_ERROR;
    }

    unsigned source = env;
    unsigned result = 0;
    unsigned vars = 0;
    unsigned vals = 0;
    unsigned frame = 0;
    unsigned copied_vars = 0;
    unsigned copied_vals = 0;
    unsigned copied_frame = 0;
    unsigned result_tail = 0;
    unsigned copied_env_cell = 0;
    gc_protect(&source);
    gc_protect(&result);
    gc_protect(&vars);
    gc_protect(&vals);
    gc_protect(&frame);
    gc_protect(&copied_vars);
    gc_protect(&copied_vals);
    gc_protect(&copied_frame);
    gc_protect(&result_tail);
    gc_protect(&copied_env_cell);

    while (IS_PAIR(source)) {
        frame = car(source);
        if (!IS_PAIR(frame) || !env_binding_list_acyclic(car(frame))) {
            show_error("environment: invalid environment frame");
            return TOK_ERROR;
        }
        vars = car(frame);
        vals = cdr(frame);
        if (!env_binding_list_acyclic(vals)) {
            show_error("environment: invalid environment values");
            return TOK_ERROR;
        }

        copied_vars = clone_binding_chain(vars);
        copied_vals = clone_binding_chain(vals);
        copied_frame = alloc_cons(copied_vars, copied_vals);
        copied_env_cell = alloc_cons(copied_frame, 0);
        if (result_tail)
            cell_set_cdr(result_tail, copied_env_cell);
        else
            result = copied_env_cell;
        result_tail = copied_env_cell;
        source = cdr(source);
    }
    if (source) {
        show_error("environment: invalid environment chain");
        return TOK_ERROR;
    }
    return result;
}

/* Project the implementation environment onto R7RS standard-library
 * exports.  This keeps environment specifiers from accidentally exposing
 * the whole interaction environment. */
static const char *const r7rs_base_exports[] = {
    "*", "+", "-", "...", "/", "<", "<=", "=", "=>", ">", ">=", "_",
    "abs", "and", "append", "apply", "assoc", "assq", "assv", "begin",
    "binary-port?", "boolean=?", "boolean?", "bytevector", "bytevector-append",
    "bytevector-copy", "bytevector-copy!", "bytevector-length", "bytevector-u8-ref",
    "bytevector-u8-set!", "bytevector?", "caar", "cadr", "call-with-current-continuation",
    "call-with-port", "call-with-values", "call/cc", "car", "case", "cdar", "cddr",
    "cdr", "ceiling", "char->integer", "char-ready?", "char<=?", "char<?", "char=?",
    "char>=?", "char>?", "char?", "close-input-port", "close-output-port", "close-port",
    "complex?", "cond", "cond-expand", "cons", "current-error-port", "current-input-port",
    "current-output-port", "define", "define-record-type", "define-syntax", "define-values",
    "denominator", "do", "dynamic-wind", "else", "eof-object", "eof-object?", "eq?",
    "equal?", "eqv?", "error", "error-object-irritants", "error-object-message",
    "error-object?", "even?", "exact", "exact-integer-sqrt", "exact-integer?", "exact?",
    "expt", "features", "file-error?", "floor", "floor-quotient", "floor-remainder",
    "floor/", "flush-output-port", "for-each", "gcd", "get-output-bytevector",
    "get-output-string", "guard", "if", "include", "include-ci", "inexact", "inexact?",
    "input-port-open?", "input-port?", "integer->char", "integer?", "lambda", "lcm",
    "length", "let", "let*", "let*-values", "let-syntax", "let-values", "letrec",
    "letrec*", "letrec-syntax", "list", "list->string", "list->vector", "list-copy",
    "list-ref", "list-set!", "list-tail", "list?", "make-bytevector", "make-list",
    "make-parameter", "make-string", "make-vector", "map", "max", "member", "memq",
    "memv", "min", "modulo", "negative?", "newline", "not", "null?", "number->string",
    "number?", "numerator", "odd?", "open-input-bytevector", "open-input-string",
    "open-output-bytevector", "open-output-string", "or", "output-port-open?", "output-port?",
    "pair?", "parameterize", "peek-char", "peek-u8", "port?", "positive?", "procedure?",
    "quasiquote", "quote", "quotient", "raise", "raise-continuable", "rational?",
    "rationalize", "read-bytevector", "read-bytevector!", "read-char", "read-error?",
    "read-line", "read-string", "read-u8", "real?", "remainder", "reverse", "round",
    "set-car!", "set-cdr!", "string", "string->list", "string->number", "string->symbol",
    "string->utf8", "string->vector", "string-append", "string-copy", "string-copy!",
    "string-fill!", "string-for-each", "string-length", "string-map", "string-ref",
    "string-set!", "string<=?", "string<?", "string=?", "string>=?", "string>?", "string?",
    "substring", "symbol->string", "symbol=?", "symbol?", "syntax-error", "syntax-rules",
    "textual-port?", "truncate", "truncate-quotient", "truncate-remainder", "truncate/",
    "u8-ready?", "unless", "unquote", "unquote-splicing", "utf8->string", "values", "vector",
    "vector->list", "vector->string", "vector-append", "vector-copy", "vector-copy!",
    "vector-fill!", "vector-for-each", "vector-length", "vector-map", "vector-ref", "vector-set!",
    "vector?", "when", "with-exception-handler", "write-bytevector", "write-char", "write-string",
    "write-u8", "zero?", NULL};

static const char *valid_atom_name(unsigned atom)
{
    if (!IS_ATOM(atom) || CELL_ID(atom) < 0 ||
        (uint64_t)CELL_ID(atom) >= ctx.atom_table_cap)
        return NULL;
    return ctx.atom_table[CELL_ID(atom)];
}

static bool r7rs_in_exports(const char *name, const char *const *exports)
{
    for (size_t i = 0; exports[i]; i++)
        if (strcmp(name, exports[i]) == 0)
            return true;
    return false;
}

static bool r7rs_library_exports(const char *name, unsigned spec)
{
    if (!IS_PAIR(spec) || !valid_atom_name(car(spec)) ||
        strcmp(valid_atom_name(car(spec)), "scheme") != 0)
        return false;
    spec = cdr(spec);
    if (!IS_PAIR(spec) || !valid_atom_name(car(spec)) || cdr(spec) != 0)
        return false;
    const char *library = valid_atom_name(car(spec));
    if (strcmp(library, "base") == 0)
        return r7rs_in_exports(name, r7rs_base_exports);
    if (strcmp(library, "case-lambda") == 0)
        return strcmp(name, "case-lambda") == 0;
    if (strcmp(library, "char") == 0) {
        static const char *const exports[] = {
            "char-alphabetic?", "char-ci<=?", "char-ci<?", "char-ci=?", "char-ci>=?",
            "char-ci>?", "char-downcase", "char-foldcase", "char-lower-case?", "char-numeric?",
            "char-upcase", "char-upper-case?", "char-whitespace?", "digit-value", "string-ci<=?",
            "string-ci<?", "string-ci=?", "string-ci>=?", "string-ci>?", "string-downcase",
            "string-foldcase", "string-upcase", NULL};
        return r7rs_in_exports(name, exports);
    }
    if (strcmp(library, "complex") == 0) {
        static const char *const exports[] = {
            "angle", "imag-part", "magnitude", "make-polar", "make-rectangular",
            "real-part", NULL};
        return r7rs_in_exports(name, exports);
    }
    if (strcmp(library, "cxr") == 0) {
        static const char *const exports[] = {
            "caaaar", "caaadr", "caaar", "caadar", "caaddr", "caadr",
            "cadaar", "cadadr", "cadar", "caddar", "cadddr", "caddr",
            "cdaaar", "cdaadr", "cdaar", "cdadar", "cdaddr", "cdadr",
            "cddaar", "cddadr", "cddar", "cdddar", "cddddr", "cdddr", NULL};
        return r7rs_in_exports(name, exports);
    }
    if (strcmp(library, "eval") == 0)
        return strcmp(name, "environment") == 0 || strcmp(name, "eval") == 0;
    if (strcmp(library, "file") == 0) {
        static const char *const exports[] = {"call-with-input-file", "call-with-output-file",
            "delete-file", "file-exists?", "open-binary-input-file", "open-binary-output-file",
            "open-input-file", "open-output-file", "with-input-from-file", "with-output-to-file", NULL};
        return r7rs_in_exports(name, exports);
    }
    if (strcmp(library, "inexact") == 0) {
        static const char *const exports[] = {"acos", "asin", "atan", "cos", "exp", "finite?",
            "infinite?", "log", "nan?", "sin", "sqrt", "tan", NULL};
        return r7rs_in_exports(name, exports);
    }
    if (strcmp(library, "lazy") == 0) {
        static const char *const exports[] = {"delay", "delay-force", "force", "make-promise", "promise?", NULL};
        return r7rs_in_exports(name, exports);
    }
    if (strcmp(library, "load") == 0) return strcmp(name, "load") == 0;
    if (strcmp(library, "process-context") == 0) {
        static const char *const exports[] = {"command-line", "emergency-exit", "exit",
            "get-environment-variable", "get-environment-variables", NULL};
        return r7rs_in_exports(name, exports);
    }
    if (strcmp(library, "read") == 0) return strcmp(name, "read") == 0;
    if (strcmp(library, "repl") == 0) return strcmp(name, "interaction-environment") == 0;
    if (strcmp(library, "sort") == 0) {
        static const char *const exports[] = {"list-sort", "vector-sort", "vector-sort!", NULL};
        return r7rs_in_exports(name, exports);
    }
    if (strcmp(library, "time") == 0) {
        static const char *const exports[] = {"current-jiffy", "current-second", "jiffies-per-second", NULL};
        return r7rs_in_exports(name, exports);
    }
    if (strcmp(library, "write") == 0) {
        static const char *const exports[] = {"display", "write", "write-shared", "write-simple", NULL};
        return r7rs_in_exports(name, exports);
    }
    return false;
}

static bool proper_finite_list(unsigned list)
{
    if (!env_binding_list_acyclic(list))
        return false;
    while (IS_PAIR(list))
        list = cdr(list);
    return list == 0;
}

static bool valid_identifier_list(unsigned list)
{
    if (!proper_finite_list(list))
        return false;
    while (IS_PAIR(list)) {
        if (!valid_atom_name(car(list)))
            return false;
        list = cdr(list);
    }
    return true;
}

static bool r7rs_known_library_name(const char *name)
{
    static const char *const libraries[] = {
        "base", "case-lambda", "char", "complex", "cxr", "eval", "file",
        "inexact", "lazy", "load", "process-context", "read", "repl",
        "sort", "time", "write", NULL};
    for (size_t i = 0; libraries[i]; i++)
        if (strcmp(name, libraries[i]) == 0)
            return true;
    return false;
}

static bool r7rs_simple_library_spec_valid(unsigned spec)
{
    return IS_PAIR(spec) && valid_atom_name(car(spec)) &&
           strcmp(valid_atom_name(car(spec)), "scheme") == 0 &&
           IS_PAIR(cdr(spec)) && valid_atom_name(cadr(spec)) &&
           cddr(spec) == 0 &&
           r7rs_known_library_name(valid_atom_name(cadr(spec)));
}

static bool r7rs_import_set_valid(unsigned spec)
{
    if (r7rs_simple_library_spec_valid(spec))
        return true;
    if (!IS_PAIR(spec) || !valid_atom_name(car(spec)))
        return false;
    const char *form = valid_atom_name(car(spec));
    unsigned args = cdr(spec);
    if (strcmp(form, "only") == 0 || strcmp(form, "except") == 0)
        return IS_PAIR(args) && IS_PAIR(cdr(args)) &&
               r7rs_import_set_valid(car(args)) &&
               valid_identifier_list(cdr(args));
    if (strcmp(form, "prefix") == 0)
        return IS_PAIR(args) && IS_PAIR(cdr(args)) && cddr(args) == 0 &&
               valid_atom_name(cadr(args)) &&
               r7rs_import_set_valid(car(args));
    if (strcmp(form, "rename") == 0) {
        if (!IS_PAIR(args) || !r7rs_import_set_valid(car(args)) ||
            !IS_PAIR(cdr(args)) || !proper_finite_list(cdr(args)))
            return false;
        for (unsigned renames = cdr(args); IS_PAIR(renames);
             renames = cdr(renames)) {
            unsigned pair = car(renames);
            if (!IS_PAIR(pair) || !valid_atom_name(car(pair)) ||
                !IS_PAIR(cdr(pair)) || cddr(pair) != 0 ||
                !valid_atom_name(cadr(pair)))
                return false;
        }
        return true;
    }
    return false;
}

/* Resolve one R7RS import set for a source binding.  target_out receives the
 * local name under which the binding should be installed. */
static bool r7rs_import_name(unsigned source_atom, unsigned spec,
                             unsigned *target_out)
{
    GC_GUARD;
    unsigned target = 0;
    unsigned inner_target = 0;
    bool selected = false;
    gc_protect(&target);
    gc_protect(&inner_target);

    const char *source_name = valid_atom_name(source_atom);
    if (!source_name)
        goto done;
    if (r7rs_library_exports(source_name, spec)) {
        target = source_atom;
        selected = true;
        goto done;
    }
    if (!IS_PAIR(spec) || !valid_atom_name(car(spec)))
        goto done;

    const char *form = valid_atom_name(car(spec));
    unsigned args = cdr(spec);
    if (strcmp(form, "only") == 0) {
        if (!IS_PAIR(args) || !valid_identifier_list(cdr(args)))
            goto done;
        if (!r7rs_import_name(source_atom, car(args), &inner_target))
            goto done;
        for (unsigned ids = cdr(args); IS_PAIR(ids); ids = cdr(ids)) {
            if (CELL_ID(car(ids)) == CELL_ID(inner_target)) {
                target = inner_target;
                selected = true;
                break;
            }
        }
        goto done;
    }
    if (strcmp(form, "except") == 0) {
        if (!IS_PAIR(args) || !valid_identifier_list(cdr(args)))
            goto done;
        if (!r7rs_import_name(source_atom, car(args), &inner_target))
            goto done;
        selected = true;
        for (unsigned ids = cdr(args); IS_PAIR(ids); ids = cdr(ids)) {
            if (CELL_ID(car(ids)) == CELL_ID(inner_target)) {
                selected = false;
                break;
            }
        }
        if (selected)
            target = inner_target;
        goto done;
    }
    if (strcmp(form, "prefix") == 0) {
        if (!IS_PAIR(args) || !IS_PAIR(cdr(args)) || cddr(args) != 0 ||
            !valid_atom_name(cadr(args)))
            goto done;
        if (!r7rs_import_name(source_atom, car(args), &inner_target))
            goto done;
        const char *prefix = valid_atom_name(cadr(args));
        const char *inner_name = valid_atom_name(inner_target);
        if (!prefix || !inner_name)
            goto done;
        size_t prefix_len = strlen(prefix);
        size_t inner_len = strlen(inner_name);
        if (prefix_len > SIZE_MAX - inner_len - 1)
            goto done;
        char *combined = checked_malloc_size(prefix_len + inner_len + 1);
        if (!combined)
            goto done;
        memcpy(combined, prefix, prefix_len);
        memcpy(combined + prefix_len, inner_name, inner_len + 1);
        target = atom_from_string(combined);
        free(combined);
        selected = valid_atom_name(target) != NULL;
        goto done;
    }
    if (strcmp(form, "rename") == 0) {
        if (!IS_PAIR(args) || !proper_finite_list(cdr(args)))
            goto done;
        if (!r7rs_import_name(source_atom, car(args), &inner_target))
            goto done;
        target = inner_target;
        selected = true;
        for (unsigned renames = cdr(args); IS_PAIR(renames);
             renames = cdr(renames)) {
            unsigned pair = car(renames);
            if (!IS_PAIR(pair) || !IS_ATOM(car(pair)) ||
                !IS_PAIR(cdr(pair)) || cddr(pair) != 0 ||
                !valid_atom_name(cadr(pair))) {
                selected = false;
                break;
            }
            if (CELL_ID(car(pair)) == CELL_ID(inner_target))
                target = cadr(pair);
        }
    }

done:
    if (selected && target_out)
        *target_out = target;
    gc_unprotect(2);
    return selected;
}

static bool r7rs_import_set_has_name(unsigned source, unsigned spec,
                                     unsigned wanted)
{
    for (unsigned env = source; IS_PAIR(env); env = cdr(env)) {
        unsigned frame = car(env);
        if (!IS_PAIR(frame))
            return false;
        unsigned vars = car(frame);
        while (IS_PAIR(vars)) {
            unsigned target = 0;
            if (r7rs_import_name(car(vars), spec, &target) &&
                CELL_ID(target) == CELL_ID(wanted))
                return true;
            vars = cdr(vars);
        }
    }
    return false;
}

static bool r7rs_import_references_valid(unsigned source, unsigned spec)
{
    if (r7rs_simple_library_spec_valid(spec))
        return true;
    if (!IS_PAIR(spec) || !valid_atom_name(car(spec)))
        return false;

    const char *form = valid_atom_name(car(spec));
    unsigned args = cdr(spec);
    if (!IS_PAIR(args))
        return false;
    unsigned inner = car(args);
    if (!r7rs_import_references_valid(source, inner))
        return false;

    if (strcmp(form, "only") == 0 || strcmp(form, "except") == 0) {
        for (unsigned ids = cdr(args); IS_PAIR(ids); ids = cdr(ids))
            if (!r7rs_import_set_has_name(source, inner, car(ids)))
                return false;
        return true;
    }
    if (strcmp(form, "prefix") == 0)
        return IS_PAIR(cdr(args)) && cddr(args) == 0;
    if (strcmp(form, "rename") == 0) {
        for (unsigned renames = cdr(args); IS_PAIR(renames);
             renames = cdr(renames)) {
            unsigned pair = car(renames);
            if (!r7rs_import_set_has_name(source, inner, car(pair)))
                return false;
        }
        return true;
    }
    return false;
}

unsigned environment_with_imports(unsigned source, unsigned specs)
{
    GC_GUARD;
    gc_protect(&source);
    gc_protect(&specs);
    if (!proper_finite_list(specs)) {
        show_error("environment: expected a proper list of import sets");
        gc_unprotect(2);
        return TOK_ERROR;
    }
    if (!env_chain_acyclic(source)) {
        show_error("environment: invalid source environment");
        gc_unprotect(2);
        return TOK_ERROR;
    }
    unsigned source_tail = source;
    for (; IS_PAIR(source_tail); source_tail = cdr(source_tail)) {
        unsigned env = source_tail;
        unsigned frame = car(env);
        if (!IS_PAIR(frame) || !env_binding_list_acyclic(car(frame)) ||
            !env_binding_list_acyclic(cdr(frame)) ||
            !proper_finite_list(car(frame)) ||
            !proper_finite_list(cdr(frame))) {
            show_error("environment: invalid source environment");
            gc_unprotect(2);
            return TOK_ERROR;
        }
    }
    if (source_tail != 0) {
        show_error("environment: invalid source environment");
        gc_unprotect(2);
        return TOK_ERROR;
    }
    for (unsigned sets = specs; IS_PAIR(sets); sets = cdr(sets)) {
        if (!r7rs_import_set_valid(car(sets))) {
            show_error("environment: invalid import set");
            gc_unprotect(2);
            return TOK_ERROR;
        }
        if (!r7rs_import_references_valid(source, car(sets))) {
            show_error("environment: import set references an unknown identifier");
            gc_unprotect(2);
            return TOK_ERROR;
        }
    }
    unsigned result = empty_environment();
    unsigned target = 0;
    gc_protect(&result);
    gc_protect(&target);
    for (unsigned env = source; IS_PAIR(env); env = cdr(env)) {
        unsigned frame = car(env);
        if (!IS_PAIR(frame) || !env_binding_list_acyclic(car(frame))) {
            show_error("environment: invalid source environment");
            gc_unprotect(4);
            return TOK_ERROR;
        }
        unsigned vars = car(frame);
        unsigned vals = cdr(frame);
        if (!proper_finite_list(vars) || !proper_finite_list(vals)) {
            show_error("environment: invalid source bindings");
            gc_unprotect(4);
            return TOK_ERROR;
        }
        while (IS_PAIR(vars) && IS_PAIR(vals)) {
            unsigned var = car(vars);
            if (IS_ATOM(var)) {
                const char *name = valid_atom_name(var);
                if (!name) {
                    show_error("environment: invalid source binding");
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
                /* The source may contain shadowing frames.  Since the scan
                 * proceeds from inner to outer, do not let a later outer
                 * binding overwrite the binding visible from source. */
                unsigned visible_cell =
                    env_find_binding_cell(CELL_ID(var), source);
                if (visible_cell != vals) {
                    vars = cdr(vars);
                    vals = cdr(vals);
                    continue;
                }
                bool selected = false;
                for (unsigned sets = specs; IS_PAIR(sets); sets = cdr(sets)) {
                    if (r7rs_import_name(var, car(sets), &target)) {
                        selected = true;
                        break;
                    }
                }
                if (selected) {
                    unsigned value = lookup_silent(CELL_ID(var), source);
                    gc_protect(&value);
                    if (defvar(target, value, result) == TOK_ERROR) {
                        gc_unprotect(1);
                        gc_unprotect(4);
                        return TOK_ERROR;
                    }
                    gc_unprotect(1);
                }
            }
            vars = cdr(vars);
            vals = cdr(vals);
        }
        if (vars != 0 || vals != 0) {
            show_error("environment: mismatched source bindings");
            gc_unprotect(4);
            return TOK_ERROR;
        }
    }
    mark_immutable_environment(result);
    gc_unprotect(4);
    return result;
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
