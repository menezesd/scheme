/**
 * @file eval.c
 * @brief Trampoline-based CPS evaluator for Scheme
 *
 * This file implements the core evaluation loop using continuation-passing
 * style (CPS) with a trampoline to achieve proper tail call optimization.
 *
 * ## Evaluation Model
 * Instead of recursive C function calls, evaluation proceeds by:
 * 1. Setting the global `tramp` state (mode, expr/value, env, cont)
 * 2. Returning to the main loop
 * 3. The loop dispatches based on tramp.mode:
 *    - TRAMP_EVAL: Call eval_step() to evaluate an expression
 *    - TRAMP_APPLY: Call apply_cont_step() to apply value to continuation
 *    - TRAMP_DONE/TRAMP_ERROR: Exit the loop
 *
 * This design ensures tail calls don't grow the C stack, regardless of
 * recursion depth. A tail call simply sets up tramp state and returns.
 *
 * ## Continuations
 * Continuations are linked lists of frames, each containing:
 * - Type: What operation is pending (CONT_IF, CONT_EVAL_ARGS, etc.)
 * - Data: Operation-specific data (remaining exprs, accumulated values, etc.)
 * - Env: Environment for the pending operation
 * - Next: Link to the outer continuation
 *
 * ## Special Forms
 * Special forms (if, lambda, define, let, etc.) are handled via dispatch
 * to handlers in eval_forms.c.
 *
 * ## Function Application
 * 1. Evaluate function expression (CONT_EVAL_FN)
 * 2. Evaluate arguments left-to-right (CONT_EVAL_ARGS)
 * 3. Apply function to evaluated arguments (apply_function)
 */

#include "eval.h"
#include "bytecode.h"
#include "eval_internal.h"
#include "reader.h"
#include "writer.h"
#include <stdlib.h>

// ============================================================================
// Forward Declarations
// ============================================================================

#define MAX_EVAL_MACRO_EXPANSION_DEPTH 1000
#define MAX_QUASIQUOTE_SYNTAX_DEPTH 1024

static void eval_step(void);
static void apply_cont_step(void);

static unsigned eval_macro_expansion_depth = 0;

bool eval_note_macro_expansion(void)
{
    if (eval_macro_expansion_depth >= MAX_EVAL_MACRO_EXPANSION_DEPTH) {
        show_error("macro expansion exceeded maximum depth");
        return false;
    }

    eval_macro_expansion_depth++;
    return true;
}

void eval_reset_macro_expansion_depth(void)
{
    eval_macro_expansion_depth = 0;
}

unsigned eval_macro_expansion_depth_get(void)
{
    return eval_macro_expansion_depth;
}

void eval_macro_expansion_depth_set(unsigned depth)
{
    eval_macro_expansion_depth = depth;
}

// ============================================================================
// Quasiquote Expansion
// ============================================================================

static bool qq_spine_tail_unquote(unsigned l, unsigned env);

static bool qq_syntax_valid(unsigned x, unsigned env, int depth,
                            unsigned structural_depth)
{
    if (structural_depth >= MAX_QUASIQUOTE_SYNTAX_DEPTH) {
        show_error("quasiquote: template nesting too deep");
        return false;
    }
    if (IS_VECTOR(x)) {
        unsigned len = vector_len(x);
        for (unsigned i = 0; i < len; i++) {
            if (!qq_syntax_valid(vector_data_ptr(x)[i], env, depth,
                                 structural_depth + 1))
                return false;
        }
        return true;
    }

    if (!IS_PAIR(x))
        return true;
    if (pair_chain_is_circular(x)) {
        show_error("quasiquote: circular template");
        return false;
    }

    unsigned head = car(x);
    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(CELL_ID(head), env)) {
        if (!syntax_arity_checked(x, 1, 1, "unquote"))
            return false;
        if (depth == 1)
            return true;
        return qq_syntax_valid(cadr(x), env, depth - 1,
                               structural_depth + 1);
    }
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(CELL_ID(head), env)) {
        if (!syntax_arity_checked(x, 1, 1, "unquote-splicing"))
            return false;
        if (depth == 1)
            return true;
        return qq_syntax_valid(cadr(x), env, depth - 1,
                               structural_depth + 1);
    }
    if (IS_KEYWORD(head, ctx.kw_quasiquote) &&
        !is_keyword_shadowed(CELL_ID(head), env)) {
        return syntax_arity_checked(x, 1, 1, "quasiquote") &&
               qq_syntax_valid(cadr(x), env, depth + 1,
                               structural_depth + 1);
    }

    unsigned l = x;
    for (; IS_PAIR(l); l = cdr(l)) {
        // (... . ,tail) reads as (... unquote tail): the rest of the spine
        // is an unquote form, not further elements
        if (l != x && qq_spine_tail_unquote(l, env))
            return qq_syntax_valid(l, env, depth, structural_depth + 1);
        if (!qq_syntax_valid(car(l), env, depth, structural_depth + 1))
            return false;
    }
    if (l && !qq_syntax_valid(l, env, depth, structural_depth + 1))
        return false;

    return true;
}

// Helper for nested quasiquote expansion with depth tracking
static unsigned qq_expand_depth(unsigned x, unsigned env, int depth)
{
    if (IS_VECTOR(x)) {
        GC_GUARD;
        gc_protect(&x);
        gc_protect(&env);
        unsigned len = vector_len(x);
        unsigned result = make_vector(len, 0);
        if (result == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&result);
        for (unsigned i = 0; i < len; i++) {
            unsigned elem = vector_data_ptr(x)[i];
            gc_protect(&elem);
            unsigned expanded = qq_expand_depth(elem, env, depth);
            if (expanded == TOK_ERROR)
                return TOK_ERROR;
            vector_set_elem(result, i, expanded);
            gc_unprotect(1);
        }
        return result;
    }

    if (!IS_PAIR(x)) {
        return x;
    }

    GC_GUARD;
    unsigned head = car(x);
    gc_protect(&x);
    gc_protect(&env);
    gc_protect(&head);

    // Check for (unquote expr) - only if unquote is not shadowed
    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(CELL_ID(head), env)) {
        if (depth == 1) {
            // Evaluate the unquote
            return eval_cps(cadr(x), env);
        } else {
            // Keep the unquote but expand inside at lower depth
            unsigned inner = qq_expand_depth(cadr(x), env, depth - 1);
            if (inner == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&inner);
            unsigned result_tail = alloc_cons(inner, 0);
            gc_protect(&result_tail);
            unsigned result = alloc_cons(head, result_tail);
            return result;
        }
    }

    // Check for (unquote-splicing expr) - only if not shadowed
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(CELL_ID(head), env)) {
        if (depth == 1) {
            show_error("unquote-splicing not inside list");
            return TOK_ERROR;
        } else {
            // Keep but expand inside at lower depth
            unsigned inner = qq_expand_depth(cadr(x), env, depth - 1);
            if (inner == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&inner);
            unsigned result_tail = alloc_cons(inner, 0);
            gc_protect(&result_tail);
            unsigned result = alloc_cons(head, result_tail);
            return result;
        }
    }

    // Check for nested (quasiquote expr) - only if not shadowed
    if (IS_KEYWORD(head, ctx.kw_quasiquote) &&
        !is_keyword_shadowed(CELL_ID(head), env)) {
        unsigned inner = qq_expand_depth(cadr(x), env, depth + 1);
        if (inner == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&inner);
        unsigned result_tail = alloc_cons(inner, 0);
        gc_protect(&result_tail);
        unsigned result = alloc_cons(head, result_tail);
        return result;
    }

    // Recursively process list
    unsigned result = 0, tail = 0;
    gc_protect(&result);
    gc_protect(&tail);
    unsigned l = x;
    gc_protect(&l);
    for (; IS_PAIR(l); l = cdr(l)) {
        // (... . ,tail) reads as (... unquote tail): the rest of the spine
        // is a tail form, handled by the improper-tail expansion below
        if (l != x && qq_spine_tail_unquote(l, env))
            break;
        unsigned elem = car(l);

        // Check if element is (unquote-splicing expr) - only if not shadowed
        if (IS_PAIR(elem) && IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
            !is_keyword_shadowed(CELL_ID(car(elem)), env)) {
            if (depth == 1) {
                // Evaluate and splice
                unsigned spliced = eval_cps(cadr(elem), env);
                if (spliced == TOK_ERROR) {
                    return spliced;
                }
                gc_protect(&spliced);
                if (!list_length_checked(spliced, NULL,
                                         "unquote-splicing")) {
                    return TOK_ERROR;
                }
                for (; IS_PAIR(spliced); spliced = cdr(spliced)) {
                    list_append(&result, &tail, car(spliced));
                }
                gc_unprotect(1); // spliced - per-iteration cleanup
            } else {
                // Keep but expand inside. Rebuild with the original head
                // atom cell (consing the raw keyword id would put an
                // invalid cell reference into the result)
                unsigned inner = qq_expand_depth(cadr(elem), env, depth - 1);
                if (inner == TOK_ERROR) {
                    return inner;
                }
                gc_protect(&inner);
                unsigned kept_tail = alloc_cons(inner, 0);
                gc_protect(&kept_tail);
                unsigned kept = alloc_cons(car(elem), kept_tail);
                gc_unprotect(2); // kept_tail, inner - per-iteration cleanup
                list_append(&result, &tail, kept);
            }
        } else {
            unsigned expanded = qq_expand_depth(elem, env, depth);
            if (expanded == TOK_ERROR) {
                return expanded;
            }
            list_append(&result, &tail, expanded);
        }
    }
    if (l) {
        unsigned expanded_tail = qq_expand_depth(l, env, depth);
        if (expanded_tail == TOK_ERROR)
            return TOK_ERROR;
        if (tail)
            cell_set_cdr(tail, expanded_tail);
        else
            result = expanded_tail;
    }
    return result;
}

unsigned qq_expand_cps(unsigned x, unsigned env)
{
    if (!qq_syntax_valid(x, env, 1, 0))
        return TOK_ERROR;
    return qq_expand_depth(x, env, 1);
}

// ============================================================================
// Quasiquote Transformation
// ============================================================================
// Transform a quasiquote template into an ordinary expression built from
// primitive applications (cons/append/vector) and quote forms. Evaluating
// that expression through the normal trampoline keeps first-class
// continuations working across unquotes: the old approach of running a
// nested evaluator inside the expander could neither escape from nor
// re-enter the quasiquote when a continuation fired.

static unsigned qq_quote_expr(unsigned x)
{
    GC_GUARD;
    gc_protect(&x);
    unsigned tail = alloc_cons(x, 0);
    gc_protect(&tail);
    return alloc_cons(ctx.atom_quote, tail);
}

// Build (op a b) with op a builtin primitive value, immune to shadowing
static unsigned qq_call2(int64_t prim, unsigned a, unsigned b)
{
    GC_GUARD;
    gc_protect(&a);
    gc_protect(&b);
    unsigned args = alloc_cons(b, 0);
    gc_protect(&args);
    args = alloc_cons(a, args);
    gc_protect(&args);
    unsigned op = mk_primop(prim);
    gc_protect(&op);
    return alloc_cons(op, args);
}

// Build code producing (sym . (inner-value . ())) for reconstructing
// nested unquote/quasiquote forms at depth > 1
static unsigned qq_keyword_pair_expr(unsigned sym_atom, unsigned inner_expr)
{
    GC_GUARD;
    gc_protect(&sym_atom);
    gc_protect(&inner_expr);
    unsigned nil_expr = qq_quote_expr(0);
    gc_protect(&nil_expr);
    unsigned tail_expr = qq_call2(PCONS, inner_expr, nil_expr);
    gc_protect(&tail_expr);
    unsigned sym_expr = qq_quote_expr(sym_atom);
    gc_protect(&sym_expr);
    return qq_call2(PCONS, sym_expr, tail_expr);
}

// True when the rest of a template spine is an unquote/unquote-splicing/
// quasiquote tail form, e.g. `(a . ,b) which reads as (a unquote b)
static bool qq_spine_tail_unquote(unsigned l, unsigned env)
{
    unsigned h = car(l);
    return (IS_KEYWORD(h, ctx.kw_unquote) &&
            !is_keyword_shadowed(CELL_ID(h), env)) ||
           (IS_KEYWORD(h, ctx.kw_unquote_splicing) &&
            !is_keyword_shadowed(CELL_ID(h), env)) ||
           (IS_KEYWORD(h, ctx.kw_quasiquote) &&
            !is_keyword_shadowed(CELL_ID(h), env));
}

static unsigned qq_transform(unsigned x, unsigned env, int depth)
{
    if (IS_VECTOR(x)) {
        GC_GUARD;
        gc_protect(&x);
        gc_protect(&env);
        unsigned len = vector_len(x);
        bool has_splice = false;
        for (unsigned i = 0; i < len && depth == 1; i++) {
            unsigned elem = vector_data_ptr(x)[i];
            if (IS_PAIR(elem) &&
                IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
                !is_keyword_shadowed(CELL_ID(car(elem)), env))
                has_splice = true;
        }
        if (has_splice) {
            // `#(a ,@b c) is (list->vector `(a ,@b c)) (R7RS 4.2.8)
            unsigned elems = 0;
            gc_protect(&elems);
            for (unsigned i = len; i > 0; i--)
                elems = alloc_cons(vector_data_ptr(x)[i - 1], elems);
            unsigned list_expr = qq_transform(elems, env, depth);
            if (list_expr == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&list_expr);
            unsigned args = alloc_cons(list_expr, 0);
            gc_protect(&args);
            unsigned op = mk_primop(PLIST2VEC);
            gc_protect(&op);
            return alloc_cons(op, args);
        }
        unsigned args = 0, args_tail = 0;
        gc_protect(&args);
        gc_protect(&args_tail);
        for (unsigned i = 0; i < len; i++) {
            unsigned elem = vector_data_ptr(x)[i];
            gc_protect(&elem);
            unsigned e = qq_transform(elem, env, depth);
            if (e == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&e);
            list_append(&args, &args_tail, e);
            gc_unprotect(2);
        }
        unsigned op = mk_primop(PVECTOR);
        gc_protect(&op);
        return alloc_cons(op, args);
    }

    if (!IS_PAIR(x))
        return qq_quote_expr(x);

    GC_GUARD;
    gc_protect(&x);
    gc_protect(&env);
    unsigned head = car(x);
    gc_protect(&head);

    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(CELL_ID(head), env)) {
        if (depth == 1)
            return cadr(x);
        unsigned inner = qq_transform(cadr(x), env, depth - 1);
        if (inner == TOK_ERROR)
            return TOK_ERROR;
        return qq_keyword_pair_expr(head, inner);
    }
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(CELL_ID(head), env)) {
        if (depth == 1) {
            show_error("unquote-splicing not inside list");
            return TOK_ERROR;
        }
        unsigned inner = qq_transform(cadr(x), env, depth - 1);
        if (inner == TOK_ERROR)
            return TOK_ERROR;
        return qq_keyword_pair_expr(head, inner);
    }
    if (IS_KEYWORD(head, ctx.kw_quasiquote) &&
        !is_keyword_shadowed(CELL_ID(head), env)) {
        unsigned inner = qq_transform(cadr(x), env, depth + 1);
        if (inner == TOK_ERROR)
            return TOK_ERROR;
        return qq_keyword_pair_expr(head, inner);
    }

    // General list: collect the spine iteratively (flat templates can be
    // long), transform the tail, then fold back with cons/append
    unsigned rev_elems = 0;
    gc_protect(&rev_elems);
    unsigned l = x;
    gc_protect(&l);
    while (IS_PAIR(l)) {
        if (l != x && qq_spine_tail_unquote(l, env))
            break; // (... . ,tail) - the rest is an unquote form
        rev_elems = alloc_cons(car(l), rev_elems);
        l = cdr(l);
    }

    // A vector tail, as in `(a . #(,x)), is a template too.
    unsigned result = (IS_PAIR(l) || IS_VECTOR(l)) ? qq_transform(l, env, depth)
                                                   : qq_quote_expr(l);
    if (result == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&result);

    // rev_elems is rooted, but the cursor walking it was a by-value copy
    // that nothing updated. Each iteration allocates (the recursive
    // transform and the cons/append call), so after a collection r pointed
    // at the old location of a cell that had moved - and its cdr was
    // whatever now lived there. Under GC stress the output contained
    // reclaimed cells spliced in as elements.
    unsigned r = 0;
    gc_protect(&r);
    for (r = rev_elems; r; r = cdr(r)) {
        unsigned elem = car(r);
        gc_protect(&elem);
        if (depth == 1 && IS_PAIR(elem) &&
            IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
            !is_keyword_shadowed(CELL_ID(car(elem)), env)) {
            // ,@e: splice - append validates e is a proper list
            result = qq_call2(PAPPEND, cadr(elem), result);
        } else {
            unsigned elem_expr = qq_transform(elem, env, depth);
            if (elem_expr == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&elem_expr);
            result = qq_call2(PCONS, elem_expr, result);
            gc_unprotect(1);
        }
        gc_unprotect(1);
    }
    return result;
}

unsigned qq_transform_cps(unsigned x, unsigned env)
{
    if (!qq_syntax_valid(x, env, 1, 0))
        return TOK_ERROR;
    return qq_transform(x, env, 1);
}

// Route a C-level error through the Scheme exception system (CPS mirror of
// vm_signal_error): build an error object and apply the current
// *current-exception-handler* with a CONT_ERR_RETURN continuation. A handler
// that jumps to a continuation (guard, call/cc) replaces this continuation
// via its captured frame chain; one that returns normally lands in
// handle_cont_err_return and terminates (R7RS raise semantics).
//
// True while cps_signal_error is applying a handler synchronously, i.e.
// before control is back in the trampoline. A closure handler only sets up
// tramp state and returns, so for the normal case this window is tiny. An
// error signalled inside it means the handler itself failed on the spot -
// not a procedure, a primitive that rejects the error object, a closure of
// the wrong arity - and dispatching that error would apply the same handler
// again, recursing through apply_function until the C stack overflowed.
// eval_cps saves and clears it, so a nested evaluation a primitive runs
// from inside the window still dispatches its own errors normally.
static bool cps_handler_dispatch_active = false;

// ----------------------------------------------------------------------------
// Backtraces of unhandled errors
// ----------------------------------------------------------------------------
//
// The CPS evaluator keeps no record of which procedure an activation belongs
// to: applying a closure just evaluates its body in (frame . closure-env).
// So the procedures are recovered from the continuation chain when an error
// is about to reach the default handler, and only then. Each continuation
// frame carries the environment its pending work runs in; an activation is
// identified as a closure, bound somewhere in that environment's chain, whose
// own environment is the activation's parent and whose parameters are the
// frame's variables. When several closures fit (two top-level procedures of
// one parameter x), the frame's data - which points into the body being
// evaluated (see CONT_EVAL_ARGS) - picks the closure whose body contains it.

// Closures whose environment is `env`, found by scanning env's chain once.
// Each closure comes with the variable it was found bound to, which names a
// procedure the reader could not (a named let in expanded code, say).
typedef struct {
    unsigned env;
    unsigned *procs;
    unsigned *names;
    unsigned len, cap;
    // The last frame the data picked a closure for: the frames of a
    // recursion share a call site, so each is then found without a walk.
    unsigned last_data, last_proc, last_name;
} cps_bt_candidates;

static void cps_bt_add(cps_bt_candidates *c, unsigned proc, unsigned name)
{
    for (unsigned i = 0; i < c->len; i++)
        if (c->procs[i] == proc)
            return;
    if (c->len == c->cap) {
        unsigned cap = c->cap ? c->cap * 2 : 8;
        unsigned *procs = realloc(c->procs, cap * sizeof(unsigned));
        if (!procs)
            return;
        c->procs = procs;
        unsigned *names = realloc(c->names, cap * sizeof(unsigned));
        if (!names)
            return;
        c->names = names;
        c->cap = cap;
    }
    c->procs[c->len] = proc;
    c->names[c->len] = name;
    c->len++;
}

static void cps_bt_scan(cps_bt_candidates *c, unsigned env)
{
    c->env = env;
    c->len = 0;
    c->last_data = c->last_proc = c->last_name = 0;
    unsigned steps = 0;
    for (unsigned e = env; IS_PAIR(e) && steps < 1000000; e = cdr(e)) {
        unsigned frame = car(e);
        if (!IS_PAIR(frame))
            continue;
        unsigned vars = car(frame), vals = cdr(frame);
        for (; IS_PAIR(vars) && IS_PAIR(vals) && steps < 1000000;
             vars = cdr(vars), vals = cdr(vals), steps++) {
            unsigned v = car(vals);
            if (IS_FUNCTION(v) && IS_PAIR(CELL_CDR(v)) &&
                CELL_CDR(CELL_CDR(v)) == env)
                cps_bt_add(c, v, car(vars));
        }
    }
}

// Do a closure's parameters spell the variables of an activation frame?
static bool cps_bt_params_match(unsigned params, unsigned vars)
{
    for (unsigned n = 0; n < 1000; n++) {
        if (IS_PAIR(params)) {
            if (!IS_PAIR(vars) || car(params) != car(vars))
                return false;
            params = cdr(params);
            vars = cdr(vars);
        } else if (params) {
            // rest parameter: bind_params gives it the last slot
            return IS_PAIR(vars) && car(vars) == params && !cdr(vars);
        } else {
            return !vars;
        }
    }
    return false;
}

// Is cell one of the pairs of tree? A bounded walk: this runs per frame.
static bool cps_bt_contains(unsigned tree, unsigned cell)
{
    unsigned stack[256];
    unsigned sp = 0, visited = 0;
    stack[sp++] = tree;
    while (sp && visited < 20000) {
        unsigned x = stack[--sp];
        while (IS_PAIR(x) && visited < 20000) {
            visited++;
            if (x == cell)
                return true;
            if (IS_PAIR(car(x)) && sp < 256)
                stack[sp++] = car(x);
            x = cdr(x);
        }
    }
    return false;
}

// The closure an activation environment belongs to, or 0. *activation is
// set to the env node that identifies the activation (the closure's frame),
// *binding to the variable the closure was found under.
static unsigned cps_bt_identify(unsigned env, unsigned data,
                                cps_bt_candidates *cache,
                                unsigned *activation, unsigned *binding)
{
    *activation = env;
    *binding = 0;
    for (unsigned level = 0, e = env; level < 64 && IS_PAIR(e) &&
                                      IS_PAIR(cdr(e));
         level++, e = cdr(e)) {
        unsigned frame = car(e);
        if (!IS_PAIR(frame))
            continue;
        if (cache->env != cdr(e))
            cps_bt_scan(cache, cdr(e));
        if (IS_PAIR(data) && data == cache->last_data &&
            cps_bt_params_match(CELL_CAR(cache->last_proc), car(frame))) {
            *activation = e;
            *binding = cache->last_name;
            return cache->last_proc;
        }
        unsigned found = 0, matches = 0;
        for (unsigned i = 0; i < cache->len; i++) {
            unsigned proc = cache->procs[i];
            if (!cps_bt_params_match(CELL_CAR(proc), car(frame)))
                continue;
            matches++;
            if (!found) {
                found = proc;
                *binding = cache->names[i];
            }
            if (IS_PAIR(data) && cps_bt_contains(car(CELL_CDR(proc)), data)) {
                found = proc;
                *binding = cache->names[i];
                matches = 1;
                cache->last_data = data;
                cache->last_proc = proc;
                cache->last_name = cache->names[i];
                break;
            }
        }
        if (found && matches == 1) {
            *activation = e;
            return found;
        }
        if (found) {
            // Several closures fit and the frame's data names none of them.
            *activation = e;
            return 0;
        }
    }
    return 0;
}

static __attribute__((noinline, cold)) void
cps_capture_backtrace(unsigned handler, unsigned env, unsigned cont)
{
    unsigned dflt = lookup_silent(intern("*default-exception-handler*"),
                                  exception_state_env(env));
    if (dflt == TOK_ERROR || handler != dflt)
        return;
    backtrace_start();
    cps_bt_candidates cache = {0, NULL, NULL, 0, 0, 0, 0, 0};
    unsigned last_activation = 0, last_env = 0;
    unsigned steps = 0;
    for (unsigned k = cont; IS_CONT(k) && steps < 10000000; steps++) {
        unsigned type_data = CELL_CAR(k), env_next = CELL_CDR(k);
        if (!IS_PAIR(type_data) || !IS_PAIR(env_next))
            break;
        unsigned type = CELL_CAR(type_data);
        unsigned data = CELL_CDR(type_data);
        unsigned kenv = CELL_CAR(env_next);
        k = CELL_CDR(env_next);
        if (type == CONT_HALT || type == CONT_ERR_RETURN || !IS_PAIR(kenv))
            continue;
        // Frames of an activation already reported, cheaply: most pending
        // work runs in the activation's own environment.
        if (kenv == last_env || kenv == last_activation)
            continue;
        last_env = kenv;
        // Where the frame points into the code being evaluated.
        unsigned source = data;
        if (type == CONT_EVAL_ARGS && IS_PAIR(data) && IS_PAIR(cdr(data)))
            source = cdr(cdr(data));
        unsigned activation = kenv, binding = 0;
        unsigned proc =
            cps_bt_identify(kenv, source, &cache, &activation, &binding);
        if (activation == last_activation)
            continue;
        last_activation = activation;
        unsigned body = proc ? car(CELL_CDR(proc)) : 0;
        source_info info;
        bool located = body && reader_source_info(body, &info);
        const char *name = located && info.name >= 0 &&
                                   (uint64_t)info.name < ctx.atom_table_cap
                               ? ctx.atom_table[info.name]
                               : NULL;
        if (!name && proc && IS_ATOM(binding) && atom_is_valid(binding))
            name = ctx.atom_table[CELL_ID(binding)];
        if (!proc && !IS_PAIR(cdr(kenv)))
            continue; // top level
        if (backtrace_hides(name, located ? info.file : NULL))
            continue;
        unsigned line = located ? info.line : 0;
        const void *key = proc ? (const void *)(uintptr_t)body
                               : (const void *)(uintptr_t)activation;
        if (!backtrace_next(key, line))
            continue;
        char label[160];
        backtrace_lambda_label(label, sizeof(label), name,
                               proc ? CELL_CAR(proc)
                                    : (IS_PAIR(car(activation))
                                           ? car(car(activation))
                                           : 0));
        backtrace_describe(label, located ? info.file : NULL, line);
    }
    free(cache.procs);
    free(cache.names);
    backtrace_end();
}

static void cps_signal_error(const char *msg, unsigned env, unsigned cont)
{
    if (!msg)
        msg = "unknown error";
    if (cps_handler_dispatch_active) {
        // msg may be ctx.last_error itself, which show_error overwrites.
        char reason[sizeof(ctx.last_error)];
        snprintf(reason, sizeof(reason), "%s", msg);
        show_error("exception handler failed: %s", reason);
        tramp_error();
        return;
    }
    unsigned handler = lookup_silent(intern("*current-exception-handler*"),
                                     exception_state_env(env));
    if (handler == TOK_ERROR) {
        tramp_error();
        return;
    }
    GC_GUARD;
    gc_protect(&handler);
    gc_protect(&env);
    gc_protect(&cont);
    unsigned error_obj = make_error_object_c(msg);
    if (error_obj == TOK_ERROR) {
        tramp_error();
        return;
    }
    gc_protect(&error_obj);
    unsigned args = alloc_cons(error_obj, 0);
    gc_protect(&args);
    cps_capture_backtrace(handler, env, cont);
    unsigned err_cont = make_cont(CONT_ERR_RETURN, handler, env, cont);
    cps_handler_dispatch_active = true;
    apply_function(handler, args, env, err_cont);
    cps_handler_dispatch_active = false;
}

void cps_signal_current_error(unsigned env, unsigned cont)
{
    cps_signal_error(ctx.last_error[0] ? ctx.last_error : "evaluation error",
                     env, cont);
}

// ============================================================================
// CPS Evaluator - Eval Step
// ============================================================================

static void eval_step(void)
{
    unsigned id = tramp.expr;
    unsigned env = tramp.env;
    unsigned cont = tramp.cont;

    // Macro-expansion depth is scoped to the whole form here in eval_cps:
    // resetting it per node (constants, variables, applications) would let
    // self-referential macros that interleave constants escape the cap.
    if (!id) {
        tramp_apply(0, cont);
        return;
    }
    if (IS_FIXNUM(id)) {
        tramp_apply(id, cont);
        return;
    }
    if (!IS_CELL(id)) {
        show_error("invalid expression");
        cps_signal_current_error(env, cont);
        return;
    }

    switch (CELL_TYPE(id)) {
    case BT_NUM:
    case BT_STRING:
    case BT_CHAR:
    case BT_VECTOR:
    case BT_INPORT:
    case BT_OUTPORT:
    case BT_FUNCTION:
    case BT_MACRO:
    case BT_BUILTIN:
    case BT_CONT:
        // Self-evaluating
        tramp_apply(id, cont);
        return;

    case BT_ATOM: {
        if (id == ctx.atom_true || id == ctx.atom_false) {
            tramp_apply(id, cont);
            return;
        }

        unsigned val = lookup(CELL_ID(id), env);
        if (val == TOK_ERROR) {
            cps_signal_error(ctx.last_error[0] ? ctx.last_error
                                               : "undefined variable",
                             env, cont);
            return;
        }
        tramp_apply(val, cont);
        return;
    }

    case BT_CONS: {
        unsigned head = car(id);
        // The head's value when dispatch_special_form already looked the
        // head identifier up; TOK_ERROR when the head must be evaluated.
        unsigned head_value = TOK_ERROR;

        // Check for special forms
        if (IS_ATOM(head)) {
            int64_t kw = CELL_ID(head);
            if (dispatch_special_form(kw, id, env, cont, &head_value)) {
                return;
            }
            // Boolean literals evaluate to themselves, not to a binding;
            // leave them to eval_step's atom case.
            if (head == ctx.atom_true || head == ctx.atom_false)
                head_value = TOK_ERROR;
        }

        // Not a special form - evaluate function position first
        // Protect head, arg_exprs, env and cont - use pointer version for GC
        // safety
        GC_GUARD;
        unsigned arg_exprs = cdr(id);
        if (!list_length_checked(arg_exprs, NULL, "application")) {
            cps_signal_current_error(env, cont);
            return;
        }
        gc_protect(&head);
        gc_protect(&head_value);
        gc_protect(&arg_exprs);
        gc_protect(&env);
        gc_protect(&cont);
        unsigned k =
            make_cont_from_protected(CONT_EVAL_FN, &arg_exprs, &env, &cont);
        if (head_value != TOK_ERROR)
            tramp_apply(head_value, k);
        else
            tramp_eval(head, env, k);
        return;
    }

    case BT_FREE:
        show_error("attempt to evaluate free memory");
        cps_signal_current_error(env, cont);
        return;

    default:
        tramp_apply(id, cont);
        return;
    }
}

// ============================================================================
// CPS Evaluator - Apply Continuation Step
// ============================================================================

static void apply_cont_step(void)
{
    unsigned val = tramp.value;
    unsigned k = tramp.cont;

    if (!IS_CONT(k)) {
        show_error("invalid continuation");
        tramp_error();
        return;
    }

    enum cont_type type = cont_type(k);
    unsigned data = cont_data(k);
    unsigned env = cont_env(k);
    unsigned next = cont_next(k);

    // The macro-expansion guard counts expansions but, being trampolined,
    // has no C-stack unwind to decrement on. Without a reset it was a
    // cumulative cap: any top-level form that expanded more than 1000 macro
    // uses in total - eval in a loop, say - failed with "exceeded maximum
    // depth" even though nothing was nested. A runaway macro is the one case
    // where expansion follows expansion with no other continuation ever
    // applied (CONT_MACRO_EXPAND re-dispatches the result straight back into
    // apply_syntax), so applying any *other* continuation is proof of
    // progress and the right moment to restart the count.
    if (type != CONT_MACRO_EXPAND)
        eval_reset_macro_expansion_depth();

    // Dispatch via function pointer table
    if (type < CONT_COUNT && cont_handlers[type]) {
        cont_handlers[type](val, data, env, next);
    } else {
        show_error("unknown continuation type: %d", type);
        tramp_error();
    }
}

// ============================================================================
// Function Application
// ============================================================================

void apply_function(unsigned fn, unsigned args, unsigned env, unsigned cont)
{
    if (IS_BUILTIN(fn)) {
        int64_t prim_id = CELL_ID(fn);

        // Special handling for call/cc
        if (prim_id == PCALLCC) {
            if (!args || cdr(args)) {
                show_error("call/cc expects exactly one argument");
                cps_signal_current_error(env, cont);
                return;
            }
            GC_GUARD;
            unsigned proc = car(args);
            gc_protect(&proc);
            gc_protect(&cont);
            gc_protect(&env);
            unsigned cont_args = alloc_cons(cont, 0);
            apply_function(proc, cont_args, env, cont);
            return;
        }

        // Special handling for apply
        // (apply proc arg1 arg2 ... arglist) - prepend args to arglist
        if (prim_id == PAPPLY) {
            if (!args || !cdr(args)) {
                show_error("apply expects at least two arguments");
                cps_signal_current_error(env, cont);
                return;
            }
            GC_GUARD;
            unsigned proc = car(args);
            args = cdr(args);

            // Find the last argument (must be a list) and collect preceding
            // args
            unsigned proc_args = 0;
            gc_protect(&proc);
            gc_protect(&proc_args);
            gc_protect(&args);
            gc_protect(&env);
            gc_protect(&cont);

            // Build list of all args except last, then append last
            unsigned prefix = 0;
            gc_protect(&prefix);
            while (cdr(args)) {
                prefix = alloc_cons(car(args), prefix);
                args = cdr(args);
            }
            // args is now the last element, car(args) is the final list
            proc_args = car(args);
            // Every apply target, including a continuation, must receive a
            // proper argument list.  Builtins and closures validate this
            // later, but continuations otherwise turn a cyclic list into an
            // invalid multiple-values object.
            if (!list_length_checked(proc_args, NULL, "apply")) {
                cps_signal_current_error(env, cont);
                return;
            }

            // Prepend the reversed prefix to proc_args
            while (prefix) {
                proc_args = alloc_cons(car(prefix), proc_args);
                prefix = cdr(prefix);
            }

            apply_function(proc, proc_args, env, cont);
            return;
        }

        // Special handling for call-with-values
        if (prim_id == PCALLWITHVALUES) {
            if (!args || !cdr(args) || cddr(args)) {
                show_error("call-with-values expects exactly two arguments");
                cps_signal_current_error(env, cont);
                return;
            }
            GC_GUARD;
            unsigned producer = car(args);
            unsigned consumer = cadr(args);
            // Protect producer, consumer, env and cont across make_cont
            gc_protect(&producer);
            gc_protect(&consumer);
            gc_protect(&env);
            gc_protect(&cont);
            // Create continuation to handle producer's result
            unsigned k = make_cont(CONT_CALLWITHVALUES, consumer, env, cont);
            // Call producer with no arguments
            apply_function(producer, 0, env, k);
            return;
        }

        // Special handling for raise - CPS mirror of vm_handle_raise: pass the
        // object to the current handler with a CONT_ERR_RETURN continuation
        // whose data records the dispatched handler, so handle_cont_err_return
        // can tell the default handler (silent unhandled) from a user handler
        // (R7RS violation). Cannot go through apply_primitive - raising is
        // engine machinery, not a pure primitive.
        if (prim_id == PRAISENOW) {
            if (!args || cdr(args)) {
                show_error("raise expects exactly one argument");
                cps_signal_current_error(env, cont);
                return;
            }
            unsigned obj = car(args);
            unsigned handler = lookup_silent(
                intern("*current-exception-handler*"),
                exception_state_env(env));
            if (handler == TOK_ERROR) {
                cps_signal_current_error(env, cont);
                return;
            }
            GC_GUARD;
            gc_protect(&handler);
            gc_protect(&obj);
            gc_protect(&env);
            gc_protect(&cont);
            cps_capture_backtrace(handler, env, cont);
            unsigned err_cont = make_cont(CONT_ERR_RETURN, handler, env, cont);
            unsigned raise_args = alloc_cons(obj, 0);
            apply_function(handler, raise_args, env, err_cont);
            return;
        }

        // Special handling for eval
        if (prim_id == PEVAL) {
            if (!args || !cdr(args) || cddr(args)) {
                show_error("eval expects exactly two arguments");
                cps_signal_current_error(env, cont);
                return;
            }
            // The expression is evaluated directly in the environment it
            // was given. The caller's exception handlers stay in effect
            // without any help from here: the exception machinery reads them
            // from the interaction environment (exception_state_env), not
            // from the environment of the code that raised. There used to be
            // a frame holding copies of them consed in front of eval_env,
            // and since define binds into the first frame, a definition made
            // through eval landed in that frame and vanished with it.
            tramp_eval(car(args), cadr(args), cont);
            return;
        }

        // Special handling for interaction-environment
        if (prim_id == PINTERACTIONENV) {
            if (args) {
                show_error("interaction-environment expects no arguments");
                cps_signal_current_error(env, cont);
                return;
            }
            // The interaction environment is the top-level environment, not
            // a lexical frame around the call site.  In standalone harnesses
            // ctx.global_environment is unset, so exception_state_env falls
            // back to the current environment.
            tramp_apply(exception_state_env(env), cont);
            return;
        }

        // Special handling for gc-flip
        if (prim_id == PGCFLIP) {
            if (args) {
                show_error("gc-flip expects no arguments");
                cps_signal_current_error(env, cont);
                return;
            }
            // Trigger garbage collection with environment as root
            // Set tramp state so gc() collects the right things
            tramp.cont = cont;
            tramp.env = env;
            gc(env);
            // After gc(), use updated tramp values
            tramp_apply(ctx.atom_true, tramp.cont);
            return;
        }

        // Special handling for load
        if (prim_id == PLOAD) {
            if (!args || cdr(args)) {
                show_error("load expects exactly one argument");
                cps_signal_current_error(env, cont);
                return;
            }
            unsigned filename_arg = car(args);
            if (!IS_STRING(filename_arg)) {
                show_error("load: expected string argument");
                cps_signal_current_error(env, cont);
                return;
            }
            const char *filename = GET_STRING_PTR(filename_arg);
            if (!string_is_registered(filename)) {
                show_error("load: invalid string");
                cps_signal_current_error(env, cont);
                return;
            }
            const char *old_filename = reader_get_filename();
            char *filename_copy = checked_string_copy(filename);
            if (!filename_copy) {
                show_error("load: out of memory");
                cps_signal_current_error(env, cont);
                return;
            }
            FILE *f = fopen(filename, "r");
            if (!f) {
                show_error("load: cannot open file: %s", filename);
                free(filename_copy);
                cps_signal_current_error(env, cont);
                return;
            }
            reader_set_filename(filename_copy);
            reader_reset_position();

            unsigned exprs = 0, exprs_tail = 0;
            bool load_failed = false;
            GC_GUARD;
            gc_protect(&exprs);
            gc_protect(&exprs_tail);
            gc_protect(&env);
            gc_protect(&cont);
            for (;;) {
                unsigned expr = read_obj_port(f);
                if (expr == TOK_ERROR) {
                    load_failed = true;
                    break;
                }
                if (is_eof_object(expr))
                    break;
                if (expr == TOK_CLOSE || expr == TOK_DOT) {
                    show_error("load: unexpected reader token");
                    load_failed = true;
                    break;
                }
                list_append(&exprs, &exprs_tail, expr);
            }
            reader_forget_port(f);
            if (fclose(f) != 0) {
                show_error("load: close failed");
                load_failed = true;
            }
            reader_set_filename(old_filename);
            reader_reset_position();
            free(filename_copy);

            if (load_failed) {
                cps_signal_current_error(env, cont);
                return;
            }

            if (!exprs) {
                tramp_apply(0, cont);
                return;
            }

            if (!cdr(exprs)) {
                tramp_eval(car(exprs), env, cont);
            } else {
                GC_GUARD;
                unsigned first_expr = car(exprs);
                unsigned rest = cdr(exprs);
                gc_protect(&first_expr);
                gc_protect(&rest);
                gc_protect(&env);
                gc_protect(&cont);
                unsigned k = make_cont(CONT_BEGIN, rest, env, cont);
                tramp_eval(first_expr, env, k);
            }
            return;
        }

        // Protect the dynamic environment and continuation across
        // apply_primitive (which can allocate/trigger GC).  The environment
        // is needed by the error dispatcher if the primitive fails after GC.
        {
            GC_GUARD;
            gc_protect(&args);
            gc_protect(&env);
            gc_protect(&cont);
            unsigned result = apply_primitive(prim_id, args);
            if (result == TOK_ERROR) {
                cps_signal_error(ctx.last_error[0] ? ctx.last_error
                                                   : "primitive error",
                                 env, cont);
                return;
            }
            tramp_apply(result, cont);
            return;
        }
    }

    if (IS_FUNCTION(fn)) {
        GC_GUARD;
        unsigned params = car(fn);
        unsigned body_env = cdr(fn);
        unsigned body = car(body_env);
        unsigned def_env = cdr(body_env);

        // Protect all values across allocations (including params and args for
        // bind_params)
        gc_protect(&params);
        gc_protect(&args);
        gc_protect(&body);
        gc_protect(&def_env);
        gc_protect(&env);
        gc_protect(&cont);
        unsigned frame = 0;
        gc_protect(&frame);
        frame = bind_params(params, args);
        if (frame == TOK_ERROR) {
            // bind_params reports a wrong argument count without saying
            // whose; name the procedure, as the VM does.
            unsigned required = 0, argc = 0;
            unsigned p = params;
            for (; IS_PAIR(p) && required < 100000; p = cdr(p))
                required++;
            if (strstr(ctx.last_error, " arguments") &&
                list_length_checked(args, &argc, "procedure") &&
                (p ? argc < required : argc != required))
                format_arity_error(ctx.last_error, sizeof(ctx.last_error), fn,
                                   argc, required, p != 0);
            cps_signal_current_error(env, cont);
            return;
        }
        unsigned new_env = alloc_cons(frame, def_env);
        gc_protect(&new_env);
        // Internal names must already shadow outer macros when an earlier
        // local procedure is created and expands its body. The definitions
        // below will fill these bindings as evaluation reaches them.
        new_env = extend_env_with_internal_defines(new_env, body);
        if (new_env == TOK_ERROR) {
            cps_signal_current_error(env, cont);
            return;
        }

        if (!cdr(body)) {
            tramp_eval(car(body), new_env, cont);
        } else {
            unsigned first_expr = car(body);
            unsigned rest_body = cdr(body);
            gc_protect(&first_expr);
            gc_protect(&rest_body);
            unsigned k = make_cont(CONT_APPLY_FUNC, rest_body, new_env, cont);
            tramp_eval(first_expr, new_env, k);
        }
        return;
    }

    if (IS_CONT(fn)) {
        GC_GUARD;
        gc_protect(&fn);
        gc_protect(&args);
        gc_protect(&cont);
        unsigned value = values_from_list(args);
        tramp_apply(value, fn);
        return;
    }

    if (IS_CELL(fn) && CELL_TYPE(fn) == BT_VMCONT) {
        GC_GUARD;
        gc_protect(&fn);
        gc_protect(&args);
        gc_protect(&env);
        gc_protect(&cont);
        unsigned result = 0;
        vm_cont_call_status status = vm_call_continuation(fn, args, &result);
        if (status == VM_CONT_CALL_ERROR) {
            cps_signal_current_error(env, cont);
            return;
        }
        // A continuation jump discards the CPS continuation at the call site.
        // With an active VM, vm_apply will notice the transfer flag and resume
        // the restored VM state; otherwise the temporary VM has already run
        // that state to completion and its result is the evaluator's result.
        tramp_done(status == VM_CONT_CALL_RETURNED ? result : 0);
        return;
    }

    // Handle bytecode VM closures (for CPS/bytecode interop)
    if (is_bytecode_closure_object(fn)) {
        GC_GUARD;
        gc_protect(&fn);
        gc_protect(&args);
        gc_protect(&env);
        gc_protect(&cont);
        unsigned result = vm_call_closure(fn, args);
        gc_protect(&result);
        if (result == TOK_ERROR) {
            cps_signal_current_error(env, cont);
            return;
        }
        tramp_apply(result, cont);
        return;
    }

    char message[128];
    snprintf(message, sizeof(message), "not a procedure, got %s",
             type_name(fn));
    cps_signal_error(message, env, cont);
}

// ============================================================================
// Main Evaluator Entry Points
// ============================================================================

unsigned eval_cps(unsigned expr, unsigned env)
{
    // Protect expr and env across make_halt_cont() which can trigger GC
    GC_GUARD;
    gc_protect(&expr);
    gc_protect(&env);
    eval_reset_macro_expansion_depth();
    // A nested evaluation (a primitive calling back into the evaluator while
    // a handler is being applied) is not that handler failing: let its own
    // errors dispatch, and restore the outer window on the way out.
    bool outer_dispatch_active = cps_handler_dispatch_active;
    cps_handler_dispatch_active = false;
    unsigned halt_k = make_halt_cont();
    tramp_eval(expr, env, halt_k);

    while (tramp.mode != TRAMP_DONE && tramp.mode != TRAMP_ERROR) {
        if (tramp.mode == TRAMP_EVAL) {
            eval_step();
        } else if (tramp.mode == TRAMP_APPLY) {
            apply_cont_step();
        }
    }
    cps_handler_dispatch_active = outer_dispatch_active;

    if (tramp.mode == TRAMP_ERROR) {
        return TOK_ERROR;
    }
    return tramp.value;
}

unsigned eval_obj(unsigned id, unsigned env)
{
    return eval_cps(id, env);
}
