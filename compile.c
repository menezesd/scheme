/**
 * @file compile.c
 * @brief Scheme to bytecode compiler
 *
 * Compiles Scheme S-expressions into bytecode for the VM.
 *
 * ## Compilation Strategy
 * - Each expression compiles to code that leaves one value on the stack
 * - Tail position tracking enables tail call optimization
 * - Closures compile to nested code objects
 * - Special forms have dedicated compilation routines
 */

#include "bytecode.h"
#include "compile_internal.h"
#include "context.h"
#include "env.h"
#include "eval.h"
#include "eval_internal.h"
#include "macros.h"
#include "prim_internal.h"
#include "primitives.h"
#include "reader.h"
#include "writer.h"
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declaration from eval.c
unsigned qq_expand_cps(unsigned x, unsigned env);

#define MAX_COMPILE_MACRO_EXPANSION_DEPTH 1000

// Forward declaration for helper
static void emit_gensym_definitions(compile_ctx *cctx, unsigned bindings);
static void emit(compile_ctx *cctx, unsigned op);
static void emit2(compile_ctx *cctx, unsigned op, unsigned arg);
static void emit3(compile_ctx *cctx, unsigned op, unsigned arg1,
                  unsigned arg2);

// True if a compile-time environment frame (vars . vals) binds var_id.
static bool compile_frame_binds(unsigned frame, int64_t var_id)
{
    if (!IS_PAIR(frame))
        return false;
    unsigned vars = car(frame);
    for (unsigned guard = 0; vars && guard < 1000000; guard++) {
        if (IS_ATOM(vars))
            return CELL_ID(vars) == var_id; // rest parameter
        if (!IS_PAIR(vars))
            return false;
        unsigned var = car(vars);
        if (IS_ATOM(var) && CELL_ID(var) == var_id)
            return true;
        vars = cdr(vars);
    }
    return false;
}

// A let binding held in a stack slot (see compile_call's lambda inliner).
// frame_pos identifies the compile-env frame that binds it by its position,
// the frame count of the environment with that frame on top: while the scope
// is open the chain under it is fixed, so the position names exactly one
// frame, and nothing needs rooting.
struct let_slot_binding {
    unsigned frame_pos;
    int64_t var_id;
    unsigned slot;
    unsigned scope; // the scope's ordinal, for banning it after a problem
    bool assigned;  // a set! reached it (the source scan predicted none)
};

// Resolve var_id to the stack slot holding it in this body, or -1 if it
// lives in an environment. *binding_out (if given) receives the index of the
// let-slot binding, or -1 for a parameter.
static int resolve_stack_local(compile_ctx *cctx, int64_t var_id,
                               int *binding_out)
{
    if (binding_out)
        *binding_out = -1;
    if (cctx->num_locals < 0)
        return -1; // not a stack-local body

    // Every scope opened inside the body (letrec, internal defines, an
    // inlined lambda, ...) pushes a compile-env frame above the parameter
    // frame. The innermost one binding the name decides: a let scope's frame
    // may hold it in a slot, any other holds it in that runtime frame.
    unsigned len = 0;
    for (unsigned e = cctx->env; IS_PAIR(e); e = cdr(e))
        len++;
    unsigned e = cctx->env;
    for (unsigned pos = len; pos > cctx->locals_env_len && IS_PAIR(e);
         pos--, e = cdr(e)) {
        if (!compile_frame_binds(car(e), var_id))
            continue;
        for (unsigned i = cctx->let_binding_count; i-- > 0;) {
            struct let_slot_binding *b = &cctx->let_bindings[i];
            if (b->frame_pos < pos)
                break; // bindings are kept in nesting order
            if (b->frame_pos == pos && b->var_id == var_id) {
                if (binding_out)
                    *binding_out = (int)i;
                return (int)b->slot;
            }
        }
        return -1;
    }

    for (int i = 0; i < cctx->num_locals; i++)
        if (cctx->local_ids[i] == var_id)
            return i;
    return -1;
}

static int find_stack_local_slot(compile_ctx *cctx, int64_t var_id)
{
    return resolve_stack_local(cctx, var_id, NULL);
}

// Most let slots one body may use. A binding past it goes to an
// environment frame, as all of them did before let slots.
#define LET_SLOT_LIMIT 64

static bool let_scope_banned(const compile_ctx *cctx, unsigned scope)
{
    for (unsigned i = 0; i < cctx->let_scope_ban_count; i++)
        if (cctx->let_scope_bans[i] == scope)
            return true;
    return false;
}

// What was emitted for a let scope shows its bindings cannot all stay in
// slots after all. Have compile_lambda compile the body again, with this
// scope in an environment frame as before let slots.
static void let_scope_ban(compile_ctx *cctx, unsigned scope)
{
    cctx->let_slot_retry = true;
    if (let_scope_banned(cctx, scope))
        return;
    if (cctx->let_scope_ban_count == cctx->let_scope_ban_cap) {
        unsigned cap = cctx->let_scope_ban_cap ? cctx->let_scope_ban_cap * 2
                                               : 8;
        unsigned *bans =
            checked_realloc_array(cctx->let_scope_bans, cap, sizeof *bans);
        if (!bans) {
            // Cannot record it: no let slots at all on the next attempt.
            cctx->let_slots = false;
            return;
        }
        cctx->let_scope_bans = bans;
        cctx->let_scope_ban_cap = cap;
    }
    cctx->let_scope_bans[cctx->let_scope_ban_count++] = scope;
}

static bool let_binding_push(compile_ctx *cctx, unsigned frame_pos,
                             int64_t var_id, unsigned slot, unsigned scope)
{
    if (cctx->let_binding_count == cctx->let_binding_cap) {
        unsigned cap = cctx->let_binding_cap ? cctx->let_binding_cap * 2 : 8;
        struct let_slot_binding *bindings = checked_realloc_array(
            cctx->let_bindings, cap, sizeof *bindings);
        if (!bindings)
            return false; // the binding stays in an environment frame
        cctx->let_bindings = bindings;
        cctx->let_binding_cap = cap;
    }
    cctx->let_bindings[cctx->let_binding_count++] =
        (struct let_slot_binding){frame_pos, var_id, slot, scope, false};
    return true;
}

static void emit_local_get_slot(compile_ctx *cctx, int slot)
{
    if (slot < 4)
        emit(cctx, OP_LOCAL_GET0 + slot);
    else
        emit2(cctx, OP_LOCAL_GET, (unsigned)slot);
}

static bool builtin_keyword_unbound(int64_t kw, unsigned env)
{
    return lookup_silent(kw, env) == TOK_ERROR;
}

// True if expr is a kw form: its head spells kw, or is kw's core alias, and
// that identifier has no binding in env that would shadow the core form.
static bool form_is_core(unsigned expr, int64_t kw, unsigned env)
{
    return IS_PAIR(expr) && IS_KEYWORD(car(expr), kw) &&
           builtin_keyword_unbound(CELL_ID(car(expr)), env);
}

// True if target_cell is reachable through the persistent top-level global
// environment (the outermost frame of cctx->env). Only bindings satisfying
// this are safe to embed as a compile-time cell reference (OP_DEFINE_ALIAS_REF):
// the global frame is a single object that lives for the whole program, so a
// cell found there now is the same cell OP_DEFINE will mutate later. Any
// other frame - an internal-define placeholder from scan_internal_defines,
// or a let/lambda body's frame - is a compile-time-only bookkeeping object;
// OP_PUSHENV allocates an unrelated fresh frame at runtime (once per
// invocation), so a cell resolved against the compile-time frame is never
// the one that execution actually updates.
static bool binding_cell_in_global_frame(unsigned env, int64_t var_id,
                                         unsigned target_cell)
{
    if (!IS_PAIR(env) || !target_cell)
        return false;
    while (IS_PAIR(env) && cdr(env))
        env = cdr(env);
    return env_find_binding_cell(var_id, env) == target_cell;
}

// The interaction and standard-library environments are program-lifetime
// objects too. A macro defined there (case-lambda, say) and used inside an
// (environment ...) that lacks its helpers must still reach the helpers it
// was defined against, not look them up by name in the use-site environment.
static bool binding_cell_is_stable_global(compile_ctx *cctx, int64_t var_id,
                                          unsigned target_cell)
{
    return binding_cell_in_global_frame(cctx->env, var_id, target_cell) ||
           binding_cell_in_global_frame(ctx.global_environment, var_id,
                                        target_cell) ||
           binding_cell_in_global_frame(ctx.r7rs_environment, var_id,
                                        target_cell);
}

// ============================================================================
// Compile Result - tracks constant propagation
// ============================================================================

typedef struct {
    bool is_const;  // true if result is a compile-time constant
    unsigned value; // the constant value (only valid if is_const)
} compile_result;

// Convenience constructors
static inline compile_result const_result(unsigned val)
{
    return (compile_result){true, val};
}

static inline compile_result dynamic_result(void)
{
    return (compile_result){false, 0};
}

static compile_result emit_syntax_error(compile_ctx *cctx, const char *message)
{
    unsigned msg = make_string_copy(message);
    gc_protect(&msg);
    emit2(cctx, OP_CONST, code_add_const(cctx->code, msg));
    emit3(cctx, OP_PRIM, PERROR, 1);
    gc_unprotect(1);
    return dynamic_result();
}

// Number of frames currently on the compile-time environment. Recorded
// with each known-lambda entry so a use inside any scope that pushed
// frames since registration (nested lambda, let, begin with defines)
// can be rejected - those scopes may shadow the bound name.
static unsigned compile_env_length(unsigned env)
{
    unsigned n = 0;
    FORLIST(frame, env) n++;
    return n;
}

// Look up a variable in the known_lambdas alist.
// Returns the lambda expression if found (with the compile-env length at
// registration in *depth_out), 0 otherwise.
static unsigned lookup_known_lambda(int64_t var_id, unsigned known_lambdas,
                                    unsigned *depth_out)
{
    FORLIST(entry, known_lambdas) {
        unsigned pair = car(entry);
        if (IS_PAIR(pair) && IS_ATOM(car(pair)) &&
            CELL_ID(car(pair)) == var_id) {
            unsigned vd = cdr(pair);
            if (IS_PAIR(vd) && IS_NUM(cdr(vd))) {
                *depth_out = (unsigned)CELL_ID(cdr(vd));
                return car(vd); // The lambda expression
            }
        }
    }
    return 0;
}

// Stamp var -> lambda into known_lambdas for escape-analysis inlining,
// recording the current compile-env frame count so uses in scopes pushed
// since registration are rejected (they may shadow the name).
static void register_known_lambda(compile_ctx *cctx, unsigned var,
                                  unsigned val_expr)
{
    GC_GUARD;
    gc_protect(&var);
    gc_protect(&val_expr);
    unsigned depth_cell = store(compile_env_length(cctx->env));
    gc_protect(&depth_cell);
    unsigned vd = alloc_cons(val_expr, depth_cell);
    gc_protect(&vd);
    unsigned pair = alloc_cons(var, vd);
    gc_protect(&pair);
    cctx->known_lambdas = alloc_cons(pair, cctx->known_lambdas);
    gc_unprotect(3);
}

// Forward declaration
static bool contains_reference(unsigned expr, int64_t var_id);
static unsigned is_internal_define(unsigned expr, unsigned env);
static bool is_keyword_shadowed(int64_t kw, unsigned env);

// True if this body introduces internal defines - the same set
// scan_internal_defines collects, without building the list.
//
// An internal define needs an environment frame to land in. compile_begin
// extends the compile-time environment for one but emits no PUSHENV, so at
// runtime the binding goes wherever the environment currently points - and a
// stack-local function has no frame of its own, leaving the closure's
// environment, which for a top-level procedure is the global one. Two bugs
// followed: the name leaked out of the body, and the defined procedure could
// not see the enclosing parameters, since those live in stack slots rather
// than in any environment. (letrec is fine either way: it pushes its own
// frame before defining into it.)
static bool body_defines_internally(unsigned body, unsigned env)
{
    for (unsigned e = body; IS_PAIR(e); e = cdr(e)) {
        unsigned expr = car(e);
        if (is_internal_define(expr, env))
            return true;
        if (IS_PAIR(expr) && IS_KEYWORD(car(expr), ctx.kw_begin) &&
            !is_keyword_shadowed(CELL_ID(car(expr)), env)) {
            if (body_defines_internally(cdr(expr), env))
                return true;
        }
    }
    return false;
}

// ============================================================================
// Capture and assignment prediction for stack locals
// ============================================================================
//
// A binding can live in a VM stack slot only if no closure refers to it (a
// closure reaches variables through its environment, and a slot is not in
// one) and nothing assigns it (a continuation copies the stack, so a value
// written after a capture would be lost when the capture is re-entered,
// where an environment binding keeps it). Both are decided before the body
// is compiled, from the unexpanded source, so the walks below are
// predictions. What makes them safe is the check of what was actually
// emitted: locals_body_needs_environment and compile_set for parameters, and
// the per-scope checks in compile_call for let slots. A prediction that
// misses a capture or an assignment costs a recompile, never a wrong
// answer; one that sees a capture where there is none costs the slot.
//
// So the walks aim to be exact for the forms loops are made of - lambda,
// define, the let family including named let, do, and immediately applied
// lambdas, which compile_call inlines instead of closing over - whether
// those are built in or the standard library's macros, and for any other
// syntax-rules macro consult its templates instead of assuming the worst.
// None of them allocates.

static bool body_captures(unsigned body, int64_t var_id, unsigned env);
static bool formals_bind(unsigned formals, int64_t sym);

// The standard library's do: not a core form, so it has no cached keyword.
static int64_t do_keyword_id(void)
{
    static int64_t id = 0;
    if (!id)
        id = intern("do"); // the atom table only; no cell is allocated
    return id;
}

// ((lambda formals body ...) arg ...) in a shape compile_call inlines: the
// lambda never becomes a closure. Keep the arity test in step with the
// inliner's can_inline; a disagreement is caught by the post-compile checks.
static bool inlined_lambda_application(unsigned expr, unsigned env,
                                       unsigned *formals_out,
                                       unsigned *body_out)
{
    if (!IS_PAIR(expr) || !form_is_core(car(expr), ctx.kw_lambda, env))
        return false;
    unsigned fn = car(expr);
    if (!IS_PAIR(cdr(fn)) || !IS_PAIR(cddr(fn)))
        return false;
    unsigned formals = cadr(fn);
    unsigned param_count = 0;
    bool has_rest = false;
    for (unsigned p = formals; p; p = cdr(p)) {
        if (!IS_PAIR(p)) {
            if (!IS_ATOM(p))
                return false;
            has_rest = true;
            break;
        }
        if (!IS_ATOM(car(p)))
            return false;
        param_count++;
    }
    unsigned arg_count = 0;
    unsigned a = cdr(expr);
    for (; IS_PAIR(a); a = cdr(a))
        arg_count++;
    if (a)
        return false; // improper argument list
    if (has_rest ? arg_count < param_count : arg_count != param_count)
        return false;
    *formals_out = formals;
    *body_out = cddr(fn);
    return true;
}

// Whether a syntax-rules template could expand to a closure (it uses lambda
// or define, a named let, do, or another syntax-rules macro whose templates
// could) or to an assignment (set!, directly or through another macro). A
// macro already being scanned - a recursive one - adds nothing new and is
// skipped; nesting deeper than the bound is assumed to. let, let* and letrec
// are judged by their shape rather than by their own templates, which use
// lambda for what compile_call inlines.
#define TEMPLATE_SCAN_DEPTH 4

enum template_scan_mode { SCAN_CLOSURE, SCAN_ASSIGN };

static bool syntax_template_scan(unsigned transformer,
                                 enum template_scan_mode mode,
                                 const unsigned *visiting, int depth);

static bool template_scan(unsigned tmpl, unsigned env,
                          enum template_scan_mode mode,
                          const unsigned *visiting, int depth)
{
    for (;;) {
        if (IS_ATOM(tmpl)) {
            int64_t id = CELL_ID(tmpl);
            int64_t kw = kw_canon(id);
            if (mode == SCAN_CLOSURE &&
                (kw == ctx.kw_lambda || kw == ctx.kw_define ||
                 id == do_keyword_id()))
                return true;
            if (mode == SCAN_ASSIGN && kw == ctx.kw_set)
                return true;
            if (kw == ctx.kw_let || kw == ctx.kw_letstar ||
                kw == ctx.kw_letrec || kw == ctx.kw_quote)
                return false;
            unsigned binding = lookup_silent(id, env);
            return binding != TOK_ERROR && IS_SYNTAX(binding) &&
                   syntax_template_scan(binding, mode, visiting, depth);
        }
        if (!IS_PAIR(tmpl))
            return false;
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t kw = kw_canon(CELL_ID(head));
            if (kw == ctx.kw_quote)
                return false; // a datum, not code
            if (mode == SCAN_CLOSURE && kw == ctx.kw_let &&
                IS_PAIR(cdr(tmpl)) && IS_ATOM(cadr(tmpl)))
                return true; // named let
        }
        if (template_scan(head, env, mode, visiting, depth))
            return true;
        tmpl = cdr(tmpl);
    }
}

static bool syntax_template_scan(unsigned transformer,
                                 enum template_scan_mode mode,
                                 const unsigned *visiting, int depth)
{
    for (int i = 0; i < depth; i++)
        if (visiting[i] == transformer)
            return false;
    if (depth >= TEMPLATE_SCAN_DEPTH)
        return true;
    unsigned syn_data = car(transformer);
    if (!IS_PAIR(syn_data) || !IS_PAIR(cdr(syn_data)))
        return true;
    unsigned stack[TEMPLATE_SCAN_DEPTH];
    for (int i = 0; i < depth; i++)
        stack[i] = visiting[i];
    stack[depth] = transformer;
    unsigned env = cdr(transformer);
    for (unsigned r = cddr(syn_data); IS_PAIR(r); r = cdr(r)) {
        unsigned rule = car(r);
        if (!IS_PAIR(rule) ||
            template_scan(cdr(rule), env, mode, stack, depth + 1))
            return true;
    }
    return false;
}

static bool binding_names_include(unsigned bindings, int64_t var_id)
{
    for (unsigned b = bindings; IS_PAIR(b); b = cdr(b)) {
        unsigned binding = car(b);
        if (IS_PAIR(binding) && IS_ATOM(car(binding)) &&
            CELL_ID(car(binding)) == var_id)
            return true;
    }
    return false;
}

static bool binding_inits_capture(unsigned bindings, int64_t var_id,
                                  unsigned env);

// True if evaluating the form expr might create a closure that refers to
// var_id. A reference by itself is not a capture.
static bool form_captures(unsigned expr, int64_t var_id, unsigned env)
{
    if (!IS_PAIR(expr))
        return false;
    unsigned head = car(expr);
    if (!IS_ATOM(head)) {
        unsigned formals = 0, fn_body = 0;
        if (inlined_lambda_application(expr, env, &formals, &fn_body))
            return body_captures(cdr(expr), var_id, env) ||
                   (!formals_bind(formals, var_id) &&
                    body_captures(fn_body, var_id, env));
        return form_captures(head, var_id, env) ||
               body_captures(cdr(expr), var_id, env);
    }

    // Heads are compared as keywords with core aliases read as the
    // keyword they stand for, but looked up as written: a core alias is
    // the core form even where its name is rebound.
    int64_t head_id = CELL_ID(head);
    int64_t kw = kw_canon(head_id);
    unsigned binding = lookup_silent(head_id, env);
    bool unbound = binding == TOK_ERROR;
    bool syntax = !unbound && IS_SYNTAX(binding);
    if (kw == ctx.kw_quote && unbound)
        return false;
    // A nested lambda captures this variable if its body refers to the
    // outer binding. Check the full lambda form so shadowing parameters
    // are handled by contains_reference.
    if (kw == ctx.kw_lambda && unbound)
        return contains_reference(expr, var_id);
    // (define (name . formals) body ...) builds a closure without a
    // literal lambda; treat it as (lambda formals body ...).
    if (kw == ctx.kw_define && unbound && IS_PAIR(cdr(expr)) &&
        IS_PAIR(cadr(expr)))
        return !formals_bind(cdr(cadr(expr)), var_id) &&
               contains_reference(cddr(expr), var_id);

    // The let family, built in or the standard library's macros. A plain
    // let or let* is an inlined lambda application, letrec builds no
    // closure of its own, and a named let closes over its body only.
    if ((unbound || syntax) && IS_PAIR(cdr(expr)) &&
        (kw == ctx.kw_let || kw == ctx.kw_letstar || kw == ctx.kw_letrec)) {
        unsigned second = cadr(expr);
        if (kw == ctx.kw_let && IS_ATOM(second)) {
            if (!IS_PAIR(cddr(expr)))
                return false;
            unsigned bindings = caddr(expr);
            if (binding_inits_capture(bindings, var_id, env))
                return true;
            if (CELL_ID(second) == var_id ||
                binding_names_include(bindings, var_id))
                return false; // the loop rebinds it
            return contains_reference(cdddr(expr), var_id);
        }
        return binding_inits_capture(second, var_id, env) ||
               body_captures(cddr(expr), var_id, env);
    }
    // (do ((var init step ...) ...) (test expr ...) command ...): the inits
    // run outside the loop's closure, everything else inside it.
    if ((unbound || syntax) && head_id == do_keyword_id() &&
        IS_PAIR(cdr(expr))) {
        unsigned bindings = cadr(expr);
        if (binding_inits_capture(bindings, var_id, env))
            return true;
        if (binding_names_include(bindings, var_id))
            return false;
        for (unsigned b = bindings; IS_PAIR(b); b = cdr(b))
            if (IS_PAIR(car(b)) && IS_PAIR(cdr(car(b))) &&
                contains_reference(cddr(car(b)), var_id))
                return true;
        return contains_reference(cddr(expr), var_id);
    }
    // Any other syntax-rules macro: a use that does not mention the variable
    // cannot capture it, and one whose templates make no closure is walked
    // like a call.
    if (syntax) {
        if (!contains_reference(cdr(expr), var_id))
            return false;
        if (syntax_template_scan(binding, SCAN_CLOSURE, NULL, 0))
            return true;
    }
    return body_captures(cdr(expr), var_id, env);
}

// The initializers of a let-style binding list ((var init ...) ...).
static bool binding_inits_capture(unsigned bindings, int64_t var_id,
                                  unsigned env)
{
    for (unsigned b = bindings; IS_PAIR(b); b = cdr(b)) {
        unsigned binding = car(b);
        if (IS_PAIR(binding) && IS_PAIR(cdr(binding)) &&
            form_captures(cadr(binding), var_id, env))
            return true;
    }
    return false;
}

// The same over a list of forms - a body, or an application's operands. The
// spine is walked with a loop rather than by recursing on the cdr, which
// cost one C frame per element: a lambda with a long flat body, or a macro
// that expands to one, overflowed the C stack. Only the forms recurse, so
// depth follows nesting rather than length.
static bool body_captures(unsigned body, int64_t var_id, unsigned env)
{
    for (unsigned e = body; IS_PAIR(e); e = cdr(e))
        if (form_captures(car(e), var_id, env))
            return true;
    return false;
}

// True if var_id may be captured by a closure made in body (a list of
// forms); see above for why this is a prediction.
static bool captured_by_inner_lambda(unsigned body, int64_t var_id,
                                     unsigned env)
{
    return body_captures(body, var_id, env);
}

// True if body (a list of forms) might assign var_id: a (set! var_id ...)
// anywhere in it, shadowed or not, or a use of a syntax-rules macro that
// mentions the variable and whose templates contain set!.
static bool assigned_in_body(unsigned body, int64_t var_id, unsigned env)
{
    for (unsigned e = body; IS_PAIR(e); e = cdr(e)) {
        unsigned expr = car(e);
        if (!IS_PAIR(expr))
            continue;
        unsigned head = car(expr);
        if (IS_ATOM(head)) {
            int64_t kw = kw_canon(CELL_ID(head));
            unsigned binding = lookup_silent(CELL_ID(head), env);
            if (kw == ctx.kw_quote && binding == TOK_ERROR)
                continue;
            if (kw == ctx.kw_set && binding == TOK_ERROR &&
                IS_PAIR(cdr(expr)) && IS_ATOM(cadr(expr)) &&
                CELL_ID(cadr(expr)) == var_id)
                return true;
            if (binding != TOK_ERROR && IS_SYNTAX(binding) &&
                contains_reference(cdr(expr), var_id) &&
                syntax_template_scan(binding, SCAN_ASSIGN, NULL, 0))
                return true;
        }
        if (assigned_in_body(expr, var_id, env))
            return true;
    }
    return false;
}

// Check if expr contains a reference to var_id (for detecting self-reference)
static bool contains_reference(unsigned expr, int64_t var_id)
{
    // Spine iteratively, cars recursively, for the same reason as above.
    for (;;) {
        if (!expr)
            return false;
        if (IS_ATOM(expr))
            return CELL_ID(expr) == var_id;
        if (!IS_PAIR(expr))
            return false;

        // Check lambda - don't descend if var_id is shadowed by a parameter
        if (IS_KEYWORD(car(expr), ctx.kw_lambda)) {
            unsigned params = cadr(expr);
            // Check if var_id is in params
            if (IS_ATOM(params) && CELL_ID(params) == var_id)
                return false; // Shadowed by rest param
            if (IS_PAIR(params)) {
                for (unsigned p = params; p; p = IS_PAIR(p) ? cdr(p) : 0) {
                    if (IS_ATOM(p) && CELL_ID(p) == var_id)
                        return false; // Shadowed by rest param
                    if (IS_PAIR(p) && IS_ATOM(car(p)) &&
                        CELL_ID(car(p)) == var_id)
                        return false; // Shadowed by regular param
                    if (!IS_PAIR(p))
                        break;
                }
            }
            // Check body
            expr = cddr(expr);
            continue;
        }

        if (contains_reference(car(expr), var_id))
            return true;
        expr = cdr(expr);
    }
}

// ============================================================================
// Compiler Forward Declarations
// ============================================================================

static compile_result compile_expr_internal(unsigned expr, compile_ctx *cctx);
static compile_result compile_if(unsigned expr, compile_ctx *cctx);
static compile_result compile_lambda(unsigned expr, compile_ctx *cctx);
static compile_result compile_begin(unsigned exprs, compile_ctx *cctx);
static compile_result compile_body(unsigned body, compile_ctx *cctx);
static compile_result compile_let(unsigned expr, compile_ctx *cctx);
static compile_result compile_letstar(unsigned expr, compile_ctx *cctx);
static compile_result compile_letrec(unsigned expr, compile_ctx *cctx);
static compile_result compile_and(unsigned expr, compile_ctx *cctx);
static compile_result compile_or(unsigned expr, compile_ctx *cctx);
static compile_result compile_cond(unsigned expr, compile_ctx *cctx);
static compile_result compile_cond_expand(unsigned expr, compile_ctx *cctx);
static compile_result compile_define(unsigned expr, compile_ctx *cctx);
static compile_result compile_set(unsigned expr, compile_ctx *cctx);
static compile_result compile_call(unsigned expr, compile_ctx *cctx);
static compile_result compile_quasiquote(unsigned expr, compile_ctx *cctx);

// ============================================================================
// Constant Folding Helpers
// ============================================================================

// Pure, and never returns a fresh mutable object (cons, list, append and the
// string constructors are excluded: a folded constant would be shared by
// every evaluation of the call). See PRIM_FOLDABLE in primitive_table.c.
static bool is_foldable_primitive(int64_t prim_id)
{
    return primitive_has_flag(prim_id, PRIM_FOLDABLE);
}

// ============================================================================
// Compiler Context
// ============================================================================

static bool compile_base_is_immutable(unsigned env)
{
    if (!env_chain_acyclic(env))
        return false;
    while (IS_PAIR(env)) {
        if (!environment_is_immutable(env))
            return false;
        env = cdr(env);
    }
    return env == 0;
}

static compile_ctx *cctx_new(compile_ctx *parent, unsigned env)
{
    compile_ctx *cctx = checked_calloc_array(1, sizeof(compile_ctx));
    if (!cctx) {
        show_error("compile: out of memory allocating context");
        return NULL;
    }
    cctx->code = code_new();
    if (!cctx->code) {
        free(cctx);
        show_error("compile: out of memory allocating code object");
        return NULL;
    }
    cctx->parent = parent;
    cctx->env = env;
    cctx->immutable_base = parent ? parent->immutable_base
                                  : compile_base_is_immutable(env);
    cctx->tail_position = false;
    cctx->loop_var_id = -1;
    cctx->num_locals = -1;
    // Inherit known_lambdas from parent (they're still in scope)
    cctx->known_lambdas = parent ? parent->known_lambdas : 0;
    cctx->macro_expansion_depth = parent ? parent->macro_expansion_depth : 0;
    // Protect env, known_lambdas and loop_params so they're updated if GC
    // runs. loop_params matters as much as the other two: compile_lambda
    // copies it into the nested context for the loop optimization, and
    // compile_call later walks it to decide which parameters a self tail call
    // should SET. Compiling a lambda body allocates freely, so a stale
    // loop_params yields SETs against the wrong symbols and the "loop" never
    // advances - an infinite loop rather than a wrong answer.
    gc_protect(&cctx->env);
    gc_protect(&cctx->known_lambdas);
    gc_protect(&cctx->loop_params);
    return cctx;
}

static void cctx_free(compile_ctx *cctx)
{
    // Unprotect env, known_lambdas and loop_params from cctx_new
    gc_unprotect(3);
    // Note: code is transferred out, not freed here
    free(cctx->let_bindings);
    free(cctx->let_scope_bans);
    free(cctx);
}

// ============================================================================
// Helper: Emit with operand
// ============================================================================

static void emit(compile_ctx *cctx, unsigned op)
{
    code_emit(cctx->code, op);
}

static void emit2(compile_ctx *cctx, unsigned op, unsigned arg)
{
    code_emit(cctx->code, op);
    code_emit(cctx->code, arg);
}

static void emit3(compile_ctx *cctx, unsigned op, unsigned arg1, unsigned arg2)
{
    code_emit(cctx->code, op);
    code_emit(cctx->code, arg1);
    code_emit(cctx->code, arg2);
}

static void emit4(compile_ctx *cctx, unsigned op, unsigned arg1, unsigned arg2,
                  unsigned arg3)
{
    code_emit(cctx->code, op);
    code_emit(cctx->code, arg1);
    code_emit(cctx->code, arg2);
    code_emit(cctx->code, arg3);
}

#define IC_UNCACHED 0xFFFFFFFF

// Track env frame depth for loop optimization safety
static void emit_pushenv(compile_ctx *cctx)
{
    code_emit(cctx->code, OP_PUSHENV);
    cctx->env_depth++;
}

static void emit_popenv(compile_ctx *cctx)
{
    code_emit(cctx->code, OP_POPENV);
    if (cctx->env_depth > 0)
        cctx->env_depth--;
}

// Emit a jump instruction, return position to patch
static unsigned emit_jump(compile_ctx *cctx, unsigned op)
{
    code_emit(cctx->code, op);
    unsigned pos = code_current_pos(cctx->code);
    code_emit(cctx->code, 0); // Placeholder
    return pos;
}

static void patch_jump(compile_ctx *cctx, unsigned pos)
{
    code_patch(cctx->code, pos, code_current_pos(cctx->code));
}

// Forward declarations for template helpers
static unsigned collect_template_free_vars(unsigned tmpl, unsigned pattern_vars,
                                           unsigned collected, int64_t ellipsis,
                                           unsigned env);
static unsigned rename_template_vars(unsigned tmpl, unsigned rename_map);

static unsigned add_template_bound_var(unsigned pattern_vars, unsigned var)
{
    if (!IS_ATOM(var))
        return pattern_vars;

    GC_GUARD;
    gc_protect(&pattern_vars);
    gc_protect(&var);
    return alloc_cons(var, pattern_vars);
}

static bool template_id_bound(unsigned pattern_vars, int64_t id)
{
    for (unsigned p = pattern_vars; p; p = cdr(p)) {
        if (IS_ATOM(car(p)) && CELL_ID(car(p)) == id)
            return true;
    }
    return false;
}

static bool template_keyword_bound_as_value(int64_t id, unsigned env)
{
    if (!is_special_form(id))
        return false;
    unsigned value = lookup_silent(id, env);
    return value != TOK_ERROR && !IS_SYNTAX(value) && !IS_MACRO(value);
}

static unsigned add_template_lambda_bounds(unsigned pattern_vars,
                                           unsigned params)
{
    GC_GUARD;
    gc_protect(&pattern_vars);
    gc_protect(&params);

    unsigned p = params;
    gc_protect(&p);
    for (; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        unsigned param = IS_PAIR(p) ? car(p) : p;
        pattern_vars = add_template_bound_var(pattern_vars, param);
        if (!IS_PAIR(p))
            break;
    }
    gc_unprotect(1);

    return pattern_vars;
}

static unsigned add_template_let_bounds(unsigned pattern_vars,
                                        unsigned binding_list)
{
    GC_GUARD;
    gc_protect(&pattern_vars);
    gc_protect(&binding_list);

    unsigned bl = binding_list;
    gc_protect(&bl);
    for (; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (let_binding_has_value(binding))
            pattern_vars = add_template_bound_var(pattern_vars, car(binding));
    }
    gc_unprotect(1);

    return pattern_vars;
}

static unsigned add_template_syntax_bounds(unsigned pattern_vars,
                                           unsigned bindings)
{
    GC_GUARD;
    gc_protect(&pattern_vars);
    gc_protect(&bindings);

    unsigned b = bindings;
    gc_protect(&b);
    for (; IS_PAIR(b); b = cdr(b)) {
        unsigned binding = car(b);
        if (IS_PAIR(binding))
            pattern_vars = add_template_bound_var(pattern_vars, car(binding));
    }
    gc_unprotect(1);

    return pattern_vars;
}

static unsigned collect_template_free_vars_let(unsigned tmpl,
                                               unsigned pattern_vars,
                                               unsigned collected,
                                               int64_t ellipsis,
                                               bool is_letstar,
                                               bool is_letrec,
                                               unsigned env)
{
    unsigned binding_list = cadr(tmpl);
    unsigned body = cddr(tmpl);

    GC_GUARD;
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&pattern_vars);
    gc_protect(&collected);

    if (!proper_list_silent(binding_list))
        return collect_template_free_vars(body, pattern_vars, collected,
                                          ellipsis, env);

    unsigned body_vars = pattern_vars;
    gc_protect(&body_vars);
    if (is_letrec)
        body_vars = add_template_let_bounds(body_vars, binding_list);

    unsigned bl = binding_list;
    gc_protect(&bl);
    for (; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding))
            continue;

        unsigned val_vars = (is_letstar || is_letrec) ? body_vars : pattern_vars;
        collected =
            collect_template_free_vars(cadr(binding), val_vars, collected,
                                       ellipsis, env);

        if (is_letstar && !is_letrec)
            body_vars = add_template_bound_var(body_vars, car(binding));
    }
    gc_unprotect(1);

    if (!is_letstar && !is_letrec)
        body_vars = add_template_let_bounds(body_vars, binding_list);

    return collect_template_free_vars(body, body_vars, collected, ellipsis,
                                      env);
}

static unsigned collect_template_free_vars_named_let(unsigned tmpl,
                                                     unsigned pattern_vars,
                                                     unsigned collected,
                                                     int64_t ellipsis,
                                                     unsigned env)
{
    unsigned name = cadr(tmpl);
    unsigned binding_list = caddr(tmpl);
    unsigned body = cdddr(tmpl);

    GC_GUARD;
    gc_protect(&name);
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&pattern_vars);
    gc_protect(&collected);

    if (!proper_list_silent(binding_list))
        return collect_template_free_vars(body, pattern_vars, collected,
                                          ellipsis, env);

    unsigned bl = binding_list;
    gc_protect(&bl);
    for (; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding))
            continue;
        collected = collect_template_free_vars(cadr(binding), pattern_vars,
                                               collected, ellipsis, env);
    }
    gc_unprotect(1);

    unsigned body_vars = add_template_bound_var(pattern_vars, name);
    gc_protect(&body_vars);
    body_vars = add_template_let_bounds(body_vars, binding_list);
    return collect_template_free_vars(body, body_vars, collected, ellipsis,
                                      env);
}

static unsigned collect_template_free_vars_quasiquote(unsigned tmpl,
                                                      unsigned pattern_vars,
                                                      unsigned collected,
                                                      int64_t ellipsis,
                                                      unsigned depth,
                                                      unsigned env)
{
    if (!tmpl)
        return collected;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&pattern_vars);
    gc_protect(&collected);

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if ((head_id == ctx.kw_unquote ||
                 head_id == ctx.kw_unquote_splicing) &&
                IS_PAIR(cdr(tmpl))) {
                unsigned body = cadr(tmpl);
                if (depth == 1) {
                    return collect_template_free_vars(
                        body, pattern_vars, collected, ellipsis, env);
                }
                return collect_template_free_vars_quasiquote(
                    body, pattern_vars, collected, ellipsis, depth - 1, env);
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl))) {
                return collect_template_free_vars_quasiquote(
                    cadr(tmpl), pattern_vars, collected, ellipsis, depth + 1,
                    env);
            }
        }

        // A quasiquoted datum can be wide without being deeply nested.
        // Iterate over its ordinary spine and recurse only into elements or
        // the forms whose nesting depth changes quasiquote semantics.
        for (;;) {
            collected = collect_template_free_vars_quasiquote(
                car(tmpl), pattern_vars, collected, ellipsis, depth, env);
            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next))
                return collect_template_free_vars_quasiquote(
                    next, pattern_vars, collected, ellipsis, depth, env);
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_unquote ||
                    next_id == ctx.kw_unquote_splicing ||
                    next_id == ctx.kw_quasiquote)
                    return collect_template_free_vars_quasiquote(
                        next, pattern_vars, collected, ellipsis, depth, env);
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            collected = collect_template_free_vars_quasiquote(
                data[i], pattern_vars, collected, ellipsis, depth, env);
        }
    }

    return collected;
}

// Collect free variables from a syntax-rules template
// pattern_vars: list of pattern variable atoms
// Returns: list of free variable atoms
static unsigned collect_template_free_vars(unsigned tmpl, unsigned pattern_vars,
                                           unsigned collected, int64_t ellipsis,
                                           unsigned env)
{
    if (!tmpl)
        return collected;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&pattern_vars);
    gc_protect(&collected);

    if (IS_ATOM(tmpl)) {
        int64_t id = CELL_ID(tmpl);
        // Skip if pattern variable, ellipsis, or already collected
        if (id == ellipsis)
            return collected;
        for (unsigned p = pattern_vars; p; p = cdr(p)) {
            if (IS_ATOM(car(p)) && CELL_ID(car(p)) == id)
                return collected;
        }
        for (unsigned c = collected; c; c = cdr(c)) {
            if (IS_ATOM(car(c)) && CELL_ID(car(c)) == id)
                return collected;
        }
        // Core keyword names are still collected here. If they are unbound
        // core syntax, the later definition-site lookup will not alias them;
        // if they are lexically rebound, syntax-rules must preserve that
        // definition-site binding.
        return alloc_cons(tmpl, collected);
    }

    if (IS_PAIR(tmpl)) {
        // Skip quote's contents
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quote &&
            !template_id_bound(pattern_vars, ctx.kw_quote) &&
            !template_keyword_bound_as_value(ctx.kw_quote, env))
            return collected;
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_begin &&
            !template_id_bound(pattern_vars, ctx.kw_begin) &&
            !template_keyword_bound_as_value(ctx.kw_begin, env))
            return collect_template_free_vars(
                cdr(tmpl), pattern_vars, collected, ellipsis, env);
        if (syntax_rules_form_like(tmpl) &&
            !template_id_bound(pattern_vars, ctx.kw_syntax_rules) &&
            !template_keyword_bound_as_value(ctx.kw_syntax_rules, env))
            return collected;
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quasiquote &&
            !template_id_bound(pattern_vars, ctx.kw_quasiquote) &&
            !template_keyword_bound_as_value(ctx.kw_quasiquote, env) &&
            IS_PAIR(cdr(tmpl))) {
            return collect_template_free_vars_quasiquote(
                cadr(tmpl), pattern_vars, collected, ellipsis, 1, env);
        }
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_lambda &&
            !template_id_bound(pattern_vars, ctx.kw_lambda) &&
            !template_keyword_bound_as_value(ctx.kw_lambda, env) &&
            IS_PAIR(cdr(tmpl)) && IS_PAIR(cddr(tmpl))) {
            unsigned body_vars =
                add_template_lambda_bounds(pattern_vars, cadr(tmpl));
            gc_protect(&body_vars);
            return collect_template_free_vars(cddr(tmpl), body_vars,
                                              collected, ellipsis, env);
        }
        if (IS_ATOM(car(tmpl)) &&
            (CELL_ID(car(tmpl)) == ctx.kw_let ||
             CELL_ID(car(tmpl)) == ctx.kw_letstar ||
             CELL_ID(car(tmpl)) == ctx.kw_letrec) &&
            !template_id_bound(pattern_vars, CELL_ID(car(tmpl))) &&
            !template_keyword_bound_as_value(CELL_ID(car(tmpl)), env) &&
            IS_PAIR(cdr(tmpl)) && IS_PAIR(cddr(tmpl))) {
            int64_t head_id = CELL_ID(car(tmpl));
            if (head_id == ctx.kw_let && IS_ATOM(cadr(tmpl))) {
                return collect_template_free_vars_named_let(
                    tmpl, pattern_vars, collected, ellipsis, env);
            }
            return collect_template_free_vars_let(
                tmpl, pattern_vars, collected, ellipsis,
                head_id == ctx.kw_letstar, head_id == ctx.kw_letrec, env);
        }
        if (template_case_form(tmpl)) {
            // A case clause's datum list is quoted data: an alias for a
            // datum such as + would change what the clause matches.
            collected = collect_template_free_vars(car(tmpl), pattern_vars,
                                                   collected, ellipsis, env);
            collected = collect_template_free_vars(cadr(tmpl), pattern_vars,
                                                   collected, ellipsis, env);
            unsigned c = cddr(tmpl);
            gc_protect(&c);
            for (; c; c = cdr(c)) {
                if (IS_PAIR(car(c)))
                    collected = collect_template_free_vars(
                        cdr(car(c)), pattern_vars, collected, ellipsis, env);
            }
            return collected;
        }
        // Templates may contain very wide flat forms.  Their cdr spine is
        // data, not structural nesting, so do not consume a C stack frame for
        // each element while collecting definition-site bindings.
        for (;;) {
            collected = collect_template_free_vars(
                car(tmpl), pattern_vars, collected, ellipsis, env);
            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next))
                return collect_template_free_vars(
                    next, pattern_vars, collected, ellipsis, env);
            if (IS_ATOM(car(next)) &&
                is_special_form(CELL_ID(car(next))))
                return collect_template_free_vars(
                    next, pattern_vars, collected, ellipsis, env);
            tmpl = next;
        }
    }

    // A vector is self-evaluating data; nothing in it refers to a binding.

    return collected;
}

// Rename free variables in template according to rename_map
// rename_map: list of (orig_atom . gensym_atom)
static unsigned rename_template_vars_quasiquote(unsigned tmpl,
                                                unsigned rename_map,
                                                unsigned depth)
{
    if (!tmpl || !rename_map)
        return tmpl;

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if ((head_id == ctx.kw_unquote ||
                 head_id == ctx.kw_unquote_splicing) &&
                IS_PAIR(cdr(tmpl))) {
                // The renaming allocates, and tmpl is read after it.
                GC_GUARD;
                gc_protect(&tmpl);
                unsigned body = cadr(tmpl);
                unsigned new_body;
                if (depth == 1)
                    new_body = rename_template_vars(body, rename_map);
                else
                    new_body = rename_template_vars_quasiquote(
                        body, rename_map, depth - 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(car(tmpl), result_tail);
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl))) {
                GC_GUARD;
                gc_protect(&tmpl);
                unsigned new_body = rename_template_vars_quasiquote(
                    cadr(tmpl), rename_map, depth + 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(car(tmpl), result_tail);
            }
        }

        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car =
                rename_template_vars_quasiquote(old_car, rename_map, depth);
            if (new_car == TOK_ERROR) {
                gc_unprotect(5);
                return TOK_ERROR;
            }
            // Compare with the cell as it is now, not old_car: a collection
            // in the call moves the original and can hand its old index to
            // the renamed copy, which then looked unchanged and was dropped.
            if (new_car != car(tmpl))
                changed = true;
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                unsigned new_tail =
                    rename_template_vars_quasiquote(next, rename_map, depth);
                if (new_tail == TOK_ERROR) {
                    gc_unprotect(5);
                    return TOK_ERROR;
                }
                if (new_tail != cdr(tmpl))
                    changed = true;
                if (!changed) {
                    gc_unprotect(5);
                    return original;
                }
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                gc_unprotect(5);
                return result;
            }
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_unquote ||
                    next_id == ctx.kw_unquote_splicing ||
                    next_id == ctx.kw_quasiquote) {
                    unsigned new_tail = rename_template_vars_quasiquote(
                        next, rename_map, depth);
                    if (new_tail == TOK_ERROR) {
                        gc_unprotect(5);
                        return TOK_ERROR;
                    }
                    if (new_tail != cdr(tmpl))
                        changed = true;
                    if (!changed) {
                        gc_unprotect(5);
                        return original;
                    }
                    cell_set_cdr(result_tail, new_tail);
                    gc_unprotect(5);
                    return result;
                }
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR) {
            gc_unprotect(2);
            return TOK_ERROR;
        }
        gc_protect(&new_vec);
        bool changed = false;
        for (unsigned i = 0; i < len; i++) {
            unsigned old_elem = vector_data_ptr(tmpl)[i];
            gc_protect(&old_elem);
            unsigned new_elem =
                rename_template_vars_quasiquote(old_elem, rename_map, depth);
            if (new_elem == TOK_ERROR) {
                gc_unprotect(4);
                return TOK_ERROR;
            }
            vector_set_elem(new_vec, i, new_elem);
            if (new_elem != old_elem)
                changed = true;
            gc_unprotect(1);
        }
        gc_unprotect(3);
        return changed ? new_vec : tmpl;
    }

    return tmpl;
}

static unsigned rename_map_without_id(unsigned rename_map, int64_t id)
{
    if (!rename_map)
        return 0;

    GC_GUARD;
    gc_protect(&rename_map);

    unsigned filtered = 0;
    unsigned tail = 0;
    gc_protect(&filtered);
    gc_protect(&tail);
    bool changed = false;

    unsigned m = rename_map;
    gc_protect(&m);
    for (; m; m = cdr(m)) {
        unsigned entry = car(m);
        if (IS_ATOM(car(entry)) && CELL_ID(car(entry)) == id) {
            changed = true;
            continue;
        }
        list_append(&filtered, &tail, entry);
    }
    gc_unprotect(1);

    return changed ? filtered : rename_map;
}

static unsigned rename_map_without_atom(unsigned rename_map, unsigned var)
{
    if (!IS_ATOM(var))
        return rename_map;
    return rename_map_without_id(rename_map, CELL_ID(var));
}

static unsigned rename_map_without_lambda_bounds(unsigned rename_map,
                                                 unsigned params)
{
    GC_GUARD;
    gc_protect(&rename_map);
    gc_protect(&params);

    unsigned p = params;
    gc_protect(&p);
    for (; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        unsigned param = IS_PAIR(p) ? car(p) : p;
        rename_map = rename_map_without_atom(rename_map, param);
        if (!IS_PAIR(p))
            break;
    }
    gc_unprotect(1);

    return rename_map;
}

static unsigned rename_map_without_let_bounds(unsigned rename_map,
                                              unsigned binding_list)
{
    GC_GUARD;
    gc_protect(&rename_map);
    gc_protect(&binding_list);

    unsigned bl = binding_list;
    gc_protect(&bl);
    for (; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (let_binding_has_value(binding))
            rename_map = rename_map_without_atom(rename_map, car(binding));
    }
    gc_unprotect(1);

    return rename_map;
}

static unsigned rename_template_vars_shadowed_heads(unsigned tmpl,
                                                    unsigned rename_map,
                                                    bool quote_shadowed,
                                                    bool quasiquote_shadowed);

static unsigned rename_template_vars_lambda(unsigned tmpl, unsigned rename_map)
{
    unsigned keyword = car(tmpl);
    unsigned params = cadr(tmpl);
    unsigned body = cddr(tmpl);

    // tmpl is returned as-is when nothing below it changed, and everything
    // below it allocates, so tmpl needs rooting as much as its parts do.
    // Returning a stale index here hands the caller a cell that has since
    // been reused - which silently corrupts the stored macro template.
    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&keyword);
    gc_protect(&params);
    gc_protect(&body);
    gc_protect(&rename_map);

    unsigned body_map = rename_map_without_lambda_bounds(rename_map, params);
    gc_protect(&body_map);
    unsigned new_body = rename_template_vars(body, body_map);
    if (new_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&new_body);

    if (new_body == body)
        return tmpl;
    unsigned result_tail = alloc_cons(params, new_body);
    gc_protect(&result_tail);
    return alloc_cons(keyword, result_tail);
}

static unsigned rename_template_vars_let(unsigned tmpl, unsigned rename_map,
                                         bool is_letstar, bool is_letrec)
{
    unsigned keyword = car(tmpl);
    unsigned binding_list = cadr(tmpl);
    unsigned body = cddr(tmpl);

    // Same as rename_template_vars_lambda: several paths return tmpl
    // unchanged after allocating.
    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&keyword);
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&rename_map);

    if (!proper_list_silent(binding_list))
        return tmpl;

    unsigned body_map = rename_map;
    gc_protect(&body_map);
    if (is_letrec)
        body_map = rename_map_without_let_bounds(body_map, binding_list);

    unsigned new_bindings = 0;
    unsigned new_tail = 0;
    gc_protect(&new_bindings);
    gc_protect(&new_tail);
    bool changed = false;

    unsigned bl = binding_list;
    gc_protect(&bl);
    for (; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding)) {
            list_append(&new_bindings, &new_tail, binding);
            continue;
        }

        unsigned var = car(binding);
        unsigned val = cadr(binding);
        unsigned val_map = (is_letstar || is_letrec) ? body_map : rename_map;
        gc_protect(&var);
        gc_protect(&val);
        gc_protect(&val_map);

        unsigned new_val = rename_template_vars(val, val_map);
        if (new_val == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_val);
        unsigned bind_tail = alloc_cons(new_val, 0);
        gc_protect(&bind_tail);
        unsigned new_binding = alloc_cons(var, bind_tail);
        gc_protect(&new_binding);
        list_append(&new_bindings, &new_tail, new_binding);
        if (new_val != val)
            changed = true;
        gc_unprotect(6);

        if (is_letstar && !is_letrec)
            body_map = rename_map_without_atom(body_map, var);
    }
    gc_unprotect(1);

    if (!is_letstar && !is_letrec)
        body_map = rename_map_without_let_bounds(body_map, binding_list);

    unsigned new_body;
    bool quote_shadowed = binding_list_binds_id(binding_list, ctx.kw_quote);
    bool quasiquote_shadowed =
        binding_list_binds_id(binding_list, ctx.kw_quasiquote);
    if (quote_shadowed || quasiquote_shadowed) {
        new_body = rename_template_vars_shadowed_heads(
            body, body_map, quote_shadowed, quasiquote_shadowed);
    }
    else
        new_body = rename_template_vars(body, body_map);
    if (new_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&new_body);
    if (new_body != body)
        changed = true;

    if (!changed)
        return tmpl;
    unsigned result_tail = alloc_cons(new_bindings, new_body);
    gc_protect(&result_tail);
    return alloc_cons(keyword, result_tail);
}

static unsigned rename_template_vars_named_let(unsigned tmpl,
                                               unsigned rename_map)
{
    unsigned keyword = car(tmpl);
    unsigned name = cadr(tmpl);
    unsigned binding_list = caddr(tmpl);
    unsigned body = cdddr(tmpl);

    // Same as rename_template_vars_lambda: several paths return tmpl
    // unchanged after allocating.
    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&keyword);
    gc_protect(&name);
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&rename_map);

    if (!proper_list_silent(binding_list))
        return tmpl;

    unsigned new_bindings = 0;
    unsigned new_tail = 0;
    gc_protect(&new_bindings);
    gc_protect(&new_tail);
    bool changed = false;

    unsigned bl = binding_list;
    gc_protect(&bl);
    for (; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding)) {
            list_append(&new_bindings, &new_tail, binding);
            continue;
        }

        unsigned var = car(binding);
        unsigned val = cadr(binding);
        gc_protect(&var);
        gc_protect(&val);
        unsigned new_val = rename_template_vars(val, rename_map);
        if (new_val == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_val);
        unsigned bind_tail = alloc_cons(new_val, 0);
        gc_protect(&bind_tail);
        unsigned new_binding = alloc_cons(var, bind_tail);
        gc_protect(&new_binding);
        list_append(&new_bindings, &new_tail, new_binding);
        if (new_val != val)
            changed = true;
        gc_unprotect(5);
    }
    gc_unprotect(1);

    unsigned body_map = rename_map_without_atom(rename_map, name);
    gc_protect(&body_map);
    body_map = rename_map_without_let_bounds(body_map, binding_list);

    unsigned new_body = rename_template_vars(body, body_map);
    if (new_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&new_body);
    if (new_body != body)
        changed = true;

    if (!changed)
        return tmpl;
    unsigned body_tail = alloc_cons(new_bindings, new_body);
    gc_protect(&body_tail);
    unsigned name_tail = alloc_cons(name, body_tail);
    gc_protect(&name_tail);
    return alloc_cons(keyword, name_tail);
}

static unsigned rename_template_vars(unsigned tmpl, unsigned rename_map)
{
    if (!tmpl || !rename_map)
        return tmpl;

    if (IS_ATOM(tmpl)) {
        int64_t id = CELL_ID(tmpl);
        for (unsigned m = rename_map; m; m = cdr(m)) {
            unsigned entry = car(m);
            if (IS_ATOM(car(entry)) && CELL_ID(car(entry)) == id)
                return cdr(entry);
        }
        return tmpl;
    }

    if (IS_PAIR(tmpl)) {
        bool head_is_alias = false;
        if (IS_ATOM(car(tmpl)))
            head_is_alias =
                assoc_list_has_atom_key_id(rename_map, CELL_ID(car(tmpl)));
        // Skip quote's contents
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quote &&
            !head_is_alias)
            return tmpl;
        if (syntax_rules_form_like(tmpl) && !head_is_alias)
            return tmpl;
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quasiquote &&
            IS_PAIR(cdr(tmpl)) && !head_is_alias) {
            // rename_template_vars_quasiquote allocates, and car(tmpl) /
            // cddr(tmpl) are read after it returns, so tmpl has to survive
            // the call. GC_GUARD releases these on every exit, replacing the
            // manual unprotect below.
            GC_GUARD;
            gc_protect(&tmpl);
            gc_protect(&rename_map);
            unsigned new_body =
                rename_template_vars_quasiquote(cadr(tmpl), rename_map, 1);
            if (new_body == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&new_body);
            unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
            gc_protect(&result_tail);
            return alloc_cons(car(tmpl), result_tail);
        }
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_lambda &&
            IS_PAIR(cdr(tmpl)) && IS_PAIR(cddr(tmpl)) && !head_is_alias) {
            return rename_template_vars_lambda(tmpl, rename_map);
        }
        if (IS_ATOM(car(tmpl)) &&
            (CELL_ID(car(tmpl)) == ctx.kw_let ||
             CELL_ID(car(tmpl)) == ctx.kw_letstar ||
             CELL_ID(car(tmpl)) == ctx.kw_letrec) &&
            IS_PAIR(cdr(tmpl)) && IS_PAIR(cddr(tmpl)) && !head_is_alias) {
            int64_t head_id = CELL_ID(car(tmpl));
            if (head_id == ctx.kw_let && IS_ATOM(cadr(tmpl))) {
                return rename_template_vars_named_let(tmpl, rename_map);
            }
            return rename_template_vars_let(tmpl, rename_map,
                                            head_id == ctx.kw_letstar,
                                            head_id == ctx.kw_letrec);
        }
        if (template_case_form(tmpl)) {
            // Rename the key and the clause bodies, never a datum list.
            GC_GUARD;
            gc_protect(&tmpl);
            gc_protect(&rename_map);
            unsigned out = 0, out_tail = 0;
            gc_protect(&out);
            gc_protect(&out_tail);
            list_append(&out, &out_tail,
                        rename_template_vars(car(tmpl), rename_map));
            unsigned key = rename_template_vars(cadr(tmpl), rename_map);
            if (key == TOK_ERROR)
                return TOK_ERROR;
            list_append(&out, &out_tail, key);
            unsigned c = cddr(tmpl);
            gc_protect(&c);
            for (; c; c = cdr(c)) {
                if (!IS_PAIR(car(c))) {
                    list_append(&out, &out_tail, car(c));
                    continue;
                }
                unsigned body = rename_template_vars(cdr(car(c)), rename_map);
                if (body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&body);
                unsigned clause = alloc_cons(car(car(c)), body);
                gc_unprotect(1);
                list_append(&out, &out_tail, clause);
            }
            return out;
        }
        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car = rename_template_vars(old_car, rename_map);
            if (new_car == TOK_ERROR) {
                gc_unprotect(5);
                return TOK_ERROR;
            }
            // Compare with the cell as it is now, not old_car: a collection
            // in the call moves the original and can hand its old index to
            // the renamed copy, which then looked unchanged and was dropped.
            if (new_car != car(tmpl))
                changed = true;
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                unsigned new_tail = rename_template_vars(next, rename_map);
                if (new_tail == TOK_ERROR) {
                    gc_unprotect(5);
                    return TOK_ERROR;
                }
                if (new_tail != cdr(tmpl))
                    changed = true;
                if (!changed) {
                    gc_unprotect(5);
                    return original;
                }
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                gc_unprotect(5);
                return result;
            }
            if (IS_ATOM(car(next)) && is_special_form(CELL_ID(car(next)))) {
                unsigned new_tail = rename_template_vars(next, rename_map);
                if (new_tail == TOK_ERROR) {
                    gc_unprotect(5);
                    return TOK_ERROR;
                }
                if (new_tail != cdr(tmpl))
                    changed = true;
                if (!changed) {
                    gc_unprotect(5);
                    return original;
                }
                cell_set_cdr(result_tail, new_tail);
                gc_unprotect(5);
                return result;
            }
            tmpl = next;
        }
    }

    // A vector is self-evaluating data: an alias inside one would be read
    // back as a gensym, not as the identifier written in the template.
    return tmpl;
}

static unsigned rename_template_vars_shadowed_heads(unsigned tmpl,
                                                    unsigned rename_map,
                                                    bool quote_shadowed,
                                                    bool quasiquote_shadowed)
{
    if (!tmpl || !rename_map)
        return tmpl;

    if (IS_ATOM(tmpl)) {
        int64_t id = CELL_ID(tmpl);
        for (unsigned m = rename_map; m; m = cdr(m)) {
            unsigned entry = car(m);
            if (IS_ATOM(car(entry)) && CELL_ID(car(entry)) == id)
                return cdr(entry);
        }
        return tmpl;
    }

    if (IS_PAIR(tmpl)) {
        bool head_is_alias = false;
        if (IS_ATOM(car(tmpl)))
            head_is_alias =
                assoc_list_has_atom_key_id(rename_map, CELL_ID(car(tmpl)));
        if (syntax_rules_form_like(tmpl) && !head_is_alias)
            return tmpl;
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quote &&
            !quote_shadowed && !head_is_alias)
            return tmpl;
        if (IS_ATOM(car(tmpl)) && CELL_ID(car(tmpl)) == ctx.kw_quasiquote &&
            IS_PAIR(cdr(tmpl)) && !quasiquote_shadowed && !head_is_alias) {
            // rename_template_vars_quasiquote allocates, and car(tmpl) /
            // cddr(tmpl) are read after it returns, so tmpl has to survive
            // the call. GC_GUARD releases these on every exit, replacing the
            // manual unprotect below.
            GC_GUARD;
            gc_protect(&tmpl);
            gc_protect(&rename_map);
            unsigned new_body =
                rename_template_vars_quasiquote(cadr(tmpl), rename_map, 1);
            if (new_body == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&new_body);
            unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
            gc_protect(&result_tail);
            return alloc_cons(car(tmpl), result_tail);
        }

        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car = rename_template_vars_shadowed_heads(
                old_car, rename_map, quote_shadowed, quasiquote_shadowed);
            if (new_car == TOK_ERROR) {
                gc_unprotect(5);
                return TOK_ERROR;
            }
            // Compare with the cell as it is now, not old_car: a collection
            // in the call moves the original and can hand its old index to
            // the renamed copy, which then looked unchanged and was dropped.
            if (new_car != car(tmpl))
                changed = true;
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                unsigned new_tail = rename_template_vars_shadowed_heads(
                    next, rename_map, quote_shadowed, quasiquote_shadowed);
                if (new_tail == TOK_ERROR) {
                    gc_unprotect(5);
                    return TOK_ERROR;
                }
                if (new_tail != cdr(tmpl))
                    changed = true;
                if (!changed) {
                    gc_unprotect(5);
                    return original;
                }
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                gc_unprotect(5);
                return result;
            }
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_quote || next_id == ctx.kw_quasiquote ||
                    syntax_rules_form_like(next)) {
                    unsigned new_tail = rename_template_vars_shadowed_heads(
                        next, rename_map, quote_shadowed,
                        quasiquote_shadowed);
                    if (new_tail == TOK_ERROR) {
                        gc_unprotect(5);
                        return TOK_ERROR;
                    }
                    if (new_tail != cdr(tmpl))
                        changed = true;
                    if (!changed) {
                        gc_unprotect(5);
                        return original;
                    }
                    cell_set_cdr(result_tail, new_tail);
                    gc_unprotect(5);
                    return result;
                }
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        gc_protect(&tmpl);
        gc_protect(&rename_map);
        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR) {
            gc_unprotect(2);
            return TOK_ERROR;
        }
        gc_protect(&new_vec);
        bool changed = false;
        for (unsigned i = 0; i < len; i++) {
            unsigned old_elem = vector_data_ptr(tmpl)[i];
            gc_protect(&old_elem);
            unsigned new_elem = rename_template_vars_shadowed_heads(
                old_elem, rename_map, quote_shadowed, quasiquote_shadowed);
            if (new_elem == TOK_ERROR) {
                gc_unprotect(4);
                return TOK_ERROR;
            }
            vector_set_elem(new_vec, i, new_elem);
            if (new_elem != old_elem)
                changed = true;
            gc_unprotect(1);
        }
        gc_unprotect(3);
        return changed ? new_vec : tmpl;
    }

    return tmpl;
}

// Extract pattern variables from a syntax-rules pattern
static unsigned collect_pattern_vars(unsigned pattern, unsigned collected,
                                     int64_t ellipsis, unsigned literals)
{
    if (!pattern)
        return collected;

    GC_GUARD;
    gc_protect(&pattern);
    gc_protect(&collected);
    gc_protect(&literals);

    if (IS_ATOM(pattern)) {
        int64_t id = CELL_ID(pattern);
        // Skip ellipsis, underscore, literals
        if (!identifier_valid(pattern) || id == ellipsis ||
            id == ctx.kw_underscore)
            return collected;
        for (unsigned l = literals; l; l = cdr(l)) {
            if (IS_ATOM(car(l)) && CELL_ID(car(l)) == id)
                return collected;
        }
        // Check if already collected
        for (unsigned c = collected; c; c = cdr(c)) {
            if (IS_ATOM(car(c)) && CELL_ID(car(c)) == id)
                return collected;
        }
        return alloc_cons(pattern, collected);
    }

    if (IS_PAIR(pattern)) {
        // Walk ordinary list spines iteratively.  Macro patterns can be very
        // wide, and relying on tail-call optimization here makes compilation
        // stack-safe only in optimized builds.
        while (IS_PAIR(pattern)) {
            collected =
                collect_pattern_vars(car(pattern), collected, ellipsis, literals);
            pattern = cdr(pattern);
        }
        return collect_pattern_vars(pattern, collected, ellipsis, literals);
    }

    if (IS_VECTOR(pattern)) {
        unsigned len = vector_len(pattern);
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointer each iteration - GC may have moved pattern
            unsigned *data = vector_data_ptr(pattern);
            collected = collect_pattern_vars(data[i], collected,
                                             ellipsis, literals);
        }
        return collected;
    }

    return collected;
}

// Emit bytecode to define gensyms created during macro expansion
// This ensures gensyms from referential transparency are available at runtime
// A gensym that names a transformer has to be visible to the compiler, not
// only at runtime. The expansion compiled right after this may use it as a
// macro keyword - a syntax-rules macro that recurses through its own name
// (%case-dispatch, guard-aux, do-step) always does - and
// compile_expr_internal classifies a form's head by looking it up in
// cctx->env. The runtime DEFINE alone left that lookup empty, so the nested
// use compiled as a procedure call of the transformer object and its clause
// list as an argument expression: "not a procedure, got integer". Reached
// whenever the template's reference has to be aliased, that is whenever the
// name resolves differently at the use site than where the macro was
// defined: a use site that shadows the inner macro's name, or an eval in a
// scheme-report-environment clone. The define-syntax path binds its own
// transformer into cctx->env the same way after emitting OP_DEFSYNTAX.
static void bind_gensym_transformer_at_compile_time(compile_ctx *cctx,
                                                    unsigned gensym,
                                                    unsigned transformer)
{
    // Uninterned expansion identifiers may be installed in an immutable
    // import environment; they do not alter any of its public bindings.
    defvar(gensym, transformer, cctx->env);
}

static void emit_gensym_definitions(compile_ctx *cctx, unsigned bindings)
{
    GC_GUARD;
    gc_protect(&bindings);
    unsigned b = bindings;
    gc_protect(&b);
    for (; b; b = cdr(b)) {
        unsigned entry = car(b);
        if (!IS_PAIR(entry) || !IS_ATOM(car(entry)))
            continue;

        unsigned gensym = car(entry);
        unsigned target = cdr(entry);
        int64_t gensym_id = CELL_ID(gensym);

        if (IS_BINDING_REF(target)) {
            unsigned binding_ref = target;
            unsigned target_cell = CELL_CAR(binding_ref);
            unsigned target_val = car(target_cell);
            unsigned target_var = cdr(binding_ref);

            if (IS_SYNTAX(target_val) || IS_MACRO(target_val)) {
                emit2(cctx, OP_CONST, code_add_const(cctx->code, target_val));
                emit2(cctx, OP_DEFINE, gensym_id);
                bind_gensym_transformer_at_compile_time(cctx, gensym,
                                                        target_val);
                continue;
            }

            unsigned resolved_target = lookup_silent(CELL_ID(target_var), cctx->env);
            if (resolved_target != TOK_ERROR &&
                (IS_SYNTAX(resolved_target) || IS_MACRO(resolved_target))) {
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, resolved_target));
                emit2(cctx, OP_DEFINE, gensym_id);
                bind_gensym_transformer_at_compile_time(cctx, gensym,
                                                        resolved_target);
                continue;
            }

            int local_slot = find_stack_local_slot(cctx, CELL_ID(target_var));
            if (local_slot >= 0) {
                emit_local_get_slot(cctx, local_slot);
                emit2(cctx, OP_DEFINE, gensym_id);
                continue;
            }

            if (binding_cell_is_stable_global(cctx, CELL_ID(target_var),
                                             target_cell)) {
                emit3(cctx, OP_DEFINE_ALIAS_REF, gensym_id,
                      code_add_const(cctx->code, binding_ref));
                continue;
            }

            emit3(cctx, OP_DEFINE_ALIAS, gensym_id, CELL_ID(target_var));
            continue;
        }

        emit2(cctx, OP_CONST, code_add_const(cctx->code, target));
        emit2(cctx, OP_DEFINE, gensym_id);
        if (IS_SYNTAX(target) || IS_MACRO(target))
            bind_gensym_transformer_at_compile_time(cctx, gensym, target);
    }
}

// A procedural transformer failed - to be made, or on a use: the code
// raises its message, already reported, where it runs.
static compile_result emit_procedural_syntax_error(compile_ctx *cctx)
{
    char message[sizeof(ctx.last_error)];
    snprintf(message, sizeof(message), "%s",
             ctx.last_error[0] ? ctx.last_error : "macro expansion failed");
    return emit_syntax_error(cctx, message);
}

// A rename of a local variable that the use site shadows cannot be compiled:
// a local is reached by name, and the name now means the shadowing binding.
// (A syntax-rules template's free identifiers are aliased where the macro
// is defined instead, but what a procedural transformer will rename is not
// known until it runs.) The interpreter, whose aliases bind the very cell,
// has no such limit. Reported rather than compiled to the wrong variable.
static bool procedural_aliases_reachable(compile_ctx *cctx, unsigned bindings)
{
    for (unsigned b = bindings; IS_PAIR(b); b = cdr(b)) {
        unsigned target = IS_PAIR(car(b)) ? cdr(car(b)) : 0;
        if (!IS_BINDING_REF(target) || !IS_ATOM(cdr(target)))
            continue;
        unsigned cell = CELL_CAR(target);
        int64_t var_id = CELL_ID(cdr(target));
        if (IS_SYNTAX(car(cell)) || IS_MACRO(car(cell)) ||
            env_find_binding_cell(var_id, cctx->env) == cell ||
            binding_cell_is_stable_global(cctx, var_id, cell))
            continue;
        show_error("macro transformer: renamed local %s is shadowed where "
                   "the macro is used (not supported by the compiler)",
                   ctx.atom_table[var_id]);
        return false;
    }
    return true;
}

// (define-syntax name (er-macro-transformer proc)) and the like. The
// transformer is made now, so that the rest of the unit can use the macro,
// and defined again when the code runs, over the run-time environment
// (OP_DEFSYNTAX), as a syntax-rules one is.
static compile_result compile_procedural_define_syntax(unsigned name,
                                                       unsigned form,
                                                       compile_ctx *cctx)
{
    GC_GUARD;
    gc_protect(&name);
    gc_protect(&form);
    unsigned transformer =
        macros_make_procedural_transformer(form, cctx->env, "define-syntax");
    if (transformer == TOK_ERROR)
        return emit_procedural_syntax_error(cctx);
    gc_protect(&transformer);
    emit2(cctx, OP_CONST, code_add_const(cctx->code, transformer));
    emit2(cctx, OP_DEFSYNTAX, CELL_ID(name));
    emit2(cctx, OP_CONST, code_add_const(cctx->code, name));
    defvar(name, transformer, cctx->env);
    return dynamic_result();
}

static bool compile_macro_binding(unsigned mac, unsigned expr,
                                  compile_ctx *cctx, compile_result *result)
{
    if (!IS_SYNTAX(mac) && !IS_MACRO(mac))
        return false;

    // Macro expansion runs before the ordinary application compiler, so it
    // must enforce the same proper-list invariant itself.  In particular, a
    // legacy macro with a rest parameter would otherwise accept a cyclic form
    // and expose it to the expander. A syntax-rules use may be dotted, though
    // - (_ a ... . r) exists to match (m 1 2 . 3) - so only a cycle is
    // rejected there, the way the interpreter's apply_syntax already does.
    if (IS_SYNTAX(mac) ? pair_chain_is_circular(expr)
                       : !list_length_checked(cdr(expr), NULL,
                                              "macro application")) {
        if (IS_SYNTAX(mac))
            show_error("macro application: circular form");
        *result = emit_syntax_error(cctx, "macro application: invalid syntax");
        return true;
    }

    if (cctx->macro_expansion_depth >= MAX_COMPILE_MACRO_EXPANSION_DEPTH) {
        // Deliberately silent here: emit_syntax_error defers the error to the
        // point where this code actually runs. A macro that recurses only in
        // a branch guarded by a runtime test (say (if (= n 0) 'done (m 0)))
        // cannot be expanded statically to a fixed point, but the program is
        // still correct as long as the capped branch is never reached.
        // Reporting at compile time would print an error for a program that
        // then runs fine and exits 0.
        *result =
            emit_syntax_error(cctx, "macro expansion exceeded maximum depth");
        return true;
    }

    cctx->macro_expansion_depth++;

    if (IS_SYNTAX(mac)) {
        unsigned bindings = 0;
        gc_protect(&bindings);
        // Decided before the expansion, which can collect and move mac.
        bool procedural = macros_is_procedural_transformer(mac);
        unsigned expanded = apply_syntax(mac, expr, cctx->env, &bindings);
        if (procedural && expanded != TOK_ERROR &&
            !procedural_aliases_reachable(cctx, bindings))
            expanded = TOK_ERROR;
        if (expanded == TOK_ERROR && procedural) {
            // Raised, when the code runs, with the transformer's own message
            // (already reported) rather than a generic one.
            *result = emit_procedural_syntax_error(cctx);
            cctx->macro_expansion_depth--;
            gc_unprotect(1);
            return true;
        }
        if (expanded == TOK_ERROR) {
            show_error("macro expansion failed");
            *result = emit_syntax_error(cctx, "macro expansion failed");
            cctx->macro_expansion_depth--;
            gc_unprotect(1);
            return true;
        }
        // Expansion may be a large newly allocated tree.  The gensym setup
        // below allocates before compilation starts, so keep that tree rooted
        // across the setup rather than relying on incidental heap headroom.
        gc_protect(&expanded);
        emit_gensym_definitions(cctx, bindings);
        *result = compile_expr_internal(expanded, cctx);
        gc_unprotect(2);
        cctx->macro_expansion_depth--;
        return true;
    }

    if (IS_MACRO(mac)) {
        GC_GUARD;
        unsigned params = car(mac);
        unsigned mbody = car(cdr(mac));
        unsigned menv = cdr(cdr(mac));
        gc_protect(&mbody);
        gc_protect(&menv);
        unsigned frame = bind_params(params, cdr(expr));
        if (frame == TOK_ERROR) {
            *result = emit_syntax_error(cctx, "macro expansion failed");
            cctx->macro_expansion_depth--;
            return true;
        }
        gc_protect(&frame);
        unsigned new_env = alloc_cons(frame, menv);
        gc_protect(&new_env);
        // Scheme code running while this unit is compiled: no code-object
        // sweep may free the unit meanwhile (see code_sweep_inhibit).
        code_sweep_inhibit();
        unsigned expanded = eval_cps(mbody, new_env);
        code_sweep_allow();
        gc_protect(&expanded);
        if (expanded == TOK_ERROR) {
            show_error("macro expansion failed");
            *result = emit_syntax_error(cctx, "macro expansion failed");
            cctx->macro_expansion_depth--;
            return true;
        }
        *result = compile_expr_internal(expanded, cctx);
        cctx->macro_expansion_depth--;
        return true;
    }

    cctx->macro_expansion_depth--;
    return false;
}

// ============================================================================
// define-syntax hygiene: resolve free identifiers at definition time
// ============================================================================
// A macro's free identifiers must resolve to whatever they meant at the
// macro's OWN definition point, not to whatever happens to be in scope at
// each later use site. let-syntax/letrec-syntax already do this (see the
// block above compiling those forms): they collect the transformer's free
// identifiers, rename them to gensyms in the template, and emit alias-
// defining bytecode immediately, before the transformer's own new
// environment frame's body compiles - i.e. before any subsequent
// shadowing scope in the program has a chance to exist. This mirrors
// that mechanism for define-syntax, which previously deferred free-
// identifier resolution to each macro use site via apply_syntax
// (macros.c), a point that can be arbitrarily far from the definition
// and inside arbitrarily many intervening shadowing scopes.

// A body's internal defines may follow a define-syntax that refers to them:
// (define-syntax getx (syntax-rules () ((_) x))) (define x 1) ... (getx).
// The definition-time alias below cannot be made at the define-syntax - x has
// no binding yet at that point of the run - so it is deferred until x's own
// define has run. compile_begin records the body's not-yet-compiled define
// names, and the deferred aliases, in a record (pending-names . aliases)
// bound - as (owner . record) - to a private marker in the body's syntax
// frame: compile-time env frames are GC-managed, and the innermost body's
// record is the one lookup finds. The owner is the body's scope - its code
// object and run-time frame depth - so a nested lambda body, or a let body
// compiled inline under a PUSHENV of its own, does not use (and consume)
// its enclosing body's record, while a begin spliced into the body does.
static int64_t pending_defines_marker_id(void)
{
    static int64_t marker_id = 0;
    if (!marker_id)
        marker_id = CELL_ID(make_uninterned_symbol("##pending-defines##"));
    return marker_id;
}

static bool pending_defines_owned_by(unsigned owner, compile_ctx *cctx)
{
    return IS_PAIR(owner) && IS_NUM(car(owner)) && IS_NUM(cdr(owner)) &&
           CELL_ID(car(owner)) == (int64_t)(intptr_t)cctx->code &&
           CELL_ID(cdr(owner)) == (int64_t)cctx->env_depth;
}

static unsigned pending_defines_owner(compile_ctx *cctx)
{
    GC_GUARD;
    unsigned code = store((int64_t)(intptr_t)cctx->code);
    gc_protect(&code);
    unsigned depth = store((int64_t)cctx->env_depth);
    gc_protect(&depth);
    return alloc_cons(code, depth);
}

static unsigned pending_defines_record(compile_ctx *cctx)
{
    unsigned binding = lookup_silent(pending_defines_marker_id(), cctx->env);
    if (!IS_PAIR(binding) || !pending_defines_owned_by(car(binding), cctx) ||
        !IS_PAIR(cdr(binding)))
        return 0;
    return cdr(binding);
}

// First pass: collect free identifiers across all rules of a single
// transformer, building a rename map and the alias bindings needed to
// install them. Mirrors the let-syntax collection loop but over one
// transformer's rules instead of across multiple sibling bindings.
static bool collect_syntax_free_vars(compile_ctx *cctx, unsigned rules,
                                     int64_t ellipsis_id, unsigned literals,
                                     unsigned *rename_map_out,
                                     unsigned *gensym_bindings_out,
                                     unsigned *gensym_const_bindings_out)
{
    unsigned rename_map = *rename_map_out;
    unsigned gensym_bindings = *gensym_bindings_out;
    unsigned gensym_const_bindings = *gensym_const_bindings_out;
    GC_GUARD;
    gc_protect(&rename_map);
    gc_protect(&gensym_bindings);
    gc_protect(&gensym_const_bindings);
    // literals is a by-value cell parameter that stays live across every
    // iteration below, and collect_pattern_vars/collect_template_free_vars
    // both allocate.
    gc_protect(&literals);

    unsigned r = rules;
    gc_protect(&r);
    while (r) {
        unsigned rule = car(r);
        unsigned pattern = car(rule);
        unsigned tmpl = cadr(rule);
        if (IS_PAIR(pattern))
            pattern = cdr(pattern);

        unsigned pattern_vars =
            collect_pattern_vars(pattern, 0, ellipsis_id, literals);
        gc_protect(&pattern_vars);
        unsigned free_vars = collect_template_free_vars(
            tmpl, pattern_vars, 0, ellipsis_id, cctx->env);
        gc_protect(&free_vars);

        unsigned fv = free_vars;
        gc_protect(&fv);
        while (fv) {
            unsigned var_atom = car(fv);
            gc_protect(&var_atom);
            int64_t var_id = CELL_ID(var_atom);

            bool already_mapped = false;
            for (unsigned m = rename_map; m; m = cdr(m)) {
                if (CELL_ID(car(car(m))) == var_id) {
                    already_mapped = true;
                    break;
                }
            }
            if (already_mapped)
                goto next_free_var;

            unsigned pending = pending_defines_record(cctx);
            if (pending && atom_list_contains_id(car(pending), var_id)) {
                // Defined later in this body: rename now, alias once the
                // define has run (see pending_defines_marker_id).
                gc_protect(&pending);
                extern unsigned gensym_counter;
                char name[32];
                snprintf(name, sizeof(name), "##gensym##%u",
                         gensym_counter++);
                unsigned gensym_atom = make_uninterned_symbol(name);
                gc_protect(&gensym_atom);
                macros_note_alias(CELL_ID(gensym_atom), var_id, false);
                unsigned entry = alloc_cons(var_atom, gensym_atom);
                gc_protect(&entry);
                rename_map = alloc_cons(entry, rename_map);
                unsigned deferred = alloc_cons(entry, cdr(pending));
                cell_set_cdr(pending, deferred);
                // Bound for the compile-time checks of later forms, like the
                // gensyms extend_env_with_syntax_gensyms adds.
                defvar(gensym_atom, gensym_atom, cctx->env);
                gc_unprotect(3);
                goto next_free_var;
            }

            unsigned val = lookup_silent(var_id, cctx->env);
            if (val != TOK_ERROR) {
                extern unsigned gensym_counter;
                // Keep the complete generated name for diagnostics. Symbol
                // identity is independent of its printable name.
                char name[32];
                snprintf(name, sizeof(name), "##gensym##%u",
                         gensym_counter++);
                unsigned gensym_atom = make_uninterned_symbol(name);
                gc_protect(&gensym_atom);
                gc_protect(&val);
                macros_note_alias(CELL_ID(gensym_atom), var_id,
                                  IS_SYNTAX(val));

                unsigned entry = alloc_cons(var_atom, gensym_atom);
                gc_protect(&entry);
                rename_map = alloc_cons(entry, rename_map);
                gc_unprotect(1);

                if (IS_SYNTAX(val) || IS_MACRO(val)) {
                    unsigned const_entry = alloc_cons(gensym_atom, val);
                    gc_protect(&const_entry);
                    gensym_const_bindings =
                        alloc_cons(const_entry, gensym_const_bindings);
                    gc_unprotect(1);
                } else {
                    // Resolve the binding cell now, so the emission below
                    // can embed it directly when safe (see
                    // binding_cell_is_stable_global) instead of
                    // re-resolving var_atom by name at runtime.
                    unsigned target_cell =
                        env_find_binding_cell(var_id, cctx->env);
                    unsigned binding_ref =
                        (target_cell && binding_cell_is_stable_global(
                                            cctx, var_id, target_cell))
                            ? make_binding_ref_cell(var_atom, target_cell)
                            : 0;
                    gc_protect(&binding_ref);
                    unsigned var_and_ref = alloc_cons(var_atom, binding_ref);
                    gc_protect(&var_and_ref);
                    unsigned bind_entry = alloc_cons(gensym_atom, var_and_ref);
                    gc_protect(&bind_entry);
                    gensym_bindings = alloc_cons(bind_entry, gensym_bindings);
                    gc_unprotect(3);
                }
                gc_unprotect(2); // gensym_atom, val
            }
next_free_var:
            gc_unprotect(1); // var_atom
            fv = cdr(fv);
        }
        gc_unprotect(3); // fv, free_vars, pattern_vars
        r = cdr(r);
    }
    gc_unprotect(1); // r

    *rename_map_out = rename_map;
    *gensym_bindings_out = gensym_bindings;
    *gensym_const_bindings_out = gensym_const_bindings;
    return true;
}

// Second pass: rebuild a transformer_form with every rule's template
// rewritten to use the gensym names from rename_map instead of the
// original free identifiers.
static unsigned rebuild_syntax_rules_renamed(unsigned transformer_form,
                                             int64_t default_ellipsis_id,
                                             unsigned rename_map)
{
    if (!rename_map)
        return transformer_form;

    GC_GUARD;
    gc_protect(&transformer_form);
    gc_protect(&rename_map);

    int64_t ellipsis_id = 0;
    unsigned literals = 0;
    unsigned rules = 0;
    syntax_rules_parts(transformer_form, default_ellipsis_id, &ellipsis_id,
                       &literals, &rules);
    gc_protect(&literals);
    gc_protect(&rules);

    unsigned renamed_rules = 0, renamed_tail = 0;
    gc_protect(&renamed_rules);
    gc_protect(&renamed_tail);

    unsigned r = rules;
    gc_protect(&r);
    while (r) {
        unsigned rule = car(r);
        unsigned pattern = car(rule);
        unsigned tmpl = cadr(rule);

        gc_protect(&pattern);
        unsigned renamed_tmpl = rename_template_vars(tmpl, rename_map);
        if (renamed_tmpl == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&renamed_tmpl);
        unsigned new_rule_tail = alloc_cons(renamed_tmpl, 0);
        gc_protect(&new_rule_tail);
        unsigned new_rule = alloc_cons(pattern, new_rule_tail);
        gc_protect(&new_rule);

        unsigned new_cell = alloc_cons(new_rule, 0);
        gc_unprotect(4);
        if (!renamed_rules) {
            renamed_rules = new_cell;
            renamed_tail = new_cell;
        } else {
            cell_set_cdr(renamed_tail, new_cell);
            renamed_tail = new_cell;
        }
        r = cdr(r);
    }

    unsigned sr_atom = car(transformer_form);
    unsigned second = cadr(transformer_form);
    gc_protect(&sr_atom);
    gc_protect(&second);
    unsigned sr_tail;
    if (IS_ATOM(second)) {
        unsigned rules_tail = alloc_cons(literals, renamed_rules);
        gc_protect(&rules_tail);
        sr_tail = alloc_cons(second, rules_tail);
        gc_unprotect(1); // rules_tail's slot dies here; the cons below allocates
    } else {
        sr_tail = alloc_cons(literals, renamed_rules);
    }
    return alloc_cons(sr_atom, sr_tail);
}

// Emit bytecode that binds each gensym to its resolved value, executed
// immediately (at the macro's own definition point).
static void emit_syntax_gensym_aliases(compile_ctx *cctx,
                                       unsigned gensym_bindings,
                                       unsigned gensym_const_bindings)
{
    for (unsigned gb = gensym_const_bindings; gb; gb = cdr(gb)) {
        unsigned entry = car(gb);
        unsigned gensym_atom = car(entry);
        unsigned val = cdr(entry);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, val));
        emit2(cctx, OP_DEFINE, CELL_ID(gensym_atom));
    }
    for (unsigned gb = gensym_bindings; gb; gb = cdr(gb)) {
        unsigned entry = car(gb);
        unsigned gensym_atom = car(entry);
        unsigned var_and_ref = cdr(entry);
        unsigned var_atom = car(var_and_ref);
        unsigned binding_ref = cdr(var_and_ref);
        int64_t gensym_id = CELL_ID(gensym_atom);
        int64_t var_id = CELL_ID(var_atom);

        int local_slot = find_stack_local_slot(cctx, var_id);
        if (local_slot >= 0) {
            emit_local_get_slot(cctx, local_slot);
            emit2(cctx, OP_DEFINE, gensym_id);
        } else if (binding_ref) {
            emit3(cctx, OP_DEFINE_ALIAS_REF, gensym_id,
                  code_add_const(cctx->code, binding_ref));
        } else {
            emit3(cctx, OP_DEFINE_ALIAS, gensym_id, var_id);
        }
    }
}

// Make the gensyms visible in the compile-time environment (mutating the
// CURRENT frame in place, like an ordinary internal define - define-syntax
// does not introduce its own scope, so no new frame/env_depth change is
// needed or safe here).
static void extend_env_with_syntax_gensyms(compile_ctx *cctx,
                                           unsigned gensym_bindings,
                                           unsigned gensym_const_bindings)
{
    // defvar conses a new binding onto the frame, so it can collect. Both
    // lists arrive as by-value copies that no caller's gc_protect covers, and
    // the cursor walking them has to survive each iteration's allocation -
    // otherwise the second lap reads cdr() out of a reclaimed cell.
    GC_GUARD;
    gc_protect(&cctx->env);
    gc_protect(&gensym_bindings);
    gc_protect(&gensym_const_bindings);

    unsigned gb = 0;
    gc_protect(&gb);
    for (gb = gensym_const_bindings; gb; gb = cdr(gb)) {
        unsigned entry = car(gb);
        defvar(car(entry), cdr(entry), cctx->env);
    }
    for (gb = gensym_bindings; gb; gb = cdr(gb)) {
        unsigned entry = car(gb);
        unsigned gensym_atom = car(entry);
        // Placeholder value - the real value is only known at runtime;
        // this just marks the gensym as bound for shadowing/hygiene checks
        // during compilation of later forms.
        defvar(gensym_atom, gensym_atom, cctx->env);
    }
}

// ============================================================================
// Compile Expression
// ============================================================================

static compile_result compile_expr_internal(unsigned expr, compile_ctx *cctx)
{
    // Top-level code is one code object per batch of forms; note where each
    // form starts so a frame in it reports its own form's line.
    if (!cctx->parent && IS_PAIR(expr)) {
        source_info info;
        if (reader_source_info(expr, &info)) {
            code_note_line(cctx->code, info.line);
            code_source *src = code_source_of(cctx->code);
            if (src && !src->file) {
                src->file = info.file;
                cctx->code->source_line = info.line;
            }
        }
    }
    if (!expr) {
        // nil - constant
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
        return const_result(0);
    }
    if (IS_FIXNUM(expr)) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, expr));
        return const_result(expr);
    }
    if (!IS_CELL(expr)) {
        show_error("compile: invalid expression");
        emit(cctx, OP_HALT);
        return dynamic_result();
    }

    GC_GUARD;
    gc_protect(&expr);

    switch (CELL_TYPE(expr)) {
    case BT_NUM:
    case BT_BIGNUM:
    case BT_INEXACT:
    case BT_RATIONAL:
    case BT_COMPLEX:
    case BT_STRING:
    case BT_CHAR:
    case BT_VECTOR:
        // Self-evaluating: push as constant
        emit2(cctx, OP_CONST, code_add_const(cctx->code, expr));
        return const_result(expr);

    case BT_ATOM: {
        if (expr == ctx.atom_true || expr == ctx.atom_false) {
            emit2(cctx, OP_CONST, code_add_const(cctx->code, expr));
            return const_result(expr);
        }

        // Check if this variable is a stack local
        int64_t var_id = CELL_ID(expr);
        int local_slot = find_stack_local_slot(cctx, var_id);
        if (local_slot >= 0) {
            emit_local_get_slot(cctx, local_slot);
            return dynamic_result();
        }
        // Fall back to environment lookup
        emit4(cctx, OP_LOOKUP, var_id, IC_UNCACHED, IC_UNCACHED);
        return dynamic_result();
    }

    case BT_CONS: {
        unsigned head = car(expr);

        // Check for special forms
        if (IS_ATOM(head)) {
            // Dispatch on the keyword, a core alias read as the one it
            // stands for, but look up the head as written: the alias is the
            // core form wherever its name has been rebound.
            int64_t kw = kw_canon(CELL_ID(head));
            unsigned special_binding = lookup_silent(CELL_ID(head), cctx->env);
            compile_result macro_result;
            if (special_binding != TOK_ERROR &&
                compile_macro_binding(special_binding, expr, cctx,
                                      &macro_result))
                return macro_result;

            if (special_binding == TOK_ERROR && kw == ctx.kw_quote) {
                if (!syntax_arity_checked(expr, 1, 1, "quote"))
                    return emit_syntax_error(cctx, "quote: invalid syntax");
                unsigned val = macros_strip_core_aliases(cadr(expr));
                gc_protect(&val);
                emit2(cctx, OP_CONST, code_add_const(cctx->code, val));
                gc_unprotect(1);
                return const_result(val);
            }
            if (special_binding == TOK_ERROR && kw == ctx.kw_if)
                return compile_if(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_lambda)
                return compile_lambda(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_begin)
                return compile_begin(cdr(expr), cctx);
            if (special_binding == TOK_ERROR &&
                (kw == ctx.kw_let || kw == ctx.kw_letstar ||
                 kw == ctx.kw_letrec)) {
                if (kw == ctx.kw_let)
                    return compile_let(expr, cctx);
                if (kw == ctx.kw_letstar)
                    return compile_letstar(expr, cctx);
                return compile_letrec(expr, cctx);
            }
            if (special_binding == TOK_ERROR && kw == ctx.kw_and)
                return compile_and(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_or)
                return compile_or(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_cond)
                return compile_cond(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_cond_expand)
                return compile_cond_expand(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_define)
                return compile_define(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_set)
                return compile_set(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_quasiquote)
                return compile_quasiquote(expr, cctx);
            if (special_binding == TOK_ERROR && kw == ctx.kw_define_syntax) {
                // (define-syntax name transformer)
                if (!syntax_arity_checked(expr, 2, 2, "define-syntax"))
                    return emit_syntax_error(cctx,
                                             "define-syntax: invalid syntax");
                unsigned name = cadr(expr);
                unsigned transformer_form = caddr(expr);
                if (identifier_valid(name) &&
                    macros_procedural_transformer_form(transformer_form))
                    return compile_procedural_define_syntax(
                        name, transformer_form, cctx);
                int64_t default_ellipsis_id =
                    syntax_default_ellipsis_id(cctx->env);
                if (!identifier_valid(name) ||
                    !syntax_rules_valid(transformer_form, default_ellipsis_id,
                                        "define-syntax"))
                    return emit_syntax_error(cctx,
                                             "define-syntax: invalid syntax");

                GC_GUARD;
                gc_protect(&name);
                gc_protect(&transformer_form);

                // Hygiene: resolve the transformer's free identifiers NOW,
                // at this definition point, instead of lazily at each
                // macro use site (which may be arbitrarily far away,
                // inside intervening shadowing scopes that would
                // otherwise capture them). Mirrors let-syntax below.
                int64_t ellipsis_id = 0;
                unsigned literals = 0, rules = 0;
                syntax_rules_parts(transformer_form, default_ellipsis_id,
                                   &ellipsis_id, &literals, &rules);
                gc_protect(&rules);

                unsigned rename_map = 0;
                unsigned gensym_bindings = 0;
                unsigned gensym_const_bindings = 0;
                gc_protect(&rename_map);
                gc_protect(&gensym_bindings);
                gc_protect(&gensym_const_bindings);
                collect_syntax_free_vars(cctx, rules, ellipsis_id, literals,
                                         &rename_map, &gensym_bindings,
                                         &gensym_const_bindings);

                unsigned renamed_form = rebuild_syntax_rules_renamed(
                    transformer_form, default_ellipsis_id, rename_map);
                if (renamed_form == TOK_ERROR)
                    return emit_syntax_error(cctx,
                                             "define-syntax: invalid syntax");
                gc_protect(&renamed_form);

                emit_syntax_gensym_aliases(cctx, gensym_bindings,
                                          gensym_const_bindings);
                extend_env_with_syntax_gensyms(cctx, gensym_bindings,
                                              gensym_const_bindings);

                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, renamed_form));
                emit2(cctx, OP_DEFSYNTAX, CELL_ID(name));
                emit2(cctx, OP_CONST, code_add_const(cctx->code, name));

                unsigned transformer = make_syntax_transformer_with_default_ellipsis(
                    renamed_form, cctx->env, default_ellipsis_id);
                if (transformer == TOK_ERROR)
                    return emit_syntax_error(cctx,
                                             "define-syntax: invalid syntax");
                gc_protect(&transformer);
                defvar(name, transformer, cctx->env);
                return dynamic_result();
            }

            if (special_binding == TOK_ERROR &&
                (kw == ctx.kw_let_syntax || kw == ctx.kw_letrec_syntax)) {
                // (let-syntax ((name transformer) ...) body ...)
                // (letrec-syntax ((name transformer) ...) body ...)
                const char *form_name = kw == ctx.kw_let_syntax
                                            ? "let-syntax"
                                            : "letrec-syntax";
                if (!syntax_arity_checked(expr, 2, UINT_MAX, form_name))
                    return emit_syntax_error(
                        cctx, "syntax binding: invalid syntax");
                unsigned bindings = cadr(expr);
                unsigned body = cddr(expr);
                if (!binding_list_valid(bindings, form_name) ||
                    !list_length_checked(body, NULL, form_name))
                    return emit_syntax_error(
                        cctx, "syntax binding: invalid syntax");

                // If no bindings, compile body in the current env so internal
                // defines splice into the enclosing body
                if (!bindings) {
                    return compile_begin(body, cctx);
                }

                GC_GUARD;
                gc_protect(&bindings);
                gc_protect(&body);

                unsigned new_env = cctx->env;
                gc_protect(&new_env);
                unsigned frame_vars = 0, frame_vals = 0;
                gc_protect(&frame_vars);
                gc_protect(&frame_vals);

                // Collect free variables from all templates and create rename map
                unsigned rename_map = 0;
                gc_protect(&rename_map);
                unsigned gensym_bindings = 0; // List of (gensym . orig_var_id)
                gc_protect(&gensym_bindings);
                unsigned gensym_const_bindings = 0; // List of (gensym . value)
                gc_protect(&gensym_const_bindings);

                // First pass: create frame with placeholder values for letrec-syntax
                if (kw == ctx.kw_letrec_syntax) {
                    unsigned frame = alloc_cons(0, 0);
                    gc_protect(&frame);
                    new_env = alloc_cons(frame, cctx->env);
                    gc_unprotect(1);
                }

                // First pass: collect all free variables from all templates
                unsigned b = bindings;
                gc_protect(&b);
                while (b) {
                    unsigned binding = car(b);
                    unsigned transformer_form = cadr(binding);
                    if (macros_procedural_transformer_form(transformer_form)) {
                        // No template: made in the second pass.
                        b = cdr(b);
                        continue;
                    }

                    int64_t default_ellipsis_id =
                        kw == ctx.kw_let_syntax
                            ? syntax_default_ellipsis_id(cctx->env)
                            : syntax_default_ellipsis_id(new_env);
                    if (!syntax_rules_valid(transformer_form,
                                            default_ellipsis_id, form_name))
                        return emit_syntax_error(
                            cctx, "syntax binding: invalid syntax");

                    int64_t ellipsis_id = 0;
                    unsigned literals = 0;
                    unsigned rules = 0;
                    syntax_rules_parts(transformer_form, default_ellipsis_id,
                                       &ellipsis_id, &literals, &rules);

                    unsigned r = rules;
                    gc_protect(&r);
                    while (r) {
                        unsigned rule = car(r);
                        unsigned pattern = car(rule);
                        unsigned tmpl = cadr(rule);

                        // Skip macro name in pattern
                        if (IS_PAIR(pattern))
                            pattern = cdr(pattern);

                        // Collect pattern variables
                        unsigned pattern_vars =
                            collect_pattern_vars(pattern, 0, ellipsis_id, literals);
                        gc_protect(&pattern_vars);
                        if (kw == ctx.kw_letrec_syntax)
                            pattern_vars =
                                add_template_syntax_bounds(pattern_vars,
                                                           bindings);

                        // Collect free variables from template
                        unsigned free_vars = collect_template_free_vars(
                            tmpl, pattern_vars, 0, ellipsis_id, cctx->env);
                        gc_protect(&free_vars);

                        // For each free var, create gensym if bound in outer scope
                        unsigned fv = free_vars;
                        gc_protect(&fv);
                        while (fv) {
                            unsigned var_atom = car(fv);
                            gc_protect(&var_atom);
                            int64_t var_id = CELL_ID(var_atom);

                            // Check if already in rename_map
                            bool already_mapped = false;
                            for (unsigned m = rename_map; m; m = cdr(m)) {
                                if (CELL_ID(car(car(m))) == var_id) {
                                    already_mapped = true;
                                    break;
                                }
                            }
                            if (already_mapped)
                                goto next_free_var;

                            // Check if bound in outer scope (compile-time env)
                            unsigned val = lookup_silent(var_id, cctx->env);
                            if (val != TOK_ERROR) {
                                // Create gensym
                                extern unsigned gensym_counter;
                                // Leave room for the complete diagnostic name.
                                char name[32];
                                snprintf(name, sizeof(name), "##gensym##%u",
                                         gensym_counter++);
                                unsigned gensym_atom = make_uninterned_symbol(name);
                                gc_protect(&gensym_atom);
                                gc_protect(&val);
                                // So a renamed case is still seen as case
                                macros_note_alias(CELL_ID(gensym_atom), var_id,
                                                  IS_SYNTAX(val));

                                // Add to rename map: (var_atom . gensym_atom)
                                unsigned entry = alloc_cons(var_atom, gensym_atom);
                                gc_protect(&entry);
                                rename_map = alloc_cons(entry, rename_map);
                                gc_unprotect(1);

                                if (IS_SYNTAX(val) || IS_MACRO(val)) {
                                    unsigned const_entry =
                                        alloc_cons(gensym_atom, val);
                                    gc_protect(&const_entry);
                                    gensym_const_bindings =
                                        alloc_cons(const_entry,
                                                   gensym_const_bindings);
                                    gc_unprotect(1);
                                } else {
                                    // Resolve the binding cell now, against
                                    // this definition-time environment, so
                                    // the emission below can embed it
                                    // directly instead of re-resolving
                                    // var_atom by name at runtime (which
                                    // would find whatever shadows it at the
                                    // use site instead). Only safe when the
                                    // binding lives in the persistent global
                                    // frame - see binding_cell_is_stable_global.
                                    unsigned target_cell =
                                        env_find_binding_cell(var_id,
                                                              cctx->env);
                                    unsigned binding_ref =
                                        (target_cell &&
                                        binding_cell_is_stable_global(
                                            cctx, var_id, target_cell))
                                            ? make_binding_ref_cell(
                                                  var_atom, target_cell)
                                            : 0;
                                    gc_protect(&binding_ref);
                                    unsigned var_and_ref =
                                        alloc_cons(var_atom, binding_ref);
                                    gc_protect(&var_and_ref);
                                    unsigned bind_entry =
                                        alloc_cons(gensym_atom, var_and_ref);
                                    gc_protect(&bind_entry);
                                    gensym_bindings =
                                        alloc_cons(bind_entry,
                                                   gensym_bindings);
                                    gc_unprotect(3);
                                }

                                gc_unprotect(2);
                            }
next_free_var:
                            gc_unprotect(1);
                            fv = cdr(fv);
                        }
                        gc_unprotect(1);
                        gc_unprotect(2);
                        r = cdr(r);
                    }
                    gc_unprotect(1);
                    b = cdr(b);
                }
                gc_unprotect(1);

                // Second pass: create transformers with renamed templates
                b = bindings;
                gc_protect(&b);
                while (b) {
                    unsigned binding = car(b);
                    unsigned name = car(binding);
                    unsigned transformer_form = cadr(binding);
                    gc_protect(&name);

                    if (macros_procedural_transformer_form(transformer_form)) {
                        unsigned transformer =
                            macros_make_procedural_transformer(
                                transformer_form,
                                kw == ctx.kw_let_syntax ? cctx->env : new_env,
                                form_name);
                        if (transformer == TOK_ERROR)
                            return emit_procedural_syntax_error(cctx);
                        gc_protect(&transformer);
                        unsigned vc = alloc_cons(name, frame_vars);
                        gc_protect(&vc);
                        unsigned ac = alloc_cons(transformer, frame_vals);
                        gc_unprotect(3);
                        frame_vars = vc;
                        frame_vals = ac;
                        b = cdr(b);
                        continue;
                    }

                    // Check it's a syntax-rules form
                    int64_t default_ellipsis_id =
                        kw == ctx.kw_let_syntax
                            ? syntax_default_ellipsis_id(cctx->env)
                            : syntax_default_ellipsis_id(new_env);
                    if (!syntax_rules_valid(transformer_form,
                                            default_ellipsis_id, form_name)) {
                        return emit_syntax_error(cctx,
                                                 "syntax binding: invalid syntax");
                    }

                    // Rename free vars in templates
                    unsigned renamed_form = transformer_form;
                    if (rename_map) {
                        gc_protect(&transformer_form);
                        gc_protect(&renamed_form);

                        // Rebuild syntax-rules with renamed templates
                        int64_t ellipsis_id = 0;
                        unsigned literals = 0;
                        unsigned rules = 0;
                        syntax_rules_parts(transformer_form,
                                           default_ellipsis_id,
                                           &ellipsis_id, &literals, &rules);
                        gc_protect(&literals);
                        gc_protect(&rules);

                        unsigned renamed_rules = 0;
                        gc_protect(&renamed_rules);
                        unsigned renamed_tail = 0;
                        gc_protect(&renamed_tail);

                        unsigned r = rules;
                        gc_protect(&r);
                        while (r) {
                            unsigned rule = car(r);
                            unsigned pattern = car(rule);
                            unsigned tmpl = cadr(rule);

                            gc_protect(&pattern);
                            unsigned renamed_tmpl = rename_template_vars(tmpl, rename_map);
                            if (renamed_tmpl == TOK_ERROR) {
                                return emit_syntax_error(
                                    cctx, "syntax binding: invalid syntax");
                            }
                            gc_protect(&renamed_tmpl);
                            unsigned new_rule_tail = alloc_cons(renamed_tmpl, 0);
                            gc_protect(&new_rule_tail);
                            unsigned new_rule = alloc_cons(pattern, new_rule_tail);
                            gc_protect(&new_rule);

                            unsigned new_cell = alloc_cons(new_rule, 0);
                            gc_unprotect(4);
                            if (!renamed_rules) {
                                renamed_rules = new_cell;
                                renamed_tail = new_cell;
                            } else {
                                cell_set_cdr(renamed_tail, new_cell);
                                renamed_tail = new_cell;
                            }
                            r = cdr(r);
                        }
                        gc_unprotect(1);

                        unsigned sr_atom = car(transformer_form);
                        unsigned second = cadr(transformer_form);
                        gc_protect(&sr_atom);
                        gc_protect(&second);
                        unsigned sr_tail = 0;
                        gc_protect(&sr_tail);
                        if (IS_ATOM(second)) {
                            unsigned rules_tail =
                                alloc_cons(literals, renamed_rules);
                            gc_protect(&rules_tail);
                            sr_tail = alloc_cons(second, rules_tail);
                            gc_unprotect(1);
                        } else {
                            sr_tail = alloc_cons(literals, renamed_rules);
                        }
                        renamed_form = alloc_cons(sr_atom, sr_tail);
                        gc_unprotect(9);
                    }

                    // Free vars are pre-renamed, but the default ellipsis
                    // decision still belongs to the transformer definition
                    // environment.
                    unsigned transformer_closure_env =
                        kw == ctx.kw_let_syntax ? cctx->env : new_env;
                    unsigned transformer =
                        make_syntax_transformer_with_default_ellipsis(
                            renamed_form, transformer_closure_env,
                            default_ellipsis_id);
                    if (transformer == TOK_ERROR) {
                        return emit_syntax_error(
                            cctx, "syntax binding: invalid syntax");
                    }

                    // Add to frame
                    gc_protect(&transformer);
                    unsigned vc = alloc_cons(name, frame_vars);
                    gc_protect(&vc);
                    unsigned ac = alloc_cons(transformer, frame_vals);
                    gc_unprotect(3);
                    frame_vars = vc;
                    frame_vals = ac;
                    b = cdr(b);
                }
                gc_unprotect(1);

                // Create frame if let-syntax (letrec-syntax already has frame)
                if (kw == ctx.kw_let_syntax) {
                    unsigned frame = alloc_cons(frame_vars, frame_vals);
                    gc_protect(&frame);
                    new_env = alloc_cons(frame, cctx->env);
                    gc_unprotect(1);
                } else {
                    // Update letrec-syntax frame
                    unsigned frame = car(new_env);
                    cell_set_car(frame, frame_vars);
                    cell_set_cdr(frame, frame_vals);
                }

                // Push a new runtime frame for the body
                emit_pushenv(cctx);

                // Emit gensym bindings: look up original var, define gensym
                // This happens at let-syntax entry, BEFORE any inner scopes
                for (unsigned gb = gensym_const_bindings; gb; gb = cdr(gb)) {
                    unsigned entry = car(gb);
                    unsigned gensym_atom = car(entry);
                    unsigned val = cdr(entry);
                    int64_t gensym_id = CELL_ID(gensym_atom);

                    emit2(cctx, OP_CONST, code_add_const(cctx->code, val));
                    emit2(cctx, OP_DEFINE, gensym_id);
                }

                for (unsigned gb = gensym_bindings; gb; gb = cdr(gb)) {
                    unsigned entry = car(gb);
                    unsigned gensym_atom = car(entry);
                    unsigned var_and_ref = cdr(entry);
                    unsigned var_atom = car(var_and_ref);
                    unsigned binding_ref = cdr(var_and_ref);
                    int64_t gensym_id = CELL_ID(gensym_atom);
                    int64_t var_id = CELL_ID(var_atom);

                    // A VM stack local has no heap binding cell (its value
                    // lives in a register), so it must be captured by value.
                    int local_slot = find_stack_local_slot(cctx, var_id);
                    if (local_slot >= 0) {
                        emit_local_get_slot(cctx, local_slot);
                        emit2(cctx, OP_DEFINE, gensym_id);
                    } else if (binding_ref) {
                        // Embed the binding cell resolved at let-syntax
                        // definition time instead of re-resolving var_atom
                        // by name at runtime, which would find whatever
                        // shadows it at the use site and break hygiene.
                        emit3(cctx, OP_DEFINE_ALIAS_REF, gensym_id,
                              code_add_const(cctx->code, binding_ref));
                    } else {
                        emit3(cctx, OP_DEFINE_ALIAS, gensym_id, var_id);
                    }
                }

                // Add gensyms to compile-time env so they're visible in body
                if (gensym_bindings || gensym_const_bindings) {
                    unsigned gensym_frame_vars = 0, gensym_frame_vals = 0;
                    gc_protect(&gensym_frame_vars);
                    gc_protect(&gensym_frame_vals);

                    unsigned gcb = gensym_const_bindings;
                    gc_protect(&gcb);
                    while (gcb) {
                        unsigned entry = car(gcb);
                        unsigned gensym_atom = car(entry);
                        unsigned val = cdr(entry);
                        gensym_frame_vars =
                            alloc_cons(gensym_atom, gensym_frame_vars);
                        gensym_frame_vals = alloc_cons(val, gensym_frame_vals);
                        gcb = cdr(gcb);
                    }
                    gc_unprotect(1);

                    unsigned gb = gensym_bindings;
                    gc_protect(&gb);
                    while (gb) {
                        unsigned entry = car(gb);
                        unsigned gensym_atom = car(entry);
                        // Use a placeholder value - actual value is set at runtime
                        gensym_frame_vars = alloc_cons(gensym_atom, gensym_frame_vars);
                        gensym_frame_vals = alloc_cons(gensym_atom, gensym_frame_vals);
                        gb = cdr(gb);
                    }
                    gc_unprotect(1);

                    unsigned gensym_frame = alloc_cons(gensym_frame_vars, gensym_frame_vals);
                    gc_protect(&gensym_frame);
                    new_env = alloc_cons(gensym_frame, new_env);
                    gc_unprotect(3);
                }

                // Compile body with new compile-time environment. In this
                // context, not a copy of it: the body records things that
                // belong to the whole procedure (let slots and their
                // high-water mark, assignments to stack locals, scopes to
                // recompile) and a copy would drop them on return.
                unsigned body_saved_env = cctx->env;
                unsigned body_saved_known = cctx->known_lambdas;
                gc_protect(&body_saved_env);
                gc_protect(&body_saved_known);
                cctx->env = new_env;
                compile_begin(body, cctx);
                cctx->env = body_saved_env;
                cctx->known_lambdas = body_saved_known;
                gc_unprotect(2);

                // Pop the runtime frame
                emit_popenv(cctx);
                return dynamic_result();
            }

            // Check for macro application (silent - don't warn if not found)
            unsigned mac = lookup_silent(kw, cctx->env);
            if (mac != TOK_ERROR) {
                compile_result application_macro_result;
                if (compile_macro_binding(mac, expr, cctx,
                                          &application_macro_result))
                    return application_macro_result;
            }
        }

        // Regular function call
        return compile_call(expr, cctx);
    }

    default:
        // Self-evaluating (ports, functions, etc.)
        emit2(cctx, OP_CONST, code_add_const(cctx->code, expr));
        return const_result(expr);
    }
}

// ============================================================================
// Special Form Compilation
// ============================================================================

// (if test then else)
static compile_result compile_if(unsigned expr, compile_ctx *cctx)
{
    GC_GUARD;
    gc_protect(&expr);

    if (!syntax_arity_checked(expr, 2, 3, "if"))
        return emit_syntax_error(cctx, "if: invalid syntax");

    bool tail = cctx->tail_position;
    unsigned saved_pos = cctx->code->code_len;

    // Compile test (not in tail position)
    cctx->tail_position = false;
    compile_result test_result = compile_expr_internal(cadr(expr), cctx);

    // Branch folding: if test is constant, only compile one branch
    if (test_result.is_const) {
        // Rewind - don't need the test bytecode
        code_truncate(cctx->code, saved_pos, cctx->code->children_len);
        cctx->tail_position = tail;

        if (IS_TRUTHY(test_result.value)) {
            // Test is truthy - compile only then branch
            return compile_expr_internal(caddr(expr), cctx);
        } else {
            // Test is false - compile only else branch
            if (cdddr(expr)) {
                return compile_expr_internal(cadddr(expr), cctx);
            } else {
                emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
                return const_result(0);
            }
        }
    }

    // Non-constant test - emit normal conditional
    // Jump to else if false
    unsigned branch_env = cctx->env;
    gc_protect(&branch_env);
    unsigned else_jump = emit_jump(cctx, OP_JUMPIFNOT);

    // Compile then branch
    cctx->tail_position = tail;
    compile_result then_result = compile_expr_internal(caddr(expr), cctx);
    cctx->env = branch_env;

    // Jump over else
    unsigned end_jump = emit_jump(cctx, OP_JUMP);

    // Compile else branch
    patch_jump(cctx, else_jump);
    cctx->tail_position = tail;
    compile_result else_result;
    if (cdddr(expr)) {
        else_result = compile_expr_internal(cadddr(expr), cctx);
    } else {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
        else_result = const_result(0);
    }
    cctx->env = branch_env;

    patch_jump(cctx, end_jump);

    (void)then_result;
    (void)else_result;
    return dynamic_result();
}

// (lambda params body...)
// ============================================================================
// Tail Recursion Modulo Cons
// ============================================================================
//
//   (define (build n) (if (= n 0) '() (cons n (build (- n 1)))))
//
// build is not tail recursive: the call sits inside a cons, so every level
// needs a frame to hold the pending cons. TRMC turns it into a loop that
// builds the list forwards and mutates the last cell's cdr in place - what
// map in stdlib.scm already does by hand.
//
// It is sound only while the half-built list is unobservable, and Vesper has
// full multi-shot continuations, so that is not free. If anything in the body
// captures one and it is invoked a second time, the loop resumes with the
// accumulator restored from the captured stack and starts writing into cells
// the first run already returned to somebody. OCaml's [@tail_mod_cons] never
// faces this because OCaml has no call/cc; Gambit and Bigloo restrict the
// transform.
//
// So the whole body has to be capture-free by construction, not just the cons
// operands: the base-case branch runs with the accumulator live too. The
// policy lives in the two predicates below and nowhere else, so widening it
// later is one decision rather than an audit.

// Bound on the recursive walks below. Deep enough for any body worth
// transforming, and it keeps a pathological expression from running the C
// stack out during analysis.
#define TRMC_MAX_SCAN_DEPTH 64

// A primitive with no side effects that cannot re-enter Scheme. Both modes
// need this, for different reasons: HOLE because such a primitive cannot
// capture a continuation, FOLD because deferring its application to the
// return has to be invisible. Everything foldable qualifies; cons, list,
// append and string-append are barred from constant folding only because
// they allocate fresh mutable objects, which is irrelevant to both.
static bool trmc_safe_primitive(int64_t prim_id)
{
    return primitive_has_flag(prim_id, PRIM_PURE);
}

// A plain TAILCALL hands the callee's value straight back to our caller,
// skipping the return that finishes the accumulator. Demoting it to an
// ordinary call is not good enough either: that breaks proper tail calls,
// and unboundedly, not by one frame - a mutual recursion whose cycle passes
// through this body would grow the stack once per round trip. So inside a
// TRMC body a tail call becomes TAILCALL_TRMC, which decides at runtime on
// the one condition that matters: with the accumulator still empty the
// return would pass the value through untouched, so a real tail call is
// equivalent and R7RS requires it.
static unsigned trmc_call_opcode(compile_ctx *cctx, bool tail)
{
    if (!tail)
        return OP_CALL;
    return cctx->trmc_mode != TRMC_MODE_NONE ? OP_TAILCALL_TRMC : OP_TAILCALL;
}

// True if evaluating expr cannot capture a continuation. Narrow on purpose:
// literals, quoted data, variable references, if/begin, and applications of
// capture-free primitives. A general (f x) is excluded because f might
// capture - that is what rules out (cons (f x) (walk (cdr xs))).
static bool trmc_capture_free(unsigned expr, compile_ctx *cctx, int depth)
{
    if (depth > TRMC_MAX_SCAN_DEPTH)
        return false;
    if (!IS_PAIR(expr))
        return true; // self-evaluating literal, or a variable reference

    unsigned head = car(expr);
    if (!IS_ATOM(head))
        return false;

    int64_t kw = kw_canon(CELL_ID(head));
    unsigned binding = lookup_silent(CELL_ID(head), cctx->env);

    if (binding == TOK_ERROR) {
        // Unbound: a special form, but only while unshadowed - the same
        // condition compile_expr_internal uses to dispatch one.
        if (kw == ctx.kw_quote)
            return true;
        if (kw == ctx.kw_if || kw == ctx.kw_begin) {
            for (unsigned r = cdr(expr); IS_PAIR(r); r = cdr(r))
                if (!trmc_capture_free(car(r), cctx, depth + 1))
                    return false;
            return true;
        }
        return false;
    }

    if (!IS_BUILTIN(binding) || !trmc_safe_primitive(CELL_ID(binding)) ||
        !env_binding_is_immutable(CELL_ID(head), cctx->env))
        return false;
    for (unsigned a = cdr(expr); IS_PAIR(a); a = cdr(a))
        if (!trmc_capture_free(car(a), cctx, depth + 1))
            return false;
    return true;
}

// A tail call to the letrec-bound loop variable, at the arity the loop
// optimization can turn into a jump.
static bool trmc_is_self_call(unsigned expr, compile_ctx *cctx)
{
    if (!IS_PAIR(expr) || !IS_ATOM(car(expr)) || cctx->loop_var_id < 0)
        return false;
    if ((int64_t)CELL_ID(car(expr)) != cctx->loop_var_id)
        return false;
    return list_length(cdr(expr)) == cctx->loop_arity;
}

// Only these leave an addressable hole: cons ends in one open cdr, append is
// a run of them. Every other operator has to be replayed at the return.
static bool trmc_op_has_hole(int64_t op, unsigned argc)
{
    return (op == PCONS && argc == 2) || (op == PAPPEND && argc >= 2);
}

// A tail-position application of a safe primitive whose last argument is the
// recursive call - the shape both modes rewrite. The operator must still
// denote the builtin here, the same shadowing check the foldable-primitive
// path makes, since a user is allowed to rebind cons or append.
//
// *operands is the whole argument list, whose last element is the call, and
// *call is that call.  Restrict the transform to binary calls.  Collapsing the
// leading operands of an n-ary call before the recursion is not generally
// equivalent: (list a b X) is not (list (list a b) X), comparisons do not
// left-fold through booleans, and even an associative primitive can observe a
// mutable operand only after X has run in the source program.
static bool trmc_classify(unsigned expr, compile_ctx *cctx, unsigned *operands,
                          unsigned *call, int64_t *op)
{
    if (!IS_PAIR(expr) || !IS_ATOM(car(expr)))
        return false;
    unsigned fn = lookup_silent(CELL_ID(car(expr)), cctx->env);
    if (fn == TOK_ERROR || !IS_BUILTIN(fn))
        return false;
    int64_t prim = CELL_ID(fn);
    if (!trmc_safe_primitive(prim))
        return false;

    unsigned args = cdr(expr);
    if (list_length(args) != 2)
        return false;

    unsigned last = args;
    while (IS_PAIR(cdr(last)))
        last = cdr(last);
    if (!trmc_is_self_call(car(last), cctx))
        return false;

    if (operands)
        *operands = args;
    if (call)
        *call = car(last);
    if (op)
        *op = prim;
    return true;
}

// What the scan learns about a body. capture_free is what separates the two
// modes: it is only HOLE that needs it, and it has to hold for the whole body
// rather than just the operands, because the base-case branch runs with the
// accumulator live too.
typedef struct {
    bool found;        // saw at least one site
    bool hole_ok;      // every site's operator leaves a hole
    bool capture_free; // nothing anywhere in the body can capture
} trmc_scan_state;

// Walk the tail positions of expr, recording what the scan finds. False only
// when the body cannot be transformed at all; anything that merely rules out
// HOLE mode clears st->capture_free instead. if and begin are followed
// because they push no environment frame, so env_depth stays 0 and the loop
// jump does not skip a POPENV. let does push one, which is why it is not
// followed here.
static bool trmc_scan_tail(unsigned expr, compile_ctx *cctx,
                           trmc_scan_state *st, int depth)
{
    if (depth > TRMC_MAX_SCAN_DEPTH)
        return false;

    unsigned operands = 0, call = 0;
    int64_t op = 0;
    if (trmc_classify(expr, cctx, &operands, &call, &op)) {
        // Each FOLD entry records its own operator; HOLE sites all fill the
        // same final cdr, so cons and append may share an accumulator too.
        st->found = true;
        if (!trmc_op_has_hole(op, list_length(operands)) ||
            !env_binding_is_immutable(CELL_ID(car(expr)), cctx->env))
            st->hole_ok = false;
        for (unsigned a = operands; IS_PAIR(cdr(a)); a = cdr(a))
            if (!trmc_capture_free(car(a), cctx, 0))
                st->capture_free = false;
        for (unsigned a = cdr(call); IS_PAIR(a); a = cdr(a))
            if (!trmc_capture_free(car(a), cctx, 0))
                st->capture_free = false;
        return true;
    }

    if (trmc_is_self_call(expr, cctx)) {
        // A plain self tail call already compiles to a jump, which carries the
        // accumulator along; it just does not add to it.
        for (unsigned a = cdr(expr); IS_PAIR(a); a = cdr(a))
            if (!trmc_capture_free(car(a), cctx, 0))
                st->capture_free = false;
        return true;
    }

    if (IS_PAIR(expr) && IS_ATOM(car(expr))) {
        int64_t kw = kw_canon(CELL_ID(car(expr)));
        if (lookup_silent(CELL_ID(car(expr)), cctx->env) == TOK_ERROR) {
            if (kw == ctx.kw_if) {
                unsigned rest = cdr(expr);
                unsigned len = list_length(rest);
                if (len < 2 || len > 3)
                    return false;
                // The test is not in tail position; the branches are.
                if (!trmc_capture_free(car(rest), cctx, 0))
                    st->capture_free = false;
                if (!trmc_scan_tail(cadr(rest), cctx, st, depth + 1))
                    return false;
                if (len == 3 && !trmc_scan_tail(caddr(rest), cctx, st, depth + 1))
                    return false;
                return true;
            }
            if (kw == ctx.kw_begin) {
                unsigned e = cdr(expr);
                if (!IS_PAIR(e))
                    return true;
                while (IS_PAIR(cdr(e))) {
                    if (!trmc_capture_free(car(e), cctx, 0))
                        st->capture_free = false;
                    e = cdr(e);
                }
                return trmc_scan_tail(car(e), cctx, st, depth + 1);
            }
            if (kw == ctx.kw_cond) {
                // Like if, cond pushes no environment frame, so a site in a
                // clause body is still at env_depth 0.
                for (unsigned c = cdr(expr); IS_PAIR(c); c = cdr(c)) {
                    unsigned clause = car(c);
                    if (!IS_PAIR(clause))
                        return false;
                    unsigned test = car(clause);
                    unsigned conseq = cdr(clause);
                    bool is_else =
                        IS_KEYWORD(test, ctx.kw_else) &&
                        env_find_binding_cell(CELL_ID(test), cctx->env) == 0;
                    if (!is_else && !trmc_capture_free(test, cctx, 0))
                        st->capture_free = false;
                    if (!IS_PAIR(conseq))
                        continue; // (test): the test value is the result
                    if (IS_KEYWORD(car(conseq), ctx.kw_arrow) &&
                        env_find_binding_cell(CELL_ID(car(conseq)),
                                              cctx->env) == 0) {
                        // (test => receiver) applies the receiver, and a call
                        // can capture whatever the receiver expression is.
                        st->capture_free = false;
                        continue;
                    }
                    unsigned e = conseq;
                    while (IS_PAIR(cdr(e))) {
                        if (!trmc_capture_free(car(e), cctx, 0))
                            st->capture_free = false;
                        e = cdr(e);
                    }
                    if (!trmc_scan_tail(car(e), cctx, st, depth + 1))
                        return false;
                }
                return true;
            }
            if (kw == ctx.kw_lambda)
                return true; // a lambda in tail position is just a value
            if (kw == ctx.kw_let && IS_PAIR(cdr(expr)) &&
                !IS_ATOM(cadr(expr))) {
                // Plain let, not named let. The scan reads the body in the
                // enclosing compile-time environment, so it cannot see that a
                // binding here shadows something it resolved to a primitive -
                // (let ((car f)) ...) would look capture-free and would not
                // be. Rather than extend the environment during analysis,
                // give up on HOLE for these bodies; FOLD has no such
                // requirement and takes them unchanged.
                st->capture_free = false;
                for (unsigned b = cadr(expr); IS_PAIR(b); b = cdr(b))
                    if (!IS_PAIR(car(b)))
                        return false;
                unsigned e = cddr(expr);
                if (!IS_PAIR(e))
                    return true;
                while (IS_PAIR(cdr(e)))
                    e = cdr(e);
                return trmc_scan_tail(car(e), cctx, st, depth + 1);
            }
            if (kw == ctx.kw_and || kw == ctx.kw_or) {
                // Only the last operand is in tail position - compile_and and
                // compile_or both set tail_position = tail && is_last - so a
                // site can only appear there.
                unsigned e = cdr(expr);
                if (!IS_PAIR(e))
                    return true;
                while (IS_PAIR(cdr(e))) {
                    if (!trmc_capture_free(car(e), cctx, 0))
                        st->capture_free = false;
                    e = cdr(e);
                }
                return trmc_scan_tail(car(e), cctx, st, depth + 1);
            }
        }
    }

    // ((lambda (v ...) body) arg ...) in tail position - what the stdlib's own
    // let macro expands to, and the shape compile_call inlines rather than
    // building a closure for.
    //
    // captured_by_inner_lambda knows this lambda is inlined rather than made
    // into a closure (inlined_lambda_application), so a loop parameter
    // mentioned under a let keeps its stack slot - which TRMC needs for the
    // accumulator's address.
    //
    // Same caveat as a let besides: the scan reads the body without these
    // bindings in scope, so it cannot tell that one shadows a name it
    // resolved to a primitive. HOLE is given up; FOLD is unaffected.
    if (IS_PAIR(expr) && form_is_core(car(expr), ctx.kw_lambda, cctx->env)) {
        unsigned fn = car(expr);
        unsigned params = IS_PAIR(cdr(fn)) ? cadr(fn) : 0;
        unsigned fn_body = IS_PAIR(cdr(fn)) ? cddr(fn) : 0;
        // Only the shape the inliner accepts: fixed arity, exact match.
        unsigned param_count = 0;
        bool ok = !IS_ATOM(params);
        for (unsigned p = params; ok && p; p = IS_PAIR(p) ? cdr(p) : 0) {
            if (IS_ATOM(p)) {
                ok = false; // rest parameter
                break;
            }
            param_count++;
        }
        if (ok && param_count == list_length(cdr(expr)) && IS_PAIR(fn_body)) {
            st->capture_free = false;
            for (unsigned a = cdr(expr); IS_PAIR(a); a = cdr(a))
                if (!trmc_capture_free(car(a), cctx, 0))
                    st->capture_free = false;
            unsigned e = fn_body;
            while (IS_PAIR(cdr(e)))
                e = cdr(e);
            return trmc_scan_tail(car(e), cctx, st, depth + 1);
        }
    }

    // Any other tail position - the base case, typically. FOLD mode does not
    // care what it is, since its accumulator is never mutated and so is never
    // reachable from a continuation captured here.
    if (!trmc_capture_free(expr, cctx, 0))
        st->capture_free = false;
    return true;
}

// Decided for the whole body before it is compiled, so TRMC_INIT can be
// emitted ahead of the loop entry point. Returns the mode, and the operator
// to use in FOLD mode.
static unsigned char trmc_body_qualifies(unsigned body, compile_ctx *cctx)
{
    // Stack locals only: the accumulator lives at bp + trmc_slot, and bp is
    // only per-invocation for functions whose parameters are stack locals.
    // An environment-frame call leaves bp pointing at the caller's window.
    if (!cctx->code->use_locals || cctx->loop_var_id < 0 ||
        cctx->loop_arity == 0 || cctx->loop_arity > 16)
        return TRMC_MODE_NONE;

    unsigned e = body;
    if (!IS_PAIR(e))
        return TRMC_MODE_NONE;

    trmc_scan_state st = {false, true, true};
    while (IS_PAIR(cdr(e))) {
        if (!trmc_capture_free(car(e), cctx, 0))
            st.capture_free = false;
        e = cdr(e);
    }
    if (!trmc_scan_tail(car(e), cctx, &st, 0) || !st.found)
        return TRMC_MODE_NONE;

    // HOLE needs stable external bindings as well as a capture-free body.
    // Mutable environments use deferred calls so later rebinding is visible.
    return (cctx->immutable_base && st.hole_ok && st.capture_free)
               ? TRMC_MODE_HOLE : TRMC_MODE_FOLD;
}

// A TAILCALL anywhere in a TRMC body would hand back the callee's value
// without closing the accumulator chain, silently truncating the list.
// trmc_body_qualifies is meant to rule that out, but it reads the body before
// macro expansion, so check what was actually emitted rather than trusting
// the pre-pass. A mismatch costs one recompile, not a wrong answer.
static bool trmc_code_has_tailcall(const code_object *code, unsigned from)
{
    for (unsigned ip = from; ip < code->code_len;) {
        unsigned size = instruction_size(code->code[ip]);
        if (size == 0 || size > code->code_len - ip)
            return true; // malformed; do not risk it
        if (code->code[ip] == OP_TAILCALL)
            return true;
        ip += size;
    }
    return false;
}

// True if code (a closure compiled inside a stack-local body) or anything
// nested in it reaches sym through the environment. A stack local has no
// environment binding, so such a reference would resolve to some outer
// binding instead. A nested lambda's own parameter of that name shadows it.
static bool formals_bind(unsigned formals, int64_t sym)
{
    for (unsigned guard = 0; formals && guard < 1000000; guard++) {
        if (IS_ATOM(formals))
            return CELL_ID(formals) == sym;
        if (!IS_PAIR(formals))
            return false;
        if (IS_ATOM(car(formals)) && CELL_ID(car(formals)) == sym)
            return true;
        formals = cdr(formals);
    }
    return false;
}

static bool code_reaches_symbol(const code_object *code, int64_t sym,
                                int depth)
{
    if (!code || depth > 64 || code->code_len > code->code_cap ||
        code->children_len > code->children_cap || !code->code ||
        (code->children_len && !code->children))
        return true; // be conservative
    if (code->params < code->const_len &&
        formals_bind(code->constants[code->params], sym))
        return false;
    for (unsigned ip = 0; ip < code->code_len;) {
        unsigned op = code->code[ip];
        unsigned size = instruction_size(op);
        if (size == 0 || size > code->code_len - ip)
            return true;
        switch (op) {
        case OP_LOOKUP:
        case OP_SET:
        case OP_SET_VOID:
            if ((int64_t)code->code[ip + 1] == sym)
                return true;
            break;
        case OP_GUARD_PRIMITIVE:
        case OP_DEFINE_ALIAS:
            if ((int64_t)code->code[ip + 2] == sym)
                return true;
            break;
        default:
            break;
        }
        ip += size;
    }
    for (unsigned i = 0; i < code->children_len; i++)
        if (code_reaches_symbol(code->children[i], sym, depth + 1))
            return true;
    return false;
}

// True if code emitted from 'from' on defines into whatever frame is current
// when it starts: a DEFINE (or alias, or syntax definition) not inside a
// PUSHENV ... POPENV of its own.
static bool code_defines_outside_own_frames(const code_object *code,
                                            unsigned from)
{
    unsigned depth = 0;
    for (unsigned ip = from; ip < code->code_len;) {
        unsigned op = code->code[ip];
        unsigned size = instruction_size(op);
        if (size == 0 || size > code->code_len - ip)
            return true;
        switch (op) {
        case OP_PUSHENV:
            depth++;
            break;
        case OP_POPENV:
            if (depth > 0)
                depth--;
            break;
        case OP_DEFINE:
        case OP_DEFINE_ALIAS:
        case OP_DEFINE_ALIAS_REF:
        case OP_DEFSYNTAX:
            if (depth == 0)
                return true;
            break;
        default:
            break;
        }
        ip += size;
    }
    return false;
}

// True if a closure made since children_start reaches sym by name.
static bool children_reach_symbol(const code_object *code,
                                  unsigned children_start, int64_t sym)
{
    for (unsigned i = children_start; i < code->children_len; i++)
        if (code_reaches_symbol(code->children[i], sym, 0))
            return true;
    return false;
}

// A stack-local body must not need an environment frame of its own. The
// decision to use locals is made from the unexpanded source, so check what
// was actually emitted:
// - a definition at the body's own scope (no PUSHENV around it) would land
//   in the closure's environment - for a top-level procedure, the global
//   one - e.g. from a macro that expands into define or define-values;
// - a closure created in the body that reaches a parameter by name would
//   find an outer binding, since the parameter lives only in a stack slot.
// compile_call checks each let scope that uses slots the same way.
static bool locals_body_needs_environment(const code_object *code,
                                          unsigned body_start,
                                          unsigned children_start,
                                          unsigned params)
{
    if (code_defines_outside_own_frames(code, body_start))
        return true;
    for (unsigned p = params; IS_PAIR(p); p = cdr(p)) {
        if (IS_ATOM(car(p)) &&
            children_reach_symbol(code, children_start, CELL_ID(car(p))))
            return true;
    }
    return false;
}

// The name compile_lambda gives the code object of procedure_name_expr:
// define, letrec and named let set it around compiling a binding's value, so
// only a lambda that IS the value is named - (define f (let (...) (lambda
// ...))) stays anonymous, as in MIT. compile_lambda clears it on use, so a
// lambda nested in the value can never pick it up.
static unsigned procedure_name_expr;
static int64_t procedure_name_id = -1;

static compile_result compile_named_value(unsigned value, unsigned name,
                                          compile_ctx *cctx)
{
    GC_GUARD;
    unsigned saved_expr = procedure_name_expr;
    int64_t saved_id = procedure_name_id;
    gc_protect(&saved_expr);
    procedure_name_expr = value;
    procedure_name_id = IS_ATOM(name) ? CELL_ID(name) : -1;
    gc_protect(&procedure_name_expr);
    compile_result result = compile_expr_internal(value, cctx);
    procedure_name_expr = saved_expr;
    procedure_name_id = saved_id;
    return result;
}

// Record a procedure's name and where it was read. The position comes from
// the reader (keyed by the body list, which define and named let pass on
// unchanged); a lambda a macro built has a fresh body, so it gets the
// position of the code it is compiled within instead.
static void note_procedure_source(code_object *code, int64_t name_id,
                                  unsigned body, compile_ctx *parent)
{
    source_info info;
    bool found = reader_source_info(body, &info);
    if (name_id < 0 && found)
        name_id = info.name;
    const char *file = NULL;
    if (found && info.line) {
        file = info.file;
        code->source_line = info.line;
    } else if (parent && parent->code) {
        file = code_file(parent->code);
        code->source_line =
            code_line_at(parent->code, parent->code->code_len);
    }
    bool named = name_id >= 0 && (uint64_t)name_id < ctx.atom_table_cap;
    if (!named && !file)
        return;
    code_source *src = code_source_of(code);
    if (!src)
        return;
    src->name = named ? ctx.atom_table[name_id] : NULL;
    src->file = file;
}

static compile_result compile_lambda(unsigned expr, compile_ctx *cctx)
{
    GC_GUARD;
    unsigned rest = cdr(expr);
    if (!IS_PAIR(rest))
        return emit_syntax_error(cctx, "lambda: expected formals and body");
    unsigned params = car(rest);
    unsigned body = cdr(rest);
    int64_t name_id = -1;
    if (procedure_name_expr && procedure_name_expr == expr) {
        name_id = procedure_name_id;
        procedure_name_expr = 0;
    }

    // Create compile-time environment that shadows lambda parameters
    // This prevents the compiler from inlining builtins that are shadowed
    // by lambda parameters (e.g., (lambda (exit) (exit 'x)) should not
    // call the exit primitive)
    unsigned lambda_env = cctx->env;

    gc_protect(&params);
    gc_protect(&body);
    gc_protect(&lambda_env);

    // lambda_params_valid allocates, so locals must be protected first
    if (!body || !list_length_checked(body, NULL, "lambda") ||
        !lambda_params_valid(params))
        return emit_syntax_error(cctx, "lambda: invalid syntax");

    // Build a dummy frame with parameters as vars, all bound to 0
    {
        GC_GUARD;
        unsigned vars = 0, vals = 0;
        gc_protect(&vars);
        gc_protect(&vals);
        unsigned vars_tail = 0, vals_tail = 0;
        gc_protect(&vars_tail);
        gc_protect(&vals_tail);

        unsigned p = params;
        gc_protect(&p);
        for (; p; p = IS_PAIR(p) ? cdr(p) : 0) {
            unsigned var = IS_PAIR(p) ? car(p) : p;
            gc_protect(&var);
            unsigned vc = alloc_cons(var, 0);
            gc_protect(&vc);
            unsigned ac = alloc_cons(0, 0); // dummy value
            if (!vars) {
                vars = vc;
                vals = ac;
            } else {
                cell_set_cdr(vars_tail, vc);
                cell_set_cdr(vals_tail, ac);
            }
            vars_tail = vc;
            vals_tail = ac;
            gc_unprotect(2);
            if (!IS_PAIR(p))
                break; // rest param
        }

        gc_unprotect(1);

        if (vars) {
            unsigned frame = alloc_cons(vars, vals);
            gc_protect(&frame);
            lambda_env = alloc_cons(frame, lambda_env);
            gc_unprotect(1);
        }
    }

    // Create new compilation context for lambda body with shadowed params
    compile_ctx *lambda_cctx = cctx_new(cctx, lambda_env);
    if (!lambda_cctx) {
        emit(cctx, OP_HALT);
        return dynamic_result();
    }
    lambda_cctx->tail_position = true;

    // Pick up loop info staged by an enclosing letrec. Only the letrec
    // binding's own lambda may take it (loop_pending); lambdas nested inside
    // the loop body must NOT inherit it, or their tail calls to the loop
    // variable would compile as self-jumps into the wrong code object.
    if (cctx->loop_pending && cctx->loop_var_id >= 0) {
        lambda_cctx->loop_var_id = cctx->loop_var_id;
        lambda_cctx->loop_params = cctx->loop_params;
        lambda_cctx->loop_arity = cctx->loop_arity;
        // Clear parent's loop info so it doesn't leak to other children
        cctx->loop_var_id = -1;
        cctx->loop_pending = false;
    }

    // Count parameters
    unsigned arity = 0;
    bool has_rest = false;
    for (unsigned p = params; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        if (IS_ATOM(p)) {
            // Rest parameter
            has_rest = true;
            break;
        }
        arity++;
    }

    note_procedure_source(lambda_cctx->code, name_id, body, cctx);

    // Store params in constant pool so they survive GC
    lambda_cctx->code->params = code_add_const(lambda_cctx->code, params);
    lambda_cctx->code->arity = arity;
    lambda_cctx->code->has_rest = has_rest;

    // Stack locals: keep params on VM stack for direct access (LOCAL_GET)
    // instead of in environment frames. Only for fixed-arity functions where
    // no parameter is captured by an inner closure or assigned (see the
    // prediction walks above). A body with internal defines needs an
    // environment frame for them; see body_defines_internally. A procedure
    // without parameters qualifies too: its let bindings can use slots.
    if (!has_rest && arity <= 8 &&
        !body_defines_internally(body, lambda_env)) {
        bool can_use_locals = true;
        unsigned slot = 0;
        for (unsigned p = params; p && IS_PAIR(p); p = cdr(p), slot++) {
            int64_t pid = CELL_ID(car(p));
            if (captured_by_inner_lambda(body, pid, lambda_env) ||
                assigned_in_body(body, pid, lambda_env)) {
                can_use_locals = false;
                break;
            }
            lambda_cctx->local_ids[slot] = pid;
        }
        if (can_use_locals) {
            lambda_cctx->num_locals = (int)arity;
            lambda_cctx->code->use_locals = true;
            lambda_cctx->locals_env_len = compile_env_length(lambda_env);
            lambda_cctx->let_slots = true;
        }
    }

    // compile_begin leaves its scope frames on the context's environment, so
    // a recompile below must start again from the lambda's own environment.
    unsigned body_start = lambda_cctx->code->code_len;
    unsigned body_env = lambda_cctx->env;
    gc_protect(&body_env);
    unsigned body_known = lambda_cctx->known_lambdas;
    gc_protect(&body_known);
    unsigned children_start = lambda_cctx->code->children_len;
    bool allow_trmc = true;
    unsigned let_attempts = 0;

    // Compile the body, and again whenever what was emitted shows that a
    // decision made beforehand from the source was wrong. Each retry gives
    // up something, so this ends: banned let scopes (bounded attempts, then
    // no let slots at all), then stack locals, then TRMC.
    for (;;) {
        code_truncate(lambda_cctx->code, body_start, children_start);
        lambda_cctx->env = body_env;
        lambda_cctx->known_lambdas = body_known;
        lambda_cctx->env_depth = 0;
        lambda_cctx->tail_position = true;
        lambda_cctx->locals_assigned = false;
        lambda_cctx->let_slot_retry = false;
        lambda_cctx->let_scope_counter = 0;
        lambda_cctx->let_binding_count = 0;

        // Tail recursion modulo cons. Decided before the body is compiled,
        // since TRMC_INIT has to sit ahead of the loop entry point.
        unsigned char trmc_mode = allow_trmc
                                      ? trmc_body_qualifies(body, lambda_cctx)
                                      : TRMC_MODE_NONE;
        lambda_cctx->trmc_mode = trmc_mode;
        lambda_cctx->code->trmc_mode = trmc_mode;
        if (trmc_mode != TRMC_MODE_NONE) {
            lambda_cctx->trmc_slot = (unsigned)lambda_cctx->num_locals;
            lambda_cctx->code->trmc_slot = lambda_cctx->trmc_slot;
            emit(lambda_cctx, OP_TRMC_INIT);
        }
        // Self tail calls jump here, not to ip 0: re-running TRMC_INIT would
        // discard the accumulator. Set unconditionally so the loop
        // optimization stops assuming nothing is ever emitted before the
        // body.
        lambda_cctx->loop_start = lambda_cctx->code->code_len;

        // Let slots follow the parameters and the TRMC accumulator. Their
        // count is known only once the body is compiled, so the reservation
        // is patched afterwards (and dropped by the peephole pass if zero).
        // It sits at the loop entry, so a self call clears them too.
        unsigned reserve_operand = 0;
        if (lambda_cctx->let_slots) {
            unsigned base = (unsigned)lambda_cctx->num_locals +
                            (trmc_mode == TRMC_MODE_HOLE   ? 2
                             : trmc_mode == TRMC_MODE_FOLD ? 1
                                                           : 0);
            lambda_cctx->let_slot_base = base;
            lambda_cctx->let_slot_next = base;
            lambda_cctx->let_slot_max = base;
            emit2(lambda_cctx, OP_RESERVE_LOCALS, 0);
            reserve_operand = lambda_cctx->code->code_len - 1;
        }

        compile_body(body, lambda_cctx);

        if (lambda_cctx->let_slot_retry) {
            // A let scope needs an environment frame after all; its ordinal
            // is now banned. Ordinals are stable between attempts (the same
            // source compiles in the same order), but give up on let slots
            // for this body rather than loop if they somehow are not.
            if (++let_attempts >= 4 || !lambda_cctx->let_slots) {
                lambda_cctx->let_slots = false;
                lambda_cctx->let_scope_ban_count = 0;
            }
            continue;
        }
        if (lambda_cctx->code->use_locals &&
            (lambda_cctx->locals_assigned ||
             locals_body_needs_environment(lambda_cctx->code, body_start,
                                           children_start, params))) {
            // See locals_body_needs_environment and compile_set: the
            // pre-pass over the source chose stack locals, but the expanded
            // body needs the parameters in an environment frame. Compile it
            // again that way.
            lambda_cctx->code->use_locals = false;
            lambda_cctx->num_locals = -1;
            lambda_cctx->let_slots = false;
            continue;
        }
        if (trmc_mode != TRMC_MODE_NONE &&
            trmc_code_has_tailcall(lambda_cctx->code, body_start)) {
            // See trmc_code_has_tailcall. trmc_call_opcode should have kept
            // every tail call in this body out of TAILCALL form, so reaching
            // here means some path emitted one anyway. Throw the body away
            // and compile it again plainly rather than return a truncated
            // result.
            allow_trmc = false;
            continue;
        }
        if (reserve_operand)
            code_patch(lambda_cctx->code, reserve_operand,
                       lambda_cctx->let_slot_max - lambda_cctx->let_slot_base);
        break;
    }

    if (lambda_cctx->code->use_locals) {
        emit2(lambda_cctx, OP_RETURN_LOCALS, lambda_cctx->num_locals);
    } else {
        emit(lambda_cctx, OP_RETURN);
    }

    // Add child code object
    unsigned child_idx = code_add_child(cctx->code, lambda_cctx->code);

    // Emit closure creation in parent
    emit2(cctx, OP_CLOSURE, child_idx);

    cctx_free(lambda_cctx);

    // Lambdas are never compile-time constants (they capture environment)
    return dynamic_result();
}

// Helper to check if an expression is an internal define
// Returns the variable name if it's a define, 0 otherwise
static unsigned is_internal_define(unsigned expr, unsigned env)
{
    if (!IS_PAIR(expr))
        return 0;
    if (!form_is_core(expr, ctx.kw_define, env))
        return 0;
    if (!IS_PAIR(cdr(expr)))
        return 0;
    unsigned second = cadr(expr);
    // (define (name params...) body...) or (define name expr). A name that
    // is no identifier is left to compile_define to report: bound into the
    // compile-time frame it corrupted that frame for every later define.
    unsigned name = IS_PAIR(second) ? car(second) : second;
    return identifier_valid(name) ? name : 0;
}

// Splice nested begins while collecting the leading definitions. Return
// false at the first expression so callers stop scanning the enclosing body.
static bool collect_internal_defines(unsigned body, unsigned env,
                                     unsigned *names)
{
    GC_GUARD;
    gc_protect(&body);
    gc_protect(&env);

    while (IS_PAIR(body)) {
        unsigned expr = car(body);
        gc_protect(&expr);

        // Check for direct define
        unsigned name = is_internal_define(expr, env);
        if (name) {
            gc_protect(&name);
            *names = alloc_cons(name, *names);
            gc_unprotect(1);
            body = cdr(body);
            gc_unprotect(1);
            continue;
        }

        // A begin can contain more begins, all in the same definition scope.
        if (IS_PAIR(expr) && IS_KEYWORD(car(expr), ctx.kw_begin) &&
            !is_keyword_shadowed(CELL_ID(car(expr)), env)) {
            if (collect_internal_defines(cdr(expr), env, names)) {
                body = cdr(body);
                gc_unprotect(1);
                continue;
            }
        }

        // define-syntax is a definition too (R7RS 5.3, 5.4): the defines
        // after it share the body's scope. Stopping here left them out of
        // the compile-time frame, so a macro defined first resolved their
        // names past the body - to a global, or to nothing at all.
        if (IS_PAIR(expr) && IS_KEYWORD(car(expr), ctx.kw_define_syntax) &&
            !is_keyword_shadowed(CELL_ID(car(expr)), env)) {
            body = cdr(body);
            gc_unprotect(1);
            continue;
        }

        return false;
    }
    return body == 0;
}

// Internal definitions may be nested inside any number of begin forms.
static unsigned scan_internal_defines(unsigned body, unsigned env)
{
    GC_GUARD;
    unsigned names = 0;
    gc_protect(&names);
    collect_internal_defines(body, env, &names);
    return names;
}

// Helper to extend compile-time env with a list of variable names
static unsigned extend_compile_env_with_names(unsigned env, unsigned names)
{
    if (!names)
        return env;

    unsigned frame_vars = 0, frame_vals = 0;
    gc_protect(&frame_vars);
    gc_protect(&frame_vals);
    gc_protect(&env);
    gc_protect(&names);

    unsigned n = names;
    gc_protect(&n);
    for (; n; n = cdr(n)) {
        unsigned var = car(n);
        gc_protect(&var);
        unsigned vc = alloc_cons(var, frame_vars);
        frame_vars = vc;
        unsigned val = alloc_cons(0, frame_vals); // 0 = non-builtin placeholder
        frame_vals = val;
        gc_unprotect(1);
    }
    gc_unprotect(1);

    unsigned frame = alloc_cons(frame_vars, frame_vals);
    gc_protect(&frame);
    unsigned new_env = alloc_cons(frame, env);
    gc_unprotect(5);
    return new_env;
}

// True if a define-syntax appears among a body's forms, spliced begins
// included. Only such a body needs a pending-defines record (see
// pending_defines_marker_id).
static bool body_has_define_syntax(unsigned exprs, unsigned env)
{
    for (unsigned e = exprs; IS_PAIR(e); e = cdr(e)) {
        unsigned form = car(e);
        if (!IS_PAIR(form))
            continue;
        if (IS_KEYWORD(car(form), ctx.kw_define_syntax) &&
            !is_keyword_shadowed(CELL_ID(car(form)), env))
            return true;
        if (IS_KEYWORD(car(form), ctx.kw_begin) &&
            !is_keyword_shadowed(CELL_ID(car(form)), env) &&
            proper_list_silent(cdr(form)) &&
            body_has_define_syntax(cdr(form), env))
            return true;
    }
    return false;
}

// expr, one form of a body, has just been compiled. If it defines names a
// define-syntax earlier in the body was waiting for, the defines have now run
// at this point of the code, so emit the deferred aliases here and drop the
// names from the pending list.
static void emit_pending_define_aliases(compile_ctx *cctx, unsigned pending,
                                        unsigned expr)
{
    GC_GUARD;
    gc_protect(&pending);
    unsigned defined = 0;
    gc_protect(&defined);
    unsigned name = is_internal_define(expr, cctx->env);
    if (name && IS_ATOM(name))
        defined = alloc_cons(name, 0);
    else if (IS_PAIR(expr) && IS_KEYWORD(car(expr), ctx.kw_begin) &&
             !is_keyword_shadowed(CELL_ID(car(expr)), cctx->env))
        defined = scan_internal_defines(cdr(expr), cctx->env);
    if (!defined)
        return;

    unsigned kept_names = 0, kept_aliases = 0;
    gc_protect(&kept_names);
    gc_protect(&kept_aliases);
    unsigned p = car(pending);
    gc_protect(&p);
    for (; p; p = cdr(p)) {
        if (!atom_list_contains_id(defined, CELL_ID(car(p))))
            kept_names = alloc_cons(car(p), kept_names);
    }
    for (p = cdr(pending); p; p = cdr(p)) {
        unsigned entry = car(p); // (var . gensym)
        int64_t var_id = CELL_ID(car(entry));
        if (!atom_list_contains_id(defined, var_id)) {
            kept_aliases = alloc_cons(entry, kept_aliases);
            continue;
        }
        int64_t gensym_id = CELL_ID(cdr(entry));
        int local_slot = find_stack_local_slot(cctx, var_id);
        if (local_slot >= 0) {
            emit_local_get_slot(cctx, local_slot);
            emit2(cctx, OP_DEFINE, gensym_id);
        } else {
            emit3(cctx, OP_DEFINE_ALIAS, gensym_id, var_id);
        }
    }
    cell_set_car(pending, kept_names);
    cell_set_cdr(pending, kept_aliases);
}

// Set by compile_body for the compile_begin call it makes; see there.
static bool compiling_body_scope = false;

// A <body>: a lambda's, or a let-family form's in the frame it pushes (not
// let-syntax's, which the interpreter runs form by form too). Its
// leading internal definitions are one letrec* scope, so each name is bound
// - unassigned - from the start of the body, and reading it before its
// definition has run is an "Unassigned variable" error, as in the
// interpreter (which binds them all up front, macros.c) and MIT Scheme.
// A begin spliced into a body or at top level is no scope of its own and is
// compiled by compile_begin directly.
static compile_result compile_body(unsigned body, compile_ctx *cctx)
{
    compiling_body_scope = true;
    return compile_begin(body, cctx);
}

// Before a body's first form runs, bind (to CELL_UNASSIGNED) each of its
// internal definitions that could be read before its definition has run;
// the definition itself then assigns the binding in place. OP_DEFINE adds
// the others where they are, as before: nothing can reach them sooner.
//
// A name can be read early only if it occurs in an earlier definition (a
// procedure defined there may be called before this one runs) or in its own
// initializer - except inside a lambda that is the whole initializer, which
// cannot run before the definition gives it out. Occurrence is textual, so
// a nested begin or a define-syntax counts every name in it, and quoted
// data too. Only a non-hygienic macro could hide a reference; reading the
// name then finds no binding yet ("undefined variable"), as it always did.
// Most bodies need nothing here: self-recursive helpers and definitions
// used only by later ones or by the body's expressions cost no code.
static void emit_unassigned_prebinds(compile_ctx *cctx, unsigned exprs,
                                     unsigned names, unsigned env)
{
    // Collections move cells: everything held across an allocation below
    // is rooted, loop cursors included.
    GC_GUARD;
    gc_protect(&exprs);
    gc_protect(&env);
    unsigned later = names; // defined in this form or one after it
    gc_protect(&later);
    unsigned emitted = 0, defined = 0, kept = 0, scanned = 0, n = 0;
    gc_protect(&emitted);
    gc_protect(&defined); // names the current form defines
    gc_protect(&kept);
    gc_protect(&scanned); // the part of the current form that may run early
    gc_protect(&n);
    for (; IS_PAIR(exprs) && later; exprs = cdr(exprs)) {
        unsigned form = car(exprs);
        unsigned own = is_internal_define(form, env);
        int64_t own_id = own ? CELL_ID(own) : -1;
        // The initializer is a lambda: its body runs only once called.
        bool self_safe =
            own && (IS_PAIR(cadr(form)) ||
                    (IS_PAIR(cddr(form)) && !cdr(cddr(form)) &&
                     form_is_core(car(cddr(form)), ctx.kw_lambda, env)));
        scanned = own ? cddr(form) : form;
        defined = 0;
        if (own) {
            defined = alloc_cons(own, 0);
        } else if (IS_PAIR(form) && IS_KEYWORD(car(form), ctx.kw_begin) &&
                   !is_keyword_shadowed(CELL_ID(car(form)), env)) {
            defined = scan_internal_defines(cdr(form), env);
        }
        for (n = later; IS_PAIR(n); n = cdr(n)) {
            int64_t id = CELL_ID(car(n));
            if ((self_safe && id == own_id) ||
                atom_list_contains_id(emitted, id) ||
                !contains_reference(scanned, id))
                continue;
            emitted = alloc_cons(car(n), emitted);
            emit2(cctx, OP_CONST, code_add_const(cctx->code, CELL_UNASSIGNED));
            emit2(cctx, OP_DEFINE, (unsigned)id);
        }
        // The names this form defines are not "later" for the next one.
        if (defined) {
            kept = 0;
            for (n = later; IS_PAIR(n); n = cdr(n)) {
                if (!atom_list_contains_id(defined, CELL_ID(car(n))))
                    kept = alloc_cons(car(n), kept);
            }
            later = kept;
        }
    }
}

// Compile sequence of expressions
static compile_result compile_begin(unsigned exprs, compile_ctx *cctx)
{
    // First, before anything can compile a nested body: is this one?
    bool body_scope = compiling_body_scope;
    compiling_body_scope = false;

    if (!list_length_checked(exprs, NULL, "begin"))
        return emit_syntax_error(cctx, "begin: invalid syntax");

    if (!exprs) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0)); // unspecified
        return const_result(0);
    }

    // Scan for internal defines and extend compile-time environment
    // This ensures defines shadow any macros from enclosing scopes.
    // The extension deliberately persists past this function: spliced
    // define-syntax bindings must stay visible to later forms compiled in
    // this context (see the empty-syntax-binding splice tests).
    GC_GUARD;
    gc_protect(&exprs);
    unsigned scan_env = cctx->env;
    gc_protect(&scan_env);
    unsigned internal_defs = scan_internal_defines(exprs, cctx->env);
    gc_protect(&internal_defs);

    if (internal_defs) {
        cctx->env = extend_compile_env_with_names(cctx->env, internal_defs);
        // At top level these placeholders stand for globals the defines
        // will create (a file's consecutive top-level forms compile as one
        // such begin). Mark the frame so macro hygiene treats a name bound
        // only here as the global it becomes, not as a use-site local: a
        // helper defined after the macro that calls it was renamed to an
        // unbound gensym, and a literal defined after its macro failed to
        // match. See macros_toplevel_defines_marker_id.
        if (!cctx->parent && cctx->env_depth == 0) {
            // The id first: making it the first time allocates, which
            // would collect the marker cell from under us.
            int64_t marker_id = macros_toplevel_defines_marker_id();
            unsigned marker = alloc();
            CELL_TYPE(marker) = BT_ATOM;
            CELL_ID(marker) = marker_id;
            defvar(marker, ctx.atom_true, cctx->env);
        }
    }
    unsigned syntax_frame = alloc_cons(0, 0);
    gc_protect(&syntax_frame);
    cctx->env = alloc_cons(syntax_frame, cctx->env);

    // A define-syntax here may refer to defines after it; record their names
    // in this body's syntax frame (see pending_defines_marker_id). A begin
    // spliced into a body that has a record adds its names to that one.
    unsigned pending = pending_defines_record(cctx);
    gc_protect(&pending);
    if (pending) {
        unsigned n = internal_defs;
        gc_protect(&n);
        for (; n; n = cdr(n)) {
            unsigned names = alloc_cons(car(n), car(pending));
            cell_set_car(pending, names);
        }
        gc_unprotect(1);
    } else if (internal_defs && body_has_define_syntax(exprs, cctx->env)) {
        pending = alloc_cons(internal_defs, 0);
        unsigned owner = pending_defines_owner(cctx);
        gc_protect(&owner);
        unsigned binding = alloc_cons(owner, pending);
        gc_protect(&binding);
        int64_t marker_id = pending_defines_marker_id(); // may allocate
        unsigned marker = alloc();
        CELL_TYPE(marker) = BT_ATOM;
        CELL_ID(marker) = marker_id;
        defvar(marker, binding, cctx->env);
        gc_unprotect(2);
    }

    if (body_scope && internal_defs)
        emit_unassigned_prebinds(cctx, exprs, internal_defs, scan_env);

    bool tail = cctx->tail_position;
    compile_result result = dynamic_result();
    bool had_prior_expr = false;

    while (exprs) {
        bool is_last = !cdr(exprs);
        cctx->tail_position = tail && is_last;

        result = compile_expr_internal(car(exprs), cctx);

        if (!is_last) {
            emit(cctx, OP_POP); // Discard non-final values
            had_prior_expr = true;
        }
        if (pending) {
            if (cdr(pending))
                had_prior_expr = true; // code may follow the value; no folding
            emit_pending_define_aliases(cctx, pending, car(exprs));
        }

        exprs = cdr(exprs);
    }

    // A multi-expression begin may contain effects before the final value.
    // Reporting the final value as a constant lets outer folds erase those
    // effects by rewinding emitted bytecode.
    return had_prior_expr ? dynamic_result() : result;
}

// Helper to extend compile-time environment with variable bindings
// This prevents primitive inlining for shadowed variables
static unsigned extend_compile_env(unsigned env, unsigned bindings)
{
    // Build frame with variables -> 0 (non-builtin placeholder)
    unsigned frame_vars = 0, frame_vals = 0;
    gc_protect(&frame_vars);
    gc_protect(&frame_vals);
    gc_protect(&env);
    gc_protect(&bindings);

    unsigned b = bindings;
    gc_protect(&b);
    for (; b; b = cdr(b)) {
        unsigned var = car(car(b));
        gc_protect(&var);
        unsigned vc = alloc_cons(var, frame_vars);
        frame_vars = vc;
        unsigned val = alloc_cons(0, frame_vals); // 0 = non-builtin
        frame_vals = val;
        gc_unprotect(1);
    }
    gc_unprotect(1);

    unsigned frame = alloc_cons(frame_vars, frame_vals);
    gc_protect(&frame);
    unsigned new_env = alloc_cons(frame, env);
    gc_unprotect(5);
    return new_env;
}

static compile_result compile_named_let(unsigned expr, compile_ctx *cctx)
{
    if (!syntax_arity_checked(expr, 3, UINT_MAX, "let"))
        return emit_syntax_error(cctx, "let: invalid syntax");

    unsigned name = cadr(expr);
    unsigned bindings = caddr(expr);
    unsigned body = cdddr(expr);
    if (!identifier_valid(name) || !binding_list_valid(bindings, "let") ||
        !list_length_checked(body, NULL, "let"))
        return emit_syntax_error(cctx, "let: invalid named let syntax");

    GC_GUARD;
    gc_protect(&name);
    gc_protect(&bindings);
    gc_protect(&body);

    unsigned params = 0, params_tail = 0;
    unsigned args = 0, args_tail = 0;
    gc_protect(&params);
    gc_protect(&params_tail);
    gc_protect(&args);
    gc_protect(&args_tail);

    unsigned b = bindings;
    gc_protect(&b);
    for (; b; b = cdr(b)) {
        unsigned binding = car(b);
        gc_protect(&binding);
        list_append(&params, &params_tail, car(binding));
        list_append(&args, &args_tail, cadr(binding));
        gc_unprotect(1);
    }
    gc_unprotect(1);

    unsigned argc = list_length(args);
    bool tail = cctx->tail_position;
    cctx->tail_position = false;

    unsigned a = args;
    gc_protect(&a);
    while (a) {
        compile_expr_internal(car(a), cctx);
        a = cdr(a);
    }
    gc_unprotect(1);

    emit_pushenv(cctx);
    // Bound but unassigned until the SET below: the lambda is its init.
    emit2(cctx, OP_CONST, code_add_const(cctx->code, CELL_UNASSIGNED));
    emit2(cctx, OP_DEFINE, CELL_ID(name));

    unsigned saved_env = cctx->env;
    gc_protect(&saved_env);

    unsigned value_list = 0, binding = 0, binding_list = 0;
    gc_protect(&value_list);
    gc_protect(&binding);
    gc_protect(&binding_list);
    value_list = alloc_cons(0, 0);
    binding = alloc_cons(name, value_list);
    binding_list = alloc_cons(binding, 0);
    cctx->env = extend_compile_env(cctx->env, binding_list);

    unsigned lambda_tail = 0, lambda_expr = 0;
    gc_protect(&lambda_tail);
    gc_protect(&lambda_expr);
    lambda_tail = alloc_cons(params, body);
    lambda_expr = alloc_cons(atom_from_string("lambda"), lambda_tail);
    procedure_name_expr = lambda_expr;
    procedure_name_id = CELL_ID(name);
    compile_lambda(lambda_expr, cctx);
    procedure_name_expr = 0;
    emit2(cctx, OP_SET, CELL_ID(name));
    emit2(cctx, OP_CALL, argc);

    cctx->env = saved_env;
    cctx->tail_position = tail;
    emit_popenv(cctx);

    return dynamic_result();
}

// (let ((var val) ...) body...)
static compile_result compile_let(unsigned expr, compile_ctx *cctx)
{
    if (!syntax_arity_checked(expr, 2, UINT_MAX, "let"))
        return emit_syntax_error(cctx, "let: invalid syntax");

    unsigned bindings = cadr(expr);
    if (IS_ATOM(bindings))
        return compile_named_let(expr, cctx);

    unsigned body = cddr(expr);
    if (!binding_list_valid(bindings, "let") ||
        !list_length_checked(body, NULL, "let"))
        return emit_syntax_error(cctx, "let: invalid syntax");

    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&body);

    // Count bindings
    unsigned count = 0;
    FORLIST(b, bindings) { count++; }

    bool tail = cctx->tail_position;

    // Compile all values first (in current environment)
    cctx->tail_position = false;
    unsigned bind_cursor = bindings;
    gc_protect(&bind_cursor);
    while (bind_cursor) {
        compile_expr_internal(cadr(car(bind_cursor)), cctx);
        bind_cursor = cdr(bind_cursor);
    }
    gc_unprotect(1);

    // Push new environment frame
    emit_pushenv(cctx);

    // Dynamically allocate variable array
    unsigned *vars = NULL;
    if (count > 0) {
        vars = checked_malloc_array(count, sizeof(unsigned));
        if (!vars) {
            show_error("let: out of memory for bindings");
            emit(cctx, OP_HALT);
            return dynamic_result();
        }
    }

    // Collect variables
    unsigned i = 0;
    FORLIST(b, bindings) { vars[i++] = CELL_ID(car(car(b))); }

    // Define in reverse (pop from stack)
    for (int j = (int)count - 1; j >= 0; j--) {
        emit2(cctx, OP_DEFINE, vars[j]);
    }

    // Free the variable array
    free(vars);

    // Extend compile-time environment for body compilation
    unsigned saved_env = cctx->env;
    gc_protect(&saved_env);
    cctx->env = extend_compile_env(cctx->env, bindings);

    // Track lambda bindings for escape analysis inlining
    // Save and extend known_lambdas for bindings of form (var (lambda ...))
    // Only track non-self-referential lambdas (self-ref needs letrec semantics)
    unsigned saved_known = cctx->known_lambdas;
    gc_protect(&saved_known);
    unsigned b = bindings;
    gc_protect(&b);
    for (; b; b = cdr(b)) {
        unsigned binding = car(b);
        unsigned var = car(binding);
        unsigned val_expr = cadr(binding);

        // Check if value is a lambda expression
        if (form_is_core(val_expr, ctx.kw_lambda, cctx->env)) {
            // Only inline if the lambda doesn't reference the bound variable
            // (self-referential lambdas need letrec semantics to work)
            unsigned lambda_body = cddr(val_expr);
            if (!contains_reference(lambda_body, CELL_ID(var)))
                register_known_lambda(cctx, var, val_expr);
        }
    }
    gc_unprotect(1);

    // Compile body in the same tail context as the let expression.
    cctx->tail_position = tail;
    compile_result result = compile_body(body, cctx);

    // Restore compile-time environment and known_lambdas
    cctx->env = saved_env;
    cctx->known_lambdas = saved_known;

    // Pop environment frame
    emit_popenv(cctx);

    // let introduces bindings, so result is dynamic even if body is constant
    (void)result;
    return dynamic_result();
}

// (let* ((var val) ...) body...)
static compile_result compile_letstar(unsigned expr, compile_ctx *cctx)
{
    if (!syntax_arity_checked(expr, 2, UINT_MAX, "let*"))
        return emit_syntax_error(cctx, "let*: invalid syntax");

    unsigned bindings = cadr(expr);
    unsigned body = cddr(expr);
    if (!binding_list_valid(bindings, "let*") ||
        !list_length_checked(body, NULL, "let*"))
        return emit_syntax_error(cctx, "let*: invalid syntax");

    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&body);

    // Push new environment frame
    emit_pushenv(cctx);

    // Save original compile-time environment
    unsigned saved_env = cctx->env;
    gc_protect(&saved_env);

    bool tail = cctx->tail_position;

    // Compile and define each binding sequentially
    // Each binding extends the compile-time environment for subsequent bindings
    cctx->tail_position = false;
    unsigned b = bindings;
    gc_protect(&b);
    for (; b; b = cdr(b)) {
        unsigned binding = car(b);
        gc_protect(&binding);
        compile_expr_internal(cadr(binding), cctx);
        emit2(cctx, OP_DEFINE, CELL_ID(car(binding)));

        // Extend compile-time env with this binding for next iteration
        unsigned single_binding = alloc_cons(binding, 0);
        gc_protect(&single_binding);
        cctx->env = extend_compile_env(cctx->env, single_binding);
        gc_unprotect(2);
    }
    gc_unprotect(1);

    // Compile body in the same tail context as the let* expression.
    cctx->tail_position = tail;
    compile_body(body, cctx);

    // Restore compile-time environment
    cctx->env = saved_env;

    // Pop environment frame
    emit_popenv(cctx);

    return dynamic_result();
}

// (letrec ((var val) ...) body...)
static compile_result compile_letrec(unsigned expr, compile_ctx *cctx)
{
    if (!syntax_arity_checked(expr, 2, UINT_MAX, "letrec"))
        return emit_syntax_error(cctx, "letrec: invalid syntax");

    unsigned bindings = cadr(expr);
    unsigned body = cddr(expr);
    if (!binding_list_valid(bindings, "letrec") ||
        !list_length_checked(body, NULL, "letrec"))
        return emit_syntax_error(cctx, "letrec: invalid syntax");

    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&body);

    // Count bindings
    unsigned binding_count = 0;
    FORLIST(b, bindings) { binding_count++; }

    // Push new environment frame
    emit_pushenv(cctx);

    // First pass: bind all variables, unassigned until their inits run, so
    // reading one early reports "Unassigned variable" (see CELL_UNASSIGNED)
    unsigned b = bindings;
    gc_protect(&b);
    for (; b; b = cdr(b)) {
        unsigned binding = car(b);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, CELL_UNASSIGNED));
        emit2(cctx, OP_DEFINE, CELL_ID(car(binding)));
    }

    // Mark this frame as letrec for continuation save/restore
    emit2(cctx, OP_LETREC_MARK, binding_count);

    // Extend compile-time environment with all bindings
    // (letrec bindings are mutually recursive, all visible during value compilation)
    unsigned saved_env = cctx->env;
    gc_protect(&saved_env);
    cctx->env = extend_compile_env(cctx->env, bindings);

    // Save tail position from enclosing context
    bool tail = cctx->tail_position;

    // Detect loop pattern: single binding of form (loop (lambda (params) body))
    // Pass loop info to the lambda compiler so recursive tail calls become SET+JUMP
    int64_t loop_var = -1;
    unsigned loop_params = 0;
    unsigned loop_arity = 0;
    if (binding_count == 1) {
        unsigned binding = car(car(bindings));
        unsigned val_expr = cadr(car(bindings));
        if (IS_ATOM(binding) &&
            form_is_core(val_expr, ctx.kw_lambda, cctx->env) &&
            IS_PAIR(cdr(val_expr))) {
            unsigned lparams = cadr(val_expr);
            unsigned lbody = cddr(val_expr);
            // The SET+JUMP transform reuses one mutable frame across
            // iterations, so it is only safe when the lambda has no rest
            // parameter (a matching-arity call would leave it stale) and no
            // parameter is captured by an inner closure (each iteration must
            // see a fresh binding).
            bool safe = true;
            unsigned count = 0;
            for (unsigned p = lparams; p; p = IS_PAIR(p) ? cdr(p) : 0) {
                if (!IS_PAIR(p)) {
                    safe = false; // rest parameter
                    break;
                }
                if (!IS_ATOM(car(p))) {
                    safe = false;
                    break;
                }
                count++;
            }
            if (safe) {
                for (unsigned p = lparams; p && IS_PAIR(p); p = cdr(p)) {
                    if (captured_by_inner_lambda(lbody, CELL_ID(car(p)),
                                                 cctx->env)) {
                        safe = false;
                        break;
                    }
                }
            }
            if (safe) {
                loop_var = CELL_ID(binding);
                loop_params = lparams;
                loop_arity = count;
            }
        }
    }

    // Second pass: compile values and set variables
    cctx->tail_position = false;
    // Temporarily set loop info so compile_lambda can pick it up
    int64_t saved_loop_var = cctx->loop_var_id;
    unsigned saved_loop_params = cctx->loop_params;
    unsigned saved_loop_arity = cctx->loop_arity;
    gc_protect(&saved_loop_params);
    cctx->loop_var_id = loop_var;
    cctx->loop_params = loop_params;
    gc_protect(&cctx->loop_params);
    cctx->loop_arity = loop_arity;
    cctx->loop_pending = loop_var >= 0;
    b = bindings;
    for (; b; b = cdr(b)) {
        unsigned binding = car(b);
        gc_protect(&binding);
        compile_named_value(cadr(binding), car(binding), cctx);
        emit2(cctx, OP_SET, CELL_ID(car(binding)));
        emit(cctx, OP_POP); // Discard set! result
        gc_unprotect(1);
    }
    gc_unprotect(1);
    cctx->loop_var_id = saved_loop_var;
    cctx->loop_params = saved_loop_params;
    cctx->loop_arity = saved_loop_arity;
    cctx->loop_pending = false;

    // End letrec initialization - continuations after this won't restore values
    emit(cctx, OP_LETREC_DONE);

    // Compile body with correct tail position from enclosing context
    cctx->tail_position = tail;
    compile_body(body, cctx);

    // Restore compile-time environment
    cctx->env = saved_env;

    // Pop environment frame
    emit_popenv(cctx);

    return dynamic_result();
}

// (and expr ...)
static compile_result compile_and(unsigned expr, compile_ctx *cctx)
{
    unsigned args = cdr(expr);
    if (!list_length_checked(args, NULL, "and"))
        return emit_syntax_error(cctx, "and: invalid syntax");

    GC_GUARD;
    gc_protect(&args);

    if (!args) {
        // (and) => #t
        emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_true));
        return const_result(ctx.atom_true);
    }

    // Count expressions
    unsigned expr_count = 0;
    FORLIST(a, args) { expr_count++; }

    bool tail = cctx->tail_position;
    unsigned saved_pos = cctx->code->code_len;

    // Dynamically allocate jump array
    unsigned *false_jumps = checked_malloc_array(expr_count, sizeof(unsigned));
    if (!false_jumps) {
        show_error("and: out of memory");
        emit(cctx, OP_HALT);
        return dynamic_result();
    }
    unsigned jump_count = 0;
    unsigned merge_env = 0;
    gc_protect(&merge_env);
    bool has_short_circuit_path = false;

    while (args) {
        bool is_last = !cdr(args);
        cctx->tail_position = tail && is_last;

        unsigned expr_saved_pos = cctx->code->code_len;
        compile_result result = compile_expr_internal(car(args), cctx);

        if (result.is_const) {
            if (IS_FALSE(result.value)) {
                // Constant false short-circuits the rest. If no prior runtime
                // tests were emitted, the whole form is constant; otherwise
                // keep the already-emitted code so its effects still happen.
                if (jump_count == 0) {
                    code_truncate(cctx->code, saved_pos, cctx->code->children_len);
                    emit2(cctx, OP_CONST,
                          code_add_const(cctx->code, ctx.atom_false));
                    free(false_jumps);
                    return const_result(ctx.atom_false);
                }
                break;
            }
            // Constant truthy - can skip this expression if not last
            if (!is_last) {
                // Rewind bytecode for this constant true, continue with rest
                code_truncate(cctx->code, expr_saved_pos, cctx->code->children_len);
                args = cdr(args);
                continue;
            }
            // Last expression is constant true - return it
        }

        if (!is_last) {
            // Duplicate to test and preserve value
            emit(cctx, OP_DUP);
            false_jumps[jump_count++] = emit_jump(cctx, OP_JUMPIFNOT);
            if (!has_short_circuit_path) {
                merge_env = cctx->env;
                has_short_circuit_path = true;
            }
            emit(cctx, OP_POP); // Discard if continuing
        }

        args = cdr(args);
    }

    // Patch all false jumps to here
    for (unsigned i = 0; i < jump_count; i++) {
        patch_jump(cctx, false_jumps[i]);
    }

    free(false_jumps);
    if (has_short_circuit_path)
        cctx->env = merge_env;
    return dynamic_result();
}

// (or expr ...)
static compile_result compile_or(unsigned expr, compile_ctx *cctx)
{
    unsigned args = cdr(expr);
    if (!list_length_checked(args, NULL, "or"))
        return emit_syntax_error(cctx, "or: invalid syntax");

    GC_GUARD;
    gc_protect(&args);

    if (!args) {
        // (or) => #f
        emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_false));
        return const_result(ctx.atom_false);
    }

    // Count expressions
    unsigned expr_count = 0;
    FORLIST(a, args) { expr_count++; }

    bool tail = cctx->tail_position;
    unsigned saved_pos = cctx->code->code_len;

    // Dynamically allocate jump array
    unsigned *true_jumps = checked_malloc_array(expr_count, sizeof(unsigned));
    if (!true_jumps) {
        show_error("or: out of memory");
        emit(cctx, OP_HALT);
        return dynamic_result();
    }
    unsigned jump_count = 0;
    unsigned merge_env = 0;
    gc_protect(&merge_env);
    bool has_short_circuit_path = false;

    while (args) {
        bool is_last = !cdr(args);
        cctx->tail_position = tail && is_last;

        unsigned expr_saved_pos = cctx->code->code_len;
        compile_result result = compile_expr_internal(car(args), cctx);

        if (result.is_const) {
            if (IS_TRUTHY(result.value)) {
                // Constant truthy short-circuits the rest. If prior runtime
                // tests exist, preserve them because they may have effects or
                // produce the returned truthy value.
                if (jump_count == 0) {
                    code_truncate(cctx->code, saved_pos, cctx->code->children_len);
                    emit2(cctx, OP_CONST,
                          code_add_const(cctx->code, result.value));
                    free(true_jumps);
                    return const_result(result.value);
                }
                break;
            }
            // Constant false - can skip this expression if not last
            if (!is_last) {
                // Rewind bytecode for this constant false, continue with rest
                code_truncate(cctx->code, expr_saved_pos, cctx->code->children_len);
                args = cdr(args);
                continue;
            }
            // Last expression is constant false - return it
        }

        if (!is_last) {
            // Duplicate to test and preserve value
            emit(cctx, OP_DUP);
            true_jumps[jump_count++] = emit_jump(cctx, OP_JUMPIF);
            if (!has_short_circuit_path) {
                merge_env = cctx->env;
                has_short_circuit_path = true;
            }
            emit(cctx, OP_POP); // Discard if continuing
        }

        args = cdr(args);
    }

    // Patch all true jumps to here
    for (unsigned i = 0; i < jump_count; i++) {
        patch_jump(cctx, true_jumps[i]);
    }

    free(true_jumps);
    if (has_short_circuit_path)
        cctx->env = merge_env;
    return dynamic_result();
}

// (cond (test expr ...) ... (else expr ...))
static compile_result compile_cond_expand(unsigned expr, compile_ctx *cctx)
{
    unsigned clauses = cdr(expr);
    if (!list_length_checked(clauses, NULL, "cond-expand"))
        return emit_syntax_error(cctx, "cond-expand: invalid syntax");

    FORLIST(c, clauses) {
        unsigned clause = car(c);
        if (!IS_PAIR(clause))
            return emit_syntax_error(cctx, "cond-expand: invalid syntax");
        unsigned requirement = car(clause);
        unsigned body = cdr(clause);
        if (!list_length_checked(body, NULL, "cond-expand"))
            return emit_syntax_error(cctx, "cond-expand: invalid syntax");
        bool is_else =
            IS_KEYWORD(requirement, ctx.kw_else) &&
            env_find_binding_cell(CELL_ID(requirement), cctx->env) == 0;
        if (is_else || cond_expand_requirement_satisfied(requirement))
            return compile_begin(body, cctx);
    }

    emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
    return const_result(0);
}

static compile_result compile_cond(unsigned expr, compile_ctx *cctx)
{
    unsigned clauses = cdr(expr);
    if (!cond_clauses_valid(clauses, "cond", cctx->env))
        return emit_syntax_error(cctx, "cond: invalid syntax");

    GC_GUARD;
    gc_protect(&clauses);

    if (!clauses) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0)); // unspecified
        return const_result(0);
    }

    // Count clauses
    unsigned clause_count = 0;
    FORLIST(c, clauses) { clause_count++; }

    bool tail = cctx->tail_position;

    // Dynamically allocate end jump array
    unsigned *end_jumps = checked_malloc_array(clause_count, sizeof(unsigned));
    if (!end_jumps) {
        show_error("cond: out of memory");
        emit(cctx, OP_HALT);
        return dynamic_result();
    }
    unsigned end_count = 0;
    bool found_else = false;
    unsigned merge_env = 0;
    gc_protect(&merge_env);
    bool has_conditional_path = false;

    while (clauses) {
        unsigned clause = car(clauses);
        gc_protect(&clause);
        unsigned test = car(clause);
        unsigned conseq = cdr(clause);
        gc_protect(&test);
        gc_protect(&conseq);

        // Check for else clause
        if (IS_KEYWORD(test, ctx.kw_else) &&
            env_find_binding_cell(CELL_ID(test), cctx->env) == 0) {
            cctx->tail_position = tail;
            if (conseq) {
                compile_begin(conseq, cctx);
            } else {
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, ctx.atom_true));
            }
            found_else = true;
            gc_unprotect(3);
            break;
        }

        // Compile test
        cctx->tail_position = false;
        unsigned test_saved_pos = cctx->code->code_len;
        compile_result test_result = compile_expr_internal(test, cctx);
        if (test_result.is_const) {
            code_truncate(cctx->code, test_saved_pos, cctx->code->children_len);
            if (IS_FALSE(test_result.value)) {
                clauses = cdr(clauses);
                gc_unprotect(3);
                continue;
            }

            cctx->tail_position = tail;
            bool is_arrow =
                conseq && IS_KEYWORD(car(conseq), ctx.kw_arrow) &&
                env_find_binding_cell(CELL_ID(car(conseq)), cctx->env) == 0;
            if (is_arrow) {
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, test_result.value));
                unsigned receiver_expr = cadr(conseq);
                gc_protect(&receiver_expr);
                cctx->tail_position = false;
                compile_expr_internal(receiver_expr, cctx);
                cctx->tail_position = tail;
                gc_unprotect(1);
                emit2(cctx, trmc_call_opcode(cctx, tail), 1);
            } else if (conseq) {
                compile_begin(conseq, cctx);
            } else {
                emit2(cctx, OP_CONST,
                      code_add_const(cctx->code, test_result.value));
            }
            found_else = true;
            gc_unprotect(3);
            break;
        }
        unsigned false_env = cctx->env;
        gc_protect(&false_env);

        // Check if we need the test value (=> syntax or no consequent)
        bool is_arrow = conseq && IS_KEYWORD(car(conseq), ctx.kw_arrow) &&
                        env_find_binding_cell(CELL_ID(car(conseq)),
                                              cctx->env) == 0;
        bool need_test_value = is_arrow || !conseq;

        if (need_test_value) {
            // Duplicate test result for => receiver or (test) form
            emit(cctx, OP_DUP);
        }

        unsigned next_clause = emit_jump(cctx, OP_JUMPIFNOT);
        if (!has_conditional_path) {
            merge_env = false_env;
            has_conditional_path = true;
        }

        // Compile consequence
        cctx->tail_position = tail;
        if (is_arrow) {
            // (test => receiver) syntax
            // Stack has: test-value (from DUP)
            unsigned receiver_expr = cadr(conseq);
            gc_protect(&receiver_expr);
            cctx->tail_position = false;
            compile_expr_internal(receiver_expr, cctx);
            cctx->tail_position = tail;
            gc_unprotect(1);
            // Stack has: test-value receiver
            // CALL pops fn first, then args - already in correct order
            emit2(cctx, trmc_call_opcode(cctx, tail), 1);
        } else if (conseq) {
            // Normal consequent - compile expressions
            compile_begin(conseq, cctx);
        }
        // else: no conseq, test value already on stack from DUP

        // Always jump to end after true branch
        end_jumps[end_count++] = emit_jump(cctx, OP_JUMP);
        cctx->env = false_env;

        // Patch false branch entry point
        patch_jump(cctx, next_clause);

        // Pop test value on false branch if we DUP'd
        if (need_test_value) {
            emit(cctx, OP_POP);
        }

        clauses = cdr(clauses);
        gc_unprotect(4);
    }

    // If no else clause was found, emit unspecified for fall-through
    if (!found_else) {
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
    }

    // Patch all end jumps
    for (unsigned i = 0; i < end_count; i++) {
        patch_jump(cctx, end_jumps[i]);
    }

    free(end_jumps);
    if (has_conditional_path)
        cctx->env = merge_env;
    return dynamic_result();
}

// (define var expr) or (define (name params...) body...)
static compile_result compile_define(unsigned expr, compile_ctx *cctx)
{
    if (!syntax_arity_checked(expr, 2, UINT_MAX, "define"))
        return emit_syntax_error(cctx, "define: invalid syntax");

    unsigned second = cadr(expr);

    if (IS_PAIR(second)) {
        // (define (name params...) body...) => (define name (lambda ...))
        unsigned name = car(second);
        unsigned params = cdr(second);
        unsigned body = cddr(expr);
        GC_GUARD;
        gc_protect(&name);
        gc_protect(&params);
        gc_protect(&body);
        // lambda_params_valid allocates, so locals must be protected first
        if (!identifier_valid(name) || !body ||
            !list_length_checked(body, NULL, "define") ||
            !lambda_params_valid(params))
            return emit_syntax_error(cctx, "define: invalid syntax");

        // Build lambda expression
        unsigned lambda_atom = atom_from_string("lambda");
        gc_protect(&lambda_atom);
        unsigned lambda_args = alloc_cons(params, body);
        gc_protect(&lambda_args);
        unsigned lambda_expr = alloc_cons(lambda_atom, lambda_args);
        gc_protect(&lambda_expr);

        cctx->tail_position = false;
        compile_named_value(lambda_expr, name, cctx);
        emit2(cctx, OP_DEFINE, CELL_ID(name));

        // Return the name
        emit2(cctx, OP_CONST, code_add_const(cctx->code, name));
    } else {
        if (!identifier_valid(second) || cdddr(expr))
            return emit_syntax_error(cctx, "define: invalid syntax");

        // (define var expr)
        GC_GUARD;
        gc_protect(&second);
        cctx->tail_position = false;
        compile_named_value(caddr(expr), second, cctx);
        emit2(cctx, OP_DEFINE, CELL_ID(second));

        // Return the name
        emit2(cctx, OP_CONST, code_add_const(cctx->code, second));
    }

    return dynamic_result();
}

// (set! var expr)
static compile_result compile_set(unsigned expr, compile_ctx *cctx)
{
    if (!syntax_arity_checked(expr, 2, 2, "set!"))
        return emit_syntax_error(cctx, "set!: invalid syntax");

    unsigned var = cadr(expr);
    unsigned val_expr = caddr(expr);
    if (!identifier_valid(var))
        return emit_syntax_error(cctx, "set!: expected variable");

    GC_GUARD;
    gc_protect(&var);
    gc_protect(&val_expr);

    cctx->tail_position = false;
    compile_expr_internal(val_expr, cctx);

    // Use LOCAL_SET if variable is a stack local (and not shadowed)
    int64_t var_id = CELL_ID(var);
    int binding = -1;
    int local_slot = resolve_stack_local(cctx, var_id, &binding);
    if (local_slot >= 0) {
        // The source scan (assigned_in_body) keeps assigned variables out
        // of slots, since a re-entered continuation would restore the slot
        // to its value at capture time instead of seeing the assignment.
        // Reaching one here means a macro's expansion assigned it: the code
        // emitted is a placeholder, and the body is compiled again with the
        // variable in an environment frame.
        if (binding >= 0)
            cctx->let_bindings[binding].assigned = true;
        else
            cctx->locals_assigned = true;
        emit2(cctx, OP_LOCAL_SET, (unsigned)local_slot);
        return dynamic_result();
    }
    emit2(cctx, OP_SET, var_id);

    return dynamic_result();
}

// Evaluate a recursive call exactly like a general call (arguments, then
// callee). Let the VM decide whether the binding still denotes this closure:
// set! and lexical shadowing can change it even after compilation.
static void emit_self_call_loop(unsigned fn, unsigned args, compile_ctx *cctx)
{
    GC_GUARD;
    gc_protect(&fn);
    gc_protect(&args);
    cctx->tail_position = false;
    unsigned a = args;
    gc_protect(&a);
    for (; a; a = cdr(a))
        compile_expr_internal(car(a), cctx);
    compile_expr_internal(fn, cctx);
    cctx->tail_position = true;

    if (cctx->code->use_locals) {
        emit(cctx, OP_RECURSE);
        code_emit(cctx->code, cctx->loop_start);
        code_emit(cctx->code, cctx->loop_arity);
        code_emit(cctx->code, cctx->env_depth);
    } else {
        // Environment parameters require fresh bindings at each call. The
        // ordinary tail-call path already reuses the control frame.
        emit2(cctx, trmc_call_opcode(cctx, true), cctx->loop_arity);
    }
}

// Keep argument evaluation separate from opcode selection: a mutable binding
// is checked only after all arguments have run, and fallback needs them intact.
static void emit_primitive_operation(compile_ctx *cctx, int64_t prim, unsigned argc,
                                     bool tail)
{
    if (argc == 1) {
        switch (prim) {
        case PCAR: emit(cctx, OP_CAR); return;
        case PCDR: emit(cctx, OP_CDR); return;
        // The deeper compositions go through OP_PRIM: a chain of CAR/CDR
        // would report a failure under car or cdr instead of their own name.
        case PCAAR: emit(cctx, OP_CAAR); return;
        case PCADR: emit(cctx, OP_CADR); return;
        case PCDAR: emit(cctx, OP_CDAR); return;
        case PCDDR: emit(cctx, OP_CDDR); return;
        case PNULLP: emit(cctx, OP_NULLP); return;
        case PCONSP: emit(cctx, OP_PAIRP); return;
        case PNOT: emit(cctx, OP_NOT); return;
        case PLIST: emit(cctx, OP_LIST1); return;
        case PSYMP: emit(cctx, OP_SYMBOLP); return;
        case PNUMP: emit(cctx, OP_NUMBERP); return;
        case PSTRINGP: emit(cctx, OP_STRINGP); return;
        case PVECTORP: emit(cctx, OP_VECTORP); return;
        case PBOOLP: emit(cctx, OP_BOOLEANP); return;
        case PLISTP: emit(cctx, OP_LISTP); return;
        case PINTEGERP: emit(cctx, OP_INTEGERP); return;
        case PLENGTH: emit(cctx, OP_LENGTH); return;
        case PREVERSE: emit(cctx, OP_REVERSE); return;
        case PVECLEN: emit(cctx, OP_VECTORLEN); return;
        case PABS: emit(cctx, OP_ABS); return;
        case PCALLCC: emit(cctx, OP_CALLCC); return;
        default: break;
        }
    }
    if (argc == 2) {
        switch (prim) {
        case PCONS: emit(cctx, OP_CONS); return;
        case PEQ: emit(cctx, OP_EQ); return;
        case PPLUS: emit(cctx, OP_ADD); return;
        case PMINUS: emit(cctx, OP_SUB); return;
        case PTIMES: emit(cctx, OP_MUL); return;
        case PDIV: emit(cctx, OP_DIV); return;
        case PMOD: emit(cctx, OP_MOD); return;
        case PLT: emit(cctx, OP_LT); return;
        case PGT: emit(cctx, OP_GT); return;
        case PLEQ: emit(cctx, OP_LE); return;
        case PGEQ: emit(cctx, OP_GE); return;
        case PEQUAL: emit(cctx, OP_NUMEQ); return;
        case PSETCAR: emit(cctx, OP_SETCAR); return;
        case PSETCDR: emit(cctx, OP_SETCDR); return;
        case PLIST: emit(cctx, OP_LIST2); return;
        case PAPPEND: emit(cctx, OP_APPEND); return;
        case PVECREF: emit(cctx, OP_VECTORREF); return;
        case PRECORDOFP: emit(cctx, OP_RECORDOF); return;
        default: break;
        }
    }
    if (argc == 3) {
        switch (prim) {
        case PLIST: emit(cctx, OP_LIST3); return;
        case PVECSET: emit(cctx, OP_VECTORSET); return;
        default: break;
        }
    }
    if (argc == 4 && prim == PRECORDREF) {
        emit(cctx, OP_RECORDREF);
        return;
    }
    // apply and call-with-values call their procedure argument, which R7RS
    // requires to be a tail call when they are themselves in tail position.
    bool tail_flag = tail && (prim == PAPPLY || prim == PCALLWITHVALUES);
    emit3(cctx, OP_PRIM, prim, tail_flag ? argc | PRIM_TAIL_FLAG : argc);
}

// Discard a known unit/zero operand only after the guard, so a replacement
// procedure still receives the original argument list without reevaluation.
static bool emit_unary_arithmetic(compile_ctx *cctx, int64_t prim,
                                  compile_result *args, unsigned argc)
{
    if (argc != 2)
        return false;
    for (unsigned i = 0; i < 2; i++) {
        if (!args[i].is_const || !IS_NUM(args[i].value))
            continue;
        int64_t value = CELL_ID(args[i].value);
        unsigned op = OP_COUNT;
        if (prim == PPLUS && (value == 1 || value == -1))
            op = value == 1 ? OP_ADD1 : OP_SUB1;
        else if (prim == PMINUS && i == 1 && (value == 1 || value == -1))
            op = value == 1 ? OP_SUB1 : OP_ADD1;
        else if (prim == PEQUAL && value == 0)
            op = OP_ZEROP;
        if (op != OP_COUNT) {
            if (i == 0)
                emit(cctx, OP_SWAP);
            emit(cctx, OP_POP);
            emit(cctx, op);
            return true;
        }
    }
    return false;
}

static compile_result compile_call(unsigned expr, compile_ctx *cctx)
{
    unsigned fn_expr = car(expr);
    unsigned args = cdr(expr);

    GC_GUARD;
    gc_protect(&fn_expr);
    gc_protect(&args);

    if (!list_length_checked(args, NULL, "application")) {
        unsigned msg = make_string_copy("malformed application");
        gc_protect(&msg);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, msg));
        emit3(cctx, OP_PRIM, PERROR, 1);
        return dynamic_result();
    }

    // ========================================================================
    // Tail recursion modulo cons: (cons E (self a1..an)) in tail position
    // ========================================================================
    // Append E to the accumulator and reuse the ordinary self-call loop for
    // the arguments, so the list is built forwards in constant frame space.
    // This has to come before primitive inlining, which would otherwise
    // compile the cons and leave the recursive call under it.
    unsigned trmc_operands = 0, trmc_call = 0;
    int64_t trmc_op = 0;
    // The guarded loop call unwinds pending environment frames only on its
    // fast path, and updates the original parameter slots even when an inner
    // let shadows their names.
    if (cctx->trmc_mode != TRMC_MODE_NONE && cctx->tail_position &&
        trmc_classify(expr, cctx, &trmc_operands, &trmc_call, &trmc_op)) {
        gc_protect(&trmc_operands);
        gc_protect(&trmc_call);
        cctx->tail_position = false;
        // Classification admits only (op E (self ...)). Evaluate E once;
        // there are no leading n-ary operands to combine or traverse here.
        compile_expr_internal(car(trmc_operands), cctx);
        unsigned accumulate = OP_TRMC_PUSH;
        if (cctx->trmc_mode == TRMC_MODE_HOLE)
            accumulate = trmc_op == PCONS ? OP_TRMC_APPEND : OP_TRMC_SPLICE;
        emit(cctx, accumulate);
        if (accumulate == OP_TRMC_PUSH)
            code_emit(cctx->code, code_add_const(cctx->code, fn_expr));
        cctx->tail_position = true;
        emit_self_call_loop(car(trmc_call), cdr(trmc_call), cctx);
        return dynamic_result();
    }

    if (IS_ATOM(fn_expr)) {
        unsigned fn = lookup_silent(CELL_ID(fn_expr), cctx->env);
        if (fn != TOK_ERROR && IS_BUILTIN(fn)) {
            int64_t prim = CELL_ID(fn);
            unsigned argc = list_length(args);
            bool tail = cctx->tail_position;
            bool immutable = cctx->immutable_base &&
                env_binding_is_immutable(CELL_ID(fn_expr), cctx->env);
            cctx->tail_position = false;
            unsigned saved_pos = cctx->code->code_len;
            compile_result results[256] = {0};
            bool all_const = argc <= 256 && is_foldable_primitive(prim);
            unsigned a = args, i = 0;
            gc_protect(&a);
            for (; a; a = cdr(a), i++) {
                compile_result result = compile_expr_internal(car(a), cctx);
                if (i < 256) {
                    results[i] = result;
                    if (result.is_const)
                        gc_protect(&results[i].value);
                }
                if (!result.is_const)
                    all_const = false;
            }

            unsigned guard = 0;
            if (!immutable) {
                guard = cctx->code->code_len;
                emit(cctx, OP_GUARD_PRIMITIVE);
                code_emit(cctx->code, 0); // fallback resumes after the fast path
                code_emit(cctx->code, CELL_ID(fn_expr));
                code_emit(cctx->code, prim);
                code_emit(cctx->code, argc);
                code_emit(cctx->code, tail);
                code_emit(cctx->code, 0xFFFFFFFF); // lookup cache
                code_emit(cctx->code, 0xFFFFFFFF);
            }

            bool folded = false;
            unsigned constant = 0;
            gc_protect(&constant);
            if (all_const) {
                unsigned values = 0;
                gc_protect(&values);
                for (unsigned j = argc; j > 0; j--)
                    values = alloc_cons(results[j - 1].value, values);
                bool suppress = ctx.suppress_error_output;
                ctx.suppress_error_output = true;
                constant = apply_primitive(prim, values);
                ctx.suppress_error_output = suppress;
                ctx.last_error[0] = '\0';
                folded = constant != TOK_ERROR;
                if (folded) {
                    if (immutable)
                        code_truncate(cctx->code, saved_pos, cctx->code->children_len);
                    else
                        for (unsigned j = 0; j < argc; j++)
                            emit(cctx, OP_POP);
                    emit2(cctx, OP_CONST, code_add_const(cctx->code, constant));
                }
                gc_unprotect(1);
            }
            if (!folded && !emit_unary_arithmetic(cctx, prim, results, argc))
                emit_primitive_operation(cctx, prim, argc, tail);
            if (!immutable)
                code_patch(cctx->code, guard + 1, cctx->code->code_len);
            return immutable && folded ? const_result(constant) : dynamic_result();
        }
    }

    // ========================================================================
    // Loop Optimization: recursive tail call to letrec-bound lambda
    // ========================================================================
    // The VM reuses stack locals when the resolved callee still has this
    // activation's code and environment, without allocating a call frame.
    if (IS_ATOM(fn_expr) && cctx->tail_position &&
        cctx->loop_var_id >= 0 &&
        CELL_ID(fn_expr) == cctx->loop_var_id &&
        cctx->env_depth == 0) {
        // Only safe when not inside nested let/begin frames (env_depth == 0)
        // because JUMP 0 would skip their POPENV instructions
        unsigned argc = list_length(args);
        if (argc == cctx->loop_arity && cctx->loop_arity <= 16) {
            emit_self_call_loop(fn_expr, args, cctx);
            return dynamic_result();
        }
    }

    // ========================================================================
    // Escape Analysis Inlining: (f args) where f is a known lambda
    // ========================================================================
    // If f is bound to a lambda in a let, we can inline it at the call site
    if (IS_ATOM(fn_expr) && cctx->known_lambdas) {
        int64_t var_id = CELL_ID(fn_expr);
        unsigned reg_env_len = 0;
        unsigned lambda_expr =
            lookup_known_lambda(var_id, cctx->known_lambdas, &reg_env_len);
        // Inline only while the compile-time environment is unchanged: a
        // scope pushed since registration (nested lambda, let, begin with
        // internal defines) may rebind var, and inlining would then use
        // the wrong binding (non-capture-avoiding substitution).
        if (lambda_expr &&
            compile_env_length(cctx->env) == reg_env_len) {
            GC_GUARD;
            // Found a known lambda - construct ((lambda ...) args) and recurse
            // to use the existing lambda inlining code.
            // Remove this binding from known_lambdas to prevent infinite
            // recursion if the lambda calls itself (recursive functions).
            unsigned saved_known = cctx->known_lambdas;
            gc_protect(&saved_known);
            gc_protect(&lambda_expr);
            gc_protect(&args);
            unsigned new_known = 0;
            gc_protect(&new_known);
            unsigned entry = cctx->known_lambdas;
            gc_protect(&entry);
            while (entry) {
                unsigned pair = car(entry);
                gc_protect(&pair);
                if (!IS_PAIR(pair) || !IS_ATOM(car(pair)) ||
                    CELL_ID(car(pair)) != var_id) {
                    new_known = alloc_cons(pair, new_known);
                }
                gc_unprotect(1);
                entry = cdr(entry);
            }
            gc_unprotect(1);
            cctx->known_lambdas = new_known;

            unsigned inlined_call = alloc_cons(lambda_expr, args);
            gc_protect(&inlined_call);
            compile_result result = compile_call(inlined_call, cctx);

            cctx->known_lambdas = saved_known;
            return result;
        }
    }

    // ========================================================================
    // Lambda Inlining: ((lambda (params) body) args) -> inline as let
    // ========================================================================
    // This avoids closure allocation and function call overhead
    if (form_is_core(fn_expr, ctx.kw_lambda, cctx->env)) {

        if (!syntax_arity_checked(fn_expr, 2, UINT_MAX, "lambda"))
            return emit_syntax_error(cctx, "lambda: invalid syntax");

        // Released when this block ends: a call it cannot inline falls
        // through to the general case, which allocates.
        GC_GUARD;
        unsigned lambda_params = cadr(fn_expr);
        unsigned lambda_body = cddr(fn_expr);
        gc_protect(&lambda_params);
        gc_protect(&lambda_body);
        // lambda_params_valid allocates, so locals must be protected first
        if (!lambda_params_valid(lambda_params) ||
            !list_length_checked(lambda_body, NULL, "lambda"))
            return emit_syntax_error(cctx, "lambda: invalid syntax");

        // Count params and check for rest parameter
        // Handle two cases:
        // 1. (lambda args body) - params is just an atom, all args go to rest
        // 2. (lambda (x y . rest) body) - some fixed params, then rest
        unsigned param_count = 0;
        bool has_rest = false;
        unsigned rest_param = 0;

        if (IS_ATOM(lambda_params)) {
            // Case 1: (lambda args body) - entire params is the rest param
            has_rest = true;
            rest_param = lambda_params;
            param_count = 0;
        } else {
            // Case 2: (lambda (x y ...) body) or (lambda (x y . rest) body)
            for (unsigned p = lambda_params; p; p = IS_PAIR(p) ? cdr(p) : 0) {
                if (IS_ATOM(p)) {
                    // Found rest param at end of dotted list
                    has_rest = true;
                    rest_param = p;
                    break;
                }
                param_count++;
            }
        }

        // Count arguments
        unsigned arg_count = list_length(args);

        // Inline if: exact match, OR rest param with enough args
        bool can_inline = (!has_rest && param_count == arg_count) ||
                          (has_rest && arg_count >= param_count);

        if (can_inline) {
            // Save tail position before compiling arguments
            bool tail = cctx->tail_position;
            cctx->tail_position = false;

            // Compile fixed arguments first
            unsigned args_remaining = args;
            gc_protect(&args_remaining);
            for (unsigned i = 0; i < param_count; i++) {
                compile_expr_internal(car(args_remaining), cctx);
                args_remaining = cdr(args_remaining);
            }

            // For rest parameters, build a list from remaining args
            if (has_rest) {
                unsigned rest_count = arg_count - param_count;
                if (rest_count == 0) {
                    // Empty rest list
                    emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
                } else {
                    // Compile rest args and build list
                    // Stack after compiling: [arg1 arg2 arg3]
                    // We want (arg1 arg2 arg3) = (cons arg1 (cons arg2 (cons arg3 '())))
                    unsigned a = args_remaining;
                    gc_protect(&a);
                    while (a) {
                        compile_expr_internal(car(a), cctx);
                        a = cdr(a);
                    }
                    gc_unprotect(1);
                    // Push nil: [arg1 arg2 arg3 nil]
                    emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
                    // CONS takes car from second-top, cdr from top
                    // After CONS: [arg1 arg2 (arg3 . nil)] = [arg1 arg2 (arg3)]
                    // After CONS: [arg1 (arg2 . (arg3))] = [arg1 (arg2 arg3)]
                    // After CONS: [(arg1 . (arg2 arg3))] = [(arg1 arg2 arg3)]
                    for (unsigned i = 0; i < rest_count; i++) {
                        emit(cctx, OP_CONS);
                    }
                }
            }
            gc_unprotect(1);

            // The bindings, fixed parameters first and the rest parameter
            // last, and for each the stack slot holding it (-1: the frame).
            unsigned var_count = param_count + (has_rest ? 1 : 0);
            int64_t *param_ids = NULL;
            int *param_slots = NULL;
            if (var_count > 0) {
                param_ids = checked_malloc_array(var_count, sizeof *param_ids);
                param_slots =
                    checked_malloc_array(var_count, sizeof *param_slots);
                if (!param_ids || !param_slots) {
                    free(param_ids);
                    free(param_slots);
                    show_error("lambda: out of memory for parameters");
                    emit(cctx, OP_HALT);
                    return dynamic_result();
                }
            }

            // Collect parameter names (only if params is a list)
            unsigned i = 0;
            if (param_count > 0 && IS_PAIR(lambda_params)) {
                for (unsigned p = lambda_params; p && i < param_count;
                     p = cdr(p)) {
                    if (IS_ATOM(p))
                        break; // Stop at rest param
                    param_ids[i++] = CELL_ID(car(p));
                }
            }
            if (i != param_count) {
                free(param_ids);
                free(param_slots);
                return emit_syntax_error(cctx, "lambda: invalid syntax");
            }
            if (has_rest)
                param_ids[param_count] = CELL_ID(rest_param);

            GC_GUARD;
            // Extend compile-time environment with params for body compilation
            unsigned saved_env = cctx->env;
            gc_protect(&saved_env);
            {
                GC_GUARD;
                gc_protect(&lambda_params);
                gc_protect(&lambda_body);
                gc_protect(&rest_param);
                unsigned frame_vars = 0, frame_vals = 0;
                gc_protect(&frame_vars);
                gc_protect(&frame_vals);

                // Add fixed params (only if lambda_params is a list)
                if (IS_PAIR(lambda_params)) {
                    unsigned p = lambda_params;
                    gc_protect(&p);
                    for (; p; p = IS_PAIR(p) ? cdr(p) : 0) {
                        if (IS_ATOM(p))
                            break;
                        unsigned var = car(p);
                        gc_protect(&var);
                        unsigned vc = alloc_cons(var, frame_vars);
                        frame_vars = vc;
                        unsigned val = alloc_cons(0, frame_vals);
                        frame_vals = val;
                        gc_unprotect(1);
                    }
                    gc_unprotect(1);
                }

                // Add rest param if present
                if (has_rest) {
                    unsigned vc = alloc_cons(rest_param, frame_vars);
                    frame_vars = vc;
                    unsigned val = alloc_cons(0, frame_vals);
                    frame_vals = val;
                }

                unsigned frame = alloc_cons(frame_vars, frame_vals);
                gc_protect(&frame);
                cctx->env = alloc_cons(frame, cctx->env);
            }

            // Let slots. In a stack-local body a binding that no closure
            // captures and nothing assigns can live in a stack slot (see the
            // prediction walks near the top of this file), so a let - the
            // stdlib's let, let* and named let all arrive here - costs no
            // environment frame, no defvar and no lookups. The scope still
            // needs a frame if its body defines anything, or if any binding
            // must stay in one; a scope with none pushes nothing, which also
            // keeps the loop optimization's self calls at env_depth 0.
            unsigned scope = 0;
            bool frameless = false;
            unsigned saved_slot_next = cctx->let_slot_next;
            unsigned saved_binding_count = cctx->let_binding_count;
            if (cctx->let_slots) {
                scope = cctx->let_scope_counter++;
                frameless = !let_scope_banned(cctx, scope) &&
                            !body_defines_internally(lambda_body, cctx->env) &&
                            !body_has_define_syntax(lambda_body, cctx->env);
            }
            unsigned frame_pos = frameless ? compile_env_length(cctx->env) : 0;
            bool needs_frame = !frameless;
            for (unsigned k = 0; k < var_count; k++) {
                param_slots[k] = -1;
                if (frameless && cctx->let_slot_next < LET_SLOT_LIMIT &&
                    !captured_by_inner_lambda(lambda_body, param_ids[k],
                                              cctx->env) &&
                    !assigned_in_body(lambda_body, param_ids[k], cctx->env) &&
                    let_binding_push(cctx, frame_pos, param_ids[k],
                                     cctx->let_slot_next, scope)) {
                    param_slots[k] = (int)cctx->let_slot_next++;
                } else {
                    needs_frame = true;
                }
            }
            if (cctx->let_slot_next > cctx->let_slot_max)
                cctx->let_slot_max = cctx->let_slot_next;

            // Bind in reverse, popping the values just computed: the rest
            // parameter's list is on top.
            if (needs_frame)
                emit_pushenv(cctx);
            for (unsigned k = var_count; k-- > 0;) {
                if (param_slots[k] >= 0)
                    emit2(cctx, OP_LOCAL_SET_VOID, (unsigned)param_slots[k]);
                else
                    emit2(cctx, OP_DEFINE, (unsigned)param_ids[k]);
            }

            // Track lambda arguments for escape analysis inlining
            // When ((lambda (var) body) (lambda ...)) is inlined, we can inline
            // calls to var within body if the lambda doesn't self-reference var
            unsigned saved_known = cctx->known_lambdas;
            gc_protect(&saved_known);
            {
                unsigned a = args;
                gc_protect(&a);
                if (IS_PAIR(lambda_params)) {
                    unsigned p = lambda_params;
                    gc_protect(&p);
                    for (; p && a; p = IS_PAIR(p) ? cdr(p) : 0) {
                        if (IS_ATOM(p))
                            break;
                        unsigned var = car(p);
                        unsigned val_expr = car(a);
                        // Check if arg is a lambda that doesn't self-reference
                        if (form_is_core(val_expr, ctx.kw_lambda,
                                         cctx->env)) {
                            unsigned arg_body = cddr(val_expr);
                            if (!contains_reference(arg_body, CELL_ID(var)))
                                register_known_lambda(cctx, var, val_expr);
                        }
                        a = cdr(a);
                    }
                    gc_unprotect(1);
                }
                gc_unprotect(1);
            }

            // Compile body — IIFE params shadow any same-named stack locals
            int saved_num_locals = cctx->num_locals;
            int64_t saved_local_ids[8];
            if (saved_num_locals > 0)
                memcpy(saved_local_ids, cctx->local_ids,
                       sizeof(int64_t) * saved_num_locals);
            // Remove locals that are shadowed by IIFE params, including a
            // rest parameter. Otherwise a reference to the rest binding can
            // incorrectly compile as LOCAL_GET of an outer parameter.
            if (cctx->num_locals > 0) {
                unsigned p = lambda_params;
                while (p) {
                    unsigned param = IS_PAIR(p) ? car(p) : p;
                    int64_t pid = CELL_ID(param);
                    for (int k = 0; k < cctx->num_locals; k++) {
                        if (cctx->local_ids[k] == pid)
                            cctx->local_ids[k] = -1; // disable this slot
                    }
                    if (!IS_PAIR(p))
                        break;
                    p = cdr(p);
                }
            }
            cctx->tail_position = tail;
            unsigned body_code_start = cctx->code->code_len;
            unsigned body_children_start = cctx->code->children_len;
            compile_body(lambda_body, cctx);
            cctx->num_locals = saved_num_locals;
            if (saved_num_locals > 0)
                memcpy(cctx->local_ids, saved_local_ids,
                       sizeof(int64_t) * saved_num_locals);

            // The decisions above were made from the unexpanded body. Check
            // what it compiled to, as compile_lambda does for parameters: a
            // definition with no frame of this scope's to land in, a closure
            // reaching a slot binding by name, or an assignment to one (a
            // macro's doing, all three) means this scope needs its frame.
            if (frameless) {
                bool unsafe = !needs_frame &&
                              code_defines_outside_own_frames(cctx->code,
                                                              body_code_start);
                for (unsigned k = saved_binding_count;
                     !unsafe && k < cctx->let_binding_count; k++) {
                    struct let_slot_binding *b = &cctx->let_bindings[k];
                    unsafe = b->assigned ||
                             children_reach_symbol(cctx->code,
                                                   body_children_start,
                                                   b->var_id);
                }
                if (unsafe)
                    let_scope_ban(cctx, scope);
            }
            cctx->let_binding_count = saved_binding_count;
            cctx->let_slot_next = saved_slot_next;
            free(param_ids);
            free(param_slots);

            // Restore compile-time environment and known_lambdas
            cctx->env = saved_env;
            cctx->known_lambdas = saved_known;

            // Pop environment frame
            if (needs_frame)
                emit_popenv(cctx);

            return dynamic_result();
        }
    }

    // General case: compile function and arguments
    unsigned argc = 0;
    bool tail = cctx->tail_position;

    // Compile arguments first (left to right)
    cctx->tail_position = false;
    unsigned a = args;
    gc_protect(&a);
    while (a) {
        compile_expr_internal(car(a), cctx);
        argc++;
        a = cdr(a);
    }
    gc_unprotect(1);

    // Compile function expression
    compile_expr_internal(fn_expr, cctx);

    // Emit call
    emit2(cctx, trmc_call_opcode(cctx, tail), argc);

    return dynamic_result();
}

// True when the rest of a template spine is an unquote/unquote-splicing/
// quasiquote tail form, e.g. `(a . ,b) which reads as (a unquote b)
static bool qq_compile_spine_tail_unquote(unsigned l, compile_ctx *cctx)
{
    unsigned h = car(l);
    return (IS_KEYWORD(h, ctx.kw_unquote) &&
            !is_keyword_shadowed(CELL_ID(h), cctx->env)) ||
           (IS_KEYWORD(h, ctx.kw_unquote_splicing) &&
            !is_keyword_shadowed(CELL_ID(h), cctx->env)) ||
           (IS_KEYWORD(h, ctx.kw_quasiquote) &&
            !is_keyword_shadowed(CELL_ID(h), cctx->env));
}

// Check if quasiquote contains unquote or unquote-splicing
static bool qq_has_unquote(unsigned x, compile_ctx *cctx, int depth)
{
    if (IS_VECTOR(x)) {
        unsigned len = vector_len(x);
        for (unsigned i = 0; i < len; i++) {
            if (qq_has_unquote(vector_data_ptr(x)[i], cctx, depth))
                return true;
        }
        return false;
    }

    if (!IS_PAIR(x))
        return false;

    unsigned head = car(x);

    // Found unquote at depth 1
    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(CELL_ID(head), cctx->env) && depth == 1)
        return true;
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(CELL_ID(head), cctx->env) &&
        depth == 1)
        return true;

    // Nested quasiquote increases depth
    if (IS_KEYWORD(head, ctx.kw_quasiquote) &&
        !is_keyword_shadowed(CELL_ID(head), cctx->env))
        return qq_has_unquote(cadr(x), cctx, depth + 1);

    // Unquote decreases depth
    if ((IS_KEYWORD(head, ctx.kw_unquote) &&
         !is_keyword_shadowed(CELL_ID(head), cctx->env)) ||
        (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
         !is_keyword_shadowed(CELL_ID(head), cctx->env)))
        return qq_has_unquote(cadr(x), cctx, depth - 1);

    // Check all elements of list
    unsigned l = x;
    for (; IS_PAIR(l); l = cdr(l)) {
        // (... . ,tail): the rest of the spine is an unquote form
        if (l != x && qq_compile_spine_tail_unquote(l, cctx))
            return qq_has_unquote(l, cctx, depth);
        if (qq_has_unquote(car(l), cctx, depth))
            return true;
    }
    if (l && qq_has_unquote(l, cctx, depth))
        return true;
    return false;
}

static bool qq_syntax_valid(unsigned x, compile_ctx *cctx, int depth,
                            unsigned structural_depth)
{
    if (structural_depth >= 1024)
        return false;
    if (IS_VECTOR(x)) {
        unsigned len = vector_len(x);
        for (unsigned i = 0; i < len; i++) {
            if (!qq_syntax_valid(vector_data_ptr(x)[i], cctx, depth,
                                 structural_depth + 1))
                return false;
        }
        return true;
    }

    if (!IS_PAIR(x))
        return true;
    if (pair_chain_is_circular(x))
        return false;

    unsigned head = car(x);
    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(CELL_ID(head), cctx->env)) {
        if (!syntax_arity_checked(x, 1, 1, "unquote"))
            return false;
        if (depth == 1)
            return true;
        return qq_syntax_valid(cadr(x), cctx, depth - 1,
                               structural_depth + 1);
    }
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(CELL_ID(head), cctx->env)) {
        if (!syntax_arity_checked(x, 1, 1, "unquote-splicing"))
            return false;
        if (depth == 1)
            return true;
        return qq_syntax_valid(cadr(x), cctx, depth - 1,
                               structural_depth + 1);
    }
    if (IS_KEYWORD(head, ctx.kw_quasiquote) &&
        !is_keyword_shadowed(CELL_ID(head), cctx->env)) {
        return syntax_arity_checked(x, 1, 1, "quasiquote") &&
               qq_syntax_valid(cadr(x), cctx, depth + 1,
                               structural_depth + 1);
    }

    unsigned l = x;
    for (; IS_PAIR(l); l = cdr(l)) {
        // (... . ,tail) reads as (... unquote tail): the rest of the spine
        // is an unquote form, not further elements
        if (l != x && qq_compile_spine_tail_unquote(l, cctx))
            return qq_syntax_valid(l, cctx, depth, structural_depth + 1);
        if (!qq_syntax_valid(car(l), cctx, depth, structural_depth + 1))
            return false;
    }
    if (l && !qq_syntax_valid(l, cctx, depth, structural_depth + 1))
        return false;

    return true;
}

static void compile_qq_error(compile_ctx *cctx, const char *message)
{
    unsigned msg = make_string_copy(message);
    gc_protect(&msg);
    emit2(cctx, OP_CONST, code_add_const(cctx->code, msg));
    emit3(cctx, OP_PRIM, PERROR, 1);
    gc_unprotect(1);
}

// Compile quasiquote recursively - generates runtime code
static void compile_qq_rec(unsigned x, compile_ctx *cctx, int depth)
{
    // The compile calls below allocate (macro expansion, env frames), so the
    // template and traversal cursor must stay rooted across them
    GC_GUARD;
    gc_protect(&x);

    if (IS_VECTOR(x)) {
        unsigned len = vector_len(x);
        bool has_splice = false;
        for (unsigned i = 0; i < len && depth == 1; i++) {
            unsigned elem = vector_data_ptr(x)[i];
            if (IS_PAIR(elem) &&
                IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
                !is_keyword_shadowed(CELL_ID(car(elem)), cctx->env))
                has_splice = true;
        }
        if (has_splice) {
            // `#(a ,@b c) is (list->vector `(a ,@b c)) (R7RS 4.2.8)
            unsigned elems = 0;
            gc_protect(&elems);
            for (unsigned i = len; i > 0; i--)
                elems = alloc_cons(vector_data_ptr(x)[i - 1], elems);
            compile_qq_rec(elems, cctx, depth);
            emit3(cctx, OP_PRIM, PLIST2VEC, 1);
            return;
        }
        for (unsigned i = 0; i < len; i++) {
            compile_qq_rec(vector_data_ptr(x)[i], cctx, depth);
        }
        emit3(cctx, OP_PRIM, PVECTOR, len);
        return;
    }

    if (!IS_PAIR(x)) {
        // Non-pair: just quote it. Quoted data never shows a core alias.
        x = macros_strip_core_aliases(x);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, x));
        return;
    }

    unsigned head = car(x);

    // (unquote expr) at depth 1: compile the expression
    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(CELL_ID(head), cctx->env) && depth == 1) {
        compile_expr_internal(cadr(x), cctx);
        return;
    }

    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(CELL_ID(head), cctx->env) &&
        depth == 1) {
        compile_qq_error(cctx, "unquote-splicing not inside list");
        return;
    }

    // (unquote expr) at depth > 1: decrement depth and wrap result
    if (IS_KEYWORD(head, ctx.kw_unquote) &&
        !is_keyword_shadowed(CELL_ID(head), cctx->env) && depth > 1) {
        compile_qq_rec(cadr(x), cctx, depth - 1);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_unquote));
        emit(cctx, OP_SWAP);
        emit(cctx, OP_LIST1);
        emit(cctx, OP_CONS);
        return;
    }

    // (unquote-splicing expr) at depth > 1: decrement depth and wrap result
    if (IS_KEYWORD(head, ctx.kw_unquote_splicing) &&
        !is_keyword_shadowed(CELL_ID(head), cctx->env) &&
        depth > 1) {
        compile_qq_rec(cadr(x), cctx, depth - 1);
        emit2(cctx, OP_CONST,
              code_add_const(cctx->code, ctx.atom_unquote_splicing));
        emit(cctx, OP_SWAP);
        emit(cctx, OP_LIST1);
        emit(cctx, OP_CONS);
        return;
    }

    // Nested quasiquote: keep structure but recurse with depth+1
    if (IS_KEYWORD(head, ctx.kw_quasiquote) &&
        !is_keyword_shadowed(CELL_ID(head), cctx->env)) {
        compile_qq_rec(cadr(x), cctx, depth + 1);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, ctx.atom_quasiquote));
        emit(cctx, OP_SWAP);
        emit(cctx, OP_LIST1);
        emit(cctx, OP_CONS);
        return;
    }

    // Handle list by compiling each element and building with cons
    // Need to handle unquote-splicing specially
    bool has_splice = false;
    for (unsigned l = x; IS_PAIR(l); l = cdr(l)) {
        if (l != x && qq_compile_spine_tail_unquote(l, cctx))
            break; // (... . ,tail): rest of spine is a tail form
        unsigned elem = car(l);
        if (IS_PAIR(elem) && IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
            !is_keyword_shadowed(CELL_ID(car(elem)), cctx->env) &&
            depth == 1) {
            has_splice = true;
            break;
        }
    }

    unsigned cursor = 0;
    gc_protect(&cursor);
    if (has_splice) {
        // Use append-based building for splicing
        // Start with empty list
        emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));

        bool tail_done = false;
        for (cursor = x; IS_PAIR(cursor); cursor = cdr(cursor)) {
            if (cursor != x && qq_compile_spine_tail_unquote(cursor, cctx)) {
                // (... . ,tail): compile the tail form and append it
                compile_qq_rec(cursor, cctx, depth);
                emit3(cctx, OP_PRIM, PAPPEND, 2);
                tail_done = true;
                break;
            }
            unsigned elem = car(cursor);
            if (IS_PAIR(elem) &&
                IS_KEYWORD(car(elem), ctx.kw_unquote_splicing) &&
                !is_keyword_shadowed(CELL_ID(car(elem)), cctx->env) &&
                depth == 1) {
                // ,@expr: compile expr and append
                compile_expr_internal(cadr(elem), cctx);
                emit3(cctx, OP_PRIM, PAPPEND, 2);
                // append permits its final argument to be improper, but a
                // quasiquote splice must be a proper list. Appending an empty
                // list makes the splice result a non-final argument on this
                // second call, where append validates it.
                emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
                emit3(cctx, OP_PRIM, PAPPEND, 2);
            } else {
                // Regular element: compile, wrap in list, append
                compile_qq_rec(elem, cctx, depth);
                emit(cctx, OP_LIST1);
                emit3(cctx, OP_PRIM, PAPPEND, 2);
            }
        }

        if (!tail_done) {
            unsigned tail = x;
            while (IS_PAIR(tail))
                tail = cdr(tail);
            if (tail) {
                compile_qq_rec(tail, cctx, depth);
                emit3(cctx, OP_PRIM, PAPPEND, 2);
            }
        }
    } else {
        // No splicing: build with cons from the end
        // First compile all elements, then cons them together
        unsigned len = 0;
        bool tail_done = false;
        for (cursor = x; IS_PAIR(cursor); cursor = cdr(cursor)) {
            if (cursor != x && qq_compile_spine_tail_unquote(cursor, cctx)) {
                // (... . ,tail): the rest of the spine is the tail value
                compile_qq_rec(cursor, cctx, depth);
                tail_done = true;
                break;
            }
            compile_qq_rec(car(cursor), cctx, depth);
            len++;
        }
        if (!tail_done) {
            // Handle improper list tail
            unsigned tail = x;
            while (IS_PAIR(tail))
                tail = cdr(tail);
            if (tail) {
                compile_qq_rec(tail, cctx, depth);
            } else {
                emit2(cctx, OP_CONST, code_add_const(cctx->code, 0));
            }
        }
        // Now cons them together from right to left
        for (unsigned i = 0; i < len; i++) {
            emit(cctx, OP_CONS);
        }
    }
}

// Quasiquote expansion
static compile_result compile_quasiquote(unsigned expr, compile_ctx *cctx)
{
    if (!syntax_arity_checked(expr, 1, 1, "quasiquote"))
        return emit_syntax_error(cctx, "quasiquote: invalid syntax");

    unsigned tmpl = cadr(expr);
    if (!qq_syntax_valid(tmpl, cctx, 1, 0))
        return emit_syntax_error(cctx, "quasiquote: invalid syntax");

    // Check if quasiquote has any unquotes that need runtime evaluation
    if (!qq_has_unquote(tmpl, cctx, 1)) {
        // No unquotes - can expand at compile time
        unsigned expanded = qq_expand_cps(tmpl, cctx->env);
        if (expanded == TOK_ERROR) {
            show_error("quasiquote expansion failed");
            emit(cctx, OP_HALT);
            return dynamic_result();
        }
        expanded = macros_strip_core_aliases(expanded);
        gc_protect(&expanded);
        emit2(cctx, OP_CONST, code_add_const(cctx->code, expanded));
        gc_unprotect(1);
        return const_result(expanded);
    }

    // Has unquotes - generate runtime code. The unquoted expressions are
    // never in tail position, even when the quasiquote is: the template's
    // value is assembled after they return, so a tail call there would
    // return the unquoted value alone from the enclosing procedure.
    bool tail = cctx->tail_position;
    cctx->tail_position = false;
    compile_qq_rec(tmpl, cctx, 1);
    cctx->tail_position = tail;
    return dynamic_result();
}

// ============================================================================
// Top-Level Compilation
// ============================================================================

code_object *compile_expr(unsigned expr, compile_ctx *cctx)
{
    compile_expr_internal(expr, cctx);
    return cctx->code;
}

void compile_sequence(unsigned exprs, compile_ctx *cctx, bool tail)
{
    cctx->tail_position = tail;
    compile_begin(exprs, cctx);
}

static code_object *compile_unit(unsigned expr, unsigned env,
                                 unsigned terminator)
{
    GC_GUARD;
    gc_protect(&expr);
    gc_protect(&env);

    compile_ctx *cctx = cctx_new(NULL, env);
    if (!cctx)
        return NULL;
    cctx->tail_position = false;
    procedure_name_expr = 0; // a panic may have left it set
    cctx->code->toplevel = true;

    compile_expr_internal(expr, cctx);
    emit(cctx, terminator);

    code_object *result = cctx->code;
    peephole_optimize(result);
    cctx_free(cctx);
    return result;
}

code_object *compile_toplevel(unsigned expr, unsigned env)
{
    return compile_unit(expr, env, OP_HALT);
}

// For eval inside a running VM. The VM pushes a frame and runs the unit in
// place, so it ends in RETURN and hands its value back through that frame
// like any call would. HALT would stop the VM with the caller's frames still
// pending.
code_object *compile_for_eval(unsigned expr, unsigned env)
{
    return compile_unit(expr, env, OP_RETURN);
}
