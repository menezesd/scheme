/**
 * @file macros.c
 * @brief Hygienic macro system (syntax-rules)
 *
 * This file implements R5RS-style hygienic macros using syntax-rules.
 *
 * ## Syntax-Rules Structure
 * A syntax-rules object is stored as:
 *   ((ellipsis . (literals . rules)) . closure_env)
 * where rules is: ((pattern1 . template1) (pattern2 . template2) ...)
 *
 * ## Pattern Matching
 * Patterns can contain:
 * - Literals: Must match exactly (compared by symbol ID)
 * - Pattern variables: Match any expression, bind for template
 * - Underscore (_): Wildcard, matches anything, no binding
 * - Lists: Match list structure recursively
 * - Ellipsis (...): Match zero or more of preceding pattern
 * - Vectors: Match vector literals
 *
 * ## Template Expansion
 * Templates can use:
 * - Pattern variables: Replaced with matched values
 * - Ellipsis: Replicate preceding template for each match
 * - Nested ellipsis: For nested pattern ellipsis
 *
 * ## Hygiene Implementation
 * The macro system uses a combination of techniques for hygiene:
 *
 * 1. **Gensym renaming**: Bindings introduced by the template (in let, lambda,
 *    etc.) are alpha-renamed to fresh gensyms. This prevents
 * template-introduced bindings from capturing references in the macro use site.
 *
 * 2. **Protection markers**: When a pattern variable is substituted with an
 *    identifier or compound datum, it is wrapped as (##protected## . value).
 *    This marks it as "coming from input" so expansion cleanup does not walk
 *    user data, which can contain cycles or shared structure.
 *    The protection marker is preserved through:
 *    - hygienize_template: Skips protected wrappers
 *    - rename_in_template: Skips protected wrappers
 *    - unwrap_protected: Removes markers after expansion (but not inside
 *      syntax-rules, which will be processed separately)
 *
 * This handles the challenging case of nested macros where a substituted
 * identifier has the same name as a template-introduced binding:
 *
 *   (let ((x 1))
 *     (let-syntax ((foo (syntax-rules ()
 *                         ((_ y) (let-syntax ((bar (syntax-rules ()
 *                                                    ((_) (let ((x 2)) y)))))
 *                                  (bar))))))
 *       (foo x)))  ; => 1
 *
 * When foo expands, y is substituted with (##protected## . x). When bar's
 * template is hygienized, the let-introduced x is renamed to a reserved
 * gensym, but the protected x is preserved. Result:
 * (let ((##gensym##0 2)) x) => 1.
 *
 * 3. **Referential transparency**: Free identifiers in templates (not pattern
 *    variables, not special forms) are looked up in the macro's definition
 *    environment. If found, they are renamed to gensyms and the gensym is
 *    bound to the definition-time value in the use-site environment. This
 *    ensures that macros "close over" their definition environment:
 *
 *   (let-syntax ((foo (syntax-rules ()
 *                       ((_ expr) (+ expr 1)))))
 *     (let ((+ *))
 *       (foo 3)))  ; => 4, not 3
 *
 *    The + in foo's template refers to the + at definition time (addition),
 *    not the shadowed + at use time (multiplication).
 *
 *    Assignment targets introduced by a macro are renamed to definition-site
 *    aliases, so set! and define preserve referential transparency while still
 *    updating the intended binding.
 */

#include "macros.h"
#include "compiled_pattern.h"
#include "context.h"
#include "eval_internal.h"
#include "env.h"
#include "prim_internal.h" // make_string_copy
#include <limits.h>
#include <stdlib.h>
#include <string.h>

// External gensym counter
extern unsigned gensym_counter;

// Generate a fresh gensym atom
static unsigned do_gensym(void)
{
    char buf[32];
    snprintf(buf, sizeof(buf), "##gensym##%u", gensym_counter++);
    return make_uninterned_symbol(buf);
}

static bool generated_gensym_atom(unsigned atom)
{
    return atom_is_uninterned(atom);
}

// ============================================================================
// Helper Functions
// ============================================================================

static void append_bindings_chain(unsigned *dst, unsigned src)
{
    if (!dst)
        return;
    if (!src)
        return;

    if (!*dst) {
        *dst = src;
        return;
    }

    unsigned tail = *dst;
    while (IS_PAIR(tail) && cdr(tail))
        tail = cdr(tail);

    if (IS_PAIR(tail))
        cell_set_cdr(tail, src);
}

unsigned expand_form(unsigned expr, unsigned syn_env, unsigned *bindings_out);

static unsigned expand_form_list(unsigned list, unsigned syn_env,
                                unsigned *bindings_out);

static unsigned expand_form_vector(unsigned vec, unsigned syn_env,
                                  unsigned *bindings_out)
{
    unsigned len = vector_len(vec);
    if (!len)
        return vec;

    GC_GUARD;
    gc_protect(&vec);
    gc_protect(&syn_env);

    // Build the result up front so expanded elements live in a GC-scanned
    // container while later elements are still being expanded.
    unsigned new_vec = make_vector(len, 0);
    if (new_vec == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&new_vec);

    bool changed = false;
    for (unsigned i = 0; i < len; i++) {
        unsigned expanded = expand_form(vector_data_ptr(vec)[i], syn_env,
                                        bindings_out);
        if (expanded == TOK_ERROR)
            return TOK_ERROR;
        if (expanded != vector_data_ptr(vec)[i])
            changed = true;
        vector_set_elem(new_vec, i, expanded);
    }

    return changed ? new_vec : vec;
}

static unsigned expand_form_list(unsigned list, unsigned syn_env,
                                unsigned *bindings_out)
{
    if (!list)
        return 0;

    GC_GUARD;
    gc_protect(&list);
    gc_protect(&syn_env);
    if (!IS_PAIR(list))
        return expand_form(list, syn_env, bindings_out);

    unsigned out = 0;
    unsigned out_tail = 0;
    gc_protect(&out);
    gc_protect(&out_tail);

    unsigned it = list;
    gc_protect(&it);
    while (IS_PAIR(it)) {
        unsigned expanded = expand_form(car(it), syn_env, bindings_out);
        if (expanded == TOK_ERROR)
            return TOK_ERROR;
        list_append(&out, &out_tail, expanded);
        it = cdr(it);
    }

    unsigned tail = expand_form(it, syn_env, bindings_out);
    if (tail == TOK_ERROR)
        return TOK_ERROR;
    if (!out)
        return tail;

    if (tail)
        cell_set_cdr(out_tail, tail);

    return out;
}

static unsigned expand_let_syntax(unsigned form, unsigned syn_env,
                                 unsigned *bindings_out)
{
    const char *syn_arity =
        (CELL_ID(car(form)) == ctx.kw_let_syntax) ? "let-syntax" :
                                                   "letrec-syntax";
    GC_GUARD;
    gc_protect(&form);
    gc_protect(&syn_env);
    if (!syntax_arity_checked(form, 2, UINT_MAX, syn_arity))
        return TOK_ERROR;

    unsigned bindings = cadr(form);
    unsigned body = cddr(form);
    if (!binding_list_valid(bindings, syn_arity) ||
        !list_length_checked(body, NULL, syn_arity)) {
        return TOK_ERROR;
    }

    // The keyword is rebuilt into the result after allocations; reading
    // car(form) inline as an argument to the outer alloc_cons could load it
    // before the inner one collects.
    unsigned keyword = car(form);
    gc_protect(&keyword);

    if (!bindings) {
        unsigned expanded_body = expand_form_list(body, syn_env, bindings_out);
        if (expanded_body == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&expanded_body);
        unsigned tail = alloc_cons(bindings, expanded_body);
        gc_protect(&tail);
        return alloc_cons(keyword, tail);
    }

    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&body);
    gc_protect(&syn_env);

    unsigned expanded_env = extend_env_empty(syn_env);
    gc_protect(&expanded_env);
    int64_t ellipsis_id =
        syntax_default_ellipsis_id((CELL_ID(car(form)) == ctx.kw_let_syntax)
                                       ? syn_env
                                       : expanded_env);

    unsigned b = bindings;
    gc_protect(&b);
    while (b) {
        unsigned binding = car(b);
        if (!IS_PAIR(binding) || !identifier_valid(car(binding)) ||
            !is_syntax_rules(cadr(binding))) {
            return TOK_ERROR;
        }

        // name is used after make_syntax_transformer allocates.
        unsigned name = car(binding);
        gc_protect(&name);
        unsigned transformer_form = cadr(binding);
        if (!syntax_rules_valid(transformer_form, ellipsis_id, syn_arity))
            return TOK_ERROR;

        unsigned p = make_syntax_transformer(
            transformer_form,
            (CELL_ID(keyword) == ctx.kw_let_syntax) ? syn_env :
                                                      expanded_env);
        if (p == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&p);

        if (defvar(name, p, expanded_env) == TOK_ERROR)
            return TOK_ERROR;
        gc_unprotect(2); // p, name

        b = cdr(b);
    }

    unsigned expanded_body = expand_form_list(body, expanded_env, bindings_out);
    if (expanded_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&expanded_body);
    unsigned tail = alloc_cons(bindings, expanded_body);
    gc_protect(&tail);
    return alloc_cons(keyword, tail);
}

// While a nested syntax-rules template is unwrapped, the ellipsis identifier
// of that transformer: a wrapped occurrence of it must come off too, or an
// ellipsis handed in by the outer macro is not seen as one. 0 when unset.
static int64_t unwrap_template_ellipsis_id = 0;

// The static expander recurses through macro outputs; a self-referential
// macro referenced from inside an explicit lambda body overflows the C
// stack before any engine-level depth check runs, so bound it here.
#define MAX_STATIC_EXPANSION_DEPTH 10000

static int expansion_depth = 0;

void macros_reset_expansion_depth(void)
{
    expansion_depth = 0;
    unwrap_template_ellipsis_id = 0;
}

static unsigned expand_form_inner(unsigned expr, unsigned syn_env,
                                  unsigned *bindings_out);

unsigned expand_form(unsigned expr, unsigned syn_env, unsigned *bindings_out)
{
    if (expansion_depth >= MAX_STATIC_EXPANSION_DEPTH) {
        // Leave a form that raises when it runs instead of failing the whole
        // lambda body, as the compiler does (compile_macro_binding): a
        // self-referential macro in a branch that is never taken, say
        // (if #f (loop 1) 'ok), is a correct program. Failing here made the
        // closure raise as soon as it was called. Not the unexpanded form:
        // re-expanding it at run time would never reach a limit, since the
        // run-time counter restarts whenever an argument is evaluated.
        unsigned msg =
            make_string_copy("macro expansion exceeded maximum depth");
        if (msg == TOK_ERROR)
            return TOK_ERROR;
        GC_GUARD;
        gc_protect(&msg);
        unsigned raiser = mk_primop(PERROR);
        gc_protect(&raiser);
        unsigned args = alloc_cons(msg, 0);
        gc_protect(&args);
        return alloc_cons(raiser, args);
    }
    expansion_depth++;
    unsigned result = expand_form_inner(expr, syn_env, bindings_out);
    expansion_depth--;
    return result;
}

// Bind every name in a formal-parameter list (proper or dotted) into a
// fresh frame over parent_env. Values are irrelevant placeholders: the
// frame exists so scope-sensitive decisions during expansion (ellipsis
// shadowing, macro-vs-variable dispatch) see the parameters as ordinary
// lexically bound names.
unsigned extend_env_with_binder_names(unsigned parent_env, unsigned params)
{
    GC_GUARD;
    gc_protect(&parent_env);
    gc_protect(&params);
    unsigned frame_env = extend_env_empty(parent_env);
    gc_protect(&frame_env);

    unsigned p = params;
    gc_protect(&p);
    while (IS_PAIR(p)) {
        unsigned b = car(p);
        gc_protect(&b);
        if (IS_ATOM(b) &&
            defvar(b, ctx.atom_false, frame_env) == TOK_ERROR)
            return TOK_ERROR;
        gc_unprotect(1);
        p = cdr(p);
    }
    if (p && IS_ATOM(p) &&
        defvar(p, ctx.atom_false, frame_env) == TOK_ERROR)
        return TOK_ERROR;

    return frame_env;
}

static unsigned expand_without_hygiene(unsigned transformer, unsigned form,
                                       unsigned use_env);
static unsigned unwrap_protected(unsigned expr, bool preserve_identifiers);

// Macro uses followed while deciding whether a body form is a definition.
#define MAX_DEFINITION_SCAN_EXPANSIONS 64

// Atom cells of a macro use's form, for telling the identifiers it supplied
// from ones its expansion introduced: the reader allocates a cell per
// occurrence, and expansion passes a substituted identifier on as the very
// same cell, through any number of nested macro uses. False if the form is
// too large to bother with (the scan then just stops, as before).
#define MAX_DEFINITION_SCAN_ATOMS 4096

static bool collect_form_atoms(unsigned form, unsigned *atoms, unsigned *count,
                               unsigned depth)
{
    if (depth > 256)
        return false;
    for (; IS_PAIR(form); form = cdr(form)) {
        if (!collect_form_atoms(car(form), atoms, count, depth + 1))
            return false;
    }
    if (!IS_ATOM(form))
        return true; // (), a vector or other datum: no define target in it
    if (++*count > MAX_DEFINITION_SCAN_ATOMS)
        return false;
    GC_GUARD;
    gc_protect(&form);
    *atoms = alloc_cons(form, *atoms);
    return true;
}

static bool atom_cell_listed(unsigned atom, unsigned atoms)
{
    for (; IS_PAIR(atoms); atoms = cdr(atoms)) {
        if (car(atoms) == atom)
            return true;
    }
    return false;
}

// user_atoms: 0 when body is code as written; otherwise body came out of
// expand_without_hygiene for a use whose own atom cells are listed, and only
// a define target that is one of them - a name the use supplied - is
// defined as written. A target the macros introduced is renamed by the real
// expansion (hygienize_define), so binding it here left a #f placeholder
// that hid the outer variable of the same name.
static int bind_internal_definitions_at(unsigned env, unsigned body,
                                        unsigned *frame, unsigned expansions,
                                        unsigned user_atoms,
                                        bool *saw_define_syntax)
{
    GC_GUARD;
    gc_protect(&env);
    gc_protect(&body);
    gc_protect(&user_atoms);
    for (; IS_PAIR(body); body = cdr(body)) {
        unsigned form = car(body);
        if (!IS_PAIR(form) || !IS_ATOM(car(form)))
            return 0;
        int64_t kw = CELL_ID(car(form));
        unsigned head_binding = lookup_silent(kw, env);
        if (head_binding != TOK_ERROR && IS_SYNTAX(head_binding)) {
            // A macro use may expand into definitions - define-values,
            // define-record-type, a user macro around define - and those
            // names must shadow outer macros for the rest of the body just
            // as a literal define does. Look at its expansion, made without
            // the hygiene passes (see expand_without_hygiene). Not after a
            // define-syntax in this body, though: the scan cannot tell
            // whether the keyword is still the outer macro it finds in env.
            if (expansions >= MAX_DEFINITION_SCAN_EXPANSIONS ||
                *saw_define_syntax)
                return 0;
            unsigned atoms = user_atoms;
            gc_protect(&atoms);
            if (!atoms) {
                unsigned count = 0;
                if (!collect_form_atoms(cdr(form), &atoms, &count, 0))
                    return 0;
                if (!atoms)
                    atoms = alloc_cons(0, 0); // nothing supplied; non-empty
            }
            unsigned expanded =
                expand_without_hygiene(head_binding, form, env);
            if (expanded == TOK_ERROR)
                return 0; // the real expansion reports it
            gc_protect(&expanded);
            unsigned as_body = alloc_cons(expanded, 0);
            gc_protect(&as_body);
            int status = bind_internal_definitions_at(
                env, as_body, frame, expansions + 1, atoms, saw_define_syntax);
            gc_unprotect(3);
            if (status != 1)
                return status;
            continue;
        }
        if (kw == ctx.kw_begin && head_binding == TOK_ERROR) {
            int status = bind_internal_definitions_at(
                env, cdr(form), frame, expansions, user_atoms,
                saw_define_syntax);
            if (status != 1)
                return status;
            continue;
        }
        // define-syntax is a definition too; the defines after it are in
        // the same scope (compile.c's collect_internal_defines agrees).
        if (kw == ctx.kw_define_syntax && head_binding == TOK_ERROR) {
            *saw_define_syntax = true;
            continue;
        }
        if (kw != ctx.kw_define || head_binding != TOK_ERROR ||
            !IS_PAIR(cdr(form)))
            return 0;
        unsigned target = cadr(form);
        unsigned name = IS_PAIR(target) ? car(target) : target;
        if (!IS_ATOM(name))
            return 0;
        if (user_atoms && !atom_cell_listed(name, user_atoms))
            continue; // introduced by a template: renamed, not bound
        gc_protect(&name);
        if (!*frame)
            *frame = extend_env_empty(env);
        if (*frame == TOK_ERROR ||
            defvar(name, ctx.atom_false, *frame) == TOK_ERROR)
            return -1;
        gc_unprotect(1);
    }
    return body == 0;
}

// Return 1 for an all-definition sequence, 0 at the first expression, and
// -1 on failure. Nested begins splice into the same definition scope.
static int bind_internal_definitions(unsigned env, unsigned body,
                                     unsigned *frame)
{
    bool saw_define_syntax = false;
    return bind_internal_definitions_at(env, body, frame, 0, 0,
                                        &saw_define_syntax);
}

// Layer a frame with all leading internal names over env before expansion,
// matching the compiler's scan_internal_defines, including nested begins.
unsigned extend_env_with_internal_defines(unsigned env, unsigned body)
{
    GC_GUARD;
    gc_protect(&env);
    unsigned frame = 0;
    gc_protect(&frame);
    if (bind_internal_definitions(env, body, &frame) < 0)
        return TOK_ERROR;
    return frame ? frame : env;
}

static unsigned expand_form_inner(unsigned expr, unsigned syn_env,
                                  unsigned *bindings_out)
{
    if (!expr || !IS_CELL(expr))
        return expr;

    if (IS_VECTOR(expr))
        return expand_form_vector(expr, syn_env, bindings_out);

    if (!IS_PAIR(expr))
        return expr;

    unsigned head = car(expr);
    if (IS_ATOM(head)) {
        int64_t kw = CELL_ID(head);
        if (kw != ctx.kw_quote && kw != ctx.kw_quasiquote &&
            kw != ctx.kw_cond_expand) {
            unsigned binding = lookup_silent(kw, syn_env);
            if (binding != TOK_ERROR && IS_SYNTAX(binding)) {
                GC_GUARD;
                gc_protect(&expr);
                gc_protect(&syn_env);
                unsigned produced_bindings = 0;
                gc_protect(&produced_bindings);
                unsigned expanded =
                    apply_syntax(binding, expr, syn_env, &produced_bindings);
                if (expanded == TOK_ERROR)
                    return TOK_ERROR;

                if (bindings_out && produced_bindings)
                    append_bindings_chain(bindings_out, produced_bindings);

                return expand_form(expanded, syn_env, bindings_out);
            }
        }

        if (kw == ctx.kw_quote || kw == ctx.kw_quasiquote ||
            kw == ctx.kw_cond_expand)
            return expr;

        if (kw == ctx.kw_define_syntax) {
            GC_GUARD;
            gc_protect(&expr);
            gc_protect(&syn_env);
            if (!syntax_arity_checked(expr, 2, 2, "define-syntax"))
                return TOK_ERROR;

            unsigned name = cadr(expr);
            unsigned transformer_form = caddr(expr);
            gc_protect(&name);
            gc_protect(&transformer_form);
            if (!identifier_valid(name)) {
                show_error("define-syntax: expected name");
                return TOK_ERROR;
            }
            if (!syntax_rules_valid(transformer_form,
                                   syntax_default_ellipsis_id(syn_env),
                                   "define-syntax")) {
                return TOK_ERROR;
            }

            unsigned p =
                make_syntax_transformer(transformer_form, syn_env);
            if (p == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&p);
            if (defvar(name, p, syn_env) == TOK_ERROR)
                return TOK_ERROR;

            return expr;
        }

        if (kw == ctx.kw_let_syntax || kw == ctx.kw_letrec_syntax)
            return expand_let_syntax(expr, syn_env, bindings_out);

        // Lambda bodies expand at their own creation (handle_lambda), so
        // the walker must not descend here: expanding them now would fire
        // syntax errors - and bake hygiene decisions - in the enclosing
        // scope instead of the lambda's own.
        if (kw == ctx.kw_lambda)
            return expr;

        // (define (name . formals) body ...) is a lambda in disguise, so its
        // body must expand in the scope its closure will have: walking it
        // as a plain list treated (define (inner when) (when 1 2)) as calls
        // in the enclosing scope, where when is still the macro. Expand it
        // here, once, against the frames build_lambda_closure would add -
        // leaving it for handle_define instead re-expanded the body, minting
        // fresh gensyms, every time the enclosing procedure ran. For
        // (define name expr) only expr is an expression.
        if (kw == ctx.kw_define && lookup_silent(kw, syn_env) == TOK_ERROR &&
            IS_PAIR(cdr(expr))) {
            if (IS_PAIR(cadr(expr))) {
                GC_GUARD;
                gc_protect(&expr); // lambda_params_valid may allocate
                if (!IS_ATOM(car(cadr(expr))) || !cddr(expr) ||
                    !proper_list_silent(cddr(expr)) ||
                    !lambda_params_valid(cdr(cadr(expr))))
                    return expr; // handle_define reports what is wrong
                unsigned binder_env =
                    extend_env_with_binder_names(syn_env, cdr(cadr(expr)));
                if (binder_env == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&binder_env);
                unsigned body_env =
                    extend_env_with_internal_defines(binder_env, cddr(expr));
                if (body_env == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&body_env);
                unsigned new_body =
                    expand_form(cddr(expr), body_env, bindings_out);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                if (new_body == cddr(expr))
                    return expr;
                gc_protect(&new_body);
                unsigned target_tail = alloc_cons(cadr(expr), new_body);
                gc_protect(&target_tail);
                return alloc_cons(car(expr), target_tail);
            }
            if (!IS_ATOM(cadr(expr)))
                return expr;
            if (!IS_PAIR(cddr(expr)) || cdr(cddr(expr)))
                return expr; // malformed; handle_define reports it
            GC_GUARD;
            gc_protect(&expr);
            unsigned value = expand_form(car(cddr(expr)), syn_env,
                                         bindings_out);
            if (value == TOK_ERROR)
                return TOK_ERROR;
            if (value == car(cddr(expr)))
                return expr;
            gc_protect(&value);
            unsigned value_tail = alloc_cons(value, 0);
            gc_protect(&value_tail);
            unsigned target_tail = alloc_cons(cadr(expr), value_tail);
            gc_protect(&target_tail);
            return alloc_cons(car(expr), target_tail);
        }
    }

    return expand_form_list(expr, syn_env, bindings_out);
}

// What a (##protected## . x) wrapper - one or more deep - holds, or 0 if
// tmpl is not a wrapper. Allocates nothing.
static unsigned protected_payload(unsigned tmpl)
{
    if (!IS_PAIR(tmpl) || !IS_ATOM(car(tmpl)) ||
        CELL_ID(car(tmpl)) != ctx.kw_protected)
        return 0;
    while (IS_PAIR(tmpl) && IS_ATOM(car(tmpl)) &&
           CELL_ID(car(tmpl)) == ctx.kw_protected)
        tmpl = cdr(tmpl);
    return tmpl;
}

// Pattern-variable bindings are (key . value). A variable matched outside
// any ellipsis has its identifier as key. A variable matched under N >= 1
// ellipses has key (identifier . N) - see annotate_ellipsis_depths - so the
// expander can tell an ellipsis sequence from a depth-0 variable that merely
// matched a list: in ((x y) ...) only y is iterated, x is repeated as is.
// Bindings built without depth information (the C unit tests) are depth 0.
static unsigned binding_key_var(unsigned key)
{
    return IS_PAIR(key) ? car(key) : key;
}

static unsigned binding_key_depth(unsigned key)
{
    if (!IS_PAIR(key) || !IS_NUM(cdr(key)) || CELL_ID(cdr(key)) < 1)
        return 0;
    return (unsigned)CELL_ID(cdr(key));
}

static bool binding_key_is(unsigned key, unsigned var)
{
    unsigned key_var = binding_key_var(key);
    return IS_ATOM(key_var) && IS_ATOM(var) && CELL_ID(key_var) == CELL_ID(var);
}

// Look up a pattern variable in bindings. The first match wins: ellipsis
// iteration pushes each element's binding in front of the sequence's.
static unsigned syntax_lookup(unsigned var, unsigned bindings)
{
    for (; IS_PAIR(bindings); bindings = cdr(bindings)) {
        unsigned binding = car(bindings);
        if (!IS_PAIR(binding))
            continue;
        if (binding_key_is(car(binding), var))
            return cdr(binding);
    }
    return TOK_ERROR;
}

// The binding an ellipsis following a subtemplate iterates for var, or 0 if
// var is not a pattern variable or is (now) depth 0. Only the innermost
// binding counts: once an outer iteration has rebound var to one element of
// its sequence, the older sequence binding behind it must not be iterated.
static unsigned find_ellipsis_binding(unsigned var, unsigned bindings)
{
    for (unsigned b = bindings; IS_PAIR(b); b = cdr(b)) {
        unsigned binding = car(b);
        if (!IS_PAIR(binding) || !binding_key_is(car(binding), var))
            continue;
        return binding_key_depth(car(binding)) >= 1 ? binding : 0;
    }
    return 0;
}

// Key for one element of an ellipsis variable's sequence: one ellipsis
// shallower than the sequence's own key.
static unsigned ellipsis_element_key(unsigned key)
{
    unsigned depth = binding_key_depth(key);
    unsigned var = binding_key_var(key);
    if (depth <= 1)
        return var;
    GC_GUARD;
    gc_protect(&var);
    return alloc_cons(var, store((int64_t)depth - 1));
}

// Collect ALL ellipsis-bound variables from a template into a list
// Returns list of bindings: ((var1 . values1) (var2 . values2) ...)
static unsigned collect_ellipsis_vars(unsigned tmpl, unsigned bindings,
                                      unsigned collected)
{
    if (!tmpl)
        return collected;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    gc_protect(&collected);

    if (IS_ATOM(tmpl)) {
        unsigned binding = find_ellipsis_binding(tmpl, bindings);
        if (binding && IS_PAIR(cdr(binding))) {
            // Check if already collected
            for (unsigned c = collected; IS_PAIR(c); c = cdr(c)) {
                unsigned collected_binding = car(c);
                if (!IS_PAIR(collected_binding))
                    continue;
                if (binding_key_is(car(collected_binding),
                                   binding_key_var(car(binding))))
                    return collected; // Already have it
            }
            gc_protect(&binding);
            unsigned result = alloc_cons(binding, collected);
            return result;
        }
        return collected;
    }

    if (IS_PAIR(tmpl)) {
        while (IS_PAIR(tmpl)) {
            collected = collect_ellipsis_vars(car(tmpl), bindings, collected);
            tmpl = cdr(tmpl);
        }
        return collect_ellipsis_vars(tmpl, bindings, collected);
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            collected = collect_ellipsis_vars(data[i], bindings, collected);
        }
    }

    return collected;
}

static bool ellipsis_vars_iteration_count(unsigned ellipsis_vars,
                                          unsigned *iter_count)
{
    bool have_count = false;
    unsigned expected = 0;

    for (unsigned ev = ellipsis_vars; ev; ev = cdr(ev)) {
        if (!IS_PAIR(ev)) {
            show_error("syntax-rules: invalid ellipsis bindings");
            return false;
        }
        unsigned binding = car(ev);
        if (!IS_PAIR(binding)) {
            show_error("syntax-rules: invalid ellipsis bindings");
            return false;
        }
        unsigned values = cdr(binding);
        if (!proper_list_silent(values)) {
            show_error("syntax-rules: invalid ellipsis bindings");
            return false;
        }
        unsigned count = list_length(values);
        if (!have_count) {
            expected = count;
            have_count = true;
        } else if (count != expected) {
            show_error("syntax-rules: inconsistent ellipsis match lengths");
            return false;
        }
    }

    *iter_count = have_count ? expected : 0;
    return true;
}

// ============================================================================
// Hygienic Renaming
// ============================================================================

// Unwrap protected identifiers: (##protected## . x) → x
// Called after expansion to remove protection markers
static unsigned unwrap_protected(unsigned expr, bool preserve_identifiers);

// A nested (syntax-rules ...) in an expansion keeps the wrappers on its
// template identifiers - they keep the inner macro's hygiene passes off what
// the outer macro substituted - but they must come off wherever syntax-rules
// requires a bare identifier or datum: the custom ellipsis, the literals and
// each rule's pattern. Left on, an outer pattern variable used as the inner
// rule's keyword, a pattern variable, a literal or the ellipsis (R7RS 4.3.2's
// own be-like-begin example) made the inner transformer malformed.
// A pattern variable substituted as the dotted tail of a list -
// (syntax-rules () . rules) - is the wrapper (##protected## . value), which
// read as a spine continues with the marker as its next element. Step over
// the marker to the substituted list itself. Allocates nothing.
static unsigned strip_spine_wrapper(unsigned rest)
{
    while (IS_PAIR(rest) && IS_ATOM(car(rest)) &&
           CELL_ID(car(rest)) == ctx.kw_protected)
        rest = cdr(rest);
    return rest;
}

static unsigned unwrap_syntax_rules_form(unsigned expr)
{
    GC_GUARD;
    gc_protect(&expr);
    unsigned out = 0, tail = 0;
    gc_protect(&out);
    gc_protect(&tail);
    list_append(&out, &tail, car(expr)); // the syntax-rules keyword
    unsigned rest = strip_spine_wrapper(cdr(expr));
    gc_protect(&rest);

    int64_t ellipsis_id = ctx.kw_ellipsis;
    if (IS_PAIR(rest)) {
        unsigned first = unwrap_protected(car(rest), false);
        if (first == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&first); // inspected after list_append allocates
        list_append(&out, &tail, first);
        rest = strip_spine_wrapper(cdr(rest));
        if (IS_ATOM(first) && IS_PAIR(rest)) {
            ellipsis_id = CELL_ID(first);
            // first was a custom ellipsis; the literals follow it
            unsigned literals = unwrap_protected(car(rest), false);
            if (literals == TOK_ERROR)
                return TOK_ERROR;
            list_append(&out, &tail, literals);
            rest = cdr(rest);
        }
    }

    for (;;) {
        rest = strip_spine_wrapper(rest);
        if (!IS_PAIR(rest))
            break;
        unsigned rule = car(rest);
        unsigned new_rule;
        if (IS_PAIR(rule) && IS_PAIR(cdr(rule)) && !cddr(rule)) {
            unsigned pattern = unwrap_protected(car(rule), false);
            if (pattern == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&pattern);
            int64_t saved_ellipsis_id = unwrap_template_ellipsis_id;
            unwrap_template_ellipsis_id = ellipsis_id;
            unsigned tmpl = unwrap_protected(cadr(car(rest)), true);
            unwrap_template_ellipsis_id = saved_ellipsis_id;
            if (tmpl == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&tmpl);
            unsigned rule_tail = alloc_cons(tmpl, 0);
            gc_protect(&rule_tail);
            new_rule = alloc_cons(pattern, rule_tail);
            gc_unprotect(3);
        } else {
            new_rule = unwrap_protected(rule, true); // malformed: let the
                                                     // validator report it
            if (new_rule == TOK_ERROR)
                return TOK_ERROR;
        }
        list_append(&out, &tail, new_rule);
        rest = cdr(rest);
    }
    if (rest) {
        unsigned dotted = unwrap_protected(rest, true);
        if (dotted == TOK_ERROR)
            return TOK_ERROR;
        cell_set_cdr(tail, dotted);
    }
    return out;
}

static unsigned unwrap_protected(unsigned expr, bool preserve_identifiers)
{
    if (!expr)
        return 0;

    GC_GUARD;
    gc_protect(&expr);

    // Check for protected wrapper
    if (IS_PAIR(expr)) {
        unsigned head = car(expr);
        if (IS_ATOM(head) && CELL_ID(head) == ctx.kw_protected) {
            // Unwrap: (##protected## . x) → x. A template identifier that
            // was already protected when it was substituted - an outer
            // macro's argument reused as an inner pattern variable - comes
            // back wrapped twice; peel to the innermost wrapper, without
            // looking inside the wrapped datum itself.
            unsigned wrapper = expr;
            while (IS_PAIR(cdr(wrapper)) && IS_ATOM(car(cdr(wrapper))) &&
                   CELL_ID(car(cdr(wrapper))) == ctx.kw_protected)
                wrapper = cdr(wrapper);
            if (preserve_identifiers && IS_ATOM(cdr(wrapper)) &&
                CELL_ID(cdr(wrapper)) != unwrap_template_ellipsis_id)
                return wrapper;
            return cdr(wrapper);
        }

        // Nested transformers still need identifier protection for hygiene.
        // Compound substitutions, however, must regain their original shape
        // before the nested patterns/templates are processed.
        if (IS_ATOM(head) && CELL_ID(head) == ctx.kw_syntax_rules)
            return unwrap_syntax_rules_form(expr);
        // Walk the spine iteratively, recursing only into the cars. Recursing
        // on the cdr as well cost one C frame per list element, so a macro
        // whose expansion is a long flat body - (begin e1 ... e100000), which
        // is exactly what the large-flat-template tests build - ran the C
        // stack out. Depth is now the expression's nesting, not its length.
        //
        // The result is shared with the input until something actually
        // changes: most expansions unwrap nothing, and copying every one of
        // them would be pure allocation. On the first change the identical
        // prefix is copied and building continues from there.
        unsigned out = 0, out_tail = 0, cursor = expr, tail = 0;
        unsigned prefix = 0;
        bool changed = false;
        gc_protect(&out);
        gc_protect(&out_tail);
        gc_protect(&cursor);
        gc_protect(&tail);

        for (;;) {
            // The spine ends at a non-pair - the empty list or a dotted tail -
            // or at a cell that is itself a wrapper form. Either way, what the
            // recursive call makes of it becomes the result's tail.
            bool spine_end = !IS_PAIR(cursor);
            if (!spine_end && IS_ATOM(car(cursor))) {
                int64_t id = CELL_ID(car(cursor));
                spine_end = id == ctx.kw_protected ||
                            (id == ctx.kw_syntax_rules && cursor != expr);
            }
            if (spine_end) {
                tail = unwrap_protected(cursor, preserve_identifiers);
                if (tail == TOK_ERROR)
                    return TOK_ERROR;
                if (tail != cursor && !changed) {
                    // The change is in the tail, so the whole prefix is still
                    // uncopied. (quote (a . x)) with x wrapped lands here.
                    changed = true;
                    unsigned p = expr;
                    gc_protect(&p);
                    for (unsigned i = 0; i < prefix; i++) {
                        list_append(&out, &out_tail, car(p));
                        p = cdr(p);
                    }
                    gc_unprotect(1);
                }
                break;
            }

            unsigned new_car = unwrap_protected(car(cursor), preserve_identifiers);
            if (new_car == TOK_ERROR)
                return TOK_ERROR;
            if (!changed && new_car == car(cursor)) {
                prefix++;
                cursor = cdr(cursor);
                continue;
            }
            gc_protect(&new_car); // the prefix copy below allocates
            if (!changed) {
                changed = true;
                unsigned p = expr;
                gc_protect(&p);
                for (unsigned i = 0; i < prefix; i++) {
                    list_append(&out, &out_tail, car(p));
                    p = cdr(p);
                }
                gc_unprotect(1);
            }
            list_append(&out, &out_tail, new_car);
            gc_unprotect(1);
            cursor = cdr(cursor);
        }

        if (!changed)
            return expr;
        if (!out_tail)
            return tail; // the spine ended before any element was kept
        cell_set_cdr(out_tail, tail);
        return out;
    }

    if (IS_VECTOR(expr)) {
        unsigned len = vector_len(expr);
        bool changed = false;

        for (unsigned i = 0; i < len && !changed; i++) {
            // Refresh data pointer - GC may have moved expr
            unsigned *data = vector_data_ptr(expr);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned unwrapped = unwrap_protected(orig, preserve_identifiers);
            if (unwrapped == TOK_ERROR)
                return TOK_ERROR;
            if (unwrapped != orig)
                changed = true;
            gc_unprotect(1);
        }

        if (!changed) {
            return expr;
        }

        // Protect expr across allocation - GC may move vectors
        gc_protect(&expr);
        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR) {
            gc_unprotect(1);
            return TOK_ERROR;
        }
        gc_protect(&new_vec);
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointers - GC may have moved vectors
            unsigned *data = vector_data_ptr(expr);
            unsigned elem = data[i];
            unsigned unwrapped = unwrap_protected(elem, preserve_identifiers);
            if (unwrapped == TOK_ERROR) {
                gc_unprotect(2);
                return TOK_ERROR;
            }
            // Through the write barrier: new_vec may have been promoted by a
            // collection during the recursive call, and a raw store would hide
            // this nursery element from the next minor GC.
            vector_set_elem(new_vec, i, unwrapped);
        }
        gc_unprotect(2);
        return new_vec;
    }

    return expr;
}

// Check if a symbol ID is a pattern variable (bound in bindings list)
static bool is_pattern_var(int64_t sym_id, unsigned bindings)
{
    for (; bindings; bindings = cdr(bindings)) {
        unsigned binding = car(bindings);
        unsigned pvar = car(binding);
        if (IS_ATOM(pvar) && CELL_ID(pvar) == sym_id)
            return true;
        // Also check pattern pairs (nested pattern variables)
        if (IS_PAIR(pvar)) {
            unsigned p = pvar;
            for (; p; p = IS_PAIR(p) ? cdr(p) : 0) {
                unsigned elem = IS_PAIR(p) ? car(p) : p;
                if (IS_ATOM(elem) && CELL_ID(elem) == sym_id)
                    return true;
                if (!IS_PAIR(p))
                    break;
            }
        }
    }
    return false;
}

// Check if an identifier is a special form keyword
bool is_special_form(int64_t id)
{
    return id == ctx.kw_lambda || id == ctx.kw_if || id == ctx.kw_define ||
           id == ctx.kw_set || id == ctx.kw_quote || id == ctx.kw_begin ||
           id == ctx.kw_cond || id == ctx.kw_cond_expand ||
           id == ctx.kw_and || id == ctx.kw_or ||
           id == ctx.kw_let_syntax || id == ctx.kw_letrec_syntax ||
           id == ctx.kw_define_syntax || id == ctx.kw_syntax_rules ||
           id == ctx.kw_quasiquote || id == ctx.kw_unquote ||
           id == ctx.kw_unquote_splicing || id == ctx.kw_else ||
           id == ctx.kw_ellipsis || id == ctx.kw_underscore ||
           id == ctx.kw_let || id == ctx.kw_letstar || id == ctx.kw_letrec;
}

// The identifier a hygiene alias gensym stands for, indexed by the gensym's
// atom id; 0 for anything else. Only consulted to recognise a form whose head
// was renamed to a definition alias (see template_case_form).
static int64_t *alias_origins = NULL;
static size_t alias_origins_len = 0;

void macros_note_alias(int64_t alias_id, int64_t origin_id)
{
    if (alias_id <= 0 || origin_id <= 0)
        return;
    if ((size_t)alias_id >= alias_origins_len) {
        size_t new_len = alias_origins_len ? alias_origins_len : 1024;
        while (new_len <= (size_t)alias_id)
            new_len *= 2;
        int64_t *grown = realloc(alias_origins, new_len * sizeof(int64_t));
        if (!grown)
            return; // only a recognition aid; losing an entry is harmless
        memset(grown + alias_origins_len, 0,
               (new_len - alias_origins_len) * sizeof(int64_t));
        alias_origins = grown;
        alias_origins_len = new_len;
    }
    alias_origins[alias_id] = origin_id;
}

static int64_t identifier_origin(int64_t id)
{
    for (int hops = 0; hops < 64 && id > 0 && (size_t)id < alias_origins_len &&
                       alias_origins[id];
         hops++)
        id = alias_origins[id];
    return id;
}

int64_t macros_toplevel_defines_marker_id(void)
{
    static int64_t marker_id = 0;
    if (!marker_id)
        marker_id = CELL_ID(make_uninterned_symbol("##toplevel-defines##"));
    return marker_id;
}

// True if env's first frame is a top-level begin's placeholder frame.
static bool frame_is_toplevel_defines(unsigned env)
{
    int64_t marker = macros_toplevel_defines_marker_id();
    unsigned cell = env_find_binding_cell(marker, env);
    return cell && IS_PAIR(env) &&
           cell != env_find_binding_cell(marker, cdr(env));
}

// id's binding cell for hygiene comparisons. A binding in a top-level
// placeholder frame is not one: at run time the name is the global the
// define creates, so look past the frame. Neither lookup allocates.
static unsigned hygiene_binding_cell(int64_t id, unsigned env)
{
    unsigned cell = env_find_binding_cell(id, env);
    if (!cell ||
        lookup_silent(macros_toplevel_defines_marker_id(), env) == TOK_ERROR)
        return cell; // no placeholder frame to look past
    for (unsigned e = env; IS_PAIR(e) && cell; e = cdr(e)) {
        unsigned below = env_find_binding_cell(id, cdr(e));
        if (below == cell)
            continue; // not bound in this frame
        if (!frame_is_toplevel_defines(e))
            return cell;
        cell = below;
    }
    return cell;
}

// Bound at all, for the use-site-only rename in apply_syntax, with the same
// view of top-level placeholders as hygiene_binding_cell.
static bool bound_for_hygiene(int64_t id, unsigned env)
{
    if (lookup_silent(id, env) == TOK_ERROR)
        return false;
    if (!env_find_binding_cell(id, env))
        return true;
    return hygiene_binding_cell(id, env) != 0;
}

bool macros_same_binding(int64_t id, unsigned env_a, unsigned env_b)
{
    return hygiene_binding_cell(id, env_a) == hygiene_binding_cell(id, env_b);
}

bool template_case_form(unsigned tmpl)
{
    static int case_id = -1;
    if (case_id < 0)
        case_id = intern("case");
    return IS_PAIR(tmpl) && IS_ATOM(car(tmpl)) &&
           identifier_origin(CELL_ID(car(tmpl))) == case_id &&
           IS_PAIR(cdr(tmpl)) && proper_list_silent(cddr(tmpl));
}

// Rebuild (case key clause ...) with walk applied to the key and to each
// clause's expressions, never to a clause's datum list (or else). The
// datums are quoted by case itself: renaming one changed what it matches -
// (case v ((+) ...)) stopped matching + once + was a definition alias.
typedef unsigned (*case_walker)(unsigned form, void *arg);

static unsigned map_case_form(unsigned tmpl, case_walker walk, void *arg)
{
    GC_GUARD;
    gc_protect(&tmpl);
    unsigned out = 0, tail = 0;
    gc_protect(&out);
    gc_protect(&tail);
    unsigned head = walk(car(tmpl), arg); // case itself may be an alias
    if (head == TOK_ERROR)
        return TOK_ERROR;
    list_append(&out, &tail, head);
    unsigned key = walk(cadr(tmpl), arg);
    if (key == TOK_ERROR)
        return TOK_ERROR;
    list_append(&out, &tail, key);
    unsigned c = cddr(tmpl);
    gc_protect(&c);
    for (; c; c = cdr(c)) {
        unsigned clause = car(c);
        if (!IS_PAIR(clause)) {
            list_append(&out, &tail, clause);
            continue;
        }
        unsigned body = walk(cdr(clause), arg);
        if (body == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&body);
        unsigned new_clause = alloc_cons(car(car(c)), body);
        gc_unprotect(1);
        list_append(&out, &tail, new_clause);
    }
    return out;
}

static bool binding_shadows_special(int64_t sym_id, unsigned bindings)
{
    return is_special_form(sym_id) && is_pattern_var(sym_id, bindings);
}

static bool keyword_bound_at_definition(int64_t sym_id, unsigned closure_env)
{
    if (!is_special_form(sym_id))
        return false;
    unsigned value = lookup_silent(sym_id, closure_env);
    return value != TOK_ERROR && !IS_SYNTAX(value);
}

static bool set_form_like(unsigned expr)
{
    return IS_PAIR(expr) && IS_ATOM(car(expr)) &&
           CELL_ID(car(expr)) == ctx.kw_set && IS_PAIR(cdr(expr)) &&
           IS_PAIR(cddr(expr)) && !cdddr(expr);
}

static bool define_form_like(unsigned expr)
{
    return IS_PAIR(expr) && IS_ATOM(car(expr)) &&
           CELL_ID(car(expr)) == ctx.kw_define && IS_PAIR(cdr(expr)) &&
           IS_PAIR(cddr(expr));
}

// Collect free identifiers from a template
// Returns a list of unique atoms that are free (not pattern vars, not special
// forms)
static unsigned collect_free_ids(unsigned tmpl, unsigned bindings,
                                 unsigned collected, int64_t ellipsis_id,
                                 unsigned closure_env);

static unsigned add_lexical_bound(unsigned bindings, unsigned var)
{
    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&var);

    unsigned binding = alloc_cons(var, 0);
    gc_protect(&binding);
    return alloc_cons(binding, bindings);
}

static unsigned add_lambda_bounds(unsigned bindings, unsigned params)
{
    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&params);

    unsigned p = params;
    gc_protect(&p);
    for (; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        unsigned param = IS_PAIR(p) ? car(p) : p;
        if (IS_ATOM(param))
            bindings = add_lexical_bound(bindings, param);
        if (!IS_PAIR(p))
            break;
    }
    gc_unprotect(1);

    return bindings;
}

static unsigned add_let_bounds(unsigned bindings, unsigned binding_list)
{
    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&binding_list);

    unsigned binding_cursor = binding_list;
    gc_protect(&binding_cursor);
    for (; IS_PAIR(binding_cursor); binding_cursor = cdr(binding_cursor)) {
        unsigned binding = car(binding_cursor);
        if (!let_binding_has_value(binding))
            continue;
        unsigned var = car(binding);
        if (IS_ATOM(var))
            bindings = add_lexical_bound(bindings, var);
    }
    gc_unprotect(1);

    return bindings;
}

static unsigned add_named_let_bounds(unsigned bindings, unsigned name,
                                     unsigned binding_list)
{
    GC_GUARD;
    gc_protect(&bindings);
    gc_protect(&name);
    gc_protect(&binding_list);

    if (IS_ATOM(name))
        bindings = add_lexical_bound(bindings, name);
    return add_let_bounds(bindings, binding_list);
}

static unsigned collect_free_ids_let(unsigned tmpl, unsigned bindings,
                                     unsigned collected, int64_t ellipsis_id,
                                     bool is_letstar, bool is_letrec,
                                     unsigned closure_env)
{
    unsigned binding_list = cadr(tmpl);
    unsigned body = cddr(tmpl);

    GC_GUARD;
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&bindings);
    gc_protect(&collected);

    if (!proper_list_silent(binding_list))
        return collect_free_ids(body, bindings, collected, ellipsis_id,
                                closure_env);

    unsigned body_bindings = bindings;
    gc_protect(&body_bindings);
    if (is_letrec)
        body_bindings = add_let_bounds(body_bindings, binding_list);

    unsigned binding_cursor = binding_list;
    gc_protect(&binding_cursor);
    for (; IS_PAIR(binding_cursor); binding_cursor = cdr(binding_cursor)) {
        unsigned binding = car(binding_cursor);
        if (!let_binding_has_value(binding))
            continue;

        unsigned val = cadr(binding);
        unsigned val_bindings =
            (is_letstar || is_letrec) ? body_bindings : bindings;
        collected =
            collect_free_ids(val, val_bindings, collected, ellipsis_id,
                             closure_env);

        if (is_letstar && !is_letrec && IS_ATOM(car(binding)))
            body_bindings = add_lexical_bound(body_bindings, car(binding));
    }
    gc_unprotect(1);

    if (!is_letstar && !is_letrec)
        body_bindings = add_let_bounds(body_bindings, binding_list);

    return collect_free_ids(body, body_bindings, collected, ellipsis_id,
                            closure_env);
}

static unsigned collect_free_ids_named_let(unsigned tmpl, unsigned bindings,
                                           unsigned collected,
                                           int64_t ellipsis_id,
                                           unsigned closure_env)
{
    unsigned name = cadr(tmpl);
    unsigned binding_list = caddr(tmpl);
    unsigned body = cdddr(tmpl);

    GC_GUARD;
    gc_protect(&name);
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&bindings);
    gc_protect(&collected);

    if (!proper_list_silent(binding_list))
        return collect_free_ids(body, bindings, collected, ellipsis_id,
                                closure_env);

    unsigned bl = binding_list;
    gc_protect(&bl);
    for (; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding))
            continue;
        collected =
            collect_free_ids(cadr(binding), bindings, collected, ellipsis_id,
                             closure_env);
    }
    gc_unprotect(1);

    unsigned body_bindings = add_named_let_bounds(bindings, name, binding_list);
    gc_protect(&body_bindings);
    return collect_free_ids(body, body_bindings, collected, ellipsis_id,
                            closure_env);
}

static unsigned collect_free_ids_quasiquote(unsigned tmpl, unsigned bindings,
                                            unsigned collected,
                                            int64_t ellipsis_id,
                                            unsigned depth,
                                            unsigned closure_env)
{
    if (!tmpl)
        return collected;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
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
                    return collect_free_ids(body, bindings, collected,
                                            ellipsis_id, closure_env);
                }
                return collect_free_ids_quasiquote(
                    body, bindings, collected, ellipsis_id, depth - 1,
                    closure_env);
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl))) {
                return collect_free_ids_quasiquote(
                    cadr(tmpl), bindings, collected, ellipsis_id, depth + 1,
                    closure_env);
            }
        }

        // A quasiquoted datum may be a wide, flat list.  Preserve the
        // special unquote handling while avoiding one recursive call per cdr.
        for (;;) {
            collected = collect_free_ids_quasiquote(
                car(tmpl), bindings, collected, ellipsis_id, depth,
                closure_env);
            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next))
                return collect_free_ids_quasiquote(
                    next, bindings, collected, ellipsis_id, depth,
                    closure_env);
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_unquote ||
                    next_id == ctx.kw_unquote_splicing ||
                    next_id == ctx.kw_quasiquote)
                    return collect_free_ids_quasiquote(
                        next, bindings, collected, ellipsis_id, depth,
                        closure_env);
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            collected = collect_free_ids_quasiquote(
                data[i], bindings, collected, ellipsis_id, depth, closure_env);
        }
    }

    return collected;
}

static unsigned collect_free_ids(unsigned tmpl, unsigned bindings,
                                 unsigned collected, int64_t ellipsis_id,
                                 unsigned closure_env)
{
    if (!tmpl)
        return collected;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    gc_protect(&collected);

    if (IS_ATOM(tmpl)) {
        int64_t id = CELL_ID(tmpl);
        // Skip pattern variables, ellipsis, or already collected ids. Core
        // keyword names are still collected so lexical keyword bindings in the
        // macro definition environment remain referentially transparent.
        if (!is_pattern_var(id, bindings) && id != ellipsis_id &&
            !atom_list_contains_id(collected, id)) {
            // Add to collected list
            return alloc_cons(tmpl, collected);
        }
        return collected;
    }

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        // Skip protected wrappers and syntax-rules (they'll be processed later)
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if (head_id == ctx.kw_protected) {
                // In a nested macro's template, an identifier the outer
                // macro substituted is still wrapped. It came from the outer
                // macro's use site - this macro's definition site - so it is
                // resolved there like any free identifier: (+ x v) with v
                // the user's y must not see a y bound where this macro is
                // used. A wrapped compound is input data, left alone.
                unsigned payload = protected_payload(tmpl);
                if (IS_ATOM(payload) && identifier_valid(payload) &&
                    !is_pattern_var(CELL_ID(payload), bindings) &&
                    CELL_ID(payload) != ellipsis_id &&
                    !atom_list_contains_id(collected, CELL_ID(payload)))
                    return alloc_cons(payload, collected);
                return collected;
            }
            if (syntax_rules_form_like(tmpl) &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env))
                return collected;
            // For quote, skip the quoted data
            if (head_id == ctx.kw_quote &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env)) {
                return collected;
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl)) &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env)) {
                return collect_free_ids_quasiquote(
                    cadr(tmpl), bindings, collected, ellipsis_id, 1,
                    closure_env);
            }
            if (head_id == ctx.kw_lambda &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env)) {
                if (!IS_PAIR(cdr(tmpl)) || !IS_PAIR(cddr(tmpl)))
                    return collected;
                unsigned body_bindings =
                    add_lambda_bounds(bindings, cadr(tmpl));
                gc_protect(&body_bindings);
                return collect_free_ids(cddr(tmpl), body_bindings, collected,
                                        ellipsis_id, closure_env);
            }
            if ((head_id == ctx.kw_let || head_id == ctx.kw_letstar ||
                 head_id == ctx.kw_letrec) &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env)) {
                if (!IS_PAIR(cdr(tmpl)) || !IS_PAIR(cddr(tmpl)))
                    return collected;
                if (head_id == ctx.kw_let && IS_ATOM(cadr(tmpl))) {
                    return collect_free_ids_named_let(
                        tmpl, bindings, collected, ellipsis_id, closure_env);
                }
                return collect_free_ids_let(
                    tmpl, bindings, collected, ellipsis_id,
                    head_id == ctx.kw_letstar, head_id == ctx.kw_letrec,
                    closure_env);
            }
            if (head_id == ctx.kw_set && set_form_like(tmpl) &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env)) {
                collected = collect_free_ids(cadr(tmpl), bindings, collected,
                                             ellipsis_id, closure_env);
                gc_protect(&collected);
                unsigned expr = cddr(tmpl);
                if (IS_PAIR(expr)) {
                    return collect_free_ids(car(expr), bindings, collected,
                                            ellipsis_id, closure_env);
                }
                return collected;
            }
            // For define, collect the target too. An introduced define target
            // must be renamed with introduced references to preserve hygiene.
            if (head_id == ctx.kw_define && define_form_like(tmpl) &&
                !binding_shadows_special(head_id, bindings) &&
                !keyword_bound_at_definition(head_id, closure_env)) {
                unsigned target = cadr(tmpl);
                if (IS_ATOM(target)) {
                    collected = collect_free_ids(target, bindings, collected,
                                                 ellipsis_id, closure_env);
                    gc_protect(&collected);
                }
                unsigned expr = cddr(tmpl);
                if (IS_PAIR(expr)) {
                    return collect_free_ids(car(expr), bindings, collected,
                                            ellipsis_id, closure_env);
                }
                return collected;
            }
        }
        if (template_case_form(tmpl)) {
            // Skip the clauses' datum lists; see map_case_form.
            collected = collect_free_ids(car(tmpl), bindings, collected,
                                         ellipsis_id, closure_env);
            collected = collect_free_ids(cadr(tmpl), bindings, collected,
                                         ellipsis_id, closure_env);
            unsigned c = cddr(tmpl);
            gc_protect(&c);
            for (; c; c = cdr(c)) {
                if (IS_PAIR(car(c)))
                    collected = collect_free_ids(cdr(car(c)), bindings,
                                                 collected, ellipsis_id,
                                                 closure_env);
            }
            return collected;
        }
        // Traverse ordinary list spines iteratively.  Macro templates may
        // contain very large flat executable forms (for example a begin with
        // many expressions), for which recursive cdr traversal overflows the
        // C stack.
        for (;;) {
            collected = collect_free_ids(car(tmpl), bindings, collected,
                                         ellipsis_id, closure_env);
            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next))
                return collect_free_ids(next, bindings, collected,
                                        ellipsis_id, closure_env);

            // Keep specialized handling for a tail that begins a syntactic
            // form; otherwise it is ordinary list data and can stay in this
            // loop.
            if (IS_ATOM(car(next)) && is_special_form(CELL_ID(car(next))))
                return collect_free_ids(next, bindings, collected,
                                        ellipsis_id, closure_env);
            tmpl = next;
        }
    }

    // A vector is self-evaluating data: nothing in it refers to a binding.

    return collected;
}

// Rename free identifiers in template according to rename_map
// rename_map is ((old_atom . new_gensym) ...)
static unsigned rename_free_ids(unsigned tmpl, unsigned rename_map);

static bool id_in_rename_map(int64_t id, unsigned rename_map)
{
    for (unsigned m = rename_map; m; m = cdr(m)) {
        unsigned entry = car(m);
        if (IS_PAIR(entry) && IS_ATOM(car(entry)) && CELL_ID(car(entry)) == id)
            return true;
    }
    return false;
}

static unsigned rename_free_ids_quasiquote(unsigned tmpl, unsigned rename_map,
                                           unsigned depth)
{
    if (!tmpl || !rename_map)
        return tmpl;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&rename_map);

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if ((head_id == ctx.kw_unquote ||
                 head_id == ctx.kw_unquote_splicing) &&
                IS_PAIR(cdr(tmpl))) {
                unsigned body = cadr(tmpl);
                unsigned new_body;
                if (depth == 1)
                    new_body = rename_free_ids(body, rename_map);
                else
                    new_body = rename_free_ids_quasiquote(body, rename_map,
                                                          depth - 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl))) {
                unsigned new_body = rename_free_ids_quasiquote(
                    cadr(tmpl), rename_map, depth + 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
        }

        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car =
                rename_free_ids_quasiquote(old_car, rename_map, depth);
            if (new_car == TOK_ERROR)
                return TOK_ERROR;
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
                    rename_free_ids_quasiquote(next, rename_map, depth);
                if (new_tail == TOK_ERROR)
                    return TOK_ERROR;
                if (new_tail != cdr(tmpl))
                    changed = true;
                if (!changed)
                    return original;
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                return result;
            }
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_unquote ||
                    next_id == ctx.kw_unquote_splicing ||
                    next_id == ctx.kw_quasiquote) {
                    unsigned new_tail = rename_free_ids_quasiquote(
                        next, rename_map, depth);
                    if (new_tail == TOK_ERROR)
                        return TOK_ERROR;
                    if (new_tail != cdr(tmpl))
                        changed = true;
                    if (!changed)
                        return original;
                    cell_set_cdr(result_tail, new_tail);
                    return result;
                }
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        bool changed = false;
        for (unsigned i = 0; i < len && !changed; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned renamed =
                rename_free_ids_quasiquote(orig, rename_map, depth);
            if (renamed == TOK_ERROR)
                return TOK_ERROR;
            if (renamed != orig)
                changed = true;
            gc_unprotect(1);
        }
        if (!changed)
            return tmpl;

        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_vec);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            unsigned elem = data[i];
            unsigned renamed =
                rename_free_ids_quasiquote(elem, rename_map, depth);
            if (renamed == TOK_ERROR)
                return TOK_ERROR;
            // Through the write barrier: new_vec may have been promoted by a
            // collection during the recursive call, and a raw store would hide
            // this nursery element from the next minor GC.
            vector_set_elem(new_vec, i, renamed);
        }
        return new_vec;
    }

    return tmpl;
}

static unsigned rename_free_ids_inner(unsigned tmpl, unsigned rename_map,
                                      bool quote_shadowed,
                                      bool quasiquote_shadowed);

static unsigned rename_free_ids(unsigned tmpl, unsigned rename_map)
{
    return rename_free_ids_inner(tmpl, rename_map, false, false);
}

// Drop rename_map entries whose key is bound by params: within this
// lambda's body those names denote the parameters, not the free
// identifiers the renames were minted for.
static unsigned drop_renames_bound_by_params(unsigned rename_map,
                                             unsigned params)
{
    GC_GUARD;
    gc_protect(&rename_map);
    gc_protect(&params);
    unsigned out = 0;
    unsigned tail = 0;
    gc_protect(&out);
    gc_protect(&tail);
    unsigned m = rename_map;
    gc_protect(&m);
    for (; m; m = cdr(m)) {
        unsigned entry = car(m);
        if (IS_ATOM(car(entry)) &&
            lambda_params_bind_id(params, CELL_ID(car(entry))))
            continue;
        list_append(&out, &tail, entry);
    }
    gc_unprotect(1);
    return out;
}

static unsigned drop_renames_bound_by_binding_list(unsigned rename_map,
                                                   unsigned binding_list)
{
    GC_GUARD;
    gc_protect(&rename_map);
    gc_protect(&binding_list);
    unsigned out = 0;
    unsigned tail = 0;
    gc_protect(&out);
    gc_protect(&tail);
    unsigned m = rename_map;
    gc_protect(&m);
    for (; m; m = cdr(m)) {
        unsigned entry = car(m);
        if (IS_ATOM(car(entry)) &&
            binding_list_binds_id(binding_list, CELL_ID(car(entry))))
            continue;
        list_append(&out, &tail, entry);
    }
    gc_unprotect(1);
    return out;
}

struct rename_free_ids_arg {
    unsigned rename_map;
    bool quote_shadowed;
    bool quasiquote_shadowed;
};

static unsigned rename_free_ids_walk(unsigned form, void *arg)
{
    struct rename_free_ids_arg *a = arg;
    return rename_free_ids_inner(form, a->rename_map, a->quote_shadowed,
                                 a->quasiquote_shadowed);
}

static unsigned rename_free_ids_inner(unsigned tmpl, unsigned rename_map,
                                      bool quote_shadowed,
                                      bool quasiquote_shadowed)
{
    if (!tmpl || !rename_map)
        return tmpl;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&rename_map);

    if (IS_ATOM(tmpl)) {
        int64_t id = CELL_ID(tmpl);
        // Look up in rename_map
        for (unsigned m = rename_map; m; m = cdr(m)) {
            unsigned entry = car(m);
            if (IS_ATOM(car(entry)) && CELL_ID(car(entry)) == id) {
                return cdr(entry); // Return the gensym
            }
        }
        return tmpl;
    }

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        // Skip protected wrappers and syntax-rules
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            bool head_is_definition_alias =
                id_in_rename_map(head_id, rename_map);
            if (head_id == ctx.kw_protected) {
                // A wrapped identifier collected by collect_free_ids: rename
                // it inside its wrapper, so hygienize still leaves it alone.
                unsigned payload = protected_payload(tmpl);
                if (!IS_ATOM(payload) ||
                    !id_in_rename_map(CELL_ID(payload), rename_map))
                    return tmpl;
                unsigned renamed =
                    rename_free_ids_inner(payload, rename_map, quote_shadowed,
                                          quasiquote_shadowed);
                gc_protect(&renamed);
                return alloc_cons(car(tmpl), renamed);
            }
            if (syntax_rules_form_like(tmpl) && !head_is_definition_alias)
                return tmpl;
            if (head_id == ctx.kw_quote && !quote_shadowed &&
                !head_is_definition_alias) {
                return tmpl;
            }
            if (head_id == ctx.kw_lambda && !head_is_definition_alias &&
                IS_PAIR(cdr(tmpl)) && IS_PAIR(cddr(tmpl))) {
                bool body_quote_shadowed =
                    quote_shadowed ||
                    lambda_params_bind_id(cadr(tmpl), ctx.kw_quote);
                bool body_quasiquote_shadowed =
                    quasiquote_shadowed ||
                    lambda_params_bind_id(cadr(tmpl), ctx.kw_quasiquote);
                unsigned body_map =
                    drop_renames_bound_by_params(rename_map, cadr(tmpl));
                gc_protect(&body_map);
                unsigned new_body = rename_free_ids_inner(
                    cddr(tmpl), body_map, body_quote_shadowed,
                    body_quasiquote_shadowed);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                if (new_body == cddr(tmpl))
                    return tmpl;
                unsigned result_tail = alloc_cons(cadr(tmpl), new_body);
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
            if ((head_id == ctx.kw_let || head_id == ctx.kw_letstar ||
                 head_id == ctx.kw_letrec) &&
                !head_is_definition_alias &&
                IS_PAIR(cdr(tmpl)) && IS_PAIR(cddr(tmpl)) &&
                !(head_id == ctx.kw_let && IS_ATOM(cadr(tmpl))) &&
                proper_list_silent(cadr(tmpl))) {
                unsigned binding_list = cadr(tmpl);
                unsigned body = cddr(tmpl);
                unsigned new_bindings = rename_free_ids_inner(
                    binding_list, rename_map, quote_shadowed,
                    quasiquote_shadowed);
                if (new_bindings == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_bindings);
                bool body_quote_shadowed =
                    quote_shadowed ||
                    binding_list_binds_id(binding_list, ctx.kw_quote);
                bool body_quasiquote_shadowed =
                    quasiquote_shadowed ||
                    binding_list_binds_id(binding_list, ctx.kw_quasiquote);
                unsigned body_map =
                    drop_renames_bound_by_binding_list(rename_map,
                                                       binding_list);
                gc_protect(&body_map);
                unsigned new_body = rename_free_ids_inner(
                    body, body_map, body_quote_shadowed,
                    body_quasiquote_shadowed);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                if (new_bindings == binding_list && new_body == body)
                    return tmpl;
                unsigned result_tail = alloc_cons(new_bindings, new_body);
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl)) &&
                !quasiquote_shadowed && !head_is_definition_alias) {
                unsigned new_body =
                    rename_free_ids_quasiquote(cadr(tmpl), rename_map, 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
            if (head_id == ctx.kw_set && set_form_like(tmpl) &&
                !head_is_definition_alias) {
                unsigned var = cadr(tmpl);
                unsigned expr = cddr(tmpl);
                unsigned new_var =
                    rename_free_ids_inner(var, rename_map, quote_shadowed,
                                          quasiquote_shadowed);
                if (new_var == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_var);
                if (expr && IS_PAIR(expr)) {
                    unsigned new_expr = rename_free_ids_inner(
                        car(expr), rename_map, quote_shadowed,
                        quasiquote_shadowed);
                    if (new_expr == TOK_ERROR)
                        return TOK_ERROR;
                    gc_protect(&new_expr);
                    unsigned expr_tail = alloc_cons(new_expr, 0);
                    gc_protect(&expr_tail);
                    unsigned var_tail = alloc_cons(new_var, expr_tail);
                    gc_protect(&var_tail);
                    return alloc_cons(head, var_tail);
                }
                return tmpl;
            }
            // For define, rename the target too. The later hygienize pass
            // decides whether this is an introduced binding or a pattern
            // variable binding.
            if (head_id == ctx.kw_define && define_form_like(tmpl) &&
                !head_is_definition_alias) {
                unsigned var = cadr(tmpl);
                unsigned expr = cddr(tmpl);
                unsigned new_var =
                    rename_free_ids_inner(var, rename_map, quote_shadowed,
                                          quasiquote_shadowed);
                if (new_var == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_var);
                if (expr && IS_PAIR(expr)) {
                    unsigned new_expr = rename_free_ids_inner(
                        car(expr), rename_map, quote_shadowed,
                        quasiquote_shadowed);
                    if (new_expr == TOK_ERROR)
                        return TOK_ERROR;
                    gc_protect(&new_expr);
                    unsigned expr_tail = alloc_cons(new_expr, 0);
                    gc_protect(&expr_tail);
                    unsigned var_tail = alloc_cons(new_var, expr_tail);
                    gc_protect(&var_tail);
                    return alloc_cons(head, var_tail);
                }
                return tmpl;
            }
        }
        if (template_case_form(tmpl)) {
            struct rename_free_ids_arg arg = {rename_map, quote_shadowed,
                                              quasiquote_shadowed};
            gc_protect(&arg.rename_map);
            return map_case_form(tmpl, rename_free_ids_walk, &arg);
        }
        // Ordinary template lists can be very wide.  Walking their cdrs
        // recursively makes an otherwise shallow macro expansion consume one
        // C stack frame per element (notably for a large introduced begin).
        // Build the renamed spine iteratively, while delegating a tail that
        // starts a syntactic form to the existing specialized handling.
        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car = rename_free_ids_inner(
                old_car, rename_map, quote_shadowed, quasiquote_shadowed);
            if (new_car == TOK_ERROR)
                return TOK_ERROR;
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
                unsigned new_tail = rename_free_ids_inner(
                    next, rename_map, quote_shadowed, quasiquote_shadowed);
                if (new_tail == TOK_ERROR)
                    return TOK_ERROR;
                if (new_tail != cdr(tmpl))
                    changed = true;
                if (!changed)
                    return original;
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                return result;
            }

            if (IS_ATOM(car(next)) && is_special_form(CELL_ID(car(next)))) {
                unsigned new_tail = rename_free_ids_inner(
                    next, rename_map, quote_shadowed, quasiquote_shadowed);
                if (new_tail == TOK_ERROR)
                    return TOK_ERROR;
                if (new_tail != cdr(tmpl))
                    changed = true;
                if (!changed)
                    return original;
                cell_set_cdr(result_tail, new_tail);
                return result;
            }
            tmpl = next;
        }
    }

    // Vectors are self-evaluating data; see rename_in_template.
    return tmpl;
}

// Rename all occurrences of old_id to new_sym in template
// Returns a new template with substitutions made
static unsigned rename_in_template(unsigned tmpl, int64_t old_id,
                                   unsigned new_sym);
static unsigned rename_in_syntax_rules(unsigned tmpl, int64_t old_id,
                                       unsigned new_sym);

static bool pattern_binds_id(unsigned pattern, int64_t old_id,
                             unsigned literals, int64_t ellipsis_id)
{
    if (!pattern)
        return false;

    if (IS_ATOM(pattern)) {
        int64_t id = CELL_ID(pattern);
        return identifier_valid(pattern) && id == old_id &&
               id != ellipsis_id && !syntax_is_underscore(pattern) &&
               !atom_list_contains_id(literals, id);
    }

    if (IS_PAIR(pattern)) {
        while (IS_PAIR(pattern)) {
            if (pattern_binds_id(car(pattern), old_id, literals, ellipsis_id))
                return true;
            pattern = cdr(pattern);
        }
        return pattern_binds_id(pattern, old_id, literals, ellipsis_id);
    }

    if (IS_VECTOR(pattern)) {
        unsigned len = vector_len(pattern);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(pattern);
            if (pattern_binds_id(data[i], old_id, literals, ellipsis_id))
                return true;
        }
    }

    return false;
}

static bool syntax_rule_pattern_binds_id(unsigned pattern, int64_t old_id,
                                         unsigned literals,
                                         int64_t ellipsis_id)
{
    if (IS_PAIR(pattern))
        return pattern_binds_id(cdr(pattern), old_id, literals, ellipsis_id);
    return pattern_binds_id(pattern, old_id, literals, ellipsis_id);
}

static unsigned rename_in_quasiquote(unsigned tmpl, int64_t old_id,
                                     unsigned new_sym, unsigned depth)
{
    if (!tmpl)
        return 0;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&new_sym);

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if ((head_id == ctx.kw_unquote ||
                 head_id == ctx.kw_unquote_splicing) &&
                IS_PAIR(cdr(tmpl))) {
                unsigned body = cadr(tmpl);
                unsigned new_body;
                if (depth == 1)
                    new_body = rename_in_template(body, old_id, new_sym);
                else
                    new_body =
                        rename_in_quasiquote(body, old_id, new_sym, depth - 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
            if (head_id == ctx.kw_quasiquote && head_id != old_id &&
                IS_PAIR(cdr(tmpl))) {
                unsigned new_body =
                    rename_in_quasiquote(cadr(tmpl), old_id, new_sym,
                                         depth + 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
        }

        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car =
                rename_in_quasiquote(old_car, old_id, new_sym, depth);
            if (new_car == TOK_ERROR)
                return TOK_ERROR;
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
                    rename_in_quasiquote(next, old_id, new_sym, depth);
                if (new_tail == TOK_ERROR)
                    return TOK_ERROR;
                if (new_tail != cdr(tmpl))
                    changed = true;
                if (!changed)
                    return original;
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                return result;
            }
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_unquote ||
                    next_id == ctx.kw_unquote_splicing ||
                    next_id == ctx.kw_quasiquote) {
                    unsigned new_tail = rename_in_quasiquote(
                        next, old_id, new_sym, depth);
                    if (new_tail == TOK_ERROR)
                        return TOK_ERROR;
                    if (new_tail != cdr(tmpl))
                        changed = true;
                    if (!changed)
                        return original;
                    cell_set_cdr(result_tail, new_tail);
                    return result;
                }
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        bool changed = false;
        for (unsigned i = 0; i < len && !changed; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned renamed =
                rename_in_quasiquote(orig, old_id, new_sym, depth);
            if (renamed == TOK_ERROR)
                return TOK_ERROR;
            if (renamed != orig)
                changed = true;
            gc_unprotect(1);
        }
        if (!changed)
            return tmpl;

        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_vec);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            unsigned elem = data[i];
            unsigned renamed =
                rename_in_quasiquote(elem, old_id, new_sym, depth);
            if (renamed == TOK_ERROR)
                return TOK_ERROR;
            // Through the write barrier: new_vec may have been promoted by a
            // collection during the recursive call, and a raw store would hide
            // this nursery element from the next minor GC.
            vector_set_elem(new_vec, i, renamed);
        }
        return new_vec;
    }

    return tmpl;
}

struct rename_in_template_arg {
    int64_t old_id;
    unsigned new_sym;
};

static unsigned rename_in_template_walk(unsigned form, void *arg)
{
    struct rename_in_template_arg *a = arg;
    return rename_in_template(form, a->old_id, a->new_sym);
}

static unsigned rename_in_template(unsigned tmpl, int64_t old_id,
                                   unsigned new_sym)
{
    if (!tmpl)
        return 0;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&new_sym);

    if (IS_ATOM(tmpl)) {
        if (CELL_ID(tmpl) == old_id)
            return new_sym;
        return tmpl;
    }

    if (IS_PAIR(tmpl)) {
        // Skip protected wrappers and quoted data - they should not be renamed
        // as part of an introduced binding's lexical scope.
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if (head_id == ctx.kw_protected ||
                (head_id == ctx.kw_quote && head_id != old_id)) {
                return tmpl;
            }
            if (syntax_rules_form_like(tmpl))
                return rename_in_syntax_rules(tmpl, old_id, new_sym);
            if (head_id == ctx.kw_quasiquote && head_id != old_id &&
                IS_PAIR(cdr(tmpl))) {
                unsigned new_body =
                    rename_in_quasiquote(cadr(tmpl), old_id, new_sym, 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
        }
        if (template_case_form(tmpl) &&
            !(IS_ATOM(head) && CELL_ID(head) == old_id)) {
            struct rename_in_template_arg arg = {old_id, new_sym};
            gc_protect(&arg.new_sym);
            return map_case_form(tmpl, rename_in_template_walk, &arg);
        }
        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car = rename_in_template(old_car, old_id, new_sym);
            if (new_car == TOK_ERROR)
                return TOK_ERROR;
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
                unsigned new_tail = rename_in_template(next, old_id, new_sym);
                if (new_tail == TOK_ERROR)
                    return TOK_ERROR;
                if (new_tail != cdr(tmpl))
                    changed = true;
                if (!changed)
                    return original;
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                return result;
            }
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_protected || next_id == ctx.kw_quote ||
                    next_id == ctx.kw_quasiquote ||
                    syntax_rules_form_like(next)) {
                    unsigned new_tail =
                        rename_in_template(next, old_id, new_sym);
                    if (new_tail == TOK_ERROR)
                        return TOK_ERROR;
                    if (new_tail != cdr(tmpl))
                        changed = true;
                    if (!changed)
                        return original;
                    cell_set_cdr(result_tail, new_tail);
                    return result;
                }
            }
            tmpl = next;
        }
    }

    // A vector is self-evaluating: its elements are data, never references
    // to a binding, so a binder rename must not reach into it - (let ((x 1))
    // #(x)) is #(x), not a vector holding the gensym.
    return tmpl;
}

static unsigned rename_in_syntax_rules(unsigned tmpl, int64_t old_id,
                                       unsigned new_sym)
{
    if (!IS_PAIR(tmpl) || !IS_ATOM(car(tmpl)) ||
        CELL_ID(car(tmpl)) != ctx.kw_syntax_rules)
        return tmpl;

    unsigned rest = cdr(tmpl);
    if (!IS_PAIR(rest))
        return tmpl;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&rest);
    gc_protect(&new_sym);

    unsigned prefix = 0, prefix_tail = 0;
    gc_protect(&prefix);
    gc_protect(&prefix_tail);
    list_append(&prefix, &prefix_tail, car(tmpl));

    int64_t ellipsis_id = ctx.kw_ellipsis;
    if (car(rest) && IS_ATOM(car(rest)) && IS_PAIR(cdr(rest))) {
        ellipsis_id = CELL_ID(car(rest));
        list_append(&prefix, &prefix_tail, car(rest));
        rest = cdr(rest);
    }

    if (!IS_PAIR(rest))
        return tmpl;
    list_append(&prefix, &prefix_tail, car(rest));
    unsigned literals = car(rest);
    unsigned rules = cdr(rest);
    gc_protect(&literals);

    unsigned new_rules = 0, new_rules_tail = 0;
    gc_protect(&new_rules);
    gc_protect(&new_rules_tail);

    unsigned r = rules;
    gc_protect(&r);
    for (; r; r = cdr(r)) {
        unsigned rule = car(r);
        if (!IS_PAIR(rule) || !IS_PAIR(cdr(rule))) {
            list_append(&new_rules, &new_rules_tail, rule);
            continue;
        }

        unsigned pattern = car(rule);
        unsigned template = cadr(rule);
        gc_protect(&pattern);
        gc_protect(&template);
        unsigned new_template;
        if (syntax_rule_pattern_binds_id(pattern, old_id, literals,
                                         ellipsis_id))
            new_template = template;
        else {
            new_template = rename_in_template(template, old_id, new_sym);
        }
        if (new_template == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_template);
        unsigned template_tail = alloc_cons(new_template, 0);
        gc_protect(&template_tail);
        unsigned new_rule = alloc_cons(pattern, template_tail);
        gc_protect(&new_rule);
        list_append(&new_rules, &new_rules_tail, new_rule);
        gc_unprotect(5);
    }
    gc_unprotect(1);

    cell_set_cdr(prefix_tail, new_rules);
    return prefix;
}

// Forward declaration
static unsigned hygienize_template(unsigned tmpl, unsigned bindings);
static unsigned hygienize_body_sequence(unsigned body, unsigned bindings);

static unsigned hygienize_quasiquote(unsigned tmpl, unsigned bindings,
                                     unsigned depth)
{
    if (!tmpl)
        return 0;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);
            if ((head_id == ctx.kw_unquote ||
                 head_id == ctx.kw_unquote_splicing) &&
                IS_PAIR(cdr(tmpl))) {
                unsigned body = cadr(tmpl);
                unsigned new_body;
                if (depth == 1)
                    new_body = hygienize_template(body, bindings);
                else
                    new_body =
                        hygienize_quasiquote(body, bindings, depth - 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl))) {
                unsigned new_body =
                    hygienize_quasiquote(cadr(tmpl), bindings, depth + 1);
                if (new_body == TOK_ERROR)
                    return TOK_ERROR;
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                return alloc_cons(head, result_tail);
            }
        }

        unsigned original = tmpl;
        unsigned result = 0, result_tail = 0;
        bool changed = false;
        gc_protect(&original);
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned old_car = car(tmpl);
            unsigned new_car =
                hygienize_quasiquote(old_car, bindings, depth);
            if (new_car == TOK_ERROR)
                return TOK_ERROR;
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
                    hygienize_quasiquote(next, bindings, depth);
                if (new_tail == TOK_ERROR)
                    return TOK_ERROR;
                if (new_tail != cdr(tmpl))
                    changed = true;
                if (!changed)
                    return original;
                if (new_tail)
                    cell_set_cdr(result_tail, new_tail);
                return result;
            }
            if (IS_ATOM(car(next))) {
                int64_t next_id = CELL_ID(car(next));
                if (next_id == ctx.kw_unquote ||
                    next_id == ctx.kw_unquote_splicing ||
                    next_id == ctx.kw_quasiquote) {
                    unsigned new_tail =
                        hygienize_quasiquote(next, bindings, depth);
                    if (new_tail == TOK_ERROR)
                        return TOK_ERROR;
                    if (new_tail != cdr(tmpl))
                        changed = true;
                    if (!changed)
                        return original;
                    cell_set_cdr(result_tail, new_tail);
                    return result;
                }
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        bool changed = false;
        for (unsigned i = 0; i < len && !changed; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned h = hygienize_quasiquote(orig, bindings, depth);
            if (h == TOK_ERROR)
                return TOK_ERROR;
            if (h != orig)
                changed = true;
            gc_unprotect(1);
        }
        if (!changed)
            return tmpl;

        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_vec);
        for (unsigned i = 0; i < len; i++) {
            unsigned *data = vector_data_ptr(tmpl);
            unsigned elem = data[i];
            unsigned hygienized = hygienize_quasiquote(elem, bindings, depth);
            if (hygienized == TOK_ERROR)
                return TOK_ERROR;
            // Through the write barrier: new_vec may have been promoted by a
            // collection during the recursive call, and a raw store would hide
            // this nursery element from the next minor GC.
            vector_set_elem(new_vec, i, hygienized);
        }
        return new_vec;
    }

    return tmpl;
}

// Hygienize a let-style binding form: (let ((var val) ...) body ...)
// Renames introduced bindings (not pattern variables) to gensyms
static unsigned hygienize_let(unsigned tmpl, unsigned bindings, bool is_letrec)
{
    // tmpl = (let ((var val) ...) body ...)
    unsigned keyword = car(tmpl);
    unsigned binding_list = cadr(tmpl);
    unsigned body = cddr(tmpl);

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    gc_protect(&keyword);
    gc_protect(&binding_list);
    gc_protect(&body);

    if (!proper_list_silent(binding_list))
        return tmpl;

    // For letrec, we need to rename in all vals too (they can reference each
    // other) Collect all introduced bindings first
    unsigned renames = 0; // list of (old_id . new_sym)
    gc_protect(&renames);

    // First pass: identify introduced bindings and create gensyms
    // Skip ellipsis (...) which appears in macro templates
    unsigned bl = binding_list;
    gc_protect(&bl);
    while (IS_PAIR(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding))
            goto next_intro_binding;
        // Skip ellipsis in binding list
        if (IS_ATOM(binding) && CELL_ID(binding) == ctx.kw_ellipsis)
            goto next_intro_binding;
        unsigned var = car(binding);
        if (identifier_valid(var)) {
            int64_t var_id = CELL_ID(var);
            // Skip ellipsis
            if (var_id == ctx.kw_ellipsis)
                goto next_intro_binding;
            if (!is_pattern_var(var_id, bindings)) {
                // This is an introduced binding - generate gensym
                gc_protect(&var);
                unsigned new_sym = do_gensym();
                gc_protect(&new_sym);
                unsigned rename_pair = alloc_cons(var, new_sym);
                gc_protect(&rename_pair);
                renames = alloc_cons(rename_pair, renames);
                gc_unprotect(3);
            }
        }
next_intro_binding:
        bl = cdr(bl);
    }
    gc_unprotect(1);

    if (!renames) {
        // No introduced bindings - just hygienize nested templates
        unsigned new_bindings = 0, new_bindings_tail = 0;
        gc_protect(&new_bindings);
        gc_protect(&new_bindings_tail);

        bl = binding_list;
        gc_protect(&bl);
        while (IS_PAIR(bl)) {
            unsigned binding = car(bl);
            if (!let_binding_has_value(binding)) {
                list_append(&new_bindings, &new_bindings_tail, binding);
                bl = cdr(bl);
                continue;
            }
            unsigned var = car(binding);
            unsigned val = cadr(binding);
            gc_protect(&var);
            gc_protect(&val);
            unsigned new_val = hygienize_template(val, bindings);
            if (new_val == TOK_ERROR)
                return TOK_ERROR;
            gc_protect(&new_val);
            unsigned binding_tail = alloc_cons(new_val, 0);
            gc_protect(&binding_tail);
            unsigned new_binding = alloc_cons(var, binding_tail);
            gc_protect(&new_binding);
            list_append(&new_bindings, &new_bindings_tail, new_binding);
            gc_unprotect(5);
            bl = cdr(bl);
        }
        gc_unprotect(1);

        unsigned new_body = hygienize_body_sequence(body, bindings);
        gc_protect(&new_body);
        unsigned result_tail = alloc_cons(new_bindings, new_body);
        gc_protect(&result_tail);
        unsigned result = alloc_cons(keyword, result_tail);
        gc_unprotect(10);
        return result;
    }

    // Apply renames to binding list and body
    unsigned new_binding_list = binding_list;
    unsigned new_body = body;
    gc_protect(&new_binding_list);
    gc_protect(&new_body);

    unsigned r = renames;
    gc_protect(&r);
    while (r) {
        unsigned rename = car(r);
        unsigned old_sym = car(rename);
        unsigned new_sym = cdr(rename);
        int64_t old_id = CELL_ID(old_sym);
        gc_protect(&new_sym);

        if (is_letrec) {
            // For letrec, rename in all binding vals
            new_binding_list =
                rename_in_template(new_binding_list, old_id, new_sym);
            if (new_binding_list == TOK_ERROR)
                return TOK_ERROR;
        } else {
            // For let/let*, only rename var in binding, body sees the rename
            unsigned renamed_bindings = 0, renamed_tail = 0;
            gc_protect(&renamed_bindings);
            gc_protect(&renamed_tail);
            bl = new_binding_list;
            gc_protect(&bl);
            while (IS_PAIR(bl)) {
                unsigned binding = car(bl);
                if (!let_binding_has_value(binding)) {
                    list_append(&renamed_bindings, &renamed_tail, binding);
                    bl = cdr(bl);
                    continue;
                }
                unsigned var = car(binding);
                unsigned val = cadr(binding);
                unsigned new_var = var;
                if (IS_ATOM(var) && CELL_ID(var) == old_id)
                    new_var = new_sym;
                gc_protect(&new_var);
                gc_protect(&val);
                unsigned bind_tail = alloc_cons(val, 0);
                gc_protect(&bind_tail);
                unsigned new_bind = alloc_cons(new_var, bind_tail);
                gc_protect(&new_bind);
                list_append(&renamed_bindings, &renamed_tail, new_bind);
                gc_unprotect(4);
                bl = cdr(bl);
            }
            gc_unprotect(3);
            new_binding_list = renamed_bindings;
        }
        new_body = rename_in_template(new_body, old_id, new_sym);
        if (new_body == TOK_ERROR)
            return TOK_ERROR;
        gc_unprotect(1);
        r = cdr(r);
    }
    gc_unprotect(1);

    // Now hygienize nested templates in the binding values
    unsigned final_bindings = 0, final_tail = 0;
    gc_protect(&final_bindings);
    gc_protect(&final_tail);

    bl = new_binding_list;
    gc_protect(&bl);
    while (IS_PAIR(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding)) {
            list_append(&final_bindings, &final_tail, binding);
            bl = cdr(bl);
            continue;
        }
        unsigned var = car(binding);
        unsigned val = cadr(binding);
        gc_protect(&var);
        gc_protect(&val);
        unsigned new_val = hygienize_template(val, bindings);
        if (new_val == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_val);
        unsigned binding_tail = alloc_cons(new_val, 0);
        gc_protect(&binding_tail);
        unsigned new_binding = alloc_cons(var, binding_tail);
        gc_protect(&new_binding);
        list_append(&final_bindings, &final_tail, new_binding);
        gc_unprotect(5);
        bl = cdr(bl);
    }
    gc_unprotect(1);

    unsigned final_body = hygienize_body_sequence(new_body, bindings);
    if (final_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&final_body);
    unsigned result_tail = alloc_cons(final_bindings, final_body);
    gc_protect(&result_tail);
    unsigned result = alloc_cons(keyword, result_tail);
    gc_unprotect(12); // 5 initial + renames + new_binding_list + new_body +
                      // final_bindings + final_tail + final_body + result_tail
    return result;
}

// (let* (b1 b2 ...) body ...): each binder scopes over the later inits as
// well as the body, which hygienize_let's let-style renaming does not do - a
// later init kept the original name and so referred to whatever that name
// meant at the use site. Peel off the first binding instead:
//   (let* (b1) (let* (b2 ...) body ...))
// A one-binding let* scopes exactly like let, and the nested let* is in that
// let's body, so the recursion renames b1 in b2's init and every later form.
// The keyword cell is reused, so the nested forms mean whatever this let*
// meant. A binding list whose second element is not a binding (an ellipsis
// after a repeated binding template) is left to hygienize_let unsplit.
static unsigned hygienize_letstar(unsigned tmpl, unsigned bindings)
{
    unsigned binding_list = cadr(tmpl);
    if (!proper_list_silent(binding_list) || !IS_PAIR(binding_list) ||
        !IS_PAIR(cdr(binding_list)) || !let_binding_has_value(car(binding_list)) ||
        !let_binding_has_value(cadr(binding_list)))
        return hygienize_let(tmpl, bindings, false);

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    unsigned keyword = car(tmpl);
    gc_protect(&keyword);

    unsigned inner_tail = alloc_cons(cdr(binding_list), cddr(tmpl));
    gc_protect(&inner_tail);
    unsigned inner = alloc_cons(keyword, inner_tail);
    gc_protect(&inner);
    unsigned outer_body = alloc_cons(inner, 0);
    gc_protect(&outer_body);
    unsigned first = alloc_cons(car(cadr(tmpl)), 0);
    gc_protect(&first);
    unsigned outer_tail = alloc_cons(first, outer_body);
    gc_protect(&outer_tail);
    unsigned outer = alloc_cons(keyword, outer_tail);
    gc_protect(&outer);
    return hygienize_let(outer, bindings, false);
}

static unsigned hygienize_named_let(unsigned tmpl, unsigned bindings)
{
    unsigned keyword = car(tmpl);
    unsigned name = cadr(tmpl);
    unsigned binding_list = caddr(tmpl);
    unsigned body = cdddr(tmpl);

    if (!proper_list_silent(binding_list))
        return tmpl;

    GC_GUARD;
    gc_protect(&keyword);
    gc_protect(&name);
    gc_protect(&binding_list);
    gc_protect(&body);
    gc_protect(&bindings);

    unsigned renames = 0;
    gc_protect(&renames);

    if (identifier_valid(name) && CELL_ID(name) != ctx.kw_ellipsis &&
        !is_pattern_var(CELL_ID(name), bindings)) {
        unsigned new_sym = do_gensym();
        gc_protect(&new_sym);
        unsigned rename_pair = alloc_cons(name, new_sym);
        gc_protect(&rename_pair);
        renames = alloc_cons(rename_pair, renames);
        gc_unprotect(2);
    }

    unsigned binding_cursor = binding_list;
    gc_protect(&binding_cursor);
    for (; IS_PAIR(binding_cursor); binding_cursor = cdr(binding_cursor)) {
        unsigned binding = car(binding_cursor);
        if (!let_binding_has_value(binding))
            continue;
        unsigned var = car(binding);
        if (identifier_valid(var) && CELL_ID(var) != ctx.kw_ellipsis &&
            !is_pattern_var(CELL_ID(var), bindings) &&
            !assoc_list_has_atom_key_id(renames, CELL_ID(var))) {
            gc_protect(&var);
            unsigned new_sym = do_gensym();
            gc_protect(&new_sym);
            unsigned rename_pair = alloc_cons(var, new_sym);
            gc_protect(&rename_pair);
            renames = alloc_cons(rename_pair, renames);
            gc_unprotect(3);
        }
    }
    gc_unprotect(1);

    unsigned new_name = name;
    unsigned new_binding_list = binding_list;
    unsigned new_body = body;
    gc_protect(&new_name);
    gc_protect(&new_binding_list);
    gc_protect(&new_body);

    unsigned r = renames;
    gc_protect(&r);
    for (; r; r = cdr(r)) {
        unsigned rename = car(r);
        unsigned old_sym = car(rename);
        unsigned new_sym = cdr(rename);
        // new_sym is reachable only through `r`, so the allocations below
        // (alloc_cons and list_append in the binding loop) can relocate the
        // cell and leave this raw local stale - which is then spliced into
        // the rebuilt binding list and the renamed body. hygienize_let and
        // hygienize_lambda protect the same value for the same reason.
        gc_protect(&new_sym);
        int64_t old_id = CELL_ID(old_sym);

        if (IS_ATOM(new_name) && CELL_ID(new_name) == old_id)
            new_name = new_sym;

        unsigned renamed_bindings = 0, renamed_tail = 0;
        gc_protect(&renamed_bindings);
        gc_protect(&renamed_tail);
        unsigned bl = new_binding_list;
        gc_protect(&bl);
        for (; IS_PAIR(bl); bl = cdr(bl)) {
            unsigned binding = car(bl);
            if (!let_binding_has_value(binding)) {
                list_append(&renamed_bindings, &renamed_tail, binding);
                continue;
            }

            unsigned var = car(binding);
            unsigned val = cadr(binding);
            unsigned new_var = var;
            if (IS_ATOM(var) && CELL_ID(var) == old_id)
                new_var = new_sym;
            gc_protect(&new_var);
            gc_protect(&val);
            unsigned bind_tail = alloc_cons(val, 0);
            gc_protect(&bind_tail);
            unsigned new_bind = alloc_cons(new_var, bind_tail);
            gc_protect(&new_bind);
            list_append(&renamed_bindings, &renamed_tail, new_bind);
            gc_unprotect(4);
        }
        gc_unprotect(1);
        gc_unprotect(2);
        new_binding_list = renamed_bindings;

        new_body = rename_in_template(new_body, old_id, new_sym);
        if (new_body == TOK_ERROR) {
            gc_unprotect(1);
            return TOK_ERROR;
        }
        gc_unprotect(1); // new_sym
    }
    gc_unprotect(1);

    unsigned final_bindings = 0, final_tail = 0;
    gc_protect(&final_bindings);
    gc_protect(&final_tail);
    unsigned bl = new_binding_list;
    gc_protect(&bl);
    for (; IS_PAIR(bl); bl = cdr(bl)) {
        unsigned binding = car(bl);
        if (!let_binding_has_value(binding)) {
            list_append(&final_bindings, &final_tail, binding);
            continue;
        }

        unsigned var = car(binding);
        unsigned val = cadr(binding);
        gc_protect(&var);
        gc_protect(&val);
        unsigned new_val = hygienize_template(val, bindings);
        if (new_val == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_val);
        unsigned binding_tail = alloc_cons(new_val, 0);
        gc_protect(&binding_tail);
        unsigned new_binding = alloc_cons(var, binding_tail);
        gc_protect(&new_binding);
        list_append(&final_bindings, &final_tail, new_binding);
        gc_unprotect(5);
    }
    gc_unprotect(1);

    unsigned final_body = hygienize_body_sequence(new_body, bindings);
    if (final_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&final_body);
    unsigned body_tail = alloc_cons(final_bindings, final_body);
    gc_protect(&body_tail);
    unsigned name_tail = alloc_cons(new_name, body_tail);
    gc_protect(&name_tail);
    return alloc_cons(keyword, name_tail);
}

// Hygienize a lambda form: (lambda (params...) body ...)
static unsigned hygienize_lambda(unsigned tmpl, unsigned bindings)
{
    unsigned keyword = car(tmpl);
    unsigned params = cadr(tmpl);
    unsigned body = cddr(tmpl);

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    gc_protect(&keyword);
    gc_protect(&params);
    gc_protect(&body);

    // Collect renames for introduced parameters
    unsigned renames = 0;
    gc_protect(&renames);

    // Handle both proper list and dotted list params
    // Skip ellipsis (...) which appears in macro templates
    unsigned p = params;
    gc_protect(&p);
    for (; p; p = IS_PAIR(p) ? cdr(p) : 0) {
        unsigned param = IS_PAIR(p) ? car(p) : p;
        if (identifier_valid(param)) {
            int64_t param_id = CELL_ID(param);
            // Skip ellipsis - it's a macro template construct, not a real param
            if (param_id == ctx.kw_ellipsis)
                continue;
            if (!is_pattern_var(param_id, bindings)) {
                unsigned new_sym = do_gensym();
                gc_protect(&new_sym);
                unsigned rename_pair = alloc_cons(param, new_sym);
                gc_protect(&rename_pair);
                renames = alloc_cons(rename_pair, renames);
                gc_unprotect(2);
            }
        }
        if (!IS_PAIR(p))
            break;
    }
    gc_unprotect(1);

    if (!renames) {
        // No introduced params - just hygienize body
        unsigned new_body = hygienize_body_sequence(body, bindings);
        if (new_body == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&new_body);
        unsigned result_tail = alloc_cons(params, new_body);
        gc_protect(&result_tail);
        unsigned result = alloc_cons(keyword, result_tail);
        gc_unprotect(8);
        return result;
    }

    // Apply renames to params and body
    unsigned new_params = params;
    unsigned new_body = body;
    gc_protect(&new_params);
    gc_protect(&new_body);

    unsigned r = renames;
    gc_protect(&r);
    while (r) {
        unsigned rename = car(r);
        int64_t old_id = CELL_ID(car(rename));
        unsigned new_sym = cdr(rename);
        gc_protect(&new_sym);
        new_params = rename_in_template(new_params, old_id, new_sym);
        if (new_params == TOK_ERROR)
            return TOK_ERROR;
        new_body = rename_in_template(new_body, old_id, new_sym);
        if (new_body == TOK_ERROR)
            return TOK_ERROR;
        gc_unprotect(1);
        r = cdr(r);
    }
    gc_unprotect(1);

    // Hygienize body
    unsigned final_body = hygienize_body_sequence(new_body, bindings);
    if (final_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&final_body);
    unsigned result_tail = alloc_cons(new_params, final_body);
    gc_protect(&result_tail);
    unsigned result = alloc_cons(keyword, result_tail);
    gc_unprotect(10); // 5 initial + renames + new_params + new_body +
                      // final_body + result_tail
    return result;
}

static unsigned define_target_name(unsigned expr)
{
    if (!IS_PAIR(expr) || !IS_ATOM(car(expr)) ||
        CELL_ID(car(expr)) != ctx.kw_define || !IS_PAIR(cdr(expr)))
        return 0;

    unsigned target = cadr(expr);
    if (IS_ATOM(target))
        return target;
    if (IS_PAIR(target) && IS_ATOM(car(target)))
        return car(target);
    return 0;
}

static unsigned hygienize_define_no_target_rename(unsigned tmpl,
                                                  unsigned bindings)
{
    unsigned keyword = car(tmpl);
    unsigned target = cadr(tmpl);
    unsigned exprs = cddr(tmpl);

    GC_GUARD;
    gc_protect(&keyword);
    gc_protect(&target);
    gc_protect(&exprs);
    gc_protect(&bindings);

    if (!exprs)
        return tmpl;

    if (IS_PAIR(target)) {
        unsigned name = car(target);
        unsigned params = cdr(target);
        gc_protect(&name);
        gc_protect(&params);

        unsigned lambda_atom = atom_from_string("lambda");
        gc_protect(&lambda_atom);
        unsigned lambda_tail = alloc_cons(params, exprs);
        gc_protect(&lambda_tail);
        unsigned lambda_form = alloc_cons(lambda_atom, lambda_tail);
        gc_protect(&lambda_form);

        unsigned hygienic_lambda = hygienize_lambda(lambda_form, bindings);
        if (hygienic_lambda == TOK_ERROR)
            return TOK_ERROR;
        gc_protect(&hygienic_lambda);

        unsigned new_params = cadr(hygienic_lambda);
        unsigned new_exprs = cddr(hygienic_lambda);
        gc_protect(&new_params);
        gc_protect(&new_exprs);
        unsigned new_target = alloc_cons(name, new_params);
        gc_protect(&new_target);
        unsigned target_tail = alloc_cons(new_target, new_exprs);
        gc_protect(&target_tail);
        return alloc_cons(keyword, target_tail);
    }

    unsigned new_exprs = hygienize_template(exprs, bindings);
    if (new_exprs == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&new_exprs);
    unsigned target_tail = alloc_cons(target, new_exprs);
    gc_protect(&target_tail);
    return alloc_cons(keyword, target_tail);
}

static unsigned hygienize_define(unsigned tmpl, unsigned bindings)
{
    unsigned target = define_target_name(tmpl);
    if (!target)
        return hygienize_define_no_target_rename(tmpl, bindings);

    int64_t target_id = CELL_ID(target);
    if (target_id == ctx.kw_ellipsis || is_pattern_var(target_id, bindings))
        return hygienize_define_no_target_rename(tmpl, bindings);

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);
    unsigned new_sym = do_gensym();
    gc_protect(&new_sym);
    unsigned renamed = rename_in_template(tmpl, target_id, new_sym);
    if (renamed == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&renamed);
    return hygienize_define_no_target_rename(renamed, bindings);
}

static unsigned hygienize_body_sequence(unsigned body, unsigned bindings)
{
    if (!proper_list_silent(body))
        return body;

    GC_GUARD;
    gc_protect(&body);
    gc_protect(&bindings);

    unsigned renames = 0;
    gc_protect(&renames);

    unsigned exprs = body;
    gc_protect(&exprs);
    for (; exprs; exprs = cdr(exprs)) {
        unsigned expr = car(exprs);
        unsigned target = define_target_name(expr);
        if (!target)
            break;

        int64_t target_id = CELL_ID(target);
        if (target_id != ctx.kw_ellipsis &&
            !is_pattern_var(target_id, bindings) &&
            !assoc_list_has_atom_key_id(renames, target_id)) {
            gc_protect(&target);
            unsigned new_sym = do_gensym();
            gc_protect(&new_sym);
            unsigned entry = alloc_cons(target, new_sym);
            gc_protect(&entry);
            renames = alloc_cons(entry, renames);
            gc_unprotect(3);
        }
    }
    gc_unprotect(1);

    unsigned renamed_body = body;
    gc_protect(&renamed_body);
    unsigned r = renames;
    gc_protect(&r);
    for (; r; r = cdr(r)) {
        unsigned entry = car(r);
        unsigned old_sym = car(entry);
        unsigned new_sym = cdr(entry);
        renamed_body =
            rename_in_template(renamed_body, CELL_ID(old_sym), new_sym);
        if (renamed_body == TOK_ERROR) {
            gc_unprotect(1);
            return TOK_ERROR;
        }
    }
    gc_unprotect(1);

    unsigned new_body = 0, new_tail = 0;
    gc_protect(&new_body);
    gc_protect(&new_tail);

    exprs = renamed_body;
    gc_protect(&exprs);
    for (; exprs; exprs = cdr(exprs)) {
        unsigned expr = car(exprs);
        gc_protect(&expr);
        unsigned new_expr;
        if (define_target_name(expr))
            new_expr = hygienize_define_no_target_rename(expr, bindings);
        else
            new_expr = hygienize_template(expr, bindings);
        if (new_expr == TOK_ERROR) {
            gc_unprotect(1);
            return TOK_ERROR;
        }
        gc_protect(&new_expr);
        list_append(&new_body, &new_tail, new_expr);
        gc_unprotect(2);
    }

    gc_unprotect(1);

    return new_body;
}

static unsigned hygienize_begin(unsigned tmpl, unsigned bindings)
{
    unsigned keyword = car(tmpl);
    unsigned body = cdr(tmpl);

    GC_GUARD;
    gc_protect(&keyword);
    gc_protect(&body);
    gc_protect(&bindings);

    unsigned new_body = hygienize_body_sequence(body, bindings);
    if (new_body == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&new_body);

    return alloc_cons(keyword, new_body);
}

// Main hygienize function - walk template and rename introduced bindings
static unsigned hygienize_template(unsigned tmpl, unsigned bindings)
{
    if (!tmpl)
        return 0;

    GC_GUARD;
    gc_protect(&tmpl);
    gc_protect(&bindings);

    // Atoms don't need hygienization (they're handled by syntax_expand)
    if (IS_ATOM(tmpl)) {
        gc_unprotect(2);
        return tmpl;
    }

    if (IS_PAIR(tmpl)) {
        unsigned head = car(tmpl);

        // Check for protected wrapper: (##protected## . identifier)
        // These come from pattern variable substitution and should not be
        // renamed
        if (IS_ATOM(head) && CELL_ID(head) == ctx.kw_protected) {
            gc_unprotect(2);
            return tmpl; // Return protected wrapper unchanged
        }

        // Check for binding forms
        if (IS_ATOM(head)) {
            int64_t head_id = CELL_ID(head);

            // let, let*, letrec
            if (head_id == ctx.kw_let || head_id == ctx.kw_letstar) {
                if (!IS_PAIR(cdr(tmpl)))
                    return tmpl;
                // Check it's not named let: (let name ((var val) ...) body)
                unsigned second = cadr(tmpl);
                if (head_id == ctx.kw_let && IS_ATOM(second)) {
                    if (!IS_PAIR(cddr(tmpl)))
                        return tmpl;
                    gc_unprotect(2);
                    return hygienize_named_let(tmpl, bindings);
                }
                if (IS_PAIR(cddr(tmpl)) &&
                    (!second || (IS_PAIR(second) && IS_PAIR(car(second))))) {
                    gc_unprotect(2);
                    return head_id == ctx.kw_letstar
                               ? hygienize_letstar(tmpl, bindings)
                               : hygienize_let(tmpl, bindings, false);
                }
            }
            if (head_id == ctx.kw_letrec) {
                if (!IS_PAIR(cdr(tmpl)) || !IS_PAIR(cddr(tmpl)))
                    return tmpl;
                gc_unprotect(2);
                return hygienize_let(tmpl, bindings, true);
            }

            // lambda
            if (head_id == ctx.kw_lambda) {
                if (!IS_PAIR(cdr(tmpl)) || !IS_PAIR(cddr(tmpl)))
                    return tmpl;
                gc_unprotect(2);
                return hygienize_lambda(tmpl, bindings);
            }

            if (head_id == ctx.kw_define) {
                if (!IS_PAIR(cdr(tmpl)))
                    return tmpl;
                gc_unprotect(2);
                return hygienize_define(tmpl, bindings);
            }

            if (head_id == ctx.kw_begin) {
                gc_unprotect(2);
                return hygienize_begin(tmpl, bindings);
            }

            // Skip syntax-rules - they have their own patterns/templates
            // that will be hygienized when expanded
            if (syntax_rules_form_like(tmpl)) {
                gc_unprotect(2);
                return tmpl;
            }

            if (head_id == ctx.kw_quasiquote && IS_PAIR(cdr(tmpl))) {
                unsigned new_body = hygienize_quasiquote(cadr(tmpl), bindings,
                                                         1);
                if (new_body == TOK_ERROR) {
                    gc_unprotect(2);
                    return TOK_ERROR;
                }
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(new_body, cddr(tmpl));
                gc_protect(&result_tail);
                unsigned result = alloc_cons(head, result_tail);
                gc_unprotect(4);
                return result;
            }

            // For let-syntax/letrec-syntax, hygienize the body but not the
            // syntax-rules bindings
            if (head_id == ctx.kw_let_syntax ||
                head_id == ctx.kw_letrec_syntax) {
                if (!IS_PAIR(cdr(tmpl)) || !IS_PAIR(cddr(tmpl)))
                    return tmpl;
                // (let-syntax ((name transformer) ...) body ...)
                unsigned syn_bindings = cadr(tmpl);
                unsigned syn_body = cddr(tmpl);
                gc_protect(&syn_bindings);
                gc_protect(&syn_body);
                // Only hygienize the body, not the transformer bindings
                unsigned new_body = hygienize_template(syn_body, bindings);
                if (new_body == TOK_ERROR) {
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
                gc_protect(&new_body);
                unsigned result_tail = alloc_cons(syn_bindings, new_body);
                gc_protect(&result_tail);
                unsigned result = alloc_cons(head, result_tail);
                gc_unprotect(
                    6); // result_tail, new_body, syn_body, syn_bindings, tmpl, bindings
                return result;
            }
        }

        // Walk ordinary list spines iteratively.  A macro template may hold
        // a very large flat datum, for which recursing through each cdr would
        // overflow the C stack despite shallow structural nesting.
        unsigned result = 0, result_tail = 0;
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            unsigned new_car = hygienize_template(car(tmpl), bindings);
            if (new_car == TOK_ERROR) {
                gc_unprotect(4);
                return TOK_ERROR;
            }
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);

            unsigned next = cdr(tmpl);
            if (!IS_PAIR(next)) {
                if (next) {
                    unsigned new_tail = hygienize_template(next, bindings);
                    if (new_tail == TOK_ERROR) {
                        gc_unprotect(4);
                        return TOK_ERROR;
                    }
                    cell_set_cdr(result_tail, new_tail);
                }
                gc_unprotect(4);
                return result;
            }

            // Preserve the existing special-form handling if a cdr happens
            // to begin a new syntactic form rather than being list data.
            if (IS_ATOM(car(next)) && is_special_form(CELL_ID(car(next)))) {
                unsigned new_tail = hygienize_template(next, bindings);
                if (new_tail == TOK_ERROR) {
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
                cell_set_cdr(result_tail, new_tail);
                gc_unprotect(4);
                return result;
            }
            tmpl = next;
        }
    }

    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        bool changed = false;

        for (unsigned i = 0; i < len && !changed; i++) {
            // Refresh data pointer - GC may have moved tmpl
            unsigned *data = vector_data_ptr(tmpl);
            unsigned orig = data[i];
            gc_protect(&orig);
            unsigned h = hygienize_template(orig, bindings);
            if (h == TOK_ERROR) {
                gc_unprotect(3);
                return TOK_ERROR;
            }
            if (h != orig)
                changed = true;
            gc_unprotect(1);
        }

        if (!changed) {
            gc_unprotect(2);
            return tmpl;
        }

        unsigned new_vec = make_vector(len, 0);
        if (new_vec == TOK_ERROR) {
            gc_unprotect(2);
            return TOK_ERROR;
        }
        gc_protect(&new_vec);
        for (unsigned i = 0; i < len; i++) {
            // Refresh data pointers - GC may have moved vectors
            unsigned *data = vector_data_ptr(tmpl);
            unsigned elem = data[i];
            unsigned hygienized = hygienize_template(elem, bindings);
            if (hygienized == TOK_ERROR) {
                gc_unprotect(3);
                return TOK_ERROR;
            }
            // Through the write barrier: new_vec may have been promoted by a
            // collection during the recursive call, and a raw store would hide
            // this nursery element from the next minor GC.
            vector_set_elem(new_vec, i, hygienized);
        }
        gc_unprotect(3);
        return new_vec;
    }

    gc_unprotect(2);
    return tmpl;
}

// ============================================================================
// Template Expansion
// ============================================================================

// Fresh list of a vector's elements.
static unsigned vector_to_list_copy(unsigned vec)
{
    GC_GUARD;
    gc_protect(&vec);
    unsigned list = 0;
    gc_protect(&list);
    for (unsigned i = vector_len(vec); i > 0; i--) {
        list = alloc_cons(vector_data_ptr(vec)[i - 1], list);
        if (list == TOK_ERROR)
            return TOK_ERROR;
    }
    return list;
}

// Fresh vector of a proper list's elements.
static unsigned list_to_vector_copy(unsigned list)
{
    GC_GUARD;
    gc_protect(&list);
    unsigned vec = make_vector(list_length(list), 0);
    if (vec == TOK_ERROR)
        return TOK_ERROR;
    gc_protect(&vec);
    unsigned i = 0;
    for (unsigned p = list; IS_PAIR(p); p = cdr(p))
        vector_set_elem(vec, i++, car(p)); // no allocation in this loop
    return vec;
}

unsigned syntax_expand(unsigned tmpl, unsigned bindings, unsigned mark,
                       int64_t ellipsis_id)
{
    (void)mark; // For now, we don't do full hygiene renaming

    // Protect parameters - this function is recursive and allocates
    gc_protect(&tmpl);
    gc_protect(&bindings);

    if (!tmpl) {
        gc_unprotect(2);
        return 0;
    }

    // Atom - check if it's a pattern variable
    if (IS_ATOM(tmpl)) {
        unsigned lookup_result = syntax_lookup(tmpl, bindings);
        if (lookup_result != TOK_ERROR) {
            // Protect lookup_result BEFORE any allocations - this is critical!
            gc_protect(&lookup_result);
            // Keep compound input opaque to cleanup as well: traversing it
            // can loop on cyclic literals or break their shared identity.
            if (IS_ATOM(lookup_result) || IS_PAIR(lookup_result) ||
                IS_VECTOR(lookup_result)) {
                unsigned protected_marker = alloc();
                CELL_TYPE(protected_marker) = BT_ATOM;
                CELL_ID(protected_marker) = ctx.kw_protected;
                gc_protect(&protected_marker);
                unsigned wrapped = alloc_cons(protected_marker, lookup_result);
                gc_unprotect(
                    4); // protected_marker, lookup_result, bindings, tmpl
                return wrapped;
            }
            gc_unprotect(3); // lookup_result, bindings, tmpl
            return lookup_result;
        }
        gc_unprotect(2);
        return tmpl;
    }

    // List template
    if (IS_PAIR(tmpl)) {
        // R7RS ellipsis escape: (<ellipsis> <template>) expands to
        // <template> with ellipses inside treated as ordinary identifiers.
        // Passing -1 as the ellipsis id disables ellipsis handling below.
        if (syntax_is_ellipsis(car(tmpl), ellipsis_id) &&
            IS_PAIR(cdr(tmpl)) && !cddr(tmpl)) {
            unsigned escaped = syntax_expand(cadr(tmpl), bindings, mark, -1);
            gc_unprotect(2); // bindings, tmpl
            return escaped;
        }
        // Check for ellipsis: (tmpl ... rest)
        if (IS_PAIR(cdr(tmpl)) && syntax_is_ellipsis(cadr(tmpl), ellipsis_id)) {
            // Collect ALL ellipsis-bound variables in the template.
            // car(tmpl)/cddr(tmpl) are deliberately not hoisted above this
            // call: it allocates, so a collection inside it moves them, and
            // tmpl is the only one of the three that is rooted. Re-read them
            // afterwards - the same refresh the vector branch below already
            // does.
            unsigned ellipsis_vars =
                collect_ellipsis_vars(car(tmpl), bindings, 0);
            gc_protect(&ellipsis_vars);

            // Find iteration count from first variable's values
            unsigned iter_count = 0;
            if (!ellipsis_vars_iteration_count(ellipsis_vars, &iter_count)) {
                gc_unprotect(3);
                return TOK_ERROR;
            }

            // Expand template for each iteration
            unsigned result = 0, result_tail = 0;
            gc_protect(&result);
            gc_protect(&result_tail);
            unsigned elem_tmpl = car(tmpl);
            unsigned rest_tmpl = cddr(tmpl);
            gc_protect(&elem_tmpl);
            gc_protect(&rest_tmpl);

            for (unsigned i = 0; i < iter_count; i++) {
                // Create iteration bindings for ALL ellipsis variables
                unsigned iter_bindings = bindings;
                gc_protect(&iter_bindings);

                // For each ellipsis variable, bind to i-th value
                unsigned ev = ellipsis_vars;
                gc_protect(&ev);
                while (ev) {
                    unsigned binding = car(ev);
                    unsigned values = cdr(binding);

                    // Get i-th value
                    unsigned val = values;
                    for (unsigned j = 0; j < i && val; j++)
                        val = cdr(val);
                    if (val)
                        val = car(val);

                    gc_protect(&val);
                    // The element is one ellipsis shallower than its sequence
                    unsigned var = ellipsis_element_key(car(binding));
                    gc_protect(&var);
                    unsigned new_binding = alloc_cons(var, val);
                    gc_protect(&new_binding);
                    iter_bindings = alloc_cons(new_binding, iter_bindings);
                    gc_unprotect(3);
                    ev = cdr(ev);
                }
                gc_unprotect(1);

                unsigned expanded =
                    syntax_expand(elem_tmpl, iter_bindings, mark, ellipsis_id);
                if (expanded == TOK_ERROR) {
                    gc_unprotect(8);
                    return TOK_ERROR;
                }
                gc_protect(&expanded);
                list_append(&result, &result_tail, expanded);
                gc_unprotect(2); // expanded, iter_bindings
            }
            gc_unprotect(5); // rest_tmpl, elem_tmpl, result_tail, result,
                             // ellipsis_vars

            // Append expanded rest
            gc_protect(&result);
            gc_protect(&result_tail);
            unsigned rest_expanded =
                syntax_expand(rest_tmpl, bindings, mark, ellipsis_id);
            if (rest_expanded == TOK_ERROR) {
                gc_unprotect(4);
                return TOK_ERROR;
            }
            if (result) {
                cell_set_cdr(result_tail, rest_expanded);
                gc_unprotect(4); // result_tail, result, bindings, tmpl
                return result;
            }
            gc_unprotect(4); // result_tail, result, bindings, tmpl
            return rest_expanded;
        }

        // Expand ordinary list spines iteratively.  Flat templates can be
        // arbitrarily long even though their structural nesting is shallow;
        // recursively expanding every cdr used to overflow the C stack.
        unsigned result = 0, result_tail = 0;
        gc_protect(&result);
        gc_protect(&result_tail);
        for (;;) {
            if (!IS_PAIR(tmpl)) {
                if (tmpl) {
                    unsigned rest =
                        syntax_expand(tmpl, bindings, mark, ellipsis_id);
                    if (rest == TOK_ERROR) {
                        gc_unprotect(4);
                        return TOK_ERROR;
                    }
                    if (result)
                        cell_set_cdr(result_tail, rest);
                    else
                        result = rest;
                }
                gc_unprotect(4);
                return result;
            }

            // Delegate an ellipsis group to its dedicated expansion path.
            // That path appends its expanded tail, while this loop handles
            // every ordinary cons before it without growing the C stack.
            if (IS_PAIR(cdr(tmpl)) &&
                syntax_is_ellipsis(cadr(tmpl), ellipsis_id)) {
                unsigned rest = syntax_expand(tmpl, bindings, mark,
                                              ellipsis_id);
                if (rest == TOK_ERROR) {
                    gc_unprotect(4);
                    return TOK_ERROR;
                }
                if (result)
                    cell_set_cdr(result_tail, rest);
                else
                    result = rest;
                gc_unprotect(4);
                return result;
            }

            unsigned new_car =
                syntax_expand(car(tmpl), bindings, mark, ellipsis_id);
            if (new_car == TOK_ERROR) {
                gc_unprotect(4);
                return TOK_ERROR;
            }
            gc_protect(&new_car);
            list_append(&result, &result_tail, new_car);
            gc_unprotect(1);
            tmpl = cdr(tmpl);
        }
    }

    // Vector template - expand each element
    if (IS_VECTOR(tmpl)) {
        unsigned len = vector_len(tmpl);
        unsigned *data = vector_data_ptr(tmpl);

        bool has_ellipsis = false;
        for (unsigned i = 0; i < len && !has_ellipsis; i++)
            has_ellipsis = syntax_is_ellipsis(data[i], ellipsis_id);

        if (has_ellipsis) {
            // #(t ...) is (t ...) as a vector. Expanding the element list
            // with the list path reuses its handling of any number of
            // ellipses, each followed by further elements - the old
            // vector-only path honoured just the first ellipsis. The
            // validator rejects a leading ellipsis, so the list cannot
            // read as an escape (... t).
            unsigned elems = vector_to_list_copy(tmpl);
            if (elems == TOK_ERROR) {
                gc_unprotect(2);
                return TOK_ERROR;
            }
            unsigned expanded = syntax_expand(elems, bindings, mark,
                                              ellipsis_id);
            gc_unprotect(2); // bindings, tmpl
            if (expanded == TOK_ERROR)
                return TOK_ERROR;
            return list_to_vector_copy(expanded);
        }

        // No ellipsis - expand each element
        unsigned result = make_vector(len, 0);
        if (result == TOK_ERROR) {
            gc_unprotect(2);
            return TOK_ERROR;
        }
        gc_protect(&result); // Protect result from GC
        for (unsigned i = 0; i < len; i++) {
            // Refresh pointers - GC may have moved vectors
            data = vector_data_ptr(tmpl);
            unsigned elem = data[i];
            unsigned expanded = syntax_expand(elem, bindings, mark, ellipsis_id);
            if (expanded == TOK_ERROR) {
                gc_unprotect(3);
                return TOK_ERROR;
            }
            // Write barrier required - see unwrap_protected.
            vector_set_elem(result, i, expanded);
        }
        gc_unprotect(3); // result, tmpl, bindings
        return result;
    }

    // Other values pass through unchanged
    gc_unprotect(2);
    return tmpl;
}

// ============================================================================
// Transformer Application
// ============================================================================

static bool syntax_transformer_well_formed(unsigned transformer, unsigned input,
                                           unsigned *syn_data_out,
                                           unsigned *closure_env_out,
                                           int64_t *ellipsis_id_out,
                                           unsigned *literals_out,
                                           unsigned *rules_out)
{
    if (!IS_SYNTAX(transformer) || !IS_PAIR(input) ||
        pair_chain_is_circular(input))
        return false;

    unsigned syn_data = car(transformer);
    if (!IS_PAIR(syn_data))
        return false;

    unsigned ellipsis_cell = car(syn_data);
    int64_t ellipsis_id = 0;
    if (IS_FIXNUM(ellipsis_cell)) {
        ellipsis_id = FIXNUM_VALUE(ellipsis_cell);
    } else if (IS_CELL(ellipsis_cell)) {
        ellipsis_id = CELL_ID(ellipsis_cell);
    } else {
        return false;
    }
    if (!IS_PAIR(cdr(syn_data)))
        return false;

    unsigned literals = cadr(syn_data);
    unsigned rules = cddr(syn_data);
    if (!proper_list_silent(literals) || !proper_list_silent(rules))
        return false;

    for (unsigned r = rules; r; r = cdr(r)) {
        unsigned rule = car(r);
        if (!IS_PAIR(rule))
            return false;

        unsigned rule_head = car(rule);
        unsigned cpat_cell = IS_PAIR(rule_head) ? car(rule_head) : rule_head;
        if (!is_compiled_pattern_object(cpat_cell))
            return false;
    }

    *syn_data_out = syn_data;
    *closure_env_out = cdr(transformer);
    *ellipsis_id_out = ellipsis_id;
    *literals_out = literals;
    *rules_out = rules;
    return true;
}

// form's expansion by transformer with only matching and substitution: no
// free-identifier renaming, no binder hygiene, so no gensym is minted. It is
// only for bind_internal_definitions to see whether a body form is (or
// expands to) a definition; that scan runs on every closure creation and
// every call, so it must not consume an atom-table slot each time the way a
// real expansion's renames do. Errors are not reported: the real expansion
// of the same form reports them. TOK_ERROR when no rule matches.
static void annotate_ellipsis_depths(compiled_pattern *cpat, unsigned bindings);

static unsigned expand_without_hygiene(unsigned transformer, unsigned form,
                                       unsigned use_env)
{
    GC_GUARD;
    gc_protect(&transformer);
    gc_protect(&form);
    gc_protect(&use_env);
    unsigned syn_data = 0, closure_env = 0, literals = 0, rules = 0;
    int64_t ellipsis_id = 0;
    if (!syntax_transformer_well_formed(transformer, form, &syn_data,
                                        &closure_env, &ellipsis_id, &literals,
                                        &rules))
        return TOK_ERROR;
    gc_protect(&closure_env);
    gc_protect(&rules);

    bool suppress = ctx.suppress_error_output;
    ctx.suppress_error_output = true;
    unsigned result = TOK_ERROR;
    for (; rules; rules = cdr(rules)) {
        unsigned rule_head = car(car(rules));
        unsigned cpat_cell = IS_PAIR(rule_head) ? car(rule_head) : rule_head;
        compiled_pattern *cpat = (compiled_pattern *)CELL_PTR(cpat_cell);
        unsigned bindings =
            execute_pattern_in_envs(cpat, cdr(form), closure_env, use_env);
        if (bindings == TOK_ERROR)
            continue;
        gc_protect(&bindings);
        annotate_ellipsis_depths(cpat, bindings);
        result = syntax_expand(cdr(car(rules)), bindings, 0, ellipsis_id);
        if (result != TOK_ERROR)
            result = unwrap_protected(result, false);
        break;
    }
    ctx.suppress_error_output = suppress;
    return result;
}

// execute_pattern returns a plain (identifier . value) alist. Rekey every
// variable that was matched under an ellipsis as (identifier . depth), taking
// the depth from the compiled pattern's slots, so syntax_expand can iterate
// exactly the ellipsis variables (see binding_key_var). The alist is fresh
// per match, so it is updated in place.
static void annotate_ellipsis_depths(compiled_pattern *cpat, unsigned bindings)
{
    GC_GUARD;
    unsigned b = bindings;
    gc_protect(&b);
    for (; IS_PAIR(b); b = cdr(b)) {
        unsigned binding = car(b);
        if (!IS_PAIR(binding) || !IS_ATOM(car(binding)))
            continue;
        unsigned depth = 0;
        for (unsigned i = 0; i < cpat->var_count; i++) {
            if (CELL_ID(cpat->var_slots[i].atom) == CELL_ID(car(binding))) {
                depth = cpat->var_slots[i].depth;
                break;
            }
        }
        if (depth < 1)
            continue;
        unsigned key = alloc_cons(car(binding), store((int64_t)depth));
        cell_set_car(car(b), key); // re-read: the allocation may have moved it
    }
}

unsigned apply_syntax(unsigned transformer, unsigned input, unsigned use_env,
                      unsigned *bindings_out)
{
    // Protect parameters that may be in nursery - GC can run during
    // expansion. GC_GUARD releases every protection below on any return
    // path, so no manual unprotect accounting is needed at the exits.
    GC_GUARD;
    gc_protect(&transformer);
    gc_protect(&input);
    gc_protect(&use_env);

    if (bindings_out)
        *bindings_out = 0;

    // Extract transformer structure:
    // CAR = (ellipsis_cell . (literals . rules)), CDR = closure_env
    unsigned syn_data = 0;
    unsigned closure_env = 0;
    int64_t ellipsis_id = 0;
    unsigned literals = 0;
    unsigned rules = 0;
    if (!syntax_transformer_well_formed(transformer, input, &syn_data,
                                        &closure_env, &ellipsis_id, &literals,
                                        &rules)) {
        show_error("syntax transformer: invalid structure");
        return TOK_ERROR;
    }
    gc_protect(&syn_data);
    gc_protect(&literals);
    gc_protect(&closure_env);

    gc_protect(&rules);

    unsigned tmpl = 0;
    gc_protect(&tmpl);
    unsigned input_args = 0;
    gc_protect(&input_args);

    unsigned produced_bindings = 0;
    gc_protect(&produced_bindings);

    // Try each rule (rules now contain compiled patterns)
    for (; rules; rules = cdr(rules)) {
        unsigned rule = car(rules);
        unsigned rule_head = car(rule);
        // The source pattern kept beside the compiled one (cdr of a pair
        // rule head) is no longer consulted here.
        unsigned cpat_cell = IS_PAIR(rule_head) ? car(rule_head) : rule_head;
        tmpl = cdr(rule);

        // Get compiled pattern from cell
        compiled_pattern *cpat = (compiled_pattern *)CELL_PTR(cpat_cell);

        // Skip the keyword in input
        input_args = cdr(input);

        // Execute compiled pattern. Literals are matched by binding inside
        // the matcher, so its backtracking never settles on a split whose
        // literals denote something else at the use site.
        unsigned bindings =
            execute_pattern_in_envs(cpat, input_args, closure_env, use_env);

        if (bindings != TOK_ERROR) {
            // Generate unique mark for hygiene
            static unsigned syntax_mark = 0;
            unsigned mark = ++syntax_mark;

            gc_protect(&bindings);
            annotate_ellipsis_depths(cpat, bindings);

            // Referential transparency: find free identifiers in template
            // and bind them to their definition-time values
            unsigned free_ids =
                collect_free_ids(tmpl, bindings, 0, ellipsis_id, closure_env);
            gc_protect(&free_ids);

            unsigned rename_map = 0;
            gc_protect(&rename_map);

            // For each free identifier, check if it's bound in closure_env
            unsigned f = free_ids;
            gc_protect(&f);
            while (f) {
                unsigned free_atom = car(f);
                gc_protect(&free_atom);
                int64_t free_id = CELL_ID(free_atom);

                // Look up in closure environment (macro definition site)
                unsigned closure_val = lookup_silent(free_id, closure_env);

                // If the identifier already denotes the very same binding at
                // the use site as it does at the definition site, renaming it
                // would be a no-op: the alias we would install in use_env
                // resolves to exactly the cell the original name already
                // reaches. Skipping it keeps referential transparency intact
                // and, critically, mints no gensym - so a macro expanded in a
                // loop (the CPS interpreter re-expands on every evaluation)
                // does not consume one permanent atom-table slot per call.
                // Only the cells are compared, never retained: neither lookup
                // allocates, so no GC can intervene before the comparison.
                bool same_binding_at_use_site = false;
                if (closure_val != TOK_ERROR) {
                    unsigned def_cell =
                        env_find_binding_cell(free_id, closure_env);
                    same_binding_at_use_site =
                        def_cell != 0 &&
                        hygiene_binding_cell(free_id, closure_env) ==
                            hygiene_binding_cell(free_id, use_env);
                }

                if (same_binding_at_use_site) {
                    // No rename needed - fall through to the next free id.
                } else if (closure_val != TOK_ERROR &&
                           !generated_gensym_atom(free_atom)) {
                    // Found in closure env - create gensym and record the
                    // binding needed to materialize it at the call site.
                    // Definition-time gensyms already have private bindings
                    // in the use environment (including imported ones).
                    unsigned gensym = do_gensym();
                    gc_protect(&gensym);
                    gc_protect(&closure_val);
                    macros_note_alias(CELL_ID(gensym), free_id);

                    unsigned target_val_cell =
                        env_find_binding_cell(free_id, closure_env);
                    if (target_val_cell) {
                        unsigned binding_ref = make_binding_ref_cell(
                            free_atom, target_val_cell);
                        gc_protect(&binding_ref);
                        unsigned entry = alloc_cons(gensym, binding_ref);
                        gc_protect(&entry);
                        produced_bindings = alloc_cons(entry, produced_bindings);
                        gc_unprotect(2);
                    } else {
                        unsigned entry = alloc_cons(gensym, closure_val);
                        gc_protect(&entry);
                        produced_bindings = alloc_cons(entry,
                                                      produced_bindings);
                        gc_unprotect(1);
                    }

                    // Add to rename map: (free_atom . gensym)
                    unsigned entry = alloc_cons(free_atom, gensym);
                    gc_protect(&entry);
                    rename_map = alloc_cons(entry, rename_map);
                    gc_unprotect(1);

                    gc_unprotect(2); // gensym, closure_val
                } else if (!generated_gensym_atom(free_atom) &&
                           bound_for_hygiene(free_id, use_env)) {
                    // The identifier was introduced by the transformer, but
                    // is only bound at the expansion site. Rename it to an
                    // unbound gensym so use-site locals cannot capture it.
                    unsigned gensym = do_gensym();
                    gc_protect(&gensym);
                    unsigned entry = alloc_cons(free_atom, gensym);
                    gc_protect(&entry);
                    rename_map = alloc_cons(entry, rename_map);
                    gc_unprotect(2);
                }
                gc_unprotect(1);
                f = cdr(f);
            }
            gc_unprotect(1);

            // Apply free variable renames to template
            unsigned renamed_tmpl = rename_free_ids(tmpl, rename_map);
            gc_protect(&renamed_tmpl);
            if (renamed_tmpl == TOK_ERROR)
                return TOK_ERROR;

            // Hygienize template: rename introduced bindings to gensyms
            unsigned hygienic_tmpl = hygienize_template(renamed_tmpl, bindings);
            gc_protect(&hygienic_tmpl);
            if (hygienic_tmpl == TOK_ERROR)
                return TOK_ERROR;
            unsigned result =
                syntax_expand(hygienic_tmpl, bindings, mark, ellipsis_id);
            gc_protect(&result);
            if (result == TOK_ERROR)
                return TOK_ERROR;

            if (bindings_out)
                append_bindings_chain(bindings_out, produced_bindings);
            return unwrap_protected(result, false);
        }
    }

    show_error("syntax-rules: no matching pattern");
    return TOK_ERROR;
}

void apply_syntax_bindings(unsigned env, unsigned bindings)
{
    if (!bindings)
        return;

    GC_GUARD;
    gc_protect(&env);
    unsigned b = bindings;
    gc_protect(&b);
    for (; b; b = cdr(b)) {
        unsigned entry = car(b);
        gc_protect(&entry);
        if (!IS_PAIR(entry) || !IS_ATOM(car(entry))) {
            gc_unprotect(1);
            continue;
        }

        unsigned gensym = car(entry);
        unsigned target = cdr(entry);
        gc_protect(&target);
        if (IS_BINDING_REF(target)) {
            unsigned target_var = cdr(target);
            unsigned target_val_cell = CELL_CAR(target);
            defvar_alias(gensym, target_var, target_val_cell, env);
        } else {
            defvar(gensym, target, env);
        }
        gc_unprotect(2);
    }
}
