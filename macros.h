#ifndef MACROS_H
#define MACROS_H

#include "types.h"
#include <stdbool.h>

// Expand a syntax-rules template with bindings
// ellipsis_id: symbol ID of ellipsis (0 if ellipsis is disabled/shadowed)
unsigned syntax_expand(unsigned tmpl, unsigned bindings, unsigned mark,
                       int64_t ellipsis_id);

// Apply a syntax transformer to input.
// Returns expanded form, and appends explicit syntax-time bindings to
// bindings_out as (gensym . target), where target is either:
//   - a BT_BINDING_REF cell (alias to a heap binding cell)
//   - a concrete value to be captured by value
// If a target identifier is only present at use-site, no entry is emitted.
unsigned apply_syntax(unsigned transformer, unsigned input, unsigned use_env,
                      unsigned *bindings_out);

// Expand an expression with explicit syntax bindings. Returns the expanded form.
// bindings_out accumulates per-call generated names as (gensym . target), where
// target follows the same convention as apply_syntax(). The walker does not
// mutate any runtime frames itself.
unsigned expand_form(unsigned expr, unsigned syn_env, unsigned *bindings_out);
// Expand each form of a procedure body (a list of forms, not itself a
// form) in syn_env, the frames naming its formals and internal definitions
// over real_env, the environment the procedure closes over.
unsigned expand_body(unsigned body, unsigned syn_env, unsigned real_env,
                     unsigned *bindings_out);

// Clear the static expander's recursion depth counter. Needed after a
// longjmp past its unwind (panic recovery), where the counter would stay
// pinned at the limit.
void macros_reset_expansion_depth(void);

// Materialize binding entries produced by apply_syntax into a runtime
// environment.
void apply_syntax_bindings(unsigned env, unsigned bindings);

// Fresh frame over parent_env with every formal parameter name bound to a
// placeholder value, for expansion-time scope decisions.
unsigned extend_env_with_binder_names(unsigned parent_env, unsigned params);

// Frame with the names of a body's leading (define ...) forms layered over
// env, for expansion and for establishing their scope before evaluation.
unsigned extend_env_with_internal_defines(unsigned env, unsigned body);

// Check if an identifier is a special form keyword
bool is_special_form(int64_t id);

// True for (case <key> <clause> ...). Its clauses' datum lists are quoted
// data, so template walkers that rename identifiers must leave them alone.
bool template_case_form(unsigned tmpl);

// Record that hygiene gensym alias_id renames origin_id, so a renamed case
// keyword is still recognised by template_case_form. syntax: the binding the
// alias stands for is a syntax-rules transformer - say the stdlib's let,
// renamed because the use site rebinds let - so a template walker that
// recognises let by name sees the alias as let too.
void macros_note_alias(int64_t alias_id, int64_t origin_id, bool syntax);

// Private identifier the compiler binds in a top-level begin's placeholder
// frame: the names there are globals-to-be (see compile_begin).
int64_t macros_toplevel_defines_marker_id(void);

// True if id denotes the same binding in both environments, for matching a
// syntax-rules literal; a top-level placeholder counts as the global.
bool macros_same_binding(int64_t id, unsigned env_a, unsigned env_b);

// True if id has no binding in env (a top-level placeholder counting as the
// global it stands for): for a core keyword, that it is the core form there.
bool macros_binding_is_core(int64_t id, unsigned env);

// datum with any core alias in it replaced by its keyword's plain symbol:
// a template keyword renamed past a use site that rebinds it, then quoted,
// is the symbol written (MIT strips renaming from quoted data too). Data too
// large or circular to scan cheaply is returned as it is.
unsigned macros_strip_core_aliases(unsigned datum);

// While set (as the standard library loads), every syntax-rules transformer
// made is first passed through macros_freeze_core_keywords: the core
// keywords it uses become their core aliases where they are core.
extern bool macros_freezing_core_keywords;
unsigned macros_freeze_core_keywords(unsigned transformer_form,
                                     unsigned closure_env,
                                     int64_t default_ellipsis_id);

#endif // MACROS_H
