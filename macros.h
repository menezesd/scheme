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
// env, for expansion-time scope decisions.
unsigned extend_env_with_internal_defines(unsigned env, unsigned body);

// Check if an identifier is a special form keyword
bool is_special_form(int64_t id);

#endif // MACROS_H
