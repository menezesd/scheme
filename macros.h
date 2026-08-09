#ifndef MACROS_H
#define MACROS_H

#include "types.h"
#include <stdbool.h>

// Pattern matching for syntax-rules
// Returns bindings alist on success, TOK_ERROR on failure
// ellipsis_id: symbol ID of ellipsis (0 if ellipsis is disabled/shadowed)
unsigned syntax_match(unsigned pattern, unsigned input, unsigned literals,
                      unsigned bindings, int64_t ellipsis_id);

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

// Materialize binding entries produced by apply_syntax into a runtime
// environment.
void apply_syntax_bindings(unsigned env, unsigned bindings);

// Check if an identifier is a special form keyword
bool is_special_form(int64_t id);

#endif // MACROS_H
