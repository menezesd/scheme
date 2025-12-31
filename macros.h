#ifndef MACROS_H
#define MACROS_H

#include "types.h"

// Pattern matching for syntax-rules
// Returns bindings alist on success, TOK_ERROR on failure
// ellipsis_id: symbol ID of ellipsis (0 if ellipsis is disabled/shadowed)
unsigned syntax_match(unsigned pattern, unsigned input, unsigned literals,
                      unsigned bindings, int64_t ellipsis_id);

// Expand a syntax-rules template with bindings
// ellipsis_id: symbol ID of ellipsis (0 if ellipsis is disabled/shadowed)
unsigned syntax_expand(unsigned tmpl, unsigned bindings, unsigned mark,
                       int64_t ellipsis_id);

// Apply a syntax transformer to input
unsigned apply_syntax(unsigned transformer, unsigned input, unsigned use_env);

#endif // MACROS_H
