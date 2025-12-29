#ifndef MACROS_H
#define MACROS_H

#include "types.h"

// Pattern matching for syntax-rules
// Returns bindings alist on success, TOK_ERROR on failure
unsigned syntax_match(unsigned pattern, unsigned input, unsigned literals,
                      unsigned bindings);

// Expand a syntax-rules template with bindings
unsigned syntax_expand(unsigned tmpl, unsigned bindings, unsigned mark);

// Apply a syntax transformer to input
unsigned apply_syntax(unsigned transformer, unsigned input, unsigned use_env);

#endif // MACROS_H
