#ifndef EVAL_H
#define EVAL_H

#include "types.h"

// Main CPS evaluator - evaluate expression in environment
unsigned eval_cps(unsigned expr, unsigned env);

// Wrapper that uses eval_cps
unsigned eval_obj(unsigned id, unsigned env);

#endif // EVAL_H
