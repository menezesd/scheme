/**
 * @file compile_internal.h
 * @brief Internal declarations shared between compiler modules
 *
 * This header provides declarations for internal functions and data
 * that need to be shared between compile.c, code_object.c, and optimize.c.
 */

#ifndef COMPILE_INTERNAL_H
#define COMPILE_INTERNAL_H

#include "bytecode.h"
#include "context.h"

// ============================================================================
// Code Object Management (code_object.c)
// ============================================================================

// Register a code object with the GC registry
void code_register(code_object *code);

// ============================================================================
// Optimization (optimize.c)
// ============================================================================

// Get the size of an instruction (opcode + operands)
unsigned instruction_size(unsigned op);

// Peephole optimization with proper jump target fixup
void peephole_optimize(code_object *code);

// ============================================================================
// GC Integration (code_object.c)
// ============================================================================

// Collect code object constants during GC
unsigned gc_collect_code(code_object *code);

#endif // COMPILE_INTERNAL_H
