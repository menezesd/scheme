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

// True for every opcode whose first operand is a jump target. Shared so that
// the optimizer's jump fixup and the VM's bytecode verifier cannot disagree:
// adding a branching opcode and teaching only one of them about it would
// silently corrupt jump offsets.
bool is_jump_opcode(unsigned op);

// Peephole optimization with proper jump target fixup
void peephole_optimize(code_object *code);

// ============================================================================
// GC Integration (code_object.c)
// ============================================================================

// Collect code object constants during GC
unsigned gc_collect_code(code_object *code);

// Test seam: force the membership table's rebuild allocation to fail, so the
// path where a registered object cannot be indexed is reachable without an
// actual out-of-memory condition. Nothing in the runtime sets this.
void code_set_force_alloc_failure(bool fail);

#endif // COMPILE_INTERNAL_H
