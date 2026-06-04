/**
 * @file optimize.c
 * @brief Bytecode optimization and disassembly
 *
 * This file contains:
 * - Peephole optimizer for compiled bytecode
 * - Bytecode disassembler for debugging
 */

#include "bytecode.h"
#include "compile_internal.h"
#include "context.h"
#include "writer.h"
#include <limits.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void *calloc_array_plus_one(unsigned count, size_t elem_size)
{
    if (count == UINT_MAX)
        return NULL;
    unsigned count_plus_one = count + 1;
    return checked_calloc_array(count_plus_one, elem_size);
}

static bool is_jump_opcode(unsigned op)
{
    switch (op) {
    case OP_JUMP:
    case OP_JUMPIF:
    case OP_JUMPIFNOT:
    case OP_JUMPIFNULL:
    case OP_JUMPIFNOTNULL:
    case OP_JUMPIFZERO:
    case OP_JUMPIFNOTZERO:
    case OP_NUMEQ_JUMPIFNOT:
    case OP_LT_JUMPIFNOT:
    case OP_GT_JUMPIFNOT:
    case OP_LE_JUMPIFNOT:
    case OP_GE_JUMPIFNOT:
    case OP_NUMEQ_INT_JUMPIFNOT:
    case OP_LT_INT_JUMPIFNOT:
        return true;
    default:
        return false;
    }
}

// ============================================================================
// Instruction Size
// ============================================================================

// Get the size of an instruction (opcode + operands)
unsigned instruction_size(unsigned op)
{
    switch (op) {
    case OP_LOOKUP:
    case OP_LOOKUP_ADD1:
    case OP_LOOKUP_SUB1:
        return 4; // opcode + sym_id + cached_depth + cached_offset
    case OP_NUMEQ_JUMPIFNOT:
    case OP_NUMEQ_INT_JUMPIFNOT:
    case OP_LT_JUMPIFNOT:
    case OP_LT_INT_JUMPIFNOT:
    case OP_GT_JUMPIFNOT:
    case OP_LE_JUMPIFNOT:
    case OP_GE_JUMPIFNOT:
    case OP_RETURN_LOCALS:
    case OP_LOCAL_GET:
    case OP_LOCAL_SET:
    case OP_LOCAL_SET_VOID:
    case OP_CONST:
    case OP_DEFINE:
    case OP_SET:
    case OP_SET_VOID:
    case OP_CLOSURE:
    case OP_CALL:
    case OP_TAILCALL:
    case OP_JUMP:
    case OP_JUMPIF:
    case OP_JUMPIFNOT:
    case OP_JUMPIFNULL:
    case OP_JUMPIFNOTNULL:
    case OP_JUMPIFZERO:
    case OP_JUMPIFNOTZERO:
    case OP_VALUES:
    case OP_DEFSYNTAX:
    case OP_LETREC_MARK:
        return 2; // opcode + 1 operand
    case OP_DEFINE_ALIAS:
        return 3; // opcode + alias + target
    case OP_PRIM:
        return 3; // opcode + 2 operands
    default:
        return 1; // opcode only
    }
}

static bool instruction_fits(const unsigned *code, unsigned len, unsigned ip,
                             unsigned *size_out)
{
    if (!code || ip >= len)
        return false;
    unsigned size = instruction_size(code[ip]);
    if (size == 0 || size > len - ip)
        return false;
    if (size_out)
        *size_out = size;
    return true;
}

static bool code_is_well_formed(const unsigned *code, unsigned len)
{
    for (unsigned i = 0; i < len;) {
        unsigned size;
        if (!instruction_fits(code, len, i, &size))
            return false;
        i += size;
    }
    return true;
}

static bool instruction_starts_at(const unsigned *code, unsigned len,
                                  unsigned target)
{
    for (unsigned i = 0; i < len;) {
        if (i == target)
            return true;
        unsigned size;
        if (!instruction_fits(code, len, i, &size))
            return false;
        if (i > target || size > len - i)
            return false;
        i += size;
    }
    return target == len;
}

static void mark_jump_targets(const unsigned *code, unsigned len,
                              bool *is_jump_target)
{
    for (unsigned i = 0; i < len;) {
        unsigned op = code[i];
        unsigned size = instruction_size(op);
        if (is_jump_opcode(op)) {
            unsigned target = code[i + 1];
            if (target <= len)
                is_jump_target[target] = true;
        }
        i += size;
    }
}

static void mark_dead_until_target(const unsigned *code, unsigned len,
                                   const bool *is_jump_target, bool *remove,
                                   unsigned start)
{
    for (unsigned i = start; i < len && !is_jump_target[i];) {
        unsigned size = instruction_size(code[i]);
        for (unsigned j = 0; j < size && i + j < len; j++) {
            remove[i + j] = true;
        }
        i += size;
    }
}

// ============================================================================
// Common Subexpression Elimination (CSE)
// ============================================================================

// Helper: check if a constant is a small integer and get its value
static bool get_small_int(code_object *code, unsigned const_idx, int64_t *val)
{
    if (const_idx >= code->const_len)
        return false;
    unsigned cell = code->constants[const_idx];
    if (IS_FIXNUM(cell)) {
        *val = FIXNUM_VALUE(cell);
        return true;
    }
    if (!IS_NUM(cell))
        return false;
    int64_t boxed = CELL_ID(cell);
    if (boxed <= INT32_MIN || boxed >= INT32_MAX)
        return false;
    *val = boxed;
    return true;
}

// Helper: check if result fits in small integer range
static bool fits_small_int(int64_t val)
{
    // Conservative: avoid overflow edge cases
    return val > INT32_MIN && val < INT32_MAX;
}

// CSE pass: detect consecutive identical CONST/LOOKUP and use DUP
// Also performs constant folding for arithmetic on small integers
// This pass rebuilds the code array to handle instruction size changes
static void cse_pass(code_object *code)
{
    if (!code || code->code_len < 4)
        return;

    unsigned *c = code->code;
    unsigned len = code->code_len;
    if (!code_is_well_formed(c, len))
        return;

    // Allocate new code buffer (max same size as original)
    unsigned *new_code = checked_malloc_array(len, sizeof(unsigned));
    if (!new_code)
        return;

    // First, identify jump targets to avoid unsafe optimizations
    bool *is_jump_target = calloc_array_plus_one(len, sizeof(bool));
    if (!is_jump_target) {
        free(new_code);
        return;
    }
    mark_jump_targets(c, len, is_jump_target);

    // Build offset map as we go (old position -> new position)
    unsigned *offset_map = calloc_array_plus_one(len, sizeof(unsigned));
    if (!offset_map) {
        free(new_code);
        free(is_jump_target);
        return;
    }

    unsigned write = 0;
    unsigned i = 0;
    while (i < len) {
        offset_map[i] = write;
        unsigned op = c[i];
        unsigned size = instruction_size(op);

        // Pattern: CONST x, CONST x -> CONST x, DUP
        // Only if second CONST is not a jump target
        if (op == OP_CONST && i + 3 < len && c[i + 2] == OP_CONST &&
            c[i + 1] == c[i + 3] && !is_jump_target[i + 2]) {
            // Emit CONST x
            new_code[write++] = OP_CONST;
            new_code[write++] = c[i + 1];
            offset_map[i + 2] = write;
            // Emit DUP instead of second CONST
            new_code[write++] = OP_DUP;
            i += 4; // Skip both CONST instructions
            continue;
        }

        // Pattern: LOOKUP x d o, LOOKUP x d2 o2 -> LOOKUP x d o, DUP
        if (op == OP_LOOKUP && i + 7 < len && c[i + 4] == OP_LOOKUP &&
            c[i + 1] == c[i + 5] && !is_jump_target[i + 4]) {
            // Emit first LOOKUP x d o
            new_code[write++] = OP_LOOKUP;
            new_code[write++] = c[i + 1];
            new_code[write++] = c[i + 2];
            new_code[write++] = c[i + 3];
            offset_map[i + 4] = write;
            // Emit DUP instead of second LOOKUP
            new_code[write++] = OP_DUP;
            i += 8;
            continue;
        }

        // ================================================================
        // Constant Folding: evaluate constant expressions at compile time
        // ================================================================

        // Pattern: CONST a, CONST b, <arith-op> -> CONST (a op b)
        // where arith-op is ADD, SUB, MUL and both are small integers
        if (op == OP_CONST && i + 4 < len && c[i + 2] == OP_CONST &&
            !is_jump_target[i + 2] && !is_jump_target[i + 4]) {
            unsigned arith_op = c[i + 4];
            int64_t a, b, result;
            bool can_fold = false;

            if ((arith_op == OP_ADD || arith_op == OP_SUB || arith_op == OP_MUL) &&
                get_small_int(code, c[i + 1], &a) &&
                get_small_int(code, c[i + 3], &b)) {

                if (arith_op == OP_ADD) {
                    result = a + b;
                    can_fold = fits_small_int(result);
                } else if (arith_op == OP_SUB) {
                    result = a - b;
                    can_fold = fits_small_int(result);
                } else if (arith_op == OP_MUL) {
                    // Check for overflow before computing
                    if (a == 0 || b == 0) {
                        result = 0;
                        can_fold = true;
                    } else if (a > 0 && b > 0 && a <= INT32_MAX / b) {
                        result = a * b;
                        can_fold = fits_small_int(result);
                    } else if (a < 0 && b < 0 && -a <= INT32_MAX / -b) {
                        result = a * b;
                        can_fold = fits_small_int(result);
                    } else if (a > 0 && b < 0 && a <= INT32_MAX / -b) {
                        result = a * b;
                        can_fold = fits_small_int(result);
                    } else if (a < 0 && b > 0 && -a <= INT32_MAX / b) {
                        result = a * b;
                        can_fold = fits_small_int(result);
                    }
                }

                if (can_fold) {
                    // Create new constant for result
                    unsigned result_cell = store(result);
                    unsigned result_idx = code_add_const(code, result_cell);
                    // Emit single CONST
                    new_code[write++] = OP_CONST;
                    new_code[write++] = result_idx;
                    // Skip all 5 instructions (CONST, idx, CONST, idx, op)
                    offset_map[i + 2] = write;
                    offset_map[i + 4] = write;
                    i += 5;
                    continue;
                }
            }
        }

        // Fuse: CMP + JUMPIFNOT -> CMP_JUMPIFNOT (size-changing, in CSE pass)
        if (i + 1 < len && c[i + 1] == OP_JUMPIFNOT &&
            !is_jump_target[i + 1]) {
            unsigned fused_op = 0;
            switch (op) {
            case OP_NUMEQ: fused_op = OP_NUMEQ_JUMPIFNOT; break;
            case OP_LT:    fused_op = OP_LT_JUMPIFNOT; break;
            case OP_GT:    fused_op = OP_GT_JUMPIFNOT; break;
            case OP_LE:    fused_op = OP_LE_JUMPIFNOT; break;
            case OP_GE:    fused_op = OP_GE_JUMPIFNOT; break;
            default: break;
            }
            if (fused_op) {
                new_code[write++] = fused_op;
                new_code[write++] = c[i + 2]; // jump target
                offset_map[i + 1] = write;
                offset_map[i + 2] = write;
                i += 3;
                continue;
            }
        }

        // Copy instruction unchanged
        for (unsigned j = 0; j < size && i + j < len; j++) {
            new_code[write++] = c[i + j];
        }
        i += size;
    }
    offset_map[len] = write; // End marker

    // Fix jump targets if any instructions were removed
    if (write < len) {
        for (unsigned j = 0; j < write;) {
            unsigned op = new_code[j];
            unsigned size = instruction_size(op);

            if (is_jump_opcode(op)) {
                unsigned old_target = new_code[j + 1];
                // Find the new target using offset map
                // Search for the closest mapped position
                unsigned new_target = write; // default to end
                if (old_target <= len) {
                    new_target = offset_map[old_target];
                }
                new_code[j + 1] = new_target;
            }
            j += size;
        }

        // Copy back to original
        memcpy(code->code, new_code, write * sizeof(unsigned));
        code->code_len = write;
    }

    free(offset_map);
    free(is_jump_target);
    free(new_code);
}

// ============================================================================
// Peephole Optimizer
// ============================================================================

// Peephole optimization with proper jump target fixup
void peephole_optimize(code_object *code)
{
    if (!code || code->code_len < 2)
        return;

    if (!code_is_well_formed(code->code, code->code_len))
        return;

    // Run CSE pass first (handles instruction size changes separately)
    cse_pass(code);

    unsigned *c = code->code;
    unsigned len = code->code_len;

    // Collect all jump targets - needed to avoid unsafe fusions
    bool *is_jump_target = calloc_array_plus_one(len, sizeof(bool));
    if (!is_jump_target)
        return;
    mark_jump_targets(c, len, is_jump_target);

    // First pass: identify which bytes to remove and build offset map
    // offset_map[old] = new offset after compaction
    unsigned *offset_map = checked_malloc_array(len, sizeof(unsigned));
    if (!offset_map) {
        free(is_jump_target);
        return;
    }

    bool *remove = checked_calloc_array(len, sizeof(bool));
    if (!remove) {
        free(offset_map);
        free(is_jump_target);
        return;
    }

    // Mark patterns to remove
    for (unsigned i = 0; i < len;) {
        // Skip positions already marked for removal
        if (remove[i]) {
            i++;
            continue;
        }

        unsigned op = c[i];
        unsigned size = instruction_size(op);

        // ================================================================
        // Dead Code Elimination: remove unreachable code after terminators
        // ================================================================

        // Pattern: RETURN/HALT followed by non-jump-target code -> remove dead code
        if ((op == OP_RETURN || op == OP_HALT) && i + 1 < len) {
            // Mark all instructions after RETURN/HALT until we hit a jump target
            mark_dead_until_target(c, len, is_jump_target, remove, i + size);
            i += size;
            continue;
        }

        // Pattern: unconditional JUMP followed by non-jump-target code -> remove dead code
        if (op == OP_JUMP && i + 2 < len && !is_jump_target[i + 2]) {
            mark_dead_until_target(c, len, is_jump_target, remove, i + 2);
            // Don't skip - continue to check other patterns for JUMP
        }

        // ================================================================
        // Existing peephole patterns
        // ================================================================

        // Pattern: CONST x, POP -> nothing (dead code)
        if (op == OP_CONST && i + 2 < len && c[i + 2] == OP_POP &&
            !is_jump_target[i] && !is_jump_target[i + 2]) {
            remove[i] = remove[i + 1] = remove[i + 2] = true;
            i += 3;
            continue;
        }

        // Pattern: DUP, POP -> nothing
        if (op == OP_DUP && i + 1 < len && c[i + 1] == OP_POP &&
            !is_jump_target[i] && !is_jump_target[i + 1]) {
            remove[i] = remove[i + 1] = true;
            i += 2;
            continue;
        }

        // Pattern: SWAP, SWAP -> nothing (identity)
        if (op == OP_SWAP && i + 1 < len && c[i + 1] == OP_SWAP &&
            !is_jump_target[i] && !is_jump_target[i + 1]) {
            remove[i] = remove[i + 1] = true;
            i += 2;
            continue;
        }

        // Pattern: NOT, JUMPIFNOT -> JUMPIF (remove NOT, change jump type)
        if (op == OP_NOT && i + 1 < len && c[i + 1] == OP_JUMPIFNOT &&
            !is_jump_target[i] && !is_jump_target[i + 1]) {
            remove[i] = true;
            c[i + 1] = OP_JUMPIF;
            i += 1; // Continue from JUMPIF
            continue;
        }

        // Pattern: NOT, JUMPIF -> JUMPIFNOT (remove NOT, change jump type)
        if (op == OP_NOT && i + 1 < len && c[i + 1] == OP_JUMPIF &&
            !is_jump_target[i] && !is_jump_target[i + 1]) {
            remove[i] = true;
            c[i + 1] = OP_JUMPIFNOT;
            i += 1;
            continue;
        }

        // Pattern: JUMP to immediately next instruction -> nothing
        if (op == OP_JUMP && i + 2 < len && c[i + 1] == i + 2) {
            remove[i] = remove[i + 1] = true;
            i += 2;
            continue;
        }

        // Pattern: NULLP, JUMPIFNOT target -> JUMPIFNOTNULL target
        // Only safe if no other code jumps to the JUMPIFNOT instruction
        if (op == OP_NULLP && i + 1 < len && c[i + 1] == OP_JUMPIFNOT &&
            !is_jump_target[i + 1]) {
            remove[i] = true;            // Remove NULLP
            c[i + 1] = OP_JUMPIFNOTNULL; // Replace with fused opcode
            i += 1;
            continue;
        }

        // Pattern: NULLP, JUMPIF target -> JUMPIFNULL target
        // Only safe if no other code jumps to the JUMPIF instruction
        if (op == OP_NULLP && i + 1 < len && c[i + 1] == OP_JUMPIF &&
            !is_jump_target[i + 1]) {
            remove[i] = true;
            c[i + 1] = OP_JUMPIFNULL;
            i += 1;
            continue;
        }

        // Pattern: ZEROP, JUMPIFNOT target -> JUMPIFNOTZERO target
        // Only safe if no other code jumps to the JUMPIFNOT instruction
        if (op == OP_ZEROP && i + 1 < len && c[i + 1] == OP_JUMPIFNOT &&
            !is_jump_target[i + 1]) {
            remove[i] = true;
            c[i + 1] = OP_JUMPIFNOTZERO;
            i += 1;
            continue;
        }

        // Pattern: ZEROP, JUMPIF target -> JUMPIFZERO target
        // Only safe if no other code jumps to the JUMPIF instruction
        if (op == OP_ZEROP && i + 1 < len && c[i + 1] == OP_JUMPIF &&
            !is_jump_target[i + 1]) {
            remove[i] = true;
            c[i + 1] = OP_JUMPIFZERO;
            i += 1;
            continue;
        }

        // Fuse: SET sym, POP -> SET_VOID sym (common in loop variable updates)
        if (op == OP_SET && i + 2 < len && c[i + 2] == OP_POP &&
            !is_jump_target[i + 2]) {
            c[i] = OP_SET_VOID;
            remove[i + 2] = true;
            i += 2;
            continue;
        }

        // Fuse: NUMEQ, JUMPIFNOT target -> NUMEQ_JUMPIFNOT target
        // Handled in CSE pass (not peephole) since it changes instruction sizes

        // Superinstruction: LOOKUP sym d o, ADD1 -> LOOKUP_ADD1 sym d o
        if (op == OP_LOOKUP && i + 4 < len && c[i + 4] == OP_ADD1 &&
            !is_jump_target[i + 4]) {
            c[i] = OP_LOOKUP_ADD1;
            remove[i + 4] = true;
            i += 4;
            continue;
        }

        // Superinstruction: LOOKUP sym d o, SUB1 -> LOOKUP_SUB1 sym d o
        if (op == OP_LOOKUP && i + 4 < len && c[i + 4] == OP_SUB1 &&
            !is_jump_target[i + 4]) {
            c[i] = OP_LOOKUP_SUB1;
            remove[i + 4] = true;
            i += 4;
            continue;
        }

        // Pattern: CAR, CDR can be fused (if we add CADR) - already have
        // OP_CADR Pattern: CDR, CDR can be fused (if we add CDDR) - already
        // have OP_CDDR

        // Pattern: PAIRP, NOT -> use directly in conditional
        if (op == OP_PAIRP && i + 1 < len && c[i + 1] == OP_NOT &&
            i + 2 < len && c[i + 2] == OP_JUMPIFNOT &&
            !is_jump_target[i] && !is_jump_target[i + 1] &&
            !is_jump_target[i + 2]) {
            // PAIRP, NOT, JUMPIFNOT -> PAIRP, JUMPIF
            remove[i + 1] = true;
            c[i + 2] = OP_JUMPIF;
            i += 2;
            continue;
        }

        // Pattern: CALL n, RETURN -> TAILCALL n (tail call optimization)
        // Only safe if no other code jumps to the RETURN
        if (op == OP_CALL && i + 2 < len && c[i + 2] == OP_RETURN &&
            !is_jump_target[i + 2]) {
            c[i] = OP_TAILCALL; // Convert CALL to TAILCALL
            remove[i + 2] = true; // Remove RETURN
            i += 2;
            continue;
        }

        // Pattern: CDR, CDR -> CDDR (if not jump target)
        if (op == OP_CDR && i + 1 < len && c[i + 1] == OP_CDR &&
            !is_jump_target[i + 1]) {
            c[i] = OP_CDDR;
            remove[i + 1] = true;
            i += 1;
            continue;
        }

        // Pattern: CDR, CAR -> CADR (access second element)
        if (op == OP_CDR && i + 1 < len && c[i + 1] == OP_CAR &&
            !is_jump_target[i + 1]) {
            c[i] = OP_CADR;
            remove[i + 1] = true;
            i += 1;
            continue;
        }

        // Pattern: CAR, CDR -> CDAR (cdr of car) (if not jump target)
        if (op == OP_CAR && i + 1 < len && c[i + 1] == OP_CDR &&
            !is_jump_target[i + 1]) {
            c[i] = OP_CDAR;
            remove[i + 1] = true;
            i += 1;
            continue;
        }

        // Pattern: CAR, CAR -> CAAR (car of car)
        if (op == OP_CAR && i + 1 < len && c[i + 1] == OP_CAR &&
            !is_jump_target[i + 1]) {
            c[i] = OP_CAAR;
            remove[i + 1] = true;
            i += 1;
            continue;
        }

        // Pattern: CDR, CDR, CDR -> CDDDR
        if (op == OP_CDR && i + 2 < len && c[i + 1] == OP_CDR &&
            c[i + 2] == OP_CDR && !is_jump_target[i + 1] &&
            !is_jump_target[i + 2]) {
            c[i] = OP_CDDDR;
            remove[i + 1] = remove[i + 2] = true;
            i += 2;
            continue;
        }

        // Pattern: CDDR, CDR -> CDDDR
        if (op == OP_CDDR && i + 1 < len && c[i + 1] == OP_CDR &&
            !is_jump_target[i + 1]) {
            c[i] = OP_CDDDR;
            remove[i + 1] = true;
            i += 1;
            continue;
        }

        // Pattern: CDR, CDDR -> CDDDR
        if (op == OP_CDR && i + 1 < len && c[i + 1] == OP_CDDR &&
            !is_jump_target[i + 1]) {
            c[i] = OP_CDDDR;
            remove[i + 1] = true;
            i += 1;
            continue;
        }

        // Pattern: CADR, CDR -> CADDR
        if (op == OP_CADR && i + 1 < len && c[i + 1] == OP_CDR &&
            !is_jump_target[i + 1]) {
            c[i] = OP_CADDR;
            remove[i + 1] = true;
            i += 1;
            continue;
        }

        // Pattern: CDDR, CAR -> CADDR
        if (op == OP_CDDR && i + 1 < len && c[i + 1] == OP_CAR &&
            !is_jump_target[i + 1]) {
            c[i] = OP_CADDR;
            remove[i + 1] = true;
            i += 1;
            continue;
        }

        // NOTE: CONST x, CONST x -> CONST x, DUP is handled in cse_pass()
        // which properly rebuilds the code array to handle size changes.

        i += size;
    }

    // ========================================================================
    // Jump Threading: follow chains of unconditional JUMPs to final target
    // ========================================================================
    for (unsigned i = 0; i < len;) {
        unsigned op = c[i];
        unsigned size = instruction_size(op);

        // Thread any jump instruction
        if (is_jump_opcode(op)) {

            unsigned target = c[i + 1];
            unsigned visited_count = 0;
            const unsigned max_chain = 10; // Prevent infinite loops

            // Follow chain of unconditional JUMPs
            while (target < len && instruction_starts_at(c, len, target) &&
                   instruction_fits(c, len, target, NULL) &&
                   c[target] == OP_JUMP && visited_count < max_chain) {
                target = c[target + 1];
                visited_count++;
            }

            // Update target if we found a shorter path
            if (target != c[i + 1]) {
                c[i + 1] = target;
            }
        }
        i += size;
    }

    // Build offset map
    unsigned new_offset = 0;
    for (unsigned i = 0; i < len; i++) {
        offset_map[i] = new_offset;
        if (!remove[i])
            new_offset++;
    }
    // Map for end-of-code (jump targets can point here)
    unsigned final_len = new_offset;

    // Second pass: compact code and fix jump targets
    unsigned write = 0;
    for (unsigned read = 0; read < len;) {
        unsigned op = c[read];
        unsigned size = instruction_size(op);

        if (remove[read]) {
            read += size;
            continue;
        }

        // Copy opcode
        c[write++] = op;

        // Handle operands, fixing jump targets
        if (is_jump_opcode(op)) {
            unsigned old_target = c[read + 1];
            unsigned new_target =
                (old_target < len) ? offset_map[old_target] : final_len;
            c[write++] = new_target;
            read += 2;
        } else {
            // Copy remaining operands as-is
            for (unsigned j = 1; j < size; j++) {
                c[write++] = c[read + j];
            }
            read += size;
        }
    }

    code->code_len = write;

    free(offset_map);
    free(remove);
    free(is_jump_target);

    // Recursively optimize children
    for (unsigned i = 0; i < code->children_len; i++) {
        peephole_optimize(code->children[i]);
    }
}

// ============================================================================
// Bytecode Disassembler
// ============================================================================

static const char *opcode_names[] = {
    [OP_CONST] = "CONST",
    [OP_POP] = "POP",
    [OP_DUP] = "DUP",
    [OP_SWAP] = "SWAP",
    [OP_LOOKUP] = "LOOKUP",
    [OP_DEFINE] = "DEFINE",
    [OP_DEFINE_ALIAS] = "DEFINE_ALIAS",
    [OP_SET] = "SET",
    [OP_CLOSURE] = "CLOSURE",
    [OP_CALL] = "CALL",
    [OP_TAILCALL] = "TAILCALL",
    [OP_RETURN] = "RETURN",
    [OP_JUMP] = "JUMP",
    [OP_JUMPIF] = "JUMPIF",
    [OP_JUMPIFNOT] = "JUMPIFNOT",
    [OP_PRIM] = "PRIM",
    [OP_PUSHCONT] = "PUSHCONT",
    [OP_CALLCC] = "CALLCC",
    [OP_APPLY] = "APPLY",
    [OP_PUSHENV] = "PUSHENV",
    [OP_POPENV] = "POPENV",
    [OP_VALUES] = "VALUES",
    [OP_CALLWITHVALUES] = "CALLWITHVALUES",
    [OP_DEFSYNTAX] = "DEFSYNTAX",
    [OP_HALT] = "HALT",
    [OP_CAR] = "CAR",
    [OP_CDR] = "CDR",
    [OP_CONS] = "CONS",
    [OP_NULLP] = "NULLP",
    [OP_PAIRP] = "PAIRP",
    [OP_ADD1] = "ADD1",
    [OP_SUB1] = "SUB1",
    [OP_ZEROP] = "ZEROP",
    [OP_NOT] = "NOT",
    [OP_EQ] = "EQ",
    [OP_ADD] = "ADD",
    [OP_SUB] = "SUB",
    [OP_MUL] = "MUL",
    [OP_DIV] = "DIV",
    [OP_MOD] = "MOD",
    [OP_LT] = "LT",
    [OP_GT] = "GT",
    [OP_LE] = "LE",
    [OP_GE] = "GE",
    [OP_NUMEQ] = "NUMEQ",
    [OP_CADR] = "CADR",
    [OP_CDDR] = "CDDR",
    [OP_SETCAR] = "SETCAR",
    [OP_SETCDR] = "SETCDR",
    [OP_LIST1] = "LIST1",
    [OP_LIST2] = "LIST2",
    [OP_LIST3] = "LIST3",
    [OP_JUMPIFNULL] = "JUMPIFNULL",
    [OP_JUMPIFNOTNULL] = "JUMPIFNOTNULL",
    [OP_JUMPIFZERO] = "JUMPIFZERO",
    [OP_JUMPIFNOTZERO] = "JUMPIFNOTZERO",
    [OP_LETREC_MARK] = "LETREC_MARK",
    [OP_LETREC_DONE] = "LETREC_DONE",
    // Additional list accessors
    [OP_CAAR] = "CAAR",
    [OP_CDAR] = "CDAR",
    [OP_CADDR] = "CADDR",
    [OP_CDDDR] = "CDDDR",
    // Type predicates
    [OP_SYMBOLP] = "SYMBOLP",
    [OP_NUMBERP] = "NUMBERP",
    [OP_STRINGP] = "STRINGP",
    [OP_VECTORP] = "VECTORP",
    [OP_BOOLEANP] = "BOOLEANP",
    [OP_LISTP] = "LISTP",
    [OP_INTEGERP] = "INTEGERP",
    // List operations
    [OP_LENGTH] = "LENGTH",
    [OP_APPEND] = "APPEND",
    [OP_REVERSE] = "REVERSE",
    [OP_MEMQ] = "MEMQ",
    // Vector operations
    [OP_VECTORREF] = "VECTORREF",
    [OP_VECTORSET] = "VECTORSET",
    [OP_VECTORLEN] = "VECTORLEN",
    // Numeric operations
    [OP_NEG] = "NEG",
    [OP_ABS] = "ABS",
    [OP_POSITIVE] = "POSITIVE",
    [OP_NEGATIVE] = "NEGATIVE",
    [OP_EVEN] = "EVEN",
    [OP_ODD] = "ODD",
    [OP_LOOKUP_ADD1] = "LOOKUP_ADD1",
    [OP_LOOKUP_SUB1] = "LOOKUP_SUB1",
    [OP_SET_VOID] = "SET_VOID",
    [OP_NUMEQ_JUMPIFNOT] = "NUMEQ_JUMPIFNOT",
    [OP_LT_JUMPIFNOT] = "LT_JUMPIFNOT",
    [OP_GT_JUMPIFNOT] = "GT_JUMPIFNOT",
    [OP_LE_JUMPIFNOT] = "LE_JUMPIFNOT",
    [OP_GE_JUMPIFNOT] = "GE_JUMPIFNOT",
    [OP_RETURN_LOCALS] = "RETURN_LOCALS",
    [OP_LOCAL_GET] = "LOCAL_GET",
    [OP_LOCAL_SET] = "LOCAL_SET",
    [OP_LOCAL_SET_VOID] = "LOCAL_SET_VOID",
    [OP_LOCAL_GET0] = "LOCAL_GET0",
    [OP_LOCAL_GET1] = "LOCAL_GET1",
    [OP_LOCAL_GET2] = "LOCAL_GET2",
    [OP_LOCAL_GET3] = "LOCAL_GET3",
    [OP_ADD_INT] = "ADD_INT",
    [OP_SUB_INT] = "SUB_INT",
    [OP_MUL_INT] = "MUL_INT",
    [OP_LT_INT_JUMPIFNOT] = "LT_INT_JUMPIFNOT",
    [OP_NUMEQ_INT_JUMPIFNOT] = "NUMEQ_INT_JUMPIFNOT",
};

static const char *disassemble_atom_name(unsigned atom_id)
{
    if (atom_id < ctx.atom_table_cap && ctx.atom_table[atom_id])
        return ctx.atom_table[atom_id];
    return "<invalid-atom>";
}

void disassemble(code_object *code, const char *name)
{
    printf("=== %s ===\n", name ? name : "<anonymous>");
    printf("arity: %u%s\n", code->arity, code->has_rest ? "+" : "");

    // Print constants
    if (code->const_len > 0) {
        printf("constants:\n");
        for (unsigned i = 0; i < code->const_len; i++) {
            printf("  [%u] ", i);
            unsigned c = code->constants[i];
            if (c == 0) {
                printf("nil");
            } else {
                write_obj(c);
            }
            printf("\n");
        }
    }

    // Print code
    printf("code:\n");
    unsigned ip = 0;
    while (ip < code->code_len) {
        unsigned start = ip;
        unsigned size;
        printf("  %04u: ", ip);
        if (!instruction_fits(code->code, code->code_len, ip, &size)) {
            printf("<truncated> opcode=%u\n", code->code[ip]);
            break;
        }
        unsigned op = code->code[ip];
        ip += size;

        if (op < sizeof(opcode_names) / sizeof(opcode_names[0]) &&
            opcode_names[op]) {
            printf("%-12s", opcode_names[op]);
        } else {
            printf("UNKNOWN(%u)", op);
            printf("\n");
            continue;
        }

        switch (op) {
        case OP_CONST:
        case OP_CLOSURE:
            printf(" %u", code->code[start + 1]);
            break;
        case OP_LOOKUP:
        case OP_LOOKUP_ADD1:
        case OP_LOOKUP_SUB1:
            printf(" %s", disassemble_atom_name(code->code[start + 1]));
            if (code->code[start + 2] != 0xFFFFFFFF)
                printf(" [d=%u o=%u]", code->code[start + 2],
                       code->code[start + 3]);
            else
                printf(" [uncached]");
            break;
        case OP_DEFINE:
        case OP_SET:
        case OP_SET_VOID:
        case OP_DEFSYNTAX:
            printf(" %s", disassemble_atom_name(code->code[start + 1]));
            break;
        case OP_DEFINE_ALIAS:
            printf(" %s %s", disassemble_atom_name(code->code[start + 1]),
                   disassemble_atom_name(code->code[start + 2]));
            break;
        case OP_CALL:
        case OP_TAILCALL:
        case OP_VALUES:
        case OP_LETREC_MARK:
        case OP_RETURN_LOCALS:
        case OP_LOCAL_GET:
        case OP_LOCAL_SET:
        case OP_LOCAL_SET_VOID:
            printf(" %u", code->code[start + 1]);
            break;
        case OP_JUMP:
        case OP_JUMPIF:
        case OP_JUMPIFNOT:
        case OP_JUMPIFNULL:
        case OP_JUMPIFNOTNULL:
        case OP_JUMPIFZERO:
        case OP_JUMPIFNOTZERO:
        case OP_NUMEQ_JUMPIFNOT:
        case OP_LT_JUMPIFNOT:
        case OP_GT_JUMPIFNOT:
        case OP_LE_JUMPIFNOT:
        case OP_GE_JUMPIFNOT:
        case OP_NUMEQ_INT_JUMPIFNOT:
        case OP_LT_INT_JUMPIFNOT:
            printf(" -> %u", code->code[start + 1]);
            break;
        case OP_PRIM:
            printf(" %u argc=%u", code->code[start + 1],
                   code->code[start + 2]);
            break;
        default:
            // No operands
            break;
        }
        printf("\n");
    }

    // Recursively disassemble children
    for (unsigned i = 0; i < code->children_len; i++) {
        char child_name[64];
        snprintf(child_name, sizeof(child_name), "%s/lambda%u",
                 name ? name : "<anon>", i);
        disassemble(code->children[i], child_name);
    }
}
