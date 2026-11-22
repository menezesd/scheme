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

bool is_jump_opcode(unsigned op)
{
    switch (op) {
    case OP_GUARD_PRIMITIVE:
    case OP_RECURSE:
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
    if (op >= OP_COUNT)
        return 0;
    switch (op) {
    case OP_GUARD_PRIMITIVE:
        return 8;
    case OP_RECURSE:
    case OP_LOOKUP:
        return 4; // opcode + sym_id + cached_depth + cached_offset
    case OP_NUMEQ_JUMPIFNOT:
    case OP_NUMEQ_INT_JUMPIFNOT:
    case OP_LT_JUMPIFNOT:
    case OP_LT_INT_JUMPIFNOT:
    case OP_GT_JUMPIFNOT:
    case OP_LE_JUMPIFNOT:
    case OP_GE_JUMPIFNOT:
    case OP_TRMC_PUSH:
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
    case OP_TAILCALL_TRMC:
    case OP_JUMP:
    case OP_JUMPIF:
    case OP_JUMPIFNOT:
    case OP_JUMPIFNULL:
    case OP_JUMPIFNOTNULL:
    case OP_JUMPIFZERO:
    case OP_JUMPIFNOTZERO:
    case OP_DEFSYNTAX:
    case OP_LETREC_MARK:
        return 2; // opcode + 1 operand
    case OP_DEFINE_ALIAS:
    case OP_DEFINE_ALIAS_REF:
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

// CSE pass: detect consecutive identical CONST/LOOKUP and use DUP, and fuse
// comparisons with a following JUMPIFNOT. This pass rebuilds the code array
// to handle instruction size changes. (Constant arithmetic needs no pass
// here: compile_call folds calls whose arguments are all constants, and a
// mutable binding's GUARD_PRIMITIVE separates the operands from the opcode.)
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
    if (code)
        code->verified = false; // the instruction array is about to change
    if (!code || code->optimizing || code->code_len < 2 ||
        code->code_len > code->code_cap ||
        code->children_len > code->children_cap || !code->code ||
        (code->children_len != 0 && !code->children))
        return;

    if (!code_is_well_formed(code->code, code->code_len))
        return;

    code->optimizing = true;

    // Run CSE pass first (handles instruction size changes separately)
    cse_pass(code);

    unsigned *c = code->code;
    unsigned len = code->code_len;

    // Collect all jump targets - needed to avoid unsafe fusions
    bool *is_jump_target = calloc_array_plus_one(len, sizeof(bool));
    if (!is_jump_target) {
        code->optimizing = false;
        return;
    }
    mark_jump_targets(c, len, is_jump_target);

    // First pass: identify which bytes to remove and build offset map
    // offset_map[old] = new offset after compaction
    unsigned *offset_map = checked_calloc_array(len, sizeof(unsigned));
    if (!offset_map) {
        free(is_jump_target);
        code->optimizing = false;
        return;
    }

    bool *remove = checked_calloc_array(len, sizeof(bool));
    if (!remove) {
        free(offset_map);
        free(is_jump_target);
        code->optimizing = false;
        return;
    }

    // Reuse the relocation table as an instruction-boundary map until
    // compaction. Peephole replacements preserve widths until then.
    // (The code was checked well formed above and cse_pass preserves that,
    // so a zero size cannot occur; stop rather than spin if it ever does.)
    for (unsigned i = 0, size; i < len && (size = instruction_size(c[i])) != 0;
         i += size)
        offset_map[i] = 1;

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

        // Only two-instruction cxr fusions exist. The compiler never emits a
        // composite accessor itself and a fused instruction is not examined
        // again, so patterns starting from CDDR/CADR (or three-op chains,
        // shadowed by the pairs below) could never fire.
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
            while (visited_count < max_chain && target < len &&
                   offset_map[target] && c[target] == OP_JUMP) {
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

        // Only the first operand is a branch target. Other operands (such
        // as RECURSE's arity and lexical depth) are data even if their values
        // happen to equal opcode numbers.
        for (unsigned j = 1; j < size; j++) {
            unsigned operand = c[read + j];
            if (j == 1 && is_jump_opcode(op))
                operand = operand < len ? offset_map[operand] : final_len;
            c[write++] = operand;
        }
        read += size;
    }

    code->code_len = write;

    free(offset_map);
    free(remove);
    free(is_jump_target);

    // Recursively optimize children
    for (unsigned i = 0; i < code->children_len; i++) {
        peephole_optimize(code->children[i]);
    }
    code->optimizing = false;
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
    [OP_DEFINE_ALIAS_REF] = "DEFINE_ALIAS_REF",
    [OP_SET] = "SET",
    [OP_CLOSURE] = "CLOSURE",
    [OP_CALL] = "CALL",
    [OP_TAILCALL] = "TAILCALL",
    [OP_RECURSE] = "RECURSE",
    [OP_GUARD_PRIMITIVE] = "GUARD_PRIMITIVE",
    [OP_RETURN] = "RETURN",
    [OP_JUMP] = "JUMP",
    [OP_JUMPIF] = "JUMPIF",
    [OP_JUMPIFNOT] = "JUMPIFNOT",
    [OP_PRIM] = "PRIM",
    [OP_PUSHCONT] = "PUSHCONT",
    [OP_CALLCC] = "CALLCC",
    [OP_PUSHENV] = "PUSHENV",
    [OP_POPENV] = "POPENV",
    [OP_CALLWITHVALUES] = "CALLWITHVALUES",
    [OP_CWV_FINISH] = "CWV_FINISH",
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
    // Vector operations
    [OP_VECTORREF] = "VECTORREF",
    [OP_VECTORSET] = "VECTORSET",
    [OP_VECTORLEN] = "VECTORLEN",
    [OP_RECORDOF] = "RECORDOF",
    [OP_RECORDREF] = "RECORDREF",
    // Numeric operations
    [OP_ABS] = "ABS",
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
    [OP_TRMC_INIT] = "TRMC_INIT",
    [OP_TRMC_APPEND] = "TRMC_APPEND",
    [OP_TRMC_SPLICE] = "TRMC_SPLICE",
    [OP_TRMC_PUSH] = "TRMC_PUSH",
    [OP_TAILCALL_TRMC] = "TAILCALL_TRMC",
    [OP_ERROR_RETURN] = "ERROR_RETURN",
    [OP_LOAD_STEP] = "LOAD_STEP",
};

static const char *disassemble_atom_name(unsigned atom_id)
{
    if (atom_id < ctx.atom_table_cap && ctx.atom_table[atom_id])
        return ctx.atom_table[atom_id];
    return "<invalid-atom>";
}

void disassemble(code_object *code, const char *name)
{
    if (!code) {
        printf("=== %s: <null> ===\n", name ? name : "<anonymous>");
        return;
    }
    if (code->disassembling) {
        printf("=== %s: <cycle> ===\n", name ? name : "<anonymous>");
        return;
    }
    if (code->code_len > code->code_cap ||
        code->const_len > code->const_cap ||
        code->children_len > code->children_cap ||
        (code->code_len != 0 && !code->code) ||
        (code->const_len != 0 && !code->constants) ||
        (code->children_len != 0 && !code->children)) {
        printf("=== %s: <invalid> ===\n", name ? name : "<anonymous>");
        return;
    }

    code->disassembling = true;
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
        case OP_GUARD_PRIMITIVE:
            printf(" %u %s prim=%u argc=%u tail=%u", code->code[start + 1],
                   disassemble_atom_name(code->code[start + 2]),
                   code->code[start + 3], code->code[start + 4], code->code[start + 5]);
            break;
        case OP_RECURSE:
            printf(" %u argc=%u depth=%u", code->code[start + 1],
                   code->code[start + 2], code->code[start + 3]);
            break;
        case OP_TRMC_PUSH:
        case OP_CONST:
        case OP_CLOSURE:
            printf(" %u", code->code[start + 1]);
            break;
        case OP_LOOKUP:
            printf(" %s", disassemble_atom_name(code->code[start + 1]));
            if (code->code[start + 2] != 0xFFFFFFFF)
                printf(" [ic=%u]", code->code[start + 2]);
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
        case OP_DEFINE_ALIAS_REF:
            printf(" %s const[%u]",
                   disassemble_atom_name(code->code[start + 1]),
                   code->code[start + 2]);
            break;
        case OP_CALL:
        case OP_TAILCALL:
        case OP_TAILCALL_TRMC:
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
            printf(" %u argc=%u%s", code->code[start + 1],
                   code->code[start + 2] & ~PRIM_TAIL_FLAG,
                   (code->code[start + 2] & PRIM_TAIL_FLAG) ? " tail" : "");
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
        if (code_object_is_registered(code->children[i]))
            disassemble(code->children[i], child_name);
        else
            printf("=== %s: <invalid child> ===\n", child_name);
    }
    code->disassembling = false;
}
