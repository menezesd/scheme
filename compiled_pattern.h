/**
 * @file compiled_pattern.h
 * @brief Compiled pattern matching for syntax-rules macros
 *
 * This file defines the bytecode instruction set and data structures for
 * compiled pattern matching. Patterns are compiled at macro definition time
 * and executed during macro expansion, replacing recursive tree-walking.
 *
 * ## Design Overview
 * - Patterns compile to linear bytecode with explicit backtracking
 * - Choice points enable greedy ellipsis matching with backtracking
 * - Bindings stored in dense array indexed by variable slot
 * - Integrates with existing GC via registry (like code_object)
 */

#ifndef COMPILED_PATTERN_H
#define COMPILED_PATTERN_H

#include "types.h"
#include <stdbool.h>

// ============================================================================
// Pattern Bytecode Opcodes
// ============================================================================

/**
 * Pattern matching instruction opcodes.
 *
 * Instructions are 2 words: opcode + operand (operand may be unused).
 * The matcher uses an input stack for navigation and a choice stack for
 * backtracking.
 */
enum pat_opcode {
    // Navigation - manipulate current input position
    PAT_INPUT_CAR,       // Push input, set input = car(input)
    PAT_INPUT_CDR,       // Push input, set input = cdr(input)
    PAT_INPUT_POP,       // Pop and restore previous input
    PAT_INPUT_ADVANCE,   // Set input = cdr(input) without push (for ellipsis)
    PAT_INPUT_VECREF,    // Push input, set input = vector_data_ptr(input)[operand]

    // Type checks - fail (backtrack) if check fails
    PAT_CHECK_PAIR,      // Fail if !IS_PAIR(input)
    PAT_CHECK_NULL,      // Fail if input != 0
    PAT_CHECK_ATOM,      // Fail if !IS_ATOM(input)
    PAT_CHECK_VECTOR,    // Fail if !IS_VECTOR(input)
    PAT_CHECK_VECLEN,    // Fail if vector_len(input) != operand
    PAT_CHECK_VECLEN_MIN,// Fail if vector_len(input) < operand

    // Vector ellipsis iteration
    PAT_VEC_ELLIPSIS_INIT,  // Setup iteration: pre | (post << 16); saves the
                            // enclosing iterator so vector ellipses can nest
    PAT_VEC_ELLIPSIS_NEXT,  // If done, jump to operand; else advance
    PAT_VEC_ELLIPSIS_DONE,  // Restore the enclosing iterator saved by INIT
    PAT_INPUT_VEC_ITER,     // Push input, set input = vec[pre_count + iter_idx]
    PAT_INPUT_VECREF_END,   // Push input, set input = vec[len - 1 - operand]

    // Matching - compare input against expected value
    PAT_MATCH_ATOM_ID,   // Fail if !IS_ATOM(input) || CELL_ID(input) != operand
    PAT_MATCH_LITERAL,   // Fail if input != constants[operand]

    // Variable binding
    PAT_BIND_VAR,        // bindings[operand] = input
    PAT_BIND_UNDERSCORE, // Accept any input, no binding (wildcard)

    // Ellipsis - choice points and accumulation
    PAT_CHOICE_POINT,    // Push (input, operand) onto choice stack
    PAT_COMMIT,          // Pop choice point without backtracking (success path)
    PAT_ELLIPSIS_SAVE,   // Save current bindings for ellipsis accumulation
    PAT_ELLIPSIS_ACCUM,  // Append saved bindings to ellipsis lists
    PAT_ELLIPSIS_LIST,   // Finalize: bindings[operand] = reverse(ellipsis_list[operand])

    // Control flow
    PAT_JUMP,            // ip = operand
    PAT_SUCCESS,         // Pattern matched successfully
    PAT_OPCODE_COUNT,
};

// ============================================================================
// Compiled Pattern Structure
// ============================================================================

/**
 * Instruction format: opcode + operand
 * Some opcodes ignore the operand.
 */
typedef struct {
    unsigned opcode;
    unsigned operand;
} pat_instruction;

/**
 * Variable slot metadata.
 * Tracks which pattern variables exist and their ellipsis status.
 */
typedef struct {
    unsigned atom;       // Variable atom (cell index for symbol)
    bool is_ellipsis;    // True if this variable is under ellipsis
    unsigned depth;      // Ellipsis nesting depth (0 = not under ellipsis)
} pat_var_slot;

/**
 * Compiled pattern object.
 * Created at macro definition time, executed during expansion.
 */
typedef struct compiled_pattern {
    // Bytecode
    pat_instruction *code;
    unsigned code_len;
    unsigned code_cap;

    // Constant pool - literals to match (cell indices, updated by GC)
    unsigned *constants;
    unsigned const_len;
    unsigned const_cap;

    // Variable slots
    pat_var_slot *var_slots;
    unsigned var_count;
    unsigned var_cap;

    // GC registry (linked list of all patterns)
    struct compiled_pattern *gc_next;
    bool gc_marked;
} compiled_pattern;

// ============================================================================
// Pattern Matching State (for execution)
// ============================================================================

/**
 * Choice point for backtracking.
 * Saved when entering an ellipsis, restored on match failure.
 */
typedef struct {
    unsigned input;      // Input position when choice was made
    unsigned ip;         // Instruction pointer for retry path
    unsigned input_sp;   // Input stack depth
    unsigned vec_iter_vec;
    unsigned vec_iter_idx;
    unsigned vec_iter_count;
    unsigned vec_iter_pre;
    unsigned *bindings;
    unsigned *bound; // Per-var flags: bindings[i] is a real match this
                     // iteration (distinguishes a () binding from unbound)
    // Flat copies of the per-depth accumulators (max_depth * var_count)
    unsigned *level_lists;
    unsigned *level_tails;
    // Snapshot of the saved enclosing vector iterators
    struct pat_vec_iter *vec_stack;
    unsigned vec_sp;
} pat_choice_point;

/**
 * Execution state for pattern matching.
 */
typedef struct {
    // Input navigation
    unsigned input;          // Current input (cell index)
    unsigned *input_stack;   // Stack of saved input positions
    unsigned input_sp;
    unsigned input_cap;

    // Pattern being executed
    compiled_pattern *pattern;
    unsigned ip;             // Instruction pointer

    // Variable bindings (indexed by var slot)
    unsigned *bindings;      // Current binding for each variable
    unsigned *bound;         // Nonzero if bindings[i] holds a real match
                             // (distinguishes a () binding from unbound)

    // Ellipsis accumulation: one lists/tails pair per nesting depth, stored
    // flat as max_depth rows of var_count entries. Row 0 (depth 1, the
    // outermost ellipsis) holds the final result read by build_bindings;
    // deeper rows are temporaries finalized into bindings[] per group.
    unsigned *level_lists;
    unsigned *level_tails;
    unsigned max_depth; // Number of accumulator rows (>= 1)

    // Backtracking
    pat_choice_point *choices;
    unsigned choice_sp;
    unsigned choice_cap;

    // Vector ellipsis iteration state (current innermost iterator)
    unsigned vec_iter_vec;   // Vector being iterated
    unsigned vec_iter_idx;   // Current iteration index
    unsigned vec_iter_count; // Total iterations
    unsigned vec_iter_pre;   // Pre-ellipsis element count

    // Enclosing iterators saved by nested PAT_VEC_ELLIPSIS_INIT
    struct pat_vec_iter *vec_stack;
    unsigned vec_sp;
    unsigned vec_cap;
} pat_match_state;

// Saved vector iterator (for nested vector ellipses)
typedef struct pat_vec_iter {
    unsigned vec;
    unsigned idx;
    unsigned count;
    unsigned pre;
} pat_vec_iter;

// ============================================================================
// Compiled Pattern API
// ============================================================================

// Allocation and cleanup
compiled_pattern *compiled_pattern_new(void);
void compiled_pattern_free(compiled_pattern *pat);
bool compiled_pattern_is_registered(const compiled_pattern *needle);
bool is_compiled_pattern_object(unsigned value);

// Bytecode emission
void pattern_emit(compiled_pattern *pat, unsigned opcode, unsigned operand);
unsigned pattern_current_pos(compiled_pattern *pat);
void pattern_patch(compiled_pattern *pat, unsigned pos, unsigned value);

// Constant pool
unsigned pattern_add_constant(compiled_pattern *pat, unsigned value);

// Variable slots
unsigned pattern_add_var(compiled_pattern *pat, unsigned atom, bool is_ellipsis,
                         unsigned depth);
int pattern_find_var(compiled_pattern *pat, unsigned atom);

// Pattern compilation
compiled_pattern *compile_pattern(unsigned pattern, unsigned literals,
                                  int64_t ellipsis_id);

// Pattern execution
unsigned execute_pattern(compiled_pattern *pat, unsigned input);

// GC integration
void pattern_register(compiled_pattern *pat);
void gc_update_all_patterns(void);
void minor_gc_update_all_patterns(void);
void gc_update_active_pattern_state(void);
void minor_gc_update_active_pattern_state(void);
void gc_sweep_patterns(void);
void gc_mark_pattern(compiled_pattern *pat);

// Debug
void pattern_disassemble(compiled_pattern *pat);

// Mnemonic for a pattern opcode, or "???" if out of range or unnamed.
// Exposed so tests can assert every opcode in the enum has a name.
const char *pattern_opcode_name(unsigned op);

// Global registry
extern compiled_pattern *compiled_pattern_registry;

#endif // COMPILED_PATTERN_H
