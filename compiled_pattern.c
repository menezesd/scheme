/**
 * @file compiled_pattern.c
 * @brief Compiled pattern matching for syntax-rules macros
 *
 * This file implements pattern compilation and execution for hygienic macros.
 * Patterns are compiled to bytecode at macro definition time and executed
 * during expansion.
 *
 * ## Lifecycle
 * 1. compile_pattern() creates a compiled_pattern from a syntax-rules pattern
 * 2. execute_pattern() runs the bytecode against macro input
 * 3. GC updates constants during collection
 * 4. gc_sweep_patterns() frees unreachable patterns
 */

#include "compiled_pattern.h"
#include "context.h"
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// ============================================================================
// Pattern Registry (for GC)
// ============================================================================

compiled_pattern *compiled_pattern_registry = NULL;
static pat_match_state *active_match_state = NULL;

void pattern_register(compiled_pattern *pat)
{
    if (!pat || compiled_pattern_is_registered(pat))
        return;
    pat->gc_next = compiled_pattern_registry;
    compiled_pattern_registry = pat;
}

static void pattern_unregister(compiled_pattern *pat)
{
    compiled_pattern **prev = &compiled_pattern_registry;
    while (*prev) {
        if (*prev == pat) {
            *prev = pat->gc_next;
            pat->gc_next = NULL;
            return;
        }
        prev = &(*prev)->gc_next;
    }
}

// ============================================================================
// Allocation and Cleanup
// ============================================================================

static void compiled_pattern_destroy_unregistered(compiled_pattern *pat);

static bool pattern_arrays_well_formed(const compiled_pattern *pat)
{
    return pat && pat->code_len <= pat->code_cap &&
           pat->const_len <= pat->const_cap && pat->var_count <= pat->var_cap &&
           (pat->code_len == 0 || pat->code != NULL) &&
           (pat->const_len == 0 || pat->constants != NULL) &&
           (pat->var_count == 0 || pat->var_slots != NULL);
}

static vector_data *pattern_vector_data(unsigned value)
{
    if (!IS_VECTOR(value))
        return NULL;
    vector_data *vd = GET_VECTOR_PTR(value);
    return vector_data_well_formed(vd) ? vd : NULL;
}

compiled_pattern *compiled_pattern_new(void)
{
    compiled_pattern *pat = checked_calloc_array(1, sizeof(compiled_pattern));
    if (!pat)
        return NULL;

    pat->code_cap = 32;
    pat->code = checked_malloc_array(pat->code_cap, sizeof(pat_instruction));
    if (!pat->code) {
        compiled_pattern_destroy_unregistered(pat);
        return NULL;
    }

    pat->const_cap = 8;
    pat->constants = checked_malloc_array(pat->const_cap, sizeof(unsigned));
    if (!pat->constants) {
        compiled_pattern_destroy_unregistered(pat);
        return NULL;
    }

    pat->var_cap = 8;
    pat->var_slots = checked_malloc_array(pat->var_cap, sizeof(pat_var_slot));
    if (!pat->var_slots) {
        compiled_pattern_destroy_unregistered(pat);
        return NULL;
    }

    // Register with GC
    pattern_register(pat);

    return pat;
}

void compiled_pattern_free(compiled_pattern *pat)
{
    if (!pat || !compiled_pattern_is_registered(pat))
        return;
    pattern_unregister(pat);
    free(pat->code);
    free(pat->constants);
    free(pat->var_slots);
    free(pat);
}

static void compiled_pattern_destroy_unregistered(compiled_pattern *pat)
{
    if (!pat)
        return;
    free(pat->code);
    free(pat->constants);
    free(pat->var_slots);
    free(pat);
}

// ============================================================================
// Bytecode Emission
// ============================================================================

void pattern_emit(compiled_pattern *pat, unsigned opcode, unsigned operand)
{
    if (pat->code_len >= pat->code_cap) {
        unsigned new_cap =
            checked_grow_capacity(pat->code_cap, sizeof(pat_instruction),
                                  "pattern_emit: capacity overflow");
        pat_instruction *new_code = checked_realloc_array(
            pat->code, new_cap, sizeof(pat_instruction));
        if (!new_code)
            lisp_panic("pattern_emit: realloc failed");
        pat->code = new_code;
        pat->code_cap = new_cap;
    }
    pat->code[pat->code_len].opcode = opcode;
    pat->code[pat->code_len].operand = operand;
    pat->code_len++;
}

unsigned pattern_current_pos(compiled_pattern *pat)
{
    return pat->code_len;
}

void pattern_patch(compiled_pattern *pat, unsigned pos, unsigned value)
{
    pat->code[pos].operand = value;
}

// ============================================================================
// Constant Pool
// ============================================================================

unsigned pattern_add_constant(compiled_pattern *pat, unsigned value)
{
    // Check if constant already exists
    for (unsigned i = 0; i < pat->const_len; i++) {
        if (pat->constants[i] == value)
            return i;
    }

    if (pat->const_len >= pat->const_cap) {
        unsigned new_cap = checked_grow_capacity(
            pat->const_cap, sizeof(unsigned),
            "pattern_add_constant: capacity overflow");
        unsigned *new_constants =
            checked_realloc_array(pat->constants, new_cap, sizeof(unsigned));
        if (!new_constants)
            lisp_panic("pattern_add_constant: realloc failed");
        pat->constants = new_constants;
        pat->const_cap = new_cap;
    }
    pat->constants[pat->const_len] = value;
    return pat->const_len++;
}

// ============================================================================
// Variable Slots
// ============================================================================

unsigned pattern_add_var(compiled_pattern *pat, unsigned atom, bool is_ellipsis,
                         unsigned depth)
{
    // Check if variable already exists
    for (unsigned i = 0; i < pat->var_count; i++) {
        if (pat->var_slots[i].atom == atom) {
            // Update ellipsis status if needed
            if (is_ellipsis && !pat->var_slots[i].is_ellipsis) {
                pat->var_slots[i].is_ellipsis = true;
                pat->var_slots[i].depth = depth;
            }
            return i;
        }
    }

    if (pat->var_count >= pat->var_cap) {
        unsigned new_cap =
            checked_grow_capacity(pat->var_cap, sizeof(pat_var_slot),
                                  "pattern_add_var: capacity overflow");
        pat_var_slot *new_slots = checked_realloc_array(
            pat->var_slots, new_cap, sizeof(pat_var_slot));
        if (!new_slots)
            lisp_panic("pattern_add_var: realloc failed");
        pat->var_slots = new_slots;
        pat->var_cap = new_cap;
    }

    pat->var_slots[pat->var_count].atom = atom;
    pat->var_slots[pat->var_count].is_ellipsis = is_ellipsis;
    pat->var_slots[pat->var_count].depth = depth;
    return pat->var_count++;
}

int pattern_find_var(compiled_pattern *pat, unsigned atom)
{
    for (unsigned i = 0; i < pat->var_count; i++) {
        if (pat->var_slots[i].atom == atom)
            return (int)i;
    }
    return -1;
}

// ============================================================================
// GC Integration
// ============================================================================

static void update_all_patterns(unsigned (*update)(unsigned))
{
    for (compiled_pattern *pat = compiled_pattern_registry; pat;
         pat = pat->gc_next) {
        if (!pattern_arrays_well_formed(pat))
            continue;
        // Update constants
        for (unsigned i = 0; i < pat->const_len; i++) {
            pat->constants[i] = update(pat->constants[i]);
        }
        // Update variable atoms
        for (unsigned i = 0; i < pat->var_count; i++) {
            pat->var_slots[i].atom = update(pat->var_slots[i].atom);
        }
    }
}

// Update constants during major GC (called before scan phase)
void gc_update_all_patterns(void)
{
    update_all_patterns(collect);
}

void minor_gc_update_all_patterns(void)
{
    update_all_patterns(collect_to_old);
}

static void update_match_state_roots(pat_match_state *state,
                                     unsigned (*update)(unsigned))
{
    if (!state)
        return;

    state->input = update(state->input);
    state->vec_iter_vec = update(state->vec_iter_vec);

    for (unsigned i = 0; i < state->input_sp; i++) {
        state->input_stack[i] = update(state->input_stack[i]);
    }
    for (unsigned i = 0; i < state->choice_sp; i++) {
        state->choices[i].input = update(state->choices[i].input);
        state->choices[i].vec_iter_vec = update(state->choices[i].vec_iter_vec);
        unsigned n = state->pattern ? state->pattern->var_count : 0;
        for (unsigned j = 0; j < n; j++) {
            if (state->choices[i].bindings)
                state->choices[i].bindings[j] =
                    update(state->choices[i].bindings[j]);
            if (state->choices[i].ellipsis_lists)
                state->choices[i].ellipsis_lists[j] =
                    update(state->choices[i].ellipsis_lists[j]);
            if (state->choices[i].ellipsis_tails)
                state->choices[i].ellipsis_tails[j] =
                    update(state->choices[i].ellipsis_tails[j]);
            if (state->choices[i].inner_lists)
                state->choices[i].inner_lists[j] =
                    update(state->choices[i].inner_lists[j]);
            if (state->choices[i].inner_tails)
                state->choices[i].inner_tails[j] =
                    update(state->choices[i].inner_tails[j]);
        }
    }

    unsigned n = state->pattern ? state->pattern->var_count : 0;
    for (unsigned i = 0; i < n; i++) {
        if (state->bindings)
            state->bindings[i] = update(state->bindings[i]);
        if (state->ellipsis_lists)
            state->ellipsis_lists[i] = update(state->ellipsis_lists[i]);
        if (state->ellipsis_tails)
            state->ellipsis_tails[i] = update(state->ellipsis_tails[i]);
        if (state->inner_lists)
            state->inner_lists[i] = update(state->inner_lists[i]);
        if (state->inner_tails)
            state->inner_tails[i] = update(state->inner_tails[i]);
    }
}

void gc_update_active_pattern_state(void)
{
    update_match_state_roots(active_match_state, collect);
}

void minor_gc_update_active_pattern_state(void)
{
    update_match_state_roots(active_match_state, collect_to_old);
}

bool compiled_pattern_is_registered(const compiled_pattern *needle)
{
    for (compiled_pattern *pat = compiled_pattern_registry; pat;
         pat = pat->gc_next) {
        if (pat == needle)
            return true;
    }
    return false;
}

bool is_compiled_pattern_object(unsigned value)
{
    return IS_CELL(value) && CELL_TYPE(value) == BT_COMPILED_PATTERN &&
           compiled_pattern_is_registered(
               (const compiled_pattern *)CELL_PTR(value));
}

// Mark a registered pattern as reachable.
void gc_mark_pattern(compiled_pattern *pat)
{
    if (!compiled_pattern_is_registered(pat) || pat->gc_marked)
        return;
    pat->gc_marked = true;
}

// Sweep unreachable patterns after GC
void gc_sweep_patterns(void)
{
    // First, clear all marks
    for (compiled_pattern *pat = compiled_pattern_registry; pat;
         pat = pat->gc_next) {
        pat->gc_marked = false;
    }

    // Walk the heap and mark patterns referenced by BT_COMPILED_PATTERN cells
    // Scan old generation: [mmin, hptr)
    for (unsigned i = ctx.mmin; i < ctx.hptr; i++) {
        if (CELL_TYPE(i) == BT_COMPILED_PATTERN) {
            compiled_pattern *pat = (compiled_pattern *)CELL_PTR(i);
            gc_mark_pattern(pat);
        }
    }
    // Scan nursery if generational GC is enabled
    if (ctx.card_table) {
        for (unsigned i = ctx.nursery_start; i < ctx.nursery_ptr; i++) {
            if (CELL_TYPE(i) == BT_COMPILED_PATTERN) {
                compiled_pattern *pat = (compiled_pattern *)CELL_PTR(i);
                gc_mark_pattern(pat);
            }
        }
    }
    if (active_match_state) {
        gc_mark_pattern(active_match_state->pattern);
    }

    // Sweep: remove unreachable patterns from registry
    compiled_pattern **prev = &compiled_pattern_registry;
    while (*prev) {
        compiled_pattern *pat = *prev;
        if (!pat->gc_marked) {
            // Unlink from registry
            *prev = pat->gc_next;
            // Free the pattern
            compiled_pattern_destroy_unregistered(pat);
        } else {
            prev = &pat->gc_next;
        }
    }
}

// ============================================================================
// Debug: Disassembler
// ============================================================================

static const char *opcode_names[] = {
    [PAT_INPUT_CAR] = "INPUT_CAR",
    [PAT_INPUT_CDR] = "INPUT_CDR",
    [PAT_INPUT_POP] = "INPUT_POP",
    [PAT_INPUT_ADVANCE] = "INPUT_ADVANCE",
    [PAT_CHECK_PAIR] = "CHECK_PAIR",
    [PAT_CHECK_NULL] = "CHECK_NULL",
    [PAT_CHECK_ATOM] = "CHECK_ATOM",
    [PAT_CHECK_VECTOR] = "CHECK_VECTOR",
    [PAT_CHECK_VECLEN] = "CHECK_VECLEN",
    [PAT_CHECK_VECLEN_MIN] = "CHECK_VECLEN_MIN",
    [PAT_VEC_ELLIPSIS_INIT] = "VEC_ELLIPSIS_INIT",
    [PAT_VEC_ELLIPSIS_NEXT] = "VEC_ELLIPSIS_NEXT",
    [PAT_INPUT_VEC_ITER] = "INPUT_VEC_ITER",
    [PAT_INPUT_VECREF_END] = "INPUT_VECREF_END",
    [PAT_MATCH_ATOM_ID] = "MATCH_ATOM_ID",
    [PAT_MATCH_LITERAL] = "MATCH_LITERAL",
    [PAT_BIND_VAR] = "BIND_VAR",
    [PAT_BIND_UNDERSCORE] = "BIND_UNDERSCORE",
    [PAT_CHOICE_POINT] = "CHOICE_POINT",
    [PAT_COMMIT] = "COMMIT",
    [PAT_ELLIPSIS_SAVE] = "ELLIPSIS_SAVE",
    [PAT_ELLIPSIS_ACCUM] = "ELLIPSIS_ACCUM",
    [PAT_ELLIPSIS_LIST] = "ELLIPSIS_LIST",
    [PAT_JUMP] = "JUMP",
    [PAT_SUCCESS] = "SUCCESS",
};

static bool pattern_jump_target_is_valid(compiled_pattern *pat,
                                         unsigned target)
{
    return target <= pat->code_len;
}

static bool pattern_bytecode_is_well_formed(compiled_pattern *pat)
{
    // Patterns are registry-managed.  Validate their backing-array metadata
    // before following instruction operands or allocating match-state arrays.
    if (!pat || !compiled_pattern_is_registered(pat) ||
        !pattern_arrays_well_formed(pat) || pat->code_len == 0)
        return false;

    for (unsigned i = 0; i < pat->code_len; i++) {
        pat_instruction *instr = &pat->code[i];

        if (instr->opcode >= PAT_OPCODE_COUNT)
            return false;

        switch (instr->opcode) {
        case PAT_MATCH_LITERAL:
            if (instr->operand >= pat->const_len)
                return false;
            break;

        case PAT_BIND_VAR:
            if (instr->operand >= pat->var_count)
                return false;
            break;

        case PAT_CHOICE_POINT:
        case PAT_VEC_ELLIPSIS_NEXT:
        case PAT_JUMP:
            if (!pattern_jump_target_is_valid(pat, instr->operand))
                return false;
            break;

        default:
            break;
        }
    }

    return true;
}

void pattern_disassemble(compiled_pattern *pat)
{
    if (!pat) {
        printf("=== Compiled Pattern: <null> ===\n");
        return;
    }
    if (!pattern_arrays_well_formed(pat)) {
        printf("=== Compiled Pattern: <invalid> ===\n");
        return;
    }

    printf("=== Compiled Pattern ===\n");
    printf("Variables (%u):\n", pat->var_count);
    for (unsigned i = 0; i < pat->var_count; i++) {
        printf("  [%u] atom=%u ellipsis=%d depth=%u\n", i,
               pat->var_slots[i].atom, pat->var_slots[i].is_ellipsis,
               pat->var_slots[i].depth);
    }
    printf("Constants (%u):\n", pat->const_len);
    for (unsigned i = 0; i < pat->const_len; i++) {
        printf("  [%u] %u\n", i, pat->constants[i]);
    }
    printf("Code (%u instructions):\n", pat->code_len);
    for (unsigned i = 0; i < pat->code_len; i++) {
        unsigned op = pat->code[i].opcode;
        unsigned operand = pat->code[i].operand;
        const char *name = (op < PAT_OPCODE_COUNT)
                               ? opcode_names[op]
                               : "???";
        printf("  %3u: %-16s %u\n", i, name ? name : "???", operand);
    }
    printf("========================\n");
}

// ============================================================================
// Pattern Compilation (Phase 2)
// ============================================================================

// Forward declarations for compiler
typedef struct {
    compiled_pattern *pattern;
    unsigned literals;
    int64_t ellipsis_id;
    unsigned ellipsis_depth;
} pattern_compile_ctx;

// Forward declaration
static void compile_pattern_node(unsigned pattern, pattern_compile_ctx *pctx);

// Compile ellipsis pattern: (elem ... rest)
static void compile_ellipsis(unsigned pattern, pattern_compile_ctx *pctx)
{
    unsigned elem = car(pattern);
    unsigned rest = cddr(pattern);  // Skip the ellipsis symbol

    // Strategy: greedy with backtracking
    // Loop that tries rest first, on failure matches one element and retries.

    // Loop start
    unsigned loop_start = pattern_current_pos(pctx->pattern);

    // Push choice point: if rest fails, jump to elem_path
    pattern_emit(pctx->pattern, PAT_CHOICE_POINT, 0);  // Placeholder
    unsigned choice_patch = pattern_current_pos(pctx->pattern) - 1;

    // Try matching rest (success path) — at current depth, so post-ellipsis
    // trailing patterns are NOT registered as ellipsis-bound variables.
    compile_pattern_node(rest, pctx);
    pattern_emit(pctx->pattern, PAT_COMMIT, 0);

    // Jump to success (skip elem path) - placeholder
    pattern_emit(pctx->pattern, PAT_JUMP, 0);
    unsigned success_patch = pattern_current_pos(pctx->pattern) - 1;

    // Now increment depth: element vars ARE ellipsis-bound.
    pctx->ellipsis_depth++;

    // Elem path: match one element, accumulate it, consume it, retry
    unsigned elem_path = pattern_current_pos(pctx->pattern);
    pattern_patch(pctx->pattern, choice_patch, elem_path);

    // Check that we have more input
    pattern_emit(pctx->pattern, PAT_CHECK_PAIR, 0);

    // Save bindings before matching element
    pattern_emit(pctx->pattern, PAT_ELLIPSIS_SAVE, 0);

    // Match car(input) against element pattern
    pattern_emit(pctx->pattern, PAT_INPUT_CAR, 0);
    compile_pattern_node(elem, pctx);
    pattern_emit(pctx->pattern, PAT_INPUT_POP, 0);

    // Accumulate the matched bindings (pass current depth as operand)
    pattern_emit(pctx->pattern, PAT_ELLIPSIS_ACCUM, pctx->ellipsis_depth);

    // Advance to cdr(input)
    pattern_emit(pctx->pattern, PAT_INPUT_ADVANCE, 0);

    pattern_emit(pctx->pattern, PAT_JUMP, loop_start);

    // Emit depth-based ELLIPSIS_LIST to finalize all vars at this depth
    unsigned success_pos = pattern_current_pos(pctx->pattern);
    pattern_emit(pctx->pattern, PAT_ELLIPSIS_LIST, pctx->ellipsis_depth);

    // Patch success jump to point here (after ELLIPSIS_LIST instruction)
    pattern_patch(pctx->pattern, success_patch, success_pos);

    pctx->ellipsis_depth--;
}

// Compile a pattern node
static void compile_pattern_node(unsigned pattern, pattern_compile_ctx *pctx)
{
    // Null: must match empty
    if (!pattern) {
        pattern_emit(pctx->pattern, PAT_CHECK_NULL, 0);
        return;
    }

    // Underscore: wildcard
    if (syntax_is_underscore(pattern)) {
        pattern_emit(pctx->pattern, PAT_BIND_UNDERSCORE, 0);
        return;
    }

    // Atom: literal or variable
    if (IS_ATOM(pattern)) {
        int64_t sym = CELL_ID(pattern);

        if (atom_list_contains_id(pctx->literals, sym)) {
            // Must match exact symbol
            pattern_emit(pctx->pattern, PAT_MATCH_ATOM_ID, (unsigned)sym);
        } else if (!identifier_valid(pattern)) {
            // Self-evaluating atom literal, such as #t or #f.
            pattern_emit(pctx->pattern, PAT_MATCH_ATOM_ID, (unsigned)sym);
        } else {
            // Pattern variable: bind it
            unsigned slot = pattern_add_var(pctx->pattern, pattern,
                                            pctx->ellipsis_depth > 0,
                                            pctx->ellipsis_depth);
            pattern_emit(pctx->pattern, PAT_BIND_VAR, slot);
        }
        return;
    }

    // Vector: check type and match elements
    if (IS_VECTOR(pattern)) {
        unsigned len = vector_len(pattern);
        unsigned *data = vector_data_ptr(pattern);

        // Check for ellipsis in vector
        unsigned ellipsis_pos = len;
        for (unsigned i = 0; i < len; i++) {
            if (syntax_is_ellipsis(data[i], pctx->ellipsis_id)) {
                ellipsis_pos = i;
                break;
            }
        }

        pattern_emit(pctx->pattern, PAT_CHECK_VECTOR, 0);

        if (ellipsis_pos < len) {
            // Vector has ellipsis at ellipsis_pos
            // Element before ellipsis is the repeated pattern
            if (ellipsis_pos == 0) {
                // No pattern before ellipsis - error
                pattern_emit(pctx->pattern, PAT_CHECK_NULL, 0);  // Force fail
                return;
            }

            unsigned elem_pattern = data[ellipsis_pos - 1];
            unsigned pre_count = ellipsis_pos - 1;
            unsigned post_count = len - ellipsis_pos - 1;
            if (pre_count > UINT16_MAX || post_count > UINT16_MAX) {
                pattern_emit(pctx->pattern, PAT_CHECK_NULL, 0);  // Force fail
                return;
            }

            // Check minimum length
            pattern_emit(pctx->pattern, PAT_CHECK_VECLEN_MIN,
                         pre_count + post_count);

            // Match pre-ellipsis elements (before the repeated pattern)
            for (unsigned i = 0; i < pre_count; i++) {
                pattern_emit(pctx->pattern, PAT_INPUT_VECREF, i);
                compile_pattern_node(data[i], pctx);
                pattern_emit(pctx->pattern, PAT_INPUT_POP, 0);
            }

            // Setup ellipsis iteration
            pctx->ellipsis_depth++;

            // Encode pre_count and post_count in single operand
            unsigned init_operand = pre_count | (post_count << 16);
            pattern_emit(pctx->pattern, PAT_VEC_ELLIPSIS_INIT, init_operand);

            // Loop start
            unsigned loop_start = pattern_current_pos(pctx->pattern);

            // Check if done, jump to done_label
            pattern_emit(pctx->pattern, PAT_VEC_ELLIPSIS_NEXT, 0);  // Placeholder
            unsigned done_patch = pattern_current_pos(pctx->pattern) - 1;

            // Save and match element
            pattern_emit(pctx->pattern, PAT_ELLIPSIS_SAVE, 0);
            pattern_emit(pctx->pattern, PAT_INPUT_VEC_ITER, 0);
            compile_pattern_node(elem_pattern, pctx);
            pattern_emit(pctx->pattern, PAT_INPUT_POP, 0);

            // Accumulate
            pattern_emit(pctx->pattern, PAT_ELLIPSIS_ACCUM, pctx->ellipsis_depth);

            // Jump back to loop start
            pattern_emit(pctx->pattern, PAT_JUMP, loop_start);

            // Done label
            unsigned done_label = pattern_current_pos(pctx->pattern);
            pattern_patch(pctx->pattern, done_patch, done_label);

            // Emit depth-based ELLIPSIS_LIST for all vars at this depth
            pattern_emit(pctx->pattern, PAT_ELLIPSIS_LIST, pctx->ellipsis_depth);

            pctx->ellipsis_depth--;

            // Match post-ellipsis elements (from end of vector)
            for (unsigned i = 0; i < post_count; i++) {
                pattern_emit(pctx->pattern, PAT_INPUT_VECREF_END, post_count - 1 - i);
                compile_pattern_node(data[ellipsis_pos + 1 + i], pctx);
                pattern_emit(pctx->pattern, PAT_INPUT_POP, 0);
            }
        } else {
            // No ellipsis - exact length match
            pattern_emit(pctx->pattern, PAT_CHECK_VECLEN, len);

            for (unsigned i = 0; i < len; i++) {
                pattern_emit(pctx->pattern, PAT_INPUT_VECREF, i);
                compile_pattern_node(data[i], pctx);
                pattern_emit(pctx->pattern, PAT_INPUT_POP, 0);
            }
        }
        return;
    }

    // Pair: compile an ordinary list spine iteratively.  A fixed-arity macro
    // pattern may have many arguments while remaining structurally shallow;
    // recursive cdr compilation would otherwise overflow the C stack.
    if (IS_PAIR(pattern)) {
        unsigned cdr_depth = 0;
        for (;;) {
            if (!IS_PAIR(pattern)) {
                compile_pattern_node(pattern, pctx);
                break;
            }
            if (IS_PAIR(cdr(pattern)) &&
                syntax_is_ellipsis(cadr(pattern), pctx->ellipsis_id)) {
                compile_ellipsis(pattern, pctx);
                break;
            }

            pattern_emit(pctx->pattern, PAT_CHECK_PAIR, 0);
            pattern_emit(pctx->pattern, PAT_INPUT_CAR, 0);
            compile_pattern_node(car(pattern), pctx);
            pattern_emit(pctx->pattern, PAT_INPUT_POP, 0);
            pattern_emit(pctx->pattern, PAT_INPUT_CDR, 0);
            cdr_depth++;
            pattern = cdr(pattern);
        }
        while (cdr_depth > 0) {
            pattern_emit(pctx->pattern, PAT_INPUT_POP, 0);
            cdr_depth--;
        }
        return;
    }

    // Other values (numbers, strings): match exactly
    unsigned const_idx = pattern_add_constant(pctx->pattern, pattern);
    pattern_emit(pctx->pattern, PAT_MATCH_LITERAL, const_idx);
}

compiled_pattern *compile_pattern(unsigned pattern, unsigned literals,
                                  int64_t ellipsis_id)
{
    pattern_compile_ctx pctx = {
        .pattern = compiled_pattern_new(),
        .literals = literals,
        .ellipsis_id = ellipsis_id,
        .ellipsis_depth = 0,
    };
    if (!pctx.pattern)
        return NULL;

    compile_pattern_node(pattern, &pctx);
    pattern_emit(pctx.pattern, PAT_SUCCESS, 0);

    return pctx.pattern;
}

// ============================================================================
// Pattern Execution (Phase 3)
// ============================================================================

static void free_match_arrays(pat_match_state *state)
{
    free(state->input_stack);
    free(state->bindings);
    free(state->ellipsis_lists);
    free(state->ellipsis_tails);
    free(state->inner_lists);
    free(state->inner_tails);
    state->input_stack = NULL;
    state->bindings = NULL;
    state->ellipsis_lists = NULL;
    state->ellipsis_tails = NULL;
    state->inner_lists = NULL;
    state->inner_tails = NULL;
}

static void free_choice_snapshot(pat_choice_point *cp)
{
    free(cp->bindings);
    free(cp->ellipsis_lists);
    free(cp->ellipsis_tails);
    free(cp->inner_lists);
    free(cp->inner_tails);
    cp->bindings = NULL;
    cp->ellipsis_lists = NULL;
    cp->ellipsis_tails = NULL;
    cp->inner_lists = NULL;
    cp->inner_tails = NULL;
}

// Initialize match state - returns false on allocation failure
static bool init_match_state(pat_match_state *state, compiled_pattern *pat,
                             unsigned input)
{
    memset(state, 0, sizeof(*state));

    state->input = input;
    state->pattern = pat;
    state->ip = 0;

    // Input stack
    state->input_cap = 32;
    state->input_stack =
        checked_malloc_array(state->input_cap, sizeof(unsigned));
    if (!state->input_stack)
        return false;
    state->input_sp = 0;

    // Bindings
    if (pat->var_count > 0) {
        state->bindings =
            checked_calloc_array(pat->var_count, sizeof(unsigned));
        state->ellipsis_lists =
            checked_calloc_array(pat->var_count, sizeof(unsigned));
        state->ellipsis_tails =
            checked_calloc_array(pat->var_count, sizeof(unsigned));
        state->inner_lists =
            checked_calloc_array(pat->var_count, sizeof(unsigned));
        state->inner_tails =
            checked_calloc_array(pat->var_count, sizeof(unsigned));
        if (!state->bindings || !state->ellipsis_lists ||
            !state->ellipsis_tails || !state->inner_lists ||
            !state->inner_tails) {
            free_match_arrays(state);
            return false;
        }
    }

    // Choice points
    state->choice_cap = 16;
    state->choices =
        checked_malloc_array(state->choice_cap, sizeof(pat_choice_point));
    if (!state->choices) {
        free_match_arrays(state);
        return false;
    }
    state->choice_sp = 0;
    return true;
}

// Cleanup match state
static void cleanup_match_state(pat_match_state *state)
{
    for (unsigned i = 0; i < state->choice_sp; i++) {
        free_choice_snapshot(&state->choices[i]);
    }
    free_match_arrays(state);
    free(state->choices);
    state->choices = NULL;
}

// Push input onto stack
static void push_input(pat_match_state *state, unsigned input)
{
    if (state->input_sp >= state->input_cap) {
        unsigned new_cap = checked_grow_capacity(
            state->input_cap, sizeof(unsigned),
            "pattern input stack: capacity overflow");
        unsigned *new_stack =
            checked_realloc_array(state->input_stack, new_cap,
                                  sizeof(unsigned));
        if (!new_stack)
            lisp_panic("pattern input stack: realloc failed");
        state->input_stack = new_stack;
        state->input_cap = new_cap;
    }
    state->input_stack[state->input_sp++] = input;
}

// Pop input from stack
static bool pop_input(pat_match_state *state, unsigned *input_out)
{
    if (state->input_sp == 0)
        return false;
    *input_out = state->input_stack[--state->input_sp];
    return true;
}

static unsigned *copy_choice_array(unsigned *src, unsigned len)
{
    if (!src || len == 0)
        return NULL;
    unsigned *copy = checked_malloc_array(len, sizeof(unsigned));
    if (!copy)
        lisp_panic("pattern choice snapshot: allocation failed");
    memcpy(copy, src, len * sizeof(unsigned));
    return copy;
}

// Push choice point
static void push_choice(pat_match_state *state, unsigned retry_ip)
{
    if (state->choice_sp >= state->choice_cap) {
        unsigned new_cap = checked_grow_capacity(
            state->choice_cap, sizeof(pat_choice_point),
            "pattern choice stack: capacity overflow");
        pat_choice_point *new_choices = checked_realloc_array(
            state->choices, new_cap, sizeof(pat_choice_point));
        if (!new_choices)
            lisp_panic("pattern choice stack: realloc failed");
        state->choices = new_choices;
        state->choice_cap = new_cap;
    }
    pat_choice_point *cp = &state->choices[state->choice_sp++];
    cp->input = state->input;
    cp->ip = retry_ip;
    cp->input_sp = state->input_sp;
    cp->vec_iter_vec = state->vec_iter_vec;
    cp->vec_iter_idx = state->vec_iter_idx;
    cp->vec_iter_count = state->vec_iter_count;
    cp->vec_iter_pre = state->vec_iter_pre;

    unsigned n = state->pattern ? state->pattern->var_count : 0;
    cp->bindings = copy_choice_array(state->bindings, n);
    cp->ellipsis_lists = copy_choice_array(state->ellipsis_lists, n);
    cp->ellipsis_tails = copy_choice_array(state->ellipsis_tails, n);
    cp->inner_lists = copy_choice_array(state->inner_lists, n);
    cp->inner_tails = copy_choice_array(state->inner_tails, n);
}

// Backtrack to previous choice point
static bool backtrack(pat_match_state *state)
{
    if (state->choice_sp == 0) {
        return false;
    }

    pat_choice_point *cp = &state->choices[--state->choice_sp];
    state->input = cp->input;
    state->ip = cp->ip;
    state->input_sp = cp->input_sp;
    state->vec_iter_vec = cp->vec_iter_vec;
    state->vec_iter_idx = cp->vec_iter_idx;
    state->vec_iter_count = cp->vec_iter_count;
    state->vec_iter_pre = cp->vec_iter_pre;

    unsigned n = state->pattern ? state->pattern->var_count : 0;
    if (n) {
        memcpy(state->bindings, cp->bindings, n * sizeof(unsigned));
        memcpy(state->ellipsis_lists, cp->ellipsis_lists,
               n * sizeof(unsigned));
        memcpy(state->ellipsis_tails, cp->ellipsis_tails,
               n * sizeof(unsigned));
        memcpy(state->inner_lists, cp->inner_lists, n * sizeof(unsigned));
        memcpy(state->inner_tails, cp->inner_tails, n * sizeof(unsigned));
    }
    free_choice_snapshot(cp);

    return true;
}

// Build bindings alist from match state
static unsigned build_bindings_alist(pat_match_state *state)
{
    unsigned result = 0;
    gc_protect(&result);

    for (unsigned i = 0; i < state->pattern->var_count; i++) {
        unsigned atom = state->pattern->var_slots[i].atom;
        unsigned value;

        if (state->pattern->var_slots[i].is_ellipsis) {
            value = state->ellipsis_lists[i];
            // Convert CELL_ATOM_TRUE sentinels (empty inner ellipsis)
            // back to nil in the binding list
            if (state->pattern->var_slots[i].depth >= 2 && value) {
                // Walk the list and replace #t with ()
                for (unsigned p = value; p && IS_PAIR(p); p = cdr(p)) {
                    if (car(p) == CELL_ATOM_TRUE)
                        cell_set_car(p, 0); // nil = empty list
                }
            }
        } else {
            value = state->bindings[i];
        }

        gc_protect(&atom);
        gc_protect(&value);
        unsigned binding = alloc_cons(atom, value);
        gc_protect(&binding);
        result = alloc_cons(binding, result);
        gc_unprotect(3);
    }

    gc_unprotect(1);
    return result;
}

unsigned execute_pattern(compiled_pattern *pat, unsigned input)
{
    if (!pattern_bytecode_is_well_formed(pat)) {
        show_error("pattern match: invalid compiled pattern");
        return TOK_ERROR;
    }

    pat_match_state state;
    if (!init_match_state(&state, pat, input)) {
        show_error("pattern match: out of memory");
        return TOK_ERROR;
    }

    // Protect input and state bindings from GC
    gc_protect(&input);
    gc_protect(&state.input);
    pat_match_state *prev_active_match_state = active_match_state;
    active_match_state = &state;

    bool matched = false;
    bool failed = false;

    while (!matched && !failed && state.ip < pat->code_len) {
        pat_instruction *instr = &pat->code[state.ip++];

        switch (instr->opcode) {
        case PAT_INPUT_CAR:
            if (!IS_PAIR(state.input)) {
                if (!backtrack(&state))
                    failed = true;
                break;
            }
            push_input(&state, state.input);
            state.input = car(state.input);
            break;

        case PAT_INPUT_CDR:
            if (!IS_PAIR(state.input)) {
                if (!backtrack(&state))
                    failed = true;
                break;
            }
            push_input(&state, state.input);
            state.input = cdr(state.input);
            break;

        case PAT_INPUT_POP:
            if (!pop_input(&state, &state.input)) {
                show_error("pattern match: invalid input stack state");
                failed = true;
            }
            break;

        case PAT_INPUT_ADVANCE:
            // Advance to cdr without pushing (for ellipsis consumption)
            if (!IS_PAIR(state.input)) {
                if (!backtrack(&state))
                    failed = true;
                break;
            }
            state.input = cdr(state.input);
            break;

        case PAT_INPUT_VECREF:
        {
            vector_data *vd = pattern_vector_data(state.input);
            if (!vd || instr->operand >= vd->len) {
                if (!backtrack(&state))
                    failed = true;
                break;
            }
            push_input(&state, state.input);
            state.input = vd->data[instr->operand];
            break;
        }

        case PAT_CHECK_PAIR:
            if (!IS_PAIR(state.input)) {
                if (!backtrack(&state))
                    failed = true;
            }
            break;

        case PAT_CHECK_NULL:
            if (state.input != 0) {
                if (!backtrack(&state))
                    failed = true;
            }
            break;

        case PAT_CHECK_ATOM:
            if (!IS_ATOM(state.input)) {
                if (!backtrack(&state))
                    failed = true;
            }
            break;

        case PAT_CHECK_VECTOR:
            if (!pattern_vector_data(state.input)) {
                if (!backtrack(&state))
                    failed = true;
            }
            break;

        case PAT_CHECK_VECLEN:
        {
            vector_data *vd = pattern_vector_data(state.input);
            if (!vd || vd->len != instr->operand) {
                if (!backtrack(&state))
                    failed = true;
            }
            break;
        }

        case PAT_MATCH_ATOM_ID:
            if (!IS_ATOM(state.input) ||
                CELL_ID(state.input) != (int64_t)instr->operand) {
                if (!backtrack(&state))
                    failed = true;
            }
            break;

        case PAT_MATCH_LITERAL: {
            unsigned expected = pat->constants[instr->operand];
            if (!deep_equal(state.input, expected)) {
                if (!backtrack(&state))
                    failed = true;
            }
            break;
        }

        case PAT_BIND_VAR:
            state.bindings[instr->operand] = state.input;
            break;

        case PAT_BIND_UNDERSCORE:
            // Accept anything, don't bind
            break;

        case PAT_CHOICE_POINT:
            push_choice(&state, instr->operand);
            break;

        case PAT_COMMIT:
            if (state.choice_sp > 0) {
                free_choice_snapshot(&state.choices[state.choice_sp - 1]);
                state.choice_sp--;
            }
            break;

        case PAT_ELLIPSIS_SAVE:
            // No-op: we use current bindings directly in ELLIPSIS_ACCUM
            break;

        case PAT_ELLIPSIS_ACCUM: {
            // Append current bindings to ellipsis lists at the given depth.
            // Accumulate vars with depth >= operand so nested ellipsis
            // variables (e.g., step in ((var init step ...) ...)) are
            // captured by the outer ellipsis too.
            unsigned depth = instr->operand;
            // Select accumulator arrays by depth
            unsigned *lists = (depth <= 1) ? state.ellipsis_lists
                                           : state.inner_lists;
            unsigned *tails = (depth <= 1) ? state.ellipsis_tails
                                           : state.inner_tails;
            for (unsigned i = 0; i < pat->var_count; i++) {
                if (pat->var_slots[i].is_ellipsis &&
                    pat->var_slots[i].depth >= depth && state.bindings[i]) {
                    unsigned val = state.bindings[i];
                    gc_protect(&val);
                    unsigned new_cell = alloc_cons(val, 0);
                    gc_unprotect(1);

                    if (tails[i]) {
                        cell_set_cdr(tails[i], new_cell);
                    } else {
                        lists[i] = new_cell;
                    }
                    tails[i] = new_cell;
                }
            }
            break;
        }

        case PAT_ELLIPSIS_LIST: {
            // Finalize ellipsis accumulation at given depth.
            // operand = depth level being finalized.
            unsigned depth = instr->operand;
            if (depth >= 2) {
                // Inner ellipsis: finalize inner_lists into bindings
                // so the outer ACCUM can pick them up.
                // Use CELL_ATOM_TRUE as sentinel for "matched but empty"
                // (distinguishes from 0 = "never matched")
                for (unsigned i = 0; i < pat->var_count; i++) {
                    if (pat->var_slots[i].is_ellipsis &&
                        pat->var_slots[i].depth >= depth) {
                        state.bindings[i] = state.inner_lists[i]
                            ? state.inner_lists[i]
                            : CELL_ATOM_TRUE; // sentinel for empty
                        state.inner_lists[i] = 0;
                        state.inner_tails[i] = 0;
                    }
                }
            } else {
                // Outermost ellipsis: clear bindings for all ellipsis vars
                // (ellipsis_lists already has the final result for
                //  build_bindings_alist to read)
                for (unsigned i = 0; i < pat->var_count; i++) {
                    if (pat->var_slots[i].is_ellipsis &&
                        pat->var_slots[i].depth >= 1) {
                        state.bindings[i] = 0;
                    }
                }
            }
            break;
        }

        case PAT_JUMP:
            state.ip = instr->operand;
            break;

        case PAT_SUCCESS:
            matched = true;
            break;

        case PAT_CHECK_VECLEN_MIN:
        {
            vector_data *vd = pattern_vector_data(state.input);
            if (!vd || vd->len < instr->operand) {
                if (!backtrack(&state))
                    failed = true;
            }
            break;
        }

        case PAT_VEC_ELLIPSIS_INIT: {
            // Setup vector ellipsis iteration
            // operand = pre_count | (post_count << 16)
            vector_data *vd = pattern_vector_data(state.input);
            if (!vd) {
                if (!backtrack(&state))
                    failed = true;
                break;
            }
            unsigned pre_count = instr->operand & 0xFFFF;
            unsigned post_count = instr->operand >> 16;
            unsigned len = vd->len;
            // Validate bounds to prevent underflow
            if (pre_count + post_count > len) {
                if (!backtrack(&state))
                    failed = true;
                break;
            }
            state.vec_iter_vec = state.input;
            state.vec_iter_pre = pre_count;
            state.vec_iter_count = len - pre_count - post_count;
            state.vec_iter_idx = 0;
            break;
        }

        case PAT_VEC_ELLIPSIS_NEXT:
            // Check if iteration is done
            if (state.vec_iter_idx >= state.vec_iter_count) {
                state.ip = instr->operand;  // Jump to done label
            } else {
                state.vec_iter_idx++;
            }
            break;

        case PAT_INPUT_VEC_ITER: {
            // Access current ellipsis iteration element
            // idx is incremented before access, so ranges from 1 to vec_iter_count
            vector_data *vd = pattern_vector_data(state.vec_iter_vec);
            if (!vd || state.vec_iter_idx == 0) {
                if (!backtrack(&state))
                    failed = true;
                break;
            }
            unsigned idx = state.vec_iter_pre + state.vec_iter_idx - 1;
            if (idx >= vd->len) {
                if (!backtrack(&state))
                    failed = true;
                break;
            }
            push_input(&state, state.input);
            state.input = vd->data[idx];
            break;
        }

        case PAT_INPUT_VECREF_END: {
            // Access element from end of vector (operand = offset from end)
            vector_data *vd = pattern_vector_data(state.input);
            if (!vd) {
                if (!backtrack(&state))
                    failed = true;
                break;
            }
            unsigned len = vd->len;
            if (instr->operand >= len) {
                if (!backtrack(&state))
                    failed = true;
                break;
            }
            push_input(&state, state.input);
            state.input = vd->data[len - 1 - instr->operand];
            break;
        }

        default:
            failed = true;
            break;
        }
    }

    gc_unprotect(2);

    if (matched) {
        unsigned result = build_bindings_alist(&state);
        active_match_state = prev_active_match_state;
        cleanup_match_state(&state);
        return result;
    }

    active_match_state = prev_active_match_state;
    cleanup_match_state(&state);
    return TOK_ERROR;
}
