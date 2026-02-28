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
#include <stdlib.h>
#include <string.h>

// ============================================================================
// Pattern Registry (for GC)
// ============================================================================

compiled_pattern *compiled_pattern_registry = NULL;

void pattern_register(compiled_pattern *pat)
{
    pat->gc_next = compiled_pattern_registry;
    compiled_pattern_registry = pat;
}

// ============================================================================
// Allocation and Cleanup
// ============================================================================

compiled_pattern *compiled_pattern_new(void)
{
    compiled_pattern *pat = calloc(1, sizeof(compiled_pattern));

    pat->code_cap = 32;
    pat->code = malloc(pat->code_cap * sizeof(pat_instruction));

    pat->const_cap = 8;
    pat->constants = malloc(pat->const_cap * sizeof(unsigned));

    pat->var_cap = 8;
    pat->var_slots = malloc(pat->var_cap * sizeof(pat_var_slot));

    // Register with GC
    pattern_register(pat);

    return pat;
}

void compiled_pattern_free(compiled_pattern *pat)
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
        pat->code_cap *= 2;
        pat->code = realloc(pat->code, pat->code_cap * sizeof(pat_instruction));
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
        pat->const_cap *= 2;
        pat->constants = realloc(pat->constants,
                                 pat->const_cap * sizeof(unsigned));
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
        pat->var_cap *= 2;
        pat->var_slots = realloc(pat->var_slots,
                                 pat->var_cap * sizeof(pat_var_slot));
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

// Update constants during GC (called before scan phase)
void gc_update_all_patterns(void)
{
    for (compiled_pattern *pat = compiled_pattern_registry; pat;
         pat = pat->gc_next) {
        // Update constants
        for (unsigned i = 0; i < pat->const_len; i++) {
            pat->constants[i] = collect(pat->constants[i]);
        }
        // Update variable atoms
        for (unsigned i = 0; i < pat->var_count; i++) {
            pat->var_slots[i].atom = collect(pat->var_slots[i].atom);
        }
    }
}

// Mark a pattern as reachable
void gc_mark_pattern(compiled_pattern *pat)
{
    if (!pat || pat->gc_marked)
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

    // Sweep: remove unreachable patterns from registry
    compiled_pattern **prev = &compiled_pattern_registry;
    while (*prev) {
        compiled_pattern *pat = *prev;
        if (!pat->gc_marked) {
            // Unlink from registry
            *prev = pat->gc_next;
            // Free the pattern
            compiled_pattern_free(pat);
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

void pattern_disassemble(compiled_pattern *pat)
{
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
        const char *name = (op < sizeof(opcode_names) / sizeof(opcode_names[0]))
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

// Check if symbol is in literals list
static bool is_literal(int64_t sym, unsigned literals)
{
    for (; literals; literals = cdr(literals)) {
        if (IS_ATOM(car(literals)) && CELL_ID(car(literals)) == sym)
            return true;
    }
    return false;
}

// Check if symbol is ellipsis
static bool is_ellipsis(unsigned x, int64_t ellipsis_id)
{
    if (!ellipsis_id)
        return false;
    return IS_ATOM(x) && CELL_ID(x) == ellipsis_id;
}

// Check if symbol is underscore
static bool is_underscore(unsigned x)
{
    return IS_KEYWORD(x, ctx.kw_underscore);
}

// Forward declaration
static void compile_pattern_node(unsigned pattern, pattern_compile_ctx *pctx);

// Compile ellipsis pattern: (elem ... rest)
static void compile_ellipsis(unsigned pattern, pattern_compile_ctx *pctx)
{
    unsigned elem = car(pattern);
    unsigned rest = cddr(pattern);  // Skip the ellipsis symbol

    pctx->ellipsis_depth++;

    // First, pre-scan element pattern to collect variables so we know
    // which variables need ELLIPSIS_LIST finalization.
    // We do this by remembering the variable count before and after.
    unsigned var_start = pctx->pattern->var_count;

    // Strategy: greedy with backtracking
    // Loop that tries rest first, on failure matches one element and retries.

    // Loop start
    unsigned loop_start = pattern_current_pos(pctx->pattern);

    // Push choice point: if rest fails, jump to elem_path
    pattern_emit(pctx->pattern, PAT_CHOICE_POINT, 0);  // Placeholder
    unsigned choice_patch = pattern_current_pos(pctx->pattern) - 1;

    // Try matching rest (success path)
    compile_pattern_node(rest, pctx);
    pattern_emit(pctx->pattern, PAT_COMMIT, 0);

    // Jump to success (skip elem path) - placeholder
    pattern_emit(pctx->pattern, PAT_JUMP, 0);
    unsigned success_patch = pattern_current_pos(pctx->pattern) - 1;

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

    // Emit ELLIPSIS_LIST for variables added during elem compilation
    unsigned success_pos = pattern_current_pos(pctx->pattern);
    for (unsigned i = var_start; i < pctx->pattern->var_count; i++) {
        if (pctx->pattern->var_slots[i].is_ellipsis &&
            pctx->pattern->var_slots[i].depth == pctx->ellipsis_depth) {
            pattern_emit(pctx->pattern, PAT_ELLIPSIS_LIST, i);
        }
    }

    // Patch success jump to point here (after ELLIPSIS_LIST instructions)
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
    if (is_underscore(pattern)) {
        pattern_emit(pctx->pattern, PAT_BIND_UNDERSCORE, 0);
        return;
    }

    // Atom: literal or variable
    if (IS_ATOM(pattern)) {
        int64_t sym = CELL_ID(pattern);

        if (is_literal(sym, pctx->literals)) {
            // Must match exact symbol
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
        pattern_emit(pctx->pattern, PAT_CHECK_VECTOR, 0);
        pattern_emit(pctx->pattern, PAT_CHECK_VECLEN, len);

        // TODO: Handle vector ellipsis and element matching
        // For now, just check length - full vector pattern support later
        (void)len;  // Suppress unused warning
        return;
    }

    // Pair: check for ellipsis
    if (IS_PAIR(pattern)) {
        // Check for ellipsis: (elem ... rest)
        if (cdr(pattern) && is_ellipsis(cadr(pattern), pctx->ellipsis_id)) {
            compile_ellipsis(pattern, pctx);
            return;
        }

        // Regular pair: match car and cdr
        pattern_emit(pctx->pattern, PAT_CHECK_PAIR, 0);
        pattern_emit(pctx->pattern, PAT_INPUT_CAR, 0);
        compile_pattern_node(car(pattern), pctx);
        pattern_emit(pctx->pattern, PAT_INPUT_POP, 0);
        pattern_emit(pctx->pattern, PAT_INPUT_CDR, 0);
        compile_pattern_node(cdr(pattern), pctx);
        pattern_emit(pctx->pattern, PAT_INPUT_POP, 0);
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

    compile_pattern_node(pattern, &pctx);
    pattern_emit(pctx.pattern, PAT_SUCCESS, 0);

    return pctx.pattern;
}

// ============================================================================
// Pattern Execution (Phase 3)
// ============================================================================

// Initialize match state
static void init_match_state(pat_match_state *state, compiled_pattern *pat,
                             unsigned input)
{
    memset(state, 0, sizeof(*state));

    state->input = input;
    state->pattern = pat;
    state->ip = 0;

    // Input stack
    state->input_cap = 32;
    state->input_stack = malloc(state->input_cap * sizeof(unsigned));
    state->input_sp = 0;

    // Bindings
    if (pat->var_count > 0) {
        state->bindings = calloc(pat->var_count, sizeof(unsigned));
        state->ellipsis_lists = calloc(pat->var_count, sizeof(unsigned));
        state->ellipsis_tails = calloc(pat->var_count, sizeof(unsigned));
    }

    // Choice points
    state->choice_cap = 16;
    state->choices = malloc(state->choice_cap * sizeof(pat_choice_point));
    state->choice_sp = 0;
}

// Cleanup match state
static void cleanup_match_state(pat_match_state *state)
{
    free(state->input_stack);
    free(state->bindings);
    free(state->ellipsis_lists);
    free(state->ellipsis_tails);
    free(state->choices);
}

// Push input onto stack
static void push_input(pat_match_state *state, unsigned input)
{
    if (state->input_sp >= state->input_cap) {
        state->input_cap *= 2;
        state->input_stack = realloc(state->input_stack,
                                     state->input_cap * sizeof(unsigned));
    }
    state->input_stack[state->input_sp++] = input;
}

// Pop input from stack
static unsigned pop_input(pat_match_state *state)
{
    if (state->input_sp == 0) {
        return 0;  // Should not happen
    }
    return state->input_stack[--state->input_sp];
}

// Push choice point
static void push_choice(pat_match_state *state, unsigned retry_ip)
{
    if (state->choice_sp >= state->choice_cap) {
        state->choice_cap *= 2;
        state->choices = realloc(state->choices,
                                 state->choice_cap * sizeof(pat_choice_point));
    }
    pat_choice_point *cp = &state->choices[state->choice_sp++];
    cp->input = state->input;
    cp->ip = retry_ip;
    // Save ellipsis accumulator lengths
    cp->accum_len = 0;  // TODO: proper save
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
    // Restore ellipsis accumulators (TODO)

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
    pat_match_state state;
    init_match_state(&state, pat, input);

    // Protect input and state bindings from GC
    gc_protect(&input);
    gc_protect(&state.input);

    bool matched = false;
    bool failed = false;

    while (!matched && !failed && state.ip < pat->code_len) {
        pat_instruction *instr = &pat->code[state.ip++];

        switch (instr->opcode) {
        case PAT_INPUT_CAR:
            push_input(&state, state.input);
            state.input = car(state.input);
            break;

        case PAT_INPUT_CDR:
            push_input(&state, state.input);
            state.input = cdr(state.input);
            break;

        case PAT_INPUT_POP:
            state.input = pop_input(&state);
            break;

        case PAT_INPUT_ADVANCE:
            // Advance to cdr without pushing (for ellipsis consumption)
            state.input = cdr(state.input);
            break;

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
            if (!IS_VECTOR(state.input)) {
                if (!backtrack(&state))
                    failed = true;
            }
            break;

        case PAT_CHECK_VECLEN:
            if (!IS_VECTOR(state.input) ||
                vector_len(state.input) != instr->operand) {
                if (!backtrack(&state))
                    failed = true;
            }
            break;

        case PAT_MATCH_ATOM_ID:
            if (!IS_ATOM(state.input) ||
                CELL_ID(state.input) != (int64_t)instr->operand) {
                if (!backtrack(&state))
                    failed = true;
            }
            break;

        case PAT_MATCH_LITERAL: {
            unsigned expected = pat->constants[instr->operand];
            // Simple equality check (TODO: proper equal?)
            if (CELL_TYPE(state.input) != CELL_TYPE(expected) ||
                CELL_ID(state.input) != CELL_ID(expected)) {
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
                state.choice_sp--;
            }
            break;

        case PAT_ELLIPSIS_SAVE:
            // No-op: we use current bindings directly in ELLIPSIS_ACCUM
            break;

        case PAT_ELLIPSIS_ACCUM: {
            // Append current bindings to ellipsis lists
            // Only accumulate variables at the current depth (operand)
            unsigned depth = instr->operand;
            for (unsigned i = 0; i < pat->var_count; i++) {
                if (pat->var_slots[i].is_ellipsis &&
                    pat->var_slots[i].depth == depth && state.bindings[i]) {
                    unsigned val = state.bindings[i];
                    gc_protect(&val);
                    unsigned new_cell = alloc_cons(val, 0);
                    gc_unprotect(1);

                    if (state.ellipsis_tails[i]) {
                        CELL_CDR(state.ellipsis_tails[i]) = new_cell;
                    } else {
                        state.ellipsis_lists[i] = new_cell;
                    }
                    state.ellipsis_tails[i] = new_cell;
                }
            }
            break;
        }

        case PAT_ELLIPSIS_LIST:
            // Ellipsis list is complete for this variable
            // Clear the binding so it won't be re-accumulated by outer loops
            state.bindings[instr->operand] = 0;
            break;

        case PAT_JUMP:
            state.ip = instr->operand;
            break;

        case PAT_SUCCESS:
            matched = true;
            break;

        default:
            failed = true;
            break;
        }
    }

    gc_unprotect(2);

    if (matched) {
        unsigned result = build_bindings_alist(&state);
        cleanup_match_state(&state);
        return result;
    }

    cleanup_match_state(&state);
    return TOK_ERROR;
}
