/**
 * @file code_object.c
 * @brief Code object management and GC integration
 *
 * This file manages compiled code objects - their creation, destruction,
 * and integration with the garbage collector.
 *
 * ## Code Object Lifecycle
 * 1. code_new() creates a new code object and registers it with GC
 * 2. Compiler fills in bytecode, constants, and children
 * 3. GC updates constants during collection (gc_update_all_code_objects)
 * 4. gc_sweep_code_objects() frees unreachable code objects
 */

#include "bytecode.h"
#include "compile_internal.h"
#include "context.h"
#include <stdlib.h>

// ============================================================================
// Code Object Registry
// ============================================================================

// Global registry of all code objects for GC integration
code_object *code_object_registry = NULL;

// Register a code object with the GC registry
void code_register(code_object *code)
{
    code->gc_next = code_object_registry;
    code_object_registry = code;
}

// ============================================================================
// Code Object Creation and Destruction
// ============================================================================

code_object *code_new(void)
{
    code_object *c = calloc(1, sizeof(code_object));
    if (!c)
        return NULL;

    c->code_cap = 64;
    c->code = malloc(c->code_cap * sizeof(unsigned));
    if (!c->code) {
        free(c);
        return NULL;
    }

    c->const_cap = 16;
    c->constants = malloc(c->const_cap * sizeof(unsigned));
    if (!c->constants) {
        free(c->code);
        free(c);
        return NULL;
    }

    c->children_cap = 4;
    c->children = malloc(c->children_cap * sizeof(code_object *));
    if (!c->children) {
        free(c->constants);
        free(c->code);
        free(c);
        return NULL;
    }

    // Register with GC
    code_register(c);
    return c;
}

void code_free(code_object *code)
{
    if (!code)
        return;
    free(code->code);
    free(code->constants);
    for (unsigned i = 0; i < code->children_len; i++) {
        code_free(code->children[i]);
    }
    free(code->children);
    free(code);
}

// ============================================================================
// Code Object Mutation
// ============================================================================

void code_emit(code_object *code, unsigned instr)
{
    if (code->code_len >= code->code_cap) {
        code->code_cap *= 2;
        code->code = realloc(code->code, code->code_cap * sizeof(unsigned));
    }
    code->code[code->code_len++] = instr;
}

unsigned code_add_const(code_object *code, unsigned val)
{
    // Check if constant already exists
    for (unsigned i = 0; i < code->const_len; i++) {
        if (code->constants[i] == val)
            return i;
    }
    if (code->const_len >= code->const_cap) {
        code->const_cap *= 2;
        code->constants =
            realloc(code->constants, code->const_cap * sizeof(unsigned));
    }
    code->constants[code->const_len] = val;
    return code->const_len++;
}

unsigned code_add_child(code_object *code, code_object *child)
{
    if (code->children_len >= code->children_cap) {
        code->children_cap *= 2;
        code->children =
            realloc(code->children, code->children_cap * sizeof(code_object *));
    }
    code->children[code->children_len] = child;
    return code->children_len++;
}

unsigned code_current_pos(code_object *code) { return code->code_len; }

void code_patch(code_object *code, unsigned pos, unsigned val)
{
    code->code[pos] = val;
}

// ============================================================================
// GC Integration
// ============================================================================

unsigned gc_collect_code(code_object *code)
{
    if (!code)
        return 0;

    // Collect constants
    for (unsigned i = 0; i < code->const_len; i++) {
        code->constants[i] = collect(code->constants[i]);
    }

    // Recursively collect children
    for (unsigned i = 0; i < code->children_len; i++) {
        gc_collect_code(code->children[i]);
    }

    return 0;
}

// Update all code object constants during GC
// Called from gc() in context.c BEFORE the scan phase
void gc_update_all_code_objects(void)
{
    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        // Collect constants - the scan phase will then process their CAR/CDR
        for (unsigned i = 0; i < code->const_len; i++) {
            code->constants[i] = collect(code->constants[i]);
        }
    }
}

// Mark a code object and all its children as reachable
static void mark_code_object(code_object *code)
{
    if (!code || code->gc_marked)
        return;
    code->gc_marked = true;
    // Mark children recursively
    for (unsigned i = 0; i < code->children_len; i++) {
        mark_code_object(code->children[i]);
    }
}

// Sweep unreachable code objects after GC
// Call this after the heap GC is complete
void gc_sweep_code_objects(void)
{
    // First, clear all marks
    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        code->gc_marked = false;
    }

    // Walk the heap and mark code objects referenced by closures
    // Scan old generation: [mmin, hptr)
    for (unsigned i = ctx.mmin; i < ctx.hptr; i++) {
        if (CELL_TYPE(i) == BT_CLOSURE) {
            code_object *code = (code_object *)CELL_PTR(i);
            mark_code_object(code);
        }
    }
    // Scan nursery if generational GC is enabled: [nursery_start, nursery_ptr)
    if (ctx.card_table) {
        for (unsigned i = ctx.nursery_start; i < ctx.nursery_ptr; i++) {
            if (CELL_TYPE(i) == BT_CLOSURE) {
                code_object *code = (code_object *)CELL_PTR(i);
                mark_code_object(code);
            }
        }
    }

    // Sweep: remove unreachable code objects from registry
    code_object **prev = &code_object_registry;
    while (*prev) {
        code_object *code = *prev;
        if (!code->gc_marked) {
            // Unlink from registry
            *prev = code->gc_next;
            // Free the code object (but not children - they're in registry too)
            free(code->code);
            free(code->constants);
            free(code->children);
            free(code);
        } else {
            prev = &code->gc_next;
        }
    }
}
