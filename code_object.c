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
#include <stdint.h>
#include <stdlib.h>

// ============================================================================
// Code Object Registry
// ============================================================================

// Global registry of all code objects for GC integration
code_object *code_object_registry = NULL;

// Register a code object with the GC registry
void code_register(code_object *code)
{
    if (!code || code_object_is_registered(code))
        return;
    code->gc_next = code_object_registry;
    code_object_registry = code;
}

static void code_unregister(code_object *code)
{
    code_object **prev = &code_object_registry;
    while (*prev) {
        if (*prev == code) {
            *prev = code->gc_next;
            code->gc_next = NULL;
            return;
        }
        prev = &(*prev)->gc_next;
    }
}

static bool code_arrays_well_formed(const code_object *code)
{
    return code && code->code_len <= code->code_cap &&
           code->const_len <= code->const_cap &&
           code->children_len <= code->children_cap &&
           (code->code_len == 0 || code->code != NULL) &&
           (code->const_len == 0 || code->constants != NULL) &&
           (code->children_len == 0 || code->children != NULL);
}

// A child can be shared by more than one code object (the public code-object
// API permits DAGs, not just trees).  Only release it once no registered
// parent still owns a reference to it.
static bool code_has_registered_parent(const code_object *child)
{
    for (code_object *parent = code_object_registry; parent;
         parent = parent->gc_next) {
        if (!code_arrays_well_formed(parent))
            continue;
        for (unsigned i = 0; i < parent->children_len; i++) {
            if (parent->children[i] == child)
                return true;
        }
    }
    return false;
}

// ============================================================================
// Code Object Creation and Destruction
// ============================================================================

static void code_destroy_shallow(code_object *code)
{
    if (!code)
        return;
    free(code->code);
    free(code->constants);
    free(code->children);
    free(code);
}

code_object *code_new(void)
{
    code_object *c = checked_calloc_array(1, sizeof(code_object));
    if (!c)
        return NULL;

    c->code_cap = 64;
    c->code = checked_malloc_array(c->code_cap, sizeof(unsigned));
    if (!c->code) {
        code_destroy_shallow(c);
        return NULL;
    }

    c->const_cap = 16;
    c->constants = checked_malloc_array(c->const_cap, sizeof(unsigned));
    if (!c->constants) {
        code_destroy_shallow(c);
        return NULL;
    }

    c->children_cap = 4;
    c->children = checked_malloc_array(c->children_cap, sizeof(code_object *));
    if (!c->children) {
        code_destroy_shallow(c);
        return NULL;
    }

    // Register with GC
    code_register(c);
    return c;
}

void code_free(code_object *code)
{
    if (!code || !code_object_is_registered(code))
        return;
    code_unregister(code);
    if (code->children_len <= code->children_cap && code->children) {
        for (unsigned i = 0; i < code->children_len; i++) {
            code_object *child = code->children[i];
            if (!code_has_registered_parent(child))
                code_free(child);
        }
    }
    code_destroy_shallow(code);
}

// ============================================================================
// Code Object Mutation
// ============================================================================

void code_emit(code_object *code, unsigned instr)
{
    if (code->code_len >= code->code_cap) {
        unsigned new_cap =
            checked_grow_capacity(code->code_cap, sizeof(unsigned),
                                  "code_emit: capacity overflow");
        unsigned *new_code =
            checked_realloc_array(code->code, new_cap, sizeof(unsigned));
        if (!new_code) {
            lisp_panic("code_emit: realloc failed");
        }
        code->code = new_code;
        code->code_cap = new_cap;
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
        unsigned new_cap =
            checked_grow_capacity(code->const_cap, sizeof(unsigned),
                                  "code_add_const: capacity overflow");
        unsigned *new_consts =
            checked_realloc_array(code->constants, new_cap, sizeof(unsigned));
        if (!new_consts) {
            lisp_panic("code_add_const: realloc failed");
        }
        code->constants = new_consts;
        code->const_cap = new_cap;
    }
    code->constants[code->const_len] = val;
    return code->const_len++;
}

unsigned code_add_child(code_object *code, code_object *child)
{
    if (code->children_len >= code->children_cap) {
        unsigned new_cap =
            checked_grow_capacity(code->children_cap, sizeof(code_object *),
                                  "code_add_child: capacity overflow");
        code_object **new_children = checked_realloc_array(
            code->children, new_cap, sizeof(code_object *));
        if (!new_children) {
            lisp_panic("code_add_child: realloc failed");
        }
        code->children = new_children;
        code->children_cap = new_cap;
    }
    code->children[code->children_len] = child;
    return code->children_len++;
}

unsigned code_current_pos(code_object *code)
{
    return code->code_len;
}

void code_patch(code_object *code, unsigned pos, unsigned val)
{
    if (!code || pos >= code->code_len)
        lisp_panic("code_patch: position out of bounds");
    code->code[pos] = val;
}

// ============================================================================
// GC Integration
// ============================================================================

static void gc_collect_code_inner(code_object *code)
{
    if (!code || !code_object_is_registered(code) ||
        !code_arrays_well_formed(code) || code->gc_updating)
        return;

    code->gc_updating = true;

    // Collect constants
    for (unsigned i = 0; i < code->const_len; i++) {
        code->constants[i] = collect(code->constants[i]);
    }

    // Recursively collect children
    for (unsigned i = 0; i < code->children_len; i++) {
        gc_collect_code_inner(code->children[i]);
    }
}

unsigned gc_collect_code(code_object *code)
{
    gc_collect_code_inner(code);

    // Clear the temporary traversal marks, including in the unlikely case
    // that an invalid child graph prevented a normal recursive unwind.
    for (code_object *it = code_object_registry; it; it = it->gc_next)
        it->gc_updating = false;
    return 0;
}

// Update all code object constants during GC
// Called from gc() in context.c BEFORE the scan phase
void gc_update_all_code_objects(void)
{
    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        if (!code_arrays_well_formed(code))
            continue;
        // Collect constants - the scan phase will then process their CAR/CDR
        for (unsigned i = 0; i < code->const_len; i++) {
            code->constants[i] = collect(code->constants[i]);
        }
    }
}

void minor_gc_update_all_code_objects(void)
{
    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        if (!code_arrays_well_formed(code))
            continue;
        for (unsigned i = 0; i < code->const_len; i++) {
            code->constants[i] = collect_to_old(code->constants[i]);
        }
    }
}

// Mark a code object and all its children as reachable
static void mark_code_object(code_object *code)
{
    if (!code || !code_object_is_registered(code) ||
        !code_arrays_well_formed(code) || code->gc_marked)
        return;
    code->gc_marked = true;
    // Mark children recursively
    for (unsigned i = 0; i < code->children_len; i++) {
        mark_code_object(code->children[i]);
    }
}

bool code_object_is_registered(const code_object *needle)
{
    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        if (code == needle)
            return true;
    }
    return false;
}

static void mark_registered_code_object(code_object *code)
{
    if (code_object_is_registered(code))
        mark_code_object(code);
}

static void mark_vm_continuation_code(vm_continuation *cont)
{
    if (!vm_continuation_is_registered(cont))
        return;
    mark_registered_code_object(cont->code);
    if (!cont->frames || cont->fp > VM_MAX_FRAMES_SIZE)
        return;
    for (unsigned i = 0; i < cont->fp; i++) {
        mark_registered_code_object(cont->frames[i].code);
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
            mark_registered_code_object(code);
        } else if (is_vm_continuation_object(i)) {
            mark_vm_continuation_code((vm_continuation *)CELL_PTR(i));
        }
    }
    // Scan nursery if generational GC is enabled: [nursery_start, nursery_ptr)
    if (ctx.card_table) {
        for (unsigned i = ctx.nursery_start; i < ctx.nursery_ptr; i++) {
            if (CELL_TYPE(i) == BT_CLOSURE) {
                code_object *code = (code_object *)CELL_PTR(i);
                mark_registered_code_object(code);
            } else if (is_vm_continuation_object(i)) {
                mark_vm_continuation_code((vm_continuation *)CELL_PTR(i));
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
            code_destroy_shallow(code);
        } else {
            prev = &code->gc_next;
        }
    }
}
