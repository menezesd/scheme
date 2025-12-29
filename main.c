#define _POSIX_C_SOURCE 200809L
#include "context.h"
#include "env.h"
#include "eval.h"
#include "reader.h"
#include "types.h"
#include "writer.h"
#include <ctype.h>
#include <string.h>

// Embedded standard library
#include "stdlib_data.h"

// ============================================================================
// File Loading Utilities
// ============================================================================

// Check if expression is the eof-object atom
static bool is_eof_object(unsigned expr)
{
    return CELL_TYPE(expr) == BT_ATOM &&
           strcmp(ctx.atom_table[CELL_ID(expr)], "eof-object") == 0;
}

// Load and evaluate expressions from a port
// Returns true on success, false on error
static bool load_from_port(FILE *f, unsigned *env, bool warn_on_error)
{
    for (;;) {
        // Skip whitespace
        int c;
        while ((c = fgetc(f)) != EOF && isspace(c))
            ;
        if (c == EOF)
            break;
        ungetc(c, f);

        // Read expression
        unsigned expr = read_obj_port(f);
        if (expr == TOK_ERROR)
            break;
        if (is_eof_object(expr))
            break;

        // Evaluate
        unsigned result = eval_obj(expr, *env);
        if (result == TOK_ERROR) {
            if (warn_on_error) {
                fprintf(stderr, "Warning: error during load\n");
            }
            return false;
        }

        // GC when heap is 75% full
        *env = maybe_gc(*env, 75);
    }
    // Final GC
    *env = gc(*env);
    return true;
}

// ============================================================================
// Standard Library Loading
// ============================================================================

static void load_stdlib(unsigned *env)
{
    // Try external file first (for development)
    FILE *f = fopen("./stdlib.scm", "r");
    if (f) {
        load_from_port(f, env, false);
        fclose(f);
        return;
    }

    // Fall back to embedded stdlib
    f = fmemopen((void *)stdlib_scm, stdlib_scm_len, "r");
    if (!f) {
        fprintf(stderr, "Warning: could not load embedded stdlib\n");
        return;
    }
    load_from_port(f, env, true);
    fclose(f);
}

// ============================================================================
// Main Entry Point
// ============================================================================

int main(int argc, char **argv)
{
    // Initialize the heap first
    init_heap();

    // Initialize keywords and special atoms
    init_keywords();

    // Create default environment with primitives
    unsigned env = default_environment();

    // Set up automatic GC during allocation
    set_alloc_gc_root(&env);

    // Load standard library
    load_stdlib(&env);

    // Execute file if provided
    if (argc > 1) {
        FILE *f = fopen(argv[1], "r");
        if (!f) {
            fprintf(stderr, "Cannot open file: %s\n", argv[1]);
            return 1;
        }
        load_from_port(f, &env, false);
        fclose(f);
        return 0;
    }

    // REPL with error recovery
    panic_jmp_set = true;
    if (setjmp(panic_jmp) != 0) {
        // Recovered from a fatal error - reset and continue
        fprintf(stderr, "Returning to REPL...\n");
        env = gc(env);
    }

    for (;;) {
        printf("]=> ");
        fflush(stdout);

        reader_reset_labels();
        unsigned expr = read_obj();
        // Re-read env after read_obj() since :g escape can trigger GC
        unsigned x = eval_obj(expr, env);
        printf("\n;Value: ");
        write_obj(x);
        puts("");
        env = gc(env);
    }
}
