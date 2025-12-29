#define _POSIX_C_SOURCE 200809L
#include "bytecode.h"
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

// Global flag for bytecode mode
static bool use_bytecode = false;

// ============================================================================
// File Loading Utilities
// ============================================================================

// Check if expression is the eof-object atom
static bool is_eof_object(unsigned expr)
{
    return CELL_TYPE(expr) == BT_ATOM &&
           strcmp(ctx.atom_table[CELL_ID(expr)], "eof-object") == 0;
}

// Evaluate expression (interpreter or bytecode depending on mode)
static unsigned eval_expr(unsigned expr, unsigned env)
{
    if (use_bytecode) {
        code_object *code = compile_toplevel(expr, env);
        if (!code) {
            show_error("compilation failed");
            return TOK_ERROR;
        }
        vm_state vm;
        vm_init(&vm);
        unsigned result = vm_run(&vm, code, env);
        vm_free(&vm);
        // Note: We don't free the code object here because closures
        // in the environment may still reference child code objects.
        // In a production system, we'd use reference counting or
        // a code object GC. For now, this is a small memory leak.
        // code_free(code);
        return result;
    } else {
        return eval_obj(expr, env);
    }
}

// Load and evaluate expressions from a port
// Returns true on success, false on error
static bool load_from_port(FILE *f, unsigned *env, bool warn_on_error,
                           const char *filename)
{
    const char *old_filename = reader_get_filename();
    reader_set_filename(filename);
    reader_reset_position();

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
        unsigned result = eval_expr(expr, *env);
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
    reader_set_filename(old_filename);
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
        load_from_port(f, env, false, "stdlib.scm");
        fclose(f);
        return;
    }

    // Fall back to embedded stdlib
    f = fmemopen((void *)stdlib_scm, stdlib_scm_len, "r");
    if (!f) {
        fprintf(stderr, "Warning: could not load embedded stdlib\n");
        return;
    }
    load_from_port(f, env, true, "<stdlib>");
    fclose(f);
}

// ============================================================================
// Main Entry Point
// ============================================================================

static void print_usage(const char *prog)
{
    fprintf(stderr, "Usage: %s [options] [file.scm]\n", prog);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  --bytecode   Use bytecode compiler and VM\n");
    fprintf(stderr, "  --help       Show this help message\n");
}

int main(int argc, char **argv)
{
    // Parse command line options
    int file_arg = 0;
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--bytecode") == 0) {
            use_bytecode = true;
        } else if (strcmp(argv[i], "--help") == 0 ||
                   strcmp(argv[i], "-h") == 0) {
            print_usage(argv[0]);
            return 0;
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            print_usage(argv[0]);
            return 1;
        } else {
            file_arg = i;
        }
    }

    // Initialize the heap first
    init_heap();

    // Initialize keywords and special atoms
    init_keywords();

    // Create default environment with primitives
    unsigned env = default_environment();

    // Set up automatic GC during allocation
    set_alloc_gc_root(&env);

    // Load standard library (always use interpreter for stdlib)
    bool saved_mode = use_bytecode;
    use_bytecode = false;
    load_stdlib(&env);
    use_bytecode = saved_mode;

    if (use_bytecode) {
        fprintf(stderr, "; Bytecode mode enabled\n");
    }

    // Execute file if provided
    if (file_arg > 0) {
        FILE *f = fopen(argv[file_arg], "r");
        if (!f) {
            fprintf(stderr, "Cannot open file: %s\n", argv[file_arg]);
            return 1;
        }
        load_from_port(f, &env, false, argv[file_arg]);
        fclose(f);
        return 0;
    }

    // REPL with error recovery
    reader_set_filename("<stdin>");
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
        reader_reset_position(); // Reset line/col for each REPL input
        unsigned expr = read_obj();
        // Re-read env after read_obj() since :g escape can trigger GC
        unsigned x = eval_expr(expr, env);
        printf("\n;Value: ");
        write_obj(x);
        puts("");
        env = gc(env);
    }
}
