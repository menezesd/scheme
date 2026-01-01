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

// Global flag for bytecode mode (default: true for performance)
static bool use_bytecode = true;

// ============================================================================
// File Loading Utilities
// ============================================================================

// Check if expression is the eof-object atom
static bool is_eof_object(unsigned expr)
{
    return CELL_TYPE(expr) == BT_ATOM &&
           strcmp(ctx.atom_table[CELL_ID(expr)], "eof-object") == 0;
}

// Track evaluation depth to only sweep code objects at top level
static int eval_depth = 0;

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
        eval_depth++;
        unsigned result = vm_run(&vm, code, env);
        eval_depth--;
        vm_free(&vm);
        // Only sweep unreachable code objects at top level
        // (nested eval via callback must not sweep parent's code)
        if (eval_depth == 0) {
            gc_sweep_code_objects();
        }
        return result;
    } else {
        return eval_obj(expr, env);
    }
}

// Load and evaluate expressions from a port
// Returns true on success, false on error
// Check if expression is a define-syntax form
static bool is_define_syntax(unsigned expr)
{
    return IS_PAIR(expr) && IS_ATOM(car(expr)) &&
           CELL_ID(car(expr)) == ctx.kw_define_syntax;
}

// Evaluate a batch of expressions as a begin form
static unsigned eval_batch(unsigned exprs, unsigned env)
{
    if (!exprs)
        return 0;
    if (!cdr(exprs))
        return eval_expr(car(exprs), env);

    gc_protect(&exprs);
    int begin_id = intern("begin");
    unsigned begin_atom = alloc();
    CELL_TYPE(begin_atom) = BT_ATOM;
    CELL_ID(begin_atom) = begin_id;
    gc_protect(&begin_atom);
    unsigned begin_form = alloc_cons(begin_atom, exprs);
    gc_unprotect(2);

    return eval_expr(begin_form, env);
}

static bool load_from_port(FILE *f, unsigned *env, bool warn_on_error,
                           const char *filename)
{
    const char *old_filename = reader_get_filename();
    reader_set_filename(filename);
    reader_reset_position();

    // Read all expressions first
    unsigned all_exprs = 0, all_tail = 0;
    gc_protect(&all_exprs);
    gc_protect(&all_tail);

    for (;;) {
        int c;
        while ((c = fgetc(f)) != EOF && isspace(c))
            ;
        if (c == EOF)
            break;
        ungetc(c, f);

        unsigned expr = read_obj_port(f);
        if (expr == TOK_ERROR) {
            gc_unprotect(2);
            reader_set_filename(old_filename);
            return false;
        }
        if (is_eof_object(expr))
            break;

        gc_protect(&expr);
        list_append(&all_exprs, &all_tail, expr);
        gc_unprotect(1);
        *env = maybe_gc(*env, 75);
    }

    gc_unprotect(2);

    if (!all_exprs) {
        *env = gc(*env);
        reader_set_filename(old_filename);
        return true;
    }

    // Evaluate expressions, batching consecutive non-macro expressions
    // for call/cc support, but evaluating define-syntax immediately
    // so macros are available for subsequent expressions.
    unsigned batch = 0, batch_tail = 0;
    gc_protect(&batch);
    gc_protect(&batch_tail);
    gc_protect(&all_exprs);

    // Protect loop variable - it can become stale if GC runs during eval
    unsigned exprs = all_exprs;
    gc_protect(&exprs);

    for (; exprs; exprs = cdr(exprs)) {
        unsigned expr = car(exprs);
        // Protect expr - it can become stale if GC runs during eval_batch
        gc_protect(&expr);

        if (is_define_syntax(expr)) {
            // Evaluate any pending batch first
            if (batch) {
                unsigned result = eval_batch(batch, *env);
                if (result == TOK_ERROR) {
                    gc_unprotect(5);  // expr, exprs, all_exprs, batch_tail, batch
                    reader_set_filename(old_filename);
                    if (warn_on_error)
                        fprintf(stderr, "Warning: error during load\n");
                    return false;
                }
                batch = batch_tail = 0;
            }
            // Evaluate define-syntax immediately
            unsigned result = eval_expr(expr, *env);
            gc_unprotect(1);  // expr
            if (result == TOK_ERROR) {
                gc_unprotect(4);  // exprs, all_exprs, batch_tail, batch
                reader_set_filename(old_filename);
                if (warn_on_error)
                    fprintf(stderr, "Warning: error during load\n");
                return false;
            }
        } else {
            gc_unprotect(1);  // expr (will be re-protected by list_append)
            // Add to batch
            gc_protect(&expr);
            list_append(&batch, &batch_tail, expr);
            gc_unprotect(1);
        }
    }

    // Evaluate final batch
    if (batch) {
        unsigned result = eval_batch(batch, *env);
        if (result == TOK_ERROR) {
            gc_unprotect(4);  // exprs, all_exprs, batch_tail, batch
            reader_set_filename(old_filename);
            if (warn_on_error)
                fprintf(stderr, "Warning: error during load\n");
            return false;
        }
    }

    gc_unprotect(4);  // exprs, all_exprs, batch_tail, batch
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
// VM Callbacks for Special Primitives
// ============================================================================

// Callback for (load "filename") in bytecode mode
static unsigned load_callback(const char *filename, unsigned *env_ptr)
{
    FILE *f = fopen(filename, "r");
    if (!f) {
        show_error("load: cannot open file: %s", filename);
        return TOK_ERROR;
    }
    if (!load_from_port(f, env_ptr, true, filename)) {
        fclose(f);
        return TOK_ERROR;
    }
    fclose(f);
    return 0; // Return nil on success (like the CPS evaluator)
}

// Callback for (eval expr env) in bytecode mode
static unsigned eval_callback(unsigned expr, unsigned env)
{
    return eval_expr(expr, env);
}

// ============================================================================
// Main Entry Point
// ============================================================================

static void print_usage(const char *prog)
{
    fprintf(stderr, "Usage: %s [options] [file.scm]\n", prog);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  --interpreter  Use CPS interpreter instead of bytecode VM\n");
    fprintf(stderr, "  --help         Show this help message\n");
}

int main(int argc, char **argv)
{
    // Parse command line options
    int file_arg = 0;
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--interpreter") == 0) {
            use_bytecode = false;
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

    // Set up VM callbacks for special primitives
    ctx.load_callback = load_callback;
    ctx.eval_callback = eval_callback;

    // Initialize keywords and special atoms
    init_keywords();

    // Create default environment with primitives
    unsigned env = default_environment();

    // Set up automatic GC during allocation
    set_alloc_gc_root(&env);

    // Load standard library
    load_stdlib(&env);

    if (!use_bytecode) {
        fprintf(stderr, "; CPS interpreter mode (use default for faster bytecode VM)\n");
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
        unsigned x = eval_expr(expr, env);
        printf("\n;Value: ");
        write_obj(x);
        puts("");
        env = gc(env);
    }
}
