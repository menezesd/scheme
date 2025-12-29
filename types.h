/**
 * @file types.h
 * @brief Core type definitions for the Scheme interpreter
 *
 * This file defines the fundamental data structures used throughout the
 * interpreter:
 *
 * ## Memory Model
 * The interpreter uses a tagged union cell structure (cons_cell) where each
 * cell contains a type tag and either:
 * - A car/cdr pair (for pairs, functions, continuations, etc.)
 * - A 64-bit integer ID (for atoms, numbers, pointers to external data)
 *
 * Cells are stored in a heap managed by a semispace copying garbage collector.
 * See context.c for the GC implementation.
 *
 * ## Evaluation Model
 * The evaluator uses trampolined continuation-passing style (CPS) to achieve
 * proper tail call optimization. The tramp_state structure holds the current
 * evaluation state, and the evaluator loops until completion rather than
 * using recursive C calls.
 *
 * ## Numeric Tower
 * Supports the full Scheme numeric tower:
 * - BT_NUM: Exact integers that fit in int64_t
 * - BT_BIGNUM: Arbitrary precision integers
 * - BT_RATIONAL: Exact rationals (numerator/denominator)
 * - BT_INEXACT: IEEE 754 doubles
 * - BT_COMPLEX: Complex numbers with any numeric parts
 */

#ifndef TYPES_H
#define TYPES_H

#define _POSIX_C_SOURCE 200809L
#include <setjmp.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// ============================================================================
// Core Type Definitions
// ============================================================================

/**
 * Cell type tags - stored in cons_cell.type
 *
 * The type tag determines how to interpret the cell's car/cdr or id field.
 * Types fall into several categories:
 * - Immediate: Value stored directly in id field (atoms, numbers, chars)
 * - Pointer: id field contains pointer to external data (strings, vectors)
 * - Compound: car/cdr point to other cells (pairs, functions, continuations)
 */
enum lisp_type {
    BT_FREE,     // Unused cell (on free list)
    BT_READY,    // Allocated but not yet typed
    BT_ATOM,     // Symbol: id = index into atom table
    BT_NUM,      // Exact integer: id = int64_t value
    BT_BIGNUM,   // Arbitrary precision integer: id = bignum* pointer
    BT_INEXACT,  // Floating point: id reinterpreted as double
    BT_RATIONAL, // Exact rational: car = numerator cell, cdr = denominator cell
    BT_COMPLEX,  // Complex number: car = real part cell, cdr = imag part cell
    BT_STRING,   // Mutable string: id = char* pointer
    BT_CHAR,     // Character: id = Unicode code point
    BT_VECTOR,   // Vector: id = vector_data* pointer
    BT_CONS,     // Pair: car = first element, cdr = rest
    BT_FUNCTION, // Lambda closure: car = params, cdr = (body . env)
    BT_MACRO,    // Legacy macro (not hygienic)
    BT_SYNTAX,   // Hygienic macro: syntax-rules transformer object
    BT_CONT,     // First-class continuation (captured by call/cc)
    BT_INPORT,   // Input file port: id = FILE* pointer
    BT_OUTPORT,  // Output file port: id = FILE* pointer
    BT_STRINPORT,       // String input port: id = string_port* pointer
    BT_STROUTPORT,      // String output port: id = string_port* pointer
    BT_MULTIVAL,        // Multiple return values: car = list of values
    BT_FORM,            // Special form (unused)
    BT_BUILTIN,         // Built-in primitive: id = primitive_id enum value
    BT_DYNAMIC,         // Dynamic binding (unused)
    BT_BROKENHEART = -1 // GC forwarding pointer: car = new location
};

// Continuation frame types for full CPS evaluator
enum cont_type {
    CONT_HALT,      // Top level - return final value
    CONT_IF,        // Evaluated condition; data = (then . else), env, next
    CONT_BEGIN,     // Evaluated one expr; data = remaining-exprs, env, next
    CONT_SET,       // Evaluated value; data = var-atom, env, next
    CONT_DEFINE,    // Evaluated value; data = var-atom, env, next
    CONT_EVAL_FN,   // Evaluated function; data = arg-exprs, env, next
    CONT_EVAL_ARGS, // Evaluating args; data = (fn . (evaled-rev . remaining)),
                    // env, next
    CONT_AND,       // Evaluated one; data = remaining, env, next
    CONT_OR,        // Evaluated one; data = remaining, env, next
    CONT_COND_TEST, // Evaluated condition; data = (conseq . rest-clauses), env,
                    // next
    CONT_COND_BODY, // Evaluated body expr; data = remaining-body, env, next
    CONT_LET_VALS,  // Evaluating let values; data = (vars . (vals . (bindings .
                    // body))), env, next
    CONT_LET_BODY,  // Evaluating let body; data = remaining-body, new-env, next
    CONT_LETSTAR_VALS, // Evaluating let* values; data = (bindings . body), env,
                       // next
    CONT_LETREC_INIT,  // Initializing letrec; data = (bindings . (vals-ptr .
                       // body)), env, next
    CONT_APPLY_FUNC,   // Apply user function; data = body, env, next
    CONT_QQ,           // Quasiquote; data = 0 (unused), env, next
    CONT_QQ_LIST,      // QQ list elements; data = (result-head . (result-tail .
                       // remaining)), env, next
    CONT_MACRO_EXPAND, // Macro expansion done; data = 0, env, next
    CONT_CALLWITHVALUES // call-with-values producer done; data = consumer, env,
                        // next
};

enum token {
    TOK_NIL,
    TOK_ERROR,
    TOK_OPEN,
    TOK_CLOSE,
    TOK_QUOTE,
    TOK_DOT,
    TOK_QUASIQUOTE,
    TOK_UNQUOTE,
    TOK_UNQUOTE_SPLICING,
    TOK_VECTOR_OPEN
};

// CPS trampoline state
enum tramp_mode {
    TRAMP_EVAL,  // Evaluate expression
    TRAMP_APPLY, // Apply continuation to value
    TRAMP_DONE,  // Finished
    TRAMP_ERROR  // Error occurred
};

// ============================================================================
// Data Structures
// ============================================================================

/**
 * Fundamental cell structure - all Scheme values are represented as cells.
 *
 * Each cell is 24 bytes (packed) containing:
 * - type: 4-byte enum identifying the cell type
 * - Either car/cdr pair (two 4-byte cell indices) or 8-byte id value
 *
 * The car/cdr fields are cell indices into ctx.cons_cells[], not pointers.
 * The id field shares storage with car/cdr and is used for:
 * - Atoms: index into the atom table
 * - Numbers: the actual int64_t value
 * - Pointers: cast to intptr_t for strings, vectors, bignums, ports
 */
typedef struct __attribute__((packed)) {
    enum lisp_type type;
    union {
        struct {
            unsigned car; // First element / head
            unsigned cdr; // Rest / tail
        };
        int64_t id; // Immediate value or pointer (reinterpret cast)
    };
} cons_cell;

/**
 * Trampoline state for the CPS evaluator.
 *
 * Instead of recursive C function calls, evaluation proceeds by updating
 * this global state and looping. This achieves proper tail call optimization
 * regardless of how deeply tail calls are nested.
 *
 * The evaluator alternates between two modes:
 * - TRAMP_EVAL: Evaluate expr in env, then apply result to cont
 * - TRAMP_APPLY: Apply value to continuation cont
 *
 * The loop terminates when mode becomes TRAMP_DONE or TRAMP_ERROR.
 */
typedef struct {
    enum tramp_mode mode;
    unsigned expr;  // Expression to evaluate (TRAMP_EVAL mode)
    unsigned env;   // Current environment
    unsigned cont;  // Current continuation (stack of pending operations)
    unsigned value; // Value to apply to continuation (TRAMP_APPLY mode)
} tramp_state;

// Vector data structure (stored in id pointer)
typedef struct {
    unsigned len;
    unsigned data[]; // Flexible array member
} vector_data;

// String port structure (for string I/O with fast appending)
typedef struct {
    char *data; // Buffer data
    size_t len; // Current length
    size_t cap; // Allocated capacity
    size_t pos; // Read position (for input ports)
} string_port;

// ============================================================================
// Constants
// ============================================================================

#define TABLE_SIZE 999983             // Size of atom table (prime for hashing)
#define SEMISPACE_SIZE (16384 * 1024) // Size of each GC semispace
#define INITIAL_STRING_CAP 32         // Initial capacity for string buffers

// Reserved cell IDs for permanent atoms (never garbage collected)
#define CELL_ATOM_TRUE 10
#define CELL_ATOM_QUOTE 11
#define CELL_ATOM_QUASIQUOTE 12
#define CELL_ATOM_UNQUOTE 13
#define CELL_ATOM_UNQUOTE_SPLICING 14

// Small integer cache: cells 15-270 hold integers 0-255
#define INT_CACHE_MIN 0
#define INT_CACHE_MAX 255
#define INT_CACHE_START 15    // First cell for cached integers
#define HEAP_RESERVED 271     // Reserved cells (0-270)
#define NUMBER_BUF_SIZE 128   // Buffer size for number->string conversion
#define CHAR_NAME_BUF_SIZE 16 // Buffer size for character name parsing

// Global context structure
typedef struct {
    unsigned hptr;
    unsigned mmin;
    unsigned nmin;
    cons_cell *cons_cells;   // Dynamically allocated heap
    const char **atom_table; // Dynamically allocated atom table
    unsigned atom_count;     // Number of atoms in table (for load factor)
    unsigned atom_quote;
    unsigned atom_true;
    // Cached keyword IDs
    int kw_quote;
    int kw_lambda;
    int kw_begin;
    int kw_and;
    int kw_or;
    int kw_cond;
    int kw_set;
    int kw_define;
    int kw_if;
    int kw_let;
    int kw_letstar;
    int kw_letrec;
    int kw_quasiquote;
    int kw_unquote;
    int kw_unquote_splicing;
    int kw_define_macro;
    int kw_define_syntax;
    int kw_syntax_rules;
    int kw_ellipsis;
    int kw_underscore;
    int kw_else;
    int kw_let_syntax;
    int kw_letrec_syntax;
    unsigned atom_quasiquote;
    unsigned atom_unquote;
    unsigned atom_unquote_splicing;
    // Current ports for dynamic I/O
    FILE *current_input;
    FILE *current_output;
    FILE *transcript; // NULL if not recording
} lisp_context;

// ============================================================================
// Primitive Operation IDs
// ============================================================================

enum primitive_id {
    PPLUS,
    PMINUS,
    PTIMES,
    PDIV,
    PMOD,
    PREMAINDER,
    PCONS,
    PCAR,
    PCDR,
    PEQUAL,
    PNOT,
    PEQ,
    PEQUALP,
    PSETCAR,
    PSETCDR,
    PAPPLY,
    PLIST,
    PREAD,
    PDISPLAY,
    PWRITE,
    PNEWLINE,
    PLOAD,
    PLT,
    PGT,
    PGEQ,
    PLEQ,
    PNUMP,
    PPROCP,
    PSYMP,
    PCONSP,
    PNULLP,
    PSTRINGP,
    PCHARP,
    PVECTORP,
    PLENGTH,
    PAPPEND,
    PREVERSE,
    // String operations
    PSTRLEN,
    PSTRREF,
    PSTRAPP,
    PSUBSTR,
    PSTR2SYM,
    PSYM2STR,
    PNUM2STR,
    PSTR2NUM,
    // Character operations
    PCHARCODE,
    PCODECHAR,
    // Vector operations
    PMAKEVEC,
    PVECREF,
    PVECSET,
    PVECLEN,
    PLIST2VEC,
    PVEC2LIST,
    // I/O
    PREADCHAR,
    PPEEKCHAR,
    PEOF,
    PWRITECHAR,
    // Misc
    PERROR,
    PGENSYM,
    PGCFLIP,
    PCALLCC,
    // R3RS additions
    PQUOTIENT,
    PABS,
    PBOOLP,
    PLISTP,
    // Character comparisons
    PCHAREQ,
    PCHARLT,
    PCHARGT,
    PCHARLE,
    PCHARGE,
    PCHAREQI,
    PCHARLTI,
    PCHARGTI,
    PCHARLEI,
    PCHARGEI,
    // Character predicates
    PCHARALPHA,
    PCHARNUMERIC,
    PCHARWHITE,
    PCHARUPPER,
    PCHARLOWER,
    // Character case
    PCHARUP,
    PCHARDOWN,
    // String operations
    PMAKESTR,
    PSTRSET,
    PSTREQ,
    PSTRLT,
    PSTRGT,
    PSTRLE,
    PSTRGE,
    PSTREQI,
    PSTRLTI,
    PSTRGTI,
    PSTRLEI,
    PSTRGEI,
    PSTR2LIST,
    PLIST2STR,
    PSTRCOPY,
    // Vector
    PVECTOR,
    PVECFILL,
    // Additional R3RS
    PINTEGERP,
    PNUMBERP,
    PREALP,
    PEXACTP,
    PINEXACTP,
    PLASTPAIR,
    PSTRFILL,
    PCHARREADY,
    PSQRT,
    PEXPT,
    PSIN,
    PCOS,
    PTAN,
    PASIN,
    PACOS,
    PATAN,
    PLOG,
    PEXP,
    PFLOOR,
    PCEILING,
    PTRUNCATE,
    PROUND,
    // Port I/O
    POPENINPUT,
    POPENOUTPUT,
    PCLOSEINPUT,
    PCLOSEOUTPUT,
    PINPUTPORTP,
    POUTPUTPORTP,
    PCURRENTINPUT,
    PCURRENTOUTPUT,
    // R3RS numeric tower
    PCOMPLEXP,
    PRATIONALP,
    PNUMERATOR,
    PDENOMINATOR,
    PMAKERECT,
    PMAKEPOLAR,
    PREALPART,
    PIMAGPART,
    PMAGNITUDE,
    PANGLE,
    PEXACT2INEXACT,
    PINEXACT2EXACT,
    PRATIONALIZE,
    // String constructor
    PSTRING,
    // Dynamic ports
    PSETCURRENTINPUT,
    PSETCURRENTOUTPUT,
    // Transcript
    PTRANSCRIPTON,
    PTRANSCRIPTOFF,
    // R5RS multiple values
    PVALUES,
    PCALLWITHVALUES,
    // R5RS eval and environments
    PEVAL,
    PSCHEMEENV,
    PNULLENV,
    PINTERACTIONENV,
    // String ports
    POPENOUTPUTSTRING,
    PGETOUTPUTSTRING,
    POPENINPUTSTRING,
    PSTRINGPORTP,
    PRIM_COUNT // Total number of primitives
};

// ============================================================================
// Global Context and Trampoline State
// ============================================================================

extern lisp_context ctx;
extern tramp_state tramp;
extern unsigned gensym_counter;

// Error recovery jump buffer (for fatal errors like heap exhaustion)
extern jmp_buf panic_jmp;
extern bool panic_jmp_set;

// Panic function - longjmps back to REPL on fatal errors
void lisp_panic(const char *msg);

// ============================================================================
// Error/Warning Macros
// ============================================================================

// Reader position accessors (defined in reader.c)
int reader_get_line(void);
int reader_get_col(void);
const char *reader_get_filename(void);

// Error with location info (file:line:col format)
#define show_error(...)                                                        \
    do {                                                                       \
        fprintf(stderr, "%s:%d:%d: error: ", reader_get_filename(),            \
                reader_get_line(), reader_get_col());                          \
        fprintf(stderr, __VA_ARGS__);                                          \
        fprintf(stderr, "\n");                                                 \
    } while (0)

#define show_warning(...)                                                      \
    do {                                                                       \
        fprintf(stderr, "%s:%d:%d: warning: ", reader_get_filename(),          \
                reader_get_line(), reader_get_col());                          \
        fprintf(stderr, __VA_ARGS__);                                          \
        fprintf(stderr, "\n");                                                 \
    } while (0)

// ============================================================================
// List Iteration Macros
// ============================================================================

// Iterate over a list: FORLIST(node, list) { use car(node); }
#define FORLIST(node, list) for (unsigned node = (list); node; node = cdr(node))

// ============================================================================
// Cell Accessor Macros
// ============================================================================

#define CELL(c) (ctx.cons_cells[c])
#define CELL_TYPE(c) (ctx.cons_cells[c].type)
#define CELL_CAR(c) (ctx.cons_cells[c].car)
#define CELL_CDR(c) (ctx.cons_cells[c].cdr)
#define CELL_ID(c) (ctx.cons_cells[c].id)

// Type checking macros
#define IS_PAIR(c) ((c) != 0 && CELL_TYPE(c) == BT_CONS)
#define IS_ATOM(c) ((c) != 0 && CELL_TYPE(c) == BT_ATOM)
#define IS_NUM(c) ((c) != 0 && CELL_TYPE(c) == BT_NUM)
#define IS_BIGNUM(c) ((c) != 0 && CELL_TYPE(c) == BT_BIGNUM)
#define IS_EXACT_INT(c)                                                        \
    ((c) != 0 && (CELL_TYPE(c) == BT_NUM || CELL_TYPE(c) == BT_BIGNUM))
#define IS_STRING(c) ((c) != 0 && CELL_TYPE(c) == BT_STRING)
#define IS_CHAR(c) ((c) != 0 && CELL_TYPE(c) == BT_CHAR)
#define IS_VECTOR(c) ((c) != 0 && CELL_TYPE(c) == BT_VECTOR)
#define IS_NIL(c) ((c) == 0)
#define IS_INEXACT(c) ((c) != 0 && CELL_TYPE(c) == BT_INEXACT)
#define IS_RATIONAL(c) ((c) != 0 && CELL_TYPE(c) == BT_RATIONAL)
#define IS_COMPLEX(c) ((c) != 0 && CELL_TYPE(c) == BT_COMPLEX)
#define IS_FUNCTION(c) ((c) != 0 && CELL_TYPE(c) == BT_FUNCTION)
#define IS_BUILTIN(c) ((c) != 0 && CELL_TYPE(c) == BT_BUILTIN)
#define IS_MACRO(c) ((c) != 0 && CELL_TYPE(c) == BT_MACRO)
#define IS_SYNTAX(c) ((c) != 0 && CELL_TYPE(c) == BT_SYNTAX)
#define IS_CONT(c) ((c) != 0 && CELL_TYPE(c) == BT_CONT)
#define IS_INPORT(c) ((c) != 0 && CELL_TYPE(c) == BT_INPORT)
#define IS_OUTPORT(c) ((c) != 0 && CELL_TYPE(c) == BT_OUTPORT)
#define IS_STRINPORT(c) ((c) != 0 && CELL_TYPE(c) == BT_STRINPORT)
#define IS_STROUTPORT(c) ((c) != 0 && CELL_TYPE(c) == BT_STROUTPORT)
#define IS_INPUT_PORT(c) (IS_INPORT(c) || IS_STRINPORT(c))
#define IS_OUTPUT_PORT(c) (IS_OUTPORT(c) || IS_STROUTPORT(c))
#define IS_MULTIVAL(c) ((c) != 0 && CELL_TYPE(c) == BT_MULTIVAL)

// Keyword checking macro (for special forms)
#define IS_KEYWORD(c, kw) (IS_ATOM(c) && CELL_ID(c) == (kw))

// ============================================================================
// Pointer Accessor Macros (for type-safe access to pointer-based cells)
// ============================================================================

#define GET_STRING_PTR(c) ((char *)(intptr_t)CELL_ID(c))
#define GET_VECTOR_PTR(c) ((vector_data *)(intptr_t)CELL_ID(c))
#define GET_PORT_PTR(c) ((FILE *)(intptr_t)CELL_ID(c))
#define GET_STRPORT_PTR(c) ((string_port *)(intptr_t)CELL_ID(c))
#define GET_CHAR_CODE(c) ((int)CELL_ID(c))
#define STORE_PTR(ptr) ((int64_t)(intptr_t)(ptr))

// ============================================================================
// Argument Checking Macros
// ============================================================================

#define REQUIRE_ARGS(args, min, max, name)                                     \
    do {                                                                       \
        if (!check_args(args, min, max, name))                                 \
            return TOK_ERROR;                                                  \
    } while (0)

#define REQUIRE_TYPE(val, expected_type, name)                                 \
    do {                                                                       \
        if (CELL_TYPE(val) != expected_type) {                                 \
            show_error("%s: wrong type", name);                                \
            return TOK_ERROR;                                                  \
        }                                                                      \
    } while (0)

// ============================================================================
// Error Handling Macros
// ============================================================================

#define ERROR_RETURN(msg)                                                      \
    do {                                                                       \
        show_error(msg);                                                       \
        return TOK_ERROR;                                                      \
    } while (0)

#define ERROR_RETURNF(fmt, ...)                                                \
    do {                                                                       \
        show_error(fmt, __VA_ARGS__);                                          \
        return TOK_ERROR;                                                      \
    } while (0)

#define CHECK_CONDITION(cond, msg)                                             \
    do {                                                                       \
        if (!(cond))                                                           \
            ERROR_RETURN(msg);                                                 \
    } while (0)

#define CHECK_ZERO_DIVISOR(divisor)                                            \
    do {                                                                       \
        if ((divisor) == 0)                                                    \
            ERROR_RETURN("division by zero");                                  \
    } while (0)

#define CHECK_NUMERIC(val, name)                                               \
    do {                                                                       \
        if (!is_numeric(val))                                                  \
            ERROR_RETURNF("%s: not a number", name);                           \
    } while (0)

#define CHECK_STRING(val, name)                                                \
    do {                                                                       \
        if (!IS_STRING(val))                                                   \
            ERROR_RETURNF("%s: not a string", name);                           \
    } while (0)

#define CHECK_SYMBOL(val, name)                                                \
    do {                                                                       \
        if (!IS_ATOM(val))                                                     \
            ERROR_RETURNF("%s: not a symbol", name);                           \
    } while (0)

#define CHECK_DIV_ZERO(val, name)                                              \
    do {                                                                       \
        if ((val) == 0)                                                        \
            ERROR_RETURNF("%s: division by zero", name);                       \
    } while (0)

// Check if either of two values is a bignum (requiring bignum arithmetic)
#define EITHER_BIGNUM(a, b)                                                    \
    (CELL_TYPE(a) == BT_BIGNUM || CELL_TYPE(b) == BT_BIGNUM)

#define CHECK_DIV_ZERO_DBL(val, name)                                          \
    do {                                                                       \
        if ((val) == 0.0)                                                      \
            ERROR_RETURNF("%s: division by zero", name);                       \
    } while (0)

#define CHECK_INDEX_BOUNDS(idx, limit, name)                                   \
    do {                                                                       \
        if ((idx) < 0 || (idx) >= (int64_t)(limit))                            \
            ERROR_RETURNF("%s: index out of bounds", name);                    \
    } while (0)

#define CHECK_VECTOR_BOUNDS(idx, vec, name)                                    \
    CHECK_INDEX_BOUNDS(idx, vector_len(vec), name)

#define CHECK_CHAR(val, name)                                                  \
    do {                                                                       \
        if (!IS_CHAR(val))                                                     \
            ERROR_RETURNF("%s: not a character", name);                        \
    } while (0)

#define CHECK_VECTOR(val, name)                                                \
    do {                                                                       \
        if (!IS_VECTOR(val))                                                   \
            ERROR_RETURNF("%s: not a vector", name);                           \
    } while (0)

#define CHECK_PAIR(val, name)                                                  \
    do {                                                                       \
        if (!IS_PAIR(val))                                                     \
            ERROR_RETURNF("%s: not a pair", name);                             \
    } while (0)

#define CHECK_INPUT_PORT(val, name)                                            \
    do {                                                                       \
        if (!IS_INPUT_PORT(val))                                               \
            ERROR_RETURNF("%s: not an input port", name);                      \
    } while (0)

#define CHECK_OUTPUT_PORT(val, name)                                           \
    do {                                                                       \
        if (!IS_OUTPUT_PORT(val))                                              \
            ERROR_RETURNF("%s: not an output port", name);                     \
    } while (0)

#endif // TYPES_H
