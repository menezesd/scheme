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
    BT_INPORT,   // Input file port: id = file_port* pointer
    BT_OUTPORT,  // Output file port: id = file_port* pointer
    BT_STRINPORT,       // String input port: id = string_port* pointer
    BT_STROUTPORT,      // String output port: id = string_port* pointer
    BT_MULTIVAL,        // Multiple return values: car = list of values
    BT_BUILTIN,         // Built-in primitive: id = primitive_id enum value
    BT_CLOSURE = 100,   // VM closure (bytecode)
    BT_VMCONT = 101,    // VM continuation (distinct from CPS BT_CONT)
    BT_COMPILED_PATTERN = 102, // Compiled pattern: ptr = compiled_pattern*
    BT_BYTEVEC = 103,          // Bytevector: ptr = bytevec_data*
    BT_BINDING_REF = 104,      // Alias to an environment value cell
    BT_HASHTABLE = 105,        // Hash table: ptr = hash_table_data*
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
    CONT_COND_ARROW, // Evaluated receiver expr; data = test-value, env, next
    CONT_LET_VALS, // Evaluating let values; data = (vars . (vals . (bindings .
                   // body))), env, next
    CONT_LET_BODY, // Evaluating let body; data = remaining-body, new-env, next
    CONT_LETSTAR_VALS, // Evaluating let* values; data = (bindings . body), env,
                       // next
    CONT_LETREC_INIT,  // Initializing letrec; data = (bindings . (vals-ptr .
                       // body)), env, next
    CONT_APPLY_FUNC,   // Apply user function; data = body, env, next
    CONT_MACRO_EXPAND, // Macro expansion done; data = 0, env, next
    CONT_CALLWITHVALUES, // call-with-values producer done; data = consumer,
                         // env, next
    CONT_COUNT           // Number of continuation types (must be last)
};

enum token {
    TOK_RESERVED_ZERO, // Keep TOK_ERROR != 0 (0 is empty list)
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

#define TOK_EOF ((unsigned)-1)

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
        int64_t id;  // Immediate value (atoms, numbers, chars)
        void *ptr;   // External data pointer (strings, vectors, bignums, ports)
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

// Bytevector data structure (stored in ptr field)
typedef struct {
    unsigned len;
    uint8_t data[]; // Flexible array member
} bytevec_data;

typedef struct {
    FILE *file;
    bool binary;
    bool input;
    bool owns_file;
} file_port;

typedef enum {
    HASH_EQ,
    HASH_EQV,
    HASH_EQUAL,
} hash_equiv;

// Hash table bucket entry. Keys and values are Scheme cell IDs.
typedef struct hash_entry {
    unsigned key;
    unsigned value;
    struct hash_entry *next;
} hash_entry;

typedef struct {
    unsigned size;
    unsigned capacity;
    hash_equiv equiv;
    hash_entry **buckets;
} hash_table_data;

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
#define SEMISPACE_SIZE (16384 * 1024) // Size of each GC semispace (16M cells)
#define INITIAL_STRING_CAP 32         // Initial capacity for string buffers

// Generational GC settings
#define NURSERY_SIZE (256 * 1024) // Size of nursery (young generation)
#define CARD_SIZE 512             // Cells per card (2KB on 64-bit)

// Card table bitfield macros (8x memory savings over byte-per-card)
// card = cell_index / CARD_SIZE
#define CARD_BYTE(card) ((card) >> 3)
#define CARD_BIT(card) (1u << ((card) & 7))
#define CARD_MARK(tbl, card) ((tbl)[CARD_BYTE(card)] |= CARD_BIT(card))
#define CARD_CLEAR(tbl, card) ((tbl)[CARD_BYTE(card)] &= ~CARD_BIT(card))
#define CARD_IS_DIRTY(tbl, card) ((tbl)[CARD_BYTE(card)] & CARD_BIT(card))

// Reserved cell IDs for permanent atoms (never garbage collected)
// Note: These must be >= 10 to avoid conflicts with the token enum (0-9)
#define CELL_ATOM_FALSE 10
#define CELL_ATOM_TRUE 11
#define CELL_ATOM_QUOTE 12
#define CELL_ATOM_QUASIQUOTE 13
#define CELL_ATOM_UNQUOTE 14
#define CELL_ATOM_UNQUOTE_SPLICING 15

// Small integer cache: cells 16-271 hold integers 0-255
#define INT_CACHE_MIN 0
#define INT_CACHE_MAX 255
#define INT_CACHE_START 16    // First cell for cached integers
#define HEAP_RESERVED 272     // Reserved cells (0-271)
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
    unsigned atom_table_cap; // Current capacity of atom table
    unsigned atom_quote;
    unsigned atom_true;
    unsigned atom_false;
    // Generational GC state
    unsigned nursery_start; // Start of nursery region
    unsigned nursery_ptr;   // Next free cell in nursery (bump pointer)
    uint8_t *
        card_table; // Card table for write barrier (1 byte per CARD_SIZE cells)
    unsigned minor_gc_count; // Statistics: number of minor GCs
    unsigned major_gc_count; // Statistics: number of major GCs
    // Cached keyword IDs
    int kw_quote;
    int kw_lambda;
    int kw_begin;
    int kw_and;
    int kw_or;
    int kw_cond;
    int kw_cond_expand;
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
    int kw_and_feature;
    int kw_or_feature;
    int kw_not_feature;
    int kw_arrow; // => for cond receiver syntax
    int kw_let_syntax;
    int kw_letrec_syntax;
    int kw_protected; // Marker for protected identifiers in hygiene
    unsigned atom_quasiquote;
    unsigned atom_unquote;
    unsigned atom_unquote_splicing;
    // Current ports for dynamic I/O
    FILE *current_input;
    FILE *current_output;
    FILE *current_error;
    unsigned current_input_cell;  // Current input port cell (0 = use FILE*)
    unsigned current_output_cell; // Current output port cell (0 = use FILE*)
    unsigned current_error_cell;  // Current error port cell (0 = use FILE*)
    FILE *transcript;             // NULL if not recording
    // Callbacks for VM special primitives (set by main.c)
    unsigned (*load_callback)(const char *filename,
                              unsigned *env); // Returns result or TOK_ERROR
    unsigned (*eval_callback)(unsigned expr,
                              unsigned env); // Returns result or TOK_ERROR
    // Last error message for better error reporting
    char last_error[256];
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
    PGCSTATS,
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
    PREADLINE,
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
    PFINITE,
    PINFINITE,
    PNAN,
    // String constructor
    PSTRING,
    // Dynamic ports
    PSETCURRENTINPUT,
    PSETCURRENTOUTPUT,
    PFLUSHOUTPUT,
    PCURRENTSECOND,
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
    // Random numbers (SRFI-27 style)
    PRANDOMINTEGER,
    PRANDOMREAL,
    PRANDOMSEED,
    // Process control
    PEXIT,
    // Bitwise operations
    PBITWISEAND,
    PBITWISEIOR,
    PBITWISENOT,
    PBITWISEXOR,
    PARITHSHIFT,
    // Bytevector operations
    PMAKEBYTEVEC,
    PBYTEVECREF,
    PBYTEVECSET,
    PBYTEVECLEN,
    PBYTEVECCOPY,
    PBYTEVECCOPYTO,
    PBYTEVECAPPEND,
    PBYTEVEC,
    PBYTEVECUP,
    // Command line
    PCOMMANDLINE,
    // write-to-string
    PWRITETOSTRING,
    // list-ref
    PLISTREF,
    // Binary I/O
    POPENBINARYINPUT,
    PREADBYTEVEC,
    // File system
    PFILEEXISTS,
    PDELETEFILE,
    PRENAMEFILE,
    PCURRENTDIRECTORY,
    PDIRECTORYFILES,
    PGETENV,
    POPENBINARYOUTPUT,
    PWRITEBYTEVEC,
    PREADBYTEVECINTO,
    PREADSTRING,
    PWRITESTRING,
    PU8READY,
    PCURRENTERROR,
    PSETCURRENTERROR,
    PPORTOPENP,
    PINPUTPORTOPENP,
    POUTPUTPORTOPENP,
    PTEXTUALPORTP,
    PBINARYPORTP,
    PFEATURES,
    PMAKEHASHTABLE,
    PMAKESTRONGEQHASHTABLE,
    PMAKEEQHASHTABLE,
    PMAKESTRONGEQVHASHTABLE,
    PMAKEEQVHASHTABLE,
    PMAKEEQUALHASHTABLE,
    PHASHTABLEP,
    PHASHTABLEREF,
    PHASHTABLESET,
    PHASHTABLEDELETE,
    PHASHTABLEEXISTS,
    PHASHTABLESIZE,
    PHASHTABLECLEAR,
    PHASHTABLEKEYS,
    PHASHTABLEVALUES,
    PHASHTABLEALIST,
    PCURRENTJIFFY,
    PJIFFIESPERSECOND,
    PGETENVS,
    PEMERGENCYEXIT,
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
void lisp_panic(const char *msg) __attribute__((noreturn));

// ============================================================================
// Error/Warning Macros
// ============================================================================

// Reader position accessors (defined in reader.c)
int reader_get_line(void);
int reader_get_col(void);
const char *reader_get_filename(void);

// Error with location info (file:line:col format)
// Also stores message in ctx.last_error for programmatic access
#define show_error(...)                                                        \
    do {                                                                       \
        fprintf(stderr, "%s:%d:%d: error: ", reader_get_filename(),            \
                reader_get_line(), reader_get_col());                          \
        fprintf(stderr, __VA_ARGS__);                                          \
        fprintf(stderr, "\n");                                                 \
        snprintf(ctx.last_error, sizeof(ctx.last_error), __VA_ARGS__);         \
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
// Fixnum Tagging (for unboxed integers on the VM stack)
// ============================================================================
//
// Bit 31 of an unsigned value is used as a fixnum tag. Since the maximum
// cell index is 2*SEMISPACE_SIZE (~33M, 25 bits), bit 31 is always zero
// for valid cell indices. This lets us store small integers directly on
// the VM stack without allocating heap cells.
//
// Encoding: MAKE_FIXNUM(v) = (v & 0x7FFFFFFF) | 0x80000000
// Range: -2^30 to 2^30-1 (±1,073,741,824)

#define FIXNUM_TAG     0x80000000u
#define FIXNUM_MIN     (-1073741824)     // -(1 << 30)
#define FIXNUM_MAX     1073741823        // (1 << 30) - 1
#define IS_FIXNUM(v)   ((v) & FIXNUM_TAG)
#define MAKE_FIXNUM(v) ((unsigned)((int32_t)(v) & 0x7FFFFFFF) | FIXNUM_TAG)
#define FITS_FIXNUM(v) ((v) >= FIXNUM_MIN && (v) <= FIXNUM_MAX)

// Extract signed integer from fixnum (portable sign extension from bit 30)
static inline int32_t FIXNUM_VALUE(unsigned v)
{
    int32_t raw = (int32_t)(v & 0x7FFFFFFF);
    return (raw ^ 0x40000000) - 0x40000000;
}

// ============================================================================
// Cell Accessor Macros
// ============================================================================

#define CELL(c) (ctx.cons_cells[c])
#define CELL_TYPE(c) (ctx.cons_cells[c].type)
#define CELL_CAR(c) (ctx.cons_cells[c].car)
#define CELL_CDR(c) (ctx.cons_cells[c].cdr)
#define CELL_ID(c) (ctx.cons_cells[c].id)

// Type checking macros
#define IS_CELL(c) ((c) != 0 && !IS_FIXNUM(c))
#define IS_PAIR(c) (IS_CELL(c) && CELL_TYPE(c) == BT_CONS)
#define IS_ATOM(c) (IS_CELL(c) && CELL_TYPE(c) == BT_ATOM)
#define IS_NUM(c) (IS_CELL(c) && CELL_TYPE(c) == BT_NUM)
#define IS_BIGNUM(c) (IS_CELL(c) && CELL_TYPE(c) == BT_BIGNUM)
#define IS_EXACT_INT(c)                                                        \
    (IS_FIXNUM(c) ||                                                           \
     (IS_CELL(c) && (CELL_TYPE(c) == BT_NUM || CELL_TYPE(c) == BT_BIGNUM)))
#define IS_STRING(c) (IS_CELL(c) && CELL_TYPE(c) == BT_STRING)
#define IS_CHAR(c) (IS_CELL(c) && CELL_TYPE(c) == BT_CHAR)
#define IS_VECTOR(c) (IS_CELL(c) && CELL_TYPE(c) == BT_VECTOR)
#define IS_BYTEVEC(c) (IS_CELL(c) && CELL_TYPE(c) == BT_BYTEVEC)
#define IS_BINDING_REF(c) (IS_CELL(c) && CELL_TYPE(c) == BT_BINDING_REF)
#define IS_HASHTABLE(c) (IS_CELL(c) && CELL_TYPE(c) == BT_HASHTABLE)
#define IS_NIL(c) ((c) == 0)
#define IS_FALSE(c) ((c) == CELL_ATOM_FALSE)
#define IS_TRUTHY(c) ((c) != CELL_ATOM_FALSE)
#define IS_INEXACT(c) (IS_CELL(c) && CELL_TYPE(c) == BT_INEXACT)
#define IS_RATIONAL(c) (IS_CELL(c) && CELL_TYPE(c) == BT_RATIONAL)
#define IS_COMPLEX(c) (IS_CELL(c) && CELL_TYPE(c) == BT_COMPLEX)
#define IS_FUNCTION(c) (IS_CELL(c) && CELL_TYPE(c) == BT_FUNCTION)
#define IS_BUILTIN(c) (IS_CELL(c) && CELL_TYPE(c) == BT_BUILTIN)
#define IS_MACRO(c) (IS_CELL(c) && CELL_TYPE(c) == BT_MACRO)
#define IS_SYNTAX(c) (IS_CELL(c) && CELL_TYPE(c) == BT_SYNTAX)
#define IS_CONT(c) (IS_CELL(c) && CELL_TYPE(c) == BT_CONT)
#define IS_INPORT(c) (IS_CELL(c) && CELL_TYPE(c) == BT_INPORT)
#define IS_OUTPORT(c) (IS_CELL(c) && CELL_TYPE(c) == BT_OUTPORT)
#define IS_STRINPORT(c) (IS_CELL(c) && CELL_TYPE(c) == BT_STRINPORT)
#define IS_STROUTPORT(c) (IS_CELL(c) && CELL_TYPE(c) == BT_STROUTPORT)
#define IS_INPUT_PORT(c) (IS_INPORT(c) || IS_STRINPORT(c))
#define IS_OUTPUT_PORT(c) (IS_OUTPORT(c) || IS_STROUTPORT(c))
#define IS_MULTIVAL(c) (IS_CELL(c) && CELL_TYPE(c) == BT_MULTIVAL)

// Keyword checking macro (for special forms)
#define IS_KEYWORD(c, kw) (IS_ATOM(c) && CELL_ID(c) == (kw))

// ============================================================================
// Pointer Accessor Macros (for type-safe access to pointer-based cells)
// ============================================================================

#define CELL_PTR(c) (ctx.cons_cells[(c)].ptr)
#define GET_STRING_PTR(c) ((char *)CELL_PTR(c))
#define GET_VECTOR_PTR(c) ((vector_data *)CELL_PTR(c))
#define GET_FILE_PORT_PTR(c) ((file_port *)CELL_PTR(c))
#define GET_PORT_PTR(c) (GET_FILE_PORT_PTR(c)->file)
#define GET_STRPORT_PTR(c) ((string_port *)CELL_PTR(c))
#define GET_HASHTABLE_PTR(c) ((hash_table_data *)CELL_PTR(c))
#define GET_CHAR_CODE(c) ((int)CELL_ID(c))

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
    (IS_BIGNUM(a) || IS_BIGNUM(b))

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

// ============================================================================
// Debug Assertions
// ============================================================================
//
// These macros provide runtime invariant checking in debug builds.
// They are compiled out in release builds (when NDEBUG is defined).
//
// Usage:
//   LISP_ASSERT(condition) - panics with condition text if false
//   LISP_ASSERT_MSG(condition, msg) - panics with custom message if false
//   LISP_ASSERT_FMT(condition, fmt, ...) - panics with formatted message
//
// Unlike ERROR_RETURN, these indicate internal bugs, not user errors.

#ifdef NDEBUG
#define LISP_ASSERT(cond) ((void)0)
#define LISP_ASSERT_MSG(cond, msg) ((void)0)
#define LISP_ASSERT_FMT(cond, fmt, ...) ((void)0)
#else
#define LISP_ASSERT(cond)                                                      \
    do {                                                                       \
        if (!(cond)) {                                                         \
            fprintf(stderr, "Assertion failed at %s:%d: %s\n", __FILE__,       \
                    __LINE__, #cond);                                          \
            lisp_panic("assertion failed");                                    \
        }                                                                      \
    } while (0)

#define LISP_ASSERT_MSG(cond, msg)                                             \
    do {                                                                       \
        if (!(cond)) {                                                         \
            fprintf(stderr, "Assertion failed at %s:%d: %s\n", __FILE__,       \
                    __LINE__, (msg));                                          \
            lisp_panic("assertion failed");                                    \
        }                                                                      \
    } while (0)

#define LISP_ASSERT_FMT(cond, fmt, ...)                                        \
    do {                                                                       \
        if (!(cond)) {                                                         \
            fprintf(stderr, "Assertion failed at %s:%d: " fmt "\n", __FILE__,  \
                    __LINE__, __VA_ARGS__);                                    \
            lisp_panic("assertion failed");                                    \
        }                                                                      \
    } while (0)
#endif

// Convenience macros for common invariants
#define LISP_ASSERT_VALID_CELL(c)                                              \
    LISP_ASSERT_FMT((c) < SEMISPACE_SIZE * 2, "invalid cell index: %u", (c))

#define LISP_ASSERT_TYPE(c, expected)                                          \
    LISP_ASSERT_FMT(CELL_TYPE(c) == (expected), "expected type %d, got %d",    \
                    (expected), CELL_TYPE(c))

#endif // TYPES_H
