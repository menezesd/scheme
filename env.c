/**
 * @file env.c
 * @brief Environment and variable binding management
 *
 * This file implements lexical environments for variable scoping.
 *
 * ## Environment Structure
 * Environments are represented as a list of frames, where each frame is a
 * cons cell of (vars . vals):
 *
 *   env = (frame1 . (frame2 . (frame3 . nil)))
 *
 * Each frame:
 *   frame = (vars . vals)
 *   vars  = list of variable atoms (or single atom for rest parameter)
 *   vals  = list of corresponding values (parallel structure to vars)
 *
 * ## Operations
 * - lookup: Search frames from innermost to outermost
 * - defvar: Add binding to current (innermost) frame
 * - setvar: Modify existing binding (error if not found)
 * - extend_env: Create new frame for function application
 *
 * ## Rest Parameters
 * For (lambda (a b . rest) ...), the vars list is dotted:
 *   vars = (a . (b . rest-atom))
 * The rest atom receives remaining arguments as a list.
 */

#include "env.h"
#include "context.h"

// ============================================================================
// Lookup Cache
// ============================================================================
// 8-entry associative cache for variable lookups with move-to-front on hit.
// Most code has high locality - the same variables are accessed repeatedly.
// Move-to-front keeps hot entries at the front, approximating LRU eviction.

#define LOOKUP_CACHE_SIZE 8

typedef struct {
    int64_t var;       // Variable atom ID (-1 = empty)
    unsigned env;      // Environment where found
    unsigned val_cell; // Cell containing the value (so mutations are visible)
} lookup_cache_entry;

static lookup_cache_entry lookup_cache[LOOKUP_CACHE_SIZE];

// Global inline cache epoch for bytecode IC invalidation
unsigned global_ic_epoch = 0;

// Invalidate all cache entries
static inline void invalidate_lookup_cache(void)
{
    for (int i = 0; i < LOOKUP_CACHE_SIZE; i++) {
        lookup_cache[i].var = -1;
    }
}

// Public function to invalidate cache (called after GC)
void env_invalidate_cache(void)
{
    invalidate_lookup_cache();
}

static unsigned deref_binding_value(unsigned val)
{
    if (IS_BINDING_REF(val))
        return car(CELL_CAR(val));
    return val;
}

static unsigned set_binding_value(unsigned val_cell, unsigned aval)
{
    unsigned old = car(val_cell);
    if (IS_BINDING_REF(old)) {
        unsigned target_val_cell = CELL_CAR(old);
        unsigned target_old = car(target_val_cell);
        cell_set_car(target_val_cell, aval);
        return deref_binding_value(target_old);
    }
    cell_set_car(val_cell, aval);
    return old;
}

static unsigned make_binding_ref(unsigned target_var, unsigned target_val_cell)
{
    unsigned ref = alloc();
    CELL_TYPE(ref) = BT_BINDING_REF;
    CELL_CAR(ref) = target_val_cell;
    CELL_CDR(ref) = target_var;
    return ref;
}

// ============================================================================
// Environment Structure
// ============================================================================
//
// Environments are represented as a list of frames, where each frame is a
// cons cell of (vars . vals):
//
//   env = (frame1 . (frame2 . (frame3 . nil)))
//
// Each frame:
//   frame = (vars . vals)
//   vars  = list of variable atoms (or single atom for rest parameter)
//   vals  = list of corresponding values (parallel structure to vars)
//
// Example: After (define x 1) (define y 2):
//   env = (((x . (y . nil)) . (1 . (2 . nil))) . parent-env)
//
// Special case - rest parameter (lambda (a . rest) ...):
//   vars = (a . rest-atom)  ; dotted list - rest is an atom, not a cons
//   vals = (1 . (2 3 4))    ; rest parameter gets remaining args as list
//
// ============================================================================
// Environment Operations
// ============================================================================

unsigned empty_environment(void)
{
    GC_GUARD;
    unsigned frame = alloc_cons(0, 0);
    gc_protect(&frame);
    unsigned env = alloc_cons(frame, 0);
    return env;
}

unsigned defvar(unsigned var, unsigned aval, unsigned env)
{
    // Invalidate lookup cache - new binding may shadow outer variables
    invalidate_lookup_cache();
    global_ic_epoch++;

    unsigned frame = car(env);
    int64_t vid = CELL_ID(var);
    unsigned vals = cdr(frame);
    unsigned vars = car(frame);

    for (; vars; vars = cdr(vars), vals = cdr(vals)) {
        if (CELL_TYPE(vars) == BT_ATOM) {
            if (CELL_ID(vars) == vid) {
                cell_set_car(vals, aval);
                return var;
            } else {
                break;
            }
        }

        if (CELL_ID(car(vars)) == vid) {
            cell_set_car(vals, aval);
            return var;
        }
    }

    vars = car(frame);
    vals = cdr(frame);
    // Protect all variables used across allocations
    unsigned new_vars, new_vals;
    {
        GC_GUARD;
        gc_protect(&frame);
        gc_protect(&var);
        gc_protect(&vars);
        gc_protect(&aval);
        gc_protect(&vals);
        new_vars = alloc_cons(var, vars);
        gc_protect(&new_vars);
        new_vals = alloc_cons(aval, vals);
    }
    cell_set_car(frame, new_vars);
    cell_set_cdr(frame, new_vals);
    return var;
}

unsigned defvar_alias(unsigned var, unsigned target_var, unsigned target_val_cell,
                      unsigned env)
{
    GC_GUARD;
    gc_protect(&var);
    gc_protect(&target_var);
    gc_protect(&target_val_cell);
    gc_protect(&env);
    unsigned ref = make_binding_ref(target_var, target_val_cell);
    return defvar(var, ref, env);
}

unsigned env_find_binding_cell(int64_t var, unsigned env)
{
    while (env) {
        unsigned frame = car(env);
        unsigned vars = car(frame);
        unsigned vals = cdr(frame);

        while (vars) {
            if (CELL_TYPE(vars) == BT_ATOM) {
                if (CELL_ID(vars) == var)
                    return vals;
                break;
            }
            if (CELL_ID(car(vars)) == var)
                return vals;
            vars = cdr(vars);
            vals = cdr(vals);
        }
        env = cdr(env);
    }

    return 0;
}

unsigned setvar(int64_t var, unsigned aval, unsigned env)
{
    while (env) {
        unsigned frame = car(env);
        unsigned vars = car(frame);
        unsigned vals = cdr(frame);

        while (vars) {
            if (CELL_TYPE(vars) == BT_ATOM) {
                if (CELL_ID(vars) == var) {
                    return set_binding_value(vals, aval);
                } else {
                    break;
                }
            }
            if (CELL_ID(car(vars)) == var) {
                return set_binding_value(vals, aval);
            }
            vars = cdr(vars);
            vals = cdr(vals);
        }
        env = cdr(env);
    }

    // Bounds check before accessing atom_table
    if (var >= 0 && (unsigned)var < ctx.atom_table_cap && ctx.atom_table[var]) {
        show_error("unbound variable: %s", ctx.atom_table[var]);
    } else {
        show_error("unbound variable: <invalid id %ld>", (long)var);
    }
    return TOK_ERROR;
}

// Move cache entry at index i to front (index 0), shifting others down
static inline void cache_move_to_front(int i)
{
    if (i == 0)
        return;
    lookup_cache_entry tmp = lookup_cache[i];
    for (int j = i; j > 0; j--) {
        lookup_cache[j] = lookup_cache[j - 1];
    }
    lookup_cache[0] = tmp;
}

// Insert new entry at front, shifting others down (evicts last)
static inline void cache_insert_front(int64_t var, unsigned env,
                                      unsigned val_cell)
{
    for (int j = LOOKUP_CACHE_SIZE - 1; j > 0; j--) {
        lookup_cache[j] = lookup_cache[j - 1];
    }
    lookup_cache[0].var = var;
    lookup_cache[0].env = env;
    lookup_cache[0].val_cell = val_cell;
}

// Internal lookup - returns TOK_ERROR if not found (no error message)
static unsigned lookup_internal(int64_t var, unsigned env)
{
    // Check cache first - scan all entries for match
    for (int i = 0; i < LOOKUP_CACHE_SIZE; i++) {
        if (lookup_cache[i].var == var && lookup_cache[i].env == env) {
            // Move to front on hit (LRU approximation)
            cache_move_to_front(i);
            return deref_binding_value(car(lookup_cache[0].val_cell));
        }
    }

    unsigned orig_env = env;
    while (env) {
        unsigned frame = car(env);
        for (unsigned vars = car(frame), vals = cdr(frame); vars;
             vars = cdr(vars), vals = cdr(vals)) {
            if (CELL_TYPE(vars) == BT_ATOM) {
                if (CELL_ID(vars) == var) {
                    // Insert at front on miss
                    cache_insert_front(var, orig_env, vals);
                    return deref_binding_value(car(vals));
                } else {
                    break;
                }
            }

            if (CELL_ID(car(vars)) == var) {
                // Insert at front on miss
                cache_insert_front(var, orig_env, vals);
                return deref_binding_value(car(vals));
            }
        }
        env = cdr(env);
    }

    return TOK_ERROR;
}

unsigned lookup(int64_t var, unsigned env)
{
    unsigned result = lookup_internal(var, env);
    if (result == TOK_ERROR) {
        // Bounds check before accessing atom_table
        if (var >= 0 && (unsigned)var < ctx.atom_table_cap && ctx.atom_table[var]) {
            show_error("undefined variable: %s", ctx.atom_table[var]);
        } else {
            show_error("undefined variable: <invalid id %ld>", (long)var);
        }
    }
    return result;
}

// Silent lookup - returns TOK_ERROR if not found (no error message)
// Used by the compiler to check for macros
unsigned lookup_silent(int64_t var, unsigned env)
{
    return lookup_internal(var, env);
}

unsigned bind_params(unsigned params, unsigned args)
{
    GC_GUARD;
    unsigned vars = 0, vals = 0;
    unsigned vars_tail = 0, vals_tail = 0;
    unsigned var = 0, val = 0, vc = 0, ac = 0;

    // Protect all variables once at function entry (not per-iteration)
    gc_protect(&vars);
    gc_protect(&vals);
    gc_protect(&vars_tail);
    gc_protect(&vals_tail);
    gc_protect(&params);
    gc_protect(&args);
    gc_protect(&var);
    gc_protect(&val);
    gc_protect(&vc);
    gc_protect(&ac);

    while (IS_PAIR(params)) {
        if (!IS_PAIR(args)) {
            show_error("procedure: too few arguments");
            return TOK_ERROR;
        }
        var = car(params);
        val = car(args);

        // Validate var is an atom before using it
        if (var && !IS_ATOM(var)) {
            lisp_panic("bind_params: var is not BT_ATOM");
        }

        vc = alloc_cons(var, 0);
        ac = alloc_cons(val, 0);

        if (!vars) {
            vars = vc;
            vals = ac;
        } else {
            cell_set_cdr(vars_tail, vc);
            cell_set_cdr(vals_tail, ac);
        }
        vars_tail = vc;
        vals_tail = ac;

        params = cdr(params);
        args = cdr(args);
    }

    // Handle rest parameter (dotted notation)
    if (IS_ATOM(params)) {
        vc = alloc_cons(params, 0);
        ac = alloc_cons(args, 0);

        if (!vars) {
            vars = vc;
            vals = ac;
        } else {
            cell_set_cdr(vars_tail, vc);
            cell_set_cdr(vals_tail, ac);
        }
    } else if (args) {
        show_error("procedure: too many arguments");
        return TOK_ERROR;
    }

    return alloc_cons(vars, vals);
}

unsigned mk_primop(int64_t id)
{
    unsigned p = alloc();
    CELL_TYPE(p) = BT_BUILTIN;
    CELL_ID(p) = id;
    return p;
}

// ============================================================================
// Builtin Registration Table
// ============================================================================

static const struct {
    const char *name;
    int prim;
} builtins[] = {
    // Arithmetic
    {"+", PPLUS},
    {"-", PMINUS},
    {"*", PTIMES},
    {"/", PDIV},
    {"modulo", PMOD},
    {"remainder", PREMAINDER},
    {"truncate/", PTRUNCATEDIVREM},
    {"floor/", PFLOORDIVREM},
    {"quotient", PQUOTIENT},
    {"abs", PABS},

    // Comparison
    {"=", PEQUAL},
    {"<", PLT},
    {">", PGT},
    {"<=", PLEQ},
    {">=", PGEQ},

    // Logic
    {"not", PNOT},
    {"eq?", PEQ},
    {"eqv?", PEQ},
    {"equal?", PEQUALP},

    // List operations
    {"cons", PCONS},
    {"car", PCAR},
    {"cdr", PCDR},
    {"set-car!", PSETCAR},
    {"set-cdr!", PSETCDR},
    {"list", PLIST},
    {"length", PLENGTH},
    {"append", PAPPEND},
    {"reverse", PREVERSE},
    {"last-pair", PLASTPAIR},

    // Type predicates
    {"symbol?", PSYMP},
    {"number?", PNUMP},
    {"procedure?", PPROCP},
    {"pair?", PCONSP},
    {"null?", PNULLP},
    {"string?", PSTRINGP},
    {"char?", PCHARP},
    {"vector?", PVECTORP},
    {"boolean?", PBOOLP},
    {"list?", PLISTP},
    {"integer?", PINTEGERP},
    {"real?", PREALP},
    {"exact?", PEXACTP},
    {"inexact?", PINEXACTP},

    // I/O
    {"display", PDISPLAY},
    {"write", PWRITE},
    {"write-shared", PWRITESHARED},
    {"write-simple", PWRITESIMPLE},
    {"newline", PNEWLINE},
    {"read", PREAD},
    {"read-char", PREADCHAR},
    {"peek-char", PPEEKCHAR},
    {"write-char", PWRITECHAR},
    {"eof-object", PEOFOBJECT},
    {"eof-object?", PEOF},
    {"char-ready?", PCHARREADY},
    {"read-line", PREADLINE},
    {"load", PLOAD},

    // Ports
    {"open-input-file", POPENINPUT},
    {"open-output-file", POPENOUTPUT},
    {"close-input-port", PCLOSEINPUT},
    {"close-output-port", PCLOSEOUTPUT},
    {"input-port?", PINPUTPORTP},
    {"output-port?", POUTPUTPORTP},
    {"current-input-port", PCURRENTINPUT},
    {"current-output-port", PCURRENTOUTPUT},

    // String operations
    {"string-length", PSTRLEN},
    {"string-ref", PSTRREF},
    {"string-set!", PSTRSET},
    {"string-append", PSTRAPP},
    {"substring", PSUBSTR},
    {"string->symbol", PSTR2SYM},
    {"symbol->string", PSYM2STR},
    {"number->string", PNUM2STR},
    {"string->number", PSTR2NUM},
    {"make-string", PMAKESTR},
    {"string-copy", PSTRCOPY},
    {"string->list", PSTR2LIST},
    {"list->string", PLIST2STR},
    {"string-fill!", PSTRFILL},
    {"string-normalize-nfd", PSTRNFD},
    {"string-normalize-nfc", PSTRNFC},
    {"string-normalize-nfkd", PSTRNFKD},
    {"string-normalize-nfkc", PSTRNFKC},
    {"string-foldcase", PSTRFOLD},
    {"string-upcase", PSTRUP},
    {"string-downcase", PSTRDOWN},
    {"string-titlecase", PSTRTITLE},
    {"string->utf8", PSTRINGTOUTF8},
    {"utf8->string", PUTF8TOSTRING},

    // String comparisons
    {"string=?", PSTREQ},
    {"string<?", PSTRLT},
    {"string>?", PSTRGT},
    {"string<=?", PSTRLE},
    {"string>=?", PSTRGE},
    {"string-ci=?", PSTREQI},
    {"string-ci<?", PSTRLTI},
    {"string-ci>?", PSTRGTI},
    {"string-ci<=?", PSTRLEI},
    {"string-ci>=?", PSTRGEI},

    // Character operations
    {"char->integer", PCHARCODE},
    {"integer->char", PCODECHAR},
    {"char-upcase", PCHARUP},
    {"char-downcase", PCHARDOWN},
    {"char-foldcase", PCHARFOLD},

    // Character comparisons
    {"char=?", PCHAREQ},
    {"char<?", PCHARLT},
    {"char>?", PCHARGT},
    {"char<=?", PCHARLE},
    {"char>=?", PCHARGE},
    {"char-ci=?", PCHAREQI},
    {"char-ci<?", PCHARLTI},
    {"char-ci>?", PCHARGTI},
    {"char-ci<=?", PCHARLEI},
    {"char-ci>=?", PCHARGEI},

    // Character predicates
    {"char-alphabetic?", PCHARALPHA},
    {"char-numeric?", PCHARNUMERIC},
    {"char-whitespace?", PCHARWHITE},
    {"char-upper-case?", PCHARUPPER},
    {"char-lower-case?", PCHARLOWER},

    // Vector operations
    {"make-vector", PMAKEVEC},
    {"vector", PVECTOR},
    {"vector-ref", PVECREF},
    {"vector-set!", PVECSET},
    {"vector-length", PVECLEN},
    {"vector-fill!", PVECFILL},
    {"list->vector", PLIST2VEC},
    {"vector->list", PVEC2LIST},

    // Math functions
    {"sqrt", PSQRT},
    {"expt", PEXPT},
    {"sin", PSIN},
    {"cos", PCOS},
    {"tan", PTAN},
    {"asin", PASIN},
    {"acos", PACOS},
    {"atan", PATAN},
    {"log", PLOG},
    {"exp", PEXP},
    {"log1p", PLOG1P},
    {"logp1", PLOG1P},
    {"expm1", PEXPM1},
    {"sqrt1pm1", PSQRT1PM1},
    {"log1pexp", PLOG1PEXP},
    {"floor", PFLOOR},
    {"ceiling", PCEILING},
    {"truncate", PTRUNCATE},
    {"round", PROUND},

    // Random numbers (SRFI-27)
    {"random-integer", PRANDOMINTEGER},
    {"random-real", PRANDOMREAL},
    {"random-seed!", PRANDOMSEED},

    // Process control
    {"exit", PEXIT},

    // Bitwise operations
    {"bitwise-and", PBITWISEAND},
    {"bitwise-ior", PBITWISEIOR},
    {"bitwise-not", PBITWISENOT},
    {"bitwise-xor", PBITWISEXOR},
    {"arithmetic-shift", PARITHSHIFT},

    // Bytevector operations
    {"make-bytevector", PMAKEBYTEVEC},
    {"bytevector-u8-ref", PBYTEVECREF},
    {"bytevector-u8-set!", PBYTEVECSET},
    {"bytevector-length", PBYTEVECLEN},
    {"bytevector-copy", PBYTEVECCOPY},
    {"bytevector-copy!", PBYTEVECCOPYTO},
    {"bytevector-append", PBYTEVECAPPEND},
    {"bytevector", PBYTEVEC},
    {"bytevector?", PBYTEVECUP},

    // Misc
    {"command-line", PCOMMANDLINE},
    {"write-to-string", PWRITETOSTRING},
    {"list-ref", PLISTREF},
    // Binary I/O
    {"open-binary-input-file", POPENBINARYINPUT},
    {"open-binary-output-file", POPENBINARYOUTPUT},
    {"read-bytevector", PREADBYTEVEC},
    {"read-bytevector!", PREADBYTEVECINTO},
    {"write-bytevector", PWRITEBYTEVEC},
    {"read-string", PREADSTRING},
    {"write-string", PWRITESTRING},
    {"u8-ready?", PU8READY},
    {"peek-u8", PPEEKU8},
    // File system
    {"file-exists?", PFILEEXISTS},
    {"file-regular?", PFILEREGULARP},
    {"file-directory?", PFILEDIRECTORYP},
    {"delete-file", PDELETEFILE},
    {"make-directory", PMAKEDIRECTORY},
    {"delete-directory", PDELETEDIRECTORY},
    {"rename-file", PRENAMEFILE},
    {"current-directory", PCURRENTDIRECTORY},
    {"directory-files", PDIRECTORYFILES},
    {"temporary-file-path", PTEMPFILEPATH},
    {"get-environment-variable", PGETENV},

    // Control
    {"apply", PAPPLY},
    {"call/cc", PCALLCC},
    {"call-with-current-continuation", PCALLCC},

    // Misc
    {"error", PERROR},
    {"gensym", PGENSYM},
    {"gc-flip", PGCFLIP},
    {"gc-stats", PGCSTATS},

    // R3RS numeric tower
    {"complex?", PCOMPLEXP},
    {"rational?", PRATIONALP},
    {"numerator", PNUMERATOR},
    {"denominator", PDENOMINATOR},
    {"make-rectangular", PMAKERECT},
    {"make-polar", PMAKEPOLAR},
    {"real-part", PREALPART},
    {"imag-part", PIMAGPART},
    {"magnitude", PMAGNITUDE},
    {"angle", PANGLE},
    {"exact->inexact", PEXACT2INEXACT},
    {"inexact->exact", PINEXACT2EXACT},
    {"rationalize", PRATIONALIZE},
    {"finite?", PFINITE},
    {"infinite?", PINFINITE},
    {"nan?", PNAN},

    // String constructor
    {"string", PSTRING},

    // Transcript
    {"transcript-on", PTRANSCRIPTON},
    {"transcript-off", PTRANSCRIPTOFF},

    // R5RS multiple values
    {"values", PVALUES},
    {"call-with-values", PCALLWITHVALUES},

    // R5RS eval and environments
    {"eval", PEVAL},
    {"scheme-report-environment", PSCHEMEENV},
    {"null-environment", PNULLENV},
    {"interaction-environment", PINTERACTIONENV},

    // String ports
    {"open-output-string", POPENOUTPUTSTRING},
    {"get-output-string", PGETOUTPUTSTRING},
    {"open-input-string", POPENINPUTSTRING},
    {"string-port?", PSTRINGPORTP},

    // Internal port setters
    {"set-current-input-port!", PSETCURRENTINPUT},
    {"set-current-output-port!", PSETCURRENTOUTPUT},
    {"set-current-error-port!", PSETCURRENTERROR},
    {"current-error-port", PCURRENTERROR},
    {"flush-output-port", PFLUSHOUTPUT},
    {"flush-output", PFLUSHOUTPUT},
    {"current-second", PCURRENTSECOND},
    {"current-jiffy", PCURRENTJIFFY},
    {"jiffies-per-second", PJIFFIESPERSECOND},
    {"get-environment-variables", PGETENVS},
    {"emergency-exit", PEMERGENCYEXIT},
    {"features", PFEATURES},

    // Hash tables
    {"make-hash-table", PMAKEHASHTABLE},
    {"make-strong-eq-hash-table", PMAKESTRONGEQHASHTABLE},
    {"make-eq-hash-table", PMAKEEQHASHTABLE},
    {"make-strong-eqv-hash-table", PMAKESTRONGEQVHASHTABLE},
    {"make-eqv-hash-table", PMAKEEQVHASHTABLE},
    {"make-equal-hash-table", PMAKEEQUALHASHTABLE},
    {"hash-table?", PHASHTABLEP},
    {"hash-table-ref", PHASHTABLEREF},
    {"hash-table-set!", PHASHTABLESET},
    {"hash-table-delete!", PHASHTABLEDELETE},
    {"hash-table-exists?", PHASHTABLEEXISTS},
    {"hash-table-size", PHASHTABLESIZE},
    {"hash-table-clear!", PHASHTABLECLEAR},
    {"hash-table-keys", PHASHTABLEKEYS},
    {"hash-table-values", PHASHTABLEVALUES},
    {"hash-table->alist", PHASHTABLEALIST},

    // R7RS-ish port predicates
    {"port-open?", PPORTOPENP},
    {"input-port-open?", PINPUTPORTOPENP},
    {"output-port-open?", POUTPUTPORTOPENP},
    {"textual-port?", PTEXTUALPORTP},
    {"binary-port?", PBINARYPORTP},

    {NULL, 0}};

unsigned default_environment(void)
{
    GC_GUARD;
    unsigned env = empty_environment();
    gc_protect(&env);

    // Register all builtins from table
    for (int i = 0; builtins[i].name; i++) {
        unsigned name = atom_from_string(builtins[i].name);
        gc_protect(&name);
        unsigned prim = mk_primop(builtins[i].prim);
        gc_protect(&prim);
        defvar(name, prim, env);
        gc_unprotect(2);
    }

    // Register special atoms
    defvar(ctx.atom_true, ctx.atom_true, env);
    defvar(ctx.atom_false, ctx.atom_false, env);
    // Make 't' an alias for true
    unsigned t = atom_from_string("t");
    gc_protect(&t);
    defvar(t, ctx.atom_true, env);

    return env;
}
