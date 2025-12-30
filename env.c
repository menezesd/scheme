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
    unsigned frame = alloc_cons(0, 0);
    return alloc_cons(frame, 0);
}

unsigned defvar(unsigned var, unsigned aval, unsigned env)
{
    unsigned frame = car(env);
    int64_t vid = CELL_ID(var);
    unsigned vals = cdr(frame);
    unsigned vars = car(frame);

    for (; vars; vars = cdr(vars), vals = cdr(vals)) {
        if (CELL_TYPE(vars) == BT_ATOM) {
            if (CELL_ID(vars) == vid) {
                write_barrier(vals, aval); // Generational GC
                CELL_CAR(vals) = aval;
                return var;
            } else {
                break;
            }
        }

        if (CELL_ID(car(vars)) == vid) {
            write_barrier(vals, aval); // Generational GC
            CELL_CAR(vals) = aval;
            return var;
        }
    }

    vars = car(frame);
    vals = cdr(frame);
    // Protect all variables used across allocations
    gc_protect(&frame);
    gc_protect(&var);
    gc_protect(&vars);
    gc_protect(&aval);
    gc_protect(&vals);
    unsigned new_vars = 0;
    gc_protect(&new_vars);
    new_vars = alloc_cons(var, vars);
    unsigned new_vals = alloc_cons(aval, vals);
    gc_unprotect(6);
    write_barrier(frame, new_vars); // Generational GC
    write_barrier(frame, new_vals);
    CELL_CAR(frame) = new_vars;
    CELL_CDR(frame) = new_vals;
    return var;
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
                    unsigned oid = car(vals);
                    write_barrier(vals, aval); // Generational GC
                    CELL_CAR(vals) = aval;
                    return oid;
                } else {
                    break;
                }
            }
            if (CELL_ID(car(vars)) == var) {
                unsigned oid = car(vals);
                write_barrier(vals, aval); // Generational GC
                CELL_CAR(vals) = aval;
                return oid;
            }
            vars = cdr(vars);
            vals = cdr(vals);
        }
        env = cdr(env);
    }

    show_error("unbound variable: %s", ctx.atom_table[var]);
    return TOK_ERROR;
}

// Internal lookup - returns TOK_ERROR if not found (no error message)
static unsigned lookup_internal(int64_t var, unsigned env)
{
    while (env) {
        unsigned frame = car(env);
        for (unsigned vars = car(frame), vals = cdr(frame); vars;
             vars = cdr(vars), vals = cdr(vals)) {
            if (CELL_TYPE(vars) == BT_ATOM) {
                if (CELL_ID(vars) == var) {
                    return vals;
                } else {
                    break;
                }
            }

            if (CELL_ID(car(vars)) == var) {
                return car(vals);
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
        show_error("undefined variable: %s", ctx.atom_table[var]);
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
    unsigned vars = 0, vals = 0;
    unsigned vars_tail = 0, vals_tail = 0;

    // Protect all loop-carried variables
    gc_protect(&vars);
    gc_protect(&vals);
    gc_protect(&vars_tail);
    gc_protect(&vals_tail);
    gc_protect(&params);
    gc_protect(&args);

    while (params && CELL_TYPE(params) == BT_CONS) {
        unsigned var = car(params);
        unsigned val = args ? car(args) : 0;

        unsigned vc = 0;
        gc_protect(&val);  // val may be in nursery
        gc_protect(&vc);
        vc = alloc_cons(var, 0);
        unsigned ac = alloc_cons(val, 0);
        gc_unprotect(2);
        if (!vars) {
            vars = vc;
            vals = ac;
        } else {
            write_barrier(vars_tail, vc); // GC may have promoted vars_tail
            write_barrier(vals_tail, ac);
            CELL_CDR(vars_tail) = vc;
            CELL_CDR(vals_tail) = ac;
        }
        vars_tail = vc;
        vals_tail = ac;

        params = cdr(params);
        args = args ? cdr(args) : 0;
    }

    // Handle rest parameter (dotted notation)
    if (params && CELL_TYPE(params) == BT_ATOM) {
        unsigned vc = 0;
        gc_protect(&vc);
        vc = alloc_cons(params, 0);
        unsigned ac = alloc_cons(args, 0);
        gc_unprotect(1);
        if (!vars) {
            vars = vc;
            vals = ac;
        } else {
            write_barrier(vars_tail, vc); // GC may have promoted vars_tail
            write_barrier(vals_tail, ac);
            CELL_CDR(vars_tail) = vc;
            CELL_CDR(vals_tail) = ac;
        }
    }

    unsigned result = alloc_cons(vars, vals);
    gc_unprotect(6);
    return result;
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
    {"newline", PNEWLINE},
    {"read", PREAD},
    {"read-char", PREADCHAR},
    {"peek-char", PPEEKCHAR},
    {"write-char", PWRITECHAR},
    {"eof-object?", PEOF},
    {"char-ready?", PCHARREADY},
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
    {"floor", PFLOOR},
    {"ceiling", PCEILING},
    {"truncate", PTRUNCATE},
    {"round", PROUND},

    // Random numbers (SRFI-27)
    {"random-integer", PRANDOMINTEGER},
    {"random-real", PRANDOMREAL},
    {"random-seed!", PRANDOMSEED},

    // Control
    {"apply", PAPPLY},
    {"call/cc", PCALLCC},
    {"call-with-current-continuation", PCALLCC},

    // Misc
    {"error", PERROR},
    {"gensym", PGENSYM},
    {"gc-flip", PGCFLIP},

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

    {NULL, 0}};

unsigned default_environment(void)
{
    unsigned env = empty_environment();

    // Register all builtins from table
    for (int i = 0; builtins[i].name; i++) {
        defvar(atom_from_string(builtins[i].name), mk_primop(builtins[i].prim),
               env);
    }

    // Register special atoms
    defvar(ctx.atom_true, ctx.atom_true, env);

    return env;
}
