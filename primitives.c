/**
 * @file primitives.c
 * @brief Built-in primitive procedures - main dispatch
 *
 * This file contains the main dispatch function for primitives and remaining
 * operations not extracted to separate modules. Most primitives are now in:
 *
 * - prim_numeric.c: Arithmetic (+, -, *, /, mod, quotient, remainder, abs)
 * - prim_compare.c: Comparison operations (=, <, >, <=, >=, char/string compare)
 * - prim_list.c: List operations (append, reverse)
 * - prim_string.c: String operations (string-append, substring)
 * - prim_type.c: Type predicates (number?, symbol?, etc.)
 * - prim_char.c: Character operations (char->integer, char predicates)
 * - prim_vector.c: Vector operations (make-vector, vector-ref, etc.)
 * - prim_math.c: Math functions (sqrt, sin, cos, floor, etc.)
 * - prim_io.c: I/O operations (read, write, display)
 * - prim_port.c: Port operations (open/close, string ports)
 * - prim_numtower.c: Numeric tower (complex, rational, exactness)
 */

#include "primitives.h"
#include "prim_internal.h"

// ============================================================================
// Main Dispatch Function
// ============================================================================

unsigned apply_primitive(unsigned prim_id, unsigned args)
{
    switch (prim_id) {
    // Arithmetic - delegated to prim_numeric.c
    case PPLUS:
        return prim_plus(args);
    case PMINUS:
        return prim_minus(args);
    case PTIMES:
        return prim_mult(args);
    case PDIV:
        return prim_div(args);
    case PMOD:
        return prim_modulo(args);
    case PREMAINDER:
        return prim_remainder(args);
    case PQUOTIENT:
        return prim_quotient(args);
    case PABS:
        return prim_abs(args);

    // Numeric comparison - delegated to prim_compare.c
    case PEQUAL:
        return numeric_compare(args, CMP_EQ);
    case PLT:
        return numeric_compare(args, CMP_LT);
    case PGT:
        return numeric_compare(args, CMP_GT);
    case PLEQ:
        return numeric_compare(args, CMP_LE);
    case PGEQ:
        return numeric_compare(args, CMP_GE);

    // Logic
    case PNOT:
        REQUIRE_ARGS(args, 1, 1, "not");
        return car(args) ? 0 : ctx.atom_true;
    case PEQ: {
        unsigned arg1 = car(args);
        unsigned arg2 = cadr(args);
        if (CELL_TYPE(arg1) != CELL_TYPE(arg2))
            return 0;
        switch (CELL_TYPE(arg1)) {
        case BT_NUM:
        case BT_FUNCTION:
        case BT_BUILTIN:
        case BT_ATOM:
            return CELL_ID(arg1) == CELL_ID(arg2) ? ctx.atom_true : 0;
        default:
            return arg1 == arg2 ? ctx.atom_true : 0;
        }
    }
    case PEQUALP:
        REQUIRE_ARGS(args, 2, 2, "equal?");
        return deep_equal(car(args), cadr(args)) ? ctx.atom_true : 0;

    // List operations
    case PCONS:
        return alloc_cons(car(args), cadr(args));
    case PCAR:
        return caar(args);
    case PCDR:
        return cdar(args);
    case PSETCAR: {
        REQUIRE_ARGS(args, 2, 2, "set-car!");
        unsigned arg1 = car(args);
        if (!IS_PAIR(arg1))
            ERROR_RETURN("set-car!: not a pair");
        unsigned arg2 = cadr(args);
        return CELL_CAR(arg1) = arg2;
    }
    case PSETCDR: {
        REQUIRE_ARGS(args, 2, 2, "set-cdr!");
        unsigned arg1 = car(args);
        if (!IS_PAIR(arg1))
            ERROR_RETURN("set-cdr!: not a pair");
        unsigned arg2 = cadr(args);
        return CELL_CDR(arg1) = arg2;
    }
    case PLIST:
        return args;
    case PLENGTH: {
        REQUIRE_ARGS(args, 1, 1, "length");
        unsigned lst = car(args);
        if (CELL_TYPE(lst) == BT_STRING)
            return store(strlen((char *)(intptr_t)CELL_ID(lst)));
        if (CELL_TYPE(lst) == BT_VECTOR)
            return store(vector_len(lst));
        return store(list_length(lst));
    }
    case PAPPEND:
        return prim_append(args);
    case PREVERSE:
        return prim_reverse(args);
    case PLASTPAIR: {
        REQUIRE_ARGS(args, 1, 1, "last-pair");
        unsigned lst = car(args);
        if (!lst || CELL_TYPE(lst) != BT_CONS) {
            show_error("last-pair: not a pair");
            return TOK_ERROR;
        }
        while (cdr(lst) && CELL_TYPE(cdr(lst)) == BT_CONS)
            lst = cdr(lst);
        return lst;
    }

    // Type predicates - delegated to prim_type.c
    case PSYMP:
    case PNUMP:
    case PNUMBERP:
    case PINTEGERP:
    case PREALP:
    case PEXACTP:
    case PINEXACTP:
    case PCOMPLEXP:
    case PRATIONALP:
    case PPROCP:
    case PCONSP:
    case PNULLP:
    case PSTRINGP:
    case PCHARP:
    case PVECTORP:
    case PBOOLP:
    case PLISTP:
        return apply_type_predicate(prim_id, args);

    // I/O - delegated to prim_io.c
    case PDISPLAY:
    case PWRITE:
    case PNEWLINE:
    case PREAD:
    case PREADCHAR:
    case PPEEKCHAR:
    case PWRITECHAR:
    case PEOF:
    case PCHARREADY:
        return apply_io_primitive(prim_id, args);

    // Ports - delegated to prim_port.c
    case POPENINPUT:
    case POPENOUTPUT:
    case PCLOSEINPUT:
    case PCLOSEOUTPUT:
    case PINPUTPORTP:
    case POUTPUTPORTP:
    case PCURRENTINPUT:
    case PCURRENTOUTPUT:
    case POPENOUTPUTSTRING:
    case PGETOUTPUTSTRING:
    case POPENINPUTSTRING:
    case PSTRINGPORTP:
        return apply_port_primitive(prim_id, args);

    // String operations
    case PSTRLEN: {
        REQUIRE_ARGS(args, 1, 1, "string-length");
        CHECK_STRING(car(args), "string-length");
        char *s = GET_STRING_PTR(car(args));
        return store(strlen(s));
    }
    case PSTRREF: {
        REQUIRE_ARGS(args, 2, 2, "string-ref");
        CHECK_STRING(car(args), "string-ref");
        char *s = GET_STRING_PTR(car(args));
        size_t len = strlen(s);
        int64_t idx = CELL_ID(cadr(args));
        if (idx < 0 || (size_t)idx >= len) {
            show_error("string-ref: index out of bounds");
            return TOK_ERROR;
        }
        return make_char(s[idx]);
    }
    case PSTRSET: {
        REQUIRE_ARGS(args, 3, 3, "string-set!");
        CHECK_STRING(car(args), "string-set!");
        char *s = GET_STRING_PTR(car(args));
        size_t len = strlen(s);
        int64_t idx = CELL_ID(cadr(args));
        char c = (char)CELL_ID(caddr(args));
        if (idx < 0 || (size_t)idx >= len) {
            show_error("string-set!: index out of bounds");
            return TOK_ERROR;
        }
        s[idx] = c;
        return 0;
    }
    case PSTRAPP:
        return prim_string_append(args);
    case PSUBSTR:
        return prim_substring(args);
    case PSTR2SYM: {
        REQUIRE_ARGS(args, 1, 1, "string->symbol");
        CHECK_STRING(car(args), "string->symbol");
        char *s = GET_STRING_PTR(car(args));
        return atom_from_string(s);
    }
    case PSYM2STR: {
        REQUIRE_ARGS(args, 1, 1, "symbol->string");
        CHECK_SYMBOL(car(args), "symbol->string");
        const char *s = ctx.atom_table[CELL_ID(car(args))];
        return make_string_copy(s);
    }
    case PNUM2STR: {
        REQUIRE_ARGS(args, 1, 2, "number->string");
        unsigned num = car(args);
        int radix = cdr(args) ? (int)CELL_ID(cadr(args)) : 10;
        if (radix < 2 || radix > 36) {
            show_error("number->string: radix must be between 2 and 36");
            return TOK_ERROR;
        }
        char buf[NUMBER_BUF_SIZE];
        if (IS_NUM(num)) {
            int64_t n = CELL_ID(num);
            if (radix == 10) {
                snprintf(buf, sizeof(buf), "%" PRId64, n);
            } else {
                // Convert to specified radix
                char *p = buf + sizeof(buf) - 1;
                *p = '\0';
                bool neg = n < 0;
                if (neg)
                    n = -n;
                if (n == 0) {
                    *--p = '0';
                } else {
                    while (n > 0) {
                        int d = n % radix;
                        *--p = (d < 10) ? '0' + d : 'a' + d - 10;
                        n /= radix;
                    }
                }
                if (neg)
                    *--p = '-';
                memmove(buf, p, buf + sizeof(buf) - p);
            }
        } else if (IS_INEXACT(num)) {
            if (radix != 10) {
                show_error("number->string: inexact numbers require radix 10");
                return TOK_ERROR;
            }
            double d = to_double(num);
            snprintf(buf, sizeof(buf), "%g", d);
        } else {
            show_error("number->string: not a number");
            return TOK_ERROR;
        }
        return make_string_copy(buf);
    }
    case PSTR2NUM: {
        REQUIRE_ARGS(args, 1, 2, "string->number");
        CHECK_STRING(car(args), "string->number");
        char *s = GET_STRING_PTR(car(args));
        int radix = cdr(args) ? (int)CELL_ID(cadr(args)) : 10;
        if (radix < 2 || radix > 36) {
            show_error("string->number: radix must be between 2 and 36");
            return TOK_ERROR;
        }
        if (radix == 10) {
            // Use standard parsing which handles floats
            return atom_from_string(s);
        }
        // Parse integer in specified radix
        char *end;
        long long val = strtoll(s, &end, radix);
        if (end == s || *end != '\0') {
            return 0; // Return #f for invalid number
        }
        return store(val);
    }
    case PMAKESTR: {
        REQUIRE_ARGS(args, 1, 2, "make-string");
        int64_t len = CELL_ID(car(args));
        if (len < 0) {
            show_error("make-string: negative length");
            return TOK_ERROR;
        }
        char fill = cdr(args) ? (char)CELL_ID(cadr(args)) : ' ';
        char *s = malloc(len + 1);
        if (!s) {
            show_error("make-string: out of memory");
            return TOK_ERROR;
        }
        memset(s, fill, len);
        s[len] = '\0';
        return make_string_owned(s);
    }
    case PSTRCOPY: {
        REQUIRE_ARGS(args, 1, 1, "string-copy");
        CHECK_STRING(car(args), "string-copy");
        char *s = GET_STRING_PTR(car(args));
        return make_string_copy(s);
    }
    case PSTR2LIST: {
        REQUIRE_ARGS(args, 1, 1, "string->list");
        CHECK_STRING(car(args), "string->list");
        char *s = GET_STRING_PTR(car(args));
        size_t len = strlen(s);
        unsigned result = 0;
        for (size_t i = len; i > 0; i--) {
            result = alloc_cons(make_char(s[i - 1]), result);
        }
        return result;
    }
    case PLIST2STR: {
        REQUIRE_ARGS(args, 1, 1, "list->string");
        unsigned lst = car(args);
        size_t len = list_length(lst);
        char *s = malloc(len + 1);
        if (!s) {
            show_error("list->string: out of memory");
            return TOK_ERROR;
        }
        size_t i = 0;
        for (; lst; lst = cdr(lst), i++) {
            s[i] = (char)CELL_ID(car(lst));
        }
        s[i] = '\0';
        return make_string_owned(s);
    }
    case PSTRFILL: {
        REQUIRE_ARGS(args, 2, 2, "string-fill!");
        CHECK_STRING(car(args), "string-fill!");
        char *s = GET_STRING_PTR(car(args));
        int c = (int)CELL_ID(cadr(args));
        size_t len = strlen(s);
        for (size_t i = 0; i < len; i++)
            s[i] = c;
        return 0;
    }

    // String comparisons - delegated to prim_compare.c
    case PSTREQ:
    case PSTRLT:
    case PSTRGT:
    case PSTRLE:
    case PSTRGE:
    case PSTREQI:
    case PSTRLTI:
    case PSTRGTI:
    case PSTRLEI:
    case PSTRGEI: {
        unsigned offset = prim_id - PSTREQ;
        return string_compare(args, (cmp_op)(offset % 5), offset >= 5);
    }

    // Character operations - delegated to prim_char.c
    case PCHARCODE:
    case PCODECHAR:
    case PCHARUP:
    case PCHARDOWN:
    case PCHAREQ:
    case PCHARLT:
    case PCHARGT:
    case PCHARLE:
    case PCHARGE:
    case PCHAREQI:
    case PCHARLTI:
    case PCHARGTI:
    case PCHARLEI:
    case PCHARGEI:
    case PCHARALPHA:
    case PCHARNUMERIC:
    case PCHARWHITE:
    case PCHARUPPER:
    case PCHARLOWER:
        return apply_char_primitive(prim_id, args);

    // Vector operations - delegated to prim_vector.c
    case PMAKEVEC:
    case PVECTOR:
    case PVECREF:
    case PVECSET:
    case PVECLEN:
    case PVECFILL:
    case PLIST2VEC:
    case PVEC2LIST:
        return apply_vector_primitive(prim_id, args);

    // Math functions - delegated to prim_math.c
    case PSQRT:
    case PEXPT:
    case PSIN:
    case PCOS:
    case PTAN:
    case PASIN:
    case PACOS:
    case PATAN:
    case PLOG:
    case PEXP:
    case PFLOOR:
    case PCEILING:
    case PTRUNCATE:
    case PROUND:
        return apply_math_primitive(prim_id, args);

    // Misc
    case PERROR: {
        fprintf(stderr, "error: ");
        FORLIST(a, args)
        {
            display_obj(car(a));
            if (cdr(a))
                fprintf(stderr, " ");
        }
        fprintf(stderr, "\n");
        return TOK_ERROR;
    }
    case PGENSYM: {
        char buf[32];
        snprintf(buf, sizeof(buf), "g%u", gensym_counter++);
        return atom_from_string(buf);
    }
    // PGCFLIP is handled specially in eval.c (needs environment as root)

    // Numeric tower operations - delegated to prim_numtower.c
    case PNUMERATOR:
    case PDENOMINATOR:
    case PMAKERECT:
    case PMAKEPOLAR:
    case PREALPART:
    case PIMAGPART:
    case PMAGNITUDE:
    case PANGLE:
    case PEXACT2INEXACT:
    case PINEXACT2EXACT:
    case PRATIONALIZE:
    case PFINITE:
    case PINFINITE:
    case PNAN:
        return apply_numtower_primitive(prim_id, args);

    // String constructor
    case PSTRING: {
        // (string char ...) - construct string from characters
        unsigned len = list_length(args);
        char *s = malloc(len + 1);
        if (!s) {
            show_error("string: out of memory");
            return TOK_ERROR;
        }
        unsigned i = 0;
        for (unsigned a = args; a; a = cdr(a), i++) {
            unsigned ch = car(a);
            if (!IS_CHAR(ch)) {
                free(s);
                show_error("string: argument is not a character");
                return TOK_ERROR;
            }
            s[i] = (char)CELL_ID(ch);
        }
        s[len] = '\0';
        return make_string_owned(s);
    }

    // Transcript
    case PTRANSCRIPTON: {
        REQUIRE_ARGS(args, 1, 1, "transcript-on");
        CHECK_STRING(car(args), "transcript-on");
        if (ctx.transcript) {
            show_error("transcript-on: transcript already active");
            return TOK_ERROR;
        }
        char *filename = GET_STRING_PTR(car(args));
        ctx.transcript = fopen(filename, "w");
        if (!ctx.transcript) {
            show_error("transcript-on: cannot open %s", filename);
            return TOK_ERROR;
        }
        return 0;
    }
    case PTRANSCRIPTOFF: {
        if (!ctx.transcript) {
            show_error("transcript-off: no transcript active");
            return TOK_ERROR;
        }
        fclose(ctx.transcript);
        ctx.transcript = NULL;
        return 0;
    }

    // R5RS multiple values
    case PVALUES: {
        // (values) => single unspecified value
        // (values x) => x
        // (values x y ...) => multiple values object
        if (!args)
            return 0; // No values = unspecified
        if (!cdr(args))
            return car(args); // Single value
        // Multiple values - wrap in BT_MULTIVAL
        unsigned mv = alloc();
        CELL_TYPE(mv) = BT_MULTIVAL;
        CELL_CAR(mv) = args;
        CELL_CDR(mv) = 0;
        return mv;
    }

    // R5RS environment procedures
    case PSCHEMEENV: {
        REQUIRE_ARGS(args, 1, 1, "scheme-report-environment");
        int64_t version = CELL_ID(car(args));
        if (version != 5) {
            show_error("scheme-report-environment: unsupported version %lld",
                       (long long)version);
            return TOK_ERROR;
        }
        return default_environment();
    }
    case PNULLENV: {
        REQUIRE_ARGS(args, 1, 1, "null-environment");
        int64_t version = CELL_ID(car(args));
        if (version != 5) {
            show_error("null-environment: unsupported version %lld",
                       (long long)version);
            return TOK_ERROR;
        }
        // Return environment with only syntax bindings (no procedures)
        // Need to include #t for boolean values
        unsigned env = empty_environment();
        defvar(ctx.atom_true, ctx.atom_true, env);
        return env;
    }

    // Special cases handled elsewhere
    case PAPPLY:
        show_error(
            "apply: internal error - should be handled in apply_function");
        return TOK_ERROR;
    case PLOAD:
        show_error(
            "load: internal error - should be handled in apply_function");
        return TOK_ERROR;
    case PCALLCC:
        show_error("call/cc must be called as a function, not a primitive");
        return TOK_ERROR;
    case PCALLWITHVALUES:
        show_error("call-with-values: internal error - should be handled in "
                   "apply_function");
        return TOK_ERROR;
    case PEVAL:
        show_error(
            "eval: internal error - should be handled in apply_function");
        return TOK_ERROR;
    case PINTERACTIONENV:
        show_error("interaction-environment: internal error - should be "
                   "handled in apply_function");
        return TOK_ERROR;

    default:
        show_error("unknown primitive: %u", prim_id);
        return TOK_ERROR;
    }
}
