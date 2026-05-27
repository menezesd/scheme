/**
 * @file primitives.c
 * @brief Built-in primitive procedures - main dispatch
 *
 * This file contains the main dispatch function for primitives and remaining
 * operations not extracted to separate modules. Most primitives are now in:
 *
 * - prim_numeric.c: Arithmetic (+, -, *, /, mod, quotient, remainder, abs)
 * - prim_compare.c: Comparison operations (=, <, >, <=, >=, char/string
 * compare)
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
#include <limits.h>
#include <stdint.h>
#include <time.h>

// Command line args (set by main.c, defaults for test binaries)
int saved_argc __attribute__((weak)) = 0;
char **saved_argv __attribute__((weak)) = NULL;

static void *malloc_array(unsigned count, size_t elem_size)
{
    if (elem_size != 0 && (size_t)count > SIZE_MAX / elem_size)
        return NULL;
    return malloc((size_t)count * elem_size);
}

// ============================================================================
// Main Dispatch Function
// ============================================================================

static bool expect_u8(unsigned value, int64_t *out, const char *name)
{
    int64_t byte;
    if (!expect_exact_int64(value, &byte, name))
        return false;
    if (byte < 0 || byte > 255) {
        show_error("%s: byte out of range", name);
        return false;
    }
    *out = byte;
    return true;
}

unsigned apply_primitive_argv(unsigned prim_id, unsigned argc, unsigned *argv)
{
    switch (prim_id) {
    // Arithmetic - delegated to prim_numeric.c
    case PPLUS:
        return prim_plus(argc, argv);
    case PMINUS:
        return prim_minus(argc, argv);
    case PTIMES:
        return prim_mult(argc, argv);
    case PDIV:
        return prim_div(argc, argv);
    case PMOD:
        return prim_modulo(argc, argv);
    case PREMAINDER:
        return prim_remainder(argc, argv);
    case PQUOTIENT:
        return prim_quotient(argc, argv);
    case PABS:
        return prim_abs(argc, argv);

    // Numeric comparison - delegated to prim_compare.c
    case PEQUAL:
        return numeric_compare(argc, argv, CMP_EQ);
    case PLT:
        return numeric_compare(argc, argv, CMP_LT);
    case PGT:
        return numeric_compare(argc, argv, CMP_GT);
    case PLEQ:
        return numeric_compare(argc, argv, CMP_LE);
    case PGEQ:
        return numeric_compare(argc, argv, CMP_GE);

    // Logic
    case PNOT:
        REQUIRE_ARGC(argc, 1, 1, "not");
        return IS_FALSE(argv[0]) ? ctx.atom_true : ctx.atom_false;
    case PEQ: {
        REQUIRE_ARGC(argc, 2, 2, "eq?");
        unsigned arg1 = argv[0];
        unsigned arg2 = argv[1];
        if (CELL_TYPE(arg1) != CELL_TYPE(arg2))
            return ctx.atom_false;
        switch (CELL_TYPE(arg1)) {
        case BT_NUM:
        case BT_FUNCTION:
        case BT_BUILTIN:
        case BT_ATOM:
        case BT_CHAR:
            return CELL_ID(arg1) == CELL_ID(arg2) ? ctx.atom_true
                                                  : ctx.atom_false;
        default:
            return arg1 == arg2 ? ctx.atom_true : ctx.atom_false;
        }
    }
    case PEQUALP:
        REQUIRE_ARGC(argc, 2, 2, "equal?");
        return deep_equal(argv[0], argv[1]) ? ctx.atom_true
                                                 : ctx.atom_false;

    // List operations
    case PCONS:
        REQUIRE_ARGC(argc, 2, 2, "cons");
        return alloc_cons(argv[0], argv[1]);
    case PCAR:
        REQUIRE_ARGC(argc, 1, 1, "car");
        CHECK_PAIR(argv[0], "car");
        return car(argv[0]);
    case PCDR:
        REQUIRE_ARGC(argc, 1, 1, "cdr");
        CHECK_PAIR(argv[0], "cdr");
        return cdr(argv[0]);
    case PSETCAR: {
        REQUIRE_ARGC(argc, 2, 2, "set-car!");
        unsigned arg1 = argv[0];
        CHECK_PAIR(arg1, "set-car!");
        unsigned arg2 = argv[1];
        cell_set_car(arg1, arg2);
        return arg2;
    }
    case PSETCDR: {
        REQUIRE_ARGC(argc, 2, 2, "set-cdr!");
        unsigned arg1 = argv[0];
        CHECK_PAIR(arg1, "set-cdr!");
        unsigned arg2 = argv[1];
        cell_set_cdr(arg1, arg2);
        return arg2;
    }
    case PLIST:
        if (argc == 0)
            return 0;
        {
            GC_GUARD;
            unsigned result = 0;
            gc_protect(&result);
            for (unsigned i = argc; i > 0; i--) {
                result = alloc_cons(argv[i - 1], result);
            }
            return result;
        }
    case PLENGTH: {
        REQUIRE_ARGC(argc, 1, 1, "length");
        unsigned lst = argv[0];
        if (CELL_TYPE(lst) == BT_STRING)
            return store(strlen(GET_STRING_PTR(lst)));
        if (CELL_TYPE(lst) == BT_VECTOR)
            return store(vector_len(lst));
        unsigned len = 0;
        if (!list_length_checked(lst, &len, "length"))
            return TOK_ERROR;
        return store(len);
    }
    case PAPPEND:
        return prim_append(argc, argv);
    case PREVERSE:
        return prim_reverse(argc, argv);
    case PLASTPAIR: {
        REQUIRE_ARGC(argc, 1, 1, "last-pair");
        unsigned lst = argv[0];
        if (!IS_PAIR(lst)) {
            show_error("last-pair: not a pair");
            return TOK_ERROR;
        }
        for (;;) {
            unsigned next = cdr(lst);
            if (!next)
                return lst;
            if (!IS_PAIR(next)) {
                show_error("last-pair: improper list");
                return TOK_ERROR;
            }
            lst = next;
        }
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
        return apply_type_predicate(prim_id, argc, argv);

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
    case PREADLINE:
    case PEXIT:
        return apply_io_primitive(prim_id, argc, argv);

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
    case PSETCURRENTINPUT:
    case PSETCURRENTOUTPUT:
    case PFLUSHOUTPUT:
        return apply_port_primitive(prim_id, argc, argv);

    // Time
    case PCURRENTSECOND: {
        REQUIRE_ARGC(argc, 0, 0, "current-second");
        time_t now = time(NULL);
        // Return as inexact for sub-second precision compatibility
        return store_inexact((double)now);
    }

    // String operations
    case PSTRLEN: {
        REQUIRE_ARGC(argc, 1, 1, "string-length");
        CHECK_STRING(argv[0], "string-length");
        char *s = GET_STRING_PTR(argv[0]);
        return store(strlen(s));
    }
    case PSTRREF: {
        REQUIRE_ARGC(argc, 2, 2, "string-ref");
        CHECK_STRING(argv[0], "string-ref");
        char *s = GET_STRING_PTR(argv[0]);
        size_t len = strlen(s);
        int64_t idx;
        if (!expect_nonneg_int64(argv[1], &idx, "string-ref"))
            return TOK_ERROR;
        if (idx < 0 || (size_t)idx >= len) {
            show_error("string-ref: index out of bounds");
            return TOK_ERROR;
        }
        return make_char(s[idx]);
    }
    case PSTRSET: {
        REQUIRE_ARGC(argc, 3, 3, "string-set!");
        CHECK_STRING(argv[0], "string-set!");
        char *s = GET_STRING_PTR(argv[0]);
        size_t len = strlen(s);
        int64_t idx;
        if (!expect_nonneg_int64(argv[1], &idx, "string-set!"))
            return TOK_ERROR;
        CHECK_CHAR(argv[2], "string-set!");
        char c = (char)CELL_ID(argv[2]);
        if (idx < 0 || (size_t)idx >= len) {
            show_error("string-set!: index out of bounds");
            return TOK_ERROR;
        }
        s[idx] = c;
        return 0;
    }
    case PSTRAPP:
        return prim_string_append(argc, argv);
    case PSUBSTR:
        return prim_substring(argc, argv);
    case PSTR2SYM: {
        REQUIRE_ARGC(argc, 1, 1, "string->symbol");
        CHECK_STRING(argv[0], "string->symbol");
        char *s = GET_STRING_PTR(argv[0]);
        return atom_from_string(s);
    }
    case PSYM2STR: {
        REQUIRE_ARGC(argc, 1, 1, "symbol->string");
        CHECK_SYMBOL(argv[0], "symbol->string");
        const char *s = ctx.atom_table[CELL_ID(argv[0])];
        return make_string_copy(s);
    }
    case PNUM2STR: {
        REQUIRE_ARGC(argc, 1, 2, "number->string");
        unsigned num = argv[0];
        int radix = 10;
        if (argc > 1) {
            int64_t radix64;
            if (!expect_exact_int64(argv[1], &radix64, "number->string"))
                return TOK_ERROR;
            radix = (int)radix64;
        }
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
                uint64_t magnitude = neg ? -(uint64_t)n : (uint64_t)n;
                if (magnitude == 0) {
                    *--p = '0';
                } else {
                    while (magnitude > 0) {
                        int d = (int)(magnitude % (uint64_t)radix);
                        *--p = (d < 10) ? '0' + d : 'a' + d - 10;
                        magnitude /= (uint64_t)radix;
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
        REQUIRE_ARGC(argc, 1, 2, "string->number");
        CHECK_STRING(argv[0], "string->number");
        char *s = GET_STRING_PTR(argv[0]);
        int radix = 10;
        if (argc > 1) {
            int64_t radix64;
            if (!expect_exact_int64(argv[1], &radix64, "string->number"))
                return TOK_ERROR;
            radix = (int)radix64;
        }
        if (radix < 2 || radix > 36) {
            show_error("string->number: radix must be between 2 and 36");
            return TOK_ERROR;
        }
        if (radix == 10) {
            // Use standard parsing which handles floats
            unsigned parsed = atom_from_string(s);
            enum lisp_type t = CELL_TYPE(parsed);
            if (t == BT_NUM || t == BT_BIGNUM || t == BT_RATIONAL ||
                t == BT_INEXACT || t == BT_COMPLEX) {
                return parsed;
            }
            return ctx.atom_false;
        }
        // Parse integer in specified radix
        bignum *bn = bn_from_string(s, radix);
        if (!bn) {
            return ctx.atom_false; // Return #f for invalid number
        }
        return store_integer(bn);
    }
    case PMAKESTR: {
        REQUIRE_ARGC(argc, 1, 2, "make-string");
        int64_t len;
        if (!expect_nonneg_int64(argv[0], &len, "make-string"))
            return TOK_ERROR;
        if ((uint64_t)len > SIZE_MAX - 1) {
            show_error("make-string: length too large");
            return TOK_ERROR;
        }
        char fill = ' ';
        if (argc > 1) {
            CHECK_CHAR(argv[1], "make-string");
            fill = (char)CELL_ID(argv[1]);
        }
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
        REQUIRE_ARGC(argc, 1, 1, "string-copy");
        CHECK_STRING(argv[0], "string-copy");
        char *s = GET_STRING_PTR(argv[0]);
        return make_string_copy(s);
    }
    case PSTR2LIST: {
        REQUIRE_ARGC(argc, 1, 1, "string->list");
        CHECK_STRING(argv[0], "string->list");
        char *s = GET_STRING_PTR(argv[0]);
        size_t len = strlen(s);
        GC_GUARD;
        unsigned result = 0;
        gc_protect(&result);
        for (size_t i = len; i > 0; i--) {
            result = alloc_cons(make_char(s[i - 1]), result);
        }
        return result;
    }
    case PLIST2STR: {
        REQUIRE_ARGC(argc, 1, 1, "list->string");
        unsigned lst = argv[0];
        size_t len = 0;
        for (unsigned it = lst; it; it = cdr(it)) {
            if (!IS_PAIR(it)) {
                show_error("list->string: improper list");
                return TOK_ERROR;
            }
            if (!IS_CHAR(car(it))) {
                show_error("list->string: list elements must be characters");
                return TOK_ERROR;
            }
            if (len == SIZE_MAX) {
                show_error("list->string: result too large");
                return TOK_ERROR;
            }
            len++;
        }
        if (len == SIZE_MAX) {
            show_error("list->string: result too large");
            return TOK_ERROR;
        }
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
        REQUIRE_ARGC(argc, 2, 2, "string-fill!");
        CHECK_STRING(argv[0], "string-fill!");
        char *s = GET_STRING_PTR(argv[0]);
        CHECK_CHAR(argv[1], "string-fill!");
        int c = (unsigned char)CELL_ID(argv[1]);
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
        return string_compare(argc, argv, (cmp_op)(offset % 5), offset >= 5);
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
        return apply_char_primitive(prim_id, argc, argv);

    // Vector operations - delegated to prim_vector.c
    case PMAKEVEC:
    case PVECTOR:
    case PVECREF:
    case PVECSET:
    case PVECLEN:
    case PVECFILL:
    case PLIST2VEC:
    case PVEC2LIST:
        return apply_vector_primitive(prim_id, argc, argv);

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
    case PRANDOMINTEGER:
    case PRANDOMREAL:
    case PRANDOMSEED:
        return apply_math_primitive(prim_id, argc, argv);

    // Misc
    case PERROR: {
        fprintf(stderr, "error: ");
        for (unsigned i = 0; i < argc; i++) {
            display_obj(argv[i]);
            if (i + 1 < argc)
                fprintf(stderr, " ");
        }
        fprintf(stderr, "\n");
        return TOK_ERROR;
    }
    case PGENSYM: {
        REQUIRE_ARGC(argc, 0, 0, "gensym");
        char buf[32];
        snprintf(buf, sizeof(buf), "g%u", gensym_counter++);
        return atom_from_string(buf);
    }
    case PGCSTATS: {
        REQUIRE_ARGC(argc, 0, 0, "gc-stats");
        GC_GUARD;
        // Return ((minor-gc . count) (major-gc . count) ...)
        unsigned minor = store(ctx.minor_gc_count);
        gc_protect(&minor);
        unsigned major = store(ctx.major_gc_count);
        gc_protect(&major);
        unsigned heap_used = store(ctx.hptr - ctx.mmin);
        gc_protect(&heap_used);
        unsigned nursery_used = store(ctx.nursery_ptr - ctx.nursery_start);
        gc_protect(&nursery_used);
        unsigned result = 0;
        gc_protect(&result);

        unsigned key = atom_from_string("nursery");
        gc_protect(&key);
        unsigned entry = alloc_cons(key, nursery_used);
        gc_protect(&entry);
        result = alloc_cons(entry, result);
        gc_unprotect(2);

        key = atom_from_string("old-gen");
        gc_protect(&key);
        entry = alloc_cons(key, heap_used);
        gc_protect(&entry);
        result = alloc_cons(entry, result);
        gc_unprotect(2);

        key = atom_from_string("major-gc");
        gc_protect(&key);
        entry = alloc_cons(key, major);
        gc_protect(&entry);
        result = alloc_cons(entry, result);
        gc_unprotect(2);

        key = atom_from_string("minor-gc");
        gc_protect(&key);
        entry = alloc_cons(key, minor);
        gc_protect(&entry);
        result = alloc_cons(entry, result);
        gc_unprotect(2);

        return result;
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
        return apply_numtower_primitive(prim_id, argc, argv);

    // String constructor
    case PSTRING: {
        // (string char ...) - construct string from characters
        if (argc == UINT_MAX) {
            show_error("string: result too large");
            return TOK_ERROR;
        }
        char *s = malloc(argc + 1);
        if (!s) {
            show_error("string: out of memory");
            return TOK_ERROR;
        }
        for (unsigned i = 0; i < argc; i++) {
            if (!IS_CHAR(argv[i])) {
                free(s);
                show_error("string: argument is not a character");
                return TOK_ERROR;
            }
            s[i] = (char)CELL_ID(argv[i]);
        }
        s[argc] = '\0';
        return make_string_owned(s);
    }

    // Transcript
    case PTRANSCRIPTON: {
        REQUIRE_ARGC(argc, 1, 1, "transcript-on");
        CHECK_STRING(argv[0], "transcript-on");
        if (ctx.transcript) {
            show_error("transcript-on: transcript already active");
            return TOK_ERROR;
        }
        char *filename = GET_STRING_PTR(argv[0]);
        ctx.transcript = fopen(filename, "w");
        if (!ctx.transcript) {
            show_error("transcript-on: cannot open %s", filename);
            return TOK_ERROR;
        }
        return 0;
    }
    case PTRANSCRIPTOFF: {
        REQUIRE_ARGC(argc, 0, 0, "transcript-off");
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
        if (argc == 0)
            return 0; // No values = unspecified
        if (argc == 1)
            return argv[0]; // Single value
        // Multiple values - wrap in BT_MULTIVAL
        GC_GUARD;
        unsigned values = 0;
        gc_protect(&values);
        for (unsigned i = argc; i > 0; i--) {
            values = alloc_cons(argv[i - 1], values);
        }
        unsigned mv = alloc();
        CELL_TYPE(mv) = BT_MULTIVAL;
        CELL_CAR(mv) = values;
        CELL_CDR(mv) = 0;
        return mv;
    }

    // R5RS environment procedures
    case PSCHEMEENV: {
        REQUIRE_ARGC(argc, 1, 1, "scheme-report-environment");
        int64_t version = CELL_ID(argv[0]);
        if (version != 5) {
            show_error("scheme-report-environment: unsupported version %lld",
                       (long long)version);
            return TOK_ERROR;
        }
        return default_environment();
    }
    case PNULLENV: {
        REQUIRE_ARGC(argc, 1, 1, "null-environment");
        int64_t version = CELL_ID(argv[0]);
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

    // ========================================================================
    // Bitwise operations
    // ========================================================================
    case PBITWISEAND: {
        REQUIRE_ARGC(argc, 1, 999, "bitwise-and");
        int64_t result;
        if (!expect_exact_int64(argv[0], &result, "bitwise-and"))
            return TOK_ERROR;
        for (unsigned i = 1; i < argc; i++) {
            int64_t b;
            if (!expect_exact_int64(argv[i], &b, "bitwise-and"))
                return TOK_ERROR;
            result &= b;
        }
        return store(result);
    }
    case PBITWISEIOR: {
        REQUIRE_ARGC(argc, 1, 999, "bitwise-ior");
        int64_t result;
        if (!expect_exact_int64(argv[0], &result, "bitwise-ior"))
            return TOK_ERROR;
        for (unsigned i = 1; i < argc; i++) {
            int64_t b;
            if (!expect_exact_int64(argv[i], &b, "bitwise-ior"))
                return TOK_ERROR;
            result |= b;
        }
        return store(result);
    }
    case PBITWISEXOR: {
        REQUIRE_ARGC(argc, 1, 999, "bitwise-xor");
        int64_t result;
        if (!expect_exact_int64(argv[0], &result, "bitwise-xor"))
            return TOK_ERROR;
        for (unsigned i = 1; i < argc; i++) {
            int64_t b;
            if (!expect_exact_int64(argv[i], &b, "bitwise-xor"))
                return TOK_ERROR;
            result ^= b;
        }
        return store(result);
    }
    case PBITWISENOT: {
        REQUIRE_ARGC(argc, 1, 1, "bitwise-not");
        int64_t a;
        if (!expect_exact_int64(argv[0], &a, "bitwise-not"))
            return TOK_ERROR;
        return store(~a);
    }
    case PARITHSHIFT: {
        REQUIRE_ARGC(argc, 2, 2, "arithmetic-shift");
        int64_t val, count;
        if (!expect_exact_int64(argv[0], &val, "arithmetic-shift") ||
            !expect_exact_int64(argv[1], &count, "arithmetic-shift"))
            return TOK_ERROR;
        if (count >= 0) {
            if ((uint64_t)count > SIZE_MAX)
                ERROR_RETURN("arithmetic-shift: count too large");
            bignum *bn = bn_from_int(val);
            bignum *shifted = bn ? bn_lshift(bn, (size_t)count) : NULL;
            bn_free(bn);
            if (!shifted)
                ERROR_RETURN("arithmetic-shift: out of memory");
            return store_integer(shifted);
        } else {
            uint64_t shift = -(uint64_t)count;
            if (shift >= 63)
                return store(val < 0 ? -1 : 0);
            return store(val >> shift);
        }
    }

    // ========================================================================
    // Bytevector operations
    // ========================================================================
    case PMAKEBYTEVEC: {
        REQUIRE_ARGC(argc, 1, 2, "make-bytevector");
        int64_t len;
        if (!expect_nonneg_int64(argv[0], &len, "make-bytevector"))
            return TOK_ERROR;
        if ((uint64_t)len > UINT_MAX ||
            (uint64_t)len > SIZE_MAX - sizeof(bytevec_data)) {
            show_error("make-bytevector: length too large");
            return TOK_ERROR;
        }
        uint8_t fill = 0;
        if (argc > 1) {
            int64_t f;
            if (!expect_u8(argv[1], &f, "make-bytevector"))
                return TOK_ERROR;
            fill = (uint8_t)f;
        }
        bytevec_data *bv = malloc(sizeof(bytevec_data) + (size_t)len);
        if (!bv) {
            show_error("make-bytevector: out of memory");
            return TOK_ERROR;
        }
        bv->len = (unsigned)len;
        memset(bv->data, fill, (size_t)len);
        unsigned cell = alloc();
        CELL_TYPE(cell) = BT_BYTEVEC;
        CELL_PTR(cell) = bv;
        return cell;
    }
    case PBYTEVECREF: {
        REQUIRE_ARGC(argc, 2, 2, "bytevector-u8-ref");
        if (CELL_TYPE(argv[0]) != BT_BYTEVEC) {
            show_error("bytevector-u8-ref: not a bytevector");
            return TOK_ERROR;
        }
        bytevec_data *bv = (bytevec_data *)CELL_PTR(argv[0]);
        int64_t idx;
        if (!expect_nonneg_int64(argv[1], &idx, "bytevector-u8-ref"))
            return TOK_ERROR;
        if ((uint64_t)idx >= bv->len) {
            show_error("bytevector-u8-ref: index out of bounds");
            return TOK_ERROR;
        }
        return store(bv->data[idx]);
    }
    case PBYTEVECSET: {
        REQUIRE_ARGC(argc, 3, 3, "bytevector-u8-set!");
        if (CELL_TYPE(argv[0]) != BT_BYTEVEC) {
            show_error("bytevector-u8-set!: not a bytevector");
            return TOK_ERROR;
        }
        bytevec_data *bv = (bytevec_data *)CELL_PTR(argv[0]);
        int64_t idx, val;
        if (!expect_nonneg_int64(argv[1], &idx, "bytevector-u8-set!") ||
            !expect_u8(argv[2], &val, "bytevector-u8-set!"))
            return TOK_ERROR;
        if ((uint64_t)idx >= bv->len) {
            show_error("bytevector-u8-set!: index out of bounds");
            return TOK_ERROR;
        }
        bv->data[idx] = (uint8_t)val;
        return 0;
    }
    case PBYTEVECLEN: {
        REQUIRE_ARGC(argc, 1, 1, "bytevector-length");
        if (CELL_TYPE(argv[0]) != BT_BYTEVEC) {
            show_error("bytevector-length: not a bytevector");
            return TOK_ERROR;
        }
        return store(((bytevec_data *)CELL_PTR(argv[0]))->len);
    }
    case PBYTEVECUP: {
        REQUIRE_ARGC(argc, 1, 1, "bytevector?");
        return (argv[0] && CELL_TYPE(argv[0]) == BT_BYTEVEC)
                   ? ctx.atom_true : ctx.atom_false;
    }
    case PBYTEVEC: {
        // (bytevector b1 b2 ...) - construct from byte values
        if ((size_t)argc > SIZE_MAX - sizeof(bytevec_data)) {
            show_error("bytevector: result too large");
            return TOK_ERROR;
        }
        bytevec_data *bv = malloc(sizeof(bytevec_data) + argc);
        if (!bv) {
            show_error("bytevector: out of memory");
            return TOK_ERROR;
        }
        bv->len = argc;
        for (unsigned i = 0; i < argc; i++) {
            int64_t val;
            if (!expect_u8(argv[i], &val, "bytevector")) {
                free(bv);
                return TOK_ERROR;
            }
            bv->data[i] = (uint8_t)val;
        }
        unsigned cell = alloc();
        CELL_TYPE(cell) = BT_BYTEVEC;
        CELL_PTR(cell) = bv;
        return cell;
    }
    case PBYTEVECCOPY: {
        REQUIRE_ARGC(argc, 1, 3, "bytevector-copy");
        if (CELL_TYPE(argv[0]) != BT_BYTEVEC) {
            show_error("bytevector-copy: not a bytevector");
            return TOK_ERROR;
        }
        bytevec_data *src = (bytevec_data *)CELL_PTR(argv[0]);
        int64_t start = 0, end = src->len;
        if (argc > 1 &&
            !expect_nonneg_int64(argv[1], &start, "bytevector-copy"))
            return TOK_ERROR;
        if (argc > 2 &&
            !expect_nonneg_int64(argv[2], &end, "bytevector-copy"))
            return TOK_ERROR;
        if (start > end || (uint64_t)end > src->len) {
            show_error("bytevector-copy: invalid range");
            return TOK_ERROR;
        }
        unsigned len = (unsigned)(end - start);
        if ((size_t)len > SIZE_MAX - sizeof(bytevec_data)) {
            show_error("bytevector-copy: result too large");
            return TOK_ERROR;
        }
        bytevec_data *bv = malloc(sizeof(bytevec_data) + len);
        if (!bv) {
            show_error("bytevector-copy: out of memory");
            return TOK_ERROR;
        }
        bv->len = len;
        memcpy(bv->data, src->data + start, len);
        unsigned cell = alloc();
        CELL_TYPE(cell) = BT_BYTEVEC;
        CELL_PTR(cell) = bv;
        return cell;
    }
    case PBYTEVECCOPYTO: {
        REQUIRE_ARGC(argc, 3, 5, "bytevector-copy!");
        if (CELL_TYPE(argv[0]) != BT_BYTEVEC ||
            CELL_TYPE(argv[2]) != BT_BYTEVEC) {
            show_error("bytevector-copy!: not a bytevector");
            return TOK_ERROR;
        }
        bytevec_data *dst = (bytevec_data *)CELL_PTR(argv[0]);
        bytevec_data *src = (bytevec_data *)CELL_PTR(argv[2]);
        int64_t at, start = 0, end = src->len;
        if (!expect_nonneg_int64(argv[1], &at, "bytevector-copy!"))
            return TOK_ERROR;
        if (argc > 3 &&
            !expect_nonneg_int64(argv[3], &start, "bytevector-copy!"))
            return TOK_ERROR;
        if (argc > 4 &&
            !expect_nonneg_int64(argv[4], &end, "bytevector-copy!"))
            return TOK_ERROR;
        if (start > end) {
            show_error("bytevector-copy!: invalid range");
            return TOK_ERROR;
        }
        unsigned len = (unsigned)(end - start);
        if ((uint64_t)at + len > dst->len || (uint64_t)end > src->len) {
            show_error("bytevector-copy!: out of bounds");
            return TOK_ERROR;
        }
        memmove(dst->data + at, src->data + start, len);
        return 0;
    }
    case PBYTEVECAPPEND: {
        // (bytevector-append bv1 bv2 ...)
        unsigned total = 0;
        for (unsigned i = 0; i < argc; i++) {
            if (CELL_TYPE(argv[i]) != BT_BYTEVEC) {
                show_error("bytevector-append: not a bytevector");
                return TOK_ERROR;
            }
            bytevec_data *src = (bytevec_data *)CELL_PTR(argv[i]);
            if (src->len > UINT_MAX - total) {
                show_error("bytevector-append: result too large");
                return TOK_ERROR;
            }
            total += src->len;
        }
        if ((size_t)total > SIZE_MAX - sizeof(bytevec_data)) {
            show_error("bytevector-append: result too large");
            return TOK_ERROR;
        }
        bytevec_data *bv = malloc(sizeof(bytevec_data) + total);
        if (!bv) {
            show_error("bytevector-append: out of memory");
            return TOK_ERROR;
        }
        bv->len = total;
        unsigned pos = 0;
        for (unsigned i = 0; i < argc; i++) {
            bytevec_data *src = (bytevec_data *)CELL_PTR(argv[i]);
            memcpy(bv->data + pos, src->data, src->len);
            pos += src->len;
        }
        unsigned cell = alloc();
        CELL_TYPE(cell) = BT_BYTEVEC;
        CELL_PTR(cell) = bv;
        return cell;
    }

    // ========================================================================
    // Misc: command-line, write-to-string, list-ref
    // ========================================================================
    case PCOMMANDLINE: {
        REQUIRE_ARGC(argc, 0, 0, "command-line");
        extern int saved_argc;
        extern char **saved_argv;
        unsigned result = 0;
        gc_protect(&result);
        for (int i = saved_argc - 1; i >= 0; i--) {
            unsigned s = make_string_copy(saved_argv[i]);
            gc_protect(&s);
            result = alloc_cons(s, result);
            gc_unprotect(1);
        }
        gc_unprotect(1);
        return result;
    }
    case PWRITETOSTRING: {
        REQUIRE_ARGC(argc, 1, 1, "write-to-string");
        char *buf = NULL;
        size_t buf_len = 0;
        FILE *mem = open_memstream(&buf, &buf_len);
        if (!mem) { show_error("write-to-string: out of memory"); return TOK_ERROR; }
        write_obj_port(argv[0], mem);
        fclose(mem);
        unsigned result = make_string_copy(buf);
        free(buf);
        return result;
    }
    case PLISTREF: {
        REQUIRE_ARGC(argc, 2, 2, "list-ref");
        int64_t idx;
        if (!expect_nonneg_int64(argv[1], &idx, "list-ref"))
            return TOK_ERROR;
        unsigned lst = argv[0];
        for (int64_t i = 0; i < idx; i++) {
            if (!IS_PAIR(lst)) {
                show_error("list-ref: index out of bounds"); return TOK_ERROR;
            }
            lst = cdr(lst);
        }
        if (!IS_PAIR(lst)) {
            show_error("list-ref: index out of bounds"); return TOK_ERROR;
        }
        return car(lst);
    }

    // ========================================================================
    // Binary I/O
    // ========================================================================
    case POPENBINARYINPUT: {
        REQUIRE_ARGC(argc, 1, 1, "open-binary-input-file");
        CHECK_STRING(argv[0], "open-binary-input-file");
        const char *fname = GET_STRING_PTR(argv[0]);
        FILE *f = fopen(fname, "rb");
        if (!f) {
            show_error("open-binary-input-file: cannot open %s", fname);
            return TOK_ERROR;
        }
        unsigned cell = alloc();
        CELL_TYPE(cell) = BT_INPORT;
        CELL_PTR(cell) = f;
        return cell;
    }
    case PREADBYTEVEC: {
        REQUIRE_ARGC(argc, 2, 2, "read-bytevector");
        int64_t count;
        if (!expect_nonneg_int64(argv[0], &count, "read-bytevector"))
            return TOK_ERROR;
        if (!IS_INPORT(argv[1])) {
            show_error("read-bytevector: not an input port");
            return TOK_ERROR;
        }
        if ((uint64_t)count > UINT_MAX) {
            show_error("read-bytevector: count too large");
            return TOK_ERROR;
        }
        if ((uint64_t)count > SIZE_MAX - sizeof(bytevec_data)) {
            show_error("read-bytevector: count too large");
            return TOK_ERROR;
        }
        if (count == 0) {
            bytevec_data *bv = malloc(sizeof(bytevec_data));
            if (!bv) {
                show_error("read-bytevector: out of memory");
                return TOK_ERROR;
            }
            bv->len = 0;
            unsigned cell = alloc();
            CELL_TYPE(cell) = BT_BYTEVEC;
            CELL_PTR(cell) = bv;
            return cell;
        }
        FILE *f = GET_PORT_PTR(argv[1]);
        uint8_t *buf = malloc((size_t)count);
        if (!buf) {
            show_error("read-bytevector: out of memory");
            return TOK_ERROR;
        }
        size_t n = fread(buf, 1, (size_t)count, f);
        if (n == 0) {
            free(buf);
            // Return eof-object
            return atom_from_string("eof-object");
        }
        bytevec_data *bv = malloc(sizeof(bytevec_data) + n);
        if (!bv) {
            free(buf);
            show_error("read-bytevector: out of memory");
            return TOK_ERROR;
        }
        bv->len = (unsigned)n;
        memcpy(bv->data, buf, n);
        free(buf);
        unsigned cell = alloc();
        CELL_TYPE(cell) = BT_BYTEVEC;
        CELL_PTR(cell) = bv;
        return cell;
    }
    case PFILEEXISTS: {
        REQUIRE_ARGC(argc, 1, 1, "file-exists?");
        CHECK_STRING(argv[0], "file-exists?");
        FILE *f = fopen(GET_STRING_PTR(argv[0]), "r");
        if (f) { fclose(f); return ctx.atom_true; }
        return ctx.atom_false;
    }

    default:
        show_error("unknown primitive: %u", prim_id);
        return TOK_ERROR;
    }
}

unsigned apply_primitive(unsigned prim_id, unsigned args)
{
    unsigned argc = 0;
    if (!list_length_checked(args, &argc, "primitive"))
        return TOK_ERROR;

    unsigned argv_stack[8];
    unsigned *argv = argv_stack;
    if (argc > sizeof(argv_stack) / sizeof(argv_stack[0])) {
        argv = malloc_array(argc, sizeof(*argv));
        if (!argv) {
            show_error("primitive: out of memory");
            return TOK_ERROR;
        }
    }

    unsigned i = 0;
    for (unsigned it = args; it; it = cdr(it))
        argv[i++] = car(it);

    GC_GUARD;
    for (i = 0; i < argc; i++)
        gc_protect(&argv[i]);
    unsigned result = apply_primitive_argv(prim_id, argc, argv);

    if (argv != argv_stack)
        free(argv);
    return result;
}
