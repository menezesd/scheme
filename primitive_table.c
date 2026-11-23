#include "primitive_table.h"

// One entry per primitive, indexed by its enum primitive_id:
//
//     [PID] = PRIM("name", min_args, max_args, flags),
//     [PID] = PRIM_AS("%name", min_args, max_args, flags, "error-name"),
//
// To add a primitive: append its id to enum primitive_id (types.h) just
// before PRIM_COUNT, append its entry at the end of this table, and add its
// case to dispatch_primitive (primitives.c). apply_primitive_argv checks the
// argument count against the entry before the case runs, reporting
// "NAME: too few/many arguments ...", so the case needs no REQUIRE_ARGC and
// may index argv below min_args freely. ANY as max_args means no upper
// bound. PRIM_AS gives the name errors use when it is not the bound name.
// Flags (see primitive_table.h), or 0 for none:
//   NO_ALLOC   never allocates; argv is not rooted
//   PURE       no side effects, never calls back into Scheme
//   FOLD       PURE and never returns a fresh mutable object: foldable
//   SPECIAL    applied by the engines themselves (apply, call/cc, eval, ...)
//   OWN_ARITY  reports a wrong argument count itself, with its own message
//   BOOTSTRAP  bound only while the stdlib loads, not in default_environment
// test_context checks that every id has a well-formed entry.

#define PRIM(name, min, max, flags) {name, min, max, flags, NULL}
#define PRIM_AS(name, min, max, flags, who) {name, min, max, flags, who}

#define ANY PRIM_ARGS_ANY
#define NO_ALLOC PRIM_NO_ALLOC
#define PURE PRIM_PURE
#define FOLD (PRIM_PURE | PRIM_FOLDABLE)
#define SPECIAL PRIM_SPECIAL
#define OWN_ARITY PRIM_OWN_ARITY
#define BOOTSTRAP PRIM_BOOTSTRAP

const primitive_info primitive_table[PRIM_COUNT] = {
    // Arithmetic
    [PPLUS] = PRIM("+", 0, ANY, FOLD),
    [PMINUS] = PRIM("-", 1, ANY, FOLD | OWN_ARITY),
    [PTIMES] = PRIM("*", 0, ANY, FOLD),
    [PDIV] = PRIM("/", 1, ANY, FOLD | OWN_ARITY),
    [PMOD] = PRIM("modulo", 2, 2, FOLD),
    [PREMAINDER] = PRIM("remainder", 2, 2, FOLD),
    [PTRUNCATEDIVREM] = PRIM("truncate/", 2, 2, 0),
    [PFLOORDIVREM] = PRIM("floor/", 2, 2, 0),
    [PQUOTIENT] = PRIM("quotient", 2, 2, FOLD),
    [PABS] = PRIM("abs", 1, 1, FOLD),

    // Comparison
    [PEQUAL] = PRIM("=", 0, ANY, FOLD),
    [PLT] = PRIM("<", 0, ANY, FOLD),
    [PGT] = PRIM(">", 0, ANY, FOLD),
    [PLEQ] = PRIM("<=", 0, ANY, FOLD),
    [PGEQ] = PRIM(">=", 0, ANY, FOLD),

    // Logic
    [PNOT] = PRIM("not", 1, 1, NO_ALLOC | FOLD),
    [PEQ] = PRIM("eq?", 2, 2, NO_ALLOC | FOLD),
    [PEQV] = PRIM("eqv?", 2, 2, NO_ALLOC | FOLD),
    [PEQUALP] = PRIM("equal?", 2, 2, NO_ALLOC | FOLD),

    // List operations
    [PCONS] = PRIM("cons", 2, 2, PURE),
    [PCAR] = PRIM("car", 1, 1, NO_ALLOC | FOLD),
    [PCDR] = PRIM("cdr", 1, 1, NO_ALLOC | FOLD),
    [PCAAR] = PRIM("caar", 1, 1, NO_ALLOC),
    [PCADR] = PRIM("cadr", 1, 1, NO_ALLOC),
    [PCDAR] = PRIM("cdar", 1, 1, NO_ALLOC),
    [PCDDR] = PRIM("cddr", 1, 1, NO_ALLOC),
    [PCAAAR] = PRIM("caaar", 1, 1, NO_ALLOC),
    [PCAADR] = PRIM("caadr", 1, 1, NO_ALLOC),
    [PCADAR] = PRIM("cadar", 1, 1, NO_ALLOC),
    [PCADDR] = PRIM("caddr", 1, 1, NO_ALLOC),
    [PCDAAR] = PRIM("cdaar", 1, 1, NO_ALLOC),
    [PCDADR] = PRIM("cdadr", 1, 1, NO_ALLOC),
    [PCDDAR] = PRIM("cddar", 1, 1, NO_ALLOC),
    [PCDDDR] = PRIM("cdddr", 1, 1, NO_ALLOC),
    [PCAAAAR] = PRIM("caaaar", 1, 1, NO_ALLOC),
    [PCAAADR] = PRIM("caaadr", 1, 1, NO_ALLOC),
    [PCAADAR] = PRIM("caadar", 1, 1, NO_ALLOC),
    [PCAADDR] = PRIM("caaddr", 1, 1, NO_ALLOC),
    [PCADAAR] = PRIM("cadaar", 1, 1, NO_ALLOC),
    [PCADADR] = PRIM("cadadr", 1, 1, NO_ALLOC),
    [PCADDAR] = PRIM("caddar", 1, 1, NO_ALLOC),
    [PCADDDR] = PRIM("cadddr", 1, 1, NO_ALLOC),
    [PCDAAAR] = PRIM("cdaaar", 1, 1, NO_ALLOC),
    [PCDAADR] = PRIM("cdaadr", 1, 1, NO_ALLOC),
    [PCDADAR] = PRIM("cdadar", 1, 1, NO_ALLOC),
    [PCDADDR] = PRIM("cdaddr", 1, 1, NO_ALLOC),
    [PCDDAAR] = PRIM("cddaar", 1, 1, NO_ALLOC),
    [PCDDADR] = PRIM("cddadr", 1, 1, NO_ALLOC),
    [PCDDDAR] = PRIM("cdddar", 1, 1, NO_ALLOC),
    [PCDDDDR] = PRIM("cddddr", 1, 1, NO_ALLOC),
    [PSETCAR] = PRIM("set-car!", 2, 2, NO_ALLOC),
    [PSETCDR] = PRIM("set-cdr!", 2, 2, NO_ALLOC),
    [PLIST] = PRIM("list", 0, ANY, PURE),
    [PLENGTH] = PRIM("length", 1, 1, FOLD),
    [PAPPEND] = PRIM("append", 0, ANY, PURE),
    [PREVERSE] = PRIM("reverse", 1, 1, 0),
    [PLASTPAIR] = PRIM("last-pair", 1, 1, NO_ALLOC),
    [PLISTTAIL] = PRIM("list-tail", 2, 2, NO_ALLOC),
    [PMEMQ] = PRIM("memq", 2, 2, NO_ALLOC),
    [PMEMV] = PRIM("memv", 2, 2, NO_ALLOC),
    [PASSQ] = PRIM("assq", 2, 2, NO_ALLOC),
    [PASSV] = PRIM("assv", 2, 2, NO_ALLOC),
    // The equal? searches: stdlib's member and assoc take an optional
    // comparison procedure, and call these when it is omitted.
    [PMEMBER] = PRIM_AS("%member", 2, 2, NO_ALLOC, "member"),
    [PASSOC] = PRIM_AS("%assoc", 2, 2, NO_ALLOC, "assoc"),

    // Type predicates
    [PSYMP] = PRIM("symbol?", 1, 1, NO_ALLOC | FOLD),
    [PNUMP] = PRIM("number?", 1, 1, NO_ALLOC | FOLD),
    [PPROCP] = PRIM("procedure?", 1, 1, NO_ALLOC),
    [PCONSP] = PRIM("pair?", 1, 1, NO_ALLOC | FOLD),
    [PNULLP] = PRIM("null?", 1, 1, NO_ALLOC | FOLD),
    [PSTRINGP] = PRIM("string?", 1, 1, NO_ALLOC | FOLD),
    [PCHARP] = PRIM("char?", 1, 1, NO_ALLOC | FOLD),
    [PVECTORP] = PRIM("vector?", 1, 1, NO_ALLOC | FOLD),
    [PBOOLP] = PRIM("boolean?", 1, 1, NO_ALLOC | FOLD),
    [PLISTP] = PRIM("list?", 1, 1, NO_ALLOC),
    [PINTEGERP] = PRIM("integer?", 1, 1, NO_ALLOC),
    [PREALP] = PRIM("real?", 1, 1, NO_ALLOC),
    [PEXACTP] = PRIM("exact?", 1, 1, NO_ALLOC),
    [PINEXACTP] = PRIM("inexact?", 1, 1, NO_ALLOC),

    // I/O
    [PDISPLAY] = PRIM("display", 1, 2, 0),
    [PWRITE] = PRIM("write", 1, 2, 0),
    [PWRITESHARED] = PRIM("write-shared", 1, 2, 0),
    [PWRITESIMPLE] = PRIM("write-simple", 1, 2, 0),
    [PNEWLINE] = PRIM("newline", 0, 1, 0),
    [PREAD] = PRIM("read", 0, 1, 0),
    [PREADCHAR] = PRIM("read-char", 0, 1, 0),
    [PREADCHARNOHANG] = PRIM("read-char-no-hang", 0, 1, 0),
    [PUNREADCHAR] = PRIM("unread-char", 1, 2, 0),
    [PPEEKCHAR] = PRIM("peek-char", 0, 1, 0),
    [PWRITECHAR] = PRIM("write-char", 1, 2, 0),
    [PEOFOBJECT] = PRIM("eof-object", 0, 0, 0),
    [PEOF] = PRIM("eof-object?", 1, 1, 0),
    [PCHARREADY] = PRIM("char-ready?", 0, 1, 0),
    [PREADLINE] = PRIM("read-line", 0, 1, 0),
    [PLOAD] = PRIM("load", 1, 1, SPECIAL),

    // Ports
    [POPENINPUT] = PRIM("open-input-file", 1, 1, 0),
    [POPENOUTPUT] = PRIM("open-output-file", 1, 2, 0),
    [PCLOSEINPUT] = PRIM("close-input-port", 1, 1, 0),
    [PCLOSEOUTPUT] = PRIM("close-output-port", 1, 1, 0),
    [PINPUTPORTP] = PRIM("input-port?", 1, 1, 0),
    [POUTPUTPORTP] = PRIM("output-port?", 1, 1, 0),
    [PCURRENTINPUT] = PRIM("current-input-port", 0, 0, 0),
    [PCURRENTOUTPUT] = PRIM("current-output-port", 0, 0, 0),

    // String operations
    [PSTRLEN] = PRIM("string-length", 1, 1, FOLD),
    [PSTRREF] = PRIM("string-ref", 2, 2, FOLD),
    [PSTRSET] = PRIM("string-set!", 3, 3, 0),
    [PSTRAPP] = PRIM("string-append", 0, ANY, PURE),
    [PSUBSTR] = PRIM("substring", 1, 3, 0),
    [PSTRSLICE] = PRIM("string-slice", 1, 3, 0),
    [PSTR2SYM] = PRIM("string->symbol", 1, 1, FOLD),
    [PSYM2STR] = PRIM("symbol->string", 1, 1, 0),
    [PNUM2STR] = PRIM("number->string", 1, 2, 0),
    [PSTR2NUM] = PRIM("string->number", 1, 2, 0),
    [PMAKESTR] = PRIM("make-string", 1, 2, 0),
    [PSTRCOPY] = PRIM("string-copy", 1, 3, 0),
    [PSTR2LIST] = PRIM("string->list", 1, 3, 0),
    [PLIST2STR] = PRIM("list->string", 1, 1, 0),
    [PSTRFILL] = PRIM("string-fill!", 2, 4, 0),
    [PSTRNFD] = PRIM("string-normalize-nfd", 1, 1, 0),
    [PSTRNFC] = PRIM("string-normalize-nfc", 1, 1, 0),
    [PSTRNFKD] = PRIM("string-normalize-nfkd", 1, 1, 0),
    [PSTRNFKC] = PRIM("string-normalize-nfkc", 1, 1, 0),
    [PSTRFOLD] = PRIM("string-foldcase", 1, 1, 0),
    [PSTRUP] = PRIM("string-upcase", 1, 1, 0),
    [PSTRDOWN] = PRIM("string-downcase", 1, 1, 0),
    [PSTRTITLE] = PRIM("string-titlecase", 1, 1, 0),
    [PSTRINGTOUTF8] = PRIM("string->utf8", 1, 3, 0),
    [PUTF8TOSTRING] = PRIM("utf8->string", 1, 3, 0),

    // String comparisons
    [PSTREQ] = PRIM("string=?", 2, ANY, OWN_ARITY),
    [PSTRLT] = PRIM("string<?", 2, ANY, OWN_ARITY),
    [PSTRGT] = PRIM("string>?", 2, ANY, OWN_ARITY),
    [PSTRLE] = PRIM("string<=?", 2, ANY, OWN_ARITY),
    [PSTRGE] = PRIM("string>=?", 2, ANY, OWN_ARITY),
    [PSTREQI] = PRIM("string-ci=?", 2, ANY, OWN_ARITY),
    [PSTRLTI] = PRIM("string-ci<?", 2, ANY, OWN_ARITY),
    [PSTRGTI] = PRIM("string-ci>?", 2, ANY, OWN_ARITY),
    [PSTRLEI] = PRIM("string-ci<=?", 2, ANY, OWN_ARITY),
    [PSTRGEI] = PRIM("string-ci>=?", 2, ANY, OWN_ARITY),

    // Character operations
    [PCHARCODE] = PRIM("char->integer", 1, 1, FOLD),
    [PCODECHAR] = PRIM("integer->char", 1, 1, FOLD),
    [PCHARUP] = PRIM("char-upcase", 1, 1, FOLD),
    [PCHARDOWN] = PRIM("char-downcase", 1, 1, FOLD),
    [PCHARFOLD] = PRIM("char-foldcase", 1, 1, 0),

    // Character comparisons
    [PCHAREQ] = PRIM("char=?", 2, ANY, OWN_ARITY),
    [PCHARLT] = PRIM("char<?", 2, ANY, OWN_ARITY),
    [PCHARGT] = PRIM("char>?", 2, ANY, OWN_ARITY),
    [PCHARLE] = PRIM("char<=?", 2, ANY, OWN_ARITY),
    [PCHARGE] = PRIM("char>=?", 2, ANY, OWN_ARITY),
    [PCHAREQI] = PRIM("char-ci=?", 2, ANY, OWN_ARITY),
    [PCHARLTI] = PRIM("char-ci<?", 2, ANY, OWN_ARITY),
    [PCHARGTI] = PRIM("char-ci>?", 2, ANY, OWN_ARITY),
    [PCHARLEI] = PRIM("char-ci<=?", 2, ANY, OWN_ARITY),
    [PCHARGEI] = PRIM("char-ci>=?", 2, ANY, OWN_ARITY),

    // Character predicates
    [PCHARALPHA] = PRIM("char-alphabetic?", 1, 1, 0),
    [PCHARNUMERIC] = PRIM("char-numeric?", 1, 1, 0),
    [PCHARWHITE] = PRIM("char-whitespace?", 1, 1, 0),
    [PCHARUPPER] = PRIM("char-upper-case?", 1, 1, 0),
    [PCHARLOWER] = PRIM("char-lower-case?", 1, 1, 0),

    // Vector operations
    [PMAKEVEC] = PRIM("make-vector", 1, 2, 0),
    [PVECTOR] = PRIM("vector", 0, ANY, 0),
    [PVECREF] = PRIM("vector-ref", 2, 2, 0),
    [PVECSET] = PRIM("vector-set!", 3, 3, 0),
    [PVECLEN] = PRIM("vector-length", 1, 1, 0),
    [PVECFILL] = PRIM("vector-fill!", 2, 4, 0),
    [PLIST2VEC] = PRIM("list->vector", 1, 1, 0),
    [PVEC2LIST] = PRIM("vector->list", 1, 3, 0),

    // Math functions
    [PSQRT] = PRIM("sqrt", 1, 1, FOLD),
    [PEXPT] = PRIM("expt", 2, 2, FOLD),
    [PSIN] = PRIM("sin", 1, 1, FOLD),
    [PCOS] = PRIM("cos", 1, 1, FOLD),
    [PTAN] = PRIM("tan", 1, 1, FOLD),
    [PASIN] = PRIM("asin", 1, 1, FOLD),
    [PACOS] = PRIM("acos", 1, 1, FOLD),
    [PATAN] = PRIM("atan", 1, 2, FOLD),
    [PLOG] = PRIM("log", 1, 2, FOLD),
    [PEXP] = PRIM("exp", 1, 1, FOLD),
    [PLOG1P] = PRIM("log1p", 1, 1, 0),
    [PEXPM1] = PRIM("expm1", 1, 1, 0),
    [PSQRT1PM1] = PRIM("sqrt1pm1", 1, 1, 0),
    [PLOG1PEXP] = PRIM("log1pexp", 1, 1, 0),
    [PFLOOR] = PRIM("floor", 1, 1, FOLD),
    [PCEILING] = PRIM("ceiling", 1, 1, FOLD),
    [PTRUNCATE] = PRIM("truncate", 1, 1, FOLD),
    [PROUND] = PRIM("round", 1, 1, FOLD),

    // Random numbers (SRFI-27)
    [PRANDOMINTEGER] = PRIM("random-integer", 1, 1, 0),
    [PRANDOMREAL] = PRIM("random-real", 0, 0, 0),
    [PRANDOMSEED] = PRIM("random-seed!", 1, 1, 0),

    // Process control (exit is a Scheme wrapper in stdlib.scm that runs
    // dynamic-wind after-thunks first; this is the raw primitive behind it)
    [PEXIT] = PRIM_AS("primitive-exit", 0, 1, 0, "exit"),

    // Bitwise operations
    [PBITWISEAND] = PRIM("bitwise-and", 0, ANY, 0),
    [PBITWISEIOR] = PRIM("bitwise-ior", 0, ANY, 0),
    [PBITWISENOT] = PRIM("bitwise-not", 1, 1, 0),
    [PBITWISEXOR] = PRIM("bitwise-xor", 0, ANY, 0),
    [PARITHSHIFT] = PRIM("arithmetic-shift", 2, 2, 0),

    // Bytevector operations
    [PMAKEBYTEVEC] = PRIM("make-bytevector", 1, 2, 0),
    [PBYTEVECREF] = PRIM("bytevector-u8-ref", 2, 2, 0),
    [PBYTEVECSET] = PRIM("bytevector-u8-set!", 3, 3, 0),
    [PBYTEVECLEN] = PRIM("bytevector-length", 1, 1, 0),
    [PBYTEVECCOPY] = PRIM("bytevector-copy", 1, 3, 0),
    [PBYTEVECCOPYTO] = PRIM("bytevector-copy!", 3, 5, 0),
    [PBYTEVECAPPEND] = PRIM("bytevector-append", 0, ANY, 0),
    [PBYTEVEC] = PRIM("bytevector", 0, ANY, 0),
    [PBYTEVECUP] = PRIM("bytevector?", 1, 1, 0),

    // Misc
    [PCOMMANDLINE] = PRIM("command-line", 0, 0, 0),
    [PWRITETOSTRING] = PRIM("write-to-string", 1, 1, 0),
    [PLISTREF] = PRIM("list-ref", 2, 2, NO_ALLOC),

    // Binary I/O
    [POPENBINARYINPUT] = PRIM("open-binary-input-file", 1, 1, 0),
    [POPENBINARYOUTPUT] = PRIM("open-binary-output-file", 1, 1, 0),
    [PREADBYTEVEC] = PRIM("read-bytevector", 2, 2, 0),
    [PREADBYTEVECINTO] = PRIM("read-bytevector!", 2, 4, 0),
    [PWRITEBYTEVEC] = PRIM("write-bytevector", 2, 2, 0),
    [PREADSTRING] = PRIM("read-string", 1, 2, 0),
    [PWRITESTRING] = PRIM("write-string", 1, 4, 0),
    [PU8READY] = PRIM("u8-ready?", 0, 1, 0),
    [PPEEKU8] = PRIM("peek-u8", 0, 1, 0),

    // File system
    [PFILEEXISTS] = PRIM("file-exists?", 1, 1, 0),
    [PFILEREGULARP] = PRIM("file-regular?", 1, 1, 0),
    [PFILEDIRECTORYP] = PRIM("file-directory?", 1, 1, 0),
    [PDELETEFILE] = PRIM("delete-file", 1, 1, 0),
    [PMAKEDIRECTORY] = PRIM("make-directory", 1, 1, 0),
    [PDELETEDIRECTORY] = PRIM("delete-directory", 1, 1, 0),
    [PRENAMEFILE] = PRIM("rename-file", 2, 2, 0),
    [PCURRENTDIRECTORY] = PRIM("current-directory", 0, 1, 0),
    [PDIRECTORYFILES] = PRIM("directory-files", 1, 1, 0),
    [PTEMPFILEPATH] = PRIM("temporary-file-path", 0, 0, 0),
    [PGETENV] = PRIM("get-environment-variable", 1, 1, 0),

    // Control
    [PAPPLY] = PRIM("apply", 2, ANY, SPECIAL),
    [PCALLCC] = PRIM("call/cc", 1, 1, SPECIAL),

    // Misc
    [PERROR] = PRIM("error", 0, ANY, 0),
    [PRAISENOW] = PRIM("raise-now", 1, 1, SPECIAL),
    [PGENSYM] = PRIM("gensym", 0, 0, 0),
    [PGCFLIP] = PRIM("gc-flip", 0, 0, 0),
    [PGCSTATS] = PRIM("gc-stats", 0, 0, 0),

    // R3RS numeric tower
    [PCOMPLEXP] = PRIM("complex?", 1, 1, NO_ALLOC),
    [PRATIONALP] = PRIM("rational?", 1, 1, NO_ALLOC),
    [PNUMERATOR] = PRIM("numerator", 1, 1, 0),
    [PDENOMINATOR] = PRIM("denominator", 1, 1, 0),
    [PMAKERECT] = PRIM("make-rectangular", 2, 2, 0),
    [PMAKEPOLAR] = PRIM("make-polar", 2, 2, 0),
    [PREALPART] = PRIM("real-part", 1, 1, 0),
    [PIMAGPART] = PRIM("imag-part", 1, 1, 0),
    [PMAGNITUDE] = PRIM("magnitude", 1, 1, 0),
    [PANGLE] = PRIM("angle", 1, 1, 0),
    [PEXACT2INEXACT] = PRIM("exact->inexact", 1, 1, 0),
    [PINEXACT2EXACT] = PRIM("inexact->exact", 1, 1, 0),
    [PRATIONALIZE] = PRIM("rationalize", 2, 2, 0),
    [PFINITE] = PRIM("finite?", 1, 1, 0),
    [PINFINITE] = PRIM("infinite?", 1, 1, 0),
    [PNAN] = PRIM("nan?", 1, 1, 0),

    // String constructor
    [PSTRING] = PRIM("string", 0, ANY, 0),

    // Transcript
    [PTRANSCRIPTON] = PRIM("transcript-on", 1, 1, 0),
    [PTRANSCRIPTOFF] = PRIM("transcript-off", 0, 0, 0),

    // R5RS multiple values
    [PVALUES] = PRIM("values", 0, ANY, 0),
    [PCALLWITHVALUES] = PRIM("call-with-values", 2, 2, SPECIAL),

    // R5RS eval and environments
    [PEVAL] = PRIM("eval", 2, 2, SPECIAL),
    [PSCHEMEENV] = PRIM("scheme-report-environment", 1, 1, 0),
    [PNULLENV] = PRIM("null-environment", 1, 1, 0),
    [PR7RSENV] = PRIM("%r7rs-environment", 0, 1, 0),
    [PINTERACTIONENV] = PRIM("interaction-environment", 0, 0, SPECIAL),

    // String ports
    [POPENOUTPUTSTRING] = PRIM("open-output-string", 0, 0, 0),
    [PGETOUTPUTSTRING] = PRIM("get-output-string", 1, 1, 0),
    [POPENINPUTSTRING] = PRIM("open-input-string", 1, 3, 0),
    [PSTRINGPORTP] = PRIM("string-port?", 1, 1, 0),

    // Internal port setters
    [PSETCURRENTINPUT] = PRIM("set-current-input-port!", 1, 1, 0),
    [PSETCURRENTOUTPUT] = PRIM("set-current-output-port!", 1, 1, 0),
    [PSETCURRENTERROR] = PRIM("set-current-error-port!", 1, 1, 0),
    [PCURRENTERROR] = PRIM("current-error-port", 0, 0, 0),
    [PFLUSHOUTPUT] = PRIM("flush-output-port", 0, 1, 0),
    [PCURRENTSECOND] = PRIM("current-second", 0, 0, 0),
    [PCURRENTJIFFY] = PRIM("current-jiffy", 0, 0, 0),
    [PJIFFIESPERSECOND] = PRIM("jiffies-per-second", 0, 0, 0),
    [PGETENVS] = PRIM("get-environment-variables", 0, 0, 0),
    [PEMERGENCYEXIT] = PRIM("emergency-exit", 0, 1, 0),
    [PFEATURES] = PRIM("features", 0, 0, 0),

    // Hash tables
    [PMAKEHASHTABLE] = PRIM("make-hash-table", 0, 1, 0),
    [PMAKESTRONGEQHASHTABLE] = PRIM("make-strong-eq-hash-table", 0, 1, 0),
    [PMAKEEQHASHTABLE] = PRIM("make-eq-hash-table", 0, 1, 0),
    [PMAKESTRONGEQVHASHTABLE] = PRIM("make-strong-eqv-hash-table", 0, 1, 0),
    [PMAKEEQVHASHTABLE] = PRIM("make-eqv-hash-table", 0, 1, 0),
    [PMAKEEQUALHASHTABLE] = PRIM("make-equal-hash-table", 0, 1, 0),
    [PHASHTABLEP] = PRIM("hash-table?", 1, 1, 0),
    [PHASHTABLEREF] = PRIM("hash-table-ref", 2, 3, 0),
    [PHASHTABLESET] = PRIM("hash-table-set!", 3, 3, 0),
    [PHASHTABLEDELETE] = PRIM("hash-table-delete!", 2, 2, 0),
    [PHASHTABLEEXISTS] = PRIM("hash-table-exists?", 2, 2, 0),
    [PHASHTABLESIZE] = PRIM("hash-table-size", 1, 1, 0),
    [PHASHTABLECLEAR] = PRIM("hash-table-clear!", 1, 1, 0),
    [PHASHTABLEKEYS] = PRIM("hash-table-keys", 1, 1, 0),
    [PHASHTABLEVALUES] = PRIM("hash-table-values", 1, 1, 0),
    [PHASHTABLEALIST] = PRIM("hash-table->alist", 1, 1, 0),
    [PHASHTABLECOPY] = PRIM("hash-table-copy", 1, 1, 0),
    [PHASHTABLEEQUIV] = PRIM("hash-table-equivalence-function", 1, 1, 0),
    [PHASHTABLEHASH] = PRIM("%hash-table-hash", 2, 3, 0),
    [PHASH] = PRIM("hash", 1, 2, 0),
    [PHASHBYIDENTITY] = PRIM("hash-by-identity", 1, 2, 0),

    // Records: internal, wrapped by define-record-type and the MIT record
    // procedures in stdlib.scm
    [PMAKERECORDTYPE] =
        PRIM_AS("%make-record-type", 2, 3, 0, "make-record-type"),
    [PRECORDTYPEP] = PRIM_AS("%record-type?", 1, 1, 0, "record-type?"),
    [PRECORDTYPENAME] =
        PRIM_AS("%record-type-name", 1, 1, 0, "record-type-name"),
    [PRECORDTYPEFIELDNAMES] = PRIM_AS("%record-type-field-names", 1, 1, 0,
                                      "record-type-field-names"),
    [PRECORDTYPEFIELDINDEX] = PRIM("%record-type-field-index", 3, 3, 0),
    [PRECORDTYPESEALEDP] = PRIM("%record-type-sealed?", 1, 1, 0),
    [PRECORDLAYOUT] = PRIM("%record-layout", 3, 3, 0),
    [PMAKERECORD] = PRIM("%make-record", 2, ANY, OWN_ARITY),
    [PRECORDP] = PRIM_AS("%record?", 1, 1, 0, "record?"),
    [PRECORDOFP] = PRIM("%record-of?", 2, 2, 0),
    [PRECORDTYPEDESCRIPTOR] = PRIM_AS("%record-type-descriptor", 1, 1, 0,
                                      "record-type-descriptor"),
    [PRECORDREF] = PRIM("%record-ref", 4, 4, 0),
    [PRECORDSET] = PRIM("%record-set!", 5, 5, 0),
    [PREGISTERBUILTINRECORDTYPE] =
        PRIM("%register-builtin-record-type!", 2, 2, 0),

    // R7RS-ish port predicates
    [PPORTOPENP] = PRIM("port-open?", 1, 1, 0),
    [PINPUTPORTOPENP] = PRIM("input-port-open?", 1, 1, 0),
    [POUTPUTPORTOPENP] = PRIM("output-port-open?", 1, 1, 0),
    [PTEXTUALPORTP] = PRIM("textual-port?", 1, 1, 0),
    [PBINARYPORTP] = PRIM("binary-port?", 1, 1, 0),

    // Bound only while the standard library loads (main.c)
    [PUNICODEASSIGNED] = PRIM("%unicode-assigned?", 1, 1, BOOTSTRAP),
    [PCHARCOMBINING] = PRIM("%char-combining?", 1, 1, BOOTSTRAP),
    [PUNICODEPUNCTUATION] = PRIM("%unicode-punctuation?", 1, 1, BOOTSTRAP),
    [PUNICODESYMBOL] = PRIM("%unicode-symbol?", 1, 1, BOOTSTRAP),
    // MIT library support (stdlib.scm "MIT library procedures")
    [PRUNTIME] = PRIM("runtime", 0, 0, 0),
    [PCOMPOUNDPROCP] = PRIM("compound-procedure?", 1, 1, NO_ALLOC),

    // Introspection
    [PPROCARITY] = PRIM("procedure-arity", 1, 1, 0),
};

const primitive_alias primitive_aliases[] = {
    {"logp1", PLOG1P},
    {"call-with-current-continuation", PCALLCC},
    {"flush-output", PFLUSHOUTPUT},
    {NULL, 0},
};
