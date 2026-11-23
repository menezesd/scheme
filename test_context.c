// Unit tests for context and env modules
#define _POSIX_C_SOURCE 200809L
#include "bytecode.h"
#include "compile_internal.h"
#include "compiled_pattern.h"
#include "context.h"
#include "env.h"
#include "feature_table.h"
#include "prim_internal.h"
#include "primitive_table.h"
#include "primitives.h"
#include "reader.h"
#include "test_framework.h"
#include "types.h"
#include "writer.h"
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <limits.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

static unsigned test_gc_collector_from;
static unsigned test_gc_collector_to;

static unsigned test_gc_forward_one(unsigned value)
{
    return value == test_gc_collector_from ? test_gc_collector_to : value;
}

// ============================================================================
// Context Tests - Allocation
// ============================================================================

TEST(alloc_returns_valid_cell)
{
    unsigned x = alloc();
    ASSERT(x > 0);
    ASSERT(x < SEMISPACE_SIZE);
    PASS();
}

TEST(alloc_cells_are_distinct)
{
    unsigned x = alloc();
    unsigned y = alloc();
    unsigned z = alloc();
    ASSERT(x != y);
    ASSERT(y != z);
    ASSERT(x != z);
    PASS();
}

TEST(alloc_cons_creates_pair)
{
    unsigned x = alloc_cons(store(1), store(2));
    ASSERT(CELL_TYPE(x) == BT_CONS);
    ASSERT_EQ(CELL_ID(car(x)), 1);
    ASSERT_EQ(CELL_ID(cdr(x)), 2);
    PASS();
}

// ============================================================================
// Context Tests - Numeric Storage
// ============================================================================

TEST(store_positive)
{
    unsigned x = store(42);
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

TEST(store_negative)
{
    unsigned x = store(-123);
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), -123);
    PASS();
}

TEST(store_zero)
{
    unsigned x = store(0);
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 0);
    PASS();
}

TEST(store_inexact)
{
    unsigned x = store_inexact(3.14159);
    ASSERT(CELL_TYPE(x) == BT_INEXACT);
    double val = to_double(x);
    ASSERT(val > 3.14 && val < 3.15);
    PASS();
}

TEST(store_rational)
{
    unsigned x = store_rational(1, 2);
    ASSERT(CELL_TYPE(x) == BT_RATIONAL);
    // Rational stores num in car(x), denom in cdr(x) - both are cells
    ASSERT_EQ(CELL_ID(car(x)), 1);
    ASSERT_EQ(CELL_ID(cdr(x)), 2);
    PASS();
}

TEST(store_rational_normalized)
{
    // 4/6 should normalize to 2/3
    unsigned x = normalize_rational(4, 6);
    ASSERT(CELL_TYPE(x) == BT_RATIONAL);
    ASSERT_EQ(CELL_ID(car(x)), 2);
    ASSERT_EQ(CELL_ID(cdr(x)), 3);
    PASS();
}

TEST(store_rational_to_integer)
{
    // 6/3 should normalize to integer 2
    unsigned x = normalize_rational(6, 3);
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 2);
    PASS();
}

TEST(numeric_helpers_accept_direct_fixnum)
{
    ASSERT(is_numeric(MAKE_FIXNUM(42)));
    ASSERT(is_exact(MAKE_FIXNUM(-7)));
    ASSERT(to_double(MAKE_FIXNUM(5)) == 5.0);
    PASS();
}

TEST(numeric_helpers_reject_token_sentinels)
{
    ASSERT(!is_numeric(TOK_ERROR));
    ASSERT(!is_exact(TOK_ERROR));
    ASSERT(to_double(TOK_ERROR) == 0.0);
    PASS();
}

TEST(store_complex_accepts_direct_fixnum_parts)
{
    unsigned real = MAKE_FIXNUM(3);
    unsigned zero_imag = store_complex(real, MAKE_FIXNUM(0));
    ASSERT(zero_imag == real);

    unsigned complex = store_complex(real, MAKE_FIXNUM(4));
    ASSERT(CELL_TYPE(complex) == BT_COMPLEX);
    ASSERT(CELL_CAR(complex) == real);
    ASSERT(CELL_CDR(complex) == MAKE_FIXNUM(4));
    ASSERT(to_double(complex) == 3.0);
    PASS();
}

TEST(numeric_sign_helpers_accept_direct_fixnum)
{
    ASSERT(is_negative_number(MAKE_FIXNUM(-3)));
    ASSERT(!is_negative_number(MAKE_FIXNUM(3)));
    ASSERT(negate_number(MAKE_FIXNUM(3)) == MAKE_FIXNUM(-3));
    ASSERT(negate_number(MAKE_FIXNUM(-3)) == MAKE_FIXNUM(3));

    unsigned min_negated = negate_number(MAKE_FIXNUM(FIXNUM_MIN));
    ASSERT(CELL_TYPE(min_negated) == BT_NUM);
    ASSERT_EQ(CELL_ID(min_negated), -(int64_t)FIXNUM_MIN);
    ASSERT(!is_negative_number(TOK_ERROR));
    ASSERT_EQ(negate_number(TOK_ERROR), TOK_ERROR);
    PASS();
}

TEST(bignum_helpers_accept_direct_fixnum)
{
    bignum *bn = to_bignum(MAKE_FIXNUM(-42));
    ASSERT(bn != NULL);
    ASSERT_EQ(bn_sign(bn), -1);
    ASSERT_EQ(bn->len, 1);
    ASSERT_EQ(bn->limbs[0], 42);
    bn_free(bn);

    ASSERT(get_bignum(MAKE_FIXNUM(1)) == NULL);
    ASSERT(to_bignum(TOK_ERROR) == NULL);

    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_BIGNUM;
    CELL_PTR(bad) = NULL;
    ASSERT(get_bignum(bad) == NULL);
    ASSERT(to_bignum(bad) == NULL);
    PASS();
}

TEST(malformed_bignum_payload_is_safe)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_BIGNUM;
    CELL_PTR(bad) = NULL;
    unsigned other_bad = alloc();
    CELL_TYPE(other_bad) = BT_BIGNUM;
    CELL_PTR(other_bad) = NULL;

    ASSERT(!is_negative_number(bad));
    ASSERT_EQ(negate_number(bad), bad);
    ASSERT(!deep_equal(bad, other_bad));

    unsigned args[1] = {bad};
    ASSERT_EQ(apply_primitive_argv(PNUMP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PINTEGERP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PRATIONALP, 1, args), ctx.atom_false);
    ASSERT(apply_primitive_argv(PNUM2STR, 1, args) == TOK_ERROR);
    unsigned cmp_args[2] = {bad, store(0)};
    ASSERT(apply_primitive_argv(PEQUAL, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PLT, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PMOD, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PQUOTIENT, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PREMAINDER, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PABS, 1, args) == TOK_ERROR);

    CELL_PTR(bad) = (void *)(uintptr_t)1;
    ASSERT(!is_negative_number(bad));
    ASSERT_EQ(negate_number(bad), bad);
    ASSERT(!deep_equal(bad, other_bad));
    ASSERT_EQ(apply_primitive_argv(PNUMP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PINTEGERP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PRATIONALP, 1, args), ctx.atom_false);
    ASSERT(apply_primitive_argv(PNUM2STR, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PEQUAL, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PLT, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PMOD, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PQUOTIENT, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PREMAINDER, 2, cmp_args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PABS, 1, args) == TOK_ERROR);

    FILE *mem = tmpfile();
    ASSERT(mem != NULL);
    display_obj_port(bad, mem);
    fclose(mem);

    PASS();
}

TEST(malformed_symbol_payload_is_safe)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_ATOM;
    CELL_ID(bad) = INT64_MAX;
    ASSERT(!atom_is_valid(bad));

    unsigned args[1] = {bad};
    ASSERT_EQ(apply_primitive_argv(PSYM2STR, 1, args), TOK_ERROR);
    ASSERT_EQ(apply_primitive_argv(PEOF, 1, args), ctx.atom_false);
    ASSERT(!is_eof_object(bad));

    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);
    write_obj_port(bad, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "[invalid-symbol]");
    free(buf);
    PASS();
}

TEST(out_of_range_value_is_not_a_cell)
{
    unsigned bad = 2u * SEMISPACE_SIZE;
    ASSERT(!IS_CELL(bad));
    ASSERT(!IS_PAIR(bad));
    ASSERT(!IS_ATOM(bad));
    ASSERT(!is_numeric(bad));

    unsigned args[1] = {bad};
    ASSERT_EQ(apply_primitive_argv(PNUMP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PPROCP, 1, args), ctx.atom_false);
    ASSERT_EQ(collect(bad), bad);
    ASSERT_EQ(collect_to_old(bad), bad);
    PASS();
}

TEST(equal_hash_table_handles_cyclic_numeric_payload)
{
    unsigned cyclic = alloc();
    CELL_TYPE(cyclic) = BT_RATIONAL;
    CELL_CAR(cyclic) = cyclic;
    CELL_CDR(cyclic) = store(1);

    unsigned table = apply_primitive_argv(PMAKEEQUALHASHTABLE, 0, NULL);
    ASSERT(IS_HASHTABLE(table));
    unsigned args[3] = {table, cyclic, store(1)};
    ASSERT_EQ(apply_primitive_argv(PHASHTABLESET, 3, args), 0);
    PASS();
}

TEST(string_slice_set_survives_an_exhausted_nursery)
{
    GC_GUARD;
    char buffer[INT_CACHE_MAX + 16];
    memset(buffer, 'a', sizeof(buffer) - 1);
    buffer[sizeof(buffer) - 1] = '\0';
    unsigned original = make_string_copy(buffer);
    gc_protect(&original);
    unsigned slice_args[3] = {original, MAKE_FIXNUM(INT_CACHE_MAX + 1),
                             MAKE_FIXNUM(INT_CACHE_MAX + 3)};
    unsigned slice = apply_primitive_argv(PSTRSLICE, 3, slice_args);
    ASSERT(IS_STRING(slice));
    gc_protect(&slice);
    unsigned value = make_char(0x1D11E);
    gc_protect(&value);
    if (ctx.card_table) {
        unsigned nursery_end =
            (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
        ctx.nursery_ptr = nursery_end;
    }
    unsigned args[3] = {slice, MAKE_FIXNUM(1), value};
    unsigned result = apply_primitive_argv(PSTRSET, 3, args);
    ASSERT(result != TOK_ERROR);
    ASSERT_STR_EQ(GET_STRING_PTR(original) + INT_CACHE_MAX + 1,
                  "a𝄞aaaaaaaaaaaa");
    ASSERT_STR_EQ(GET_STRING_PTR(slice), "a𝄞");
    PASS();
}

TEST(hash_table_enumeration_survives_gc_rehash)
{
    enum { ENTRY_COUNT = 32 };
    GC_GUARD;
    unsigned table = apply_primitive_argv(PMAKEEQUALHASHTABLE, 0, NULL);
    ASSERT(IS_HASHTABLE(table));
    gc_protect(&table);

    for (unsigned i = 0; i < ENTRY_COUNT; i++) {
        unsigned key = alloc_cons(MAKE_FIXNUM((int)i),
                                  MAKE_FIXNUM((int)(i + 100)));
        unsigned args[3] = {table, key, MAKE_FIXNUM((int)i)};
        ASSERT_EQ(apply_primitive_argv(PHASHTABLESET, 3, args), 0);
    }

    if (ctx.card_table) {
        unsigned nursery_end =
            (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
        ctx.nursery_ptr = nursery_end;
    }

    unsigned args[1] = {table};
    unsigned alist = apply_primitive_argv(PHASHTABLEALIST, 1, args);
    ASSERT(IS_PAIR(alist));
    bool seen[ENTRY_COUNT] = {false};
    unsigned count = 0;
    for (unsigned it = alist; IS_PAIR(it); it = cdr(it)) {
        unsigned entry = car(it);
        ASSERT(IS_PAIR(entry));
        ASSERT(IS_FIXNUM(cdr(entry)));
        int64_t value = FIXNUM_VALUE(cdr(entry));
        ASSERT(value >= 0 && value < ENTRY_COUNT);
        ASSERT(!seen[value]);
        seen[value] = true;
        count++;
    }
    ASSERT_EQ(count, ENTRY_COUNT);
    for (unsigned i = 0; i < ENTRY_COUNT; i++)
        ASSERT(seen[i]);
    PASS();
}

TEST(error_rejects_malformed_string_message)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_STRING;
    CELL_PTR(bad) = (void *)(uintptr_t)1;
    unsigned args[1] = {bad};

    ASSERT_EQ(apply_primitive_argv(PERROR, 1, args), TOK_ERROR);
    CELL_PTR(bad) = NULL;
    PASS();
}

TEST(string_port_rejects_malformed_source_string)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_STRING;
    CELL_PTR(bad) = (void *)(uintptr_t)1;

    string_port *sp = checked_calloc_array(1, sizeof(string_port));
    ASSERT(sp != NULL);
    sp->data = (char *)(uintptr_t)1;
    sp->len = 1;
    sp->cap = 2;
    sp->source_string = bad;
    sp->source_end = 1;
    string_port_register(sp);

    unsigned port = alloc();
    CELL_TYPE(port) = BT_STRINPORT;
    CELL_PTR(port) = sp;
    unsigned args[1] = {port};
    ASSERT_EQ(apply_primitive_argv(PPORTOPENP, 1, args), ctx.atom_false);

    CELL_PTR(port) = NULL;
    string_port_unregister(sp);
    free(sp);
    CELL_PTR(bad) = NULL;
    PASS();
}

TEST(malformed_rational_payload_is_not_numeric)
{
    unsigned bad_num = alloc();
    CELL_TYPE(bad_num) = BT_RATIONAL;
    CELL_CAR(bad_num) = ctx.atom_true;
    CELL_CDR(bad_num) = store(1);

    unsigned bad_den = alloc();
    CELL_TYPE(bad_den) = BT_RATIONAL;
    CELL_CAR(bad_den) = store(1);
    CELL_CDR(bad_den) = store(0);

    ASSERT(!is_numeric(bad_num));
    ASSERT(!is_exact(bad_num));
    ASSERT(!is_numeric(bad_den));
    ASSERT(!is_exact(bad_den));
    ASSERT_EQ(to_double(bad_num), 0.0);
    ASSERT_EQ(to_double(bad_den), 0.0);

    unsigned args[1] = {bad_num};
    ASSERT_EQ(apply_primitive_argv(PNUMP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PRATIONALP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PNUMERATOR, 1, args), TOK_ERROR);
    ASSERT_EQ(apply_primitive_argv(PABS, 1, args), TOK_ERROR);

    args[0] = bad_den;
    ASSERT_EQ(apply_primitive_argv(PNUMP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PRATIONALP, 1, args), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PDENOMINATOR, 1, args), TOK_ERROR);
    ASSERT_EQ(apply_primitive_argv(PSQRT, 1, args), TOK_ERROR);
    PASS();
}

TEST(malformed_complex_payload_is_not_numeric)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_COMPLEX;
    CELL_CAR(bad) = ctx.atom_true;
    CELL_CDR(bad) = store(0);

    unsigned nested = alloc();
    CELL_TYPE(nested) = BT_COMPLEX;
    CELL_CAR(nested) = store(1);
    CELL_CDR(nested) = bad;

    ASSERT(!is_numeric(bad));
    ASSERT(!is_exact(bad));
    ASSERT(!is_numeric(nested));
    ASSERT(!is_exact(nested));
    ASSERT_EQ(to_double(bad), 0.0);
    ASSERT_EQ(to_double(nested), 0.0);

    unsigned args[1] = {bad};
    ASSERT_EQ(apply_primitive_argv(PREALPART, 1, args), TOK_ERROR);
    ASSERT_EQ(apply_primitive_argv(PMAGNITUDE, 1, args), TOK_ERROR);

    args[0] = nested;
    ASSERT_EQ(apply_primitive_argv(PIMAGPART, 1, args), TOK_ERROR);
    ASSERT_EQ(apply_primitive_argv(PEXACT2INEXACT, 1, args), TOK_ERROR);
    PASS();
}

TEST(store_bignum_test)
{
    bignum *bn = bn_from_string("12345678901234567890", 10);
    unsigned x = store_bignum(bn);
    ASSERT(CELL_TYPE(x) == BT_BIGNUM);
    bignum *retrieved = get_bignum(x);
    ASSERT(retrieved != NULL);
    ASSERT_EQ(bn_cmp(retrieved, bn), 0);
    PASS();
}

// ============================================================================
// Context Tests - Interning
// ============================================================================

TEST(intern_same_string)
{
    int a = intern("hello");
    int b = intern("hello");
    ASSERT_EQ(a, b);
    PASS();
}

TEST(intern_different_strings)
{
    int a = intern("foo");
    int b = intern("bar");
    ASSERT(a != b);
    PASS();
}

TEST(uninterned_symbols_keep_distinct_identities)
{
    GC_GUARD;
    unsigned first = make_uninterned_symbol("fresh-identity");
    gc_protect(&first);
    unsigned ordinary = atom_from_string("fresh-identity");
    gc_protect(&ordinary);
    unsigned second = make_uninterned_symbol("fresh-identity");
    gc_protect(&second);
    ASSERT(atom_is_uninterned(first));
    ASSERT(atom_is_uninterned(second));
    ASSERT(!atom_is_uninterned(ordinary));
    ASSERT(CELL_ID(first) != CELL_ID(second));
    ASSERT(CELL_ID(first) != CELL_ID(ordinary));
    ASSERT(CELL_ID(second) != CELL_ID(ordinary));
    ASSERT_EQ(intern("fresh-identity"), CELL_ID(ordinary));
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(first)], "fresh-identity");
    ASSERT(!deep_equal(first, ordinary));
    ASSERT(!deep_equal(first, second));
    gc(0);
    ASSERT(atom_is_uninterned(first));
    ASSERT_EQ(intern("fresh-identity"), CELL_ID(ordinary));
    PASS();
}

TEST(atom_from_string_creates_atom)
{
    unsigned x = atom_from_string("test-symbol");
    ASSERT(CELL_TYPE(x) == BT_ATOM);
    ASSERT_STR_EQ(ctx.atom_table[CELL_ID(x)], "test-symbol");
    PASS();
}

TEST(atom_from_string_parses_number)
{
    unsigned x = atom_from_string("42");
    ASSERT(CELL_TYPE(x) == BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

// ============================================================================
// Context Tests - Characters
// ============================================================================

TEST(make_char_test)
{
    unsigned x = make_char('A');
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), 'A');
    PASS();
}

TEST(make_char_special)
{
    unsigned x = make_char('\n');
    ASSERT(CELL_TYPE(x) == BT_CHAR);
    ASSERT_EQ(CELL_ID(x), '\n');
    PASS();
}

// ============================================================================
// Context Tests - Vectors
// ============================================================================

TEST(make_vector_empty)
{
    unsigned v = make_vector(0, 0);
    ASSERT(CELL_TYPE(v) == BT_VECTOR);
    ASSERT_EQ(vector_len(v), 0);
    PASS();
}

TEST(make_vector_with_fill)
{
    unsigned fill = store(42);
    unsigned v = make_vector(5, fill);
    ASSERT(CELL_TYPE(v) == BT_VECTOR);
    ASSERT_EQ(vector_len(v), 5);
    unsigned *data = vector_data_ptr(v);
    for (int i = 0; i < 5; i++) {
        ASSERT_EQ(CELL_ID(data[i]), 42);
    }
    PASS();
}

TEST(vector_data_access)
{
    unsigned v = make_vector(3, 0);
    unsigned *data = vector_data_ptr(v);
    data[0] = store(10);
    data[1] = store(20);
    data[2] = store(30);
    ASSERT_EQ(CELL_ID(data[0]), 10);
    ASSERT_EQ(CELL_ID(data[1]), 20);
    ASSERT_EQ(CELL_ID(data[2]), 30);
    PASS();
}

TEST(vector_primitive_roots_arguments_across_gc)
{
    unsigned args[2] = {store(11), store(22)};
    if (ctx.card_table) {
        unsigned nursery_end =
            (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
        ctx.nursery_ptr = nursery_end;
    }
    unsigned v = apply_vector_primitive(PVECTOR, 2, args);
    ASSERT(CELL_TYPE(v) == BT_VECTOR);
    unsigned *data = vector_data_ptr(v);
    ASSERT(data != NULL);
    ASSERT(CELL_TYPE(data[0]) == BT_NUM);
    ASSERT(CELL_TYPE(data[1]) == BT_NUM);
    ASSERT_EQ(CELL_ID(data[0]), 11);
    ASSERT_EQ(CELL_ID(data[1]), 22);
    PASS();
}

TEST(writer_handles_direct_fixnum)
{
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(MAKE_FIXNUM(-42), mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "-42");
    free(buf);
    PASS();
}

TEST(writer_handles_out_of_range_value)
{
    unsigned bad = 2u * SEMISPACE_SIZE;
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(bad, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "[invalid-value]");
    ASSERT_STR_EQ(type_name(bad), "invalid-value");
    free(buf);
    PASS();
}

TEST(writer_handles_vector_containing_direct_fixnum)
{
    unsigned v = make_vector(2, MAKE_FIXNUM(7));
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(v, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "#(7 7)");
    free(buf);
    PASS();
}

TEST(writer_handles_vm_continuation)
{
    unsigned cont = alloc();
    CELL_TYPE(cont) = BT_VMCONT;
    CELL_PTR(cont) = NULL;

    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(cont, mem);
    fclose(mem);
    // MIT's form, numbered by object hash: #[continuation N]
    ASSERT(strncmp(buf, "#[continuation ", 15) == 0);
    ASSERT(buf[len - 1] == ']');
    free(buf);
    PASS();
}

TEST(writer_handles_malformed_multiple_values)
{
    unsigned values = alloc_cons(store(1), 0);
    cell_set_cdr(values, values);
    unsigned multi = alloc();
    CELL_TYPE(multi) = BT_MULTIVAL;
    CELL_CAR(multi) = values;

    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(multi, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "[invalid-values]");
    free(buf);
    PASS();
}

TEST(writer_handles_cyclic_numeric_payload)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_RATIONAL;
    CELL_CAR(bad) = bad;
    CELL_CDR(bad) = store(1);

    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(bad, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "[invalid-number]");
    free(buf);
    PASS();
}

TEST(writer_handles_bytecode_closure_marker)
{
    unsigned marker = alloc();
    CELL_TYPE(marker) = BT_CLOSURE;
    CELL_PTR(marker) = NULL;

    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(marker, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "[bytecode-closure]");
    ASSERT_STR_EQ(type_name(marker), "bytecode-closure");
    free(buf);
    PASS();
}

TEST(writer_escapes_control_strings_with_scalar_escape)
{
    char bytes[] = {'a', 1, 0x7f, 'b', '\0'};
    unsigned s = make_string_copy(bytes);
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(s, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "\"a\\x01;\\x7F;b\"");
    free(buf);
    PASS();
}

TEST(writer_writes_unicode_character_literal)
{
    unsigned ch = make_char(0x1D11E);
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(ch, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "#\\x1D11E");
    free(buf);
    PASS();
}

TEST(writer_displays_unicode_character_as_utf8)
{
    unsigned ch = make_char(0x03BB);
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    display_obj_port(ch, mem);
    fclose(mem);
    ASSERT_EQ((unsigned char)buf[0], 0xCE);
    ASSERT_EQ((unsigned char)buf[1], 0xBB);
    ASSERT_EQ(buf[2], 0);
    free(buf);
    PASS();
}

TEST(writer_escapes_non_roundtripping_symbols)
{
    unsigned sym = atom_from_string("Hello World");
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(sym, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "|Hello World|");
    free(buf);

    sym = alloc();
    CELL_TYPE(sym) = BT_ATOM;
    CELL_ID(sym) = intern("123");
    buf = NULL;
    len = 0;
    mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);
    write_obj_port(sym, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "|123|");
    free(buf);
    PASS();
}

TEST(writer_escapes_symbol_bar_and_backslash)
{
    unsigned sym = atom_from_string("a|b\\c");
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    write_obj_port(sym, mem);
    fclose(mem);
    ASSERT_STR_EQ(buf, "|a\\|b\\\\c|");
    free(buf);
    PASS();
}

TEST(write_simple_rejects_cycles)
{
    unsigned cell = alloc_cons(store(1), 0);
    cell_set_cdr(cell, cell);

    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    ASSERT(mem != NULL);

    ASSERT(!write_simple_obj_port_checked(cell, mem));
    fclose(mem);
    free(buf);
    PASS();
}

TEST(write_simple_rejects_cycle_after_tracking_table_exhaustion)
{
    unsigned head = 0;
    unsigned tail = 0;
    for (unsigned i = 0; i < 8300; i++)
        list_append(&head, &tail, MAKE_FIXNUM((int)i));
    cell_set_cdr(tail, tail);

    FILE *mem = tmpfile();
    ASSERT(mem != NULL);
    ASSERT(!write_simple_obj_port_checked(head, mem));
    fclose(mem);
    PASS();
}

TEST(write_shared_handles_cycle_after_tracking_table_exhaustion)
{
    unsigned head = 0;
    unsigned tail = 0;
    for (unsigned i = 0; i < 8300; i++)
        list_append(&head, &tail, MAKE_FIXNUM((int)i));
    cell_set_cdr(tail, tail);

    FILE *mem = tmpfile();
    ASSERT(mem != NULL);
    write_shared_obj_port(head, mem);
    fclose(mem);
    PASS();
}

TEST(string_port_write_failures_return_false)
{
    string_port *sp = strport_new();
    ASSERT(sp != NULL);

    sp->cap = SIZE_MAX / 2 + 1;
    sp->len = sp->cap - 1;
    ASSERT(!strport_putc(sp, 'x'));

    sp->len = SIZE_MAX - 2;
    ASSERT(!strport_puts(sp, "xx"));

    sp->len = 0;
    sp->cap = INITIAL_STRING_CAP;
    sp->data[0] = '\0';
    strport_free(sp);
    PASS();
}

TEST(make_string_owned_rejects_null)
{
    ASSERT(make_string_owned(NULL) == TOK_ERROR);
    PASS();
}

// ============================================================================
// Context Tests - List Utilities
// ============================================================================

TEST(list_length_empty)
{
    ASSERT_EQ(list_length(0), 0);
    PASS();
}

TEST(list_length_non_pair_is_zero)
{
    ASSERT_EQ(list_length(MAKE_FIXNUM(42)), 0);
    PASS();
}

TEST(proper_list_silent_rejects_circular_list)
{
    unsigned cycle = alloc_cons(MAKE_FIXNUM(1), 0);
    cell_set_cdr(cycle, cycle);
    ASSERT(!proper_list_silent(cycle));
    PASS();
}

TEST(last_pair_rejects_circular_list)
{
    unsigned cycle = alloc_cons(MAKE_FIXNUM(1), 0);
    cell_set_cdr(cycle, cycle);
    unsigned args[1] = {cycle};
    ASSERT_EQ(apply_primitive_argv(PLASTPAIR, 1, args), TOK_ERROR);
    PASS();
}

TEST(metadata_list_helpers_reject_circular_lists)
{
    unsigned x = atom_from_string("metadata-cycle");
    unsigned cycle = alloc_cons(x, 0);
    cell_set_cdr(cycle, cycle);

    ASSERT(!binding_list_binds_id(cycle, CELL_ID(x)));
    ASSERT(!lambda_params_bind_id(cycle, CELL_ID(x)));
    ASSERT(!atom_list_contains_id(cycle, CELL_ID(x)));
    ASSERT(!assoc_list_has_atom_key_id(cycle, CELL_ID(x)));
    PASS();
}

TEST(list_length_three)
{
    unsigned lst =
        alloc_cons(store(1), alloc_cons(store(2), alloc_cons(store(3), 0)));
    ASSERT_EQ(list_length(lst), 3);
    PASS();
}

TEST(list_append_builds_list)
{
    unsigned head = 0, tail = 0;
    list_append(&head, &tail, store(1));
    list_append(&head, &tail, store(2));
    list_append(&head, &tail, store(3));
    ASSERT_EQ(list_length(head), 3);
    ASSERT_EQ(CELL_ID(car(head)), 1);
    ASSERT_EQ(CELL_ID(cadr(head)), 2);
    ASSERT_EQ(CELL_ID(caddr(head)), 3);
    PASS();
}

TEST(numeric_comparison_roots_middle_argument_across_gc)
{
    GC_GUARD;
    unsigned args[3] = {0, 0, 0};
    for (unsigned i = 0; i < 3; i++)
        gc_protect(&args[i]);
    args[0] = normalize_rational(1, 2);
    args[1] = store_inexact(0.5);
    args[2] = normalize_rational(1, 2);
    if (ctx.card_table) {
        unsigned nursery_end =
            (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
        ctx.nursery_ptr = nursery_end;
    }
    ASSERT(numeric_compare(3, args, CMP_EQ) == ctx.atom_true);
    PASS();
}

TEST(append_primitive_roots_arguments_across_gc)
{
    unsigned first = alloc_cons(store(1), alloc_cons(store(2), 0));
    unsigned second = alloc_cons(store(3), alloc_cons(store(4), 0));
    unsigned args[2] = {first, second};
    if (ctx.card_table) {
        unsigned nursery_end =
            (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
        ctx.nursery_ptr = nursery_end;
    }
    unsigned result = apply_primitive_argv(PAPPEND, 2, args);
    ASSERT(result != TOK_ERROR);
    ASSERT(CELL_TYPE(result) == BT_CONS);
    ASSERT_EQ(CELL_ID(car(result)), 1);
    result = cdr(result);
    ASSERT_EQ(CELL_ID(car(result)), 2);
    result = cdr(result);
    ASSERT_EQ(CELL_ID(car(result)), 3);
    result = cdr(result);
    ASSERT_EQ(CELL_ID(car(result)), 4);
    ASSERT_EQ(cdr(result), 0);
    PASS();
}

// ============================================================================
// Context Tests - Deep Equality
// ============================================================================

TEST(deep_equal_numbers)
{
    ASSERT(deep_equal(store(42), store(42)));
    ASSERT(!deep_equal(store(42), store(43)));
    PASS();
}

TEST(deep_equal_fixnum_and_boxed_integer)
{
    ASSERT(deep_equal(MAKE_FIXNUM(42), store(42)));
    ASSERT(deep_equal(store(-7), MAKE_FIXNUM(-7)));
    ASSERT(!deep_equal(MAKE_FIXNUM(42), store(43)));
    PASS();
}

TEST(deep_equal_rejects_token_sentinels)
{
    ASSERT(!deep_equal(MAKE_FIXNUM(42), TOK_ERROR));
    ASSERT(!deep_equal(TOK_ERROR, store(42)));
    PASS();
}

TEST(primitive_eq_handles_fixnum_and_boxed_integer)
{
    unsigned argv_same[2] = {MAKE_FIXNUM(42), store(42)};
    unsigned argv_diff[2] = {MAKE_FIXNUM(42), store(43)};
    unsigned argv_nil[2] = {MAKE_FIXNUM(42), 0};
    ASSERT(apply_primitive_argv(PEQ, 2, argv_same) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PEQ, 2, argv_diff) == ctx.atom_false);
    ASSERT(apply_primitive_argv(PEQ, 2, argv_nil) == ctx.atom_false);
    PASS();
}

TEST(primitive_arithmetic_handles_direct_fixnums)
{
    unsigned plus_args[2] = {MAKE_FIXNUM(2), MAKE_FIXNUM(3)};
    unsigned plus = apply_primitive_argv(PPLUS, 2, plus_args);
    ASSERT(CELL_TYPE(plus) == BT_NUM);
    ASSERT_EQ(CELL_ID(plus), 5);

    unsigned minus_args[2] = {MAKE_FIXNUM(7), MAKE_FIXNUM(4)};
    unsigned minus = apply_primitive_argv(PMINUS, 2, minus_args);
    ASSERT(CELL_TYPE(minus) == BT_NUM);
    ASSERT_EQ(CELL_ID(minus), 3);

    unsigned times_args[2] = {MAKE_FIXNUM(6), MAKE_FIXNUM(7)};
    unsigned times = apply_primitive_argv(PTIMES, 2, times_args);
    ASSERT(CELL_TYPE(times) == BT_NUM);
    ASSERT_EQ(CELL_ID(times), 42);

    unsigned divide_args[2] = {MAKE_FIXNUM(21), MAKE_FIXNUM(7)};
    unsigned divide = apply_primitive_argv(PDIV, 2, divide_args);
    ASSERT(CELL_TYPE(divide) == BT_NUM);
    ASSERT_EQ(CELL_ID(divide), 3);

    unsigned abs_args[1] = {MAKE_FIXNUM(-8)};
    unsigned abs_result = apply_primitive_argv(PABS, 1, abs_args);
    ASSERT(CELL_TYPE(abs_result) == BT_NUM);
    ASSERT_EQ(CELL_ID(abs_result), 8);

    unsigned modulo_args[2] = {MAKE_FIXNUM(17), MAKE_FIXNUM(5)};
    unsigned modulo = apply_primitive_argv(PMOD, 2, modulo_args);
    ASSERT(CELL_TYPE(modulo) == BT_NUM);
    ASSERT_EQ(CELL_ID(modulo), 2);

    unsigned remainder_args[2] = {MAKE_FIXNUM(17), MAKE_FIXNUM(5)};
    unsigned remainder = apply_primitive_argv(PREMAINDER, 2, remainder_args);
    ASSERT(CELL_TYPE(remainder) == BT_NUM);
    ASSERT_EQ(CELL_ID(remainder), 2);

    unsigned quotient_args[2] = {MAKE_FIXNUM(17), MAKE_FIXNUM(5)};
    unsigned quotient = apply_primitive_argv(PQUOTIENT, 2, quotient_args);
    ASSERT(CELL_TYPE(quotient) == BT_NUM);
    ASSERT_EQ(CELL_ID(quotient), 3);
    PASS();
}

TEST(primitive_arithmetic_roots_arguments_across_gc)
{
    unsigned args[2] = {store_rational(1, 2), store_rational(1, 3)};
    if (ctx.card_table) {
        unsigned nursery_end =
            (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
        ctx.nursery_ptr = nursery_end;
    }
    unsigned result = apply_primitive_argv(PPLUS, 2, args);
    ASSERT(result != TOK_ERROR);
    ASSERT(CELL_TYPE(result) == BT_RATIONAL);
    ASSERT_EQ(CELL_ID(car(result)), 5);
    ASSERT_EQ(CELL_ID(cdr(result)), 6);
    PASS();
}

TEST(numeric_compare_roots_previous_operand_across_gc)
{
    unsigned args[3] = {store_rational(1, 2), store_rational(1, 2),
                        store_rational(1, 2)};
    if (ctx.card_table) {
        unsigned nursery_end =
            (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
        ctx.nursery_ptr = nursery_end;
    }
    ASSERT(apply_primitive_argv(PEQUAL, 3, args) == ctx.atom_true);
    PASS();
}

TEST(bytevector_primitives_reject_direct_fixnum)
{
    unsigned args[1] = {MAKE_FIXNUM(1)};
    ASSERT(apply_primitive_argv(PBYTEVECUP, 1, args) == ctx.atom_false);
    ASSERT(apply_primitive_argv(PBYTEVECLEN, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PBYTEVECCOPY, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PBYTEVECAPPEND, 1, args) == TOK_ERROR);
    PASS();
}

TEST(bytevector_primitives_reject_malformed_payload)
{
    unsigned bytevec = alloc();
    CELL_TYPE(bytevec) = BT_BYTEVEC;
    CELL_PTR(bytevec) = NULL;

    unsigned args[1] = {bytevec};
    ASSERT(apply_primitive_argv(PBYTEVECUP, 1, args) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PBYTEVECLEN, 1, args) == TOK_ERROR);

    CELL_PTR(bytevec) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PBYTEVECUP, 1, args) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PBYTEVECLEN, 1, args) == TOK_ERROR);

    bytevec_data *bv = checked_malloc_flex(sizeof(bytevec_data), 4, 1);
    ASSERT(bv != NULL);
    bv->len = 4;
    bytevec_register(bv);
    bv->len = UINT_MAX;
    CELL_PTR(bytevec) = bv;
    ASSERT(apply_primitive_argv(PBYTEVECLEN, 1, args) == TOK_ERROR);

    CELL_PTR(bytevec) = NULL;
    bytevec_unregister(bv);
    free(bv);
    PASS();
}

TEST(string_primitives_reject_malformed_payload)
{
    unsigned string = alloc();
    CELL_TYPE(string) = BT_STRING;
    CELL_PTR(string) = NULL;

    unsigned args[1] = {string};
    ASSERT(apply_primitive_argv(PSTRINGP, 1, args) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PSTRLEN, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PDISPLAY, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PLOAD, 1, args) == TOK_ERROR);

    CELL_PTR(string) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PSTRINGP, 1, args) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PSTRLEN, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PDISPLAY, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PLOAD, 1, args) == TOK_ERROR);

    PASS();
}

TEST(vector_primitives_reject_malformed_payload)
{
    unsigned vector = alloc();
    CELL_TYPE(vector) = BT_VECTOR;
    CELL_PTR(vector) = NULL;

    unsigned args[1] = {vector};
    ASSERT(apply_primitive_argv(PVECTORP, 1, args) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PVECLEN, 1, args) == TOK_ERROR);

    CELL_PTR(vector) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PVECTORP, 1, args) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PVECLEN, 1, args) == TOK_ERROR);

    vector_data *vd = checked_malloc_flex(sizeof(vector_data), 4,
                                          sizeof(unsigned));
    ASSERT(vd != NULL);
    vd->len = 4;
    vector_register(vd);
    vd->len = UINT_MAX;
    CELL_PTR(vector) = vd;
    ASSERT(apply_primitive_argv(PVECLEN, 1, args) == TOK_ERROR);

    CELL_PTR(vector) = NULL;
    vector_unregister(vd);
    free(vd);
    PASS();
}

TEST(port_primitives_reject_malformed_file_payload)
{
    unsigned port = alloc();
    CELL_TYPE(port) = BT_INPORT;
    CELL_PTR(port) = NULL;

    unsigned pred_args[1] = {port};
    ASSERT(apply_primitive_argv(PPORTOPENP, 1, pred_args) == ctx.atom_false);
    ASSERT(apply_primitive_argv(PINPUTPORTOPENP, 1, pred_args) ==
           ctx.atom_false);

    unsigned read_args[2] = {MAKE_FIXNUM(1), port};
    ASSERT(apply_primitive_argv(PREADBYTEVEC, 2, read_args) == TOK_ERROR);

    CELL_PTR(port) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PPORTOPENP, 1, pred_args) == ctx.atom_false);
    ASSERT(apply_primitive_argv(PINPUTPORTOPENP, 1, pred_args) ==
           ctx.atom_false);
    ASSERT(apply_primitive_argv(PREADBYTEVEC, 2, read_args) == TOK_ERROR);
    CELL_PTR(port) = NULL;

    PASS();
}

TEST(port_primitives_reject_malformed_string_payload)
{
    string_port *sp = checked_calloc_array(1, sizeof(string_port));
    ASSERT(sp != NULL);
    sp->data = NULL;
    sp->len = 0;
    sp->cap = 1;
    sp->pos = 0;

    unsigned port = alloc();
    CELL_TYPE(port) = BT_STROUTPORT;
    CELL_PTR(port) = sp;

    unsigned args[1] = {port};
    ASSERT(apply_primitive_argv(PPORTOPENP, 1, args) == ctx.atom_false);
    ASSERT(apply_primitive_argv(PGETOUTPUTSTRING, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PSETCURRENTOUTPUT, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PFLUSHOUTPUT, 1, args) == TOK_ERROR);

    CELL_PTR(port) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PPORTOPENP, 1, args) == ctx.atom_false);
    ASSERT(apply_primitive_argv(PGETOUTPUTSTRING, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PSETCURRENTOUTPUT, 1, args) == TOK_ERROR);
    ASSERT(apply_primitive_argv(PFLUSHOUTPUT, 1, args) == TOK_ERROR);

    CELL_PTR(port) = NULL;
    free(sp);
    PASS();
}

TEST(hash_table_primitives_reject_malformed_payload)
{
    hash_table_data *ht = checked_malloc_size(sizeof(hash_table_data));
    ASSERT(ht != NULL);
    ht->size = 0;
    ht->capacity = 16;
    ht->equiv = HASH_EQUAL;
    ht->buckets = NULL;
    hash_table_register(ht);

    unsigned table = alloc();
    CELL_TYPE(table) = BT_HASHTABLE;
    CELL_PTR(table) = ht;

    unsigned args[1] = {table};
    ASSERT(apply_primitive_argv(PHASHTABLESIZE, 1, args) == TOK_ERROR);

    CELL_PTR(table) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PHASHTABLESIZE, 1, args) == TOK_ERROR);

    CELL_PTR(table) = NULL;
    hash_table_unregister(ht);
    free(ht);
    PASS();
}

TEST(numtower_primitives_handle_direct_fixnums)
{
    unsigned positive_arg[1] = {MAKE_FIXNUM(9)};
    unsigned negative_arg[1] = {MAKE_FIXNUM(-9)};

    ASSERT(apply_primitive_argv(PINTEGERP, 1, positive_arg) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PRATIONALP, 1, positive_arg) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PFINITE, 1, positive_arg) == ctx.atom_true);
    ASSERT(apply_primitive_argv(PINFINITE, 1, positive_arg) ==
           ctx.atom_false);
    ASSERT(apply_primitive_argv(PNAN, 1, positive_arg) == ctx.atom_false);

    unsigned numerator = apply_primitive_argv(PNUMERATOR, 1, positive_arg);
    ASSERT(IS_FIXNUM(numerator));
    ASSERT_EQ(FIXNUM_VALUE(numerator), 9);

    unsigned denominator =
        apply_primitive_argv(PDENOMINATOR, 1, positive_arg);
    ASSERT(CELL_TYPE(denominator) == BT_NUM);
    ASSERT_EQ(CELL_ID(denominator), 1);

    unsigned magnitude = apply_primitive_argv(PMAGNITUDE, 1, negative_arg);
    ASSERT(CELL_TYPE(magnitude) == BT_NUM);
    ASSERT_EQ(CELL_ID(magnitude), 9);

    unsigned real = apply_primitive_argv(PREALPART, 1, positive_arg);
    ASSERT(IS_FIXNUM(real));
    ASSERT_EQ(FIXNUM_VALUE(real), 9);

    unsigned imag = apply_primitive_argv(PIMAGPART, 1, positive_arg);
    ASSERT(CELL_TYPE(imag) == BT_NUM);
    ASSERT_EQ(CELL_ID(imag), 0);

    PASS();
}

TEST(math_primitives_handle_direct_fixnums)
{
    unsigned square_arg[1] = {MAKE_FIXNUM(9)};
    unsigned negative_arg[1] = {MAKE_FIXNUM(-9)};
    unsigned exponent_args[2] = {MAKE_FIXNUM(2), MAKE_FIXNUM(10)};

    unsigned sqrt_result = apply_primitive_argv(PSQRT, 1, square_arg);
    ASSERT(CELL_TYPE(sqrt_result) == BT_NUM);
    ASSERT_EQ(CELL_ID(sqrt_result), 3);

    unsigned expt_result = apply_primitive_argv(PEXPT, 2, exponent_args);
    ASSERT(CELL_TYPE(expt_result) == BT_NUM);
    ASSERT_EQ(CELL_ID(expt_result), 1024);

    unsigned floor_result = apply_primitive_argv(PFLOOR, 1, negative_arg);
    ASSERT(IS_FIXNUM(floor_result));
    ASSERT_EQ(FIXNUM_VALUE(floor_result), -9);

    ASSERT(apply_primitive_argv(PRANDOMSEED, 1, square_arg) ==
           MAKE_FIXNUM(9));
    PASS();
}

TEST(random_seed_rejects_malformed_bignum)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_BIGNUM;
    CELL_PTR(bad) = NULL;

    unsigned args[1] = {bad};
    ASSERT_EQ(apply_primitive_argv(PRANDOMSEED, 1, args), TOK_ERROR);
    CELL_PTR(bad) = (void *)(uintptr_t)1;
    ASSERT_EQ(apply_primitive_argv(PRANDOMSEED, 1, args), TOK_ERROR);
    args[0] = ctx.atom_true;
    ASSERT_EQ(apply_primitive_argv(PRANDOMSEED, 1, args), TOK_ERROR);
    args[0] = store_inexact(1.5);
    ASSERT_EQ(apply_primitive_argv(PRANDOMSEED, 1, args), TOK_ERROR);
    PASS();
}

TEST(number_to_string_handles_direct_fixnum)
{
    unsigned args[1] = {MAKE_FIXNUM(-123)};
    unsigned result = apply_primitive_argv(PNUM2STR, 1, args);
    ASSERT(IS_STRING(result));
    ASSERT_STR_EQ(GET_STRING_PTR(result), "-123");
    PASS();
}

TEST(procedure_predicate_rejects_pair_with_fixnum_car)
{
    unsigned pair = alloc_cons(MAKE_FIXNUM(1), 0);
    unsigned args[1] = {pair};
    ASSERT(apply_primitive_argv(PPROCP, 1, args) == ctx.atom_false);
    PASS();
}

TEST(procedure_predicate_rejects_malformed_bytecode_closure)
{
    unsigned marker = alloc();
    CELL_TYPE(marker) = BT_CLOSURE;
    CELL_PTR(marker) = NULL;
    unsigned closure = alloc_cons(marker, empty_environment());

    unsigned args[1] = {closure};
    ASSERT(apply_primitive_argv(PPROCP, 1, args) == ctx.atom_false);
    ASSERT_EQ(vm_call_closure(closure, 0), TOK_ERROR);

    code_object *unregistered = checked_calloc_array(1, sizeof(code_object));
    ASSERT(unregistered != NULL);
    CELL_PTR(marker) = unregistered;
    ASSERT(apply_primitive_argv(PPROCP, 1, args) == ctx.atom_false);
    ASSERT_EQ(vm_call_closure(closure, 0), TOK_ERROR);
    free(unregistered);
    CELL_PTR(marker) = NULL;
    PASS();
}

TEST(procedure_predicate_rejects_malformed_vm_continuation)
{
    unsigned cont = alloc();
    CELL_TYPE(cont) = BT_VMCONT;
    CELL_PTR(cont) = NULL;

    unsigned args[1] = {cont};
    ASSERT(apply_primitive_argv(PPROCP, 1, args) == ctx.atom_false);

    code_object *code = code_new();
    ASSERT(code != NULL);
    unsigned cont_idx = code_add_const(code, cont);
    code_emit(code, OP_CONST);
    code_emit(code, cont_idx);
    code_emit(code, OP_CALL);
    code_emit(code, 0);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);
    code_free(code);
    ASSERT_EQ(result, TOK_ERROR);

    CELL_PTR(cont) = (void *)(uintptr_t)1;
    ASSERT(apply_primitive_argv(PPROCP, 1, args) == ctx.atom_false);

    code = code_new();
    ASSERT(code != NULL);
    cont_idx = code_add_const(code, cont);
    code_emit(code, OP_CONST);
    code_emit(code, cont_idx);
    code_emit(code, OP_CALL);
    code_emit(code, 0);
    code_emit(code, OP_HALT);

    vm_init(&vm);
    result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);
    code_free(code);
    ASSERT_EQ(result, TOK_ERROR);
    CELL_PTR(cont) = NULL;

    PASS();
}

TEST(deep_equal_atoms)
{
    unsigned a = atom_from_string("foo");
    unsigned b = atom_from_string("foo");
    unsigned c = atom_from_string("bar");
    ASSERT(deep_equal(a, b));
    ASSERT(!deep_equal(a, c));
    PASS();
}

TEST(deep_equal_lists)
{
    unsigned l1 = alloc_cons(store(1), alloc_cons(store(2), 0));
    unsigned l2 = alloc_cons(store(1), alloc_cons(store(2), 0));
    unsigned l3 = alloc_cons(store(1), alloc_cons(store(3), 0));
    ASSERT(deep_equal(l1, l2));
    ASSERT(!deep_equal(l1, l3));
    PASS();
}

TEST(deep_equal_handles_large_lists_without_recursion)
{
    const unsigned count = 100000;
    GC_GUARD;
    unsigned left = 0, right = 0;
    gc_protect(&left);
    gc_protect(&right);
    for (unsigned i = 0; i < count; i++) {
        left = alloc_cons(MAKE_FIXNUM(i), left);
        right = alloc_cons(MAKE_FIXNUM(i), right);
    }
    ASSERT(deep_equal(left, right));
    PASS();
}

TEST(deep_equal_nested)
{
    unsigned inner1 = alloc_cons(store(1), alloc_cons(store(2), 0));
    unsigned inner2 = alloc_cons(store(1), alloc_cons(store(2), 0));
    unsigned outer1 = alloc_cons(inner1, 0);
    unsigned outer2 = alloc_cons(inner2, 0);
    ASSERT(deep_equal(outer1, outer2));
    PASS();
}

// ============================================================================
// Context Tests - Continuations
// ============================================================================

TEST(make_cont_test)
{
    unsigned k = make_cont(CONT_IF, store(42), 0, 0);
    ASSERT_EQ(cont_type(k), CONT_IF);
    ASSERT_EQ(CELL_ID(cont_data(k)), 42);
    PASS();
}

TEST(make_halt_cont_test)
{
    unsigned k = make_halt_cont();
    ASSERT_EQ(cont_type(k), CONT_HALT);
    PASS();
}

TEST(code_free_unregisters_tree)
{
    code_object *parent = code_new();
    code_object *child = code_new();
    ASSERT(parent != NULL);
    ASSERT(child != NULL);
    code_add_child(parent, child);

    code_free(parent);

    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        ASSERT(code != parent);
        ASSERT(code != child);
    }
    PASS();
}

TEST(code_register_is_idempotent)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_register(code);
    code_free(code);
    ASSERT(!code_object_is_registered(code));
    PASS();
}

TEST(gc_collect_code_handles_cyclic_children)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_add_child(code, code);

    ASSERT_EQ(gc_collect_code(code), 0);
    code_free(code);
    ASSERT(!code_object_is_registered(code));
    PASS();
}

TEST(minor_gc_code_update_ignores_inconsistent_storage_metadata)
{
    code_object *code = code_new();
    ASSERT(code != NULL);

    unsigned saved_const_len = code->const_len;
    code->const_len = code->const_cap + 1;
    minor_gc_update_all_code_objects();
    code->const_len = saved_const_len;

    code_free(code);
    PASS();
}

TEST(code_free_handles_cyclic_and_duplicate_children)
{
    code_object *parent = code_new();
    code_object *child = code_new();
    ASSERT(parent != NULL);
    ASSERT(child != NULL);

    code_add_child(parent, parent);
    code_add_child(parent, child);
    code_add_child(parent, child);
    code_free(parent);

    ASSERT(!code_object_is_registered(parent));
    ASSERT(!code_object_is_registered(child));
    PASS();
}

TEST(code_free_preserves_shared_child)
{
    code_object *left = code_new();
    code_object *right = code_new();
    code_object *child = code_new();
    ASSERT(left != NULL && right != NULL && child != NULL);

    code_add_child(left, child);
    code_add_child(right, child);
    code_free(left);

    ASSERT(!code_object_is_registered(left));
    ASSERT(code_object_is_registered(right));
    ASSERT(code_object_is_registered(child));

    code_free(right);
    ASSERT(!code_object_is_registered(child));
    PASS();
}

static bool code_registry_contains(code_object *needle)
{
    for (code_object *code = code_object_registry; code; code = code->gc_next) {
        if (code == needle)
            return true;
    }
    return false;
}

static bool context_pattern_registry_contains(compiled_pattern *needle)
{
    for (compiled_pattern *pat = compiled_pattern_registry; pat;
         pat = pat->gc_next) {
        if (pat == needle)
            return true;
    }
    return false;
}

TEST(code_sweep_marks_vm_continuation_code)
{
    code_object *code = code_new();
    code_object *child = code_new();
    code_object *frame_code = code_new();
    ASSERT(code != NULL);
    ASSERT(child != NULL);
    ASSERT(frame_code != NULL);
    code_add_child(code, child);

    unsigned cont_cell = alloc();
    size_t block_size = sizeof(vm_continuation) + sizeof(vm_frame);
    vm_continuation *cont = checked_calloc_array(1, block_size);
    ASSERT(cont != NULL);
    cont->code = code;
    cont->fp = 1;
    cont->frames = (vm_frame *)((char *)cont + sizeof(vm_continuation));
    cont->frames[0].code = frame_code;
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    gc_sweep_code_objects();
    ASSERT(code_registry_contains(code));
    ASSERT(code_registry_contains(child));
    ASSERT(code_registry_contains(frame_code));

    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    code_free(code);
    code_free(frame_code);
    PASS();
}

TEST(code_sweep_ignores_unregistered_closure_pointer)
{
    code_object *unreferenced = code_new();
    ASSERT(unreferenced != NULL);

    unsigned marker = alloc();
    CELL_TYPE(marker) = BT_CLOSURE;
    CELL_PTR(marker) = (void *)(uintptr_t)1;

    gc_sweep_code_objects();

    ASSERT(!code_registry_contains(unreferenced));
    CELL_TYPE(marker) = BT_FREE;
    CELL_PTR(marker) = NULL;
    PASS();
}

// The sweep frees a dead code object without code_unregister. Its address
// must leave every membership cache, the recently-confirmed table included:
// malloc reuses addresses, and a new code object allocated there that still
// looked registered would never be added to the registry, so the collector
// would never trace its constants. (The pointer is only compared below.)
TEST(code_sweep_forgets_a_swept_object_it_had_confirmed)
{
    code_object *dead = code_new();
    ASSERT(dead != NULL);
    ASSERT(code_object_is_registered(dead)); // now a recent confirmation
    ASSERT(code_object_is_registered(dead));
    uintptr_t dead_addr = (uintptr_t)dead;

    gc_sweep_code_objects();

    ASSERT(!code_object_is_registered((const code_object *)dead_addr));
    PASS();
}

TEST(code_sweep_ignores_unregistered_vm_continuation_pointer)
{
    code_object *unreferenced = code_new();
    ASSERT(unreferenced != NULL);

    unsigned cont_cell = alloc();
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = (void *)(uintptr_t)1;

    gc_sweep_code_objects();

    ASSERT(!code_registry_contains(unreferenced));
    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    PASS();
}

TEST(gc_ignores_unregistered_vm_continuation_payload)
{
    unsigned cont_cell = alloc();
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = (void *)(uintptr_t)1;

    unsigned preserved = gc(cont_cell);
    ASSERT(CELL_TYPE(preserved) == BT_VMCONT);
    ASSERT(CELL_PTR(preserved) == (void *)(uintptr_t)1);

    CELL_TYPE(preserved) = BT_FREE;
    CELL_PTR(preserved) = NULL;
    PASS();
}

TEST(pattern_sweep_ignores_unregistered_pattern_pointer)
{
    compiled_pattern *unreferenced = compiled_pattern_new();
    ASSERT(unreferenced != NULL);

    unsigned cell = alloc();
    CELL_TYPE(cell) = BT_COMPILED_PATTERN;
    CELL_PTR(cell) = (void *)(uintptr_t)1;

    gc_sweep_patterns();

    ASSERT(!context_pattern_registry_contains(unreferenced));
    CELL_TYPE(cell) = BT_FREE;
    CELL_PTR(cell) = NULL;
    PASS();
}

TEST(gc_mark_pattern_ignores_unregistered_pointer)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    ASSERT(context_pattern_registry_contains(pat));

    gc_mark_pattern((compiled_pattern *)(uintptr_t)1);
    ASSERT(!pat->gc_marked);

    compiled_pattern_free(pat);
    PASS();
}

TEST(code_sweep_ignores_malformed_vm_continuation_frames)
{
    code_object *code = code_new();
    ASSERT(code != NULL);

    unsigned cont_cell = alloc();
    vm_continuation *cont = checked_calloc_array(1, sizeof(vm_continuation));
    ASSERT(cont != NULL);
    cont->code = code;
    cont->fp = UINT_MAX;
    cont->frames = NULL;
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    gc_sweep_code_objects();
    ASSERT(code_registry_contains(code));

    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    code_free(code);
    PASS();
}

TEST(peephole_optimize_ignores_truncated_instruction)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_JUMP);

    peephole_optimize(code);

    ASSERT_EQ(code->code_len, 1);
    ASSERT_EQ(code->code[0], OP_JUMP);

    code_free(code);
    PASS();
}

TEST(peephole_optimize_ignores_truncated_child_instruction)
{
    code_object *parent = code_new();
    code_object *child = code_new();
    ASSERT(parent != NULL);
    ASSERT(child != NULL);
    code_add_child(parent, child);
    code_emit(parent, OP_HALT);
    code_emit(child, OP_CONST);

    peephole_optimize(parent);

    ASSERT_EQ(parent->code_len, 1);
    ASSERT_EQ(parent->code[0], OP_HALT);
    ASSERT_EQ(child->code_len, 1);
    ASSERT_EQ(child->code[0], OP_CONST);

    code_free(parent);
    PASS();
}

TEST(peephole_optimize_handles_cyclic_children)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CONST);
    code_emit(code, 0);
    code_emit(code, OP_HALT);
    code_add_child(code, code);

    peephole_optimize(code);

    ASSERT(!code->optimizing);
    ASSERT_EQ(code->code_len, 3);
    ASSERT_EQ(code->code[0], OP_CONST);
    ASSERT_EQ(code->code[2], OP_HALT);
    code_free(code);
    PASS();
}

TEST(disassemble_handles_cyclic_children)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_HALT);
    code_add_child(code, code);

    disassemble(code, "cyclic-code");

    ASSERT(!code->disassembling);
    code_free(code);
    PASS();
}

TEST(peephole_optimize_preserves_recursive_call_operands)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CONST);
    code_emit(code, 0);
    code_emit(code, OP_POP);
    code_emit(code, OP_RECURSE);
    code_emit(code, 7);
    code_emit(code, OP_JUMP); // argument count happens to equal an opcode
    code_emit(code, 7);       // lexical depth must not be relocated
    code_emit(code, OP_HALT);
    peephole_optimize(code);
    ASSERT_EQ(code->code_len, 5);
    ASSERT_EQ(code->code[0], OP_RECURSE);
    ASSERT_EQ(code->code[1], 4);
    ASSERT_EQ(code->code[2], OP_JUMP);
    ASSERT_EQ(code->code[3], 7);
    ASSERT_EQ(code->code[4], OP_HALT);
    code_free(code);
    PASS();
}

TEST(peephole_optimize_preserves_primitive_guard_operands)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CONST);
    code_emit(code, 0);
    code_emit(code, OP_POP);
    code_emit(code, OP_GUARD_PRIMITIVE);
    code_emit(code, 11);
    code_emit(code, OP_JUMP); // symbol and primitive IDs are not instructions
    code_emit(code, OP_JUMP);
    code_emit(code, 2);
    code_emit(code, 1);
    code_emit(code, 0xFFFFFFFF);
    code_emit(code, 0xFFFFFFFF);
    code_emit(code, OP_HALT);
    peephole_optimize(code);
    ASSERT_EQ(code->code_len, 9);
    ASSERT_EQ(code->code[0], OP_GUARD_PRIMITIVE);
    ASSERT_EQ(code->code[1], 8);
    ASSERT_EQ(code->code[2], OP_JUMP);
    ASSERT_EQ(code->code[3], OP_JUMP);
    ASSERT_EQ(code->code[4], 2);
    ASSERT_EQ(code->code[5], 1);
    ASSERT_EQ(code->code[6], 0xFFFFFFFF);
    ASSERT_EQ(code->code[7], 0xFFFFFFFF);
    code_free(code);
    PASS();
}

TEST(peephole_optimize_does_not_thread_into_operand)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_JUMPIF);
    code_emit(code, 3);
    code_emit(code, OP_CONST);
    code_emit(code, OP_JUMP);

    peephole_optimize(code);

    ASSERT_EQ(code->code_len, 4);
    ASSERT_EQ(code->code[0], OP_JUMPIF);
    ASSERT_EQ(code->code[1], 3);
    ASSERT_EQ(code->code[2], OP_CONST);
    ASSERT_EQ(code->code[3], OP_JUMP);

    code_free(code);
    PASS();
}

TEST(peephole_optimize_keeps_cdr_of_cadr)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CADR);
    code_emit(code, OP_CDR);
    code_emit(code, OP_HALT);

    peephole_optimize(code);

    ASSERT_EQ(code->code_len, 3);
    ASSERT_EQ(code->code[0], OP_CADR);
    ASSERT_EQ(code->code[1], OP_CDR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_truncated_bytecode)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CONST);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_inconsistent_code_storage_metadata)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_HALT);

    unsigned saved_children_len = code->children_len;
    code->children_len = code->children_cap + 1;

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    code->children_len = saved_children_len;
    ASSERT_EQ(result, TOK_ERROR);

    unsigned *saved_code = code->code;
    code->code = NULL;
    vm_init(&vm);
    result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);
    code->code = saved_code;

    code_free(code);
    ASSERT_EQ(result, TOK_ERROR);
    PASS();
}

TEST(vm_run_rejects_invalid_constant_index)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CONST);
    code_emit(code, 99);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_unknown_opcode)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_COUNT);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_jump_into_operand)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    unsigned value_idx = code_add_const(code, store(1));
    code_emit(code, OP_JUMP);
    code_emit(code, 3);
    code_emit(code, OP_CONST);
    code_emit(code, value_idx);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_invalid_closure_child_index)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CLOSURE);
    code_emit(code, 0);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_primitive_stack_underflow)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_PRIM);
    code_emit(code, PPLUS);
    code_emit(code, 1);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_stack_underflow_does_not_mutate_environment)
{
    unsigned env = empty_environment();
    unsigned existing = atom_from_string("underflow-existing");
    unsigned new_name = atom_from_string("underflow-new");
    defvar(existing, store(17), env);

    code_object *set_code = code_new();
    ASSERT(set_code != NULL);
    code_emit(set_code, OP_SET);
    code_emit(set_code, (unsigned)CELL_ID(existing));
    code_emit(set_code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    ASSERT_EQ(vm_run(&vm, set_code, env), TOK_ERROR);
    vm_free(&vm);
    ASSERT_EQ(CELL_ID(lookup(CELL_ID(existing), env)), 17);
    code_free(set_code);

    code_object *define_code = code_new();
    ASSERT(define_code != NULL);
    code_emit(define_code, OP_DEFINE);
    code_emit(define_code, (unsigned)CELL_ID(new_name));
    code_emit(define_code, OP_HALT);

    vm_init(&vm);
    ASSERT_EQ(vm_run(&vm, define_code, env), TOK_ERROR);
    vm_free(&vm);
    ASSERT_EQ(lookup(CELL_ID(new_name), env), TOK_ERROR);
    code_free(define_code);
    PASS();
}

TEST(vm_run_rejects_local_get_out_of_bounds)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_LOCAL_GET);
    code_emit(code, 0);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_local_get_fast_out_of_bounds)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_LOCAL_GET3);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_run_rejects_local_set_out_of_bounds)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    unsigned value_idx = code_add_const(code, store(1));
    code_emit(code, OP_CONST);
    code_emit(code, value_idx);
    code_emit(code, OP_LOCAL_SET);
    code_emit(code, 1);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

#define TEST_IC_UNCACHED 0xFFFFFFFF

static code_object *make_test_lookup_code(int64_t sym_id, unsigned depth,
                                          unsigned offset)
{
    code_object *code = code_new();
    if (!code)
        return NULL;
    code_emit(code, OP_LOOKUP);
    code_emit(code, (unsigned)sym_id);
    code_emit(code, depth);
    code_emit(code, offset);
    code_emit(code, OP_HALT);
    return code;
}

static unsigned run_unary_opcode_on_const(unsigned opcode, unsigned value)
{
    code_object *code = code_new();
    if (!code)
        return TOK_ERROR;
    unsigned idx = code_add_const(code, value);
    code_emit(code, OP_CONST);
    code_emit(code, idx);
    code_emit(code, opcode);
    code_emit(code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, empty_environment());
    vm_free(&vm);
    code_free(code);
    return result;
}

TEST(vm_unary_numeric_ops_reject_malformed_bignum)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_BIGNUM;
    CELL_PTR(bad) = NULL;

    unsigned opcodes[] = {OP_ADD1, OP_SUB1, OP_ABS};
    for (size_t i = 0; i < sizeof(opcodes) / sizeof(opcodes[0]); i++) {
        ASSERT_EQ(run_unary_opcode_on_const(opcodes[i], bad), TOK_ERROR);
    }
    CELL_PTR(bad) = (void *)(uintptr_t)1;
    for (size_t i = 0; i < sizeof(opcodes) / sizeof(opcodes[0]); i++) {
        ASSERT_EQ(run_unary_opcode_on_const(opcodes[i], bad), TOK_ERROR);
    }
    PASS();
}

TEST(vm_vector_ops_reject_malformed_payload)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_VECTOR;
    CELL_PTR(bad) = NULL;

    ASSERT_EQ(run_unary_opcode_on_const(OP_VECTORLEN, bad), TOK_ERROR);

    code_object *ref_code = code_new();
    ASSERT(ref_code != NULL);
    unsigned bad_idx = code_add_const(ref_code, bad);
    unsigned zero_idx = code_add_const(ref_code, store(0));
    code_emit(ref_code, OP_CONST);
    code_emit(ref_code, bad_idx);
    code_emit(ref_code, OP_CONST);
    code_emit(ref_code, zero_idx);
    code_emit(ref_code, OP_VECTORREF);
    code_emit(ref_code, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, ref_code, empty_environment());
    vm_free(&vm);
    code_free(ref_code);
    ASSERT_EQ(result, TOK_ERROR);

    code_object *set_code = code_new();
    ASSERT(set_code != NULL);
    bad_idx = code_add_const(set_code, bad);
    zero_idx = code_add_const(set_code, store(0));
    unsigned value_idx = code_add_const(set_code, store(1));
    code_emit(set_code, OP_CONST);
    code_emit(set_code, bad_idx);
    code_emit(set_code, OP_CONST);
    code_emit(set_code, zero_idx);
    code_emit(set_code, OP_CONST);
    code_emit(set_code, value_idx);
    code_emit(set_code, OP_VECTORSET);
    code_emit(set_code, OP_HALT);

    vm_init(&vm);
    result = vm_run(&vm, set_code, empty_environment());
    vm_free(&vm);
    code_free(set_code);
    ASSERT_EQ(result, TOK_ERROR);

    PASS();
}

TEST(vm_lookup_rejects_malformed_environment)
{
    unsigned sym = atom_from_string("malformed-env-var");
    code_object *code =
        make_test_lookup_code(CELL_ID(sym), TEST_IC_UNCACHED,
                              TEST_IC_UNCACHED);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, store(1));
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_lookup_rejects_malformed_frame)
{
    unsigned sym = atom_from_string("malformed-frame-var");
    code_object *code =
        make_test_lookup_code(CELL_ID(sym), TEST_IC_UNCACHED,
                              TEST_IC_UNCACHED);
    unsigned env = alloc_cons(store(1), 0);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_lookup_rejects_malformed_frame_variables)
{
    unsigned sym = atom_from_string("malformed-vars-var");
    code_object *code =
        make_test_lookup_code(CELL_ID(sym), TEST_IC_UNCACHED,
                              TEST_IC_UNCACHED);
    unsigned frame = alloc_cons(store(1), 0);
    unsigned env = alloc_cons(frame, 0);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_lookup_ignores_stale_cached_depth)
{
    unsigned env = empty_environment();
    unsigned sym = atom_from_string("stale-depth-var");
    unsigned val = store(42);
    defvar(sym, val, env);
    code_object *code = make_test_lookup_code(CELL_ID(sym), 99, 0);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);

    ASSERT(result != TOK_ERROR);
    ASSERT_EQ(CELL_ID(result), 42);
    code_free(code);
    PASS();
}

TEST(vm_lookup_ignores_stale_cached_offset)
{
    unsigned env = empty_environment();
    unsigned sym = atom_from_string("stale-offset-var");
    unsigned val = store(43);
    defvar(sym, val, env);
    code_object *code = make_test_lookup_code(CELL_ID(sym), 0, 99);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);

    ASSERT(result != TOK_ERROR);
    ASSERT_EQ(CELL_ID(result), 43);
    code_free(code);
    PASS();
}

// The inline cache (vm.c, ic_lookup) remembers where a lookup instruction
// last found its binding and trusts that while env_binding_stamp() is
// unchanged. The tests below run one lookup instruction against several
// environments shaped the way activations of one piece of code are - the
// same frames, differing only in what internal defines added to them - and
// change bindings between runs, so each reuse of a cache entry is checked
// against the answer a full lookup gives.

static unsigned run_test_lookup(code_object *code, unsigned env)
{
    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);
    return result;
}

static int64_t test_lookup_int(code_object *code, unsigned env)
{
    unsigned result = run_test_lookup(code, env);
    if (result == TOK_ERROR)
        return -1;
    if (IS_FIXNUM(result))
        return FIXNUM_VALUE(result);
    return IS_CELL(result) && CELL_TYPE(result) == BT_NUM ? CELL_ID(result)
                                                         : -2;
}

// A new activation frame binding var to val, in front of env.
static unsigned push_test_frame(unsigned var, unsigned val, unsigned env)
{
    GC_GUARD;
    gc_protect(&var);
    gc_protect(&val);
    gc_protect(&env);
    unsigned vars = alloc_cons(var, 0);
    gc_protect(&vars);
    unsigned vals = alloc_cons(val, 0);
    gc_protect(&vals);
    unsigned frame = alloc_cons(vars, vals);
    gc_protect(&frame);
    return alloc_cons(frame, env);
}

TEST(vm_lookup_cache_sees_define_in_frame_it_passed)
{
    GC_GUARD;
    unsigned x = atom_from_string("ic-shadowed-later");
    unsigned y = atom_from_string("ic-frame-var");
    gc_protect(&x);
    gc_protect(&y);
    unsigned global = empty_environment();
    gc_protect(&global);
    defvar(x, store(1), global);
    unsigned env = push_test_frame(y, store(0), global);
    gc_protect(&env);
    code_object *code = make_test_lookup_code(CELL_ID(x), TEST_IC_UNCACHED,
                                              TEST_IC_UNCACHED);

    ASSERT_EQ(test_lookup_int(code, env), 1);
    ASSERT_EQ(test_lookup_int(code, env), 1);
    // An internal define after the instruction cached the outer binding.
    defvar(x, store(2), env);
    ASSERT_EQ(test_lookup_int(code, env), 2);
    // Redefining in place and assigning reach the cached cell.
    defvar(x, store(3), env);
    ASSERT_EQ(test_lookup_int(code, env), 3);
    setvar(CELL_ID(x), store(4), env);
    ASSERT_EQ(test_lookup_int(code, env), 4);
    code_free(code);
    PASS();
}

TEST(vm_lookup_cache_searches_activations_with_conditional_defines)
{
    // (define (make flag) (if flag (define x 'local)) (lambda () x)):
    // whether an activation's frame binds x depends on the path it took, so
    // a binding cached from one activation says nothing about another's.
    GC_GUARD;
    unsigned x = atom_from_string("ic-conditional-define");
    unsigned y = atom_from_string("ic-conditional-flag");
    gc_protect(&x);
    gc_protect(&y);
    unsigned global = empty_environment();
    gc_protect(&global);
    defvar(x, store(1), global);
    unsigned with_x = push_test_frame(y, ctx.atom_true, global);
    gc_protect(&with_x);
    defvar(x, store(2), with_x);
    unsigned without_x = push_test_frame(y, ctx.atom_false, global);
    gc_protect(&without_x);
    code_object *code = make_test_lookup_code(CELL_ID(x), TEST_IC_UNCACHED,
                                              TEST_IC_UNCACHED);

    // Fill from the activation without the define, after it happened in
    // the other one: the stamp alone cannot tell the two apart.
    ASSERT_EQ(test_lookup_int(code, without_x), 1);
    ASSERT_EQ(test_lookup_int(code, with_x), 2);
    ASSERT_EQ(test_lookup_int(code, without_x), 1);
    ASSERT_EQ(test_lookup_int(code, with_x), 2);
    unsigned third = push_test_frame(y, ctx.atom_false, global);
    gc_protect(&third);
    ASSERT_EQ(test_lookup_int(code, third), 1);
    defvar(x, store(3), third);
    ASSERT_EQ(test_lookup_int(code, third), 3);
    ASSERT_EQ(test_lookup_int(code, without_x), 1);
    code_free(code);
    PASS();
}

TEST(vm_lookup_cache_global_seen_from_every_activation)
{
    // A binding found in the outermost frame is reused from any other
    // activation without walking to it - until the name is defined in a
    // nested frame somewhere, which changes its stamp.
    GC_GUARD;
    unsigned x = atom_from_string("ic-global-from-activations");
    unsigned y = atom_from_string("ic-global-activation-param");
    gc_protect(&x);
    gc_protect(&y);
    unsigned global = empty_environment();
    gc_protect(&global);
    defvar(x, store(1), global);
    unsigned a = push_test_frame(y, store(0), global);
    gc_protect(&a);
    unsigned b = push_test_frame(y, store(0), global);
    gc_protect(&b);
    code_object *code = make_test_lookup_code(CELL_ID(x), TEST_IC_UNCACHED,
                                              TEST_IC_UNCACHED);

    ASSERT_EQ(test_lookup_int(code, a), 1);
    ASSERT_EQ(test_lookup_int(code, b), 1);
    setvar(CELL_ID(x), store(2), global);
    ASSERT_EQ(test_lookup_int(code, a), 2);
    ASSERT_EQ(test_lookup_int(code, b), 2);
    defvar(x, store(3), global); // redefinition assigns in place
    ASSERT_EQ(test_lookup_int(code, b), 3);
    defvar(x, store(10), b); // an internal define in one activation
    ASSERT_EQ(test_lookup_int(code, b), 10);
    ASSERT_EQ(test_lookup_int(code, a), 3);
    unsigned c = push_test_frame(y, store(0), global);
    gc_protect(&c);
    ASSERT_EQ(test_lookup_int(code, c), 3);
    ASSERT_EQ(test_lookup_int(code, b), 10);
    c = gc(c);
    ASSERT_EQ(test_lookup_int(code, c), 3);
    ASSERT_EQ(test_lookup_int(code, a), 3);
    code_free(code);
    PASS();
}

TEST(vm_lookup_cache_rechecks_offset_in_other_activation)
{
    GC_GUARD;
    unsigned n = atom_from_string("ic-activation-param");
    unsigned z = atom_from_string("ic-prepended-define");
    gc_protect(&n);
    gc_protect(&z);
    unsigned global = empty_environment();
    gc_protect(&global);
    unsigned a = push_test_frame(n, store(10), global);
    gc_protect(&a);
    unsigned b = push_test_frame(n, store(20), global);
    gc_protect(&b);
    code_object *code = make_test_lookup_code(CELL_ID(n), TEST_IC_UNCACHED,
                                              TEST_IC_UNCACHED);

    ASSERT_EQ(test_lookup_int(code, a), 10);
    ASSERT_EQ(test_lookup_int(code, b), 20);
    ASSERT_EQ(test_lookup_int(code, a), 10);
    // A define in one activation moves its parameter down the frame.
    defvar(z, store(0), b);
    ASSERT_EQ(test_lookup_int(code, b), 20);
    ASSERT_EQ(test_lookup_int(code, a), 10);
    ASSERT_EQ(test_lookup_int(code, b), 20);
    code_free(code);
    PASS();
}

TEST(vm_lookup_cache_define_in_fresh_frame_keeps_stamp)
{
    // A let body defines its variables into a frame pushed just before.
    // Once the name has been defined in a nested frame at all, doing so in
    // a frame no cache entry has seen leaves the stamp alone - and a lookup
    // that later meets that frame searches it.
    GC_GUARD;
    unsigned x = atom_from_string("ic-let-bound");
    gc_protect(&x);
    unsigned global = empty_environment();
    gc_protect(&global);
    defvar(x, store(1), global);
    unsigned older = extend_env_empty(global);
    gc_protect(&older);
    defvar(x, store(5), older); // the name is now defined in a nested frame
    code_object *code = make_test_lookup_code(CELL_ID(x), TEST_IC_UNCACHED,
                                              TEST_IC_UNCACHED);
    unsigned first = extend_env_empty(global);
    gc_protect(&first);
    ASSERT_EQ(test_lookup_int(code, first), 1);

    unsigned fresh = extend_env_empty(global);
    gc_protect(&fresh);
    uint64_t stamp = env_binding_stamp(CELL_ID(x));
    defvar(x, store(2), fresh);
    ASSERT(env_binding_stamp(CELL_ID(x)) == stamp);
    ASSERT_EQ(test_lookup_int(code, fresh), 2);
    ASSERT_EQ(test_lookup_int(code, first), 1);
    ASSERT_EQ(test_lookup_int(code, older), 5);
    // first has been seen by the cache now, so defining into it must
    // change the stamp.
    defvar(x, store(3), first);
    ASSERT(env_binding_stamp(CELL_ID(x)) != stamp);
    ASSERT_EQ(test_lookup_int(code, first), 3);
    ASSERT_EQ(test_lookup_int(code, fresh), 2);
    code_free(code);
    PASS();
}

TEST(vm_lookup_cache_is_dropped_by_collections)
{
    GC_GUARD;
    unsigned x = atom_from_string("ic-moved-binding");
    unsigned y = atom_from_string("ic-moved-frame-var");
    gc_protect(&x);
    gc_protect(&y);
    unsigned global = empty_environment();
    gc_protect(&global);
    defvar(x, store(1), global);
    unsigned env = push_test_frame(y, store(0), global);
    gc_protect(&env);
    code_object *code = make_test_lookup_code(CELL_ID(x), TEST_IC_UNCACHED,
                                              TEST_IC_UNCACHED);

    ASSERT_EQ(test_lookup_int(code, env), 1);
    uint64_t stamp = env_binding_stamp(CELL_ID(x));
    env = minor_gc(env);
    ASSERT(env_binding_stamp(CELL_ID(x)) != stamp);
    setvar(CELL_ID(x), store(2), env);
    ASSERT_EQ(test_lookup_int(code, env), 2);
    env = gc(env);
    setvar(CELL_ID(x), store(3), env);
    ASSERT_EQ(test_lookup_int(code, env), 3);
    defvar(x, store(4), env);
    env = gc(env);
    ASSERT_EQ(test_lookup_int(code, env), 4);
    code_free(code);
    PASS();
}

TEST(vm_lookup_cache_follows_binding_references)
{
    GC_GUARD;
    unsigned target = atom_from_string("ic-alias-target");
    unsigned alias = atom_from_string("ic-alias");
    gc_protect(&target);
    gc_protect(&alias);
    unsigned env = empty_environment();
    gc_protect(&env);
    defvar(target, store(1), env);
    unsigned cell = env_find_binding_cell(CELL_ID(target), env);
    defvar_alias(alias, target, cell, env);
    code_object *code = make_test_lookup_code(CELL_ID(alias), TEST_IC_UNCACHED,
                                              TEST_IC_UNCACHED);

    ASSERT_EQ(test_lookup_int(code, env), 1);
    setvar(CELL_ID(target), store(2), env);
    ASSERT_EQ(test_lookup_int(code, env), 2);
    setvar(CELL_ID(alias), store(3), env); // writes through to the target
    ASSERT_EQ(test_lookup_int(code, env), 3);
    defvar(alias, store(4), env); // replaces the reference itself
    ASSERT_EQ(test_lookup_int(code, env), 4);
    setvar(CELL_ID(target), store(5), env);
    ASSERT_EQ(test_lookup_int(code, env), 4);
    code_free(code);
    PASS();
}

TEST(vm_lookup_cache_entry_belongs_to_one_instruction)
{
    // Cache words are plain operands: copying an instruction copies them.
    // An entry is only used by the instruction it was made for.
    GC_GUARD;
    unsigned x = atom_from_string("ic-owner-x");
    unsigned y = atom_from_string("ic-owner-y");
    gc_protect(&x);
    gc_protect(&y);
    unsigned env = empty_environment();
    gc_protect(&env);
    defvar(x, store(1), env);
    defvar(y, store(2), env);
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_LOOKUP);
    code_emit(code, (unsigned)CELL_ID(x));
    code_emit(code, TEST_IC_UNCACHED);
    code_emit(code, TEST_IC_UNCACHED);
    code_emit(code, OP_POP);
    code_emit(code, OP_LOOKUP);
    code_emit(code, (unsigned)CELL_ID(y));
    code_emit(code, TEST_IC_UNCACHED);
    code_emit(code, TEST_IC_UNCACHED);
    code_emit(code, OP_HALT);

    ASSERT_EQ(test_lookup_int(code, env), 2);
    code->code[7] = code->code[2];
    code->code[8] = code->code[3];
    ASSERT_EQ(test_lookup_int(code, env), 2);
    ASSERT_EQ(test_lookup_int(code, env), 2);
    code_free(code);
    PASS();
}

static unsigned make_test_bytecode_closure(code_object *code, unsigned env)
{
    unsigned marker = alloc();
    CELL_TYPE(marker) = BT_CLOSURE;
    CELL_PTR(marker) = code;
    gc_protect(&marker);
    unsigned closure = alloc_cons(marker, env);
    gc_unprotect(1);
    return closure;
}

TEST(vm_call_closure_rejects_invalid_bytecode)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CONST);
    code->arity = 0;

    unsigned closure = make_test_bytecode_closure(code, empty_environment());
    unsigned result = vm_call_closure(closure, 0);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

TEST(vm_call_closure_rejects_invalid_parameter_metadata)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_HALT);
    code->arity = 0;
    code->use_locals = false;
    code->params = 99;

    unsigned closure = make_test_bytecode_closure(code, empty_environment());
    unsigned result = vm_call_closure(closure, 0);

    ASSERT_EQ(result, TOK_ERROR);
    code_free(code);
    PASS();
}

static code_object *make_test_continuation_call_code(unsigned cont_cell)
{
    code_object *code = code_new();
    if (!code)
        return NULL;
    unsigned cont_idx = code_add_const(code, cont_cell);
    code_emit(code, OP_CONST);
    code_emit(code, cont_idx);
    code_emit(code, OP_CALL);
    code_emit(code, 0);
    code_emit(code, OP_HALT);
    return code;
}

TEST(vm_continuation_rejects_oversized_stack)
{
    code_object *restore_code = code_new();
    ASSERT(restore_code != NULL);
    code_emit(restore_code, OP_HALT);

    unsigned cont_cell = alloc();
    vm_continuation *cont = checked_calloc_array(1, sizeof(vm_continuation));
    ASSERT(cont != NULL);
    cont->sp = UINT_MAX;
    cont->code = restore_code;
    cont->ip = 0;
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    code_object *call_code = make_test_continuation_call_code(cont_cell);
    ASSERT(call_code != NULL);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, call_code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    code_free(call_code);
    code_free(restore_code);
    PASS();
}

TEST(vm_continuation_rejects_invalid_base_pointer)
{
    code_object *restore_code = code_new();
    ASSERT(restore_code != NULL);
    code_emit(restore_code, OP_RETURN_LOCALS);
    code_emit(restore_code, 0);

    unsigned cont_cell = alloc();
    vm_continuation *cont = checked_calloc_array(1, sizeof(vm_continuation));
    ASSERT(cont != NULL);
    cont->bp = UINT_MAX;
    cont->code = restore_code;
    cont->ip = 0;
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    code_object *call_code = make_test_continuation_call_code(cont_cell);
    ASSERT(call_code != NULL);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, call_code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    code_free(call_code);
    code_free(restore_code);
    PASS();
}

TEST(vm_continuation_rejects_invalid_frame_state)
{
    code_object *restore_code = code_new();
    code_object *frame_code = code_new();
    ASSERT(restore_code != NULL);
    ASSERT(frame_code != NULL);
    code_emit(restore_code, OP_RETURN);
    code_emit(frame_code, OP_HALT);

    unsigned cont_cell = alloc();
    size_t block_size = sizeof(vm_continuation) + sizeof(vm_frame);
    vm_continuation *cont = checked_calloc_array(1, block_size);
    ASSERT(cont != NULL);
    cont->code = restore_code;
    cont->ip = 0;
    cont->fp = 1;
    cont->frames = (vm_frame *)((char *)cont + sizeof(vm_continuation));
    cont->frames[0].code = frame_code;
    cont->frames[0].ip = 0;
    cont->frames[0].sp = UINT_MAX;
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    code_object *call_code = make_test_continuation_call_code(cont_cell);
    ASSERT(call_code != NULL);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, call_code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    code_free(call_code);
    code_free(restore_code);
    code_free(frame_code);
    PASS();
}

TEST(vm_continuation_rejects_oversized_frame_stack)
{
    code_object *restore_code = code_new();
    ASSERT(restore_code != NULL);
    code_emit(restore_code, OP_HALT);

    unsigned cont_cell = alloc();
    vm_continuation *cont = checked_calloc_array(1, sizeof(vm_continuation));
    ASSERT(cont != NULL);
    cont->fp = UINT_MAX;
    cont->code = restore_code;
    cont->ip = 0;
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    code_object *call_code = make_test_continuation_call_code(cont_cell);
    ASSERT(call_code != NULL);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, call_code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    code_free(call_code);
    code_free(restore_code);
    PASS();
}

TEST(vm_continuation_rejects_truncated_letrec_values)
{
    code_object *restore_code = code_new();
    ASSERT(restore_code != NULL);
    code_emit(restore_code, OP_HALT);

    unsigned frame = alloc_cons(0, 0);
    unsigned letrec_frame = alloc_cons(frame, 0);
    unsigned cont_cell = alloc();
    size_t block_size = sizeof(vm_continuation) + sizeof(vm_letrec_state) +
                        sizeof(unsigned);
    vm_continuation *cont = checked_calloc_array(1, block_size);
    ASSERT(cont != NULL);
    cont->code = restore_code;
    cont->ip = 0;
    cont->letrec_depth = 1;
    cont->letrecs =
        (vm_letrec_state *)((char *)cont + sizeof(vm_continuation));
    cont->letrecs[0].frame = letrec_frame;
    cont->letrecs[0].count = 1;
    cont->letrec_saved_len = 1;
    cont->letrec_saved =
        (unsigned *)((char *)cont + sizeof(vm_continuation) +
                     sizeof(vm_letrec_state));
    cont->letrec_saved[0] = store(42);
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    code_object *call_code = make_test_continuation_call_code(cont_cell);
    ASSERT(call_code != NULL);

    vm_state vm;
    vm_init(&vm);
    unsigned result = vm_run(&vm, call_code, empty_environment());
    vm_free(&vm);

    ASSERT_EQ(result, TOK_ERROR);
    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    code_free(call_code);
    code_free(restore_code);
    PASS();
}

TEST(vm_call_with_values_rejects_cyclic_multiple_values)
{
    unsigned value_list = alloc_cons(store(1), 0);
    cell_set_cdr(value_list, value_list);
    unsigned multiple_values = alloc();
    CELL_TYPE(multiple_values) = BT_MULTIVAL;
    CELL_CAR(multiple_values) = value_list;

    code_object *producer = code_new();
    code_object *caller = code_new();
    ASSERT(producer != NULL && caller != NULL);
    unsigned producer_const = code_add_const(producer, multiple_values);
    code_emit(producer, OP_CONST);
    code_emit(producer, producer_const);
    code_emit(producer, OP_RETURN);

    unsigned consumer = alloc();
    CELL_TYPE(consumer) = BT_BUILTIN;
    CELL_ID(consumer) = PPLUS;
    unsigned consumer_const = code_add_const(caller, consumer);
    code_add_child(caller, producer);
    code_emit(caller, OP_CLOSURE);
    code_emit(caller, 0);
    code_emit(caller, OP_CONST);
    code_emit(caller, consumer_const);
    code_emit(caller, OP_CALLWITHVALUES);
    code_emit(caller, OP_HALT);

    vm_state vm;
    vm_init(&vm);
    ASSERT_EQ(vm_run(&vm, caller, empty_environment()), TOK_ERROR);
    vm_free(&vm);
    code_free(caller);
    PASS();
}

TEST(execute_pattern_rejects_unknown_opcode)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_OPCODE_COUNT, 0);

    unsigned result = execute_pattern(pat, 0);

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_invalid_literal_index)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_MATCH_LITERAL, 0);
    pattern_emit(pat, PAT_SUCCESS, 0);

    unsigned result = execute_pattern(pat, store(1));

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_invalid_binding_slot)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_BIND_VAR, 0);
    pattern_emit(pat, PAT_SUCCESS, 0);

    unsigned result = execute_pattern(pat, store(1));

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_input_stack_underflow)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_INPUT_POP, 0);
    pattern_emit(pat, PAT_SUCCESS, 0);

    ASSERT_EQ(execute_pattern(pat, store(1)), TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_inconsistent_storage_metadata)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_SUCCESS, 0);

    unsigned saved_var_count = pat->var_count;
    pat->var_count = pat->var_cap + 1;
    unsigned result = execute_pattern(pat, 0);
    pat->var_count = saved_var_count;

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(gc_pattern_update_ignores_inconsistent_storage_metadata)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);

    unsigned saved_var_count = pat->var_count;
    pat->var_count = pat->var_cap + 1;
    gc_update_all_patterns();
    pat->var_count = saved_var_count;

    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_invalid_jump_target)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_JUMP, 99);

    unsigned result = execute_pattern(pat, 0);

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_input_car_on_non_pair)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_INPUT_CAR, 0);
    pattern_emit(pat, PAT_SUCCESS, 0);

    unsigned result = execute_pattern(pat, store(1));

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_vector_iteration_on_non_vector)
{
    compiled_pattern *pat = compiled_pattern_new();
    ASSERT(pat != NULL);
    pattern_emit(pat, PAT_VEC_ELLIPSIS_INIT, 0);
    pattern_emit(pat, PAT_SUCCESS, 0);

    unsigned result = execute_pattern(pat, store(1));

    ASSERT_EQ(result, TOK_ERROR);
    compiled_pattern_free(pat);
    PASS();
}

TEST(execute_pattern_rejects_malformed_vector_payload)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_VECTOR;
    CELL_PTR(bad) = NULL;

    unsigned opcodes[] = {PAT_CHECK_VECTOR, PAT_CHECK_VECLEN,
                          PAT_CHECK_VECLEN_MIN, PAT_INPUT_VECREF,
                          PAT_VEC_ELLIPSIS_INIT, PAT_INPUT_VECREF_END};
    for (size_t i = 0; i < sizeof(opcodes) / sizeof(opcodes[0]); i++) {
        compiled_pattern *pat = compiled_pattern_new();
        ASSERT(pat != NULL);
        pattern_emit(pat, opcodes[i], 0);
        pattern_emit(pat, PAT_SUCCESS, 0);
        ASSERT_EQ(execute_pattern(pat, bad), TOK_ERROR);
        compiled_pattern_free(pat);
    }

    compiled_pattern *iter_pat = compiled_pattern_new();
    ASSERT(iter_pat != NULL);
    pattern_emit(iter_pat, PAT_INPUT_VEC_ITER, 0);
    pattern_emit(iter_pat, PAT_SUCCESS, 0);
    ASSERT_EQ(execute_pattern(iter_pat, bad), TOK_ERROR);
    compiled_pattern_free(iter_pat);

    PASS();
}

TEST(gc_updates_vm_continuation_letrec_roots)
{
    unsigned saved = alloc();
    CELL_TYPE(saved) = BT_STRING;
    CELL_PTR(saved) = checked_string_copy("saved");
    ASSERT(CELL_PTR(saved) != NULL);
    string_register(GET_STRING_PTR(saved));

    GC_GUARD;
    gc_protect(&saved);
    unsigned vals = alloc_cons(saved, 0);
    gc_protect(&vals);
    unsigned frame = alloc_cons(0, vals);
    gc_protect(&frame);
    unsigned letrec_frame = alloc_cons(frame, 0);
    gc_protect(&letrec_frame);

    unsigned cont_cell = alloc();
    size_t block_size = sizeof(vm_continuation) + sizeof(vm_letrec_state) +
                        sizeof(unsigned);
    vm_continuation *cont = checked_calloc_array(1, block_size);
    ASSERT(cont != NULL);
    cont->letrec_depth = 1;
    cont->letrecs =
        (vm_letrec_state *)((char *)cont + sizeof(vm_continuation));
    cont->letrecs[0].frame = letrec_frame;
    cont->letrecs[0].count = 1;
    cont->letrec_saved_len = 1;
    cont->letrec_saved =
        (unsigned *)((char *)cont + sizeof(vm_continuation) +
                     sizeof(vm_letrec_state));
    cont->letrec_saved[0] = saved;
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    cont_cell = gc(cont_cell);
    cont = (vm_continuation *)CELL_PTR(cont_cell);
    ASSERT(CELL_TYPE(cont->letrecs[0].frame) == BT_CONS);
    ASSERT(CELL_TYPE(cont->letrec_saved[0]) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(cont->letrec_saved[0]), "saved");
    PASS();
}

TEST(gc_ignores_malformed_vm_continuation_arrays)
{
    unsigned env_marker = alloc();
    CELL_TYPE(env_marker) = BT_STRING;
    CELL_PTR(env_marker) = checked_string_copy("env-marker");
    ASSERT(CELL_PTR(env_marker) != NULL);
    string_register(GET_STRING_PTR(env_marker));

    unsigned cont_cell = alloc();
    size_t block_size = sizeof(vm_continuation) + sizeof(vm_letrec_state);
    vm_continuation *cont = checked_calloc_array(1, block_size);
    ASSERT(cont != NULL);
    cont->sp = UINT_MAX;
    cont->fp = UINT_MAX;
    cont->letrec_depth = UINT_MAX;
    cont->letrecs =
        (vm_letrec_state *)((char *)cont + sizeof(vm_continuation));
    cont->letrecs[0].frame = env_marker;
    cont->letrec_saved_len = UINT_MAX;
    cont->letrec_saved = (unsigned *)cont;
    cont->env = env_marker;
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    cont_cell = gc(cont_cell);
    ASSERT(CELL_TYPE(cont_cell) == BT_VMCONT);
    cont = (vm_continuation *)CELL_PTR(cont_cell);
    ASSERT(cont != NULL);
    ASSERT(CELL_TYPE(cont->env) == BT_STRING);

    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    PASS();
}

TEST(vm_continuation_capture_copies_after_gc)
{
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_PUSHCONT);
    code_emit(code, OP_HALT);

    unsigned env = empty_environment();
    unsigned old_env = env;
    unsigned handler = make_string_copy("handler");
    ASSERT(IS_STRING(handler));

    if (ctx.card_table) {
        unsigned nursery_end =
            (ctx.mmin < SEMISPACE_SIZE) ? SEMISPACE_SIZE : 2 * SEMISPACE_SIZE;
        ctx.nursery_ptr = nursery_end;
    }

    vm_state vm;
    vm_init(&vm);
    vm.signal_handler = handler;
    unsigned result = vm_run(&vm, code, env);
    vm_free(&vm);

    ASSERT(CELL_TYPE(result) == BT_VMCONT);
    vm_continuation *cont = (vm_continuation *)CELL_PTR(result);
    ASSERT(cont != NULL);
    ASSERT(cont->env != old_env);
    ASSERT(CELL_TYPE(cont->env) == BT_CONS);
    ASSERT(CELL_TYPE(cont->signal_handler) == BT_STRING);
    ASSERT_STR_EQ(GET_STRING_PTR(cont->signal_handler), "handler");

    vm_continuation_unregister(cont);
    free(cont);
    CELL_TYPE(result) = BT_FREE;
    CELL_PTR(result) = NULL;
    code_free(code);
    PASS();
}

TEST(gc_updates_vm_signal_handler_root)
{
    vm_state vm = {0};
    test_gc_collector_from = store(41);
    test_gc_collector_to = store(42);
    vm.signal_handler = test_gc_collector_from;

    gc_update_vm_roots_minor(&vm, test_gc_forward_one);

    ASSERT_EQ(vm.signal_handler, test_gc_collector_to);
    PASS();
}

TEST(vm_continuation_restores_signal_handler)
{
    code_object *restore_code = code_new();
    ASSERT(restore_code != NULL);
    code_emit(restore_code, OP_HALT);

    unsigned cont_cell = alloc();
    vm_continuation *cont = checked_calloc_array(1, sizeof(vm_continuation));
    ASSERT(cont != NULL);
    cont->code = restore_code;
    cont->ip = 0;
    cont->signal_handler = store(41);
    vm_continuation_register(cont);
    CELL_TYPE(cont_cell) = BT_VMCONT;
    CELL_PTR(cont_cell) = cont;

    code_object *call_code = make_test_continuation_call_code(cont_cell);
    ASSERT(call_code != NULL);

    vm_state vm;
    vm_init(&vm);
    vm.signal_handler = store(42);
    unsigned result = vm_run(&vm, call_code, empty_environment());

    ASSERT(result != TOK_ERROR);
    ASSERT_EQ(vm.signal_handler, store(41));
    vm_free(&vm);
    CELL_TYPE(cont_cell) = BT_FREE;
    CELL_PTR(cont_cell) = NULL;
    vm_continuation_unregister(cont);
    free(cont);
    code_free(call_code);
    code_free(restore_code);
    PASS();
}

TEST(gc_updates_active_vm_letrec_roots)
{
    vm_state vm = {0};
    vm_letrec_state letrecs[2] = {{0}};
    test_gc_collector_from = store(51);
    test_gc_collector_to = store(52);
    letrecs[0].frame = test_gc_collector_from;
    letrecs[0].count = 3;
    letrecs[1].frame = store(53);
    letrecs[1].count = 1;
    vm.letrecs = letrecs;
    vm.letrec_depth = 2;

    gc_update_vm_roots_minor(&vm, test_gc_forward_one);

    ASSERT_EQ(vm.letrecs[0].frame, test_gc_collector_to);
    ASSERT_EQ(vm.letrecs[0].count, 3);
    ASSERT_EQ(vm.letrecs[1].frame, store(53));
    ASSERT_EQ(vm.letrecs[1].count, 1);
    PASS();
}

TEST(gc_closes_unreachable_file_port)
{
    FILE *f = tmpfile();
    ASSERT(f != NULL);
    int fd = fileno(f);
    ASSERT(fd >= 0);

    unsigned port = alloc();
    CELL_TYPE(port) = BT_INPORT;
    CELL_PTR(port) = file_port_new(f, false, true, true);
    ASSERT(CELL_PTR(port) != NULL);
    gc_note_external(port);

    gc(0);

    errno = 0;
    ASSERT(fcntl(fd, F_GETFD) == -1);
    ASSERT(errno == EBADF);
    PASS();
}

TEST(gc_forgets_unreachable_file_port_reader_state)
{
    FILE *f = fmemopen((void *)"12.\n", 4, "r");
    ASSERT(f != NULL);

    unsigned port = alloc();
    CELL_TYPE(port) = BT_INPORT;
    CELL_PTR(port) = file_port_new(f, false, true, true);
    ASSERT(CELL_PTR(port) != NULL);
    gc_note_external(port);

    unsigned value = read_obj_port(f);
    ASSERT(value != TOK_ERROR);
    ASSERT(reader_port_pending_bytes(f) > 0);

    gc(0);

    ASSERT_EQ(reader_port_pending_bytes(f), 0);
    PASS();
}

TEST(gc_preserves_current_file_port_until_replaced)
{
    FILE *f = tmpfile();
    ASSERT(f != NULL);
    int fd = fileno(f);
    ASSERT(fd >= 0);

    unsigned port = alloc();
    CELL_TYPE(port) = BT_INPORT;
    CELL_PTR(port) = file_port_new(f, false, true, true);
    ASSERT(CELL_PTR(port) != NULL);
    gc_note_external(port);
    ctx.current_input = f;
    ctx.current_input_cell = port;

    gc(0);
    ASSERT(fcntl(fd, F_GETFD) != -1);

    ctx.current_input = stdin;
    ctx.current_input_cell = 0;
    gc(0);

    errno = 0;
    ASSERT(fcntl(fd, F_GETFD) == -1);
    ASSERT(errno == EBADF);
    PASS();
}

// ============================================================================
// Env Tests
// ============================================================================

TEST(empty_environment_test)
{
    unsigned env = empty_environment();
    ASSERT(env != 0);
    ASSERT(CELL_TYPE(env) == BT_CONS);
    PASS();
}

TEST(defvar_and_lookup)
{
    unsigned env = empty_environment();
    unsigned var = atom_from_string("x");
    unsigned val = store(42);
    defvar(var, val, env);

    unsigned result = lookup(CELL_ID(var), env);
    ASSERT_EQ(CELL_ID(result), 42);
    PASS();
}

TEST(defvar_multiple)
{
    unsigned env = empty_environment();
    unsigned x = atom_from_string("x");
    unsigned y = atom_from_string("y");
    defvar(x, store(1), env);
    defvar(y, store(2), env);

    ASSERT_EQ(CELL_ID(lookup(CELL_ID(x), env)), 1);
    ASSERT_EQ(CELL_ID(lookup(CELL_ID(y), env)), 2);
    PASS();
}

TEST(defvar_overwrites)
{
    unsigned env = empty_environment();
    unsigned x = atom_from_string("x");
    defvar(x, store(1), env);
    defvar(x, store(2), env);

    ASSERT_EQ(CELL_ID(lookup(CELL_ID(x), env)), 2);
    PASS();
}

TEST(setvar_updates)
{
    unsigned env = empty_environment();
    unsigned x = atom_from_string("x");
    defvar(x, store(1), env);
    setvar(CELL_ID(x), store(99), env);

    ASSERT_EQ(CELL_ID(lookup(CELL_ID(x), env)), 99);
    PASS();
}

TEST(lookup_rejects_malformed_environment)
{
    unsigned x = atom_from_string("malformed-lookup");
    unsigned result = lookup(CELL_ID(x), store(1));

    ASSERT_EQ(result, TOK_ERROR);
    PASS();
}

TEST(lookup_silent_rejects_malformed_frame)
{
    unsigned x = atom_from_string("malformed-silent-lookup");
    unsigned env = alloc_cons(store(1), 0);
    unsigned result = lookup_silent(CELL_ID(x), env);

    ASSERT_EQ(result, TOK_ERROR);
    PASS();
}

TEST(environment_cycle_is_rejected)
{
    unsigned x = atom_from_string("cyclic-environment");
    unsigned frame = alloc_cons(0, 0);
    unsigned env = alloc_cons(frame, 0);
    cell_set_cdr(env, env);

    ASSERT_EQ(lookup_silent(CELL_ID(x), env), TOK_ERROR);
    ASSERT_EQ(env_find_binding_cell(CELL_ID(x), env), 0);
    ASSERT_EQ(setvar(CELL_ID(x), store(1), env), TOK_ERROR);
    PASS();
}

TEST(environment_frame_binding_cycle_is_rejected)
{
    unsigned x = atom_from_string("cyclic-frame-bindings");
    unsigned vars = alloc_cons(x, 0);
    cell_set_cdr(vars, vars);
    unsigned frame = alloc_cons(vars, alloc_cons(store(1), 0));
    unsigned env = alloc_cons(frame, 0);

    ASSERT_EQ(lookup_silent(CELL_ID(x), env), TOK_ERROR);
    ASSERT_EQ(env_find_binding_cell(CELL_ID(x), env), 0);
    ASSERT_EQ(setvar(CELL_ID(x), store(2), env), TOK_ERROR);
    ASSERT_EQ(defvar(x, store(3), env), TOK_ERROR);
    PASS();
}

TEST(acyclic_verdict_cache_does_not_leak_between_frames)
{
    // env_binding_list_acyclic memoizes its verdict per binding-list head
    // cell (it runs on every lookup and dominated the profile otherwise).
    // The hazard is cross-contamination: a cached "acyclic" verdict being
    // reused for a different frame, which would turn a corrupt environment
    // into a silently accepted one. Probe a healthy frame first so a verdict
    // is definitely cached, then confirm a cyclic frame is still rejected,
    // then confirm the healthy frame still resolves.
    unsigned good_var = atom_from_string("acyclic-cache-good");
    unsigned good_vars = alloc_cons(good_var, 0);
    unsigned good_frame = alloc_cons(good_vars, alloc_cons(store(42), 0));
    unsigned good_env = alloc_cons(good_frame, 0);

    ASSERT_EQ(CELL_ID(lookup_silent(CELL_ID(good_var), good_env)), 42);
    // Repeat: this one is served from the memo rather than a fresh walk.
    ASSERT_EQ(CELL_ID(lookup_silent(CELL_ID(good_var), good_env)), 42);

    unsigned bad_var = atom_from_string("acyclic-cache-bad");
    unsigned bad_vars = alloc_cons(bad_var, 0);
    cell_set_cdr(bad_vars, bad_vars);
    unsigned bad_frame = alloc_cons(bad_vars, alloc_cons(store(7), 0));
    unsigned bad_env = alloc_cons(bad_frame, 0);

    ASSERT_EQ(lookup_silent(CELL_ID(bad_var), bad_env), TOK_ERROR);
    ASSERT_EQ(env_find_binding_cell(CELL_ID(bad_var), bad_env), 0);
    // The rejection must be stable across repeats too, not just the first.
    ASSERT_EQ(lookup_silent(CELL_ID(bad_var), bad_env), TOK_ERROR);

    // The healthy frame must be unaffected by the corrupt one.
    ASSERT_EQ(CELL_ID(lookup_silent(CELL_ID(good_var), good_env)), 42);
    PASS();
}

TEST(setvar_rejects_malformed_environment)
{
    unsigned x = atom_from_string("malformed-setvar");
    unsigned result = setvar(CELL_ID(x), store(1), store(2));

    ASSERT_EQ(result, TOK_ERROR);
    PASS();
}

TEST(env_find_binding_cell_rejects_malformed_environment)
{
    unsigned x = atom_from_string("malformed-env-find");
    unsigned result = env_find_binding_cell(CELL_ID(x), store(1));

    ASSERT_EQ(result, 0);
    PASS();
}

TEST(defvar_rejects_malformed_environment)
{
    unsigned x = atom_from_string("malformed-defvar");
    unsigned result = defvar(x, store(1), store(2));

    ASSERT_EQ(result, TOK_ERROR);
    PASS();
}

TEST(environment_metadata_does_not_collide_with_hygiene_symbol)
{
    unsigned env = empty_environment();
    unsigned protected_name = atom_from_string("##protected##");
    unsigned another_name = atom_from_string("ordinary-after-protected");

    ASSERT(defvar(protected_name, store(1), env) != TOK_ERROR);
    ASSERT(!environment_is_immutable(env));
    ASSERT(defvar(another_name, store(2), env) != TOK_ERROR);
    ASSERT_EQ(CELL_ID(lookup(CELL_ID(another_name), env)), 2);
    PASS();
}

TEST(immutable_environment_is_detected_and_enforced)
{
    // environment_is_immutable now reads position 0 instead of scanning the
    // whole binding list. Public definitions are rejected, and private macro
    // bindings must be inserted after the marker rather than ahead of it.
    unsigned env = empty_environment();
    unsigned before = atom_from_string("immutable-before-marking");
    unsigned after = atom_from_string("immutable-after-marking");

    ASSERT(defvar(before, store(1), env) != TOK_ERROR);
    ASSERT(!environment_is_immutable(env));

    mark_immutable_environment(env);
    ASSERT(environment_is_immutable(env));

    // Bindings present before marking must remain visible...
    ASSERT_EQ(CELL_ID(lookup(CELL_ID(before), env)), 1);
    // ...and no new public binding may be added.
    ASSERT_EQ(defvar(after, store(2), env), TOK_ERROR);
    ASSERT(environment_is_immutable(env));

    unsigned private_name = make_uninterned_symbol("immutable-after-marking");
    ASSERT(defvar(private_name, store(4), env) != TOK_ERROR);
    ASSERT(environment_is_immutable(env));
    ASSERT_EQ(CELL_ID(lookup(CELL_ID(private_name), env)), 4);
    ASSERT_EQ(lookup_silent(CELL_ID(after), env), TOK_ERROR);
    ASSERT_EQ(defvar(after, store(5), env), TOK_ERROR);
    ASSERT_EQ(setvar(CELL_ID(before), store(6), env), TOK_ERROR);

    // Marking is idempotent and must not stack a second marker.
    mark_immutable_environment(env);
    ASSERT(environment_is_immutable(env));
    ASSERT_EQ(CELL_ID(lookup(CELL_ID(before), env)), 1);

    // A sibling environment must be unaffected.
    unsigned other = empty_environment();
    ASSERT(!environment_is_immutable(other));
    ASSERT(defvar(after, store(3), other) != TOK_ERROR);
    PASS();
}

TEST(environment_rejects_malformed_r7rs_atoms)
{
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_ATOM;
    CELL_ID(bad) = INT64_MAX;
    unsigned spec = alloc_cons(atom_from_string("scheme"),
                               alloc_cons(bad, 0));
    unsigned specs = alloc_cons(spec, 0);

    unsigned result = environment_with_imports(default_environment(), specs);
    ASSERT_EQ(result, TOK_ERROR);

    unsigned source = empty_environment();
    unsigned frame = car(source);
    unsigned bad_vars = alloc_cons(bad, 0);
    unsigned bad_vals = alloc_cons(store(1), 0);
    cell_set_car(frame, bad_vars);
    cell_set_cdr(frame, bad_vals);
    unsigned valid_spec = alloc_cons(atom_from_string("scheme"),
                                     alloc_cons(atom_from_string("base"), 0));
    unsigned valid_specs = alloc_cons(valid_spec, 0);
    ASSERT(environment_with_imports(source, valid_specs) == TOK_ERROR);

    unsigned mismatch_source = empty_environment();
    unsigned mismatch_frame = car(mismatch_source);
    cell_set_car(mismatch_frame, alloc_cons(atom_from_string("+"),
                                            alloc_cons(atom_from_string("-"), 0)));
    cell_set_cdr(mismatch_frame, alloc_cons(store(1), 0));
    ASSERT(environment_with_imports(mismatch_source, valid_specs) == TOK_ERROR);
    PASS();
}

TEST(environment_validates_source_before_r7rs_name_filtering)
{
    unsigned source = empty_environment();
    unsigned frame = car(source);
    unsigned vars = alloc_cons(atom_from_string("+"), 0);
    cell_set_car(frame, vars);
    cell_set_cdr(vars, vars);

    unsigned only = alloc_cons(atom_from_string("only"),
                               alloc_cons(alloc_cons(atom_from_string("scheme"),
                                                    alloc_cons(atom_from_string("base"), 0)),
                                          alloc_cons(atom_from_string("+"), 0)));
    unsigned specs = alloc_cons(only, 0);
    ASSERT_EQ(environment_with_imports(source, specs), TOK_ERROR);
    PASS();
}

TEST(environment_imports_visible_shadowing_binding)
{
    unsigned outer = empty_environment();
    unsigned inner = empty_environment();
    unsigned plus = atom_from_string("+");
    unsigned spec = alloc_cons(atom_from_string("scheme"),
                               alloc_cons(atom_from_string("base"), 0));

    ASSERT(defvar(plus, store(1), outer) != TOK_ERROR);
    cell_set_cdr(inner, outer);
    ASSERT(defvar(plus, store(2), inner) != TOK_ERROR);

    unsigned result = environment_with_imports(inner, alloc_cons(spec, 0));
    ASSERT(result != TOK_ERROR);
    ASSERT_EQ(CELL_ID(lookup(CELL_ID(plus), result)), 2);
    PASS();
}

TEST(clone_environment_preserves_frame_order)
{
    unsigned outer = empty_environment();
    unsigned inner = empty_environment();
    unsigned x = atom_from_string("clone-shadow-x");
    unsigned y = atom_from_string("clone-outer-y");

    ASSERT(defvar(x, store(1), outer) != TOK_ERROR);
    ASSERT(defvar(y, store(3), outer) != TOK_ERROR);
    cell_set_cdr(inner, outer);
    ASSERT(defvar(x, store(2), inner) != TOK_ERROR);

    unsigned copy = clone_environment(inner);
    ASSERT(copy != TOK_ERROR);
    ASSERT_EQ(CELL_ID(lookup(CELL_ID(x), copy)), 2);
    ASSERT_EQ(CELL_ID(lookup(CELL_ID(y), copy)), 3);
    PASS();
}

TEST(bind_params_simple)
{
    unsigned params =
        alloc_cons(atom_from_string("a"), alloc_cons(atom_from_string("b"), 0));
    unsigned args = alloc_cons(store(1), alloc_cons(store(2), 0));

    unsigned frame = bind_params(params, args);
    ASSERT(frame != 0);
    // Frame is (vars . vals)
    unsigned vars = car(frame);
    unsigned vals = cdr(frame);
    ASSERT_EQ(list_length(vars), 2);
    ASSERT_EQ(list_length(vals), 2);
    PASS();
}

TEST(bind_params_rejects_invalid_formals)
{
    unsigned params = alloc_cons(atom_from_string("x"), store(1));
    unsigned args = alloc_cons(store(1), 0);

    unsigned frame = bind_params(params, args);

    ASSERT_EQ(frame, TOK_ERROR);
    PASS();
}

TEST(bind_params_rejects_circular_formals)
{
    unsigned x = atom_from_string("circular-formal");
    unsigned params = alloc_cons(x, 0);
    cell_set_cdr(params, params);
    unsigned args = alloc_cons(store(1), 0);

    ASSERT_EQ(lambda_params_valid(params), false);
    ASSERT_EQ(bind_params(params, args), TOK_ERROR);
    PASS();
}

TEST(bind_params_rejects_duplicate_formals)
{
    unsigned x = atom_from_string("dup-formal");
    unsigned params = alloc_cons(x, alloc_cons(x, 0));
    unsigned args = alloc_cons(store(1), alloc_cons(store(2), 0));

    unsigned frame = bind_params(params, args);

    ASSERT_EQ(frame, TOK_ERROR);
    PASS();
}

TEST(bind_params_rest_only_collects_all_args)
{
    unsigned rest = atom_from_string("rest-only");
    unsigned args = alloc_cons(store(1), alloc_cons(store(2), 0));

    unsigned frame = bind_params(rest, args);

    ASSERT(frame != TOK_ERROR);
    ASSERT(IS_PAIR(car(frame)));
    ASSERT_EQ(car(car(frame)), rest);
    ASSERT(IS_PAIR(cdr(frame)));
    ASSERT_EQ(car(cdr(frame)), args);
    PASS();
}

TEST(mk_primop_test)
{
    unsigned p = mk_primop(PPLUS);
    ASSERT(CELL_TYPE(p) == BT_BUILTIN);
    ASSERT_EQ(CELL_ID(p), PPLUS);
    PASS();
}

TEST(default_environment_has_primitives)
{
    unsigned env = default_environment();
    GC_GUARD;
    gc_protect(&env);

    // Look up '+' primitive
    unsigned plus = atom_from_string("+");
    unsigned val = lookup(CELL_ID(plus), env);
    ASSERT(val != TOK_ERROR);
    ASSERT(CELL_TYPE(val) == BT_BUILTIN);
    ASSERT_EQ(CELL_ID(val), PPLUS);
    PASS();
}

// Every enum primitive_id has an entry: a designated-initializer gap would
// leave a NULL name, and apply_primitive_argv would check that primitive
// against a 0..0 arity.
TEST(primitive_table_is_well_formed)
{
    for (unsigned id = 0; id < PRIM_COUNT; id++) {
        const primitive_info *info = primitive_info_for(id);
        ASSERT(info == &primitive_table[id]);
        ASSERT(info->name != NULL);
        ASSERT(info->name[0] != '\0');
        ASSERT(info->who == NULL || info->who[0] != '\0');
        ASSERT(info->max_args == PRIM_ARGS_ANY ||
               info->min_args <= info->max_args);
        // An exact arity above 8 would be a typo: no primitive takes more
        // than 5 fixed arguments.
        ASSERT(info->min_args <= 8);
        ASSERT(info->max_args == PRIM_ARGS_ANY || info->max_args <= 8);
        // Foldable implies pure; a primitive the engines apply themselves
        // is neither foldable nor allocation-free, and never bootstrap-only.
        if (info->flags & PRIM_FOLDABLE)
            ASSERT(info->flags & PRIM_PURE);
        if (info->flags & PRIM_SPECIAL)
            ASSERT(!(info->flags & (PRIM_PURE | PRIM_NO_ALLOC |
                                    PRIM_BOOTSTRAP)));
        for (unsigned j = id + 1; j < PRIM_COUNT; j++)
            ASSERT(strcmp(info->name, primitive_table[j].name) != 0);
        for (const primitive_alias *a = primitive_aliases; a->name; a++)
            ASSERT(strcmp(info->name, a->name) != 0);
    }
    ASSERT(primitive_info_for(PRIM_COUNT) == NULL);

    for (const primitive_alias *a = primitive_aliases; a->name; a++) {
        ASSERT(a->prim < PRIM_COUNT);
        ASSERT(!(primitive_table[a->prim].flags & PRIM_BOOTSTRAP));
    }

    // A sample against the arities the implementations used to check
    // themselves.
    ASSERT_EQ(primitive_table[PCAR].min_args, 1);
    ASSERT_EQ(primitive_table[PCAR].max_args, 1);
    ASSERT_EQ(primitive_table[PPLUS].min_args, 0);
    ASSERT_EQ(primitive_table[PPLUS].max_args, PRIM_ARGS_ANY);
    ASSERT_EQ(primitive_table[PBYTEVECCOPYTO].min_args, 3);
    ASSERT_EQ(primitive_table[PBYTEVECCOPYTO].max_args, 5);
    ASSERT_EQ(primitive_table[PRECORDSET].min_args, 5);
    ASSERT_STR_EQ(primitive_error_name(&primitive_table[PMEMBER]), "member");
    ASSERT_STR_EQ(primitive_error_name(&primitive_table[PCAR]), "car");
    ASSERT(primitive_has_flag(PCAR, PRIM_NO_ALLOC | PRIM_FOLDABLE));
    ASSERT(!primitive_has_flag(PCONS, PRIM_FOLDABLE));
    ASSERT(primitive_has_flag(PCONS, PRIM_PURE));
    ASSERT(primitive_has_flag(PCALLCC, PRIM_SPECIAL));
    ASSERT(!primitive_has_flag(-1, PRIM_PURE));
    ASSERT(!primitive_has_flag(PRIM_COUNT, PRIM_PURE));

    PASS();
}

// Every table name is bound in a default environment to its own primitive,
// except the bootstrap-only ones, and every alias to its target.
TEST(primitive_table_names_are_bound)
{
    GC_GUARD;
    unsigned env = default_environment();
    gc_protect(&env);
    for (unsigned id = 0; id < PRIM_COUNT; id++) {
        unsigned sym = atom_from_string(primitive_table[id].name);
        unsigned val = lookup_silent(CELL_ID(sym), env);
        if (primitive_table[id].flags & PRIM_BOOTSTRAP) {
            ASSERT(val == TOK_ERROR);
            continue;
        }
        ASSERT(val != TOK_ERROR);
        ASSERT(IS_BUILTIN(val));
        ASSERT_EQ(CELL_ID(val), id);
    }
    for (const primitive_alias *a = primitive_aliases; a->name; a++) {
        unsigned sym = atom_from_string(a->name);
        unsigned val = lookup_silent(CELL_ID(sym), env);
        ASSERT(val != TOK_ERROR && IS_BUILTIN(val));
        ASSERT_EQ(CELL_ID(val), a->prim);
    }
    PASS();
}

// apply_primitive_argv rejects a wrong argument count from the table, under
// the primitive's error name and in the format the implementations used.
TEST(primitive_arity_checked_from_table)
{
    unsigned argv[8] = {0};
    ASSERT_EQ(apply_primitive_argv(PCAR, 0, argv), TOK_ERROR);
    ASSERT_STR_EQ(ctx.last_error,
                  "car: too few arguments (expected 1, got 0)");
    ASSERT_EQ(apply_primitive_argv(PCONS, 3, argv), TOK_ERROR);
    ASSERT_STR_EQ(ctx.last_error,
                  "cons: too many arguments (expected at most 2)");
    ASSERT_EQ(apply_primitive_argv(PMEMBER, 1, argv), TOK_ERROR);
    ASSERT_STR_EQ(ctx.last_error,
                  "member: too few arguments (expected 2, got 1)");
    ASSERT_EQ(apply_primitive_argv(PRECORDTYPEP, 2, argv), TOK_ERROR);
    ASSERT_STR_EQ(ctx.last_error,
                  "record-type?: too many arguments (expected at most 1)");
    ASSERT_EQ(apply_primitive_argv(PVECSET, 2, argv), TOK_ERROR);
    ASSERT_STR_EQ(ctx.last_error,
                  "vector-set!: too few arguments (expected 3, got 2)");
    // A primitive that reports its own count keeps its message.
    ASSERT_EQ(apply_primitive_argv(PMINUS, 0, argv), TOK_ERROR);
    ASSERT_STR_EQ(ctx.last_error, "-: requires at least one argument");
    // Any number of arguments, including more than PRIM_ARGS_ANY.
    unsigned many[300];
    for (unsigned i = 0; i < 300; i++)
        many[i] = MAKE_FIXNUM(1);
    unsigned sum = apply_primitive_argv(PPLUS, 300, many);
    ASSERT(CELL_TYPE(sum) == BT_NUM);
    ASSERT_EQ(CELL_ID(sum), 300);
    PASS();
}

TEST(feature_table_is_well_formed)
{
    const char *const *features = feature_names();
    size_t count = feature_name_count();

    ASSERT(features != NULL);
    ASSERT(count > 0);
    ASSERT(features[count] == NULL);

    for (size_t i = 0; i < count; i++) {
        ASSERT(features[i] != NULL);
        ASSERT(features[i][0] != '\0');
        for (size_t j = i + 1; j < count; j++) {
            ASSERT(strcmp(features[i], features[j]) != 0);
        }
    }

    PASS();
}

TEST(cond_expand_rejects_malformed_and_requirement)
{
    unsigned req = alloc_cons(atom_from_string("and"), store(1));
    ASSERT(!cond_expand_requirement_satisfied(req));
    PASS();
}

TEST(cond_expand_rejects_malformed_or_requirement)
{
    unsigned req = alloc_cons(atom_from_string("or"), store(1));
    ASSERT(!cond_expand_requirement_satisfied(req));
    PASS();
}

TEST(cond_expand_rejects_recursive_requirement)
{
    unsigned req = alloc_cons(atom_from_string("and"), 0);
    unsigned args = alloc_cons(req, 0);
    cell_set_cdr(req, args);
    ASSERT(!cond_expand_requirement_satisfied(req));
    PASS();
}

// ============================================================================
// Context Tests - GC
// ============================================================================

TEST(gc_preserves_root)
{
    unsigned root = alloc_cons(store(42), 0);
    unsigned preserved = gc(root);
    ASSERT(preserved != 0);
    ASSERT_EQ(CELL_ID(car(preserved)), 42);
    PASS();
}

TEST(gc_preserves_list)
{
    unsigned lst =
        alloc_cons(store(1), alloc_cons(store(2), alloc_cons(store(3), 0)));
    unsigned preserved = gc(lst);
    ASSERT_EQ(list_length(preserved), 3);
    ASSERT_EQ(CELL_ID(car(preserved)), 1);
    ASSERT_EQ(CELL_ID(cadr(preserved)), 2);
    ASSERT_EQ(CELL_ID(caddr(preserved)), 3);
    PASS();
}

TEST(swept_small_payloads_are_reused_by_their_allocated_length)
{
    // Small vector and record payloads are kept for reuse when swept, filed
    // by the length the registry recorded at allocation - not by what the
    // payload says by then, which a malformed cell may have overwritten.
    unsigned v = make_vector(2, ctx.atom_true);
    vector_data *vd = GET_VECTOR_PTR(v);
    vd->len = 8; // claims more slots than the block holds
    minor_gc(0);
    ASSERT(!vector_is_registered(vd));
    GC_GUARD;
    unsigned w = make_vector(8, ctx.atom_false);
    gc_protect(&w);
    ASSERT(GET_VECTOR_PTR(w) != vd);
    unsigned u = make_vector(2, ctx.atom_false);
    gc_protect(&u);
    ASSERT(GET_VECTOR_PTR(u) == vd);
    ASSERT(vector_data_well_formed(GET_VECTOR_PTR(u)));
    ASSERT_EQ(vector_len(u), 2);
    ASSERT(vector_data_ptr(u)[0] == ctx.atom_false);
    ASSERT(vector_data_ptr(u)[1] == ctx.atom_false);
    PASS();
}

TEST(gc_preserves_vector)
{
    unsigned v = make_vector(3, store(42));
    unsigned root = alloc_cons(v, 0);
    unsigned preserved = gc(root);
    unsigned vec = car(preserved);
    ASSERT(CELL_TYPE(vec) == BT_VECTOR);
    ASSERT_EQ(vector_len(vec), 3);
    PASS();
}

TEST(gc_preserves_direct_vector_root)
{
    unsigned v = make_vector(3, store(42));
    unsigned preserved = gc(v);
    ASSERT(CELL_TYPE(preserved) == BT_VECTOR);
    ASSERT_EQ(vector_len(preserved), 3);
    for (unsigned i = 0; i < vector_len(preserved); i++) {
        unsigned elem = vector_data_ptr(preserved)[i];
        ASSERT(CELL_TYPE(elem) == BT_NUM);
        ASSERT_EQ(CELL_ID(elem), 42);
    }
    PASS();
}

TEST(gc_ignores_malformed_hash_table_payload)
{
    hash_table_data *ht = checked_malloc_size(sizeof(hash_table_data));
    ASSERT(ht != NULL);
    ht->size = 0;
    ht->capacity = 16;
    ht->equiv = HASH_EQUAL;
    ht->buckets = NULL;
    hash_table_register(ht);

    unsigned table = alloc();
    CELL_TYPE(table) = BT_HASHTABLE;
    CELL_PTR(table) = ht;

    unsigned preserved = gc(table);
    ASSERT(CELL_TYPE(preserved) == BT_HASHTABLE);
    ASSERT(GET_HASHTABLE_PTR(preserved) == ht);

    CELL_TYPE(preserved) = BT_FREE;
    CELL_PTR(preserved) = NULL;
    hash_table_unregister(ht);
    free(ht);
    PASS();
}

TEST(gc_ignores_malformed_vector_payload)
{
    unsigned vector = alloc();
    CELL_TYPE(vector) = BT_VECTOR;
    CELL_PTR(vector) = NULL;

    unsigned preserved = gc(vector);
    ASSERT(CELL_TYPE(preserved) == BT_VECTOR);
    ASSERT(GET_VECTOR_PTR(preserved) == NULL);

    CELL_PTR(preserved) = (void *)(uintptr_t)1;
    preserved = gc(preserved);
    ASSERT(CELL_TYPE(preserved) == BT_VECTOR);
    ASSERT(GET_VECTOR_PTR(preserved) == (void *)(uintptr_t)1);

    PASS();
}

// ============================================================================
// Records
// ============================================================================

static unsigned test_symbol(const char *name)
{
    unsigned sym = alloc();
    CELL_TYPE(sym) = BT_ATOM;
    CELL_ID(sym) = intern(name);
    return sym;
}

// A record type named name with the given fields, as %make-record-type makes.
static unsigned test_record_type(const char *name, const char *first_field,
                                 const char *second_field)
{
    GC_GUARD;
    unsigned fields = 0;
    gc_protect(&fields);
    // Each symbol is made before fields is read: argument evaluation order
    // is unspecified, and the allocation can move the list.
    if (second_field) {
        unsigned field = test_symbol(second_field);
        fields = alloc_cons(field, fields);
    }
    if (first_field) {
        unsigned field = test_symbol(first_field);
        fields = alloc_cons(field, fields);
    }
    unsigned name_symbol = test_symbol(name);
    return make_record_type(name_symbol, fields, 0);
}

static char *test_write_to_string(unsigned obj, bool display)
{
    char *buf = NULL;
    size_t len = 0;
    FILE *mem = open_memstream(&buf, &len);
    if (!mem)
        return NULL;
    if (display)
        display_obj_port(obj, mem);
    else
        write_obj_port(obj, mem);
    fclose(mem);
    return buf;
}

TEST(gc_preserves_record_and_its_type)
{
    GC_GUARD;
    unsigned rtd = test_record_type("point", "x", "y");
    ASSERT(record_type_well_formed(rtd));
    gc_protect(&rtd);
    unsigned rec = make_record(rtd);
    ASSERT(IS_RECORD(rec));
    gc_protect(&rec);
    unsigned number = store(1000);
    vector_set_elem(rec, RECORD_FIELDS_START, number);
    unsigned text = make_string_copy("field");
    vector_set_elem(rec, RECORD_FIELDS_START + 1, text);
    // Only a list reaches the record; the type only through the record.
    unsigned root = alloc_cons(rec, 0);
    gc_unprotect(2);
    root = gc(root);
    rec = car(root);
    ASSERT(IS_RECORD(rec));
    rtd = record_rtd(rec);
    ASSERT(rtd != 0 && IS_RECORD_TYPE(rtd));
    ASSERT_STR_EQ(record_type_name_string(rtd), "point");
    ASSERT_EQ(record_type_field_count(rtd), 2);
    unsigned *fields = vector_data_ptr(rec);
    ASSERT(IS_NUM(fields[RECORD_FIELDS_START]));
    ASSERT_EQ(CELL_ID(fields[RECORD_FIELDS_START]), 1000);
    ASSERT(IS_STRING(fields[RECORD_FIELDS_START + 1]));
    ASSERT_STR_EQ(GET_STRING_PTR(fields[RECORD_FIELDS_START + 1]), "field");
    unsigned names[1] = {rtd};
    unsigned list = apply_primitive_argv(PRECORDTYPEFIELDNAMES, 1, names);
    ASSERT_EQ(list_length(list), 2);
    ASSERT(IS_ATOM(car(list)) && CELL_ID(car(list)) == intern("x"));
    PASS();
}

// The minor collection finds old-to-young references through the dirty-card
// scan, which goes through gc_scan_cell. A record left out of it keeps its
// nursery referent's index after the nursery is reclaimed.
TEST(old_record_set_to_nursery_object_survives_minor_gc)
{
    GC_GUARD;
    unsigned rtd = test_record_type("box", "value", NULL);
    gc_protect(&rtd);
    unsigned rec = make_record(rtd);
    gc_protect(&rec);
    minor_gc(0);
    ASSERT(is_in_old_gen(rec));
    ASSERT(is_in_old_gen(rtd));

    unsigned who = test_symbol("set-box!");
    gc_protect(&who);
    unsigned head = store(12345);
    gc_protect(&head);
    unsigned tail = store(67890);
    gc_protect(&tail);
    unsigned fresh = alloc_cons(head, tail);
    gc_unprotect(2);
    ASSERT(is_in_nursery(fresh));
    // store(0) is a cached cell: nothing here allocates.
    unsigned args[5] = {rec, rtd, store(0), fresh, who};
    ASSERT(apply_primitive_argv(PRECORDSET, 5, args) == fresh);
    // Nothing else refers to the pair now: no root, only the old record.
    unsigned majors = ctx.major_gc_count;
    unsigned minors = ctx.minor_gc_count;
    minor_gc(0);
    ASSERT_EQ(ctx.major_gc_count, majors);
    ASSERT_EQ(ctx.minor_gc_count, minors + 1);

    unsigned value = vector_data_ptr(rec)[RECORD_FIELDS_START];
    ASSERT(!is_in_nursery(value));
    ASSERT(IS_PAIR(value));
    ASSERT(IS_NUM(car(value)) && CELL_ID(car(value)) == 12345);
    ASSERT(IS_NUM(cdr(value)) && CELL_ID(cdr(value)) == 67890);
    unsigned ref_args[4] = {rec, rtd, store(0), who};
    ASSERT_EQ(apply_primitive_argv(PRECORDREF, 4, ref_args), value);
    PASS();
}

// Records are a type of their own: no vector operation accepts one, and
// equal? compares them by identity.
TEST(records_are_disjoint_from_vectors)
{
    GC_GUARD;
    unsigned rtd = test_record_type("point", "x", "y");
    gc_protect(&rtd);
    unsigned rec = make_record(rtd);
    gc_protect(&rec);
    unsigned twin = make_record(rtd);
    gc_protect(&twin);
    unsigned one[1] = {rec};
    ASSERT_EQ(apply_primitive_argv(PVECTORP, 1, one), ctx.atom_false);
    ASSERT_EQ(apply_primitive_argv(PVECLEN, 1, one), TOK_ERROR);
    unsigned ref[2] = {rec, store(0)};
    ASSERT_EQ(apply_primitive_argv(PVECREF, 2, ref), TOK_ERROR);
    one[0] = rtd;
    ASSERT_EQ(apply_primitive_argv(PVECTORP, 1, one), ctx.atom_false);
    ASSERT(deep_equal(rec, rec));
    ASSERT(!deep_equal(rec, twin));
    ASSERT_STR_EQ(type_name(rec), "point");
    ASSERT_STR_EQ(type_name(rtd), "record-type");
    // A record whose payload is not a registered vector is rejected.
    unsigned bad = alloc();
    CELL_TYPE(bad) = BT_RECORD;
    CELL_PTR(bad) = NULL;
    ASSERT_EQ(record_rtd(bad), 0);
    unsigned of[2] = {bad, rtd};
    ASSERT_EQ(apply_primitive_argv(PRECORDOFP, 2, of), ctx.atom_false);
    char *text = test_write_to_string(bad, false);
    ASSERT(text != NULL);
    ASSERT_STR_EQ(text, "[invalid-record]");
    free(text);
    unsigned preserved = gc(bad);
    ASSERT(CELL_TYPE(preserved) == BT_RECORD);
    CELL_TYPE(preserved) = BT_FREE;
    PASS();
}

TEST(writer_prints_records_with_stable_hash_numbers)
{
    GC_GUARD;
    unsigned rtd = test_record_type("<pt>", "a", NULL);
    gc_protect(&rtd);
    unsigned rec = make_record(rtd);
    gc_protect(&rec);
    char *first = test_write_to_string(rec, false);
    ASSERT(first != NULL);
    ASSERT(strncmp(first, "#[pt ", 5) == 0);
    unsigned number = object_hash_number(rec);
    ASSERT(number > 0);
    char expected[64];
    snprintf(expected, sizeof(expected), "#[pt %u]", number);
    ASSERT_STR_EQ(first, expected);
    free(first);
    // The number follows the object through a collection.
    gc(0);
    char *again = test_write_to_string(rec, true);
    ASSERT(again != NULL);
    ASSERT_STR_EQ(again, expected);
    free(again);
    char *type_text = test_write_to_string(rtd, false);
    ASSERT(type_text != NULL);
    snprintf(expected, sizeof(expected), "#[record-type %u pt]",
             object_hash_number(rtd));
    ASSERT_STR_EQ(type_text, expected);
    ASSERT(object_hash_number(rtd) == number + 1);
    free(type_text);
    PASS();
}

// The sweep frees an unreachable record's payload together with its
// object-hash entry, so a later payload at the same address starts fresh.
TEST(gc_sweep_frees_record_payload_and_hash_number)
{
    GC_GUARD;
    unsigned rtd = test_record_type("temp", "a", NULL);
    gc_protect(&rtd);
    unsigned rec = make_record(rtd);
    vector_data *payload = GET_VECTOR_PTR(rec);
    unsigned number = object_hash_number(rec);
    ASSERT(number > 0);
    rec = 0;
    gc(0);
    ASSERT(!vector_is_registered(payload));
    for (int i = 0; i < 64; i++) {
        unsigned other = make_record(rtd);
        ASSERT(object_hash_number(other) > number);
    }
    PASS();
}

TEST(make_error_object_needs_a_condition_type)
{
    GC_GUARD;
    ASSERT_EQ(ctx.builtin_record_types[BRT_CONDITION], 0);
    ASSERT_EQ(make_error_object_c("boom"), TOK_ERROR);
    unsigned fields = 0;
    gc_protect(&fields);
    const char *names[3] = {"irritants", "message", "kind"};
    for (int i = 0; i < 3; i++) {
        unsigned field = test_symbol(names[i]);
        fields = alloc_cons(field, fields);
    }
    unsigned name = test_symbol("condition");
    unsigned rtd = make_record_type(name, fields, 0);
    ctx.builtin_record_types[BRT_CONDITION] = rtd;
    unsigned condition = make_error_object_c("boom");
    ASSERT(IS_RECORD(condition));
    ASSERT(record_rtd(condition) == ctx.builtin_record_types[BRT_CONDITION]);
    unsigned *data = vector_data_ptr(condition);
    ASSERT(IS_ATOM(data[RECORD_FIELDS_START]) &&
           CELL_ID(data[RECORD_FIELDS_START]) == intern("error"));
    ASSERT_STR_EQ(GET_STRING_PTR(data[RECORD_FIELDS_START + 1]), "boom");
    ASSERT_EQ(data[RECORD_FIELDS_START + 2], 0);
    char *text = test_write_to_string(condition, false);
    ASSERT(text != NULL);
    char expected[64];
    snprintf(expected, sizeof(expected), "#[condition %u \"simple-error\"]",
             object_hash_number(condition));
    ASSERT_STR_EQ(text, expected);
    free(text);
    ctx.builtin_record_types[BRT_CONDITION] = 0;
    PASS();
}

// A char-set as stdlib.scm builds it: a (kind payload) record of the type
// registered as BRT_CHAR_SET.
static unsigned test_char_set(unsigned rtd, const char *kind, unsigned payload)
{
    GC_GUARD;
    gc_protect(&rtd);
    gc_protect(&payload);
    unsigned kind_symbol = test_symbol(kind);
    gc_protect(&kind_symbol);
    unsigned set = make_record(rtd);
    vector_set_elem(set, RECORD_FIELDS_START, kind_symbol);
    vector_set_elem(set, RECORD_FIELDS_START + 1, payload);
    return set;
}

// ((start . end)) as a freshly allocated range list.
static unsigned test_range_list(int64_t start, int64_t end)
{
    unsigned range = alloc_cons(store(start), store(end));
    return alloc_cons(range, 0);
}

TEST(equal_compares_char_sets_by_contents)
{
    GC_GUARD;
    unsigned rtd = test_record_type("char-set", "kind", "payload");
    gc_protect(&rtd);
    unsigned other_rtd = test_record_type("other", "kind", "payload");
    gc_protect(&other_rtd);
    unsigned payload = test_range_list(97, 99);
    gc_protect(&payload);
    unsigned same = 0, different = 0, other = 0, pred_a = 0, pred_b = 0;
    gc_protect(&same);
    gc_protect(&different);
    gc_protect(&other);
    gc_protect(&pred_a);
    gc_protect(&pred_b);
    unsigned a = test_char_set(rtd, "ranges", payload);
    gc_protect(&a);
    unsigned b = test_range_list(97, 99);
    gc_protect(&b);
    b = test_char_set(rtd, "ranges", b);
    // Unregistered, a char-set is a record like any other: equal? only to
    // itself.
    ASSERT(deep_equal(a, a));
    ASSERT(!deep_equal(a, b));
    ctx.builtin_record_types[BRT_CHAR_SET] = rtd;
    ASSERT(deep_equal(a, b));
    different = test_range_list(97, 100);
    different = test_char_set(rtd, "ranges", different);
    ASSERT(!deep_equal(a, different));
    // Same payload, other kind; same fields, other record type.
    other = test_char_set(rtd, "predicate", payload);
    ASSERT(!deep_equal(a, other));
    other = test_char_set(other_rtd, "ranges", payload);
    ASSERT(!deep_equal(a, other));
    ASSERT(!deep_equal(other, a));
    // A predicate-based set's payload is a procedure: only the very same one
    // makes two sets equal.
    unsigned proc = mk_primop(PCAR);
    gc_protect(&proc);
    pred_a = test_char_set(rtd, "predicate", proc);
    pred_b = test_char_set(rtd, "predicate", proc);
    ASSERT(deep_equal(pred_a, pred_b));
    unsigned other_proc = mk_primop(PCDR);
    pred_b = test_char_set(rtd, "predicate", other_proc);
    ASSERT(!deep_equal(pred_a, pred_b));
    ctx.builtin_record_types[BRT_CHAR_SET] = 0;
    PASS();
}

TEST(gc_stress_many_allocations)
{
    // Allocate a long list of numbers
    unsigned lst = 0;
    for (int i = 0; i < 1000; i++) {
        lst = alloc_cons(store(i), lst);
    }
    // GC should preserve all of them
    lst = gc(lst);
    ASSERT_EQ(list_length(lst), 1000);
    // Verify first and last values
    ASSERT_EQ(CELL_ID(car(lst)), 999);
    for (int i = 0; i < 999; i++) {
        lst = cdr(lst);
    }
    ASSERT_EQ(CELL_ID(car(lst)), 0);
    PASS();
}

TEST(gc_stress_deep_nesting)
{
    // Create deeply nested structure: ((((1))))
    unsigned x = store(42);
    for (int i = 0; i < 100; i++) {
        x = alloc_cons(x, 0);
    }
    x = gc(x);
    // Navigate down and verify
    for (int i = 0; i < 100; i++) {
        ASSERT(CELL_TYPE(x) == BT_CONS);
        x = car(x);
    }
    ASSERT_EQ(CELL_TYPE(x), BT_NUM);
    ASSERT_EQ(CELL_ID(x), 42);
    PASS();
}

TEST(gc_stress_multiple_collections)
{
    unsigned root = alloc_cons(store(1), alloc_cons(store(2), 0));
    // Run GC multiple times
    for (int i = 0; i < 10; i++) {
        root = gc(root);
        ASSERT_EQ(list_length(root), 2);
        ASSERT_EQ(CELL_ID(car(root)), 1);
        ASSERT_EQ(CELL_ID(cadr(root)), 2);
    }
    PASS();
}

TEST(gc_preserves_permanent_atoms)
{
    // Permanent atoms should have same cell IDs after GC
    unsigned root = alloc_cons(ctx.atom_true, 0);
    unsigned before_true = ctx.atom_true;
    unsigned before_quote = ctx.atom_quote;
    root = gc(root);
    ASSERT(root != 0);
    ASSERT_EQ(ctx.atom_true, before_true);
    ASSERT_EQ(ctx.atom_quote, before_quote);
    ASSERT_EQ(ctx.atom_true, CELL_ATOM_TRUE);
    PASS();
}

TEST(gc_heap_usage_percent)
{
    // heap_usage_percent should return a valid percentage
    int usage = heap_usage_percent();
    ASSERT(usage >= 0 && usage <= 100);
    // After GC with minimal data, usage should decrease
    unsigned root = alloc_cons(store(1), 0);
    root = gc(root);
    ASSERT(root != 0);
    int usage_after = heap_usage_percent();
    ASSERT(usage_after >= 0 && usage_after <= 100);
    PASS();
}

TEST(gc_maybe_gc_threshold)
{
    unsigned root = alloc_cons(store(1), 0);
    // With low threshold, should trigger GC
    root = maybe_gc(root, 0); // 0% threshold = always GC
    ASSERT(root != 0);
    ASSERT_EQ(CELL_ID(car(root)), 1);
    PASS();
}

TEST(code_truncate_clears_verification_and_children)
{
    code_object *parent = code_new();
    code_object *child = code_new();
    ASSERT(parent != NULL && child != NULL);
    code_emit(child, OP_HALT);
    code_add_child(parent, child);
    code_emit(parent, OP_CLOSURE);
    code_emit(parent, 0);
    code_emit(parent, OP_HALT);
    parent->verified = true;

    code_truncate(parent, 1, 0);

    ASSERT_EQ(parent->code_len, 1);
    ASSERT_EQ(parent->children_len, 0);
    ASSERT(!parent->verified);
    code_free(parent);
    code_free(child);
    PASS();
}

TEST(peephole_optimize_prim_tail_flag_survives)
{
    // The tail flag lives in OP_PRIM's argc operand; it is data, not a jump
    // target, and compaction must copy it unchanged.
    code_object *code = code_new();
    ASSERT(code != NULL);
    code_emit(code, OP_CONST);
    code_emit(code, 0);
    code_emit(code, OP_POP);
    code_emit(code, OP_PRIM);
    code_emit(code, 18);
    code_emit(code, 2u | PRIM_TAIL_FLAG);
    code_emit(code, OP_RETURN);

    peephole_optimize(code);

    ASSERT_EQ(code->code_len, 4);
    ASSERT_EQ(code->code[0], OP_PRIM);
    ASSERT_EQ(code->code[2], 2u | PRIM_TAIL_FLAG);
    code_free(code);
    PASS();
}

// ============================================================================
// Main
// ============================================================================

// ----------------------------------------------------------------------------
// External payload tracking (context.c): the collectors free dead cells'
// payloads from the young/old external lists instead of walking every cell.
// ----------------------------------------------------------------------------

// Collect twice so that no cell left over by an earlier test is in a range
// the verifier walks: the second collection copies only what is live.
static void settle_heap_for_external_tests(void)
{
    gc(0);
    gc(0);
}

static unsigned test_vector(unsigned len)
{
    return make_vector(len, ctx.atom_false);
}

TEST(external_payload_freed_when_nursery_cell_dies_in_minor_gc)
{
    settle_heap_for_external_tests();
    gc_set_verify_external(true);
    unsigned vec = test_vector(3);
    vector_data *payload = GET_VECTOR_PTR(vec);
    unsigned str = make_string_copy("dies young");
    char *text = GET_STRING_PTR(str);
    ASSERT(is_in_nursery(vec) && is_in_nursery(str));
    ASSERT(gc_tracked_external_count(true) >= 2);
    minor_gc(0);
    gc_set_verify_external(false);
    ASSERT(!vector_is_registered(payload));
    ASSERT(!string_is_registered(text));
    ASSERT_EQ(gc_tracked_external_count(true), 0);
    PASS();
}

TEST(external_payload_follows_promotion_and_dies_in_major_gc)
{
    settle_heap_for_external_tests();
    unsigned old_before = gc_tracked_external_count(false);
    gc_set_verify_external(true);
    GC_GUARD;
    unsigned vec = test_vector(4);
    gc_protect(&vec);
    vector_data *payload = GET_VECTOR_PTR(vec);
    unsigned big = store_bignum(bn_from_string("123456789012345678901234567890",
                                               10));
    gc_protect(&big);
    bignum *digits = (bignum *)CELL_PTR(big);
    ASSERT(is_in_nursery(vec) && is_in_nursery(big));

    minor_gc(0);
    ASSERT(is_in_old_gen(vec) && is_in_old_gen(big));
    ASSERT(GET_VECTOR_PTR(vec) == payload);
    ASSERT(vector_is_registered(payload));
    ASSERT(bignum_is_registered(digits));
    ASSERT_EQ(gc_tracked_external_count(true), 0);
    ASSERT(gc_tracked_external_count(false) >= old_before + 2);

    // Still reachable: a major collection keeps both and their payloads.
    gc(0);
    ASSERT(GET_VECTOR_PTR(vec) == payload);
    ASSERT(vector_is_registered(payload));
    ASSERT(bignum_is_registered(digits));
    ASSERT(gc_tracked_external_count(false) >= old_before + 2);

    // Unreachable: the next major collection frees them.
    vec = 0;
    big = 0;
    gc(0);
    gc_set_verify_external(false);
    ASSERT(!vector_is_registered(payload));
    ASSERT(!bignum_is_registered(digits));
    ASSERT_EQ(gc_tracked_external_count(false), old_before);
    PASS();
}

TEST(external_payload_of_nursery_cell_freed_by_major_gc)
{
    settle_heap_for_external_tests();
    gc_set_verify_external(true);
    unsigned str = make_string_copy("never promoted");
    char *text = GET_STRING_PTR(str);
    GC_GUARD;
    unsigned kept = make_string_copy("kept");
    gc_protect(&kept);
    char *kept_text = GET_STRING_PTR(kept);
    gc(0);
    gc_set_verify_external(false);
    ASSERT(!string_is_registered(text));
    ASSERT(string_is_registered(kept_text));
    ASSERT(GET_STRING_PTR(kept) == kept_text);
    ASSERT_EQ(gc_tracked_external_count(true), 0);
    PASS();
}

// Noting a cell twice, or noting one that owns nothing, must not free a
// payload twice or keep anything alive.
TEST(external_duplicate_and_non_owning_notes_are_harmless)
{
    settle_heap_for_external_tests();
    unsigned old_before = gc_tracked_external_count(false);
    GC_GUARD;
    unsigned vec = test_vector(2);
    gc_protect(&vec);
    vector_data *payload = GET_VECTOR_PTR(vec);
    gc_note_external(vec);
    unsigned pair = alloc_cons(ctx.atom_true, ctx.atom_false);
    gc_protect(&pair);
    gc_note_external(pair);
    unsigned dead = make_string_copy("noted twice");
    char *dead_text = GET_STRING_PTR(dead);
    gc_note_external(dead);

    minor_gc(0);
    ASSERT(vector_is_registered(payload));
    ASSERT(!string_is_registered(dead_text));
    ASSERT(IS_PAIR(pair) && car(pair) == ctx.atom_true);
    gc(0);
    ASSERT(vector_is_registered(payload));
    // The major collection settled the duplicate entry into one.
    ASSERT_EQ(gc_tracked_external_count(false), old_before + 1);
    vec = 0;
    gc(0);
    ASSERT(!vector_is_registered(payload));
    ASSERT_EQ(gc_tracked_external_count(false), old_before);
    PASS();
}

// An old cell that takes over a payload (as a reader placeholder does) is
// noted while old; the minor collection moves it to the old list.
TEST(external_payload_taken_over_by_old_cell_is_tracked)
{
    settle_heap_for_external_tests();
    gc_set_verify_external(true);
    GC_GUARD;
    unsigned holder = alloc_cons(0, 0);
    gc_protect(&holder);
    minor_gc(0);
    ASSERT(is_in_old_gen(holder));

    char *text = checked_string_copy("adopted");
    ASSERT(text != NULL);
    CELL_TYPE(holder) = BT_STRING;
    CELL_PTR(holder) = text;
    string_register(text);
    gc_note_external(holder);
    minor_gc(0);
    ASSERT(string_is_registered(text));
    gc(0);
    ASSERT(string_is_registered(text));
    ASSERT(GET_STRING_PTR(holder) == text);
    holder = 0;
    gc(0);
    gc_set_verify_external(false);
    ASSERT(!string_is_registered(text));
    PASS();
}

// Reused nursery cells come back from alloc() typed BT_FREE even though the
// collector no longer resets them.
TEST(alloc_returns_cells_typed_free)
{
    GC_GUARD;
    unsigned keep = alloc_cons(ctx.atom_true, 0);
    gc_protect(&keep);
    for (int i = 0; i < 16; i++)
        (void)alloc_cons(ctx.atom_true, 0);
    minor_gc(0);
    for (int i = 0; i < 64; i++) {
        unsigned cell = alloc();
        ASSERT(is_in_nursery(cell));
        ASSERT_EQ(CELL_TYPE(cell), BT_FREE);
        CELL_TYPE(cell) = BT_CONS;
        CELL_CAR(cell) = 0;
        CELL_CDR(cell) = 0;
    }
    PASS();
}

// With the lists marked incomplete the collectors sweep every cell, and the
// next major collection rebuilds the old list from the copied heap.
TEST(external_tracking_falls_back_to_full_sweep)
{
    settle_heap_for_external_tests();
    unsigned old_before = gc_tracked_external_count(false);
    GC_GUARD;
    unsigned vec = test_vector(5);
    gc_protect(&vec);
    vector_data *payload = GET_VECTOR_PTR(vec);
    unsigned dead = make_string_copy("swept the old way");
    char *dead_text = GET_STRING_PTR(dead);
    gc_lose_external_tracking();
    minor_gc(0);
    ASSERT(!string_is_registered(dead_text));
    ASSERT(vector_is_registered(payload));
    gc(0);
    ASSERT(vector_is_registered(payload));
    ASSERT_EQ(gc_tracked_external_count(false), old_before + 1);
    // Tracking is complete again: verification passes and the vector is
    // freed from the rebuilt list once it dies.
    gc_set_verify_external(true);
    unsigned young = make_string_copy("tracked again");
    char *young_text = GET_STRING_PTR(young);
    vec = 0;
    gc(0);
    gc_set_verify_external(false);
    ASSERT(!vector_is_registered(payload));
    ASSERT(!string_is_registered(young_text));
    ASSERT_EQ(gc_tracked_external_count(false), old_before);
    PASS();
}

// VESPER_GC_VERIFY stops at a cell that owns a payload but was never noted.
// It aborts, so check it in a child process.
TEST(external_verification_catches_unnoted_cell)
{
    fflush(stdout);
    pid_t pid = fork();
    ASSERT(pid >= 0);
    if (pid == 0) {
        int devnull = open("/dev/null", O_WRONLY);
        if (devnull >= 0)
            dup2(devnull, STDERR_FILENO);
        settle_heap_for_external_tests();
        gc_set_verify_external(true);
        char *text = checked_string_copy("untracked");
        unsigned cell = alloc();
        CELL_TYPE(cell) = BT_STRING;
        CELL_PTR(cell) = text;
        string_register(text);
        minor_gc(0);
        _exit(0);
    }
    int status = 0;
    ASSERT(waitpid(pid, &status, 0) == pid);
    ASSERT(WIFSIGNALED(status) && WTERMSIG(status) == SIGABRT);
    PASS();
}

// Many objects of every owning type, some kept and some dropped, through
// minor and major collections with verification on.
TEST(external_tracking_survives_mixed_workload)
{
    settle_heap_for_external_tests();
    gc_set_verify_external(true);
    GC_GUARD;
    unsigned keep = 0;
    gc_protect(&keep);
    for (int round = 0; round < 200; round++) {
        unsigned obj;
        switch (round % 5) {
        case 0:
            obj = test_vector((unsigned)(round % 7));
            break;
        case 1:
            obj = make_string_copy("workload");
            break;
        case 2:
            obj = store_bignum(bn_from_string("99999999999999999999999", 10));
            break;
        case 3: {
            unsigned args[1] = {store(round % 4)};
            obj = apply_primitive_argv(PMAKEBYTEVEC, 1, args);
            break;
        }
        default:
            obj = apply_primitive_argv(PMAKEEQUALHASHTABLE, 0, NULL);
            break;
        }
        ASSERT(obj != TOK_ERROR);
        if (round % 3 == 0)
            keep = alloc_cons(obj, keep);
        if (round % 17 == 0)
            minor_gc(0);
        if (round % 61 == 0)
            gc(0);
    }
    gc(0);
    ASSERT_EQ(list_length(keep), 67);
    keep = 0;
    gc(0);
    gc_set_verify_external(false);
    PASS();
}

int main(void)
{
    // Initialize interpreter infrastructure
    init_heap();
    init_keywords();

    printf("=== Context & Env Unit Tests ===\n");

    // Allocation
    RUN_TEST(alloc_returns_valid_cell);
    RUN_TEST(alloc_cells_are_distinct);
    RUN_TEST(alloc_cons_creates_pair);

    // Numeric storage
    RUN_TEST(store_positive);
    RUN_TEST(store_negative);
    RUN_TEST(store_zero);
    RUN_TEST(store_inexact);
    RUN_TEST(store_rational);
    RUN_TEST(store_rational_normalized);
    RUN_TEST(store_rational_to_integer);
    RUN_TEST(numeric_helpers_accept_direct_fixnum);
    RUN_TEST(numeric_helpers_reject_token_sentinels);
    RUN_TEST(store_complex_accepts_direct_fixnum_parts);
    RUN_TEST(numeric_sign_helpers_accept_direct_fixnum);
    RUN_TEST(bignum_helpers_accept_direct_fixnum);
    RUN_TEST(malformed_bignum_payload_is_safe);
    RUN_TEST(malformed_symbol_payload_is_safe);
    RUN_TEST(out_of_range_value_is_not_a_cell);
    RUN_TEST(equal_hash_table_handles_cyclic_numeric_payload);
    RUN_TEST(string_slice_set_survives_an_exhausted_nursery);
    RUN_TEST(hash_table_enumeration_survives_gc_rehash);
    RUN_TEST(error_rejects_malformed_string_message);
    RUN_TEST(string_port_rejects_malformed_source_string);
    RUN_TEST(malformed_rational_payload_is_not_numeric);
    RUN_TEST(malformed_complex_payload_is_not_numeric);
    RUN_TEST(store_bignum_test);

    // Interning
    RUN_TEST(intern_same_string);
    RUN_TEST(intern_different_strings);
    RUN_TEST(uninterned_symbols_keep_distinct_identities);
    RUN_TEST(atom_from_string_creates_atom);
    RUN_TEST(atom_from_string_parses_number);

    // Characters
    RUN_TEST(make_char_test);
    RUN_TEST(make_char_special);

    // Vectors
    RUN_TEST(make_vector_empty);
    RUN_TEST(make_vector_with_fill);
    RUN_TEST(vector_data_access);
    RUN_TEST(vector_primitive_roots_arguments_across_gc);
    RUN_TEST(writer_handles_direct_fixnum);
    RUN_TEST(writer_handles_out_of_range_value);
    RUN_TEST(writer_handles_vector_containing_direct_fixnum);
    RUN_TEST(writer_handles_vm_continuation);
    RUN_TEST(writer_handles_malformed_multiple_values);
    RUN_TEST(writer_handles_cyclic_numeric_payload);
    RUN_TEST(writer_handles_bytecode_closure_marker);
    RUN_TEST(writer_escapes_control_strings_with_scalar_escape);
    RUN_TEST(writer_writes_unicode_character_literal);
    RUN_TEST(writer_displays_unicode_character_as_utf8);
    RUN_TEST(writer_escapes_non_roundtripping_symbols);
    RUN_TEST(writer_escapes_symbol_bar_and_backslash);
    RUN_TEST(write_simple_rejects_cycles);
    RUN_TEST(write_simple_rejects_cycle_after_tracking_table_exhaustion);
    RUN_TEST(write_shared_handles_cycle_after_tracking_table_exhaustion);
    RUN_TEST(string_port_write_failures_return_false);
    RUN_TEST(make_string_owned_rejects_null);

    // List utilities
    RUN_TEST(list_length_empty);
    RUN_TEST(list_length_non_pair_is_zero);
    RUN_TEST(proper_list_silent_rejects_circular_list);
    RUN_TEST(last_pair_rejects_circular_list);
    RUN_TEST(metadata_list_helpers_reject_circular_lists);
    RUN_TEST(list_length_three);
    RUN_TEST(list_append_builds_list);
    RUN_TEST(numeric_comparison_roots_middle_argument_across_gc);
    RUN_TEST(append_primitive_roots_arguments_across_gc);

    // Deep equality
    RUN_TEST(deep_equal_numbers);
    RUN_TEST(deep_equal_fixnum_and_boxed_integer);
    RUN_TEST(deep_equal_rejects_token_sentinels);
    RUN_TEST(primitive_eq_handles_fixnum_and_boxed_integer);
    RUN_TEST(primitive_arithmetic_handles_direct_fixnums);
    RUN_TEST(primitive_arithmetic_roots_arguments_across_gc);
    RUN_TEST(numeric_compare_roots_previous_operand_across_gc);
    RUN_TEST(bytevector_primitives_reject_direct_fixnum);
    RUN_TEST(bytevector_primitives_reject_malformed_payload);
    RUN_TEST(string_primitives_reject_malformed_payload);
    RUN_TEST(vector_primitives_reject_malformed_payload);
    RUN_TEST(port_primitives_reject_malformed_file_payload);
    RUN_TEST(port_primitives_reject_malformed_string_payload);
    RUN_TEST(hash_table_primitives_reject_malformed_payload);
    RUN_TEST(numtower_primitives_handle_direct_fixnums);
    RUN_TEST(math_primitives_handle_direct_fixnums);
    RUN_TEST(random_seed_rejects_malformed_bignum);
    RUN_TEST(number_to_string_handles_direct_fixnum);
    RUN_TEST(procedure_predicate_rejects_pair_with_fixnum_car);
    RUN_TEST(procedure_predicate_rejects_malformed_bytecode_closure);
    RUN_TEST(procedure_predicate_rejects_malformed_vm_continuation);
    RUN_TEST(deep_equal_atoms);
    RUN_TEST(deep_equal_lists);
    RUN_TEST(deep_equal_handles_large_lists_without_recursion);
    RUN_TEST(deep_equal_nested);

    // Continuations
    RUN_TEST(make_cont_test);
    RUN_TEST(make_halt_cont_test);
    RUN_TEST(code_free_unregisters_tree);
    RUN_TEST(code_register_is_idempotent);
    RUN_TEST(gc_collect_code_handles_cyclic_children);
    RUN_TEST(minor_gc_code_update_ignores_inconsistent_storage_metadata);
    RUN_TEST(code_free_handles_cyclic_and_duplicate_children);
    RUN_TEST(code_free_preserves_shared_child);
    RUN_TEST(code_sweep_marks_vm_continuation_code);
    RUN_TEST(code_sweep_ignores_unregistered_closure_pointer);
    RUN_TEST(code_sweep_forgets_a_swept_object_it_had_confirmed);
    RUN_TEST(code_sweep_ignores_unregistered_vm_continuation_pointer);
    RUN_TEST(gc_ignores_unregistered_vm_continuation_payload);
    RUN_TEST(pattern_sweep_ignores_unregistered_pattern_pointer);
    RUN_TEST(gc_mark_pattern_ignores_unregistered_pointer);
    RUN_TEST(code_sweep_ignores_malformed_vm_continuation_frames);
    RUN_TEST(peephole_optimize_ignores_truncated_instruction);
    RUN_TEST(peephole_optimize_ignores_truncated_child_instruction);
    RUN_TEST(peephole_optimize_handles_cyclic_children);
    RUN_TEST(disassemble_handles_cyclic_children);
    RUN_TEST(peephole_optimize_preserves_recursive_call_operands);
    RUN_TEST(peephole_optimize_preserves_primitive_guard_operands);
    RUN_TEST(peephole_optimize_does_not_thread_into_operand);
    RUN_TEST(peephole_optimize_keeps_cdr_of_cadr);
    RUN_TEST(vm_run_rejects_truncated_bytecode);
    RUN_TEST(vm_run_rejects_inconsistent_code_storage_metadata);
    RUN_TEST(vm_run_rejects_invalid_constant_index);
    RUN_TEST(vm_run_rejects_unknown_opcode);
    RUN_TEST(vm_run_rejects_jump_into_operand);
    RUN_TEST(vm_run_rejects_invalid_closure_child_index);
    RUN_TEST(vm_run_rejects_primitive_stack_underflow);
    RUN_TEST(vm_stack_underflow_does_not_mutate_environment);
    RUN_TEST(vm_run_rejects_local_get_out_of_bounds);
    RUN_TEST(vm_run_rejects_local_get_fast_out_of_bounds);
    RUN_TEST(vm_run_rejects_local_set_out_of_bounds);
    RUN_TEST(vm_unary_numeric_ops_reject_malformed_bignum);
    RUN_TEST(vm_vector_ops_reject_malformed_payload);
    RUN_TEST(vm_lookup_rejects_malformed_environment);
    RUN_TEST(vm_lookup_rejects_malformed_frame);
    RUN_TEST(vm_lookup_rejects_malformed_frame_variables);
    RUN_TEST(vm_lookup_ignores_stale_cached_depth);
    RUN_TEST(vm_lookup_ignores_stale_cached_offset);
    RUN_TEST(vm_lookup_cache_sees_define_in_frame_it_passed);
    RUN_TEST(vm_lookup_cache_searches_activations_with_conditional_defines);
    RUN_TEST(vm_lookup_cache_global_seen_from_every_activation);
    RUN_TEST(vm_lookup_cache_rechecks_offset_in_other_activation);
    RUN_TEST(vm_lookup_cache_define_in_fresh_frame_keeps_stamp);
    RUN_TEST(vm_lookup_cache_is_dropped_by_collections);
    RUN_TEST(vm_lookup_cache_follows_binding_references);
    RUN_TEST(vm_lookup_cache_entry_belongs_to_one_instruction);
    RUN_TEST(vm_call_closure_rejects_invalid_bytecode);
    RUN_TEST(vm_call_closure_rejects_invalid_parameter_metadata);
    RUN_TEST(vm_continuation_rejects_oversized_stack);
    RUN_TEST(vm_continuation_rejects_invalid_base_pointer);
    RUN_TEST(vm_continuation_rejects_invalid_frame_state);
    RUN_TEST(vm_continuation_rejects_oversized_frame_stack);
    RUN_TEST(vm_continuation_rejects_truncated_letrec_values);
    RUN_TEST(vm_call_with_values_rejects_cyclic_multiple_values);
    RUN_TEST(execute_pattern_rejects_unknown_opcode);
    RUN_TEST(execute_pattern_rejects_invalid_literal_index);
    RUN_TEST(execute_pattern_rejects_invalid_binding_slot);
    RUN_TEST(execute_pattern_rejects_input_stack_underflow);
    RUN_TEST(execute_pattern_rejects_inconsistent_storage_metadata);
    RUN_TEST(gc_pattern_update_ignores_inconsistent_storage_metadata);
    RUN_TEST(execute_pattern_rejects_invalid_jump_target);
    RUN_TEST(execute_pattern_rejects_input_car_on_non_pair);
    RUN_TEST(execute_pattern_rejects_vector_iteration_on_non_vector);
    RUN_TEST(execute_pattern_rejects_malformed_vector_payload);
    RUN_TEST(gc_updates_vm_continuation_letrec_roots);
    RUN_TEST(gc_ignores_malformed_vm_continuation_arrays);
    RUN_TEST(vm_continuation_capture_copies_after_gc);
    RUN_TEST(gc_updates_vm_signal_handler_root);
    RUN_TEST(vm_continuation_restores_signal_handler);
    RUN_TEST(gc_updates_active_vm_letrec_roots);
    RUN_TEST(gc_closes_unreachable_file_port);
    RUN_TEST(gc_forgets_unreachable_file_port_reader_state);
    RUN_TEST(gc_preserves_current_file_port_until_replaced);
    RUN_TEST(external_payload_freed_when_nursery_cell_dies_in_minor_gc);
    RUN_TEST(external_payload_follows_promotion_and_dies_in_major_gc);
    RUN_TEST(external_payload_of_nursery_cell_freed_by_major_gc);
    RUN_TEST(external_duplicate_and_non_owning_notes_are_harmless);
    RUN_TEST(external_payload_taken_over_by_old_cell_is_tracked);
    RUN_TEST(alloc_returns_cells_typed_free);
    RUN_TEST(external_tracking_falls_back_to_full_sweep);
    RUN_TEST(external_verification_catches_unnoted_cell);
    RUN_TEST(external_tracking_survives_mixed_workload);

    // Environment
    RUN_TEST(empty_environment_test);
    RUN_TEST(defvar_and_lookup);
    RUN_TEST(defvar_multiple);
    RUN_TEST(defvar_overwrites);
    RUN_TEST(setvar_updates);
    RUN_TEST(lookup_rejects_malformed_environment);
    RUN_TEST(lookup_silent_rejects_malformed_frame);
    RUN_TEST(environment_cycle_is_rejected);
    RUN_TEST(environment_frame_binding_cycle_is_rejected);
    RUN_TEST(acyclic_verdict_cache_does_not_leak_between_frames);
    RUN_TEST(setvar_rejects_malformed_environment);
    RUN_TEST(env_find_binding_cell_rejects_malformed_environment);
    RUN_TEST(defvar_rejects_malformed_environment);
    RUN_TEST(environment_metadata_does_not_collide_with_hygiene_symbol);
    RUN_TEST(immutable_environment_is_detected_and_enforced);
    RUN_TEST(environment_rejects_malformed_r7rs_atoms);
    RUN_TEST(environment_validates_source_before_r7rs_name_filtering);
    RUN_TEST(environment_imports_visible_shadowing_binding);
    RUN_TEST(clone_environment_preserves_frame_order);
    RUN_TEST(bind_params_simple);
    RUN_TEST(bind_params_rejects_invalid_formals);
    RUN_TEST(bind_params_rejects_circular_formals);
    RUN_TEST(bind_params_rejects_duplicate_formals);
    RUN_TEST(bind_params_rest_only_collects_all_args);
    RUN_TEST(mk_primop_test);
    RUN_TEST(default_environment_has_primitives);
    RUN_TEST(primitive_table_is_well_formed);
    RUN_TEST(primitive_table_names_are_bound);
    RUN_TEST(primitive_arity_checked_from_table);
    RUN_TEST(feature_table_is_well_formed);
    RUN_TEST(cond_expand_rejects_malformed_and_requirement);
    RUN_TEST(cond_expand_rejects_malformed_or_requirement);
    RUN_TEST(cond_expand_rejects_recursive_requirement);

    // GC
    RUN_TEST(gc_preserves_root);
    RUN_TEST(gc_preserves_list);
    RUN_TEST(gc_preserves_vector);
    RUN_TEST(swept_small_payloads_are_reused_by_their_allocated_length);
    RUN_TEST(gc_preserves_direct_vector_root);
    RUN_TEST(gc_ignores_malformed_hash_table_payload);
    RUN_TEST(gc_ignores_malformed_vector_payload);
    RUN_TEST(gc_preserves_record_and_its_type);
    RUN_TEST(old_record_set_to_nursery_object_survives_minor_gc);
    RUN_TEST(records_are_disjoint_from_vectors);
    RUN_TEST(writer_prints_records_with_stable_hash_numbers);
    RUN_TEST(gc_sweep_frees_record_payload_and_hash_number);
    RUN_TEST(make_error_object_needs_a_condition_type);
    RUN_TEST(equal_compares_char_sets_by_contents);
    RUN_TEST(gc_stress_many_allocations);
    RUN_TEST(gc_stress_deep_nesting);
    RUN_TEST(gc_stress_multiple_collections);
    RUN_TEST(gc_preserves_permanent_atoms);
    RUN_TEST(gc_heap_usage_percent);
    RUN_TEST(gc_maybe_gc_threshold);
    RUN_TEST(code_truncate_clears_verification_and_children);
    RUN_TEST(peephole_optimize_prim_tail_flag_survives);

    TEST_SUMMARY("context & env");
}
