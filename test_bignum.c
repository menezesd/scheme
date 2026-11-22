// Unit tests for bignum module
#include "bignum.h"
#include "test_framework.h"
#include <stdint.h>
#include <stdlib.h>

// ============================================================================
// Bignum Tests
// ============================================================================

TEST(bn_from_int_zero)
{
    bignum *bn = bn_from_int(0);
    ASSERT(bn != NULL);
    ASSERT(bn_is_zero(bn));
    bn_free(bn);
    PASS();
}

TEST(bn_from_int_positive)
{
    bignum *bn = bn_from_int(12345);
    ASSERT(bn != NULL);
    ASSERT(!bn_is_zero(bn));
    ASSERT(bn_sign(bn) > 0);
    bn_free(bn);
    PASS();
}

TEST(bn_from_int_negative)
{
    bignum *bn = bn_from_int(-12345);
    ASSERT(bn != NULL);
    ASSERT(!bn_is_zero(bn));
    ASSERT(bn_sign(bn) < 0);
    bn_free(bn);
    PASS();
}

TEST(bn_from_int_min)
{
    bignum *bn = bn_from_int(INT64_MIN);
    ASSERT(bn != NULL);
    ASSERT(bn_sign(bn) < 0);

    int64_t value;
    ASSERT_EQ(bn_to_int64(bn, &value), 0);
    ASSERT_EQ(value, INT64_MIN);

    char *s = bn_to_string(bn, 10);
    ASSERT_STR_EQ(s, "-9223372036854775808");
    free(s);
    bn_free(bn);
    PASS();
}

TEST(bn_add_simple)
{
    bignum *a = bn_from_int(100);
    bignum *b = bn_from_int(200);
    bignum *c = bn_add(a, b);

    bignum *expected = bn_from_int(300);
    ASSERT_EQ(bn_cmp(c, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(c);
    bn_free(expected);
    PASS();
}

TEST(bn_add_negative)
{
    bignum *a = bn_from_int(100);
    bignum *b = bn_from_int(-50);
    bignum *c = bn_add(a, b);

    bignum *expected = bn_from_int(50);
    ASSERT_EQ(bn_cmp(c, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(c);
    bn_free(expected);
    PASS();
}

TEST(bn_sub_simple)
{
    bignum *a = bn_from_int(300);
    bignum *b = bn_from_int(100);
    bignum *c = bn_sub(a, b);

    bignum *expected = bn_from_int(200);
    ASSERT_EQ(bn_cmp(c, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(c);
    bn_free(expected);
    PASS();
}

TEST(bn_sub_to_negative)
{
    bignum *a = bn_from_int(100);
    bignum *b = bn_from_int(300);
    bignum *c = bn_sub(a, b);

    bignum *expected = bn_from_int(-200);
    ASSERT_EQ(bn_cmp(c, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(c);
    bn_free(expected);
    PASS();
}

TEST(bn_mul_simple)
{
    bignum *a = bn_from_int(12);
    bignum *b = bn_from_int(34);
    bignum *c = bn_mul(a, b);

    bignum *expected = bn_from_int(408);
    ASSERT_EQ(bn_cmp(c, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(c);
    bn_free(expected);
    PASS();
}

TEST(bn_mul_negative)
{
    bignum *a = bn_from_int(-12);
    bignum *b = bn_from_int(34);
    bignum *c = bn_mul(a, b);

    bignum *expected = bn_from_int(-408);
    ASSERT_EQ(bn_cmp(c, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(c);
    bn_free(expected);
    PASS();
}

static bignum *bn_make_all_ones(size_t limbs)
{
    bignum *one = bn_from_int(1);
    if (!one)
        return NULL;
    bignum *high = bn_lshift(one, LIMB_BITS * limbs);
    bignum *result = high ? bn_sub(high, one) : NULL;
    bn_free(high);
    bn_free(one);
    return result;
}

static void assert_all_ones_square_shape(const bignum *prod, size_t limbs)
{
    ASSERT(prod != NULL);
    ASSERT(prod->len == limbs * 2);
    ASSERT(prod->limbs[0] == 1);
    for (size_t i = 1; i < limbs; i++)
        ASSERT(prod->limbs[i] == 0);
    ASSERT(prod->limbs[limbs] == LIMB_MAX - 1);
    for (size_t i = limbs + 1; i < limbs * 2; i++)
        ASSERT(prod->limbs[i] == LIMB_MAX);
}

TEST(bn_mul_karatsuba_threshold)
{
    bignum *a = bn_make_all_ones(KARATSUBA_THRESHOLD);
    ASSERT(a != NULL);
    ASSERT(a->len == KARATSUBA_THRESHOLD);

    bignum *prod = bn_mul(a, a);
    assert_all_ones_square_shape(prod, KARATSUBA_THRESHOLD);

    bn_free(prod);
    bn_free(a);
    PASS();
}

TEST(bn_mul_large_karatsuba)
{
    const size_t limbs = KARATSUBA_THRESHOLD * 4;
    bignum *a = bn_make_all_ones(limbs);
    ASSERT(a != NULL);
    ASSERT(a->len == limbs);

    bignum *prod = bn_mul(a, a);
    assert_all_ones_square_shape(prod, limbs);

    bn_free(prod);
    bn_free(a);
    PASS();
}

TEST(bn_mul_large_power_matches_square)
{
    bignum *ten = bn_from_int(10);
    ASSERT(ten != NULL);
    bignum *a = bn_pow(ten, 2000);
    ASSERT(a != NULL);
    bignum *square = bn_mul(a, a);
    ASSERT(square != NULL);
    bignum *expected = bn_pow(ten, 4000);
    ASSERT(expected != NULL);
    ASSERT_EQ(bn_cmp(square, expected), 0);

    bignum *quotient = expected;
    expected = NULL;
    for (unsigned i = 0; i < 4000; i++) {
        bignum *remainder = NULL;
        bignum *next = bn_div(quotient, ten, &remainder);
        ASSERT(next != NULL);
        ASSERT(remainder != NULL);
        ASSERT(bn_is_zero(remainder));
        bn_free(remainder);
        bn_free(quotient);
        quotient = next;
    }
    bignum *one = bn_from_int(1);
    ASSERT(one != NULL);
    ASSERT_EQ(bn_cmp(quotient, one), 0);

    bn_free(ten);
    bn_free(a);
    bn_free(square);
    bn_free(quotient);
    bn_free(one);
    PASS();
}

TEST(bn_div_simple)
{
    bignum *a = bn_from_int(100);
    bignum *b = bn_from_int(7);
    bignum *q = bn_div(a, b, NULL);

    bignum *expected = bn_from_int(14);
    ASSERT_EQ(bn_cmp(q, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(q);
    bn_free(expected);
    PASS();
}

TEST(bn_div_with_remainder)
{
    bignum *a = bn_from_int(100);
    bignum *b = bn_from_int(7);
    bignum *r = NULL;
    bignum *q = bn_div(a, b, &r);

    bignum *exp_q = bn_from_int(14);
    bignum *exp_r = bn_from_int(2);
    ASSERT_EQ(bn_cmp(q, exp_q), 0);
    ASSERT_EQ(bn_cmp(r, exp_r), 0);

    bn_free(a);
    bn_free(b);
    bn_free(q);
    bn_free(r);
    bn_free(exp_q);
    bn_free(exp_r);
    PASS();
}

TEST(bn_mod_simple)
{
    bignum *a = bn_from_int(100);
    bignum *b = bn_from_int(7);
    bignum *r = bn_mod(a, b);

    bignum *expected = bn_from_int(2);
    ASSERT_EQ(bn_cmp(r, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(r);
    bn_free(expected);
    PASS();
}

TEST(bn_cmp_equal)
{
    bignum *a = bn_from_int(12345);
    bignum *b = bn_from_int(12345);
    ASSERT_EQ(bn_cmp(a, b), 0);
    bn_free(a);
    bn_free(b);
    PASS();
}

TEST(bn_cmp_less)
{
    bignum *a = bn_from_int(100);
    bignum *b = bn_from_int(200);
    ASSERT(bn_cmp(a, b) < 0);
    bn_free(a);
    bn_free(b);
    PASS();
}

TEST(bn_cmp_greater)
{
    bignum *a = bn_from_int(200);
    bignum *b = bn_from_int(100);
    ASSERT(bn_cmp(a, b) > 0);
    bn_free(a);
    bn_free(b);
    PASS();
}

TEST(bn_neg)
{
    bignum *a = bn_from_int(100);
    bignum *b = bn_neg(a);

    bignum *expected = bn_from_int(-100);
    ASSERT_EQ(bn_cmp(b, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(expected);
    PASS();
}

TEST(bn_abs_positive)
{
    bignum *a = bn_from_int(100);
    bignum *b = bn_abs(a);

    bignum *expected = bn_from_int(100);
    ASSERT_EQ(bn_cmp(b, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(expected);
    PASS();
}

TEST(bn_abs_negative)
{
    bignum *a = bn_from_int(-100);
    bignum *b = bn_abs(a);

    bignum *expected = bn_from_int(100);
    ASSERT_EQ(bn_cmp(b, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(expected);
    PASS();
}

TEST(bn_large_factorial)
{
    // Calculate 20! = 2432902008176640000
    bignum *result = bn_from_int(1);
    for (int i = 2; i <= 20; i++) {
        bignum *mul = bn_from_int(i);
        bignum *tmp = bn_mul(result, mul);
        bn_free(result);
        bn_free(mul);
        result = tmp;
    }

    // Check against known value
    // 20! = 2432902008176640000
    bignum *expected = bn_from_string("2432902008176640000", 10);
    ASSERT_EQ(bn_cmp(result, expected), 0);

    bn_free(result);
    bn_free(expected);
    PASS();
}

TEST(bn_from_string_positive)
{
    bignum *a = bn_from_string("12345", 10);
    bignum *expected = bn_from_int(12345);
    ASSERT_EQ(bn_cmp(a, expected), 0);
    bn_free(a);
    bn_free(expected);
    PASS();
}

TEST(bn_from_string_negative)
{
    bignum *a = bn_from_string("-12345", 10);
    bignum *expected = bn_from_int(-12345);
    ASSERT_EQ(bn_cmp(a, expected), 0);
    bn_free(a);
    bn_free(expected);
    PASS();
}

TEST(bn_from_string_rejects_trailing_junk)
{
    bignum *a = bn_from_string("123abc", 10);
    ASSERT(a == NULL);
    PASS();
}

TEST(bn_from_string_rejects_non_ascii_char)
{
    ASSERT(bn_from_string("\xFF", 10) == NULL);
    PASS();
}

TEST(bn_from_string_rejects_empty_digits)
{
    bignum *a = bn_from_string("-", 10);
    ASSERT(a == NULL);
    PASS();
}

TEST(bn_to_string_test)
{
    bignum *a = bn_from_int(12345);
    char *s = bn_to_string(a, 10);
    ASSERT_STR_EQ(s, "12345");
    free(s);
    bn_free(a);
    PASS();
}

TEST(bn_to_string_negative)
{
    bignum *a = bn_from_int(-12345);
    char *s = bn_to_string(a, 10);
    ASSERT_STR_EQ(s, "-12345");
    free(s);
    bn_free(a);
    PASS();
}

// ============================================================================
// In-place Operation Tests
// ============================================================================

TEST(bn_add_ip_simple)
{
    bignum *a = bn_from_int(100);
    bignum *b = bn_from_int(200);
    bn_add_ip(a, b);

    bignum *expected = bn_from_int(300);
    ASSERT_EQ(bn_cmp(a, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(expected);
    PASS();
}

TEST(bn_add_ip_negative)
{
    bignum *a = bn_from_int(100);
    bignum *b = bn_from_int(-150);
    bn_add_ip(a, b);

    bignum *expected = bn_from_int(-50);
    ASSERT_EQ(bn_cmp(a, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(expected);
    PASS();
}

TEST(bn_sub_ip_simple)
{
    bignum *a = bn_from_int(300);
    bignum *b = bn_from_int(100);
    bn_sub_ip(a, b);

    bignum *expected = bn_from_int(200);
    ASSERT_EQ(bn_cmp(a, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(expected);
    PASS();
}

TEST(bn_mul_limb_ip_test)
{
    bignum *a = bn_from_int(12345);
    bn_mul_limb_ip(a, 100);

    bignum *expected = bn_from_int(1234500);
    ASSERT_EQ(bn_cmp(a, expected), 0);

    bn_free(a);
    bn_free(expected);
    PASS();
}

TEST(bn_add_limb_ip_test)
{
    bignum *a = bn_from_int(999);
    bn_add_limb_ip(a, 1);

    bignum *expected = bn_from_int(1000);
    ASSERT_EQ(bn_cmp(a, expected), 0);

    bn_free(a);
    bn_free(expected);
    PASS();
}

TEST(bn_neg_ip_test)
{
    bignum *a = bn_from_int(42);
    bn_neg_ip(a);

    bignum *expected = bn_from_int(-42);
    ASSERT_EQ(bn_cmp(a, expected), 0);

    bn_free(a);
    bn_free(expected);
    PASS();
}

TEST(bn_add_ip_large)
{
    // Test with numbers that span multiple limbs
    bignum *a = bn_from_string("10000000000000000000", 10);
    bignum *b = bn_from_string("20000000000000000000", 10);
    bn_add_ip(a, b);

    bignum *expected = bn_from_string("30000000000000000000", 10);
    ASSERT_EQ(bn_cmp(a, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(expected);
    PASS();
}

TEST(bn_checked_in_place_ops)
{
    bignum *a = bn_from_int(100);
    bignum *b = bn_from_int(250);
    ASSERT(bn_add_ip_checked(a, b));

    bignum *expected = bn_from_int(350);
    ASSERT_EQ(bn_cmp(a, expected), 0);
    bn_free(expected);

    ASSERT(bn_sub_ip_checked(a, b));
    expected = bn_from_int(100);
    ASSERT_EQ(bn_cmp(a, expected), 0);
    bn_free(expected);

    ASSERT(bn_mul_limb_ip_checked(a, 42));
    expected = bn_from_int(4200);
    ASSERT_EQ(bn_cmp(a, expected), 0);
    bn_free(expected);

    ASSERT(bn_add_limb_ip_checked(a, 1));
    expected = bn_from_int(4201);
    ASSERT_EQ(bn_cmp(a, expected), 0);
    bn_free(expected);

    bn_free(a);
    bn_free(b);
    PASS();
}

TEST(bn_checked_add_limb_negative)
{
    bignum *a = bn_from_int(-100);
    ASSERT(bn_add_limb_ip_checked(a, 25));

    bignum *expected = bn_from_int(-75);
    ASSERT_EQ(bn_cmp(a, expected), 0);

    bn_free(a);
    bn_free(expected);
    PASS();
}

TEST(bn_gcd_large)
{
    bignum *a = bn_from_string("123456789012345678901234567890", 10);
    bignum *b = bn_from_string("98765432109876543210", 10);
    ASSERT(a != NULL);
    ASSERT(b != NULL);

    bignum *gcd = bn_gcd(a, b);
    bignum *expected = bn_from_int(90);
    ASSERT(gcd != NULL);
    ASSERT(expected != NULL);
    ASSERT_EQ(bn_cmp(gcd, expected), 0);

    bn_free(a);
    bn_free(b);
    bn_free(gcd);
    bn_free(expected);
    PASS();
}

// ============================================================================
// Randomized Cross-checks
// ============================================================================

static uint64_t test_rng_state = 0x9e3779b97f4a7c15ULL;

static limb_t test_random_limb(void)
{
    test_rng_state ^= test_rng_state << 13;
    test_rng_state ^= test_rng_state >> 7;
    test_rng_state ^= test_rng_state << 17;
    return (limb_t)(test_rng_state >> 16);
}

// Operand shapes that stress the multiplication splits: dense random limbs,
// all ones, runs of zero limbs, and a zero low half.
static void fill_test_limbs(limb_t *out, size_t len, int pattern)
{
    for (size_t i = 0; i < len; i++) {
        switch (pattern) {
        case 0:
            out[i] = test_random_limb();
            break;
        case 1:
            out[i] = LIMB_MAX;
            break;
        case 2:
            out[i] = (i / 7) % 2 ? 0 : test_random_limb();
            break;
        default:
            out[i] = i < len / 2 ? 0 : test_random_limb();
            break;
        }
    }
    out[len - 1] |= 1; // Keep the top limb nonzero so len is exact
}

static bignum *random_test_bignum(size_t len, int pattern, int sign)
{
    limb_t *limbs = malloc(len * sizeof(limb_t));
    if (!limbs)
        return NULL;
    fill_test_limbs(limbs, len, pattern);
    bignum *result = bn_from_limbs(limbs, len, sign);
    free(limbs);
    return result;
}

TEST(bn_from_limbs_normalizes)
{
    const limb_t five[] = {5, 0, 0};
    bignum *a = bn_from_limbs(five, 3, 1);
    bignum *expected = bn_from_int(-5);
    ASSERT(a != NULL && expected != NULL);
    ASSERT_EQ(a->len, 1);
    ASSERT_EQ(bn_cmp(a, expected), 0);
    bn_free(a);
    bn_free(expected);

    const limb_t zeros[] = {0, 0};
    bignum *zero = bn_from_limbs(zeros, 2, 1);
    ASSERT(zero != NULL);
    ASSERT(bn_is_zero(zero));
    ASSERT_EQ(bn_sign(zero), 0);
    ASSERT_EQ(zero->sign, 0);
    bn_free(zero);

    const limb_t two_32[] = {0, 1};
    bignum *b = bn_from_limbs(two_32, 2, 0);
    bignum *expected_b = bn_from_int(INT64_C(1) << 32);
    ASSERT(b != NULL && expected_b != NULL);
    ASSERT_EQ(bn_cmp(b, expected_b), 0);
    bn_free(b);
    bn_free(expected_b);
    PASS();
}

static size_t reference_product(const bignum *a, const bignum *b, limb_t *out)
{
    size_t len = a->len + b->len;
    memset(out, 0, len * sizeof(limb_t));
    for (size_t i = 0; i < a->len; i++) {
        uint64_t carry = 0;
        for (size_t j = 0; j < b->len; j++) {
            uint64_t t =
                (uint64_t)a->limbs[i] * b->limbs[j] + out[i + j] + carry;
            out[i + j] = (limb_t)t;
            carry = t >> LIMB_BITS;
        }
        out[i + b->len] = (limb_t)carry;
    }
    while (len > 0 && out[len - 1] == 0)
        len--;
    return len;
}

TEST(bn_mul_matches_reference_for_all_shapes)
{
    // Lengths around the Karatsuba threshold and its multiples, including
    // pairs at least twice as long as each other (the unbalanced path).
    static const size_t lengths[] = {1,  5,  31, 32,  33,  47,  64,
                                     65, 96, 127, 200, 333, 700};
    const size_t count = sizeof(lengths) / sizeof(lengths[0]);
    limb_t *expected = malloc(2 * 700 * sizeof(limb_t));
    ASSERT(expected != NULL);
    for (size_t i = 0; i < count; i++) {
        for (size_t j = 0; j < count; j++) {
            int pattern_a = (int)((i + j) % 4);
            int pattern_b = (int)((i * 3 + j) % 4);
            int sign_a = (int)(i & 1), sign_b = (int)((j >> 1) & 1);
            bignum *a = random_test_bignum(lengths[i], pattern_a, sign_a);
            bignum *b = random_test_bignum(lengths[j], pattern_b, sign_b);
            ASSERT(a != NULL && b != NULL);
            ASSERT_EQ(a->len, lengths[i]);
            ASSERT_EQ(b->len, lengths[j]);
            bignum *prod = bn_mul(a, b);
            ASSERT(prod != NULL);
            size_t expected_len = reference_product(a, b, expected);
            ASSERT_EQ(prod->len, expected_len);
            ASSERT(memcmp(prod->limbs, expected,
                          expected_len * sizeof(limb_t)) == 0);
            ASSERT_EQ(prod->sign, sign_a != sign_b);
            bn_free(a);
            bn_free(b);
            bn_free(prod);
        }
    }
    free(expected);
    PASS();
}

TEST(bn_div_random_identity)
{
    static const size_t lengths[] = {1, 2, 3, 17, 40, 41, 120};
    const size_t count = sizeof(lengths) / sizeof(lengths[0]);
    for (size_t i = 0; i < count; i++) {
        for (size_t j = 0; j < count; j++) {
            int sign_a = (int)(i & 1), sign_b = (int)(j % 3 == 1);
            bignum *a = random_test_bignum(lengths[i], (int)(i % 4), sign_a);
            bignum *b = random_test_bignum(lengths[j], (int)(j % 3), sign_b);
            ASSERT(a != NULL && b != NULL);
            bignum *r = NULL;
            bignum *q = bn_div(a, b, &r);
            ASSERT(q != NULL && r != NULL);
            // Truncating division: a = q*b + r, |r| < |b|, r has a's sign
            ASSERT(bn_cmp_abs(r, b) < 0);
            ASSERT(bn_is_zero(r) || r->sign == a->sign);
            ASSERT(bn_is_zero(q) || q->sign == (sign_a != sign_b));
            bignum *qb = bn_mul(q, b);
            ASSERT(qb != NULL);
            bignum *sum = bn_add(qb, r);
            ASSERT(sum != NULL);
            ASSERT_EQ(bn_cmp(sum, a), 0);
            bignum *q_only = bn_div(a, b, NULL);
            ASSERT(q_only != NULL);
            ASSERT_EQ(bn_cmp(q_only, q), 0);
            bn_free(a);
            bn_free(b);
            bn_free(q);
            bn_free(r);
            bn_free(qb);
            bn_free(sum);
            bn_free(q_only);
        }
    }
    PASS();
}

// Conversion one digit per division: the obviously correct algorithm.
static char *reference_to_string(const bignum *a, int base)
{
    size_t cap = a->len * LIMB_BITS + 3;
    char *buf = malloc(cap);
    bignum *divisor = bn_from_int(base);
    bignum *value = bn_abs(a);
    size_t pos = 0;
    if (!buf || !divisor || !value) {
        free(buf);
        bn_free(divisor);
        bn_free(value);
        return NULL;
    }
    do {
        bignum *rem = NULL;
        bignum *quot = bn_div(value, divisor, &rem);
        int digit = rem && rem->len ? (int)rem->limbs[0] : 0;
        buf[pos++] = (char)(digit < 10 ? '0' + digit : 'a' + digit - 10);
        bn_free(rem);
        bn_free(value);
        value = quot;
    } while (value && !bn_is_zero(value));
    if (a->sign)
        buf[pos++] = '-';
    buf[pos] = '\0';
    for (size_t i = 0; i < pos / 2; i++) {
        char t = buf[i];
        buf[i] = buf[pos - 1 - i];
        buf[pos - 1 - i] = t;
    }
    bn_free(divisor);
    bn_free(value);
    return buf;
}

TEST(bn_string_conversion_matches_reference)
{
    static const size_t lengths[] = {1, 2, 3, 15, 16, 17, 40, 150};
    static const int bases[] = {2, 3, 7, 10, 16, 36};
    for (size_t i = 0; i < sizeof(lengths) / sizeof(lengths[0]); i++) {
        for (size_t k = 0; k < sizeof(bases) / sizeof(bases[0]); k++) {
            bignum *a = random_test_bignum(lengths[i], (int)(i % 4),
                                           (int)((i + k) & 1));
            ASSERT(a != NULL);
            char *expected = reference_to_string(a, bases[k]);
            char *actual = bn_to_string(a, bases[k]);
            ASSERT(expected != NULL && actual != NULL);
            ASSERT_STR_EQ(actual, expected);
            bignum *parsed = bn_from_string(actual, bases[k]);
            ASSERT(parsed != NULL);
            ASSERT_EQ(bn_cmp(parsed, a), 0);
            free(expected);
            free(actual);
            bn_free(parsed);
            bn_free(a);
        }
    }
    PASS();
}

// Values at the edges of the digit chunks that conversion handles per limb
// (9 decimal digits, 7 hex digits): interior chunks must be zero padded and
// the leading chunk must not be.
TEST(bn_string_conversion_chunk_boundaries)
{
    static const struct {
        const char *text;
        int base;
    } cases[] = {
        {"999999999", 10},
        {"1000000000", 10},
        {"1000000001", 10},
        {"999999999999999999", 10},
        {"1000000000000000000", 10},
        {"-1000000000000000000000000001", 10},
        {"100000000000000000000000000000000000000000000000000", 10},
        {"fffffff", 16},
        {"10000000", 16},
        {"-100000000000000000000000000000000000000f", 16},
        {"10000000000000000000000000000000000000000000000000000000000000000",
         2},
        {"zzzzzz", 36},
        {"1000000", 36},
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        bignum *a = bn_from_string(cases[i].text, cases[i].base);
        ASSERT(a != NULL);
        char *expected = reference_to_string(a, cases[i].base);
        char *actual = bn_to_string(a, cases[i].base);
        ASSERT(expected != NULL && actual != NULL);
        ASSERT_STR_EQ(expected, cases[i].text);
        ASSERT_STR_EQ(actual, cases[i].text);
        free(expected);
        free(actual);
        bn_free(a);
    }
    bignum *billion = bn_from_string("1000000000", 10);
    bignum *expected = bn_from_int(1000000000);
    ASSERT(billion != NULL && expected != NULL);
    ASSERT_EQ(bn_cmp(billion, expected), 0);
    bn_free(billion);
    bn_free(expected);
    PASS();
}

// ============================================================================
// Main
// ============================================================================

int main(void)
{
    printf("=== Bignum Unit Tests ===\n");

    RUN_TEST(bn_from_int_zero);
    RUN_TEST(bn_from_int_positive);
    RUN_TEST(bn_from_int_negative);
    RUN_TEST(bn_from_int_min);
    RUN_TEST(bn_add_simple);
    RUN_TEST(bn_add_negative);
    RUN_TEST(bn_sub_simple);
    RUN_TEST(bn_sub_to_negative);
    RUN_TEST(bn_mul_simple);
    RUN_TEST(bn_mul_negative);
    RUN_TEST(bn_mul_karatsuba_threshold);
    RUN_TEST(bn_mul_large_karatsuba);
    RUN_TEST(bn_mul_large_power_matches_square);
    RUN_TEST(bn_div_simple);
    RUN_TEST(bn_div_with_remainder);
    RUN_TEST(bn_mod_simple);
    RUN_TEST(bn_cmp_equal);
    RUN_TEST(bn_cmp_less);
    RUN_TEST(bn_cmp_greater);
    RUN_TEST(bn_neg);
    RUN_TEST(bn_abs_positive);
    RUN_TEST(bn_abs_negative);
    RUN_TEST(bn_large_factorial);
    RUN_TEST(bn_from_string_positive);
    RUN_TEST(bn_from_string_negative);
    RUN_TEST(bn_from_string_rejects_trailing_junk);
    RUN_TEST(bn_from_string_rejects_non_ascii_char);
    RUN_TEST(bn_from_string_rejects_empty_digits);
    RUN_TEST(bn_to_string_test);
    RUN_TEST(bn_to_string_negative);

    // In-place operations
    RUN_TEST(bn_add_ip_simple);
    RUN_TEST(bn_add_ip_negative);
    RUN_TEST(bn_sub_ip_simple);
    RUN_TEST(bn_mul_limb_ip_test);
    RUN_TEST(bn_add_limb_ip_test);
    RUN_TEST(bn_neg_ip_test);
    RUN_TEST(bn_add_ip_large);
    RUN_TEST(bn_checked_in_place_ops);
    RUN_TEST(bn_checked_add_limb_negative);
    RUN_TEST(bn_gcd_large);

    // Randomized cross-checks against reference algorithms
    RUN_TEST(bn_from_limbs_normalizes);
    RUN_TEST(bn_mul_matches_reference_for_all_shapes);
    RUN_TEST(bn_div_random_identity);
    RUN_TEST(bn_string_conversion_matches_reference);
    RUN_TEST(bn_string_conversion_chunk_boundaries);

    TEST_SUMMARY("bignum");
}
