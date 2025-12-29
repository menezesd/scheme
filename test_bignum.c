// Unit tests for bignum module
#include "bignum.h"
#include "test_framework.h"
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

// ============================================================================
// Main
// ============================================================================

int main(void)
{
    printf("=== Bignum Unit Tests ===\n");

    RUN_TEST(bn_from_int_zero);
    RUN_TEST(bn_from_int_positive);
    RUN_TEST(bn_from_int_negative);
    RUN_TEST(bn_add_simple);
    RUN_TEST(bn_add_negative);
    RUN_TEST(bn_sub_simple);
    RUN_TEST(bn_sub_to_negative);
    RUN_TEST(bn_mul_simple);
    RUN_TEST(bn_mul_negative);
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

    TEST_SUMMARY("bignum");
}
