#include "bignum.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ============================================================================
// Internal Helpers
// ============================================================================

static bignum *bn_alloc(size_t cap)
{
    bignum *b = malloc(sizeof(bignum));
    if (!b)
        return NULL;
    b->limbs = calloc(cap, sizeof(limb_t));
    if (!b->limbs) {
        free(b);
        return NULL;
    }
    b->len = 0;
    b->cap = cap;
    b->sign = 0;
    return b;
}

static void bn_ensure_cap(bignum *b, size_t cap)
{
    if (b->cap >= cap)
        return;
    size_t new_cap = b->cap * 2;
    if (new_cap < cap)
        new_cap = cap;
    limb_t *new_limbs = realloc(b->limbs, new_cap * sizeof(limb_t));
    if (!new_limbs) {
        fprintf(stderr, "bignum: out of memory\n");
        abort();
    }
    b->limbs = new_limbs;
    memset(b->limbs + b->cap, 0, (new_cap - b->cap) * sizeof(limb_t));
    b->cap = new_cap;
}

// Remove leading zeros
static void bn_normalize(bignum *b)
{
    while (b->len > 0 && b->limbs[b->len - 1] == 0) {
        b->len--;
    }
    if (b->len == 0) {
        b->sign = 0; // Zero is positive
    }
}

// ============================================================================
// Creation and Destruction
// ============================================================================

bignum *bn_new(void) { return bn_alloc(4); }

bignum *bn_from_int(int64_t val)
{
    bignum *b = bn_alloc(2);
    if (!b)
        return NULL;

    if (val < 0) {
        b->sign = 1;
        val = -val;
    }

    uint64_t uval = (uint64_t)val;
    if (uval == 0) {
        b->len = 0;
    } else if (uval <= LIMB_MAX) {
        b->limbs[0] = (limb_t)uval;
        b->len = 1;
    } else {
        b->limbs[0] = (limb_t)(uval & LIMB_MAX);
        b->limbs[1] = (limb_t)(uval >> LIMB_BITS);
        b->len = 2;
    }
    bn_normalize(b);
    return b;
}

bignum *bn_from_uint(uint64_t val)
{
    bignum *b = bn_alloc(2);
    if (!b)
        return NULL;

    if (val == 0) {
        b->len = 0;
    } else if (val <= LIMB_MAX) {
        b->limbs[0] = (limb_t)val;
        b->len = 1;
    } else {
        b->limbs[0] = (limb_t)(val & LIMB_MAX);
        b->limbs[1] = (limb_t)(val >> LIMB_BITS);
        b->len = 2;
    }
    return b;
}

bignum *bn_from_string(const char *str, int base)
{
    if (!str || base < 2 || base > 36)
        return NULL;

    bignum *result = bn_new();
    if (!result)
        return NULL;

    int sign = 0;
    if (*str == '-') {
        sign = 1;
        str++;
    } else if (*str == '+') {
        str++;
    }

    // Use in-place operations to avoid allocations per digit
    // Since base <= 36 fits in a single limb, we can use bn_mul_limb_ip
    // and bn_add_limb_ip for efficient parsing
    while (*str) {
        int digit;
        if (isdigit(*str)) {
            digit = *str - '0';
        } else if (isalpha(*str)) {
            digit = tolower(*str) - 'a' + 10;
        } else {
            break;
        }

        if (digit >= base)
            break;

        // result = result * base + digit (all in-place, no allocations)
        bn_mul_limb_ip(result, (limb_t)base);
        bn_add_limb_ip(result, (limb_t)digit);

        str++;
    }

    result->sign = sign;
    bn_normalize(result);
    return result;
}

bignum *bn_copy(const bignum *a)
{
    if (!a)
        return NULL;
    bignum *b = bn_alloc(a->len > 0 ? a->len : 1);
    if (!b)
        return NULL;
    memcpy(b->limbs, a->limbs, a->len * sizeof(limb_t));
    b->len = a->len;
    b->sign = a->sign;
    return b;
}

void bn_free(bignum *a)
{
    if (a) {
        free(a->limbs);
        free(a);
    }
}

// ============================================================================
// Conversion
// ============================================================================

bool bn_fits_int64(const bignum *a)
{
    if (a->len == 0)
        return true;
    if (a->len > 2)
        return false;
    if (a->len == 1)
        return true;

    // len == 2
    uint64_t val = ((uint64_t)a->limbs[1] << LIMB_BITS) | a->limbs[0];
    if (a->sign) {
        return val <= (uint64_t)INT64_MAX + 1;
    } else {
        return val <= (uint64_t)INT64_MAX;
    }
}

int bn_to_int64(const bignum *a, int64_t *out)
{
    if (!bn_fits_int64(a))
        return -1;

    uint64_t val = 0;
    if (a->len >= 1)
        val = a->limbs[0];
    if (a->len >= 2)
        val |= (uint64_t)a->limbs[1] << LIMB_BITS;

    if (a->sign) {
        if (val == (uint64_t)INT64_MAX + 1) {
            *out = INT64_MIN;
        } else {
            *out = -(int64_t)val;
        }
    } else {
        *out = (int64_t)val;
    }
    return 0;
}

int bn_to_uint64(const bignum *a, uint64_t *out)
{
    if (a->sign && a->len > 0)
        return -1; // Negative
    if (a->len > 2)
        return -1;

    uint64_t val = 0;
    if (a->len >= 1)
        val = a->limbs[0];
    if (a->len >= 2)
        val |= (uint64_t)a->limbs[1] << LIMB_BITS;

    *out = val;
    return 0;
}

char *bn_to_string(const bignum *a, int base)
{
    if (!a || base < 2 || base > 36)
        return NULL;

    if (a->len == 0) {
        char *s = malloc(2);
        s[0] = '0';
        s[1] = '\0';
        return s;
    }

    // Estimate size: log_base(2^(len*32)) + sign + null
    size_t est_digits = (a->len * LIMB_BITS) / 3 + 3;
    char *buf = malloc(est_digits);
    if (!buf)
        return NULL;

    bignum *tmp = bn_copy(a);
    tmp->sign = 0; // Work with absolute value

    size_t pos = 0;
    bignum *base_bn = bn_from_int(base);

    while (!bn_is_zero(tmp)) {
        bignum *rem = NULL;
        bignum *quot = bn_div(tmp, base_bn, &rem);

        int digit = rem->len > 0 ? rem->limbs[0] : 0;
        if (digit < 10) {
            buf[pos++] = '0' + digit;
        } else {
            buf[pos++] = 'a' + (digit - 10);
        }

        bn_free(rem);
        bn_free(tmp);
        tmp = quot;
    }

    bn_free(tmp);
    bn_free(base_bn);

    // Add sign
    if (a->sign) {
        buf[pos++] = '-';
    }
    buf[pos] = '\0';

    // Reverse the string
    for (size_t i = 0; i < pos / 2; i++) {
        char t = buf[i];
        buf[i] = buf[pos - 1 - i];
        buf[pos - 1 - i] = t;
    }

    return buf;
}

// ============================================================================
// Comparison
// ============================================================================

int bn_cmp_abs(const bignum *a, const bignum *b)
{
    if (a->len != b->len) {
        return a->len > b->len ? 1 : -1;
    }

    for (size_t i = a->len; i > 0; i--) {
        if (a->limbs[i - 1] != b->limbs[i - 1]) {
            return a->limbs[i - 1] > b->limbs[i - 1] ? 1 : -1;
        }
    }
    return 0;
}

int bn_cmp(const bignum *a, const bignum *b)
{
    // Handle signs
    if (a->sign != b->sign) {
        if (bn_is_zero(a) && bn_is_zero(b))
            return 0;
        return a->sign ? -1 : 1;
    }

    int cmp = bn_cmp_abs(a, b);
    return a->sign ? -cmp : cmp;
}

bool bn_is_zero(const bignum *a) { return a->len == 0; }

int bn_sign(const bignum *a)
{
    if (a->len == 0)
        return 0;
    return a->sign ? -1 : 1;
}

// ============================================================================
// Low-level Arithmetic (unsigned)
// ============================================================================

// Add absolute values: result = |a| + |b|
static bignum *bn_add_abs(const bignum *a, const bignum *b)
{
    size_t max_len = (a->len > b->len ? a->len : b->len) + 1;
    bignum *result = bn_alloc(max_len);
    if (!result)
        return NULL;

    dlimb_t carry = 0;
    size_t i;
    for (i = 0; i < max_len; i++) {
        dlimb_t sum = carry;
        if (i < a->len)
            sum += a->limbs[i];
        if (i < b->len)
            sum += b->limbs[i];
        result->limbs[i] = (limb_t)(sum & LIMB_MAX);
        carry = sum >> LIMB_BITS;
    }
    result->len = max_len;
    bn_normalize(result);
    return result;
}

// Subtract absolute values: result = |a| - |b|, assumes |a| >= |b|
static bignum *bn_sub_abs(const bignum *a, const bignum *b)
{
    bignum *result = bn_alloc(a->len);
    if (!result)
        return NULL;

    int64_t borrow = 0;
    for (size_t i = 0; i < a->len; i++) {
        int64_t diff = (int64_t)a->limbs[i] - borrow;
        if (i < b->len)
            diff -= b->limbs[i];
        if (diff < 0) {
            diff += (int64_t)1 << LIMB_BITS;
            borrow = 1;
        } else {
            borrow = 0;
        }
        result->limbs[i] = (limb_t)diff;
    }
    result->len = a->len;
    bn_normalize(result);
    return result;
}

// ============================================================================
// Signed Arithmetic
// ============================================================================

bignum *bn_add(const bignum *a, const bignum *b)
{
    if (a->sign == b->sign) {
        // Same sign: add absolute values
        bignum *result = bn_add_abs(a, b);
        result->sign = a->sign;
        return result;
    } else {
        // Different signs: subtract absolute values
        int cmp = bn_cmp_abs(a, b);
        if (cmp == 0) {
            return bn_new(); // Zero
        } else if (cmp > 0) {
            bignum *result = bn_sub_abs(a, b);
            result->sign = a->sign;
            return result;
        } else {
            bignum *result = bn_sub_abs(b, a);
            result->sign = b->sign;
            return result;
        }
    }
}

bignum *bn_sub(const bignum *a, const bignum *b)
{
    // a - b = a + (-b)
    bignum *neg_b = bn_copy(b);
    neg_b->sign = !neg_b->sign;
    bignum *result = bn_add(a, neg_b);
    bn_free(neg_b);
    bn_normalize(result);
    return result;
}

bignum *bn_neg(const bignum *a)
{
    bignum *result = bn_copy(a);
    if (result->len > 0) {
        result->sign = !result->sign;
    }
    return result;
}

bignum *bn_abs(const bignum *a)
{
    bignum *result = bn_copy(a);
    result->sign = 0;
    return result;
}

// ============================================================================
// Multiplication
// ============================================================================

// Schoolbook multiplication O(n^2)
static bignum *bn_mul_schoolbook(const bignum *a, const bignum *b)
{
    if (a->len == 0 || b->len == 0) {
        return bn_new();
    }

    size_t result_len = a->len + b->len;
    bignum *result = bn_alloc(result_len);
    if (!result)
        return NULL;
    result->len = result_len;

    for (size_t i = 0; i < a->len; i++) {
        dlimb_t carry = 0;
        for (size_t j = 0; j < b->len; j++) {
            dlimb_t prod = (dlimb_t)a->limbs[i] * b->limbs[j];
            prod += result->limbs[i + j] + carry;
            result->limbs[i + j] = (limb_t)(prod & LIMB_MAX);
            carry = prod >> LIMB_BITS;
        }
        result->limbs[i + b->len] += (limb_t)carry;
    }

    bn_normalize(result);
    return result;
}

// Split bignum at position k: low = a mod B^k, high = a / B^k
static void bn_split(const bignum *a, size_t k, bignum **low, bignum **high)
{
    *low = bn_alloc(k > a->len ? a->len : k);
    *high = bn_alloc(a->len > k ? a->len - k : 1);

    size_t low_len = k < a->len ? k : a->len;
    memcpy((*low)->limbs, a->limbs, low_len * sizeof(limb_t));
    (*low)->len = low_len;
    bn_normalize(*low);

    if (a->len > k) {
        memcpy((*high)->limbs, a->limbs + k, (a->len - k) * sizeof(limb_t));
        (*high)->len = a->len - k;
        bn_normalize(*high);
    }
}

// Shift left by k limbs (multiply by B^k)
static bignum *bn_lshift_limbs(const bignum *a, size_t k)
{
    if (a->len == 0)
        return bn_new();

    bignum *result = bn_alloc(a->len + k);
    if (!result)
        return NULL;

    memset(result->limbs, 0, k * sizeof(limb_t));
    memcpy(result->limbs + k, a->limbs, a->len * sizeof(limb_t));
    result->len = a->len + k;
    return result;
}

// Karatsuba multiplication O(n^1.585)
static bignum *bn_mul_karatsuba(const bignum *a, const bignum *b)
{
    // Base case: use schoolbook for small numbers
    if (a->len < KARATSUBA_THRESHOLD || b->len < KARATSUBA_THRESHOLD) {
        return bn_mul_schoolbook(a, b);
    }

    // Split at half of the larger length
    size_t k = (a->len > b->len ? a->len : b->len) / 2;

    bignum *a0, *a1, *b0, *b1;
    bn_split(a, k, &a0, &a1);
    bn_split(b, k, &b0, &b1);

    // z0 = a0 * b0
    bignum *z0 = bn_mul_karatsuba(a0, b0);

    // z2 = a1 * b1
    bignum *z2 = bn_mul_karatsuba(a1, b1);

    // z1 = (a0 + a1) * (b0 + b1) - z0 - z2
    bignum *a0_plus_a1 = bn_add(a0, a1);
    bignum *b0_plus_b1 = bn_add(b0, b1);
    bignum *z1_temp = bn_mul_karatsuba(a0_plus_a1, b0_plus_b1);
    bignum *z1_temp2 = bn_sub(z1_temp, z0);
    bignum *z1 = bn_sub(z1_temp2, z2);

    bn_free(a0);
    bn_free(a1);
    bn_free(b0);
    bn_free(b1);
    bn_free(a0_plus_a1);
    bn_free(b0_plus_b1);
    bn_free(z1_temp);
    bn_free(z1_temp2);

    // result = z0 + z1 * B^k + z2 * B^(2k)
    bignum *z1_shifted = bn_lshift_limbs(z1, k);
    bignum *z2_shifted = bn_lshift_limbs(z2, 2 * k);

    bignum *temp = bn_add(z0, z1_shifted);
    bignum *result = bn_add(temp, z2_shifted);

    bn_free(z0);
    bn_free(z1);
    bn_free(z2);
    bn_free(z1_shifted);
    bn_free(z2_shifted);
    bn_free(temp);

    return result;
}

bignum *bn_mul(const bignum *a, const bignum *b)
{
    bignum *result = bn_mul_karatsuba(a, b);
    result->sign = (a->sign != b->sign) && !bn_is_zero(result);
    return result;
}

// ============================================================================
// Division
// ============================================================================

// Single-limb division helper
static limb_t bn_div_limb(bignum *a, limb_t d)
{
    dlimb_t rem = 0;
    for (size_t i = a->len; i > 0; i--) {
        dlimb_t cur = (rem << LIMB_BITS) | a->limbs[i - 1];
        a->limbs[i - 1] = (limb_t)(cur / d);
        rem = cur % d;
    }
    bn_normalize(a);
    return (limb_t)rem;
}

// Long division for multi-limb divisor
bignum *bn_div(const bignum *a, const bignum *b, bignum **remainder)
{
    if (bn_is_zero(b)) {
        // Division by zero
        if (remainder)
            *remainder = bn_new();
        return bn_new();
    }

    int result_sign = (a->sign != b->sign);

    // Work with absolute values
    bignum *dividend = bn_abs(a);
    bignum *divisor = bn_abs(b);

    int cmp = bn_cmp_abs(dividend, divisor);
    if (cmp < 0) {
        // |a| < |b|, quotient is 0
        if (remainder) {
            *remainder = bn_copy(a);
        }
        bn_free(dividend);
        bn_free(divisor);
        return bn_new();
    }

    if (divisor->len == 1) {
        // Single-limb divisor - use fast path
        bignum *quot = bn_copy(dividend);
        limb_t rem = bn_div_limb(quot, divisor->limbs[0]);
        quot->sign = result_sign && !bn_is_zero(quot);
        if (remainder) {
            *remainder = bn_from_int(rem);
            (*remainder)->sign = a->sign && rem != 0;
        }
        bn_free(dividend);
        bn_free(divisor);
        return quot;
    }

    // Multi-limb division using Algorithm D (Knuth)
    // Simplified version: binary long division
    bignum *quotient = bn_new();
    bn_ensure_cap(quotient, dividend->len - divisor->len + 1);

    // Binary search / shift-subtract method
    while (bn_cmp_abs(dividend, divisor) >= 0) {
        // Find how many bits to shift divisor to align with dividend
        size_t shift = 0;
        bignum *shifted = bn_copy(divisor);

        while (bn_cmp_abs(bn_lshift(shifted, 1), dividend) <= 0) {
            bignum *tmp = bn_lshift(shifted, 1);
            bn_free(shifted);
            shifted = tmp;
            shift++;
        }

        // Subtract and set quotient bit
        bignum *tmp = bn_sub(dividend, shifted);
        bn_free(dividend);
        dividend = tmp;

        // Add 2^shift to quotient
        size_t limb_idx = shift / LIMB_BITS;
        size_t bit_idx = shift % LIMB_BITS;
        bn_ensure_cap(quotient, limb_idx + 1);
        if (quotient->len <= limb_idx) {
            quotient->len = limb_idx + 1;
        }
        quotient->limbs[limb_idx] |= (limb_t)1 << bit_idx;

        bn_free(shifted);
    }

    bn_normalize(quotient);
    quotient->sign = result_sign && !bn_is_zero(quotient);

    if (remainder) {
        *remainder = dividend;
        (*remainder)->sign = a->sign && !bn_is_zero(dividend);
    } else {
        bn_free(dividend);
    }

    bn_free(divisor);
    return quotient;
}

bignum *bn_mod(const bignum *a, const bignum *b)
{
    bignum *rem = NULL;
    bignum *quot = bn_div(a, b, &rem);
    bn_free(quot);
    return rem;
}

// ============================================================================
// Bit Shifting
// ============================================================================

bignum *bn_lshift(const bignum *a, size_t bits)
{
    if (bn_is_zero(a) || bits == 0)
        return bn_copy(a);

    size_t limb_shift = bits / LIMB_BITS;
    size_t bit_shift = bits % LIMB_BITS;

    size_t new_len = a->len + limb_shift + 1;
    bignum *result = bn_alloc(new_len);
    if (!result)
        return NULL;

    result->sign = a->sign;

    // Shift by whole limbs
    memset(result->limbs, 0, limb_shift * sizeof(limb_t));

    // Shift remaining bits
    limb_t carry = 0;
    for (size_t i = 0; i < a->len; i++) {
        dlimb_t val = ((dlimb_t)a->limbs[i] << bit_shift) | carry;
        result->limbs[i + limb_shift] = (limb_t)(val & LIMB_MAX);
        carry = (limb_t)(val >> LIMB_BITS);
    }
    if (carry) {
        result->limbs[a->len + limb_shift] = carry;
    }

    result->len = new_len;
    bn_normalize(result);
    return result;
}

bignum *bn_rshift(const bignum *a, size_t bits)
{
    if (bn_is_zero(a))
        return bn_new();

    size_t limb_shift = bits / LIMB_BITS;
    size_t bit_shift = bits % LIMB_BITS;

    if (limb_shift >= a->len)
        return bn_new();

    size_t new_len = a->len - limb_shift;
    bignum *result = bn_alloc(new_len);
    if (!result)
        return NULL;

    result->sign = a->sign;

    for (size_t i = 0; i < new_len; i++) {
        result->limbs[i] = a->limbs[i + limb_shift] >> bit_shift;
        if (bit_shift > 0 && i + limb_shift + 1 < a->len) {
            result->limbs[i] |= a->limbs[i + limb_shift + 1]
                                << (LIMB_BITS - bit_shift);
        }
    }

    result->len = new_len;
    bn_normalize(result);
    return result;
}

// ============================================================================
// Other Operations
// ============================================================================

bignum *bn_gcd(const bignum *a, const bignum *b)
{
    bignum *x = bn_abs(a);
    bignum *y = bn_abs(b);

    while (!bn_is_zero(y)) {
        bignum *rem = bn_mod(x, y);
        bn_free(x);
        x = y;
        y = rem;
    }

    bn_free(y);
    return x;
}

bignum *bn_pow(const bignum *base, uint64_t exp)
{
    if (exp == 0)
        return bn_from_int(1);

    bignum *result = bn_from_int(1);
    bignum *b = bn_copy(base);

    while (exp > 0) {
        if (exp & 1) {
            bignum *tmp = bn_mul(result, b);
            bn_free(result);
            result = tmp;
        }
        exp >>= 1;
        if (exp > 0) {
            bignum *tmp = bn_mul(b, b);
            bn_free(b);
            b = tmp;
        }
    }

    bn_free(b);
    return result;
}

// ============================================================================
// In-place Operations
// ============================================================================

// In-place add absolute values: a = |a| + |b|
static void bn_add_abs_ip(bignum *a, const bignum *b)
{
    size_t max_len = (a->len > b->len ? a->len : b->len) + 1;
    bn_ensure_cap(a, max_len);

    // Zero-extend a if needed
    for (size_t i = a->len; i < max_len; i++) {
        a->limbs[i] = 0;
    }

    dlimb_t carry = 0;
    for (size_t i = 0; i < max_len; i++) {
        dlimb_t sum = carry + a->limbs[i];
        if (i < b->len)
            sum += b->limbs[i];
        a->limbs[i] = (limb_t)(sum & LIMB_MAX);
        carry = sum >> LIMB_BITS;
    }
    a->len = max_len;
    bn_normalize(a);
}

// In-place subtract absolute values: a = |a| - |b|, assumes |a| >= |b|
static void bn_sub_abs_ip(bignum *a, const bignum *b)
{
    int64_t borrow = 0;
    for (size_t i = 0; i < a->len; i++) {
        int64_t diff = (int64_t)a->limbs[i] - borrow;
        if (i < b->len)
            diff -= b->limbs[i];
        if (diff < 0) {
            diff += (int64_t)1 << LIMB_BITS;
            borrow = 1;
        } else {
            borrow = 0;
        }
        a->limbs[i] = (limb_t)diff;
    }
    bn_normalize(a);
}

void bn_add_ip(bignum *a, const bignum *b)
{
    if (a->sign == b->sign) {
        // Same sign: add absolute values
        bn_add_abs_ip(a, b);
    } else {
        // Different signs: subtract absolute values
        int cmp = bn_cmp_abs(a, b);
        if (cmp == 0) {
            // Result is zero
            a->len = 0;
            a->sign = 0;
        } else if (cmp > 0) {
            // |a| > |b|, keep a's sign
            bn_sub_abs_ip(a, b);
        } else {
            // |b| > |a|, switch sign
            // Need to copy b and subtract a from it
            bignum *tmp = bn_sub_abs(b, a);
            // Copy result back to a
            bn_ensure_cap(a, tmp->len);
            memcpy(a->limbs, tmp->limbs, tmp->len * sizeof(limb_t));
            a->len = tmp->len;
            a->sign = b->sign;
            bn_free(tmp);
        }
    }
}

void bn_sub_ip(bignum *a, const bignum *b)
{
    // a - b = a + (-b)
    bignum neg_b = *b;
    neg_b.sign = !b->sign;
    bn_add_ip(a, &neg_b);
}

// In-place multiplication by a single limb
void bn_mul_limb_ip(bignum *a, limb_t b)
{
    if (b == 0) {
        a->len = 0;
        a->sign = 0;
        return;
    }
    if (b == 1 || a->len == 0) {
        return;
    }

    bn_ensure_cap(a, a->len + 1);

    dlimb_t carry = 0;
    for (size_t i = 0; i < a->len; i++) {
        dlimb_t prod = (dlimb_t)a->limbs[i] * b + carry;
        a->limbs[i] = (limb_t)(prod & LIMB_MAX);
        carry = prod >> LIMB_BITS;
    }
    if (carry) {
        a->limbs[a->len++] = (limb_t)carry;
    }
}

// In-place add a single limb
void bn_add_limb_ip(bignum *a, limb_t b)
{
    if (b == 0)
        return;

    if (a->len == 0) {
        bn_ensure_cap(a, 1);
        a->limbs[0] = b;
        a->len = 1;
        return;
    }

    if (a->sign) {
        // Negative number: subtracting
        bignum tmp;
        tmp.limbs = &b;
        tmp.len = 1;
        tmp.cap = 1;
        tmp.sign = 0;
        bn_add_ip(a, &tmp);
        return;
    }

    bn_ensure_cap(a, a->len + 1);

    dlimb_t carry = b;
    for (size_t i = 0; i < a->len && carry; i++) {
        dlimb_t sum = (dlimb_t)a->limbs[i] + carry;
        a->limbs[i] = (limb_t)(sum & LIMB_MAX);
        carry = sum >> LIMB_BITS;
    }
    if (carry) {
        a->limbs[a->len++] = (limb_t)carry;
    }
}

// In-place negate
void bn_neg_ip(bignum *a)
{
    if (a->len > 0) {
        a->sign = !a->sign;
    }
}
