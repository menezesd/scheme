/**
 * @file bignum.c
 * @brief Arbitrary precision integer arithmetic
 *
 * Implements bignums for the Scheme numeric tower, supporting integers
 * larger than int64_t range.
 *
 * ## Representation
 * Bignums are stored as:
 * - sign: 0 = positive/zero, 1 = negative
 * - limbs[]: Array of 32-bit unsigned words (little-endian, least significant
 * first)
 * - len: Number of significant limbs
 * - cap: Allocated capacity
 *
 * ## Operations
 * All arithmetic operations produce new bignums; inputs are not modified.
 * The caller is responsible for freeing results with bn_free().
 *
 * ## Conversion
 * - bn_from_int: Convert int64_t to bignum
 * - bn_from_str: Parse decimal string to bignum
 * - bn_to_str: Convert bignum to decimal string
 * - bn_to_int: Convert to int64_t if it fits
 */

#include "bignum.h"
#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ============================================================================
// Internal Helpers
// ============================================================================

static bignum *bn_alloc(size_t cap)
{
    size_t limbs_size;
    if (cap > SIZE_MAX / sizeof(limb_t))
        return NULL;
    bignum *b = malloc(sizeof(bignum));
    if (!b)
        return NULL;
    limbs_size = cap * sizeof(limb_t);
    b->limbs = calloc(limbs_size, 1);
    if (!b->limbs) {
        free(b);
        return NULL;
    }
    b->len = 0;
    b->cap = cap;
    b->sign = 0;
    return b;
}

static char *bn_alloc_string(size_t len)
{
    if (len == SIZE_MAX)
        return NULL;
    return malloc(len + 1);
}

static limb_t *bn_realloc_limbs(limb_t *limbs, size_t cap)
{
    if (cap > SIZE_MAX / sizeof(limb_t))
        return NULL;
    return realloc(limbs, cap * sizeof(limb_t));
}

static void bn_ensure_cap(bignum *b, size_t cap)
{
    if (b->cap >= cap)
        return;
    if (b->cap > SIZE_MAX / 2 || cap > SIZE_MAX / sizeof(limb_t)) {
        fprintf(stderr, "bignum: capacity overflow\n");
        abort();
    }
    size_t new_cap = b->cap * 2;
    if (new_cap < cap)
        new_cap = cap;
    if (new_cap > SIZE_MAX / sizeof(limb_t)) {
        fprintf(stderr, "bignum: capacity overflow\n");
        abort();
    }
    limb_t *new_limbs = bn_realloc_limbs(b->limbs, new_cap);
    if (!new_limbs) {
        fprintf(stderr, "bignum: out of memory\n");
        abort();
    }
    b->limbs = new_limbs;
    memset(b->limbs + b->cap, 0, (new_cap - b->cap) * sizeof(limb_t));
    b->cap = new_cap;
}

static void bn_abort_oom(void)
{
    fprintf(stderr, "bignum: out of memory\n");
    abort();
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

bignum *bn_new(void)
{
    return bn_alloc(4);
}

bignum *bn_from_int(int64_t val)
{
    bignum *b = bn_alloc(2);
    if (!b)
        return NULL;

    uint64_t uval;
    if (val < 0) {
        b->sign = 1;
        uval = -(uint64_t)val;
    } else {
        uval = (uint64_t)val;
    }

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
    bool saw_digit = false;
    while (*str) {
        int digit;
        if (isdigit(*str)) {
            digit = *str - '0';
        } else if (isalpha(*str)) {
            digit = tolower(*str) - 'a' + 10;
        } else {
            bn_free(result);
            return NULL;
        }

        if (digit >= base) {
            bn_free(result);
            return NULL;
        }

        // result = result * base + digit (all in-place, no allocations)
        bn_mul_limb_ip(result, (limb_t)base);
        bn_add_limb_ip(result, (limb_t)digit);
        saw_digit = true;

        str++;
    }
    if (!saw_digit) {
        bn_free(result);
        return NULL;
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

// Simple digit-by-digit conversion for small numbers
static char *bn_to_string_simple(const bignum *a, int base)
{
    // Check for potential overflow in size calculation
    if (a->len > SIZE_MAX / LIMB_BITS) {
        return NULL;  // Number too large to convert to string
    }
    // Worst case is base 2: one digit per bit, plus sign and NUL.
    size_t max_digits = a->len * LIMB_BITS;
    if (max_digits > SIZE_MAX - 2)
        return NULL;
    char *buf = bn_alloc_string(max_digits + 1);
    if (!buf)
        return NULL;

    bignum *tmp = bn_copy(a);
    if (!tmp) {
        free(buf);
        return NULL;
    }
    tmp->sign = 0; // Work with absolute value

    size_t pos = 0;
    bignum *base_bn = bn_from_int(base);
    if (!base_bn) {
        bn_free(tmp);
        free(buf);
        return NULL;
    }

    while (!bn_is_zero(tmp)) {
        bignum *rem = NULL;
        bignum *quot = bn_div(tmp, base_bn, &rem);
        if (!quot || !rem) {
            bn_free(quot);
            bn_free(rem);
            bn_free(tmp);
            bn_free(base_bn);
            free(buf);
            return NULL;
        }

        int digit = rem->len > 0 ? (int)rem->limbs[0] : 0;
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

// Divide-and-conquer conversion for base 10
// Split number at 10^k, convert each half recursively, concatenate
static char *bn_to_string_dc(const bignum *a, size_t num_digits)
{
    // Base case: use simple method for small numbers
    if (a->len <= 16 || num_digits <= 150) {
        return bn_to_string_simple(a, 10);
    }

    // Split at roughly half the digits
    size_t split = num_digits / 2;

    // Compute 10^split using binary exponentiation
    bignum *ten = bn_from_int(10);
    if (!ten)
        return NULL;
    bignum *divisor = bn_pow(ten, split);
    bn_free(ten);
    if (!divisor)
        return NULL;

    // Divide: a = hi * 10^split + lo
    bignum *lo = NULL;
    bignum *hi = bn_div(a, divisor, &lo);
    bn_free(divisor);
    if (!hi || !lo) {
        bn_free(hi);
        bn_free(lo);
        return NULL;
    }

    // Estimate digits in each half
    size_t hi_digits = num_digits - split;
    size_t lo_digits = split;

    // Recursively convert each half
    char *hi_str;
    if (bn_is_zero(hi)) {
        hi_str = bn_alloc_string(0);
        if (!hi_str) {
            bn_free(hi);
            bn_free(lo);
            return NULL;
        }
        hi_str[0] = '\0';
    } else {
        hi_str = bn_to_string_dc(hi, hi_digits);
    }
    bn_free(hi);
    if (!hi_str) {
        bn_free(lo);
        return NULL;
    }

    char *lo_str = bn_to_string_dc(lo, lo_digits);
    bn_free(lo);
    if (!lo_str) {
        free(hi_str);
        return NULL;
    }

    // Concatenate with zero-padding for low part
    size_t hi_len = strlen(hi_str);
    size_t lo_len = strlen(lo_str);
    size_t pad = (lo_len < split) ? split - lo_len : 0;

    if (hi_len > SIZE_MAX - pad || hi_len + pad > SIZE_MAX - lo_len ||
        hi_len + pad + lo_len == SIZE_MAX) {
        free(hi_str);
        free(lo_str);
        return NULL;
    }
    char *result = bn_alloc_string(hi_len + pad + lo_len);
    if (!result) {
        free(hi_str);
        free(lo_str);
        return NULL;
    }
    memcpy(result, hi_str, hi_len);
    memset(result + hi_len, '0', pad);
    memcpy(result + hi_len + pad, lo_str, lo_len + 1);

    free(hi_str);
    free(lo_str);

    return result;
}

char *bn_to_string(const bignum *a, int base)
{
    if (!a || base < 2 || base > 36)
        return NULL;

    if (a->len == 0) {
        char *s = bn_alloc_string(1);
        if (!s)
            return NULL;
        s[0] = '0';
        s[1] = '\0';
        return s;
    }

    // For base 10 and large numbers, use divide-and-conquer
    if (base == 10 && a->len > 16) {
        // Estimate decimal digits: len * LIMB_BITS * log10(2) ≈ len * 9.63
        size_t est_digits = (size_t)(a->len * LIMB_BITS * 0.30103) + 1;

        bignum *abs_a = bn_abs(a);
        if (!abs_a)
            return NULL;
        char *digits = bn_to_string_dc(abs_a, est_digits);
        bn_free(abs_a);
        if (!digits)
            return NULL;

        if (a->sign) {
            size_t len = strlen(digits);
            if (len > SIZE_MAX - 2) {
                free(digits);
                return NULL;
            }
            char *result = bn_alloc_string(len + 1);
            if (!result) {
                free(digits);
                return NULL;
            }
            result[0] = '-';
            memcpy(result + 1, digits, len + 1);
            free(digits);
            return result;
        }
        return digits;
    }

    // For other bases or small numbers, use simple method
    return bn_to_string_simple(a, base);
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

bool bn_is_zero(const bignum *a)
{
    return a->len == 0;
}

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
    size_t max_input_len = a->len > b->len ? a->len : b->len;
    if (max_input_len == SIZE_MAX)
        return NULL;
    size_t max_len = max_input_len + 1;
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
        if (!result)
            return NULL;
        result->sign = a->sign;
        return result;
    } else {
        // Different signs: subtract absolute values
        int cmp = bn_cmp_abs(a, b);
        if (cmp == 0) {
            return bn_new(); // Zero
        } else if (cmp > 0) {
            bignum *result = bn_sub_abs(a, b);
            if (!result)
                return NULL;
            result->sign = a->sign;
            return result;
        } else {
            bignum *result = bn_sub_abs(b, a);
            if (!result)
                return NULL;
            result->sign = b->sign;
            return result;
        }
    }
}

bignum *bn_sub(const bignum *a, const bignum *b)
{
    // a - b = a + (-b)
    bignum *neg_b = bn_copy(b);
    if (!neg_b)
        return NULL;
    neg_b->sign = !neg_b->sign;
    bignum *result = bn_add(a, neg_b);
    bn_free(neg_b);
    if (!result)
        return NULL;
    bn_normalize(result);
    return result;
}

bignum *bn_neg(const bignum *a)
{
    bignum *result = bn_copy(a);
    if (!result)
        return NULL;
    if (result->len > 0) {
        result->sign = !result->sign;
    }
    return result;
}

bignum *bn_abs(const bignum *a)
{
    bignum *result = bn_copy(a);
    if (!result)
        return NULL;
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

    if (a->len > SIZE_MAX - b->len)
        return NULL;
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
    if (!*low || !*high)
        bn_abort_oom();

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

    if (k > SIZE_MAX - a->len)
        return NULL;
    bignum *result = bn_alloc(a->len + k);
    if (!result)
        return NULL;

    memset(result->limbs, 0, k * sizeof(limb_t));
    memcpy(result->limbs + k, a->limbs, a->len * sizeof(limb_t));
    result->len = a->len + k;
    return result;
}

// Forward declaration for mutual recursion
static bignum *bn_mul_toom3(const bignum *a, const bignum *b);

// Karatsuba multiplication O(n^1.585)
static bignum *bn_mul_karatsuba(const bignum *a, const bignum *b)
{
    // Base case: use schoolbook for small numbers
    if (a->len < KARATSUBA_THRESHOLD || b->len < KARATSUBA_THRESHOLD) {
        return bn_mul_schoolbook(a, b);
    }

    // Use Toom-3 for very large numbers
    if (a->len >= TOOM3_THRESHOLD && b->len >= TOOM3_THRESHOLD) {
        return bn_mul_toom3(a, b);
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

// Split bignum into 3 parts for Toom-3
// a = a0 + a1 * B^k + a2 * B^(2k)
static void bn_split3(const bignum *a, size_t k, bignum **p0, bignum **p1,
                      bignum **p2)
{
    // p0 = a mod B^k (lowest k limbs)
    *p0 = bn_alloc(k > a->len ? a->len : k);
    if (!*p0)
        bn_abort_oom();
    size_t p0_len = k < a->len ? k : a->len;
    memcpy((*p0)->limbs, a->limbs, p0_len * sizeof(limb_t));
    (*p0)->len = p0_len;
    bn_normalize(*p0);

    // p1 = (a / B^k) mod B^k (middle k limbs)
    *p1 = bn_alloc(k);
    if (!*p1)
        bn_abort_oom();
    if (a->len > k) {
        size_t p1_len = (a->len - k) < k ? (a->len - k) : k;
        memcpy((*p1)->limbs, a->limbs + k, p1_len * sizeof(limb_t));
        (*p1)->len = p1_len;
    }
    bn_normalize(*p1);

    // p2 = a / B^(2k) (highest limbs)
    *p2 = bn_alloc(a->len > 2 * k ? a->len - 2 * k : 1);
    if (!*p2)
        bn_abort_oom();
    if (a->len > 2 * k) {
        memcpy((*p2)->limbs, a->limbs + 2 * k, (a->len - 2 * k) * sizeof(limb_t));
        (*p2)->len = a->len - 2 * k;
    }
    bn_normalize(*p2);
}

// Toom-Cook-3 multiplication O(n^1.465)
// Uses 5 evaluation points: 0, 1, -1, 2, infinity
static bignum *bn_mul_toom3(const bignum *a, const bignum *b)
{
    // Fall back to Karatsuba for smaller numbers
    if (a->len < TOOM3_THRESHOLD || b->len < TOOM3_THRESHOLD) {
        // Avoid infinite recursion - use schoolbook for very small
        if (a->len < KARATSUBA_THRESHOLD || b->len < KARATSUBA_THRESHOLD) {
            return bn_mul_schoolbook(a, b);
        }
        // Use Karatsuba directly, bypassing Toom-3 check
        size_t k = (a->len > b->len ? a->len : b->len) / 2;
        bignum *a0, *a1, *b0, *b1;
        bn_split(a, k, &a0, &a1);
        bn_split(b, k, &b0, &b1);
        bignum *z0 = bn_mul_karatsuba(a0, b0);
        bignum *z2 = bn_mul_karatsuba(a1, b1);
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

    // Split at 1/3 of the larger length
    size_t n = (a->len > b->len ? a->len : b->len);
    size_t k = (n + 2) / 3; // Ceiling division

    // Split: a = a0 + a1*x + a2*x^2, b = b0 + b1*x + b2*x^2
    bignum *a0, *a1, *a2, *b0, *b1, *b2;
    bn_split3(a, k, &a0, &a1, &a2);
    bn_split3(b, k, &b0, &b1, &b2);

    // Evaluate at x=0: p0 = a0, q0 = b0
    // r0 = a0 * b0
    bignum *r0 = bn_mul_toom3(a0, b0);

    // Evaluate at x=inf: p_inf = a2, q_inf = b2
    // r_inf = a2 * b2
    bignum *r_inf = bn_mul_toom3(a2, b2);

    // Evaluate at x=1: p1 = a0+a1+a2, q1 = b0+b1+b2
    bignum *p1_tmp = bn_add(a0, a1);
    bignum *p1 = bn_add(p1_tmp, a2);
    bn_free(p1_tmp);
    bignum *q1_tmp = bn_add(b0, b1);
    bignum *q1 = bn_add(q1_tmp, b2);
    bn_free(q1_tmp);
    // r1 = p1 * q1
    bignum *r1 = bn_mul_toom3(p1, q1);
    bn_free(p1);
    bn_free(q1);

    // Evaluate at x=-1: p_m1 = a0-a1+a2, q_m1 = b0-b1+b2
    bignum *p_m1_tmp = bn_sub(a0, a1);
    bignum *p_m1 = bn_add(p_m1_tmp, a2);
    bn_free(p_m1_tmp);
    bignum *q_m1_tmp = bn_sub(b0, b1);
    bignum *q_m1 = bn_add(q_m1_tmp, b2);
    bn_free(q_m1_tmp);
    // r_m1 = p_m1 * q_m1
    bignum *r_m1 = bn_mul_toom3(p_m1, q_m1);
    bn_free(p_m1);
    bn_free(q_m1);

    // Evaluate at x=2: p2 = a0+2*a1+4*a2, q2 = b0+2*b1+4*b2
    bignum *two = bn_from_int(2);
    bignum *four = bn_from_int(4);
    bignum *a1x2 = bn_mul(a1, two);
    bignum *a2x4 = bn_mul(a2, four);
    bignum *p2_tmp = bn_add(a0, a1x2);
    bignum *p2 = bn_add(p2_tmp, a2x4);
    bn_free(p2_tmp);
    bn_free(a1x2);
    bn_free(a2x4);

    bignum *b1x2 = bn_mul(b1, two);
    bignum *b2x4 = bn_mul(b2, four);
    bignum *q2_tmp = bn_add(b0, b1x2);
    bignum *q2 = bn_add(q2_tmp, b2x4);
    bn_free(q2_tmp);
    bn_free(b1x2);
    bn_free(b2x4);
    bn_free(two);
    bn_free(four);
    // r2 = p2 * q2
    bignum *r2 = bn_mul_toom3(p2, q2);
    bn_free(p2);
    bn_free(q2);

    // Free input splits
    bn_free(a0);
    bn_free(a1);
    bn_free(a2);
    bn_free(b0);
    bn_free(b1);
    bn_free(b2);

    // Interpolation to get c0, c1, c2, c3, c4 where
    // result = c0 + c1*B^k + c2*B^(2k) + c3*B^(3k) + c4*B^(4k)
    //
    // From the 5 evaluations:
    // r0    = c0
    // r1    = c0 + c1 + c2 + c3 + c4
    // r_m1  = c0 - c1 + c2 - c3 + c4
    // r2    = c0 + 2*c1 + 4*c2 + 8*c3 + 16*c4
    // r_inf = c4
    //
    // Solving:
    // c0 = r0
    // c4 = r_inf
    // c2 = (r1 + r_m1)/2 - r0 - r_inf
    // c1 = (r1 - r_m1)/2 - c3
    // c3 = (r2 - r0 - 4*c2 - 16*r_inf) / 2 - c1
    //    which simplifies to a system we solve step by step

    // c0 = r0
    bignum *c0 = bn_copy(r0);

    // c4 = r_inf
    bignum *c4 = bn_copy(r_inf);

    // t1 = (r1 + r_m1) / 2
    bignum *sum_r1_rm1 = bn_add(r1, r_m1);
    bignum *t1 = bn_rshift(sum_r1_rm1, 1); // divide by 2
    bn_free(sum_r1_rm1);

    // t2 = (r1 - r_m1) / 2
    bignum *diff_r1_rm1 = bn_sub(r1, r_m1);
    bignum *t2 = bn_rshift(diff_r1_rm1, 1); // divide by 2
    bn_free(diff_r1_rm1);

    // c2 = t1 - c0 - c4
    bignum *c2_tmp = bn_sub(t1, c0);
    bignum *c2 = bn_sub(c2_tmp, c4);
    bn_free(c2_tmp);
    bn_free(t1);

    // For c1 and c3, we need more work:
    // r2 = c0 + 2*c1 + 4*c2 + 8*c3 + 16*c4
    // Let's compute: r2 - c0 - 4*c2 - 16*c4 = 2*c1 + 8*c3 = 2*(c1 + 4*c3)

    bignum *sixteen = bn_from_int(16);
    bignum *c4x16 = bn_mul(c4, sixteen);
    bn_free(sixteen);

    four = bn_from_int(4);
    bignum *c2x4 = bn_mul(c2, four);
    bn_free(four);

    bignum *tmp1 = bn_sub(r2, c0);
    bignum *tmp2 = bn_sub(tmp1, c2x4);
    bn_free(tmp1);
    bignum *tmp3 = bn_sub(tmp2, c4x16);
    bn_free(tmp2);
    bn_free(c2x4);
    bn_free(c4x16);

    // tmp3 = 2*(c1 + 4*c3)
    // t3 = c1 + 4*c3
    bignum *t3 = bn_rshift(tmp3, 1);
    bn_free(tmp3);

    // We also have t2 = c1 + c3 (from (r1 - r_m1)/2)
    // So: t3 - t2 = 3*c3
    // c3 = (t3 - t2) / 3

    bignum *diff_t3_t2 = bn_sub(t3, t2);
    bignum *three = bn_from_int(3);
    bignum *c3 = bn_div(diff_t3_t2, three, NULL);
    bn_free(diff_t3_t2);
    bn_free(three);

    // c1 = t2 - c3
    bignum *c1 = bn_sub(t2, c3);
    bn_free(t2);
    bn_free(t3);

    // Free evaluation results
    bn_free(r0);
    bn_free(r1);
    bn_free(r_m1);
    bn_free(r2);
    bn_free(r_inf);

    // result = c0 + c1*B^k + c2*B^(2k) + c3*B^(3k) + c4*B^(4k)
    bignum *c1_shift = bn_lshift_limbs(c1, k);
    bignum *c2_shift = bn_lshift_limbs(c2, 2 * k);
    bignum *c3_shift = bn_lshift_limbs(c3, 3 * k);
    bignum *c4_shift = bn_lshift_limbs(c4, 4 * k);

    bignum *sum1 = bn_add(c0, c1_shift);
    bignum *sum2 = bn_add(sum1, c2_shift);
    bignum *sum3 = bn_add(sum2, c3_shift);
    bignum *result = bn_add(sum3, c4_shift);

    bn_free(c0);
    bn_free(c1);
    bn_free(c2);
    bn_free(c3);
    bn_free(c4);
    bn_free(c1_shift);
    bn_free(c2_shift);
    bn_free(c3_shift);
    bn_free(c4_shift);
    bn_free(sum1);
    bn_free(sum2);
    bn_free(sum3);

    bn_normalize(result);
    return result;
}

bignum *bn_mul(const bignum *a, const bignum *b)
{
    bignum *result = bn_mul_karatsuba(a, b);
    if (!result)
        return NULL;
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

// Count leading zeros in a limb
static int clz_limb(limb_t x)
{
    if (x == 0)
        return LIMB_BITS;
#if LIMB_BITS == 32
    return __builtin_clz(x);
#else
    return __builtin_clzll(x);
#endif
}

// Long division using Knuth's Algorithm D
// This is O(n*m) where n is dividend length and m is divisor length
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

    // Knuth's Algorithm D
    size_t n = divisor->len;
    size_t m = dividend->len - n;

    // D1: Normalize - shift so divisor's top bit is set
    int shift = clz_limb(divisor->limbs[n - 1]);

    // Shift both dividend and divisor left by 'shift' bits
    bignum *u = bn_lshift(dividend, shift);
    bignum *v = bn_lshift(divisor, shift);

    // Ensure u has m+n+1 limbs (may need extra limb from shift)
    bn_ensure_cap(u, m + n + 1);
    while (u->len < m + n + 1) {
        u->limbs[u->len++] = 0;
    }

    // Allocate quotient
    bignum *q = bn_alloc(m + 1);
    q->len = m + 1;

    limb_t v_n1 = v->limbs[n - 1]; // Most significant limb of divisor
    limb_t v_n2 = n >= 2 ? v->limbs[n - 2] : 0; // Second most significant

    // D2-D7: Main loop - compute each quotient digit
    for (size_t j = m + 1; j > 0; j--) {
        size_t jj = j - 1; // 0-based index

        // D3: Calculate quotient estimate qhat
        // qhat = floor((u[j+n]*B + u[j+n-1]) / v[n-1])
        dlimb_t u_hi =
            ((dlimb_t)u->limbs[jj + n] << LIMB_BITS) | u->limbs[jj + n - 1];
        dlimb_t qhat = u_hi / v_n1;
        dlimb_t rhat = u_hi % v_n1;

        // Refine qhat estimate (Knuth's correction)
        // While qhat >= B or qhat * v[n-2] > B * rhat + u[j+n-2]
        while (qhat > LIMB_MAX ||
               (qhat * v_n2 > ((rhat << LIMB_BITS) |
                               (jj + n >= 2 ? u->limbs[jj + n - 2] : 0)))) {
            qhat--;
            rhat += v_n1;
            if (rhat > LIMB_MAX)
                break;
        }

        // D4: Multiply and subtract: u[j..j+n] -= qhat * v[0..n-1]
        int64_t borrow = 0;
        for (size_t i = 0; i < n; i++) {
            dlimb_t prod = qhat * v->limbs[i];
            int64_t diff = (int64_t)u->limbs[jj + i] - (limb_t)prod - borrow;
            u->limbs[jj + i] = (limb_t)diff;
            borrow = (prod >> LIMB_BITS) - (diff >> LIMB_BITS);
        }
        int64_t diff = (int64_t)u->limbs[jj + n] - borrow;
        u->limbs[jj + n] = (limb_t)diff;

        // D5: Store quotient digit
        q->limbs[jj] = (limb_t)qhat;

        // D6: Add back if we subtracted too much
        if (diff < 0) {
            // qhat was 1 too large
            q->limbs[jj]--;
            dlimb_t carry = 0;
            for (size_t i = 0; i < n; i++) {
                dlimb_t sum = (dlimb_t)u->limbs[jj + i] + v->limbs[i] + carry;
                u->limbs[jj + i] = (limb_t)sum;
                carry = sum >> LIMB_BITS;
            }
            u->limbs[jj + n] += (limb_t)carry;
        }
    }

    // D8: Unnormalize remainder
    bn_normalize(q);
    q->sign = result_sign && !bn_is_zero(q);

    if (remainder) {
        // Shift remainder right to undo normalization
        bignum *rem = bn_rshift(u, shift);
        rem->sign = a->sign && !bn_is_zero(rem);
        *remainder = rem;
    }

    bn_free(u);
    bn_free(v);
    bn_free(dividend);
    bn_free(divisor);

    return q;
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

    if (a->len > SIZE_MAX - 1 || limb_shift > SIZE_MAX - a->len - 1)
        return NULL;
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
    if (!result || !b) {
        bn_free(result);
        bn_free(b);
        return NULL;
    }

    while (exp > 0) {
        if (exp & 1) {
            bignum *tmp = bn_mul(result, b);
            bn_free(result);
            result = tmp;
            if (!result) {
                bn_free(b);
                return NULL;
            }
        }
        exp >>= 1;
        if (exp > 0) {
            bignum *tmp = bn_mul(b, b);
            bn_free(b);
            b = tmp;
            if (!b) {
                bn_free(result);
                return NULL;
            }
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
            if (!tmp) {
                fprintf(stderr, "bignum: out of memory\n");
                abort();
            }
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
