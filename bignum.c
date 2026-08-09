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

static bool bn_ensure_cap(bignum *b, size_t cap)
{
    if (b->cap >= cap)
        return true;
    if (b->cap > SIZE_MAX / 2 || cap > SIZE_MAX / sizeof(limb_t))
        return false;
    size_t new_cap = b->cap * 2;
    if (new_cap < cap)
        new_cap = cap;
    if (new_cap > SIZE_MAX / sizeof(limb_t))
        return false;
    limb_t *new_limbs = bn_realloc_limbs(b->limbs, new_cap);
    if (!new_limbs)
        return false;
    b->limbs = new_limbs;
    memset(b->limbs + b->cap, 0, (new_cap - b->cap) * sizeof(limb_t));
    b->cap = new_cap;
    return true;
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
        if (isdigit((unsigned char)*str)) {
            digit = *str - '0';
        } else if (isalpha((unsigned char)*str)) {
            digit = tolower((unsigned char)*str) - 'a' + 10;
        } else {
            bn_free(result);
            return NULL;
        }

        if (digit >= base) {
            bn_free(result);
            return NULL;
        }

        // result = result * base + digit
        if (!bn_mul_limb_ip_checked(result, (limb_t)base) ||
            !bn_add_limb_ip_checked(result, (limb_t)digit)) {
            bn_free(result);
            return NULL;
        }
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
        // Estimate decimal digits from the total number of significant bits.
        if (a->len > (SIZE_MAX - 1) / LIMB_BITS)
            return NULL;
        size_t bit_count = a->len * LIMB_BITS;
        double estimated_digits = (double)bit_count * 0.30103;
        if (estimated_digits > (double)(SIZE_MAX - 1))
            return NULL;
        size_t est_digits = (size_t)estimated_digits + 1;

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
static bool bn_split(const bignum *a, size_t k, bignum **low, bignum **high)
{
    *low = bn_alloc(k > a->len ? a->len : k);
    *high = bn_alloc(a->len > k ? a->len - k : 1);
    if (!*low || !*high) {
        bn_free(*low);
        bn_free(*high);
        *low = NULL;
        *high = NULL;
        return false;
    }

    size_t low_len = k < a->len ? k : a->len;
    memcpy((*low)->limbs, a->limbs, low_len * sizeof(limb_t));
    (*low)->len = low_len;
    bn_normalize(*low);

    if (a->len > k) {
        memcpy((*high)->limbs, a->limbs + k, (a->len - k) * sizeof(limb_t));
        (*high)->len = a->len - k;
        bn_normalize(*high);
    }
    return true;
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

// Karatsuba multiplication O(n^1.585)
static bignum *bn_mul_karatsuba(const bignum *a, const bignum *b)
{
    // Base case: use schoolbook for small numbers
    if (a->len < KARATSUBA_THRESHOLD || b->len < KARATSUBA_THRESHOLD) {
        return bn_mul_schoolbook(a, b);
    }

    /*
     * The Toom-3 interpolation path is not used here: it has historically
     * produced incorrect products for some non-uniform large operands.  The
     * Karatsuba recurrence remains sub-quadratic and, unlike that path,
     * preserves exact arithmetic for every operand shape.
     */

    // Split at half of the larger length
    size_t k = (a->len > b->len ? a->len : b->len) / 2;

    bignum *a0 = NULL, *a1 = NULL, *b0 = NULL, *b1 = NULL;
    if (!bn_split(a, k, &a0, &a1) || !bn_split(b, k, &b0, &b1)) {
        bn_free(a0);
        bn_free(a1);
        bn_free(b0);
        bn_free(b1);
        return NULL;
    }

    // z0 = a0 * b0
    bignum *z0 = bn_mul_karatsuba(a0, b0);

    // z2 = a1 * b1
    bignum *z2 = bn_mul_karatsuba(a1, b1);

    // z1 = (a0 + a1) * (b0 + b1) - z0 - z2
    bignum *a0_plus_a1 = bn_add(a0, a1);
    bignum *b0_plus_b1 = bn_add(b0, b1);
    bignum *z1_temp = (a0_plus_a1 && b0_plus_b1)
                          ? bn_mul_karatsuba(a0_plus_a1, b0_plus_b1)
                          : NULL;
    bignum *z1_temp2 = (z1_temp && z0) ? bn_sub(z1_temp, z0) : NULL;
    bignum *z1 = (z1_temp2 && z2) ? bn_sub(z1_temp2, z2) : NULL;

    bn_free(a0);
    bn_free(a1);
    bn_free(b0);
    bn_free(b1);
    bn_free(a0_plus_a1);
    bn_free(b0_plus_b1);
    bn_free(z1_temp);
    bn_free(z1_temp2);
    if (!z0 || !z2 || !z1) {
        bn_free(z0);
        bn_free(z1);
        bn_free(z2);
        return NULL;
    }

    // result = z0 + z1 * B^k + z2 * B^(2k)
    bignum *z1_shifted = bn_lshift_limbs(z1, k);
    bignum *z2_shifted = bn_lshift_limbs(z2, 2 * k);

    bignum *temp = (z1_shifted && z2_shifted) ? bn_add(z0, z1_shifted) : NULL;
    bignum *result = temp ? bn_add(temp, z2_shifted) : NULL;

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
    if (remainder)
        *remainder = NULL;

    if (bn_is_zero(b)) {
        // Division by zero
        bignum *quot = bn_new();
        if (!quot)
            return NULL;
        if (remainder) {
            *remainder = bn_new();
            if (!*remainder) {
                bn_free(quot);
                return NULL;
            }
        }
        return quot;
    }

    int result_sign = (a->sign != b->sign);

    // Work with absolute values
    bignum *dividend = bn_abs(a);
    bignum *divisor = bn_abs(b);
    if (!dividend || !divisor) {
        bn_free(dividend);
        bn_free(divisor);
        return NULL;
    }

    int cmp = bn_cmp_abs(dividend, divisor);
    if (cmp < 0) {
        // |a| < |b|, quotient is 0
        if (remainder) {
            *remainder = bn_copy(a);
            if (!*remainder) {
                bn_free(dividend);
                bn_free(divisor);
                return NULL;
            }
        }
        bignum *zero = bn_new();
        bn_free(dividend);
        bn_free(divisor);
        return zero;
    }

    if (divisor->len == 1) {
        // Single-limb divisor - use fast path
        bignum *quot = bn_copy(dividend);
        if (!quot) {
            bn_free(dividend);
            bn_free(divisor);
            return NULL;
        }
        limb_t rem = bn_div_limb(quot, divisor->limbs[0]);
        quot->sign = result_sign && !bn_is_zero(quot);
        if (remainder) {
            *remainder = bn_from_int(rem);
            if (!*remainder) {
                bn_free(quot);
                bn_free(dividend);
                bn_free(divisor);
                return NULL;
            }
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
    if (!u || !v || dividend->len == SIZE_MAX) {
        bn_free(u);
        bn_free(v);
        bn_free(dividend);
        bn_free(divisor);
        return NULL;
    }

    // Ensure u has m+n+1 limbs (may need extra limb from shift)
    if (!bn_ensure_cap(u, m + n + 1)) {
        bn_free(u);
        bn_free(v);
        bn_free(dividend);
        bn_free(divisor);
        return NULL;
    }
    while (u->len < m + n + 1) {
        u->limbs[u->len++] = 0;
    }

    // Allocate quotient
    bignum *q = bn_alloc(m + 1);
    if (!q) {
        bn_free(u);
        bn_free(v);
        bn_free(dividend);
        bn_free(divisor);
        return NULL;
    }
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
        if (!rem) {
            bn_free(q);
            bn_free(u);
            bn_free(v);
            bn_free(dividend);
            bn_free(divisor);
            return NULL;
        }
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

bignum *bn_arshift(const bignum *a, size_t bits)
{
    if (!a)
        return NULL;
    if (!a->sign || bits == 0)
        return bn_rshift(a, bits);

    // Arithmetic right shift of a negative value is floor(a / 2^bits),
    // i.e. the negation of the rounded-up magnitude quotient.
    bignum *magnitude = bn_abs(a);
    if (!magnitude)
        return NULL;
    bool has_remainder = false;
    size_t limb_shift = bits / LIMB_BITS;
    size_t bit_shift = bits % LIMB_BITS;
    if (limb_shift >= magnitude->len) {
        has_remainder = !bn_is_zero(magnitude);
    } else {
        for (size_t i = 0; i < limb_shift; i++) {
            if (magnitude->limbs[i] != 0) {
                has_remainder = true;
                break;
            }
        }
        if (!has_remainder && bit_shift > 0) {
            limb_t mask = ((limb_t)1 << bit_shift) - 1;
            has_remainder = (magnitude->limbs[limb_shift] & mask) != 0;
        }
    }

    bignum *result = bn_rshift(magnitude, bits);
    bn_free(magnitude);
    if (!result)
        return NULL;
    if (has_remainder && !bn_add_limb_ip_checked(result, 1)) {
        bn_free(result);
        return NULL;
    }
    if (!bn_is_zero(result))
        result->sign = 1;
    return result;
}

static bool bn_to_twos_complement(const bignum *value, limb_t *digits,
                                  size_t width)
{
    if (!value || !digits || width == 0 || value->len > width)
        return false;
    memset(digits, 0, width * sizeof(limb_t));
    if (value->len > 0)
        memcpy(digits, value->limbs, value->len * sizeof(limb_t));
    if (!value->sign)
        return true;

    for (size_t i = 0; i < width; i++)
        digits[i] = ~digits[i];
    dlimb_t carry = 1;
    for (size_t i = 0; i < width && carry; i++) {
        dlimb_t sum = (dlimb_t)digits[i] + carry;
        digits[i] = (limb_t)sum;
        carry = sum >> LIMB_BITS;
    }
    return carry == 0;
}

static bignum *bn_from_twos_complement(limb_t *digits, size_t width)
{
    if (!digits || width == 0)
        return NULL;
    bool negative = (digits[width - 1] & ((limb_t)1 << (LIMB_BITS - 1))) != 0;
    if (negative) {
        for (size_t i = 0; i < width; i++)
            digits[i] = ~digits[i];
        dlimb_t carry = 1;
        for (size_t i = 0; i < width && carry; i++) {
            dlimb_t sum = (dlimb_t)digits[i] + carry;
            digits[i] = (limb_t)sum;
            carry = sum >> LIMB_BITS;
        }
    }

    bignum *result = bn_alloc(width);
    if (!result)
        return NULL;
    memcpy(result->limbs, digits, width * sizeof(limb_t));
    result->len = width;
    result->sign = negative ? 1 : 0;
    bn_normalize(result);
    return result;
}

static bignum *bn_bitwise_with_width(const bignum *a, const bignum *b,
                                     int op, size_t width)
{
    if (width == 0 || width > SIZE_MAX / sizeof(limb_t))
        return NULL;
    limb_t *ad = calloc(width, sizeof(limb_t));
    limb_t *bd = calloc(width, sizeof(limb_t));
    if (!ad || !bd) {
        free(ad);
        free(bd);
        return NULL;
    }
    bool ok = bn_to_twos_complement(a, ad, width) &&
              bn_to_twos_complement(b, bd, width);
    if (!ok) {
        free(ad);
        free(bd);
        return NULL;
    }
    for (size_t i = 0; i < width; i++) {
        if (op == 0)
            ad[i] &= bd[i];
        else if (op == 1)
            ad[i] |= bd[i];
        else if (op == 2)
            ad[i] ^= bd[i];
        else {
            free(ad);
            free(bd);
            return NULL;
        }
    }
    free(bd);
    bignum *result = bn_from_twos_complement(ad, width);
    free(ad);
    return result;
}

bignum *bn_bitwise(const bignum *a, const bignum *b, int op)
{
    if (!a || !b || a->len == SIZE_MAX || b->len == SIZE_MAX)
        return NULL;
    size_t width = a->len > b->len ? a->len : b->len;
    if (width == SIZE_MAX)
        return NULL;
    return bn_bitwise_with_width(a, b, op, width + 1);
}

bignum *bn_bitwise_not(const bignum *a)
{
    if (!a || a->len == SIZE_MAX)
        return NULL;
    size_t width = a->len + 1;
    if (width == 0)
        return NULL;
    limb_t *digits = calloc(width, sizeof(limb_t));
    if (!digits)
        return NULL;
    if (!bn_to_twos_complement(a, digits, width)) {
        free(digits);
        return NULL;
    }
    for (size_t i = 0; i < width; i++)
        digits[i] = ~digits[i];
    bignum *result = bn_from_twos_complement(digits, width);
    free(digits);
    return result;
}

// ============================================================================
// Other Operations
// ============================================================================

bignum *bn_gcd(const bignum *a, const bignum *b)
{
    bignum *x = bn_abs(a);
    bignum *y = bn_abs(b);
    if (!x || !y) {
        bn_free(x);
        bn_free(y);
        return NULL;
    }

    while (!bn_is_zero(y)) {
        bignum *rem = bn_mod(x, y);
        if (!rem) {
            bn_free(x);
            bn_free(y);
            return NULL;
        }
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
static bool bn_add_abs_ip_checked(bignum *a, const bignum *b)
{
    size_t max_input_len = a->len > b->len ? a->len : b->len;
    if (max_input_len == SIZE_MAX)
        return false;
    size_t max_len = max_input_len + 1;
    if (!bn_ensure_cap(a, max_len))
        return false;

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
    return true;
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

bool bn_add_ip_checked(bignum *a, const bignum *b)
{
    if (a->sign == b->sign) {
        // Same sign: add absolute values
        return bn_add_abs_ip_checked(a, b);
    } else {
        // Different signs: subtract absolute values
        int cmp = bn_cmp_abs(a, b);
        if (cmp == 0) {
            // Result is zero
            a->len = 0;
            a->sign = 0;
            return true;
        } else if (cmp > 0) {
            // |a| > |b|, keep a's sign
            bn_sub_abs_ip(a, b);
            return true;
        } else {
            // |b| > |a|, switch sign
            // Need to copy b and subtract a from it
            bignum *tmp = bn_sub_abs(b, a);
            if (!tmp)
                return false;
            // Copy result back to a
            if (!bn_ensure_cap(a, tmp->len)) {
                bn_free(tmp);
                return false;
            }
            memcpy(a->limbs, tmp->limbs, tmp->len * sizeof(limb_t));
            a->len = tmp->len;
            a->sign = b->sign;
            bn_free(tmp);
            return true;
        }
    }
}

void bn_add_ip(bignum *a, const bignum *b)
{
    if (bn_add_ip_checked(a, b))
        return;
    fprintf(stderr, "bignum: capacity allocation failed\n");
    abort();
}

bool bn_sub_ip_checked(bignum *a, const bignum *b)
{
    // a - b = a + (-b)
    bignum neg_b = *b;
    neg_b.sign = !b->sign;
    return bn_add_ip_checked(a, &neg_b);
}

void bn_sub_ip(bignum *a, const bignum *b)
{
    if (bn_sub_ip_checked(a, b))
        return;
    fprintf(stderr, "bignum: capacity allocation failed\n");
    abort();
}

// In-place multiplication by a single limb
bool bn_mul_limb_ip_checked(bignum *a, limb_t b)
{
    if (b == 0) {
        a->len = 0;
        a->sign = 0;
        return true;
    }
    if (b == 1 || a->len == 0) {
        return true;
    }

    if (a->len == SIZE_MAX || !bn_ensure_cap(a, a->len + 1))
        return false;

    dlimb_t carry = 0;
    for (size_t i = 0; i < a->len; i++) {
        dlimb_t prod = (dlimb_t)a->limbs[i] * b + carry;
        a->limbs[i] = (limb_t)(prod & LIMB_MAX);
        carry = prod >> LIMB_BITS;
    }
    if (carry) {
        a->limbs[a->len++] = (limb_t)carry;
    }
    return true;
}

void bn_mul_limb_ip(bignum *a, limb_t b)
{
    if (bn_mul_limb_ip_checked(a, b))
        return;
    fprintf(stderr, "bignum: capacity allocation failed\n");
    abort();
}

// In-place add a single limb
bool bn_add_limb_ip_checked(bignum *a, limb_t b)
{
    if (b == 0)
        return true;

    if (a->len == 0) {
        if (!bn_ensure_cap(a, 1))
            return false;
        a->limbs[0] = b;
        a->len = 1;
        return true;
    }

    if (a->sign) {
        // Negative number: compute out-of-place so failure can be reported.
        bignum tmp;
        tmp.limbs = &b;
        tmp.len = 1;
        tmp.cap = 1;
        tmp.sign = 0;
        bignum *sum = bn_add(a, &tmp);
        if (!sum)
            return false;
        if (!bn_ensure_cap(a, sum->len)) {
            bn_free(sum);
            return false;
        }
        memcpy(a->limbs, sum->limbs, sum->len * sizeof(limb_t));
        a->len = sum->len;
        a->sign = sum->sign;
        bn_free(sum);
        return true;
    }

    if (a->len == SIZE_MAX || !bn_ensure_cap(a, a->len + 1))
        return false;

    dlimb_t carry = b;
    for (size_t i = 0; i < a->len && carry; i++) {
        dlimb_t sum = (dlimb_t)a->limbs[i] + carry;
        a->limbs[i] = (limb_t)(sum & LIMB_MAX);
        carry = sum >> LIMB_BITS;
    }
    if (carry) {
        a->limbs[a->len++] = (limb_t)carry;
    }
    return true;
}

void bn_add_limb_ip(bignum *a, limb_t b)
{
    if (bn_add_limb_ip_checked(a, b))
        return;
    fprintf(stderr, "bignum: capacity allocation failed\n");
    abort();
}

// In-place negate
void bn_neg_ip(bignum *a)
{
    if (a->len > 0) {
        a->sign = !a->sign;
    }
}
