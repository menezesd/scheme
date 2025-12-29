#ifndef BIGNUM_H
#define BIGNUM_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Bignum representation:
// - Uses 32-bit limbs for easy carry handling (fits in 64-bit for
// multiplication)
// - Stored little-endian (least significant limb first)
// - Sign stored separately
// - Normalized: no leading zero limbs (except for zero itself)

typedef uint32_t limb_t;
typedef uint64_t dlimb_t; // Double limb for multiplication

#define LIMB_BITS 32
#define LIMB_MAX UINT32_MAX
#define KARATSUBA_THRESHOLD                                                    \
    32 // Use Karatsuba for numbers with >= this many limbs

typedef struct {
    limb_t *limbs; // Array of limbs (little-endian)
    size_t len;    // Number of limbs
    size_t cap;    // Allocated capacity
    int sign;      // 0 = positive/zero, 1 = negative
} bignum;

// ============================================================================
// Creation and Destruction
// ============================================================================

bignum *bn_new(void);
bignum *bn_from_int(int64_t val);
bignum *bn_from_uint(uint64_t val);
bignum *bn_from_string(const char *str, int base);
bignum *bn_copy(const bignum *a);
void bn_free(bignum *a);

// ============================================================================
// Conversion
// ============================================================================

// Returns 0 if fits, non-zero if overflow
int bn_to_int64(const bignum *a, int64_t *out);
int bn_to_uint64(const bignum *a, uint64_t *out);
char *bn_to_string(const bignum *a, int base);

// Check if bignum fits in int64
bool bn_fits_int64(const bignum *a);

// ============================================================================
// Comparison
// ============================================================================

int bn_cmp(const bignum *a, const bignum *b);     // -1, 0, 1
int bn_cmp_abs(const bignum *a, const bignum *b); // Compare absolute values
bool bn_is_zero(const bignum *a);
int bn_sign(const bignum *a); // -1, 0, 1

// ============================================================================
// Arithmetic Operations
// ============================================================================

bignum *bn_add(const bignum *a, const bignum *b);
bignum *bn_sub(const bignum *a, const bignum *b);
bignum *bn_mul(const bignum *a, const bignum *b);
bignum *bn_div(const bignum *a, const bignum *b, bignum **remainder);
bignum *bn_mod(const bignum *a, const bignum *b);
bignum *bn_neg(const bignum *a);
bignum *bn_abs(const bignum *a);

// In-place operations for efficiency
void bn_add_ip(bignum *a, const bignum *b);
void bn_sub_ip(bignum *a, const bignum *b);
void bn_mul_limb_ip(bignum *a, limb_t b);
void bn_add_limb_ip(bignum *a, limb_t b);
void bn_neg_ip(bignum *a);

// ============================================================================
// Bitwise Operations
// ============================================================================

bignum *bn_lshift(const bignum *a, size_t bits);
bignum *bn_rshift(const bignum *a, size_t bits);

// ============================================================================
// Other Operations
// ============================================================================

bignum *bn_gcd(const bignum *a, const bignum *b);
bignum *bn_pow(const bignum *base, uint64_t exp);

#endif // BIGNUM_H
