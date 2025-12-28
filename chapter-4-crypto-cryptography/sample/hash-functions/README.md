# Hash Function Examples

This directory contains implementations and demonstrations of cryptographic hash functions, particularly SHA-256.

## Files

### SHA-256 Implementation

- **sha256.js** - JavaScript implementation of SHA-256 hash function
- **sha256_functions.py** - Python implementation of SHA-256 internal functions
  - Ch (Choose) function
  - Ma (Majority) function
  - Sigma functions (Σ₀, Σ₁, σ₀, σ₁)
  - Detailed educational implementation showing the internals

## Running the Examples

### JavaScript Examples

```bash
# SHA-256 implementation
node sha256.js
```

### Python Examples

```bash
# SHA-256 internal functions
python3 sha256_functions.py
```

## Key Concepts

### Cryptographic Hash Functions

A cryptographic hash function must have these properties:

1. **Deterministic**: Same input always produces same output
2. **Fast to compute**: Efficient calculation
3. **Pre-image resistance**: Cannot reverse the hash to get original input
4. **Small changes cascade**: Avalanche effect (1 bit change → ~50% output bits change)
5. **Collision resistant**: Hard to find two inputs with same hash

### SHA-256 Internals

The SHA-256 algorithm uses several logical functions:

- **Ch(x, y, z)**: Choose function - selects bits from y or z based on x
- **Ma(x, y, z)**: Majority function - returns majority bit from x, y, z
- **Σ₀, Σ₁**: Large sigma functions for mixing
- **σ₀, σ₁**: Small sigma functions for message schedule

### Applications

SHA-256 is used extensively in:
- **Bitcoin**: Block hashing, proof of work
- **Git**: Commit hashes
- **Digital signatures**: Message digests
- **Merkle trees**: Efficient data verification
- **Commitment schemes**: As shown in poker cryptography

## Security Notes

SHA-256 is currently considered secure:
- Part of SHA-2 family
- 256-bit output (2²⁵⁶ possible hashes)
- No practical attacks known
- Used in Bitcoin and other cryptocurrencies

For even higher security, SHA-3 (Keccak) is available, though SHA-256 remains the standard.

## Related Examples

- See `../randomness/avalanche-effect.go` for demonstration of hash avalanche effect
- See `../merkle-trees/` for Merkle tree implementation using SHA-256
- See `../poker/commitment-scheme.go` for commitment schemes using SHA-256
