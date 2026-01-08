# Digital Signatures Examples

This directory contains examples demonstrating digital signature algorithms, key generation, signing, verification, and security vulnerabilities.

## Files

### ECDSA Signing

- **ecdsa-signing.go** - Complete ECDSA signing and verification example
  - Key pair generation
  - Message signing with SHA-256
  - Signature verification
  - Tampering detection
  - Wrong public key detection

### Security Demonstrations

- **nonce-reuse-attack.go** - Demonstrates the catastrophic nonce reuse vulnerability
  - Shows how reusing nonces leaks private keys
  - Mathematical explanation of the attack
  - Real-world examples (PS3, Bitcoin wallets)

### Bitcoin Address Generation

- **bitcoin-address-generation.js** - Complete Bitcoin address generation from private key
  - Step-by-step demonstration of all 6 steps
  - Private key → Public key (secp256k1)
  - SHA-256 and RIPEMD-160 hashing
  - Base58 encoding implementation
  - Shows both compressed and uncompressed public keys

## Running the Examples

### Go Examples

```bash
# ECDSA signing and verification
go run ecdsa-signing.go

# Nonce reuse attack demonstration
go run nonce-reuse-attack.go
```

### JavaScript Examples

```bash
# Install dependencies first (only needed once)
yarn add secp256k1
# or: npm install secp256k1

# Bitcoin address generation from private key
node bitcoin-address-generation.js
```

## Key Concepts

### Digital Signatures

Digital signatures provide three guarantees:
1. **Authentication**: Proves who signed the message
2. **Integrity**: Detects any tampering with the message
3. **Non-repudiation**: Signer cannot deny signing

### ECDSA (Elliptic Curve Digital Signature Algorithm)

Used by Bitcoin and Ethereum, ECDSA signs messages using elliptic curve cryptography:

```text
Signing:
1. Generate random nonce k
2. Calculate point R = k * G (G is generator point)
3. r = R.x mod n
4. s = k^-1 * (hash(m) + r * privateKey) mod n
5. Signature = (r, s)

Verification:
1. Calculate w = s^-1 mod n
2. u1 = hash(m) * w mod n
3. u2 = r * w mod n
4. Point P = u1 * G + u2 * PublicKey
5. Valid if P.x mod n == r
```

### Key Properties

**Private Key**:
- Random 256-bit number
- Must remain secret
- Used to create signatures
- One compromise = total loss

**Public Key**:
- Derived from private key via elliptic curve multiplication
- Can be shared publicly
- Used to verify signatures
- Cannot be reversed to get private key

**Signature**:
- Two numbers (r, s), each 32 bytes
- Proves ownership of private key
- Specific to both message and private key
- Anyone can verify with public key

## Security Warnings

### Critical: Never Reuse Nonces!

The `nonce-reuse-attack.go` example demonstrates why:

```go
// If you sign two messages with the same nonce:
sig1 = (r, s1) for message1
sig2 = (r, s2) for message2  // Same r = same nonce!

// Attacker can extract your private key:
privateKey = (s1 - s2)^-1 * (hash(m1) - hash(m2)) * r^-1
```

**Real Attacks**:
- **PlayStation 3 (2010)**: Sony reused nonces → console hacked
- **Blockchain.info wallet (2013)**: Weak RNG → Bitcoin stolen
- **Multiple wallets**: Nonce reuse → millions lost

### Use Cryptographically Secure Randomness

```go
// ❌ WRONG
import "math/rand"
k := rand.Int63() // Predictable!

// ✅ CORRECT
import "crypto/rand"
k, err := rand.Int(rand.Reader, curve.Params().N)
```

### Always Hash Before Signing

```go
// ❌ WRONG - signing raw message
sig := Sign(message, privateKey)

// ✅ CORRECT - hash first
hash := SHA256(message)
sig := Sign(hash, privateKey)
```

## Comparison: ECDSA vs EdDSA vs Schnorr

| Property | ECDSA | EdDSA | Schnorr |
|----------|-------|-------|---------|
| **Curve** | secp256k1, P-256 | Ed25519 | secp256k1 |
| **Signature Size** | 64-72 bytes | 64 bytes | 64 bytes |
| **Speed** | Fast | Faster | Fast |
| **Nonce** | Random (dangerous) | Deterministic | Deterministic |
| **Security Proof** | No | Yes | Yes |
| **Signature Aggregation** | No | No | Yes |
| **Used In** | Bitcoin, Ethereum | Monero, Solana | Bitcoin Taproot |

**Recommendations**:
- **Bitcoin/Ethereum**: Use secp256k1 ECDSA (required by protocol)
- **New projects**: Use Ed25519 for simplicity and speed
- **Advanced features**: Use Schnorr for aggregation

## Bitcoin and Ethereum Differences

### Bitcoin (ECDSA secp256k1)
```text
Private Key → Public Key → SHA-256 → RIPEMD-160
           → Add version → Add checksum → Base58
           → Bitcoin Address (1...)
```

### Ethereum (ECDSA secp256k1)
```text
Private Key → Public Key (uncompressed) → Keccak-256
           → Take last 20 bytes → Add 0x prefix
           → Ethereum Address (0x...)
```

**Key Differences**:
- Bitcoin uses SHA-256 + RIPEMD-160, Ethereum uses Keccak-256
- Bitcoin uses Base58 encoding, Ethereum uses hexadecimal
- Bitcoin addresses have checksums, Ethereum addresses don't (optional EIP-55)

## Multi-Signature Examples

Multi-signature schemes require multiple parties to sign:

### Use Cases
- **2-of-3 escrow**: Buyer, seller, arbitrator
- **3-of-5 corporate**: Require 3 executives for large transfers
- **2-of-3 personal**: Desktop, mobile, hardware wallet

### Types
1. **Script-based**: Multiple signatures in transaction (visible)
2. **Schnorr aggregation**: Combine into single signature (private)
3. **Threshold signatures**: Split key among parties

## Common Vulnerabilities

1. **Nonce reuse**: Private key recovery
2. **Weak RNG**: Predictable signatures
3. **Side-channel attacks**: Timing, power analysis
4. **Signature malleability**: (r, s) vs (r, -s)
5. **Key management**: Insecure storage, backup

## Best Practices

### Key Generation
- Use `crypto/rand` for entropy
- Generate on air-gapped device for cold storage
- Never share or upload private keys
- Use BIP39 mnemonics for backup

### Signing
- Hash messages before signing
- Use deterministic nonces (RFC 6979)
- Verify implementation is constant-time
- Consider hardware wallets for valuable keys

### Verification
- Always verify signatures before trusting
- Check message hash matches expected
- Ensure public key is from trusted source
- Validate signature format (non-malleable)

## Resources

### Standards
- **RFC 6979**: Deterministic Usage of DSA and ECDSA
- **SEC 1**: Elliptic Curve Cryptography
- **SEC 2**: Recommended Elliptic Curve Domain Parameters
- **FIPS 186-4**: Digital Signature Standard (DSS)

### Bitcoin
- **BIP 32**: Hierarchical Deterministic Wallets
- **BIP 39**: Mnemonic Code for Generating Keys
- **BIP 340**: Schnorr Signatures
- **BIP 341**: Taproot

### Ethereum
- **EIP-191**: Signed Data Standard
- **EIP-712**: Typed Structured Data Hashing
- **EIP-2098**: Compact Signature Representation

### Books
- "Mastering Bitcoin" by Andreas Antonopoulos
- "Mastering Ethereum" by Antonopoulos & Wood

## Related Examples

- See `../hash-functions/` for SHA-256 implementation
- See `../randomness/` for secure random number generation
- See `../poker/commitment-scheme.go` for signature-like commitments
