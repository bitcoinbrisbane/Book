# Digital Signatures

Digital signatures are the cryptographic equivalent of handwritten signatures, but with mathematical guarantees that physical signatures cannot provide. In blockchain systems, digital signatures are fundamental - they prove ownership of funds, authorize transactions, and secure the entire network without requiring trust in any central authority.

A digital signature serves three critical purposes:
1. **Authentication**: Proves the signer's identity
2. **Non-repudiation**: The signer cannot deny having signed
3. **Integrity**: Ensures the message hasn't been tampered with

Unlike symmetric encryption which uses the same key for encryption and decryption, digital signatures use asymmetric cryptography with a key pair: a private key for signing and a public key for verification.

## Why Digital Signatures Matter

In blockchain systems, digital signatures are everywhere:

- **Bitcoin transactions**: Every transaction must be signed by the private key controlling the funds
- **Smart contracts**: Ethereum transactions are signed to authorize state changes
- **Our poker application**: Signatures will verify player actions and commitments
- **Website security**: SSL/TLS certificates sign website content for authenticity

Without digital signatures, there would be no way to prove ownership or authorize actions in a decentralized system.

## How Digital Signatures Work

The signing process involves three steps:

### 1. Key Generation

```text
Private Key (secret)  →  Public Key (shared)
     sk                        pk
```

The private key is a random number kept secret by the owner. The public key is derived from the private key using elliptic curve mathematics (one-way function - cannot reverse).

### 2. Signing a Message

```text
Message → Hash → Sign with Private Key → Signature
  msg      h         Sign(h, sk)            sig
```

Steps:
1. Hash the message to get a fixed-size digest
2. Apply the signing algorithm using your private key
3. Output is the signature (usually 64-72 bytes)

### 3. Verification

```text
Message + Signature + Public Key → Valid/Invalid
  msg       sig          pk          ✓ or ✗
```

Anyone with the public key can verify:
- Hash the message
- Use the signature and public key in verification algorithm
- Returns true if signature is valid, false otherwise

**Key Property**: Only the private key holder can create valid signatures, but anyone can verify them.

## Signing in Practice

Here's a complete example using ECDSA (Elliptic Curve Digital Signature Algorithm):

```go
package main

import (
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/sha256"
	"fmt"
)

func main() {
	fmt.Println("Digital Signature Example (ECDSA)")
	fmt.Println("=================================\n")

	// 1. Generate key pair
	privateKey, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		panic(err)
	}
	publicKey := &privateKey.PublicKey

	fmt.Println("Step 1: Key Generation")
	fmt.Printf("Private key: %x\n", privateKey.D.Bytes()[:8]) // Show only first 8 bytes
	fmt.Printf("Public key X: %x\n", publicKey.X.Bytes()[:8])
	fmt.Printf("Public key Y: %x\n\n", publicKey.Y.Bytes()[:8])

	// 2. Create a message and sign it
	message := "I authorize payment of 10 BTC to Alice"
	messageHash := sha256.Sum256([]byte(message))

	fmt.Println("Step 2: Signing")
	fmt.Printf("Message: %s\n", message)
	fmt.Printf("Message hash: %x\n", messageHash)

	r, s, err := ecdsa.Sign(rand.Reader, privateKey, messageHash[:])
	if err != nil {
		panic(err)
	}

	fmt.Printf("Signature (r): %x\n", r.Bytes()[:16]) // First 16 bytes
	fmt.Printf("Signature (s): %x\n\n", s.Bytes()[:16])

	// 3. Verify the signature
	fmt.Println("Step 3: Verification")
	valid := ecdsa.Verify(publicKey, messageHash[:], r, s)
	fmt.Printf("Signature valid: %v\n\n", valid)

	// 4. Demonstrate tampering detection
	fmt.Println("Step 4: Tampering Detection")
	tamperedMessage := "I authorize payment of 100 BTC to Alice"
	tamperedHash := sha256.Sum256([]byte(tamperedMessage))

	tamperedValid := ecdsa.Verify(publicKey, tamperedHash[:], r, s)
	fmt.Printf("Tampered message: %s\n", tamperedMessage)
	fmt.Printf("Signature valid for tampered message: %v\n", tamperedValid)
	fmt.Println("✓ Tampering detected! Signature verification failed.")
}
```

## Key Pairs

A key pair consists of two mathematically related numbers:

### Private Key
- A large random number (typically 256 bits)
- Must be kept absolutely secret
- Used to create signatures
- If compromised, attacker can steal funds or impersonate you

### Public Key
- Derived from the private key using elliptic curve mathematics
- Can be safely shared with anyone
- Used to verify signatures
- Cannot be used to derive the private key (one-way function)

### Key Representations

Keys are typically encoded in different formats for readability:

```text
Private Key (raw):      A very large number (2^256 possibilities)
Hexadecimal (base 16):  18e14a7b6a307f426a94f8114701e7c8e774e7f9a47e2c2035db29a206321725
Base64:                 GOFKe2owf0JqlPgRRwHnyOd05/mkfiICNdspogYyFyU=
WIF (Bitcoin):          5J3mBbAH58CpQ3Y5RNJpUKPE62SQ5tfcvU2JpbnkeyhfsYB1Jcn
```

**Important**: Messages sent over the wire are encoded as byte arrays for efficient transmission.

## Digital Signatures in Bitcoin

Bitcoin uses ECDSA with the secp256k1 elliptic curve. Every Bitcoin transaction must be signed to prove ownership and authorize the transfer of funds.

### Transaction Signing Process

```text
Alice wants to send 1 BTC to Bob:

1. Create transaction:
   Input: Previous UTXO (unspent output) Alice controls
   Output: Bob's address
   Amount: 1 BTC

2. Hash the transaction data

3. Sign the hash with Alice's private key
   Signature = Sign(txHash, alicePrivateKey)

4. Broadcast: Transaction + Signature + Alice's Public Key

5. Network verification:
   - Nodes hash the transaction
   - Verify signature using Alice's public key
   - Confirm Alice owns the input UTXO
   - If valid, include in next block
```

### Bitcoin Address Generation

Your Bitcoin address is derived from your public key through multiple hashing steps:

```text
Private Key (256 bits random)
    ↓
ECDSA secp256k1 curve multiplication
    ↓
Public Key (33 or 65 bytes, compressed or uncompressed)
    ↓
SHA-256 hash
    ↓
RIPEMD-160 hash
    ↓
Add version byte (0x00 for mainnet)
    ↓
Add checksum (double SHA-256, first 4 bytes)
    ↓
Base58 encoding
    ↓
Bitcoin Address: 1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa
```

### Schnorr Signatures (Taproot)

Bitcoin's Taproot upgrade (activated November 2021) introduced Schnorr signatures as an alternative to ECDSA.

**Advantages**:
- **Smaller signatures**: 64 bytes vs 71-72 bytes for ECDSA
- **Signature aggregation**: Combine multiple signatures into one
- **Better privacy**: Multi-signature looks identical to single signature
- **Provably secure**: Mathematical security proof (unlike ECDSA)
- **Linearity**: Enables advanced features like adaptor signatures

**Schnorr Signing Algorithm**:
```text
Given: message m, private key x, public key P = x*G

1. Generate random nonce: k
2. Calculate R = k*G
3. Calculate challenge: e = Hash(R || P || m)
4. Calculate signature: s = k + e*x

Signature = (R, s)

Verification:
s*G = R + e*P  (using public information only)
```

## Digital Signatures in Ethereum

Ethereum also uses ECDSA with secp256k1, but with some differences from Bitcoin:

### Ethereum Address Generation

```text
Private Key (256 bits)
    ↓
ECDSA secp256k1
    ↓
Public Key (64 bytes, uncompressed, without prefix)
    ↓
Keccak-256 hash
    ↓
Take last 20 bytes
    ↓
Add "0x" prefix
    ↓
Ethereum Address: 0x742d35Cc6634C0532925a3b844Bc9e7595f0bEb4
```

**Key Differences from Bitcoin**:
- Uses Keccak-256 instead of SHA-256 + RIPEMD-160
- No Base58 encoding (uses hexadecimal with 0x prefix)
- No checksum in address (checksum is optional via EIP-55 capitalization)

### EIP-191: Signed Data Standard

Ethereum uses a specific format for signing messages to prevent replay attacks:

```text
"\x19Ethereum Signed Message:\n" + len(message) + message
```

This prevents a signature intended for one context (like a message) from being used in another (like a transaction).

## Signature Algorithm Comparison

| Algorithm | Signature Size | Speed | Security | Used In |
|-----------|---------------|-------|----------|---------|
| **ECDSA (secp256k1)** | 64-72 bytes | Fast | Secure* | Bitcoin, Ethereum |
| **EdDSA (Ed25519)** | 64 bytes | Faster | Provably Secure | Monero, Polkadot, Solana |
| **Schnorr (secp256k1)** | 64 bytes | Fast | Provably Secure | Bitcoin (Taproot) |
| **RSA (2048-bit)** | 256 bytes | Slower | Secure | TLS/SSL, PGP |
| **BLS** | 48-96 bytes | Moderate | Secure | Ethereum 2.0 |

*ECDSA security requires careful nonce generation (see Security Considerations)

### Choosing an Algorithm

For blockchain applications:
- **Bitcoin integration**: Use ECDSA secp256k1 (or Schnorr for Taproot)
- **Ethereum integration**: Use ECDSA secp256k1 with Keccak-256
- **New protocols**: Consider EdDSA (Ed25519) for better performance
- **Threshold signatures**: BLS signatures enable efficient multi-party signing

## Signature Verification

The verification process allows anyone with the public key to confirm:

1. The message was signed by the holder of the corresponding private key
2. The message has not been altered since it was signed
3. The signature was created for this specific message (not reused from elsewhere)

Verification is typically fast and can be performed by anyone, making it ideal for distributed systems like blockchain where thousands of nodes verify the same signatures.

## Security Considerations

Digital signatures are only secure if implemented correctly. Here are critical security concerns:

### 1. Nonce Reuse Catastrophe

**Critical**: Never reuse the random nonce (k) when signing different messages with the same private key!

```text
If you sign two different messages with the same nonce:
  sig1 = (r, s1) for message1
  sig2 = (r, s2) for message2  // Same r means same nonce k!

An attacker can recover your private key:
  s1 = k + hash(msg1) * privateKey
  s2 = k + hash(msg2) * privateKey

  s1 - s2 = (hash(msg1) - hash(msg2)) * privateKey
  privateKey = (s1 - s2) / (hash(msg1) - hash(msg2))
```

**Real-World Examples**:
- **PlayStation 3 hack (2010)**: Sony reused nonces in ECDSA signatures, allowing hackers to extract the private key and sign custom firmware
- **Blockchain.info Android wallet (2013)**: Weak random number generator led to predictable nonces, resulting in Bitcoin theft
- **Various Bitcoin wallets**: Multiple instances of nonce reuse leading to fund theft

### 2. Weak Random Number Generation

```go
// ❌ WRONG - Never use math/rand for cryptography!
import "math/rand"
nonce := rand.Int63()

// ✅ CORRECT - Always use crypto/rand
import "crypto/rand"
nonce, err := rand.Int(rand.Reader, curve.Params().N)
```

**Why it matters**:
- Predictable nonces = predictable signatures = recoverable private keys
- `math/rand` is deterministic and predictable
- `crypto/rand` uses OS-level entropy sources

### 3. Side-Channel Attacks

Attackers can extract private keys by measuring:
- **Timing**: How long signing takes
- **Power consumption**: Changes during computation
- **Electromagnetic radiation**: Signals emitted by hardware

**Mitigation**:
- Use constant-time implementations
- Hardware wallets (isolated environment)
- Avoid signing on untrusted devices

### 4. Signature Malleability

ECDSA signatures can be modified without invalidating them:

```text
If (r, s) is a valid signature, then (r, -s mod n) is also valid!
```

**Impact**:
- Transaction ID changes
- Can cause double-spend detection issues
- Breaks payment protocols relying on txid

**Bitcoin's Solution (BIP 62)**:
- Require s to be in lower half of range
- Reject high-s signatures
- Makes signatures non-malleable

### 5. Key Management Best Practices

**Storage**:
- ❌ Never store private keys in plain text
- ❌ Never upload private keys to cloud services
- ❌ Never share private keys via email/chat
- ✅ Use hardware wallets for long-term storage
- ✅ Use encrypted keystores with strong passwords
- ✅ Use BIP39 mnemonics for backup (12-24 word phrases)

**Usage**:
- Use deterministic wallets (BIP32/BIP44)
- One key pair per transaction (for privacy)
- Multi-signature for high-value accounts
- Timelock mechanisms for recovery

**Operational Security**:
- Generate keys on air-gapped computers
- Never type seed phrases on internet-connected devices
- Verify addresses on hardware wallet screen
- Test recovery process with small amounts first

### 6. Message Hashing

Always hash messages before signing:

```go
// ❌ WRONG - Signing raw message
signature := Sign(message, privateKey)

// ✅ CORRECT - Hash first, then sign
messageHash := SHA256(message)
signature := Sign(messageHash, privateKey)
```

**Reasons**:
- Signing algorithms expect fixed-size input
- Hashing provides collision resistance
- Prevents certain cryptographic attacks

## Multi-Signature Schemes

Multi-signature (multisig) requires multiple parties to sign before a transaction is valid.

### Use Cases
- **Corporate accounts**: Require 3-of-5 executives to approve large transfers
- **Escrow**: 2-of-3 signatures (buyer, seller, arbiter)
- **Personal security**: 2-of-3 (desktop, mobile, hardware wallet)
- **DAO treasuries**: M-of-N governance members

### Implementation Approaches

**1. Script-based (Bitcoin)**:
```text
2-of-3 MultiSig Address:
  Requires 2 signatures out of 3 possible keys
  Larger transaction size
  Obvious on blockchain
```

**2. Schnorr Aggregation**:
```text
Multiple signatures combine into single signature
  Same size as single-sig
  Better privacy
  More efficient verification
```

**3. Threshold Signatures (TSS)**:
```text
Key is split among parties
  No single party knows full private key
  Generate signature collaboratively
  Appears as single signature on-chain
```

## References and Further Reading

### Academic Papers
- **ECDSA**: ANSI X9.62-2005, "Public Key Cryptography for the Financial Services Industry"
- **EdDSA**: Bernstein, D. et al. (2012). "High-speed high-security signatures"
- **Schnorr**: Schnorr, C.P. (1991). "Efficient signature generation by smart cards"

### Bitcoin Improvement Proposals (BIPs)
- **BIP 32**: Hierarchical Deterministic Wallets
- **BIP 39**: Mnemonic code for generating deterministic keys
- **BIP 62**: Dealing with malleability
- **BIP 340**: Schnorr Signatures for secp256k1
- **BIP 341**: Taproot: SegWit version 1 spending rules
- **BIP 342**: Validation of Taproot Scripts

### Ethereum Improvement Proposals (EIPs)
- **EIP-155**: Simple replay attack protection
- **EIP-191**: Signed Data Standard
- **EIP-712**: Typed structured data hashing and signing
- **EIP-2098**: Compact signature representation

### Books and Resources
- **"Mastering Bitcoin"** by Andreas Antonopoulos - Chapter 4 (Keys, Addresses)
- **"Mastering Ethereum"** by Antonopoulos & Wood - Chapter 5 (Wallets)
- **Bitcoin Developer Guide**: bitcoin.org/en/developer-guide
- **Ethereum Yellow Paper**: ethereum.github.io/yellowpaper

### Security Incidents
- **PlayStation 3 ECDSA Fail**: Console-hacking blog posts (2010)
- **Bitcoin Nonce Reuse**: Various incident reports
- **Mt. Gox**: Lessons in key management

### Related Topics in This Book
- Chapter 2: Randomness (for nonce generation)
- Chapter 3: Hash functions (SHA-256, Keccak-256)
- Chapter 6: Poker cryptography (commitment schemes)
- Chapter 11: Zero-knowledge proofs (privacy-preserving signatures)
