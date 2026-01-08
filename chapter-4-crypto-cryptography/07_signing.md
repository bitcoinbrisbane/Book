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

## Mathematical Example (Simplified)

Let's work through a simplified example using small numbers to understand the algebra. This uses a toy RSA-style signature (not ECDSA) to illustrate the mathematical concepts.

**Note**: Real ECDSA uses elliptic curves and much larger numbers. This example demonstrates the core principles.

### Setup: Alice's Key Pair

```text
Choose two small primes: p = 3, q = 11
n = p * q = 3 * 11 = 33

Choose private exponent: d = 7 (Alice's private key)
Calculate public exponent: e = 3 (Alice's public key)

Alice's public key: (e=3, n=33)
Alice's private key: (d=7, n=33)
```

### Signing: Alice Signs a Message

```text
Message: m = 4

Alice creates signature using her private key:
signature = m^d mod n
         = 4^7 mod 33
         = 16384 mod 33
         = 16

Alice sends: (message=4, signature=16)
```

**Let's verify the math**:
```
4^7 = 16384
16384 ÷ 33 = 496 remainder 16
So 4^7 mod 33 = 16 ✓
```

### Verification: Bob Verifies the Signature

Bob receives the message (4) and signature (16). He uses Alice's **public key** to verify:

```text
Verify = signature^e mod n
       = 16^3 mod 33
       = 4096 mod 33
       = 4

Does this equal the original message? 4 = 4 ✓ VALID!
```

**Let's verify the math**:
```
16^3 = 4096
4096 ÷ 33 = 124 remainder 4
So 16^3 mod 33 = 4 ✓
```

### Tamper Detection

What if Eve tries to change the message from 4 to 5?

```text
Attacker's forged message: m' = 5
Original signature: 16

Verification:
16^3 mod 33 = 4 ≠ 5 ✗ INVALID!

The signature doesn't match! Tampering detected.
```

### Forgery Attempt

What if Eve tries to create her own signature for message 5 without the private key?

```text
She would need to find s such that:
s^3 mod 33 = 5

Trying some values:
1^3 = 1 ✗
2^3 = 8 ✗
3^3 = 27 ✗
4^3 mod 33 = 64 mod 33 = 31 ✗
5^3 mod 33 = 125 mod 33 = 26 ✗
...

Actually, s = 26 works because:
26^3 = 17576
17576 mod 33 = 5 ✓
```

**But**: Eve had to try many values (or solve a hard math problem). With real cryptography using 256-bit numbers, this would take billions of years!

### Why This Works

The mathematical relationship:
```text
(m^d)^e mod n = m^(d*e) mod n = m mod n

Signing:   signature = m^d mod n     (uses private key d)
Verifying: m = signature^e mod n     (uses public key e)

The relationship holds because:
d and e are chosen such that d*e ≡ 1 mod φ(n)
where φ(n) = (p-1)(q-1) = 2*10 = 20

In our example: 7*3 = 21 = 1*20 + 1 ✓
```

### Real-World ECDSA

The actual ECDSA algorithm used in Bitcoin and Ethereum:
- Uses **elliptic curve points** instead of simple exponentiation
- Works with **256-bit numbers** (77 digits!)
- Includes a **random nonce** in each signature
- Much more secure, but same core principle: **one-way mathematical relationship**

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
Decimal (base 10):      11253563012059685825953619222107823549092147699031672238385790369351542642469
Hexadecimal (base 16):  18e14a7b6a307f426a94f8114701e7c8e774e7f9a47e2c2035db29a206321725
Base64:                 GOFKe2owf0JqlPgRRwHnyOd05/mkfiICNdspogYyFyU=
WIF (Bitcoin):          5J3mBbAH58CpQ3Y5RNJpUKPE62SQ5tfcvU2JpbnkeyhfsYB1Jcn
```

**Note**: This is the same private key in different representations:
- **Decimal**: 77 digits - hard to read and type
- **Hexadecimal**: 64 characters (256 bits / 4 bits per hex digit) - more compact
- **Base64**: 44 characters - even more compact, used for data transmission
- **WIF**: 51 characters - Bitcoin-specific format with checksum

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

Here's a complete JavaScript implementation showing each step:

```javascript
const crypto = require('crypto');

// Step 1: Generate or use a private key (32 bytes)
function generatePrivateKey() {
    // In practice, use crypto.randomBytes(32) for production
    // Using a fixed key for demonstration
    return Buffer.from(
        '18e14a7b6a307f426a94f8114701e7c8e774e7f9a47e2c2035db29a206321725',
        'hex'
    );
}

// Step 2: Derive public key from private key using secp256k1
// Note: This requires the 'secp256k1' npm package
// npm install secp256k1
function getPublicKey(privateKey) {
    const secp256k1 = require('secp256k1');

    // Get uncompressed public key (65 bytes: 0x04 + x + y)
    const publicKeyUncompressed = secp256k1.publicKeyCreate(privateKey, false);

    // Get compressed public key (33 bytes: 0x02/0x03 + x)
    const publicKeyCompressed = secp256k1.publicKeyCreate(privateKey, true);

    console.log('Public Key (uncompressed):', publicKeyUncompressed.toString('hex'));
    console.log('Public Key (compressed):  ', publicKeyCompressed.toString('hex'));

    return publicKeyCompressed; // Bitcoin uses compressed keys now
}

// Step 3: SHA-256 hash
function sha256(buffer) {
    return crypto.createHash('sha256').update(buffer).digest();
}

// Step 4: RIPEMD-160 hash
function ripemd160(buffer) {
    return crypto.createHash('ripemd160').update(buffer).digest();
}

// Step 5: Create public key hash (hash160)
function hash160(buffer) {
    // SHA-256 followed by RIPEMD-160
    const sha = sha256(buffer);
    const ripe = ripemd160(sha);
    console.log('Hash160 (RIPEMD160(SHA256(pubKey))):', ripe.toString('hex'));
    return ripe;
}

// Step 6: Add version byte and checksum
function addVersionAndChecksum(hash160, version = 0x00) {
    // Version byte: 0x00 for mainnet, 0x6f for testnet
    const versionedHash = Buffer.concat([Buffer.from([version]), hash160]);
    console.log('Versioned hash:', versionedHash.toString('hex'));

    // Checksum: first 4 bytes of double SHA-256
    const checksum = sha256(sha256(versionedHash)).slice(0, 4);
    console.log('Checksum:', checksum.toString('hex'));

    // Combine: version + hash160 + checksum
    const addressBytes = Buffer.concat([versionedHash, checksum]);
    console.log('Address bytes:', addressBytes.toString('hex'));

    return addressBytes;
}

// Step 7: Base58 encoding
function base58Encode(buffer) {
    const ALPHABET = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';

    // Convert bytes to big integer
    let num = BigInt('0x' + buffer.toString('hex'));
    let encoded = '';

    // Convert to base58
    while (num > 0n) {
        const remainder = num % 58n;
        num = num / 58n;
        encoded = ALPHABET[Number(remainder)] + encoded;
    }

    // Add '1' for each leading zero byte
    for (let i = 0; i < buffer.length && buffer[i] === 0; i++) {
        encoded = '1' + encoded;
    }

    return encoded;
}

// Complete Bitcoin address generation
function generateBitcoinAddress() {
    console.log('Bitcoin Address Generation\n' + '='.repeat(50) + '\n');

    // Step 1: Private key
    const privateKey = generatePrivateKey();
    console.log('Step 1 - Private Key:');
    console.log('  Hex:', privateKey.toString('hex'));
    console.log('  Decimal:', BigInt('0x' + privateKey.toString('hex')).toString());
    console.log();

    // Step 2: Public key (secp256k1)
    console.log('Step 2 - Public Key (ECDSA secp256k1):');
    const publicKey = getPublicKey(privateKey);
    console.log();

    // Step 3 & 4: Hash the public key (SHA-256 then RIPEMD-160)
    console.log('Step 3 & 4 - Hash Public Key:');
    const pkHash = hash160(publicKey);
    console.log();

    // Step 5: Add version and checksum
    console.log('Step 5 - Add Version (0x00) and Checksum:');
    const addressBytes = addVersionAndChecksum(pkHash, 0x00);
    console.log();

    // Step 6: Base58 encode
    console.log('Step 6 - Base58 Encoding:');
    const address = base58Encode(addressBytes);
    console.log('  Bitcoin Address:', address);
    console.log();

    return address;
}

// Run the example
console.log('Example: Generating Bitcoin Address from Private Key\n');
const address = generateBitcoinAddress();

console.log('\n' + '='.repeat(50));
console.log('FINAL RESULT');
console.log('='.repeat(50));
console.log('Bitcoin Address:', address);
console.log('\nThis address can now receive Bitcoin!');
console.log('The private key is needed to spend from this address.');

// Demonstrate with Satoshi's genesis block address
console.log('\n\n' + '='.repeat(50));
console.log('BONUS: Verify with known address');
console.log('='.repeat(50));
console.log('\nNote: The address generated depends on the private key used.');
console.log('Different private keys generate different addresses.');
console.log('\nEach step is deterministic:');
console.log('  Same private key → Same public key → Same address');
```

**Expected Output**:

```text
Bitcoin Address Generation
==================================================

Step 1 - Private Key:
  Hex: 18e14a7b6a307f426a94f8114701e7c8e774e7f9a47e2c2035db29a206321725
  Decimal: 11253563012059685825953619222107823549092147699031672238385790369351542642469

Step 2 - Public Key (ECDSA secp256k1):
Public Key (uncompressed): 04[64 bytes]...
Public Key (compressed):   02[32 bytes]...

Step 3 & 4 - Hash Public Key:
Hash160 (RIPEMD160(SHA256(pubKey))): [20 bytes]

Step 5 - Add Version (0x00) and Checksum:
Versioned hash: 00[20 bytes]
Checksum: [4 bytes]
Address bytes: [25 bytes total]

Step 6 - Base58 Encoding:
  Bitcoin Address: 1[base58 string]

==================================================
FINAL RESULT
==================================================
Bitcoin Address: 1PrFGvMPvHXxCuQfiNQ6VgHxJt8Dum8RFe

This address can now receive Bitcoin!
The private key is needed to spend from this address.
```

**Key Implementation Details**:

1. **crypto module**: Node.js built-in for SHA-256 and RIPEMD-160
2. **secp256k1 package**: Required for elliptic curve operations (not in Node.js stdlib)
3. **BigInt**: Native JavaScript for large number handling
4. **Base58**: Custom implementation (Bitcoin uses Base58, not Base64)

**Installation**:
```bash
yarn add secp256k1
node bitcoin-address-generation.js
```

**Security Notes**:
- ⚠️ Never use predictable private keys in production
- ⚠️ Use `crypto.randomBytes(32)` for real key generation
- ⚠️ Store private keys securely (hardware wallet, encrypted storage)
- ✓ This code is for educational purposes to understand the process

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

## Real-World Applications

Digital signatures are fundamental to modern computing security. Here are the major use cases you encounter daily:

### SSH Authentication

SSH (Secure Shell) uses digital signatures to authenticate users and servers without passwords.

**Timeline of Algorithm Adoption**:

| Year | Event | Impact |
|------|-------|--------|
| **1999** | SSH-2 protocol with RSA/DSA | Initial public key authentication |
| **2009** | RFC 5656: ECDSA support | Added NIST P-256, P-384, P-521 curves |
| **2011** | OpenSSH 5.7 | First widespread ECDSA implementation |
| **2014** | OpenSSH 6.5: Ed25519 | Game-changer: fast, secure, simple |
| **2015** | OpenSSH 7.0 | **DSA deprecated** (security concerns) |
| **2016+** | Industry shift | Ed25519 becomes recommended default |

**Why the Migration Happened**:

```text
DSA (deprecated):
  - 1024-bit keys broken by modern computers
  - Poor random number generation = catastrophic failure
  - Removed from OpenSSH 7.0

RSA (still used):
  - Requires 2048-4096 bit keys for security
  - Slower key generation and signing
  - Large key files (~3KB for 4096-bit)
  - But: widely compatible, well-understood

ECDSA (good):
  - 256-bit keys = 3072-bit RSA security
  - Much smaller keys and signatures
  - But: NIST curves potentially backdoored by NSA
  - Complex implementation (side-channel risks)

Ed25519 (recommended):
  - 256-bit keys, extremely fast
  - Designed to avoid implementation mistakes
  - Resistant to timing attacks
  - Simple, modern, secure
  - ~68 byte keys vs ~400 bytes for RSA 2048
```

**Current Best Practices**:

```bash
# Generate Ed25519 key (recommended)
ssh-keygen -t ed25519 -C "your_email@example.com"
# Key size: 68 bytes
# Security: Excellent
# Speed: Very fast

# Generate RSA 4096 key (legacy compatibility)
ssh-keygen -t rsa -b 4096 -C "your_email@example.com"
# Key size: ~400 bytes
# Security: Excellent
# Speed: Slower

# AVOID: DSA (deprecated)
# CAUTION: ECDSA (NIST curves have concerns)
```

**How SSH Uses Signatures**:

```text
1. Server Authentication (prevents MITM attacks):
   - Server signs random challenge with its private key
   - Client verifies with server's public key (known from ~/.ssh/known_hosts)
   - Ensures you're connecting to the real server

2. Client Authentication (passwordless login):
   - Client signs random challenge from server
   - Server verifies with client's public key (in ~/.ssh/authorized_keys)
   - Proves client owns the private key without transmitting it

3. Key Exchange:
   - ECDH or RSA used to establish session keys
   - All subsequent traffic encrypted with symmetric keys
```

**Security Lessons from SSH**:
- **Algorithm agility matters**: SSH successfully migrated from DSA → RSA → ECDSA → Ed25519
- **Deprecate weak algorithms**: DSA removal shows importance of sunsetting old crypto
- **Performance drives adoption**: Ed25519's speed made it popular despite RSA being "good enough"
- **Side-channel resistance**: Constant-time implementations prevent timing attacks

### TLS/SSL Certificates (HTTPS)

Every website you visit uses digital signatures for authentication.

**How It Works**:

```text
1. Certificate Authority (CA) signs website's public key:
   - Website generates key pair
   - Creates Certificate Signing Request (CSR)
   - CA verifies website ownership
   - CA signs certificate with CA's private key

2. Your browser verifies:
   - Browser has CA's public key (pre-installed root certificates)
   - Verifies CA's signature on website certificate
   - Ensures certificate hasn't expired
   - Checks certificate hasn't been revoked
```

**Certificate Chain Example**:

```text
Root CA (DigiCert, Let's Encrypt, etc.)
    ↓ signs
Intermediate CA
    ↓ signs
Website Certificate (example.com)
    ↓ used to establish
Encrypted HTTPS Connection
```

**Migration to Elliptic Curves**:

| Algorithm | Key Size | Speed | Current Use |
|-----------|----------|-------|-------------|
| RSA 2048 | 2048 bits | Baseline | Still common, being phased out |
| RSA 3072+ | 3072+ bits | Slower | Recommended for new RSA certs |
| ECDSA P-256 | 256 bits | 10x faster | Increasingly common |
| Ed25519 | 256 bits | 15x faster | Emerging (TLS 1.3) |

**Let's Encrypt Stats** (2024):
- Issues ~3 million certificates per day
- ~70% use ECDSA P-256
- ~30% still use RSA 2048
- Driving industry to shorter, faster keys

**Browser Trust**:
- Chrome/Firefox ship with ~150 root CA certificates
- Distrusted CAs get removed (e.g., Symantec 2018)
- Certificate Transparency logs all certificates publicly
- OCSP/CRL checks for revoked certificates

### Code Signing

Software publishers sign their code to prove authenticity and integrity.

**Operating Systems**:

```text
macOS (codesign):
  - Apps must be signed with Apple Developer certificate
  - Gatekeeper verifies signature before allowing execution
  - Notarization required for App Store distribution
  - Uses ECDSA P-256 or RSA 2048

Windows (Authenticode):
  - EXE/DLL files signed with code signing certificate
  - SmartScreen checks publisher reputation
  - Extended Validation (EV) certificates show verified publisher
  - Uses RSA 2048 or ECDSA P-256

Android (APK Signing):
  - APK files signed with developer's key
  - Google Play enforces signing requirements
  - Version 2+: ECDSA or RSA
  - Version 4: Supports Ed25519
```

**Package Managers**:

```bash
# Debian/Ubuntu (APT)
# Package maintainers sign with GPG keys
apt-key list  # Show trusted keys
# Uses RSA or EdDSA

# Node.js (npm)
npm install --verify-signatures
# Verifies package signatures from registry

# Python (PyPI)
pip install --require-signature package
# Supports PGP signatures

# Container Images (Docker)
docker trust sign myimage:tag
# Signs container images with Notary
```

**Security Impact**:
- **Malware prevention**: Unsigned code triggers warnings
- **Supply chain security**: Detects compromised packages
- **Accountability**: Traces code back to specific developer/organization
- **Integrity**: Detects file corruption or tampering

### Git Commit Signing

Developers sign Git commits to prove authorship.

**Why Sign Commits?**

```text
Problem: Git commits can be forged
  git config user.name "Linus Torvalds"
  git config user.email "torvalds@linux.org"
  git commit -m "Backdoor added"
  # Looks like it came from Linus, but it didn't!

Solution: Sign commits with GPG/SSH keys
  git commit -S -m "Feature added"
  # Cryptographically proves YOU made this commit
```

**GitHub's Verified Badges**:

```text
Unsigned commit:  [ commit abc123 ]
Signed commit:    [ commit abc123 ] ✓ Verified

GitHub verifies:
  - Commit signature matches known public key
  - Key belongs to GitHub account
  - Signature was valid at time of commit
```

**Common Signing Methods**:

```bash
# GPG signing (traditional)
git config --global user.signingkey YOUR_GPG_KEY_ID
git config --global commit.gpgsign true
# Uses RSA, DSA, or EdDSA

# SSH signing (modern, simpler)
git config --global gpg.format ssh
git config --global user.signingkey ~/.ssh/id_ed25519.pub
git config --global commit.gpgsign true
# Uses your existing SSH key!

# Sign a commit
git commit -S -m "Add feature"

# Verify signatures
git log --show-signature
git verify-commit HEAD
```

**Real-World Usage**:
- **Linux kernel**: All maintainer commits signed
- **Bitcoin Core**: Release tags signed by developers
- **Kubernetes**: Requires signed commits for security patches
- **Enterprise**: Many companies require signed commits for compliance

### Blockchain and Cryptocurrency

Digital signatures are the foundation of all blockchain systems.

**Bitcoin Transactions**:

```text
Every Bitcoin transaction is a signed message:

Transaction:
  Input: Previous UTXO (coins to spend)
  Output: Recipient address
  Amount: 1 BTC

Signature proves:
  - You own the private key for the input address
  - You authorize this specific transaction
  - Transaction cannot be modified after signing

Without signatures:
  - Anyone could spend anyone's Bitcoin
  - No proof of ownership
  - Complete system failure
```

**Smart Contract Interactions**:

```text
Ethereum transaction to smart contract:

{
  to: "0x123...abc",           // Contract address
  data: "transfer(address,uint256)",  // Function call
  value: "0",
  gas: "50000",
  nonce: 42
}

Signed with private key → Only account holder can:
  - Call contract functions
  - Transfer tokens
  - Execute transactions

MetaMask/Hardware Wallet:
  - User approves transaction visually
  - Wallet signs with private key
  - Signature broadcast to network
```

**Multi-Signature Wallets**:

```text
3-of-5 Multisig (e.g., company treasury):
  - Requires 3 signatures out of 5 possible signers
  - CFO + CEO + COO must all sign for large transfers
  - Prevents single point of failure
  - Used by Coinbase, BitGo, Gnosis Safe
```

**Why Signatures Matter in Blockchain**:
- **No central authority**: Signatures replace username/password
- **Immutable**: Can't "reverse" a signed transaction
- **Public verification**: Anyone can verify, but only owner can sign
- **Self-custody**: "Not your keys, not your coins"

### Cross-Application Patterns

Notice the common thread across all applications:

| Application | What's Signed | Who Signs | Who Verifies |
|-------------|---------------|-----------|--------------|
| **SSH** | Authentication challenge | User/Server | Server/User |
| **TLS** | Website certificate | Certificate Authority | Browser |
| **Code Signing** | Software binary | Developer/Publisher | OS/User |
| **Git** | Commit hash | Developer | Developers/CI |
| **Blockchain** | Transaction data | Account owner | Network nodes |

**Universal Properties**:
1. **Authentication**: Proves identity without passwords
2. **Integrity**: Detects any modification
3. **Non-repudiation**: Signer can't deny signing
4. **Trust distribution**: No single point of failure

**Algorithm Trends Across Industries**:
- **2000s**: RSA dominance (2048-bit standard)
- **2010s**: ECDSA adoption (NIST P-256)
- **2015+**: Ed25519 emergence (SSH, Git, modern systems)
- **2020s**: Schnorr (Bitcoin), BLS (Ethereum 2.0), post-quantum prep

The migration pattern is consistent: **larger RSA → smaller ECDSA → faster Ed25519 → specialized curves (Schnorr, BLS)**

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
