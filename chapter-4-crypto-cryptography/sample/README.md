# Chapter 4: Crypto & Cryptography - Code Examples

This directory contains all code examples for Chapter 4, organized by topic.

## Documentation Statistics

| Directory | Files | Lines | Words | Topics Covered |
|-----------|-------|-------|-------|----------------|
| `encryption/` | 4 | 66 | 244 | RC4, 3DES, symmetric encryption |
| `hash-functions/` | 3 | 76 | 318 | SHA-256 internals, hash properties |
| `merkle-trees/` | 3 | 139 | 611 | Merkle proofs, blockchain applications |
| `poker/` | 3 | 106 | 494 | Commitment schemes, mental poker |
| `randomness/` | 5 | 66 | 254 | PRNGs, avalanche effect |
| `signing/` | 4 | 390 | 975 | ECDSA, Bitcoin/Ethereum, security |
| **Total** | **22** | **843** | **2,896** | **6 major topics** |

## Directory Structure

```
sample/
├── encryption/          # Symmetric encryption algorithms
│   ├── rc4.js
│   ├── tripledes.js
│   └── tripledes.py
├── hash-functions/      # Cryptographic hash implementations
│   ├── sha256.js
│   └── sha256_functions.py
├── merkle-trees/        # Merkle tree implementations
│   ├── merkle_tree.py
│   └── merkle-whitelist.js
├── poker/              # Poker cryptography (commitment schemes, mental poker)
│   ├── commitment-scheme.go
│   └── mental-poker.go
├── randomness/         # PRNGs and randomness demonstrations
│   ├── avalanche-effect.go
│   ├── avalanche-effect.py
│   ├── mersenne-twister.go
│   └── mersenne-twister.py
└── signing/            # Digital signatures and cryptographic signing
    ├── ecdsa-signing.go
    ├── nonce-reuse-attack.go
    └── bitcoin-address-generation.js
```

## Quick Start

Each subdirectory contains a README with:
- Explanation of the concepts
- Instructions for running the code
- Security considerations
- Real-world applications

### Running Go Examples

```bash
# From the sample/ directory:
cd sample/poker
go run commitment-scheme.go
go run mental-poker.go

cd ../randomness
go run mersenne-twister.go
go run avalanche-effect.go

cd ../signing
go run ecdsa-signing.go
go run nonce-reuse-attack.go
```

### Running Python Examples

```bash
# From the sample/ directory:
cd sample/randomness
python3 mersenne-twister.py
python3 avalanche-effect.py

cd ../merkle-trees
python3 merkle_tree.py

cd ../hash-functions
python3 sha256_functions.py

cd ../encryption
pip install pycryptodome  # Only needed for tripledes.py
python3 tripledes.py
```

### Running JavaScript Examples

```bash
# From the sample/ directory:
cd sample/encryption
node rc4.js
node tripledes.js

cd ../hash-functions
node sha256.js

cd ../merkle-trees
node merkle-whitelist.js

cd ../signing
yarn add secp256k1  # Install dependencies first
node bitcoin-address-generation.js
```

## Topics Covered

### 🔐 Encryption (`encryption/`) - 244 words
**Files**: `rc4.js`, `tripledes.js`, `tripledes.py`, `README.md`

**Sections in README**:
- Symmetric encryption concepts (block vs stream ciphers)
- RC4 stream cipher implementation
- Triple DES (3DES) block cipher
- Security notes (why RC4 is deprecated)
- Modes of operation (CBC, GCM)
- Best practices (use AES-256 instead)

**Key Concepts**: Stream ciphers, block ciphers, symmetric encryption, CBC mode

### #️⃣ Hash Functions (`hash-functions/`) - 318 words
**Files**: `sha256.js`, `sha256_functions.py`, `README.md`

**Sections in README**:
- Cryptographic hash function properties
- SHA-256 internal functions (Ch, Ma, Sigma)
- Pre-image resistance
- Collision resistance
- Applications in Bitcoin, Git, signatures
- Security notes (SHA-256 vs SHA-3)

**Key Concepts**: One-way functions, avalanche effect, collision resistance, Bitcoin mining

### 🌳 Merkle Trees (`merkle-trees/`) - 611 words
**Files**: `merkle_tree.py`, `merkle-whitelist.js`, `README.md`

**Sections in README**:
- What are Merkle trees
- Merkle proof generation and verification
- Properties (efficiency, tamper evidence)
- Applications in Bitcoin SPV
- Ethereum state trees
- Whitelist implementation example
- Security considerations (second pre-image attack)

**Key Concepts**: Binary trees, hash trees, SPV clients, efficient verification, O(log n) proofs

### 🎲 Randomness (`randomness/`) - 254 words
**Files**: `mersenne-twister.go`, `mersenne-twister.py`, `avalanche-effect.go`, `avalanche-effect.py`, `README.md`

**Sections in README**:
- Mersenne Twister PRNG algorithm
- MT19937 properties (period 2^19937-1)
- Why it's NOT cryptographically secure
- Avalanche effect demonstration
- Bit-level change analysis
- Secure alternatives (crypto/rand, secrets module)

**Key Concepts**: PRNGs, Mersenne Twister, avalanche effect, cryptographic randomness

### 🃏 Poker Cryptography (`poker/`) - 494 words
**Files**: `commitment-scheme.go`, `mental-poker.go`, `README.md`

**Sections in README**:
- Commitment scheme properties (hiding, binding)
- Coin flip game implementation
- Mental poker protocol
- Card shuffling without dealer
- Commutative encryption
- Zero-knowledge proofs
- Connection to HTLCs and blockchain
- Use cases (decentralized gaming, escrow)

**Key Concepts**: Commit-reveal, mental poker, trustless protocols, commutative encryption

### 🔏 Digital Signatures (`signing/`) - 975 words
**Files**: `ecdsa-signing.go`, `nonce-reuse-attack.go`, `bitcoin-address-generation.js`, `README.md`

**Sections in README**:
- ECDSA algorithm explanation
- Key pair generation
- Signing and verification
- Bitcoin vs Ethereum differences
- Nonce reuse catastrophe (with math)
- Real-world attacks (PS3, Bitcoin wallets)
- Algorithm comparison (ECDSA, EdDSA, Schnorr, RSA, BLS)
- Security best practices
- Multi-signature schemes
- Key management

**Key Concepts**: ECDSA, secp256k1, private/public keys, nonce reuse, signature malleability, multisig

## Learning Path

### For Beginners
1. Start with **hash-functions/** to understand SHA-256
2. Try **randomness/avalanche-effect** to see hashing in action
3. Explore **merkle-trees/** to see hashes in data structures
4. Move to **poker/commitment-scheme** for practical applications

### For Intermediate Learners
1. Study **poker/mental-poker** for advanced cryptographic protocols
2. Compare **randomness/mersenne-twister** (insecure) vs `crypto/rand` (secure)
3. Understand why **encryption/rc4** is deprecated
4. Implement your own variations

### For Advanced Study
1. Implement commutative encryption for real mental poker
2. Add zero-knowledge proofs to poker protocol
3. Build a Merkle tree-based blockchain
4. Create a provably fair gambling system

## Security Warnings

⚠️ **Educational Code**: All examples are for learning purposes.

**Do NOT use in production**:
- RC4 (broken)
- 3DES (deprecated)
- Mersenne Twister for cryptography
- SHA-256 hashing instead of proper encryption

**DO use in production**:
- AES-256-GCM for encryption
- SHA-256 for hashing (it's designed for this!)
- `crypto/rand` (Go) or `secrets` (Python) for random values
- Established libraries like OpenSSL, libsodium, cryptography

## Real-World Connections

### Bitcoin
- **Merkle trees**: Transaction verification in blocks
- **SHA-256**: Mining, addresses, signatures
- **Commitment schemes**: HTLCs in Lightning Network

### Ethereum
- **Patricia Merkle Trees**: State and storage
- **Keccak-256**: Hashing (SHA-3 family)
- **Mental poker**: Decentralized gaming (Virtue Poker)

### General Blockchain
- **Hash functions**: Block linking, content addressing
- **Merkle trees**: Efficient proofs, light clients
- **Randomness**: VRFs for consensus (Algorand, Cardano)

## Contributing

When adding new examples:
1. Place in appropriate subdirectory
2. Include comments explaining the code
3. Add security warnings if applicable
4. Update the subdirectory README
5. Add cross-references to related examples

## References

See the main chapter markdown files for academic references and further reading:
- `02_randomness.md` - Mersenne Twister, PRNGs
- `03_hash-functions.md` - SHA-256, collision resistance
- `05_merkle-trees.md` - Merkle tree theory and applications
- `06_poker-cryptography.md` - Mental poker, commitment schemes
- `07_signing.md` - Digital signatures, ECDSA, Bitcoin/Ethereum
- `08_encryption.md` - Symmetric and asymmetric encryption

## Summary Statistics

### By Language
- **Go**: 6 files (signing, poker, randomness examples)
- **Python**: 5 files (encryption, hashing, merkle trees, randomness)
- **JavaScript**: 5 files (encryption, hashing, merkle trees, signing)
- **Documentation**: 6 READMEs (2,896 words total)

### Coverage
- **Total Code Files**: 16 working examples
- **Total Lines of Documentation**: 843 lines
- **Total Words of Documentation**: 2,896 words
- **Average README Length**: 483 words
- **Topics Covered**: 30+ cryptographic concepts
- **Real-World Examples**: Bitcoin, Ethereum, PS3 hack, blockchain protocols

### Complexity Distribution
| Level | Directories | Description |
|-------|-------------|-------------|
| **Beginner** | randomness, hash-functions | Basic concepts, easy to understand |
| **Intermediate** | encryption, merkle-trees | Requires some crypto knowledge |
| **Advanced** | poker, signing | Complex protocols, security critical |

### Documentation Quality
✅ All directories have comprehensive READMEs
✅ Every README includes running instructions
✅ Security warnings where applicable
✅ Real-world use cases and examples
✅ Cross-references to related topics
✅ Academic references and further reading

## Questions or Issues?

Each example includes inline comments and documentation. If something is unclear:
1. Check the subdirectory README (2,896 words of documentation available)
2. Review the corresponding chapter markdown file
3. Look at related examples for context
4. Consult the references in the chapter

### Most Comprehensive Documentation
1. **signing/** (975 words) - Digital signatures, ECDSA, security
2. **merkle-trees/** (611 words) - Merkle proofs, blockchain applications
3. **poker/** (494 words) - Commitment schemes, mental poker
4. **hash-functions/** (318 words) - SHA-256 internals
5. **randomness/** (254 words) - PRNGs, avalanche effect
6. **encryption/** (244 words) - Symmetric ciphers
