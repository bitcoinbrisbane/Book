# Chapter 4: Crypto & Cryptography - Code Examples

This directory contains all code examples for Chapter 4, organized by topic.

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
└── randomness/         # PRNGs and randomness demonstrations
    ├── avalanche-effect.go
    ├── avalanche-effect.py
    ├── mersenne-twister.go
    └── mersenne-twister.py
```

## Quick Start

Each subdirectory contains a README with:
- Explanation of the concepts
- Instructions for running the code
- Security considerations
- Real-world applications

### Running Go Examples

```bash
cd poker
go run commitment-scheme.go

cd ../randomness
go run mersenne-twister.go
go run avalanche-effect.go
```

### Running Python Examples

```bash
cd randomness
python3 mersenne-twister.py
python3 avalanche-effect.py

cd ../merkle-trees
python3 merkle_tree.py

cd ../encryption
pip install pycryptodome  # Only needed for tripledes.py
python3 tripledes.py
```

### Running JavaScript Examples

```bash
cd encryption
node rc4.js
node tripledes.js

cd ../hash-functions
node sha256.js

cd ../merkle-trees
node merkle-whitelist.js
```

## Topics Covered

### 🔐 Encryption (`encryption/`)
- **RC4**: Stream cipher (deprecated, educational only)
- **3DES**: Block cipher (legacy, use AES instead)
- Understanding symmetric encryption

### #️⃣ Hash Functions (`hash-functions/`)
- **SHA-256**: Cryptographic hash implementation
- Internal functions (Ch, Ma, Sigma)
- Pre-image resistance and collision resistance

### 🌳 Merkle Trees (`merkle-trees/`)
- Building Merkle trees from data
- Generating and verifying proofs
- Whitelist applications
- Blockchain use cases (Bitcoin SPV, Ethereum state)

### 🎲 Randomness (`randomness/`)
- **Mersenne Twister**: PRNG algorithm (not cryptographically secure)
- **Avalanche Effect**: How small input changes create large output changes
- Cryptographically secure alternatives

### 🃏 Poker Cryptography (`poker/`)
- **Commitment Schemes**: Prevent cheating in remote games
- **Mental Poker**: Trustless card dealing without a dealer
- Connection to blockchain (HTLCs, multi-sig)

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
- `2_randomness.md` - Mersenne Twister, PRNGs
- `3_hash-functions.md` - SHA-256, collision resistance
- `5_merkle-trees.md` - Merkle tree theory and applications
- `6_poker-cryptography.md` - Mental poker, commitment schemes
- `8_encryption.md` - Symmetric and asymmetric encryption

## Questions or Issues?

Each example includes inline comments and documentation. If something is unclear:
1. Check the subdirectory README
2. Review the corresponding chapter markdown file
3. Look at related examples for context
4. Consult the references in the chapter
