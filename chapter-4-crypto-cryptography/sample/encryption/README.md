# Encryption Examples

This directory contains examples of various encryption algorithms and techniques.

## Files

### RC4 Stream Cipher

- **rc4.js** - JavaScript implementation of the RC4 stream cipher
  - Demonstrates symmetric encryption
  - Shows pseudocode from the book in working code
  - Historical cipher (now considered insecure)

### Triple DES (3DES)

- **tripledes.js** - JavaScript implementation of Triple DES
- **tripledes.py** - Python implementation of Triple DES
  - Block cipher encryption
  - Uses CBC mode with IV (Initialization Vector)
  - Shows padding for block alignment

## Running the Examples

### JavaScript Examples

```bash
# RC4 cipher
node rc4.js

# Triple DES
node tripledes.js
```

### Python Examples

```bash
# Triple DES (requires pycryptodome)
pip install pycryptodome
python3 tripledes.py
```

## Security Notes

⚠️ **Important**: These implementations are for educational purposes only.

- **RC4**: Deprecated and insecure. Do not use in production.
- **3DES**: Legacy algorithm, being phased out. Use AES-256 instead.

For production use:
- **JavaScript**: Use Web Crypto API or libsodium.js
- **Python**: Use `cryptography` library with AES-GCM or ChaCha20-Poly1305

## Key Concepts

### Symmetric Encryption
- Same key for encryption and decryption
- Fast and efficient for large data
- Key distribution is the main challenge

### Block vs Stream Ciphers
- **Block ciphers** (3DES, AES): Encrypt fixed-size blocks
- **Stream ciphers** (RC4, ChaCha20): Encrypt byte-by-byte

### Modes of Operation
- **CBC** (Cipher Block Chaining): Each block depends on previous block
- **GCM** (Galois/Counter Mode): Provides both encryption and authentication
