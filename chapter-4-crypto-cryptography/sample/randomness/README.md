# Randomness and Cryptography Examples

This directory contains code examples demonstrating concepts from Chapter 4: Crypto & Cryptography.

## Files

### Mersenne Twister Examples

- **mersenne-twister.go** - Go implementation demonstrating the Mersenne Twister PRNG
- **mersenne-twister.py** - Python implementation using the built-in random module

### Avalanche Effect Examples

- **avalanche-effect.go** - Go code demonstrating how small input changes create large hash differences
- **avalanche-effect.py** - Python implementation of the avalanche effect demonstration

## Running the Examples

### Go Examples

```bash
# Run Mersenne Twister example
go run mersenne-twister.go

# Run Avalanche Effect example
go run avalanche-effect.go
```

### Python Examples

```bash
# Run Mersenne Twister example
python3 mersenne-twister.py

# Run Avalanche Effect example
python3 avalanche-effect.py
```

## Key Concepts

### Mersenne Twister (MT19937)

- **Period**: 2^19937-1 (extremely long before repeating)
- **Use Case**: Simulations, games, general-purpose randomness
- **NOT for**: Cryptographic keys, passwords, security-sensitive applications
- **Secure Alternative**: Use `crypto/rand` (Go) or `secrets` module (Python)

### Avalanche Effect

- Small input changes (even 1 bit) cause ~50% of output bits to change
- Critical property for hash functions and block ciphers
- Ensures data integrity - tampering is immediately detectable
- Makes it impossible to predict hash values from similar inputs

## Security Warning

⚠️ **Important**: The Mersenne Twister examples are for educational purposes only. Never use `math/rand` (Go) or the `random` module (Python) for:

- Generating passwords
- Creating cryptographic keys
- Security tokens
- Any security-critical random values

Always use cryptographically secure alternatives:
- **Go**: `crypto/rand` package
- **Python**: `secrets` module
