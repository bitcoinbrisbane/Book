# Base64, Hex and Other Encodings

When working with cryptographic data, we often need to represent binary data as text. Different encoding schemes serve different purposes.

## Hexadecimal (Base16)

Hexadecimal uses 16 characters: `0-9` and `a-f`. Each hex character represents 4 bits (a nibble), so two hex characters represent one byte.

**Example:**
```
Binary: 11111111
Hex: ff
Decimal: 255
```

Hex is commonly used for:
- Hash outputs
- Ethereum addresses
- Private/public keys

## Base64

Base64 uses 64 characters: `A-Z`, `a-z`, `0-9`, `+`, and `/`. It's more space-efficient than hex for representing binary data as text.

**Example:**
```
"Hello" in Base64: SGVsbG8=
```

Base64 is commonly used for:
- Email attachments
- Data URLs
- JWT tokens

## Base58

Base58 is similar to Base64 but removes characters that can be confusing:
- Removes `0` (zero) and `O` (capital o)
- Removes `I` (capital i) and `l` (lowercase L)
- Removes `+` and `/`

This makes it ideal for:
- **Bitcoin addresses** - Less prone to transcription errors
- **IPFS hashes**

The Base58 alphabet is:
```
123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz
```

## Comparison

| Encoding | Characters | Efficiency | Use Case |
|----------|------------|------------|----------|
| Hex | 16 | Low | Debug, hash display |
| Base64 | 64 | High | Data transfer |
| Base58 | 58 | Medium | Human-readable addresses |

## Why Different Encodings Matter

Understanding encodings is crucial because:

1. **Interoperability**: Different systems expect different formats
2. **Storage efficiency**: Base64 is ~33% smaller than hex
3. **Human readability**: Base58 reduces errors in manual entry
4. **Protocol requirements**: Some protocols mandate specific encodings
