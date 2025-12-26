# Private and Public Keys

Blockchains use a cryptography method called public key, private key cryptography to mathematically prove that the user or machine interfacing with the blockchain is who they say they are.

A user has a private key, which is a large number, represented as a hexadecimal number, base 64 string, or sometimes as a decimal. Because of their size, by using a larger base or radix, they're easier to store in our application. Also, we will define memory on a chain as 32 byte words, so a 256 bit number conveniently fits within that memory allocation.

## Schnorr Signatures

Schnorr signatures are now implemented in the bitcoin protocol as of the Taproot upgrade. A relatively new algorithm EdDSA, known as Edwards-curve Digital Signature Algorithm, is based on the implementation of Schnorr signatures.

## Cryptographic Algorithms

Now let's discuss some crypto algorithms!

### RSA

RSA (Rivest-Shamir-Adleman) was one of the first public-key cryptosystems and is widely used for secure data transmission. It was invented in 1977 by Ron Rivest, Adi Shamir, and Leonard Adleman from MIT.

### Other Encryption Algorithms

There are many algorithms to encrypt data:

- **bcrypt** - A password hashing function
- **Triple DES** - A symmetric-key block cipher
- **PGP/GPG** - Pretty Good Privacy for encryption and signing

## Asymmetric vs Symmetric

The key distinction in cryptography is between:

- **Symmetric encryption**: Uses the same key for encryption and decryption
- **Asymmetric encryption**: Uses a key pair (public and private keys)

We'll explore both types in detail in the encryption section.
