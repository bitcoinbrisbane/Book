# Signing

Signing is a mathematical function that we use to verify that a message is written by a specific private key, just like putting your physical signature at the end of a letter, but with great certainty. The properties of the signing function are similar to the hash function we just learned about. Signing uses a hash function to make sure the length of the message is constant.

## Why Signing Matters

In our application, signing will be important as well as encryption for obfuscating data in the form of playing cards to the chain.

### Website Certificates

Websites sign the HTML of the website using SSL/TLS certificates, allowing browsers to verify the authenticity of the content.

## Key Pairs

A key pair is a pair of numbers that relate to one another. A private key is a secret that only the user should know. Depending on the algorithm, this will determine the size of the number. It's often written as a hexadecimal number (base 16) or a base 64 number. The reason for this is to make the representation more compact and readable.

It is important to note that messages sent over the wire will be encoded in an array of bytes.

## Choosing a Curve and Algorithm

What curve should we choose and what encryption algorithm should our project use? One of the layers of the application will include a staking mechanism, whereby master nodes deposit a significant amount of tokens into an Ethereum contract, as a bond that they behave fairly.

### Common Signature Algorithms

- **ECDSA (Elliptic Curve Digital Signature Algorithm)**: Used by Bitcoin and Ethereum
- **EdDSA (Edwards-curve Digital Signature Algorithm)**: A more modern approach with better performance
- **Schnorr Signatures**: Recently added to Bitcoin via the Taproot upgrade

## Signature Verification

The verification process allows anyone with the public key to confirm:

1. The message was signed by the holder of the corresponding private key
2. The message has not been altered since it was signed
