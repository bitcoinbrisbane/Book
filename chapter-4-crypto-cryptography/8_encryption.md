# Encryption

It may come as a bit of a surprise to some readers beginning their blockchain journey, that blockchains use little to no encryption. Some will say "encrypted data" when they mean hashed data.

Let's take a look at two types of encryption then explore some algorithms: Asymmetric and Symmetric.

## Symmetric Encryption

Symmetric encryption is a type of encryption where the same key is used for both encryption and decryption of data. This means that both the sender and the recipient must have access to the same secret key to encode and decode the information. They are generally faster algorithms than their asymmetric cousins.

This can be useful for encrypting a file on disk, where the user is the person that locks and unlocks the data, or between parties who have shared a secret key beforehand. This often happens when someone sends an encrypted file by email and they will say "I'll send you the password via Signal" or other end-to-end encrypted services. Good enough for most everyday communications.

### TripleDES

TripleDES and BlowFish are two such symmetric encryption algorithms. TripleDES involves three stages of encryption/decryption which can be described as follows:

1. Encrypt with the first key (K1)
2. Decrypt with the second key (K2)
3. Encrypt with the third key (K3)

Each stage operates on a block of plaintext or intermediate cipher text of size 64 bytes.

### NodeJS Example

Here is a NodeJS example using TripleDES to encrypt a file:

```javascript
const crypto = require("crypto");

// Define the key and IV (Initialization Vector)
const key = Buffer.from("0123456789abcdef01234567", "utf8"); // 24 bytes key for 3DES
const iv = Buffer.from("12345678", "utf8"); // 8 bytes IV for DES

// Function to encrypt plaintext using 3DES
function encrypt(text) {
  const cipher = crypto.createCipheriv("des-ede3-cbc", key, iv);
  let encrypted = cipher.update(text, "utf8", "base64");
  encrypted += cipher.final("base64");
  return encrypted;
}

// Function to decrypt ciphertext using 3DES
function decrypt(encryptedText) {
  const decipher = crypto.createDecipheriv("des-ede3-cbc", key, iv);
  let decrypted = decipher.update(encryptedText, "base64", "utf8");
  decrypted += decipher.final("utf8");
  return decrypted;
}

// Test the encryption and decryption
const plaintext = "Hello, this is a plaintext message!";
console.log("Plaintext:", plaintext);

const encryptedText = encrypt(plaintext);
console.log("Encrypted:", encryptedText);

const decryptedText = decrypt(encryptedText);
console.log("Decrypted:", decryptedText);
```

We can see on line 2 we set the key to a 24 byte array, as that is the key size required for TripleDES. As the functions are deterministic, you should see the outputs as:

```
Plaintext: Hello, this is a plaintext message!
Encrypted: HYtEf/sQLP/Sybpf7b+Yql5zQajIKC0oAbN1iWVUykQ1EwbAGZHl8A==
Decrypted: Hello, this is a plaintext message!
```

### Python Example

Let's do the same code snippet using Python, so we're sure the code is language agnostic:

```bash
touch tripledes.py
pip install pycryptodome
python tripledes.py
```

The outputs should match:

```
Plaintext: Hello, this is a plaintext message!
Encrypted: HYtEf/sQLP/Sybpf7b+Yql5zQajIKC0oAbN1iWVUykQ1EwbAGZHl8A==
Decrypted: Hello, this is a plaintext message!
```

We can now be sure if we send the encrypted text `HYtEf/sQLP/Sybpf7b+Yql5zQajIKC0oAbN1iWVUykQ1EwbAGZHl8A==` that we encrypted in NodeJS to our friend who then uses Python to decrypt, that they will receive the correct message. The authors of the libraries we have used have implemented the algorithm correctly, or both incorrectly which is unlikely.

## Asymmetric Encryption

In our GPG example in Chapter 1, we encrypted a message to myself using my PGP public key. This is an example of Asymmetric Encryption!

The advantage of using an Asymmetric schema is that the parties like yourself and myself never have to meet to send a secure communication. It is considered safe that I can publish my public key to the world.

### How It Works

1. **Key Generation**: Generate a public/private key pair
2. **Encryption**: Anyone can encrypt a message using your public key
3. **Decryption**: Only you can decrypt the message using your private key

This solves the key distribution problem that symmetric encryption has - you never need to securely share a secret key beforehand.
