# Randomness

As you're probably aware, computers are deterministic; given a set of inputs they should always return the same outputs. If they didn't, it would be pretty chaotic using a computer! Random Number Generators or RNGs are a way to introduce unpredictability. There are USB hardware devices like the TrueRNG by ubld.it that create randomness for your device or server for around $100 USD.

Computers and software however, use Pseudo Random Number Generators to create a random output. However, they rely on a "seed" input, often the computer's timestamp. When creating passwords or perhaps more importantly crypto private keys, using a random number is critical. There is a good YouTube video by the former hacker "Kingpin" where he cracks a password on a bitcoin wallet to recover a few million dollars (at time of writing) worth of bitcoin. The software used to generate the password took the computer's time as the seed to generate the password. The attackers took a range of dates where they thought the original password was generated and replayed all the milliseconds in that range. The attack was successful and the coins were recovered!

## RC4

```text
i = (i + 1) mod 256
j = (j + Si) mod 256
swap Si and Sj
t = (Si + Sj) mod 256
K = St
```

Ciphertext (in base64): `wqLCthTDlMKvZR7Dh8Kvf1JZw5s=`
Decrypted text: `Hello, World!`

## What is Entropy?

Let's explore some Go code to explain the topic further, and how our validators will use a random seed to create a random number.

[TODO: GO CODE]

Online randomness as a service is also available for applications that require verifiable randomness.

## The Avalanche Effect

The avalanche effect in cryptography refers to a desirable property of cryptographic algorithms, particularly block ciphers and hash functions. This property ensures that a small change in the input (such as flipping a single bit) results in a significant and unpredictable change in the output.

In functions like the hash function, a small change in the input message, even one bit, should result in a hashed value that looks completely different from the original hash. This property is critical for ensuring data integrity and security.
