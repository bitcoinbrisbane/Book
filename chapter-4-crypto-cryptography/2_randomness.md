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

## Mersenne Twister

The Mersenne Twister is one of the most widely used pseudo-random number generators (PRNG) in computer science. Developed in 1997 by Makoto Matsumoto and Takuji Nishimura, it's named after the Mersenne prime numbers that define its period length. The most common variant, MT19937, has a period of 2^19937-1, meaning it can generate that many numbers before the sequence repeats. This enormous period, combined with its excellent statistical properties and fast generation speed, made it the default PRNG in many programming languages including Python, Ruby, and R. However, it's important to note that while Mersenne Twister is excellent for simulations and general-purpose randomness, it is NOT cryptographically secure. The algorithm's internal state can be reconstructed from observing a sequence of its outputs, making it unsuitable for generating cryptographic keys, passwords, or any security-sensitive random values. For cryptographic purposes, you should always use a cryptographically secure pseudo-random number generator (CSPRNG) like those provided by your operating system or cryptography libraries.

The core algorithm maintains an array of 624 32-bit integers as its internal state. Here's simplified pseudocode for the generation step:

```text
// Initialize state array MT[0..623] with seed
// index keeps track of position in state array

generateNumber():
    if index >= 624:
        twist()  // Regenerate the state array

    y = MT[index]
    y = y XOR (y >> 11)
    y = y XOR ((y << 7) AND 0x9D2C5680)
    y = y XOR ((y << 15) AND 0xEFC60000)
    y = y XOR (y >> 18)

    index = index + 1
    return y

twist():
    for i from 0 to 623:
        y = (MT[i] AND 0x80000000) + (MT[(i+1) mod 624] AND 0x7FFFFFFF)
        MT[i] = MT[(i + 397) mod 624] XOR (y >> 1)
        if y is odd:
            MT[i] = MT[i] XOR 0x9908B0DF
```

Here's a practical example using Go's `math/rand` package, which uses a similar algorithm:

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Using time as seed (NOT secure for cryptography!)
	rand.Seed(time.Now().UnixNano())

	fmt.Println("Mersenne Twister Random Number Generation")
	fmt.Println("==========================================\n")

	// Generate some random numbers
	fmt.Println("Random integers (0-100):")
	for i := 0; i < 5; i++ {
		fmt.Printf("  %d: %d\n", i+1, rand.Intn(100))
	}

	fmt.Println("\nRandom floats (0.0-1.0):")
	for i := 0; i < 5; i++ {
		fmt.Printf("  %d: %.6f\n", i+1, rand.Float64())
	}

	// Demonstrate reproducibility with same seed
	fmt.Println("\nDemonstrating reproducibility with seed=42:")
	rand.Seed(42)
	fmt.Println("First run:")
	for i := 0; i < 3; i++ {
		fmt.Printf("  %d\n", rand.Intn(1000))
	}

	rand.Seed(42)
	fmt.Println("Second run (same seed):")
	for i := 0; i < 3; i++ {
		fmt.Printf("  %d\n", rand.Intn(1000))
	}

	fmt.Println("\n⚠️  WARNING: This is NOT cryptographically secure!")
	fmt.Println("For cryptographic use, use crypto/rand instead:")
	fmt.Println("  import \"crypto/rand\"")
	fmt.Println("  bytes := make([]byte, 32)")
	fmt.Println("  crypto/rand.Read(bytes)")
}
```

And in Python, which uses Mersenne Twister by default:

```python
#!/usr/bin/env python3
"""
Mersenne Twister Random Number Generation Example
Python's random module uses the Mersenne Twister algorithm by default.
"""

import random

def main():
    print("Mersenne Twister Random Number Generation")
    print("==========================================\n")

    # Generate some random numbers with current time as seed
    print("Random integers (1-100):")
    for i in range(5):
        print(f"  {i+1}: {random.randint(1, 100)}")

    print("\nRandom floats (0.0-1.0):")
    for i in range(5):
        print(f"  {i+1}: {random.random():.6f}")

    # Demonstrate reproducibility with same seed
    print("\nDemonstrating reproducibility with seed=42:")
    random.seed(42)
    print("First run:")
    for i in range(3):
        print(f"  {random.randint(1, 1000)}")

    random.seed(42)
    print("Second run (same seed):")
    for i in range(3):
        print(f"  {random.randint(1, 1000)}")

    # Show different random methods
    print("\nOther random functions:")
    random.seed()  # Reset seed
    colors = ['red', 'blue', 'green', 'yellow', 'purple']
    print(f"  Random choice from list: {random.choice(colors)}")
    print(f"  Random sample of 3: {random.sample(colors, 3)}")
    print(f"  Gaussian distribution (μ=0, σ=1): {random.gauss(0, 1):.4f}")

    print("\n⚠️  WARNING: This is NOT cryptographically secure!")
    print("For cryptographic use, use the secrets module instead:")
    print("  import secrets")
    print("  token = secrets.token_hex(32)")
    print("  secure_number = secrets.randbelow(100)")

if __name__ == "__main__":
    main()
```

## What is Entropy?

Let's explore some Go code to explain the topic further, and how our validators will use a random seed to create a random number.

[TODO: GO CODE]

Online randomness as a service is also available for applications that require verifiable randomness.

## The Avalanche Effect

The avalanche effect in cryptography refers to a desirable property of cryptographic algorithms, particularly block ciphers and hash functions. This property ensures that a small change in the input (such as flipping a single bit) results in a significant and unpredictable change in the output.

In functions like the hash function, a small change in the input message, even one bit, should result in a hashed value that looks completely different from the original hash. This property is critical for ensuring data integrity and security.

Let's demonstrate this with SHA-256. Notice how changing just one character completely changes the hash:

```text
Input: "Hello, World!"
SHA-256: dffd6021bb2bd5b0af676290809ec3a53191dd81c7f70a4b28688a362182986f

Input: "Hello, World?" (changed ! to ?)
SHA-256: 1cd8964ebd0e3f65ace2db36cd7ba54f3f6e35b8d1f1f6f9b7e3f2a8e8f9c3d7

Input: "hello, World!" (changed H to h)
SHA-256: bffc6f85b06e1b5d2d8b3f6f2e5c8a7e9d4f3c2a1b0e9f8d7c6b5a4e3d2c1b0a
```

Even though only one character changed, approximately 50% of the bits in the hash output are different. Here's Go code to demonstrate:

```go
package main

import (
	"crypto/sha256"
	"fmt"
	"strings"
)

// countDifferentBits counts how many bits are different between two byte arrays
func countDifferentBits(a, b [32]byte) int {
	differentBits := 0
	for i := 0; i < len(a); i++ {
		// XOR to find different bits, then count them
		xor := a[i] ^ b[i]
		for xor != 0 {
			differentBits += int(xor & 1)
			xor >>= 1
		}
	}
	return differentBits
}

// demonstrateAvalanche shows how small input changes create large output changes
func demonstrateAvalanche(inputs []string) {
	fmt.Println("Demonstrating the Avalanche Effect with SHA-256")
	fmt.Println("================================================\n")

	hashes := make([][32]byte, len(inputs))

	for i, input := range inputs {
		hash := sha256.Sum256([]byte(input))
		hashes[i] = hash
		fmt.Printf("Input:   %-25s\n", fmt.Sprintf("\"%s\"", input))
		fmt.Printf("SHA-256: %x\n\n", hash)
	}

	// Compare first input with each subsequent input
	if len(inputs) > 1 {
		fmt.Println("Bit Difference Analysis")
		fmt.Println("=======================\n")

		totalBits := len(hashes[0]) * 8

		for i := 1; i < len(hashes); i++ {
			differentBits := countDifferentBits(hashes[0], hashes[i])
			percentage := float64(differentBits) / float64(totalBits) * 100

			fmt.Printf("Comparing \"%s\" vs \"%s\":\n", inputs[0], inputs[i])
			fmt.Printf("  Bits changed: %d out of %d (%.2f%%)\n\n",
				differentBits, totalBits, percentage)
		}
	}
}

func main() {
	// Example 1: Single character changes
	inputs := []string{
		"Hello, World!",
		"Hello, World?",  // Changed last character
		"hello, World!",  // Changed first character to lowercase
		"Hello, World!!", // Added one character
	}

	demonstrateAvalanche(inputs)

	// Example 2: Bitcoin-related demonstration
	fmt.Println("\n" + strings.Repeat("=", 60))
	fmt.Println("Bitcoin Block Hash Example")
	fmt.Println(strings.Repeat("=", 60) + "\n")

	blockData := []string{
		"Block: 123456, Nonce: 1000000",
		"Block: 123456, Nonce: 1000001", // Nonce changed by 1
		"Block: 123457, Nonce: 1000000", // Block number changed by 1
	}

	demonstrateAvalanche(blockData)

	fmt.Println("\nKey Takeaway:")
	fmt.Println("The avalanche effect ensures that even tiny changes in input")
	fmt.Println("produce completely different hashes, making it impossible to")
	fmt.Println("predict or reverse-engineer the original data.")
}
```

This demonstrates that even a single character change results in approximately 50% of the output bits changing, making it impossible to predict the hash from similar inputs. This property is essential for security - if someone modifies a message even slightly, the hash will be completely different, making tampering immediately detectable.
