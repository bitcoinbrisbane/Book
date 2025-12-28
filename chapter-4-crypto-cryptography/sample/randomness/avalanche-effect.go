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
