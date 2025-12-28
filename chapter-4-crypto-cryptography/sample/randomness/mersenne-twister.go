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
