package main

import (
	"crypto/rand"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"math/big"
)

// Commitment represents a cryptographic commitment
type Commitment struct {
	Hash  string
	Value string
	Nonce string
}

// createCommitment creates a commitment for a given choice
func createCommitment(choice string) (*Commitment, error) {
	// Generate a random nonce (128 bits)
	nonce, err := rand.Int(rand.Reader, new(big.Int).Lsh(big.NewInt(1), 128))
	if err != nil {
		return nil, err
	}
	nonceStr := nonce.String()

	// Create commitment: hash(choice || nonce)
	data := choice + nonceStr
	hash := sha256.Sum256([]byte(data))
	hashStr := hex.EncodeToString(hash[:])

	return &Commitment{
		Hash:  hashStr,
		Value: choice,
		Nonce: nonceStr,
	}, nil
}

// verifyCommitment verifies that a revealed choice matches the commitment
func verifyCommitment(hash, choice, nonce string) bool {
	data := choice + nonce
	computedHash := sha256.Sum256([]byte(data))
	computedHashStr := hex.EncodeToString(computedHash[:])
	return computedHashStr == hash
}

func main() {
	fmt.Println("Cryptographic Commitment Scheme - Coin Flip Game")
	fmt.Println("=================================================\n")

	// Alice's turn - she commits to her choice
	fmt.Println("Phase 1: Alice commits to her choice")
	aliceChoice := "heads"
	commitment, err := createCommitment(aliceChoice)
	if err != nil {
		panic(err)
	}

	fmt.Printf("Alice chooses: %s (hidden)\n", aliceChoice)
	fmt.Printf("Alice's commitment (sent to Bob): %s\n\n", commitment.Hash)

	// Bob makes his choice after seeing the commitment
	fmt.Println("Phase 2: Bob makes his choice")
	bobChoice := "tails"
	fmt.Printf("Bob chooses: %s\n\n", bobChoice)

	// Alice reveals her choice and nonce
	fmt.Println("Phase 3: Alice reveals her choice")
	fmt.Printf("Alice reveals: %s\n", commitment.Value)
	fmt.Printf("Alice's nonce: %s\n\n", commitment.Nonce)

	// Bob verifies Alice's commitment
	fmt.Println("Phase 4: Bob verifies Alice didn't cheat")
	isValid := verifyCommitment(commitment.Hash, commitment.Value, commitment.Nonce)
	fmt.Printf("Commitment valid: %v\n\n", isValid)

	// Determine winner
	fmt.Println("Result:")
	if aliceChoice == bobChoice {
		fmt.Println("It's a tie!")
	} else {
		fmt.Printf("Alice chose %s, Bob chose %s\n", aliceChoice, bobChoice)
		fmt.Println("Winner determined by game rules!")
	}

	// Demonstrate cheating attempt
	fmt.Println("\n" + "=================================================")
	fmt.Println("Demonstrating cheating prevention")
	fmt.Println("=================================================\n")

	fmt.Println("What if Alice tries to cheat by changing her choice?")
	fakeChoice := "tails" // Alice tries to change to match Bob
	isCheating := verifyCommitment(commitment.Hash, fakeChoice, commitment.Nonce)
	fmt.Printf("Trying to verify with '%s': %v\n", fakeChoice, isCheating)
	fmt.Println("The commitment scheme prevents Alice from changing her choice!")
}
