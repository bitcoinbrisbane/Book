package main

import (
	"crypto/sha256"
	"fmt"
	"math/big"
)

// This demonstrates why nonce reuse is catastrophic in ECDSA
// WARNING: This is educational code showing an attack, not for production use

func main() {
	fmt.Println("ECDSA Nonce Reuse Attack Demonstration")
	fmt.Println("========================================\n")

	// Simulated scenario: Attacker observes two signatures
	// Both were created with the SAME nonce (k) but different messages

	// Setup (values chosen for demonstration)
	message1 := "Transfer 1 BTC to Bob"
	message2 := "Transfer 100 BTC to Eve"

	// Hash the messages
	hash1 := sha256.Sum256([]byte(message1))
	hash2 := sha256.Sum256([]byte(message2))

	// Convert to big integers
	z1 := new(big.Int).SetBytes(hash1[:])
	z2 := new(big.Int).SetBytes(hash2[:])

	// Simulate the private key (in reality, this is secret)
	privateKey := new(big.Int)
	privateKey.SetString("12345678901234567890", 10) // Example private key

	// Simulate signing with SAME nonce (THIS IS THE VULNERABILITY)
	k := big.NewInt(9876543210) // Reused nonce - THIS IS BAD!
	n := new(big.Int)
	n.SetString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141", 16) // secp256k1 order

	// Calculate r (same for both signatures since k is reused)
	r := big.NewInt(1234567890) // Simplified: normally r = (k*G).x

	// Calculate s1 and s2
	// s = k^-1 * (z + r*privateKey) mod n
	kInv := new(big.Int).ModInverse(k, n)

	temp1 := new(big.Int).Mul(r, privateKey)
	temp1.Add(temp1, z1)
	s1 := new(big.Int).Mul(kInv, temp1)
	s1.Mod(s1, n)

	temp2 := new(big.Int).Mul(r, privateKey)
	temp2.Add(temp2, z2)
	s2 := new(big.Int).Mul(kInv, temp2)
	s2.Mod(s2, n)

	fmt.Println("Victim Signs Two Messages (BAD: Reusing Nonce)")
	fmt.Println("------------------------------------------------")
	fmt.Printf("Message 1: %s\n", message1)
	fmt.Printf("Message 2: %s\n", message2)
	fmt.Printf("\nSignature 1: (r=%s, s1=%s)\n", r.String(), s1.String())
	fmt.Printf("Signature 2: (r=%s, s2=%s)\n\n", r.String(), s2.String())
	fmt.Println("Notice: Both signatures have the SAME r value!")
	fmt.Println("This reveals that the same nonce was used.\n")

	// ATTACK: Recover the private key
	fmt.Println("Attacker Recovers Private Key")
	fmt.Println("------------------------------")

	// privateKey = (s1*z2 - s2*z1) / (s1 - s2) * r^-1 mod n
	// Or equivalently: privateKey = (s1 - s2)^-1 * (z1 - z2) * r^-1 mod n

	sDiff := new(big.Int).Sub(s1, s2)
	zDiff := new(big.Int).Sub(z1, z2)
	rInv := new(big.Int).ModInverse(r, n)
	sDiffInv := new(big.Int).ModInverse(sDiff, n)

	recoveredKey := new(big.Int).Mul(sDiffInv, zDiff)
	recoveredKey.Mul(recoveredKey, rInv)
	recoveredKey.Mod(recoveredKey, n)

	fmt.Printf("Original private key:  %s\n", privateKey.String())
	fmt.Printf("Recovered private key: %s\n\n", recoveredKey.String())

	if privateKey.Cmp(recoveredKey) == 0 {
		fmt.Println("✓ ATTACK SUCCESSFUL!")
		fmt.Println("  The attacker recovered the private key from two signatures!")
		fmt.Println("  They can now sign any message and steal funds.\n")
	}

	fmt.Println("Key Takeaway")
	fmt.Println("------------")
	fmt.Println("NEVER reuse nonces when signing!")
	fmt.Println("- Each signature must use a unique, random nonce")
	fmt.Println("- Use crypto/rand, not math/rand")
	fmt.Println("- Better yet: use deterministic nonces (RFC 6979)")
	fmt.Println("\nReal-world victims of nonce reuse:")
	fmt.Println("- PlayStation 3 (2010): Console security broken")
	fmt.Println("- Blockchain.info wallet (2013): Bitcoin stolen")
	fmt.Println("- Various cryptocurrency wallets: Millions lost")
}
