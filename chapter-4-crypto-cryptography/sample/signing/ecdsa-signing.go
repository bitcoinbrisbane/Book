package main

import (
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/sha256"
	"fmt"
)

func main() {
	fmt.Println("Digital Signature Example (ECDSA)")
	fmt.Println("=================================\n")

	// 1. Generate key pair
	privateKey, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		panic(err)
	}
	publicKey := &privateKey.PublicKey

	fmt.Println("Step 1: Key Generation")
	fmt.Printf("Private key: %x\n", privateKey.D.Bytes()[:8]) // Show only first 8 bytes
	fmt.Printf("Public key X: %x\n", publicKey.X.Bytes()[:8])
	fmt.Printf("Public key Y: %x\n\n", publicKey.Y.Bytes()[:8])

	// 2. Create a message and sign it
	message := "I authorize payment of 10 BTC to Alice"
	messageHash := sha256.Sum256([]byte(message))

	fmt.Println("Step 2: Signing")
	fmt.Printf("Message: %s\n", message)
	fmt.Printf("Message hash: %x\n", messageHash)

	r, s, err := ecdsa.Sign(rand.Reader, privateKey, messageHash[:])
	if err != nil {
		panic(err)
	}

	fmt.Printf("Signature (r): %x\n", r.Bytes()[:16]) // First 16 bytes
	fmt.Printf("Signature (s): %x\n\n", s.Bytes()[:16])

	// 3. Verify the signature
	fmt.Println("Step 3: Verification")
	valid := ecdsa.Verify(publicKey, messageHash[:], r, s)
	fmt.Printf("Signature valid: %v\n\n", valid)

	// 4. Demonstrate tampering detection
	fmt.Println("Step 4: Tampering Detection")
	tamperedMessage := "I authorize payment of 100 BTC to Alice"
	tamperedHash := sha256.Sum256([]byte(tamperedMessage))

	tamperedValid := ecdsa.Verify(publicKey, tamperedHash[:], r, s)
	fmt.Printf("Tampered message: %s\n", tamperedMessage)
	fmt.Printf("Signature valid for tampered message: %v\n", tamperedValid)
	fmt.Println("✓ Tampering detected! Signature verification failed.")

	// 5. Demonstrate wrong public key
	fmt.Println("\nStep 5: Wrong Public Key Detection")
	wrongPrivateKey, _ := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	wrongPublicKey := &wrongPrivateKey.PublicKey

	wrongKeyValid := ecdsa.Verify(wrongPublicKey, messageHash[:], r, s)
	fmt.Printf("Signature valid with wrong public key: %v\n", wrongKeyValid)
	fmt.Println("✓ Wrong signer detected! Signature verification failed.")
}
