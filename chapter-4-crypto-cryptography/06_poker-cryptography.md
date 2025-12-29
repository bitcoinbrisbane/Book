# Poker Cryptography

In this section, we explore how cryptographic primitives solve fundamental problems in trustless gaming. These same techniques are used in blockchain systems for fair randomness, verifiable computation, and trustless state transitions. While we use poker as our example, these concepts apply to any decentralized application requiring fairness without a trusted authority.

## The Coinflip Problem

Let's say you'd like to play a game of coin flip or rock-paper-scissors over the phone, or probably via text message these days. The game is pretty simple: each player chooses an outcome prior to performing the action, and the winner is determined once the coin lands or the hands are revealed.

This works in a live environment because both parties witness the event in real time, and should agree or form consensus on the winner. If one of the parties cannot witness the event in real time, it would be fairly easy to cheat. So how can we play this game fairly using cryptography?

This problem extends beyond games. In blockchain systems, similar challenges arise when validators must commit to values (like block proposals or vote choices) that shouldn't be revealed until all parties have committed. Bitcoin's transaction system uses commitment schemes through hash locks in HTLCs (Hash Time-Locked Contracts) for atomic swaps.

## Commitment Schemes

We can do this by using a one-way function, such as the SHA256 function we discussed in previous sections. Alice picks either heads or tails, called x. We add an arbitrary random number to the value, r.

```
f(x + r) = y
```

Alice can then send the result as a message to Bob. Bob has no way to know the value Alice chose but has a copy of the hash.

After Bob makes his choice, Alice reveals both her original choice (x) and the random number (r). Bob can then verify by computing the hash himself and comparing it to what Alice originally sent.

This scheme has two critical properties:

- **Hiding**: The commitment reveals nothing about the choice. Given only `hash(x + r)`, Bob cannot determine x, thanks to the pre-image resistance of SHA-256.
- **Binding**: Alice cannot change her choice after committing. Any different value of x will produce a completely different hash (due to the avalanche effect we discussed earlier).

The random number `r` (called a nonce or "blinding factor") is essential. Without it, Bob could simply hash all possible values ("heads" and "tails") and compare them to Alice's commitment, trivially breaking the scheme.

Here's a practical implementation of a commitment scheme for a coin flip game:

```go
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
```

## Mental Poker

Mental poker refers to a cryptographic problem that allows players to play a fair game of poker without requiring a trusted third party to deal the cards. The challenge is:

1. **No trusted dealer**: Players must be able to shuffle and deal cards without any single party knowing the full deck state
2. **Hidden information**: Each player should only see their own cards
3. **Verifiability**: At the end of the game, all actions should be verifiable as fair

This is a foundational concept that we'll build upon throughout this book as we implement our poker application.

### History and Real-World Use

The mental poker problem was first formally described by Shamir, Rivest, and Adleman in 1979 (the inventors of RSA encryption). Their solution used commutative encryption, where the order of applying different encryption keys doesn't matter.

Today, mental poker protocols are being implemented in:
- **Decentralized poker platforms** like Virtue Poker and CoinPoker
- **Blockchain gaming protocols** requiring provable fairness
- **State channels** for off-chain game states with on-chain dispute resolution
- **Zero-knowledge gaming** where game logic executes privately

### The Mental Poker Protocol

The classic mental poker protocol works as follows:

1. **Card Encoding**: Each card in the deck is assigned a unique number or identifier
2. **Encryption**: Players take turns encrypting the entire deck with their own secret keys
3. **Shuffling**: Each player shuffles the encrypted deck before passing it to the next player
4. **Dealing**: When a card needs to be dealt, players collaboratively decrypt only that specific card
5. **Reveal**: At showdown, players reveal their decryption keys to verify fair play

The security relies on the fact that:
- No single player can decrypt a card without cooperation from all other players
- The shuffling by multiple players ensures randomness
- All actions are recorded and can be verified later

```text
Mental Poker Flow:
┌─────────┐         ┌─────────┐         ┌─────────┐
│  Alice  │         │   Bob   │         │ Charlie │
└────┬────┘         └────┬────┘         └────┬────┘
     │                   │                    │
     │ 1. Encrypt deck   │                    │
     ├──────────────────>│                    │
     │                   │ 2. Encrypt deck    │
     │                   ├───────────────────>│
     │                   │                    │ 3. Encrypt deck
     │                   │                    ├──────────┐
     │                   │                    │          │
     │                   │ 4. Shuffle & pass  │<─────────┘
     │                   │<───────────────────┤
     │ 5. Shuffle & pass │                    │
     │<──────────────────┤                    │
     │                   │                    │
     │         6. Cards are now encrypted     │
     │            by all three players        │
     │                   │                    │
     │ 7. To reveal a card, all must decrypt │
     └───────────────────┴────────────────────┘
```

## Cryptographic Card Shuffling

The key insight is that we can use encryption to "lock" cards, and commutative encryption schemes allow multiple players to each add their own "lock" without revealing the underlying card values.

Here's a simplified implementation demonstrating the concept:

> **⚠️ Security Warning**: The code example below uses SHA-256 hashing for encryption, which is NOT a proper encryption scheme. This is purely educational. Real mental poker requires commutative encryption schemes like RSA or SRA where encryption order doesn't affect the result. SHA-256 is a one-way function and cannot be "decrypted" - it only demonstrates the concept of layered obfuscation.

```go
package main

import (
	"crypto/rand"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"math/big"
)

// Card represents a playing card
type Card struct {
	Value string // e.g., "Ace of Spades", "2 of Hearts"
	ID    int    // Unique identifier
}

// EncryptedCard represents an encrypted card
type EncryptedCard struct {
	ID        int
	Encrypted string
}

// Player represents a poker player with encryption capabilities
type Player struct {
	Name   string
	Secret string
}

// createDeck creates a standard deck of cards
func createDeck() []Card {
	suits := []string{"Hearts", "Diamonds", "Clubs", "Spades"}
	ranks := []string{"2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace"}

	deck := make([]Card, 0, 52)
	id := 0
	for _, suit := range suits {
		for _, rank := range ranks {
			deck = append(deck, Card{
				Value: rank + " of " + suit,
				ID:    id,
			})
			id++
		}
	}
	return deck
}

// encryptCard encrypts a card value with a player's secret
// In practice, this would use a proper commutative encryption scheme
func encryptCard(cardValue string, secret string) string {
	data := cardValue + secret
	hash := sha256.Sum256([]byte(data))
	return hex.EncodeToString(hash[:])
}

// mentalPokerShuffle demonstrates the mental poker shuffling protocol
func mentalPokerShuffle() {
	fmt.Println("Mental Poker Card Shuffling Protocol")
	fmt.Println("=====================================\n")

	// Initialize players
	players := []Player{
		{Name: "Alice", Secret: generateSecret()},
		{Name: "Bob", Secret: generateSecret()},
		{Name: "Charlie", Secret: generateSecret()},
	}

	fmt.Printf("Players: %d\n", len(players))
	for i, p := range players {
		fmt.Printf("  %d. %s (secret: %s...)\n", i+1, p.Name, p.Secret[:8])
	}
	fmt.Println()

	// Create the deck
	deck := createDeck()
	fmt.Printf("Created deck with %d cards\n\n", len(deck))

	// Each player encrypts and shuffles the deck
	encryptedDeck := make([]EncryptedCard, len(deck))
	for i, card := range deck {
		encryptedDeck[i] = EncryptedCard{
			ID:        card.ID,
			Encrypted: card.Value,
		}
	}

	for playerIdx, player := range players {
		fmt.Printf("Phase %d: %s encrypts and shuffles\n", playerIdx+1, player.Name)

		// Encrypt each card with this player's secret
		for i := range encryptedDeck {
			encryptedDeck[i].Encrypted = encryptCard(
				encryptedDeck[i].Encrypted,
				player.Secret,
			)
		}

		// Shuffle the deck (Fisher-Yates shuffle)
		shuffleDeck(encryptedDeck)

		fmt.Printf("  Deck encrypted and shuffled by %s\n", player.Name)
		fmt.Printf("  Sample encrypted card: %s...\n\n", encryptedDeck[0].Encrypted[:16])
	}

	fmt.Println("Final State:")
	fmt.Println("  All players have encrypted and shuffled the deck")
	fmt.Println("  No single player knows the order or can decrypt any card alone")
	fmt.Println("  Cards can only be revealed through collaborative decryption")
	fmt.Println("\nDemonstration of first 5 encrypted cards:")
	for i := 0; i < 5 && i < len(encryptedDeck); i++ {
		fmt.Printf("  Card %d: %s...\n", i+1, encryptedDeck[i].Encrypted[:32])
	}
}

// shuffleDeck performs Fisher-Yates shuffle
func shuffleDeck(deck []EncryptedCard) {
	for i := len(deck) - 1; i > 0; i-- {
		j, _ := rand.Int(rand.Reader, big.NewInt(int64(i+1)))
		deck[i], deck[j.Int64()] = deck[j.Int64()], deck[i]
	}
}

// generateSecret generates a random secret for a player
func generateSecret() string {
	bytes := make([]byte, 32)
	rand.Read(bytes)
	return hex.EncodeToString(bytes)
}

func main() {
	mentalPokerShuffle()

	fmt.Println("\n" + "=====================================")
	fmt.Println("Key Properties of Mental Poker")
	fmt.Println("=====================================\n")

	properties := []string{
		"No trusted third party required",
		"Each player contributes to randomness",
		"No single player can predict card order",
		"All actions are cryptographically verifiable",
		"Cheating can be detected after the game",
	}

	for i, prop := range properties {
		fmt.Printf("%d. %s\n", i+1, prop)
	}
}
```

### Practical Considerations

While the above example demonstrates the concept, real-world mental poker implementations require:

1. **Commutative Encryption**: The encryption must be commutative, meaning the order of encryption doesn't matter: `E_Alice(E_Bob(card)) = E_Bob(E_Alice(card))`

2. **Zero-Knowledge Proofs**: Players must prove they're following the protocol without revealing their secrets

3. **Secure Multi-Party Computation**: For collaborative card decryption without revealing individual keys

4. **Timeout Mechanisms**: To handle players who disconnect or refuse to decrypt

5. **Dispute Resolution**: A way to verify and resolve disputes about game fairness

These advanced topics will be covered in later chapters as we build our complete poker implementation.

### Why Mental Poker Matters for Blockchain

Mental poker protocols are fundamental to decentralized gaming because they solve the "trusted dealer" problem without requiring:
- Centralized servers that could be compromised
- Oracle services that could be bribed or fail
- On-chain computation that would be prohibitively expensive

By moving the cryptographic operations off-chain and only settling disputes on-chain, mental poker enables:
- **Gas efficiency**: Only final results or disputes touch the blockchain
- **Privacy**: Card values never appear on the public ledger
- **Scalability**: Hundreds of games can run simultaneously off-chain
- **Fairness**: Mathematical guarantees replace trust in operators

This pattern appears throughout decentralized applications: do the heavy computation off-chain using cryptography, and only use the blockchain as a dispute resolution layer.

## Connection to Bitcoin and Blockchain

The cryptographic techniques in mental poker directly relate to Bitcoin and blockchain systems:

### Commitment Schemes in Bitcoin
- **Hash Time-Locked Contracts (HTLCs)**: Used in Lightning Network for atomic swaps
- **Pay-to-Script-Hash (P2SH)**: Commits to a script without revealing it
- **Taproot**: Commits to multiple spending conditions, revealing only the used path

### Collaborative Protocols
- **Multi-signature transactions**: Like collaborative card decryption, require multiple parties
- **Threshold signatures**: N-of-M parties must collaborate to sign
- **Distributed key generation**: Creating shared secrets without a trusted dealer

### Verifiable Randomness
- **Block hashes**: Verifiable but predictable by miners
- **VRFs (Verifiable Random Functions)**: Provably random and verifiable (used in Cardano, Algorand)
- **Commit-reveal schemes**: Like our coin flip, used for leader election

The mental poker problem essentially asks: "How do we play a game without a dealer?" Bitcoin asks: "How do we have money without a bank?" The answer in both cases involves cryptography, consensus, and verification.

## References and Further Reading

### Academic Papers
- Shamir, A., Rivest, R., & Adleman, L. (1981). "Mental Poker". *The Mathematical Gardner*, pp. 37-43.
- Crépeau, C. (1986). "A Zero-Knowledge Poker Protocol that Achieves Confidentiality of the Players' Strategy or How to Achieve an Electronic Poker Face". *CRYPTO*.
- Barnett, A. & Smart, N. P. (2003). "Mental Poker Revisited". *Cryptography and Coding*.

### Practical Implementations
- **Poker.TH Protocol**: Open-source poker client with cryptographic shuffling
- **Mental Poker Toolkit**: Reference implementation in various languages
- **Ethereum Mental Poker**: Smart contract-based poker using commit-reveal

### Cryptographic Primitives
- **Commitment Schemes**: Pedersen commitments, hash-based commitments
- **Commutative Encryption**: RSA, SRA (Shamir-Rivest-Adleman), ElGamal variants
- **Zero-Knowledge Proofs**: zk-SNARKs, zk-STARKs, Bulletproofs

### Blockchain Gaming
- **Virtue Poker**: Decentralized poker using mental poker + Ethereum (I was on this team)
- **CoinPoker**: Cryptocurrency poker with random number generation
- **Funfair**: Blockchain casino using state channels and fate channels

### Related Topics in This Book
- Chapter 3: Hash functions and pre-image resistance
- Chapter 7: Consensus mechanisms and leader election
- Chapter 11: Zero-knowledge proofs
- Chapter 15: State channels and payment channels
