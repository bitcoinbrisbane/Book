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
