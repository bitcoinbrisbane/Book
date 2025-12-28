# Poker Cryptography Examples

This directory contains code examples demonstrating cryptographic concepts used in secure poker games from Chapter 4: Crypto & Cryptography.

## Files

### Commitment Scheme

- **commitment-scheme.go** - Demonstrates how cryptographic commitments prevent cheating in games like coin flip

### Mental Poker Protocol

- **mental-poker.go** - Shows how multiple players can shuffle and encrypt a deck without a trusted dealer

## Running the Examples

### Go Examples

```bash
# Run commitment scheme example (coin flip)
go run commitment-scheme.go

# Run mental poker shuffling example
go run mental-poker.go
```

## Key Concepts

### Commitment Scheme

The commitment scheme solves the problem of playing fair games remotely where players make choices that must be hidden until revealed:

1. **Commit Phase**: Alice chooses a value and creates `hash(choice + nonce)`, sends hash to Bob
2. **Choice Phase**: Bob makes his choice after seeing the commitment (but not the value)
3. **Reveal Phase**: Alice reveals both her choice and the nonce
4. **Verify Phase**: Bob verifies `hash(revealed_choice + revealed_nonce)` matches the original commitment

**Properties**:
- **Binding**: Alice cannot change her choice after committing
- **Hiding**: Bob cannot determine Alice's choice from the commitment
- **Verifiable**: Bob can verify Alice didn't cheat

### Mental Poker

Mental poker enables players to play poker without a trusted dealer:

1. **Card Encoding**: Each card gets a unique identifier
2. **Sequential Encryption**: Each player encrypts the entire deck with their secret key
3. **Sequential Shuffling**: Each player shuffles after encrypting
4. **Collaborative Decryption**: Cards are revealed only through cooperation of all players
5. **Verification**: All actions can be verified after the game

**Properties**:
- No single player knows the deck order
- No trusted third party needed
- Each player contributes to randomness
- Cheating can be detected
- All actions are cryptographically verifiable

## Security Considerations

### Commitment Scheme

⚠️ **Important**: The nonce must be:
- Cryptographically random (use `crypto/rand`)
- Long enough to prevent brute force (128+ bits)
- Unique for each commitment

### Mental Poker

⚠️ **Note**: This implementation is simplified for educational purposes. A production implementation requires:

1. **Commutative Encryption**: The order of encryption shouldn't matter
   - `Encrypt_Alice(Encrypt_Bob(card)) = Encrypt_Bob(Encrypt_Alice(card))`
   - Example: RSA with same modulus, SRA (Shamir-Rivest-Adleman)

2. **Zero-Knowledge Proofs**: Prove you're following the protocol without revealing secrets

3. **Secure Multi-Party Computation**: For collaborative decryption

4. **Timeout Handling**: Deal with disconnected or malicious players

5. **Dispute Resolution**: Mechanism to verify and resolve fairness disputes

## Use Cases

### Commitment Schemes
- Coin flips and dice rolls
- Rock-paper-scissors
- Sealed-bid auctions
- Voting systems
- Any game requiring simultaneous hidden choices

### Mental Poker
- Online poker without a house
- Decentralized card games
- Peer-to-peer gambling
- Trustless gaming platforms
- Blockchain-based card games

## Further Reading

- **Mental Poker** - Shamir, Rivest, Adleman (1981)
- **Commitment Schemes** - Cryptographic primitives and protocols
- **Zero-Knowledge Proofs** - Proving statements without revealing information
- **Secure Multi-Party Computation** - Computing functions with private inputs
