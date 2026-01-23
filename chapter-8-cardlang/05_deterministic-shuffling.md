# Deterministic Shuffling

For applications requiring verifiability, shuffling must be deterministic given a seed. We specify the Fisher-Yates shuffle with cryptographic random number generation.

## Fisher-Yates Algorithm

```
Algorithm: Fisher-Yates Shuffle
Require: Deck D = [c₀, c₁, ..., cₙ₋₁], seed s ∈ {0,1}²⁵⁶
Ensure: Permuted deck D'

1. Initialize RNG with seed s
2. For i = n-1 downto 1:
   a. j ← random(0, i)  // Uniform in [0, i]
   b. Swap D[i] and D[j]
3. Return D
```

## Shuffle Primitive

```haskell
shuffle :: Seed -> Deck -> Deck
shuffle seed deck = fisherYates (initRng seed) deck
  where
    initRng :: Seed -> ChaCha20
    fisherYates :: ChaCha20 -> Deck -> Deck
```

## Seed Generation for Multiplayer

In adversarial multiplayer settings, the seed must be generated via a commitment scheme to prevent manipulation.

### Commitment-Based Seed Protocol

For n players, the combined seed is generated as:

1. Each player i generates random sᵢ ∈ {0,1}²⁵⁶
2. Each player broadcasts commitment hᵢ = H(sᵢ)
3. After all commitments received, players reveal sᵢ
4. Verify H(sᵢ) = hᵢ for all i
5. Combined seed: s = H(s₁ || s₂ || ... || sₙ)

```haskell
type Seed = ByteString  -- 32 bytes
type Commitment = ByteString  -- SHA256 hash

data SeedProtocol = SeedProtocol
    { commit :: Seed -> Commitment
    , reveal :: Seed -> Commitment -> Bool
    , combine :: [Seed] -> Seed
    }

defaultProtocol :: SeedProtocol
defaultProtocol = SeedProtocol
    { commit  = sha256
    , reveal  = \s c -> sha256 s == c
    , combine = sha256 . mconcat
    }
```

## Shuffle Fairness Theorem

Given the commitment-based seed generation protocol, no coalition of fewer than n players can predict or influence the shuffle outcome, assuming the hash function H is a random oracle.
