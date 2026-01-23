# Chapter 8: CardLang - Code Examples

This directory contains Haskell implementations of the CardLang domain-specific language concepts.

## Directory Structure

```
sample/
├── primitives/           # Core type definitions
│   └── Types.hs          # Rank, Suit, Card, Deck types
├── deck-combinators/     # Deck construction functions
│   └── Combinators.hs    # Higher-order deck building
├── shuffling/            # Deterministic shuffling
│   └── Shuffle.hs        # Fisher-Yates with seeded RNG
├── draw/                 # Card transfer operations
│   └── Draw.hs           # Zone management and draw primitive
├── manipulation/         # Deck manipulation operations
│   └── DeckOps.hs        # Cut, riffle, strip cut, discard
└── game/                 # Complete game implementations
    └── CutForHighCard.hs # Full game example
```

## Topics Covered

| Directory | Files | Description |
|-----------|-------|-------------|
| `primitives/` | Types.hs | Card, Rank, Suit, Deck types with byte encoding |
| `deck-combinators/` | Combinators.hs | Functional deck construction (Euchre, Pinochle, etc.) |
| `shuffling/` | Shuffle.hs | Fisher-Yates shuffle, commitment schemes |
| `draw/` | Draw.hs | Zone-based card transfers, deal operations |
| `manipulation/` | DeckOps.hs | Cut, riffle, strip cut, discard, combine |
| `game/` | CutForHighCard.hs | Complete game with state machine |

## Running the Examples

### Prerequisites

You'll need GHC (Glasgow Haskell Compiler) installed:

```bash
# macOS
brew install ghc

# Ubuntu/Debian
sudo apt install ghc

# Or use GHCup (recommended)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

### Running Individual Files

Each file has a `main` function that demonstrates its concepts:

```bash
# From the sample/ directory:

# Primitive types
cd primitives
runghc Types.hs

# Deck combinators
cd ../deck-combinators
runghc Combinators.hs

# Shuffling
cd ../shuffling
runghc Shuffle.hs

# Draw operations
cd ../draw
runghc Draw.hs

# Deck manipulation
cd ../manipulation
runghc DeckOps.hs

# Complete game
cd ../game
runghc CutForHighCard.hs
```

### Using GHCi (Interactive)

For exploration, use the GHC interpreter:

```bash
cd primitives
ghci Types.hs

# Then in GHCi:
> standard52
> encodeCard (Card RA Spades)
> decodeCard 51
> length standard52
```

## Code Overview

### Primitives (Types.hs)

Defines the four fundamental types:

```haskell
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | RT | RJ | RQ | RK | RA
data Suit = Clubs | Diamonds | Hearts | Spades
data Card = Card Rank Suit
type Deck = [Card]

-- Byte encoding for efficient representation
encodeCard :: Card -> Int  -- 0-51
decodeCard :: Int -> Card
```

### Deck Combinators (Combinators.hs)

Higher-order functions for building deck variants:

```haskell
-- Build deck from predicates
deckWhere :: (Rank -> Bool) -> (Suit -> Bool) -> Deck

-- Example decks
euchre   = deckWhere (>= R9) (const True)       -- 24 cards
pinochle = duplicated 2 $ deckWhere (>= R9) (const True)  -- 48 cards
```

### Shuffling (Shuffle.hs)

Deterministic Fisher-Yates shuffle:

```haskell
shuffle :: Seed -> Deck -> Deck

-- Multiplayer seed combination
combineSeeds :: [Seed] -> Seed

-- Commitment scheme
commit :: Seed -> Commitment
verifyCommitment :: Seed -> Commitment -> Bool
```

### Draw Operations (Draw.hs)

Zone-based card management with formal verification:

```haskell
draw :: Int -> ZoneId -> ZoneId -> GameState -> Either DrawError GameState

-- Preconditions: n > 0, n <= |source|, zones exist
-- Postconditions: source shrinks by n, destination grows by n
-- Invariant: total card count preserved
```

### Deck Manipulation (DeckOps.hs)

Physical deck operations:

```haskell
cut :: Int -> Deck -> Either CutError Deck      -- Rotate at position
riffle :: Deck -> Deck                          -- Interleave halves
stripCut :: [Int] -> [Int] -> Deck -> Either StripError Deck
```

### Complete Game (CutForHighCard.hs)

Full game implementation demonstrating:

- Game state management
- Draw and reveal phases
- Ranking comparison
- Tie handling with replay
- Deterministic verification

## Key Concepts

### Card Conservation Theorem

All operations preserve the multiset of cards:

```haskell
-- Before and after any operation:
multiset(allCards(state)) == multiset(allCards(state'))
```

### Determinism

Given the same seed, all operations produce identical results:

```haskell
shuffle seed deck == shuffle seed deck  -- Always true
```

### Formal Specification

Operations have explicit pre/post conditions:

```
draw(n, src, dst, state):
  Pre:  n > 0, n <= |state(src)|, src exists, dst exists
  Post: |state'(src)| = |state(src)| - n
        |state'(dst)| = |state(dst)| + n
```

## Security Notes

**Educational Code**: These implementations are for learning purposes.

For production use:
- Replace the LCG with ChaCha20 or similar CSPRNG
- Use SHA-256 for commitment hashes
- Implement proper constant-time comparisons
- Add comprehensive input validation

## Learning Path

### Beginner
1. Start with `primitives/Types.hs` - understand Card/Deck types
2. Try `deck-combinators/Combinators.hs` - see functional composition
3. Run `shuffling/Shuffle.hs` - understand deterministic shuffling

### Intermediate
1. Study `draw/Draw.hs` - zone management and formal specs
2. Explore `manipulation/DeckOps.hs` - physical deck operations
3. Understand card conservation proofs

### Advanced
1. Implement additional games using the primitives
2. Add multiplayer seed protocol implementation
3. Create formal proofs using a proof assistant
4. Build blockchain integration for verifiable games

## References

See the chapter markdown files for theoretical background:
- `03_primitive-types.md` - Type theory
- `04_deck-definitions.md` - Functional deck construction
- `05_deterministic-shuffling.md` - Fisher-Yates and commitment schemes
- `06_draw-primitive.md` - Formal specification
- `07_deck-manipulation.md` - Deck operations
- `09_game-specification.md` - Game DSL syntax
