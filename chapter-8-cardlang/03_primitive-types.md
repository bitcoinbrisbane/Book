# Primitive Types

The foundation of CardLang consists of four primitive types that capture the essential structure of playing cards.

## Rank

A `Rank` is an enumerated type representing the face value of a card:

```haskell
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
          | RT | RJ | RQ | RK | RA
    deriving (Eq, Enum, Bounded)
```

The set of ranks is denoted R = {2, 3, 4, 5, 6, 7, 8, 9, T, J, Q, K, A} with |R| = 13.

## Suit

A `Suit` is an enumerated type representing the suit of a card:

```haskell
data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Eq, Enum, Bounded)
```

The set of suits is denoted S = {Clubs, Diamonds, Hearts, Spades} with |S| = 4.

## Card

A `Card` is a product type of `Rank` and `Suit`:

```haskell
data Card = Card Rank Suit
    deriving (Eq)
```

The set of standard cards is C = R x S with |C| = 52.

## Deck

A `Deck` is an ordered sequence of cards:

```haskell
type Deck = [Card]
```

A deck D is a sequence (c₀, c₁, ..., cₙ₋₁) where each cᵢ ∈ C.

## Byte Encoding

For efficient runtime representation and cryptographic operations, cards are encoded as single bytes.

### Card Encoding

Given a canonical deck definition D = (c₀, c₁, ..., cₙ₋₁), the encoding function encode_D : C → B maps each card to its index:

```
encode_D(c) = i where c = cᵢ
```

For the standard 52-card deck with the canonical ordering (clubs, diamonds, hearts, spades) x (2 through A):

```
encode(Card r s) = fromEnum(r) + fromEnum(s) × 13
```

This yields the mapping: 2♣ → 0, 3♣ → 1, ..., A♣ → 12, 2♦ → 13, ..., A♠ → 51.
