# Deck Definitions as Expressions

A key insight of CardLang is that deck definitions can be expressed as pure functions over the primitive types. This enables composition, verification, and reuse.

## The Standard 52-Card Deck

```haskell
standard52 :: Deck
standard52 = [Card r s | s <- [Clubs ..], r <- [R2 ..]]
```

The list comprehension generates the Cartesian product R × S in canonical order.

## Deck Combinators

We define higher-order functions for constructing decks:

```haskell
-- Build deck from predicates
deckWhere :: (Rank -> Bool) -> (Suit -> Bool) -> Deck
deckWhere rankP suitP =
    [Card r s | s <- filter suitP [Clubs ..]
              , r <- filter rankP [R2 ..]]

-- Add jokers to any deck
withJokers :: Int -> Deck -> Deck
withJokers n deck = deck ++ replicate n Joker

-- Duplicate cards (for Pinochle, etc.)
duplicated :: Int -> Deck -> Deck
duplicated n = concatMap (replicate n)

-- Filter a deck
restricted :: (Card -> Bool) -> Deck -> Deck
restricted = filter
```

## Example Deck Definitions

Using these combinators, common deck variants become concise expressions:

```haskell
-- Euchre: 9 through A (24 cards)
euchre :: Deck
euchre = deckWhere (>= R9) (const True)

-- Pinochle: 9 through A, doubled (48 cards)
pinochle :: Deck
pinochle = duplicated 2 $ deckWhere (>= R9) (const True)

-- 500 deck: complex filtering plus joker
fiveHundred :: Deck
fiveHundred = redCards ++ blackCards ++ [Card R4 Hearts, Card R4 Diamonds, Joker]
  where
    redCards   = deckWhere (>= R7) (`elem` [Hearts, Diamonds])
    blackCards = deckWhere (>= R5) (`elem` [Spades, Clubs])
```

## Deck Cardinality Property

For any deck definition D constructed via combinators, |D| is statically computable at compile time.
