# Ranking Systems

Different games impose different orderings on cards. We capture this via the `Ranking` type class.

```haskell
class Ranking a where
    compareCards :: a -> Card -> Card -> Ordering

data HighCardRanking = HighCardRanking
    { aceHigh :: Bool
    , suitOrder :: Maybe [Suit]  -- Nothing = suits don't break ties
    }

instance Ranking HighCardRanking where
    compareCards cfg c1 c2 =
        case compare (rankValue cfg $ rank c1) (rankValue cfg $ rank c2) of
            EQ -> compareSuits cfg (suit c1) (suit c2)
            x  -> x
```

## Ranking Configuration

The `Ranking` type class allows games to define their own card comparison rules:

- **Ace Position**: Whether aces are high, low, or both (for straights)
- **Suit Ordering**: Optional suit hierarchy for tie-breaking
- **Custom Rank Values**: Games like Blackjack assign numeric values to face cards

## Common Ranking Systems

### High Card (Standard)

Ranks ordered 2 < 3 < ... < K < A, suits don't break ties.

### Poker Hand Rankings

Full poker hand evaluation including pairs, straights, flushes, etc.

### Blackjack Values

Face cards = 10, Aces = 1 or 11.
