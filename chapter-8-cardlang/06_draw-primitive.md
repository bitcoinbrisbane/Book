# The Draw Primitive

The `draw` operation is the fundamental state transition in card games, moving cards from one zone to another. We formalize it as a pure function with explicit preconditions and postconditions.

## Zones and Game State

Cards exist in *zones*—distinct regions with different visibility and ownership properties.

### Zone Definition

```haskell
data ZoneType = DeckZone      -- face down, no owner
              | HandZone      -- hidden, owned by player
              | CommunityZone -- face up, shared
              | DiscardZone   -- face up, no owner
              | BurnZone      -- face down, out of play
    deriving (Eq, Show)

data Zone = Zone
    { zoneId    :: ZoneId
    , zoneType  :: ZoneType
    , zoneCards :: [Card]
    , zoneOwner :: Maybe PlayerId
    }
```

### Game State Definition

The game state Γ is a mapping from zone identifiers to zones:

```haskell
type GameState = Map ZoneId Zone
```

We write Γ(z) for the cards in zone z, and |Γ(z)| for the count.

## Draw as a Pure Function

The `draw` function transfers n cards from a source zone to a destination zone:

```haskell
draw :: Int -> ZoneId -> ZoneId -> GameState -> Either DrawError GameState
draw n src dst state
    | n <= 0              = Left (InvalidCount n)
    | n > length srcCards = Left (InsufficientCards src n (length srcCards))
    | otherwise           = Right state'
  where
    srcCards      = zoneCards (state ! src)
    (drawn, rest) = splitAt n srcCards
    state'        = state
                  & ix src . zoneCards .~ rest
                  & ix dst . zoneCards %~ (++ drawn)
```

The type signature makes explicit that `draw` may fail, returning a `DrawError` in the `Left` case.

```haskell
data DrawError
    = InvalidCount Int
    | InsufficientCards ZoneId Int Int  -- zone, requested, available
    | ZoneNotFound ZoneId
    | CardNotInZone Card ZoneId
    deriving (Eq, Show)
```

## Formal Specification

We specify `draw` via preconditions, postconditions, and invariants.

### Draw Preconditions

For `draw(n, src, dst, Γ)` to succeed:

- P1 (positive count): n > 0
- P2 (sufficient cards): n ≤ |Γ(src)|
- P3 (source exists): src ∈ dom(Γ)
- P4 (destination exists): dst ∈ dom(Γ)

### Draw Postconditions

If Γ' = `draw(n, src, dst, Γ)` succeeds, then:

- Q1 (source shrinks): |Γ'(src)| = |Γ(src)| - n
- Q2 (destination grows): |Γ'(dst)| = |Γ(dst)| + n
- Q3 (first n removed): Γ'(src) = drop n Γ(src)
- Q4 (cards transferred): take n Γ(src) ⊆ Γ'(dst)

## Card Conservation Theorem

The `draw` operation preserves the total card count:

```
∑(z ∈ dom(Γ)) |Γ(z)| = ∑(z ∈ dom(Γ')) |Γ'(z)|
```

**Proof:** By Q1 and Q2, the source loses exactly n cards and the destination gains exactly n cards. All other zones are unchanged. Thus the total is preserved.

## Card Identity Preservation Theorem

The multiset of all cards across all zones is invariant under `draw`:

```
⊎(z ∈ dom(Γ)) Γ(z) = ⊎(z ∈ dom(Γ')) Γ'(z)
```

**Proof:** The cards removed from src are exactly the cards added to dst; no cards are created or destroyed.

## Draw Variants

Several common operations are defined in terms of the base `draw`:

```haskell
-- Draw a single card
draw1 :: ZoneId -> ZoneId -> GameState -> Either DrawError (Card, GameState)
draw1 src dst state = do
    state' <- draw 1 src dst state
    let card = head $ zoneCards (state ! src)  -- safe: draw succeeded
    return (card, state')

-- Draw from bottom of deck
drawBottom :: Int -> ZoneId -> ZoneId -> GameState -> Either DrawError GameState
drawBottom n src dst state
    | n > length srcCards = Left (InsufficientCards src n (length srcCards))
    | otherwise           = Right state'
  where
    srcCards        = zoneCards (state ! src)
    (rest, drawn)   = splitAt (length srcCards - n) srcCards
    state'          = state
                    & ix src . zoneCards .~ rest
                    & ix dst . zoneCards %~ (++ drawn)

-- Draw specific card (for games with choice)
drawCard :: Card -> ZoneId -> ZoneId -> GameState -> Either DrawError GameState
drawCard card src dst state
    | card `notElem` srcCards = Left (CardNotInZone card src)
    | otherwise               = Right state'
  where
    srcCards = zoneCards (state ! src)
    state'   = state
             & ix src . zoneCards %~ delete card
             & ix dst . zoneCards %~ (++ [card])

-- Draw all cards
drawAll :: ZoneId -> ZoneId -> GameState -> GameState
drawAll src dst state = state
    & ix dst . zoneCards %~ (++ srcCards)
    & ix src . zoneCards .~ []
  where
    srcCards = zoneCards (state ! src)
```

## Indexed Draw for Verifiability

For cryptographic verification, we track not just the resulting state but also the exact operation performed:

```haskell
data DrawRecord = DrawRecord
    { drCount   :: Int
    , drSource  :: ZoneId
    , drDest    :: ZoneId
    , drCards   :: [Card]      -- which cards moved
    , drIndices :: [Int]       -- their positions in source
    } deriving (Eq, Show)

drawWithRecord :: Int -> ZoneId -> ZoneId -> GameState
               -> Either DrawError (GameState, DrawRecord)
drawWithRecord n src dst state = do
    state' <- draw n src dst state
    let srcCards = zoneCards (state ! src)
        drawn    = take n srcCards
        record   = DrawRecord n src dst drawn [0..n-1]
    return (state', record)
```

The `DrawRecord` provides an audit trail: given the prior state and the record, anyone can verify the transition and reconstruct the new state.

## Composition: The Deal Operation

The `deal` operation is repeated `draw` to multiple destinations:

```haskell
-- Deal n cards to each player in order
deal :: Int -> ZoneId -> [ZoneId] -> GameState -> Either DrawError GameState
deal n src dsts = foldM dealOne
  where
    dealOne state dst = draw n src dst state

-- Deal one card at a time, rotating through players (standard dealing)
dealRound :: Int -> ZoneId -> [ZoneId] -> GameState -> Either DrawError GameState
dealRound rounds src dsts state = foldM dealOne state (concat $ replicate rounds dsts)
  where
    dealOne st dst = draw 1 src dst st
```

In game specification syntax:

```yaml
setup:
  - deal: 2 to each_player from deck    # dealRound 2 deck players
  - deal: 5 to community from deck      # draw 5 deck community
```

## Deal Determinism Property

Given the same initial deck state (post-shuffle) and player order, `deal` produces identical hands regardless of implementation, as it is defined purely in terms of `draw`.
