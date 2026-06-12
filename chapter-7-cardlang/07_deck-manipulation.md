# Deck Manipulation Primitives

Beyond `draw`, several operations manipulate deck structure without transferring cards between zones. We formalize these as pure functions with verifiable properties.

## Cut

The `cut` operation splits a deck at position k and transposes the two halves. This is a standard anti-cheating measure in physical card games.

### Cut Definition

For a deck D = (c₀, c₁, ..., cₙ₋₁) and cut position k ∈ [0, n]:

```
cut(k, D) = (cₖ, cₖ₊₁, ..., cₙ₋₁, c₀, c₁, ..., cₖ₋₁)
```

```haskell
cut :: Int -> ZoneId -> GameState -> Either CutError GameState
cut k zone state
    | k < 0            = Left (InvalidCutPosition k)
    | k > length cards = Left (InvalidCutPosition k)
    | otherwise        = Right $ state & ix zone . zoneCards .~ cut'
  where
    cards         = zoneCards (state ! zone)
    (top, bottom) = splitAt k cards
    cut'          = bottom ++ top

data CutError = InvalidCutPosition Int
              | ZoneNotFound ZoneId
    deriving (Eq, Show)
```

### Cut is a Rotation Property

The cut operation is equivalent to a left rotation by k positions:

```
cut(k, D) = rotate_L(k, D)
```

### Cut Preserves Cards Theorem

For any valid cut position k:

```
multiset(cut(k, D)) = multiset(D)
```

The cut operation is a permutation; no cards are created, destroyed, or duplicated.

### Cut Inverse Theorem

Cutting is self-inverse with complementary position:

```
cut(n - k, cut(k, D)) = D
```

### Deterministic Cut Selection

In a verifiable system, the cut position itself must be determined fairly. We define a seeded cut:

```haskell
cutWithSeed :: Seed -> ZoneId -> GameState -> GameState
cutWithSeed seed zone state =
    let cards = zoneCards (state ! zone)
        n     = length cards
        k     = bytesToInt seed `mod` n  -- uniform in [0, n-1]
    in fromRight state $ cut k zone state

-- For player-selected cuts with commitment
data CutCommitment = CutCommitment
    { cutPlayer   :: PlayerId
    , cutHash     :: Hash        -- H(position || salt)
    , cutPosition :: Maybe Int   -- revealed after commitment
    , cutSalt     :: Maybe Salt
    }
```

## Burn

The `burn` operation discards cards face-down, removing them from play without revealing their values. This prevents exploitation of marked cards or deck tracking.

### Burn Definition

Burning n cards transfers them from the source zone to the burn zone:

```
burn(n, src, Γ) = draw(n, src, burnZone, Γ)
```

```haskell
burn :: Int -> ZoneId -> GameState -> Either DrawError GameState
burn n src = draw n src burnZone
  where
    burnZone = ZoneId "burn"

-- Burn with record for verification
burnWithRecord :: Int -> ZoneId -> GameState
               -> Either DrawError (GameState, BurnRecord)
burnWithRecord n src state = do
    (state', drawRec) <- drawWithRecord n src burnZone state
    let burnRec = BurnRecord
            { brCount  = n
            , brSource = src
            , brCards  = drCards drawRec  -- hidden in practice, revealed for verification
            }
    return (state', burnRec)

data BurnRecord = BurnRecord
    { brCount  :: Int
    , brSource :: ZoneId
    , brCards  :: [Card]  -- for post-hoc verification only
    } deriving (Eq, Show)
```

### Burn Semantics Property

Burned cards are:
1. Removed from the source zone
2. Not revealed to any player during normal play
3. Verifiable post-game via the burn record
4. Not recyclable (unlike discard)

## Discard

The `discard` operation moves cards to a discard pile, which unlike the burn pile may be recycled back into the deck. Discards may be either face-up (visible to all) or face-down (hidden).

### Visibility Definition

Card visibility is a property of the zone and the discard action:

```haskell
data Visibility = FaceUp | FaceDown
    deriving (Eq, Show)

data DiscardZone = DiscardZone
    { dzId         :: ZoneId
    , dzCards      :: [Card]
    , dzVisibility :: [(Card, Visibility)]  -- per-card visibility
    }
```

### Discard Definition

Discarding n cards transfers them from source to the discard zone with specified visibility:

```
discard(n, v, src, Γ) = (Γ', R)
```

where Γ' is the updated game state and R is the discard record.

```haskell
discard :: Int -> Visibility -> ZoneId -> GameState
        -> Either DiscardError GameState
discard n vis src state
    | n <= 0              = Left (InvalidDiscardCount n)
    | n > length srcCards = Left (InsufficientCards src n (length srcCards))
    | otherwise           = Right state'
  where
    srcCards          = zoneCards (state ! src)
    (discarded, rest) = splitAt n srcCards
    discardZone       = state ! discardZoneId
    newVisibility     = dzVisibility discardZone ++ map (, vis) discarded
    state'            = state
                      & ix src . zoneCards .~ rest
                      & ix discardZoneId . dzCards %~ (++ discarded)
                      & ix discardZoneId . dzVisibility .~ newVisibility

data DiscardError
    = InvalidDiscardCount Int
    | InsufficientCards ZoneId Int Int
    | DiscardZoneNotFound
    deriving (Eq, Show)
```

### Face-Up Discard

Face-up discards are visible to all players. This is common in games like Rummy, where players may draw from the discard pile.

```haskell
discardFaceUp :: Int -> ZoneId -> GameState -> Either DiscardError GameState
discardFaceUp n = discard n FaceUp

-- Discard specific cards face-up (player choice)
discardCards :: [Card] -> Visibility -> ZoneId -> GameState
             -> Either DiscardError GameState
discardCards cards vis src state
    | not (all (`elem` srcCards) cards) = Left CardNotInZone
    | otherwise = Right state'
  where
    srcCards    = zoneCards (state ! src)
    state'      = state
                & ix src . zoneCards %~ (\\ cards)
                & ix discardZoneId . dzCards %~ (++ cards)
                & ix discardZoneId . dzVisibility %~ (++ map (, vis) cards)
```

### Face-Down Discard

Face-down discards hide the card values. This is used when a player mucks (folds) in poker without revealing.

```haskell
discardFaceDown :: Int -> ZoneId -> GameState -> Either DiscardError GameState
discardFaceDown n = discard n FaceDown

-- Muck: discard entire hand face-down (poker fold)
muck :: ZoneId -> GameState -> GameState
muck hand state = fromRight state $ discard n FaceDown hand state
  where
    n = length $ zoneCards (state ! hand)
```

### Discard vs Burn Comparison

| Property | Discard | Burn |
|----------|---------|------|
| Visibility options | Face-up or face-down | Always face-down |
| Recyclable | Yes (reshuffle) | No |
| Drawable | Yes (some games) | Never |
| Destination zone | Discard pile | Burn pile |
| Typical use | Played/folded cards | Anti-cheat mechanism |

## Recycle

The `recycle` operation returns discarded cards to the deck, typically followed by a shuffle:

```haskell
recycle :: ZoneId -> GameState -> GameState
recycle discardZone state = state
    & ix deckZone . zoneCards %~ (++ discardedCards)
    & ix discardZone . dzCards .~ []
    & ix discardZone . dzVisibility .~ []
  where
    discardedCards = dzCards (state ! discardZone)

-- Full recycle with shuffle
recycleAndShuffle :: Seed -> ZoneId -> GameState -> GameState
recycleAndShuffle seed discardZone state =
    state & recycle discardZone & shuffle seed deckZone
```

In CardLang DSL:

```yaml
on_deck_empty:
  - recycle: discard_pile into deck
  - shuffle: deck with new_seed
  - burn: 1 from deck
```

### Recycle Preserves Cards Theorem

The recycle operation preserves the card multiset:

```
multiset(Γ(deck)) ⊎ multiset(Γ(discard)) = multiset(Γ'(deck))
```

where Γ' = recycle(discard, Γ).

## Riffle and Wash

For completeness, we define additional physical shuffling operations that may appear in game specifications.

### Riffle Shuffle Definition

A riffle shuffle interleaves two halves of the deck. Given deck D split at midpoint into L and R:

```
riffle(D) = interleave(L, R)
```

where `interleave` alternates cards from each half.

```haskell
riffle :: ZoneId -> GameState -> GameState
riffle zone state = state & ix zone . zoneCards .~ riffled
  where
    cards         = zoneCards (state ! zone)
    (left, right) = splitAt (length cards `div` 2) cards
    riffled       = interleave left right

interleave :: [a] -> [a] -> [a]
interleave []     ys = ys
interleave xs     [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- Seeded riffle with realistic imperfection
riffleWithSeed :: Seed -> ZoneId -> GameState -> GameState
riffleWithSeed seed zone state = state & ix zone . zoneCards .~ riffled
  where
    cards   = zoneCards (state ! zone)
    rng     = initRng seed
    riffled = simulateRiffle rng cards

    -- Models imperfect human riffle: variable packet sizes
    simulateRiffle :: RNG -> [Card] -> [Card]
```

### Wash / Scramble Definition

A wash (or scramble) spreads cards face-down and mixes them randomly. This is modeled as a Fisher-Yates shuffle:

```
wash(seed, D) = shuffle(seed, D)
```

```haskell
wash :: Seed -> ZoneId -> GameState -> GameState
wash seed zone state = state & ix zone . zoneCards .~ washed
  where
    cards  = zoneCards (state ! zone)
    washed = fisherYates (initRng seed) cards
```

## Strip Cut

A strip cut removes packets from the top and reassembles them in a different order.

### Strip Cut Definition

Given packet sizes (p₁, p₂, ..., pₘ) summing to n, and a permutation σ of [1..m]:

```
stripCut((p₁, ..., pₘ), σ, D) = P_σ(1) ++ P_σ(2) ++ ... ++ P_σ(m)
```

where Pᵢ is the i-th packet of size pᵢ.

```haskell
stripCut :: [Int] -> [Int] -> ZoneId -> GameState -> Either StripError GameState
stripCut packetSizes ordering zone state
    | sum packetSizes /= length cards = Left InvalidPacketSizes
    | sort ordering /= [0..length packetSizes - 1] = Left InvalidOrdering
    | otherwise = Right $ state & ix zone . zoneCards .~ stripped
  where
    cards    = zoneCards (state ! zone)
    packets  = splitIntoPackets packetSizes cards
    stripped = concatMap (packets !!) ordering

splitIntoPackets :: [Int] -> [a] -> [[a]]
splitIntoPackets [] _ = []
splitIntoPackets (p:ps) xs =
    let (packet, rest) = splitAt p xs
    in packet : splitIntoPackets ps rest

data StripError = InvalidPacketSizes | InvalidOrdering
    deriving (Eq, Show)
```

## Combine and Split

Operations for merging and dividing card collections.

```haskell
-- Combine multiple zones into one (e.g., collecting cards for reshuffle)
combine :: [ZoneId] -> ZoneId -> GameState -> GameState
combine srcs dst state = foldr collectFrom state srcs
  where
    collectFrom src st = fromRight st $ drawAll src dst st

-- Split a zone into multiple zones (e.g., dealing to separate hands)
split :: ZoneId -> [(ZoneId, Int)] -> GameState -> Either DrawError GameState
split src distributions = foldM distribute
  where
    distribute state (dst, n) = draw n src dst state
```

## Operation Composition

Complex dealing procedures are compositions of primitives:

```haskell
-- Standard casino poker dealing procedure
casinoDeal :: Seed -> [PlayerId] -> GameState -> GameState
casinoDeal seed players state = state
    & execState (do
        -- Wash and riffle
        modify $ wash seed1 deckZone
        modify $ riffle deckZone
        modify $ riffle deckZone
        modify $ riffle deckZone

        -- Player cut
        modify $ cutWithSeed seed2 deckZone

        -- Burn and deal
        modify $ fromRight id . burn 1 deckZone
        forM_ [1..2] $ \_ ->
            forM_ players $ \p ->
                modify $ fromRight id . draw 1 deckZone (handZone p)
      )
  where
    (seed1, seed2) = splitSeed seed
```

Equivalent in CardLang DSL:

```yaml
dealing_procedure: casino_standard

setup:
  - wash: deck with seed
  - riffle: deck times 3
  - cut: deck by player
  - burn: 1 from deck
  - deal: 2 to each_player from deck
```

## Summary of Deck Operations

| Operation | Signature | Effect |
|-----------|-----------|--------|
| `shuffle` | Seed → Zone → Γ → Γ | Random permutation |
| `cut` | Int → Zone → Γ → Γ | Rotate at position |
| `burn` | Int → Zone → Γ → Γ | Discard face-down |
| `riffle` | Zone → Γ → Γ | Interleave halves |
| `wash` | Seed → Zone → Γ → Γ | Spread and scramble |
| `stripCut` | [Int] → [Int] → Zone → Γ → Γ | Packet reorder |
| `combine` | [Zone] → Zone → Γ → Γ | Merge zones |
| `draw` | Int → Zone → Zone → Γ → Γ | Transfer cards |

## Composition Closure Theorem

All deck manipulation primitives preserve card conservation: the multiset of cards across all zones is invariant under any sequence of operations.
