{-|
Module      : CardLang.Manipulation
Description : Deck manipulation primitives (cut, burn, discard, etc.)
Copyright   : (c) 2026
License     : MIT

This module implements deck manipulation operations that don't transfer
cards between zones but modify deck structure.
-}

module CardLang.Manipulation where

import Data.List (intercalate, delete, sortBy)
import Data.Ord (comparing)

-- Primitive types
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | RT | RJ | RQ | RK | RA
    deriving (Eq, Ord, Enum, Bounded, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Eq, Ord, Enum, Bounded, Show)

data Card = Card Rank Suit deriving (Eq, Ord)

instance Show Card where
    show (Card r s) = showRank r ++ showSuit s
      where
        showRank R2 = "2"; showRank R3 = "3"; showRank R4 = "4"
        showRank R5 = "5"; showRank R6 = "6"; showRank R7 = "7"
        showRank R8 = "8"; showRank R9 = "9"; showRank RT = "T"
        showRank RJ = "J"; showRank RQ = "Q"; showRank RK = "K"
        showRank RA = "A"
        showSuit Clubs = "c"; showSuit Diamonds = "d"
        showSuit Hearts = "h"; showSuit Spades = "s"

type Deck = [Card]

standard52 :: Deck
standard52 = [Card r s | s <- [Clubs ..], r <- [R2 ..]]

-- ============================================================
-- Cut Operation
-- ============================================================

data CutError = InvalidCutPosition Int
    deriving (Eq, Show)

-- | Cut the deck at position k
-- cut(k, D) = (c_k, c_{k+1}, ..., c_{n-1}, c_0, c_1, ..., c_{k-1})
cut :: Int -> Deck -> Either CutError Deck
cut k deck
    | k < 0           = Left (InvalidCutPosition k)
    | k > length deck = Left (InvalidCutPosition k)
    | otherwise       = Right $ bottom ++ top
  where
    (top, bottom) = splitAt k deck

-- | Cut with a seeded position (for deterministic games)
cutWithSeed :: Int -> Deck -> Deck
cutWithSeed seed deck =
    let n = length deck
        k = seed `mod` n
    in case cut k deck of
        Right d -> d
        Left _  -> deck  -- Should never happen

-- | Property: cut is a rotation
-- cut(k, D) = rotate_L(k, D)
rotateLeft :: Int -> [a] -> [a]
rotateLeft k xs = drop k xs ++ take k xs

-- | Property: cut inverse
-- cut(n - k, cut(k, D)) = D
verifyCutInverse :: Int -> Deck -> Bool
verifyCutInverse k deck =
    let n = length deck
        Right cut1 = cut k deck
        Right cut2 = cut (n - k) cut1
    in cut2 == deck

-- ============================================================
-- Riffle Shuffle
-- ============================================================

-- | Interleave two lists
interleave :: [a] -> [a] -> [a]
interleave []     ys = ys
interleave xs     [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- | Perfect riffle shuffle - interleaves two halves
riffle :: Deck -> Deck
riffle deck =
    let n = length deck
        (left, right) = splitAt (n `div` 2) deck
    in interleave left right

-- | Out-shuffle (top card stays on top)
outShuffle :: Deck -> Deck
outShuffle = riffle

-- | In-shuffle (top card moves to second position)
inShuffle :: Deck -> Deck
inShuffle deck =
    let n = length deck
        (left, right) = splitAt (n `div` 2) deck
    in interleave right left

-- ============================================================
-- Strip Cut
-- ============================================================

data StripError = InvalidPacketSizes | InvalidOrdering
    deriving (Eq, Show)

-- | Split deck into packets of specified sizes
splitIntoPackets :: [Int] -> Deck -> [Deck]
splitIntoPackets [] _ = []
splitIntoPackets (p:ps) cards =
    let (packet, rest) = splitAt p cards
    in packet : splitIntoPackets ps rest

-- | Strip cut: rearrange packets in a new order
stripCut :: [Int] -> [Int] -> Deck -> Either StripError Deck
stripCut packetSizes ordering deck
    | sum packetSizes /= length deck = Left InvalidPacketSizes
    | sortBy compare ordering /= [0..length packetSizes - 1] = Left InvalidOrdering
    | otherwise = Right $ concatMap (packets !!) ordering
  where
    packets = splitIntoPackets packetSizes deck

-- ============================================================
-- Visibility Types for Discard
-- ============================================================

data Visibility = FaceUp | FaceDown
    deriving (Eq, Show)

data DiscardedCard = DiscardedCard
    { dcCard       :: Card
    , dcVisibility :: Visibility
    } deriving (Eq, Show)

type DiscardPile = [DiscardedCard]

-- | Discard cards with visibility tracking
discardCards :: Visibility -> [Card] -> DiscardPile -> DiscardPile
discardCards vis cards pile = pile ++ map (\c -> DiscardedCard c vis) cards

-- | Get visible cards from discard pile
visibleCards :: DiscardPile -> [Card]
visibleCards = map dcCard . filter ((== FaceUp) . dcVisibility)

-- | Get all cards from discard pile (for recycling)
allDiscardedCards :: DiscardPile -> [Card]
allDiscardedCards = map dcCard

-- ============================================================
-- Combine and Split
-- ============================================================

-- | Combine multiple decks into one
combine :: [Deck] -> Deck
combine = concat

-- | Split a deck into parts
split :: [Int] -> Deck -> Either String [Deck]
split sizes deck
    | sum sizes /= length deck = Left "Sizes don't sum to deck length"
    | otherwise = Right $ splitIntoPackets sizes deck

-- ============================================================
-- Verification Properties
-- ============================================================

-- | Verify that an operation preserves cards (is a permutation)
verifyPermutation :: Deck -> Deck -> Bool
verifyPermutation d1 d2 =
    length d1 == length d2 &&
    sortBy compare d1 == sortBy compare d2

-- | Verify multiset equality
multisetEqual :: Deck -> Deck -> Bool
multisetEqual = verifyPermutation

-- ============================================================
-- Example Usage
-- ============================================================

showDeck :: String -> Deck -> IO ()
showDeck name deck = do
    putStrLn $ name ++ " (" ++ show (length deck) ++ " cards):"
    putStrLn $ "  " ++ intercalate " " (map show deck)
    putStrLn ""

main :: IO ()
main = do
    putStrLn "=== CardLang Deck Manipulation ==="
    putStrLn ""

    -- Use a small deck for demonstration
    let smallDeck = take 10 standard52
    showDeck "Original (10 cards)" smallDeck

    -- Cut operations
    putStrLn "=== Cut Operations ==="
    case cut 4 smallDeck of
        Right cutDeck -> showDeck "Cut at position 4" cutDeck
        Left err -> putStrLn $ "Error: " ++ show err

    case cut 0 smallDeck of
        Right cutDeck -> showDeck "Cut at position 0 (no change)" cutDeck
        Left err -> putStrLn $ "Error: " ++ show err

    -- Verify cut properties
    putStrLn "Cut Properties:"
    putStrLn $ "  cut == rotateLeft: " ++ show (cut 4 smallDeck == Right (rotateLeft 4 smallDeck))
    putStrLn $ "  cut inverse (k=4): " ++ show (verifyCutInverse 4 smallDeck)
    putStrLn ""

    -- Riffle operations
    putStrLn "=== Riffle Operations ==="
    let riffled = riffle smallDeck
    showDeck "After riffle" riffled
    putStrLn $ "  Preserves cards: " ++ show (verifyPermutation smallDeck riffled)
    putStrLn ""

    -- Multiple riffles
    let riffled3 = riffle $ riffle $ riffle smallDeck
    showDeck "After 3 riffles" riffled3

    -- Strip cut
    putStrLn "=== Strip Cut ==="
    case stripCut [3, 3, 4] [2, 0, 1] smallDeck of
        Right stripped -> do
            putStrLn "Strip cut: [3,3,4] packets, reorder [2,0,1]"
            showDeck "Result" stripped
            putStrLn $ "  Preserves cards: " ++ show (verifyPermutation smallDeck stripped)
        Left err -> putStrLn $ "Error: " ++ show err
    putStrLn ""

    -- Discard pile demonstration
    putStrLn "=== Discard Operations ==="
    let cards = [Card RA Spades, Card RK Hearts, Card RQ Diamonds]
    let pile1 = discardCards FaceUp [Card RA Spades, Card RK Hearts] []
    let pile2 = discardCards FaceDown [Card RQ Diamonds] pile1

    putStrLn "Discard pile contents:"
    mapM_ (\dc -> putStrLn $ "  " ++ show (dcCard dc) ++ " - " ++ show (dcVisibility dc)) pile2
    putStrLn $ "Visible cards: " ++ intercalate " " (map show $ visibleCards pile2)
    putStrLn $ "All cards: " ++ intercalate " " (map show $ allDiscardedCards pile2)
    putStrLn ""

    -- Combine and split
    putStrLn "=== Combine and Split ==="
    let deck1 = take 5 standard52
    let deck2 = take 5 $ drop 5 standard52
    let combined = combine [deck1, deck2]
    showDeck "Deck 1" deck1
    showDeck "Deck 2" deck2
    showDeck "Combined" combined

    case split [3, 4, 3] combined of
        Right parts -> do
            putStrLn "Split into [3, 4, 3]:"
            mapM_ (\(i, p) -> putStrLn $ "  Part " ++ show i ++ ": " ++ intercalate " " (map show p))
                  (zip [1..] parts)
        Left err -> putStrLn $ "Error: " ++ err
