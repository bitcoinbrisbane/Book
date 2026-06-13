{-|
Module      : CardLang.Shuffling
Description : Deterministic shuffling with Fisher-Yates algorithm
Copyright   : (c) 2026
License     : MIT

This module implements deterministic shuffling using the Fisher-Yates algorithm
with seeded random number generation for verifiable card games.
-}

module CardLang.Shuffling where

import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.Word (Word64)
import Data.Bits
import Data.List (intercalate)

-- Primitive types
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | RT | RJ | RQ | RK | RA
    deriving (Eq, Ord, Enum, Bounded, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Eq, Ord, Enum, Bounded, Show)

data Card = Card Rank Suit deriving (Eq)

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
-- Simple Linear Congruential Generator (for demonstration)
-- In production, use ChaCha20 or similar CSPRNG
-- ============================================================

-- | Simple PRNG state
type Seed = Word64

-- | LCG constants (same as glibc)
lcgA, lcgC, lcgM :: Word64
lcgA = 1103515245
lcgC = 12345
lcgM = 2^31

-- | Generate next random number
nextRandom :: Seed -> (Word64, Seed)
nextRandom seed = (seed', seed')
  where
    seed' = (lcgA * seed + lcgC) `mod` lcgM

-- | Generate random number in range [0, n)
randomInRange :: Int -> Seed -> (Int, Seed)
randomInRange n seed =
    let (r, seed') = nextRandom seed
    in (fromIntegral r `mod` n, seed')

-- ============================================================
-- Fisher-Yates Shuffle
-- ============================================================

-- | Fisher-Yates shuffle algorithm
-- For i from n-1 down to 1:
--   j <- random(0, i)
--   swap(deck[i], deck[j])
fisherYates :: Seed -> [a] -> [a]
fisherYates seed xs = runST $ do
    let n = length xs
    arr <- newListArray (0, n-1) xs :: ST s (STArray s Int a)
    go arr (n-1) seed
    getElems arr
  where
    go _ 0 _ = return ()
    go arr i currentSeed = do
        let (j, nextSeed) = randomInRange (i + 1) currentSeed
        -- Swap elements at i and j
        vi <- readArray arr i
        vj <- readArray arr j
        writeArray arr i vj
        writeArray arr j vi
        go arr (i-1) nextSeed

-- | Shuffle a deck with a given seed
shuffle :: Seed -> Deck -> Deck
shuffle = fisherYates

-- ============================================================
-- Seed Combination (for multiplayer)
-- ============================================================

-- | Simple hash function for combining seeds (demonstration only)
-- In production, use SHA-256
simpleHash :: [Word64] -> Word64
simpleHash = foldl combine 0
  where
    combine acc x = (acc `xor` x) * 0x517cc1b727220a95 + 0x9e3779b97f4a7c15

-- | Combine multiple player seeds into one
combineSeeds :: [Seed] -> Seed
combineSeeds = simpleHash

-- | Commitment scheme types
type Commitment = Word64  -- In production, this would be a SHA-256 hash

-- | Create a commitment to a seed
commit :: Seed -> Commitment
commit seed = simpleHash [seed, 0xDEADBEEF]  -- Simplified; real impl uses SHA-256

-- | Verify a commitment
verifyCommitment :: Seed -> Commitment -> Bool
verifyCommitment seed commitment = commit seed == commitment

-- ============================================================
-- Verification
-- ============================================================

-- | Verify that a shuffle is deterministic
-- Given the same seed, shuffle produces the same result
verifyDeterministic :: Seed -> Deck -> Bool
verifyDeterministic seed deck =
    shuffle seed deck == shuffle seed deck

-- | Verify that shuffle is a permutation (no cards lost or duplicated)
verifyPermutation :: Deck -> Deck -> Bool
verifyPermutation original shuffled =
    length original == length shuffled &&
    all (`elem` shuffled) original &&
    all (`elem` original) shuffled

-- ============================================================
-- Example Usage
-- ============================================================

main :: IO ()
main = do
    putStrLn "=== CardLang Deterministic Shuffling ==="
    putStrLn ""

    let deck = standard52
    let seed1 = 12345 :: Seed
    let seed2 = 67890 :: Seed

    -- Demonstrate deterministic shuffling
    putStrLn "Original deck (first 13):"
    putStrLn $ "  " ++ intercalate " " (map show $ take 13 deck)
    putStrLn ""

    let shuffled1 = shuffle seed1 deck
    putStrLn $ "Shuffled with seed " ++ show seed1 ++ " (first 13):"
    putStrLn $ "  " ++ intercalate " " (map show $ take 13 shuffled1)
    putStrLn ""

    let shuffled2 = shuffle seed2 deck
    putStrLn $ "Shuffled with seed " ++ show seed2 ++ " (first 13):"
    putStrLn $ "  " ++ intercalate " " (map show $ take 13 shuffled2)
    putStrLn ""

    -- Verify determinism
    let shuffled1Again = shuffle seed1 deck
    putStrLn "Determinism verification:"
    putStrLn $ "  Same seed produces same shuffle: " ++ show (shuffled1 == shuffled1Again)
    putStrLn $ "  Different seeds produce different shuffles: " ++ show (shuffled1 /= shuffled2)
    putStrLn ""

    -- Verify permutation property
    putStrLn "Permutation verification:"
    putStrLn $ "  Shuffle preserves all cards: " ++ show (verifyPermutation deck shuffled1)
    putStrLn $ "  Original deck size: " ++ show (length deck)
    putStrLn $ "  Shuffled deck size: " ++ show (length shuffled1)
    putStrLn ""

    -- Demonstrate seed combination for multiplayer
    putStrLn "=== Multiplayer Seed Generation ==="
    putStrLn ""

    let playerSeeds = [111, 222, 333] :: [Seed]
    let combinedSeed = combineSeeds playerSeeds

    putStrLn "Player seeds:"
    mapM_ (\(i, s) -> putStrLn $ "  Player " ++ show i ++ ": " ++ show s)
          (zip [1..] playerSeeds)
    putStrLn $ "Combined seed: " ++ show combinedSeed
    putStrLn ""

    -- Commitment scheme demonstration
    putStrLn "=== Commitment Scheme ==="
    putStrLn ""

    let playerSeed = 42 :: Seed
    let commitment = commit playerSeed

    putStrLn $ "Player's secret seed: " ++ show playerSeed
    putStrLn $ "Commitment (broadcast first): " ++ show commitment
    putStrLn $ "Verification after reveal: " ++ show (verifyCommitment playerSeed commitment)
    putStrLn $ "Wrong seed verification: " ++ show (verifyCommitment 43 commitment)
