{-|
Module      : CardLang.Primitives.Types
Description : Core primitive types for CardLang
Copyright   : (c) 2026
License     : MIT

This module defines the four primitive types that form the foundation of CardLang:
Rank, Suit, Card, and Deck.
-}

module CardLang.Primitives.Types where

import Data.List (intercalate)

-- | Rank represents the face value of a card (2 through Ace)
-- The set of ranks R = {2, 3, 4, 5, 6, 7, 8, 9, T, J, Q, K, A} with |R| = 13
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | RT | RJ | RQ | RK | RA
    deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
    show R2 = "2"
    show R3 = "3"
    show R4 = "4"
    show R5 = "5"
    show R6 = "6"
    show R7 = "7"
    show R8 = "8"
    show R9 = "9"
    show RT = "T"
    show RJ = "J"
    show RQ = "Q"
    show RK = "K"
    show RA = "A"

-- | Suit represents the four suits of a standard deck
-- The set of suits S = {Clubs, Diamonds, Hearts, Spades} with |S| = 4
data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Eq, Ord, Enum, Bounded)

instance Show Suit where
    show Clubs    = "c"
    show Diamonds = "d"
    show Hearts   = "h"
    show Spades   = "s"

-- | A Card is a product type of Rank and Suit
-- The set of standard cards is C = R x S with |C| = 52
data Card = Card Rank Suit
    deriving (Eq)

instance Show Card where
    show (Card r s) = show r ++ show s

instance Ord Card where
    compare (Card r1 s1) (Card r2 s2) =
        case compare r1 r2 of
            EQ -> compare s1 s2
            x  -> x

-- | A Deck is an ordered sequence of cards
-- A deck D is a sequence (c_0, c_1, ..., c_{n-1}) where each c_i in C
type Deck = [Card]

-- | Joker card for games that use them
data ExtendedCard = StandardCard Card | Joker
    deriving (Eq, Show)

type ExtendedDeck = [ExtendedCard]

-- | Encode a card as a single byte (0-51 for standard deck)
-- encode(Card r s) = fromEnum(r) + fromEnum(s) * 13
encodeCard :: Card -> Int
encodeCard (Card r s) = fromEnum r + fromEnum s * 13

-- | Decode a byte back to a card
decodeCard :: Int -> Card
decodeCard n = Card (toEnum $ n `mod` 13) (toEnum $ n `div` 13)

-- | Get the rank of a card
rank :: Card -> Rank
rank (Card r _) = r

-- | Get the suit of a card
suit :: Card -> Suit
suit (Card _ s) = s

-- | All ranks in order
allRanks :: [Rank]
allRanks = [minBound .. maxBound]

-- | All suits in order
allSuits :: [Suit]
allSuits = [minBound .. maxBound]

-- | The standard 52-card deck in canonical order
-- Generates the Cartesian product R x S
standard52 :: Deck
standard52 = [Card r s | s <- allSuits, r <- allRanks]

-- | Pretty print a deck
showDeck :: Deck -> String
showDeck deck = "[" ++ intercalate ", " (map show deck) ++ "]"

-- | Example usage and tests
main :: IO ()
main = do
    putStrLn "=== CardLang Primitive Types ==="
    putStrLn ""

    -- Show all ranks
    putStrLn "All Ranks:"
    putStrLn $ "  " ++ unwords (map show allRanks)
    putStrLn $ "  Count: " ++ show (length allRanks)
    putStrLn ""

    -- Show all suits
    putStrLn "All Suits:"
    putStrLn $ "  " ++ unwords (map show allSuits)
    putStrLn $ "  Count: " ++ show (length allSuits)
    putStrLn ""

    -- Show some example cards
    putStrLn "Example Cards:"
    let aceOfSpades = Card RA Spades
    let twoOfClubs = Card R2 Clubs
    let queenOfHearts = Card RQ Hearts
    putStrLn $ "  Ace of Spades: " ++ show aceOfSpades
    putStrLn $ "  Two of Clubs: " ++ show twoOfClubs
    putStrLn $ "  Queen of Hearts: " ++ show queenOfHearts
    putStrLn ""

    -- Demonstrate byte encoding
    putStrLn "Byte Encoding:"
    putStrLn $ "  2c -> " ++ show (encodeCard twoOfClubs) ++ " (first card)"
    putStrLn $ "  As -> " ++ show (encodeCard aceOfSpades) ++ " (last card)"
    putStrLn $ "  Decode 0 -> " ++ show (decodeCard 0)
    putStrLn $ "  Decode 51 -> " ++ show (decodeCard 51)
    putStrLn ""

    -- Standard deck
    putStrLn "Standard 52-Card Deck:"
    putStrLn $ "  Size: " ++ show (length standard52)
    putStrLn $ "  First 13 (Clubs): " ++ showDeck (take 13 standard52)
    putStrLn $ "  Last 13 (Spades): " ++ showDeck (drop 39 standard52)
    putStrLn ""

    -- Verify encoding roundtrip
    let allEncodings = map encodeCard standard52
    let decoded = map decodeCard allEncodings
    putStrLn "Encoding Verification:"
    putStrLn $ "  All cards encode to unique bytes: " ++ show (allEncodings == [0..51])
    putStrLn $ "  Roundtrip successful: " ++ show (decoded == standard52)
