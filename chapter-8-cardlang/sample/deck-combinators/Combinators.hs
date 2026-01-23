{-|
Module      : CardLang.DeckCombinators
Description : Higher-order functions for constructing decks
Copyright   : (c) 2026
License     : MIT

This module provides composable combinators for building deck variants
from the primitive types.
-}

module CardLang.DeckCombinators where

import Data.List (intercalate)

-- Primitive types (would normally import from Types.hs)
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | RT | RJ | RQ | RK | RA
    deriving (Eq, Ord, Enum, Bounded, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Eq, Ord, Enum, Bounded, Show)

data Card = Card Rank Suit
    deriving (Eq)

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

-- | The standard 52-card deck
standard52 :: Deck
standard52 = [Card r s | s <- [Clubs ..], r <- [R2 ..]]

-- | Build a deck from rank and suit predicates
-- This is the fundamental combinator for deck construction
deckWhere :: (Rank -> Bool) -> (Suit -> Bool) -> Deck
deckWhere rankP suitP =
    [Card r s | s <- filter suitP [Clubs ..]
              , r <- filter rankP [R2 ..]]

-- | Duplicate each card n times (for Pinochle, etc.)
duplicated :: Int -> Deck -> Deck
duplicated n = concatMap (replicate n)

-- | Filter a deck by a card predicate
restricted :: (Card -> Bool) -> Deck -> Deck
restricted = filter

-- | Combine multiple decks
combined :: [Deck] -> Deck
combined = concat

-- | Remove specific cards from a deck
without :: [Card] -> Deck -> Deck
without cards = filter (`notElem` cards)

-- | Keep only specific ranks
ranksOnly :: [Rank] -> Deck -> Deck
ranksOnly ranks = filter (\(Card r _) -> r `elem` ranks)

-- | Keep only specific suits
suitsOnly :: [Suit] -> Deck -> Deck
suitsOnly suits = filter (\(Card _ s) -> s `elem` suits)

-- ============================================================
-- Example Deck Definitions
-- ============================================================

-- | Euchre deck: 9 through A (24 cards)
euchre :: Deck
euchre = deckWhere (>= R9) (const True)

-- | Pinochle deck: 9 through A, doubled (48 cards)
pinochle :: Deck
pinochle = duplicated 2 $ deckWhere (>= R9) (const True)

-- | Piquet deck: 7 through A (32 cards)
piquet :: Deck
piquet = deckWhere (>= R7) (const True)

-- | Bezique deck: 7 through A, doubled (64 cards)
bezique :: Deck
bezique = duplicated 2 $ deckWhere (>= R7) (const True)

-- | Blackjack shoe: 6 standard decks (312 cards)
blackjackShoe :: Deck
blackjackShoe = duplicated 6 standard52

-- | Spanish deck: standard deck without 8s, 9s, 10s (40 cards)
spanish40 :: Deck
spanish40 = deckWhere (\r -> r < R8 || r > RT) (const True)

-- | Red cards only
redCards :: Deck
redCards = deckWhere (const True) (`elem` [Hearts, Diamonds])

-- | Black cards only
blackCards :: Deck
blackCards = deckWhere (const True) (`elem` [Clubs, Spades])

-- | Face cards only (J, Q, K)
faceCards :: Deck
faceCards = deckWhere (`elem` [RJ, RQ, RK]) (const True)

-- | Number cards only (2-10)
numberCards :: Deck
numberCards = deckWhere (<= RT) (const True)

-- | 500 deck: complex filtering plus joker would be added
-- Red suits: 7 and above
-- Black suits: 5 and above
-- Plus 4h, 4d, and a Joker
fiveHundred :: Deck
fiveHundred = redPart ++ blackPart ++ [Card R4 Hearts, Card R4 Diamonds]
  where
    redPart   = deckWhere (>= R7) (`elem` [Hearts, Diamonds])
    blackPart = deckWhere (>= R5) (`elem` [Spades, Clubs])

-- | Pretty print deck with count
showDeck :: String -> Deck -> IO ()
showDeck name deck = do
    putStrLn $ name ++ " (" ++ show (length deck) ++ " cards):"
    putStrLn $ "  " ++ intercalate " " (map show deck)
    putStrLn ""

-- | Example usage
main :: IO ()
main = do
    putStrLn "=== CardLang Deck Combinators ==="
    putStrLn ""

    showDeck "Standard 52" standard52
    showDeck "Euchre (9-A)" euchre
    showDeck "Pinochle (9-A doubled)" pinochle
    showDeck "Piquet (7-A)" piquet
    showDeck "Spanish 40 (no 8,9,T)" spanish40
    showDeck "Red Cards" redCards
    showDeck "Face Cards" faceCards
    showDeck "500 Deck" fiveHundred

    -- Demonstrate combinator composition
    putStrLn "=== Combinator Composition ==="
    putStrLn ""

    -- High cards (T-A) in red suits
    let highRed = deckWhere (>= RT) (`elem` [Hearts, Diamonds])
    showDeck "High Red (T-A in Hearts/Diamonds)" highRed

    -- Custom filter: cards whose rank ordinal equals suit ordinal mod 4
    let customDeck = restricted (\(Card r s) -> fromEnum r `mod` 4 == fromEnum s) standard52
    showDeck "Custom (rank mod 4 == suit)" customDeck

    -- Multi-deck for casino blackjack
    putStrLn $ "Blackjack 6-deck shoe: " ++ show (length blackjackShoe) ++ " cards"
    putStrLn $ "  Ace of Spades count: " ++ show (length $ filter (== Card RA Spades) blackjackShoe)
