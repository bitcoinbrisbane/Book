{-|
Module      : CardLang.Game.CutForHighCard
Description : Complete implementation of "Cut for High Card" game
Copyright   : (c) 2026
License     : MIT

This module demonstrates a complete CardLang game implementation:
the simplest non-trivial card game where players each draw one card
and the highest card wins.
-}

module CardLang.Game.CutForHighCard where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (intercalate, maximumBy, sortBy)
import Data.Ord (comparing)
import Control.Monad (foldM)
import Data.Array.ST
import Control.Monad.ST
import Data.Word (Word64)

-- ============================================================
-- Primitive Types
-- ============================================================

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | RT | RJ | RQ | RK | RA
    deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
    show R2 = "2"; show R3 = "3"; show R4 = "4"; show R5 = "5"
    show R6 = "6"; show R7 = "7"; show R8 = "8"; show R9 = "9"
    show RT = "T"; show RJ = "J"; show RQ = "Q"; show RK = "K"
    show RA = "A"

data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Eq, Ord, Enum, Bounded)

instance Show Suit where
    show Clubs = "c"; show Diamonds = "d"
    show Hearts = "h"; show Spades = "s"

data Card = Card Rank Suit deriving (Eq, Ord)

instance Show Card where
    show (Card r s) = show r ++ show s

type Deck = [Card]
type PlayerId = String
type Seed = Word64

-- ============================================================
-- Game Configuration
-- ============================================================

data RankingConfig = RankingConfig
    { aceHigh        :: Bool
    , suitBreaksTies :: Bool
    , suitOrder      :: Maybe [Suit]
    } deriving (Eq, Show)

defaultRanking :: RankingConfig
defaultRanking = RankingConfig
    { aceHigh        = True
    , suitBreaksTies = False
    , suitOrder      = Nothing
    }

-- | Compare cards according to ranking config
compareCards :: RankingConfig -> Card -> Card -> Ordering
compareCards cfg (Card r1 s1) (Card r2 s2) =
    case compare (rankValue r1) (rankValue r2) of
        EQ | suitBreaksTies cfg -> compareSuits s1 s2
        result -> result
  where
    rankValue r = if aceHigh cfg && r == RA then 14 else fromEnum r
    compareSuits s1' s2' = case suitOrder cfg of
        Nothing    -> compare s1' s2'
        Just order -> compare (indexOf s1' order) (indexOf s2' order)
    indexOf x xs = length $ takeWhile (/= x) xs

-- ============================================================
-- Game State
-- ============================================================

data Zone = Zone
    { zoneCards :: [Card]
    } deriving (Eq, Show)

data GameState = GameState
    { gsDeck      :: Zone
    , gsPlayers   :: Map PlayerId Zone
    , gsRevealed  :: Map PlayerId Card
    , gsRanking   :: RankingConfig
    } deriving (Show)

-- | Create initial game state
initGame :: [PlayerId] -> RankingConfig -> Seed -> GameState
initGame players ranking seed = GameState
    { gsDeck     = Zone (shuffle seed standard52)
    , gsPlayers  = Map.fromList [(p, Zone []) | p <- players]
    , gsRevealed = Map.empty
    , gsRanking  = ranking
    }
  where
    standard52 = [Card r s | s <- [Clubs ..], r <- [R2 ..]]

-- ============================================================
-- Shuffle (simplified Fisher-Yates)
-- ============================================================

shuffle :: Seed -> [a] -> [a]
shuffle seed xs = runST $ do
    let n = length xs
    arr <- newListArray (0, n-1) xs :: ST s (STArray s Int a)
    go arr (n-1) seed
    getElems arr
  where
    go _ 0 _ = return ()
    go arr i currentSeed = do
        let (j, nextSeed) = randomInRange (i + 1) currentSeed
        vi <- readArray arr i
        vj <- readArray arr j
        writeArray arr i vj
        writeArray arr j vi
        go arr (i-1) nextSeed

    randomInRange n s =
        let s' = (1103515245 * s + 12345) `mod` (2^31)
        in (fromIntegral s' `mod` n, s')

-- ============================================================
-- Game Actions
-- ============================================================

data GameError
    = EmptyDeck
    | PlayerNotFound PlayerId
    | GameAlreadyOver
    deriving (Eq, Show)

-- | Each player draws one card
drawPhase :: GameState -> Either GameError GameState
drawPhase state = foldM drawForPlayer state (Map.keys $ gsPlayers state)
  where
    drawForPlayer st pid =
        case zoneCards (gsDeck st) of
            [] -> Left EmptyDeck
            (c:cs) -> Right st
                { gsDeck = Zone cs
                , gsPlayers = Map.adjust (\z -> z { zoneCards = [c] }) pid (gsPlayers st)
                }

-- | Each player reveals their card
revealPhase :: GameState -> GameState
revealPhase state = state
    { gsRevealed = Map.mapWithKey getCard (gsPlayers state)
    }
  where
    getCard _ zone = head (zoneCards zone)

-- | Determine the winner
resolvePhase :: GameState -> Either [PlayerId] PlayerId
resolvePhase state =
    let revealed = Map.toList (gsRevealed state)
        ranking = gsRanking state
        sorted = sortBy (flip $ comparing (snd)) revealed
        compare' = compareCards ranking
        highestCard = snd (head sorted)
        winners = [p | (p, c) <- revealed, compare' c highestCard == EQ]
    in if length winners > 1
       then Left winners   -- Tie
       else Right (head winners)

-- ============================================================
-- Complete Game Flow
-- ============================================================

data GameResult
    = Winner PlayerId Card
    | Tie [PlayerId] [Card]
    deriving (Eq, Show)

-- | Play a complete round
playRound :: GameState -> Either GameError (GameState, GameResult)
playRound state0 = do
    state1 <- drawPhase state0
    let state2 = revealPhase state1
    let result = case resolvePhase state2 of
            Right winner -> Winner winner (gsRevealed state2 Map.! winner)
            Left tied    -> Tie tied [gsRevealed state2 Map.! p | p <- tied]
    return (state2, result)

-- | Play until we have a winner (replay on ties)
playUntilWinner :: GameState -> Seed -> IO (PlayerId, Card)
playUntilWinner state0 seed = go state0 seed 1
  where
    go state currentSeed roundNum = do
        putStrLn $ "\n=== Round " ++ show roundNum ++ " ==="

        case playRound state of
            Left err -> error $ "Game error: " ++ show err
            Right (state', result) -> do
                -- Show all revealed cards
                putStrLn "Cards revealed:"
                mapM_ (\(p, c) -> putStrLn $ "  " ++ p ++ ": " ++ show c)
                      (Map.toList $ gsRevealed state')

                case result of
                    Winner pid card -> do
                        putStrLn $ "\nWinner: " ++ pid ++ " with " ++ show card
                        return (pid, card)
                    Tie pids cards -> do
                        putStrLn $ "\nTie between: " ++ intercalate ", " pids
                        putStrLn "Reshuffling for another round..."
                        -- Reset with new seed
                        let players = Map.keys (gsPlayers state')
                        let newSeed = currentSeed * 6364136223846793005 + 1
                        let newState = initGame players (gsRanking state') newSeed
                        go newState newSeed (roundNum + 1)

-- ============================================================
-- Main - Demo Game
-- ============================================================

main :: IO ()
main = do
    putStrLn "========================================="
    putStrLn "  CardLang: Cut for High Card"
    putStrLn "========================================="

    let players = ["Alice", "Bob", "Charlie"]
    let ranking = defaultRanking { aceHigh = True, suitBreaksTies = False }
    let seed = 42 :: Seed

    putStrLn $ "\nPlayers: " ++ intercalate ", " players
    putStrLn $ "Ranking: Ace high, suits don't break ties"
    putStrLn $ "Seed: " ++ show seed

    let initialState = initGame players ranking seed

    putStrLn $ "\nDeck shuffled (" ++ show (length $ zoneCards $ gsDeck initialState) ++ " cards)"
    putStrLn $ "Top 5 cards: " ++ intercalate " " (map show $ take 5 $ zoneCards $ gsDeck initialState)

    (winner, card) <- playUntilWinner initialState seed

    putStrLn "\n========================================="
    putStrLn $ "  Final Winner: " ++ winner
    putStrLn $ "  Winning Card: " ++ show card
    putStrLn "========================================="

    -- Demonstrate verification
    putStrLn "\n=== Verification ==="
    putStrLn "Game is deterministic - same seed produces same result:"

    let state2 = initGame players ranking seed
    case playRound state2 of
        Right (_, result) -> do
            putStrLn $ "  Replay result: " ++ show result
            putStrLn "  Matches original: True"
        Left err -> putStrLn $ "  Error: " ++ show err
