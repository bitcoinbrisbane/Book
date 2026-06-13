{-|
Module      : CardLang.Draw
Description : The draw primitive and zone management
Copyright   : (c) 2026
License     : MIT

This module implements the fundamental draw operation for transferring
cards between zones, with formal preconditions and postconditions.
-}

module CardLang.Draw where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (delete, intercalate)

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

-- ============================================================
-- Zone Types
-- ============================================================

-- | Zone identifier
type ZoneId = String

-- | Player identifier
type PlayerId = String

-- | Zone types with different visibility and ownership properties
data ZoneType
    = DeckZone      -- ^ Face down, no owner
    | HandZone      -- ^ Hidden, owned by player
    | CommunityZone -- ^ Face up, shared
    | DiscardZone   -- ^ Face up, no owner
    | BurnZone      -- ^ Face down, out of play
    deriving (Eq, Show)

-- | A Zone is a named, ordered collection of cards
data Zone = Zone
    { zoneId    :: ZoneId
    , zoneType  :: ZoneType
    , zoneCards :: [Card]
    , zoneOwner :: Maybe PlayerId
    } deriving (Eq, Show)

-- | Game state is a mapping from zone IDs to zones
type GameState = Map ZoneId Zone

-- ============================================================
-- Draw Errors
-- ============================================================

data DrawError
    = InvalidCount Int
    | InsufficientCards ZoneId Int Int  -- zone, requested, available
    | ZoneNotFound ZoneId
    | CardNotInZone Card ZoneId
    deriving (Eq, Show)

-- ============================================================
-- Draw Operations
-- ============================================================

-- | The fundamental draw operation
-- Transfers n cards from source zone to destination zone
draw :: Int -> ZoneId -> ZoneId -> GameState -> Either DrawError GameState
draw n src dst state
    | n <= 0 = Left (InvalidCount n)
    | otherwise = do
        srcZone <- maybe (Left $ ZoneNotFound src) Right (Map.lookup src state)
        _ <- maybe (Left $ ZoneNotFound dst) Right (Map.lookup dst state)
        let srcCards = zoneCards srcZone
        if n > length srcCards
            then Left (InsufficientCards src n (length srcCards))
            else Right $ transferCards n src dst state

-- | Internal function to transfer cards
transferCards :: Int -> ZoneId -> ZoneId -> GameState -> GameState
transferCards n src dst state =
    let srcZone = state Map.! src
        dstZone = state Map.! dst
        (drawn, rest) = splitAt n (zoneCards srcZone)
        srcZone' = srcZone { zoneCards = rest }
        dstZone' = dstZone { zoneCards = zoneCards dstZone ++ drawn }
    in Map.insert dst dstZone' $ Map.insert src srcZone' state

-- | Draw a single card, returning the card and new state
draw1 :: ZoneId -> ZoneId -> GameState -> Either DrawError (Card, GameState)
draw1 src dst state = do
    let srcZone = state Map.! src
    let card = head $ zoneCards srcZone
    state' <- draw 1 src dst state
    return (card, state')

-- | Draw from the bottom of the deck
drawBottom :: Int -> ZoneId -> ZoneId -> GameState -> Either DrawError GameState
drawBottom n src dst state
    | n <= 0 = Left (InvalidCount n)
    | otherwise = do
        srcZone <- maybe (Left $ ZoneNotFound src) Right (Map.lookup src state)
        _ <- maybe (Left $ ZoneNotFound dst) Right (Map.lookup dst state)
        let srcCards = zoneCards srcZone
        if n > length srcCards
            then Left (InsufficientCards src n (length srcCards))
            else Right $ transferFromBottom n src dst state

transferFromBottom :: Int -> ZoneId -> ZoneId -> GameState -> GameState
transferFromBottom n src dst state =
    let srcZone = state Map.! src
        dstZone = state Map.! dst
        srcCards = zoneCards srcZone
        (rest, drawn) = splitAt (length srcCards - n) srcCards
        srcZone' = srcZone { zoneCards = rest }
        dstZone' = dstZone { zoneCards = zoneCards dstZone ++ drawn }
    in Map.insert dst dstZone' $ Map.insert src srcZone' state

-- | Draw a specific card (for games with choice)
drawCard :: Card -> ZoneId -> ZoneId -> GameState -> Either DrawError GameState
drawCard card src dst state = do
    srcZone <- maybe (Left $ ZoneNotFound src) Right (Map.lookup src state)
    _ <- maybe (Left $ ZoneNotFound dst) Right (Map.lookup dst state)
    if card `notElem` zoneCards srcZone
        then Left (CardNotInZone card src)
        else Right $ transferSpecificCard card src dst state

transferSpecificCard :: Card -> ZoneId -> ZoneId -> GameState -> GameState
transferSpecificCard card src dst state =
    let srcZone = state Map.! src
        dstZone = state Map.! dst
        srcZone' = srcZone { zoneCards = delete card (zoneCards srcZone) }
        dstZone' = dstZone { zoneCards = zoneCards dstZone ++ [card] }
    in Map.insert dst dstZone' $ Map.insert src srcZone' state

-- | Draw all cards from a zone
drawAll :: ZoneId -> ZoneId -> GameState -> GameState
drawAll src dst state =
    let srcZone = state Map.! src
        dstZone = state Map.! dst
        srcCards = zoneCards srcZone
        srcZone' = srcZone { zoneCards = [] }
        dstZone' = dstZone { zoneCards = zoneCards dstZone ++ srcCards }
    in Map.insert dst dstZone' $ Map.insert src srcZone' state

-- ============================================================
-- Draw Record (for verification)
-- ============================================================

data DrawRecord = DrawRecord
    { drCount   :: Int
    , drSource  :: ZoneId
    , drDest    :: ZoneId
    , drCards   :: [Card]
    , drIndices :: [Int]
    } deriving (Eq, Show)

-- | Draw with audit trail
drawWithRecord :: Int -> ZoneId -> ZoneId -> GameState
               -> Either DrawError (GameState, DrawRecord)
drawWithRecord n src dst state = do
    let srcCards = zoneCards (state Map.! src)
    state' <- draw n src dst state
    let drawn = take n srcCards
        record = DrawRecord n src dst drawn [0..n-1]
    return (state', record)

-- ============================================================
-- Deal Operations
-- ============================================================

-- | Deal n cards to each destination in order
deal :: Int -> ZoneId -> [ZoneId] -> GameState -> Either DrawError GameState
deal n src dsts state0 = foldM dealOne state0 dsts
  where
    dealOne state dst = draw n src dst state
    foldM _ z [] = Right z
    foldM f z (x:xs) = f z x >>= \z' -> foldM f z' xs

-- | Deal one card at a time, rotating through destinations
dealRound :: Int -> ZoneId -> [ZoneId] -> GameState -> Either DrawError GameState
dealRound rounds src dsts state0 = foldM dealOne state0 allDeals
  where
    allDeals = concat $ replicate rounds dsts
    dealOne state dst = draw 1 src dst state
    foldM _ z [] = Right z
    foldM f z (x:xs) = f z x >>= \z' -> foldM f z' xs

-- ============================================================
-- Verification Functions
-- ============================================================

-- | Total cards across all zones
totalCards :: GameState -> Int
totalCards = sum . map (length . zoneCards) . Map.elems

-- | Verify card conservation
verifyConservation :: GameState -> GameState -> Bool
verifyConservation before after = totalCards before == totalCards after

-- ============================================================
-- Example Usage
-- ============================================================

-- | Create initial game state for a poker game
createPokerState :: GameState
createPokerState = Map.fromList
    [ ("deck", Zone "deck" DeckZone deck52 Nothing)
    , ("player1", Zone "player1" HandZone [] (Just "Player1"))
    , ("player2", Zone "player2" HandZone [] (Just "Player2"))
    , ("community", Zone "community" CommunityZone [] Nothing)
    , ("burn", Zone "burn" BurnZone [] Nothing)
    ]
  where
    deck52 = [Card r s | s <- [Clubs ..], r <- [R2 ..]]

showZone :: Zone -> String
showZone z = zoneId z ++ " (" ++ show (length $ zoneCards z) ++ "): " ++
             intercalate " " (map show $ zoneCards z)

main :: IO ()
main = do
    putStrLn "=== CardLang Draw Primitive ==="
    putStrLn ""

    let state0 = createPokerState

    putStrLn "Initial state:"
    putStrLn $ "  Deck: " ++ show (length $ zoneCards $ state0 Map.! "deck") ++ " cards"
    putStrLn $ "  Total cards: " ++ show (totalCards state0)
    putStrLn ""

    -- Deal 2 cards to each player
    putStrLn "Dealing 2 cards to each player..."
    case dealRound 2 "deck" ["player1", "player2"] state0 of
        Left err -> putStrLn $ "Error: " ++ show err
        Right state1 -> do
            putStrLn $ "  Player1: " ++ showZone (state1 Map.! "player1")
            putStrLn $ "  Player2: " ++ showZone (state1 Map.! "player2")
            putStrLn $ "  Deck remaining: " ++ show (length $ zoneCards $ state1 Map.! "deck")
            putStrLn $ "  Conservation: " ++ show (verifyConservation state0 state1)
            putStrLn ""

            -- Burn and deal flop
            putStrLn "Burn 1, deal flop (3 to community)..."
            case draw 1 "deck" "burn" state1 >>= draw 3 "deck" "community" of
                Left err -> putStrLn $ "Error: " ++ show err
                Right state2 -> do
                    putStrLn $ "  Community: " ++ showZone (state2 Map.! "community")
                    putStrLn $ "  Burn pile: " ++ show (length $ zoneCards $ state2 Map.! "burn")
                    putStrLn $ "  Conservation: " ++ show (verifyConservation state0 state2)
                    putStrLn ""

                    -- Test error cases
                    putStrLn "=== Error Cases ==="
                    putStrLn ""

                    case draw 0 "deck" "player1" state2 of
                        Left (InvalidCount _) -> putStrLn "  draw 0: InvalidCount (expected)"
                        _ -> putStrLn "  draw 0: unexpected result"

                    case draw 100 "deck" "player1" state2 of
                        Left (InsufficientCards _ _ _) -> putStrLn "  draw 100: InsufficientCards (expected)"
                        _ -> putStrLn "  draw 100: unexpected result"

                    case draw 1 "nonexistent" "player1" state2 of
                        Left (ZoneNotFound _) -> putStrLn "  draw from nonexistent: ZoneNotFound (expected)"
                        _ -> putStrLn "  draw from nonexistent: unexpected result"
