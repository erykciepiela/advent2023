module Poker where

import Text.Parsec
import Data.Function
import Utils
-- import Data.Sequence
import Control.Monad
import Data.List ( group, sortBy, sortOn )
import Data.Ord (Down(Down))
import Data.Sequence (mapWithIndex, fromList)

-- >>> answer 2023 7 1 totalWinnings
-- 252052080
totalWinnings input =
    let hands = input `parsed` do
            flip sepEndBy (char '\n') $ do
                cards <- replicateM 5 anyChar
                char ' '
                bid <- read <$> many1 digit
                pure $ Hand cards bid
    in sum $ mapWithIndex (\index hand -> winning (index + 1) hand.bid) $ fromList $ sortHands type' cardValue hands

data Hand = Hand
    { cards :: String
    , bid :: Int
    } deriving (Show, Eq)

sortHands :: (String -> Int) -> (Char -> Int) -> [Hand] -> [Hand]
sortHands typeValue cardValue = sortOn (Down . typeValue . cards) . sortBy (on (compareCards cardValue) cards)

winning :: Int -> Int -> Int
winning rank bid = rank * bid

-- >>> type' <$> ["32T3K","T55J5","KK677","KTJJT","QQQJA"]
-- [5,3,4,4,3]
type' :: String -> Int
type' cards =
    let
        sortedCards = sortOn cardValue cards
        groupedCards = group sortedCards
        countedCards = sortOn Down $ length <$> groupedCards
    in case countedCards of
        [5] -> 0
        [4, 1] -> 1
        [3, 2] -> 2
        [3, 1, 1] -> 3
        [2, 2, 1] -> 4
        [2, 1, 1, 1] -> 5
        _ -> 6

-- >>> typeWithJoker <$> ["32T3K","T55J5","KK677","KTJJT","QQQJA"]
-- [5,1,4,1,1]
-- >>> typeWithJoker "KTJJT"
-- 1
typeWithJoker :: String -> Int
typeWithJoker cards =
    let
        sortedCards = sortOn cardValue cards
        groupedCards = sortOn (Down . length) $ group sortedCards
        countedCards = length <$> groupedCards
    in case countedCards of
        [5] -> 0
        [4, 1]
            | head (groupedCards !! 0) == 'J' -> 0
            | head (groupedCards !! 1) == 'J' -> 0
            | otherwise -> 1
        [3, 2] 
            | head (groupedCards !! 0) == 'J' -> 0
            | head (groupedCards !! 1) == 'J' -> 0
            | otherwise -> 2
        [3, 1, 1] 
            | head (groupedCards !! 0) == 'J' -> 1
            | head (groupedCards !! 1) == 'J' -> 1
            | head (groupedCards !! 2) == 'J' -> 1
            | otherwise -> 3
        [2, 2, 1]
            | head (groupedCards !! 0) == 'J' -> 1
            | head (groupedCards !! 1) == 'J' -> 1
            | head (groupedCards !! 2) == 'J' -> 2
            | otherwise -> 4
        [2, 1, 1, 1]
            | head (groupedCards !! 0) == 'J' -> 3
            | head (groupedCards !! 1) == 'J' -> 3
            | head (groupedCards !! 2) == 'J' -> 3
            | head (groupedCards !! 3) == 'J' -> 3
            | otherwise -> 5
        [1, 1, 1, 1, 1]
            | head (groupedCards !! 0) == 'J' -> 5
            | head (groupedCards !! 1) == 'J' -> 5
            | head (groupedCards !! 2) == 'J' -> 5
            | head (groupedCards !! 3) == 'J' -> 5
            | head (groupedCards !! 4) == 'J' -> 5
            | otherwise -> 6
        _ -> error $ "invalid cards: " <> cards

-- >>> compareCards cardValue "33332" "33AAA"
-- LT
compareCards :: (Char -> Int) -> String -> String -> Ordering
compareCards _ [] [] = LT
compareCards cardValue cards1 cards2 = case compare (cardValue (head cards1)) (cardValue (head cards2)) of
    EQ -> compareCards cardValue (tail cards1) (tail cards2)
    o -> o

cardValue :: Char -> Int
cardValue = \case
    'A' -> 13
    'K' -> 12
    'Q' -> 11
    'J' -> 10
    'T' -> 9
    '9' -> 8
    '8' -> 7
    '7' -> 6
    '6' -> 5
    '5' -> 4
    '4' -> 3
    '3' -> 2
    '2' -> 1
    ch -> error $ "invalid card: " <> show ch

cardValueWithJoker :: Char -> Int
cardValueWithJoker = \case
    'J' -> 0
    o -> cardValue o

-- >>> answer 2023 7 2 totalWinningsWithJokers
-- 252898370
totalWinningsWithJokers input =
    let hands = input `parsed` do
            flip sepEndBy (char '\n') $ do
                cards <- replicateM 5 anyChar
                char ' '
                bid <- read <$> many1 digit
                pure $ Hand cards bid
    in sum $ mapWithIndex (\index hand -> winning (index + 1) hand.bid) $ fromList $ sortHands typeWithJoker cardValueWithJoker hands


