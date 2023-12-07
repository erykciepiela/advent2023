module Poker where

import Text.Parsec
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
import Data.Maybe
import Data.Function
import Utils
-- import Data.Sequence
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow
import Data.List
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
    in sum $ mapWithIndex (\index hand -> winning (index + 1) hand.bid) $ fromList $ sortHands hands

data Hand = Hand
    { cards :: String
    , bid :: Int
    } deriving (Show, Eq)

sortHands :: [Hand] -> [Hand]
sortHands = sortOn (Down . type' . cards) . sortBy (on compareCards cards)

-- >>> sortCards ["32T3K","T55J5","KK677","KTJJT","QQQJA"]
-- ["32T3K","KTJJT","KK677","T55J5","QQQJA"]
sortCards :: [String] -> [String]
sortCards hands = sortOn (Down . type') $ sortBy compareCards hands

winning :: Int -> Int -> Int
winning rank bid = rank * bid

-- >>> rank ["32T3K","T55J5","KK677","KTJJT","QQQJA"]
-- rank :: Int -> String -> Int
-- rank otherCards cards = cast (elemIndex cards otherCards) + 1

-- >>> type' <$> ["32T3K","T55J5","KK677","KTJJT","QQQJA"]
-- [2,4,3,3,4]
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

-- >>> compareCards "33332" "33AAA"
-- LT
compareCards :: String -> String -> Ordering
compareCards [] [] = LT
compareCards cards1 cards2 = case compare (cardValue (head cards1)) (cardValue (head cards2)) of
    EQ -> compareCards (tail cards1) (tail cards2)
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


-- >>> answer 2023 7 2 new2
-- 1
new2 input = 1

