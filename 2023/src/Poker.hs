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

-- >>> answer 2023 7 1 totalWinnings
-- 252039365
totalWinnings input =
    let hands = input `parsed` do
            flip sepEndBy (char '\n') $ do
                cards <- replicateM 5 anyChar
                char ' '
                bid <- read <$> many1 digit
                pure $ Hand cards bid
    in sum $ winning hands <$> hands

data Hand = Hand
    { cards :: String
    , bid :: Int
    } deriving (Show, Eq)

winning :: [Hand] -> Hand -> Int
winning hands hand = hand.bid * rank (sortOn (Down . type') $ sortBy compareCards $ cards <$> hands) hand.cards

rank :: [String] -> String -> Int
rank otherCards cards = cast (elemIndex cards otherCards) + 1

type' :: String -> Int
type' cards = 1

-- >>> compareCards "33332" "33AAA"
-- LT
compareCards :: String -> String -> Ordering
compareCards [] [] = error "no cards to compare"
compareCards cards1 cards2 = case compare (cardValue (head cards1)) (cardValue (head cards2)) of
    EQ -> compareCards (tail cards1) (tail cards2)
    o -> o

cardValue :: Char -> Int
cardValue = \case
    'A' -> 12
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

