module Scratchpads where

import Text.Parsec
import qualified Data.Map as Map
import Data.Foldable
import Utils
import Data.Sequence (mapWithIndex, fromList)
import Control.Monad (replicateM)
import qualified Data.Set as Set

-- >>> answer 2023 4 1 pileOfCardsWorth
-- 21105
pileOfCardsWorth input = sum $ (\case
    0 -> 0
    n -> 2 ^ (n - 1)). cardWorth <$> lines input

cardWorth :: String -> Int
cardWorth card = cast $ parse cardParser "" card

cardParser :: Parsec   String   u   Int
cardParser = do
    string "Card"
    many1 (char ' ')
    many1 digit
    string ": "
    (winningNumbers :: [Int]) <- replicateM 10 $ do
        many (char ' ')
        read <$> many1 digit
    string " | "
    (numbersIHave :: [Int]) <- replicateM 25 $ do
        many (char ' ')
        read <$> many1 digit
    pure $ Set.size $ Set.intersection (Set.fromList winningNumbers) (Set.fromList numbersIHave)
    
-- >>> answer 2023 4 2 totalScratchpadNumber
-- 5329815
totalScratchpadNumber input =
    let cardWorths = Map.fromAscList $ toList $ mapWithIndex (\i card -> (i + 1, (+) (i + 1) <$> [1..cardWorth card])) (fromList $ lines input)
    in length $ Map.keys cardWorths >>= closure cardWorths

-- >>> closure (Map.fromList [(1, [2]), (2, [])]) 1
-- [1,2]
closure :: Ord v => Map.Map v [v] -> v -> [v]
closure map key = key : (cast (Map.lookup key map) >>= closure map)
