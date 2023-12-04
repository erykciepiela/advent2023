{-# LANGUAGE ScopedTypeVariables #-}
module New where

import Text.Parsec
import Data.Functor
import qualified Data.Map as Map
import Data.Foldable
import Data.Maybe
import Data.Function
import Utils
import Data.Sequence (mapWithIndex, fromList)
import Control.Monad (join, replicateM)
import Data.Set (Set)
import qualified Data.Set as Set

-- >>> answer 2023 4 1 pileOfCardsWorth
-- 21105
pileOfCardsWorth input = sum $ cardWorth <$> lines input

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
    let matchingNumbers = Set.size $ Set.intersection (Set.fromList winningNumbers) (Set.fromList numbersIHave)
    pure $ case matchingNumbers of
        0 -> 0
        n -> 2 ^ (n - 1)










-- >>> answer 2023 4 2 foo
bar input = 0
