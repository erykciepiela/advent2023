module Race where

import Text.Parsec
import Data.Functor
import qualified Data.Map as Map
import Data.Foldable
import Data.Maybe
import Data.Function
import Utils
import Data.Sequence (mapWithIndex, fromList, sort)
import Control.Monad (join, replicateM)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow
import Data.List (groupBy, sortOn, singleton)

-- >>> answer 2023 6 1 productOfWinningPushes
-- 1083852
productOfWinningPushes input =
    let
        (Right records) = parse recordParser "" input
            where
                recordParser :: Parsec String u [Record]
                recordParser = do
                    string "Time:"
                    times <- many $ many (char ' ') *> (read <$> many1 digit)
                    string "\nDistance:"
                    distances <- many $ many (char ' ') *> (read <$> many1 digit)
                    string "\n"
                    pure $ zipWith Record times distances
    in product $ numberOfWinningPushes <$> records

numberOfWinningPushes :: Record -> Int
numberOfWinningPushes record = length [ p | p <- [1..(record.t - 1)], p * (record.t - p) > record.s ]

data Record = Record { t :: Int, s :: Int } deriving Show

-- >>> answer 2023 6 2 productOfWinningPushesWithCorrectedNote
-- 23501589
productOfWinningPushesWithCorrectedNote input =
    let
        (Right record) = parse recordParser "" input
            where
                recordParser :: Parsec String u Record
                recordParser = do
                    string "Time:"
                    time <- read . join <$> many (many (char ' ') *> many1 digit)
                    string "\nDistance:"
                    distance <- read . join <$> many (many (char ' ') *> many1 digit)
                    string "\n"
                    pure $ Record time distance
    in numberOfWinningPushes record
