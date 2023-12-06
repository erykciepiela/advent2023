module Race where

import Text.Parsec
import Utils
import Control.Monad (join)

-- >>> answer 2023 6 1 productOfWinningPushes
-- 1083852
productOfWinningPushes input = product $ numberOfWinningPushes <$> records
    where
        records = input `parsed` do
            string "Time:"
            times <- many $ many (char ' ') *> (read <$> many1 digit)
            string "\nDistance:"
            distances <- many $ many (char ' ') *> (read <$> many1 digit)
            string "\n"
            pure $ zipWith Record times distances

numberOfWinningPushes :: Record -> Int
numberOfWinningPushes record = length [ p | p <- [1..(record.t - 1)], p * (record.t - p) > record.s ]

data Record = Record { t :: Int, s :: Int } deriving Show

-- >>> answer 2023 6 2 productOfWinningPushesWithCorrectedNote
-- 23501589
productOfWinningPushesWithCorrectedNote input = numberOfWinningPushes record
    where  
        record = input `parsed` do
            string "Time:"
            time <- read . join <$> many (many (char ' ') *> many1 digit)
            string "\nDistance:"
            distance <- read . join <$> many (many (char ' ') *> many1 digit)
            string "\n"
            pure $ Record time distance

