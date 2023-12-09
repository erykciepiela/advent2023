module Mirage where

import Text.Parsec
import Utils

-- >>> answer 2023 9 1 sumOfExtrapolatedValues
-- 1731106378
sumOfExtrapolatedValues input = sum $ extrapolatedValue <$> sequences
    where
        sequences :: [[Int]]
        sequences = input `parsed` do
            flip sepEndBy1 (char '\n') $ do
                flip sepEndBy1 (char ' ') $ do
                    read <$> many1 (char '-' <|> digit)

-- >>> extrapolatedValue [0, 0, 0]
-- >>> extrapolatedValue [0, 1, 2]
-- >>> extrapolatedValue [10,  13,  16,  21,  30,  45]
-- 0
-- 3
-- 68
extrapolatedValue :: [Int] -> Int
extrapolatedValue seq = let 
    d = deltas seq
    in if all (== 0) seq then 0 else last seq + extrapolatedValue d

-- >>> delta [10,  13,  16,  21,  30,  45,  68]
-- [3,3,5,9,15,23]
deltas seq = zipWith (-) (tail seq) seq 


-- >>> answer 2023 9 2 sumOfExtrapolatedPreviousValues
-- 1087
sumOfExtrapolatedPreviousValues input = sum $ extrapolatedPreviousValue <$> sequences
    where
        sequences :: [[Int]]
        sequences = input `parsed` do
            flip sepEndBy1 (char '\n') $ do
                flip sepEndBy1 (char ' ') $ do
                    read <$> many1 (char '-' <|> digit)

-- >>> extrapolatedPreviousValue [0, 0]
-- >>> extrapolatedPreviousValue [1, 2]
-- >>> extrapolatedPreviousValue [10,  13,  16,  21,  30,  45]
-- 0
-- 0
-- 5
extrapolatedPreviousValue :: [Int] -> Int
extrapolatedPreviousValue seq = let 
    d = deltas seq
    in if all (== 0) seq then 0 else head seq - extrapolatedPreviousValue d
