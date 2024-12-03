module HistorianHysteria where

import Utils

import Data.List (sort)
import Data.Function (on)

-- >>> answer 2024 1 1 totalDistance
-- 2166959
totalDistance :: String -> Int
totalDistance input = sum $ (zipWith (\a b -> abs (a - b)) `on` sort) list1 list2
    where
        list1 = read . (!! 0) . words <$> lines input
        list2 = read . (!! 1) . words <$> lines input

-- >>> answer 2024 1 2 similarityScore
-- 23741109
similarityScore :: String -> Int
similarityScore input = sum $ (\a -> a * occurrences a list2) <$> list1 
  where
    occurrences element = length . filter (== element)
    list1 = read . (!! 0) . words <$> lines input
    list2 = read . (!! 1) . words <$> lines input
