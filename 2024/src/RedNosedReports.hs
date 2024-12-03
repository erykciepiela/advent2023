module RedNosedReports where

import Utils
import Data.Functor ((<&>))

-- >>> answer 2024 2 1 safeReportsNumber
-- 332
safeReportsNumber :: String -> Int
safeReportsNumber input = length $ filter isSafe reports
  where
    reports = lines input
    isSafe report = let numbers = read @Int <$> words report; steps = zipWith (-) numbers (tail numbers)
        in all (\step -> step >= 1 && step <= 3) steps || all (\step -> step <= -1 && step >= -3) steps

-- >>> answer 2024 2 2 safeishReportsNumber
-- 335
safeishReportsNumber :: String -> Int
safeishReportsNumber input = length $ filter isSafeish reports
  where
    reports = lines input
    isSafeish report = any isSafe (similarCollections report)
    isSafe report =
        let numbers = read @Int <$> words report; steps = zipWith (-) numbers (tail numbers)
        in all (\step -> step >= 1 && step <= 3) steps || all (\step -> step <= -1 && step >= -3) steps

-- >>> similarCollections [1,2,3]
-- [[2,3],[1,3],[1,2],[1,2,3]]
similarCollections :: [a] -> [[a]]
similarCollections as = [0..length as] <&> (\i -> take i as <> drop (i + 1) as)
