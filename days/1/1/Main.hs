module Main (main) where

import Calibration

main :: IO ()
main = do
    input <- readFile "days/1/input"
    print $ calibrationSum input
