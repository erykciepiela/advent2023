module Main (main) where

import Calibration

main :: IO ()
main = do
    input <- readFile "days/2/input"
    print input
