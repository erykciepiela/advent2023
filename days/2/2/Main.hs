module Main (main) where

import DrawingGame

main :: IO ()
main = do
    input <- readFile "days/2/input.txt"
    print $ sumOfPowersOfSetOfCubes input
