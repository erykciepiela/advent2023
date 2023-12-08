module Wasteland where

import Text.Parsec
import Data.Functor
import qualified Data.Map as Map
import Data.Function
import Utils
import Control.Monad
import Control.Arrow
import Data.List

-- >>> answer 2023 8 1 stepsRequired
-- Just 14429
stepsRequired input = findIndex (== "ZZZ") positions
    where
        positions = scanl nextPosition "AAA" (cycle steps)
        (nextPosition, steps) = input `parsed` do
            steps <- many1 ((read @Step <<< singleton) <$> letter)
            string "\n\n"
            nextPositions <- flip sepEndBy (char '\n') $ do
                from <- replicateM 3 letter
                string  " = ("
                toLeft <- replicateM 3 letter
                string ", "
                toRight <- replicateM 3 letter
                string ")"
                pure (from, (toLeft, toRight))
            pure (\position -> let lookup = cast $ Map.lookup position (Map.fromList nextPositions) in \case
                R -> snd lookup
                L -> fst lookup, steps)

data Step = L | R deriving (Read)

-- >>> answer 2023 8 2 stepsRequiredInParallel
-- 10921547990923
stepsRequiredInParallel input = foldr1 lcm stepNumbers
    where
        stepNumbers = initialPositions <&> \initialPosition -> cast $ findIndex ("Z" `isSuffixOf`) (positions initialPosition)
        positions initialPosition = scanl nextPosition initialPosition (cycle steps)
        (nextPosition, steps, initialPositions) = input `parsed` do
            steps <- many1 ((read @Step <<< singleton) <$> letter)
            string "\n\n"
            nextPositions <- flip sepEndBy (char '\n') $ do
                from <- replicateM 3 letter
                string  " = ("
                toLeft <- replicateM 3 letter
                string ", "
                toRight <- replicateM 3 letter
                string ")"
                pure (from, (toLeft, toRight))
            pure (\position -> let found = cast $ Map.lookup position (Map.fromList nextPositions) in \case
                R -> snd found
                L -> fst found, steps, (fst <$> nextPositions) & filter ("A" `isSuffixOf`))

