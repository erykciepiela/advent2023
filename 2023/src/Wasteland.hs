module Wasteland where

import Text.Parsec
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
import Data.Maybe
import Data.Function
import Utils
-- import Data.Sequence
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
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

-- >>> answer 2023 8 2 new2
-- 1
new2 input = 1

