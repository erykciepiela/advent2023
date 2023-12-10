module PipeMaze where

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
import Debug.Trace (trace)

data Dir = N | W | S | E deriving Show

-- >>> answer 2023 10 1 stepsToFurthestPosition
-- 6890
stepsToFurthestPosition input = loopLength (parseBoard input) ((95, 101), N) `div` 2

parseBoard :: String -> [[Dir -> Maybe ((Int, Int), Dir)]]
parseBoard input = mapWithIndex (lines input) $ \y row -> mapWithIndex row $ \x cell -> case cell of
    'S' -> const Nothing
    '|' -> \case
        S -> Just ((y+1, x), S)
        N -> Just ((y-1, x), N)
        dir -> error ("!" <> show y <> ":" <> show x <> " " <> show cell <> " " <> show dir)
    '-' -> \case 
        E -> Just ((y, x+1), E)
        W -> Just ((y, x-1), W)
        dir -> error ("!" <> show y <> ":" <> show x <> " " <> show cell <> " " <> show dir)
    'L' -> \case 
        S -> Just ((y, x+1), E)
        W -> Just ((y-1, x), N)
        dir -> error ("!" <> show y <> ":" <> show x <> " " <> show cell <> " " <> show dir)
    'J' -> \case 
        S -> Just ((y, x-1), W)
        E -> Just ((y-1, x), N)
        dir -> error ("!" <> show y <> ":" <> show x <> " " <> show cell <> " " <> show dir)
    '7' -> \case 
        N -> Just ((y, x-1), W)
        E -> Just ((y+1, x), S)
        dir -> error ("!" <> show y <> ":" <> show x <> " " <> show cell <> " " <> show dir)
    'F' -> \case 
        N -> Just ((y, x+1), E)
        W -> Just ((y+1, x), S)
        dir -> error ("!" <> show y <> ":" <> show x <> " " <> show cell <> " " <> show dir)
    cell -> \dir -> error ("!" <> show y <> ":" <> show x <> " " <> show cell <> " " <> show dir)


-- >>> loopLength (parseBoard ".....\n.S-7.\n.|.|.\n.L-J.\n.....") ((1,2), E)
-- 8
loopLength :: [[Dir -> Maybe ((Int, Int), Dir)]] -> ((Int, Int), Dir) -> Int
loopLength board ((y, x), dir) = 1 + case (board !! y !! x) dir of
    Nothing -> 0
    Just n -> trace (show n) loopLength board n

mapWithIndex :: [a] -> (Int -> a -> b) -> [b]
mapWithIndex as f = zipWith f [0..] as

-- >>> answer 2023 10 2 stepsToFurthestPosition2
-- 1
stepsToFurthestPosition2 input = 1

