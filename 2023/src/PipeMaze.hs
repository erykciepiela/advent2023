module PipeMaze where

import Prelude hiding (Bounded)
import Utils
-- import Data.Sequence

data Dir = N | W | S | E deriving (Show, Eq, Ord)

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
    where  
        mapWithIndex :: [a] -> (Int -> a -> b) -> [b]
        mapWithIndex as f = zipWith f [0..] as

-- >>> loopLength (parseBoard ".....\n.S-7.\n.|.|.\n.L-J.\n.....") ((1,2), E)
-- 8
loopLength :: [[Dir -> Maybe ((Int, Int), Dir)]] -> ((Int, Int), Dir) -> Int
loopLength board ((y, x), dir) = 1 + case (board !! y !! x) dir of
    Nothing -> 0
    Just n -> loopLength board n


-- >>> loop (parseBoard ".....\n.S-7.\n.|.|.\n.L-J.\n.....") ((1,2), E)
-- >>> loop (parseBoard ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ...") ((4, 13), E)
-- [(1,1),(1,2),(1,3),(2,3),(3,3),(3,2),(3,1),(2,1)]
-- [(4,12),(4,13),(5,13),(6,13),(7,13),(8,13),(9,13),(9,14),(8,14),(7,14),(7,15),(8,15),(9,15),(9,16),(8,16),(7,16),(6,16),(6,15),(5,15),(5,14),(4,14),(4,15),(4,16),(5,16),(5,17),(6,17),(6,18),(7,18),(7,19),(6,19),(5,19),(5,18),(4,18),(4,17),(3,17),(3,16),(3,15),(2,15),(2,14),(1,14),(1,15),(0,15),(0,14),(0,13),(1,13),(2,13),(3,13),(3,12),(2,12),(1,12),(0,12),(0,11),(1,11),(2,11),(3,11),(4,11),(4,10),(3,10),(2,10),(1,10),(0,10),(0,9),(1,9),(2,9),(3,9),(3,8),(2,8),(1,8),(0,8),(0,7),(1,7),(2,7),(3,7),(3,6),(2,6),(1,6),(0,6),(0,5),(0,4),(0,3),(0,2),(0,1),(1,1),(2,1),(3,1),(3,0),(4,0),(4,1),(4,2),(4,3),(3,3),(3,2),(2,2),(1,2),(1,3),(1,4),(1,5),(2,5),(2,4),(3,4),(3,5),(4,5),(4,6),(5,6),(5,5),(5,4),(6,4),(6,5),(7,5),(8,5),(8,4),(9,4),(9,5),(9,6),(9,7),(9,8),(8,8),(8,7),(8,6),(7,6),(7,7),(6,7),(6,8),(7,8),(7,9),(6,9),(5,9),(5,10),(6,10),(7,10),(8,10),(9,10),(9,11),(8,11),(7,11),(7,12),(6,12),(6,11),(5,11),(5,12)]
loop :: [[Dir -> Maybe ((Int, Int), Dir)]] -> ((Int, Int), Dir) -> [((Int, Int))]
loop board start = let l = goLoop board start in (finish start):l
    where
        finish ((y, x), E) = (y, x - 1)
        finish ((y, x), W) = (y, x + 1)
        finish ((y, x), N) = (y + 1, x)
        finish ((y, x), S) = (y - 1, x)

goLoop :: [[Dir -> Maybe ((Int, Int), Dir)]] -> ((Int, Int), Dir) -> [(Int, Int)]
goLoop board ((y, x), dir) = case (board !! y !! x) dir of
    Nothing -> []
    Just new@(_, _) -> (y, x): goLoop board new

-- >>> area $ loop (parseBoard ".....\n.S-7.\n.|.|.\n.L-J.\n.....") ((1,2), E)
-- >>> area $ loop (parseBoard ".....\n.S-7.\n.|.|.\n.L-J.\n.....") ((2,1), S)
-- >>> area $ loop (parseBoard "...........\n.S-------7.\n.|F-----7|.\n.||OOOOO||.\n.||OOOOO||.\n.|L-7OF-J|.\n.|II|O|II|.\n.L--JOL--J.\n.....O.....") ((1,2), E)
-- >>> area $ loop (parseBoard "..........\n.S------7.\n.|F----7|.\n.||OOOO||.\n.||OOOO||.\n.|L-7F-J|.\n.|II||II|.\n.L--JL--J.\n..........") ((1,2), E)
-- >>> area $ loop (parseBoard ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ...") ((4, 13), E)
-- >>> area $ loop (parseBoard "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJIF7FJ-\nL---JF-JLJIIIIFJLJJ7\n|F|F-JF---7IIIL7L|7|\n|FFJF7L7F-JF7IIL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L") ((1, 4), S)
-- 1
-- 1
-- 4
-- 4
-- 8
-- 10
area :: [(Int, Int)] -> Int
area loop = abs (sum (zipWith (\(x1, y1) (x2, y2) -> x1 * y2 - x2 * y1) loop (tail loop <> [head loop]))) `div` 2 - (length loop - 1) `div` 2 -- shoelace formula

-- >>> answer 2023 10 2 mainLoopArea
-- 453
mainLoopArea input = area $ loop (parseBoard input) ((96, 102), E)

