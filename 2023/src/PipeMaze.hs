module PipeMaze where

import Prelude hiding (Bounded)
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
-- import Control.Arrow
import Data.List

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


-- >>> loopLength (parseBoard ".....\n.S-7.\n.|.|.\n.L-J.\n.....") ((1,2), E)
-- 8
loopLength :: [[Dir -> Maybe ((Int, Int), Dir)]] -> ((Int, Int), Dir) -> Int
loopLength board ((y, x), dir) = 1 + case (board !! y !! x) dir of
    Nothing -> 0
    Just n -> loopLength board n


-- >>> loop (parseBoard ".....\n.S-7.\n.|.|.\n.L-J.\n.....") ((1,2), E)
-- >>> loop (parseBoard ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ...") ((4, 13), E)
-- [((1,1),(N,E)),((1,2),(E,E)),((1,3),(E,S)),((2,3),(S,S)),((3,3),(S,W)),((3,2),(W,W)),((3,1),(W,N)),((2,1),(N,N))]
-- [((4,12),(N,E)),((4,13),(E,S)),((5,13),(S,S)),((6,13),(S,S)),((7,13),(S,S)),((8,13),(S,S)),((9,13),(S,E)),((9,14),(E,N)),((8,14),(N,N)),((7,14),(N,E)),((7,15),(E,S)),((8,15),(S,S)),((9,15),(S,E)),((9,16),(E,N)),((8,16),(N,N)),((7,16),(N,N)),((6,16),(N,W)),((6,15),(W,N)),((5,15),(N,W)),((5,14),(W,N)),((4,14),(N,E)),((4,15),(E,E)),((4,16),(E,S)),((5,16),(S,E)),((5,17),(E,S)),((6,17),(S,E)),((6,18),(E,S)),((7,18),(S,E)),((7,19),(E,N)),((6,19),(N,N)),((5,19),(N,W)),((5,18),(W,N)),((4,18),(N,W)),((4,17),(W,N)),((3,17),(N,W)),((3,16),(W,W)),((3,15),(W,N)),((2,15),(N,W)),((2,14),(W,N)),((1,14),(N,E)),((1,15),(E,N)),((0,15),(N,W)),((0,14),(W,W)),((0,13),(W,S)),((1,13),(S,S)),((2,13),(S,S)),((3,13),(S,W)),((3,12),(W,N)),((2,12),(N,N)),((1,12),(N,N)),((0,12),(N,W)),((0,11),(W,S)),((1,11),(S,S)),((2,11),(S,S)),((3,11),(S,S)),((4,11),(S,W)),((4,10),(W,N)),((3,10),(N,N)),((2,10),(N,N)),((1,10),(N,N)),((0,10),(N,W)),((0,9),(W,S)),((1,9),(S,S)),((2,9),(S,S)),((3,9),(S,W)),((3,8),(W,N)),((2,8),(N,N)),((1,8),(N,N)),((0,8),(N,W)),((0,7),(W,S)),((1,7),(S,S)),((2,7),(S,S)),((3,7),(S,W)),((3,6),(W,N)),((2,6),(N,N)),((1,6),(N,N)),((0,6),(N,W)),((0,5),(W,W)),((0,4),(W,W)),((0,3),(W,W)),((0,2),(W,W)),((0,1),(W,S)),((1,1),(S,S)),((2,1),(S,S)),((3,1),(S,W)),((3,0),(W,S)),((4,0),(S,E)),((4,1),(E,E)),((4,2),(E,E)),((4,3),(E,N)),((3,3),(N,W)),((3,2),(W,N)),((2,2),(N,N)),((1,2),(N,E)),((1,3),(E,E)),((1,4),(E,E)),((1,5),(E,S)),((2,5),(S,W)),((2,4),(W,S)),((3,4),(S,E)),((3,5),(E,S)),((4,5),(S,E)),((4,6),(E,S)),((5,6),(S,W)),((5,5),(W,W)),((5,4),(W,S)),((6,4),(S,E)),((6,5),(E,S)),((7,5),(S,S)),((8,5),(S,W)),((8,4),(W,S)),((9,4),(S,E)),((9,5),(E,E)),((9,6),(E,E)),((9,7),(E,E)),((9,8),(E,N)),((8,8),(N,W)),((8,7),(W,W)),((8,6),(W,N)),((7,6),(N,E)),((7,7),(E,N)),((6,7),(N,E)),((6,8),(E,S)),((7,8),(S,E)),((7,9),(E,N)),((6,9),(N,N)),((5,9),(N,E)),((5,10),(E,S)),((6,10),(S,S)),((7,10),(S,S)),((8,10),(S,S)),((9,10),(S,E)),((9,11),(E,N)),((8,11),(N,N)),((7,11),(N,E)),((7,12),(E,N)),((6,12),(N,W)),((6,11),(W,N)),((5,11),(N,E)),((5,12),(E,N))]
loop :: [[Dir -> Maybe ((Int, Int), Dir)]] -> ((Int, Int), Dir) -> [((Int, Int), (Dir, Dir))]
loop board start = let l = goLoop board start in (finish start, (snd $ snd $ last l, snd start)):l
    where
        finish ((y, x), E) = (y, x - 1)
        finish ((y, x), W) = (y, x + 1)
        finish ((y, x), N) = (y + 1, x)
        finish ((y, x), S) = (y - 1, x)

goLoop :: [[Dir -> Maybe ((Int, Int), Dir)]] -> ((Int, Int), Dir) -> [((Int, Int), (Dir, Dir))]
goLoop board ((y, x), dir) = case (board !! y !! x) dir of
    Nothing -> []
    Just new@(_, newdir) -> ((y, x), (dir, newdir)): goLoop board new


type Bounded = (Int, Int) -> Dir -> Bool

-- >>> field $ loop (parseBoard ".....\n.S-7.\n.|.|.\n.L-J.\n.....") ((1,2), E)
-- >>> field $ loop (parseBoard ".....\n.S-7.\n.|.|.\n.L-J.\n.....") ((2,1), S)
-- >>> field $ loop (parseBoard "...........\n.S-------7.\n.|F-----7|.\n.||OOOOO||.\n.||OOOOO||.\n.|L-7OF-J|.\n.|II|O|II|.\n.L--JOL--J.\n.....O.....") ((1,2), E)
-- >>> field $ loop (parseBoard "..........\n.S------7.\n.|F----7|.\n.||OOOO||.\n.||OOOO||.\n.|L-7F-J|.\n.|II||II|.\n.L--JL--J.\n..........") ((1,2), E)
-- >>> field $ loop (parseBoard ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ...") ((4, 13), E)
-- >>> field $ loop (parseBoard "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJIF7FJ-\nL---JF-JLJIIIIFJLJJ7\n|F|F-JF---7IIIL7L|7|\n|FFJF7L7F-JF7IIL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L") ((1, 4), S)
-- 1
-- 1
-- 4
-- 4
-- 1
-- 0
field :: [((Int, Int), (Dir, Dir))] -> Int
field loop = let
    bounded = goField loop (\_ _ -> False)
    (miny, maxy) = (minimum $ fst . fst <$> loop, maximum $ fst . fst <$>loop)
    (minx, maxx) = (minimum $ snd . fst <$> loop, maximum $ snd . fst <$> loop)
    in length [(y, x) | y <- [miny..maxy], x <- [minx..maxx], (y, x) `notElem` (fst <$> loop) && bounded (y,x) N && bounded (y,x) S && bounded (y,x) E && bounded (y,x) W]

goField :: [((Int, Int), (Dir, Dir))] -> Bounded -> Bounded
goField [] b = b
goField (((y, x),(d1, d2)):rest) bounded = goField rest $ \(y', x') -> \case
    N -> (if ((d1 /= N) && (d2 /= N)) && (x' == x) && (y' > y) then not (bounded (y', x') N) else bounded (y', x') N)
    S -> (if ((d1 /= S) && (d2 /= S)) && (x' == x) && (y' < y) then not (bounded (y', x') S) else bounded (y', x') S)
    W -> (if ((d1 /= W) && (d2 /= W)) && (x' > x) && (y' == y) then not (bounded (y', x') W) else bounded (y', x') W)
    E -> (if ((d1 /= E) && (d2 /= E)) && (x' < x) && (y' == y) then not (bounded (y', x') E) else bounded (y', x') E)


mapWithIndex :: [a] -> (Int -> a -> b) -> [b]
mapWithIndex as f = zipWith f [0..] as

-- >>> answer 2023 10 2 mainLoopField
-- 227
-- 857
-- 4374
-- 14057
-- mainLoopField input = field $ loop (parseBoard input) ((95, 101), N)
mainLoopField input = field $ loop (parseBoard input) ((96, 102), E)

