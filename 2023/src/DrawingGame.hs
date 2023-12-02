module DrawingGame where

import Text.Parsec
import Data.Map (Map)
import Data.Functor
import qualified Data.Map as Map
import Data.Foldable (find)
import Data.Maybe
import Data.Function
import Utils

data Game = Game
    { gid :: Int
    , draws :: [Map Color Int] } deriving Show

data Color = Red | Green | Blue deriving (Eq, Ord, Show)

-- >>> parse gameParser "" "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
-- >>> parse gameParser "" "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
-- >>> parse gameParser "" "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
-- >>> parse gameParser "" "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
-- >>> parse gameParser "" "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
-- Right (Game {draws = [fromList [(Red,4),(Blue,3)],fromList [(Red,1),(Green,2),(Blue,6)],fromList [(Green,2)]]})
-- Right (Game {draws = [fromList [(Green,2),(Blue,1)],fromList [(Red,1),(Green,3),(Blue,4)],fromList [(Green,1),(Blue,1)]]})
-- Right (Game {draws = [fromList [(Red,20),(Green,8),(Blue,6)],fromList [(Red,4),(Green,13),(Blue,5)],fromList [(Red,1),(Green,5)]]})
-- Right (Game {draws = [fromList [(Red,3),(Green,1),(Blue,6)],fromList [(Red,6),(Green,3)],fromList [(Red,14),(Green,3),(Blue,15)]]})
-- Right (Game {draws = [fromList [(Red,6),(Green,3),(Blue,1)],fromList [(Red,1),(Green,2),(Blue,2)]]})
gameParser :: Parsec String u Game
gameParser = do
    string "Game "
    id' <- many1 digit <&> read
    string ": "
    Game id' <$> drawParser `sepBy1` string "; "
        where
            drawParser :: Parsec String u (Map Color Int)
            drawParser = do
                entries <- flip sepBy1 (string ", ") $ do
                    qty <- read <$> many1 digit
                    string " "
                    color <- (string "red" $> Red) <|> (string "green" $> Green) <|> (string "blue" $> Blue)
                    pure $ Map.singleton color qty
                pure $ foldr1 (Map.unionWith (+)) entries

-- >>> possible <$> parse gameParser "" "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
-- >>> possible <$> parse gameParser "" "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
-- >>> possible <$> parse gameParser "" "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
-- >>> possible <$> parse gameParser "" "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
-- >>> possible <$> parse gameParser "" "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
-- Right True
-- Right True
-- Right False
-- Right False
-- Right True
possible :: Game -> Bool
possible game = isNothing $ find exceedingDraw $ draws game
    where
        exceedingDraw draw = 12 < fromMaybe 0 (Map.lookup Red draw) || 13 < fromMaybe 0 (Map.lookup Green draw) || 14 < fromMaybe 0 (Map.lookup Blue draw)

-- >>> answer 2023 2 1 sumOfPossibleGameIds
-- 2156
sumOfPossibleGameIds :: String -> Int
sumOfPossibleGameIds input = lines input <&> cast . parse gameParser "" & Prelude.filter possible <&> gid & sum

powerOfSetOfCubes :: Game -> Int
powerOfSetOfCubes game = draws game & foldr1 (Map.unionWith max) & product

-- >>> answer 2023 2 2 sumOfPowersOfSetOfCubes
-- 66909
sumOfPowersOfSetOfCubes :: String -> Int
sumOfPowersOfSetOfCubes input = lines input <&> powerOfSetOfCubes . cast . parse gameParser "" & sum
