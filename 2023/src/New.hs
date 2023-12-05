module New where

import Text.Parsec
import Data.Functor
import qualified Data.Map as Map
import Data.Foldable
import Data.Maybe
import Data.Function
import Utils
import Data.Sequence (mapWithIndex, fromList, sort)
import Control.Monad (join, replicateM)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow

-- >>> answer 2023 5 1 new1
-- 462648396
new1 input = let 
    Right (seeds, maps) = parse inputParser "" input
    mapsFunction = foldl1 (>>>) (mapFunction <$> maps)  
    in minimum $ mapsFunction <$> seeds

mapFunction :: [(Int, Int, Int)] -> Int -> Int
mapFunction map x = fromMaybe x $ mapMaybe (\(dstStart, srcStart, range) -> if x >= srcStart && x < srcStart + range then Just (dstStart + x - srcStart) else Nothing) map & listToMaybe

-- >>> parse inputParser "" "seeds: 19 20 21\n\nfertilizer-to-water map:\n12 13 14\n1 2 3*\n\nwater-to-light map:\n88 18 7\n18 25 70"
-- Right ([19,20,21],[[(12,13,14),(1,2,3)],[(88,18,7),(18,25,70)]])
inputParser :: Parsec String u ([Int], [[(Int, Int, Int)]])
inputParser = do
    string "seeds: "
    seeds <- (read <$> many1 digit) `sepBy` char ' '
    string "\n\n"
    maps <- mapsParser
    pure (seeds, maps)

-- >>> parse mapsParser "" "fertilizer-to-water map:\n12 13 14\n1 2 3*\n\nwater-to-light map:\n88 18 7\n18 25 70"
-- Right [[(12,13,14),(1,2,3)],[(88,18,7),(18,25,70)]]
mapsParser :: Parsec String u [[(Int, Int, Int)]] 
mapsParser = mapParser `sepBy` (string "*\n\n") 

-- >>> parse mapParser "" "fertilizer-to-water map:\n12 13 14\n1 2 3*"
-- Right [(12,13,14),(1,2,3)]
mapParser  :: Parsec String u [(Int, Int, Int)]
mapParser = do
    headerParser
    r <- ruleParser `sepBy` char '\n'
    pure r

-- >>> parse ruleParser "" "12 13 14"
-- Right (12,13,14)
ruleParser :: Parsec String u (Int, Int, Int)
ruleParser = do
    numbers <- (read <$> many1 digit) `sepBy` char ' '
    pure (numbers !! 0, numbers !! 1, numbers !! 2)

-- >>> parse headerParser "" "fertilizer-to-water map:\n"
-- Right ()
headerParser :: Parsec String u ()
headerParser = do
    try (string "seed-to-soil") <|> try (string "soil-to-fertilizer") <|> try (string "fertilizer-to-water") <|> try (string "water-to-light") <|> try (string "light-to-temperature") <|> try (string "temperature-to-humidity") <|> string "humidity-to-location"
    string " map:\n"
    pure ()

lowestLocationNumberOfSeeds maps = 1 

-- >>> answer 2023 5 2 new2
new2 input = 0
