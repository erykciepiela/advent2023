module CeresSearch where

import Data.Functor ((<&>))
import Data.List.NonEmpty (nonEmpty, NonEmpty)
import Utils
import Control.Comonad ((=>>))

-- >>> answer 2024 4 1 result
-- 140
result :: String -> Int
result input = grid =>> isStartOfXmasText 
  where
    grid :: NonEmpty (NonEmpty Char) = cast $ nonEmpty $ lines input <&> \line -> cast (nonEmpty line)
    isStartOfXmasText grid = True
