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
import Data.List (groupBy, sortOn, singleton)

-- >>> answer 2023 7 1 new
-- 1
new input = 1

-- >>> answer 2023 7 2 new2
-- 1
new2 input = 1

