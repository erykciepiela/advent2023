module BridgeRepair where

import Prelude
import Utils
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import Text.Parsec.String (Parser)
import Text.Parsec ( runParser, string, sepBy, char, many, digit )
import Data.Foldable1 (foldlM1)
import Data.List.NonEmpty (NonEmpty, nonEmpty)

-- >>> answer 2024 7 1 totalCalibration
-- 945512582195
totalCalibration :: String -> Integer
totalCalibration input = sum $ lines input <&> (fromMaybe 0 . calibration [(*), (+)])

calibration :: [Integer -> Integer -> Integer] -> String -> Maybe Integer
calibration operators line = let (target, numbers) = cast $ runParser lineParser () "" line in if possiblyTrue operators target numbers then Just target else Nothing

lineParser :: Parser (Integer, NonEmpty Integer)
lineParser = do
    target <- read <$> many digit
    string ": "
    numbers <- (read <$> many digit) `sepBy` char ' '
    pure (target, cast $ nonEmpty numbers)

possiblyTrue :: [Integer -> Integer -> Integer] -> Integer -> NonEmpty Integer -> Bool
possiblyTrue operators target numbers = target `elem` possibleExpressionValues
    where
        possibleExpressionValues = foldlM1 (\number agg -> (\op -> op agg number) <$> operators) numbers


-- >>> answer 2024 7 2 totalCalibration2
-- 271691107779347
totalCalibration2 :: String -> Integer
totalCalibration2 input = sum $ lines input <&> (fromMaybe 0 . calibration [(*), (+), concat123])
    where
        concat123 a b = read (show b <> show a)
