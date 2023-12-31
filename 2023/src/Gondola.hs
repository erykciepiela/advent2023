module Gondola where

import Text.Parsec hiding (labels)
import Data.Functor
import qualified Data.Map as Map
import Data.Foldable
import Data.Maybe
import Data.Function
import Utils
import Data.Sequence (mapWithIndex, fromList)
import Control.Monad (join)
import Data.Set (Set)
import qualified Data.Set as Set

-- >>> answer 2023 3 1 sumOfAllPartNumbers
-- 531932
sumOfAllPartNumbers input = labels input & Set.toList <&> number & sum

partMap :: String -> Map.Map (Int, Int) Part
partMap input = parts input <&> (\p@(Part ch y x) -> ((y, x), p)) & Map.fromList

parts :: String -> [Part]
parts input = join $ toList $ mapWithIndex (\y -> cast . parse (partsParser (y + 1) 1) "") (fromList $ lines input)

data Part = Part { symbol :: Char, y :: Int, x :: Int } deriving (Show, Eq, Ord)
data Label = Label { number :: Int, part :: Part } deriving (Show, Eq, Ord)

partsParser :: Int -> Int -> Parsec String a [Part]
partsParser y x' = do
    paddingLength <- length <$> many (digit <|> char '.')
    let x = x' + paddingLength
    mPartSymbol <- (anyChar <&> Just) <|> (eof $> Nothing)
    case mPartSymbol of
        Nothing -> pure []
        Just partSymbol -> do
            partSymbols <- partsParser y (x + 1)
            pure $ Part partSymbol y x : partSymbols

labelsParser :: Map.Map (Int, Int) Part -> Int -> Int -> Parsec String a [Label]
labelsParser partsMap y x' = do
    paddingLength <- length <$> many (oneOf (Map.elems partsMap <&> symbol) <|> char '.')
    let x = x' + paddingLength
    mSymbol <- (many1 digit <&> Just) <|> (eof $> Nothing)
    case mSymbol of
        Nothing -> pure []
        Just symbol -> do
            let len = length symbol
            labels <- labelsParser partsMap y (x + len)
            let parts = mapMaybe (`Map.lookup` partsMap) ([(y, x - 1), (y - 1, x - 1), (y + 1, x - 1), (y, x + len), (y - 1, x + len), (y + 1, x + len)] <> ([0..(len - 1)] >>= (\pos -> [(y - 1, x + pos), (y + 1, x + pos)])))
            case parts of
                [] -> pure labels
                part:[] -> pure $ Label (read symbol) part: labels
                _ -> error $ "too many parts: " <> show parts <> " for label " <> symbol <> " at " <> show y <> " " <> show x

labels :: String -> Set Label
labels input = Set.fromList $ join $ toList $ mapWithIndex (\y -> cast . parse (labelsParser (partMap input) (y + 1) 1) "") (fromList $ lines input)

-- >>> answer 2023 3 2 sumOfGearRatios
-- 73646890
sumOfGearRatios input = labels input & Set.filter (\l -> l.part.symbol  == '*') & (\set -> Set.cartesianProduct set set) & Set.filter (uncurry (<)) & Set.filter onTheSamePart & Set.toList <&> gearRatio & sum
    where
        onTheSamePart (label1, label2) = on (==) part label1 label2
        gearRatio (label1, label2) = on (*) number label1 label2
