module Calibration
    ( calibrationSum
    , calibrationSum2
    ) where

import Text.Parsec
import Data.Function
import Data.Functor
import Data.Char

-- >>> extractDigits "ksdfj1kljsd2lks34s"
-- Right [1,2,3,4]
extractDigits :: String -> Either ParseError [Int]
extractDigits = parse parser ""
    where
        parser = do
            ds <- many (digit <&> digitToInt)
            ds' <- (eof $> []) <|> do
                void anyChar
                parser
            pure $ ds <> ds'

-- >>> extractCalibration [1,2,3]
-- >>> extractCalibration [8]
-- 13
-- 88
extractCalibration :: [Int] -> Int
extractCalibration digits = let firstDigit = head digits; lastDigit = last digits in firstDigit * 10 + lastDigit

-- >>> parseCalibration "one2345sixseven"
-- 25
parseCalibration :: String -> Int
parseCalibration line = extractCalibration $ fromRight $ extractDigits line
    where
        fromRight = either (error . show) id :: (Show a => Either a b -> b)

calibrationSum :: String -> Int
calibrationSum calibrationString = (lines calibrationString <&> parseCalibration) & sum

-- >>> extractDigitsAsSymbolsAndNames "aoneaskdjflkas5skldfjalksixsdf"
-- Right [1,5,6]
extractDigitsAsSymbolsAndNames :: String -> Either ParseError [Int]
extractDigitsAsSymbolsAndNames = parse parser ""
    where
        parser = do
            ds <- many ((digit <&> digitToInt) <|> try (string "one" $> 1) <|> try (string "two" $> 2) <|> try (string "three" $> 3) <|> try (string "four" $> 4) <|> try (string "five" $> 5) <|> try (string "six" $> 6) <|> try (string "seven" $> 7) <|> try (string "eight" $> 8) <|> try (string "nine" $> 9))
            ds' <- (eof $> []) <|> do
                void anyChar
                parser
            pure $ ds <> ds'

-- >>> parseCalibration2 "one2345sixseven"
-- 17
parseCalibration2 :: String -> Int
parseCalibration2 line = extractCalibration $ fromRight $ extractDigitsAsSymbolsAndNames line
    where
        fromRight = either (error . show) id :: (Show a => Either a b -> b)

calibrationSum2 :: String -> Int
calibrationSum2 calibrationString = (lines calibrationString <&> parseCalibration2) & sum

-- (try $ string "oneight" $> [ 1, 8 ]) <|> (try $ string "threeight" $> [ 3, 8 ]) <|> (try $ string "fiveight" $> [ 5, 8 ]) <|> (try $ string "nineight" $> [ 9, 8 ]) <|> (try $ string "twone" $> [ 2, 1 ]) <|> (try $ string "sevenine" $> [ 7, 9 ]) <|> (try $ string "eightwo" $> [ 8, 2 ])
