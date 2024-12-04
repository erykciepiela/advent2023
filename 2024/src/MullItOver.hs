module MullItOver where

import Utils
import Control.Monad (guard)
import Text.Parsec
import Text.Parsec.String (Parser)

-- >>> answer 2024 3 1 result
-- 173517243
result :: String -> Int
result input = sum $ cast $ runParser (garbageParser mulParser) () "" input

mulParser :: Parser Int
mulParser = do
    string "mul("
    x <- read @Int <$> many digit
    guard $ x < 1000
    string ","
    y <- read @Int <$> many digit
    guard $ y < 1000
    string ")"
    pure $ x * y
    
garbageParser :: Parser a -> Parser [a]
garbageParser p = try parseExpressionAndContinue <|> try skipCharacterAndContinue <|> end
    where
        parseExpressionAndContinue = do
            n <- p
            ns <- garbageParser p
            pure $ n : ns
        skipCharacterAndContinue = do
            anyChar
            garbageParser p
        end = do 
            eof
            pure []

data Instr = Mul Int | Do | Dont

instrParser :: Parser Instr
instrParser = (Mul <$> try mulParser) <|> (Do <$ try (string "do()")) <|> (Dont <$ try (string "don't()"))

-- >>> answer 2024 3 2 result2
-- 100450138
result2 :: String -> Int
result2 input = interpretInstrs $ cast $ runParser (garbageParser instrParser) () "" input

interpretInstrs :: [Instr] -> Int
interpretInstrs is = snd $ foldl (\(enabled, sum) instr -> case instr of
    Mul i | enabled -> (enabled, sum + i)
    Do -> (True, sum)
    Dont -> (False, sum)
    _ -> (enabled, sum)) (True, 0) is
