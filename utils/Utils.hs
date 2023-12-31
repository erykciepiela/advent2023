module Utils where

import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString as BS
import Data.ByteString.Char8 as C
import System.Directory
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import System.Hclip
import Text.Parsec

answer :: Show a => Int -> Int -> Int -> (String -> a) -> a
answer year day _ solution = unsafePerformIO $ do
    exists <- doesFileExist inputFile
    input <- if exists then readInput else downloadInput
    let result = solution input
    setClipboard $ show result
    pure result
        where
            inputFile = show year <> "/inputs/" <> show day <> ".txt"
            readInput = C.unpack <$> C.readFile inputFile
            downloadInput :: IO String
            downloadInput = do
                manager <- newManager tlsManagerSettings
                session <- BS.readFile ".session"
                input <- httpsGet manager session
                Prelude.writeFile inputFile input
                pure input
                where
                    url :: String
                    url = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"
                    httpsGet :: Manager -> BS.ByteString -> IO String
                    httpsGet manager session = do
                        re <- setRequestHeader "cookie" ["session=" <> session] <$> parseRequest url
                        let request = setRequestManager manager re
                        response <- httpBS request
                        return $ C.unpack $ getResponseBody response

class SumType t a | t -> a where
    cast :: t -> a

instance SumType (Maybe a) a where
    cast (Just a) = a
    cast Nothing = error "unable to cast from Nothing"

instance Show e => SumType (Either e a) a where
    cast (Right a) = a
    cast (Left e) = error $ "unable to cast from Left " <> show e

instance SumType [a] a where
    cast (a:_) = a
    cast [] = error "unable to cast from []"



-- >>> parsed "123" (read @Int <$> many1 digit)
-- 123
-- >>> "123" `parsed` (read @Int <$> many1 digit)
-- 123
parsed :: String -> Parsec String () a -> a
parsed input parser = cast $ parse parser "" input
