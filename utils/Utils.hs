module Utils where

import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString as BS
import Data.ByteString.Char8 as C
import System.Directory
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import System.Hclip

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

class SumType t a where
    cast :: t -> a

instance SumType (Maybe a) a where
    cast (Just a) = a
    cast Nothing = error "unable to cast from Nothing"

instance Show e => SumType (Either e a) a where
    cast (Right a) = a
    cast (Left e) = error $ "unable to cast from Left " <> show e