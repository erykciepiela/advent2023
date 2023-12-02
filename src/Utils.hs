module Utils where

import System.IO.Unsafe (unsafePerformIO)

input :: Int -> String
input dayNo = unsafePerformIO $ readFile $ "inputs/" <> show dayNo <> ".txt"

class SumType t a where
    cast :: t -> a

instance SumType (Maybe a) a where
    cast (Just a) = a
    cast Nothing = error "unable to cast from Nothing" 

instance Show e => SumType (Either e a) a where
    cast (Right a) = a
    cast (Left e) = error $ "unable to cast from Left " <> show e