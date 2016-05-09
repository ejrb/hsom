module Freddie where

shift8Bit :: Int -> String -> String
shift8Bit i = map (toEnum . (`mod` 256) . (+) i . fromEnum)

encrypt, decrypt :: String -> String
encrypt = shift8Bit 1
decrypt = shift8Bit (-1)
