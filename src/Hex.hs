module Hex (

    hexStringToInteger
  , octalStringToInteger

) where

import Prelude hiding (lookup)
import Data.Map       (Map, fromList, lookup)


hexDigits :: Map Char Integer
hexDigits = fromList $ zip keys vals
  where keys = ['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']
        vals = [0 .. 15]                    ++ [10 .. 15]


sumIt :: Integer -> Integer -> [Integer] -> Integer
sumIt base total []     = total
sumIt base total (x:xs) = sumIt base (base * total + x) xs


hexStringToInteger :: String -> Integer
hexStringToInteger = sumIt 16 0 . map (\c -> case lookup c hexDigits of (Just x) -> x)


octalDigits :: Map Char Integer
octalDigits = fromList $ zip ['0' .. '7'] [0 .. 7]


octalStringToInteger :: String -> Integer
octalStringToInteger = sumIt 8 0 . map (\c -> case lookup c octalDigits of (Just x) -> x)
