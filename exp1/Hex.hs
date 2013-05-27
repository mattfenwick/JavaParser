module Hex (

    hexStringToInteger

) where

import Prelude hiding (lookup)
import Data.Map       (Map, fromList, lookup)


digits :: Map Char Integer
digits = fromList $ zip keys vals
  where keys = ['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']
        vals = [0 .. 15]                    ++ [10 .. 15]


sumIt :: Integer -> [Integer] -> Integer
sumIt total []     = total
sumIt total (x:xs) = sumIt (16 * total + x) xs


hexStringToInteger :: String -> Integer
hexStringToInteger = sumIt 0 . map (\c -> case lookup c digits of (Just x) -> x)
