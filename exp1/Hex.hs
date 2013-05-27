module Hex (

    hexStringToInteger

) where

import Prelude hiding (lookup)
import Data.Map       (Map, fromList, lookup)


-- problem:  what about capital A-F?  they should work too
digits :: Map Char Integer
digits = fromList $ zip (['0' .. '9'] ++ ['a' .. 'f']) [0 .. 15]


sumIt :: Integer -> [Integer] -> Integer
sumIt total []     = total
sumIt total (x:xs) = sumIt (16 * total + x) xs


hexStringToInteger :: String -> Integer
hexStringToInteger = sumIt 0 . map (\c -> case lookup c digits of (Just x) -> x)
