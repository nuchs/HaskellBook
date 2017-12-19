module Cipher where

import Data.Char

ceaser :: Int -> String -> String
ceaser = shiftString

unCeaser :: Int -> String -> String
unCeaser x = shiftString (-x)

shiftString :: Int -> String -> String
shiftString  s xs = map shifter xs
  where shifter = shiftChar s

shiftChar :: Int -> Char -> Char
shiftChar x c = newValue
  where base = ord 'a'
        ordinal = ord c - base
        shiftedOrdinal = (ordinal + x) `mod` 26
        newValue = chr (base + shiftedOrdinal)


