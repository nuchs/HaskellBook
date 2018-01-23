module Cipher where

import Data.Char

ceaser :: Int -> String -> String
ceaser = shiftString

unCeaser :: Int -> String -> String
unCeaser x = shiftString (-x)

shiftString :: Int -> String -> String
shiftString  s xs = map shifter xs
  where shifter = shiftChar s

vignere :: String -> String -> String
vignere k m = keyShift keys m
  where keys = map toOrdinal $ cycle k

unvignere :: String -> String -> String
unvignere k m = keyShift keys m
  where keys = map (negate . toOrdinal) $ cycle k

keyShift :: [Int] -> String -> String
keyShift _ [] = []
keyShift [] _ = []
keyShift ks (' ':ms) = ' ' : keyShift ks ms
keyShift (k:ks) (m:ms) = shiftChar k m : keyShift ks ms

base :: Int
base = ord 'A'

toOrdinal :: Char -> Int
toOrdinal c = ord c - base

toLetter :: Int -> Char
toLetter x = chr (ordinal + base)
  where ordinal = (x `mod` 26) 

shiftChar :: Int -> Char -> Char
shiftChar x c = toLetter $ toOrdinal c + x 


