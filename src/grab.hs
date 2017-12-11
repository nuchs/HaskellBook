module Grab where

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

functionC x y = case (x > y) of
  True -> x
  False -> y

ifEvenAdd2 n = case even n of
  True  -> n + 2
  False -> n

nums x = case compare x 0 of
  LT -> -1
  EQ -> 0
  GT -> 1

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneisTwo = (flip dodgy) 2

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  where y = x / 100
