module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

triple :: (Num a) => a -> a
triple x = x * 3

square :: (Num a) => a -> a
square x = x * x

areaOfCircle :: (Floating a) => a -> a
areaOfCircle r = pi * square r

