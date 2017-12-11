module C9 where

import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool = myEft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = myEft
  
eftInt :: Int -> Int -> [Int]
eftInt = myEft

eftChar :: Char -> Char -> [Char]
eftChar = myEft

myEft :: (Enum a, Ord a) => a -> a -> [a]
myEft f t
  | f > t = []
  | f == t = [t]
  | otherwise = f : myEft (succ f) t

myWords :: String -> [String]
myWords [] = []
myWords s = nextWord : myWords remainingWords
  where trimmed = dropWhile (==' ') s
        nextWord = takeWhile (/=' ') trimmed
        remainingWords = dropWhile (/=' ') trimmed

mySqr = [x^2 | x <- [1..10]]

ctl1 = [x | x <- mySqr, rem x 2 == 0]

ctl2 =  [(x, y) | x <- mySqr,
          y <- mySqr,
          x < 50, y > 50]

ctl3 = take 5 [(x, y) | x <- mySqr,
                 y <- mySqr,
                 x < 50, y > 50]

mySqr2 = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]
myTuple = [(x, y) | x <- mySqr2, y <- myCube]
myTuple' = [(x, y) | x <- mySqr2, y <- myCube, x < 50, y < 50]

myFilter :: String -> [String]
myFilter = (filter notAnArticle) . words
  where notAnArticle = not . flip elem ["the", "a", "an"]

myzip :: [a] -> [b] -> [(a, b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x, y) : myzip xs ys

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith _ [] _ = []
myzipWith _ _ [] = []
myzipWith f (x:xs) (y:ys) = f x y : myzipWith f xs ys

myzip' :: [a] -> [b] -> [(a, b)]
myzip' xs ys = zipWith zipper xs ys
  where zipper x y = (x, y)

capitalise :: String -> String
capitalise (x:xs) = toUpper x : xs

shout :: String -> String
shout [] = []
shout (x:xs) = toUpper x : shout xs

initials :: String -> Char
initials = toUpper . head 
