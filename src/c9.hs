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

-- 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (True:xs) = True
myOr (_:xs) = myOr xs

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | f x == True = True
  | otherwise = myAny f xs

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
  | x == y = True
  | otherwise = myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x ys = myAny (\a -> a == x) ys

-- 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = acc [] xs
  where acc ys [] = ys
        acc ys (x:xs) = acc (x:ys) xs

-- 5
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- 6

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = findMax f x xs
  where findMax _ m [] = m
        findMax f m (x:xs) = let currMax = if (f x m == GT) then x else m
                             in findMax f currMax xs

-- 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = findMin f x xs
  where findMin _ m [] = m
        findMin f m (x:xs) = let currMin = if (f x m == LT) then x else m
                             in findMin f currMin xs

-- 10
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
