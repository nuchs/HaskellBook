module C10 where

import Data.Time

five_a = foldr (++) [] ["woot", "WOOT", "woot"]
five_b = foldr max 'a' "fear is the little death"
five_c = foldr (&&) True [False, True]
five_d = foldr(||) False [False, True]
five_e = foldr ((++) . show) "" [1..5]
five_f = foldl const 'a' [1..5]
five_g = foldl const 0 "tacos"
five_h = foldr (flip const) 0 "burritos"
five_i = foldr (flip const) 'z' [1..5]


data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldl (\a b -> case b of
                         DbDate date -> date : a
                         _ -> a) []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldl (\a b -> case b of
                         DbNumber number -> number : a
                         _ -> a) []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = total / count
  where count = fromIntegral . length $ filterDbNumber db
        total = fromInteger $ sumDb db

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibs20 = take 20 $ 1 : scanl (+) 1 fibs20
fibs100 = takeWhile ((>) 100) fibs
  where fibs = 1 : scanl (+) 1 fibs

factorial n = take n $ scanl (*) 1 [2..]

stops = "pbtdkg"
vowels = "aeiou"

svs :: String -> String -> [(Char, Char, Char)]
svs stops vowels = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

svs2 :: String -> String -> [(Char, Char, Char)]
svs2 stops vowels = [(s1, v, s2) | s1 <- stops, s1 == 'p',
                                   v <- vowels,
                                   s2 <- stops]
nouns = ["dog", "sausage", "hose", "stick"]
verbs = ["inserts", "licks", "rubs"]
nvn :: [String] -> [String] -> [(String, String, String)]
nvn nouns verbs = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns]

seekritFunc :: Fractional a => String -> a
seekritFunc x = numLetters / numWords
  where numLetters = fromIntegral $ sum (map length (words x))
        numWords = fromIntegral $ length (words x)


myOr :: [Bool] -> Bool
myOr = foldl (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> b || f a) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> b || (x == a)) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldl (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldl (\b a -> if f a b == GT then a else b) (head xs) (tail xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldl (\b a -> if f a b == LT then a else b) (head xs) (tail xs)
