module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord n = error $ "Unrecognised digit : " ++ (show n)

digits :: Int -> [Int]
digits 0 = [0]
digits x = reverse . go $ x
    where go 0 = []
          go n = (n `mod` 10) : go (n `div` 10)

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" $ map digitToWord $ digits n

digitToWord' :: Char -> String
digitToWord' '0' = "zero"
digitToWord' '1' = "one"
digitToWord' '2' = "two"
digitToWord' '3' = "three"
digitToWord' '4' = "four"
digitToWord' '5' = "five"
digitToWord' '6' = "six"
digitToWord' '7' = "seven"
digitToWord' '8' = "eight"
digitToWord' '9' = "nine"
digitToWord' '-' = "minus"
digitToWord' x = error $ "Unrecognised character : " ++ (show x)

wordNumber' :: Int -> String
wordNumber' n = concat $ intersperse "-" digitList
  where digitList = map digitToWord' $ show n
