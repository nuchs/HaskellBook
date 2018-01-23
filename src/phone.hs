module DaPhone where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map

type Digit = Char
type Presses = Int
type Path = (Digit, Presses)
type Button = (Digit, [Char])

data DaPhone = DaPhone [Button]

buttonPath :: Char -> Button -> Maybe Path
buttonPath c (d, cs) = fmap f $ elemIndex c cs
  where f x = (d, x+1)
  
findButtonPath :: DaPhone -> Char -> Path
findButtonPath (DaPhone bs) c =  head . mapMaybe (buttonPath c) $ bs         
  
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c = if isUpper c
                  then ('*', 1) : path
                  else path
  where path = [findButtonPath p (toLower c)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p xs = concatMap (reverseTaps p) xs

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr f 0
  where f (_, p) b = p + b

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopularThing ' ' . filter isAlpha

mostPopularWord :: String -> String
mostPopularWord = mostPopularThing "" . words

mostPopularThing :: Ord a => a -> [a] -> a
mostPopularThing z = fst . Map.foldrWithKey keyWithMaxValue (z, 0) . frequencyMap 

frequencyMap :: (Ord k, Num v) => [k] -> Map.Map k v
frequencyMap = foldr addToMap Map.empty 

addToMap :: (Ord k, Num v) => k -> Map.Map k v -> Map.Map k v
addToMap k m = Map.insertWith (+) k 1 m

keyWithMaxValue :: (Ord v) => k -> v -> (k, v) -> (k, v)
keyWithMaxValue k1 f1 (k2, f2) = if f1 > f2
                          then (k1, f1)
                          else (k2, f2)
  
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopularWord . intercalate " "

testPhone :: DaPhone
testPhone = DaPhone
            [('0', " 0")
            ,('1', "1")
            ,('2', "abc2")
            ,('3', "def3")
            ,('4', "ghi4")
            ,('5', "jkl5")
            ,('6', "mno6")
            ,('7', "pqrs7")
            ,('8', "tuv8")
            ,('9', "wxyz9")
            ,('*', "")
            ,('#', ".,#")
            ]

convo :: [String]
convo = ["Wanna play 20 questions"
        ,"Ya"
        ,"U 1st haha"
        ,"Lol ok. Have u ever tasted alcohol"
        ,"Lol ya"
        ,"Wow ur cool haha. Ur turn"
        ,"Ok. Do u think I am pretty Lol"
        ,"Lol ya"
        ,"Just making sure rofl ur turn"
        ]
