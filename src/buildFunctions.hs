module BuildFunctions where

exclaim :: String -> String
exclaim x = x ++ "!"

fifth :: [a] -> a
fifth x = head $ drop 4 x

drop9 :: [a] -> [a]
drop9 x = drop 9 x

thirdLetter :: [a] -> a
thirdLetter x = x !! 3

letterIndex :: Int -> Char
letterIndex x = message !! x
  where message = "Curry is awesome!"

rvrs = word3 ++ " " ++ word2 ++ " " ++ word1
  where word1 = take 5 message
        word2 = take 2 $ drop 6 message
        word3 = take 7 $ drop 9 message
        message = "Curry is awesome"
