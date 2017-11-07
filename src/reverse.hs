module Reverse where


rvrs :: String -> String
rvrs x = word3 ++ " " ++ word2 ++ " " ++ word1
  where word1 = take 5 x
        word2 = take 2 $ drop 6 x
        word3 = take 7 $ drop 9 x

main :: IO ()
main = print $ rvrs "Curry is awesome"
