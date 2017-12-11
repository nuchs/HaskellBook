module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry\n"
sentances = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myParts :: Eq a => a -> [a] -> [[a]]
myParts _ [] = []
myParts s xs = nextPart : myParts s remainingParts
  where nextPart = takeWhile (/=s) xs
        remainingParts = dropWhile (==s) $ dropWhile (/=s) xs

myLines :: String -> [String]
myLines [] = []
myLines s = nextLine : myLines remainingLines
  where nextLine = takeWhile (/='\n') s
        remainingLines = dropWhile (=='\n') $ dropWhile (/='\n') s

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry"
  ]

main :: IO ()
main =
  print $ "Are they equal? " ++ show (myLines sentances == shouldEqual)

myWords' = myParts ' '
myLines' = myParts '\n'
