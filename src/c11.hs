module C11 where

import qualified Data.Char as Char

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size  deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCar :: [Vehicle] -> [Bool]
areCar = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

data OperatingSystem = GnuPlusLinux
                     | OpenBsdPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgLang = Haskell
              | Agda
              | Idris
              | PureScript
              deriving (Eq, Show)

data Programmer = Programmer
                { os :: OperatingSystem
                , lang :: ProgLang
                } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBsdPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer o l | o <- allOperatingSystems, l <- allLanguages]

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' a Leaf = Node Leaf a Leaf
insert' a (Node left b right)
  | a == b = Node left b right
  | a <  b = Node (insert' a left) b right
  | a >  b = Node left b (insert' a right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)
    
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left ++ preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ (a : inorder right)
  
postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2,1,3]
               then putStrLn "Preorder works"
               else putStrLn "Preorder doesn't work"

testInorder :: IO ()
testInorder = if inorder testTree == [1,2,3]
              then putStrLn "Inorder works"
              else putStrLn "Inorder doesn't work"

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2]
                then putStrLn "Postorder works"
                else putStrLn "Postorder doesn't work"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = f a subFold
  where rightFold = foldTree f b right
        subFold   = foldTree f rightFold left

isSubSeqOf :: (Eq a) => [a] -> [a] -> Bool
isSubSeqOf [] _ = True
isSubSeqOf _ [] = False
isSubSeqOf xss@(x:xs) yss@(y:ys)
  | x == y    = isSubSeqOf xs yss
  | otherwise = isSubSeqOf xss ys

capitaliseWords :: String -> [(String, String)]
capitaliseWords = map capitalise . words 

capitalise :: String -> (String, String)
capitalise [] = ("", "")
capitalise wss@(w:ws) = (wss, Char.toUpper w : ws)

capitaliseWord :: String -> String
capitaliseWord [] = ""
capitaliseWord (w:ws) = Char.toUpper w : ws

capitaliseParagraph :: String -> String
capitaliseParagraph ws = unwords $ capitaliseRest
  where capitaliseFirst = capitaliseWord ws
        capitaliseRest  = rearWindow isEndOfSentance capitaliseWord (words capitaliseFirst)

isEndOfSentance :: String -> Bool
isEndOfSentance "" = False
isEndOfSentance ws = last ws == '.'

rearWindow :: (a -> Bool) -> (a -> a) -> [a] -> [a]
rearWindow _ _ [] = []
rearWindow _ _ [x] = [x]
rearWindow t f (x1:x2:xs) = x1 : rearWindow t f (newX2:xs)
  where newX2 = if t x1 then f x2 else x2

