module C12 where

notThe :: String -> Maybe String
notThe x
  | x == "the" = Nothing
  | otherwise  = Just x

replaceThe :: String -> String
replaceThe = unwords . map f .words
  where f x = case notThe x of
              Just w -> w
              Nothing -> "a"

window :: (b -> a -> a -> b) -> b -> [a] -> b
window _ z []  = z
window _ z [_] = z
window f z (x1:x2:xs) = window f (f z x1 x2) (x2:xs)

theBeforeVowel :: String -> String -> Bool
theBeforeVowel "the" (x:_) = x `elem` "aeiou"
theBeforeVowel _ _ = False

counter :: Integer -> String -> String -> Integer
counter z w1 w2 = if theBeforeVowel w1 w2 then z+1 else z

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = window counter 0 . words

isVowel :: Char -> Bool
isVowel x = elem x "aeiou"

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"
consonsants = "bcdfghjklmnpqrstvwxyz"

countInstances :: Eq a => [a] -> [a] -> Int
countInstances xs ys = length $ filter isInstance xs
  where isInstance x = x `elem` ys

mkWord :: String -> Maybe Word'
mkWord x = if noVowels > noConsonants then Nothing else Just (Word' x)
  where noVowels = countInstances x vowels 
        noConsonants = countInstances x consonsants

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = 1 + natToInteger a

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just $ go x
  where go 0 = Zero
        go x = Succ $ go (x-1)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

maybee :: b -> (a -> b) -> Maybe a -> b
maybee d _ Nothing = d
maybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe d x = maybee d id x

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = maybee [] (:[])

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f []
 where f Nothing  a = a
       f (Just x) a = x : a

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where f Nothing _ = Nothing
        f _ Nothing = Nothing
        f (Just x) (Just a) = Just $ x:a

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left x) a = x:a
        f _ a = a

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right x) a = x:a
        f _ a = a

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b ->  c
either' fl _ (Left x)  = fl x
either' _ fr (Right x) = fr x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' fl fr
  where fl _ = Nothing
        fr x = Just $ f x

myIterate :: (a -> a) -> a -> [a]
myIterate f z = z : myIterate f (f z)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                Nothing -> []
                Just (c, n) ->  c : myUnfoldr f n

betterIterate :: (a -> a) -> a -> [a]
betterIterate f z = myUnfoldr g z
  where g x = Just (x, f x)

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
             Nothing -> Leaf
             Just (al, b, ar) -> Node (unfold f al) b (unfold f ar)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold grow 0
  where grow c = if c >= n then Nothing else Just (c+1, c, c+1)
