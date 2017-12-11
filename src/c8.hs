module C8 where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

sum' :: (Eq a, Num a) => a -> a
sum' 0 = 0
sum' n = n + sum' (n - 1)

multiple :: Integral a => a -> a -> a
multiple x y = sign $ go (abs x) (abs y) 0
  where go _    0     total = total
        go step count total = go step (count - 1) (total + step)
        sign = if (signum x == signum y) then id else negate
    

data DividedResult = Result (Integer, Integer) | DividedByZero deriving Show

dividedByOld :: Integral a => a -> a -> (a, a)
dividedByOld num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy x y = go (abs x) (abs y) 0
  where go _ 0 _ = DividedByZero
        go n d count
          | n < d = Result (sign count, sign . fromIntegral $ n)
          | otherwise = go (n - d) d (count + 1)
        sign = if (signum x == signum y) then id else negate
 

mc91 :: (Num a, Ord a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11
