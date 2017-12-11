module C7 where

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit :: Integral a => a -> a
tensDigit x = snd . divMod10 $ fst . divMod10 $ x
  where divMod10 = flip divMod 10

hunsD :: Integral a => a -> a
hunsD x = snd . divMod10 $ fst . divMod100 $ x
  where divMod100 = flip divMod 100
        divMod10 = flip divMod 10

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y test
  | test      = x
  | otherwise = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y test = case test of
  True  -> x
  False -> y
