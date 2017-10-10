module WaxOn where

waxOn :: Num a => a
waxOn = x * 5
  where x = y ^ 2
        y = z +8
        z = 7

triple :: Num a => a -> a
triple x = x * 3

waxOff :: Num a => a -> a
waxOff x = triple x
