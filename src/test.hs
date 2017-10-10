sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello " ++ x ++ "!")

triple x = x * 3

areaOfCircle :: (Floating a) => a -> a
areaOfCircle r = pi * r ^ 2
