module Print3Fixed where

printSecond :: String -> IO ()
printSecond msg = do
  putStrLn msg


main :: IO ()
main = do
  putStrLn greeting
  printSecond greeting
  where greeting = "Yarrrr"
