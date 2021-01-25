import System.IO

getIntegers :: Int -> Int -> IO Int
getIntegers t n = do
  if n == 0
    then return t
    else do
      xs <- getLine
      getIntegers (t + (read xs :: Int)) (n - 1)

adder :: IO ()
adder = do
  putStr "How many numbers? "
  n <- getLine
  t <- getIntegers 0 (read n :: Int)
  putStrLn ("The total is " ++ show t)

main = adder