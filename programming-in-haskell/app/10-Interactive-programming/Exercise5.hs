import System.IO

adder' :: IO ()
adder' = do
  putStr "How many numbers? "
  n <- getLine
  res <- sequence (replicate (read n :: Int) getLine)
  putStrLn ("The total is " ++ show (sum (map (read :: String -> Int) res)))

main = adder'