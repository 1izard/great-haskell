putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putRow' :: Board -> Int -> IO ()
putRow' [] _ = return ()
putRow' (b : bs) row = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate b "* "))
  putRow' bs (row + 1)

putBoard' :: Board -> IO ()
putBoard' b = putRow' b 1

putBoard'' :: Board -> IO ()
putBoard'' b = sequence_ [putRow r n | (r, n) <- zip [1 ..] b]

main = do
  putStr' "Haskell"
  putStrLn ""
  putBoard' [1, 2, 3, 4, 5, 6, 7]
  putStrLn ""
  putBoard'' [1, 2, 3, 4, 5, 6, 7]