grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x', y') | x' <- [0 .. x], y' <- [0 .. y]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <- [0 .. n - 1]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfect :: Int -> [Int]
perfect n = [x | x <- [1 .. n], sum (init (factors x)) == x]

main = do
  print (sum [x ^ 2 | x <- [1 .. 100]]) -- 338350
  print (grid 1 2) -- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
  print (square 2) -- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
  print (replicate' 3 True) -- [True,True,True]
  print (pyths 10) -- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
  print (perfect 500) -- [6,28,496]
  print (concat [[(x, y) | x <- [1, 2]] | y <- [3, 4]]) -- [(1,3),(2,3),(1,4),(2,4)]
