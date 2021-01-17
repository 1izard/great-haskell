main = do
  let x = [1, 2, 3, 4, 5]
  print (head x) -- 1
  print (tail x) -- [2, 3, 4, 5]
  print (init x) -- [1, 2, 3, 4]
  print (last x) -- 5
  print (x !! 2) -- 3
  print (take 3 x) -- [1, 2, 3]
  print (drop 3 x) -- [4, 5]
  print (length x) -- 5
  print (sum x) -- 15
  print (product x) -- 120
  print ([1, 2, 3] ++ [4, 5]) -- [1 , 2, 3, 4, 5]
  print (reverse x) -- [5, 4, 3, 2, 1]
