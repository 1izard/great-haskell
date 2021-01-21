main = do
  print (all even [2, 4, 6, 8]) -- True
  print (any odd [2, 4, 6, 8]) -- False
  print (takeWhile even [2, 4, 6, 7, 8]) -- [2, 4, 6]
  print (dropWhile odd [1, 3, 5, 6, 7]) -- [6, 7]