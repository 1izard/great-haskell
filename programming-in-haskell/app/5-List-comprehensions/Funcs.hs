firsts ps = [x | (x, _) <- ps]

main = do
  print (concat [[1, 2, 3], [4, 5, 6]]) -- [1,2,3,4,5,6]
  print ([1, 2, 3] ++ [4, 5, 6]) -- [1,2,3,4,5,6]
  print (firsts [(1, 2), (3, 4), (5, 6)]) -- [1, 3, 5]
  print (length [1, 2, 3]) -- 3