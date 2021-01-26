xs = [1, 2, 3, 4, 5]

init' :: [a] -> [a]
init' xs = take (length xs - 1) xs

init'' :: [a] -> [a]
init'' [x] = []
init'' (x : xs) = x : init'' xs

main = do
  print (init' xs)
  print (init'' xs)