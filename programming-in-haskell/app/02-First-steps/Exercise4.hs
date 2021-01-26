xs = [1, 2, 3, 4, 5]

last' :: [a] -> a
last' xs = xs !! (length xs - 1)

last'' :: [a] -> a
last'' [x] = x
last'' (x : xs) = last'' xs

main = do
  print (last' xs)
  print (last'' xs)