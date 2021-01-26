abs n
  | n >= 0 = n
  | otherwise = - n

signum n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

main = do
  print (abs 3)