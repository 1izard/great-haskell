mul' :: Num a => [a] -> a
mul' [] = 1
mul' (x : xs) = x * mul' xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

rqsort :: Ord a => [a] -> [a]
rqsort [] = []
rqsort (x : xs) = rqsort larger ++ [x] ++ rqsort smaller
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b >= x]

main = do
  print (mul' [2, 3, 4])
  print (qsort [5, 1, 4, 2, 3])
  print (rqsort [5, 1, 4, 2, 3])