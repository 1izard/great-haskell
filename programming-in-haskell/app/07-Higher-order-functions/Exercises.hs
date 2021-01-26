listCmp :: (a -> b) -> (b -> Bool) -> [a] -> [b]
listCmp f p = filter p . map f

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x : xs) = if p x then x : takeWhile' p xs else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p (x : xs) = if p x then dropWhile' p xs else x : xs

map' :: (a -> b) -> [a] -> [b]
map' p = foldr (\x xs -> p x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\v x -> 10 * v + x) 0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y

main = do
  print (listCmp sum (<= 10) [[1, 2, 6], [3, 1, 4], [7, 5, 9]]) -- [9,8]
  print (all' even [2, 4, 6, 8]) -- True
  print (all' even [1, 4, 6, 8]) -- False
  print (any' odd [2, 4, 6, 8]) -- False
  print (any' odd [1, 4, 6, 8]) -- True
  print (takeWhile' (<= 3) [1, 2, 3, 4, 5]) -- [1, 2, 3]
  print (dropWhile' (<= 3) [1, 2, 3, 4, 5]) -- [4, 5]
  print (map' (^ 2) [1, 2, 3, 4, 5]) -- [1, 4, 9, 16, 25]
  print (filter' (<= 3) [3, 5, 2, 1, 4]) -- [3, 2, 1]
  print (dec2int [2, 3, 4, 5]) -- 2345
  print (curry' (\(x, y) -> 10 * x + y) 2 3) -- 23
  print (uncurry' (\x y -> 10 * x + y) (2, 3)) -- 23