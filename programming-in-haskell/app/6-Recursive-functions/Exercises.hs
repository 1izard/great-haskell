fac :: Int -> Int
fac n
  | n <= 0 = 1
  | otherwise = n * fac (n - 1)

sumdown' :: Int -> Int
sumdown' n
  | n <= 0 = 0
  | otherwise = n + sumdown' (n - 1)

(^+) :: Int -> Int -> Int
x ^+ 0 = 1
x ^+ n = x * x ^+ (n - 1)

euclid' :: Int -> Int -> Int
euclid' a 0 = a
euclid' a b = euclid' b (a `mod` b)

and' :: [Bool] -> Bool
and' [True] = True
and' (False : _) = False
and' (True : xs) = and' xs

concat' :: [[a]] -> [a]
concat' = foldr (++) []

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

(!!+) :: [a] -> Int -> a
(x : xs) !!+ 0 = x
(x : xs) !!+ n = xs !!+ (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' v [] = False
elem' v (x : xs)
  | v == x = True
  | otherwise = elem' v xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt n xs
  where
    n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort xs') (msort ys')
  where
    (xs', ys') = halve xs

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

take' :: Int -> [a] -> a
take' 0 (x : _) = x
take' n (x : xs) = take' (n - 1) xs

last' :: [a] -> a
last' [x] = x
last' (x : xs) = last xs

main = do
  print (fac 6) -- 720
  print (sumdown' 3) -- 6
  print (2 ^+ 3) -- 8
  print (euclid' 6 27) -- 3
  print (concat' [[1, 2, 3], [4, 5], [6, 7, 8, 9]]) -- [1,2,3,4,5,6,7,8,9]
  print (replicate' 3 10) -- [10,10,10]
  print ([10, 11, 12, 13] !!+ 2) -- 12
  print (elem' 3 [1, 2, 3, 4, 5]) -- True
  print (merge [2, 5, 6] [1, 3, 4]) -- [1,2,3,4,5,6]
  print (msort [5, 8, 1, 2, 9, 3, 4, 7, 6]) -- [1,2,3,4,5,6,7,8,9]
  print (sum' [1, 2, 3, 4, 5]) -- 15
  print (take' 2 [10, 11, 12, 13]) -- 12
  print (last' [1, 2, 3, 4, 5]) -- 5
