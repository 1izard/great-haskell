evens :: Integral a => a -> [a]
evens n = [x | x <- [0 .. n], even x]

factors :: Integral a => a -> [a]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

prime :: Integral a => a -> Bool
prime n = factors n == [1, n]

primes :: Integral a => a -> [a]
primes n = [x | x <- [2 .. n], prime x]

find :: Eq a1 => a1 -> [(a1, a2)] -> [a2]
find k t = [v | (k', v) <- t, k == k']

main = do
  print (evens 10) -- [0,2,4,6,8,10]
  print (factors 12) -- [1,2,3,4,6,12]
  print (prime 15) -- False
  print (prime 7) -- True
  print (primes 40) -- [2,3,5,7,11,13,17,19,23,29,31,37]
  print (find 'b' [('a', 1), ('b', 2), ('c', 3), ('b', 4)]) -- [2, 4]
