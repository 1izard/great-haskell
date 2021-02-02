primes :: [Int]
primes = sieve [2 ..]

sieve :: [Int] -> [Int]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main = do
  print (take 10 primes) -- [2,3,5,7,11,13,17,19,23,29]
  print (takeWhile (< 10) primes) -- [2,3,5,7]