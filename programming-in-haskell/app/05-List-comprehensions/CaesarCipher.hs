import Data.Char

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

lowers :: [Char] -> Int
lowers xs = length [x | x <- xs, isAsciiLower x]

count :: Eq a => a -> [a] -> Int
count x xs = length [x' | x' <- xs, x == x']

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> [Char] -> [Char]
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']] where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (- factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs

main = do
  print (encode 3 "haskell is fun") -- "kdvnhoo lv ixq"
  print (encode (-3) "kdvnhoo lv ixq") -- "haskell is fun"
  print (percent 5 15) -- 33.33333333333333
  print (freqs "abbcccddddeeeee") -- [6.666667,13.333334,20.0,26.666668,33.333336,...,0.0,0.0]
  print (rotate 3 [1, 2, 3, 4, 5]) -- [4,5,1,2,3]
  print (crack "kdvnhoo lv ixq") -- "haskell is fun"
  print (crack "vscd mywzboroxcsyxc kbo ecopev") -- "list comprehensions are useful"