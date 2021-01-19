import Data.Char (isAsciiLower)

lowers :: [Char] -> Int
lowers xs = length [x | x <- xs, isAsciiLower x] -- [is]

count :: Eq a => a -> [a] -> Int
count x xs = length [x' | x' <- xs, x == x']

main = do
  print ("abcde" !! 2) -- 'c'
  print (take 3 "abcde") -- "abc"
  print (length "abcde") -- 5
  print (zip "abc" [1, 2, 3, 4]) -- [('a',1),('b',2),('c',3)]
  print (lowers "Haskell") -- 6
  print (count 's' "Mississippi") -- 4