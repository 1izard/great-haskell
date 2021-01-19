pairs :: [b] -> [(b, b)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

main = do
  print (zip ['a', 'b', 'c'] [1, 2, 3, 4]) -- [('a',1),('b',2),('c',3)]
  print (pairs [1, 2, 3, 4]) -- [(1,2),(2,3),(3,4)]
  print (sorted [1, 2, 3, 4]) -- True
  print (sorted [1, 3, 2, 4]) -- False
  print (positions False [True, False, True, False])
