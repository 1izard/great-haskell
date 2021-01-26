data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance xs') (balance ys')
  where
    (xs', ys') = splitAt (length xs `div` 2) xs

main = do
  print (balance [1, 2, 3, 4, 5, 6, 7, 8, 9])

{-
Node
  (Node
    (Node
      (Leaf 1) (Leaf 2))
    (Node
      (Leaf 3) (Leaf 4)))
  (Node
    (Node
      (Leaf 5) (Leaf 6))
    (Node
      (Leaf 7)
      (Node
        (Leaf 8) (Leaf 9))))
-}