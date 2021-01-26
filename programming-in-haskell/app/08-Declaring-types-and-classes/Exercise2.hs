-- data Ordering = LT | EQ | GT
-- compare :: Ord a => a -> a -> Ordering

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare x y of
  EQ -> True
  LT -> occurs x l
  GT -> occurs x r

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

main = do
  print (occurs 5 t) -- True
  print (occurs 8 t) -- False