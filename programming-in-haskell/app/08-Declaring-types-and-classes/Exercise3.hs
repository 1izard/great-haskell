data Tree a = Leaf a | Node (Tree a) (Tree a)

cnt :: Tree a -> Int
cnt (Leaf x) = 1
cnt (Node x y) = cnt x + cnt y

balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node x y) = abs (cnt x - cnt y) <= 1

t1 :: Tree Int
t1 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))

t2 :: Tree Int
t2 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Node (Leaf 5) (Leaf 6))))

main = do
  print (balanced t1) -- True
  print (balanced t2) -- False