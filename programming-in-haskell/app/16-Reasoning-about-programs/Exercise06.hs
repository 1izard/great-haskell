data Tree = Leaf Int | Node Tree Tree

count :: Tree -> (Int, Int)
count (Leaf _) = (0, 1)
count (Node l r) = (fst cl + fst cr + 1, snd cl + snd cr)
  where
    cl = count l
    cr = count r

main = do
  print (count (Leaf 0))
  print (count (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))))
