```hs
data Tree = Leaf Int | Node Tree Tree
```

suppose trees that is one greater than the number of nodes.  
as n is the number of leaves in the tree and m is the number of nodes,
n = m + 1 holds.  

At first, define the following function to count leaves and nodes.  

```hs
-- return (m, n)
count :: Tree -> (Int, Int)
count (Leaf _) = (0, 1)
count (Node l r) = (fst cl + fst cr + 1, snd cl + snd cr)
  where
    cl = count l
    cr = count r
```

```
Induction hypothesis: count tree = (m, m + 1)

Base case:
count (Leaf _) = (0, 1)

Inductive case:
count (Node l r)
= (fst cl + fst cr + 1, snd cl + snd cr)
= (fst (ml, ml + 1) + fst (mr, mr + 1) + 1, snd (ml, ml + 1) + snd (mr, mr + 1))
= (ml + mr + 1, (ml + 1) + (mr + 1))
= (ml + mr + 1, (ml + mr + 1) + 1)
```