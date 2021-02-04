```hs
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

all p [] = True
all p (x : xs) = p x && all p xs
```

```
Induction hypothesis: all (== x) (replicate n xs) = True

Base case:
all (== x) (replicate 0 xs)
= { applying replicate }
all (== x) []
= { applying all }
= True

Inductive case:
all (== x) (replicate (n + 1) xs)
= { applying replicate }
all (== x) (x : replicate n xs)
= { applying all }
(x == x) && (all (== x) (replicate n xs))
= { induction hypothesis }
True
```

