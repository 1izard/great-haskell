```hs
take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

drop 0 xs = xs
drop _ [] = []
drop n (_ : xs) = drop (n - 1) xs
```

```
Induction hypothesis: take n xs ++ drop n xs = xs

Base case on integer n:
take 0 xs ++ drop 0 xs
= { applying take and drop }
[] ++ xs
= { using [] ++ ys = ys }
xs

Base case on list xs:
take _ [] ++ drop _ []
= { applying take and drop }
[] ++ []
= { applying [] ++ ys }
[]

Inductive case:
take (n + 1) (x : xs) ++ drop (n + 1) (x : xs)
= { applying take and drop }
(x : take n xs) ++ drop n xs
= { using (x : xs) ++ ys = x : (xs ++ ys) }
x : (take n xs ++ drop n xs)
= { induction hypothesis }
x : xs
```