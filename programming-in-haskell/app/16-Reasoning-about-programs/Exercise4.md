```hs
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)
```

verify following properties using above definitions.  

```hs
xs ++ [] = xs
xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
```

```
Induction hypothesis: xs ++ [] = xs

Base case:
[] ++ []
= { using [] ++ ys = ys }
[]

Inductive case
(x : xs) ++ []
= { using (x : xs) ++ ys = x : (xs ++ ys) }
x : (xs ++ [])
= { induction hypothesis }
x : xs
```

```
Induction hypothesis: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

Base case:
[] ++ (ys ++ zs)
= { using [] ++ ys = ys }
ys ++ zs
= { using [] ++ ys = ys }
([] ++ ys) ++ zs

Inductive case:
(x : xs) ++ (ys ++ zs)
= { using (x : xs) ++ ys = x : (xs ++ ys) }
x : (xs ++ (ys ++ zs))
= { induction hypothesis }
x : ((xs ++ ys) ++ zs)
= { using (x : xs) ++ ys = x : (xs ++ ys) }
x : (xs ++ ys) ++ zs
= { using (x : xs) ++ ys = x : (xs ++ ys) }
(x : xs ++ ys) ++ zs
```
