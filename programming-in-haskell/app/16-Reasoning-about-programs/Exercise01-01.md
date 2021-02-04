```hs
data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)
```

## Exercise 1.  

```
Induction hypothesis: add n (Succ m) = Succ (add n m)

Base case:
add Zero (Succ m)
= { applying add }
Succ m
= { unapplying add }
Succ (add Zero m)

Inductive case:
add (Succ n) (Succ m)
= { applying add }
Succ (add n (Succ m))
= { induction hypothesis }
Succ (Succ (add n m))
= { unapplying add }
Succ (add (Succ n) m)
```

## Exercise 2.

```
Induction hypothesis: add n m = add m n

Base case:
add Zero m
= { applying add }
m
= { using add n Zero = n }
add m Zero

Inductive case:
add (Succ n) m
= { applying add }
Succ (add n m)
= { induction hypothesis }
Succ (add m n)
= { unapplying add }
add m (Succ n)
```

