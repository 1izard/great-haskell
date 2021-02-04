```
given: comp' e c = comp e ++ c
```

```
Base case:
comp' (Val x) c
= { specification of comp' }
comp (Val x) ++ c
= { applying comp }
[PUSH n] ++ c
= { applying comp }
PUSH n : c

Inductive case:
comp' (Add x y) c
= { specification of comp' }
comp (Add x y) ++ c
= { applying comp }
(comp x ++ comp y ++ [ADD]) ++ c
= { associativity o+ ++ }
comp x ++ (comp y ++ [ADD] ++ c)
= { induction hypothesis for x }
comp' x (comp y ++ [ADD] ++ c)
= { induction hypothesis for y }
comp' x (comp' y ([ADD] ++ c))
= { applying ++ }
comp' x (comp' y (ADD : c))
```