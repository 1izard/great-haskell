```hs
instance Monad [] where
  -- return :: a -> [a]
  return = pure

  -- (>>=) :: m a -> (a -> m b) -> m b
  xs >>= f = [y | x <- xs, y <- f x]
```

```
-- monad laws
return x >>= f = f x
mx >>= return = mx
(mx >>= f) >>= g = mx >= (\x -> (f x >>= g))
```

```
Proof of return x >>= f = f x

return x >>= f
= { applying return }
pure x >>= f
= { applying pure }
[x] >>= f
= { applying >>= }
[z | y <- [x], y <- f y]
= { applying the first generator }
[z | z <- f x]
= { unusing list comprehension }
f x
```

```
Proof of mx >>= return = mx

[x] >>= return
= { applying >>= }
[z | y <- [x], z <- return y]
= { applying the first generator }
[z | z <- return x]
= { applying return }
[z | z <- pure x]
= { applying pure }
[z | z <- [x]]
= { unusing list comprehension }
[x]
```

```
Proof of (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))

([x] >>= f) >>= g
= { applying the second >>= }
[z | y <- ([x] >>= f), z <- g y]
= { applying >>= }
[z | y <- [z' | y' <- [x], z' <- f y'], z <- g y]
= { applying the second generator }
[z | y <- [z' | z' <- f x], z <- g y]
= { unusing inner list comprehension }
[z | y <- f x, z <- g y]
= { unapplying >>= }
f x >>= g
= { using list comprehension }
[z | z <- (f x >>= g)]
= { unapplying generator }
[z | y <- [x], z <- (\x -> (f x >>= g)) y]
= { unapplying >>= }
[x] >>= (\x -> (f x >>= g))
```