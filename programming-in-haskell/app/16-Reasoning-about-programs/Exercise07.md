```hs
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap g (Just x) = Just (g x)
```

```
-- functor laws
fmap id = id
fmap (g . h) = fmap g . fmap h
```

```
Proof of fmap id = id

case 1:
fmap id Nothing
= { applying fmap }
Nothing
= { unapplying id }
id Nothing

case 2:
fmap id (Just x)
= { applying fmap }
Just (id x)
= { applying id }
Just x
= { unapplying id }
id (Just x)
```

```
Proof of fmap (g . h) = fmap g . fmap h

case 1:
fmap (g . h) Nothing
= { applying fmap }
Nothing
= { unapplying fmap }
fmap h Nothing
= { unapplying fmap }
fmap g (fmap h Nothing)
= { using (g . h) x = g (h (x)) }
fmap g . fmap h Nothing

case 2:
fmap (g . h) (Just x)
= { applying fmap }
Just ((g . h) x)
= { using (g . h) x = g (h (x)) }
Just (g (h (x)))
= { unapplying fmap }
fmap g (Just (h (x)))
= { unapplying fmap }
fmap g (fmap h (Just x))
= { using (g . h) x = g (h (x)) }
fmap g . fmap h (Just x)
```
