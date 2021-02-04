```hs
instance Applicative Maybe where
    -- pure :: a -> Maybe a
  pure = Just

  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _ = Nothing
  (Just g) <*> mx = fmap g mx
```

```
-- applicative laws

pure id <*> x = x -- pure is complemented
pure (g x) = pure g <*> pure x  -- occurences of pure can be combined into one
x <*> pure y = pure (\g -> g y) <*> x -- the order in which we evaluate the two components doesn't matter
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z  -- <*> is associativ
```

```
Proof of pure id <*> x = x

case 1:
pure id <*> Nothing
= { applying pure }
Just id <*> Nothing
= { applying <*> }
fmap id Nothing
= { applying fmap }
Nothing

case 2:
pure id <*> Just x
= { applying pure }
Just id <*> Just x
= { applying <*> }
fmap id (Just x)
= { applying fmap }
Just x
```

```
Proof of pure (g x) = pure g <*> pure x

case 1:
pure (g Nothing)
= { applying pure }
Just (g Nothing)
= { unapplying fmap }
fmap g Just (Nothing)
= { unapplying <*> }
(Just g) <*> Just (Nothing)
= { unapplying pure }
pure g <*> pure Nothing

case 2:
pure (g (Just x))
= { applying pure }
Just (g (Just x))
= { unapplying fmap }
fmap g (Just (Just x))
= { applying <*> }
(Just g) <*> (Just x)
= { unapplying pure }
pure g <*> pure x
```

```
Proof of x <*> pure y = pure (\g -> g y) <*> x

case 1:
pure (\g -> g y) <*> Nothing
= { applying pure }
Just (\g -> g y)
= { applying <*> }
fmap (\g -> g y) Nothing
= { applying fmap }
Nothing
= { applying <*> }
Nothing <*> pure y

case 2:
pure (\g -> g y) <*> Just x
= { applying pure }
Just (\g -> g y) <*> Just x
= { applying <*> }
fmap (\g -> g y) (Just x)
= { applying fmap }
Just (x y)
= { unapplying pure }
pure (x y)
= { using pure (g x) = pure g <*> pure y }
pure x <*> pure y
= { applying pure }
Just x <*> pure y
```

```
Proof of x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

case 1 (show only case such y = Nothing):
Just x <*> (Nothing <*> Just z)
= { applying the second <*> }
Just x <*> Nothing
= { applying <*> }
fmap x Nothing
= { applying fmap }
Nothing

case 2 (show only case such x, y and z are Just x, Just y, and Just z respectively)
Just x <*> (Just y <*> Just z)
= { unapplying pure }
Just x <*> (pure y <*> pure z)
= { using pure (x y) <*> pure x <*> pure y }
Just x <*> pure (y z)
= { unapplying pure }
pure x <*> pure (y z)
= { using pure (x y) <*> pure x <*> pure y }
pure (x (y z))
= { using f . g x = f (g (x)) }
pure (x . y z)
= { using pure (x y) <*> pure x <*> pure y }
pure (x . y) <*> pure z
= { applying pure }
Just (x . y) <*> Just z
= { unapplying fmap }
fmap ((.) x) (Just y) <*> Just z
= { unapplying <*> }
(Just ((.) x) <*> Just y) <*> Just z
= { unapplying fmap }
(fmap (.) (Just x) <*> Just y) <*> Just z
= { unapplying <*> }
(Just (.) <*> Just x <*> Just y) <*> Just z
= { unapplying first Just }
(pure (.) <*> Just x <*> Just y) <*> Just z
```