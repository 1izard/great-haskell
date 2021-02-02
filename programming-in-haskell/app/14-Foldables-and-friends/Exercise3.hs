import Data.Foldable

instance Foldable Maybe where
  -- fold :: Monoid a => Maybe a -> a
  fold Nothing = mempty
  fold (Just x) = x

  -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
  foldMap _ Nothing = mempty
  foldMap f (Just x) = f x

  -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr _ v Nothing = v
  foldr f v (Just x) = f x v

  -- foldl :: (a -> b -> a) -> a -> Myabe b -> a
  foldl _ v Nothing = v
  foldl f v (Just x) = f v x

instance Traversable Maybe where
  -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing = pure Nothing
  traverse g (Just x) = fmap g x
