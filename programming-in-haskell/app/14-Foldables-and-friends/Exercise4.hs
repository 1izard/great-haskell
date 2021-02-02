import Data.Foldable

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold Leaf = mempty
  fold (Node l x r) = fold l `mappend` fold r `mappend` x

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap f (Node l x r) = foldMap f l `mappend` foldMap f r `mappend` f x

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f v Leaf = v
  foldr f v (Node l x r) = f x (foldr f (foldr f v r) l)

  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f v Leaf = v
  foldl f v (Node l x r) = f (foldl f (foldl f v l) r) x

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree a)
  traverse _ Leaf = pure Leaf
  traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r
