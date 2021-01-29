data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Functor ((->) a) where
  -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
  fmap g h = g . h

instance Applicative ((->) a) where
  -- pure :: a -> (r -> a)
  pure x = \_ -> x

  -- (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
  g <*> h = \x -> g x (h x)

instance Monad ((->) a) where
  -- return :: a -> (r -> a)
  return = pure

  -- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
  g >>= h = \x -> h (g x) x

newtype ZipList a = Z [a] deriving (Show)

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure xs = Z xs

  -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip (gs xs)]