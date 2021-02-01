import Data.Foldable

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

instance Foldable Tree where
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l <> foldMap f r

average :: [Int] -> Int
average ns = sum ns `div` length ns

average' :: Foldable t => t Int -> Int
average' ns = sum ns `div` length ns

main = do
  print (average' [1 .. 10])
  print (average' (Node (Leaf 1) (Leaf 3)))