-- repeat :: a -> [a]
-- repeat x = xs
--   where xs = x : xs

-- take :: Int -> [a] -> [a]
-- take 0 _ = []
-- take _ [] = []
-- take n (x : xs) = x : take (n - 1) xs

-- replicate :: Int -> a -> [a]
-- replicate n = take n . repeat

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

repeat' :: a -> Tree a
repeat' x = Node t x t
  where
    t = Node t x t

take' :: Int -> Tree a -> Tree a
take' 0 _ = Leaf
take' _ Leaf = Leaf
take' n (Node l x r) = Node (take' (n - 1) l) x (take' (n - 1) r)

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat'

main = do
  print (replicate' 3 'a')

-- Node (
--   Node (
--     Node Leaf 'a' Leaf)
--     'a' (
--     Node Leaf 'a' Leaf)
--   )
--   'a' (
--   Node (
--     Node Leaf 'a' Leaf)
--     'a' (
--     Node Leaf 'a' Leaf)
--   )