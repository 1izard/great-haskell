filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF g = foldMap (\x -> [x | g x])