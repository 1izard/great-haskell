instance Eq a => Eq (Maybe a) where
  Just x == Just y = x == y
  Nothing == Nothing = x == y
  _ == _ = False
  x /= y = not (x == y)

instance Eq a => Eq [a] where
  x == y = (length x == length y) && all [x' == y' | (x', y') <- zip x y]
  x /= y = not (x == y)