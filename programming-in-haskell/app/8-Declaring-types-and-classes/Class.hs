class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)

instance Eq Bool where
  False == False = True
  True == True = True
  _ == _ = False

class Eq a => Ord a where
  (<), (<=), (>), (>=) :: a -> a -> Bool
  min, max :: a -> a -> a
  min x y
    | x <= y = x
    | otherwise = y
  max x y
    | x <= y = y
    | otherwise = x

instance Ord Bool where
  False < True = True
  _ < _ = False

  b <= c = (b < c) || (b == c)
  b > c = c < b
  b >= c = c <= b