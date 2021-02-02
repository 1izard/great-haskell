sqroot :: Double -> Double
sqroot n = head [y| (x, y) <- zip xs ys, abs (x - y) <= dist]
  where
    a0 = 1.0
    dist = 0.00001
    next = \x -> (x + n / x) / 2
    xs = iterate next a0
    ys = iterate next (head (tail xs))
    
main = do
  print (sqroot 2)  -- 1.4142135623746899
  print (sqroot 3)  -- 1.7320508075688772
  print (sqroot 100)  -- 10.0