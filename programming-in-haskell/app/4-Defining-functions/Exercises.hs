halve xs = splitAt n xs
  where
    n = length xs `div` 2

third xs = head (tail (tail xs))

thirdB xs = xs !! 2

thirdC (_ : _ : x : _) = x

safetail xs = if null xs then tail xs else xs

safetailB xs
  | null xs = tail xs
  | otherwise = xs

safetailC (_ : xs) = xs
safetailC [] = []

False || False = False
_ || _ = True

-- (&&) x y = if x then if y then True else False else False
-- (&&) True b = if True then if b then b else False else False

mult :: Integer -> Integer -> Integer -> Integer
mult = \x -> (\y -> (\z -> x * y * z))

luhnDouble i x =
  if odd i
    then
      if x * 2 > 9
        then x * 2 - 9
        else x * 2
    else x

luhn xs = sum (zipWith luhnDouble [0 ..] (reverse xs)) `mod` 10 == 0

main = do
  let xs = [1, 2, 3, 4, 5, 6]
  print (halve xs)
  print (third xs)
  print (thirdB xs)
  print (thirdC xs)
  print (safetailC xs)
  print (safetailC [] :: [Int])
  print (luhn [1, 7, 8, 4])
  print (luhn [4, 7, 8, 3])
  print (luhn [7, 9, 9, 2, 7, 3, 9, 8, 7, 1, 3])
