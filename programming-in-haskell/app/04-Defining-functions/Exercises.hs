halve xs = splitAt n xs
  where
    n = length xs `div` 2

third :: [a] -> a
third xs = head (tail (tail xs))

thirdB xs = xs !! 2

thirdC (_ : _ : x : _) = x

safetail :: [a] -> [a]
safetail xs = if null xs then tail xs else xs

safetailB :: [a] -> [a]
safetailB xs
  | null xs = tail xs
  | otherwise = xs

safetailC :: [a] -> [a]
safetailC (_ : xs) = xs
safetailC [] = []

-- (||) :: Bool -> Bool -> Bool
-- False || False = False
-- _ || _ = True

-- (&&) :: Bool -> Bool -> Bool
-- (&&) x y =
--   if x
--     then y
--     else False

-- (&&) :: Bool -> Bool -> Bool
-- (&&) x b =
--   if x
--     then b
--     else False

mult :: Integer -> Integer -> Integer -> Integer
mult = \x -> (\y -> (\z -> x * y * z))

luhnDouble x =
  if x * 2 > 9
    then x * 2 - 9
    else x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w + x + luhnDouble y + z) `mod` 10 == 0

main = do
  let xs = [1, 2, 3, 4, 5, 6]
  print (halve xs)
  print (third xs)
  print (thirdB xs)
  print (thirdC xs)
  print (safetailC xs)
  print (safetailC [] :: [Int])
  print (luhn 1 7 8 4) -- True
  print (luhn 4 7 8 3) -- False
