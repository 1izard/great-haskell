import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
-- bin2int bits = sum [w * b | (w, b) <- zip weights bits]
--   where
--     weights = iterate (* 2) 1
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

addParity :: [Bit] -> [Bit]
addParity bits = if odd (sum bits) then bits ++ [1] else bits ++ [0]

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

parityCheck :: [Bit] -> [Bit]
parityCheck bits = if even (sum bits) then init bits else error "Incorrect!"

decode :: [Bit] -> String
decode = map (chr . bin2int . parityCheck) . chop9

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x : xs) = f x : altMap g f xs

luhnDouble :: Int -> Int
luhnDouble x
  | x * 2 > 9 = x * 2 - 9
  | otherwise = x * 2

luhn :: [Int] -> Bool
luhn xs = sum (altMap id luhnDouble xs) `mod` 10 == 0

main = do
  print (bin2int [1, 0, 1, 1]) -- 13
  print (int2bin 13) -- [1, 0, 1, 1]
  print (make8 [1, 0, 1, 1]) -- [1,0,1,1,0,0,0,0]
  print (addParity [1, 0, 0, 0, 0, 0, 0, 0])
  print (encode "abc") -- [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
  print (decode [1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0]) -- "abc"
  -- print (decode (tail [1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0])) -- "Incorrect!"
  print (altMap (+ 10) (+ 100) [0, 1, 2, 3, 4]) -- [10,101,12,103,14]
  print (luhn [7, 9, 9, 2, 7, 3, 9, 8, 7, 1, 3]) -- True
