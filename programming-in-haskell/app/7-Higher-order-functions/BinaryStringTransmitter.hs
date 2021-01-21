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

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

main = do
  print (bin2int [1, 0, 1, 1]) -- 13
  print (int2bin 13) -- [1, 0, 1, 1]
  print (make8 [1, 0, 1, 1]) -- [1,0,1,1,0,0,0,0]
  print (encode "abc") -- [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
  print (decode [1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0]) -- "abc"
  print ()
  print (transmit "higher-order functions are easy") -- "higher-order functions are easy"