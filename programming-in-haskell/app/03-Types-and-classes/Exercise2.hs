bools :: [Bool]
bools = [True, False, True, False]

nums :: [[Int]]
nums = [[1, 2, 3], [4, 5], [6, 7, 8, 9]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply f = f
