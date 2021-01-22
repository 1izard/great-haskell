data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

main = do
  let e = Add (Add (Val 1) (Val 2)) (Add (Add (Val 3) (Val 4)) (Val 5))
  print (eval e) -- 15
  print (size e) -- 5