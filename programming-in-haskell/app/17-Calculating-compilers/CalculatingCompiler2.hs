data Expr = Val Int | Add Expr Expr

type Stack = [Int]

push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m : n : s) = n + m : s

eval :: Expr -> Int
eval e = head (eval' e [])

type Cont = Stack -> Stack

eval'' :: Expr -> Cont -> Cont
eval'' (Val n) c s = c (push n s)
eval'' (Add x y) c s = eval'' x (eval'' y (c . add)) s

eval' :: Expr -> Cont
eval' e s = eval'' e id s

data Code = HALT | PUSH Int Code | ADD Code
  deriving (Show)

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m : n : s) = exec c (n + m : s)

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))