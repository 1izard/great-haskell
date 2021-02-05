data Expr = Val Int | Add Expr Expr

-- eval :: Expr -> Int
-- eval (Val n) = n
-- eval (Add x y) = eval x + eval y

type Stack = [Int]

-- eval' :: Expr -> Stack -> Stack
-- eval' (Val n) s = push n s
-- eval' (Add x y) s = add (eval' y (eval' x s))

push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m : n : s) = n + m : s

eval :: Expr -> Int
eval e = head (eval' e [])

type Cont = Stack -> Stack

-- eval'' :: Expr -> Cont -> Cont
-- eval'' (Val n) c s = c (push n s)
-- eval'' (Add x y) c s = eval'' x (eval'' y (c . add)) s

-- eval' :: Expr -> Cont
-- eval' e s = eval'' e id s

haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c = c . push n

addC :: Cont -> Cont
addC c = c . add

eval' :: Expr -> Cont
eval' e = eval'' e haltC

eval'' :: Expr -> Cont -> Cont
eval'' (Val n) c = pushC n c
eval'' (Add x y) c = eval'' x (eval'' y (addC c))

data Code = HALT | PUSH Int Code | ADD Code
  deriving (Show)

-- exec :: Code -> Cont
-- exec HALT = haltC
-- exec (PUSH n c) = pushC n (exec c)
-- exec (ADD c) = addC (exec c)

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m : n : s) = exec c (n + m : s)

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))