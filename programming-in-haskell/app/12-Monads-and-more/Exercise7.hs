data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving (Show)

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap g (Var a) = Var (g a)
  fmap _ (Val x) = Val x
  fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure = Var

  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  _ <*> Val x = Val x
  (Val x) <*> _ = Val x
  (Var g) <*> ex = fmap g ex
  (Add eg eh) <*> ex = Add (eg <*> ex) (eh <*> ex)

instance Monad Expr where
  -- return :: a -> Expr a
  return = pure

  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (Val x) >>= f = Val x
  (Var x) >>= f = f x
  (Add ex ey) >>= f = Add (ex >>= f) (ey >>= f)

ec :: Expr Char
ec = Add (Add (Var 'a') (Var 'b')) (Val 3)

vars :: Char -> Expr Int
vars 'a' = Val 1
vars 'b' = Val 2

subst :: Expr a -> (a -> Expr b) -> Expr b
subst ex vs = ex >>= vs

eval :: Expr a -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y

main = do
  print (eval (subst ec vars)) -- 6