```hs
data Expr = Val Int
  | Add Expr Expr
  | Throw
  | Catch Expr Expr

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Add x y) = case eval x of
                    Just n -> case eval y of
                        Just m -> Just (n + m)
                        Nothing -> Nothing
                    Nothing -> Nothing
eval Throw = Nothing
eval (Catch x h) = case eval x of
                      Just n -> Just n
                      Nothing -> eval h
                  
type Stack = [Maybe Int]

data Code = HALT | PUSH Maybe Int Code | ADD Code                
  deriving (Show)

comp :: Expr -> Code                    

comp' :: Expr -> Code -> Code

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m : n : s) = exec c ((+)
```

