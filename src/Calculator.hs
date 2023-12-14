module Calculator where

data BinOp = Add | Subtract | Multiply | Divide
    deriving (Show)

data Expr = Constant Rational | BinOp BinOp Expr Expr | Negate Expr
    deriving (Show)

eval :: Expr -> Rational
eval (Constant x) = x
eval (Negate x) = negate (eval x)
eval (BinOp op x y) = case op of
    Add -> eval x + eval y
    Subtract -> eval x - eval y
    Multiply -> eval x * eval y
    Divide -> eval x / eval y
