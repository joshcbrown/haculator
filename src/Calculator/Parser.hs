module Calculator.Parser where

import Data.Bool (bool)
import Data.Functor (($>))
import qualified Data.Map as M
import Data.Ratio
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Debug

data BinOp = Add | Subtract | Multiply | Divide
    deriving (Eq, Ord, Enum, Bounded)

instance Show BinOp where
    show Add = "+"
    show Subtract = "-"
    show Multiply = "*"
    show Divide = "/"

data Expr = Number Rational | BinOp BinOp Expr Expr | Negate Expr
    deriving (Show)

showExpr :: Expr -> String
showExpr (Number x) = (show . fromRational) x
showExpr (BinOp op x y) = "(" ++ showExpr x ++ show op ++ showExpr y ++ ")"
showExpr (Negate x) = "-(" ++ showExpr x ++ ")"

failInput = "((-((-2.0*-(-(((1.5+-((-(2.0)--(-(2.0)))))+0.5)))))"

type Parser = Parsec Void T.Text

binOp :: Parser BinOp -> Parser Expr -> Parser Expr
binOp op side = do
    lhs <- side
    rhs <- some ((,) <$> op <*> side)
    let (op1, rhs1) = head rhs
        start = BinOp op1 lhs rhs1
    pure
        $ foldr (\(newOp, newRhs) t -> BinOp newOp t newRhs) start (tail rhs)

add :: Parser Expr
add = binOp (Add <$ char '+' <|> Subtract <$ char '-') (try mult <|> negatable)

mult :: Parser Expr
mult = binOp (Multiply <$ char '*' <|> Divide <$ char '/') negatable

negative :: Parser Expr
negative = Negate <$> (char '-' *> negatable)

number :: Parser Expr
number = Number . toRational <$> (try float <|> decimal)

negatable :: Parser Expr
negatable = choice [negative, bracketed, number]

bracketed :: Parser Expr
bracketed = between (char '(') (char ')') expr

expr :: Parser Expr
expr = try add <|> try mult <|> negatable

full :: Parser Expr
full = expr <* eof
