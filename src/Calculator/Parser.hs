module Calculator.Parser where

import Data.Bool (bool)
import qualified Data.Map as M
import Data.Ratio
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Debug

data BinOp = Add | Subtract | Multiply | Divide
    deriving (Show, Eq, Ord)

data Expr = Constant Rational | BinOp BinOp Expr Expr | Negate Expr
    deriving (Show)

type Parser = Parsec Void T.Text




binop :: Parser BinOp -> Parser Expr -> Bool -> Parser Expr
binop op side bail = do
    lhs <- dbg "lhs" side
    rhs <- dbg "rhs" $ many ((,) <$> op <*> side)
    if null rhs
        then if bail then fail "unable to find" else pure lhs
        else
            let (op1, rhs1) = head rhs
                start = BinOp op1 lhs rhs1
             in pure $ foldr (\(newOp, newRhs) t -> BinOp newOp t newRhs) start (tail rhs)

add :: Parser Expr
add = dbg "add" $ binop (Add <$ char '+' <|> Subtract <$ char '-') (mult <|> escaped) True

mult :: Parser Expr
mult = dbg "mult" $ binop (Multiply <$ char '*' <|> Divide <$ char '/') escaped False

negative :: Parser Expr
negative = dbg "neg" $ Negate <$> (char '-' *> escaped)

number :: Parser Expr
number = dbg "num" $ Constant . toRational <$> (try float <|> decimal)

escaped :: Parser Expr
escaped = choice [negative, bracketed, number]

bracketed :: Parser Expr
bracketed = dbg "brackets" $ between (char '(') (char ')') expr

expr :: Parser Expr
expr = try add <|> mult

full :: Parser Expr
full = dbg "full" $ expr <* eof
