{-# LANGUAGE OverloadedStrings #-}

module Calculator.Parser where

import Calculator
import Data.Ratio
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void T.Text

binop :: Parser BinOp -> Parser Expr -> Parser Expr
binop op side = do
    lhs <- side
    rhs <- many ((,) <$> op <*> side)
    pure
        $ if null rhs
            then lhs
            else
                let (op1, rhs1) = head rhs
                    start = BinOp op1 lhs rhs1
                 in foldr (\(newOp, newRhs) t -> BinOp newOp t newRhs) start (tail rhs)

add :: Parser Expr
add = binop (Add <$ char '+' <|> Subtract <$ char '-') mult <|> escaped

mult :: Parser Expr
mult = binop (Multiply <$ char '*' <|> Divide <$ char '/') escaped

negative :: Parser Expr
negative = Negate <$> (char '-' *> escaped)

number :: Parser Expr
number = Constant . toRational <$> (try float <|> decimal)

escaped :: Parser Expr
escaped = choice [bracketed, negative, number]

bracketed :: Parser Expr
bracketed = between (char '(') (char ')') expr

expr :: Parser Expr
expr = choice [add, mult, bracketed, escaped]
