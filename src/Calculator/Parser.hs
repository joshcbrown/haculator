{-# LANGUAGE OverloadedStrings #-}

module Calculator.Parser where

import Calculator
import Data.Ratio
import Data.Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

binop :: Parser BinOp -> Parser Expr -> Parser Expr
binop op newExpr = do
    a <- newExpr
    try (BinOp <$> op <*> pure a <*> expr) <|> pure a

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
