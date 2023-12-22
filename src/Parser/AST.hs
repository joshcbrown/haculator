{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Parser.AST where

import Parser.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

data Expr = Add Expr Mult | Subtract Expr Mult | OfMult Mult
    deriving (Show)

data Mult = Multiply Mult Negate | Divide Mult Negate | OfNegate Negate
    deriving (Show)

data Negate = Neg Negate | OfAtom Atom
    deriving (Show)

data Atom = Number Rational | Parens Expr
    deriving (Show)

instance Atom < Negate where upcast = OfAtom
instance Negate < Mult where upcast = OfNegate
instance Mult < Expr where upcast = OfMult

atom :: Parser Atom
atom =
    between (char '(') (char ')') (Parens <$> expr)
        <|> (Number . toRational <$> (try (float :: Parser Double) <|> decimal))

expr :: Parser Expr
expr =
    precedence
        $ Atom atom
        >+ sops Prefix [Neg <$ char '-']
        >+ sops InfixL [Multiply <$ char '*' <|> Divide <$ char '/']
        >+ sops InfixL [Add <$ char '+' <|> Subtract <$ char '-']

full :: Parser Expr
full = expr <* eof
