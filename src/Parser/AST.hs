{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Parser.AST where

import Control.Applicative ((<**>))
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

type Parser = Parsec Void T.Text

data Expr = Add Expr Mult | Subtract Expr Mult | OfMult Mult
    deriving (Show)

data Mult = Multiply Mult Negate | Divide Mult Negate | OfNegate Negate
    deriving (Show)

data Negate = Neg Negate | OfAtom Atom
    deriving (Show)

data Atom = Number Rational | Parens Expr
    deriving (Show)

infixL :: (a -> b) -> Parser a -> Parser (b -> a -> b) -> Parser b
infixL wrap p op = postfix wrap p (flip <$> op <*> p)

infixR :: (a -> b) -> Parser a -> Parser (a -> b -> b) -> Parser b
infixR wrap p op = p <**> (continued <|> finished)
  where
    continued = flip <$> op <*> infixR wrap p op
    finished = pure wrap

prefix :: (a -> b) -> Parser (b -> b) -> Parser a -> Parser b
prefix wrap op p = (op <*> prefix wrap op p) <|> (wrap <$> p)

postfix :: (a -> b) -> Parser a -> Parser (b -> b) -> Parser b
postfix wrap p op = (wrap <$> p) <**> rest
  where
    rest = continued <|> finished
    continued = flip (.) <$> op <*> rest
    finished = pure id

data Fixity a b sig where
    InfixL :: Fixity a b (b -> a -> b)
    InfixR :: Fixity a b (a -> b -> b)
    Prefix :: Fixity a b (b -> b)
    Postfix :: Fixity a b (b -> b)

data Op a b where
    Op :: Fixity a b sig -> (a -> b) -> Parser sig -> Op a b

data Prec a where
    Level :: Prec a -> Op a b -> Prec b
    Atom :: Parser a -> Prec a

(>+) = Level
infixl 5 >+
(+<) = flip (>+)

precedence :: Prec a -> Parser a
precedence (Atom a) = a
precedence (Level lvls ops) = con (precedence lvls) ops
  where
    con :: Parser a -> Op a b -> Parser b
    con p (Op InfixL wrap op) = infixL wrap p op
    con p (Op InfixR wrap op) = infixR wrap p op
    con p (Op Prefix wrap op) = prefix wrap op p
    con p (Op Postfix wrap op) = postfix wrap p op

class sub < sup where
    upcast :: sub -> sup

instance Atom < Negate where upcast = OfAtom
instance Negate < Mult where upcast = OfNegate
instance Mult < Expr where upcast = OfMult

sops :: (a < b) => Fixity a b sig -> [Parser sig] -> Op a b
sops fixity = Op fixity upcast . choice

atom :: Parser Atom
atom =
    between (char '(') (char ')') (Parens <$> expr)
        <|> (Number . toRational <$> (try float <|> decimal))

expr :: Parser Expr
expr =
    precedence
        $ Atom atom
        >+ sops Prefix [Neg <$ char '-']
        >+ sops InfixL [Multiply <$ char '*' <|> Divide <$ char '/']
        >+ sops InfixL [Add <$ char '+' <|> Subtract <$ char '/']

full :: Parser Expr
full = expr <* eof
