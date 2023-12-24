{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Parser.AST where

import Control.Monad (void)
import Parser.Common
import Parser.Expr
import Parser.Lexer
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)

data Expr = Add Expr Mult | Subtract Expr Mult | OfMult Mult
    deriving (Show)

data Mult = Multiply Mult Negate | Divide Mult Negate | OfNegate Negate
    deriving (Show)

data Negate = Neg Negate | OfAtom Atom
    deriving (Show)

data Atom = Number Rational | Ident String | Parens Expr
    deriving (Show)

data Line = Equate Expr Expr | Expression Expr

instance Atom < Negate where upcast = OfAtom
instance Negate < Mult where upcast = OfNegate
instance Mult < Expr where upcast = OfMult

ident :: Parser String
ident = token $ (:) <$> letterChar <*> many alphaNumChar

atom :: Parser Atom
atom =
    choice
        [ between (symbol "(") (symbol ")") (Parens <$> expr)
        , Number . toRational <$> token (try (L.float :: Parser Double) <|> L.decimal)
        , Ident <$> ident
        ]

expr :: Parser Expr
expr =
    precedence
        $ Atom atom
        >+ sops Prefix [Neg <$ symbol "-"]
        >+ sops InfixL [Multiply <$ (symbol "*" <|> void (lookAhead ident)), Divide <$ symbol "/"]
        >+ sops InfixL [Add <$ symbol "+", Subtract <$ symbol "-"]

full :: Parser Line
full = do
    lhs <- expr
    (Equate lhs <$> (symbol "=" *> expr <* eof))
        <|> (Expression lhs <$ eof)
