{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

{-
 - parser structure borrowed from the excellent
 - Design patterns for parser combinators (functional pearl)
 - https://dl.acm.org/doi/10.1145/3471874.3472984
-}

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

data Mult = Multiply Mult Expo | Divide Mult Expo | OfExpo Expo
    deriving (Show)

data Expo = Expo Negate Expo | OfNegate Negate
    deriving (Show)

data Negate = Neg Negate | OfAtom Atom
    deriving (Show)

data Atom = Number Rational | Ident String | Parens Expr
    deriving (Show)

data Line = Equate Expr Expr | Expression Expr
    deriving (Show)

instance Atom < Negate where upcast = OfAtom
instance Negate < Expo where upcast = OfNegate
instance Expo < Mult where upcast = OfExpo
instance Mult < Expr where upcast = OfMult

ident :: Parser String
ident = token $ (:) <$> letterChar <*> many alphaNumChar

atom :: Parser Atom
atom =
    choice
        [ between (symbol "(") (symbol ")") (Parens <$> expr)
        , Number . toRational <$> token L.scientific
        , Ident <$> ident
        ]

expr :: Parser Expr
expr =
    precedence
        $ Atom atom
        >+ sops Prefix [Neg <$ symbol "-"]
        >+ sops InfixR [Expo <$ symbol "^"]
        >+ sops InfixL [Multiply <$ (symbol "*" <|> void (lookAhead ident)), Divide <$ symbol "/"]
        >+ sops InfixL [Add <$ symbol "+", Subtract <$ symbol "-"]

full :: Parser Line
full = do
    lhs <- expr
    (Equate lhs <$> (symbol "=" *> expr <* eof))
        <|> (Expression lhs <$ eof)
