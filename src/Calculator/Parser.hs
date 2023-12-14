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

precedences :: M.Map Int (Parser BinOp)
precedences =
    M.fromList
        [ (2, Add <$ char '+' <|> Subtract <$ char '-')
        , (4, Multiply <$ char '*' <|> Divide <$ char '/')
        ]

conv :: (Ord a) => [([a], b)] -> M.Map a b
conv = M.fromList . concatMap (\(keys, val) -> zip keys (replicate (length keys) val))

ops :: M.Map BinOp Int
ops = conv [([Add, Subtract], 2), ([Multiply, Divide], 4)]

binop :: Parser BinOp -> Parser Expr -> Bool -> Parser Expr
binop op side bail = do
    lhs <- side <?> "lhs"
    rhs <- many ((,) <$> op <*> (side <?> "rhs"))
    if null rhs
        then if bail then fail "unable to find rhs" else pure lhs
        else
            let (op1, rhs1) = head rhs
                start = BinOp op1 lhs rhs1
             in pure $ foldr (\(newOp, newRhs) t -> BinOp newOp t newRhs) start (tail rhs)

add :: Parser Expr
add = binop ((Add <$ char '+' <|> Subtract <$ char '-') <?> "+/-") (mult <|> escaped) True <?> "add"

mult :: Parser Expr
mult = binop ((Multiply <$ char '*' <|> Divide <$ char '/') <?> "*//") escaped False <?> "mult"

negative :: Parser Expr
negative = Negate <$> (char '-' *> escaped) <?> "negative"

number :: Parser Expr
number = Constant . toRational <$> (try float <|> decimal) <?> "number"

escaped :: Parser Expr
escaped = choice [negative, bracketed, number]

bracketed :: Parser Expr
bracketed = between (char '(') (char ')') expr <?> "brackets"

expr :: Parser Expr
expr = (try add <|> mult) <?> "expr"

full :: Parser Expr
full = dbg "full" $ expr <* eof
