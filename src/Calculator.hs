module Calculator where

import Control.Arrow (ArrowChoice (left))
import Control.Exception (ArithException (..))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Text as T
import Parser.AST
import Text.Megaparsec (errorBundlePretty, runParser)

evalAtom :: Atom -> Either ArithException Rational
evalAtom (Number x) = Right x
evalAtom (Parens e) = eval e
evalAtom (Var _) = Left Denormal

evalNegate :: Negate -> Either ArithException Rational
evalNegate (Neg n) = negate <$> evalNegate n
evalNegate (OfAtom a) = evalAtom a

evalMult :: Mult -> Either ArithException Rational
evalMult (Multiply m n) = (*) <$> evalMult m <*> evalNegate n
evalMult (Divide m n) =
    case evalNegate n of
        e@(Left _) -> e
        (Right 0) -> Left DivideByZero
        (Right x) -> (/ x) <$> evalMult m
evalMult (OfNegate n) = evalNegate n

eval :: Expr -> Either ArithException Rational
eval (Add e m) = (+) <$> eval e <*> evalMult m
eval (Subtract e m) = (-) <$> eval e <*> evalMult m
eval (OfMult m) = evalMult m

calculate :: (Fractional a) => String -> Either String a
calculate =
    join
        . left errorBundlePretty
        . fmap (bimap show fromRational . eval)
        . runParser full "input"
        . T.pack
