{-# LANGUAGE DeriveFunctor #-}

module Calculator where

import Calculator.LinearExpr
import Control.Arrow (ArrowChoice (left))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Either (isRight)
import qualified Data.Text as T
import Parser.AST
import Text.Megaparsec (errorBundlePretty, runParser)

data Solution a = Solved Term a
    deriving (Functor, Eq)

instance (Show a) => Show (Solution a) where
    show (Solved (Var s) x) = s ++ show (Solved Constant x)
    show (Solved Constant x) = " = " ++ show x

evalAtom :: Atom -> EvalResult (LinearExpr Rational)
evalAtom (Number x) = Right $ single Constant x
evalAtom (Parens e) = eval e
evalAtom (Ident s) = Right $ single (Var s) 1

evalNegate :: Negate -> EvalResult (LinearExpr Rational)
evalNegate (Neg n) = fmap negate <$> evalNegate n
evalNegate (OfAtom a) = evalAtom a

evalExpo :: Expo -> EvalResult (LinearExpr Rational)
evalExpo (Expo n e) = join $ expo <$> evalNegate n <*> evalExpo e
evalExpo (OfNegate n) = evalNegate n

evalMult :: Mult -> EvalResult (LinearExpr Rational)
evalMult (Multiply m n) = join $ multiply <$> evalMult m <*> evalExpo n
evalMult (Divide m n) = join $ divide <$> evalMult m <*> evalExpo n
evalMult (OfExpo n) = evalExpo n

eval :: Expr -> EvalResult (LinearExpr Rational)
eval (Add e m) = add <$> eval e <*> evalMult m
eval (Subtract e m) = sub <$> eval e <*> evalMult m
eval (OfMult m) = evalMult m

solve :: Line -> EvalResult (Solution Rational)
solve (Equate e1 e2) = do
    m1 <- eval e1
    m2 <- eval e2
    (name, coef) <- getVar $ sub m1 m2
    let rhs = getConst $ sub m2 m1
    if coef == 0 then Left NoVariable else pure $ Solved name (rhs / coef)
solve (Expression e) =
    if isRight (m >>= getVar)
        then Left VarInExpression
        else Solved Constant . getConst <$> m
  where
    m = eval e

calculate :: (Fractional a) => String -> Either String (Solution a)
calculate =
    join
        . left errorBundlePretty
        . fmap (bimap show (fmap fromRational) . solve)
        . runParser full "input"
        . T.pack
