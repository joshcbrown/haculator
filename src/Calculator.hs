{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Calculator where

import Control.Arrow (ArrowChoice (left))
import Control.Exception (ArithException (..))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Either (isRight)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Parser.AST
import Text.Megaparsec (errorBundlePretty, runParser)

data Term = Constant | Var String
    deriving (Show, Eq, Ord)

data EvalErr = Arith ArithException | NonLinear | NoVariable | TooManyVars [Term]
    deriving (Show)

data Solution a = Solved Term a
    deriving (Functor, Eq)

instance (Show a) => Show (Solution a) where
    show (Solved (Var s) x) = s ++ show (Solved Constant x)
    show (Solved Constant x) = " = " ++ show x

type EvalResult a = Either EvalErr a
type Added = M.Map Term Rational

evalAtom :: Atom -> EvalResult Added
evalAtom (Number x) = Right $ M.singleton Constant x
evalAtom (Parens e) = eval e
evalAtom (Ident s) = Right $ M.singleton (Var s) 1

evalNegate :: Negate -> EvalResult Added
evalNegate (Neg n) = fmap negate <$> evalNegate n
evalNegate (OfAtom a) = evalAtom a

evalExpo :: Expo -> EvalResult Added
evalExpo (Expo e i) = do
    e' <- evalNegate e
    i' <- evalExpo i
    if any (isRight . getVar) [e', i']
        then Left NonLinear
        else Right $ M.singleton Constant (getConst e' `rationalPow` getConst i')
evalExpo (OfNegate n) = evalNegate n

-- possible that this slows things down a lot.
rationalPow :: Rational -> Rational -> Rational
rationalPow x y = toRational $ fromRational x ** fromRational y

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

containsVar :: Added -> Bool
containsVar m = any isVar (M.keys m)

-- horrendously ugly but i think it works
multiply :: Added -> Added -> EvalResult Added
multiply m1 m2 = M.foldrWithKey (\k v -> (foilIter k v =<<)) (Right M.empty) m1
  where
    foilIter k v m = M.unionWith (+) m <$> multiplyEach k v
    multiplyEach k v = case k of
        Constant -> Right ((* v) <$> m2)
        Var s ->
            if containsVar m2
                then Left NonLinear
                else Right $ M.singleton (Var s) (v * constVal)
    constVal = fromJust (M.lookup Constant m2)

evalMult :: Mult -> EvalResult Added
evalMult (Multiply m n) = join $ multiply <$> evalMult m <*> evalExpo n
evalMult (Divide m n) =
    case evalExpo n of
        e@(Left _) -> e
        (Right x) -> case M.lookup Constant x of
            Just 0 -> Left $ Arith RatioZeroDenominator
            Just y -> fmap (/ y) <$> evalMult m
            Nothing -> Left NonLinear
evalMult (OfExpo n) = evalExpo n

eval :: Expr -> EvalResult Added
eval (Add e m) = M.unionWith (+) <$> eval e <*> evalMult m
eval (Subtract e m) = unionWithDefault (-) 0 <$> eval e <*> evalMult m
eval (OfMult m) = evalMult m

getConst :: Added -> Rational
getConst m = fromMaybe 0 (M.lookup Constant m)

getVar :: Added -> EvalResult (Term, Rational)
getVar m =
    case length ts of
        0 -> Left NoVariable
        1 -> Right $ head ts
        _ -> Left (TooManyVars (map fst ts))
  where
    ts = filter (isVar . fst) (M.toList m)

solve :: Line -> EvalResult (Solution Rational)
solve (Equate e1 e2) = do
    m1 <- eval e1
    m2 <- eval e2
    (name, coef) <- getVar $ unionWithDefault (-) 0 m1 m2
    let rhs = getConst $ unionWithDefault (-) 0 m2 m1
    if coef == 0 then Left NoVariable else pure $ Solved name (rhs / coef)
solve (Expression e) = Solved Constant . getConst <$> eval e

unionWithDefault :: (Ord k) => (v -> v -> v) -> v -> M.Map k v -> M.Map k v -> M.Map k v
unionWithDefault f defaultv m1 m2 = M.unionWith f m1' m2'
  where
    fillIn m3 = M.union m3 . M.fromAscList . map (,defaultv) . M.keys
    m1' = fillIn m1 m2
    m2' = fillIn m2 m1

calculate :: (Fractional a) => String -> Either String (Solution a)
calculate =
    join
        . left errorBundlePretty
        . fmap (bimap show (fmap fromRational) . solve)
        . runParser full "input"
        . T.pack
