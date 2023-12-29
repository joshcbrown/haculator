{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Calculator.LinearExpression (
    Term (..),
    EvalErr (..),
    LinearExpr,
    EvalResult,
    single,
    add,
    sub,
    multiply,
    divide,
    expo,
    getVar,
    getConst,
)
where

import Control.Exception (ArithException (..))
import Data.Either (isRight)
import qualified Data.Map as M
import Data.Maybe (fromJust)

data Term = Constant | Var String
    deriving (Show, Eq, Ord)

data EvalErr = Arith ArithException | NonLinear | NoVariable | TooManyVars [Term] | VarInExpression
    deriving (Show)

{- | values represent the coefficents of the keys in a given expression
implementing this as a map because without a scan ahead of time,
nothing is known about the number of terms in a given expression/equation
-}
newtype LinearExpr a = LinearExpr (M.Map Term a)
    deriving (Semigroup, Monoid, Functor, Show)

type EvalResult a = Either EvalErr a

single :: (Num a) => Term -> a -> LinearExpr a
single Constant x = LinearExpr $ M.singleton Constant x
single v@(Var _) x = single Constant 0 <> LinearExpr (M.singleton v x)

unionWithDefault :: (Ord k) => (v -> v -> v) -> v -> M.Map k v -> M.Map k v -> M.Map k v
unionWithDefault f defaultv m1 m2 = M.unionWith f m1' m2'
  where
    fillIn m3 = M.union m3 . M.fromAscList . map (,defaultv) . M.keys
    m1' = fillIn m1 m2
    m2' = fillIn m2 m1

add :: (Num a) => LinearExpr a -> LinearExpr a -> LinearExpr a
add (LinearExpr e1) (LinearExpr e2) =
    LinearExpr $ unionWithDefault (+) 0 e1 e2

sub :: (Num a) => LinearExpr a -> LinearExpr a -> LinearExpr a
sub (LinearExpr e1) (LinearExpr e2) =
    LinearExpr $ unionWithDefault (-) 0 e1 e2

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

containsVar :: M.Map Term a -> Bool
containsVar m = any isVar (M.keys m)

multiply :: (Num a) => LinearExpr a -> LinearExpr a -> EvalResult (LinearExpr a)
multiply (LinearExpr m1) (LinearExpr m2) =
    LinearExpr <$> M.foldrWithKey (\k v -> (foilIter k v =<<)) (Right M.empty) m1
  where
    foilIter k v m = M.unionWith (+) m <$> multiplyEach k v
    multiplyEach k v = case k of
        Constant -> Right ((* v) <$> m2)
        Var s ->
            if containsVar m2
                then Left NonLinear
                else Right $ M.singleton (Var s) (v * constVal)
    constVal = fromJust (M.lookup Constant m2)

divide :: (Fractional a, Eq a) => LinearExpr a -> LinearExpr a -> EvalResult (LinearExpr a)
divide (LinearExpr m1) (LinearExpr m2) =
    case M.lookup Constant m2 of
        Just 0 -> Left $ Arith RatioZeroDenominator
        Just y -> Right . LinearExpr $ (/ y) <$> m1
        Nothing -> Left NonLinear

getVar :: LinearExpr a -> EvalResult (Term, a)
getVar (LinearExpr m) =
    case length ts of
        0 -> Left NoVariable
        1 -> Right $ head ts
        _ -> Left (TooManyVars (map fst ts))
  where
    ts = filter (isVar . fst) (M.toList m)

getConst :: LinearExpr a -> a
getConst (LinearExpr m) = fromJust (M.lookup Constant m)

rationalPow :: Rational -> Rational -> Rational
rationalPow x y = toRational $ fromRational x ** fromRational y

-- unfortunately not sure how to make this polymorphic
expo :: LinearExpr Rational -> LinearExpr Rational -> EvalResult (LinearExpr Rational)
expo e1 e2 =
    if any (isRight . getVar) [e1, e2]
        then Left NonLinear
        else Right $ single Constant (getConst e1 `rationalPow` getConst e2)
