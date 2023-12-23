{-# LANGUAGE TupleSections #-}

module Calculator where

import Control.Arrow (ArrowChoice (left))
import Control.Exception (ArithException (..))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Parser.AST
import Text.Megaparsec (errorBundlePretty, runParser)

data Term = Constant | Var String
    deriving (Show, Eq, Ord)

type Added = M.Map Term Rational
data EvalErr = Arith ArithException | NonLinear | NoVariable | TooManyVars [Term]
    deriving (Show)

type EvalResult a = Either EvalErr a
type Solution a = (Term, a)

evalAtom :: Atom -> EvalResult Added
evalAtom (Number x) = Right $ M.singleton Constant x
evalAtom (Parens e) = eval e
evalAtom (Ident s) = Right $ M.singleton (Var s) 1

evalNegate :: Negate -> EvalResult Added
evalNegate (Neg n) = fmap negate <$> evalNegate n
evalNegate (OfAtom a) = evalAtom a

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
evalMult (Multiply m n) = join $ multiply <$> evalMult m <*> evalNegate n
evalMult (Divide m n) =
    case evalNegate n of
        e@(Left _) -> e
        (Right x) -> case M.lookup Constant x of
            Just 0 -> Left $ Arith RatioZeroDenominator
            Just y -> fmap (/ y) <$> evalMult m
            Nothing -> Left NonLinear
evalMult (OfNegate n) = evalNegate n

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

solve :: Equation -> EvalResult (Solution Rational)
solve (Equate e1 e2) = do
    m1 <- eval e1
    m2 <- eval e2
    (name, coef) <- getVar $ unionWithDefault (-) 0 m1 m2
    let rhs = getConst $ unionWithDefault (-) 0 m2 m1
    if coef == 0 then Left NoVariable else pure (name, rhs / coef)

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
