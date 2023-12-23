module Calculator where

import Control.Arrow (Arrow (second), ArrowChoice (left))
import Control.Exception (ArithException (..))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Parser.AST
import Text.Megaparsec (errorBundlePretty, runParser)

data Term = Constant | Var String
    deriving (Show, Eq, Ord)

type Added = M.Map Term Rational

type EvalResult = Either ArithException Added
type Solution a = (Term, a)

evalAtom :: Atom -> EvalResult
evalAtom (Number x) = Right $ M.singleton Constant x
evalAtom (Parens e) = eval e
evalAtom (Ident s) = Right $ M.singleton (Var s) 1

evalNegate :: Negate -> EvalResult
evalNegate (Neg n) = fmap negate <$> evalNegate n
evalNegate (OfAtom a) = evalAtom a

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

containsVar :: Added -> Bool
containsVar m = any isVar (M.keys m)

-- horrendously ugly but i think it works
multiply :: Added -> Added -> EvalResult
multiply m1 m2 = M.foldrWithKey (\k v -> (multiplyEach k v =<<)) (Right M.empty) m1
  where
    multiplyEach k v m = M.unionWith (+) m <$> new k v
    new k v = case k of
        Constant -> Right $ (* v) <$> m2
        Var s -> if containsVar m2 then Left Denormal else Right $ M.singleton (Var s) (v * constVal)
    -- TODO: should really error here if there's no constant
    constVal = fromMaybe 1 (M.lookup Constant m2)

evalMult :: Mult -> EvalResult
evalMult (Multiply m n) = join $ multiply <$> evalMult m <*> evalNegate n
evalMult (Divide m n) =
    case evalNegate n of
        e@(Left _) -> e
        (Right x) -> case M.lookup Constant x of
            Just 0 -> Left RatioZeroDenominator
            Just y -> fmap (/ y) <$> evalMult m
            Nothing -> Left Denormal
evalMult (OfNegate n) = evalNegate n

eval :: Expr -> EvalResult
eval (Add e m) = M.unionWith (+) <$> eval e <*> evalMult m
eval (Subtract e m) = M.unionWith (-) <$> eval e <*> evalMult m
eval (OfMult m) = evalMult m

getConst :: Added -> Rational
getConst m = fromMaybe 0 (M.lookup Constant m)

getVar :: Added -> Maybe (Term, Rational)
getVar m = if length ts /= 1 then Nothing else Just $ head ts
  where
    ts = filter (isVar . fst) (M.toList m)

solve :: Equation -> Either ArithException (Solution Rational)
solve (Equate e1 e2) = do
    m1 <- eval e1
    m2 <- eval e2
    let lhs = getVar $ M.unionWith (-) m1 m2
        rhs = getConst $ M.unionWith (-) m2 m1
    case lhs of
        Nothing -> Left Denormal
        Just (name, coef) -> pure (name, rhs / coef)

calculate :: (Fractional a) => String -> Either String (Solution a)
calculate =
    join
        . left errorBundlePretty
        . fmap (bimap show (fmap fromRational) . solve)
        . runParser full "input"
        . T.pack
