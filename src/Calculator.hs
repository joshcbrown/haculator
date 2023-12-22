module Calculator where

import Control.Arrow (ArrowChoice (left))
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
type Solution = M.Map Term Rational

evalAtom :: Atom -> EvalResult
evalAtom (Number x) = Right $ M.singleton Constant x
evalAtom (Parens e) = eval e
evalAtom (Ident s) = Right $ M.singleton (Var s) 1

evalNegate :: Negate -> EvalResult
evalNegate (Neg n) = fmap negate <$> evalNegate n
evalNegate (OfAtom a) = evalAtom a

containsVar :: Added -> Bool
containsVar m = any isVar (M.keys m)
  where
    isVar (Var _) = True
    isVar _ = False

-- horrendously ugly but i think it works
multiply :: Added -> Added -> EvalResult
multiply m1 m2 = M.foldrWithKey (\k v -> (multiplyEach k v =<<)) (Right M.empty) m1
  where
    multiplyEach k v m = M.unionWith (+) m <$> new k v
    new k v = case k of
        Constant -> Right $ (* v) <$> m2
        Var s -> if containsVar m2 then Left Denormal else Right $ M.singleton (Var s) (v * constVal)
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

-- solve :: Added -> Added -> Either ArithException Solution
-- solve e1 e2 = M.foldrWithKey

calculate :: (Fractional a) => String -> Either String (M.Map Term a)
calculate =
    join
        . left errorBundlePretty
        . fmap
            ( bimap show (fmap fromRational)
                . (\(Equate e1 e2) -> M.unionWith (+) <$> eval e1 <*> eval e2)
            )
        . runParser full "input"
        . T.pack
