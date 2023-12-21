module Calculator where

import Calculator.Parser
import Control.Arrow (ArrowChoice (left, right))
import Control.Exception (ArithException (DivideByZero), catch, try)
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)

eval :: Expr -> Either ArithException Rational
eval (Number x) = Right x
eval (Negate x) = negate <$> eval x
eval (BinOp op x y) = case op of
    Add -> (+) <$> eval x <*> eval y
    Subtract -> (-) <$> eval x <*> eval y
    Multiply -> (*) <$> eval x <*> eval y
    Divide -> case eval y of
        e@(Left _) -> e
        (Right 0) -> Left DivideByZero
        (Right ey) -> (/ ey) <$> eval x

calculate :: (Fractional a) => String -> Either String a
calculate =
    join
        . left errorBundlePretty
        . fmap (bimap show fromRational . eval)
        . runParser full "input"
        . T.pack
