module Calculator where

import Calculator.Parser
import Control.Arrow (ArrowChoice (left))
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)

eval :: Expr -> Rational
eval (Constant x) = x
eval (Negate x) = negate (eval x)
eval (BinOp op x y) = case op of
    Add -> eval x + eval y
    Subtract -> eval x - eval y
    Multiply -> eval x * eval y
    Divide -> eval x / eval y

calculate :: (Fractional a) => String -> Either String a
calculate = left errorBundlePretty . fmap (fromRational . eval) . runParser full "input" . T.pack
