module Parser.Common where

import Data.Text as T
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void T.Text
