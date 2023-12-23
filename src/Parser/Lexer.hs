module Parser.Lexer where

import Control.Monad (void)
import qualified Data.Text as T
import Parser.Common
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

whitespace :: Parser ()
whitespace =
    L.space
        space1
        (L.skipLineComment "--")
        (L.skipBlockComment "{-" "-}")

-- | makes a parser atomic and consume trailing whitespace
token :: Parser a -> Parser a
token = L.lexeme whitespace . try

symbol :: T.Text -> Parser ()
symbol = void . L.symbol whitespace
