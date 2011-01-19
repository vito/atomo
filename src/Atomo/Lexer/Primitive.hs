module Atomo.Lexer.Primitive where

import Control.Monad (liftM)
import Data.Ratio
import Text.Parsec

import Atomo.Lexer.Base
import Atomo.Types as T


-- | Character literal.
--
-- Examples: @$a@, @$ @, @$\\EOT@, @$\\n@
lvChar :: Lexer Value
lvChar = liftM Char charLiteral

-- | String literal.
--
-- Examples: @\"\"@, @\"foo\"@, @\"foo\\nbar\"@
lvString :: Lexer Value
lvString = liftM T.string stringLiteral

-- | Double literal.
--
-- Examples: @1.0@, @4.6e10@, @-1.0@
lvDouble :: Lexer Value
lvDouble = liftM Double float

-- | Integer literal.
--
-- Examples: @1@, @2@, @-1@, @-2@
lvInteger :: Lexer Value
lvInteger = liftM Integer integer

-- | Boolean literal.
--
-- Examples: @True@, @False@
lvBoolean :: Lexer Value
lvBoolean = liftM Boolean $ true <|> false
  where
    true = reserved "True" >> return True
    false = reserved "False" >> return False

-- | Rational literal.
--
-- Examples: @1\/2@, @-1\/2@, @1\/-2@
lvRational :: Lexer Value
lvRational = do
    n <- integer
    char '/'
    d <- integer
    return (Rational (n % d))
