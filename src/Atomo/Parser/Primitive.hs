module Atomo.Parser.Primitive where

import Control.Monad (liftM)
import Data.Ratio
import Text.Parsec

import Atomo.Parser.Base
import Atomo.Types as T


-- | Parser for all primitive values.
pPrim :: Parser Value
pPrim = choice
    [ pvChar
    , pvString
    , try pvRational
    , try pvDouble
    , try pvInteger
    , try pvBoolean
    ]

-- | Character literal.
--
-- Examples: @$a@, @$ @, @$\\EOT@, @$\\n@
pvChar :: Parser Value
pvChar = liftM Char charLiteral

-- | String literal.
--
-- Examples: @\"\"@, @\"foo\"@, @\"foo\\nbar\"@
pvString :: Parser Value
pvString = liftM T.string stringLiteral

-- | Double literal.
--
-- Examples: @1.0@, @4.6e10@, @-1.0@
pvDouble :: Parser Value
pvDouble = liftM Double float

-- | Integer literal.
--
-- Examples: @1@, @2@, @-1@, @-2@
pvInteger :: Parser Value
pvInteger = liftM Integer integer

-- | Boolean literal.
--
-- Examples: @True@, @False@
pvBoolean :: Parser Value
pvBoolean = liftM Boolean $ true <|> false
  where
    true = reserved "True" >> return True
    false = reserved "False" >> return False

-- | Rational literal.
--
-- Examples: @1\/2@, @-1\/2@, @1\/-2@
pvRational :: Parser Value
pvRational = do
    n <- integer
    char '/'
    d <- integer
    return (Rational (n % d))
