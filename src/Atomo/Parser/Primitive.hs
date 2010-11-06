module Atomo.Parser.Primitive where

import Control.Monad (liftM)
import Data.Ratio
import Text.Parsec

import Atomo.Parser.Base
import Atomo.Types as T


pPrimitive :: Parser Expr
pPrimitive = tagged $ liftM (Primitive Nothing) pPrim

pPrim :: Parser Value
pPrim = choice
    [ pvChar
    , pvString
    , try pvRational
    , try pvDouble
    , try pvInteger
    , try pvBoolean
    ]

pvChar :: Parser Value
pvChar = liftM Char charLiteral

pvString :: Parser Value
pvString = liftM T.string stringLiteral

pvDouble :: Parser Value
pvDouble = liftM Double float

pvInteger :: Parser Value
pvInteger = liftM Integer integer

pvBoolean :: Parser Value
pvBoolean = liftM Boolean $ true <|> false
  where
    true = reserved "True" >> return True
    false = reserved "False" >> return False

pvRational :: Parser Value
pvRational = do
    n <- integer
    char '/'
    d <- integer
    return (Rational (n % d))
