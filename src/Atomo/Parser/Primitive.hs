module Atomo.Parser.Primitive where

import Data.Ratio
import Text.Parsec

import Atomo.Parser.Base
import Atomo.Types as T


pPrimitive :: Parser Expr
pPrimitive = tagged $ fmap (Primitive Nothing) pPrim

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
pvChar = charLiteral >>= return . Char

pvString :: Parser Value
pvString = stringLiteral >>= return . T.string

pvDouble :: Parser Value
pvDouble = float >>= return . Double

pvInteger :: Parser Value
pvInteger = integer >>= return . Integer

pvBoolean :: Parser Value
pvBoolean = fmap Boolean $ true <|> false
  where
    true = reserved "True" >> return True
    false = reserved "False" >> return False

pvRational :: Parser Value
pvRational = do
    n <- integer
    char '/'
    d <- integer
    return (Rational (n % d))
