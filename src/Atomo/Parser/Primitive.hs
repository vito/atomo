module Atomo.Parser.Primitive where

import Text.Parsec

import Atomo.Parser.Base
import Atomo.Types as T


pPrimitive :: Parser Expr
pPrimitive = tagged $ fmap (Primitive Nothing) pPrim

pPrim :: Parser Value
pPrim = choice
    [ pvChar
    , pvString
    , try pvDouble
    , try pvInteger
    , pvBoolean
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
    true = try (reserved "True") >> return True
    false = try (reserved "False") >> return False
