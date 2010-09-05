module Atomo.Parser.Primitive where

import Text.Parsec

import Atomo.Parser.Base
import Atomo.Types as T


pPrimitive :: Parser Expr
pPrimitive = tagged $ fmap (Primitive Nothing) pPrim

pPrim :: Parser Value
pPrim = choice
    [ pvChar
    , try pvDouble
    , try pvInteger
    ]

pvChar :: Parser Value
pvChar = charLiteral >>= return . Char

pvDouble :: Parser Value
pvDouble = float >>= return . Double

pvInteger :: Parser Value
pvInteger = integer >>= return . Integer
