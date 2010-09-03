module Atomo.Parser.Pattern (pPattern, ppDefine) where

import Text.Parsec.String

import Atomo.Types

pPattern :: Parser Pattern
ppDefine :: Parser Pattern
