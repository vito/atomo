module Atomo.Parser.Pattern (pPattern, ppDefine, ppSet) where

import Text.Parsec.String

import Atomo.Types

pPattern :: Parser Pattern
ppDefine :: Parser Pattern
ppSet :: Parser Pattern
