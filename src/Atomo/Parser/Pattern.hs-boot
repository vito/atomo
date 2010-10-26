module Atomo.Parser.Pattern (pPattern, ppDefine, ppMacro, ppSet) where

import Atomo.Parser.Base (Parser)
import Atomo.Types

pPattern :: Parser Pattern
ppDefine :: Parser Pattern
ppMacro :: Parser Pattern
ppSet :: Parser Pattern
