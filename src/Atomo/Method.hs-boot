module Atomo.Method (addMethod, insertMethod, toMethods) where

import Atomo.Types

addMethod :: Method -> MethodMap -> MethodMap
insertMethod :: Method -> [Method] -> [Method]
toMethods :: [(Pattern, Value)] -> MethodMap
