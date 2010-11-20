module Atomo.PrettyVM where

import Control.Monad (liftM)
import Atomo.Environment
import Atomo.Types


-- | Pretty-print a value by sending @show@ to it.
prettyVM :: Value -> VM String
prettyVM = liftM (fromText . fromString) . dispatch . single "show"

