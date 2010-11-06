module Atomo.PrettyVM where

import Control.Monad (liftM)
import Atomo.Environment
import Atomo.Types


-- | pretty-print by sending \@show to the object
prettyVM :: Value -> VM String
prettyVM = liftM (fromText . fromString) . dispatch . single "show"

