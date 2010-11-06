module Atomo.PrettyVM where

import "monads-fd" Control.Monad.State
import qualified Text.PrettyPrint as P

import Atomo.Environment
import Atomo.Types


-- | pretty-print by sending \@show to the object
prettyVM :: Value -> VM P.Doc
prettyVM = liftM (P.text . fromText . fromString) . dispatch . (single "show")

