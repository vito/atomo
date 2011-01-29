{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel (load) where

import Atomo.Types

import qualified Atomo.Kernel.Nucleus as Nucleus
import qualified Atomo.Kernel.Numeric as Numeric
import qualified Atomo.Kernel.List as List
import qualified Atomo.Kernel.String as String
import qualified Atomo.Kernel.Block as Block
import qualified Atomo.Kernel.Expression as Expression
import qualified Atomo.Kernel.Concurrency as Concurrency
import qualified Atomo.Kernel.Message as Message
import qualified Atomo.Kernel.Method as Method
import qualified Atomo.Kernel.Comparable as Comparable
import qualified Atomo.Kernel.Particle as Particle
import qualified Atomo.Kernel.Pattern as Pattern
import qualified Atomo.Kernel.Ports as Ports
import qualified Atomo.Kernel.Time as Time
import qualified Atomo.Kernel.Environment as Environment
import qualified Atomo.Kernel.Continuation as Continuation
import qualified Atomo.Kernel.Char as Char
import qualified Atomo.Kernel.Regexp as Regexp
import qualified Atomo.Kernel.Pretty as Pretty
import qualified Atomo.Kernel.Format as Format

load :: VM ()
load = do
    Nucleus.load
    Numeric.load
    List.load
    String.load
    Block.load
    Expression.load
    Concurrency.load
    Message.load
    Method.load
    Comparable.load
    Particle.load
    Pattern.load
    Ports.load
    Time.load
    Environment.load
    Continuation.load
    Char.load
    Regexp.load
    Pretty.load
    Format.load
