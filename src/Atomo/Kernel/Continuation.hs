{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Continuation where

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    [$p|current-continuation|] =: callCC $ \c ->
        return (Continuation c)

    [$p|(b: Object) call/cc|] =: callCC $ \c -> do
        b <- here "b"
        as <- list [Continuation c]
        dispatch (keyword ["call"] [b, as])

    [$p|(c: Continuation) yield: v|] =: do
        Continuation c <- here "c" >>= findContinuation
        v <- here "v"
        c v

    [$p|(c: Continuation) call: [v]|] =::: [$e|c yield: v|]
