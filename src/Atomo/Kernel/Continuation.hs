{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Continuation where

import Data.IORef

import Atomo


load :: VM ()
load = do
    [$p|current-continuation|] =:::
        [$e|{ cc | cc } call/cc|]

    -- call/cc actually makes an object delegating to Continuation
    -- so just add the show definiton here
    [$p|Continuation show|] =:: string "<continuation>"

    [$p|(c: Continuation) yield: v|] =: do
        Continuation c <- here "c" >>= findContinuation
        v <- here "v"
        liftIO (readIORef c) >>= ($ v)

    -- this enables call/cc as well
    [$p|(c: Continuation) call: [v]|] =::: [$e|c yield: v|]

    -- effectively just "jumping" to a continuation
    [$p|(c: Continuation) yield|] =::: [$e|c yield: @ok|]
    [$p|(c: Continuation) call|] =::: [$e|c yield: @ok|]

    -- internal call/cc, quite unsafe (no unwinding)
    [$p|(o: Object) raw-call/cc|] =: callCC $ \c -> do
        o <- here "o"
        cr <- liftIO (newIORef c)
        dispatch (keyword ["call"] [o, list [Continuation cr]])
