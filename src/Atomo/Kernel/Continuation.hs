{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Continuation where

import Data.IORef

import Atomo.Environment
import Atomo.Haskell


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

    -- an object providing lower-level call/cc functionality
    -- only used in call/cc's definition
    callccObj <- newScope $ do
        ([$p|o|] =::) =<< eval [$e|Object clone|]
        [$p|(o) pass-to: b|] =: callCC $ \c -> do
            b <- here "b"
            cr <- liftIO (newIORef c)
            as <- list [Continuation cr]
            dispatch (keyword ["call"] [b, as])
        eval [$e|o|]

    -- define call/cc and dynamic-wind in a new scope for hiding
    -- helper methods (internal-call/cc, dynamic-winds, dynamic-unwind)
    newScope (dynamicWind callccObj >> return (particle "ok"))

    return ()

dynamicWind :: Value -> VM ()
dynamicWind callccObj = do
    [$p|internal-call/cc|] =:: callccObj
    ([$p|dynamic-winds|] =::) =<< eval [$e|Parameter new: []|]

    [$p|(o: Object) call/cc|] =::: [$e|internal-call/cc pass-to: { cont |
        winds = dynamic-winds _? copy

        new = cont clone do: {
            yield: v := {
                dynamic-unwind call: [
                    winds
                    dynamic-winds _? length - winds length
                ]

                cont yield: v
            } call
        }

        o call: [new]
    }|]

    [$p|(v: Block) before: (b: Block)|] =::: [$e|v before: b after: { @ok }|]
    [$p|(v: Block) after: (a: Block)|] =::: [$e|v before: { @ok } after: a|]
    [$p|(v: Block) before: (b: Block) after: (a: Block)|] =::: [$e|{
        b call

        dynamic-winds =! ((b -> a) . dynamic-winds _?)

        { v call } ensuring: {
            dynamic-winds =! dynamic-winds _? tail
            a call
        }
    } call|]

    ([$p|dynamic-unwind|] =::) =<< eval [$e|{ to d |
        condition: {
            (dynamic-winds _? == to) -> @ok

            (d < 0) -> {
                dynamic-unwind call: [to tail, d + 1]
                to head from call
                dynamic-winds =! to
                @ok
            } call

            otherwise -> {
                post = dynamic-winds _? head to
                dynamic-winds =! dynamic-winds _? tail
                post call
                dynamic-unwind call: [to, d - 1]
            } call
        }
    }|]
