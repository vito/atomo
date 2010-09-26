{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Continuation where

import Data.IORef

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    [$p|current-continuation|] =:::
        [$e|{ cc | cc } call/cc|]

    callccObj <- newScope $ do
        ([$p|o|] =::) =<< eval [$e|Object clone|]
        [$p|o call: b|] =: callCC $ \c -> do
            b <- here "b"
            cr <- liftIO (newIORef c)
            as <- list [Continuation cr]
            dispatch (keyword ["call"] [b, as])
        eval [$e|o|]

    [$p|(c: Continuation) yield: v|] =: do
        Continuation c <- here "c" >>= findContinuation
        v <- here "v"
        liftIO (readIORef c) >>= ($ v)

    [$p|(c: Continuation) call: [v]|] =::: [$e|c yield: v|]

    newScope (dynamicWind callccObj >> return (particle "ok"))

    return ()

dynamicWind :: Value -> VM ()
dynamicWind callccObj = do
    [$p|internal-call/cc|] =:: callccObj
    ([$p|dynamic-winds|] =::) =<< eval [$e|Parameter new: []|]

    [$p|(o: Object) call/cc|] =::: [$e|{
        winds = dynamic-winds _? copy

        internal-call/cc call: { cont |
            new = Object clone do: {
                delegates-to: cont

                yield: v := {
                    dynamic-unwind call: [winds, dynamic-winds _? length - winds length]
                    cont yield: v
                } call

                show = "<continuation>"
            }

            o call: [new]
        }
    } call|]

    [$p|dynamic-wind: action before: before after: after|] =::: [$e|{
        before call

        dynamic-winds =! ((before -> after) . dynamic-winds _?)

        { action call } ensuring: {
            dynamic-winds _? pop!
            after call
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
                dynamic-winds _? pop!
                post call
                dynamic-unwind call: [to, d - 1]
            } call
        }
    }|]
