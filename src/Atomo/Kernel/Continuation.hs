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

    [$p|(o: Object) call/cc|] =::: [$e|o call/cc: []|]
    [$p|(o: Object) call/cc: (as: List)|] =: callCC $ \c -> do
        o <- here "o"
        as <- getList [$e|as|]
        cr <- mkContinuation c
        dispatch (keyword ["call"] [o, list (Continuation cr:as)])

    [$p|(v: Block) before: (b: Block) after: (a: Block)|] =: do
        v <- here "v"
        b <- here "b"
        a <- here "a"

        dispatch (single "call" b)

        modify $ \env -> env { unwinds = (b, a) : unwinds env }

        res <- dispatch (single "call" v)

        dispatch (single "call" a)

        modify $ \env -> env { unwinds = tail (unwinds env) }

        return res

mkContinuation :: (Value -> VM Value) -> VM Continuation
mkContinuation c = do
    env <- get
    liftIO . newIORef $ \v -> do
        ws <- gets unwinds
        unwind (unwinds env) (length ws - length (unwinds env))

        modify $ \env' -> env'
            { top = top env
            , dynamic = dynamic env
            }

        put env
        c v

unwind :: [(Value, Value)] -> Int -> VM Value
unwind to d = do
    ws <- gets unwinds
    if ws == to
        then return (particle "ok")
        else do

    if d < 0
        then do
            unwind us (d + 1)
            dispatch (single "call" pre)
            modify $ \env -> env { unwinds = to }
            return (particle "ok")
        else do

    let post = snd (head ws)
    modify $ \env -> env { unwinds = tail (unwinds env) }
    dispatch (single "call" post)
    unwind to (d - 1)
  where
    (u:us) = to
    pre = fst u
