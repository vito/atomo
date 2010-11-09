{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Message (load) where

import Atomo


load :: VM ()
load = do
    [$p|(m: Message) type|] =: do
        Message m <- here "m" >>= findMessage
        case m of
            Single {} -> return (particle "single")
            Keyword {} -> return (particle "keyword")

    [$p|(m: Message) dispatch|] =:
        here "m" >>= findMessage >>= dispatch . fromMessage

    [$p|(m: Message) particle|] =: do
        Message m <- here "m" >>= findMessage
        case m of
            Single { mName = n } -> return (particle n)
            Keyword { mNames = ns } -> return (keyParticle ns (replicate (length ns + 1) Nothing))

    [$p|(m: Message) target|] =: do
        Message (Single { mTarget = t }) <- here "m" >>= findMessage
        return t

    [$p|(m: Message) targets|] =: do
        Message (Keyword { mTargets = ts }) <- here "m" >>= findMessage
        return $ list ts
