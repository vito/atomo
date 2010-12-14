{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Nucleus where

import Data.IORef
import Data.Maybe (isJust)

import Atomo
import Atomo.Load
import Atomo.Method
import Atomo.Pretty

load :: VM ()
load = do
    [$p|(x: Object) clone|] =: do
        x <- here "x"
        newObject [x] noMethods

    [$p|(x: Object) copy|] =: do
        x <- here "x" >>= objectFor
        ms <- liftIO (readIORef (oMethods x))
        liftM (Object (oDelegates x)) (liftIO (newIORef ms))

    [$p|(x: Object) copy: (diff: Block)|] =:::
        [$e|x copy do: diff|]

    [$p|(x: Object) delegating-to: (y: Object)|] =: do
        f <- here "x" >>= objectFor
        t <- here "y"

        return f
            { oDelegates = oDelegates f ++ [t]
            }

    [$p|(x: Object) delegates-to?: (y: Object)|] =: do
        x <- here "x"
        y <- here "y"
        return (Boolean (delegatesTo x y))

    [$p|(x: Object) delegates|] =: do
        o <- here "x" >>= objectFor
        return $ list (oDelegates o)

    [$p|(x: Object) with-delegates: (ds: List)|] =: do
        ds <- getList [$e|ds|]
        x <- here "x" >>= objectFor
        return x { oDelegates = ds }

    [$p|(x: Object) super|] =::: [$e|x delegates head|]

    [$p|(x: Object) is-a?: (y: Object)|] =: do
        x <- here "x"
        y <- here "y"
        liftM Boolean (isA x y)

    [$p|(x: Object) responds-to?: (p: Particle)|] =: do
        x <- here "x"
        Particle p' <- here "p" >>= findParticle

        let completed =
                case p' of
                    PMKeyword ns mvs -> keyword ns (completeKP mvs [x])
                    PMSingle n -> single n x

        liftM (Boolean . isJust) $ findMethod x completed

    [$p|(o: Object) methods|] =: do
        o <- here "o" >>= objectFor
        (ss, ks) <- liftIO (readIORef (oMethods o))

        [$e|Object|] `newWith`
            [ ("singles", list (map (list . map Method) (elemsMap ss)))
            , ("keywords", list (map (list . map Method) (elemsMap ks)))
            ]

    [$p|(x: Object) dump|] =: do
        o <- here "x"
        liftIO (print o)
        return o

    [$p|(x: Object) describe-error|] =::: [$e|x as: String|]

    [$p|(s: String) as: String|] =::: [$e|s|]

    [$p|(x: Object) as: String|] =::: [$e|x show|]

    [$p|(x: Object) show|] =:
        liftM (string . show . pretty) (here "x")

    [$p|(t: Object) load: (fn: String)|] =: do
        t <- here "t"
        fn <- getString [$e|fn|]

        withTop t (loadFile fn)

        return (particle "ok")

    [$p|(t: Object) require: (fn: String)|] =: do
        t <- here "t"
        fn <- getString [$e|fn|]

        withTop t (requireFile fn)

        return (particle "ok")

