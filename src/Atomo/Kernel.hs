{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel (load) where

import Data.IORef
import Data.Maybe (isJust)

import Atomo
import Atomo.Load
import Atomo.Method
import Atomo.Pattern (bindings')
import Atomo.Pretty

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

    [$p|(x: Object) delegates-to: (y: Object)|] =: do
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

    [$p|v do: (b: Block)|] =: do
        v <- here "v"
        b <- here "b" >>= findBlock
        joinWith v b []
        return v
    [$p|v do: (b: Block) with: (l: List)|] =: do
        v <- here "v"
        b <- here "b" >>= findBlock
        as <- getList [$e|l|]
        joinWith v b as
        return v

    [$p|v join: (b: Block)|] =: do
        v <- here "v"
        b <- here "b" >>= findBlock
        joinWith v b []
    [$p|v join: (b: Block) with: (l: List)|] =: do
        v <- here "v"
        b <- here "b" >>= findBlock
        as <- getList [$e|l|]
        joinWith v b as


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


joinWith :: Value -> Value -> [Value] -> VM Value
joinWith t (Block s ps bes) as
    | length ps > length as =
        throwError (BlockArity (length ps) (length as))
    | null as || null ps =
        case t of
            Object { oDelegates = ds, oMethods = rms } -> do
                ms <- liftIO (readIORef rms)
                blockScope <- newObject (ds ++ [s]) ms

                res <- withTop blockScope (evalAll bes)
                new <- objectFor blockScope

                -- replace the old object with the new one
                liftIO (readIORef (oMethods new)) >>= liftIO . writeIORef rms

                finalize (oMethods new)

                return res
            _ -> do
                blockScope <- newObject [t, s] noMethods
                withTop blockScope (evalAll bes)
    | otherwise = do
        -- a toplevel scope with transient definitions
        pseudoScope <- newObject [] (bs, emptyMap)

        case t of
            Object { oDelegates = ds, oMethods = rms } -> do
                ms <- liftIO (readIORef rms)

                -- the original prototype, but without its delegations
                -- this is to prevent dispatch loops
                doppelganger <- newObject [] ms

                -- the main scope, methods are taken from here and merged with
                -- the originals. delegates to the pseudoscope and doppelganger
                -- so it has their methods in scope, but definitions go here
                blockScope <- newObject (pseudoScope : doppelganger : ds ++ [s]) noMethods

                res <- withTop blockScope (evalAll bes)
                new <- objectFor blockScope

                nms <- liftIO (readIORef (oMethods new))
                liftIO (writeIORef rms (merge ms nms))

                finalize (oMethods new)

                return res
            _ -> do
                blockScope <- newObject [pseudoScope, t, s] noMethods

                withTop blockScope (evalAll bes)
  where
    bs = toMethods . concat $ zipWith bindings' ps as

    merge (os, ok) (ns, nk) =
        ( foldl (flip addMethod) os (concat $ elemsMap ns)
        , foldl (flip addMethod) ok (concat $ elemsMap nk)
        )

    -- clear the toplevel object's methods and have it delegate to the updated
    -- object
    --
    -- this is important because method definitions have this as their context,
    -- so methods changed in the new object wouldn't otherwise be available to
    -- them
    --
    -- TODO: this doesn't update delegates anymore
    finalize :: Methods -> VM ()
    finalize rms = liftIO (writeIORef rms noMethods)
        {-{ oDelegates = t : oDelegates c-}
        {-, oMethods = noMethods-}
        {-}-}

joinWith _ v _ = error $ "impossible: joinWith on " ++ show v
