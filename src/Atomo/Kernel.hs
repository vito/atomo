{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel (load) where

import Data.IORef
import Data.List ((\\))
import Data.Maybe (isJust)
import qualified Data.IntMap as M
import qualified Data.Vector as V

import Atomo.Debug
import Atomo.Environment
import Atomo.Haskell
import Atomo.Method
import Atomo.Pretty

import qualified Atomo.Kernel.Numeric as Numeric
import qualified Atomo.Kernel.List as List
import qualified Atomo.Kernel.Block as Block
import qualified Atomo.Kernel.Expression as Expression
import qualified Atomo.Kernel.Concurrency as Concurrency
import qualified Atomo.Kernel.Message as Message
import qualified Atomo.Kernel.Comparable as Comparable
import qualified Atomo.Kernel.Particle as Particle
import qualified Atomo.Kernel.Pattern as Pattern
import qualified Atomo.Kernel.Ports as Ports
import qualified Atomo.Kernel.Time as Time
import qualified Atomo.Kernel.Bool as Bool
import qualified Atomo.Kernel.Association as Association
import qualified Atomo.Kernel.Parameter as Parameter
import qualified Atomo.Kernel.Exception as Exception

load :: VM ()
load = do
    eval [$e|operator right ->|]

    [$p|this|] =::: [$e|dispatch sender|]

    [$p|(x: Object) clone|] =: do
        x <- here "x"
        newObject $ \o -> o
            { oDelegates = [x]
            }

    [$p|(x: Object) delegates-to: (y: Object)|] =: do
        f <- here "x" >>= orefFor
        t <- here "y"

        from <- liftIO (readIORef f)
        liftIO $ writeIORef f (from { oDelegates = oDelegates from ++ [t] })
        return (particle "ok")

    [$p|(x: Object) delegates-to?: (y: Object)|] =: do
        x <- here "x"
        y <- here "y"
        delegatesTo x y >>= bool

    [$p|(x: Object) is-a?: (y: Object)|] =: do
        x <- here "x"
        y <- here "y"
        isA x y >>= bool

    [$p|(x: Object) responds-to?: (p: Particle)|] =: do
        x <- here "x"
        Particle p' <- here "p" >>= findValue isParticle

        let completed =
                case p' of
                    PMKeyword ns mvs -> keyword ns (completeKP mvs [x])
                    PMSingle n -> single n x

        findMethod x completed
            >>= bool . isJust

    [$p|(s: String) as: String|] =: do
        s <- here "s"
        cs <- fmap V.toList (getList [$e|s|])
        if all isChar cs
            then return s
            else string . show . pretty $ s

    [$p|(x: Object) as: String|] =::: [$e|x show|]

    [$p|(x: Object) show|] =: do
        v <- here "x"

        if isReference v
            then string (show (pretty v))
            else prettyVM v >>= string . show

    [$p|(x: Object) dump|] =: do
        x <- here "x"
        liftIO (putStrLn (prettyShow x))
        return x

    [$p|load: (fn: String)|] =: do
        fn <- fmap V.toList $ getList [$e|fn|]

        -- escape from the method scope back to the sender
        sender <- eval [$e|dispatch sender|]
        lift . modify $ \s -> s { top = sender }

        if all isChar fn
            then loadFile (map (\(Char c) -> c) fn)
            else throwError $ ErrorMsg "@load: applied to non-String"

        return (particle "ok")

    [$p|v do: (b: Block)|] =: do
        v <- here "v"
        b <- here "b" >>= findValue isBlock
        joinWith v b []
        return v
    [$p|v do: (b: Block) with: (l: List)|] =: do
        v <- here "v"
        b <- here "b" >>= findValue isBlock
        as <- fmap V.toList $ getList [$e|l|]
        joinWith v b as
        return v

    [$p|v join: (b: Block)|] =: do
        v <- here "v"
        b <- here "b" >>= findValue isBlock
        joinWith v b []
    [$p|v join: (b: Block) with: (l: List)|] =: do
        v <- here "v"
        b <- here "b" >>= findValue isBlock
        as <- fmap V.toList $ getList [$e|l|]
        joinWith v b as


    Association.load
    Parameter.load
    Numeric.load
    List.load
    Block.load
    Expression.load
    Concurrency.load
    Message.load
    Comparable.load
    Particle.load
    Pattern.load
    Ports.load
    Time.load
    Bool.load
    Exception.load

    prelude


prelude :: VM ()
prelude = mapM_ eval [$es|
    v match: (b: Block) :=
        if: b contents empty?
            then: { @no-match }
            else: {
                es = b contents
                [p, e] = es head targets

                match = (p as: Pattern) matches?: v
                if: (match == @no)
                    then: {
                        v match: (Block new: es tail in: b context)
                    }
                    else: {
                        @(yes: obj) = match
                        obj join: (Block new: [e] in: b context)
                    }
            }
|]

joinWith :: Value -> Value -> [Value] -> VM Value
joinWith t (Block s ps bes) as
    | length ps > length as = throwError . ErrorMsg . unwords $
        [ "block expects"
        , show (length ps)
        , "arguments, given"
        , show (length as)
        ]
    | null as || null ps = do
        case t of
            Reference r -> do
                Object ds ms <- objectFor t
                blockScope <- newObject $ \o -> o
                    { oDelegates = ds ++ [s]
                    , oMethods = ms
                    }

                res <- withTop blockScope (evalAll bes)
                new <- objectFor blockScope
                liftIO $ writeIORef r new
                    { oDelegates = oDelegates new \\ [s]
                    , oMethods = oMethods new
                    }

                return res
            _ -> do
                blockScope <- newObject $ \o -> o
                    { oDelegates = [t, s]
                    }

                withTop blockScope (evalAll bes)
    | otherwise = do
        -- a toplevel scope with transient definitions
        pseudoScope <- newObject $ \o -> o
            { oMethods = (bs, M.empty)
            }

        case t of
            Reference r -> do
                Object ds ms <- objectFor t
                -- the original prototype, but without its delegations
                -- this is to prevent dispatch loops
                doppelganger <- newObject $ \o -> o
                    { oMethods = ms
                    }

                -- the 3 additional scopes for the block
                let fakes = [pseudoScope, doppelganger, s]

                -- the main scope, methods are taken from here and merged with
                -- the originals. delegates to the pseudoscope and doppelganger
                -- so it has their methods in scope, but definitions go here
                blockScope <- newObject $ \o -> o
                    { oDelegates = ds ++ fakes
                    }

                res <- withTop blockScope (evalAll bes)
                new <- objectFor blockScope
                liftIO (writeIORef r new
                    { oDelegates = oDelegates new \\ fakes
                    , oMethods = merge ms (oMethods new)
                    })

                return res
            _ -> do
                blockScope <- newObject $ \o -> o
                    { oDelegates = [t, pseudoScope, s]
                    }

                withTop blockScope (evalAll bes)
  where
    bs = addMethod (Slot (psingle "this" PSelf) t) $
            toMethods . concat $ zipWith bindings' ps as

    merge (os, ok) (ns, nk) =
        ( foldl (flip addMethod) os (concat $ M.elems ns)
        , foldl (flip addMethod) ok (concat $ M.elems nk)
        )
joinWith _ v _ = error $ "impossible: joinWith on " ++ show v
