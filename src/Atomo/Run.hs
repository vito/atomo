module Atomo.Run where

import Control.Concurrent
import "monads-fd" Control.Monad.State

import Atomo.Core
import Atomo.Environment
import Atomo.Load
import Atomo.Types
import qualified Atomo.Kernel as Kernel

import Paths_atomo


-----------------------------------------------------------------------------
-- Execution ----------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Execute an action in a new thread, initializing the environment first.
exec :: VM Value -> IO ()
exec x = execWith (initEnv >> x) startEnv

-- | Execute an action in a new thread.
execWith :: VM Value -> Env -> IO ()
execWith x e = do
    haltChan <- newChan

    forkIO $ do
        runWith (x >> gets halt >>= liftIO >> return (particle "ok")) e
            { halt = writeChan haltChan () >> myThreadId >>= killThread
            }

        writeChan haltChan ()

    readChan haltChan

-- | Execute x, initializing the environment first.
run :: VM Value -> IO Value
run x = runWith (initEnv >> x) startEnv

-- | Set up the primitive objects, and load up the kernel and prelude.
initEnv :: VM ()
{-# INLINE initEnv #-}
initEnv = initCore >> Kernel.load >> loadPrelude

-- | Load all of the prelude and load the ecosystem.
loadPrelude :: VM ()
loadPrelude = do
    forM_ preludes $ \p ->
        liftIO (getDataFileName ("prelude/" ++ p))
            >>= loadFile

    here "Eco" >>= dispatch . single "load"

    return ()
  where
    preludes =
        [ "core"

        , "boolean"
        , "association"
        , "parameter"
        , "string"

        , "condition"
        , "exception"

        , "block"
        , "comparable"
        , "continuation"
        , "list"
        , "numeric"
        , "particle"
        , "ports"
        , "time"

        , "version"
        , "eco"

        , "repl"
        ]
