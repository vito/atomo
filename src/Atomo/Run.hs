module Atomo.Run where

import Control.Concurrent
import "monads-fd" Control.Monad.State

import Atomo.Core
import Atomo.Environment
import Atomo.Load
import Atomo.Spawn (go)
import Atomo.Types
import qualified Atomo.Kernel as Kernel

import Paths_atomo


-----------------------------------------------------------------------------
-- Execution ----------------------------------------------------------------
-----------------------------------------------------------------------------

-- | execute an action in a new thread, initializing the environment and
-- printing a traceback on error
exec :: VM Value -> IO ()
exec x = execWith (initEnv >> x) startEnv

-- | execute an action in a new thread, printing a traceback on error
execWith :: VM Value -> Env -> IO ()
execWith x e = do
    haltChan <- newChan

    forkIO $ do
        r <- runWith (go x >> gets halt >>= liftIO >> return (particle "ok")) e
            { halt = writeChan haltChan () >> myThreadId >>= killThread
            }

        {-either-}
            {-(putStrLn . ("WARNING: exited abnormally with: " ++) . show)-}
            {-(\_ -> return ())-}
            {-r-}

        writeChan haltChan ()

    readChan haltChan

-- | execute x, initializing the environment with initEnv
run :: VM Value -> IO Value
run x = runWith (initEnv >> x) startEnv

-- | set up the primitive objects, etc.
initEnv :: VM ()
{-# INLINE initEnv #-}
initEnv = initCore >> Kernel.load >> loadPrelude

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
        ]
