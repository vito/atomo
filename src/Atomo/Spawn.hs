module Atomo.Spawn where

import Control.Concurrent
import "monads-fd" Control.Monad.State

import Atomo.Types


-- | spawn a process to execute x. returns the Process.
spawn :: VM Value -> VM Value
spawn x = do
    e <- get
    chan <- liftIO newChan
    tid <- liftIO . forkIO $ do
        runWith (go x >> return (particle "ok")) (e { channel = chan })
        return ()

    return (Process chan tid)

-- | execute x, printing an error if there is one
go :: VM Value -> VM Value
go x = x --catchError x (\e -> printError e >> return (particle "ok"))
