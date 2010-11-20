module Atomo.Spawn where

import Control.Concurrent
import "monads-fd" Control.Monad.State

import Atomo.Types


-- | Spawn a process to execute x. Returns the Process value.
spawn :: VM Value -> VM Value
spawn x = do
    e <- get
    chan <- liftIO newChan
    tid <- liftIO . forkIO $ do
        runWith (x >> return (particle "ok")) (e { channel = chan })
        return ()

    return (Process chan tid)
