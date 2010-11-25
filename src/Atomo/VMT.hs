module Atomo.VMT where

import "monads-fd" Control.Monad.Cont
import "monads-fd" Control.Monad.State
import "monads-fd" Control.Monad.Trans

import Atomo.Types

-- | A monad transformer for Atomo's VM which just passes around the VM
-- environment.
type VMT = StateT Env

-- | Execute a VM action inside a VMT monad.
vm :: MonadIO m => VM Value -> VMT m Value
vm x = StateT (liftIO . runStateT (runContT x return))

-- | Like `vm', but ignores the result.
vm_ :: MonadIO m => VM a -> VMT m ()
vm_ x = vm (x >> return (particle "ok")) >> return ()

-- | Grab the underlying VM environment.
getEnv :: Monad m => VMT m Env
getEnv = StateT $ \e -> return (e, e)

-- | Replace the underlying VM environment.
putEnv :: Monad m => Env -> VMT m ()
putEnv e = StateT $ \_ -> return ((), e)

-- | Execute a VMT monad with an initial environment, returning its result.
execVM :: Monad m => VMT m a -> Env -> m a
execVM x e = liftM fst (runStateT x e)
