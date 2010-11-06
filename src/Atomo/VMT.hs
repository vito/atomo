module Atomo.VMT where

import "monads-fd" Control.Monad.Cont
import "monads-fd" Control.Monad.State
import "monads-fd" Control.Monad.Trans

import Atomo.Types

newtype VMT m a =
    VMT
        { runVMT :: Env -> m (a, Env)
        }

instance Monad m => Monad (VMT m) where
    x >>= f = VMT $ \e -> do
        (v, e') <- runVMT x e
        runVMT (f v) e'

    return x = VMT $ \e -> return (x, e)

instance MonadTrans VMT where
    lift m = VMT $ \e -> do
        a <- m
        return (a, e)

instance MonadIO m => MonadIO (VMT m) where
    liftIO f = VMT $ \e -> do
        x <- liftIO f
        return (x, e)

instance MonadCont m => MonadCont (VMT m) where
    callCC f = VMT $ \e ->
        callCC $ \c -> runVMT (f (\a -> VMT (\e' -> c (a, e')))) e

vm :: MonadIO m => VM Value -> VMT m Value
vm x = VMT (liftIO . runStateT (runContT x return))

vm_ :: MonadIO m => VM a -> VMT m ()
vm_ x = vm (x >> return (particle "ok")) >> return ()

getEnv :: Monad m => VMT m Env
getEnv = VMT $ \e -> return (e, e)

putEnv :: Monad m => Env -> VMT m ()
putEnv e = VMT $ \_ -> return ((), e)

execVM :: Monad m => VMT m a -> Env -> m a
execVM x e = liftM fst (runVMT x e)
