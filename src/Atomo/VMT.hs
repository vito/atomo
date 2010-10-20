module Atomo.VMT where

import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans

import Atomo.Types

newtype VMT m a =
    VMT
        { runVMT :: Env -> m (Either AtomoError a, Env)
        }

instance Monad m => Monad (VMT m) where
    x >>= f = VMT $ \e -> do
        (ev, e') <- runVMT x e
        case ev of
            Right v -> runVMT (f v) e'
            Left err -> return (Left err, e')

    return x = VMT $ \e -> return (Right x, e)

instance MonadTrans VMT where
    lift m = VMT $ \e -> do
        a <- m
        return (Right a, e)

instance MonadIO m => MonadIO (VMT m) where
    liftIO f = VMT $ \e -> do
        x <- liftIO f
        return (Right x, e)

instance MonadCont m => MonadCont (VMT m) where
    callCC f = VMT $ \e ->
        callCC $ \c -> runVMT (f (\a -> VMT (\e' -> c (Right a, e')))) e

vm :: MonadIO m => VM Value -> VMT m Value
vm x = VMT $ \e ->
    liftIO $ runStateT (runContT (runErrorT x) return) e

getEnv :: Monad m => VMT m Env
getEnv = VMT $ \e -> return (Right e, e)

putEnv :: Monad m => Env -> VMT m ()
putEnv e = VMT $ \_ -> return (Right (), e)

execVM :: Monad m => (VMT m a) -> Env -> (AtomoError -> m a) -> m a
execVM x e h = runVMT x e >>= either h return . fst
